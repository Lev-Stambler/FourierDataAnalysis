"""edu-GL: dataset-GL compression of a SCALAR target.

Prior dataset-GL arcs fought vector-valued targets (a full next-token
distribution / a 768-d teacher hidden state).  Here the target is one number:
f(window) = the fineweb-edu-classifier's educational score (~0-5) of a w-token
FineWeb window.  The student is a sparse Fourier model over encoded token bits
-- degree <= 2, every coefficient CALCULATED (closed-form LS / exact pair
enumeration / matching-pursuit deflation), never Adam/ridge.

Self-contained rewrite; math recipes are cribbed (with line refs in
docstrings) from experiments/canonical/qary_lsh_dataset_gl.py on branch
canonical-qary-lsh-gl.

Stages (Modal, volume fda-cache):
  label    - stream FineWeb windows, score with the classifier, cache npz
  fit      - encode -> exact deg-1 -> exact deg-2 psi -> deflate -> ladder
  ceilings - fitted references: ridge + tiny MLP on mean-pooled embeddings
  show     - print saved summaries
"""

import hashlib
import json
import os

import numpy as np

import modal

MODEL_ID = "HuggingFaceFW/fineweb-edu-classifier"
FINEWEB = ("HuggingFaceFW/fineweb", "CC-MAIN-2024-10")
W_WIN = 64            # window tokens (classifier tokenizer)
B_LSH = 64            # sign-LSH bits per token (before tie-breaks)
ROOT = "/cache/edu_gl"

app = modal.App("edu-gl")
vol = modal.Volume.from_name("fda-cache", create_if_missing=True)
image = (
    modal.Image.debian_slim(python_version="3.11")
    .pip_install("numpy>=1.26", "torch>=2.5", "transformers>=4.56",
                 "datasets>=4.0", "wandb>=0.18")
    .env({"HF_HOME": "/cache/hf"})
)
try:
    WANDB_SECRET = [modal.Secret.from_name("wandb")]
except Exception:
    WANDB_SECRET = []
try:
    HF_SECRET = [modal.Secret.from_name("hf-token")]
except Exception:
    HF_SECRET = []


def _wandb_run(name, config):
    """Best-effort W&B run; logging never breaks a fit."""
    try:
        import wandb
        return wandb.init(project="fda-edu-gl", name=name, config=config,
                          reinit=True)
    except Exception as e:
        print(f"[wandb] disabled ({e})", flush=True)
        return None


# ------------------------------------------------------------------ code tables
# crib: canonical qary_lsh_dataset_gl.py L115-165 (fixed B here, no doubling)

def _tie_break(codes):
    """Append rank bits inside duplicate-code groups; assert global uniqueness."""
    _, inv, counts = np.unique(codes, axis=0, return_inverse=True, return_counts=True)
    groups = [np.flatnonzero(inv == g) for g in np.flatnonzero(counts > 1)]
    if groups:
        width = int(max(len(g) - 1 for g in groups)).bit_length()
        extra = np.zeros((len(codes), width), dtype=np.uint8)
        for group in groups:
            for rank, row in enumerate(np.sort(group)):
                extra[row] = (rank >> np.arange(width)) & 1
        codes = np.concatenate([codes, extra], axis=1)
    if len(np.unique(codes, axis=0)) < len(codes):
        raise RuntimeError("codes are not unique after tie-breaking")
    return codes


def build_lsh_codes(E, B=B_LSH, seed=0):
    """Sign-LSH: mean-center embedding rows, project onto B fixed Gaussians,
    sign-threshold; duplicate codes get rank tie-break bits."""
    E = np.asarray(E, dtype=np.float32)
    centered = E - E.mean(axis=0, keepdims=True)
    G = np.random.default_rng(seed).standard_normal((B, E.shape[1])).astype(np.float32)
    return _tie_break((centered @ G.T > 0).astype(np.uint8))


def control_codes(q, B, seed=1):
    """Capacity-matched iid random bits; deterministic seed retry on the
    (small-B) birthday collisions."""
    for s in range(seed, seed + 100):
        codes = np.random.default_rng(s).integers(0, 2, (q, B), dtype=np.uint8)
        if len(np.unique(codes, axis=0)) == q:
            return codes
    raise RuntimeError("control codes collided")


def token_id_codes(q):
    width = max(1, int(q - 1).bit_length())
    return ((np.arange(q, dtype=np.int64)[:, None] >> np.arange(width)) & 1).astype(np.uint8)


def window_bits(windows, codes):
    """(D, w) token ids -> (D, w*B) uint8 bits, token blocks in text order
    (no reversal: plain deg-1/2 needs no suffix conditioning)."""
    windows = np.asarray(windows, dtype=np.int64)
    return np.asarray(codes, dtype=np.uint8)[windows].reshape(len(windows), -1)


# -------------------------------------------------------------------- math core
# every coefficient below is CALCULATED; no gradient fitting anywhere

def xor_parity_features(bits, idx):
    """(D, K) +-1 Walsh characters from (K, <=4) bit-index rows (-1 = unused):
    XOR of the selected bits, so a duplicated index cancels exactly like the
    +-1 product.  torch-in/torch-out on the tensor's device, else numpy.
    crib: canonical L1021-1041."""
    import torch
    if torch.is_tensor(bits):
        idx_t = torch.as_tensor(np.asarray(idx, np.int64), device=bits.device)
        par = torch.zeros((len(bits), len(idx_t)), dtype=torch.uint8, device=bits.device)
        for s in range(idx_t.shape[1]):
            sel = idx_t[:, s]; v = sel >= 0
            if bool(v.any()):
                par[:, v] ^= bits[:, sel[v]]
        return 1.0 - 2.0 * par.float()
    idx = np.asarray(idx, np.int64)
    par = np.zeros((len(bits), len(idx)), np.uint8)
    for s in range(idx.shape[1]):
        sel = idx[:, s]; v = sel >= 0
        if v.any():
            par[:, v] ^= bits[:, sel[v]]
    return (1.0 - 2.0 * par).astype(np.float32)


def mask_parity_features(bits, masks, ctx_chunk=262144):
    """(D, K) +-1 parities of DENSE bit-masks of any degree: fp32 GEMM is
    exact (bit counts << 2^24), 1 - 2*((bits @ m^T) % 2).
    crib: canonical sequential_deflate masks branch L1207-1219."""
    import torch
    if torch.is_tensor(bits):
        m_t = torch.as_tensor(np.asarray(masks, np.float32), device=bits.device)
        out = torch.empty((len(bits), len(m_t)), device=bits.device)
        for lo in range(0, len(bits), ctx_chunk):
            bf = bits[lo:lo + ctx_chunk].float()
            out[lo:lo + ctx_chunk] = 1.0 - 2.0 * ((bf @ m_t.t()) % 2.0)
        return out
    m = np.asarray(masks, np.float32)
    return (1.0 - 2.0 * ((np.asarray(bits, np.float32) @ m.T) % 2.0)) \
        .astype(np.float32)


def fit_deg1_exact(bits, y, device=None, wd=1e-3, ctx_chunk=131072):
    """Closed-form LS of the (centered) scalar target on ALL n degree-1
    characters + bias: tiled fp32 Gram accumulation, one fp64 solve, never
    penalize the bias.  Subtracting this fit leaves the train residual with
    no degree-1 content.  Returns (W (n,) float32, b float).
    crib: canonical fit_deg1_exact L1257-1283 with dY=1."""
    import torch
    device = device or ("cuda" if torch.cuda.is_available() else "cpu")
    bits_t = bits if torch.is_tensor(bits) else \
        torch.tensor(np.asarray(bits, np.uint8), device=device)
    dev = bits_t.device
    y_t = y if torch.is_tensor(y) else torch.tensor(np.asarray(y, np.float32), device=dev)
    n = bits_t.shape[1]
    S = torch.zeros((n + 1, n + 1), dtype=torch.float64, device=dev)
    bvec = torch.zeros((n + 1,), dtype=torch.float64, device=dev)
    for lo in range(0, len(bits_t), ctx_chunk):
        X = 1.0 - 2.0 * bits_t[lo:lo + ctx_chunk].float()
        Xa = torch.cat([X, torch.ones((len(X), 1), device=dev)], dim=1)
        S += (Xa.t() @ Xa).double()
        bvec += (Xa.t() @ y_t[lo:lo + ctx_chunk].float()).double()
    reg = wd * torch.eye(n + 1, dtype=torch.float64, device=dev)
    reg[-1, -1] = 0.0
    Wa = torch.linalg.solve(S + reg, bvec)
    return Wa[:-1].to(torch.float32).cpu().numpy(), float(Wa[-1])


def deg2_psi_scalar(bits, g, device=None, ctx_chunk=131072):
    """EXACT degree-2 spectroscopy for a SCALAR residual g (dY=1 kills the PCA
    projection of the vector version): the plain Fourier coefficient of every
    pair character chi_a*chi_b is an entry of M = X^T diag(g) X / D, so ONE
    tiled GEMM enumerates all n(n-1)/2 pairs.  Per-pair estimation noise
    ~ mean(g^2)/D (the caller's floor).  Returns (psi2 (n, n) float32,
    symmetric, zero diagonal; mass = mean(g^2)).
    crib: canonical deg2_exact_psi L1435-1485."""
    import torch
    device = device or ("cuda" if torch.cuda.is_available() else "cpu")
    bits_t = bits if torch.is_tensor(bits) else \
        torch.tensor(np.asarray(bits, np.uint8), device=device)
    dev = bits_t.device
    g_t = g if torch.is_tensor(g) else torch.tensor(np.asarray(g, np.float32), device=dev)
    D, n = bits_t.shape
    ft = torch.bfloat16 if dev.type == "cuda" else torch.float32
    M = torch.zeros((n, n), dtype=torch.float32, device=dev)
    for lo in range(0, D, ctx_chunk):
        X = 1.0 - 2.0 * bits_t[lo:lo + ctx_chunk].to(ft)
        gd = g_t[lo:lo + ctx_chunk].float()
        M += (X.t() @ (X * gd[:, None].to(ft))).float()
    M /= D
    psi2 = M ** 2
    psi2.fill_diagonal_(0.0)
    return psi2.cpu().numpy(), float((g_t.float() ** 2).mean())


def sequential_deflate(bits, g, idx, device=None, char_chunk=1024, block=512,
                       masks=None):
    """TRUE matching-pursuit deflation of a scalar residual: subtract each
    character's plain Fourier coefficient against the CURRENT residual, in the
    given order.  Batch deflation is a KNOWN FAILURE (amplifies on-data
    duplicate clusters); sequentially a later duplicate sees ~0 residual and
    gets ~0 coefficient, and every rank-1 update is a projection so the
    residual can never grow.  ``block > 1`` = block-OMP: exact joint LS per
    block (row-tiled Gram, 1e-4 jitter -- a duplicate cluster's coefficient
    SPLITS across copies, summing correctly), Gauss-Seidel across blocks.
    Returns (C (K,) float32, g_out) with g mutated in place when torch.
    crib: canonical sequential_deflate L1180-1254."""
    import torch
    device = device or ("cuda" if torch.cuda.is_available() else "cpu")
    was_np = not torch.is_tensor(g)
    bits_t = bits if torch.is_tensor(bits) else \
        torch.tensor(np.asarray(bits, np.uint8), device=device)
    dev = bits_t.device
    g_t = g if torch.is_tensor(g) else torch.tensor(np.asarray(g, np.float32), device=dev)
    if masks is not None:
        masks = np.asarray(masks, np.uint8)
        idx = np.zeros((len(masks), 4), np.int16)                 # length only
        _feat = lambda lo, hi: mask_parity_features(bits_t, masks[lo:hi])
    else:
        idx = np.asarray(idx, np.int16)
        _feat = lambda lo, hi: xor_parity_features(bits_t, idx[lo:hi])
    D = len(bits_t)
    C = np.empty(len(idx), np.float32)
    if block > 1:
        for klo in range(0, len(idx), block):
            F = _feat(klo, min(klo + block, len(idx)))
            k = F.shape[1]
            S = torch.zeros((k, k), dtype=torch.float32, device=dev)
            bvec = torch.zeros((k,), dtype=torch.float32, device=dev)
            for lo in range(0, D, 262144):
                Ft = F[lo:lo + 262144]
                S += Ft.t() @ Ft
                bvec += Ft.t() @ g_t[lo:lo + 262144]
            S /= D; bvec /= D
            S += 1e-4 * torch.eye(k, device=dev)                   # dup clusters: singular
            C_b = torch.linalg.solve(S.double(), bvec.double()).float()
            for lo in range(0, D, 262144):
                g_t[lo:lo + 262144] -= F[lo:lo + 262144] @ C_b
            C[klo:klo + k] = C_b.cpu().numpy()
        return C, (g_t.cpu().numpy() if was_np else g_t)
    for klo in range(0, len(idx), char_chunk):
        F = _feat(klo, min(klo + char_chunk, len(idx)))
        for j in range(F.shape[1]):
            c = float(F[:, j] @ g_t) / D
            g_t -= c * F[:, j]
            C[klo + j] = c
    return C, (g_t.cpu().numpy() if was_np else g_t)


def deg3_anchored_psi(bits, g, pair_idx, device=None, anchor_chunk=256):
    """EXACT anchored degree-3 spectroscopy: for every anchor pair (a,b) and
    every third bit c, the plain Fourier coefficient of chi_a chi_b chi_c on
    the residual g is M3[c, j] = mean(g * chi_{ab_j} * x_c) -- one
    (n x D)(D x A) GEMM sweep over anchor chunks.  Anchoring on the top
    deflated pairs is the same local-first heuristic as the canonical deg-3
    stage (qary_lsh_dataset_gl.py deg3_fit).  Returns M3 (n, A) float32."""
    import torch
    device = device or ("cuda" if torch.cuda.is_available() else "cpu")
    bits_t = bits if torch.is_tensor(bits) else \
        torch.tensor(np.asarray(bits, np.uint8), device=device)
    dev = bits_t.device
    g_t = g if torch.is_tensor(g) else torch.tensor(np.asarray(g, np.float32), device=dev)
    D, n = bits_t.shape
    A = len(pair_idx)
    ft = torch.bfloat16 if dev.type == "cuda" else torch.float32
    M3 = torch.zeros((n, A), dtype=torch.float32, device=dev)
    for alo in range(0, A, anchor_chunk):
        ahi = min(alo + anchor_chunk, A)
        Chi = xor_parity_features(bits_t, pair_idx[alo:ahi])       # (D, a)
        Chi *= g_t[:, None]
        acc = torch.zeros((n, ahi - alo), dtype=torch.float32, device=dev)
        for lo in range(0, D, 131072):
            X = 1.0 - 2.0 * bits_t[lo:lo + 131072].to(ft)
            acc += (X.t() @ Chi[lo:lo + 131072].to(ft)).float()
        M3[:, alo:ahi] = acc / D
    return M3.cpu().numpy()


def select_triples(M3, pair_idx, floor, max_triples):
    """Candidate triples (a, b, c) from the anchored map: psi3 = M3^2 above
    2x floor, c not in the anchor pair, canonical-sorted and deduped keeping
    each triple's highest-psi occurrence, ordered by psi desc, capped."""
    n, A = M3.shape
    psi3 = M3.astype(np.float64) ** 2
    cs, js = np.nonzero(psi3 > 2.0 * floor)
    if len(cs) == 0:
        return np.zeros((0, 4), np.int16), np.zeros(0)
    a = pair_idx[js, 0].astype(np.int64); b = pair_idx[js, 1].astype(np.int64)
    c = cs.astype(np.int64)
    keep = (c != a) & (c != b)                       # duplicated bit = deg-1 alias
    a, b, c, vals = a[keep], b[keep], c[keep], psi3[cs[keep], js[keep]]
    tri = np.sort(np.stack([a, b, c], 1), axis=1)
    keys = (tri[:, 0] * n + tri[:, 1]) * n + tri[:, 2]
    order = np.argsort(-vals, kind="stable")
    _, first = np.unique(keys[order], return_index=True)
    sel = order[first]                               # max-psi occurrence per triple
    sel = sel[np.argsort(-vals[sel], kind="stable")][:max_triples]
    idx3 = np.full((len(sel), 4), -1, np.int16)
    idx3[:, :3] = tri[sel]
    return idx3, vals[sel]


# --------------------------------------------------------------------- metrics

def normalize_scores(y_raw):
    """Raw classifier logits -> f in [-1, 1] (theory contract ||f|| <= 1):
    clip to the model card's [0, 5], center, scale."""
    return ((np.clip(np.asarray(y_raw, np.float32), 0.0, 5.0) - 2.5) / 2.5)


def _spearman(a, b):
    """Rank correlation (stable-sort ranks; raw scores are continuous, so tie
    handling is immaterial)."""
    def rank(x):
        order = np.argsort(x, kind="stable")
        r = np.empty(len(x)); r[order] = np.arange(len(x))
        return r
    ra, rb = rank(np.asarray(a)), rank(np.asarray(b))
    ra -= ra.mean(); rb -= rb.mean()
    denom = float(np.sqrt((ra ** 2).sum() * (rb ** 2).sum()))
    return float((ra * rb).sum() / denom) if denom > 0 else 0.0


def score_metrics(pred_norm, y_raw):
    """Student prediction (normalized f units) vs raw classifier score, all
    reported in 0-5 units with the model card's clipping.  Spearman uses the
    unclipped prediction (monotone-equivalent, avoids tie inflation).
    f1_ge3 matches the card's binary >=3 evaluation (its Llama3 F1 is 0.82)."""
    y = np.clip(np.asarray(y_raw, np.float64), 0.0, 5.0)
    p_lin = 2.5 * np.asarray(pred_norm, np.float64) + 2.5
    p = np.clip(p_lin, 0.0, 5.0)
    mse = float(np.mean((p - y) ** 2))
    var = float(np.var(y)) + 1e-12
    pt, yt = p >= 3.0, y >= 3.0
    tp = float((pt & yt).sum())
    f1 = 2 * tp / max(1.0, 2 * tp + float((pt & ~yt).sum()) + float((~pt & yt).sum()))
    return {"mse": mse, "r2": 1.0 - mse / var, "spearman": _spearman(p_lin, y),
            "binned_acc": float(np.mean(np.rint(p) == np.rint(y))), "f1_ge3": f1}


# ------------------------------------------------------------------------- fit

def fit_core(bits_tr, y_tr_raw, evals, max_pairs=400_000, block=512, device=None,
             ks=(100, 1_000, 10_000, 50_000, 200_000), log=None, ctx_chunk=131072,
             deg3_anchors=0, max_triples=200_000,
             ks3=(1_000, 10_000, 50_000, 100_000)):
    """The whole calculated pipeline: c0 -> exact deg-1 -> exact deg-2 psi with
    noise floor -> matching-pursuit deflation -> cumulative pair ladder.
    ``evals`` maps tag -> (bits, y_raw); metrics per rung per tag.
    Flow crib: canonical deg2_exact/deg2_fit L3401-3585 (scalar, unit weights).
    Returns (summary dict, model dict {c0, W1, b1, idx, C})."""
    import torch
    device = device or ("cuda" if torch.cuda.is_available() else "cpu")
    emit = log or (lambda d: None)
    ytr = normalize_scores(y_tr_raw)
    c0 = float(ytr.mean())
    bits_t = torch.tensor(np.asarray(bits_tr, np.uint8), device=device)
    dev = bits_t.device
    g = torch.tensor(ytr - c0, device=dev)
    D, n = bits_t.shape
    mass0 = float((g ** 2).mean())
    W1, b1 = fit_deg1_exact(bits_t, g, device=dev)
    W1_t = torch.tensor(W1, device=dev)
    for lo in range(0, D, ctx_chunk):
        g[lo:lo + ctx_chunk] -= \
            (1.0 - 2.0 * bits_t[lo:lo + ctx_chunk].float()) @ W1_t + b1
    res1 = float((g ** 2).mean())
    eb = {t: torch.tensor(np.asarray(b, np.uint8), device=dev) for t, (b, _) in evals.items()}
    base = {t: (1.0 - 2.0 * eb[t].float()) @ W1_t + (b1 + c0) for t in eb}
    summary = {"n_bits": int(n), "D": int(D), "c0": c0, "mass0": mass0,
               "res_deg1": res1,
               "const": {t: score_metrics(np.full(len(y), c0), y)
                         for t, (_, y) in evals.items()},
               "deg1": {t: score_metrics(base[t].cpu().numpy(), evals[t][1])
                        for t in evals}}
    print(f"[fit] n={n} bits, D={D}; deg-1 residual {res1:.4f}/{mass0:.4f}; "
          + " ".join(f"{t} R2 {summary['deg1'][t]['r2']:.4f}" for t in evals),
          flush=True)
    emit({"pairs": 0, **{f"{t}_r2": summary["deg1"][t]["r2"] for t in evals}})
    psi2, _ = deg2_psi_scalar(bits_t, g, device=dev)
    floor = res1 / D
    iu = np.triu_indices(n, k=1)
    vals = psi2[iu]
    order = np.argsort(-vals)
    n_above = int((vals > 2.0 * floor).sum())
    K = int(min(n_above, max_pairs))
    summary.update({"pair_floor": floor, "pairs_above_2floor": n_above, "K": K,
                    "psi_max": float(vals[order[0]]) if len(vals) else 0.0})
    print(f"[fit] pair floor {floor:.3e}; {n_above} pairs > 2x floor "
          f"(max psi {summary['psi_max']:.3e}); fitting K={K}", flush=True)
    idx = np.full((K, 4), -1, np.int16)
    ladder = []
    C = np.zeros(0, np.float32)
    if K:
        idx[:, 0] = iu[0][order[:K]]
        idx[:, 1] = iu[1][order[:K]]
        C, g = sequential_deflate(bits_t, g, idx, device=dev, block=block)
        summary["res_deg2"] = float((g ** 2).mean())
        cum_cap = np.cumsum(C.astype(np.float64) ** 2)
        adds = {t: torch.zeros(len(eb[t]), device=dev) for t in eb}
        C_t = torch.tensor(C, device=dev)
        lo = 0
        for k in sorted({k for k in ks if k < K} | {K}):
            for blo in range(lo, k, 4096):
                hi = min(blo + 4096, k)
                for t in eb:
                    adds[t] += xor_parity_features(eb[t], idx[blo:hi]) @ C_t[blo:hi]
            lo = k
            entry = {"k": int(k), "cum_captured": float(cum_cap[k - 1])}
            for t in eb:
                entry[t] = score_metrics((base[t] + adds[t]).cpu().numpy(), evals[t][1])
            ladder.append(entry)
            print(f"[fit] === deg-1 + {k} pairs: "
                  + " ".join(f"{t} R2 {entry[t]['r2']:.4f} MSE {entry[t]['mse']:.4f}"
                             for t in eb)
                  + f" (captured {entry['cum_captured']:.4f})", flush=True)
            emit({"pairs": k, "cum_captured": entry["cum_captured"],
                  **{f"{t}_{m}": entry[t][m] for t in eb
                     for m in ("r2", "mse", "spearman", "f1_ge3")}})
    summary["ladder"] = ladder
    idx3 = np.zeros((0, 4), np.int16)
    C3 = np.zeros(0, np.float32)
    if deg3_anchors and K:
        # deg-3 on the deg-2 residual, anchored on the top deflated pairs --
        # run with max_pairs at the val-selected deg-2 rung so the residual
        # (and the eval base) is the val-optimal deg-2 model, not the noise tail
        anch = np.argsort(-np.abs(C))[:deg3_anchors]
        M3 = deg3_anchored_psi(bits_t, g, idx[anch], device=dev)
        floor3 = float((g ** 2).mean()) / D
        idx3, psi3 = select_triples(M3, idx[anch], floor3, max_triples)
        print(f"[fit] deg-3: {deg3_anchors} anchors, floor {floor3:.3e}, "
              f"{len(idx3)} triples kept (max psi {psi3[0]:.3e})"
              if len(idx3) else "[fit] deg-3: no triples above floor", flush=True)
        if len(idx3):
            C3, g = sequential_deflate(bits_t, g, idx3, device=dev, block=block)
            summary["res_deg3"] = float((g ** 2).mean())
            C3_t = torch.tensor(C3, device=dev)
            lo = 0
            ladder3 = []
            for k in sorted({k for k in ks3 if k < len(idx3)} | {len(idx3)}):
                for blo in range(lo, k, 4096):
                    hi = min(blo + 4096, k)
                    for t in eb:
                        adds[t] += xor_parity_features(eb[t], idx3[blo:hi]) @ C3_t[blo:hi]
                lo = k
                entry = {"k3": int(k)}
                for t in eb:
                    entry[t] = score_metrics((base[t] + adds[t]).cpu().numpy(),
                                             evals[t][1])
                ladder3.append(entry)
                print(f"[fit] === deg-2 + {k} triples: "
                      + " ".join(f"{t} R2 {entry[t]['r2']:.4f}" for t in eb),
                      flush=True)
                emit({"triples": k,
                      **{f"{t}3_{m}": entry[t][m] for t in eb
                         for m in ("r2", "mse", "spearman")}})
            summary["ladder3"] = ladder3
    return summary, {"c0": c0, "W1": W1, "b1": np.float32(b1), "idx": idx, "C": C,
                     "idx3": idx3, "C3": C3}


# ------------------------------------------------------------------------ data

def stream_windows(tok, n, w=W_WIN, skip=0):
    """Stream distinct w-token windows from FineWeb with the classifier's own
    tokenizer; deterministic hash-of-text window start; ``skip`` discards the
    first ``skip`` qualifying docs so later draws never reuse held-out ones.
    crib: canonical _stream_spans L1610-1635."""
    from datasets import load_dataset
    tok_hf = (os.environ.get("HF_TOKEN") or os.environ.get("HF_HUB_TOKEN")
              or os.environ.get("HUGGING_FACE_HUB_TOKEN"))
    ds = load_dataset(FINEWEB[0], name=FINEWEB[1], split="train", streaming=True,
                      token=tok_hf)
    wins, seen = [], 0
    for row in ds:
        text = row.get("text") or ""
        ids = np.asarray(tok(text, add_special_tokens=False)["input_ids"],
                         dtype=np.int32)
        if len(ids) >= w:
            seen += 1
            if seen > skip:
                digest = hashlib.sha256(("win:" + text).encode("utf-8", "ignore")).digest()
                start = int.from_bytes(digest[:8], "big") % (len(ids) - w + 1)
                wins.append(ids[start:start + w])
                if len(wins) % 100_000 == 0:
                    print(f"[stream] {len(wins)}/{n} windows", flush=True)
        if len(wins) >= n:
            return np.stack(wins)
    raise RuntimeError(f"stream ended at {len(wins)}/{n} windows")


def _score_windows(model, tok, wins, batch=256):
    """f(window) exactly: [CLS] + window ids + [SEP] (no decode/re-encode
    round trip), single logit = educational score."""
    import torch
    cls_col = np.full((len(wins), 1), tok.cls_token_id, np.int32)
    sep_col = np.full((len(wins), 1), tok.sep_token_id, np.int32)
    ids = np.concatenate([cls_col, wins, sep_col], axis=1)
    ys = []
    for lo in range(0, len(ids), batch):
        t = torch.tensor(ids[lo:lo + batch].astype(np.int64), device="cuda")
        with torch.inference_mode():
            ys.append(model(input_ids=t).logits.squeeze(-1).float().cpu().numpy())
    return np.concatenate(ys).astype(np.float32)


def _data_path(w, n_train, n_val, n_test):
    return f"{ROOT}/data_w{w}_tr{n_train}_va{n_val}_te{n_test}.npz"


def _shard_ok(path, m):
    """A shard of the wrong length (e.g. from a smaller earlier run sharing the
    same skip offset) counts as MISSING -- silent reuse shrank a test split
    once (2026-07-15)."""
    if not os.path.exists(path):
        return False
    try:
        z = np.load(path)
        return len(z["y"]) == m and len(z["tokens"]) == m
    except Exception:
        return False


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=43200,
              memory=32768, secrets=WANDB_SECRET + HF_SECRET)
def label(n_train: int = 1_000_000, n_val: int = 25_000, n_test: int = 25_000,
          w: int = W_WIN, batch: int = 256):
    """Stream + score windows; splits by deterministic stream position (test =
    first n_test qualifying docs, val next, train after), one window per doc.
    Shard-resumable; also caches the classifier's token embeddings for the
    LSH encoder + param count for compression accounting."""
    import time
    import torch
    from transformers import AutoModelForSequenceClassification, AutoTokenizer
    vol.reload()
    out = _data_path(w, n_train, n_val, n_test)
    if os.path.exists(out) and os.path.exists(f"{ROOT}/emb.npz"):
        z = np.load(out)
        if all(len(z[f"y_{t}"]) == m for t, m in
               (("te", n_test), ("va", n_val), ("tr", n_train))):
            print(f"exists: {out}", flush=True)
            return out
        print(f"[label] {out} has wrong split sizes -- rebuilding", flush=True)
    os.makedirs(ROOT, exist_ok=True)
    tok = AutoTokenizer.from_pretrained(MODEL_ID)
    model = AutoModelForSequenceClassification.from_pretrained(
        MODEL_ID, dtype=torch.bfloat16).cuda().eval()
    if not os.path.exists(f"{ROOT}/emb.npz"):
        E = model.bert.embeddings.word_embeddings.weight.detach().float().cpu().numpy()
        np.savez(f"{ROOT}/emb.npz", E=E.astype(np.float16),
                 n_params=np.int64(sum(p.numel() for p in model.parameters())))
        vol.commit()
    run = _wandb_run(f"label-w{w}-N{n_train}",
                     {"w": w, "n_train": n_train, "n_val": n_val,
                      "n_test": n_test, "batch": batch})
    shard = 50_000
    sdir = f"{ROOT}/shards_w{w}"
    os.makedirs(sdir, exist_ok=True)
    data = {}
    for tag, n, skip in (("te", n_test, 0), ("va", n_val, n_test),
                         ("tr", n_train, n_test + n_val)):
        t0 = time.time()
        missing = [lo for lo in range(0, n, shard)
                   if not _shard_ok(f"{sdir}/{tag}_s{skip + lo}.npz",
                                    min(shard, n - lo))]
        if missing:
            # one stream pass covers this split's whole missing range
            base = missing[0]
            wins = stream_windows(tok, min(n, missing[-1] + shard) - base, w,
                                  skip + base)
            for lo in missing:
                sl = wins[lo - base: lo - base + min(shard, n - lo)]
                y = _score_windows(model, tok, sl, batch)
                sp = f"{sdir}/{tag}_s{skip + lo}.npz"
                np.savez(sp + ".tmp.npz", tokens=sl, y=y)
                os.replace(sp + ".tmp.npz", sp)
                vol.commit()
                done = min(lo + shard, n)
                print(f"[label:{tag}] {done}/{n} ({time.time() - t0:.0f}s)",
                      flush=True)
                if run is not None:
                    run.log({f"{tag}_labeled": done,
                             "windows_per_s": done / max(1.0, time.time() - t0),
                             f"{tag}_score_mean": float(np.clip(y, 0, 5).mean())})
        parts = [np.load(f"{sdir}/{tag}_s{skip + lo}.npz")
                 for lo in range(0, n, shard)]
        data[f"tok_{tag}"] = np.concatenate([p["tokens"] for p in parts])
        data[f"y_{tag}"] = np.concatenate([p["y"] for p in parts])
        assert len(data[f"y_{tag}"]) == n, (tag, len(data[f"y_{tag}"]), n)
    y = np.clip(data["y_tr"], 0, 5)
    hist, _ = np.histogram(y, bins=10, range=(0, 5))
    print(f"[label] train score mean {y.mean():.3f} std {y.std():.3f}; "
          f"hist(0..5, 10 bins) {hist.tolist()}", flush=True)
    np.savez(out + ".tmp.npz", **data)
    os.replace(out + ".tmp.npz", out)
    vol.commit()
    if run is not None:
        try:
            import wandb
            run.log({"train_score_hist": wandb.Histogram(
                np_histogram=(hist.tolist(), np.linspace(0, 5, 11).tolist()))})
        except Exception:
            pass
        run.summary.update({"train_score_mean": float(y.mean()),
                            "train_score_std": float(y.std())})
        run.finish()
    print(f"saved {out}", flush=True)
    return out


# ------------------------------------------------------------------ fit stages

def build_codes(encoding, b=B_LSH):
    """Code table per encoding; ctrl is capacity-matched to the LSH table."""
    E = np.load(f"{ROOT}/emb.npz")["E"].astype(np.float32)
    lsh = build_lsh_codes(E, B=b)
    if encoding == "lsh":
        return lsh
    if encoding == "ctrl":
        return control_codes(len(E), lsh.shape[1])
    if encoding == "tokid":
        return token_id_codes(len(E))
    raise ValueError(f"unknown encoding {encoding}")


@app.function(image=image, gpu="A100-40GB", volumes={"/cache": vol},
              timeout=43200, memory=49152, secrets=WANDB_SECRET)
def fit(encoding: str = "lsh", w: int = W_WIN, b: int = B_LSH,
        max_pairs: int = 400_000, n_train: int = 1_000_000,
        n_val: int = 25_000, n_test: int = 25_000, block: int = 512,
        deg3_anchors: int = 0, max_triples: int = 200_000):
    """Encode -> fit_core -> compression accounting -> save model + summary."""
    vol.reload()
    data = np.load(_data_path(w, n_train, n_val, n_test))
    codes = build_codes(encoding, b)
    q, B_total = codes.shape
    bits_tr = window_bits(data["tok_tr"], codes)
    evals = {"val": (window_bits(data["tok_va"], codes), data["y_va"]),
             "test": (window_bits(data["tok_te"], codes), data["y_te"])}
    run = _wandb_run(f"fit-{encoding}-w{w}-N{n_train}",
                     {"encoding": encoding, "w": w, "B_total": B_total,
                      "n_train": n_train, "max_pairs": max_pairs})
    summary, model = fit_core(bits_tr, data["y_tr"], evals,
                              max_pairs=max_pairs, block=block,
                              deg3_anchors=deg3_anchors, max_triples=max_triples,
                              log=(run.log if run is not None else None))
    n_params = int(np.load(f"{ROOT}/emb.npz")["n_params"])
    teacher_bytes = 4 * n_params
    fixed = q * B_total / 8 + (summary["n_bits"] + 1) * 4    # code table + deg-1
    for entry in summary["ladder"]:
        entry["student_bytes"] = int(fixed + 8 * entry["k"])  # 2xint16 + fp32/pair
        entry["compression_x"] = teacher_bytes / entry["student_bytes"]
    summary.update({"encoding": encoding, "w": w, "B_total": int(B_total),
                    "n_train": n_train, "teacher_bytes": teacher_bytes})
    np.savez_compressed(f"{ROOT}/model_{encoding}_w{w}_N{n_train}.npz",
                        codes=codes, **model)
    with open(f"{ROOT}/summary_fit_{encoding}_w{w}_N{n_train}.json", "w") as fh:
        json.dump(summary, fh, indent=1)
    vol.commit()
    if run is not None:
        best = max((e["test"]["r2"] for e in summary["ladder"]),
                   default=summary["deg1"]["test"]["r2"])
        run.summary.update({"deg1_test_r2": summary["deg1"]["test"]["r2"],
                            "best_test_r2": best, "K": summary["K"]})
        run.finish()
    print(json.dumps(summary), flush=True)
    return summary


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=43200,
              memory=32768, secrets=WANDB_SECRET)
def ceilings(w: int = W_WIN, n_train: int = 1_000_000, n_val: int = 25_000,
             n_test: int = 25_000):
    """FITTED reference ceilings (fitting allowed here, unlike the student):
    ridge + tiny MLP on mean-pooled classifier word-embeddings of the window."""
    import torch
    vol.reload()
    data = np.load(_data_path(w, n_train, n_val, n_test))
    E = np.load(f"{ROOT}/emb.npz")["E"].astype(np.float32)
    dev = "cuda" if torch.cuda.is_available() else "cpu"
    E_t = torch.tensor(E, device=dev)

    def pool(tokens):
        out = torch.empty((len(tokens), E.shape[1]), device=dev)
        for lo in range(0, len(tokens), 65536):
            t = torch.tensor(tokens[lo:lo + 65536].astype(np.int64), device=dev)
            out[lo:lo + 65536] = E_t[t].mean(dim=1)
        return out

    Xtr, Xva, Xte = pool(data["tok_tr"]), pool(data["tok_va"]), pool(data["tok_te"])
    ytr = torch.tensor(normalize_scores(data["y_tr"]), device=dev)
    mu, sd = Xtr.mean(0, keepdim=True), Xtr.std(0, keepdim=True) + 1e-6
    Xtr, Xva, Xte = (Xtr - mu) / sd, (Xva - mu) / sd, (Xte - mu) / sd
    c0 = float(ytr.mean())
    summary = {}
    # ridge, closed form, lambda swept on val
    G = (Xtr.t() @ Xtr).double()
    r = (Xtr.t() @ (ytr - c0)).double()
    eye = torch.eye(Xtr.shape[1], dtype=torch.float64, device=dev)
    best = None
    for lam in [10.0 ** e for e in range(-2, 7)]:
        beta = torch.linalg.solve(G + lam * eye, r).float()
        vm = score_metrics((Xva @ beta + c0).cpu().numpy(), data["y_va"])
        if best is None or vm["mse"] < best[1]["mse"]:
            best = (lam, vm, beta)
    summary["ridge"] = {"lam": best[0], "val": best[1],
                        "test": score_metrics((Xte @ best[2] + c0).cpu().numpy(),
                                              data["y_te"])}
    print(f"[ceilings] ridge lam {best[0]:g}: test R2 "
          f"{summary['ridge']['test']['r2']:.4f}", flush=True)
    # tiny MLP, early stop on val
    torch.manual_seed(0)
    net = torch.nn.Sequential(torch.nn.Linear(E.shape[1], 256), torch.nn.ReLU(),
                              torch.nn.Linear(256, 1)).to(dev)
    opt = torch.optim.Adam(net.parameters(), lr=1e-3)
    best_val, best_state, bad = float("inf"), None, 0
    for epoch in range(40):
        perm = torch.randperm(len(Xtr), device=dev)
        for lo in range(0, len(Xtr), 8192):
            sel = perm[lo:lo + 8192]
            loss = ((net(Xtr[sel]).squeeze(-1) - ytr[sel]) ** 2).mean()
            opt.zero_grad(); loss.backward(); opt.step()
        with torch.no_grad():
            vm = float(((net(Xva).squeeze(-1)
                         - torch.tensor(normalize_scores(data["y_va"]), device=dev))
                        ** 2).mean())
        if vm < best_val - 1e-5:
            best_val, bad = vm, 0
            best_state = {k: v.clone() for k, v in net.state_dict().items()}
        else:
            bad += 1
            if bad >= 3:
                break
    net.load_state_dict(best_state)
    with torch.no_grad():
        summary["mlp"] = {"val": score_metrics(net(Xva).squeeze(-1).cpu().numpy(),
                                               data["y_va"]),
                          "test": score_metrics(net(Xte).squeeze(-1).cpu().numpy(),
                                                data["y_te"])}
    print(f"[ceilings] mlp: test R2 {summary['mlp']['test']['r2']:.4f}", flush=True)
    with open(f"{ROOT}/summary_ceilings_w{w}_N{n_train}.json", "w") as fh:
        json.dump(summary, fh, indent=1)
    vol.commit()
    run = _wandb_run(f"ceilings-w{w}-N{n_train}", {"w": w, "n_train": n_train})
    if run is not None:
        run.summary.update({"ridge_test_r2": summary["ridge"]["test"]["r2"],
                            "mlp_test_r2": summary["mlp"]["test"]["r2"]})
        run.finish()
    print(json.dumps(summary), flush=True)
    return summary


@app.function(image=image, volumes={"/cache": vol}, timeout=600)
def show():
    import glob
    vol.reload()
    for path in sorted(glob.glob(f"{ROOT}/summary_*.json")):
        with open(path) as fh:
            print(f"=== {os.path.basename(path)}\n{fh.read()}", flush=True)


@app.local_entrypoint()
def main(stage: str = "fit", encoding: str = "lsh", w: int = W_WIN,
         b: int = B_LSH, max_pairs: int = 400_000, n_train: int = 1_000_000,
         n_val: int = 25_000, n_test: int = 25_000, batch: int = 256,
         deg3_anchors: int = 0, max_triples: int = 200_000):
    if stage == "label":
        print(label.remote(n_train, n_val, n_test, w, batch))
    elif stage == "fit":
        encs = ("lsh", "ctrl", "tokid") if encoding == "all" else (encoding,)
        for enc in encs:
            fit.remote(enc, w, b, max_pairs, n_train, n_val, n_test,
                       512, deg3_anchors, max_triples)
    elif stage == "ceilings":
        ceilings.remote(w, n_train, n_val, n_test)
    elif stage == "show":
        show.remote()
    else:
        raise SystemExit(f"unknown stage {stage}")
