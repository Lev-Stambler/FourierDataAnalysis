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


def fit_token_table(tok_tr, y, q, device=None, wd=1e2, row_chunk=8192):
    """CLOSED-FORM q-ary degree-1 fit: a per-token scalar value summed over
    the window (bag-of-tokens ridge on count features).  This is the FULL
    30522-dim degree-1 basis in TOKEN space -- the bit-deg-1 fit is its
    69-dim shadow and the ridge-on-pooled-embeddings ceiling its 768-dim
    shadow.  One Gram accumulation + one solve; still a calculated student
    (the table is 61KB at fp16).  Returns (v (q,) float32, b float)."""
    import torch
    device = device or ("cuda" if torch.cuda.is_available() else "cpu")
    tok = torch.tensor(np.asarray(tok_tr, np.int64), device=device)
    y_t = torch.tensor(np.asarray(y, np.float32), device=device)
    D, w = tok.shape
    S = torch.zeros((q + 1, q + 1), dtype=torch.float32, device=device)
    bvec = torch.zeros(q + 1, dtype=torch.float32, device=device)
    for lo in range(0, D, row_chunk):
        t = tok[lo:lo + row_chunk]
        ones = torch.ones((len(t), 1), device=device)
        X = torch.zeros((len(t), q), device=device)
        X.scatter_add_(1, t, ones.expand(-1, w))     # token counts per window
        Xa = torch.cat([X, ones], dim=1)
        S += Xa.t() @ Xa
        bvec += Xa.t() @ y_t[lo:lo + row_chunk]
        del X, Xa
    out = []
    wds = sorted(wd) if isinstance(wd, (tuple, list)) else [wd]
    if q > 40_000:
        # 61k^2 solves need ~45GB with GPU copies: CPU LAPACK fp32 with
        # IN-PLACE diagonal regularization (ascending wd, no matrix copies)
        S_c = S.cpu().numpy(); del S
        torch.cuda.empty_cache()
        b_c = bvec.cpu().numpy()
        prev = 0.0
        for wd_i in wds:
            S_c[np.arange(q), np.arange(q)] += (wd_i - prev)
            prev = wd_i
            va = np.linalg.solve(S_c, b_c).astype(np.float32)
            out.append((float(wd_i), va[:-1], float(va[-1])))
    else:
        for wd_i in wds:
            reg = wd_i * torch.eye(q + 1, device=device)
            reg[-1, -1] = 0.0
            va = torch.linalg.solve((S + reg).double(), bvec.double()).float()
            out.append((float(wd_i), va[:-1].cpu().numpy(), float(va[-1])))
    return out if isinstance(wd, (tuple, list)) else (out[0][1], out[0][2])


def token_table_apply(tok, v, b, device=None):
    import torch
    device = device or ("cuda" if torch.cuda.is_available() else "cpu")
    v_t = torch.tensor(np.asarray(v, np.float32), device=device)
    t = torch.tensor(np.asarray(tok, np.int64), device=device)
    return v_t[t].sum(dim=1) + b


@app.function(image=image, gpu="A100-40GB", volumes={"/cache": vol},
              timeout=43200, memory=49152, secrets=WANDB_SECRET)
def fit_token(encoding: str = "lsh", w: int = W_WIN, b: int = B_LSH,
              max_pairs: int = 400_000, n_train: int = 2_000_000,
              n_val: int = 25_000, n_test: int = 25_000, block: int = 512,
              pos_buckets: int = 1):
    """Token-table base (full q-ary deg-1, wd swept on val over closed-form
    solves) + bit-deg-2 pairs deflated on ITS residual.  The match-the-MLP
    push: the ridge ceiling (0.629) is itself a token-deg-1 function."""
    import torch
    vol.reload()
    data = np.load(_data_path(w, n_train, n_val, n_test))
    q0 = int(np.load(f"{ROOT}/emb.npz")["E"].shape[0])
    q = q0 * pos_buckets
    if pos_buckets > 1:
        max_pairs = 0        # augmented ids can't index the bit codes; the
    bucket = (np.arange(w) * pos_buckets // w).astype(np.int64) * q0  # pair
    # stage added ~+0.002 on the token residual anyway

    def aug(tok):                                    # position-bucketed ids:
        return tok.astype(np.int64) + bucket[None, :]  # per-bucket value table

    data = {k: (aug(data[k]) if k.startswith("tok_") else data[k])
            for k in data.files}
    dev = "cuda"
    ytr = normalize_scores(data["y_tr"])
    run = _wandb_run(f"token-{encoding}-w{w}-N{n_train}",
                     {"encoding": encoding, "w": w, "n_train": n_train,
                      "max_pairs": max_pairs})
    cands = fit_token_table(data["tok_tr"], ytr, q,
                            wd=(1.0, 10.0, 100.0, 1000.0))
    best = None
    for wd_i, v, b0 in cands:
        pv = token_table_apply(data["tok_va"], v, b0, dev).cpu().numpy()
        m = score_metrics(pv, data["y_va"])
        print(f"[token] wd {wd_i:g}: val R2 {m['r2']:.4f}", flush=True)
        if best is None or m["mse"] < best[3]["mse"]:
            best = (wd_i, v, b0, m)
    wd_i, v, b0, _ = best
    base = {t: token_table_apply(data[f"tok_{k}"], v, b0, dev)
            for t, k in (("val", "va"), ("test", "te"))}
    summary = {"wd": wd_i, "q": q, "n_train": n_train, "encoding": encoding,
               "token_deg1": {t: score_metrics(base[t].cpu().numpy(),
                                               data[f"y_{k}"])
                              for t, k in (("val", "va"), ("test", "te"))}}
    print(f"[token] === token-deg1: val R2 "
          f"{summary['token_deg1']['val']['r2']:.4f} test "
          f"{summary['token_deg1']['test']['r2']:.4f}", flush=True)
    if run is not None:
        run.log({"pairs": 0,
                 **{f"{t}_r2": summary["token_deg1"][t]["r2"]
                    for t in ("val", "test")}})
    # bit-deg-2 pairs on the token residual
    codes = build_codes(encoding, b)
    bits_t = torch.tensor(window_bits(data["tok_tr"], codes), device=dev)
    D, n = bits_t.shape
    g = torch.tensor(ytr, device=dev) \
        - token_table_apply(data["tok_tr"], v, b0, dev)
    res1 = float((g ** 2).mean())
    print(f"[token] residual mass {res1:.4f}", flush=True)
    psi2, _ = deg2_psi_scalar(bits_t, g, device=dev)
    floor = res1 / D
    iu = np.triu_indices(n, k=1)
    vals = psi2[iu]
    order = np.argsort(-vals)
    K = int(min((vals > 2.0 * floor).sum(), max_pairs))
    idx = np.full((K, 4), -1, np.int16)
    idx[:, 0] = iu[0][order[:K]]
    idx[:, 1] = iu[1][order[:K]]
    print(f"[token] floor {floor:.3e}; fitting K={K} pairs", flush=True)
    ladder = []
    C = np.zeros(0, np.float32)
    if K:
        C, g = sequential_deflate(bits_t, g, idx, device=dev, block=block)
        eb = {t: torch.tensor(window_bits(data[f"tok_{k}"], codes), device=dev)
              for t, k in (("val", "va"), ("test", "te"))}
        adds = {t: torch.zeros(len(eb[t]), device=dev) for t in eb}
        C_t = torch.tensor(C, device=dev)
        lo = 0
        for k in sorted({x for x in (1_000, 10_000, 50_000, 200_000)
                         if x < K} | {K}):
            for blo in range(lo, k, 4096):
                hi = min(blo + 4096, k)
                for t in eb:
                    adds[t] += xor_parity_features(eb[t], idx[blo:hi]) \
                        @ C_t[blo:hi]
            lo = k
            entry = {"k": int(k)}
            for t, dk in (("val", "va"), ("test", "te")):
                entry[t] = score_metrics((base[t] + adds[t]).cpu().numpy(),
                                         data[f"y_{dk}"])
            ladder.append(entry)
            print(f"[token] === +{k} pairs: val R2 {entry['val']['r2']:.4f} "
                  f"test R2 {entry['test']['r2']:.4f}", flush=True)
            if run is not None:
                run.log({"pairs": k, **{f"{t}_r2": entry[t]["r2"]
                                        for t in ("val", "test")}})
    summary["K"] = K
    summary["ladder"] = ladder
    suf = "" if b == B_LSH else f"_b{b}"
    np.savez_compressed(f"{ROOT}/model_token_{encoding}_w{w}_N{n_train}{suf}.npz",
                        v=v.astype(np.float16), b0=b0, codes=codes, idx=idx, C=C)
    with open(f"{ROOT}/summary_token_{encoding}_w{w}_N{n_train}{suf}.json",
              "w") as fh:
        json.dump(summary, fh, indent=1)
    vol.commit()
    if run is not None:
        run.summary.update({"token_deg1_test_r2":
                            summary["token_deg1"]["test"]["r2"],
                            "best_test_r2": max(
                                [e["test"]["r2"] for e in ladder]
                                + [summary["token_deg1"]["test"]["r2"]])})
        run.finish()
    print(json.dumps(summary), flush=True)
    return summary


def _ngram_ids(tok, q, orders=(1, 2), n_hash=1 << 22, seed=0):
    """Per-window sparse feature ids: unigrams (raw ids) + hashed ADJACENT
    n-grams (order >= 2) -- degree-2+ in TOKEN space with a locality prior
    (63 adjacent pairs per window, not 30k^2).  Returns ((D, F) int64 ids
    into a value table, table_size)."""
    tok = np.asarray(tok, np.uint64)
    mix = np.uint64(0x9E3779B97F4A7C15) + np.uint64(seed)
    cols = [tok.astype(np.int64)] if 1 in orders else []
    base = q
    w = tok.shape[1]
    for r in sorted(o for o in orders if o >= 2):
        h = tok[:, : w - r + 1].copy()
        for j in range(1, r):
            h = (h * mix) ^ tok[:, j: w - r + 1 + j]
        h = (((h * mix) >> np.uint64(40)) % np.uint64(n_hash)).astype(np.int64)
        cols.append(h + base)
        base += n_hash
    return np.concatenate(cols, axis=1), int(base)


def fit_ngram_cg(ids_tr, y, n_feat, wd=10.0, iters=200, device=None,
                 row_chunk=131072, tol=1e-8):
    """CALCULATED n-gram value table at any width: ridge normal equations
    solved by conjugate gradient -- each iteration is two sparse gather/
    scatter passes, no dense Gram.  One linear solve, no SGD."""
    import torch
    device = device or ("cuda" if torch.cuda.is_available() else "cpu")
    ids = torch.tensor(np.asarray(ids_tr, np.int64), device=device)
    y_t = torch.tensor(np.asarray(y, np.float32), device=device)
    D, F = ids.shape

    def AtA(v):                                      # (X^T X + wd I) v
        out = wd * v
        for lo in range(0, D, row_chunk):
            sl = ids[lo:lo + row_chunk]
            xv = v[sl].sum(dim=1)                    # X v on the chunk
            out.scatter_add_(0, sl.reshape(-1),
                             xv[:, None].expand(-1, F).reshape(-1))
        return out

    b = torch.zeros(n_feat, device=device)
    for lo in range(0, D, row_chunk):
        sl = ids[lo:lo + row_chunk]
        b.scatter_add_(0, sl.reshape(-1),
                       y_t[lo:lo + row_chunk, None].expand(-1, F).reshape(-1))
    v = torch.zeros(n_feat, device=device)
    r = b.clone()
    p = r.clone()
    rs = float(r @ r)
    rs0 = rs
    for it in range(iters):
        Ap = AtA(p)
        alpha = rs / float(p @ Ap)
        v += alpha * p
        r -= alpha * Ap
        rs_new = float(r @ r)
        if it % 20 == 0:
            print(f"[cg] iter {it} rel-residual {rs_new / rs0:.3e}", flush=True)
        if rs_new < tol * rs0 or rs_new != rs_new:
            break
        p = r + (rs_new / rs) * p
        rs = rs_new
    return v.cpu().numpy()


def ngram_apply(ids, v, device=None):
    import torch
    device = device or ("cuda" if torch.cuda.is_available() else "cpu")
    v_t = torch.tensor(np.asarray(v, np.float32), device=device)
    ids_t = torch.tensor(np.asarray(ids, np.int64), device=device)
    return v_t[ids_t].sum(dim=1)


@app.function(image=image, gpu="A100-40GB", volumes={"/cache": vol},
              timeout=43200, memory=49152, secrets=WANDB_SECRET)
def fit_ngram(orders: str = "1,2", n_hash: int = 1 << 22,
              n_train: int = 2_000_000, n_val: int = 25_000,
              n_test: int = 25_000, w: int = W_WIN, iters: int = 300):
    """Unigram + hashed adjacent n-gram value tables, CG-solved ridge with
    wd swept on val.  The word-ORDER rung of the calculated ladder."""
    vol.reload()
    data = np.load(_data_path(w, n_train, n_val, n_test))
    q = int(np.load(f"{ROOT}/emb.npz")["E"].shape[0])
    ords = tuple(int(x) for x in orders.split(","))
    ids_tr, n_feat = _ngram_ids(data["tok_tr"], q, ords, n_hash)
    ids_va, _ = _ngram_ids(data["tok_va"], q, ords, n_hash)
    ids_te, _ = _ngram_ids(data["tok_te"], q, ords, n_hash)
    ytr = normalize_scores(data["y_tr"])
    run = _wandb_run(f"ngram-{orders.replace(',', '')}-N{n_train}",
                     {"orders": orders, "n_hash": n_hash, "n_feat": n_feat,
                      "n_train": n_train})
    best = None
    for wd in (3.0, 10.0, 30.0):
        v = fit_ngram_cg(ids_tr, ytr - ytr.mean(), n_feat, wd=wd, iters=iters)
        m = score_metrics(ngram_apply(ids_va, v).cpu().numpy() + ytr.mean(),
                          data["y_va"])
        print(f"[ngram] wd {wd:g}: val R2 {m['r2']:.4f}", flush=True)
        if run is not None:
            run.log({"wd": wd, "val_r2": m["r2"]})
        if best is None or m["mse"] < best[2]["mse"]:
            best = (wd, v, m)
    wd, v, _ = best
    summary = {"orders": orders, "n_hash": n_hash, "n_feat": n_feat,
               "wd": wd, "n_train": n_train,
               "table_bytes": int(n_feat * 2)}
    for t, k, ids_e in (("val", "va", ids_va), ("test", "te", ids_te)):
        summary[t] = score_metrics(
            ngram_apply(ids_e, v).cpu().numpy() + ytr.mean(), data[f"y_{k}"])
    print(f"[ngram] === orders {orders}: val R2 {summary['val']['r2']:.4f} "
          f"test R2 {summary['test']['r2']:.4f} sp "
          f"{summary['test']['spearman']:.3f} ({n_feat * 2 / 1e6:.1f}MB)",
          flush=True)
    np.savez_compressed(
        f"{ROOT}/model_ngram_{orders.replace(',', '')}_N{n_train}.npz",
        v=v.astype(np.float16), mean=ytr.mean(), n_hash=n_hash, orders=ords)
    with open(f"{ROOT}/summary_ngram_{orders.replace(',', '')}_N{n_train}.json",
              "w") as fh:
        json.dump(summary, fh, indent=1)
    vol.commit()
    if run is not None:
        run.summary.update({"test_r2": summary["test"]["r2"],
                            "val_r2": summary["val"]["r2"]})
        run.finish()
    print(json.dumps(summary), flush=True)
    return summary


def _pair_key(tok, i, j, q, n_hash):
    """Hashed cell id of the ORDERED token pair at positions (i, j)."""
    h = (np.asarray(tok[:, i], np.uint64) * np.uint64(q)
         + np.asarray(tok[:, j], np.uint64))
    h = h * np.uint64(0x9E3779B97F4A7C15)
    return ((h >> np.uint64(40)) % np.uint64(n_hash)).astype(np.int64)


def _cell_table(keys, g, n_hash, lam=10.0, device=None):
    """Closed-form ridge cell means: v[c] = sum_g[c] / (n[c] + lam)."""
    import torch
    device = device or ("cuda" if torch.cuda.is_available() else "cpu")
    k = torch.tensor(keys, device=device)
    gs = torch.zeros(n_hash, device=device).scatter_add_(0, k, g)
    ns = torch.zeros(n_hash, device=device).scatter_add_(
        0, k, torch.ones_like(g))
    return gs / (ns + lam)


@app.function(image=image, gpu="A100-40GB", volumes={"/cache": vol},
              timeout=43200, memory=49152, secrets=WANDB_SECRET)
def fit_qpair(n_train: int = 2_000_000, n_val: int = 25_000,
              n_test: int = 25_000, w: int = W_WIN, top_pairs: int = 64,
              n_hash: int = 1 << 22, lam: float = 10.0):
    """ARBITRARY-support q-ary degree-2 DISCOVERY, fully calculated: score
    every position pair (i, j) by the val gain of its exact (ridge-shrunk)
    conditional-mean table on the unigram residual, then greedily deflate
    the top discovered pairs.  No adjacency prior -- if word order matters,
    the search must FIND it.  The hand-designed bigram table (0.767) is the
    baseline this must match with learned support."""
    import torch
    vol.reload()
    data = np.load(_data_path(w, n_train, n_val, n_test))
    tok = {k: np.ascontiguousarray(data[f"tok_{k}"])   # npz member access
           for k in ("tr", "va", "te")}                # re-reads from disk
    q = int(np.load(f"{ROOT}/emb.npz")["E"].shape[0])  # EVERY time -- hoist
    dev = "cuda"
    run = _wandb_run(f"qpair-N{n_train}", {"top_pairs": top_pairs,
                                           "n_hash": n_hash, "lam": lam})
    yv = {"tr": normalize_scores(data["y_tr"]),
          "va": normalize_scores(data["y_va"]),
          "te": normalize_scores(data["y_te"])}
    v_uni, b0 = fit_token_table(tok["tr"], yv["tr"], q, wd=10.0)
    g = {k: torch.tensor(yv[k], device=dev)
         - token_table_apply(tok[k], v_uni, b0, dev)
         for k in ("tr", "va", "te")}
    base_r2 = {k: 1.0 - float((g[k] ** 2).mean()) / float(np.var(yv[k]))
               for k in ("va", "te")}
    print(f"[qpair] unigram base: val R2 {base_r2['va']:.4f} test "
          f"{base_r2['te']:.4f}", flush=True)
    scores = []
    for i in range(w):
        for j in range(i + 1, w):
            kt = _pair_key(tok["tr"], i, j, q, n_hash)
            v = _cell_table(kt, g["tr"], n_hash, lam, dev)
            kv = torch.tensor(_pair_key(tok["va"], i, j, q, n_hash),
                              device=dev)
            gain = float((g["va"] ** 2).mean()) \
                - float(((g["va"] - v[kv]) ** 2).mean())
            scores.append((gain, i, j))
    scores.sort(reverse=True)
    print("[qpair] top-15 discovered pairs (gain, i, j): "
          + str([(round(s, 5), i, j) for s, i, j in scores[:15]]), flush=True)
    adj = sum(1 for _, i, j in scores[:top_pairs] if j == i + 1)
    print(f"[qpair] adjacency among top-{top_pairs}: {adj}", flush=True)
    tables = []
    ladder = []
    for rank, (s0, i, j) in enumerate(scores[:top_pairs]):
        kt = _pair_key(tok["tr"], i, j, q, n_hash)
        v = _cell_table(kt, g["tr"], n_hash, lam, dev)
        g["tr"] -= v[torch.tensor(kt, device=dev)]
        for k in ("va", "te"):
            kk = torch.tensor(_pair_key(tok[k], i, j, q, n_hash), device=dev)
            g[k] -= v[kk]
        tables.append((i, j, v.cpu().numpy().astype(np.float16)))
        if (rank + 1) in (8, 16, 32, 64, top_pairs):
            entry = {"k": rank + 1}
            for k, tag in (("va", "val"), ("te", "test")):
                pred = torch.tensor(yv[k], device=dev) - g[k]
                entry[tag] = score_metrics(pred.cpu().numpy(),
                                           data[f"y_{k}"])
            ladder.append(entry)
            print(f"[qpair] === {rank + 1} discovered pairs: val R2 "
                  f"{entry['val']['r2']:.4f} test {entry['test']['r2']:.4f}",
                  flush=True)
            if run is not None:
                run.log({"pairs": rank + 1, "val_r2": entry["val"]["r2"],
                         "test_r2": entry["test"]["r2"]})
    summary = {"n_train": n_train, "top_pairs": top_pairs, "n_hash": n_hash,
               "lam": lam, "unigram_base": base_r2,
               "adjacent_in_top": adj,
               "top20": [(round(s, 5), i, j) for s, i, j in scores[:20]],
               "ladder": ladder}
    np.savez_compressed(f"{ROOT}/model_qpair_N{n_train}.npz",
                        v_uni=v_uni.astype(np.float16), b0=b0,
                        pair_ij=np.array([(i, j) for i, j, _ in tables],
                                         np.int16),
                        tables=np.stack([t for _, _, t in tables]))
    with open(f"{ROOT}/summary_qpair_N{n_train}.json", "w") as fh:
        json.dump(summary, fh, indent=1)
    vol.commit()
    if run is not None:
        run.summary.update({"best_test_r2":
                            max(e["test"]["r2"] for e in ladder)})
        run.finish()
    print(json.dumps(summary), flush=True)
    return summary




def _tuple_key(tok, pos, q, n_hash):
    """Hashed cell id of the ordered token tuple at positions ``pos``."""
    mix = np.uint64(0x9E3779B97F4A7C15)
    h = np.asarray(tok[:, pos[0]], np.uint64)
    for p in pos[1:]:
        h = (h * mix) ^ np.asarray(tok[:, p], np.uint64)
    h = h * mix
    return ((h >> np.uint64(40)) % np.uint64(n_hash)).astype(np.int64)


@app.function(image=image, gpu="A100-40GB", volumes={"/cache": vol},
              timeout=43200, memory=49152, secrets=WANDB_SECRET)
def fit_qhh(n_train: int = 2_000_000, n_val: int = 25_000,
            n_test: int = 25_000, w: int = W_WIN, rounds: int = 24,
            n_hash: int = 1 << 22, lam: float = 10.0,
            min_gain: float = 1e-4):
    """Iterated HEAVY-HITTER discovery, v2.  Candidates per round:
    (a) OFFSET-POOLED pair tables (one shared token-pair table per offset
        d = j - i, pooled over all positions -- translation invariance is a
        CANDIDATE that must win, not an assumption; pooling is what makes
        q^2 cells estimable: v1's per-position tables saw each cell ~once
        and deflated noise, test 0.699 -> 0.692),
    (b) raw position pairs (win when position matters),
    (c) offset-pattern triples anchored on selected offsets.
    Each candidate table is closed-form shrunk cell means with a 1-D
    calculated rescale alpha = <pred,g>/<pred,pred> (fixes additive
    overcounting of pooled sums).  Greedy top-1 per round, val-gated."""
    import torch
    vol.reload()
    data = np.load(_data_path(w, n_train, n_val, n_test))
    tok = {k: np.ascontiguousarray(data[f"tok_{k}"]).astype(np.int64)
           for k in ("tr", "va", "te")}
    q = int(np.load(f"{ROOT}/emb.npz")["E"].shape[0])
    dev = "cuda"
    run = _wandb_run(f"qhh2-N{n_train}", {"rounds": rounds, "n_hash": n_hash,
                                          "lam": lam})
    yv = {k: normalize_scores(data[f"y_{k}"]) for k in ("tr", "va", "te")}
    v_uni, b0 = fit_token_table(tok["tr"], yv["tr"], q, wd=10.0)
    g = {k: torch.tensor(yv[k], device=dev)
         - token_table_apply(tok[k], v_uni, b0, dev)
         for k in ("tr", "va", "te")}

    def r2(k):
        return 1.0 - float((g[k] ** 2).mean()) / float(np.var(yv[k]))

    print(f"[qhh] unigram base: val {r2('va'):.4f} test {r2('te'):.4f}",
          flush=True)
    MIX = -7046029254386353131                       # 0x9E3779B97F4A7C15 as
    tokt = {k: torch.tensor(tok[k], device=dev)      # wrapped int64
            for k in ("tr", "va", "te")}

    def _finish(h):
        # logical top bits of the wrapped product, sign-safe: arithmetic
        # shift then mask; n_hash is a power of two
        return ((h * MIX) >> 40) & (n_hash - 1)

    def cand_keys(cand, split):
        """Cell ids (D, P) on GPU: ('off', d) pooled pairs, ('pos', i, j)
        one pair, ('off3', d1, d2) pooled triples.  numpy hashing was
        60-80ms/candidate -- 15+min rounds; GPU is ~1ms."""
        t = tokt[split]
        if cand[0] == "pos":
            _, i, j = cand
            return _finish(t[:, i] * q + t[:, j])[:, None]
        if cand[0] == "off":
            d = cand[1]
            return _finish(t[:, : w - d] * q + t[:, d:])
        d1, d2 = cand[1], cand[2]
        span = d1 + d2
        return _finish(((t[:, : w - span] * MIX) ^ t[:, d1: w - d2]) * MIX
                       ^ t[:, span:])

    def fit_apply(cand, splits=("tr", "va", "te")):
        """Closed-form table + alpha on train; returns per-split preds."""
        kt = cand_keys(cand, "tr")
        P = kt.shape[1]
        flat = kt.reshape(-1)
        gs = torch.zeros(n_hash, device=dev).scatter_add_(
            0, flat, g["tr"].repeat_interleave(P))
        ns = torch.zeros(n_hash, device=dev).scatter_add_(
            0, flat, torch.ones(len(flat), device=dev))
        v = gs / (ns + lam)
        preds = {s: v[cand_keys(cand, s)].sum(dim=1) for s in splits}
        num = float(preds["tr"] @ g["tr"])
        den = float(preds["tr"] @ preds["tr"]) + 1e-12
        alpha = num / den
        return v, alpha, {s: alpha * p for s, p in preds.items()}

    selected = []
    tables = []
    best_state = (r2("va"), 0)
    for rnd in range(rounds):
        cands = [("off", d) for d in range(1, w)]
        cands += [("pos", i, j) for i in range(w) for j in range(i + 1, w)]
        for s in selected:
            if s[0] == "off":
                for d2 in range(1, w - s[1]):
                    c3 = ("off3", s[1], d2)
                    if c3 not in selected:
                        cands.append(c3)
        cands = [c for c in dict.fromkeys(cands) if c not in selected]
        vm = float((g["va"] ** 2).mean())
        best = None
        for cand in cands:
            _, _, preds = fit_apply(cand, splits=("tr", "va"))
            gain = vm - float(((g["va"] - preds["va"]) ** 2).mean())
            if best is None or gain > best[0]:
                best = (gain, cand)
        gain, cand = best
        if gain < min_gain:
            print(f"[qhh] round {rnd}: dry (best {cand} gain {gain:.2e})",
                  flush=True)
            break
        v, alpha, preds = fit_apply(cand)
        for k in ("tr", "va", "te"):
            g[k] -= preds[k]
        selected.append(cand)
        tables.append((cand, alpha, v.cpu().numpy().astype(np.float16)))
        print(f"[qhh] === round {rnd}: {cand} gain {gain:.2e} alpha "
              f"{alpha:.3f}; val {r2('va'):.4f} test {r2('te'):.4f}",
              flush=True)
        if run is not None:
            run.log({"round": rnd, "val_r2": r2("va"), "test_r2": r2("te"),
                     "gain": gain})
    nz = sum(int((np.abs(t) > 1e-3).sum()) for _, _, t in tables)
    summary = {"n_train": n_train, "n_supports": len(selected),
               "supports": [list(map(str, s)) for s in selected],
               "nz_cells": nz, "sparse_bytes": int(q * 2 + nz * 10),
               "final": {}}
    for k, tag in (("va", "val"), ("te", "test")):
        pred = torch.tensor(yv[k], device=dev) - g[k]
        summary["final"][tag] = score_metrics(pred.cpu().numpy(),
                                              data[f"y_{k}"])
    print(f"[qhh] FINAL: val {summary['final']['val']['r2']:.4f} test "
          f"{summary['final']['test']['r2']:.4f}; {len(selected)} supports, "
          f"{nz} heavy cells ~ {summary['sparse_bytes'] / 1e6:.1f}MB",
          flush=True)
    with open(f"{ROOT}/summary_qhh_N{n_train}.json", "w") as fh:
        json.dump(summary, fh, indent=1)
    vol.commit()
    if run is not None:
        run.summary.update({"test_r2": summary["final"]["test"]["r2"]})
        run.finish()
    print(json.dumps(summary), flush=True)
    return summary




@app.function(image=image, gpu="A100-40GB", volumes={"/cache": vol},
              timeout=43200, memory=49152, secrets=WANDB_SECRET)
def fit_qjoint(offsets: str = "1,2,3,4", n_hash: int = 1 << 22,
               n_train: int = 2_000_000, n_val: int = 25_000,
               n_test: int = 25_000, w: int = W_WIN, iters: int = 300):
    """JOINT CG ridge over the DISCOVERED supports (unigrams + the offset
    tables qhh selected), then a heavy-cell truncation sweep: sort all cells
    by |v|, zero below top-K, chart the calculated R2-vs-size frontier."""
    import torch
    vol.reload()
    data = np.load(_data_path(w, n_train, n_val, n_test))
    tok = {k: np.ascontiguousarray(data[f"tok_{k}"]).astype(np.int64)
           for k in ("tr", "va", "te")}
    q = int(np.load(f"{ROOT}/emb.npz")["E"].shape[0])
    offs = [int(x) for x in offsets.split(",")]
    run = _wandb_run(f"qjoint-{offsets.replace(',', '')}-N{n_train}",
                     {"offsets": offsets, "n_hash": n_hash})
    mixi = -7046029254386353131

    def ids_of(t):
        cols = [t]                                   # unigrams: raw ids
        base = q
        for d in offs:
            h = ((t[:, : w - d] * q + t[:, d:]) * mixi >> 40) & (n_hash - 1)
            cols.append(h + base)
            base += n_hash
        return np.concatenate(cols, axis=1), base

    ids = {}
    for k in ("tr", "va", "te"):
        ids[k], n_feat = ids_of(tok[k])
    ytr = normalize_scores(data["y_tr"])
    best = None
    for wd in (30.0, 100.0, 300.0):
        v = fit_ngram_cg(ids["tr"], ytr - ytr.mean(), n_feat, wd=wd,
                         iters=iters)
        m = score_metrics(ngram_apply(ids["va"], v).cpu().numpy()
                          + ytr.mean(), data["y_va"])
        print(f"[qjoint] wd {wd:g}: val R2 {m['r2']:.4f}", flush=True)
        if best is None or m["mse"] < best[2]["mse"]:
            best = (wd, v, m)
    wd, v, _ = best
    summary = {"offsets": offsets, "wd": wd, "n_feat": int(n_feat),
               "n_train": n_train, "full": {}, "trunc": []}
    for k, tag in (("va", "val"), ("te", "test")):
        summary["full"][tag] = score_metrics(
            ngram_apply(ids[k], v).cpu().numpy() + ytr.mean(),
            data[f"y_{k}"])
    print(f"[qjoint] === full: val {summary['full']['val']['r2']:.4f} test "
          f"{summary['full']['test']['r2']:.4f}", flush=True)
    order = np.argsort(-np.abs(v))
    for keep in (100_000, 400_000, 1_600_000, 6_400_000):
        if keep >= n_feat:
            continue
        vt = np.zeros_like(v)
        vt[order[:keep]] = v[order[:keep]]
        entry = {"cells": keep, "mb": round(keep * 6 / 1e6, 2)}
        for k, tag in (("va", "val"), ("te", "test")):
            entry[tag] = score_metrics(
                ngram_apply(ids[k], vt).cpu().numpy() + ytr.mean(),
                data[f"y_{k}"])["r2"]
        summary["trunc"].append(entry)
        print(f"[qjoint] top-{keep} cells (~{entry['mb']}MB): val "
              f"{entry['val']:.4f} test {entry['test']:.4f}", flush=True)
        if run is not None:
            run.log({"cells": keep, "test_r2": entry["test"]})
    np.savez_compressed(f"{ROOT}/model_qjoint_N{n_train}.npz",
                        v=v.astype(np.float16), mean=ytr.mean(),
                        offsets=np.array(offs), n_hash=n_hash)
    with open(f"{ROOT}/summary_qjoint_N{n_train}.json", "w") as fh:
        json.dump(summary, fh, indent=1)
    vol.commit()
    if run is not None:
        run.summary.update({"test_r2": summary["full"]["test"]["r2"]})
        run.finish()
    print(json.dumps(summary), flush=True)
    return summary





@app.function(image=image, gpu="A100-40GB", volumes={"/cache": vol},
              timeout=43200, memory=98304, secrets=WANDB_SECRET)
def fit_qfull(n_hash: int = 1 << 22, n_train: int = 2_000_000,
              n_val: int = 25_000, n_test: int = 25_000, w: int = W_WIN,
              iters: int = 600, wds: str = "30,100,300"):
    """FULL joint solve over unigrams + ALL 63 offset-pooled pair tables
    with EMPIRICAL-BAYES per-group ridge: each table's shrinkage scales
    inversely with its AUTO-MEASURED solo val gain (no hand-fed constants).
    Jacobi-preconditioned CG, fp64 dots.  Scalar-wd all-in ridge: 0.748;
    selected-4: 0.772; evidence-weighted all-in: 0.782."""
    import torch
    vol.reload()
    data = np.load(_data_path(w, n_train, n_val, n_test))
    tok = {k: np.ascontiguousarray(data[f"tok_{k}"]).astype(np.int64)
           for k in ("tr", "va", "te")}
    q = int(np.load(f"{ROOT}/emb.npz")["E"].shape[0])
    dev = "cuda"
    run = _wandb_run(f"qfull-N{n_train}", {"n_hash": n_hash, "iters": iters})
    mixi = -7046029254386353131
    ytr = normalize_scores(data["y_tr"])
    yva = normalize_scores(data["y_va"])
    y_t = torch.tensor(ytr - ytr.mean(), device=dev)

    # ---- phase 1 (small memory): auto-measure per-offset solo gains
    v0 = fit_token_table(tok["tr"], ytr, q, wd=10.0)
    g_tr0 = torch.tensor(ytr, device=dev) \
        - token_table_apply(tok["tr"], v0[0], v0[1], dev)
    g_va0 = torch.tensor(yva, device=dev) \
        - token_table_apply(tok["va"], v0[0], v0[1], dev)
    tok_tr_t = torch.tensor(tok["tr"], device=dev)
    tok_va_t = torch.tensor(tok["va"], device=dev)
    gains = {}
    vm0 = float((g_va0 ** 2).mean())
    for d in range(1, w):
        ktr = ((tok_tr_t[:, : w - d] * q + tok_tr_t[:, d:])
               * mixi >> 40) & (n_hash - 1)
        gs = torch.zeros(n_hash, device=dev).scatter_add_(
            0, ktr.reshape(-1), g_tr0.repeat_interleave(w - d))
        ns = torch.zeros(n_hash, device=dev).scatter_add_(
            0, ktr.reshape(-1), torch.ones(ktr.numel(), device=dev))
        vtab = gs / (ns + 10.0)
        ptr = vtab[ktr].sum(dim=1)
        alpha = float(ptr @ g_tr0) / (float(ptr @ ptr) + 1e-12)
        kva = ((tok_va_t[:, : w - d] * q + tok_va_t[:, d:])
               * mixi >> 40) & (n_hash - 1)
        pred = vtab[kva].sum(dim=1)
        gains[d] = max(vm0 - float(((g_va0 - alpha * pred) ** 2).mean()),
                       1e-7)
        del ktr, gs, ns, vtab, ptr, kva, pred
        torch.cuda.empty_cache()
    del tok_tr_t, tok_va_t, g_tr0, g_va0
    torch.cuda.empty_cache()
    gmax = max(gains.values())
    print("[qfull] measured gains (top 8): "
          + str(sorted(((round(v, 6), d) for d, v in gains.items()),
                       reverse=True)[:8]), flush=True)

    # ---- phase 2: full joint solve with evidence-weighted diagonal ridge
    def ids_of(t):
        cols = [t]
        base = q
        for d in range(1, w):
            h = ((t[:, : w - d] * q + t[:, d:]) * mixi >> 40) & (n_hash - 1)
            cols.append(h + base)
            base += n_hash
        return np.concatenate(cols, axis=1).astype(np.int32), base

    ids = {}
    for k in ("tr", "va", "te"):
        ids[k], n_feat = ids_of(tok[k])
    ids_t = torch.tensor(ids["tr"], device=dev)      # int32: 16.6GB
    del ids["tr"]
    D, F = ids_t.shape
    chunk = 65536

    def scatter_rows(vec_per_row):
        out = torch.zeros(n_feat, device=dev)
        for lo in range(0, D, chunk):
            sl = ids_t[lo:lo + chunk].long()
            out.scatter_add_(0, sl.reshape(-1),
                             vec_per_row[lo:lo + chunk, None]
                             .expand(-1, F).reshape(-1))
        return out

    def Xv(v):
        out = torch.empty(D, device=dev)
        for lo in range(0, D, chunk):
            out[lo:lo + chunk] = v[ids_t[lo:lo + chunk].long()].sum(dim=1)
        return out

    counts = scatter_rows(torch.ones(D, device=dev))
    b = scatter_rows(y_t)
    reg = torch.empty(n_feat, device=dev)
    reg[:q] = 1.0
    base_i = q
    for d in range(1, w):
        reg[base_i: base_i + n_hash] = gmax / gains[d]
        base_i += n_hash
    summary = {"n_feat": int(n_feat), "n_train": n_train,
               "gains": {str(d): gains[d] for d in gains}, "full": {},
               "trunc": []}
    best = None
    for wd in (float(x) for x in wds.split(",")):
        wdv = wd * reg
        Minv = 1.0 / (counts + wdv)
        v = torch.zeros(n_feat, device=dev)
        r = b.clone()
        z = Minv * r
        p = z.clone()
        rz = float(r.double() @ z.double())
        rz0 = rz
        for it in range(iters):
            Ap = scatter_rows(Xv(p)) + wdv * p
            alpha = rz / float(p.double() @ Ap.double())
            v += alpha * p
            r -= alpha * Ap
            z = Minv * r
            rz_new = float(r.double() @ z.double())
            if it % 100 == 0:
                print(f"[qfull] wd {wd:g} iter {it} rel {rz_new / rz0:.3e}",
                      flush=True)
            if rz_new < 1e-10 * rz0:
                break
            p = z + (rz_new / rz) * p
            rz = rz_new
        m = {}
        for k, tag in (("va", "val"), ("te", "test")):
            kk = torch.tensor(ids[k].astype(np.int64), device=dev)
            pred = v[kk].sum(dim=1).cpu().numpy() + ytr.mean()
            m[tag] = score_metrics(pred, data[f"y_{k}"])
            del kk
        print(f"[qfull] === wd {wd:g}: val {m['val']['r2']:.4f} test "
              f"{m['test']['r2']:.4f}", flush=True)
        if run is not None:
            run.log({"wd": wd, "val_r2": m["val"]["r2"],
                     "test_r2": m["test"]["r2"]})
        if best is None or m["val"]["mse"] < best[2]["val"]["mse"]:
            best = (wd, v.cpu(), m)
        del v, r, z, p
        torch.cuda.empty_cache()
    wd, v_cpu, m = best
    summary["wd"] = wd
    summary["full"] = {t: m[t] for t in ("val", "test")}
    v = v_cpu.to(dev)
    order = torch.argsort(-v.abs()).cpu().numpy()
    for keep in (400_000, 1_600_000, 6_400_000, 25_600_000):
        if keep >= n_feat:
            continue
        vt = torch.zeros_like(v)
        sel = torch.tensor(order[:keep].copy(), device=dev)
        vt[sel] = v[sel]
        entry = {"cells": keep, "mb": round(keep * 6 / 1e6, 2)}
        for k, tag in (("va", "val"), ("te", "test")):
            kk = torch.tensor(ids[k].astype(np.int64), device=dev)
            pred = vt[kk].sum(dim=1).cpu().numpy() + ytr.mean()
            entry[tag] = score_metrics(pred, data[f"y_{k}"])["r2"]
            del kk
        summary["trunc"].append(entry)
        print(f"[qfull] top-{keep} (~{entry['mb']}MB): val "
              f"{entry['val']:.4f} test {entry['test']:.4f}", flush=True)
    with open(f"{ROOT}/summary_qfull_N{n_train}.json", "w") as fh:
        json.dump(summary, fh, indent=1)
    vol.commit()
    if run is not None:
        run.summary.update({"test_r2": summary["full"]["test"]["r2"]})
        run.finish()
    print(json.dumps(summary), flush=True)
    return summary




def slot_forward(feat_batch, theta, Z, eps=1e-3):
    """Fourier SLOT forward: Phi_s(x) = prod over OPEN gates p of
    u_s(x_p), u = tanh(feat . z_s).  Value = EXACT hard product (already
    differentiable in Z/A); only the discrete gates need STE -- gradient to
    theta flows through the log-magnitude surrogate computed on u.detach()
    so the surrogate never double-feeds Z.  feat_batch (B, w, r);
    theta (S, w); Z (S, r).  Returns (B, S)."""
    import torch
    # 1/sqrt(r) scaling: unscaled pre-activations have std ~ sqrt(r) = 8,
    # |tanh| saturates to 1 - 1e-28, log|u| = 0 in fp32 -> theta gradient
    # IDENTICALLY zero (observed theta_grad_mean = 0.0).  Scaled, |u| ~ 0.6:
    # expressive AND carries gate signal
    u = torch.tanh(torch.einsum("bwr,sr->bws", feat_batch, Z)
                   / feat_batch.shape[-1] ** 0.5)
    mask = (theta > 0).t()                           # (w, S) hard gates
    hard = torch.where(mask.unsqueeze(0), u, torch.ones_like(u))
    phi_hard = hard.prod(dim=1)                      # (B, S) exact value
    # gate-surrogate path in fp32 ALWAYS: sigma'(8)*log|u| ~ 1e-4 underflows
    # in bf16 autocast -> theta grads exactly 0 -> frozen supports (observed:
    # mean_degree pinned to 4 decimals while val flatlined at 0.59)
    with torch.amp.autocast(device_type=u.device.type, enabled=False):
        uf = u.detach().float()
        g = torch.sigmoid(theta.float()).t()         # (w, S) soft gates
        logmag = torch.einsum(
            "bws,ws->bs", torch.log(torch.clamp(uf.abs(), min=eps)), g)
        soft = (torch.sign(phi_hard).detach().float()
                * torch.exp(logmag))
        out = phi_hard.float() + (soft - soft.detach())
    return out


def slot_forward_phase(feat_batch, theta, Z, psi):
    """PHASE slots -- the true character form computed in LOG SPACE: a
    character is prod_p e^{i phi_p}; its log is i*sum(phi), so the slot is
    cos( sum_p sigma(theta_p) * <feat_p, z_s>/sqrt(r) + psi_s ).  Unit
    modulus at ANY degree: no magnitude decay, no STE (gates are smooth
    phase multipliers), gradients O(0.1-1).  feat (B, w, r); theta (S, w);
    Z (S, r); psi (S,).  Returns (B, S)."""
    import torch
    phase = torch.einsum("bwr,sr->bws", feat_batch,
                         Z) / feat_batch.shape[-1] ** 0.5
    with torch.amp.autocast(device_type=phase.device.type, enabled=False):
        g = torch.sigmoid(theta.float()).t()         # (w, S) soft gates
        total = torch.einsum("bws,ws->bs", phase.float(), g) + psi.float()
        return torch.cos(total)


def slots_core(tok_tr, y_tr_raw, evals, E, S=100_000, r=64,
               lr_theta=3e-2, lr_z=1e-3, wd=1e-4, steps=40_000, batch=2048,
               eval_every=500, patience=15, warmup=500, clip=1.0,
               lam_div=1e-3, lam_deg=3e-4, div_sub=1024, warm_frac=0.25,
               slot_chunk=4096, val_fast=8192, base_tr=None, base_ev=None,
               device=None, seed=0, log=None, save_cb=None, resume=None):
    """PHASE slot machine (log-space characters): S slots, each learning its
    phase table z_s (character content), soft support sigma(theta_s), offset
    psi_s, and coefficient c_s -- one AdamW, no STE, no anneal (the churn it
    treated cannot occur).  Optionally trains on the RESIDUAL of a base
    model (base_tr / base_ev in f-units); metrics are on base + slots."""
    import torch
    device = device or ("cuda" if torch.cuda.is_available() else "cpu")
    emit = log or (lambda d: None)
    torch.manual_seed(seed)
    rng = np.random.default_rng(seed)
    dev = torch.device(device)
    amp = dev.type == "cuda"
    if amp:
        torch.backends.cuda.matmul.allow_tf32 = True
    tok_t = torch.tensor(np.asarray(tok_tr, np.int64), device=dev)
    D, w = tok_t.shape
    ytr = normalize_scores(y_tr_raw)
    if base_tr is not None:
        ytr = ytr - np.asarray(base_tr, np.float32)  # train on the residual
    y_t = torch.tensor(ytr, device=dev)
    E_t = torch.tensor(np.asarray(E, np.float32), device=dev)
    th = np.full((S, w), -3.0, np.float32)           # soft gates: sigma 0.05
    n_warm = int(S * warm_frac)
    for s in range(n_warm):                          # structural warm: local
        i = int(rng.integers(0, w - 4))
        d = int(rng.integers(1, 5))
        th[s, i] = 2.0                               # sigma 0.88
        th[s, min(i + d, w - 1)] = 2.0
    for s in range(n_warm, S):
        deg = int(rng.integers(1, 5))
        th[s, rng.choice(w, deg, replace=False)] = 2.0
    theta = torch.tensor(th, device=dev).requires_grad_(True)
    A = (torch.randn(E_t.shape[1], r, device=dev) / E_t.shape[1] ** 0.5
         ).requires_grad_(True)
    Z = torch.randn(S, r, device=dev).requires_grad_(True)
    psi = (2 * np.pi * torch.rand(S, device=dev)).requires_grad_(True)
    # c = 0: phase slots emit O(1), so 100k x 0.01-coef features sum to
    # ~2.2 std of init noise on a 0.11-std residual (step-0 val -1.27).
    # With smooth gates c gets immediate gradient; the STE-era warm-c
    # requirement no longer applies.
    c = torch.zeros(S, device=dev).requires_grad_(True)
    b = torch.tensor(float(ytr.mean()), device=dev).requires_grad_(True)
    if resume is not None:
        with torch.no_grad():
            theta.copy_(torch.tensor(resume["theta"], device=dev))
            A.copy_(torch.tensor(resume["A"], device=dev))
            Z.copy_(torch.tensor(resume["Z"], device=dev))
            psi.copy_(torch.tensor(resume["psi"], device=dev))
            c.copy_(torch.tensor(resume["c"], device=dev))
            b.fill_(float(resume["b"]))
        print(f"[slots] resumed from checkpoint (step {resume['step']})",
              flush=True)
    opt = torch.optim.AdamW([
        {"params": [theta], "lr": lr_theta, "weight_decay": 0.0},
        {"params": [A, Z, c], "lr": lr_z, "weight_decay": wd},
        {"params": [psi, b], "lr": lr_z, "weight_decay": 0.0}])
    sched = torch.optim.lr_scheduler.LambdaLR(
        opt, lambda s_: min(1.0, (s_ + 1) / max(1, warmup)))

    def predict(tok_eval, nrows=None):               # slot-machine part only
        with torch.no_grad():
            te = tok_eval[:nrows] if nrows else tok_eval
            EA = E_t @ A
            out = torch.empty(len(te), device=dev)
            for rlo in range(0, len(te), 2048):
                feat = EA[te[rlo:rlo + 2048]]
                acc = torch.full((len(feat),), float(b), device=dev)
                with torch.autocast("cuda", dtype=torch.bfloat16,
                                    enabled=amp):
                    for lo in range(0, S, slot_chunk):
                        acc = acc + (slot_forward_phase(
                            feat, theta[lo:lo + slot_chunk],
                            Z[lo:lo + slot_chunk],
                            psi[lo:lo + slot_chunk]).float()
                            @ c[lo:lo + slot_chunk])
                out[rlo:rlo + 2048] = acc
            return out.cpu().numpy()

    eb = {t: torch.tensor(np.asarray(tk, np.int64), device=dev)
          for t, (tk, _) in evals.items()}
    bev = {t: (np.zeros(len(evals[t][1]), np.float32) if base_ev is None
               else np.asarray(base_ev[t], np.float32)) for t in evals}
    gen = torch.Generator(device=device).manual_seed(seed)

    def val_metrics():
        sp = predict(eb["val"], val_fast)
        comb = score_metrics(sp + bev["val"][:val_fast],
                             evals["val"][1][:val_fast])
        resid = normalize_scores(evals["val"][1][:val_fast]) \
            - bev["val"][:val_fast]
        res_r2 = 1.0 - float(np.mean((sp - resid) ** 2)) \
            / (float(np.var(resid)) + 1e-12)
        return comb, res_r2

    vm0, rr0 = val_metrics()
    best = (vm0["mse"], {k: v.detach().clone() for k, v in
                         (("theta", theta), ("A", A), ("Z", Z), ("psi", psi),
                          ("c", c), ("b", b))}, 0)
    emit({"step": 0, "val_r2": vm0["r2"], "res_r2": rr0})
    print(f"[slots] step 0: combined val R2 {vm0['r2']:.4f} "
          f"(residual R2 {rr0:.4f})", flush=True)
    for s_ in range(steps):
        sel = torch.randint(0, D, (batch,), device=dev, generator=gen)
        with torch.autocast("cuda", dtype=torch.bfloat16, enabled=amp):
            feat = (E_t @ A)[tok_t[sel]]
            pred = torch.full((batch,), 0.0, device=dev) + b.float()
            for lo in range(0, S, slot_chunk):
                phi = torch.utils.checkpoint.checkpoint(
                    slot_forward_phase, feat, theta[lo:lo + slot_chunk],
                    Z[lo:lo + slot_chunk], psi[lo:lo + slot_chunk],
                    use_reentrant=False)
                pred = pred + phi.float() @ c[lo:lo + slot_chunk]
        loss = ((pred - y_t[sel]) ** 2).mean()
        if lam_div > 0:
            ks = torch.randint(0, S, (div_sub,), device=dev, generator=gen)
            m = Z[ks]
            mn = m / (m.norm(dim=1, keepdim=True) + 1e-8)
            Sim = mn @ mn.t()
            loss = loss + lam_div * (Sim - torch.eye(div_sub, device=dev)
                                     ).pow(2).mean()
        if lam_deg > 0:
            loss = loss + lam_deg * torch.sigmoid(theta).mean()
        opt.zero_grad(); loss.backward()
        torch.nn.utils.clip_grad_norm_([theta, A, Z, psi, c, b], clip)
        opt.step(); sched.step()
        if (s_ + 1) % eval_every == 0:
            vm, rr = val_metrics()
            with torch.no_grad():
                gsum = torch.sigmoid(theta).sum(1)
                tg = float(theta.grad.abs().mean()) \
                    if theta.grad is not None else 0.0
            emit({"step": s_ + 1, "train_loss": float(loss),
                  "val_r2": vm["r2"], "val_mse": vm["mse"], "res_r2": rr,
                  "lr_theta": opt.param_groups[0]["lr"],
                  "theta_grad_mean": tg,
                  "eff_degree": float(gsum.mean()),
                  "max_eff_degree": float(gsum.max()),
                  "c_abs_mean": float(c.abs().mean())})
            if vm["mse"] < best[0] - 1e-6:
                best = (vm["mse"], {k: v.detach().clone() for k, v in
                                    (("theta", theta), ("A", A), ("Z", Z),
                                     ("psi", psi), ("c", c), ("b", b))}, 0)
                if save_cb is not None:
                    save_cb({"theta": theta.detach().cpu().numpy(),
                             "A": A.detach().cpu().numpy(),
                             "Z": Z.detach().cpu().numpy(),
                             "psi": psi.detach().cpu().numpy(),
                             "c": c.detach().cpu().numpy(),
                             "b": float(b), "step": s_ + 1,
                             "val_mse": vm["mse"]})
            else:
                best = (best[0], best[1], best[2] + 1)
                if best[2] >= patience:
                    print(f"[slots] early stop at step {s_ + 1}", flush=True)
                    break
    with torch.no_grad():
        theta.copy_(best[1]["theta"]); A.copy_(best[1]["A"])
        Z.copy_(best[1]["Z"]); psi.copy_(best[1]["psi"])
        c.copy_(best[1]["c"]); b.copy_(best[1]["b"])
    degs = (torch.sigmoid(theta) > 0.5).sum(1).cpu().numpy()
    summary = {"S": S, "r": r, "steps_run": s_ + 1,
               "deg_hist": np.bincount(degs, minlength=10)[:10].tolist(),
               "max_degree": int(degs.max()),
               "sparse_bytes": int(degs.sum() * 2 + S * (r * 2 + 4 + 2)
                                   + E_t.shape[1] * r * 2)}
    for t in eb:
        sp = predict(eb[t])
        summary[t] = score_metrics(sp + bev[t], evals[t][1])
    model = {"theta": theta.detach().cpu().numpy().astype(np.float16),
             "A": A.detach().cpu().numpy(),
             "Z": Z.detach().cpu().numpy().astype(np.float16),
             "psi": psi.detach().cpu().numpy(),
             "c": c.detach().cpu().numpy(), "b": float(b)}
    return summary, model


@app.function(image=image, gpu=["A100-40GB", "H100", "L40S", "A10G"],
              volumes={"/cache": vol}, timeout=43200, memory=98304,
              secrets=WANDB_SECRET)
def fit_slots(S: int = 100_000, r: int = 64, n_train: int = 2_000_000,
              n_val: int = 25_000, n_test: int = 25_000, w: int = W_WIN,
              lr_theta: float = 3e-2, lr_z: float = 1e-3,
              lam_div: float = 1e-3, steps: int = 40_000,
              batch: int = 2048, warm_frac: float = 0.25):
    """The Fourier slot machine on the real data."""
    import torch
    vol.reload()
    data = np.load(_data_path(w, n_train, n_val, n_test))
    E = np.load(f"{ROOT}/emb.npz")["E"].astype(np.float32)
    run = _wandb_run(f"slots-phase-S{S}-N{n_train}",
                     {"S": S, "r": r, "lr_theta": lr_theta, "lr_z": lr_z,
                      "lam_div": lam_div, "warm_frac": warm_frac,
                      "batch": batch, "arch": "phase", "base": "qjoint"})
    evals = {"val": (data["tok_va"], data["y_va"]),
             "test": (data["tok_te"], data["y_te"])}
    # RESIDUAL BASE: the saved discovered-support joint model (test 0.7717)
    bm = np.load(f"{ROOT}/model_qjoint_N{n_train}.npz")
    v_base = torch.tensor(bm["v"].astype(np.float32), device="cuda")
    offs = [int(x) for x in bm["offsets"]]
    nh = int(bm["n_hash"])
    q0 = E.shape[0]
    mixi = -7046029254386353131

    def base_apply(tokens):
        t = np.asarray(tokens, np.int64)
        out = np.empty(len(t), np.float32)
        for lo in range(0, len(t), 262144):
            tc = torch.tensor(t[lo:lo + 262144], device="cuda")
            acc = v_base[tc].sum(dim=1)
            for d in offs:
                h = ((tc[:, : w - d] * q0 + tc[:, d:]) * mixi >> 40) & (nh - 1)
                acc = acc + v_base[q0 + offs.index(d) * nh + h].sum(dim=1)
            out[lo:lo + 262144] = acc.cpu().numpy()
        return out + float(bm["mean"])

    base_tr = base_apply(data["tok_tr"])
    base_ev = {"val": base_apply(data["tok_va"]),
               "test": base_apply(data["tok_te"])}
    del v_base
    torch.cuda.empty_cache()
    ck_path = f"{ROOT}/ckpt_slots_phase_S{S}_N{n_train}.npz"

    def save_cb(state):
        np.savez(ck_path + ".tmp.npz", **state)
        os.replace(ck_path + ".tmp.npz", ck_path)
        vol.commit()

    resume = dict(np.load(ck_path)) if os.path.exists(ck_path) else None
    summary, model = slots_core(
        data["tok_tr"], data["y_tr"], evals, E, S=S, r=r,
        lr_theta=lr_theta, lr_z=lr_z, lam_div=lam_div, steps=steps,
        batch=batch, warm_frac=warm_frac, save_cb=save_cb, resume=resume,
        base_tr=base_tr, base_ev=base_ev,
        log=(run.log if run is not None else None))
    print(f"[slots] FINAL: val {summary['val']['r2']:.4f} test "
          f"{summary['test']['r2']:.4f}; deg hist {summary['deg_hist']} "
          f"max {summary['max_degree']}; sparse "
          f"{summary['sparse_bytes'] / 1e6:.1f}MB", flush=True)
    np.savez_compressed(f"{ROOT}/model_slots_phase_S{S}_N{n_train}.npz",
                        **model)
    with open(f"{ROOT}/summary_slots_phase_S{S}_N{n_train}.json", "w") as fh:
        json.dump(summary, fh, indent=1)
    vol.commit()
    if run is not None:
        run.summary.update({"test_r2": summary["test"]["r2"],
                            "val_r2": summary["val"]["r2"]})
        run.finish()
    print(json.dumps(summary), flush=True)
    return summary



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


# --------------------------------------------------------- fitted arms (AdamW)
# Controlled challenge to the "coefficients CALCULATED, never Adam" rule under
# new conditions (2M rows, scalar target, val early-stop).  Recipe follows the
# repo's own fourier-learn history (canonical commit a0f1275): AdamW, CONSTANT
# lr, weight decay on coefficients only, never the bias.

def _pair_logits(X, a_idx, b_idx, C_t, chunk=25_000):
    """(B,) sum_k C_k x_a x_b, pair features gathered on the fly (never
    materializes B x K)."""
    import torch
    out = torch.zeros(len(X), device=X.device)
    for lo in range(0, len(a_idx), chunk):
        out += (X[:, a_idx[lo:lo + chunk]] * X[:, b_idx[lo:lo + chunk]]) \
            @ C_t[lo:lo + chunk]
    return out


def adamw_core(bits_tr, y_tr_raw, evals, idx, W1_init, C_init, b_init,
               lr=1e-4, wd=1e-4, steps=30_000, batch=8192, eval_every=250,
               patience=10, warmup=500, clip=1.0, device=None, seed=0,
               log=None):
    """AdamW refit of the coefficient VALUES on FIXED features (deg-1 bits +
    the given pair idx).  Returns (summary, model): summary reports train R2
    next to val/test -- a train >> val gap is the memorization signature the
    calculated-only rule was built on."""
    import torch
    device = device or ("cuda" if torch.cuda.is_available() else "cpu")
    emit = log or (lambda d: None)
    torch.manual_seed(seed)
    bits_t = torch.tensor(np.asarray(bits_tr, np.uint8), device=device)
    dev = bits_t.device
    ytr = torch.tensor(normalize_scores(y_tr_raw), device=dev)
    a_idx = torch.tensor(np.asarray(idx[:, 0], np.int64), device=dev)
    b_idx = torch.tensor(np.asarray(idx[:, 1], np.int64), device=dev)
    W1 = torch.tensor(np.asarray(W1_init, np.float32), device=dev).requires_grad_(True)
    C = torch.tensor(np.asarray(C_init, np.float32), device=dev).requires_grad_(True)
    b = torch.tensor(float(b_init), device=dev).requires_grad_(True)
    opt = torch.optim.AdamW([
        {"params": [W1, C], "weight_decay": wd},
        {"params": [b], "weight_decay": 0.0}], lr=lr)
    # standard hygiene, and the repo's own final fourier-learn recipe
    # (canonical 4e05499): CONSTANT lr with LINEAR WARMUP + grad clip --
    # Adam's early second-moment estimates otherwise blow up a warm start
    sched = torch.optim.lr_scheduler.LambdaLR(
        opt, lambda s: min(1.0, (s + 1) / max(1, warmup)))

    def forward(bslice):
        X = 1.0 - 2.0 * bslice.float()
        return X @ W1 + b + _pair_logits(X, a_idx, b_idx, C)

    def predict(bits_eval):
        with torch.no_grad():
            return np.concatenate(
                [forward(bits_eval[lo:lo + 8192]).cpu().numpy()
                 for lo in range(0, len(bits_eval), 8192)])

    eb = {t: torch.tensor(np.asarray(bb, np.uint8), device=dev)
          for t, (bb, _) in evals.items()}
    gen = torch.Generator(device=dev).manual_seed(seed)
    # step-0 eval: the INIT is the first best -- round 1 never scored the
    # warm start, so 250 Adam steps wrecked the calculated solution (per-
    # coordinate step ~lr regardless of gradient) and early stop caught the
    # partial recovery at 0.265 instead of the 0.423 init
    vm0 = score_metrics(predict(eb["val"]), evals["val"][1])
    emit({"step": 0, "val_mse": vm0["mse"], "val_r2": vm0["r2"]})
    best = (vm0["mse"],
            (W1.detach().clone(), C.detach().clone(), b.detach().clone()), 0)
    for s in range(steps):
        sel = torch.randint(0, len(bits_t), (batch,), device=dev, generator=gen)
        loss = ((forward(bits_t[sel]) - ytr[sel]) ** 2).mean()
        opt.zero_grad(); loss.backward()
        torch.nn.utils.clip_grad_norm_([W1, C, b], clip)
        opt.step(); sched.step()
        if (s + 1) % eval_every == 0:
            vm = score_metrics(predict(eb["val"]), evals["val"][1]) \
                if "val" in eb else {"mse": float(loss)}
            emit({"step": s + 1, "train_loss": float(loss),
                  "val_mse": vm["mse"], "val_r2": vm.get("r2", 0.0)})
            if vm["mse"] < best[0] - 1e-6:
                best = (vm["mse"],
                        (W1.detach().clone(), C.detach().clone(),
                         b.detach().clone()), 0)
            else:
                best = (best[0], best[1], best[2] + 1)
                if best[2] >= patience:
                    print(f"[adamw] early stop at step {s + 1}", flush=True)
                    break
    if best[1] is not None:
        with torch.no_grad():
            W1.copy_(best[1][0]); C.copy_(best[1][1]); b.copy_(best[1][2])
    tr_sub = torch.randperm(len(bits_t), generator=gen, device=dev)[:200_000]
    summary = {"steps_run": s + 1, "K": int(len(a_idx)),
               "train": score_metrics(predict(bits_t[tr_sub]),
                                      np.asarray(y_tr_raw)[tr_sub.cpu().numpy()])}
    for t in eb:
        summary[t] = score_metrics(predict(eb[t]), evals[t][1])
    model = {"W1": W1.detach().cpu().numpy(), "C": C.detach().cpu().numpy(),
             "b": float(b)}
    return summary, model


@app.function(image=image, gpu="A100-40GB", volumes={"/cache": vol},
              timeout=43200, memory=49152, secrets=WANDB_SECRET)
def fit_adamw(encoding: str = "lsh", w: int = W_WIN,
              n_train: int = 2_000_000, n_val: int = 25_000,
              n_test: int = 25_000, k_pairs: int = 200_000, lr: float = 1e-4,
              wd: float = 1e-4, steps: int = 30_000, batch: int = 8192,
              init: str = "both"):
    """Arm 1: AdamW coefficients on the calculated model's own features."""
    vol.reload()
    data = np.load(_data_path(w, n_train, n_val, n_test))
    z = np.load(f"{ROOT}/model_{encoding}_w{w}_N{n_train}.npz")
    codes = z["codes"]
    idx = z["idx"][:k_pairs]
    bits_tr = window_bits(data["tok_tr"], codes)
    evals = {"val": (window_bits(data["tok_va"], codes), data["y_va"]),
             "test": (window_bits(data["tok_te"], codes), data["y_te"])}
    c0 = float(z["c0"])
    out = {}
    for tag in (("warm", "cold") if init == "both" else (init,)):
        warm = tag == "warm"
        run = _wandb_run(f"adamw-{tag}-{encoding}-w{w}-N{n_train}",
                         {"encoding": encoding, "k_pairs": len(idx), "lr": lr,
                          "wd": wd, "init": tag})
        summary, model = adamw_core(
            bits_tr, data["y_tr"], evals, idx,
            W1_init=z["W1"] if warm else np.zeros_like(z["W1"]),
            C_init=z["C"][:k_pairs] if warm else np.zeros(len(idx), np.float32),
            b_init=(float(z["b1"]) + c0) if warm else 0.0,
            lr=lr, wd=wd, steps=steps, batch=batch,
            log=(run.log if run is not None else None))
        print(f"[adamw:{tag}] train R2 {summary['train']['r2']:.4f} val "
              f"{summary['val']['r2']:.4f} test {summary['test']['r2']:.4f}",
              flush=True)
        np.savez_compressed(f"{ROOT}/model_adamw_{tag}_{encoding}_w{w}_N{n_train}.npz",
                            codes=codes, idx=idx, **model)
        if run is not None:
            run.summary.update({f"{t}_r2": summary[t]["r2"]
                                for t in ("train", "val", "test")})
            run.finish()
        out[tag] = summary
    with open(f"{ROOT}/summary_adamw_{encoding}_w{w}_N{n_train}.json", "w") as fh:
        json.dump(out, fh, indent=1)
    vol.commit()
    print(json.dumps(out), flush=True)
    return out


# ------------------------------------------------------ fitted arms (STE masks)

def logspace_ste_chars(bits_f, theta, eps=1e-3):
    """Learned characters with STE: inclusion gates m = sigmoid(theta); the
    forward value is the EXACT +-1 parity of the HARDENED mask (m > 0.5);
    gradients flow through sign * exp(<b(x), log max(|1-2m|, eps)>) -- one
    GEMM, linear in the bit vector.  crib: canonical _logspace_ste_chars
    L1302-1330.  bits_f (B, n) float 0/1; theta (K, n).  Returns (B, K)."""
    import torch
    m = torch.sigmoid(theta)
    logmag = bits_f @ torch.log(torch.clamp((1.0 - 2.0 * m).abs(), min=eps)).t()
    hard = (m > 0.5).float()
    sign = 1.0 - 2.0 * ((bits_f @ hard.t()) % 2.0)
    soft = sign.detach() * torch.exp(logmag)
    return soft + (sign - soft).detach()


def ste_core(bits_tr, y_tr_raw, evals, K=16_384, warm_idx=None, warm_C=None,
             lr_theta=1e-1, lr_c=3e-4, wd=1e-4, steps=30_000, batch=16_384,
             eval_every=500, patience=12, warmup=500, clip=1.0,
             off_init=-8.0, on_init=8.0, lam_div=1e-3, div_sub=1024,
             device=None, seed=0, log=None):
    """Arm 2: jointly learn WHICH parities (STE masks) and their weights.
    Half the masks warm-init at warm_idx pair characters, half random 2-bit.
    Eval always uses the hardened masks (exact +-1 parities)."""
    import torch
    device = device or ("cuda" if torch.cuda.is_available() else "cpu")
    emit = log or (lambda d: None)
    torch.manual_seed(seed)
    rng = np.random.default_rng(seed)
    bits_t = torch.tensor(np.asarray(bits_tr, np.uint8), device=device)
    dev = bits_t.device
    n = bits_t.shape[1]
    ytr = torch.tensor(normalize_scores(y_tr_raw), device=dev)
    # gates must start STRONGLY saturated: the surrogate magnitude is
    # exp(sum over ~2.2k set bits of log|1-2m|), so off-gates at -2 give
    # exp(-600) = 0 and the data gradient into theta VANISHES (observed:
    # div_pen trajectory identical across rounds -- theta moved by the
    # diversity term alone).  At +-8 the per-bit factor is ~1-7e-4 and the
    # magnitude stays ~0.2; Adam's per-coordinate normalization handles the
    # small but consistent gradients
    th = np.full((K, n), off_init, np.float32)
    n_warm = 0
    if warm_idx is not None:
        n_warm = min(K // 2, len(warm_idx))
        for k in range(n_warm):
            th[k, warm_idx[k, 0]] = on_init
            th[k, warm_idx[k, 1]] = on_init
    for k in range(n_warm, K):                       # random starts of MIXED
        d = int(rng.integers(1, 5))                  # degree 1-4: arbitrary-
        th[k, rng.choice(n, d, replace=False)] = on_init  # degree reachable
    theta = torch.tensor(th, device=dev).requires_grad_(True)
    # warm coefficients too: theta's gradient is PROPORTIONAL to c, so c=0
    # starves the gates of signal (observed: masks frozen for 1500 steps)
    c_init = np.zeros(K, np.float32)
    if warm_C is not None and n_warm:
        c_init[:n_warm] = np.asarray(warm_C[:n_warm], np.float32)
    c = torch.tensor(c_init, device=dev).requires_grad_(True)
    b = torch.tensor(float(normalize_scores(y_tr_raw).mean()),
                     device=dev).requires_grad_(True)
    opt = torch.optim.AdamW([
        {"params": [theta], "lr": lr_theta, "weight_decay": 0.0},
        {"params": [c], "lr": lr_c, "weight_decay": wd},
        {"params": [b], "lr": lr_c, "weight_decay": 0.0}])
    sched = torch.optim.lr_scheduler.LambdaLR(
        opt, lambda s: min(1.0, (s + 1) / max(1, warmup)))

    def predict(bits_eval):
        with torch.no_grad():
            hard = (torch.sigmoid(theta) > 0.5).float()
            out = []
            for lo in range(0, len(bits_eval), 8192):
                Xf = bits_eval[lo:lo + 8192].float()
                sign = 1.0 - 2.0 * ((Xf @ hard.t()) % 2.0)
                out.append((sign @ c + b).cpu().numpy())
            return np.concatenate(out)

    eb = {t: torch.tensor(np.asarray(bb, np.uint8), device=dev)
          for t, (bb, _) in evals.items()}
    gen = torch.Generator(device=dev).manual_seed(seed)
    vm0 = score_metrics(predict(eb["val"]), evals["val"][1])
    best = (vm0["mse"], (theta.detach().clone(), c.detach().clone(),
                         b.detach().clone()), 0)
    for s in range(steps):
        sel = torch.randint(0, len(bits_t), (batch,), device=dev, generator=gen)
        Phi = logspace_ste_chars(bits_t[sel].float(), theta)
        loss = ((Phi @ c + b - ytr[sel]) ** 2).mean()
        # diversity regularizer on a mask subsample: penalize gate-vector
        # cosine overlap so masks don't collapse onto duplicates
        div_pen = torch.tensor(0.0, device=dev)
        if lam_div > 0:
            ksub = torch.randint(0, K, (div_sub,), device=dev, generator=gen)
            m = torch.sigmoid(theta[ksub])
            mn = m / (m.norm(dim=1, keepdim=True) + 1e-8)
            S = mn @ mn.t()
            div_pen = (S - torch.eye(div_sub, device=dev)).pow(2).mean()
            loss = loss + lam_div * div_pen
        opt.zero_grad(); loss.backward()
        # clip c/b only: a global norm over the 72M theta coords crushes
        # every per-gate step to nothing
        torch.nn.utils.clip_grad_norm_([c, b], clip)
        opt.step(); sched.step()
        if (s + 1) % eval_every == 0:
            vm = score_metrics(predict(eb["val"]), evals["val"][1])
            hard = (torch.sigmoid(theta) > 0.5)
            deg = hard.sum(1).float()
            n_uniq = len(np.unique(hard.cpu().numpy(), axis=0))
            emit({"step": s + 1, "train_loss": float(loss),
                  "val_mse": vm["mse"], "val_r2": vm["r2"],
                  "lr_theta": opt.param_groups[0]["lr"],
                  "lr_c": opt.param_groups[1]["lr"],
                  "div_pen": float(div_pen),
                  "n_unique_masks": n_uniq,
                  "mean_degree": float(deg.mean()),
                  "max_degree": int(deg.max())})
            if vm["mse"] < best[0] - 1e-6:
                best = (vm["mse"], (theta.detach().clone(),
                                    c.detach().clone(), b.detach().clone()), 0)
            else:
                best = (best[0], best[1], best[2] + 1)
                if best[2] >= patience:
                    print(f"[ste] early stop at step {s + 1}", flush=True)
                    break
    if best[1] is not None:
        with torch.no_grad():
            theta.copy_(best[1][0]); c.copy_(best[1][1]); b.copy_(best[1][2])
    masks = (torch.sigmoid(theta) > 0.5).cpu().numpy().astype(np.uint8)
    degs = masks.sum(1)
    tr_sub = torch.randperm(len(bits_t), generator=gen, device=dev)[:200_000]
    summary = {"steps_run": s + 1, "K": int(K), "n_warm": int(n_warm),
               "n_unique_masks": int(len(np.unique(masks, axis=0))),
               "max_degree": int(degs.max()) if len(degs) else 0,
               "deg_hist": np.bincount(degs, minlength=8).tolist(),
               "mask_bytes": int((degs * 2 + 4).sum()),  # int16 idx + fp32 coef
               "train": score_metrics(predict(bits_t[tr_sub]),
                                      np.asarray(y_tr_raw)[tr_sub.cpu().numpy()])}
    for t in eb:
        summary[t] = score_metrics(predict(eb[t]), evals[t][1])
    return summary, {"masks": masks, "c": c.detach().cpu().numpy(),
                     "b": float(b)}


@app.function(image=image, gpu="A100-40GB", volumes={"/cache": vol},
              timeout=43200, memory=49152, secrets=WANDB_SECRET)
def fit_ste(encoding: str = "lsh", w: int = W_WIN, n_train: int = 2_000_000,
            n_val: int = 25_000, n_test: int = 25_000, K: int = 16_384,
            lr_theta: float = 1e-1, lr_c: float = 3e-4, wd: float = 1e-4,
            steps: int = 30_000, batch: int = 16_384, lam_div: float = 1e-3):
    """Arm 2 wrapper: STE masks at 2M rows, warm half from the calculated
    model's top pairs.  Also reports the deflated-coefficient variant of the
    LEARNED masks (calculated weights on fitted features)."""
    import torch
    vol.reload()
    data = np.load(_data_path(w, n_train, n_val, n_test))
    z = np.load(f"{ROOT}/model_{encoding}_w{w}_N{n_train}.npz")
    codes = z["codes"]
    bits_tr = window_bits(data["tok_tr"], codes)
    evals = {"val": (window_bits(data["tok_va"], codes), data["y_va"]),
             "test": (window_bits(data["tok_te"], codes), data["y_te"])}
    run = _wandb_run(f"ste-{encoding}-w{w}-N{n_train}-K{K}",
                     {"encoding": encoding, "K": K, "lr_theta": lr_theta,
                      "lr_c": lr_c, "wd": wd})
    summary, model = ste_core(bits_tr, data["y_tr"], evals, K=K,
                              warm_idx=z["idx"], warm_C=z["C"],
                              lr_theta=lr_theta, lr_c=lr_c,
                              wd=wd, steps=steps, batch=batch, lam_div=lam_div,
                              log=(run.log if run is not None else None))
    print(f"[ste] train R2 {summary['train']['r2']:.4f} val "
          f"{summary['val']['r2']:.4f} test {summary['test']['r2']:.4f} "
          f"deg hist {summary['deg_hist']}", flush=True)
    # hybrid: CALCULATED coefficients on the LEARNED masks (dedup identical)
    umasks = np.unique(model["masks"][model["masks"].sum(1) > 0], axis=0)
    dev = "cuda" if torch.cuda.is_available() else "cpu"
    bt = torch.tensor(bits_tr, device=dev)
    ytr = normalize_scores(data["y_tr"])
    c0 = float(ytr.mean())
    g_t = torch.tensor(ytr - c0, device=dev)
    Cd, _ = sequential_deflate(bt, g_t, None, device=dev, block=512,
                               masks=umasks)
    Cd_t = torch.tensor(Cd, device=dev)
    hyb = {}
    for t, (bb, yy) in evals.items():
        be = torch.tensor(np.asarray(bb, np.uint8), device=dev)
        pred = torch.full((len(be),), c0, device=dev)
        for lo in range(0, len(umasks), 4096):
            pred += mask_parity_features(be, umasks[lo:lo + 4096]) \
                @ Cd_t[lo:lo + 4096]
        hyb[t] = score_metrics(pred.cpu().numpy(), yy)
    summary["deflated_on_learned_masks"] = {
        "n_masks": int(len(umasks)), **{t: hyb[t] for t in hyb}}
    print(f"[ste] deflated-on-learned-masks test R2 {hyb['test']['r2']:.4f} "
          f"({len(umasks)} masks)", flush=True)
    np.savez_compressed(f"{ROOT}/model_ste_{encoding}_w{w}_N{n_train}.npz",
                        codes=codes, **model)
    with open(f"{ROOT}/summary_ste_{encoding}_w{w}_N{n_train}.json", "w") as fh:
        json.dump(summary, fh, indent=1)
    vol.commit()
    if run is not None:
        run.summary.update({f"{t}_r2": summary[t]["r2"]
                            for t in ("train", "val", "test")})
        run.finish()
    print(json.dumps(summary), flush=True)
    return summary


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
    suf = "" if b == B_LSH else f"_b{b}"           # never clobber B=64 files
    np.savez_compressed(f"{ROOT}/model_{encoding}_w{w}_N{n_train}{suf}.npz",
                        codes=codes, **model)
    with open(f"{ROOT}/summary_fit_{encoding}_w{w}_N{n_train}{suf}.json",
              "w") as fh:
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
         deg3_anchors: int = 0, max_triples: int = 200_000,
         pos_buckets: int = 1, orders: str = "1,2", n_hash: int = 1 << 22):
    if stage == "label":
        print(label.remote(n_train, n_val, n_test, w, batch))
    elif stage == "fit":
        encs = ("lsh", "ctrl", "tokid") if encoding == "all" else (encoding,)
        for enc in encs:
            fit.remote(enc, w, b, max_pairs, n_train, n_val, n_test,
                       512, deg3_anchors, max_triples)
    elif stage == "token":
        fit_token.remote(encoding, w, b, max_pairs, n_train, n_val, n_test,
                         512, pos_buckets)
    elif stage == "ngram":
        fit_ngram.remote(orders, n_hash, n_train, n_val, n_test, w)
    elif stage == "qpair":
        fit_qpair.remote(n_train, n_val, n_test, w)
    elif stage == "qhh":
        fit_qhh.remote(n_train, n_val, n_test, w)
    elif stage == "qjoint":
        fit_qjoint.remote("1,2,3,4", 1 << 22, n_train, n_val, n_test, w)
    elif stage == "qfull":
        fit_qfull.remote(1 << 22, n_train, n_val, n_test, w)
    elif stage == "slots":
        fit_slots.remote(100_000, 64, n_train, n_val, n_test, w)
    elif stage == "adamw":
        fit_adamw.remote(encoding, w, n_train, n_val, n_test)
    elif stage == "ste":
        fit_ste.remote(encoding, w, n_train, n_val, n_test)
    elif stage == "ceilings":
        ceilings.remote(w, n_train, n_val, n_test)
    elif stage == "show":
        show.remote()
    else:
        raise SystemExit(f"unknown stage {stage}")
