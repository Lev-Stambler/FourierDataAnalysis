"""Canonical experiment: q-ary Dataset GL over sign-LSH token codes.

The paper's recipe, one isolated module, one search parameter (tau):

1. FIBERS FROM THE DATASET.  Anchor at real FineWeb spans; the fixed part of
   every fiber (the real prefix, whose last k tokens are the fixed string s)
   comes from the dataset.  Fibers are deduped on s so that
   f(window) = teacher(next | prefix(s), window) is a well-defined function of
   the window.
2. FILL FROM THE MODEL.  The model itself is the natural-language sampler:
   R independent autoregressive fills of the remaining w-k window positions
   per fiber (the fills are the CSAMP pairs ~ P(g | s)).
3. ONE GENERATED DATASET, OFFLINE SEARCH.  Collapse distinct windows, label
   with the teacher's terminal next-token distribution projected onto a
   512-slot alphabet, then run the exact-Parseval csamp group-by GL tree on
   that table.  Bucket weight for a prefix character S at bit level kk:

       W(S) = (2^(n-kk) / m^2) * sum_t sum_z ( sum_{x in group z} chi_S(x) A[x, t] )^2,

   with z the un-split bit suffix.  Keep a child iff W >= tau^2/4; the last
   level is the exact leaf test.  No sampling inside the search, no beams, no
   heuristics; max_width exists only as a blowup abort.
4. ENCODING q = 2^B, DOMAIN = ALL PREVIOUS TOKENS.  Every one of the CTX
   context tokens enters the GL domain as its B-bit sign-LSH code
   (mean-centered teacher embeddings, Gaussian projections, deterministic;
   inseparable duplicate rows get tie-break bits), so the tree is binary over
   n = CTX*B bit positions.  Blocks are ordered nearest-the-prediction first:
   the model-filled tail is split first (within-fiber conditional variation),
   then the real prefix bits, whose bucket weights come from across-fiber
   variation -- Fourier analysis on the dataset over the full context.
   n >> 62 bits, so the search runs on bit arrays (no int64 packing); the math
   is anchored against brute force in the tests.

Science: the same fibers and the same tau through the LSH table, an iid
random-code table (capacity-matched control), and raw token-id bits.
"""

from __future__ import annotations

import hashlib
import json
import math

import numpy as np

import modal

MODEL_ID = "Qwen/Qwen3.5-0.8B-Base"
FINEWEB = ("HuggingFaceFW/fineweb", "CC-MAIN-2024-10")
FINEWEB_EDU = ("HuggingFaceFW/fineweb-edu", "CC-MAIN-2024-10")
CORPORA = {"fineweb": FINEWEB, "edu": FINEWEB_EDU}
CTX = 128            # real span length; fiber prefix = span[: CTX - (W - K)]
W_WIN = 6            # window tokens (fixed string s = first K, model fills W-K)
K_FIXED = 3
M_FIBERS = 4000
R_FILLS = 8
V_SLOTS = 512
B0, B_CAP = 64, 128  # LSH projection doubling range
# strided back-offsets for the sensitivity profile (0 = last context token)
SENS_POSITIONS = [0, 1, 2, 3, 4, 5, 6, 7, 11, 15, 23, 31, 47, 63, 95, 124]

ROOT = "/cache/canonical/qary_lsh_gl"
A10_PER_SECOND = 0.000306
BUDGET = 250.0  # raised for grad-sense (1M-char harvest on H100; ledger counts
                # wall-seconds at the A10 rate, so H100 hours under-count ~3.6x)

app = modal.App("canonical-qary-lsh-gl")
vol = modal.Volume.from_name("fda-cache", create_if_missing=True)
image = (
    modal.Image.debian_slim(python_version="3.11")
    .pip_install("numpy>=1.26", "torch>=2.5", "transformers>=5.13.1",
                  "accelerate>=1.2", "datasets>=4.0", "safetensors", "sentencepiece",
                  "wandb>=0.18")
    .env({"HF_HOME": "/cache/hf"})
)
try:
    WANDB_SECRET = [modal.Secret.from_name("wandb")]
except Exception:
    WANDB_SECRET = []
try:                                                             # authenticated HF streaming
    HF_SECRET = [modal.Secret.from_name("hf-token")]            # avoids rate-limit failures
except Exception:                                               # when N fan-out workers stream
    HF_SECRET = []


def _wandb_run(name, config):
    """Best-effort W&B run; returns the run or None (logging never breaks a fit).
    The `wandb` Modal secret supplies WANDB_API_KEY."""
    try:
        import wandb
        return wandb.init(project="fda-canonical-qary-lsh", name=name,
                          config=config, reinit=True)
    except Exception as e:
        print(f"[wandb] disabled ({e})", flush=True)
        return None


@app.function(image=image, volumes={"/cache": vol}, timeout=300, secrets=WANDB_SECRET)
def wandb_ping():
    """Create the W&B project now and print its URL/entity (before the long
    run logs at ~2h).  Seeds the known FineWeb deg-1+2 scaling points."""
    import wandb
    run = wandb.init(project="fda-canonical-qary-lsh", name="scaling-ref",
                     config={"note": "reference FineWeb deg1+2 scaling"}, reinit=True)
    for m, tr, te in [(2000, 0.5202, 1.4530), (8000, 0.7865, 1.1034)]:
        run.log({"m_train": m, "TRAIN_kl": tr, "TEST_kl": te})
    url = run.url
    run.finish()
    print(f"[wandb] project URL: {url}", flush=True)
    return url


# ------------------------------------------------------------------ code tables

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


def build_lsh_codes(E, B0=B0, cap=B_CAP, seed=0):
    """Sign-LSH codes, doubling B while extra bits still separate DISTINCT
    embedding rows (byte-identical rows are inseparable and get tie-breaks)."""
    E = np.asarray(E, dtype=np.float32)
    centered = E - E.mean(axis=0, keepdims=True)
    _, emb_gid = np.unique(centered, axis=0, return_inverse=True)
    G = np.random.default_rng(seed).standard_normal((cap, E.shape[1])).astype(np.float32)
    proj = (centered @ G.T > 0).astype(np.uint8)

    def unresolved(B):
        _, cid = np.unique(proj[:, :B], axis=0, return_inverse=True)
        combos = np.unique(np.stack([cid, emb_gid], 1), axis=0)
        mixed = np.flatnonzero(np.bincount(combos[:, 0]) > 1)
        return int(np.isin(cid, mixed).sum())

    B = min(B0, cap)
    curve = {B: unresolved(B)}
    while curve[B] > 0 and B < cap:
        B = min(2 * B, cap)
        curve[B] = unresolved(B)
    codes = _tie_break(proj[:, :B])
    return codes, {"B_proj": int(B), "B_total": int(codes.shape[1]),
                   "collisions_vs_B": {str(b): v for b, v in curve.items()}}


def control_codes(q, B, seed=1):
    codes = np.random.default_rng(seed).integers(0, 2, (q, B), dtype=np.uint8)
    if len(np.unique(codes, axis=0)) < q:
        raise RuntimeError("control codes collided")
    return codes


def token_id_codes(q):
    width = max(1, int(q - 1).bit_length())
    return ((np.arange(q, dtype=np.int64)[:, None] >> np.arange(width)) & 1).astype(np.uint8)


# --------------------------------------------------------------- fibers/windows

def draw_fibers(spans, w=W_WIN, k=K_FIXED, M=M_FIBERS, seed=0):
    """Fiber prefixes from real spans, deduped on the fixed string s (the last
    k prefix tokens) so f(window) is well-defined.  Returns PRE (M, ctx-(w-k))."""
    spans = np.asarray(spans, dtype=np.int64)
    pre = spans[:, : spans.shape[1] - (w - k)]
    order = np.random.default_rng(seed).permutation(len(pre))
    out, seen = [], set()
    for i in order:
        key = tuple(pre[i, -k:])
        if key in seen:
            continue
        seen.add(key)
        out.append(pre[i])
        if len(out) == M:
            break
    if len(out) < M:
        raise ValueError(f"only {len(out)} distinct fixed strings, need {M}")
    return np.stack(out)


def context_bits(contexts, codes):
    """(D, ctx) full contexts in text order -> (D, ctx*B) uint8 bits, token
    blocks reversed so the token nearest the prediction is split first and
    earlier (real) tokens sit in the conditioning suffix."""
    contexts = np.asarray(contexts, dtype=np.int64)
    return np.asarray(codes, dtype=np.uint8)[contexts[:, ::-1]].reshape(len(contexts), -1)


def soft_collapse(windows, P):
    """Distinct windows + per-row weighted target sums A = sum of P over
    duplicates (rows of one window share one fiber by s-dedupe, so P agrees)."""
    rows, inv, counts = np.unique(np.asarray(windows, dtype=np.int64), axis=0,
                                  return_inverse=True, return_counts=True)
    A = np.zeros((len(rows), P.shape[1]), dtype=np.float64)
    np.add.at(A, inv, np.asarray(P, dtype=np.float64))
    return rows, A.astype(np.float32), counts.astype(np.int64), inv


# ------------------------------------------------------------------- the search

def _group_ids(bits_suffix):
    """(m,) group ids of the un-split bit suffix (packbits + void-view unique)."""
    if bits_suffix.shape[1] == 0:
        return np.zeros(len(bits_suffix), dtype=np.int64), 1
    packed = np.packbits(bits_suffix, axis=1)
    view = np.ascontiguousarray(packed).view([("", packed.dtype)] * packed.shape[1]).ravel()
    _, inv = np.unique(view, return_inverse=True)
    return inv.astype(np.int64), int(inv.max()) + 1


def _iter_suffix_gids(bits, block=512):
    """Yield (gid, ng) of bits[:, k+1:] for k = 0..n-1 without a full per-level
    packbits scan: adding bit j to a suffix REFINES its groups, so within a
    block the gids are built backwards from one packbits base at the block
    end, then yielded in ascending level order."""
    m, n = bits.shape
    b64 = bits.astype(np.int64)
    for lo in range(1, n + 1, block):
        hi = min(lo + block, n + 1)
        gid, ng = _group_ids(bits[:, hi:]) if hi <= n else (np.zeros(m, np.int64), 1)
        cache = {}
        for j in range(hi - 1, lo - 1, -1):
            if j < n:
                _, gid = np.unique(gid * 2 + b64[:, j], return_inverse=True)
                gid = gid.astype(np.int64)
                ng = int(gid.max()) + 1
            cache[j] = (gid.copy(), ng)
        for j in range(lo, hi):
            yield cache[j]


def _bucket_Q(chi, A_t, gid_t, ng, mem_budget=8.0e8, cls_chunk=128):
    """Q(S) = sum_t sum_z (sum_{x in z} chi_S(x) A[x,t])^2 for chi rows (L, m).
    float32 throughout (fp64 is ~1/32 throughput on consumer GPUs); the final
    sum is returned as float64.  Magnitudes here are counts*probabilities."""
    import torch
    L, m = chi.shape
    V = A_t.shape[1]
    Q = torch.zeros(L, dtype=torch.float64, device=chi.device)
    cc = max(1, int(mem_budget // (m * cls_chunk)))
    for i in range(0, L, cc):
        cbi = chi[i:i + cc]
        acc = torch.zeros(cbi.shape[0], dtype=torch.float64, device=chi.device)
        for j in range(0, V, cls_chunk):
            g = cbi[:, :, None] * A_t[None, :, j:j + cls_chunk]
            G = torch.zeros((cbi.shape[0], ng, g.shape[2]), dtype=torch.float32,
                            device=chi.device)
            G.index_add_(1, gid_t, g)
            acc += (G * G).sum(dim=(1, 2)).double()
        Q[i:i + cc] = acc
    return Q


def dataset_gl_csamp(bits, F_rows, fiber_gid, tau, max_width=512, device=None,
                     n_search=None, on_progress=None, progress_every=None):
    """Offline realization of the paper's PAIRED CSAMP bucket
    (ar_categorical_gl.typ, lem:qary-kv-estimator): both draws of a pair share
    the real context Z AND the un-split continuation L_k, so a cell is
    (fiber x un-split bit VALUES) and only OFF-DIAGONAL within-cell products
    count.  By the autoregressive factorization, two independent fills
    conditioned on agreeing over the un-split coordinates have i.i.d. split
    parts, so value-matched pairs are distributionally identical to the
    cache-forked pairs of the theorem.

        psi_k(a) = (Q_k(a) - sum_x ||f(x)||^2) / n_pairs_k,
        Q_k(a)   = sum_cells |sum_{x in cell} f(x) chi_a(x)|^2,

    unbiased for the pair-weighted conditional weight E|v_{k,a}(Z, L_k)|^2;
    keep iff psi >= tau^2/4 (the paper's N_k rule).  Singleton cells cancel
    exactly against the diagonal, so unlike the group-by SQUARE there is no
    S-independent noise mass.  The flat file's price is the empirical
    collision profile: a level with zero matched pairs carries no evidence,
    so both children are kept there (recorded in ``no_evidence_levels``) and
    certification comes from the evidenced levels.  Pairing on the fiber
    alone (independent L_k) estimates the cancellation-prone pooled quantity
    the theory explicitly excludes and is exact only at the terminal level;
    this function never does that.

    bits: (m, n) uint8 PER-FILL rows (not collapsed) in newest-token-first
    order; F_rows: (m, V) centered per-fill targets; fiber_gid: (m,) fiber of
    each row.  Returns dict(status, masks, widths, psi_top, pair_profile,
    no_evidence_levels, saturated_levels).
    """
    import torch
    device = device or ("cuda" if torch.cuda.is_available() else "cpu")
    bits = np.asarray(bits, dtype=np.uint8)
    m, n = bits.shape
    n_search = n if n_search is None else min(n_search, n)
    F_t = torch.tensor(np.asarray(F_rows, dtype=np.float32), device=device)
    fib = np.unique(np.asarray(fiber_gid), return_inverse=True)[1].astype(np.int64)
    fmax = int(fib.max()) + 1
    fc = np.bincount(fib, minlength=fmax).astype(np.float64)
    if float((fc * (fc - 1)).sum()) <= 0:                # cells refine fibers
        return dict(status="no-pairs", masks=np.zeros((0, n), np.uint8),
                    widths=[], psi_top=[], pair_profile=[],
                    no_evidence_levels=[], saturated_levels=[])
    signs = torch.tensor(1.0 - 2.0 * bits[:, :n_search].astype(np.float32),
                         device=device)

    def _pair_psi(chi, cell, cc, npairs):
        """(Q - diag)/npairs restricted to collision cells: a singleton cell's
        Q equals its own diagonal, so dropping it is exact and removes most of
        the memory traffic at early levels."""
        multi = np.flatnonzero(cc[cell] > 1)
        _, mcell = np.unique(cell[multi], return_inverse=True)
        rows_t = torch.tensor(multi, dtype=torch.long, device=device)
        Fm = F_t[rows_t]
        dm = float((Fm.double() ** 2).sum().item())
        gid_t = torch.tensor(mcell.astype(np.int64), dtype=torch.long,
                             device=device)
        # small blocks + per-call cache reset: the collision-row count changes
        # every level, and caching variable multi-GB transients OOM-thrashes
        Q = _bucket_Q(chi[:, rows_t].contiguous(), Fm, gid_t,
                      int(mcell.max()) + 1, mem_budget=2.0e8).cpu().numpy()
        del Fm, gid_t, rows_t
        torch.cuda.empty_cache()
        return (Q - dm) / npairs

    thresh = tau * tau / 4.0
    live_masks = np.zeros((1, n), dtype=np.uint8)
    live_chi = torch.ones((1, m), dtype=torch.float32, device=device)
    widths, pair_profile, no_evidence, saturated = [], [], [], []
    terminal = None
    for k, (sgid, _) in zip(range(n_search), _iter_suffix_gids(bits)):
        # cell = (fiber, un-split VALUE): the shared (Z, L_k) of a paper pair
        _, cell = np.unique(sgid * fmax + fib, return_inverse=True)
        cell = cell.astype(np.int64)
        ncell = int(cell.max()) + 1
        cc = np.bincount(cell, minlength=ncell).astype(np.float64)
        npairs = float((cc * (cc - 1)).sum())
        pair_profile.append(npairs)
        chi_add = live_chi * signs[:, k][None, :]
        chi_all = torch.cat([live_chi, chi_add], dim=0)
        masks_add = live_masks.copy()
        masks_add[:, k] = 1
        masks_all = np.concatenate([live_masks, masks_add])
        if npairs > 0:
            psi = _pair_psi(chi_all, cell, cc, npairs)
            keep = np.flatnonzero(psi >= thresh)
        else:
            no_evidence.append(k)                        # no verdict: carry both
            psi = np.zeros(len(masks_all))
            keep = np.arange(len(masks_all))
        if len(keep):
            cols = ((1.0 - chi_all[torch.tensor(keep, device=device)]) / 2.0)
            packed = np.packbits(cols.to(torch.uint8).cpu().numpy(), axis=1)
            view = np.ascontiguousarray(packed).view(
                [("", packed.dtype)] * packed.shape[1]).ravel()
            _, first = np.unique(view, return_index=True)
            keep = keep[np.sort(first)]
        if len(keep) > max_width:
            saturated.append(k)
            keep = keep[np.argpartition(-psi[keep], max_width)[:max_width]]
        live_masks = masks_all[keep]
        live_chi = chi_all[torch.tensor(keep, device=device)]
        widths.append(int(len(live_masks)))
        if len(live_masks) == 0:
            break
        if k == n_search - 1:
            terminal = (cell, cc, npairs)
        step = progress_every or max(1, n_search // 20)
        if (k + 1) % step == 0 or k == n_search - 1:
            cert = int((psi >= thresh).sum()) if npairs > 0 else 0
            mass = float(psi[psi >= thresh].sum()) if npairs > 0 else 0.0
            print(f"[csamp] level {k + 1}/{n_search} width={len(live_masks)} "
                  f"pairs={npairs:.0f} filled_buckets={cert} captured_mass={mass:.4f}",
                  flush=True)
            if on_progress is not None:
                on_progress(k + 1, n_search, live_masks.copy())
    masks = live_masks[live_masks.any(axis=1)]
    psi_top = []
    if len(masks) and terminal is not None and terminal[2] > 0:
        cell, cc, npairs = terminal
        chi = torch.tensor(parity_features(bits, masks).T.copy(), device=device)
        psi_f = _pair_psi(chi, cell, cc, npairs)
        order = np.argsort(-psi_f)
        masks = masks[order]
        psi_top = [float(x) for x in psi_f[order][:10]]
    return dict(status="ok", masks=masks.astype(np.uint8), widths=widths,
                psi_top=psi_top, pair_profile=pair_profile,
                no_evidence_levels=no_evidence, saturated_levels=saturated)


def oracle_deg1_psi(split_bits, F_rows, fiber_gid, device=None, clean=False):
    """The paper's PAIRED bucket for every single-bit character of ONE split
    coordinate, evaluated on FORKED-CACHE data (lem:qary-kv-estimator): every
    row of a fiber shares the real prefix AND the generated stub L_k, so all
    within-fiber pairs are valid by construction -- no collision on the
    un-split suffix is required, and none of the frontier-burial / offline
    collision-starvation applies.  For bit b,

        psi(b) = (1/n_pairs) sum_z ( ||sum_{i in z} chi_b(i) F_i||^2
                                     - sum_{i in z} ||F_i||^2 ),

    unbiased for E_z|E[F chi_b | z]|^2 (z = fiber = one fork).

    ``clean=True`` additionally subtracts the SAME-TOKEN block within each
    fiber (pairs whose split token is IDENTICAL, so chi_b(i)chi_b(j)=1 for
    EVERY b): that is a per-fiber constant -- the token-collision / point-mass
    floor -- which shifts all psi(b) equally and so cannot change the ranking,
    but it inflates magnitudes and floods a fixed certification bar (every bit
    passes).  Removing it leaves only the discriminating cross-token pairs, so
    the bar and the reported norms become meaningful (the value that the
    theorem certifies is still the un-cleaned full weight, recovered by fitting
    the whole basis).  Returns psi (B,) and its sqrt.

    split_bits: (m, B) uint8 code of the split token per forked row;
    F_rows: (m, V) CENTERED terminal targets; fiber_gid: (m,) the fork id.
    """
    import torch
    device = device or ("cuda" if torch.cuda.is_available() else "cpu")
    split_bits = np.asarray(split_bits, dtype=np.uint8)
    _, gid = np.unique(np.asarray(fiber_gid), return_inverse=True)
    gid = gid.astype(np.int64)
    ng = int(gid.max()) + 1
    counts = np.bincount(gid, minlength=ng).astype(np.float64)
    F_t = torch.tensor(np.asarray(F_rows, dtype=np.float32), device=device)
    gid_t = torch.tensor(gid, dtype=torch.long, device=device)
    signs = torch.tensor(1.0 - 2.0 * split_bits.astype(np.float32), device=device)
    Q = _bucket_Q(signs.t().contiguous(), F_t, gid_t, ng, mem_budget=2.0e8).cpu().numpy()
    B = split_bits.shape[1]
    if not clean:
        n_pairs = float((counts * (counts - 1)).sum())
        if n_pairs <= 0:
            return np.zeros(B), np.zeros(B)
        block = float((F_t.double() ** 2).sum().item())            # diagonal only
    else:
        # subcell = (fiber, split-token VALUE); block = the same-token square,
        # n_pairs = only the cross-token (discriminating) pairs
        packed = np.packbits(split_bits, axis=1)
        view = np.ascontiguousarray(packed).view(
            [("", packed.dtype)] * packed.shape[1]).ravel()
        _, code_id = np.unique(view, return_inverse=True)
        _, sub = np.unique(gid * (int(code_id.max()) + 1) + code_id,
                           return_inverse=True)
        sub = sub.astype(np.int64)
        nsub = int(sub.max()) + 1
        sub_t = torch.tensor(sub, dtype=torch.long, device=device)
        ones = torch.ones((1, len(sub)), dtype=torch.float32, device=device)
        block = float(_bucket_Q(ones, F_t, sub_t, nsub,
                                mem_budget=2.0e8).cpu().numpy()[0])   # sum||sum_subcell F||^2
        subc = np.bincount(sub, minlength=nsub).astype(np.float64)
        n_pairs = float((counts * (counts - 1)).sum() - (subc * (subc - 1)).sum())
        if n_pairs <= 0:
            return np.zeros(B), np.zeros(B)
    psi = (Q - block) / n_pairs
    return psi, np.sqrt(np.maximum(psi, 0.0))


def _level_psi(chi, F_t, gid_t, ng, block, n_pairs, device):
    """(Q_S - block)/n_pairs for a batch of characters chi (L, m); block and
    n_pairs are the per-level clean scalars (identical-fill floor removed)."""
    Q = _bucket_Q(chi, F_t, gid_t, ng, mem_budget=2.0e8).cpu().numpy()
    return (Q - block) / n_pairs


def forked_gl_tree(levels, B, tau, max_width=128, device=None, progress=None):
    """The TRUE dataset-GL tree, native and scalable BEYOND degree two: a
    reverse-time beam search that grows Walsh characters across tokens using
    the paper's forked-cache PAIRED estimator at every level (collision-free,
    unlike the offline group-by, and enumeration-free, unlike the per-degree
    GEMMs).  ``levels[j]`` holds a fork that resampled the newest ``j+1`` tokens
    ``g`` times per fiber -- so every level supplies genuine within-fork pairs.
    A character on the newest ``k`` tokens is discovered by extending its heavy
    lower-token prefixes (hereditary GL): at level ``j`` every live parent is
    extended by each single bit of token block ``j``, the paired weight
    ``psi(S) = E_z|E[F chi_S | z]|^2`` is estimated on that level's fork, and
    children with ``psi >= tau^2/4`` are kept.  Degree = number of tokens the
    character spans, unbounded by construction.

    levels[j] = (bits (m, (j+1)*B) uint8 newest-first, F (m, V) centered,
    fiber_gid (m,)).  Returns dict(masks (K, depth*B) uint8, psi (K,),
    per_level_kept).  Point-mass note: the per-level clean subtraction removes
    the identical-fill floor (chi_S=1 for all S on repeated fills); residual
    inflation is left for the downstream val-gated fit to reject."""
    import torch
    device = device or ("cuda" if torch.cuda.is_available() else "cpu")
    depth = len(levels)
    thresh = tau * tau / 4.0
    total = depth * B
    live = np.zeros((1, total), dtype=np.uint8)                    # the empty character
    kept = {}
    per_level_kept = []
    for j in range(depth):
        bits_j, F_rows, fib = levels[j]
        bits_j = np.asarray(bits_j, dtype=np.uint8)
        w = (j + 1) * B
        m = len(F_rows)
        F_t = torch.tensor(np.asarray(F_rows, dtype=np.float32), device=device)
        _, gid = np.unique(np.asarray(fib), return_inverse=True)
        gid = gid.astype(np.int64); ng = int(gid.max()) + 1
        gid_t = torch.tensor(gid, dtype=torch.long, device=device)
        counts = np.bincount(gid, minlength=ng).astype(np.float64)
        # clean scalars: subcell = (fiber, full resampled-span value)
        packed = np.packbits(bits_j, axis=1)
        view = np.ascontiguousarray(packed).view(
            [("", packed.dtype)] * packed.shape[1]).ravel()
        _, cod = np.unique(view, return_inverse=True)
        _, sub = np.unique(gid * (int(cod.max()) + 1) + cod, return_inverse=True)
        sub = sub.astype(np.int64); nsub = int(sub.max()) + 1
        sub_t = torch.tensor(sub, dtype=torch.long, device=device)
        subc = np.bincount(sub, minlength=nsub).astype(np.float64)
        block = float(_bucket_Q(torch.ones((1, m), device=device), F_t, sub_t, nsub,
                                mem_budget=2.0e8).cpu().numpy()[0])
        n_pairs = float((counts * (counts - 1)).sum() - (subc * (subc - 1)).sum())
        if n_pairs <= 0:
            per_level_kept.append(0)
            continue
        # candidate children: each live parent + one bit of THIS token block
        block_bits = np.arange(j * B, (j + 1) * B)
        rows = []
        for parent in live:
            free = block_bits[parent[block_bits] == 0]
            if not len(free):
                continue
            rep = np.repeat(parent[None, :], len(free), axis=0)
            rep[np.arange(len(free)), free] = 1
            rows.append(rep)
        if not rows:
            per_level_kept.append(0)
            continue
        children = np.unique(np.concatenate(rows), axis=0)
        chi = torch.tensor(parity_features(bits_j, children[:, :w]).T.copy(),
                           device=device)
        psi = _level_psi(chi, F_t, gid_t, ng, block, n_pairs, device)
        heavy = np.flatnonzero(psi >= thresh)
        for idx in heavy:
            key = tuple(int(x) for x in children[idx])
            if psi[idx] > kept.get(key, -1e30):
                kept[key] = float(psi[idx])
        per_level_kept.append(int(len(heavy)))
        # carry the empty char + the globally heaviest characters forward
        top = sorted(kept.items(), key=lambda kv: -kv[1])[:max_width]
        live = np.concatenate([np.zeros((1, total), dtype=np.uint8),
                               np.array([k for k, _ in top], dtype=np.uint8)]) \
            if top else np.zeros((1, total), dtype=np.uint8)
        if progress is not None:
            progress(j, depth, kept)
    if not kept:
        return dict(masks=np.zeros((0, total), np.uint8), psi=np.zeros(0),
                    per_level_kept=per_level_kept)
    order = sorted(kept.items(), key=lambda kv: -kv[1])
    masks = np.array([k for k, _ in order], dtype=np.uint8)
    psi = np.array([v for _, v in order], dtype=np.float64)
    return dict(masks=masks, psi=psi, per_level_kept=per_level_kept)


def dataset_gl_tau(bits, A, tau, norm_m, max_width=512, device=None, n_search=None,
                   on_progress=None, progress_every=None):
    """Exact-W csamp GL tree over bit arrays: keep children with W >= tau^2/4,
    ORDERED with a bounded frontier (the campaign's fast ordered search).

    At levels where suffix groups are near-singletons, W is provably identical
    for every prefix (the dataset cannot discriminate yet), so a pure
    threshold keeps all 2^kk prefixes.  As in fast_gl.multiclass_search, when
    more than ``max_width`` children pass the threshold only the heaviest
    ``max_width`` are retained (recorded in ``saturated_levels``); the final
    level is the EXACT leaf test |f_hat| >= tau/2, so tau remains the
    statistical parameter and the width cap is purely computational.

    bits: (m, n) uint8 (distinct rows); A: (m, V) per-row weighted target sums;
    norm_m: the ORIGINAL row count (Parseval normalization for collapsed data).

    n_search: search only the first n_search bit positions (the region with
    genuine within-fiber variation); the remaining bits act purely as
    conditioning, exactly the paper's context-GL split.  Characters on
    dataset-FIXED bits are deterministic per fiber (their chi is a per-fiber
    sign that squares away in W), so the dataset cannot certify them -- the
    searched region must be where the model filled.  When n_search < n the
    final EXACT filter |f_hat(S)|_2 >= tau/2 replaces the depth-n leaf test.

    Returns dict(status, masks, widths, saturated_levels).
    """
    import torch
    device = device or ("cuda" if torch.cuda.is_available() else "cpu")
    bits = np.asarray(bits, dtype=np.uint8)
    m, n = bits.shape
    A_t = torch.tensor(np.asarray(A, dtype=np.float32), device=device)
    signs = torch.tensor(1.0 - 2.0 * bits.astype(np.float32), device=device)  # (m, n)
    log_rhs_base = math.log(tau * tau / 4.0) + 2.0 * math.log(norm_m)
    live_masks = np.zeros((1, n), dtype=np.uint8)
    live_chi = torch.ones((1, m), dtype=torch.float32, device=device)
    widths, saturated = [], []
    n_search = n if n_search is None else min(n_search, n)
    gids = _iter_suffix_gids(bits)
    for k in range(n_search):
        kk = k + 1
        gid, ng = next(gids)
        gid_t = torch.tensor(gid, device=device)
        chi_add = live_chi * signs[:, k][None, :]
        chi_all = torch.cat([live_chi, chi_add], dim=0)
        masks_add = live_masks.copy()
        masks_add[:, k] = 1
        masks_all = np.concatenate([live_masks, masks_add])
        Q = _bucket_Q(chi_all, A_t, gid_t, ng).cpu().numpy()
        log_rhs = log_rhs_base - (n - kk) * math.log(2.0)
        with np.errstate(divide="ignore"):
            keep = np.flatnonzero(np.log(Q) >= log_rhs)
        if len(keep):
            # ON-DATA DEDUP: masks with identical chi columns are the same
            # function on the empirical measure (e.g. masks differing by a bit
            # that is constant across the data); without this, tie classes
            # flood the bounded frontier and crowd out distinct characters.
            cols = ((1.0 - chi_all[torch.tensor(keep, device=device)]) / 2.0)
            packed = np.packbits(cols.to(torch.uint8).cpu().numpy(), axis=1)
            view = np.ascontiguousarray(packed).view(
                [("", packed.dtype)] * packed.shape[1]).ravel()
            _, first = np.unique(view, return_index=True)
            keep = keep[np.sort(first)]
        if len(keep) > max_width:
            keep = keep[np.argpartition(-Q[keep], max_width)[:max_width]]
            saturated.append(kk)
        live_masks = masks_all[keep]
        live_chi = chi_all[torch.tensor(keep, device=device)]
        widths.append(int(len(live_masks)))
        if len(live_masks) == 0:
            break
        step = progress_every or max(1, n_search // 20)
        if kk % step == 0 or kk == n_search:
            # filled buckets so far = distinct live characters whose current
            # coefficient already clears the leaf bar
            live = live_masks[live_masks.any(axis=1)]
            cert = 0
            if len(live):
                chi = torch.tensor(parity_features(bits, live), device=device)
                ln = np.sqrt(((chi.T @ A_t) / norm_m).double().pow(2)
                             .sum(1).cpu().numpy())
                cert = int((ln >= tau / 2.0).sum())
            print(f"[gl] level {kk}/{n_search} width={len(live_masks)} ng={ng} "
                  f"filled_buckets={cert}", flush=True)
            if on_progress is not None:
                on_progress(kk, n_search, live.copy())
    masks = live_masks[live_masks.any(axis=1)]
    frontier_masks = masks.copy()
    norms_top = []
    if n_search < n and len(masks):
        # exact leaf filter on the truncated tree: |f_hat(S)|_2 >= tau/2.
        # Pre-filter norms are reported so a too-high tau shows exactly where
        # the dial must sit.
        chi = torch.tensor(parity_features(bits, masks), device=device)
        coeff = (chi.T @ A_t) / norm_m                              # (K, V)
        leaf_norms = np.sqrt(coeff.double().pow(2).sum(1).cpu().numpy())
        norms_top = sorted(leaf_norms.tolist(), reverse=True)[:10]
        masks = masks[leaf_norms >= tau / 2.0]
    return dict(status="ok", masks=masks.astype(np.uint8), widths=widths,
                saturated_levels=saturated, leaf_norms_top=norms_top,
                frontier_masks=frontier_masks.astype(np.uint8))


# ------------------------------------------------------------------ fit and eval

def parity_features(bits, masks):
    """(D, K) float32 Walsh characters (+-1) of the recovered masks."""
    masks = np.asarray(masks, dtype=np.int64)
    if len(masks) == 0:
        return np.zeros((len(bits), 0), dtype=np.float32)
    parity = (np.asarray(bits, dtype=np.int64) @ masks.T) % 2
    return (1.0 - 2.0 * parity).astype(np.float32)


def fit_softmax_slots(F_tr, P_tr, n_tr, F_va, P_va, n_va, steps=2000, lr=0.05,
                      device=None, seed=0, weight_decay=0.0):
    """Convex weighted soft-CE fit of slot logits = F @ W + b.  Adam + cosine
    decay + grad clip; bias initialized at the train log-unigram; best-val
    iterate kept (the model never ends worse than its initialization).
    weight_decay = L2 on W (the real-thing regularizer that lets us drop the
    feature gate and still generalize)."""
    import torch
    device = device or ("cuda" if torch.cuda.is_available() else "cpu")
    torch.manual_seed(seed)
    F_tr = torch.tensor(F_tr, dtype=torch.float32, device=device)
    F_va = torch.tensor(F_va, dtype=torch.float32, device=device)
    P_tr = torch.tensor(P_tr, dtype=torch.float32, device=device)
    P_va = torch.tensor(P_va, dtype=torch.float32, device=device)
    w_tr = torch.tensor(np.asarray(n_tr, dtype=np.float32), device=device)
    w_va = torch.tensor(np.asarray(n_va, dtype=np.float32), device=device)
    V = P_tr.shape[1]
    unigram = (P_tr * w_tr[:, None]).sum(0) / w_tr.sum()
    b = torch.log(unigram.clamp_min(1e-12)).clone().requires_grad_(True)
    Wm = torch.zeros((F_tr.shape[1], V), device=device, requires_grad=True)
    opt = torch.optim.Adam([{"params": [Wm], "weight_decay": weight_decay},
                            {"params": [b], "weight_decay": 0.0}], lr=lr)
    sched = torch.optim.lr_scheduler.CosineAnnealingLR(opt, T_max=steps)

    def val_kl():
        with torch.no_grad():
            lq = torch.log_softmax(F_va @ Wm + b, dim=1)
            lp = torch.log(P_va.clamp_min(1e-12))
            return float((w_va * (P_va * (lp - lq)).sum(1)).sum() / w_va.sum())

    best = init_val = val_kl()
    best_state = (Wm.detach().clone(), b.detach().clone())
    for _ in range(steps):
        lq = torch.log_softmax(F_tr @ Wm + b, dim=1)
        loss = -(w_tr * (P_tr * lq).sum(1)).sum() / w_tr.sum()
        opt.zero_grad(set_to_none=True)
        loss.backward()
        torch.nn.utils.clip_grad_norm_([Wm, b], 1.0)
        opt.step()
        sched.step()
        vk = val_kl()
        if vk < best:
            best = vk
            best_state = (Wm.detach().clone(), b.detach().clone())
    Wm_b, b_b = best_state
    return {"W": Wm_b.cpu().numpy(), "b": b_b.cpu().numpy(), "val_kl": best,
            "improved": bool(best < init_val - 1e-9)}


def _kl_progress(name, bits_tr, P_tr, n_tr, bits_va, P_va, n_va, uni_kl,
                 max_feats=256, steps=300):
    """Build an on_progress(level, ntot, masks) callback that fits the current
    frontier's characters and prints held-out KL vs the unigram floor -- so a
    long descent shows live progress toward the goal (KL dropping) instead of
    grinding silently.  Caps the quick fit at ``max_feats`` characters."""
    def cb(level, ntot, masks):
        if len(masks) == 0:
            print(f"[{name}] level {level}/{ntot}: width 0", flush=True)
            return
        mm = masks[:max_feats]
        fit = fit_softmax_slots(parity_features(bits_tr, mm), P_tr, n_tr,
                                parity_features(bits_va, mm), P_va, n_va,
                                steps=steps, lr=0.01)
        gain = uni_kl - fit["val_kl"]
        print(f"[{name}] level {level}/{ntot}: {len(masks)} chars -> "
              f"val_kl {fit['val_kl']:.4f} (unigram {uni_kl:.4f}, gain {gain:+.4f})",
              flush=True)
    return cb


def eval_slots(Wm, b, F, P, n):
    """Weighted KL(teacher||student) and slot top-1 agreement."""
    logits = F @ Wm + b
    lq = logits - np.log(np.exp(logits - logits.max(1, keepdims=True)).sum(1, keepdims=True)) \
        - logits.max(1, keepdims=True)
    P = np.asarray(P, dtype=np.float64)
    n = np.asarray(n, dtype=np.float64)
    lp = np.log(np.clip(P, 1e-12, None))
    kl = float((n * (P * (lp - lq)).sum(1)).sum() / n.sum())
    top1 = float((n * (P.argmax(1) == logits.argmax(1))).sum() / n.sum())
    return {"kl": kl, "top1": top1}


def fit_regression(F, Y, n, wd=1e-3, device=None):
    """Weighted ridge regression Y ~ F @ W + b, closed form (no OTHER slot, no
    softmax): solve (Fa^T diag(n) Fa + wd I) Wa = Fa^T diag(n) Y with Fa = [F | 1]
    and the bias row unpenalized.  Returns {W (p, d), b (d,)}."""
    import torch
    dev = device or ("cuda" if torch.cuda.is_available() else "cpu")
    F = torch.as_tensor(np.asarray(F), dtype=torch.float64, device=dev)
    Y = torch.as_tensor(np.asarray(Y), dtype=torch.float64, device=dev)
    w = torch.as_tensor(np.asarray(n), dtype=torch.float64, device=dev)
    D, p = F.shape
    Fa = torch.cat([F, torch.ones((D, 1), dtype=torch.float64, device=dev)], dim=1)
    Fw = Fa * w[:, None]
    G = Fa.t() @ Fw
    reg = wd * torch.eye(p + 1, dtype=torch.float64, device=dev)
    reg[-1, -1] = 0.0                                             # never penalize the bias
    Wa = torch.linalg.solve(G + reg, Fa.t() @ (w[:, None] * Y))
    return {"W": Wa[:-1].to(torch.float32).cpu().numpy(),
            "b": Wa[-1].to(torch.float32).cpu().numpy()}


def unembed_top1(H, Wu, device=None):
    """Full-vocab argmax token of each hidden vector H (D, d) under the frozen
    unembedding Wu (q, d): argmax over q of H @ Wu^T.  No slots, no OTHER."""
    import torch
    dev = device or ("cuda" if torch.cuda.is_available() else "cpu")
    Ht = torch.as_tensor(np.asarray(H), dtype=torch.float32, device=dev)
    Wt = torch.as_tensor(np.asarray(Wu), dtype=torch.float32, device=dev)
    out = np.empty(len(Ht), dtype=np.int64)
    for lo in range(0, len(Ht), 4096):                            # chunk the (D, q) logits
        out[lo:lo + 4096] = (Ht[lo:lo + 4096] @ Wt.t()).argmax(1).cpu().numpy()
    return out


def code_decode(C, codes, device=None):
    """Decode predicted B-bit codes C (D, B) to the nearest-Hamming token
    (codes (q, B)).  Hamming(hard, code) = (B - signs.dot)/2, so nearest token =
    argmax of the sign-correlation -- one chunked GPU matmul, no Python loop."""
    import torch
    dev = device or ("cuda" if torch.cuda.is_available() else "cpu")
    sp = torch.as_tensor(2.0 * (np.asarray(C) > 0.5) - 1.0, dtype=torch.float32, device=dev)
    sc = torch.as_tensor(2.0 * np.asarray(codes, dtype=np.float32) - 1.0, device=dev)
    out = np.empty(len(sp), dtype=np.int64)
    for lo in range(0, len(sp), 4096):
        out[lo:lo + 4096] = (sp[lo:lo + 4096] @ sc.t()).argmax(1).cpu().numpy()
    return out


def weighted_agreement(pred, tstar, n):
    """Fiber-count-weighted top-1 agreement between predicted and teacher tokens."""
    pred = np.asarray(pred); tstar = np.asarray(tstar); n = np.asarray(n, dtype=np.float64)
    return float((n * (pred == tstar)).sum() / n.sum())


# --------------------------------------------------- learned Fourier features

def harden_masks(sel_logits_list, n):
    """Argmax bit picks of STE selection logits [(K_d, d, n) arrays] -> XOR
    parity masks (K_tot, n) uint8 + realized degrees (K_tot,).  A bit picked
    twice cancels (x*x = 1), exactly the +-1 product semantics."""
    masks, degs = [], []
    for L in sel_logits_list:
        idx = np.asarray(L).argmax(-1)                             # (K_d, d)
        m = np.zeros((len(idx), n), np.uint8)
        for j in range(idx.shape[1]):
            m[np.arange(len(idx)), idx[:, j]] ^= 1
        masks.append(m); degs.append(m.sum(1))
    return np.concatenate(masks).astype(np.uint8), np.concatenate(degs).astype(np.int64)


def random_profile_masks(n, profile, seed=0):
    """Exact-degree random baseline: profile = (K_1, K_2, K_3, ...) counts per
    degree; each mask picks d DISTINCT bits uniformly.  (K_tot, n) uint8."""
    rng = np.random.default_rng(seed)
    rows = []
    for d, k in enumerate(profile, start=1):
        for _ in range(k):
            m = np.zeros(n, np.uint8)
            m[rng.choice(n, size=d, replace=False)] = 1
            rows.append(m)
    return np.stack(rows)


def decorrelation_penalty(F):
    """Diversity regularizer: mean squared off-diagonal of the batch Gram
    F^T F / B.  Exactly 0 for distinct parity characters on the full domain."""
    import torch
    B, K = F.shape
    if K < 2:
        return F.new_zeros(())
    C = F.t() @ F / B
    off = C - torch.diag_embed(torch.diagonal(C))
    return (off ** 2).sum() / (K * (K - 1))


def _ste_features(X, sel_logits, temp):
    """Straight-through parity features: X (B, n) +-1 floats, sel_logits list
    of (K_d, d, n) torch params.  Hard one-hot forward (features are EXACT
    +-1 parities of the argmax picks), softmax(L/temp) gradient.  (B, K_tot)."""
    import torch
    feats = []
    for L in sel_logits:
        K_d, d, n = L.shape
        S = torch.softmax(L / temp, dim=-1)
        hard = torch.nn.functional.one_hot(L.argmax(-1), n).to(S.dtype)
        P = hard + S - S.detach()                                  # STE
        feats.append((X @ P.reshape(K_d * d, n).t()).view(len(X), K_d, d).prod(-1))
    return torch.cat(feats, dim=1)


def learn_fourier_masks(bits, Hs, w, vm, profile=(512, 512, 256), lam=0.1,
                        steps=3000, lr=0.02, batch=8192, temp=(1.0, 0.1),
                        device=None, seed=0):
    """Learn WHICH parities to use by gradient descent: an explicit degree
    profile of STE-selected parity features, linear head to the standardized
    hidden target, count-weighted MSE + lam * activation decorrelation.
    Adam + cosine + clip, best-val iterate kept (fit_softmax_slots skeleton).
    Returns hardened masks/degrees + head at the best-val state."""
    import torch
    device = device or ("cuda" if torch.cuda.is_available() else "cpu")
    torch.manual_seed(seed)
    n = bits.shape[1]
    dY = Hs.shape[1]
    X = torch.tensor(1.0 - 2.0 * np.asarray(bits, np.float32), device=device)
    Y = torch.tensor(np.asarray(Hs, np.float32), device=device)
    wt = torch.tensor(np.asarray(w, np.float32), device=device)
    vmask = torch.tensor(np.asarray(vm, bool), device=device)
    tr = torch.nonzero(~vmask).ravel()
    K = int(sum(profile))
    gen = torch.Generator(device="cpu").manual_seed(seed)
    logits = [(0.1 * torch.randn(k, d, n, generator=gen)).to(device).requires_grad_(True)
              for d, k in enumerate(profile, start=1) if k > 0]
    W = torch.zeros((K, dY), device=device, requires_grad=True)
    b = torch.zeros(dY, device=device, requires_grad=True)
    opt = torch.optim.Adam(logits + [W, b], lr=lr)
    sched = torch.optim.lr_scheduler.CosineAnnealingLR(opt, T_max=steps)

    def val_mse():
        with torch.no_grad():
            out, wsum = 0.0, 0.0
            for lo in range(0, int(vmask.sum()), 16384):
                idx = torch.nonzero(vmask).ravel()[lo:lo + 16384]
                Fv = _ste_features(X[idx], logits, temp[1])
                out += float((wt[idx][:, None] * (Fv @ W + b - Y[idx]) ** 2).sum())
                wsum += float(wt[idx].sum())
            return out / (wsum * dY)

    best = init_val = val_mse()
    best_state = [t.detach().clone() for t in logits + [W, b]]
    history = [(0, best)]
    for t in range(steps):
        T = temp[0] + (temp[1] - temp[0]) * t / max(steps - 1, 1)
        bi = tr[torch.randint(len(tr), (min(batch, len(tr)),),
                              generator=gen).to(device)]
        F = _ste_features(X[bi], logits, T)
        pred = F @ W + b
        mse = (wt[bi][:, None] * (pred - Y[bi]) ** 2).sum() / (wt[bi].sum() * dY)
        loss = mse + lam * decorrelation_penalty(F)
        opt.zero_grad(set_to_none=True)
        loss.backward()
        torch.nn.utils.clip_grad_norm_(logits + [W, b], 1.0)
        opt.step()
        sched.step()
        if (t + 1) % 50 == 0 or t + 1 == steps:
            vmse = val_mse()
            history.append((t + 1, vmse))
            if vmse < best:
                best = vmse
                best_state = [x.detach().clone() for x in logits + [W, b]]
    with torch.no_grad():
        for x, s in zip(logits + [W, b], best_state):
            x.copy_(s)
    logits_np = [L.detach().cpu().numpy() for L in logits]
    masks, deg = harden_masks(logits_np, n)
    nominal = np.concatenate([np.full(k, d) for d, k in enumerate(profile, 1) if k > 0])
    return {"masks": masks, "deg": deg, "W": W.detach().cpu().numpy(),
            "b": b.detach().cpu().numpy(), "val_mse": best, "init_val_mse": init_val,
            "n_collapsed": int((deg < nominal).sum()), "history": history}


def fit_mlp_hidden(bits, Hs, w, vm, bits_te, hidden=2048, steps=3000, lr=1e-3,
                   batch=8192, device=None, seed=0):
    """Unconstrained ceiling baseline: 2-hidden-layer GELU MLP on the raw +-1
    bits -> standardized hidden target, count-weighted MSE, best-val kept.
    Measures what ANY function of these bits can reach at this data size --
    the parity model is judged against this, not just against random masks."""
    import torch
    device = device or ("cuda" if torch.cuda.is_available() else "cpu")
    torch.manual_seed(seed)
    n, dY = bits.shape[1], Hs.shape[1]
    X = torch.tensor(1.0 - 2.0 * np.asarray(bits, np.float32), device=device)
    Y = torch.tensor(np.asarray(Hs, np.float32), device=device)
    wt = torch.tensor(np.asarray(w, np.float32), device=device)
    vmask = torch.tensor(np.asarray(vm, bool), device=device)
    tr = torch.nonzero(~vmask).ravel()
    va = torch.nonzero(vmask).ravel()
    net = torch.nn.Sequential(
        torch.nn.Linear(n, hidden), torch.nn.GELU(),
        torch.nn.Linear(hidden, hidden), torch.nn.GELU(),
        torch.nn.Linear(hidden, dY)).to(device)
    opt = torch.optim.Adam(net.parameters(), lr=lr)
    sched = torch.optim.lr_scheduler.CosineAnnealingLR(opt, T_max=steps)
    gen = torch.Generator(device="cpu").manual_seed(seed)

    def val_mse():
        with torch.no_grad():
            out, wsum = 0.0, 0.0
            for lo in range(0, len(va), 16384):
                idx = va[lo:lo + 16384]
                out += float((wt[idx][:, None] * (net(X[idx]) - Y[idx]) ** 2).sum())
                wsum += float(wt[idx].sum())
            return out / (wsum * dY)

    best = val_mse()
    best_state = {k: v.detach().clone() for k, v in net.state_dict().items()}
    for t in range(steps):
        bi = tr[torch.randint(len(tr), (min(batch, len(tr)),),
                              generator=gen).to(device)]
        loss = (wt[bi][:, None] * (net(X[bi]) - Y[bi]) ** 2).sum() / (wt[bi].sum() * dY)
        opt.zero_grad(set_to_none=True)
        loss.backward()
        torch.nn.utils.clip_grad_norm_(net.parameters(), 1.0)
        opt.step()
        sched.step()
        if (t + 1) % 50 == 0 or t + 1 == steps:
            vmse = val_mse()
            if vmse < best:
                best = vmse
                best_state = {k: v.detach().clone() for k, v in net.state_dict().items()}
    net.load_state_dict(best_state)
    with torch.no_grad():
        Xte = torch.tensor(1.0 - 2.0 * np.asarray(bits_te, np.float32), device=device)
        pred = np.concatenate([net(Xte[lo:lo + 16384]).cpu().numpy()
                               for lo in range(0, len(Xte), 16384)])
    return {"pred_te": pred, "val_mse": best}


# ------------------------------------------ gradient-sensed character harvest

def masks_to_indices(masks, width=4):
    """(K, n) uint8 parity masks -> (K, width) int16 SORTED bit indices, -1
    padded (the canonical character key; degree must be <= width)."""
    masks = np.asarray(masks, dtype=np.uint8)
    assert masks.sum(1).max(initial=0) <= width
    idx = np.full((len(masks), width), -1, dtype=np.int16)
    for j in range(len(masks)):
        sel = np.flatnonzero(masks[j])
        idx[j, :len(sel)] = sel
    return idx


def dedupe_indices(idx, psi, seen):
    """Drop degree-0 rows, in-batch duplicates and characters already in
    ``seen`` (a set of canonical key bytes, updated IN PLACE; first wins).
    Returns the kept (idx, psi)."""
    idx = np.asarray(idx, dtype=np.int16); psi = np.asarray(psi)
    keep = []
    for j, row in enumerate(idx):
        sel = np.sort(row[row >= 0])
        if len(sel) == 0:
            continue
        key = sel.astype(np.int16).tobytes()
        if key in seen:
            continue
        seen.add(key)
        keep.append(j)
    return idx[keep], psi[keep]


def xor_parity_features(bits, idx):
    """(D, K) ±1 Walsh characters from (K, <=4) bit-index rows (-1 = unused):
    XOR of the selected bits, so a duplicated index cancels exactly like the
    ±1 product.  torch-in/torch-out on the tensor's device, else numpy.
    Materializes (D, K) -- callers tile at scale."""
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


def _weighted_psi(F, G, w):
    """Fourier weight per character: psi_j = || sum_i w_i F_ij G_i / sum w ||^2.
    On a unit-normalized centered target Parseval bounds sum_S psi_S <= 1."""
    c = (F * w[:, None]).t() @ G / w.sum()
    return (c.float() ** 2).sum(1)


def _soft_features(X, sel_logits, temp):
    """Fully SOFT relaxation of the selection product: each slot is a softmax
    mixture over bits, feature = product of mixture values.  Its psi senses the
    SUPERPOSITION of all characters under the slots' joint distribution -- so a
    degree-d slot gets gradient toward a heavy character even when no partner
    slot has locked yet (the hard/STE product has exactly-zero gradient there).
    Anneal temp down and the mixture concentrates onto the hard character."""
    import torch
    feats = []
    for L in sel_logits:
        K_d, d, n = L.shape
        S = torch.softmax(L / temp, dim=-1)
        feats.append((X @ S.reshape(K_d * d, n).t()).view(len(X), K_d, d).prod(-1))
    return torch.cat(feats, dim=1)


def learn_sense_chunk(bits, G, w, vm, profile=(4096, 4096, 4096, 4096), lam=0.1,
                      steps=300, min_steps=64, patience=3, check_every=16,
                      lr=0.02, batch=4096, temp=(1.0, 0.1), npairs=None,
                      device=None, seed=0, log_fn=None):
    """One gradient-sense harvest round.  Fresh selection logits with an
    explicit per-degree profile (deg 1..len(profile)); influence objective
    -mean_j log(psi_j + eps) (magnitude-free: every slot must find SOME real
    Fourier weight); training senses through the SOFT annealed product
    (_soft_features -- the hard/STE product has zero gradient toward a char
    until all partner slots align); diversity = squared weighted correlation
    of randomly sampled feature pairs; early stopping on val mean-log-psi of
    the HARD characters.  Cross-round anti-duplication is the CALLER's job via
    DEFLATION: pass the residual G - sum(accepted c_S chi_S) as the target and
    re-finding an accepted character earns psi ~ 0 by construction (gradient
    matching pursuit).  Returns in-round-deduped hardened characters as index
    rows + their held-out psi on THIS target."""
    import torch
    device = device or ("cuda" if torch.cuda.is_available() else "cpu")
    torch.manual_seed(seed)
    bits_t = bits if torch.is_tensor(bits) else \
        torch.tensor(np.asarray(bits, np.uint8), device=device)
    dev = bits_t.device
    G_t = G if torch.is_tensor(G) else torch.tensor(np.asarray(G, np.float32), device=dev)
    w_t = torch.tensor(np.asarray(w, np.float32), device=dev)
    vmask = torch.tensor(np.asarray(vm, bool), device=dev)
    tr = torch.nonzero(~vmask).ravel()
    va = torch.nonzero(vmask).ravel()[:32768]                     # capped val subsample
    n = bits_t.shape[1]
    K = int(sum(profile))
    npairs = npairs or 4 * K
    gen = torch.Generator(device="cpu").manual_seed(seed)
    logits = [(0.1 * torch.randn(k, d, n, generator=gen)).to(dev).requires_grad_(True)
              for d, k in enumerate(profile, start=1) if k > 0]
    opt = torch.optim.Adam(logits, lr=lr)
    sched = torch.optim.lr_scheduler.CosineAnnealingLR(opt, T_max=steps)
    amp = torch.autocast(device_type="cuda", dtype=torch.bfloat16,
                         enabled=dev.type == "cuda")

    def val_mean_log_psi(T):
        # early-stop metric follows the SOFT sensing objective at the current
        # temperature: hard-char psi sits at the noise floor while mixtures are
        # still concentrating and would cut every round at min_steps
        with torch.no_grad(), amp:
            F = _soft_features(1.0 - 2.0 * bits_t[va].float(), logits, T)
            psi = _weighted_psi(F, G_t[va].float(), w_t[va])
        return float(torch.log(psi + 1e-8).mean())

    best, bad, stopped_at, history = -np.inf, 0, steps, []
    for t in range(steps):
        T = temp[0] + (temp[1] - temp[0]) * t / max(steps - 1, 1)
        bi = tr[torch.randint(len(tr), (min(batch, len(tr)),),
                              generator=gen).to(dev)]
        wb = w_t[bi]
        with amp:
            F = _soft_features(1.0 - 2.0 * bits_t[bi].float(), logits, T)
            psi = _weighted_psi(F, G_t[bi].float(), wb)
            pi = torch.randint(K, (npairs,), generator=gen).to(dev)
            pj = torch.randint(K, (npairs,), generator=gen).to(dev)
            ok = pi != pj
            corr = ((F[:, pi] * F[:, pj]) * wb[:, None]).sum(0).float() / wb.sum()
            div = (corr[ok] ** 2).mean()
            loss = -torch.log(psi + 1e-8).mean() + lam * div
        opt.zero_grad(set_to_none=True)
        loss.backward()
        torch.nn.utils.clip_grad_norm_(logits, 1.0)
        opt.step()
        sched.step()
        if log_fn is not None and t % check_every == 0:
            log_fn(t, {"mean_log_psi": float(torch.log(psi + 1e-8).mean()),
                       "mean_psi": float(psi.mean()), "pair_corr2": float(div),
                       "temp": T, "lr": float(sched.get_last_lr()[0])})
        if (t + 1) % check_every == 0 and t + 1 >= min_steps:
            m = val_mean_log_psi(temp[1])                          # fixed reference temp:
            history.append((t + 1, m))                             # comparable across checks
            if m > best + 1e-3:
                best, bad = m, 0
            else:
                bad += 1
            if bad >= patience:
                stopped_at = t + 1
                break
    masks, _ = harden_masks([L.detach().cpu().numpy() for L in logits], n)
    idx = masks_to_indices(masks)
    with torch.no_grad():
        psi_va = _weighted_psi(xor_parity_features(bits_t[va], idx),
                               G_t[va].float(), w_t[va]).cpu().numpy()
    idx_d, psi_d = dedupe_indices(idx, psi_va, set())
    return {"idx": idx_d, "psi": psi_d, "stopped_at": stopped_at, "history": history}


def fourier_coefficients(bits, G, w, idx, device=None, char_chunk=8192,
                         ctx_chunk=65536):
    """PLAIN weighted dataset Fourier coefficients of the (already centered)
    target: c_S = sum_i w_i chi_S(x_i) G_i / sum_i w_i.  Double-tiled; returns
    (K, dY) float32."""
    import torch
    device = device or ("cuda" if torch.cuda.is_available() else "cpu")
    bits_t = bits if torch.is_tensor(bits) else \
        torch.tensor(np.asarray(bits, np.uint8), device=device)
    dev = bits_t.device
    G_t = G if torch.is_tensor(G) else torch.tensor(np.asarray(G, np.float32), device=dev)
    w_t = torch.tensor(np.asarray(w, np.float32), device=dev)
    idx = np.asarray(idx, np.int16)
    C = torch.zeros((len(idx), G_t.shape[1]), dtype=torch.float32, device=dev)
    for klo in range(0, len(idx), char_chunk):
        sub = idx[klo:klo + char_chunk]
        for dlo in range(0, len(bits_t), ctx_chunk):
            F = xor_parity_features(bits_t[dlo:dlo + ctx_chunk], sub)
            wG = G_t[dlo:dlo + ctx_chunk].float() * w_t[dlo:dlo + ctx_chunk, None]
            C[klo:klo + len(sub)] += F.t() @ wG
    return (C / float(w_t.sum())).cpu().numpy()


def sequential_deflate(bits, G, w, idx, device=None, char_chunk=1024, block=1,
                       masks=None):
    """TRUE matching-pursuit deflation: subtract each character's plain
    weighted Fourier coefficient ONE AT A TIME against the CURRENT residual,
    in the given order.  This is the fix for the batch-deflation divergence
    (2026-07-15 smoke): batch `G -= F(F^T W G / sum w)` computes every
    coefficient against one residual snapshot and AMPLIFIES an on-data
    duplicate cluster of size m by (1-m)^2; sequentially, a later duplicate
    sees psi ~ 0 and gets coefficient ~ 0, and a constant character on the
    centered target gets coefficient 0.  Each rank-1 update is a 1-D
    projection, so the residual can never grow.
    ``block > 1`` = block-OMP: exact joint LS projection per block (Gram
    solve, tiny jitter -- a duplicate cluster's coefficient SPLITS across
    copies, summing correctly), Gauss-Seidel across blocks.  Same guarantees
    (every block update is a projection), ~block x fewer kernel launches:
    at 400k chars x 1M contexts the rank-1 loop is ~40 min of launch+HBM
    overhead, block=512 is ~2 min.  Returns (C (K, dY) float32, G_out) with
    G mutated in place when given as a torch tensor."""
    import torch
    device = device or ("cuda" if torch.cuda.is_available() else "cpu")
    was_np = not torch.is_tensor(G)
    bits_t = bits if torch.is_tensor(bits) else \
        torch.tensor(np.asarray(bits, np.uint8), device=device)
    dev = bits_t.device
    G_t = G if torch.is_tensor(G) else torch.tensor(np.asarray(G, np.float32), device=dev)
    w_t = torch.tensor(np.asarray(w, np.float32), device=dev)
    wsum = float(w_t.sum())
    if masks is not None:
        masks_t = torch.tensor(np.asarray(masks, np.float32), device=dev)
        idx = np.zeros((len(masks_t), 4), np.int16)               # length only

        def _feat(klo, hi):
            # dense-mask parities of ANY degree: fp32 GEMM is exact (counts
            # <= n << 2^24), (1 - 2*((bits @ m^T) % 2))
            out = torch.empty((len(bits_t), hi - klo), device=dev)
            for lo in range(0, len(bits_t), 262144):
                bf = bits_t[lo:lo + 262144].float()
                out[lo:lo + 262144] = 1.0 - 2.0 * \
                    ((bf @ masks_t[klo:hi].t()) % 2.0)
            return out
    else:
        idx = np.asarray(idx, np.int16)

        def _feat(klo, hi):
            return xor_parity_features(bits_t, idx[klo:hi])
    C = np.empty((len(idx), G_t.shape[1]), np.float32)
    if block > 1:
        for klo in range(0, len(idx), block):
            F = _feat(klo, min(klo + block, len(idx)))
            Fw = F * w_t[:, None]
            S = (F.t() @ Fw) / wsum
            bvec = (Fw.t() @ G_t) / wsum
            S += 1e-4 * torch.eye(len(S), device=dev)              # dup clusters: singular
            C_b = torch.linalg.solve(S.double(), bvec.double()).float()
            G_t -= F @ C_b
            C[klo:klo + len(C_b)] = C_b.cpu().numpy()
        return C, (G_t.cpu().numpy() if was_np else G_t)
    for klo in range(0, len(idx), char_chunk):
        F = _feat(klo, min(klo + char_chunk, len(idx)))
        for j in range(F.shape[1]):
            fw = F[:, j] * w_t
            c = (fw @ G_t) / wsum
            G_t -= torch.outer(F[:, j], c)
            C[klo + j] = c.cpu().numpy()
    return C, (G_t.cpu().numpy() if was_np else G_t)


def fit_deg1_exact(bits, G, w, device=None, wd=1e-3, ctx_chunk=131072):
    """Closed-form weighted LS of the target on ALL n degree-1 characters +
    bias, with TILED fp32 Gram accumulation (fit_regression materializes the
    D x n design in fp64 -- 65 GB at D=1M, n=8k) and one fp64 solve.
    Subtracting this fit leaves the train residual with NO degree-1 content.
    Returns {"W" (n, dY), "b" (dY,)}."""
    import torch
    device = device or ("cuda" if torch.cuda.is_available() else "cpu")
    bits_t = bits if torch.is_tensor(bits) else \
        torch.tensor(np.asarray(bits, np.uint8), device=device)
    dev = bits_t.device
    G_t = G if torch.is_tensor(G) else torch.tensor(np.asarray(G, np.float32), device=dev)
    w_t = torch.tensor(np.asarray(w, np.float32), device=dev)
    n, dY = bits_t.shape[1], G_t.shape[1]
    S = torch.zeros((n + 1, n + 1), dtype=torch.float64, device=dev)
    bvec = torch.zeros((n + 1, dY), dtype=torch.float64, device=dev)
    for lo in range(0, len(bits_t), ctx_chunk):
        X = 1.0 - 2.0 * bits_t[lo:lo + ctx_chunk].float()
        Xa = torch.cat([X, torch.ones((len(X), 1), device=dev)], dim=1)
        wc = w_t[lo:lo + ctx_chunk]
        S += (Xa.t() @ (Xa * wc[:, None])).double()
        bvec += (Xa.t() @ (G_t[lo:lo + ctx_chunk].float() * wc[:, None])).double()
    reg = wd * torch.eye(n + 1, dtype=torch.float64, device=dev)
    reg[-1, -1] = 0.0                                             # never penalize the bias
    Wa = torch.linalg.solve(S + reg, bvec)
    return {"W": Wa[:-1].to(torch.float32).cpu().numpy(),
            "b": Wa[-1].to(torch.float32).cpu().numpy()}


def _sparsity_stats(E):
    """Concentration report for per-character energies ||c_j||^2: effective
    character count (participation ratio) + k needed for 50/90/99% of the
    captured mass -- 'is the model ACTUALLY sparse'."""
    e = np.sort(np.asarray(E, np.float64))[::-1]
    tot = float(e.sum())
    if tot <= 0 or len(e) == 0:
        return {"N_eff": 0.0, "k50": 0, "k90": 0, "k99": 0, "total_energy": 0.0}
    cum = np.cumsum(e)
    return {"N_eff": float(tot ** 2 / (e ** 2).sum()),
            "k50": int(np.searchsorted(cum, 0.50 * tot) + 1),
            "k90": int(np.searchsorted(cum, 0.90 * tot) + 1),
            "k99": int(np.searchsorted(cum, 0.99 * tot) + 1),
            "total_energy": tot}


def _logspace_ste_chars(bits_f, theta, eps=1e-3):
    """Learned characters with STE to the CLOSEST character function, product
    computed in LOG space for stability: inclusion gates m = sigmoid(theta)
    over the bits; per-bit factor is (1-2m_i) when bit i is set, 1 otherwise,
    so log|phi(x)| = <b(x), log max(|1-2m|, eps)> -- ONE GEMM, linear in the
    bit vector.  Forward value is the EXACT +-1 parity of the hardened mask
    (m > 0.5); gradients flow through sign * exp(logmag).  bits_f: (B, n)
    float 0/1; theta: (K, n).  Returns (B, K) +-1 with grad."""
    import torch
    m = torch.sigmoid(theta)
    fac = torch.clamp((1.0 - 2.0 * m).abs(), min=eps)
    logmag = bits_f @ torch.log(fac).t()                          # (B, K)
    mag = torch.exp(logmag)
    hard = (m > 0.5).to(bits_f.dtype)
    sign = (1.0 - 2.0 * ((bits_f @ hard.t()) % 2.0)).detach()     # exact parity
    return sign * (mag + (1.0 - mag).detach())


def fourier_learn_chars(bits, G, w, vm, K=16384, rho=0.5, lam=0.1, l1=0.0,
                        gamma=0.1, steps=2000, lr=0.05, batch=8192, eps=1e-3,
                        warm_masks=None, warm_C=None, device=None, seed=0,
                        log_fn=None):
    """Directly LEARN the Fourier decomposition of G: K characters of ANY
    degree (inclusion gates, log-space product, STE to the closest character)
    x LEARNED coefficients bounded by rho (c = rho * tanh(u)).  Anti-collapse:
    decorrelation_penalty on the hard character activations (+ optional L1 on
    coefficients).  Gates init near EMPTY masks (m ~ 0.02): random init makes
    the log-product underflow; degree grows greedily from the constant char.
    Val-gated best state.  Returns {"masks", "C", "val_mse", "history"}."""
    import torch
    device = device or ("cuda" if torch.cuda.is_available() else "cpu")
    torch.manual_seed(seed)
    bits_t = bits if torch.is_tensor(bits) else \
        torch.tensor(np.asarray(bits, np.uint8), device=device)
    dev = bits_t.device
    G_t = G if torch.is_tensor(G) else torch.tensor(np.asarray(G, np.float32), device=dev)
    w_t = torch.tensor(np.asarray(w, np.float32), device=dev)
    vmask = torch.tensor(np.asarray(vm, bool), device=dev)
    tr = torch.nonzero(~vmask).ravel()
    va = torch.nonzero(vmask).ravel()[:32768]
    n, dY = bits_t.shape[1], G_t.shape[1]
    gen = torch.Generator(device="cpu").manual_seed(seed)
    theta0 = -4.0 + 0.5 * torch.randn(K, n, generator=gen)        # near-empty masks
    if warm_masks is not None and len(warm_masks):
        wm = torch.tensor(np.asarray(warm_masks[:K // 2], np.float32))
        theta0[: len(wm)] = torch.where(wm > 0.5, torch.tensor(4.0),
                                        torch.tensor(-4.0))
    theta = theta0.to(dev).requires_grad_(True)
    u0 = 0.01 * torch.randn(K, dY, generator=gen)
    if warm_C is not None and len(warm_C):
        # anchor warm slots at their CALCULATED coefficients: without this the
        # sensing term pulls triples DOWN to heavier pairs still present in
        # the residual (v2: all warm triples collapsed to deg-2)
        wc = torch.tensor(np.asarray(warm_C[:K], np.float32))
        u0[: len(wc)] = torch.atanh(torch.clamp(wc / rho, -0.999, 0.999))
    u = u0.to(dev).requires_grad_(True)
    opt = torch.optim.Adam([theta, u], lr=lr)
    sched = torch.optim.lr_scheduler.CosineAnnealingLR(opt, T_max=steps)

    def val_mse():
        with torch.no_grad():
            out, wsum = 0.0, 0.0
            for lo in range(0, len(va), 16384):
                idx = va[lo:lo + 16384]
                phi = _logspace_ste_chars(bits_t[idx].float(), theta, eps)
                pred = phi @ (rho * torch.tanh(u))
                out += float((w_t[idx][:, None] * (pred - G_t[idx].float()) ** 2).sum())
                wsum += float(w_t[idx].sum())
            return out / (wsum * dY)

    best = val_mse()
    best_state = (theta.detach().clone(), u.detach().clone())
    history = [(0, best)]
    for t in range(steps):
        bi = tr[torch.randint(len(tr), (min(batch, len(tr)),),
                              generator=gen).to(dev)]
        phi = _logspace_ste_chars(bits_t[bi].float(), theta, eps)
        C = rho * torch.tanh(u)
        pred = phi @ C
        wb = w_t[bi]
        resid = G_t[bi].float() - pred
        mse = (wb[:, None] * resid ** 2).sum() / (wb.sum() * dY)
        loss = mse + lam * decorrelation_penalty(phi)
        if gamma:
            # magnitude-free sensing pressure (grad-sense lesson): a slot with
            # a ~0 coefficient gets NO mask gradient from the MSE (coefficient
            # and mask starve each other); -log psi forces every slot toward
            # SOME residual-correlated character first
            # leave-one-out: sense against the residual PLUS the slot's own
            # contribution (phi^2 = 1), else a slot self-cancels its signal by
            # fitting its tiny projection and the completion ramp dies
            corr = (phi * wb[:, None]).t() @ resid.detach() / wb.sum() + C.detach()
            loss = loss - gamma * torch.log((corr ** 2).sum(1) + 1e-8).mean()
        if l1:
            loss = loss + l1 * C.abs().mean()
        opt.zero_grad(set_to_none=True)
        loss.backward()
        torch.nn.utils.clip_grad_norm_([theta, u], 1.0)
        opt.step()
        sched.step()
        if log_fn is not None and t % 50 == 0:
            with torch.no_grad():
                mh = torch.sigmoid(theta) > 0.5
                dg = mh.sum(1).float()
                moved = float((mh != (theta0.to(dev) > 0)).any(1).float().mean()) \
                    if warm_masks is not None else float("nan")
                cn = C.detach().norm(dim=1)
            log_fn(t, {"train_mse": float(mse), "lr": float(sched.get_last_lr()[0]),
                       "pair_decorr": float(decorrelation_penalty(phi)),
                       "deg_mean": float(dg.mean()), "deg_max": float(dg.max()),
                       "n_deg_gt4": int((dg > 4).sum()),
                       "masks_moved_frac": moved,
                       "coef_norm_mean": float(cn.mean()),
                       "coef_norm_max": float(cn.max())})
        if (t + 1) % 100 == 0:
            vmse = val_mse()
            history.append((t + 1, vmse))
            if log_fn is not None:
                log_fn(t + 1, {"val_mse": vmse, "best_val_mse": min(best, vmse)})
            if vmse < best:
                best = vmse
                best_state = (theta.detach().clone(), u.detach().clone())
    theta_b, u_b = best_state
    masks = (torch.sigmoid(theta_b) > 0.5).cpu().numpy().astype(np.uint8)
    return {"masks": masks, "C": (rho * torch.tanh(u_b)).cpu().numpy(),
            "val_mse": best, "history": history}


def deg2_exact_psi(bits, G, w, r=64, device=None, ctx_chunk=262144, proj=None):
    """EXACT degree-2 spectroscopy, gradient-free: the plain Fourier
    coefficient of EVERY pair character chi_a*chi_b on target dim d is an
    entry of the n x n map M_d = X^T (g_d w X) / sum w, so one GEMM per dim
    enumerates all n(n-1)/2 pairs at once.  The target is first projected
    onto its top-r weighted-PCA dims (psi2 is then a LOWER bound on the true
    pair weight; the retained mass is returned).  Per-pair estimation noise
    ~ mass_r / D -- far below any val-subsample sensing gate.  Returns
    (psi2 (n, n) float32 symmetric with zero diagonal, mass_r)."""
    import torch
    device = device or ("cuda" if torch.cuda.is_available() else "cpu")
    bits_t = bits if torch.is_tensor(bits) else \
        torch.tensor(np.asarray(bits, np.uint8), device=device)
    dev = bits_t.device
    G_t = G if torch.is_tensor(G) else torch.tensor(np.asarray(G, np.float32), device=dev)
    w_t = torch.tensor(np.asarray(w, np.float32), device=dev)
    wsum = float(w_t.sum())
    D, n = bits_t.shape
    if proj is not None:
        # fixed orthonormal projection supplied by the caller (still a valid
        # lower bound; anchored triple sweeps share ONE global projection --
        # the rowwise sign flip commutes with it, saving a PCA per anchor)
        U = proj if torch.is_tensor(proj) else \
            torch.as_tensor(np.asarray(proj, np.float32), device=dev)
        r_eff = U.shape[1]
        mass_r = 0.0
        for lo in range(0, D, ctx_chunk):
            P = G_t[lo:lo + ctx_chunk].float() @ U
            mass_r += float(((P ** 2).sum(1) * w_t[lo:lo + ctx_chunk]).sum())
        mass_r /= wsum
    else:
        r_eff = int(min(r, G_t.shape[1]))
        # weighted PCA of the target -> top-r directions
        C = torch.zeros((G_t.shape[1],) * 2, dtype=torch.float64, device=dev)
        for lo in range(0, D, ctx_chunk):
            Gc = G_t[lo:lo + ctx_chunk].float()
            C += (Gc.t() @ (Gc * w_t[lo:lo + ctx_chunk, None])).double()
        evals, evecs = torch.linalg.eigh(C / wsum)
        U = evecs[:, -r_eff:].to(torch.float32)                   # (dY, r)
        mass_r = float(evals[-r_eff:].sum())
    ft = torch.bfloat16 if dev.type == "cuda" else torch.float32
    psi2 = torch.zeros((n, n), dtype=torch.float32, device=dev)
    for d in range(r_eff):
        M = torch.zeros((n, n), dtype=torch.float32, device=dev)
        for lo in range(0, D, ctx_chunk):
            X = (1.0 - 2.0 * bits_t[lo:lo + ctx_chunk].to(ft))
            pd = (G_t[lo:lo + ctx_chunk].float() @ U[:, d]) * w_t[lo:lo + ctx_chunk]
            M += (X.t() @ (X * pd[:, None].to(ft))).float()
        psi2 += (M / wsum) ** 2
    psi2.fill_diagonal_(0.0)
    return psi2.cpu().numpy(), mass_r


def _full_kl(H_pred, H_true, Wu, w, device=None, rows=1024):
    """Weighted mean FULL-VOCAB KL(teacher || student) between the softmaxes of
    two hidden-state batches under the frozen unembedding.  NOT scale-invariant
    (softmax temperature): normalized predictions must be rescaled by the mean
    teacher norm before calling."""
    import torch
    device = device or ("cuda" if torch.cuda.is_available() else "cpu")
    Wt = torch.as_tensor(np.asarray(Wu, np.float32), device=device)
    w = np.asarray(w, dtype=np.float64)
    out = 0.0
    for lo in range(0, len(H_true), rows):
        lt = torch.as_tensor(np.asarray(H_true[lo:lo + rows], np.float32),
                             device=device) @ Wt.t()
        ls = torch.as_tensor(np.asarray(H_pred[lo:lo + rows], np.float32),
                             device=device) @ Wt.t()
        lp, lq = torch.log_softmax(lt, 1), torch.log_softmax(ls, 1)
        kl = (lp.exp() * (lp - lq)).sum(1).double().cpu().numpy()
        out += float((w[lo:lo + rows] * kl).sum())
    return out / float(w.sum())


def reconstruct_ladder(bits, idx, C, c0, order, ks, Wu, tstar, w, device=None,
                       char_chunk=8192, kl_ref=None, base=None):
    """Cumulative plain-Fourier reconstruction F_hat = base + c0 + sum c_S
    chi_S, walking characters in ``order`` and snapshotting full-vocab top-1
    at each k in ks (single pass).  ``base`` (D, dY) is a fixed starting
    prediction (e.g. the exact degree-1 model).  kl_ref=(H_true, scale)
    additionally reports KL(teacher || student) with predictions rescaled by
    ``scale``.  Returns [{'k', 'top1'[, 'kl']}, ...]."""
    import torch
    device = device or ("cuda" if torch.cuda.is_available() else "cpu")
    bits_t = bits if torch.is_tensor(bits) else \
        torch.tensor(np.asarray(bits, np.uint8), device=device)
    dev = bits_t.device
    C_t = torch.as_tensor(np.asarray(C, np.float32), device=dev)
    order = np.asarray(order)
    if base is not None:
        Fhat = torch.as_tensor(np.asarray(base, np.float32), device=dev).clone() \
            if not torch.is_tensor(base) else base.clone()
    else:
        Fhat = torch.zeros((len(bits_t), C_t.shape[1]), dtype=torch.float32, device=dev)
    out, pos = [], 0
    for k in [int(min(k, len(order))) for k in sorted(set(ks))]:
        while pos < k:
            sub = order[pos:pos + min(char_chunk, k - pos)]
            Fhat += xor_parity_features(bits_t, np.asarray(idx)[sub]) @ C_t[sub]
            pos += len(sub)
        H_pred = Fhat.cpu().numpy() + np.asarray(c0)[None, :]
        rec = {"k": k, "top1": weighted_agreement(unembed_top1(H_pred, Wu), tstar, w)}
        if kl_ref is not None:
            rec["kl"] = _full_kl(H_pred * kl_ref[1], kl_ref[0], Wu, w, device=dev)
        out.append(rec)
    return out


# ------------------------------------------- sensitivity -> degree bounds core

def _sens_from_groups(F):
    """Per-fiber unbiased sensitivity estimates from g iid resamples of ONE
    coordinate: F (M, g, V) slot dists -> (M,) float64 sample variances
    (ddof=1) per slot, summed over slots.  Mean over fibers estimates
    Sens_b = E_{x_{-b}}[Var_{a~nu_b}(f(x^{b<-a}))]."""
    F = np.asarray(F, dtype=np.float64)
    mu = F.mean(axis=1, keepdims=True)
    return ((F - mu) ** 2).sum(axis=(1, 2)) / (F.shape[1] - 1)


def _sens_from_top1(T):
    """Top-1 sensitivity core (FULL-VOCAB argmax -- deliberately NO slot
    projection anywhere in the sensitivity arc; see stream_top1).  T (M, g)
    int argmax ids from g iid resamples of one coordinate -> (M,) float64,
    the ddof-1 group variance of the ONE-HOT top-1 function:
    g/(g-1) * (1 - sum_t phat_t^2) -- the unbiased probability that two
    independent resamples flip the teacher's top-1 token."""
    T = np.asarray(T)
    g = T.shape[1]
    eq = (T[:, :, None] == T[:, None, :]).sum(axis=(1, 2))
    return (g / (g - 1)) * (1.0 - eq / float(g * g))


def _top1_variance(T0):
    """Total variance of the one-hot top-1 function across fibers (same
    estimator as _sens_from_top1, one draw per fiber): the bias-corrected
    probability that two random contexts disagree on the top-1 token."""
    T0 = np.asarray(T0).ravel()
    M = len(T0)
    counts = np.bincount(T0)
    return (M / (M - 1)) * (1.0 - float((counts.astype(np.float64) ** 2).sum()) / (M * M))


def _sens_report(positions, sens, var_tot, eps_rels=(0.5, 0.25, 0.1)):
    """Degree bounds from a measured sensitivity profile (thm:learning-low-
    degree: d >= 4*S/eps).  S_measured sums the measured back-offsets (a lower
    bound); S_interp densifies the strided profile by linear interpolation
    over [0, max(positions)].  Reported per RELATIVE eps: eps = eps_rel *
    var_tot, so d(eps_rel) = 4 * d_eff / eps_rel with d_eff = S / var_tot."""
    o = np.argsort(positions)
    pos = np.asarray(positions, dtype=np.int64)[o]
    s = np.asarray(sens, dtype=np.float64)[o]
    S_meas = float(s.sum())
    S_int = float(np.interp(np.arange(pos.max() + 1), pos, s).sum())
    out = {"positions": pos.tolist(), "sens": s.tolist(), "var_tot": float(var_tot),
           "S_measured": S_meas, "S_interp": S_int,
           "d_eff_measured": S_meas / var_tot, "d_eff_interp": S_int / var_tot}
    for tag, S in (("measured", S_meas), ("interp", S_int)):
        out[f"d_eps_{tag}"] = {str(e): 4.0 * S / (e * var_tot) for e in eps_rels}
    return out


# ------------------------------------------------------------------- model side

def _load_teacher(device="cuda"):
    import torch
    from transformers import AutoModelForImageTextToText, AutoTokenizer
    tok = AutoTokenizer.from_pretrained(MODEL_ID)
    model = AutoModelForImageTextToText.from_pretrained(
        MODEL_ID, dtype=torch.bfloat16, device_map=device, low_cpu_mem_usage=True).eval()
    q = len(tok)
    assert set(tok.get_vocab().values()) == set(range(q))
    return model, tok, q


def _stream_spans(tok, n_spans, span_len=CTX, max_docs=8_000_000, corpus=FINEWEB,
                  skip=0):
    """Stream distinct spans from a corpus.  ``skip`` discards the first ``skip``
    qualifying spans so a later training draw never reuses the held-out test
    spans (continual fresh sampling; each doc seen at most once)."""
    import os
    from datasets import load_dataset
    tok_hf = (os.environ.get("HF_TOKEN") or os.environ.get("HF_HUB_TOKEN")
              or os.environ.get("HUGGING_FACE_HUB_TOKEN"))
    ds = load_dataset(corpus[0], name=corpus[1], split="train", streaming=True,
                      token=tok_hf)
    spans, seen = [], 0
    for i, row in enumerate(ds):
        text = row.get("text") or ""
        ids = np.asarray(tok(text, add_special_tokens=False)["input_ids"], dtype=np.int64)
        if len(ids) >= span_len:
            seen += 1
            if seen > skip:
                digest = hashlib.sha256(("span:" + text).encode("utf-8", "ignore")).digest()
                start = int.from_bytes(digest[:8], "big") % (len(ids) - span_len + 1)
                spans.append(ids[start:start + span_len])
        if len(spans) >= n_spans or i + 1 >= max_docs:
            break
    if len(spans) < n_spans:
        raise RuntimeError(f"stream ended at {len(spans)}/{n_spans} spans")
    return np.stack(spans)


def _terminal_probs(model, ids_batch, q):
    """Teacher softmax after the last token of each row (full vocab, sliced to q)."""
    import torch
    with torch.inference_mode():
        out = model(input_ids=ids_batch, use_cache=False, return_dict=True)
        return torch.softmax(out.logits[:, -1, :q].float(), dim=-1)


def _capture_hidden(model, ids_batch, q):
    """One teacher forward with a forward hook on the unembedding: capture its
    INPUT (h, the exact pre-LM-head vector -- no final-norm ambiguity) and its
    OUTPUT (logits -> t* = full-vocab argmax).  Returns (h (B, d), t* (B,))."""
    import torch
    lm = model.get_output_embeddings()
    grab = {}

    def hook(_mod, inp, out):
        grab["h"] = inp[0].detach()
        grab["logits"] = (out[0] if isinstance(out, tuple) else out).detach()
    handle = lm.register_forward_hook(hook)
    try:
        with torch.inference_mode():
            model(input_ids=ids_batch, use_cache=False, return_dict=True, logits_to_keep=1)
    finally:
        handle.remove()
    h, lg = grab["h"], grab["logits"]
    h = h[:, -1, :] if h.dim() == 3 else h                        # (B, d)
    lg = lg[:, -1, :] if lg.dim() == 3 else lg                    # (B, vocab)
    return h.float(), lg[:, :q].argmax(-1)


def _fill_and_label(model, PRE, w_fill, R, q, slot_ids, batch=32, seed=0):
    """R AR fills of w_fill tokens per fiber + slot-projected terminal teacher
    distribution of the completed window.  Returns (G (M*R, w_fill), P (M*R, V))."""
    import torch
    rng = torch.Generator(device="cuda").manual_seed(seed)
    rows = np.repeat(np.arange(len(PRE)), R)
    G = np.empty((len(rows), w_fill), dtype=np.int64)
    P = np.empty((len(rows), len(slot_ids)), dtype=np.float32)
    slot_t = torch.tensor(slot_ids[1:], dtype=torch.long, device="cuda")
    # only the last position's logits are ever used; without this the prefix
    # forward materializes (batch, seq, 248k-vocab) logits (~8GB at batch 128)
    # and OOMs the A10G
    for lo in range(0, len(rows), batch):
        sel = rows[lo:lo + batch]
        ids = torch.tensor(PRE[sel], dtype=torch.long, device="cuda")
        with torch.inference_mode():
            out = model(input_ids=ids, use_cache=True, return_dict=True,
                        logits_to_keep=1)
            past, logits = out.past_key_values, out.logits[:, -1, :]
            made = []
            for _ in range(w_fill):
                probs = torch.softmax(logits[:, :q].float(), dim=-1)
                nxt = torch.multinomial(probs, 1, generator=rng)
                made.append(nxt)
                out = model(input_ids=nxt, past_key_values=past, use_cache=True,
                            return_dict=True, logits_to_keep=1)
                past, logits = out.past_key_values, out.logits[:, -1, :]
            term = torch.softmax(logits[:, :q].float(), dim=-1)
        G[lo:lo + batch] = torch.cat(made, dim=1).cpu().numpy()
        picked = term[:, slot_t].cpu().numpy()
        P[lo:lo + batch, 1:] = picked
        P[lo:lo + batch, 0] = np.maximum(1.0 - picked.sum(1), 0.0)
        if (lo // batch) % 20 == 0:
            print(f"[fill] {lo + len(sel)}/{len(rows)}", flush=True)
    return G, P


def _fork_and_label(model, PRE_ext, w, G_branches, q, slot_ids, batch_fibers=48,
                    seed=0, selfcheck=True):
    """The paper's cache FORK, done efficiently: forward each fiber's shared
    PRE_ext ONCE, then branch ``G_branches`` independent continuations of ``w``
    tokens from the cached state via the official ``Cache.batch_repeat_interleave``.
    ~G x less prefix compute than re-forwarding PRE_ext per branch (the whole
    point of a fork).  Rows come out fiber-major, branch-minor -- matching
    ``np.repeat(arange(M), G_branches)``.  Returns (G_toks (M*G, w), P (M*G, V),
    H (M*G, d_model) fp16) -- H is the teacher pre-LM-head hidden at the
    prediction point, grabbed free via a hook on the terminal forward."""
    import torch
    rng = torch.Generator(device="cuda").manual_seed(seed)
    M = len(PRE_ext)
    Gt = np.empty((M * G_branches, w), dtype=np.int64)
    P = np.empty((M * G_branches, len(slot_ids)), dtype=np.float32)
    slot_t = torch.tensor(slot_ids[1:], dtype=torch.long, device="cuda")
    lm = model.get_output_embeddings()
    grab = {}

    def _hook(_m, inp, _out):
        grab["h"] = inp[0].detach()
    handle = lm.register_forward_hook(_hook)
    H = None
    for lo in range(0, M, batch_fibers):
        pre = torch.tensor(PRE_ext[lo:lo + batch_fibers], dtype=torch.long,
                           device="cuda")
        bf = pre.shape[0]
        with torch.inference_mode():
            out = model(input_ids=pre, use_cache=True, return_dict=True,
                        logits_to_keep=1)
            past, logits = out.past_key_values, out.logits[:, -1, :]     # (bf, V)
            # fork the cache G-fold via beam reindex: reorder_cache dispatches to
            # every layer (KV *and* Qwen3.5's linear-attention conv/recurrent
            # states), unlike batch_repeat_interleave which only covers KV layers
            beam = torch.arange(bf, device=pre.device).repeat_interleave(G_branches)
            past.reorder_cache(beam)                                      # (bf*G, ...)
            logits = logits.repeat_interleave(G_branches, dim=0)         # (bf*G, V)
            made = []
            for _ in range(w):
                probs = torch.softmax(logits[:, :q].float(), dim=-1)
                nxt = torch.multinomial(probs, 1, generator=rng)
                made.append(nxt)
                out = model(input_ids=nxt, past_key_values=past, use_cache=True,
                            return_dict=True, logits_to_keep=1)
                past, logits = out.past_key_values, out.logits[:, -1, :]
            term = torch.softmax(logits[:, :q].float(), dim=-1)
            hb = grab["h"]                                                # capture BEFORE
            hb = (hb[:, -1, :] if hb.dim() == 3 else hb).float()         # the self-check
        toks = torch.cat(made, dim=1).cpu().numpy()                      # (bf*G, w)
        if H is None:
            H = np.empty((M * G_branches, hb.shape[1]), dtype=np.float16)
        H[lo * G_branches:lo * G_branches + bf * G_branches] = hb.cpu().numpy().astype(np.float16)
        if selfcheck and lo == 0:
            # fail loudly if the forked cache does not reproduce a fresh forward.
            # A REAL fork bug (wrong conditioning) breaks EVERY sequence by
            # O(0.1-1); fp16 incremental-decode-vs-full-forward noise is a
            # per-sequence lottery with a heavy tail (observed 1.5e-3 typical,
            # 2.3e-2 tail) -- so check branch 0 of several fibers and gate on
            # the MEDIAN, not a single-sample max
            errs = []
            for fi in range(min(4, bf)):
                seq = np.concatenate([PRE_ext[fi], toks[fi * G_branches]])[None, :]
                with torch.inference_mode():
                    ref = model(input_ids=torch.tensor(seq, device="cuda"),
                                return_dict=True, logits_to_keep=1).logits[0, -1, :q]
                    ref = torch.softmax(ref.float(), -1)[slot_t].cpu().numpy()
                got = term[fi * G_branches, slot_t].cpu().numpy()
                errs.append(float(np.abs(ref - got).max()))
            err = float(np.median(errs))
            assert err < 1e-2, \
                f"forked cache mismatch (median slot err {err}, all {errs})"
            print(f"[fork] self-check ok (median slot err {err:.2e}, "
                  f"max {max(errs):.2e})", flush=True)
        base = lo * G_branches
        Gt[base:base + bf * G_branches] = toks
        picked = term[:, slot_t].cpu().numpy()
        P[base:base + bf * G_branches, 1:] = picked
        P[base:base + bf * G_branches, 0] = np.maximum(1.0 - picked.sum(1), 0.0)
        if (lo // batch_fibers) % 10 == 0:
            print(f"[fork] {min(lo + bf, M)}/{M} fibers x{G_branches}", flush=True)
    handle.remove()
    return Gt, P, H


def _resample_and_label(model, spans, b, g, q, resample="conditional",
                        batch_fibers=16, seed=0, selfcheck=True):
    """Sensitivity kernel: substitute the token at back-offset ``b`` (position
    L-1-b) with g iid draws from nu_b, teacher-force the REAL suffix, and read
    the teacher's FULL-VOCAB top-1 token.  NO 512-slot projection here -- the
    arc moved to full-vocab top-1 agreement (stream_top1); do not reintroduce
    slot projections in the sensitivity path.  nu_b is the model's own
    conditional at the fork point (resample="conditional") or uniform over the
    full vocabulary ("uniform").  Cache forked via reorder_cache exactly as in
    ``_fork_and_label``; the fixed suffix is fed in ONE multi-token forward
    and self-checked against a fresh full forward per position.
    spans (M, L) int64 -> T (M, g) int64 argmax ids."""
    import torch
    M, L = spans.shape
    cut = L - 1 - b                      # substituted position; prefix = [:cut]
    rng = torch.Generator(device="cuda").manual_seed(seed)
    T = np.empty((M, g), dtype=np.int64)
    for lo in range(0, M, batch_fibers):
        sp = torch.tensor(spans[lo:lo + batch_fibers], dtype=torch.long,
                          device="cuda")
        bf = sp.shape[0]
        with torch.inference_mode():
            out = model(input_ids=sp[:, :cut], use_cache=True, return_dict=True,
                        logits_to_keep=1)
            past, logits = out.past_key_values, out.logits[:, -1, :]
            if resample == "conditional":
                probs = torch.softmax(logits[:, :q].float(), dim=-1)
                nxt = torch.multinomial(probs, g, replacement=True, generator=rng)
            else:
                nxt = torch.randint(q, (bf, g), generator=rng, device="cuda")
            beam = torch.arange(bf, device="cuda").repeat_interleave(g)
            past.reorder_cache(beam)                                 # (bf*g, ...)
            step = nxt.reshape(-1, 1)
            if b > 0:                    # fixed real suffix, one multi-token pass
                step = torch.cat([step, sp[:, cut + 1:].repeat_interleave(g, 0)], 1)
            out = model(input_ids=step, past_key_values=past, use_cache=True,
                        return_dict=True, logits_to_keep=1)
            term = out.logits[:, -1, :q]
            if selfcheck and lo == 0:
                seq = sp[0].clone(); seq[cut] = nxt[0, 0]
                ref = model(input_ids=seq[None], return_dict=True,
                            logits_to_keep=1).logits[0, -1, :q]
                same_top = int(ref.argmax(-1)) == int(term[0].argmax(-1))
                err = float((torch.softmax(ref.float(), -1)
                             - torch.softmax(term[0].float(), -1)).abs().max())
                # max-abs over all 248k vocab entries at a 256-row branch batch
                # carries ~1e-2 of bf16 tiling noise (the old 512-slot check
                # maxed over 485x fewer values); what the top-1 readout needs
                # is the ARGMAX surviving the fork, checked exactly
                assert same_top and err < 3e-2, \
                    f"fork mismatch at b={b} (argmax_same={same_top}, err {err})"
                print(f"[sens] b={b} self-check ok (argmax match, {err:.2e})",
                      flush=True)
        T[lo:lo + bf] = term.argmax(-1).reshape(bf, g).cpu().numpy()
        if (lo // batch_fibers) % 10 == 0:
            print(f"[sens] b={b} {min(lo + bf, M)}/{M} fibers x{g}", flush=True)
    return T


# ------------------------------------------------------------------ modal stages

def _read_json(path, default):
    import os
    if not os.path.exists(path):
        return default
    with open(path) as fh:
        return json.load(fh)


def _write_json(path, value):
    import os
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w") as fh:
        json.dump(value, fh, indent=2, sort_keys=True)


def _budget(stage, allocation):
    ledger = _read_json(f"{ROOT}/cost.json", {"gpu_seconds": 0.0, "stages": []})
    spent = ledger["gpu_seconds"] * A10_PER_SECOND
    if spent + allocation > BUDGET:
        raise RuntimeError(f"refusing {stage}: ${spent:.2f} + ${allocation:.2f} > ${BUDGET}")


def _record(stage, started, extra=None):
    import time
    ledger = _read_json(f"{ROOT}/cost.json", {"gpu_seconds": 0.0, "stages": []})
    seconds = time.time() - started
    ledger["gpu_seconds"] += seconds
    ledger["stages"].append(dict({"stage": stage, "gpu_seconds": seconds,
                                  "estimated_dollars": seconds * A10_PER_SECOND},
                                 **(extra or {})))
    _write_json(f"{ROOT}/cost.json", ledger)
    vol.commit()


def _collapse_table(tbl, lbl):
    """Collapse a flat table + hidden labels to distinct contexts: rows,
    count-mean hidden state, counts, teacher top-1 token."""
    d = np.load(tbl); L = np.load(lbl)
    ctx = np.concatenate([d["PRE"][d["fiber_id"]], d["G"]], axis=1)
    rows, inv, cnt = np.unique(ctx, axis=0, return_inverse=True, return_counts=True)
    Hs = np.zeros((len(rows), L["H"].shape[1]), np.float64)
    np.add.at(Hs, inv, L["H"].astype(np.float64))
    tg = np.empty(len(rows), np.int64); tg[inv] = L["tstar"]
    return rows, Hs / cnt[:, None], cnt.astype(np.float64), tg


def _table_path(fill_len, m_fibers, r, tag=""):
    t = f"_{tag}" if tag else ""
    if fill_len == W_WIN - K_FIXED:
        return f"{ROOT}/glds_w{W_WIN}_k{K_FIXED}_M{m_fibers}_R{r}{t}.npz"
    return f"{ROOT}/glds_ctx{CTX}_f{fill_len}_M{m_fibers}_R{r}{t}.npz"


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=21600,
              memory=32768, secrets=HF_SECRET)
def make_data(m_fibers: int = M_FIBERS, r: int = R_FILLS, seed: int = 0,
              fill_len: int = W_WIN - K_FIXED, corpus: str = "fineweb",
              skip: int = 0, tag: str = "", chunk_lo: int = 0, chunk_hi: int = 0):
    """Fibers from a corpus + AR fills + slot-projected teacher labels -> one npz.
    chunk_lo/chunk_hi restrict which fiber range this call generates (fan-out:
    disjoint workers share one shard_dir); a partial range returns after saving
    its shards without concatenating the table.
    fill_len = how many context tokens the model fills (the searchable span);
    the remaining CTX - fill_len tokens are real dataset conditioning.
    corpus in {fineweb, edu}; skip discards leading spans so train never reuses
    the held-out test spans; tag namespaces the cache file."""
    import os, time
    vol.reload()
    out = _table_path(fill_len, m_fibers, r, tag)
    if os.path.exists(out):
        return out
    _budget("data", 8.0)
    started = time.time()
    model, tok, q = _load_teacher()
    spans = _stream_spans(tok, n_spans=3 * m_fibers, corpus=CORPORA[corpus], skip=skip)
    PRE = draw_fibers(spans, w=fill_len + K_FIXED, M=m_fibers, seed=seed)
    # slot alphabet from the mean terminal distribution over a fiber sample
    import torch
    sample = torch.tensor(PRE[:256], dtype=torch.long, device="cuda")
    mass = np.zeros(q)
    for lo in range(0, len(sample), 32):
        mass += _terminal_probs(model, sample[lo:lo + 32], q).sum(0).cpu().numpy()
    slot_ids = np.concatenate([[-1], np.argsort(-mass)[: V_SLOTS - 1]]).astype(np.int64)
    # sharded fills: preemption restarts resume from the last saved shard
    # (PRE and slot_ids are deterministic given the stream + seed)
    shard_dir = f"{ROOT}/shards_f{fill_len}_M{m_fibers}_R{r}{'_' + tag if tag else ''}"
    os.makedirs(shard_dir, exist_ok=True)
    chunk = 500
    hi = chunk_hi if chunk_hi else m_fibers
    for lo in range(chunk_lo, hi, chunk):
        sp = f"{shard_dir}/s{lo}.npz"
        if os.path.exists(sp):
            continue
        Gc, Pc = _fill_and_label(model, PRE[lo:lo + chunk], fill_len, r, q,
                                 slot_ids, batch=128, seed=seed + lo)   # 4x saturation
        np.savez_compressed(sp + ".tmp.npz", G=Gc, P=Pc)
        os.replace(sp + ".tmp.npz", sp)
        vol.commit()
        print(f"[shard] {min(lo + chunk, m_fibers)}/{m_fibers} fibers "
              f"(worker {chunk_lo}:{hi})", flush=True)
    if chunk_lo != 0 or (chunk_hi and chunk_hi < m_fibers):
        return f"shards {chunk_lo}:{hi} of M{m_fibers} done"          # partial fan-out worker
    G_parts, P_parts = [], []                                        # full run -> concat all
    for lo in range(0, m_fibers, chunk):
        z = np.load(f"{shard_dir}/s{lo}.npz")
        G_parts.append(z["G"]); P_parts.append(z["P"])
    G = np.concatenate(G_parts)
    P = np.concatenate(P_parts)
    fiber_id = np.repeat(np.arange(m_fibers), r)
    os.makedirs(ROOT, exist_ok=True)
    # full contexts reconstruct as concat(PRE[fiber_id], G) -- the GL domain is
    # every previous token, so the real prefixes are part of the artifact.
    np.savez_compressed(out.replace(".npz", ".tmp.npz"), PRE=PRE, G=G, P=P,
                        fiber_id=fiber_id, slot_ids=slot_ids, q=q)
    os.replace(out.replace(".npz", ".tmp.npz"), out)
    vol.commit()
    _record("data", started, {"m_fibers": m_fibers, "r": r})
    print(f"saved {out}", flush=True)
    return out


@app.function(image=image, volumes={"/cache": vol}, timeout=14400, memory=8192)
def gen_data(m_fibers: int = 16000, r: int = 2, fill_len: int = 61,
             corpus: str = "edu", skip: int = 0, tag: str = "edu_tr",
             n_shards: int = 8, seed: int = 0):
    """FAN-OUT generation: N GPU workers fill disjoint fiber ranges into one
    shared shard_dir in parallel (~linear speedup, hours->minutes), then a final
    call concatenates the table.  Bytewise identical to a serial make_data --
    PRE and slot_ids are deterministic given (corpus, skip, m_fibers, seed)."""
    import os
    vol.reload()
    out = _table_path(fill_len, m_fibers, r, tag)
    if os.path.exists(out):
        return out
    chunk = 500
    n_chunks = (m_fibers + chunk - 1) // chunk
    per = (n_chunks + n_shards - 1) // n_shards                       # chunks per worker
    handles = []
    for w in range(n_shards):
        lo = w * per * chunk
        hi = min((w + 1) * per * chunk, m_fibers)
        if lo >= m_fibers:
            break
        handles.append(make_data.spawn(m_fibers, r, seed, fill_len, corpus, skip,
                                       tag, lo, hi))
    print(f"[gen-data] {len(handles)} workers x ~{per*chunk} fibers", flush=True)
    for h in handles:
        print(h.get(), flush=True)
    return make_data.remote(m_fibers, r, seed, fill_len, corpus, skip, tag)   # concat


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=21600, memory=32768)
def search(tau: float, m_fibers: int = M_FIBERS, r: int = R_FILLS, val_frac: float = 0.15,
           fill_len: int = W_WIN - K_FIXED, max_width: int = 512, encoding: str = "all"):
    """The GL tree at tau for every encoding on the identical fiber table.
    The searched span = the model-filled token blocks (where within-fiber
    conditional variation exists); everything earlier is conditioning."""
    import os, time
    import torch
    vol.reload()
    _budget("search", 1.5)
    started = time.time()
    data = np.load(_table_path(fill_len, m_fibers, r))
    P, fiber_id, q = data["P"], data["fiber_id"], int(data["q"])
    fill_len = data["G"].shape[1]
    contexts = np.concatenate([data["PRE"][fiber_id], data["G"]], axis=1)

    codes_path = f"{ROOT}/codes.npz"
    if os.path.exists(codes_path):
        z = np.load(codes_path)
        tables = {"lsh": z["lsh"], "ctrl": z["ctrl"], "idbits": z["idbits"]}
    else:
        model, _, _ = _load_teacher(device="cpu")
        E = model.get_input_embeddings().weight[:q].detach().float().numpy()
        del model
        lsh, report = build_lsh_codes(E)
        tables = {"lsh": lsh, "ctrl": control_codes(q, lsh.shape[1]),
                  "idbits": token_id_codes(q)}
        np.savez_compressed(codes_path, **tables)
        _write_json(f"{ROOT}/codes_report.json", report)

    rng = np.random.default_rng(0)
    val_fibers = rng.random(fiber_id.max() + 1) < val_frac
    va_rows = val_fibers[fiber_id]
    ctx_tr, A_tr, n_tr, _ = soft_collapse(contexts[~va_rows], P[~va_rows])
    ctx_va, A_va, n_va, _ = soft_collapse(contexts[va_rows], P[va_rows])
    # Search the CENTERED target (the residual f - E[f]): a biased/near-constant
    # parity has f_hat parallel to the unigram (large norm, zero conditional
    # content -- the idbits artifact); centering kills those identically.  The
    # fit keeps raw targets (the intercept carries the unigram).
    mu = A_tr.sum(0) / n_tr.sum()
    A_search = (A_tr - n_tr[:, None] * mu[None, :]).astype(np.float32)
    if encoding != "all":
        tables = {encoding: tables[encoding]}
    summary = {"tau": tau, "m_rows": int(len(contexts)), "target": "centered",
               "encodings": {}}
    for name, codes in tables.items():
        bits_tr = context_bits(ctx_tr, codes)
        bits_va = context_bits(ctx_va, codes)
        uni_kl = fit_softmax_slots(np.zeros((len(ctx_tr), 0), np.float32),
                                   A_tr / n_tr[:, None], n_tr,
                                   np.zeros((len(ctx_va), 0), np.float32),
                                   A_va / n_va[:, None], n_va, steps=1)["val_kl"]
        cb = _kl_progress(name, bits_tr, A_tr / n_tr[:, None], n_tr,
                          bits_va, A_va / n_va[:, None], n_va, uni_kl)
        t0 = time.time()
        got = dataset_gl_tau(bits_tr, A_search, tau, norm_m=int((~va_rows).sum()),
                             max_width=max_width,
                             n_search=fill_len * codes.shape[1], on_progress=cb)
        entry = {"status": got["status"], "widths": got["widths"],
                 "saturated_levels": len(got.get("saturated_levels", [])),
                 "leaf_norms_top": got.get("leaf_norms_top", []),
                 "search_seconds": time.time() - t0}
        if got["status"] == "ok":
            masks = got["masks"]
            entry["n_characters"] = int(len(masks))
            # the frontier is tau-independent (saturation orders by Q); persist
            # the carried masks so the leaf bar can be re-applied without a
            # fresh multi-hour descent
            np.savez_compressed(f"{ROOT}/frontier_f{fill_len}_{name}.npz",
                                masks=got["frontier_masks"])
            if len(masks):
                np.savez_compressed(f"{ROOT}/masks_f{fill_len}_{name}_tau{tau}.npz",
                                    masks=masks)
            B = codes.shape[1]
            entry["token_degree_hist"] = np.bincount(
                [int((mk.reshape(CTX, B).any(1)).sum()) for mk in masks],
                minlength=CTX + 1).tolist() if len(masks) else []
            fit = fit_softmax_slots(parity_features(bits_tr, masks), A_tr / n_tr[:, None],
                                    n_tr, parity_features(bits_va, masks),
                                    A_va / n_va[:, None], n_va)
            entry["val_kl"] = fit["val_kl"]
            entry["eval"] = eval_slots(fit["W"], fit["b"],
                                       parity_features(bits_va, masks),
                                       A_va / n_va[:, None], n_va)
            base = fit_softmax_slots(np.zeros((len(ctx_tr), 0), np.float32),
                                     A_tr / n_tr[:, None], n_tr,
                                     np.zeros((len(ctx_va), 0), np.float32),
                                     A_va / n_va[:, None], n_va, steps=1)
            entry["unigram_val_kl"] = base["val_kl"]
        summary["encodings"][name] = entry
        print(name, json.dumps({k: v for k, v in entry.items() if k != "widths"}),
              flush=True)
        torch.cuda.empty_cache()
    suffix = "" if fill_len == W_WIN - K_FIXED else f"_f{fill_len}"
    if encoding != "all":
        suffix += f"_{encoding}"
    _write_json(f"{ROOT}/summary{suffix}_tau{tau}.json", summary)
    _record("search", started, {"tau": tau})
    print(json.dumps({n: {k: e.get(k) for k in ("status", "n_characters", "val_kl",
                                                "unigram_val_kl", "eval")}
                      for n, e in summary["encodings"].items()}, indent=2), flush=True)
    return summary


def _oracle_table_path(fill_len, m_fibers, g, p_back):
    return f"{ROOT}/oracle_f{fill_len}_M{m_fibers}_G{g}_p{p_back}.npz"


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=21600, memory=32768)
def make_oracle_data(m_fibers: int = 2000, g: int = 24, p_back: int = 0,
                     fill_len: int = W_WIN - K_FIXED, seed: int = 0):
    """FORKED-CACHE data for the paper's paired estimator (no collisions
    needed).  To split the token at reverse-index ``p_back`` (0 = the newest
    filled token, which carries the heavy characters), fix the real prefix AND
    the older filled tokens 0..(fill_len-2-p_back) as a SHARED STUB drawn once
    per fiber, then sample the split token + any newer tokens ``g`` times.  All
    g rows of a fiber then share the un-split continuation by construction --
    exactly the theorem's cache fork -- so every within-fiber pair is valid.

    Saves split_tok (M*g,), P (M*g, V slots), fiber_gid (M*g,), slot_ids."""
    import os, time
    import torch
    vol.reload()
    out = _oracle_table_path(fill_len, m_fibers, g, p_back)
    if os.path.exists(out) and "H" in np.load(out).files:         # regen legacy P-only tables
        return out
    _budget("oracle-data", 2.0)
    started = time.time()
    stub_len = fill_len - 1 - p_back
    assert 0 <= stub_len < fill_len, f"p_back {p_back} out of range for fill {fill_len}"
    model, tok, q = _load_teacher()
    # reuse the fibers/slots from whichever fiber table for this fill_len exists
    # (the oracle only needs PRE + slot_ids; slice to m_fibers)
    src = None
    for cand in (m_fibers, 8000, 4000, 2000):
        p = _table_path(fill_len, cand, r=R_FILLS)
        if os.path.exists(p):
            src = np.load(p); break
    if src is None:
        raise RuntimeError(f"no fiber table for fill_len={fill_len}")
    PRE, slot_ids = src["PRE"], src["slot_ids"]
    if len(PRE) < m_fibers:
        raise RuntimeError(f"only {len(PRE)} fibers cached, need {m_fibers}")
    PRE = PRE[:m_fibers]
    if stub_len > 0:                                               # shared stub, one draw
        stub, _ = _fill_and_label(model, PRE, stub_len, 1, q, slot_ids,
                                  batch=128, seed=seed)
        PRE_ext = np.concatenate([PRE, stub], axis=1)
    else:
        PRE_ext = PRE
    w = fill_len - stub_len                                        # split token + newer
    # FORK the cache when the model's cache supports it (~g x less prefix
    # compute); Qwen3.5's LinearAttentionLayer cache lacks batch_repeat_interleave,
    # so fall back to the correct (if g x costlier) re-forward.
    fiber_gid = np.repeat(np.arange(m_fibers), g)
    try:
        G_toks, P, H = _fork_and_label(model, PRE_ext, w, g, q, slot_ids,
                                       batch_fibers=32, seed=seed + 7)
    except AttributeError as e:
        print(f"[fork] unsupported cache ({e}); re-forwarding per branch", flush=True)
        G_toks, P = _fill_and_label(model, PRE_ext, w, g, q, slot_ids,
                                    batch=128, seed=seed + 7)
        ctx = np.concatenate([PRE_ext[fiber_gid], G_toks], axis=1).astype(np.int64)
        dm = model.get_output_embeddings().weight.shape[1]
        H = np.empty((len(ctx), dm), np.float16)
        for lo in range(0, len(ctx), 128):
            hb, _ = _capture_hidden(model, torch.tensor(ctx[lo:lo + 128],
                                    dtype=torch.long, device="cuda"), q)
            H[lo:lo + 128] = hb.cpu().numpy().astype(np.float16)
    split_tok = G_toks[:, 0].astype(np.int64)                     # the split coordinate
    os.makedirs(ROOT, exist_ok=True)
    # gtoks = ALL resampled tokens (generation order, oldest-resampled first);
    # H = teacher pre-head hidden per fork row (the GL target for the top-1 run)
    np.savez_compressed(out.replace(".npz", ".tmp.npz"), split_tok=split_tok, P=P,
                        gtoks=G_toks.astype(np.int64), H=H,
                        fiber_gid=fiber_gid, slot_ids=slot_ids, q=q)
    os.replace(out.replace(".npz", ".tmp.npz"), out)
    vol.commit()
    _record("oracle-data", started, {"m_fibers": m_fibers, "g": g, "p_back": p_back})
    print(f"saved {out}", flush=True)
    return out


def _load_fork_levels(fill_len, m_fibers, g, depth, codes, target="slots",
                      deg1_W=None, fiber_center=False):
    """Assemble the tree's per-level fork bit-data from cached oracle tables:
    level j = fork that resampled the newest j+1 tokens (p_back=j).  Returns
    a list of (bits (m, (j+1)*B) newest-first, F (m,V) centered, gid (m,)).
    target="hidden" -> F is the teacher pre-head hidden state H, per-dim
    standardized with POOLED stats (one scale across levels, so psi is
    comparable level-to-level) and per-row UNIT-normalized -- the paper's
    vector-GL contract ||F(x)||_2 <= 1 that makes the tau^2/4 gate meaningful
    (psi in [0,1]); "slots" -> the legacy centered 512-slot P.
    deg1_W (nb, d): subtract the deg-1 flat-fit prediction of the RESAMPLED
    bits from H (raw units) so unary structure zeroes out on purpose.
    fiber_center: subtract each fiber's mean AFTER standardization -- kills
    the rollout-density pollutant E[F|z]E[chi_S|z] (measured corr 0.998 with
    raw psi, and top-512 raw-vs-functional overlap = 0 at deg-2), leaving the
    within-fiber covariance spectrum psi actually prunes on.  Frozen-stub
    deg-1 contributions are fiber-constant, so centering absorbs the blocks
    deg1_W cannot see at shallow levels."""
    B = codes.shape[1]
    tables = [np.load(_oracle_table_path(fill_len, m_fibers, g, j))
              for j in range(depth)]
    if target != "hidden":
        levels = []
        for d in tables:
            gt = d["gtoks"]                                        # (m, j+1) gen order
            bits = np.asarray(codes, np.uint8)[gt[:, ::-1]].reshape(len(gt), -1)
            Y = d["P"].astype(np.float64)
            levels.append((bits, (Y - Y.mean(0)[None, :]).astype(np.float32),
                           d["fiber_gid"]))
        return levels
    bits_l, Ys, gids = [], [], []
    for d in tables:
        gt = d["gtoks"]                                            # (m, j+1) gen order
        bits = np.asarray(codes, np.uint8)[gt[:, ::-1]].reshape(len(gt), -1)
        Y = d["H"].astype(np.float64)
        if deg1_W is not None:                                    # unary zero-out
            Y = Y - (1.0 - 2.0 * bits.astype(np.float64)) \
                @ np.asarray(deg1_W, np.float64)[:bits.shape[1]]
        bits_l.append(bits); Ys.append(Y); gids.append(d["fiber_gid"])
    if fiber_center:                                              # density zero-out FIRST,
        for k, (Y, fib) in enumerate(zip(Ys, gids)):              # so pooled stats reflect
            _, gi = np.unique(np.asarray(fib), return_inverse=True)   # within-fiber scale
            fm = np.zeros((int(gi.max()) + 1, Y.shape[1]))
            np.add.at(fm, gi, Y)
            Ys[k] = Y - (fm / np.bincount(gi)[:, None])[gi]
    allY = np.concatenate(Ys)
    mu, sd = allY.mean(0), allY.std(0)                            # pooled per-dim stats
    print(f"[fork-levels] hidden target row-normalized "
          f"(d={allY.shape[1]}, pooled stats over {len(allY)} rows, "
          f"deg1_resid={deg1_W is not None}, fiber_center={fiber_center})",
          flush=True)
    levels = []
    for bits, Y, fib in zip(bits_l, Ys, gids):
        Y = (Y - mu[None, :]) / (sd[None, :] + 1e-6)
        F = (Y / (np.linalg.norm(Y, axis=1, keepdims=True) + 1e-12)
             ).astype(np.float32)                                 # ||F(x)||_2 = 1
        levels.append((bits, F, fib))
    return levels


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=21600, memory=32768)
def gl_tree(m_fibers: int = 2000, g: int = 16, depth: int = 6,
            fill_len: int = 61, tau: float = 0.02, max_width: int = 128,
            fit_m: int = 8000, val_frac: float = 0.15, encoding: str = "all"):
    """THE TRUE DATASET GL, end to end and BEYOND degree two: generate a fork
    per tree level (newest 1..depth tokens), grow characters of every degree
    with ``forked_gl_tree``, then fit the recovered basis STAGED BY DEGREE on
    the flat table (clean 3-way fiber split) and report TEST KL -- the honest
    test of whether high-degree characters push below the deg-1+2 ceiling."""
    import os, time
    import torch
    vol.reload()
    _budget("gl-tree", 3.0)
    started = time.time()
    # ensure every fork level exists (generate the missing ones)
    for j in range(depth):
        if not os.path.exists(_oracle_table_path(fill_len, m_fibers, g, j)):
            make_oracle_data.local(m_fibers, g, j, fill_len)
    z = dict(np.load(f"{ROOT}/codes.npz"))
    tables = {"lsh": z["lsh"], "ctrl": z["ctrl"]}
    if encoding != "all":
        tables = {encoding: tables[encoding]}
    # flat fit table (clean 3-way split)
    data = np.load(_table_path(fill_len, fit_m, R_FILLS))
    P, fiber_id = data["P"], data["fiber_id"]
    fl = data["G"].shape[1]
    contexts = np.concatenate([data["PRE"][fiber_id], data["G"]], axis=1)
    rng = np.random.default_rng(0)
    u = rng.random(fiber_id.max() + 1)[fiber_id]
    te, va = u < 0.15, (u >= 0.15) & (u < 0.30)
    tr = ~(te | va)
    ctx_tr, A_tr, n_tr, _ = soft_collapse(contexts[tr], P[tr])
    ctx_va, A_va, n_va, _ = soft_collapse(contexts[va], P[va])
    ctx_te, A_te, n_te, _ = soft_collapse(contexts[te], P[te])
    Ptr, Pva, Pte = A_tr / n_tr[:, None], A_va / n_va[:, None], A_te / n_te[:, None]
    mu = A_tr.sum(0) / n_tr.sum()

    def slot_kl(off, Pn, n):
        lq = off - torch.logsumexp(off, 1, keepdim=True)
        lp = torch.tensor(np.log(np.clip(Pn, 1e-12, None)), device=off.device)
        return float((torch.tensor(n[:, None] * Pn, device=off.device)
                      * (lp - lq)).sum() / float(n.sum()))

    summary = {"tau": tau, "depth": depth, "encodings": {}}
    for name, codes in tables.items():
        B = codes.shape[1]
        levels = _load_fork_levels(fill_len, m_fibers, g, depth, codes)
        t0 = time.time()
        tree = forked_gl_tree(levels, B, tau, max_width=max_width,
                              progress=lambda j, D, kept: print(
                                  f"[tree:{name}] level {j+1}/{D} kept {len(kept)}",
                                  flush=True))
        masks = tree["masks"]                                      # (K, depth*B), psi-sorted
        deg = np.array([int((m.reshape(depth, B).any(1)).sum()) for m in masks]) \
            if len(masks) else np.zeros(0, int)
        hist = np.bincount(deg, minlength=depth + 1).tolist()
        print(f"[tree:{name}] {len(masks)} chars, degree hist {hist}, "
              f"{time.time()-t0:.0f}s", flush=True)
        # persist masks+psi+degree so the fit can be re-tuned WITHOUT re-searching
        np.savez_compressed(f"{ROOT}/gltree_masks_f{fill_len}_d{depth}_{name}.npz",
                            masks=masks, psi=tree["psi"], deg=deg)
        vol.commit()
        # deg>=3 is added top-K by psi (a huge noisy block would be rejected
        # wholesale by the val gate, hiding any genuinely useful high-degree char)
        top_per_deg = 800
        # HONEST test of "beyond deg-2": build the FULL deg-1+2 enumeration
        # baseline (all fill_len tokens -- the 1.103 recipe), then add the
        # tree's deg>=3 characters (which enumeration cannot reach) as
        # val-gated blocks.  Does TEST KL drop below the deg-1+2 ceiling?
        nb_full, ndep = fill_len * B, depth * B
        bt = context_bits(ctx_tr, codes)
        bv = context_bits(ctx_va, codes)
        bte = context_bits(ctx_te, codes)
        Mtr = int(tr.sum())
        A_c = torch.tensor((A_tr - n_tr[:, None] * mu[None, :]).astype(np.float32),
                           device="cuda")
        Ffull = torch.tensor(1.0 - 2.0 * bt[:, :nb_full].astype(np.float32), device="cuda")
        d1n = ((Ffull.T @ A_c) / Mtr).double().pow(2).sum(1).sqrt().cpu().numpy()
        anchors = np.flatnonzero(d1n >= 0.01)
        pairs = {}
        for i in anchors:
            ni = ((Ffull * Ffull[:, [i]]).T @ A_c / Mtr).double().pow(2).sum(1) \
                .sqrt().cpu().numpy()
            ni[i] = 0.0
            for j in np.argsort(-ni)[:max(20, 2000 // max(len(anchors), 1) + 1)]:
                if ni[j] >= 0.01:
                    key = (min(int(i), int(j)), max(int(i), int(j)))
                    pairs[key] = max(pairs.get(key, 0.0), float(ni[j]))
        top = sorted(pairs.items(), key=lambda kv: -kv[1])[:1000]
        ii = np.array([k[0] for k, _ in top] or [0])
        jj = np.array([k[1] for k, _ in top] or [0])
        del Ffull; torch.cuda.empty_cache()

        def base_feat(bits):
            S = 1.0 - 2.0 * bits[:, :nb_full].astype(np.float32)
            return np.concatenate([S[:, anchors], S[:, ii] * S[:, jj]], axis=1)
        uni = slot_kl(torch.tensor(np.tile(np.log(np.clip(mu, 1e-12, None)),
                      (len(Pte), 1)), device="cuda", dtype=torch.float32), Pte, n_te)
        fitB = fit_softmax_slots(base_feat(bt), Ptr, n_tr, base_feat(bv), Pva, n_va,
                                 steps=3000, lr=0.01)
        Wb = torch.tensor(fitB["W"], device="cuda"); bb = torch.tensor(fitB["b"], device="cuda")
        off_tr = torch.tensor(base_feat(bt), device="cuda") @ Wb + bb
        off_va = torch.tensor(base_feat(bv), device="cuda") @ Wb + bb
        off_te = torch.tensor(base_feat(bte), device="cuda") @ Wb + bb
        base_test = slot_kl(off_te, Pte, n_te)
        ladder = [{"stage": "deg1+2 baseline", "n": int(len(anchors) + len(top)),
                   "val_kl": slot_kl(off_va, Pva, n_va), "test_kl": base_test}]
        print(f"[fit:{name}] deg1+2 baseline ({len(anchors)}+{len(top)}) "
              f"test {base_test:.4f} (uni {uni:.4f})", flush=True)
        # add the TREE's deg>=3 characters (over the newest depth tokens)
        for dd in range(3, depth + 1):
            md = masks[deg == dd][:top_per_deg]                    # top-K by psi
            if not len(md):
                continue
            Ftr = parity_features(bt[:, :ndep], md)
            Fva = parity_features(bv[:, :ndep], md)
            Wd, vk, improved = _fit_block(Ftr, off_tr.cpu().numpy(), Ptr, n_tr,
                                          Fva, off_va.cpu().numpy(), Pva, n_va,
                                          steps=1500, lr=0.01)
            if improved:
                Wt = torch.tensor(Wd, device="cuda")
                off_tr = off_tr + torch.tensor(Ftr, device="cuda") @ Wt
                off_va = off_va + torch.tensor(Fva, device="cuda") @ Wt
                off_te = off_te + torch.tensor(parity_features(bte[:, :ndep], md),
                                               device="cuda") @ Wt
            ladder.append({"stage": f"+deg{dd}", "n": int(len(md)),
                           "improved": bool(improved), "val_kl": slot_kl(off_va, Pva, n_va),
                           "test_kl": slot_kl(off_te, Pte, n_te)})
            print(f"[fit:{name}] +deg{dd} ({len(md)}, kept={improved}) "
                  f"test {ladder[-1]['test_kl']:.4f}", flush=True)
        summary["encodings"][name] = {
            "n_chars": int(len(masks)), "degree_hist": hist,
            "TEST_unigram_kl": uni, "baseline_deg12_TEST_kl": base_test,
            "final_TEST_kl": ladder[-1]["test_kl"],
            "ladder": ladder, "per_level_kept": tree["per_level_kept"]}
        print(name, json.dumps({k: summary["encodings"][name][k]
              for k in ("n_chars", "degree_hist", "baseline_deg12_TEST_kl",
                        "final_TEST_kl", "TEST_unigram_kl")}), flush=True)
        torch.cuda.empty_cache()
    _write_json(f"{ROOT}/summary_gltree_f{fill_len}_d{depth}_tau{tau}.json", summary)
    _record("gl-tree", started, {"tau": tau, "depth": depth})
    return summary


def _deg12_basis(bits_full, A_c, Mtr, device="cuda", thresh=0.01, max_pairs=1000):
    """The GL deg-1+2 basis: certified deg-1 anchors (exact marginal norm >=
    thresh) + the top deg-2 pairs among them.  Returns (anchors, ii, jj)."""
    import torch
    Ff = torch.tensor(1.0 - 2.0 * bits_full.astype(np.float32), device=device)
    A = torch.tensor(A_c.astype(np.float32), device=device)
    d1n = ((Ff.T @ A) / Mtr).double().pow(2).sum(1).sqrt().cpu().numpy()
    anchors = np.flatnonzero(d1n >= thresh)
    pairs = {}
    for i in anchors:
        ni = ((Ff * Ff[:, [i]]).T @ A / Mtr).double().pow(2).sum(1).sqrt().cpu().numpy()
        ni[i] = 0.0
        for j in np.argsort(-ni)[:max(20, 2000 // max(len(anchors), 1) + 1)]:
            if ni[j] >= thresh:
                k = (min(int(i), int(j)), max(int(i), int(j)))
                pairs[k] = max(pairs.get(k, 0.0), float(ni[j]))
    top = sorted(pairs.items(), key=lambda kv: -kv[1])[:max_pairs]
    ii = np.array([k[0] for k, _ in top] or [0]); jj = np.array([k[1] for k, _ in top] or [0])
    del Ff, A; torch.cuda.empty_cache()
    return anchors, ii, jj


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=43200, memory=32768)
def relabel_hidden(m_fibers: int = 16000, fill_len: int = 61, r: int = 8,
                   tag: str = "edu_tr", batch: int = 128):
    """Re-label an existing table with the teacher's pre-LM-head hidden state h
    and full-vocab argmax token t* -- reuses the cached fills, one forward per
    context.  Saves labels_hidden_<...>.npz (H fp16, tstar) + lm_head.npz (Wu)."""
    import os, time
    import torch
    vol.reload()
    out = f"{ROOT}/labels_hidden_f{fill_len}_M{m_fibers}_R{r}_{tag}.npz"
    if os.path.exists(out):
        return out
    _budget("relabel", 4.0)
    started = time.time()
    d = np.load(_table_path(fill_len, m_fibers, r, tag))
    contexts = np.concatenate([d["PRE"][d["fiber_id"]], d["G"]], axis=1).astype(np.int64)
    model, tok, q = _load_teacher()
    Wu = model.get_output_embeddings().weight[:q].detach().float().cpu().numpy()
    d_model = Wu.shape[1]
    print(f"[relabel:{tag}] {len(contexts)} contexts, d_model={d_model}, q={q}", flush=True)
    H = np.empty((len(contexts), d_model), np.float16)
    tstar = np.empty(len(contexts), np.int64)
    for lo in range(0, len(contexts), batch):
        ids = torch.tensor(contexts[lo:lo + batch], dtype=torch.long, device="cuda")
        h, ts = _capture_hidden(model, ids, q)
        H[lo:lo + batch] = h.cpu().numpy().astype(np.float16)
        tstar[lo:lo + batch] = ts.cpu().numpy()
        if (lo // batch) % 50 == 0:
            print(f"[relabel:{tag}] {min(lo + batch, len(contexts))}/{len(contexts)}",
                  flush=True)
    np.savez_compressed(out.replace(".npz", ".tmp.npz"), H=H, tstar=tstar)
    os.replace(out.replace(".npz", ".tmp.npz"), out)
    if not os.path.exists(f"{ROOT}/lm_head.npz"):
        np.savez_compressed(f"{ROOT}/lm_head.npz", Wu=Wu.astype(np.float16))
    vol.commit()
    _record("relabel", started, {"tag": tag})
    print(f"saved {out}", flush=True)
    return out


def _decode_top1(target, Ypred, Wu, codes):
    """Predicted target -> token: unembed argmax (hidden) or nearest LSH code."""
    return unembed_top1(Ypred, Wu) if target == "hidden" else code_decode(Ypred, codes)


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=43200,
              memory=32768, secrets=WANDB_SECRET)
def stream_top1(m_train: int = 16000, m_test: int = 3000, fill_len: int = 61,
                r: int = 8, target: str = "hidden"):
    """Full-vocab TOP-1 agreement (no slots, no OTHER).  Target = teacher
    pre-head hidden state (target=hidden: regress h, unembed, argmax) OR the
    top-1 token's LSH code (target=code: regress code, nearest-decode).  Fixed
    FineWeb-Edu test set, disjoint fresh train, deg-1+2 GL basis, ridge fit;
    lsh vs ctrl on the same held-out top-1, logged to W&B."""
    import os, time
    import torch
    vol.reload()
    _budget("stream-top1", 8.0)
    started = time.time()
    te_tbl = make_data.local(m_test, r, 0, fill_len, "edu", 0, "edu_test")
    tr_tbl = make_data.local(m_train, r, 0, fill_len, "edu", 3 * m_test, "edu_tr")
    te_lbl = relabel_hidden.local(m_test, fill_len, r, "edu_test")
    tr_lbl = relabel_hidden.local(m_train, fill_len, r, "edu_tr")
    Wu = np.load(f"{ROOT}/lm_head.npz")["Wu"].astype(np.float32)
    ztab = dict(np.load(f"{ROOT}/codes.npz"))

    def collapse(tbl, lbl):
        d = np.load(tbl); L = np.load(lbl)
        ctx = np.concatenate([d["PRE"][d["fiber_id"]], d["G"]], axis=1)
        rows, inv, counts = np.unique(ctx, axis=0, return_inverse=True, return_counts=True)
        Hs = np.zeros((len(rows), L["H"].shape[1]), np.float64)
        np.add.at(Hs, inv, L["H"].astype(np.float64))
        tg = np.empty(len(rows), np.int64); tg[inv] = L["tstar"]
        return rows, Hs, counts.astype(np.float64), tg
    c_tr, H_tr, n_tr, t_tr = collapse(tr_tbl, tr_lbl)
    c_te, H_te, n_te, t_te = collapse(te_tbl, te_lbl)
    fl = np.load(tr_tbl)["G"].shape[1]
    meanH_tr, meanH_te = H_tr / n_tr[:, None], H_te / n_te[:, None]
    rng = np.random.default_rng(0); vm = rng.random(len(n_tr)) < 0.15
    N = float(n_tr.sum())
    # standardize the target per dim so anchor selection is scale-free (hidden
    # states have large magnitude; a fixed prob-scale threshold would pass all)
    Hs = ((meanH_tr - (H_tr.sum(0) / N)[None, :]) / (meanH_tr.std(0) + 1e-6))
    Hs_t = torch.tensor(Hs, dtype=torch.float32, device="cuda")
    nt = torch.tensor(n_tr, dtype=torch.float32, device="cuda")
    n_anchor = 512
    summary = {"target": target, "m_train": m_train, "m_test": m_test, "encodings": {}}
    for name in ("lsh", "ctrl"):
        codes = ztab[name]; B = codes.shape[1]; nb = fl * B
        bt = context_bits(c_tr, codes)[:, :nb]; bte = context_bits(c_te, codes)[:, :nb]
        Ff = torch.tensor(1.0 - 2.0 * bt.astype(np.float32), device="cuda")   # (D, nb)
        corr = (Ff * nt[:, None]).t() @ Hs_t / N                              # (nb, d)
        d1n = corr.pow(2).sum(1).sqrt().cpu().numpy()
        anchors = np.argsort(-d1n)[:n_anchor]                                 # deg-1 top-K
        del Ff, corr; torch.cuda.empty_cache()

        def feat(bits):
            return (1.0 - 2.0 * bits.astype(np.float32))[:, anchors]
        F_tr, F_te = feat(bt), feat(bte)
        Y = meanH_tr if target == "hidden" else codes[t_tr].astype(np.float32)
        best = None                                               # ridge wd on a val split
        for wd in (1.0, 10.0, 100.0, 1000.0):
            fit = fit_regression(F_tr[~vm], Y[~vm], n_tr[~vm], wd=wd)
            pv = _decode_top1(target, F_tr[vm] @ fit["W"] + fit["b"], Wu, codes)
            acc = weighted_agreement(pv, t_tr[vm], n_tr[vm])
            if best is None or acc > best[0]:
                best = (acc, wd, fit)
        val_top1, wd, _ = best
        fit = fit_regression(F_tr, Y, n_tr, wd=wd)
        pred_te = _decode_top1(target, F_te @ fit["W"] + fit["b"], Wu, codes)
        test_top1 = weighted_agreement(pred_te, t_te, n_te)
        # sanity: the TRUE target must decode back to t* (validates hook/unembed/decode)
        true_te = meanH_te if target == "hidden" else codes[t_te].astype(np.float32)
        sane = weighted_agreement(_decode_top1(target, true_te, Wu, codes), t_te, n_te)
        rec = {"n_features": int(len(anchors)), "wd": wd,
               "val_top1": val_top1, "TEST_top1": test_top1, "sanity_top1": sane}
        summary["encodings"][name] = rec
        print(f"[top1:{name}:{target}] TEST {test_top1:.4f} (val {val_top1:.4f}, "
              f"sanity {sane:.4f}, wd {wd})", flush=True)
        run = _wandb_run(f"{name}-{target}-edu-M{m_train}", {**rec, "encoding": name,
                         "target": target, "m_train": m_train})
        if run is not None:
            run.log({"TEST_top1": test_top1, "val_top1": val_top1, "m_train": m_train})
            run.summary.update(rec); run.finish()
        torch.cuda.empty_cache()
    _write_json(f"{ROOT}/summary_top1_{target}_M{m_train}.json", summary)
    _record("stream-top1", started, {"target": target, "m_train": m_train})
    print(json.dumps(summary), flush=True)
    return summary


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=43200,
              memory=32768, secrets=WANDB_SECRET)
def stream_fit(m_train: int = 16000, m_test: int = 3000, fill_len: int = 61,
               r: int = 8, wd: float = 3e-4, encoding: str = "lsh"):
    """THE REAL THING (Lev's protocol): a fixed FineWeb-Edu test set set aside
    once, a DISJOINT fresh training stream (each doc seen at most once), the GL
    deg-1+2 basis, and an UNGATED L2-regularized fit -- no feature gate.  Scale
    m_train until held-out TEST KL <= 1.0."""
    import os, time
    import torch
    vol.reload()
    _budget("stream-fit", 12.0)
    started = time.time()
    te_path = make_data.local(m_test, r, 0, fill_len, "edu", 0, "edu_test")
    tr_path = make_data.local(m_train, r, 0, fill_len, "edu", 3 * m_test, "edu_tr")
    codes = dict(np.load(f"{ROOT}/codes.npz"))[encoding]
    B = codes.shape[1]

    def collapse(path):
        d = np.load(path)
        ctx = np.concatenate([d["PRE"][d["fiber_id"]], d["G"]], axis=1)
        c, A, n, _ = soft_collapse(ctx, d["P"])
        return c, A, n
    c_te, A_te, n_te = collapse(te_path)
    c_all, A_all, n_all = collapse(tr_path)
    fl = np.load(tr_path)["G"].shape[1]
    # small val split OFF THE TRAIN STREAM for early stopping (test stays pure)
    rng = np.random.default_rng(0)
    vm = rng.random(len(n_all)) < 0.15
    c_tr, A_tr, n_tr = c_all[~vm], A_all[~vm], n_all[~vm]
    c_va, A_va, n_va = c_all[vm], A_all[vm], n_all[vm]
    mu = A_tr.sum(0) / n_tr.sum(); Mtr = float(n_tr.sum())
    nb_full = fl * B
    bt, bv, bte = (context_bits(c_tr, codes), context_bits(c_va, codes),
                   context_bits(c_te, codes))
    Ptr = A_tr / n_tr[:, None]; Pva = A_va / n_va[:, None]; Pte = A_te / n_te[:, None]
    A_c = A_tr - n_tr[:, None] * mu[None, :]
    anchors, ii, jj = _deg12_basis(bt[:, :nb_full], A_c, Mtr)

    def feat(bits):
        S = 1.0 - 2.0 * bits[:, :nb_full].astype(np.float32)
        return np.concatenate([S[:, anchors], S[:, ii] * S[:, jj]], axis=1)

    def slot_kl(off, Pn, n):
        lq = off - torch.logsumexp(off, 1, keepdim=True)
        lp = torch.tensor(np.log(np.clip(Pn, 1e-12, None)), device=off.device)
        return float((torch.tensor(n[:, None] * Pn, device=off.device) * (lp - lq)).sum()
                     / float(n.sum()))
    fit = fit_softmax_slots(feat(bt), Ptr, n_tr, feat(bv), Pva, n_va,
                            steps=4000, lr=0.01, weight_decay=wd)
    W = torch.tensor(fit["W"], device="cuda"); b = torch.tensor(fit["b"], device="cuda")
    train_kl = slot_kl(torch.tensor(feat(bt), device="cuda") @ W + b, Ptr, n_tr)
    test_kl = slot_kl(torch.tensor(feat(bte), device="cuda") @ W + b, Pte, n_te)
    uni = slot_kl(torch.tensor(np.tile(np.log(np.clip(mu, 1e-12, None)), (len(Pte), 1)),
                  device="cuda", dtype=torch.float32), Pte, n_te)
    out = {"corpus": "fineweb-edu", "encoding": encoding, "m_train": m_train,
           "m_test": m_test, "wd": wd, "n_features": int(len(anchors) + len(jj)),
           "TRAIN_kl": train_kl, "TEST_kl": test_kl, "TEST_unigram_kl": uni}
    print("[stream-fit] " + json.dumps(out), flush=True)
    run = _wandb_run(f"{encoding}-edu-M{m_train}", out)
    if run is not None:
        run.log({k: out[k] for k in ("TRAIN_kl", "TEST_kl", "TEST_unigram_kl",
                                     "n_features")} | {"m_train": m_train})
        run.summary.update(out)
        run.finish()
    _write_json(f"{ROOT}/summary_streamfit_{encoding}_M{m_train}.json", out)
    _record("stream-fit", started, {"m_train": m_train})
    return out


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=21600,
              memory=32768, secrets=WANDB_SECRET)
def sensitivity(m_fibers: int = 1000, g: int = 16, resample: str = "conditional",
                corpus: str = "fineweb", positions: str = "", seed: int = 0,
                batch_fibers: int = 16, fill_len: int = 61):
    """DEGREE BOUNDS VIA SENSITIVITY (prelims.typ) for the FULL-VOCAB TOP-1
    target -- the 512-slot projection is deliberately ABSENT from this stage
    (the arc moved to top-1 agreement, see stream_top1; do NOT reintroduce
    slot projections here).  f(x) = one-hot of the teacher's argmax token;
    Sens_b = P(two conditional resamples of the token b back flip the top-1);
    Var = P(two random contexts disagree on the top-1).  Degree bound
    d(eps) = 4*S/eps as before -- the identity is target-agnostic."""
    import os, time
    vol.reload()
    pos = sorted(int(p) for p in positions.split(",")) if positions else SENS_POSITIONS
    assert resample in ("conditional", "uniform")
    assert 0 <= min(pos) and max(pos) <= CTX - 2
    _budget("sensitivity", 2.0)
    started = time.time()
    model, tok, q = _load_teacher()
    spans = _stream_spans(tok, n_spans=m_fibers, corpus=CORPORA[corpus])
    # baseline pass on unmodified spans: fiber-level top-1 anchors Var_tot
    import torch
    sp_all = torch.tensor(spans, dtype=torch.long, device="cuda")
    T0 = np.empty(m_fibers, dtype=np.int64)
    for lo in range(0, m_fibers, 32):
        T0[lo:lo + 32] = _terminal_probs(model, sp_all[lo:lo + 32], q) \
            .argmax(-1).cpu().numpy()
    var_tot = _top1_variance(T0)
    print(f"[sens] top-1 var_tot {var_tot:.5f} over {m_fibers} spans", flush=True)
    run = _wandb_run(f"sens-top1-{resample}-{corpus}-M{m_fibers}",
                     {"model": MODEL_ID, "corpus": corpus, "resample": resample,
                      "target": "top1", "m_fibers": m_fibers, "g": g,
                      "positions": pos})
    shard_dir = f"{ROOT}/sens_top1_shards_{resample}_{corpus}_M{m_fibers}_g{g}"
    os.makedirs(shard_dir, exist_ok=True)
    sens = np.empty(len(pos)); se = np.empty(len(pos))
    sens_fiber = np.empty((len(pos), m_fibers))
    cum = 0.0
    for i, b in enumerate(pos):
        sp = f"{shard_dir}/b{b}.npz"
        if os.path.exists(sp):
            sf = np.load(sp)["sens_fiber"]
        else:
            T = _resample_and_label(model, spans, b, g, q, resample,
                                    batch_fibers, seed=seed + 13 * b)
            sf = _sens_from_top1(T)
            np.savez_compressed(sp + ".tmp.npz", sens_fiber=sf)
            os.replace(sp + ".tmp.npz", sp)
            vol.commit()
        sens_fiber[i], sens[i] = sf, sf.mean()
        se[i] = sf.std(ddof=1) / math.sqrt(len(sf))
        cum += sens[i]
        print(f"[sens] b={b} Sens={sens[i]:.5f} +- {se[i]:.5f} cum={cum:.5f}",
              flush=True)
        if run is not None:
            run.log({"back_offset": b, "sens_b": sens[i], "sens_se": se[i],
                     "sens_cum": cum})
    report = _sens_report(pos, sens, var_tot)
    report["se"] = se[np.argsort(pos)].tolist()
    out = dict({"model": MODEL_ID, "corpus": corpus, "resample": resample,
                "target": "top1", "m_fibers": m_fibers, "g": g}, **report)
    out["comparison"] = {
        "note": "d counts TOKEN coordinates; Sens_b = P(resampling token b "
                "flips the teacher's full-vocab top-1), Var = P(two contexts "
                "disagree on top-1) -- the top-1 agreement target's units"}
    np.savez_compressed(f"{ROOT}/sensitivity_top1_{resample}_{corpus}_M{m_fibers}.npz",
                        positions=np.asarray(pos), sens=sens, se=se,
                        sens_fiber=sens_fiber, T0=T0, var_tot=var_tot)
    _write_json(f"{ROOT}/sensitivity_top1_{resample}_{corpus}_M{m_fibers}.json", out)
    if run is not None:
        run.summary.update({k: v for k, v in out.items() if k != "comparison"})
        run.finish()
    _record("sensitivity", started, {"m_fibers": m_fibers, "g": g,
                                     "resample": resample, "target": "top1"})
    print("[sens] " + json.dumps(out), flush=True)
    return out


@app.function(image=image, gpu="A100-40GB", volumes={"/cache": vol}, timeout=43200,
              memory=32768, secrets=WANDB_SECRET)
def gl_tree_top1(m_fibers: int = 1500, g: int = 16, depth: int = 6,
                 fill_len: int = 61, tau: float = 0.02, max_width: int = 128,
                 flat_m: int = 16000, flat_r: int = 8, variant: str = "raw"):
    """THE ACTUAL Dataset GL for full-vocab top-1: forked_gl_tree on the teacher
    HIDDEN-state target recovers heavy multi-degree Walsh characters (per
    encoding), then ALL of them are fit on the flat table -> unembed -> top-1.
    NO deg-1 correlation shortcut, NO projection, NO per-degree feature cap.
    Intermittent W&B logging per tree level AND per fit degree of how much of
    the target we account for: TEST top-1, R2 (hidden variance explained), and
    the fraction of recovered Fourier (psi) mass.
    variant="resid": subtract the exact deg-1 flat fit from H AND per-fiber
    center inside the fork target (unary + rollout-density pollutants zeroed
    on purpose -- raw psi had corr 0.998 with the pure-density spectrum and
    ZERO top-512 overlap with the functional ranking at deg-2), so the tree
    prunes on within-fiber covariance; the ladder then fits deg-1 + the
    discovered chars jointly."""
    import os, time
    import torch
    vol.reload()
    _budget("gl-tree-top1", 6.0)
    started = time.time()
    for j in range(depth):                                            # H-fork levels
        p = _oracle_table_path(fill_len, m_fibers, g, j)
        if not os.path.exists(p) or "H" not in np.load(p).files:
            make_oracle_data.local(m_fibers, g, j, fill_len)
    te_tbl = make_data.local(3000, flat_r, 0, fill_len, "edu", 0, "edu_test")
    tr_tbl = make_data.local(flat_m, flat_r, 0, fill_len, "edu", 9000, "edu_tr")
    te_lbl = relabel_hidden.local(3000, fill_len, flat_r, "edu_test")
    tr_lbl = relabel_hidden.local(flat_m, fill_len, flat_r, "edu_tr")
    Wu = np.load(f"{ROOT}/lm_head.npz")["Wu"].astype(np.float32)
    ztab = dict(np.load(f"{ROOT}/codes.npz"))

    def collapse(tbl, lbl):
        d = np.load(tbl); L = np.load(lbl)
        ctx = np.concatenate([d["PRE"][d["fiber_id"]], d["G"]], axis=1)
        rows, inv, cnt = np.unique(ctx, axis=0, return_inverse=True, return_counts=True)
        Hs = np.zeros((len(rows), L["H"].shape[1]), np.float64)
        np.add.at(Hs, inv, L["H"].astype(np.float64))
        tg = np.empty(len(rows), np.int64); tg[inv] = L["tstar"]
        return rows, Hs, cnt.astype(np.float64), tg
    c_tr, H_tr, n_tr, t_tr = collapse(tr_tbl, tr_lbl)
    c_te, H_te, n_te, t_te = collapse(te_tbl, te_lbl)
    meanH_tr, meanH_te = H_tr / n_tr[:, None], H_te / n_te[:, None]
    rng = np.random.default_rng(0); vm = rng.random(len(n_tr)) < 0.15

    def r2_hidden(Hpred, Htrue, n):                                   # frac of H variance explained
        mu = (Htrue * n[:, None]).sum(0) / n.sum()
        ss_res = float((n[:, None] * (Htrue - Hpred) ** 2).sum())
        ss_tot = float((n[:, None] * (Htrue - mu[None, :]) ** 2).sum())
        return 1.0 - ss_res / ss_tot

    summary = {"depth": depth, "tau": tau, "variant": variant, "encodings": {}}
    sfx = "" if variant == "raw" else f"_{variant}"
    for name in ("lsh", "ctrl"):
        codes = ztab[name]; B = codes.shape[1]; nb = depth * B
        bt = context_bits(c_tr, codes)[:, :nb]; bte = context_bits(c_te, codes)[:, :nb]
        deg1_W = None
        if variant == "resid":                                    # exact unary base
            f1 = fit_deg1_exact(bt, meanH_tr.astype(np.float32), n_tr)
            deg1_W = f1["W"]
        levels = _load_fork_levels(fill_len, m_fibers, g, depth, codes,
                                   target="hidden", deg1_W=deg1_W,
                                   fiber_center=(variant == "resid"))
        # null guardrail: psi of level-0 deg-1 chars with row-permuted F (pairing
        # broken) must sit BELOW the keep threshold, else the gate is vacuous
        # and the tree would enumerate noise (the tau-scale bug class)
        bits0, F0, gid0 = levels[0]
        perm = np.random.default_rng(0).permutation(len(F0))
        null = forked_gl_tree([(bits0, F0[perm], gid0)], B, tau=0.0, max_width=B)
        null_max = float(null["psi"].max()) if len(null["psi"]) else 0.0
        thresh = tau * tau / 4.0
        print(f"[gltree:{name}] null_max {null_max:.2e} vs thresh {thresh:.2e}",
              flush=True)
        assert null_max < thresh, \
            f"vacuous gate: null_max {null_max:.2e} >= tau^2/4 {thresh:.2e}"
        tree = forked_gl_tree(levels, B, tau, max_width=max_width,
                              progress=lambda j, D, kept: print(
                                  f"[gltree:{name}] level {j+1}/{D} kept {len(kept)}",
                                  flush=True))
        masks, psi = tree["masks"], tree["psi"]
        deg = np.array([int(m.reshape(depth, B).any(1).sum()) for m in masks]) \
            if len(masks) else np.zeros(0, int)
        print(f"[gltree:{name}] {len(masks)} chars, deg hist "
              f"{np.bincount(deg, minlength=depth+1).tolist()}", flush=True)
        sane = weighted_agreement(unembed_top1(meanH_te, Wu), t_te, n_te)
        # persist masks so the fit is re-tunable without re-running the tree
        np.savez_compressed(f"{ROOT}/gltree_top1_masks_{name}_d{depth}{sfx}.npz",
                            masks=masks, psi=psi, deg=deg); vol.commit()
        run = _wandb_run(f"gltree-{name}-hidden-d{depth}{sfx}",
                         {"encoding": name, "depth": depth, "tau": tau,
                          "variant": variant,
                          "n_chars": int(len(masks)), "sanity": sane})
        psitot = float(psi.sum()) if len(psi) else 1.0
        # fit the tree's characters HEAVIEST-FIRST (masks are psi-sorted) in blocks
        # up to a memory bound; log cumulative TEST top-1, R2 (hidden variance
        # explained) and psi_frac (fraction of recovered Fourier mass covered) --
        # the honest "how much of the coeffs we account for"
        ladder = []
        max_chars = min(5000, len(masks))
        ks = list(range(1000, max_chars + 1, 1000))
        if not ks or ks[-1] < max_chars:
            ks.append(max_chars)
        base_masks = np.eye(nb, dtype=np.uint8) if variant == "resid" else \
            np.zeros((0, nb), np.uint8)                           # deg-1 base joins the fit
        if variant == "resid":
            ks = [0] + ks                                         # k=0 = deg-1-only rung
        for k in ks:
            md = np.concatenate([base_masks, masks[:k]])          # top-k by psi
            Ftr = parity_features(bt, md); Fte = parity_features(bte, md)
            best = None
            for wd in (10.0, 100.0, 1000.0):
                fit = fit_regression(Ftr[~vm], meanH_tr[~vm], n_tr[~vm], wd=wd)
                acc = weighted_agreement(
                    unembed_top1(Ftr[vm] @ fit["W"] + fit["b"], Wu), t_tr[vm], n_tr[vm])
                if best is None or acc > best[0]:
                    best = (acc, wd)
            wd = best[1]
            fit = fit_regression(Ftr, meanH_tr, n_tr, wd=wd)
            Hpred = Fte @ fit["W"] + fit["b"]
            rec = {"n_chars": int(k), "n_deg3plus": int((deg[:k] >= 3).sum()),
                   "TEST_top1": weighted_agreement(unembed_top1(Hpred, Wu), t_te, n_te),
                   "R2_hidden": r2_hidden(Hpred, meanH_te, n_te),
                   "psi_frac": float(psi[:k].sum() / psitot), "wd": wd}
            ladder.append(rec)
            print(f"[gltree:{name}] top-{k} psi ({rec['n_deg3plus']} deg>=3) "
                  f"top1 {rec['TEST_top1']:.4f} R2 {rec['R2_hidden']:.4f} "
                  f"psi_frac {rec['psi_frac']:.3f} (sanity {sane:.3f})", flush=True)
            if run is not None:
                run.log(rec)
            del Ftr, Fte; torch.cuda.empty_cache()
        if run is not None:
            run.summary.update({"final_top1": ladder[-1]["TEST_top1"] if ladder else 0.0,
                                "sanity": sane}); run.finish()
        summary["encodings"][name] = {"n_chars": int(len(masks)), "sanity": sane,
                                      "ladder": ladder}
        torch.cuda.empty_cache()
    _write_json(f"{ROOT}/summary_gltree_top1_d{depth}{sfx}.json", summary)
    _record("gl-tree-top1", started, {"depth": depth, "variant": variant})
    print(json.dumps(summary), flush=True)
    return summary


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=43200,
              memory=32768, secrets=WANDB_SECRET)
def refit_top1(depth: int = 6, fill_len: int = 61, flat_m: int = 16000, flat_r: int = 8,
               per_deg: int = 3000, train_cap: int = 40000):
    """Re-fit the SAVED gl_tree_top1 masks DEGREE-FIRST (all deg-1, then top-K
    deg-2 by psi, then deg-3, ... cumulative) -- the right order, since psi is
    not comparable across tree levels and psi-order buries the predictive
    low-degree characters.  No tree re-run.  Logs per-degree TEST top-1, R2
    (hidden variance explained) and cumulative psi_frac, lsh vs ctrl."""
    import os, time
    import torch
    vol.reload()
    _budget("refit-top1", 2.0)
    started = time.time()
    te_tbl = make_data.local(3000, flat_r, 0, fill_len, "edu", 0, "edu_test")
    tr_tbl = make_data.local(flat_m, flat_r, 0, fill_len, "edu", 9000, "edu_tr")
    te_lbl = relabel_hidden.local(3000, fill_len, flat_r, "edu_test")
    tr_lbl = relabel_hidden.local(flat_m, fill_len, flat_r, "edu_tr")
    Wu = np.load(f"{ROOT}/lm_head.npz")["Wu"].astype(np.float32)
    ztab = dict(np.load(f"{ROOT}/codes.npz"))

    c_tr, mH_tr, n_tr, t_tr = _collapse_table(tr_tbl, tr_lbl)
    c_te, mH_te, n_te, t_te = _collapse_table(te_tbl, te_lbl)
    rng = np.random.default_rng(0)
    if len(n_tr) > train_cap:                                        # subsample train (fit memory)
        sel = rng.choice(len(n_tr), train_cap, replace=False)
        c_tr, mH_tr, n_tr, t_tr = c_tr[sel], mH_tr[sel], n_tr[sel], t_tr[sel]
    vm = rng.random(len(n_tr)) < 0.15

    def r2_hidden(Hp, Ht, n):
        mu = (Ht * n[:, None]).sum(0) / n.sum()
        return 1.0 - float((n[:, None] * (Ht - Hp) ** 2).sum()) / \
            float((n[:, None] * (Ht - mu[None, :]) ** 2).sum())
    summary = {"per_deg": per_deg, "encodings": {}}
    for name in ("lsh", "ctrl"):
        mp = f"{ROOT}/gltree_top1_masks_{name}_d{depth}.npz"
        if not os.path.exists(mp):
            print(f"[refit] no masks for {name} yet, skipping", flush=True); continue
        d = np.load(mp); masks, psi, deg = d["masks"], d["psi"], d["deg"]
        codes = ztab[name]; B = codes.shape[1]; nb = depth * B
        bt = context_bits(c_tr, codes)[:, :nb]; bte = context_bits(c_te, codes)[:, :nb]
        sane = weighted_agreement(unembed_top1(mH_te, Wu), t_te, n_te)
        psitot = float(psi.sum())
        run = _wandb_run(f"refit-{name}-degfirst-d{depth}",
                         {"encoding": name, "depth": depth, "per_deg": per_deg})
        idx_used, ladder = [], []
        for D in range(1, depth + 1):
            sel_D = np.flatnonzero(deg == D)
            if not len(sel_D):
                continue
            idx_used.extend(sel_D[np.argsort(-psi[sel_D])][:per_deg].tolist())
            md = masks[idx_used]
            Ftr = parity_features(bt, md); Fte = parity_features(bte, md)
            best = None
            for wd in (10.0, 100.0, 1000.0):
                fit = fit_regression(Ftr[~vm], mH_tr[~vm], n_tr[~vm], wd=wd)
                acc = weighted_agreement(
                    unembed_top1(Ftr[vm] @ fit["W"] + fit["b"], Wu), t_tr[vm], n_tr[vm])
                if best is None or acc > best[0]:
                    best = (acc, wd)
            wd = best[1]; fit = fit_regression(Ftr, mH_tr, n_tr, wd=wd)
            Hp = Fte @ fit["W"] + fit["b"]
            rec = {"degree": D, "n_chars": len(md),
                   "TEST_top1": weighted_agreement(unembed_top1(Hp, Wu), t_te, n_te),
                   "R2_hidden": r2_hidden(Hp, mH_te, n_te),
                   "psi_frac": float(psi[idx_used].sum() / psitot), "wd": wd}
            ladder.append(rec)
            print(f"[refit:{name}] deg<={D} ({len(md)} chars) top1 {rec['TEST_top1']:.4f} "
                  f"R2 {rec['R2_hidden']:.4f} psi_frac {rec['psi_frac']:.3f} "
                  f"(sanity {sane:.3f})", flush=True)
            if run is not None:
                run.log(rec)
            del Ftr, Fte; torch.cuda.empty_cache()
        if run is not None:
            run.summary.update({"final_top1": ladder[-1]["TEST_top1"] if ladder else 0.0,
                                "sanity": sane}); run.finish()
        summary["encodings"][name] = {"sanity": sane, "ladder": ladder}
    _write_json(f"{ROOT}/summary_refit_top1_d{depth}.json", summary)
    _record("refit-top1", started, {"per_deg": per_deg})
    print(json.dumps(summary), flush=True)
    return summary


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=43200,
              memory=32768, secrets=WANDB_SECRET)
def dl_fourier(m_train: int = 16000, m_test: int = 3000, fill_len: int = 61,
               r: int = 8, k1: int = 512, k2: int = 512, k3: int = 256,
               lam: float = 0.1, steps: int = 3000, lr: float = 0.02,
               batch: int = 8192, mlp_hidden: int = 2048, seed: int = 0):
    """LEARN the Fourier features instead of GL-searching them: an explicit
    degree profile (k1 deg-1, k2 deg-2, k3 deg-3) of STE-selected parities +
    linear head, trained end-to-end on the standardized hidden target with an
    activation-decorrelation diversity term.  Evaluated four ways per encoding
    (full-vocab TEST top-1): end-to-end, ridge-refit of the hardened masks
    (comparable to stream_top1 / refit_top1), random masks with the SAME
    degree profile (what learning buys), and an unconstrained MLP on the raw
    bits (the ceiling any function of these bits reaches at this data size)."""
    import time
    import torch
    vol.reload()
    _budget("dl-fourier", 4.0)
    started = time.time()
    te_tbl = make_data.local(m_test, r, 0, fill_len, "edu", 0, "edu_test")
    tr_tbl = make_data.local(m_train, r, 0, fill_len, "edu", 3 * m_test, "edu_tr")
    te_lbl = relabel_hidden.local(m_test, fill_len, r, "edu_test")
    tr_lbl = relabel_hidden.local(m_train, fill_len, r, "edu_tr")
    Wu = np.load(f"{ROOT}/lm_head.npz")["Wu"].astype(np.float32)
    ztab = dict(np.load(f"{ROOT}/codes.npz"))
    c_tr, mH_tr, n_tr, t_tr = _collapse_table(tr_tbl, tr_lbl)
    c_te, mH_te, n_te, t_te = _collapse_table(te_tbl, te_lbl)
    vm = np.random.default_rng(0).random(len(n_tr)) < 0.15
    wmean = (mH_tr * n_tr[:, None]).sum(0) / n_tr.sum()
    std = mH_tr.std(0) + 1e-6
    Hs = ((mH_tr - wmean[None, :]) / std).astype(np.float32)
    sane = weighted_agreement(unembed_top1(mH_te, Wu), t_te, n_te)
    profile = (k1, k2, k3)
    summary = {"m_train": m_train, "profile": list(profile), "lam": lam,
               "steps": steps, "sanity_top1": sane, "encodings": {}}

    def ridge_top1(Ftr, Fte):
        """The established comparable fit: wd sweep on val top-1, full-train
        refit on the RAW mean hidden target, full-vocab decode."""
        best = None
        for wd in (1.0, 10.0, 100.0, 1000.0):
            fit = fit_regression(Ftr[~vm], mH_tr[~vm], n_tr[~vm], wd=wd)
            acc = weighted_agreement(
                unembed_top1(Ftr[vm] @ fit["W"] + fit["b"], Wu), t_tr[vm], n_tr[vm])
            if best is None or acc > best[0]:
                best = (acc, wd)
        val_top1, wd = best
        fit = fit_regression(Ftr, mH_tr, n_tr, wd=wd)
        test = weighted_agreement(
            unembed_top1(Fte @ fit["W"] + fit["b"], Wu), t_te, n_te)
        return test, val_top1, wd

    for name in ("lsh", "ctrl"):
        codes = ztab[name]; B = codes.shape[1]; nb = fill_len * B
        bt = context_bits(c_tr, codes)[:, :nb]
        bte = context_bits(c_te, codes)[:, :nb]
        res = learn_fourier_masks(bt, Hs, n_tr, vm, profile, lam, steps, lr,
                                  batch, seed=seed)
        # end-to-end: hardened features (order matches W rows), un-standardize
        Hp = parity_features(bte, res["masks"]) @ res["W"] + res["b"]
        e2e = weighted_agreement(
            unembed_top1(Hp * std[None, :] + wmean[None, :], Wu), t_te, n_te)
        # ridge-refit of the deduped learned masks (drop the collapsed empty row)
        um = np.unique(res["masks"], axis=0)
        um = um[um.sum(1) > 0]
        refit, refit_val, wd = ridge_top1(parity_features(bt, um),
                                          parity_features(bte, um))
        # random masks, same degree profile, same refit
        rm = random_profile_masks(nb, profile, seed)
        rand, _, _ = ridge_top1(parity_features(bt, rm), parity_features(bte, rm))
        torch.cuda.empty_cache()
        # unconstrained ceiling: MLP on the raw bits, same standardized target
        mlp = fit_mlp_hidden(bt, Hs, n_tr, vm, bte, hidden=mlp_hidden,
                             steps=steps, batch=batch, seed=seed)
        mlp_top1 = weighted_agreement(
            unembed_top1(mlp["pred_te"] * std[None, :] + wmean[None, :], Wu),
            t_te, n_te)
        deg_hist = {str(d): int((res["deg"] == d).sum()) for d in range(4)}
        rec = {"TEST_top1_e2e": e2e, "TEST_top1_refit": refit,
               "TEST_top1_random": rand, "TEST_top1_mlp": mlp_top1,
               "val_top1_refit": refit_val, "wd": wd, "val_mse": res["val_mse"],
               "mlp_val_mse": mlp["val_mse"], "n_unique_masks": int(len(um)),
               "n_collapsed": res["n_collapsed"], "deg_hist": deg_hist}
        summary["encodings"][name] = rec
        print(f"[dlfourier:{name}] e2e {e2e:.4f} refit {refit:.4f} "
              f"random {rand:.4f} mlp {mlp_top1:.4f} (sanity {sane:.4f}, "
              f"{len(um)} masks, {res['n_collapsed']} collapsed)", flush=True)
        np.savez_compressed(
            f"{ROOT}/dlfourier_masks_{name}_M{m_train}_K{k1}_{k2}_{k3}.npz",
            masks=res["masks"], deg=res["deg"], W=res["W"], b=res["b"])
        run = _wandb_run(f"dlfourier-{name}-M{m_train}-K{k1}+{k2}+{k3}",
                         {**rec, "encoding": name, "lam": lam, "steps": steps})
        if run is not None:
            for s, v in res["history"]:
                run.log({"val_mse": v, "step": s})
            run.summary.update(rec); run.finish()
        torch.cuda.empty_cache()
    _write_json(f"{ROOT}/summary_dlfourier_M{m_train}_K{k1}_{k2}_{k3}.json", summary)
    _record("dl-fourier", started, {"profile": list(profile), "lam": lam})
    print(json.dumps(summary), flush=True)
    return summary


def _plain_path(ctx, n_spans, skip, tag):
    return f"{ROOT}/plain_ctx{ctx}_N{n_spans}_s{skip}_{tag}.npz"


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=43200,
              memory=32768, secrets=HF_SECRET)
def make_plain_data(n_spans: int = 1_000_000, ctx: int = CTX, skip: int = 0,
                    tag: str = "plain_tr", span_lo: int = 0, span_hi: int = 0,
                    batch: int = 128):
    """PLAIN streamed supervision: FineWeb-Edu spans, ONE teacher forward each
    (pre-head hidden h at the last position + full-vocab argmax t*).  No
    fibers, no fills, no pairing -- those are Dataset-GL requirements; anything
    that only needs (context-bits, teacher-output) pairs scales to millions of
    contexts by streaming.  Sharded like make_data: span_lo/span_hi-restricted
    workers write shards; the unrestricted call concatenates.  Saves tokens
    int32 (N, ctx), H fp16 (N, d_model), tstar int32."""
    import os, time
    import torch
    vol.reload()
    out = _plain_path(ctx, n_spans, skip, tag)
    if os.path.exists(out):
        return out
    _budget("plain-data", 15.0)
    started = time.time()
    model, tok, q = _load_teacher()
    shard_dir = f"{ROOT}/plain_shards_ctx{ctx}_N{n_spans}_s{skip}_{tag}"
    os.makedirs(shard_dir, exist_ok=True)
    chunk = 25_000
    hi = span_hi if span_hi else n_spans
    todo = [lo for lo in range(span_lo, hi, chunk)
            if not os.path.exists(f"{shard_dir}/s{lo}.npz")]
    if todo:
        # one stream pass covers this worker's whole missing range (skip counts
        # qualifying spans from the stream start, so shards are deterministic)
        base = todo[0]
        spans = _stream_spans(tok, min(hi, todo[-1] + chunk) - base, span_len=ctx,
                              corpus=FINEWEB_EDU, skip=skip + base)
        for lo in todo:
            sl = spans[lo - base: lo - base + min(chunk, hi - lo)]
            H = np.empty((len(sl), 0), dtype=np.float16)
            hs, ts = [], []
            for b in range(0, len(sl), batch):
                ids = torch.tensor(sl[b:b + batch], dtype=torch.long, device="cuda")
                h, t = _capture_hidden(model, ids, q)
                hs.append(h.cpu().numpy().astype(np.float16))
                ts.append(t.cpu().numpy().astype(np.int32))
            sp = f"{shard_dir}/s{lo}.npz"
            np.savez(sp + ".tmp.npz", tokens=sl.astype(np.int32),
                     H=np.concatenate(hs), tstar=np.concatenate(ts))
            os.replace(sp + ".tmp.npz", sp)
            vol.commit()
            print(f"[plain] {min(lo + chunk, hi)}/{n_spans} spans "
                  f"(worker {span_lo}:{hi})", flush=True)
    if span_lo != 0 or (span_hi and span_hi < n_spans):
        return f"plain shards {span_lo}:{hi} of N{n_spans} done"
    parts = [np.load(f"{shard_dir}/s{lo}.npz") for lo in range(0, n_spans, chunk)]
    # uncompressed savez: fp16 hidden states barely compress and zlib on ~2.5GB
    # costs minutes per save
    np.savez(out.replace(".npz", ".tmp.npz"),
             tokens=np.concatenate([p["tokens"] for p in parts]),
             H=np.concatenate([p["H"] for p in parts]),
             tstar=np.concatenate([p["tstar"] for p in parts]))
    os.replace(out.replace(".npz", ".tmp.npz"), out)
    vol.commit()
    _record("plain-data", started, {"n_spans": n_spans, "tag": tag})
    print(f"saved {out}", flush=True)
    return out


@app.function(image=image, volumes={"/cache": vol}, timeout=43200, memory=8192)
def gen_plain_data(n_spans: int = 1_000_000, ctx: int = CTX, skip: int = 0,
                   tag: str = "plain_tr", n_shards: int = 8):
    """Fan-out plain-data generation: N GPU workers label disjoint span ranges
    (deterministic stream positions => bytewise identical to serial), then the
    unrestricted call concatenates."""
    import os
    vol.reload()
    out = _plain_path(ctx, n_spans, skip, tag)
    if os.path.exists(out):
        return out
    chunk = 25_000
    n_chunks = (n_spans + chunk - 1) // chunk
    per = (n_chunks + n_shards - 1) // n_shards
    handles = []
    for wkr in range(n_shards):
        lo = wkr * per * chunk
        hi = min((wkr + 1) * per * chunk, n_spans)
        if lo >= n_spans:
            break
        handles.append(make_plain_data.spawn(n_spans, ctx, skip, tag, lo, hi))
    print(f"[gen-plain] {len(handles)} workers x ~{per * chunk} spans", flush=True)
    for h in handles:
        print(h.get(), flush=True)
    return make_plain_data.remote(n_spans, ctx, skip, tag)


GS_MILESTONES = [1_000, 10_000, 50_000] + list(range(100_000, 1_000_001, 100_000))


@app.function(image=image, gpu="H100", volumes={"/cache": vol}, timeout=43200,
              memory=65536, secrets=WANDB_SECRET + HF_SECRET)
def grad_sense(n_train: int = 1_000_000, n_test: int = 50_000, win: int = 61,
               target_chars: int = 1_000_000, chunk: int = 16384,
               steps: int = 300, min_steps: int = 64, patience: int = 3,
               lam: float = 0.1, batch: int = 4096, lr: float = 0.02,
               kappa: float = 1.25, rounds_cap: int = 200,
               encoding: str = "lsh", mlp: int = 0, seed: int = 0):
    """Gradient-sensed sparse Fourier spectroscopy (gradient matching pursuit)
    on the UNIT-NORMALIZED centered pre-head target (Parseval: total mass <= 1,
    psi absolute).  Two stages (Lev's KISS structure): (1) ALL degree-1
    characters fit EXACTLY once (closed form) and subtracted -- the residual
    has no degree-1 content, so constant/duplicate-bit equivalence classes are
    dead on arrival; (2) ROUNDS of [sense the heaviest fresh DEGREE-2/3
    characters in the residual via soft-annealed selection + -log psi] ->
    [SEQUENTIAL matching-pursuit deflation (batch deflation amplifies on-data
    duplicate clusters -- the 2026-07-15 divergence)].  Escalation-friendly:
    a checkpoint on the volume resumes the harvest, so runs with growing
    --target-chars (1k -> 10k -> 50k -> ...) each only harvest the delta and
    log held-out top-1 + KL at every character milestone crossed (the
    character-scaling curve)."""
    import os, time
    import torch
    vol.reload()
    _budget("grad-sense", 30.0)
    started = time.time()
    te_p = make_plain_data.local(n_test, CTX, 0, "plain_test")
    tr_p = make_plain_data.local(n_train, CTX, 3 * n_test, "plain_tr")
    Wu = np.load(f"{ROOT}/lm_head.npz")["Wu"].astype(np.float32)
    codes = dict(np.load(f"{ROOT}/codes.npz"))[encoding]
    B = codes.shape[1]; nb = win * B
    dtr, dte = np.load(tr_p), np.load(te_p)
    bits_tr = context_bits(dtr["tokens"].astype(np.int64), codes)[:, :nb]
    bits_te = context_bits(dte["tokens"].astype(np.int64), codes)[:, :nb]
    t_te = dte["tstar"].astype(np.int64)
    w_tr = np.ones(len(bits_tr)); w_te = np.ones(len(bits_te))

    def normalize(H):
        F = H.astype(np.float32)
        return F / (np.linalg.norm(F, axis=1, keepdims=True) + 1e-8)
    F_tr = normalize(dtr["H"])
    c0 = F_tr.mean(0)
    sane = weighted_agreement(unembed_top1(dte["H"].astype(np.float32), Wu), t_te, w_te)
    mass0 = float(((F_tr - c0) ** 2).sum(1).mean())               # Parseval: <= 1
    print(f"[gradsense:{encoding}] sanity {sane:.4f} total mass {mass0:.4f} "
          f"n={nb} bits x {len(bits_tr)} contexts", flush=True)

    dev = "cuda"
    bits_tr_t = torch.tensor(bits_tr, device=dev)
    bits_te_t = torch.tensor(bits_te, device=dev)
    G_res = torch.tensor(F_tr - c0[None, :], dtype=torch.float32, device=dev)
    del F_tr
    vm = np.random.default_rng(0).random(len(bits_tr)) < 0.05
    n_val = int(min(vm.sum(), 32768))
    trs = np.random.default_rng(1).choice(np.flatnonzero(~vm),
                                          min(50_000, int((~vm).sum())), replace=False)
    t_trs = dtr["tstar"].astype(np.int64)[trs]

    # ---- resume from checkpoint (escalation runs harvest only the delta)
    ckpt = f"{ROOT}/gradsense_ckpt_{encoding}_win{win}_N{n_train}.npz"
    idx_all = np.zeros((0, 4), np.int16); psi_all = np.zeros(0)
    C_all = np.zeros((0, G_res.shape[1]), np.float32)
    seen, fit1 = set(), None
    if os.path.exists(ckpt):
        z = np.load(ckpt)
        idx_all, psi_all = z["idx"], z["psi"]
        C_all = z["C"].astype(np.float32)
        fit1 = {"W": z["W1"].astype(np.float32), "b": z["b1"].astype(np.float32)}
        dedupe_indices(idx_all, psi_all, seen)                    # rebuild the seen set
        print(f"[gradsense:{encoding}] resume {len(idx_all)} chars", flush=True)

    def save_ckpt():
        np.savez(ckpt + ".tmp.npz", idx=idx_all, psi=psi_all,
                 C=C_all.astype(np.float16), W1=fit1["W"].astype(np.float16),
                 b1=fit1["b"])
        os.replace(ckpt + ".tmp.npz", ckpt)

    def apply_chars(bits_t, target, idx, C, sign=1.0, cchunk=8192):
        Ct = torch.as_tensor(C, dtype=torch.float32, device=dev)
        for klo in range(0, len(idx), cchunk):
            F = xor_parity_features(bits_t, idx[klo:klo + cchunk])
            target += sign * (F @ Ct[klo:klo + cchunk])
        return target

    def deg1_pred(bits_t, cchunk=131072):
        W1 = torch.as_tensor(fit1["W"], device=dev)
        b1 = torch.as_tensor(fit1["b"], device=dev)
        out = torch.empty((len(bits_t), G_res.shape[1]), device=dev)
        for lo in range(0, len(bits_t), cchunk):
            out[lo:lo + cchunk] = \
                (1.0 - 2.0 * bits_t[lo:lo + cchunk].float()) @ W1 + b1
        return out

    # ---- stage 1: ALL degree-1 characters, exactly, once (Lev's step 1) --
    # the residual then has NO degree-1 content on train, so rounds cannot
    # rediscover deg-1 (incl. every constant/duplicate-bit equivalence class)
    if fit1 is None:
        t1 = time.time()
        fit1 = fit_deg1_exact(bits_tr_t, G_res, w_tr, device=dev)
        print(f"[gradsense:{encoding}] deg-1 exact fit ({nb} chars) "
              f"in {time.time() - t1:.0f}s", flush=True)
    G_res -= deg1_pred(bits_tr_t)
    trs_t = torch.tensor(trs, device=dev)
    Fhat_te = deg1_pred(bits_te_t)
    Fhat_trs = deg1_pred(bits_tr_t[trs_t])
    if len(idx_all):
        G_res = apply_chars(bits_tr_t, G_res, idx_all, C_all, sign=-1.0)
        Fhat_te = apply_chars(bits_te_t, Fhat_te, idx_all, C_all)
        Fhat_trs = apply_chars(bits_tr_t[trs_t], Fhat_trs, idx_all, C_all)

    # KL reference: teacher probs reconstructed from stored hidden states;
    # normalized predictions rescaled by the constant mean teacher norm
    # (softmax is not scale-invariant; no per-row oracle scale is used)
    s_bar = float(np.linalg.norm(dtr["H"][:100_000].astype(np.float32), axis=1).mean())
    H_te_f32 = dte["H"].astype(np.float32)

    def top1(Fhat, tstar, w):
        pred = unembed_top1(Fhat.cpu().numpy() + c0[None, :], Wu)
        return weighted_agreement(pred, tstar, w)

    def test_kl(Fhat):
        return _full_kl((Fhat.cpu().numpy() + c0[None, :]) * s_bar, H_te_f32,
                        Wu, w_te, device=dev)
    kl0 = test_kl(torch.zeros_like(Fhat_te))                       # k=0 mean-only reference
    deg1_base_te = deg1_pred(bits_te_t)                            # the deg-1-only baseline
    deg1_top1, deg1_kl = top1(deg1_base_te, t_te, w_te), test_kl(deg1_base_te)
    print(f"[gradsense:{encoding}] mean-only KL {kl0:.4f}; deg-1 exact: "
          f"top-1 {deg1_top1:.4f} KL {deg1_kl:.4f}", flush=True)

    run = _wandb_run(f"gradsense-{encoding}-N{n_train}-K{target_chars}",
                     {"encoding": encoding, "n_train": n_train, "win": win,
                      "target_chars": target_chars, "chunk": chunk, "lam": lam,
                      "batch": batch, "kappa": kappa, "sanity": sane})
    gstep = [0]

    def wb(payload):
        if run is not None:
            run.log(payload, step=gstep[0])

    milestones = [m for m in GS_MILESTONES if m <= target_chars]
    if target_chars not in milestones:
        milestones.append(target_chars)
    crossed = {m for m in milestones if m <= len(idx_all)}
    profile = (0, chunk // 2, chunk // 2)                          # deg 2/3 only: deg-1 is exact
    prev_mass = float((G_res ** 2).sum(1).mean())
    rnd, dry = 0, 0
    while len(idx_all) < target_chars and rnd < rounds_cap and dry < 3:
        t0 = time.time()

        def log_fn(t, payload):
            gstep[0] += 16
            wb(payload)
        res = learn_sense_chunk(bits_tr_t, G_res, w_tr, vm, profile=profile,
                                lam=lam, steps=steps, min_steps=min_steps,
                                patience=patience, check_every=16, lr=lr,
                                batch=batch, device=dev,
                                seed=seed * 10_000 + rnd, log_fn=log_fn)
        idx_f, psi_f = dedupe_indices(res["idx"], res["psi"], seen)
        # floor-relative gate: psi noise on the val subsample concentrates
        # within ~sqrt(2/dY) ~ 4% of the floor (1024-dim averaging), so even
        # kappa = 1.25 x floor is a ~6-sigma acceptance
        gate = kappa * float((G_res ** 2).sum(1).mean()) / n_val
        kept = psi_f > gate
        idx_k, psi_k = idx_f[kept], psi_f[kept]
        if len(idx_k) and float(psi_k.max()) > mass0 + 1e-3:       # Parseval contract
            raise RuntimeError(f"psi {psi_k.max():.4f} > total mass {mass0:.4f} "
                               f"at round {rnd}: residual corrupted")
        top_psi = np.sort(psi_f)[::-1][:8] if len(psi_f) else np.zeros(0)
        floor = float((G_res ** 2).sum(1).mean()) / n_val          # val psi noise floor
        if len(idx_k) == 0:
            dry += 1; rnd += 1
            print(f"[gradsense:{encoding}] round {rnd}: dry ({len(res['idx'])} raw, "
                  f"{len(idx_f)} fresh, 0 kept; top psi "
                  f"{np.array2string(top_psi, precision=6, floatmode='fixed')} vs "
                  f"gate {gate:.2e}, floor {floor:.2e})", flush=True)
            continue
        dry = 0
        order = np.argsort(-psi_k)
        idx_k, psi_k = idx_k[order], psi_k[order]
        # SEQUENTIAL matching-pursuit deflation (batch deflation amplifies
        # on-data duplicate clusters -- the 2026-07-15 divergence)
        C_k, G_res = sequential_deflate(bits_tr_t, G_res, w_tr, idx_k, device=dev)
        Fhat_te = apply_chars(bits_te_t, Fhat_te, idx_k, C_k)
        Fhat_trs = apply_chars(bits_tr_t[trs_t], Fhat_trs, idx_k, C_k)
        idx_all = np.concatenate([idx_all, idx_k])
        psi_all = np.concatenate([psi_all, psi_k])
        C_all = np.concatenate([C_all, C_k])
        res_mass = float((G_res ** 2).sum(1).mean())
        if res_mass > prev_mass * 1.001 + 1e-9:                    # projections never grow
            raise RuntimeError(f"deflation diverged at round {rnd}: "
                               f"{prev_mass:.4f} -> {res_mass:.4f}")
        prev_mass = res_mass
        deg_k = (np.asarray(idx_k) >= 0).sum(1)
        wb({"chars_accepted": len(idx_all), "fresh": len(idx_k),
            "dup_rate": 1.0 - len(idx_f) / max(len(res["idx"]), 1),
            "kept_rate": len(idx_k) / max(len(idx_f), 1),
            "mean_psi_accepted": float(psi_k.mean()),
            "max_psi_accepted": float(psi_k.max()),
            "residual_mass": res_mass, "captured_mass": mass0 - res_mass,
            "early_stop_step": res["stopped_at"],
            "chars_per_min": len(idx_k) / max((time.time() - t0) / 60, 1e-9),
            **{f"deg{d}_accepted": int((deg_k == d).sum()) for d in (2, 3)}})
        print(f"[gradsense:{encoding}] round {rnd}: +{len(idx_k)} "
              f"(total {len(idx_all)}, residual {res_mass:.4f}/{mass0:.4f}, "
              f"stop@{res['stopped_at']})", flush=True)
        for m in milestones:                                       # character-scaling curve
            if m not in crossed and len(idx_all) >= m:
                crossed.add(m)
                te1, tr1 = top1(Fhat_te, t_te, w_te), top1(Fhat_trs, t_trs, np.ones(len(trs)))
                kl = test_kl(Fhat_te)
                wb({"milestone_chars": m, "TEST_top1": te1, "TRAIN_top1": tr1,
                    "TEST_kl": kl, "milestone_residual_mass": res_mass})
                print(f"[gradsense:{encoding}] === {m} chars: TEST top-1 {te1:.4f} "
                      f"KL {kl:.4f} (train top-1 {tr1:.4f}, kl0 {kl0:.4f}, "
                      f"captured {mass0 - res_mass:.4f})", flush=True)
        rnd += 1
        if rnd % 8 == 0:
            save_ckpt()
            vol.commit()
    save_ckpt()

    final_te, final_trs = top1(Fhat_te, t_te, w_te), top1(Fhat_trs, t_trs, np.ones(len(trs)))
    final_kl = test_kl(Fhat_te)
    res_mass = float((G_res ** 2).sum(1).mean())
    ks = [m for m in milestones if m <= len(idx_all)] or [len(idx_all)]
    # sensed chars in psi order ON TOP of the exact deg-1 base
    ladder = reconstruct_ladder(bits_te_t, idx_all, C_all, c0,
                                np.argsort(-psi_all), ks, Wu, t_te, w_te,
                                kl_ref=(H_te_f32, s_bar), base=deg1_base_te)
    # random control: same realized deg-2/3 profile, PLAIN coefficients of the
    # deg-1-FREE residual target, same deg-1 base, same ladder
    rng = np.random.default_rng(7)
    deg_all = (idx_all >= 0).sum(1)
    r_rows, r_seen = [], set()
    for d in (2, 3):
        need = int((deg_all == d).sum())
        while need > 0:
            row = np.full(4, -1, np.int16)
            row[:d] = np.sort(rng.choice(nb, d, replace=False)).astype(np.int16)
            key = row[:d].tobytes()
            if key in r_seen:
                continue
            r_seen.add(key); r_rows.append(row); need -= 1
    ctl_ladder = []
    if r_rows:
        r_idx = np.stack(r_rows)
        G0 = torch.tensor(normalize(dtr["H"]) - c0[None, :], dtype=torch.float32,
                          device=dev)
        G0 -= deg1_pred(bits_tr_t)
        C_r = fourier_coefficients(bits_tr_t, G0, w_tr, r_idx, device=dev)
        del G0
        ctl_ladder = reconstruct_ladder(bits_te_t, r_idx, C_r, c0,
                                        np.arange(len(r_idx)), ks, Wu, t_te, w_te,
                                        kl_ref=(H_te_f32, s_bar), base=deg1_base_te)
    mlp_top1 = None
    if mlp:
        fit = fit_mlp_hidden(bits_tr, normalize(dtr["H"]) - c0[None, :], w_tr, vm,
                             bits_te, hidden=2048, steps=3000, batch=8192, seed=seed)
        mlp_top1 = weighted_agreement(
            unembed_top1(fit["pred_te"] + c0[None, :], Wu), t_te, w_te)
    summary = {"encoding": encoding, "n_train": n_train, "win": win,
               "target_chars": target_chars, "chars": int(len(idx_all)),
               "sanity_top1": sane, "TEST_top1": final_te, "TRAIN_sub_top1": final_trs,
               "TEST_kl": final_kl, "kl_mean_only": kl0, "s_bar": s_bar,
               "deg1_top1": deg1_top1, "deg1_kl": deg1_kl,
               "residual_mass": res_mass, "captured_mass": mass0 - res_mass,
               "total_mass": mass0, "ladder": ladder, "control_ladder": ctl_ladder,
               "mlp_top1": mlp_top1,
               "deg_hist": {str(d): int((deg_all == d).sum()) for d in (2, 3)}}
    if run is not None:
        run.summary.update({k: v for k, v in summary.items()
                            if not isinstance(v, (list, dict))})
        run.finish()
    _write_json(f"{ROOT}/summary_gradsense_{encoding}_K{target_chars}.json", summary)
    _record("grad-sense", started, {"encoding": encoding, "chars": int(len(idx_all))})
    print(json.dumps(summary), flush=True)
    return summary


@app.function(image=image, gpu="H100", volumes={"/cache": vol}, timeout=21600,
              memory=65536, secrets=WANDB_SECRET + HF_SECRET)
def deg2_exact(n_train: int = 1_000_000, n_test: int = 10_000, win: int = 61,
               r: int = 64, encoding: str = "lsh"):
    """Gradient-free enumeration of EVERY degree-2 character's Fourier weight
    on the deg-1-free residual of the normalized pre-head target -- the
    decisive check on 'the deg-2 spectrum is diffuse'.  One n x n GEMM per
    projected target dim covers all n(n-1)/2 pairs with per-pair noise floor
    mass_r/D (~7e-7 at 1M contexts, ~22x below the harvest's val gate).
    Self-sufficient: fits deg-1 exactly inline (also reporting its held-out
    top-1 for this window), so any --win runs standalone."""
    import time
    import torch
    vol.reload()
    _budget("deg2-exact", 5.0)
    started = time.time()
    te_p = make_plain_data.local(n_test, CTX, 0, "plain_test")
    tr_p = make_plain_data.local(n_train, CTX, 3 * n_test, "plain_tr")
    Wu = np.load(f"{ROOT}/lm_head.npz")["Wu"].astype(np.float32)
    codes = dict(np.load(f"{ROOT}/codes.npz"))[encoding]
    B = codes.shape[1]; nb = win * B
    dtr, dte = np.load(tr_p), np.load(te_p)
    bits_tr = context_bits(dtr["tokens"].astype(np.int64), codes)[:, :nb]
    bits_te = context_bits(dte["tokens"].astype(np.int64), codes)[:, :nb]
    t_te = dte["tstar"].astype(np.int64)
    w_tr, w_te = np.ones(len(bits_tr)), np.ones(len(bits_te))
    F = dtr["H"].astype(np.float32)
    F /= (np.linalg.norm(F, axis=1, keepdims=True) + 1e-8)
    c0 = F.mean(0)
    dev = "cuda"
    bits_t = torch.tensor(bits_tr, device=dev)
    G_t = torch.tensor(F - c0[None, :], device=dev)
    del F
    mass0 = float((G_t ** 2).sum(1).mean())
    fit1 = fit_deg1_exact(bits_t, G_t, w_tr, device=dev)
    W1 = torch.as_tensor(fit1["W"], device=dev)
    b1 = torch.as_tensor(fit1["b"], device=dev)
    for lo in range(0, len(bits_t), 131072):
        G_t[lo:lo + 131072] -= \
            (1.0 - 2.0 * bits_t[lo:lo + 131072].float()) @ W1 + b1
    res_mass = float((G_t ** 2).sum(1).mean())
    bte_t = torch.tensor(bits_te, device=dev)
    deg1_te = (1.0 - 2.0 * bte_t.float()) @ W1 + b1
    deg1_top1 = weighted_agreement(
        unembed_top1(deg1_te.cpu().numpy() + c0[None, :], Wu), t_te, w_te)
    print(f"[deg2exact:{encoding}:w{win}] deg-1 top-1 {deg1_top1:.4f}; "
          f"residual {res_mass:.4f}/{mass0:.4f}; n={nb} bits", flush=True)
    t0 = time.time()
    psi2, mass_r = deg2_exact_psi(bits_t, G_t, w_tr, r=r, device=dev)
    D = len(bits_t)
    floor = mass_r / D
    iu = np.triu_indices(nb, k=1)
    vals = psi2[iu].astype(np.float64)
    npairs = len(vals)
    counts = {str(k): int((vals > k * floor).sum())
              for k in (2, 4, 6, 10, 30, 100, 300, 1000)}
    debiased = float(vals.sum() - npairs * floor)
    top = np.argsort(-vals)[:1_000_000]
    top50 = [{"a": int(iu[0][t]), "b": int(iu[1][t]),
              "back_a": int(iu[0][t] // B), "back_b": int(iu[1][t] // B),
              "psi": float(vals[t]), "x_floor": float(vals[t] / floor)}
             for t in top[:50]]
    summary = {"encoding": encoding, "win": win, "r": r, "n_train": n_train,
               "n_bits": int(nb), "n_pairs": int(npairs),
               "deg1_top1": deg1_top1, "total_mass": mass0,
               "residual_mass": res_mass, "mass_r": mass_r,
               "pair_noise_floor": floor,
               "counts_above_floor_multiple": counts,
               "deg2_total_mass_debiased": debiased,
               "psi_max": float(vals[top[0]]), "top50": top50,
               "enum_seconds": time.time() - t0}
    np.savez_compressed(f"{ROOT}/deg2exact_{encoding}_win{win}_r{r}.npz",
                        top_a=iu[0][top].astype(np.int16),
                        top_b=iu[1][top].astype(np.int16),
                        top_psi=vals[top].astype(np.float32),
                        floor=floor, mass_r=mass_r, res_mass=res_mass,
                        W1=fit1["W"].astype(np.float16), b1=fit1["b"])
    vol.commit()
    print(f"[deg2exact:{encoding}:w{win}] floor {floor:.3e}; max psi "
          f"{vals[top[0]]:.3e} ({vals[top[0]] / floor:.1f}x floor); counts "
          f"{counts}; debiased deg-2 mass {debiased:.4f} "
          f"(residual {res_mass:.4f})", flush=True)
    run = _wandb_run(f"deg2exact-{encoding}-w{win}-N{n_train}",
                     {k: v for k, v in summary.items()
                      if not isinstance(v, (list, dict))})
    if run is not None:
        run.summary.update({k: v for k, v in summary.items()
                            if not isinstance(v, (list, dict))})
        run.finish()
    _write_json(f"{ROOT}/summary_deg2exact_{encoding}_win{win}.json", summary)
    _record("deg2-exact", started, {"win": win, "encoding": encoding})
    print(json.dumps(summary), flush=True)
    return summary


@app.function(image=image, gpu="H100", volumes={"/cache": vol}, timeout=43200,
              memory=65536, secrets=WANDB_SECRET + HF_SECRET)
def deg2_fit(n_train: int = 1_000_000, n_test: int = 10_000, win: int = 32,
             r: int = 64, max_pairs: int = 400_000, encoding: str = "lsh"):
    """Deg-1 exact + the EXACT top deg-2 pairs (from deg2_exact's enumeration),
    plain Fourier coefficients on the deg-1-free residual, cumulative TEST
    top-1 + KL ladder over the pair count -- the character-scaling curve with
    exactly-enumerated characters (no gradient search anywhere)."""
    import time
    import torch
    vol.reload()
    _budget("deg2-fit", 8.0)
    started = time.time()
    te_p = make_plain_data.local(n_test, CTX, 0, "plain_test")
    tr_p = make_plain_data.local(n_train, CTX, 3 * n_test, "plain_tr")
    Wu = np.load(f"{ROOT}/lm_head.npz")["Wu"].astype(np.float32)
    codes = dict(np.load(f"{ROOT}/codes.npz"))[encoding]
    B = codes.shape[1]; nb = win * B
    z2 = np.load(f"{ROOT}/deg2exact_{encoding}_win{win}_r{r}.npz")
    floor = float(z2["floor"])
    keep = z2["top_psi"] > 2.0 * floor
    K = int(min(keep.sum(), max_pairs))
    idx = np.full((K, 4), -1, np.int16)
    idx[:, 0] = z2["top_a"][:K]; idx[:, 1] = z2["top_b"][:K]
    psi = z2["top_psi"][:K].astype(np.float64)
    dtr, dte = np.load(tr_p), np.load(te_p)
    bits_tr = context_bits(dtr["tokens"].astype(np.int64), codes)[:, :nb]
    bits_te = context_bits(dte["tokens"].astype(np.int64), codes)[:, :nb]
    t_te = dte["tstar"].astype(np.int64)
    w_tr, w_te = np.ones(len(bits_tr)), np.ones(len(bits_te))
    F = dtr["H"].astype(np.float32)
    F /= (np.linalg.norm(F, axis=1, keepdims=True) + 1e-8)
    c0 = F.mean(0)
    dev = "cuda"
    bits_t = torch.tensor(bits_tr, device=dev)
    G_t = torch.tensor(F - c0[None, :], device=dev)
    del F
    W1 = torch.as_tensor(z2["W1"].astype(np.float32), device=dev)
    b1 = torch.as_tensor(z2["b1"].astype(np.float32), device=dev)
    for lo in range(0, len(bits_t), 131072):
        G_t[lo:lo + 131072] -= \
            (1.0 - 2.0 * bits_t[lo:lo + 131072].float()) @ W1 + b1
    bte_t = torch.tensor(bits_te, device=dev)
    deg1_te = (1.0 - 2.0 * bte_t.float()) @ W1 + b1
    s_bar = float(np.linalg.norm(dtr["H"][:100_000].astype(np.float32), axis=1).mean())
    H_te_f32 = dte["H"].astype(np.float32)
    deg1_top1 = weighted_agreement(
        unembed_top1(deg1_te.cpu().numpy() + c0[None, :], Wu), t_te, w_te)
    deg1_kl = _full_kl((deg1_te.cpu().numpy() + c0[None, :]) * s_bar, H_te_f32,
                       Wu, w_te, device=dev)
    print(f"[deg2fit:{encoding}:w{win}] deg-1 top-1 {deg1_top1:.4f} KL "
          f"{deg1_kl:.4f}; fitting {K} exact pairs (psi {psi[0]:.2e}.."
          f"{psi[K - 1]:.2e})", flush=True)
    t0 = time.time()
    res1 = float((G_t ** 2).sum(1).mean())
    # SEQUENTIAL matching pursuit, psi order: same-block pairs are strongly
    # correlated on-data (top-50 all share the newest token's block) and
    # independent plain coefficients double-count the shared component once
    # per pair -- the first plain-sum ladder DEGRADED 0.148 -> 0.017/KL 123
    C, G_t = sequential_deflate(bits_t, G_t, w_tr, idx, device=dev, block=512)
    res2 = float((G_t ** 2).sum(1).mean())
    print(f"[deg2fit:{encoding}:w{win}] MP coefficients in "
          f"{time.time() - t0:.0f}s; deg-2 captured mass "
          f"{res1 - res2:.4f} (residual {res1:.4f} -> {res2:.4f})", flush=True)
    ks = sorted({k for k in (5, 100, 1_500, 6_000, 18_000, 48_000, 160_000,
                             400_000) if k <= K} | {K})
    cum_cap = np.cumsum((C.astype(np.float64) ** 2).sum(1))       # exact per-char
    ladder = reconstruct_ladder(bte_t, idx, C, c0, np.arange(K), ks, Wu,
                                t_te, w_te, kl_ref=(H_te_f32, s_bar),
                                base=deg1_te)
    for entry in ladder:
        entry["cum_captured"] = float(cum_cap[entry["k"] - 1])
        print(f"[deg2fit:{encoding}:w{win}] === deg-1 + {entry['k']} pairs: "
              f"TEST top-1 {entry['top1']:.4f} KL {entry['kl']:.4f} "
              f"(captured {entry['cum_captured']:.4f})", flush=True)
    summary = {"encoding": encoding, "win": win, "n_train": n_train,
               "deg1_top1": deg1_top1, "deg1_kl": deg1_kl,
               "K": K, "pair_floor": floor, "ladder": ladder}
    run = _wandb_run(f"deg2fit-{encoding}-w{win}-N{n_train}",
                     {"encoding": encoding, "win": win, "K": K,
                      "deg1_top1": deg1_top1})
    if run is not None:
        for entry in ladder:
            run.log({"pairs": entry["k"], "TEST_top1": entry["top1"],
                     "TEST_kl": entry["kl"], "cum_captured": entry["cum_captured"]})
        run.summary.update({"deg1_top1": deg1_top1,
                            "best_top1": max(e["top1"] for e in ladder)})
        run.finish()
    _write_json(f"{ROOT}/summary_deg2fit_{encoding}_win{win}.json", summary)
    _record("deg2-fit", started, {"win": win, "K": K})
    print(json.dumps(summary), flush=True)
    return summary


@app.function(image=image, gpu="H100", volumes={"/cache": vol}, timeout=43200,
              memory=65536, secrets=WANDB_SECRET + HF_SECRET)
def deg3_fit(n_train: int = 1_000_000, n_test: int = 10_000, win: int = 32,
             r2: int = 64, n_pairs: int = 160_000, blocks: int = 3, r3: int = 16,
             kappa: float = 6.0, per_anchor: int = 3000, max_tris: int = 200_000,
             encoding: str = "lsh", far: int = 0, deg4_anchors: int = 0):
    """Degree 3, EXACTLY, on top of the deg-1+2 model: a triple's coefficient
    is the PAIR coefficient of the sign-flipped target (psi3(a,b,c) =
    deg2_exact_psi(bits, G*chi_c)[a,b]), so the tested pair enumerator runs
    once per anchor bit.  Anchors/pairs restricted to the newest ``blocks``
    token blocks (deg-2 said ~all significant mass lives there: same-block
    quadratics + adjacent-token bigrams).  Enumerated triples above a
    kappa x floor gate are fit by block-OMP on the deg-1+2-deflated residual;
    the TEST ladder extends the deg-2 curve."""
    import time
    import torch
    vol.reload()
    _budget("deg3-fit", 10.0)
    started = time.time()
    te_p = make_plain_data.local(n_test, CTX, 0, "plain_test")
    tr_p = make_plain_data.local(n_train, CTX, 3 * n_test, "plain_tr")
    Wu = np.load(f"{ROOT}/lm_head.npz")["Wu"].astype(np.float32)
    codes = dict(np.load(f"{ROOT}/codes.npz"))[encoding]
    B = codes.shape[1]; nb = win * B
    z2 = np.load(f"{ROOT}/deg2exact_{encoding}_win{win}_r{r2}.npz")
    K2 = int(min((z2["top_psi"] > 2.0 * float(z2["floor"])).sum(), n_pairs))
    idx2 = np.full((K2, 4), -1, np.int16)
    idx2[:, 0] = z2["top_a"][:K2]; idx2[:, 1] = z2["top_b"][:K2]
    dtr, dte = np.load(tr_p), np.load(te_p)
    bits_tr = context_bits(dtr["tokens"].astype(np.int64), codes)[:, :nb]
    bits_te = context_bits(dte["tokens"].astype(np.int64), codes)[:, :nb]
    t_te = dte["tstar"].astype(np.int64)
    w_tr, w_te = np.ones(len(bits_tr)), np.ones(len(bits_te))
    F = dtr["H"].astype(np.float32)
    F /= (np.linalg.norm(F, axis=1, keepdims=True) + 1e-8)
    c0 = F.mean(0)
    dev = "cuda"
    bits_t = torch.tensor(bits_tr, device=dev)
    G_t = torch.tensor(F - c0[None, :], device=dev)
    del F
    W1 = torch.as_tensor(z2["W1"].astype(np.float32), device=dev)
    b1 = torch.as_tensor(z2["b1"].astype(np.float32), device=dev)
    for lo in range(0, len(bits_t), 131072):
        G_t[lo:lo + 131072] -= \
            (1.0 - 2.0 * bits_t[lo:lo + 131072].float()) @ W1 + b1
    C2, G_t = sequential_deflate(bits_t, G_t, w_tr, idx2, device=dev, block=512)
    res12 = float((G_t ** 2).sum(1).mean())
    bte_t = torch.tensor(bits_te, device=dev)
    base_te = (1.0 - 2.0 * bte_t.float()) @ W1 + b1
    Ct2 = torch.as_tensor(C2, device=dev)
    for klo in range(0, K2, 8192):
        base_te += xor_parity_features(bte_t, idx2[klo:klo + 8192]) @ \
            Ct2[klo:klo + 8192]
    s_bar = float(np.linalg.norm(dtr["H"][:100_000].astype(np.float32), axis=1).mean())
    H_te_f32 = dte["H"].astype(np.float32)
    base_top1 = weighted_agreement(
        unembed_top1(base_te.cpu().numpy() + c0[None, :], Wu), t_te, w_te)
    base_kl = _full_kl((base_te.cpu().numpy() + c0[None, :]) * s_bar, H_te_f32,
                       Wu, w_te, device=dev)
    print(f"[deg3fit:{encoding}:w{win}] deg-1+{K2} pairs base: top-1 "
          f"{base_top1:.4f} KL {base_kl:.4f} (residual {res12:.4f})", flush=True)
    # ---- anchored triple enumeration over the newest `blocks` token blocks.
    # ONE global top-r3 projection of the residual is shared by every anchor
    # (the rowwise sign flip commutes with projection: (G*xc)U = (GU)*xc) --
    # a per-anchor PCA would cost ~1e15 MACs x 798 anchors
    t0 = time.time()
    Ccov = torch.zeros((G_t.shape[1],) * 2, dtype=torch.float64, device=dev)
    for lo in range(0, len(G_t), 262144):
        Gc = G_t[lo:lo + 262144].float()
        Ccov += (Gc.t() @ Gc).double()
    _, evecs = torch.linalg.eigh(Ccov)
    U3 = evecs[:, -r3:].to(torch.float32)
    G_p = torch.empty((len(G_t), r3), device=dev)
    for lo in range(0, len(G_t), 262144):
        G_p[lo:lo + 262144] = G_t[lo:lo + 262144].float() @ U3
    eye3 = torch.eye(r3, device=dev)
    combos = [(i, i) for i in range(blocks)] + \
             [(i, j) for i in range(blocks) for j in range(blocks) if i != j]
    if far:
        # long-range copy-gating shape: newest-block PAIR x a DISTANT block's
        # bit ("copy from -k now" = products spanning recent and far blocks)
        combos += [(0, j) for j in range(blocks, min(far, win))]
    found = {}
    for (pb, ab) in combos:                                        # pair block, anchor block
        sub = np.arange(pb * B, (pb + 1) * B)
        bits_sub = bits_t[:, torch.as_tensor(sub, device=dev)].contiguous()
        for cl in range(B):
            c = ab * B + cl
            xc = (1.0 - 2.0 * bits_t[:, c].float())[:, None]
            psi3, mr = deg2_exact_psi(bits_sub, G_p * xc, w_tr, proj=eye3,
                                      device=dev)
            gate = kappa * mr / len(bits_t)
            iu = np.triu_indices(len(sub), k=1)
            vals = psi3[iu]
            good = np.flatnonzero(vals > gate)
            if len(good) > per_anchor:
                good = good[np.argsort(-vals[good])[:per_anchor]]
            for gidx in good:
                tri = tuple(sorted((int(sub[iu[0][gidx]]), int(sub[iu[1][gidx]]),
                                    int(c))))
                if len(set(tri)) < 3:
                    continue
                if vals[gidx] > found.get(tri, 0.0):
                    found[tri] = float(vals[gidx])
    # STAGED ordering: LOCAL triples (all bits within the `blocks` newest
    # blocks) first by psi, then far-anchored ones -- a global psi sort mixed
    # 5M redundant far triples into the top-900k and DISPLACED better locals
    # (0.2466 vs local-only 0.2633 at matched count, 2026-07-15)
    loc = [(t, p) for t, p in found.items() if max(b // B for b in t) < blocks]
    farr = [(t, p) for t, p in found.items() if max(b // B for b in t) >= blocks]
    tris = (sorted(loc, key=lambda kv: -kv[1]) +
            sorted(farr, key=lambda kv: -kv[1]))[:max_tris]
    print(f"[deg3fit:{encoding}:w{win}] {len(found)} distinct triples above "
          f"{kappa}x floor ({len(loc)} local, {len(farr)} far; "
          f"{time.time() - t0:.0f}s); fitting top {len(tris)} staged "
          f"local-first", flush=True)
    summary = {"encoding": encoding, "win": win, "n_pairs": K2,
               "base_top1": base_top1, "base_kl": base_kl,
               "n_triples_found": len(found), "blocks": blocks, "ladder": []}
    if tris:
        idx3 = np.full((len(tris), 4), -1, np.int16)
        for j, (tri, _) in enumerate(tris):
            idx3[j, :3] = tri
        C3, G_t = sequential_deflate(bits_t, G_t, w_tr, idx3, device=dev,
                                     block=512)
        res123 = float((G_t ** 2).sum(1).mean())
        ks = sorted({k for k in (1_000, 5_000, 20_000, 50_000, 100_000,
                                 200_000, 500_000, 1_000_000, 2_000_000)
                     if k <= len(tris)} | {len(tris)})
        ladder = reconstruct_ladder(bte_t, idx3, C3, c0, np.arange(len(tris)),
                                    ks, Wu, t_te, w_te,
                                    kl_ref=(H_te_f32, s_bar), base=base_te)
        cum_cap = np.cumsum((C3.astype(np.float64) ** 2).sum(1))
        for entry in ladder:
            entry["cum_captured"] = float(cum_cap[entry["k"] - 1])
            print(f"[deg3fit:{encoding}:w{win}] === deg-1+2 + {entry['k']} "
                  f"triples: TEST top-1 {entry['top1']:.4f} KL "
                  f"{entry['kl']:.4f} (captured {entry['cum_captured']:.4f})",
                  flush=True)
        summary["ladder"] = ladder
        summary["deg3_captured"] = res12 - res123
        summary["sparsity_pairs"] = _sparsity_stats((C2 ** 2).sum(1))
        summary["sparsity_triples"] = _sparsity_stats((C3 ** 2).sum(1))
        print(f"[deg3fit:{encoding}:w{win}] SPARSITY pairs "
              f"{json.dumps(summary['sparsity_pairs'])} triples "
              f"{json.dumps(summary['sparsity_triples'])}", flush=True)
        np.savez(f"{ROOT}/deg3fit_coefs_{encoding}_win{win}.npz",
                 idx2=idx2, C2=C2.astype(np.float16),
                 idx3=idx3, C3=C3.astype(np.float16))
        vol.commit()
        Ct3 = torch.as_tensor(C3, device=dev)
        for klo in range(0, len(idx3), 8192):
            base_te += xor_parity_features(bte_t, idx3[klo:klo + 8192]) @ \
                Ct3[klo:klo + 8192]
        # ---- optional degree 4: anchor on the top FITTED pair characters
        # (flip the target by chi_a*chi_b -- the pair map then enumerates
        # quadruples (c,d,a,b); pairs-of-bigrams / gated token quadratics)
        if deg4_anchors and len(tris):
            t0 = time.time()
            sub4 = np.arange(0, 2 * B)                            # (c,d) in newest 2 blocks
            bits_sub4 = bits_t[:, :2 * B].contiguous()
            found4 = {}
            for j in range(min(deg4_anchors, K2)):
                a, b = int(idx2[j, 0]), int(idx2[j, 1])
                xab = (1.0 - 2.0 * bits_t[:, a].float()) * \
                      (1.0 - 2.0 * bits_t[:, b].float())
                psi4, mr = deg2_exact_psi(bits_sub4, G_p * xab[:, None], w_tr,
                                          proj=eye3, device=dev)
                gate4 = kappa * mr / len(bits_t)
                iu4 = np.triu_indices(len(sub4), k=1)
                vals4 = psi4[iu4]
                good4 = np.flatnonzero(vals4 > gate4)
                if len(good4) > per_anchor:
                    good4 = good4[np.argsort(-vals4[good4])[:per_anchor]]
                for gidx in good4:
                    quad = tuple(sorted((int(iu4[0][gidx]), int(iu4[1][gidx]),
                                         a, b)))
                    if len(set(quad)) < 4:
                        continue
                    if vals4[gidx] > found4.get(quad, 0.0):
                        found4[quad] = float(vals4[gidx])
            quads = sorted(found4.items(), key=lambda kv: -kv[1])[:max_tris]
            print(f"[deg3fit:{encoding}:w{win}] deg-4: {len(found4)} distinct "
                  f"quads above {kappa}x floor ({time.time() - t0:.0f}s); "
                  f"fitting top {len(quads)}", flush=True)
            summary["n_quads_found"] = len(found4)
            if quads:
                idx4 = np.stack([np.array(q, np.int16) for q, _ in quads])
                C4, G_t = sequential_deflate(bits_t, G_t, w_tr, idx4,
                                             device=dev, block=512)
                ks4 = sorted({k for k in (5_000, 20_000, 50_000, 100_000,
                                          200_000) if k <= len(quads)}
                             | {len(quads)})
                ladder4 = reconstruct_ladder(bte_t, idx4, C4, c0,
                                             np.arange(len(quads)), ks4, Wu,
                                             t_te, w_te,
                                             kl_ref=(H_te_f32, s_bar),
                                             base=base_te)
                for entry in ladder4:
                    print(f"[deg3fit:{encoding}:w{win}] === deg-1+2+3 + "
                          f"{entry['k']} quads: TEST top-1 {entry['top1']:.4f} "
                          f"KL {entry['kl']:.4f}", flush=True)
                summary["ladder4"] = ladder4
                Ct4 = torch.as_tensor(C4, device=dev)
                for klo in range(0, len(idx4), 8192):
                    base_te += xor_parity_features(bte_t, idx4[klo:klo + 8192]) \
                        @ Ct4[klo:klo + 8192]
        # ---- failure-mode diagnostics on the FULL final model --------------
        pred = unembed_top1(base_te.cpu().numpy() + c0[None, :], Wu)
        correct = (pred == t_te)
        # (a) copy/induction: teacher's token already appears in the window
        ctx_tok = dte["tokens"].astype(np.int64)[:, -win:]
        is_copy = (ctx_tok == t_te[:, None]).any(1)
        # (b) teacher confidence buckets (softmax max from stored hidden)
        import torch as _t
        conf = np.empty(len(t_te), np.float32)
        Wt = _t.as_tensor(Wu, device=dev)
        for lo in range(0, len(t_te), 1024):
            lg = _t.as_tensor(H_te_f32[lo:lo + 1024], device=dev) @ Wt.t()
            conf[lo:lo + 1024] = _t.softmax(lg, 1).max(1).values.cpu().numpy()
        # (c) teacher-token train frequency buckets
        cnts = np.bincount(dtr["tstar"].astype(np.int64), minlength=Wu.shape[0])
        freq = cnts[t_te]
        # (d) student rank of the teacher token
        rank_hits = {5: 0, 20: 0}
        Hp = _t.as_tensor(base_te.cpu().numpy() + c0[None, :], device=dev)
        for lo in range(0, len(t_te), 1024):
            lg = Hp[lo:lo + 1024] @ Wt.t()
            for kk in rank_hits:
                topk = lg.topk(kk, dim=1).indices.cpu().numpy()
                rank_hits[kk] += int((topk == t_te[lo:lo + 1024, None]).any(1).sum())
        diag = {"copy_share": float(is_copy.mean()),
                "acc_copy": float(correct[is_copy].mean()),
                "acc_nocopy": float(correct[~is_copy].mean()),
                "top5": rank_hits[5] / len(t_te), "top20": rank_hits[20] / len(t_te)}
        rare = freq < 100                                          # rare x copy cross-tab
        if (rare & is_copy).sum() > 20:
            diag["rare_copy_share_of_rare"] = float(is_copy[rare].mean())
            diag["acc_rare_copy"] = float(correct[rare & is_copy].mean())
            diag["acc_rare_nocopy"] = float(correct[rare & ~is_copy].mean())
        for name_, edges in (("conf", [0.0, 0.25, 0.5, 0.75, 1.01]),):
            for j in range(len(edges) - 1):
                m = (conf >= edges[j]) & (conf < edges[j + 1])
                if m.sum() > 20:
                    diag[f"acc_conf_{edges[j]:.2f}"] = float(correct[m].mean())
                    diag[f"share_conf_{edges[j]:.2f}"] = float(m.mean())
        for lo_f, hi_f, tag in ((0, 100, "rare"), (100, 10_000, "mid"),
                                (10_000, 10 ** 12, "common")):
            m = (freq >= lo_f) & (freq < hi_f)
            if m.sum() > 20:
                diag[f"acc_freq_{tag}"] = float(correct[m].mean())
                diag[f"share_freq_{tag}"] = float(m.mean())
        summary["diag"] = diag
        print(f"[deg3fit:{encoding}:w{win}] DIAG {json.dumps(diag)}", flush=True)
    run = _wandb_run(f"deg3fit-{encoding}-w{win}-N{n_train}",
                     {"encoding": encoding, "win": win, "n_pairs": K2,
                      "base_top1": base_top1})
    if run is not None:
        for entry in summary["ladder"]:
            run.log({"triples": entry["k"], "TEST_top1": entry["top1"],
                     "TEST_kl": entry["kl"]})
        run.summary.update({"base_top1": base_top1,
                            "n_triples_found": len(found)})
        run.finish()
    _write_json(f"{ROOT}/summary_deg3fit_{encoding}_win{win}.json", summary)
    _record("deg3-fit", started, {"win": win, "triples": len(found)})
    print(json.dumps(summary), flush=True)
    return summary


@app.function(image=image, gpu="H100", volumes={"/cache": vol}, timeout=43200,
              memory=65536, secrets=WANDB_SECRET + HF_SECRET)
def fourier_learn(n_train: int = 1_000_000, n_test: int = 10_000, win: int = 32,
                  r2: int = 64, K: int = 8192, rho: float = 0.5, lam: float = 0.1,
                  l1: float = 0.0, gamma: float = 0.1, steps: int = 1500,
                  lr: float = 0.05, batch: int = 8192, warm_pairs: int = 2048,
                  warm_tris: int = 2048, encoding: str = "lsh",
                  deflate_pairs: int = 160_000, deflate_tris: int = 1_000_000):
    """Track B: directly LEARN the Fourier decomposition on the deg-1-free
    residual -- K inclusion-gate characters of ANY degree (log-space product,
    STE to the closest character), coefficients learned within [-rho, rho],
    decorrelation anti-collapse + leave-one-out -log psi sensing.  Warm-starts
    half the slots from the EXACT enumeration's top pairs/triples; the win
    condition is hardened characters BEYOND enumeration reach (degree > 4 or
    long-range) that survive the calculated (block-OMP) refit."""
    import os, time
    import torch
    vol.reload()
    _budget("fourier-learn", 10.0)
    started = time.time()
    te_p = make_plain_data.local(n_test, CTX, 0, "plain_test")
    tr_p = make_plain_data.local(n_train, CTX, 3 * n_test, "plain_tr")
    Wu = np.load(f"{ROOT}/lm_head.npz")["Wu"].astype(np.float32)
    codes = dict(np.load(f"{ROOT}/codes.npz"))[encoding]
    B = codes.shape[1]; nb = win * B
    z2 = np.load(f"{ROOT}/deg2exact_{encoding}_win{win}_r{r2}.npz")
    dtr, dte = np.load(tr_p), np.load(te_p)
    bits_tr = context_bits(dtr["tokens"].astype(np.int64), codes)[:, :nb]
    bits_te = context_bits(dte["tokens"].astype(np.int64), codes)[:, :nb]
    t_te = dte["tstar"].astype(np.int64)
    w_tr, w_te = np.ones(len(bits_tr)), np.ones(len(bits_te))
    F = dtr["H"].astype(np.float32)
    F /= (np.linalg.norm(F, axis=1, keepdims=True) + 1e-8)
    c0 = F.mean(0)
    dev = "cuda"
    bits_t = torch.tensor(bits_tr, device=dev)
    G_t = torch.tensor(F - c0[None, :], device=dev)
    del F
    W1 = torch.as_tensor(z2["W1"].astype(np.float32), device=dev)
    b1 = torch.as_tensor(z2["b1"].astype(np.float32), device=dev)
    for lo in range(0, len(bits_t), 131072):
        G_t[lo:lo + 131072] -= \
            (1.0 - 2.0 * bits_t[lo:lo + 131072].float()) @ W1 + b1
    # deflate the FULL fitted stack (pairs + optimal triples): flearn explores
    # the DUST -- training on a pair-mass-rich residual makes every unfitted
    # heavy char one toggle away the attractor and slots collapse down-degree
    # (v2/v3 post-mortems)
    kp = int(min(deflate_pairs, (z2["top_psi"] > 2.0 * float(z2["floor"])).sum()))
    idx2d = np.full((kp, 4), -1, np.int16)
    idx2d[:, 0] = z2["top_a"][:kp]; idx2d[:, 1] = z2["top_b"][:kp]
    _, G_t = sequential_deflate(bits_t, G_t, w_tr, idx2d, device=dev, block=512)
    cpath = f"{ROOT}/deg3fit_coefs_{encoding}_win{win}.npz"
    wc = None
    wm = np.zeros((0, nb), np.uint8)
    if os.path.exists(cpath):
        zc = np.load(cpath)
        i3all = zc["idx3"]
        kt = int(min(deflate_tris, len(i3all)))
        _, G_t = sequential_deflate(bits_t, G_t, w_tr, i3all[:kt], device=dev,
                                    block=512)
        # exploration seeds: the NEXT (unfitted) triple tier + next pair tier
        i3w = i3all[kt: kt + warm_tris]
        m3 = np.zeros((len(i3w), nb), np.uint8)
        for j, row in enumerate(i3w):
            m3[j, row[row >= 0].astype(int)] = 1
        wm = np.concatenate([wm, m3])
    wp = min(warm_pairs, max(len(z2["top_a"]) - kp, 0))
    m2 = np.zeros((wp, nb), np.uint8)
    m2[np.arange(wp), z2["top_a"][kp:kp + wp].astype(int)] = 1
    m2[np.arange(wp), z2["top_b"][kp:kp + wp].astype(int)] = 1
    wm = np.concatenate([wm, m2])
    res1 = float((G_t ** 2).sum(1).mean())
    vm = np.random.default_rng(0).random(len(bits_tr)) < 0.05
    run = _wandb_run(f"fourierlearn-{encoding}-w{win}-K{K}",
                     {"K": K, "rho": rho, "lam": lam, "gamma": gamma,
                      "steps": steps, "warm": int(len(wm))})

    def log_fn(t, payload):
        if run is not None:
            run.log(payload, step=t)
    print(f"[flearn:{encoding}:w{win}] K={K} warm={len(wm)} residual "
          f"{res1:.4f}", flush=True)
    res = fourier_learn_chars(bits_t, G_t, w_tr, vm, K=K, rho=rho, lam=lam,
                              l1=l1, gamma=gamma, steps=steps, lr=lr,
                              batch=batch, warm_masks=wm, warm_C=wc,
                              device=dev, seed=0, log_fn=log_fn)
    masks = res["masks"]
    deg = masks.sum(1)
    masks = masks[deg > 0]
    masks = np.unique(masks, axis=0)
    deg = masks.sum(1)
    span = np.array([(np.flatnonzero(m).max() // B) for m in masks])
    print(f"[flearn:{encoding}:w{win}] {len(masks)} distinct hardened chars; "
          f"deg hist {np.bincount(np.minimum(deg, 9)).tolist()}; "
          f"deg>4: {(deg > 4).sum()}; span>3 blocks: {(span > 3).sum()}",
          flush=True)
    # calculated coefficients (never ship Adam's): dense block-OMP refit
    C, G_t = sequential_deflate(bits_t, G_t, w_tr, None, device=dev,
                                block=512, masks=masks)
    res2 = float((G_t ** 2).sum(1).mean())
    bte_t = torch.tensor(bits_te, device=dev)
    base_te = (1.0 - 2.0 * bte_t.float()) @ W1 + b1
    Ct = torch.as_tensor(C, device=dev)
    mt = torch.tensor(masks.astype(np.float32), device=dev)
    for klo in range(0, len(masks), 4096):
        Fte = 1.0 - 2.0 * ((bte_t.float() @ mt[klo:klo + 4096].t()) % 2.0)
        base_te += Fte @ Ct[klo:klo + 4096]
    s_bar = float(np.linalg.norm(dtr["H"][:100_000].astype(np.float32), axis=1).mean())
    H_te_f32 = dte["H"].astype(np.float32)
    top1 = weighted_agreement(
        unembed_top1(base_te.cpu().numpy() + c0[None, :], Wu), t_te, w_te)
    kl = _full_kl((base_te.cpu().numpy() + c0[None, :]) * s_bar, H_te_f32,
                  Wu, w_te, device=dev)
    spars = _sparsity_stats((C ** 2).sum(1))
    summary = {"encoding": encoding, "win": win, "K": K, "n_chars": int(len(masks)),
               "warm": int(len(wm)), "TEST_top1": top1, "TEST_kl": kl,
               "captured": res1 - res2, "val_mse": res["val_mse"],
               "deg_hist": {str(d): int((deg == d).sum())
                            for d in range(0, int(deg.max()) + 1)},
               "n_deg_gt4": int((deg > 4).sum()),
               "n_span_gt3": int((span > 3).sum()), "sparsity": spars}
    print(f"[flearn:{encoding}:w{win}] === deg-1 + {len(masks)} LEARNED chars: "
          f"TEST top-1 {top1:.4f} KL {kl:.4f} (captured {res1 - res2:.4f})",
          flush=True)
    np.savez_compressed(f"{ROOT}/flearn_{encoding}_win{win}_K{K}.npz",
                        masks=masks, C=C.astype(np.float16))
    vol.commit()
    if run is not None:
        run.summary.update({k: v for k, v in summary.items()
                            if not isinstance(v, (list, dict))})
        run.finish()
    _write_json(f"{ROOT}/summary_flearn_{encoding}_win{win}_K{K}.json", summary)
    _record("fourier-learn", started, {"win": win, "K": K})
    print(json.dumps(summary), flush=True)
    return summary


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=21600, memory=32768)
def gl_tree_refit(fill_len: int = 61, depth: int = 6, fit_m: int = 8000,
                  val_frac: float = 0.15, per_deg: int = 8000, block: int = 400,
                  encoding: str = "lsh"):
    """Cheap re-fit from SAVED tree masks (no tree, no generation): does adding
    MANY more high-degree characters -- STAGED in small val-gated blocks, not
    one wholesale block -- push below the deg-1+2 ceiling?  This exhausts the
    'maybe top-800 missed the useful ones / maybe they help cumulatively'
    possibility before concluding degree is spanned."""
    import os, time
    import torch
    vol.reload()
    _budget("gl-tree-refit", 1.0)
    started = time.time()
    z = dict(np.load(f"{ROOT}/codes.npz"))
    codes = z[encoding]; B = codes.shape[1]
    mk = np.load(f"{ROOT}/gltree_masks_f{fill_len}_d{depth}_{encoding}.npz")
    masks, deg = mk["masks"], mk["deg"]                            # psi-sorted
    data = np.load(_table_path(fill_len, fit_m, R_FILLS))
    P, fiber_id = data["P"], data["fiber_id"]
    fill_len = data["G"].shape[1]
    contexts = np.concatenate([data["PRE"][fiber_id], data["G"]], axis=1)
    rng = np.random.default_rng(0)
    u = rng.random(fiber_id.max() + 1)[fiber_id]
    te, va = u < 0.15, (u >= 0.15) & (u < 0.30); tr = ~(te | va)
    ctx_tr, A_tr, n_tr, _ = soft_collapse(contexts[tr], P[tr])
    ctx_va, A_va, n_va, _ = soft_collapse(contexts[va], P[va])
    ctx_te, A_te, n_te, _ = soft_collapse(contexts[te], P[te])
    Ptr, Pva, Pte = A_tr / n_tr[:, None], A_va / n_va[:, None], A_te / n_te[:, None]
    mu = A_tr.sum(0) / n_tr.sum(); Mtr = int(tr.sum())
    nb_full, ndep = fill_len * B, depth * B
    bt, bv, bte = (context_bits(ctx_tr, codes), context_bits(ctx_va, codes),
                   context_bits(ctx_te, codes))

    def slot_kl(off, Pn, n):
        lq = off - torch.logsumexp(off, 1, keepdim=True)
        lp = torch.tensor(np.log(np.clip(Pn, 1e-12, None)), device=off.device)
        return float((torch.tensor(n[:, None] * Pn, device=off.device) * (lp - lq)).sum()
                     / float(n.sum()))
    # deg-1+2 enumeration baseline (the 1.103 recipe)
    A_c = torch.tensor((A_tr - n_tr[:, None] * mu[None, :]).astype(np.float32), device="cuda")
    Ff = torch.tensor(1.0 - 2.0 * bt[:, :nb_full].astype(np.float32), device="cuda")
    d1n = ((Ff.T @ A_c) / Mtr).double().pow(2).sum(1).sqrt().cpu().numpy()
    anchors = np.flatnonzero(d1n >= 0.01)
    pairs = {}
    for i in anchors:
        ni = ((Ff * Ff[:, [i]]).T @ A_c / Mtr).double().pow(2).sum(1).sqrt().cpu().numpy()
        ni[i] = 0.0
        for j in np.argsort(-ni)[:max(20, 2000 // max(len(anchors), 1) + 1)]:
            if ni[j] >= 0.01:
                k = (min(int(i), int(j)), max(int(i), int(j)))
                pairs[k] = max(pairs.get(k, 0.0), float(ni[j]))
    top = sorted(pairs.items(), key=lambda kv: -kv[1])[:1000]
    ii = np.array([k[0] for k, _ in top]); jj = np.array([k[1] for k, _ in top])
    del Ff; torch.cuda.empty_cache()

    def bfeat(bits):
        S = 1.0 - 2.0 * bits[:, :nb_full].astype(np.float32)
        return np.concatenate([S[:, anchors], S[:, ii] * S[:, jj]], axis=1)
    fitB = fit_softmax_slots(bfeat(bt), Ptr, n_tr, bfeat(bv), Pva, n_va, steps=3000, lr=0.01)
    Wb = torch.tensor(fitB["W"], device="cuda"); bb = torch.tensor(fitB["b"], device="cuda")
    off_tr = torch.tensor(bfeat(bt), device="cuda") @ Wb + bb
    off_va = torch.tensor(bfeat(bv), device="cuda") @ Wb + bb
    off_te = torch.tensor(bfeat(bte), device="cuda") @ Wb + bb
    base_test = slot_kl(off_te, Pte, n_te)
    base_train = slot_kl(off_tr, Ptr, n_tr)
    print(f"[refit:{encoding}] deg1+2 baseline TRAIN {base_train:.4f} TEST {base_test:.4f}",
          flush=True)
    ladder = [{"stage": "deg1+2", "train_kl": base_train, "test_kl": base_test}]
    kept_hi = 0
    for dd in range(3, depth + 1):
        md = masks[deg == dd][:per_deg]
        bad = 0
        for lo in range(0, len(md), block):
            blk = md[lo:lo + block]
            Ftr = parity_features(bt[:, :ndep], blk); Fva = parity_features(bv[:, :ndep], blk)
            Wd, vk, improved = _fit_block(Ftr, off_tr.cpu().numpy(), Ptr, n_tr,
                                          Fva, off_va.cpu().numpy(), Pva, n_va,
                                          steps=1200, lr=0.01)
            if improved:
                Wt = torch.tensor(Wd, device="cuda")
                off_tr = off_tr + torch.tensor(Ftr, device="cuda") @ Wt
                off_va = off_va + torch.tensor(Fva, device="cuda") @ Wt
                off_te = off_te + torch.tensor(parity_features(bte[:, :ndep], blk),
                                               device="cuda") @ Wt
                kept_hi += len(blk); bad = 0
            else:
                bad += 1
                if bad >= 3:
                    break
        ladder.append({"stage": f"deg<= {dd}", "kept_hi": kept_hi,
                       "train_kl": slot_kl(off_tr, Ptr, n_tr),
                       "test_kl": slot_kl(off_te, Pte, n_te)})
        print(f"[refit:{encoding}] +deg{dd} (kept_hi {kept_hi}) "
              f"TRAIN {ladder[-1]['train_kl']:.4f} TEST {ladder[-1]['test_kl']:.4f}",
              flush=True)
    # RECONSTRUCTION probe (Lev's point): fit baseline + a big block of tree
    # deg>=3 chars on TRAIN with NO val gate.  If TRAIN KL collapses while TEST
    # stays ~1.10, the ceiling is a GENERALIZATION gap (more data -> lower TEST),
    # not an information limit.  If TRAIN also floors, the window genuinely does
    # not determine f (prefix info is missing) and degree/data cannot help.
    hi = masks[deg >= 3][:6000]
    Xtr = np.concatenate([bfeat(bt), parity_features(bt[:, :ndep], hi)], axis=1)
    Xte = np.concatenate([bfeat(bte), parity_features(bte[:, :ndep], hi)], axis=1)
    rec = fit_softmax_slots(Xtr, Ptr, n_tr, Xtr, Ptr, n_tr, steps=6000, lr=0.02)
    Wr = torch.tensor(rec["W"], device="cuda"); br = torch.tensor(rec["b"], device="cuda")
    rec_train = slot_kl(torch.tensor(Xtr, device="cuda") @ Wr + br, Ptr, n_tr)
    rec_test = slot_kl(torch.tensor(Xte, device="cuda") @ Wr + br, Pte, n_te)
    print(f"[refit:{encoding}] RECONSTRUCT (deg1+2 + {len(hi)} deg>=3, no gate) "
          f"TRAIN {rec_train:.4f} TEST {rec_test:.4f}", flush=True)
    out = {"encoding": encoding, "baseline_TEST_kl": base_test,
           "baseline_TRAIN_kl": base_train,
           "final_TEST_kl": ladder[-1]["test_kl"], "kept_high_degree": kept_hi,
           "reconstruct_TRAIN_kl": rec_train, "reconstruct_TEST_kl": rec_test,
           "ladder": ladder}
    _write_json(f"{ROOT}/summary_gltree_refit_{encoding}_f{fill_len}.json", out)
    _record("gl-tree-refit", started)
    print(json.dumps(out), flush=True)
    return out


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=21600, memory=32768)
def oracle(m_fibers: int = 2000, g: int = 24, p_back: int = 0,
           fill_len: int = W_WIN - K_FIXED, val_frac: float = 0.15,
           encoding: str = "all"):
    """The paper-native PAIRED tree with NO collision dependence: run
    ``oracle_deg1_psi`` on forked-cache data for the split token's code bits.
    This natively recovers the newest-token deg-1 spectrum -- exactly where the
    offline flat-file tree is collision-starved -- and is compared against the
    unpaired exact marginal (both must rank LSH > ctrl).  A sparse deg-1 model
    on the certified bits gives an end-to-end held-out KL."""
    import os, time
    import torch
    vol.reload()
    _budget("oracle", 1.0)
    started = time.time()
    path = _oracle_table_path(fill_len, m_fibers, g, p_back)
    if not os.path.exists(path):
        make_oracle_data.local(m_fibers, g, p_back, fill_len)
    d = np.load(path)
    split_tok, P, fiber_gid = d["split_tok"], d["P"], d["fiber_gid"]
    z = dict(np.load(f"{ROOT}/codes.npz"))
    tables = {"lsh": z["lsh"], "ctrl": z["ctrl"], "idbits": z["idbits"]}
    if encoding != "all":
        tables = {encoding: tables[encoding]}
    rng = np.random.default_rng(0)
    va = (rng.random(fiber_gid.max() + 1) < val_frac)[fiber_gid]
    mu = P[~va].astype(np.float64).mean(0)
    F = (P - mu[None, :]).astype(np.float32)                       # centered targets
    summary = {"p_back": int(p_back), "g": int(g), "m_fibers": int(m_fibers),
               "n_pairs_per_fiber": g * (g - 1), "encodings": {}}
    for name, codes in tables.items():
        sb = np.asarray(codes, dtype=np.uint8)[split_tok]         # (M*g, B) split code
        # clean=True removes the b-independent token-collision floor so the
        # ranking and the 0.01 bar are meaningful (see oracle_deg1_psi)
        psi, norm = oracle_deg1_psi(sb[~va], F[~va], fiber_gid[~va], clean=True)
        # unpaired exact marginal |E[F chi_b]|^2 on the same rows (sanity twin)
        Ftr = F[~va]; chitr = 1.0 - 2.0 * sb[~va].astype(np.float32)
        marg = ((chitr.T @ Ftr) / len(Ftr))
        marg_norm = np.sqrt((marg.astype(np.float64) ** 2).sum(1))
        order = np.argsort(-norm)
        # rank agreement of the paired estimator with the exact marginal
        rho = float(np.corrcoef(_rankdata(norm), _rankdata(marg_norm))[0, 1])
        cert = np.flatnonzero(norm >= 0.01)
        Ptr, Pva = P[~va], P[va]
        ntr = np.ones(int((~va).sum())); nva = np.ones(int(va.sum()))
        base = fit_softmax_slots(np.zeros((len(Ptr), 0), np.float32), Ptr, ntr,
                                 np.zeros((len(Pva), 0), np.float32), Pva, nva, steps=1)
        uni_kl = base["val_kl"]
        # KL LADDER: fit the top-K heaviest oracle leaves for growing K, so the
        # run PRINTS progress toward the goal (KL dropping below unigram as each
        # certified leaf is added) instead of one end-of-run number.
        ladder = []
        B = sb.shape[1]
        # sparse certified rungs + the full deg-1 basis (comparable to the exact
        # enumeration number regardless of how many bits clear the clean bar)
        rungs = [k for k in (5, 10, 25, 50, 100, 200, len(cert), B)
                 if 0 < k <= B]
        rungs = sorted(set(rungs))
        best_fit = None
        for K in rungs:
            bits_k = order[:K]
            Xtr = 1.0 - 2.0 * sb[~va][:, bits_k].astype(np.float32)
            Xva = 1.0 - 2.0 * sb[va][:, bits_k].astype(np.float32)
            fk = fit_softmax_slots(Xtr, Ptr, ntr, Xva, Pva, nva, steps=1500, lr=0.01)
            ladder.append({"K": int(K), "val_kl": fk["val_kl"],
                           "gain": uni_kl - fk["val_kl"]})
            best_fit = fk
            print(f"[oracle {name}] top-{K} leaves -> val_kl {fk['val_kl']:.4f} "
                  f"(unigram {uni_kl:.4f}, gain {uni_kl - fk['val_kl']:+.4f})",
                  flush=True)
        summary["encodings"][name] = {
            "top_norms": norm[order[:15]].round(5).tolist(),
            "top_bits": [int(b) for b in order[:15]],
            "n_ge_0.01": int((norm >= 0.01).sum()),
            "n_ge_0.02": int((norm >= 0.02).sum()),
            "total_mass": float((psi[psi > 0]).sum()),
            "max_norm": float(norm.max()),
            "rank_corr_paired_vs_marginal": rho,
            "n_certified": int(len(cert)),
            "kl_ladder": ladder,
            "sparse_val_kl": (best_fit["val_kl"] if best_fit else uni_kl),
            "sparse_improved": (best_fit["improved"] if best_fit else False),
            "unigram_val_kl": uni_kl}
        print(name, json.dumps({k: summary["encodings"][name][k] for k in
              ("max_norm", "n_ge_0.01", "rank_corr_paired_vs_marginal",
               "sparse_val_kl", "unigram_val_kl")}), flush=True)
        torch.cuda.empty_cache()
    _write_json(f"{ROOT}/oracle_f{fill_len}_M{m_fibers}_G{g}_p{p_back}.json", summary)
    _record("oracle", started, {"p_back": p_back})
    return summary


def _rankdata(a):
    order = np.argsort(a)
    ranks = np.empty(len(a), dtype=np.float64)
    ranks[order] = np.arange(len(a))
    return ranks


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=21600, memory=32768)
def search_csamp(tau: float, m_fibers: int = M_FIBERS, r: int = R_FILLS,
                 val_frac: float = 0.15, fill_len: int = W_WIN - K_FIXED,
                 max_width: int = 512, encoding: str = "all"):
    """The paper-native PAIRED CSAMP tree on the fiber table: PER-FILL rows,
    cells = (fiber x un-split value), diagonal excluded.  Reports the
    empirical collision profile alongside the recovered characters, then fits
    and evaluates them exactly like the group-by search stage."""
    import os, time
    import torch
    vol.reload()
    _budget("csamp", 1.5)
    started = time.time()
    data = np.load(_table_path(fill_len, m_fibers, r))
    P, fiber_id = data["P"], data["fiber_id"]
    fill_len = data["G"].shape[1]
    contexts = np.concatenate([data["PRE"][fiber_id], data["G"]], axis=1)
    z = np.load(f"{ROOT}/codes.npz")
    tables = {"lsh": z["lsh"], "ctrl": z["ctrl"], "idbits": z["idbits"]}
    if encoding != "all":
        tables = {encoding: tables[encoding]}
    rng = np.random.default_rng(0)
    val_fibers = rng.random(fiber_id.max() + 1) < val_frac
    va_rows = val_fibers[fiber_id]
    # centered PER-FILL targets: the estimator needs the individual pair draws
    mu = P[~va_rows].astype(np.float64).mean(0)
    F = (P[~va_rows] - mu[None, :]).astype(np.float32)
    fib_tr = fiber_id[~va_rows]
    summary = {"tau": tau, "estimator": "paired-csamp", "m_rows": int(len(F)),
               "encodings": {}}
    for name, codes in tables.items():
        bits_tr = context_bits(contexts[~va_rows], codes)
        # live progress: fit the frontier's characters on per-fill rows and
        # report held-out KL as the descent proceeds
        bits_va = context_bits(contexts[va_rows], codes)
        Ptr, Pva = P[~va_rows], P[va_rows]
        one_tr, one_va = np.ones(len(Ptr)), np.ones(len(Pva))
        uni_kl = fit_softmax_slots(bits_tr[:, :0], Ptr, one_tr,
                                   bits_va[:, :0], Pva, one_va, steps=1)["val_kl"]
        cb = _kl_progress(name, bits_tr, Ptr, one_tr, bits_va, Pva, one_va, uni_kl)
        t0 = time.time()
        got = dataset_gl_csamp(bits_tr, F, fib_tr, tau, max_width=max_width,
                               n_search=fill_len * codes.shape[1], on_progress=cb)
        prof = got["pair_profile"]
        entry = {"status": got["status"], "widths": got["widths"],
                 "psi_top": got["psi_top"],
                 "n_characters": int(len(got["masks"])),
                 "pair_profile_min": (float(min(prof)) if prof else 0.0),
                 "pair_profile_terminal": (float(prof[-1]) if prof else 0.0),
                 "n_no_evidence_levels": len(got["no_evidence_levels"]),
                 "n_saturated_levels": len(got["saturated_levels"]),
                 "search_seconds": time.time() - t0}
        if got["status"] == "ok" and len(got["masks"]):
            masks = got["masks"]
            np.savez_compressed(
                f"{ROOT}/csamp_masks_f{fill_len}_{name}_tau{tau}.npz",
                masks=masks, pair_profile=np.asarray(prof))
            ctx_tr, A_tr, n_tr, _ = soft_collapse(contexts[~va_rows], P[~va_rows])
            ctx_va, A_va, n_va, _ = soft_collapse(contexts[va_rows], P[va_rows])
            bt, bv = context_bits(ctx_tr, codes), context_bits(ctx_va, codes)
            fit = fit_softmax_slots(parity_features(bt, masks), A_tr / n_tr[:, None],
                                    n_tr, parity_features(bv, masks),
                                    A_va / n_va[:, None], n_va)
            entry["val_kl"] = fit["val_kl"]
            entry["eval"] = eval_slots(fit["W"], fit["b"], parity_features(bv, masks),
                                       A_va / n_va[:, None], n_va)
            base = fit_softmax_slots(np.zeros((len(ctx_tr), 0), np.float32),
                                     A_tr / n_tr[:, None], n_tr,
                                     np.zeros((len(ctx_va), 0), np.float32),
                                     A_va / n_va[:, None], n_va, steps=1)
            entry["unigram_val_kl"] = base["val_kl"]
        summary["encodings"][name] = entry
        print(name, json.dumps({k: v for k, v in entry.items() if k != "widths"}),
              flush=True)
        torch.cuda.empty_cache()
    _write_json(f"{ROOT}/summary_csamp_f{fill_len}_tau{tau}.json", summary)
    _record("csamp", started, {"tau": tau})
    return summary


def _fit_block(F_tr, off_tr, P_tr, n_tr, F_va, off_va, P_va, n_va,
               steps=1500, lr=0.01, device=None):
    """Fit one token block's deg-1 weights ON TOP of frozen logits ``off``.
    Returns (W (B, V), val_kl, improved)."""
    import torch
    device = device or ("cuda" if torch.cuda.is_available() else "cpu")
    F_tr = torch.tensor(F_tr, dtype=torch.float32, device=device)
    F_va = torch.tensor(F_va, dtype=torch.float32, device=device)
    off_tr = torch.tensor(off_tr, dtype=torch.float32, device=device)
    off_va = torch.tensor(off_va, dtype=torch.float32, device=device)
    P_tr = torch.tensor(P_tr, dtype=torch.float32, device=device)
    P_va = torch.tensor(P_va, dtype=torch.float32, device=device)
    w_tr = torch.tensor(np.asarray(n_tr, dtype=np.float32), device=device)
    w_va = torch.tensor(np.asarray(n_va, dtype=np.float32), device=device)
    Wm = torch.zeros((F_tr.shape[1], P_tr.shape[1]), device=device, requires_grad=True)
    opt = torch.optim.Adam([Wm], lr=lr)
    sched = torch.optim.lr_scheduler.CosineAnnealingLR(opt, T_max=steps)

    def val_kl():
        with torch.no_grad():
            lq = torch.log_softmax(off_va + F_va @ Wm, dim=1)
            lp = torch.log(P_va.clamp_min(1e-12))
            return float((w_va * (P_va * (lp - lq)).sum(1)).sum() / w_va.sum())

    best = init_val = val_kl()
    best_W = Wm.detach().clone()
    for _ in range(steps):
        lq = torch.log_softmax(off_tr + F_tr @ Wm, dim=1)
        loss = -(w_tr * (P_tr * lq).sum(1)).sum() / w_tr.sum()
        opt.zero_grad(set_to_none=True)
        loss.backward()
        torch.nn.utils.clip_grad_norm_([Wm], 1.0)
        opt.step()
        sched.step()
        vk = val_kl()
        if vk < best:
            best, best_W = vk, Wm.detach().clone()
    return best_W.cpu().numpy(), best, bool(best < init_val - 1e-4)


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=21600, memory=65536)
def fit_staged(m_fibers: int = M_FIBERS, r: int = R_FILLS, val_frac: float = 0.15,
               fill_len: int = W_WIN - K_FIXED, only: str = "lsh,ctrl",
               patience: int = 3):
    """The campaign's staged/backoff student, canonically: add one token block
    of deg-1 features at a time (nearest the prediction first), accept a block
    only if fiber-disjoint validation improves, stop after ``patience``
    consecutive rejections.  This is the honest full-context linear student:
    the val gate rejects fiber-memorizing prefix blocks by construction."""
    import time
    vol.reload()
    _budget("fit-staged", 1.5)
    started = time.time()
    data = np.load(_table_path(fill_len, m_fibers, r))
    P, fiber_id, q = data["P"], data["fiber_id"], int(data["q"])
    fill_len = data["G"].shape[1]
    contexts = np.concatenate([data["PRE"][fiber_id], data["G"]], axis=1)
    z = dict(np.load(f"{ROOT}/codes.npz"))
    tables = {k: z[k] for k in only.split(",")}
    rng = np.random.default_rng(0)
    val_fibers = rng.random(fiber_id.max() + 1) < val_frac
    va_rows = val_fibers[fiber_id]
    ctx_tr, A_tr, n_tr, _ = soft_collapse(contexts[~va_rows], P[~va_rows])
    ctx_va, A_va, n_va, _ = soft_collapse(contexts[va_rows], P[va_rows])
    P_tr, P_va = A_tr / n_tr[:, None], A_va / n_va[:, None]
    mu = A_tr.sum(0) / n_tr.sum()
    results = {}
    for name, codes in tables.items():
        B = codes.shape[1]
        bits_tr = context_bits(ctx_tr, codes)
        bits_va = context_bits(ctx_va, codes)
        off_tr = np.broadcast_to(np.log(np.clip(mu, 1e-12, None)),
                                 (len(ctx_tr), len(mu))).copy()
        off_va = np.broadcast_to(np.log(np.clip(mu, 1e-12, None)),
                                 (len(ctx_va), len(mu))).copy()
        best_val, bad, accepted = None, 0, []
        for pos in range(CTX):
            F_tr = 1.0 - 2.0 * bits_tr[:, pos * B:(pos + 1) * B].astype(np.float32)
            F_va = 1.0 - 2.0 * bits_va[:, pos * B:(pos + 1) * B].astype(np.float32)
            W, vk, improved = _fit_block(F_tr, off_tr, P_tr, n_tr,
                                         F_va, off_va, P_va, n_va)
            if improved and (best_val is None or vk < best_val - 1e-4):
                best_val, bad = vk, 0
                accepted.append(pos)
                off_tr = off_tr + F_tr @ W
                off_va = off_va + F_va @ W
                print(f"[staged:{name}] block {pos} accepted val_kl={vk:.4f}",
                      flush=True)
            else:
                bad += 1
                print(f"[staged:{name}] block {pos} rejected ({bad}/{patience})",
                      flush=True)
                if bad >= patience:
                    break
        top1 = float((n_va * (P_va.argmax(1) == off_va.argmax(1))).sum() / n_va.sum())
        uni_top1 = float((n_va * (P_va.argmax(1) == int(np.argmax(mu)))).sum()
                         / n_va.sum())
        results[name] = {"accepted_blocks": accepted,
                         "val_kl": best_val, "top1": top1, "unigram_top1": uni_top1,
                         "unigram_val_kl": float((n_va[:, None] * P_va
                                                  * (np.log(np.clip(P_va, 1e-12, None))
                                                     - np.log(np.clip(mu, 1e-12, None)))).sum()
                                                 / n_va.sum())}
        print(name, json.dumps(results[name]), flush=True)
    _write_json(f"{ROOT}/summary_staged_f{fill_len}_M{m_fibers}.json",
                dict(results=results))
    _record("fit-staged", started)
    return results


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=21600, memory=65536)
def fit_staged_real(train_path: str = "/cache/qwen35_argl/train_n20000.pt",
                    val_path: str = "/cache/qwen35_argl/val_n2000.pt",
                    test_path: str = "/cache/qwen35_argl/test_n5000.pt",
                    only: str = "lsh,ctrl", patience: int = 3, steps: int = 600,
                    interactions: int = 0):
    """The staged student on REAL cached contexts (256 real+rollout tokens,
    full teacher distributions -> 512 slots).  No fiber structure: every block
    generalizes or is rejected on its merits, so the accepted-depth profile
    measures how far honest deg-1 LSH signal reaches into real context."""
    import time
    import torch
    vol.reload()
    _budget("fit-staged-real", 2.0)
    started = time.time()
    tr = torch.load(train_path, map_location="cpu", weights_only=True, mmap=True)
    va = torch.load(val_path, map_location="cpu", weights_only=True, mmap=True)
    q = int(tr["q"])
    z = dict(np.load(f"{ROOT}/codes.npz"))
    tables = {k: z[k] for k in only.split(",")}

    def slots_and_project(logits_t, slot_ids=None):
        out = np.empty((len(logits_t), V_SLOTS), dtype=np.float32)
        mass = np.zeros(q)
        for lo in range(0, len(logits_t), 512):
            p = torch.softmax(logits_t[lo:lo + 512].float()[:, :q], dim=-1).numpy()
            if slot_ids is None:
                mass += p.sum(0)
            else:
                out[lo:lo + 512, 1:] = p[:, slot_ids[1:]]
                out[lo:lo + 512, 0] = np.maximum(1.0 - out[lo:lo + 512, 1:].sum(1), 0)
        return mass if slot_ids is None else out

    te = torch.load(test_path, map_location="cpu", weights_only=True, mmap=True)
    mass = slots_and_project(tr["teacher_logits"])
    slot_ids = np.concatenate([[-1], np.argsort(-mass)[:V_SLOTS - 1]]).astype(np.int64)
    P_tr = slots_and_project(tr["teacher_logits"], slot_ids)
    P_va = slots_and_project(va["teacher_logits"], slot_ids)
    P_te = slots_and_project(te["teacher_logits"], slot_ids)
    ctx_tr = tr["contexts"].numpy().astype(np.int64)
    ctx_va = va["contexts"].numpy().astype(np.int64)
    ctx_te = te["contexts"].numpy().astype(np.int64)
    n_tr = np.ones(len(ctx_tr))
    n_va = np.ones(len(ctx_va))
    mu = P_tr.mean(0)
    results = {}
    for name, codes in tables.items():
        B = codes.shape[1]
        bits_tr = context_bits(ctx_tr, codes)
        bits_va = context_bits(ctx_va, codes)
        off_tr = np.broadcast_to(np.log(np.clip(mu, 1e-12, None)),
                                 (len(ctx_tr), len(mu))).copy()
        off_va = np.broadcast_to(np.log(np.clip(mu, 1e-12, None)),
                                 (len(ctx_va), len(mu))).copy()
        bits_te = context_bits(ctx_te, codes)
        off_te = np.broadcast_to(np.log(np.clip(mu, 1e-12, None)),
                                 (len(ctx_te), len(mu))).copy()
        best_val, bad, accepted, block_W = None, 0, [], {}
        for pos in range(ctx_tr.shape[1]):
            F_tr = 1.0 - 2.0 * bits_tr[:, pos * B:(pos + 1) * B].astype(np.float32)
            F_va = 1.0 - 2.0 * bits_va[:, pos * B:(pos + 1) * B].astype(np.float32)
            W, vk, improved = _fit_block(F_tr, off_tr, P_tr, n_tr,
                                         F_va, off_va, P_va, n_va, steps=steps)
            if improved and (best_val is None or vk < best_val - 1e-4):
                best_val, bad = vk, 0
                accepted.append(pos)
                block_W[pos] = W
                off_tr = off_tr + F_tr @ W
                off_va = off_va + F_va @ W
                F_te = 1.0 - 2.0 * bits_te[:, pos * B:(pos + 1) * B].astype(np.float32)
                off_te = off_te + F_te @ W
                print(f"[real:{name}] block {pos} accepted val_kl={vk:.4f}", flush=True)
            else:
                bad += 1
                print(f"[real:{name}] block {pos} rejected ({bad}/{patience})", flush=True)
                if bad >= patience:
                    break
        inter_gain = None
        if interactions and accepted:
            # HEREDITY interactions: pairwise parity products among the bits
            # that individually earned their block's acceptance (top-|W| rows),
            # one val-gated block on top of the converged deg-1 student.
            sel = []
            for pos in accepted:
                norms = np.linalg.norm(block_W[pos], axis=1)
                for j in np.argsort(-norms)[:interactions]:
                    sel.append(pos * B + int(j))
            def pair_feats(bits_full):
                S = 1.0 - 2.0 * bits_full[:, sel].astype(np.float32)
                cols = [S[:, i] * S[:, j] for i in range(len(sel))
                        for j in range(i + 1, len(sel))]
                return np.stack(cols, axis=1)
            Fp_tr, Fp_va, Fp_te = (pair_feats(b) for b in (bits_tr, bits_va, bits_te))
            W2, vk2, improved2 = _fit_block(Fp_tr, off_tr, P_tr, n_tr,
                                            Fp_va, off_va, P_va, n_va, steps=steps)
            if improved2 and vk2 < best_val - 1e-4:
                inter_gain = float(best_val - vk2)
                best_val = vk2
                off_va = off_va + Fp_va @ W2
                off_te = off_te + Fp_te @ W2
                print(f"[real:{name}] interactions accepted val_kl={vk2:.4f}",
                      flush=True)
            else:
                inter_gain = 0.0
                print(f"[real:{name}] interactions rejected", flush=True)
        top1 = float((P_va.argmax(1) == off_va.argmax(1)).mean())
        # untouched split: the gate never saw it, so this is the clean number
        test_kl = float((P_te * (np.log(np.clip(P_te, 1e-12, None))
                                 - (off_te - np.log(np.exp(off_te - off_te.max(1, keepdims=True))
                                                    .sum(1, keepdims=True))
                                    - off_te.max(1, keepdims=True)))).sum(1).mean())
        results[name] = {"accepted_blocks": accepted, "val_kl": best_val, "top1": top1,
                         "interaction_gain": inter_gain,
                         "test_kl": test_kl,
                         "test_top1": float((P_te.argmax(1) == off_te.argmax(1)).mean()),
                         "test_unigram_kl": float((P_te * (np.log(np.clip(P_te, 1e-12, None))
                                                           - np.log(np.clip(mu, 1e-12, None)))).sum(1).mean()),
                         "unigram_top1": float((P_va.argmax(1) == int(np.argmax(mu))).mean()),
                         "unigram_val_kl": float((P_va * (np.log(np.clip(P_va, 1e-12, None))
                                                          - np.log(np.clip(mu, 1e-12, None)))).sum(1).mean())}
        print(name, json.dumps(results[name]), flush=True)
    import os as _os
    tag = _os.path.basename(train_path).removesuffix(".pt")
    _write_json(f"{ROOT}/summary_staged_real_{tag}.json", dict(results=results))
    _record("fit-staged-real", started)
    return results


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=21600, memory=65536)
def fit_deg1(m_fibers: int = M_FIBERS, r: int = R_FILLS, val_frac: float = 0.15,
             fill_len: int = W_WIN - K_FIXED, only: str = "", span: str = "filled"):
    """AGGREGATE test in the canonical frame: fit the COMPLETE degree-1 basis
    (every single-bit parity of the filled span) per encoding on the same
    fiber table.  Individual characters are noise-bound (proven); the encoding
    effect lives in the mass of small coefficients -- this measures it with
    the canonical data, targets, and eval."""
    import time
    vol.reload()
    _budget("fit-deg1", 1.0)
    started = time.time()
    data = np.load(_table_path(fill_len, m_fibers, r))
    P, fiber_id, q = data["P"], data["fiber_id"], int(data["q"])
    fill_len = data["G"].shape[1]
    contexts = np.concatenate([data["PRE"][fiber_id], data["G"]], axis=1)
    z = dict(np.load(f"{ROOT}/codes.npz"))
    if "lshwide" not in z:
        # width-scaling arm: same seed/Gaussians, 512 projections (+tie-breaks)
        model, _, _ = _load_teacher(device="cpu")
        E = model.get_input_embeddings().weight[:q].detach().float().numpy()
        del model
        z["lshwide"], _ = build_lsh_codes(E, B0=512, cap=512)
        np.savez_compressed(f"{ROOT}/codes.npz", **z)
        vol.commit()
    tables = {"lsh": z["lsh"], "ctrl": z["ctrl"], "idbits": z["idbits"],
              "lshwide": z["lshwide"]}
    if only:
        tables = {k: tables[k] for k in only.split(",")}
    rng = np.random.default_rng(0)
    val_fibers = rng.random(fiber_id.max() + 1) < val_frac
    va_rows = val_fibers[fiber_id]
    ctx_tr, A_tr, n_tr, _ = soft_collapse(contexts[~va_rows], P[~va_rows])
    ctx_va, A_va, n_va, _ = soft_collapse(contexts[va_rows], P[va_rows])
    import os
    results = {}
    for name, codes in tables.items():
        # span="filled": the searched window only.  span="full": every context
        # token's bits -- the full-context linear student (prefix bits learn
        # from across-fiber variation; fiber-disjoint split keeps it honest).
        nb = (fill_len if span == "filled" else CTX) * codes.shape[1]
        bits_tr = context_bits(ctx_tr, codes)
        bits_va = context_bits(ctx_va, codes)
        F_tr = 1.0 - 2.0 * bits_tr[:, :nb].astype(np.float32)
        F_va = 1.0 - 2.0 * bits_va[:, :nb].astype(np.float32)
        # searched multi-bit frontier characters on top of the deg-1 basis:
        # does the tree's frontier add held-out information beyond single bits?
        fp = f"{ROOT}/frontier_f{fill_len}_{name}.npz"
        n_frontier = 0
        if os.path.exists(fp):
            fm = np.load(fp)["masks"]
            if len(fm):
                F_tr = np.concatenate([F_tr, parity_features(bits_tr, fm)], axis=1)
                F_va = np.concatenate([F_va, parity_features(bits_va, fm)], axis=1)
                n_frontier = int(len(fm))
        # peaked soft labels diverge at fixed lr as feature count grows (the
        # campaign's unigram-collapse mode, hit three times today): scale the
        # rate with 1/sqrt(features) anchored at the 8113-feature 0.004.
        lr = 0.004 * math.sqrt(8113.0 / max(F_tr.shape[1], 1))
        fit = fit_softmax_slots(F_tr, A_tr / n_tr[:, None], n_tr,
                                F_va, A_va / n_va[:, None], n_va,
                                steps=4000, lr=lr)
        ev = eval_slots(fit["W"], fit["b"], F_va, A_va / n_va[:, None], n_va)
        base = fit_softmax_slots(F_tr[:, :0], A_tr / n_tr[:, None], n_tr,
                                 F_va[:, :0], A_va / n_va[:, None], n_va, steps=1)
        results[name] = {"features": int(F_tr.shape[1]), "n_frontier": n_frontier,
                         "val_kl": fit["val_kl"], "improved": fit["improved"],
                         "lr": lr,
                         "eval": ev, "unigram_val_kl": base["val_kl"]}
        print(name, json.dumps(results[name]), flush=True)
    tag = f"_{only.replace(',', '-')}" if only else ""
    if span != "filled":
        tag += f"_{span}"
    _write_json(f"{ROOT}/summary_deg1_f{fill_len}_M{m_fibers}{tag}.json",
                dict(results=results, m_rows=int(len(contexts)), span=span))
    _record("fit-deg1", started)
    return results


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=21600, memory=32768)
def spectrum_deg1(m_fibers: int = M_FIBERS, r: int = R_FILLS,
                  fill_len: int = W_WIN - K_FIXED, val_frac: float = 0.15):
    """GROUND TRUTH, no search: every degree-1 coefficient of the centered
    slot function computed EXACTLY in one GEMM per encoding.  This settles
    whether the tree's tie-broken frontier buried heavy deg-1 characters
    (the campaign's known under-allocation failure) or none exist."""
    import time
    import torch
    vol.reload()
    _budget("spectrum-deg1", 0.5)
    started = time.time()
    data = np.load(_table_path(fill_len, m_fibers, r))
    P, fiber_id, q = data["P"], data["fiber_id"], int(data["q"])
    fill_len = data["G"].shape[1]
    contexts = np.concatenate([data["PRE"][fiber_id], data["G"]], axis=1)
    z = dict(np.load(f"{ROOT}/codes.npz"))
    tables = {"lsh": z["lsh"], "ctrl": z["ctrl"]}
    rng = np.random.default_rng(0)
    va_rows = (rng.random(fiber_id.max() + 1) < val_frac)[fiber_id]
    ctx_tr, A_tr, n_tr, _ = soft_collapse(contexts[~va_rows], P[~va_rows])
    ctx_va, A_va, n_va, _ = soft_collapse(contexts[va_rows], P[va_rows])
    M = int((~va_rows).sum())
    mu = A_tr.sum(0) / n_tr.sum()
    A_c = torch.tensor((A_tr - n_tr[:, None] * mu[None, :]).astype(np.float32),
                       device="cuda")
    summary = {"m_rows": M, "encodings": {}}
    for name, codes in tables.items():
        nb = fill_len * codes.shape[1]
        bits_tr = context_bits(ctx_tr, codes)
        F = torch.tensor(1.0 - 2.0 * bits_tr[:, :nb].astype(np.float32),
                         device="cuda")
        coeff = (F.T @ A_c) / M                                     # (nb, 512) exact
        norms = coeff.double().pow(2).sum(1).sqrt().cpu().numpy()
        del F
        torch.cuda.empty_cache()
        top = np.argsort(-norms)[:20]
        hist, edges = np.histogram(norms, bins=[0, .002, .005, .01, .02, .05, .1, 1])
        # SPARSE-RECOVERY payoff: fit ONLY the exactly-certified characters
        # (the classical GL -> sparse-model program, deg-1 done exactly)
        cert = np.flatnonzero(norms >= 0.01)
        Fc_tr = 1.0 - 2.0 * bits_tr[:, cert].astype(np.float32)
        Fc_va = 1.0 - 2.0 * context_bits(ctx_va, codes)[:, cert].astype(np.float32)
        fit = fit_softmax_slots(Fc_tr, A_tr / n_tr[:, None], n_tr,
                                Fc_va, A_va / n_va[:, None], n_va,
                                steps=3000, lr=0.01)
        base = fit_softmax_slots(Fc_tr[:, :0], A_tr / n_tr[:, None], n_tr,
                                 Fc_va[:, :0], A_va / n_va[:, None], n_va, steps=1)
        summary["encodings"][name] = {
            "n_deg1": int(nb),
            "top_norms": norms[top].round(5).tolist(),
            "top_bits": [{"token_back": int(i // codes.shape[1]),
                          "bit": int(i % codes.shape[1])} for i in top[:10]],
            "hist_edges": edges.tolist(), "hist": hist.tolist(),
            "total_deg1_mass": float((norms ** 2).sum()),
            "n_certified": int(len(cert)),
            "sparse_fit_val_kl": fit["val_kl"], "sparse_fit_improved": fit["improved"],
            "unigram_val_kl": base["val_kl"]}
        print(name, json.dumps({k: summary['encodings'][name][k]
                                for k in ('top_norms', 'n_certified',
                                          'sparse_fit_val_kl', 'unigram_val_kl')}),
              flush=True)
        torch.cuda.empty_cache()
    _write_json(f"{ROOT}/spectrum_deg1_f{fill_len}_M{m_fibers}.json", summary)
    _record("spectrum-deg1", started)
    return summary


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=21600, memory=32768)
def spectrum_deg2(m_fibers: int = M_FIBERS, r: int = R_FILLS,
                  fill_len: int = W_WIN - K_FIXED, val_frac: float = 0.15,
                  anchor_bar: float = 0.01):
    """EXACT anchored degree-2 spectrum: for every certified deg-1 anchor
    (norm >= anchor_bar) x every searched bit, compute the pair coefficient
    exactly (one GEMM per anchor).  The tree cannot do this honestly (pair
    children tie-bury like singletons); enumeration can, in minutes."""
    import time
    import torch
    vol.reload()
    _budget("spectrum-deg2", 1.0)
    started = time.time()
    data = np.load(_table_path(fill_len, m_fibers, r))
    P, fiber_id, q = data["P"], data["fiber_id"], int(data["q"])
    fill_len = data["G"].shape[1]
    contexts = np.concatenate([data["PRE"][fiber_id], data["G"]], axis=1)
    z = dict(np.load(f"{ROOT}/codes.npz"))
    tables = {"lsh": z["lsh"], "ctrl": z["ctrl"]}
    rng = np.random.default_rng(0)
    va_rows = (rng.random(fiber_id.max() + 1) < val_frac)[fiber_id]
    ctx_tr, A_tr, n_tr, _ = soft_collapse(contexts[~va_rows], P[~va_rows])
    M = int((~va_rows).sum())
    mu = A_tr.sum(0) / n_tr.sum()
    A_c = torch.tensor((A_tr - n_tr[:, None] * mu[None, :]).astype(np.float32),
                       device="cuda")
    summary = {"m_rows": M, "anchor_bar": anchor_bar, "encodings": {}}
    for name, codes in tables.items():
        nb = fill_len * codes.shape[1]
        F = torch.tensor(1.0 - 2.0 * context_bits(ctx_tr, codes)[:, :nb]
                         .astype(np.float32), device="cuda")
        d1 = ((F.T @ A_c) / M).double().pow(2).sum(1).sqrt().cpu().numpy()
        anchors = np.flatnonzero(d1 >= anchor_bar)
        best = []
        for i in anchors:
            Fp = F * F[:, [i]]                                     # (m, nb) pair features
            norms_i = ((Fp.T @ A_c) / M).double().pow(2).sum(1).sqrt().cpu().numpy()
            norms_i[i] = 0.0                                       # (i,i) is the constant
            j = int(np.argmax(norms_i))
            best.append((float(norms_i[j]), int(i), j,
                         int((norms_i >= anchor_bar).sum())))
        best.sort(reverse=True)
        n_heavy_pairs = int(sum(b[3] for b in best))
        summary["encodings"][name] = {
            "n_anchors": int(len(anchors)),
            "deg1_top": float(d1.max()),
            "top_pairs": [{"norm": round(b[0], 5), "i": b[1], "j": b[2]}
                          for b in best[:10]],
            "pairs_above_bar_total": n_heavy_pairs}
        print(name, json.dumps(summary["encodings"][name]), flush=True)
        del F
        torch.cuda.empty_cache()
    _write_json(f"{ROOT}/spectrum_deg2_f{fill_len}_M{m_fibers}.json", summary)
    _record("spectrum-deg2", started)
    return summary


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=21600, memory=32768)
def fit_sparse2(m_fibers: int = M_FIBERS, r: int = R_FILLS,
                fill_len: int = W_WIN - K_FIXED, val_frac: float = 0.15,
                anchor_bar: float = 0.01, top_pairs: int = 1000):
    """The sparse deg-1+2 model: 217 certified singles + the top exactly
    certified pairs, fitted against the deg-1-only sparse reference."""
    import time
    import torch
    vol.reload()
    _budget("fit-sparse2", 1.0)
    started = time.time()
    data = np.load(_table_path(fill_len, m_fibers, r))
    P, fiber_id, q = data["P"], data["fiber_id"], int(data["q"])
    fill_len = data["G"].shape[1]
    contexts = np.concatenate([data["PRE"][fiber_id], data["G"]], axis=1)
    z = dict(np.load(f"{ROOT}/codes.npz"))
    tables = {"lsh": z["lsh"], "ctrl": z["ctrl"]}
    # strict 3-way fiber split: train certifies+fits, val selects the best
    # iterate, TEST is untouched -- the claimed number
    rng = np.random.default_rng(0)
    u = rng.random(fiber_id.max() + 1)[fiber_id]
    te_rows, va_rows = u < 0.15, (u >= 0.15) & (u < 0.30)
    tr_rows = ~(te_rows | va_rows)
    ctx_tr, A_tr, n_tr, _ = soft_collapse(contexts[tr_rows], P[tr_rows])
    ctx_va, A_va, n_va, _ = soft_collapse(contexts[va_rows], P[va_rows])
    ctx_te, A_te, n_te, _ = soft_collapse(contexts[te_rows], P[te_rows])
    M = int(tr_rows.sum())
    mu = A_tr.sum(0) / n_tr.sum()
    A_c = torch.tensor((A_tr - n_tr[:, None] * mu[None, :]).astype(np.float32),
                       device="cuda")
    summary = {"encodings": {}}
    for name, codes in tables.items():
        nb = fill_len * codes.shape[1]
        bits_tr = context_bits(ctx_tr, codes)
        bits_va = context_bits(ctx_va, codes)
        F = torch.tensor(1.0 - 2.0 * bits_tr[:, :nb].astype(np.float32),
                         device="cuda")
        d1 = ((F.T @ A_c) / M).double().pow(2).sum(1).sqrt().cpu().numpy()
        anchors = np.flatnonzero(d1 >= anchor_bar)
        pairs = {}
        for i in anchors:
            norms_i = ((F * F[:, [i]]).T @ A_c / M).double().pow(2).sum(1) \
                .sqrt().cpu().numpy()
            norms_i[i] = 0.0
            per_anchor = max(20, (2 * top_pairs) // max(len(anchors), 1) + 1)
            for j in np.argsort(-norms_i)[:per_anchor]:
                if norms_i[j] >= anchor_bar:
                    key = (min(int(i), int(j)), max(int(i), int(j)))
                    pairs[key] = max(pairs.get(key, 0.0), float(norms_i[j]))
        top = sorted(pairs.items(), key=lambda kv: -kv[1])[:top_pairs]
        ii = np.array([k[0] for k, _ in top]); jj = np.array([k[1] for k, _ in top])
        del F
        torch.cuda.empty_cache()

        def feats(bits):
            S = 1.0 - 2.0 * bits[:, :nb].astype(np.float32)
            return (S[:, anchors],
                    np.concatenate([S[:, anchors], S[:, ii] * S[:, jj]], axis=1))
        F1_tr, F12_tr = feats(bits_tr)
        F1_va, F12_va = feats(bits_va)
        bits_te = context_bits(ctx_te, codes)
        F1_te, F12_te = feats(bits_te)
        fit1 = fit_softmax_slots(F1_tr, A_tr / n_tr[:, None], n_tr,
                                 F1_va, A_va / n_va[:, None], n_va,
                                 steps=3000, lr=0.01)
        fit12 = fit_softmax_slots(F12_tr, A_tr / n_tr[:, None], n_tr,
                                  F12_va, A_va / n_va[:, None], n_va,
                                  steps=3000, lr=0.01)
        te1 = eval_slots(fit1["W"], fit1["b"], F1_te, A_te / n_te[:, None], n_te)
        te12 = eval_slots(fit12["W"], fit12["b"], F12_te, A_te / n_te[:, None], n_te)
        te_uni = float((n_te[:, None] * (A_te / n_te[:, None])
                        * (np.log(np.clip(A_te / n_te[:, None], 1e-12, None))
                           - np.log(np.clip(mu, 1e-12, None)))).sum() / n_te.sum())
        np.savez_compressed(f"{ROOT}/model_sparse2_{name}.npz",
                            anchors=anchors, pair_i=ii, pair_j=jj,
                            W=fit12["W"], b=fit12["b"], mu=mu,
                            slot_ids=data["slot_ids"], fill_len=fill_len,
                            B=codes.shape[1])
        vol.commit()
        summary["encodings"][name] = {
            "n_deg1": int(len(anchors)), "n_pairs": int(len(top)),
            "deg1_val_kl": fit1["val_kl"], "deg12_val_kl": fit12["val_kl"],
            "deg1_TEST_kl": te1["kl"], "deg12_TEST_kl": te12["kl"],
            "deg12_TEST_top1": te12["top1"], "TEST_unigram_kl": te_uni,
            "deg12_improved": fit12["improved"]}
        print(name, json.dumps(summary["encodings"][name]), flush=True)
    _write_json(f"{ROOT}/summary_sparse2_f{fill_len}_M{m_fibers}.json", summary)
    _record("fit-sparse2", started)
    return summary


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=21600, memory=32768)
def fit_sparse3(m_fibers: int = M_FIBERS, r: int = R_FILLS,
                fill_len: int = W_WIN - K_FIXED, anchor_bar: float = 0.01,
                top_pairs: int = 1000, pair_anchors: int = 200,
                top_triples: int = 1000):
    """Degree 3 of the recovery ladder: exactly enumerate triples anchored on
    the heaviest certified pairs, fit deg-1+2+3 vs the deg-1+2 reference on
    the strict 3-way split (test untouched)."""
    import time
    import torch
    vol.reload()
    _budget("fit-sparse3", 1.5)
    started = time.time()
    data = np.load(_table_path(fill_len, m_fibers, r))
    P, fiber_id, q = data["P"], data["fiber_id"], int(data["q"])
    fill_len = data["G"].shape[1]
    contexts = np.concatenate([data["PRE"][fiber_id], data["G"]], axis=1)
    z = dict(np.load(f"{ROOT}/codes.npz"))
    tables = {"lsh": z["lsh"], "ctrl": z["ctrl"]}
    rng = np.random.default_rng(0)
    u = rng.random(fiber_id.max() + 1)[fiber_id]
    te_rows, va_rows = u < 0.15, (u >= 0.15) & (u < 0.30)
    tr_rows = ~(te_rows | va_rows)
    ctx_tr, A_tr, n_tr, _ = soft_collapse(contexts[tr_rows], P[tr_rows])
    ctx_va, A_va, n_va, _ = soft_collapse(contexts[va_rows], P[va_rows])
    ctx_te, A_te, n_te, _ = soft_collapse(contexts[te_rows], P[te_rows])
    M = int(tr_rows.sum())
    mu = A_tr.sum(0) / n_tr.sum()
    A_c = torch.tensor((A_tr - n_tr[:, None] * mu[None, :]).astype(np.float32),
                       device="cuda")
    summary = {"encodings": {}}
    for name, codes in tables.items():
        nb = fill_len * codes.shape[1]
        bits_tr = context_bits(ctx_tr, codes)
        F = torch.tensor(1.0 - 2.0 * bits_tr[:, :nb].astype(np.float32),
                         device="cuda")
        d1 = ((F.T @ A_c) / M).double().pow(2).sum(1).sqrt().cpu().numpy()
        anchors = np.flatnonzero(d1 >= anchor_bar)
        pairs = {}
        for i in anchors:
            ni = ((F * F[:, [i]]).T @ A_c / M).double().pow(2).sum(1) \
                .sqrt().cpu().numpy()
            ni[i] = 0.0
            for j in np.argsort(-ni)[:20]:
                if ni[j] >= anchor_bar:
                    key = (min(int(i), int(j)), max(int(i), int(j)))
                    pairs[key] = max(pairs.get(key, 0.0), float(ni[j]))
        top_p = sorted(pairs.items(), key=lambda kv: -kv[1])[:top_pairs]
        ii = np.array([k[0] for k, _ in top_p]); jj = np.array([k[1] for k, _ in top_p])
        triples = {}
        for (a, b), _norm in top_p[:pair_anchors]:
            Fab = F[:, [a]] * F[:, [b]]
            nt = ((F * Fab).T @ A_c / M).double().pow(2).sum(1).sqrt().cpu().numpy()
            nt[[a, b]] = 0.0
            for c in np.argsort(-nt)[:10]:
                if nt[c] >= anchor_bar:
                    key = tuple(sorted((a, b, int(c))))
                    triples[key] = max(triples.get(key, 0.0), float(nt[c]))
        top_t = sorted(triples.items(), key=lambda kv: -kv[1])[:top_triples]
        tt = np.array([list(k) for k, _ in top_t]) if top_t else np.zeros((0, 3), int)
        del F
        torch.cuda.empty_cache()

        def feats(ctx):
            S = 1.0 - 2.0 * context_bits(ctx, codes)[:, :nb].astype(np.float32)
            f12 = np.concatenate([S[:, anchors], S[:, ii] * S[:, jj]], axis=1)
            if len(tt):
                f123 = np.concatenate(
                    [f12, S[:, tt[:, 0]] * S[:, tt[:, 1]] * S[:, tt[:, 2]]], axis=1)
            else:
                f123 = f12
            return f12, f123
        F12_tr, F123_tr = feats(ctx_tr)
        F12_va, F123_va = feats(ctx_va)
        F12_te, F123_te = feats(ctx_te)
        fit12 = fit_softmax_slots(F12_tr, A_tr / n_tr[:, None], n_tr,
                                  F12_va, A_va / n_va[:, None], n_va,
                                  steps=3000, lr=0.01)
        te12 = eval_slots(fit12["W"], fit12["b"], F12_te, A_te / n_te[:, None], n_te)
        # INCREMENTAL deg-3: triples fitted on the FROZEN deg-1+2 residual
        # (flat joint refit demonstrably overfits; staged > flat, third time)
        n12 = F12_tr.shape[1]
        off_tr = F12_tr @ fit12["W"] + fit12["b"]
        off_va = F12_va @ fit12["W"] + fit12["b"]
        off_te = F12_te @ fit12["W"] + fit12["b"]
        W3, vk3, improved3 = _fit_block(F123_tr[:, n12:], off_tr,
                                        A_tr / n_tr[:, None], n_tr,
                                        F123_va[:, n12:], off_va,
                                        A_va / n_va[:, None], n_va,
                                        steps=2000, lr=0.005)
        te123 = eval_slots(np.concatenate([fit12["W"], W3]), fit12["b"],
                           F123_te, A_te / n_te[:, None], n_te)
        summary["encodings"][name] = {
            "n_triples": int(len(tt)),
            "triple_top": float(top_t[0][1]) if top_t else 0.0,
            "deg3_improved": bool(improved3),
            "deg12_TEST_kl": te12["kl"], "deg123_TEST_kl": te123["kl"],
            "deg123_TEST_top1": te123["top1"]}
        np.savez_compressed(f"{ROOT}/model_sparse3_{name}.npz",
                            anchors=anchors, pair_i=ii, pair_j=jj, triples=tt,
                            W=np.concatenate([fit12["W"], W3]), b=fit12["b"], mu=mu,
                            slot_ids=data["slot_ids"], fill_len=fill_len,
                            B=codes.shape[1])
        vol.commit()
        print(name, json.dumps(summary["encodings"][name]), flush=True)
    _write_json(f"{ROOT}/summary_sparse3_f{fill_len}_M{m_fibers}.json", summary)
    _record("fit-sparse3", started)
    return summary


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=21600, memory=32768)
def search_slots(tau: float = 0.02, m_fibers: int = M_FIBERS, r: int = R_FILLS,
                 fill_len: int = W_WIN - K_FIXED, top_slots: int = 5,
                 max_width: int = 64, val_frac: float = 0.15):
    """CLASSICAL per-slot GL: scalar dataset-GL on individual next-slot
    indicator functions (centered), one slot at a time.  The 512-slot vector
    search aggregates Sum_t f_hat_t^2, which can wash out slots whose own
    structure IS sparse; this is the untested classical setting.  Reports each
    slot's target scale sigma so leaf norms are interpretable against the
    per-slot noise floor."""
    import time
    vol.reload()
    _budget("search-slots", 1.5)
    started = time.time()
    data = np.load(_table_path(fill_len, m_fibers, r))
    P, fiber_id, q = data["P"], data["fiber_id"], int(data["q"])
    fill_len = data["G"].shape[1]
    contexts = np.concatenate([data["PRE"][fiber_id], data["G"]], axis=1)
    z = dict(np.load(f"{ROOT}/codes.npz"))
    tables = {"lsh": z["lsh"], "ctrl": z["ctrl"]}
    rng = np.random.default_rng(0)
    va_rows = (rng.random(fiber_id.max() + 1) < val_frac)[fiber_id]
    ctx_tr, A_tr, n_tr, _ = soft_collapse(contexts[~va_rows], P[~va_rows])
    mu = A_tr.sum(0) / n_tr.sum()
    slots = np.argsort(-mu)[:top_slots]
    summary = {"tau": tau, "slots": slots.tolist(), "encodings": {}}
    for name, codes in tables.items():
        bits_tr = context_bits(ctx_tr, codes)
        per_slot = {}
        for t in slots:
            a = (A_tr[:, [t]] - n_tr[:, None] * mu[t]).astype(np.float32)
            sigma = float(np.sqrt((a[:, 0] ** 2 / np.maximum(n_tr, 1)).sum()
                                  / n_tr.sum()))
            got = dataset_gl_tau(bits_tr, a, tau, norm_m=int((~va_rows).sum()),
                                 max_width=max_width,
                                 n_search=fill_len * codes.shape[1])
            per_slot[int(t)] = {"sigma": sigma, "mu": float(mu[t]),
                                "n_characters": int(len(got["masks"])),
                                "leaf_norms_top": got["leaf_norms_top"][:5]}
            print(f"[slots:{name}] slot {t} mu={mu[t]:.3f} sigma={sigma:.4f} "
                  f"chars={len(got['masks'])} "
                  f"top={got['leaf_norms_top'][:3]}", flush=True)
        summary["encodings"][name] = per_slot
    _write_json(f"{ROOT}/summary_slots_tau{tau}.json", summary)
    _record("search-slots", started)
    return summary


@app.local_entrypoint()
def main(stage: str = "search", tau: float = 0.1, m_fibers: int = M_FIBERS,
         r: int = R_FILLS, fill_len: int = W_WIN - K_FIXED, max_width: int = 512,
         encoding: str = "all", span: str = "filled", patience: int = 3,
         steps: int = 600,
         train_path: str = "/cache/qwen35_argl/train_n20000.pt",
         interactions: int = 0, top_pairs: int = 1000,
         g: int = 24, p_back: int = 0, depth: int = 6,
         resample: str = "conditional", corpus: str = "fineweb",
         positions: str = "", target: str = "hidden",
         k1: int = 512, k2: int = 512, k3: int = 256, lam: float = 0.1,
         batch: int = 8192, n_train: int = 1_000_000, n_test: int = 50_000,
         target_chars: int = 1_000_000, chunk: int = 16384, win: int = 61,
         kappa: float = 4.0, mlp: int = 0, variant: str = "raw",
         blocks: int = 3, max_tris: int = 200_000, far: int = 0,
         deg4: int = 0):
    if stage in ("data", "all"):
        print(make_data.remote(m_fibers, r, 0, fill_len))
    if stage == "fit-deg1":
        fit_deg1.remote(m_fibers, r, 0.15, fill_len,
                        encoding if encoding != "all" else "", span)
    if stage == "fit-staged":
        fit_staged.remote(m_fibers, r, 0.15, fill_len,
                          encoding if encoding != "all" else "lsh,ctrl")
    if stage == "search-slots":
        search_slots.remote(tau, m_fibers, r, fill_len)
    if stage == "spectrum-deg1":
        spectrum_deg1.remote(m_fibers, r, fill_len)
    if stage == "spectrum-deg2":
        spectrum_deg2.remote(m_fibers, r, fill_len)
    if stage == "fit-sparse2":
        fit_sparse2.remote(m_fibers, r, fill_len, 0.15, 0.01, top_pairs)
    if stage == "fit-sparse3":
        fit_sparse3.remote(m_fibers, r, fill_len)
    if stage == "fit-staged-real":
        fit_staged_real.remote(train_path, "/cache/qwen35_argl/val_n2000.pt",
                               "/cache/qwen35_argl/test_n5000.pt",
                               only=encoding if encoding != "all" else "lsh,ctrl",
                               patience=patience, steps=steps,
                               interactions=interactions)
    if stage == "csamp":
        search_csamp.remote(tau, m_fibers, r, 0.15, fill_len, max_width, encoding)
    if stage == "oracle-data":
        make_oracle_data.remote(m_fibers, g, p_back, fill_len)
    if stage == "oracle":
        oracle.remote(m_fibers, g, p_back, fill_len, 0.15, encoding)
    if stage == "gl-tree":
        gl_tree.remote(m_fibers, g, depth, fill_len, tau, max_width,
                       8000, 0.15, encoding)
    if stage == "gl-tree-refit":
        gl_tree_refit.remote(fill_len, depth, m_fibers, 0.15, top_pairs, 400,
                             encoding if encoding != "all" else "lsh")
    if stage == "stream-fit":
        print(stream_fit.remote(m_fibers, 3000, fill_len, r, 3e-4,
                                encoding if encoding != "all" else "lsh"))
    if stage == "relabel":
        print(relabel_hidden.remote(m_fibers, fill_len, r, span))
    if stage == "gen-data":                                          # fan-out pre-generation
        gen_data.remote(3000, r, fill_len, "edu", 0, "edu_test", 4)
        print(gen_data.remote(m_fibers, r, fill_len, "edu", 9000, "edu_tr", 8))
    if stage == "gl-tree-top1":                                       # the ACTUAL Dataset GL
        print(gl_tree_top1.remote(m_fibers, g, depth, fill_len, tau, max_width,
                                  variant=variant))
    if stage == "refit-top1":                                        # degree-first re-fit
        print(refit_top1.remote(depth, fill_len))
    if stage == "stream-top1":
        print(stream_top1.remote(m_fibers, 3000, fill_len, r, target))
    if stage == "dl-fourier":                                        # learned STE parities
        print(dl_fourier.remote(m_fibers, 3000, fill_len, r, k1, k2, k3,
                                lam, steps, 0.02, batch))
    if stage == "plain-data":                                        # streamed plain supervision
        print(make_plain_data.remote(n_test, CTX, 0, "plain_test"))
        print(gen_plain_data.remote(n_train, CTX, 3 * n_test, "plain_tr", 8))
    if stage == "grad-sense":                                        # gradient matching pursuit
        print(grad_sense.remote(n_train, n_test, win, target_chars, chunk, steps,
                                64, patience, lam, batch, 0.02, kappa, 200,
                                encoding if encoding != "all" else "lsh", mlp))
    if stage == "deg2-exact":                                        # exact pair enumeration
        print(deg2_exact.remote(n_train, n_test, win, 64,
                                encoding if encoding != "all" else "lsh"))
    if stage == "deg2-fit":                                          # exact-pair reconstruction
        print(deg2_fit.remote(n_train, n_test, win, 64, target_chars,
                              encoding if encoding != "all" else "lsh"))
    if stage == "fourier-learn":                                     # learned decomposition
        print(fourier_learn.remote(n_train, n_test, win, 64, chunk, 0.5, lam,
                                   0.0, 0.1, steps, 0.05, batch,
                                   chunk // 2, chunk // 2,
                                   encoding if encoding != "all" else "lsh"))
    if stage == "deg3-fit":                                          # anchored triple ladder
        print(deg3_fit.remote(n_train, n_test, win, 64, 160_000, blocks, 16,
                              kappa, 3000, max_tris,
                              encoding if encoding != "all" else "lsh", far,
                              deg4))
    if stage == "wandb-ping":
        print(wandb_ping.remote())
    if stage == "sensitivity":
        print(sensitivity.remote(m_fibers=m_fibers, g=g, resample=resample,
                                 corpus=corpus, positions=positions))
    if stage == "oracle-sweep":
        # native deg-1 at EVERY filled token position (0=newest .. fill_len-1),
        # one container per level; the fast reorder_cache fork keeps each cheap
        calls = [oracle.spawn(m_fibers, g, pb, fill_len, 0.15, encoding)
                 for pb in range(fill_len)]
        for c in calls:
            print(json.dumps(c.get()["encodings"].get("lsh", {}).get("kl_ladder", []),
                             indent=0), flush=True)
    if stage in ("search", "all"):
        if encoding == "parallel":
            # one container per encoding; summaries land as separate files
            calls = [search.spawn(tau, m_fibers, r, 0.15, fill_len, max_width, e)
                     for e in ("lsh", "ctrl", "idbits")]
            for c in calls:
                c.get()
        else:
            search.remote(tau, m_fibers, r, 0.15, fill_len, max_width, encoding)
