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
CTX = 128            # real span length; fiber prefix = span[: CTX - (W - K)]
W_WIN = 6            # window tokens (fixed string s = first K, model fills W-K)
K_FIXED = 3
M_FIBERS = 4000
R_FILLS = 8
V_SLOTS = 512
B0, B_CAP = 64, 128  # LSH projection doubling range

ROOT = "/cache/canonical/qary_lsh_gl"
A10_PER_SECOND = 0.000306
BUDGET = 14.0   # raised from 8 for the full-span (fill 61) searches

app = modal.App("canonical-qary-lsh-gl")
vol = modal.Volume.from_name("fda-cache", create_if_missing=True)
image = (
    modal.Image.debian_slim(python_version="3.11")
    .pip_install("numpy>=1.26", "torch>=2.5", "transformers>=5.13.1",
                  "accelerate>=1.2", "datasets>=4.0", "safetensors", "sentencepiece")
    .env({"HF_HOME": "/cache/hf"})
)


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
                     n_search=None):
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
        if (k + 1) % max(1, n_search // 20) == 0:
            print(f"[csamp] level {k + 1}/{n_search} width={len(live_masks)} "
                  f"pairs={npairs:.0f}", flush=True)
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


def oracle_deg1_psi(split_bits, F_rows, fiber_gid, device=None):
    """The paper's PAIRED bucket for every single-bit character of ONE split
    coordinate, evaluated on FORKED-CACHE data (lem:qary-kv-estimator): every
    row of a fiber shares the real prefix AND the generated stub L_k, so all
    within-fiber pairs are valid by construction -- no collision on the
    un-split suffix is required, and none of the frontier-burial / offline
    collision-starvation applies.  For bit b,

        psi(b) = (1/n_pairs) sum_z ( ||sum_{i in z} chi_b(i) F_i||^2
                                     - sum_{i in z} ||F_i||^2 ),

    unbiased for E_z|E[F chi_b | z]|^2 (z = fiber = one fork).  Returns
    psi (B,) and its sqrt (the conditional deg-1 norm, comparable to the exact
    spectrum's leaf norms).

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
    n_pairs = float((counts * (counts - 1)).sum())
    if n_pairs <= 0:
        B = split_bits.shape[1]
        return np.zeros(B), np.zeros(B)
    F_t = torch.tensor(np.asarray(F_rows, dtype=np.float32), device=device)
    gid_t = torch.tensor(gid, dtype=torch.long, device=device)
    signs = torch.tensor(1.0 - 2.0 * split_bits.astype(np.float32), device=device)
    Q = _bucket_Q(signs.t().contiguous(), F_t, gid_t, ng, mem_budget=2.0e8).cpu().numpy()
    diag = float((F_t.double() ** 2).sum().item())
    psi = (Q - diag) / n_pairs
    return psi, np.sqrt(np.maximum(psi, 0.0))


def dataset_gl_tau(bits, A, tau, norm_m, max_width=512, device=None, n_search=None):
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
        if kk % 500 == 0:
            print(f"[gl] level {kk}/{n_search} width={len(live_masks)} ng={ng}",
                  flush=True)
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
                      device=None, seed=0):
    """Convex weighted soft-CE fit of slot logits = F @ W + b.  Adam + cosine
    decay + grad clip; bias initialized at the train log-unigram; best-val
    iterate kept (the model never ends worse than its initialization)."""
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
    opt = torch.optim.Adam([Wm, b], lr=lr)
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


def _stream_spans(tok, n_spans, span_len=CTX, max_docs=2_000_000):
    from datasets import load_dataset
    ds = load_dataset(FINEWEB[0], name=FINEWEB[1], split="train", streaming=True)
    spans = []
    for i, row in enumerate(ds):
        text = row.get("text") or ""
        ids = np.asarray(tok(text, add_special_tokens=False)["input_ids"], dtype=np.int64)
        if len(ids) >= span_len:
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


def _fill_and_label(model, PRE, w_fill, R, q, slot_ids, batch=32, seed=0):
    """R AR fills of w_fill tokens per fiber + slot-projected terminal teacher
    distribution of the completed window.  Returns (G (M*R, w_fill), P (M*R, V))."""
    import torch
    rng = torch.Generator(device="cuda").manual_seed(seed)
    rows = np.repeat(np.arange(len(PRE)), R)
    G = np.empty((len(rows), w_fill), dtype=np.int64)
    P = np.empty((len(rows), len(slot_ids)), dtype=np.float32)
    slot_t = torch.tensor(slot_ids[1:], dtype=torch.long, device="cuda")
    for lo in range(0, len(rows), batch):
        sel = rows[lo:lo + batch]
        ids = torch.tensor(PRE[sel], dtype=torch.long, device="cuda")
        with torch.inference_mode():
            out = model(input_ids=ids, use_cache=True, return_dict=True)
            past, logits = out.past_key_values, out.logits[:, -1, :]
            made = []
            for _ in range(w_fill):
                probs = torch.softmax(logits[:, :q].float(), dim=-1)
                nxt = torch.multinomial(probs, 1, generator=rng)
                made.append(nxt)
                out = model(input_ids=nxt, past_key_values=past, use_cache=True,
                            return_dict=True)
                past, logits = out.past_key_values, out.logits[:, -1, :]
            term = torch.softmax(logits[:, :q].float(), dim=-1)
        G[lo:lo + batch] = torch.cat(made, dim=1).cpu().numpy()
        picked = term[:, slot_t].cpu().numpy()
        P[lo:lo + batch, 1:] = picked
        P[lo:lo + batch, 0] = np.maximum(1.0 - picked.sum(1), 0.0)
        if (lo // batch) % 20 == 0:
            print(f"[fill] {lo + len(sel)}/{len(rows)}", flush=True)
    return G, P


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


def _table_path(fill_len, m_fibers, r):
    if fill_len == W_WIN - K_FIXED:
        return f"{ROOT}/glds_w{W_WIN}_k{K_FIXED}_M{m_fibers}_R{r}.npz"
    return f"{ROOT}/glds_ctx{CTX}_f{fill_len}_M{m_fibers}_R{r}.npz"


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=21600, memory=32768)
def make_data(m_fibers: int = M_FIBERS, r: int = R_FILLS, seed: int = 0,
              fill_len: int = W_WIN - K_FIXED):
    """Fibers from FineWeb + AR fills + slot-projected teacher labels -> one npz.
    fill_len = how many context tokens the model fills (the searchable span);
    the remaining CTX - fill_len tokens are real dataset conditioning."""
    import os, time
    vol.reload()
    out = _table_path(fill_len, m_fibers, r)
    if os.path.exists(out):
        return out
    _budget("data", 3.0)
    started = time.time()
    model, tok, q = _load_teacher()
    spans = _stream_spans(tok, n_spans=3 * m_fibers)
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
    shard_dir = f"{ROOT}/shards_f{fill_len}_M{m_fibers}_R{r}"
    os.makedirs(shard_dir, exist_ok=True)
    chunk = 500
    G_parts, P_parts = [], []
    for lo in range(0, m_fibers, chunk):
        sp = f"{shard_dir}/s{lo}.npz"
        if os.path.exists(sp):
            z = np.load(sp)
            G_parts.append(z["G"]); P_parts.append(z["P"])
            continue
        Gc, Pc = _fill_and_label(model, PRE[lo:lo + chunk], fill_len, r, q,
                                 slot_ids, seed=seed + lo)
        np.savez_compressed(sp + ".tmp.npz", G=Gc, P=Pc)
        os.replace(sp + ".tmp.npz", sp)
        vol.commit()
        G_parts.append(Gc); P_parts.append(Pc)
        print(f"[shard] {min(lo + chunk, m_fibers)}/{m_fibers} fibers", flush=True)
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
        t0 = time.time()
        got = dataset_gl_tau(bits_tr, A_search, tau, norm_m=int((~va_rows).sum()),
                             max_width=max_width,
                             n_search=fill_len * codes.shape[1])
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
    if os.path.exists(out):
        return out
    _budget("oracle-data", 2.0)
    started = time.time()
    stub_len = fill_len - 1 - p_back
    assert 0 <= stub_len < fill_len, f"p_back {p_back} out of range for fill {fill_len}"
    model, tok, q = _load_teacher()
    src = np.load(_table_path(fill_len, m_fibers, r=R_FILLS))       # reuse the fibers/slots
    PRE, slot_ids = src["PRE"], src["slot_ids"]
    if len(PRE) < m_fibers:
        raise RuntimeError(f"only {len(PRE)} fibers cached")
    PRE = PRE[:m_fibers]
    if stub_len > 0:                                               # shared stub, one draw
        stub, _ = _fill_and_label(model, PRE, stub_len, 1, q, slot_ids, seed=seed)
        PRE_ext = np.concatenate([PRE, stub], axis=1)
    else:
        PRE_ext = PRE
    w = fill_len - stub_len                                        # split token + newer
    G_toks, P = _fill_and_label(model, PRE_ext, w, g, q, slot_ids, seed=seed + 7)
    split_tok = G_toks[:, 0].astype(np.int64)                     # the split coordinate
    fiber_gid = np.repeat(np.arange(m_fibers), g)
    os.makedirs(ROOT, exist_ok=True)
    np.savez_compressed(out.replace(".npz", ".tmp.npz"), split_tok=split_tok, P=P,
                        fiber_gid=fiber_gid, slot_ids=slot_ids, q=q)
    os.replace(out.replace(".npz", ".tmp.npz"), out)
    vol.commit()
    _record("oracle-data", started, {"m_fibers": m_fibers, "g": g, "p_back": p_back})
    print(f"saved {out}", flush=True)
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
        psi, norm = oracle_deg1_psi(sb[~va], F[~va], fiber_gid[~va])
        # unpaired exact marginal |E[F chi_b]|^2 on the same rows (sanity twin)
        Ftr = F[~va]; chitr = 1.0 - 2.0 * sb[~va].astype(np.float32)
        marg = ((chitr.T @ Ftr) / len(Ftr))
        marg_norm = np.sqrt((marg.astype(np.float64) ** 2).sum(1))
        order = np.argsort(-norm)
        # rank agreement of the paired estimator with the exact marginal
        rho = float(np.corrcoef(_rankdata(norm), _rankdata(marg_norm))[0, 1])
        cert = np.flatnonzero(norm >= 0.01)
        # sparse deg-1 model on oracle-certified bits (fiber-disjoint eval)
        Ptr, Pva = P[~va], P[va]
        Xtr = 1.0 - 2.0 * sb[~va][:, cert].astype(np.float32)
        Xva = 1.0 - 2.0 * sb[va][:, cert].astype(np.float32)
        ntr = np.ones(len(Xtr)); nva = np.ones(len(Xva))
        fit = fit_softmax_slots(Xtr, Ptr, ntr, Xva, Pva, nva, steps=3000, lr=0.01)
        base = fit_softmax_slots(Xtr[:, :0], Ptr, ntr, Xva[:, :0], Pva, nva, steps=1)
        summary["encodings"][name] = {
            "top_norms": norm[order[:15]].round(5).tolist(),
            "top_bits": [int(b) for b in order[:15]],
            "n_ge_0.01": int((norm >= 0.01).sum()),
            "n_ge_0.02": int((norm >= 0.02).sum()),
            "total_mass": float((psi[psi > 0]).sum()),
            "max_norm": float(norm.max()),
            "rank_corr_paired_vs_marginal": rho,
            "n_certified": int(len(cert)),
            "sparse_val_kl": fit["val_kl"], "sparse_improved": fit["improved"],
            "unigram_val_kl": base["val_kl"]}
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
        t0 = time.time()
        got = dataset_gl_csamp(bits_tr, F, fib_tr, tau, max_width=max_width,
                               n_search=fill_len * codes.shape[1])
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
         g: int = 24, p_back: int = 0):
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
    if stage in ("search", "all"):
        if encoding == "parallel":
            # one container per encoding; summaries land as separate files
            calls = [search.spawn(tau, m_fibers, r, 0.15, fill_len, max_width, e)
                     for e in ("lsh", "ctrl", "idbits")]
            for c in calls:
                c.get()
        else:
            search.remote(tau, m_fibers, r, 0.15, fill_len, max_width, encoding)
