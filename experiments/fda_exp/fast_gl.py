"""GPU-accelerated ONE-SHOT MULTICLASS Walsh CSAMP search + batched coefficients.

The slow/degenerate per-token pipeline (128 sequential exact-W searches, float64 => CPU-only on Apple
MPS, threshold-blowup that recovers nothing on dense data) is replaced by a SINGLE search over the whole
next-token distribution:

  W_multi(S) = sum_t sum_U f̂_t(S u U)^2   (total Fourier mass of the next-token indicator VECTOR
                                            extending prefix S = characters predictive of the distribution)

computed in float32 (MPS/CUDA) via a suffix group-by over the collapsed distinct contexts, with:
  - the Walsh character maintained INCREMENTALLY  chi_child = chi_parent * w_k  (w_k = +/-1 per bit),
    so no Psi materialization and no int64 popcount on GPU;
  - FAST ORDERED search: keep the top-`max_width` children by W_multi each level (torch.topk), so the
    frontier is bounded (never blows up) and always retains the heaviest -> recovers the top characters
    even on dense data;
  - ONE-SHOT coefficients: once the shared code set is found, all per-token f̂_t(code) come from a single
    GEMM  coeffs (V,K) = ((2*counts - n).T @ char_matrix) / m  (computed exactly in float64 on CPU for the
    survivor codes only).

Consumes `hf_data.collapse_contexts` output (Cd, counts (D,V), n (D,), m).  Codes are base-V LSD codes
(== the packed-bit masks), so `token_degree`/`_char_columns`/`qary_recon` apply unchanged.
"""

from __future__ import annotations

import numpy as np
import torch

from .gl_torch import get_device
from .qary_gl import _encode_qary


def _bucket_W(chi, A, inv_t, ng, cls_chunk, mem_budget):
    """W(S) = sum_t sum_z (sum_{c in group z} chi_S(c) A[c,t])^2 for every row S of `chi` (L,D).
    A: (D,V) per-class weights; inv_t: (D,) suffix-group id in [0,ng).  float32, chunked over codes+classes."""
    L, D = chi.shape
    V = A.shape[1]
    W = torch.zeros(L, dtype=torch.float32, device=chi.device)
    cc = max(1, int(mem_budget // (D * cls_chunk)))               # bound the (cc, D, cls_chunk) tensor
    for i in range(0, L, cc):
        cbi = chi[i:i + cc]                                       # (c, D)
        wc = torch.zeros(cbi.shape[0], dtype=torch.float32, device=chi.device)
        for j in range(0, V, cls_chunk):
            Aj = A[:, j:j + cls_chunk]                            # (D, cj)
            g = cbi[:, :, None] * Aj[None, :, :]                  # (c, D, cj)
            G = torch.zeros(cbi.shape[0], ng, Aj.shape[1], dtype=torch.float32, device=chi.device)
            G.index_add_(1, inv_t, g)                            # (c, ng, cj) per-suffix-group sums
            wc = wc + (G * G).sum(dim=(1, 2))
        W[i:i + cc] = wc
    return W


def multiclass_search(idx, A, n_bits, max_width=6000, device=None, cls_chunk=64, mem_budget=1.2e8):
    """Top-K multiclass Walsh CSAMP over distinct contexts.

    idx: (D,) int64 packed contexts (`_encode_qary(Cd, V)`); A: (D,V) float32 per-class weights
    (`2*counts - n`); n_bits = w*log2(V).  Descends the binary Walsh tree, keeping the `max_width`
    heaviest prefixes (by W_multi) each level -> bounded frontier.  Returns (codes (K,) int64 base-V/bit
    masks, widths list)."""
    device = device or get_device()
    idx = np.asarray(idx, dtype=np.int64)
    D = len(idx)
    A_t = torch.tensor(np.asarray(A, dtype=np.float32), device=device)          # (D, V)
    idxbits = torch.tensor(1.0 - 2.0 * ((idx[:, None] >> np.arange(n_bits)) & 1), dtype=torch.float32,
                           device=device)                                       # (D, n_bits): w_b = +/-1
    live_codes = np.array([0], dtype=np.int64)
    live_chi = torch.ones((1, D), dtype=torch.float32, device=device)           # chi of the empty character
    widths = []
    for k in range(n_bits):
        kk = k + 1
        wk = idxbits[:, k]                                                       # (D,)  bit-k Walsh value
        # suffix groups z = high bits kk..n-1 (grouped on CPU, cheap 1D unique)
        _, inv = np.unique(idx >> kk, return_inverse=True)
        ng = int(inv.max()) + 1
        inv_t = torch.tensor(inv, dtype=torch.int64, device=device)
        # children = {bit k NOT in S : chi unchanged} U {bit k in S : chi * w_k}
        chi_add = live_chi * wk[None, :]
        W = torch.cat([_bucket_W(live_chi, A_t, inv_t, ng, cls_chunk, mem_budget),
                       _bucket_W(chi_add, A_t, inv_t, ng, cls_chunk, mem_budget)]).cpu().numpy()
        codes = np.concatenate([live_codes, live_codes | (np.int64(1) << k)])
        chi_all = torch.cat([live_chi, chi_add], dim=0)                          # (2L, D)
        if len(codes) > max_width:
            keep = np.argpartition(-W, max_width)[:max_width]
            live_codes, live_chi = codes[keep], chi_all[torch.tensor(keep, device=device)]
        else:
            live_codes, live_chi = codes, chi_all
        widths.append(int(len(live_codes)))
    return live_codes[live_codes != 0], widths


def char_matrix(idx, codes, n_bits, device=None):
    """(D, K) float32 Walsh characters chi_code(idx) = prod over set bits of w_b, built incrementally
    on GPU (no Psi, no popcount).  idx: (D,) packed; codes: (K,) bit masks."""
    device = device or get_device()
    idx = np.asarray(idx, dtype=np.int64); codes = np.asarray(codes, dtype=np.int64)
    idxbits = torch.tensor(1.0 - 2.0 * ((idx[:, None] >> np.arange(n_bits)) & 1), dtype=torch.float32,
                           device=device)                                       # (D, n_bits)
    out = torch.ones((len(idx), len(codes)), dtype=torch.float32, device=device)
    for b in range(n_bits):
        sel = (codes >> b) & 1                                                   # codes with bit b set
        if sel.any():
            m = torch.tensor(sel.astype(bool), device=device)
            out[:, m] = out[:, m] * idxbits[:, b][:, None]
    return out


def coeffs_all(counts, n_ctx, Cd, codes, V, w, m):
    """One-shot exact per-token coefficients f̂_t(code) = E_D[f_t chi], f_t = 2*1[y=t]-1, for ALL tokens
    at once (float64, CPU, survivor codes only): coeffs (V,K) = ((2*counts - n).T @ char_matrix) / m.
    Also returns const (V,) = f̂_t(∅) = 2*count_t/m - 1.  Uses the numpy `_char_columns` for exactness."""
    from .qary_gl import _char_columns
    from .householder import hadamard_basis
    if len(codes) == 0:
        return np.zeros((V, 0)), 2.0 * counts.sum(0) / m - 1.0
    chi = _char_columns(Cd, codes, V, hadamard_basis(V), w)                      # (D, K) float64
    A = (2.0 * counts - n_ctx[:, None])                                          # (D, V)
    coeffs = (A.T @ chi) / m                                                     # (V, K)
    const = 2.0 * counts.sum(0) / m - 1.0                                        # (V,)  f̂_t(∅)
    return coeffs, const
