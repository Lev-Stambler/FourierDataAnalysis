"""Categorical ("q-ary") Goldreich-Levin over a dataset, in the HOUSEHOLDER basis.

Generalizes the binary Walsh GL (`gl_torch.gl_search_torch`) to a V-ary alphabet: the tree
decides one position's Householder contrast per level (branching factor V).  Characters
chi_alpha(x)=prod_p Psi[alpha_p,x_p], alpha in [V]^w, degree=#{p:alpha_p!=0}; dataset
coefficient f_hat_D(alpha)=E_{x~D}[f chi_alpha].

Bucket weight (THE FIX).  The GL tree needs Completeness -- a heavy leaf keeps every ancestor
bucket heavy -- so that pruning never drops a heavy character.  The *conditional* bucket weight
Psi(S|J)=E_z[(E_{x|z}[f chi_S])^2] only satisfies Completeness when |chi|=1 pointwise (the +/-1
Walsh property); for the REAL Householder characters (V>=3) on a non-uniform dataset it FAILS
(E_z[chi_U(z)^2] != 1), so the search silently drops heavy characters.  We instead use the true
Parseval bucket sum

    W(S|J_k) = sum_{U over the suffix} f_hat_D(S u U)^2
             = (V^{w-kk}/m^2) * sum_z ( sum_{x in group z} f(x) chi_S(x_{0..k}) )^2 ,   kk = k+1,

with z = the suffix positions kk..w-1.  W >= f_hat_D(S u U)^2 automatically (one term of a sum
of squares) for ANY orthonormal product basis, so Completeness/Monotonicity/Level-mass all hold
for Householder; at the last level (kk=w) the single empty-suffix group gives W = f_hat_D(S)^2
(subsuming the old exact-leaf special case).  Since we hold the whole dataset, W is computed
EXACTLY by a group-by on the suffix -- no sampling, no self-pair diagonal correction needed.
mode='samp' keeps a context-blind estimate (independent partner) to demonstrate blindness.

Convention: mixed-radix LSD throughout -- a character CODE = sum_p a_p * V^p, digit
a_p = (code // V^p) % V, degree = #nonzero digits.  (Kept internally consistent; do NOT mix
with householder.qary_spectrum's C-order flat indices -- decode to a multi-index to compare.)
"""

from __future__ import annotations

import math

import numpy as np
import torch

from .gl_torch import get_device


def _encode_qary(C, V):
    """(m,w) ids 0..V-1 -> mixed-radix codes sum_p C[:,p]*V^p (position 0 least significant)."""
    w = C.shape[1]
    return (C.astype(np.int64) * (V ** np.arange(w))).sum(1)


def _digits(codes, V, w):
    """codes (K,) -> (K,w) digits, digit p = (code // V^p) % V."""
    codes = np.asarray(codes, dtype=np.int64)
    return (codes[:, None] // (V ** np.arange(w))[None, :]) % V


def degree_of_codes(codes, V, w):
    return (_digits(codes, V, w) != 0).sum(1)


def _qary_psi_batch(codes_dev, xdig_rr, xdig_rp, f_rr, f_rp, Psi_dev, V, w, chunk=1024):
    """Context-BLIND bucket estimate (mode='samp' only): Psi ~ mean over independent pairs of
    f chi_S(x) * f chi_S(x').  Used to demonstrate that samples-only sampling is blind to
    suffix/high-order structure; the real (mode='csamp') search uses the exact W group-by below.
    xdig_rr / xdig_rp: (w, E) token digits of the sampled rows.  Returns (L,) tensor."""
    L = codes_dev.shape[0]
    E = xdig_rr.shape[1]
    chunk = max(1, min(chunk, 40_000_000 // max(E, 1)))
    Vp = torch.tensor(V ** np.arange(w), dtype=torch.int64, device=codes_dev.device)
    out = torch.empty(L, dtype=torch.float32, device=codes_dev.device)
    for i in range(0, L, chunk):
        cc = codes_dev[i:i + chunk]
        adig = (cc[:, None] // Vp[None, :]) % V                      # (c, w) code digits
        chi_rr = torch.ones((cc.shape[0], E), device=codes_dev.device)
        chi_rp = torch.ones((cc.shape[0], E), device=codes_dev.device)
        for p in range(w):
            rowsel = Psi_dev[adig[:, p]]                             # (c, V); psi_{a}(.) rows; row 0 == 1
            chi_rr = chi_rr * rowsel[:, xdig_rr[p]]                  # (c, E)
            chi_rp = chi_rp * rowsel[:, xdig_rp[p]]
        out[i:i + chunk] = ((f_rr[None, :] * chi_rr) * (f_rp[None, :] * chi_rp)).mean(dim=1)
    return out


def _qary_bucket_Q(codes_dev, xdig_all, f_all, gid, ng, Psi_dev, V, kk, chunk=1024):
    """Reduced Parseval bucket statistic for every child code S (a prefix character on positions
    0..kk-1):  Q(S) = sum_z ( sum_{x in group z} f(x) chi_S(x_{0..kk-1}) )^2 .  The bucket weight is
    W(S) = (V^{w-kk}/m^2) * Q(S); the (level-constant) prefactor is applied in log-space by the
    caller.  gid: (m,) row -> suffix-group index in [0, ng); xdig_all: (w, m) token digits.
    Returns (L,) float64 numpy array (exact group-by, no sampling)."""
    L = codes_dev.shape[0]
    M = xdig_all.shape[1]
    chunk = max(1, min(L, 60_000_000 // max(M + ng, 1)))         # bound chi (c x M) + G (c x ng) tensors
    Vp = torch.tensor(V ** np.arange(kk), dtype=torch.int64, device=codes_dev.device)   # prefix only
    out = torch.empty(L, dtype=torch.float64, device=codes_dev.device)
    for i in range(0, L, chunk):
        cc = codes_dev[i:i + chunk]
        adig = (cc[:, None] // Vp[None, :]) % V                      # (c, kk) prefix code digits
        chi = torch.ones((cc.shape[0], M), device=codes_dev.device)
        for p in range(kk):
            rowsel = Psi_dev[adig[:, p]]                             # (c, V)
            chi = chi * rowsel[:, xdig_all[p]]                       # (c, M)
        g = (f_all[None, :] * chi).double()                         # (c, M)  f(x) chi_S(x_prefix)
        G = torch.zeros((cc.shape[0], ng), dtype=torch.float64, device=codes_dev.device)
        G.index_add_(1, gid, g)                                     # (c, ng)  per-suffix-group sums
        out[i:i + chunk] = (G * G).sum(dim=1)
    return out.cpu().numpy()


def qary_gl_search(idx_np, f_np, w, V, Psi, tau, n_exp=20000, device=None,
                   mode="csamp", seed=0, max_width=200_000, norm_m=None):
    """Run categorical GL; return recovered heavy character codes + per-level widths.

    idx_np: mixed-radix codes of the contexts (`_encode_qary`).  A leaf is heavy iff
    f_hat_D(alpha)^2 >= tau^2/4, i.e. |f_hat_D(alpha)| >= tau/2.

    norm_m: Parseval normalization (defaults to len(idx_np)).  Set it to the ORIGINAL row count when
    `idx_np` holds frequency-collapsed DISTINCT contexts and `f_np` holds the per-context weighted sum
    `sum_{x in c} f(x)` -- then W and the leaf coefficient are exact for the full empirical measure
    while the group-by cost scales with the (much smaller) number of distinct contexts.

    mode='csamp' (real search): bucket weight is the exact Parseval sum
        W(S|J_k) = (V^{w-kk}/m^2) * sum_z ( sum_{x in group z} f(x) chi_S(x_{0..k}) )^2 ,  kk=k+1,
    a group-by on the suffix z = idx//V^kk (see module docstring).  W >= f_hat_D(S u U)^2 for every
    descendant, so a heavy leaf keeps every ancestor bucket >= its coefficient^2 -- Completeness
    holds for the real Householder basis, unlike the conditional weight Psi=E_z[vbar^2] (which needs
    |chi|=1).  At the last level W = f_hat_D(S)^2 exactly, so the leaf test is exact for free.

    mode='samp' (blindness demo): context-blind estimate from independent partners -- recovers
    nothing beyond low order, illustrating why context (suffix) conditioning is essential.
    """
    device = device or get_device()
    m = len(idx_np)
    M_norm = m if norm_m is None else int(norm_m)                   # #rows for Parseval (orig m if collapsed)
    thresh = tau * tau / 4.0
    rng = np.random.default_rng(seed)
    idx_np = idx_np.astype(np.int64)
    f_np = f_np.astype(np.float32)
    Vp = V ** np.arange(w)
    Psi_dev = torch.tensor(Psi, dtype=torch.float32, device=device)
    xdig_all = torch.tensor((idx_np[None, :] // Vp[:, None]) % V, dtype=torch.int64, device=device)
    f_all = torch.tensor(f_np, dtype=torch.float32, device=device)
    log_thresh = math.log(thresh) + 2.0 * math.log(M_norm)          # W >= thresh  <=>  log Q >= this - (w-kk)logV

    live = np.array([0], dtype=np.int64)                            # empty character (constant)
    widths, experiments = [], 0
    for k in range(w):
        kk = k + 1
        children = np.unique((live[:, None] + (np.arange(V) * (V ** k))[None, :]).ravel())
        codes_dev = torch.tensor(children, dtype=torch.int64, device=device)
        if mode == "csamp":
            # exact Parseval bucket weight via a group-by on the suffix z = idx // V^kk
            z = idx_np // (V ** kk)
            _, inv = np.unique(z, return_inverse=True)
            ng = int(inv.max()) + 1
            gid = torch.tensor(inv, dtype=torch.int64, device=device)
            Q = _qary_bucket_Q(codes_dev, xdig_all, f_all, gid, ng, Psi_dev, V, kk)
            # keep iff W = (V^{w-kk}/m^2) Q >= thresh; compare in log-space (V^{w-kk} overflows).
            log_lhs = log_thresh - (w - kk) * math.log(V)           # -> -inf at deep no-repeat levels
            with np.errstate(divide="ignore"):
                keep = np.log(Q) >= log_lhs
            live = children[keep]
        else:
            # context-blind baseline (samples only): independent partner -> blind Psi estimate
            rr = rng.integers(0, m, size=n_exp)
            rp = rng.integers(0, m, size=n_exp)
            experiments += n_exp
            xdig_rr = torch.tensor((idx_np[rr][None, :] // Vp[:, None]) % V, dtype=torch.int64, device=device)
            xdig_rp = torch.tensor((idx_np[rp][None, :] // Vp[:, None]) % V, dtype=torch.int64, device=device)
            f_rr = torch.tensor(f_np[rr], dtype=torch.float32, device=device)
            f_rp = torch.tensor(f_np[rp], dtype=torch.float32, device=device)
            psi = _qary_psi_batch(codes_dev, xdig_rr, xdig_rp, f_rr, f_rp, Psi_dev, V, w).cpu().numpy()
            live = children[psi >= thresh]
        widths.append(int(len(live)))
        if len(live) > max_width:
            return dict(status="blowup", level=kk, width=int(len(live)),
                        widths=widths, experiments=experiments, L=None)
        if len(live) == 0:
            break
    return dict(status="ok", L=sorted(int(s) for s in live), widths=widths, experiments=experiments)


def _char_columns(C, codes, V, Psi, w):
    """chi_code(x) for each code, as columns (m, len(codes)).  Vectorized over the w
    positions (not over codes): cols[i,j] = prod_p Psi[digit_p(code_j), C[i,p]]."""
    codes = np.asarray(codes, dtype=np.int64)
    dig = _digits(codes, V, w)                                   # (K, w) code digits
    cols = np.ones((len(C), len(codes)))
    for p in range(w):
        cols *= Psi[dig[:, p]][:, C[:, p]].T                     # (K,V)->(K,m)->(m,K)
    return cols


def _chunk_for(m):
    return max(1, 40_000_000 // max(m, 1))                       # keep (m, chunk) <= 40M floats


def qary_coeffs_at(C, f, codes, V, Psi, w):
    """f_hat_D(alpha) = E_D[f * chi_alpha] for each code (no enumeration of V^w)."""
    codes = np.asarray(codes, dtype=np.int64)
    if len(codes) == 0:
        return np.empty(0)
    f = np.asarray(f, float)
    out = np.empty(len(codes))
    step = _chunk_for(len(C))
    for i in range(0, len(codes), step):
        cols = _char_columns(C, codes[i:i + step], V, Psi, w)
        out[i:i + step] = (cols * f[:, None]).mean(0)
    return out


def qary_recon(C, codes, coeffs, V, Psi, w, cd_inv=1.0):
    """g(x) = cd_inv * sum_alpha coeff_alpha chi_alpha(x)  (cd_inv = m/V^w recovers f on support)."""
    codes = np.asarray(codes, dtype=np.int64)
    if len(codes) == 0:
        return np.zeros(len(C))
    coeffs = np.asarray(coeffs, float)
    out = np.zeros(len(C))
    step = _chunk_for(len(C))
    for i in range(0, len(codes), step):
        out += _char_columns(C, codes[i:i + step], V, Psi, w) @ coeffs[i:i + step]
    return cd_inv * out
