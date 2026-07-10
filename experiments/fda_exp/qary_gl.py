"""Categorical ("q-ary") Goldreich-Levin over a dataset, in the HOUSEHOLDER basis.

Generalizes the binary Walsh GL (`gl_torch.gl_search_torch`) to a V-ary alphabet: the tree
decides one position's Householder contrast per level (branching factor V), and the CSAMP
bucket-weight estimator uses the real Householder characteristic function
`chi_J(x) = prod_{p<=k} Psi[alpha_p, x_p]` instead of a +/-1 Walsh parity.

Math: characters chi_alpha(x)=prod_p Psi[alpha_p,x_p], alpha in [V]^w, degree=#{p:alpha_p!=0};
coefficient f_hat_D(alpha)=E_{x~D}[f chi_alpha].  Node = prefix J=(alpha_1..alpha_k); bucket
weight Psi(J)=sum_{beta:beta_1:k=J} f_hat_D(beta)^2 = E_z[(E_{x_1:k|z}[f chi_J])^2] with z the
suffix (positions k+1..w).  CSAMP draws x~D and a partner x' sharing z; the leaf test reduces
to f_hat_D(alpha)^2 >= tau^2/4.  SAMP draws x' uniformly -> blind to high-order structure.

Convention: mixed-radix LSD throughout -- a character CODE = sum_p a_p * V^p, digit
a_p = (code // V^p) % V, degree = #nonzero digits.  (Kept internally consistent; do NOT mix
with householder.qary_spectrum's C-order flat indices -- decode to a multi-index to compare.)
"""

from __future__ import annotations

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


def sample_partners(gid, rr, rng):
    """CSAMP: for each sampled row rr[e], a random row sharing gid (the shared suffix)."""
    order = np.argsort(gid, kind="stable")
    uniq, start, counts = np.unique(gid[order], return_index=True, return_counts=True)
    grp = np.searchsorted(uniq, gid[rr])
    off = (rng.random(len(rr)) * counts[grp]).astype(np.int64)
    return order[start[grp] + off]


def _qary_psi_batch(codes_dev, xdig_rr, xdig_rp, f_rr, f_rp, Psi_dev, V, w, chunk=1024):
    """Psi(bucket) for every code, via Householder-character products over the shared pairs.
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


def _qary_exact_coeffs(codes_dev, xdig_all, f_all, Psi_dev, V, w, chunk=1024):
    """Exact f_hat_D(alpha) = mean_x f(x) chi_alpha(x) over ALL rows (noise-free leaf test).
    xdig_all: (w, m) token digits of every row; f_all: (m,).  Returns (L,) tensor."""
    L = codes_dev.shape[0]
    M = xdig_all.shape[1]
    chunk = max(1, min(chunk, 40_000_000 // max(M, 1)))
    Vp = torch.tensor(V ** np.arange(w), dtype=torch.int64, device=codes_dev.device)
    out = torch.empty(L, dtype=torch.float32, device=codes_dev.device)
    for i in range(0, L, chunk):
        cc = codes_dev[i:i + chunk]
        adig = (cc[:, None] // Vp[None, :]) % V                      # (c, w) code digits
        chi = torch.ones((cc.shape[0], M), device=codes_dev.device)
        for p in range(w):
            rowsel = Psi_dev[adig[:, p]]                             # (c, V)
            chi = chi * rowsel[:, xdig_all[p]]                       # (c, M)
        out[i:i + chunk] = (f_all[None, :] * chi).mean(dim=1)
    return out


def qary_gl_search(idx_np, f_np, w, V, Psi, tau, n_exp=20000, device=None,
                   mode="csamp", seed=0, max_width=200_000, exact_leaf=True):
    """Run categorical GL; return recovered heavy character codes + per-level widths.

    idx_np: mixed-radix codes of the contexts (`_encode_qary`).  mode='csamp' = real oracle
    (partner shares suffix idx//V^kk); mode='samp' = context-blind partner (blindness demo).
    Leaf kept iff f_hat_D(alpha)^2 >= tau^2/4, i.e. |f_hat_D(alpha)| >= tau/2.

    exact_leaf: at the final level the suffix is empty (partner is a uniform row), so the CSAMP
    estimate of the single leaf coefficient is just (mean over n_exp) with stddev ~1/sqrt(n_exp).
    Since we hold the whole dataset, compute that leaf coefficient EXACTLY over all m rows instead
    -- removes the noise that otherwise drops ~half the true heavy characters (recall ~0.5 -> ~1).
    """
    device = device or get_device()
    m = len(idx_np)
    thresh = tau * tau / 4.0
    rng = np.random.default_rng(seed)
    idx_np = idx_np.astype(np.int64)
    f_np = f_np.astype(np.float32)
    Vp = V ** np.arange(w)
    Psi_dev = torch.tensor(Psi, dtype=torch.float32, device=device)
    xdig_all = torch.tensor((idx_np[None, :] // Vp[:, None]) % V, dtype=torch.int64, device=device)
    f_all = torch.tensor(f_np, dtype=torch.float32, device=device)

    live = np.array([0], dtype=np.int64)                            # empty character (constant)
    widths, experiments = [], 0
    for k in range(w):
        kk = k + 1
        children = np.unique((live[:, None] + (np.arange(V) * (V ** k))[None, :]).ravel())
        codes_dev = torch.tensor(children, dtype=torch.int64, device=device)
        if exact_leaf and kk == w:                                  # last level: exact single coeff
            coef = _qary_exact_coeffs(codes_dev, xdig_all, f_all, Psi_dev, V, w).cpu().numpy()
            psi = coef * coef
        else:
            rr = rng.integers(0, m, size=n_exp)
            rp = sample_partners(idx_np // (V ** kk), rr, rng) if mode == "csamp" else rng.integers(0, m, size=n_exp)
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
