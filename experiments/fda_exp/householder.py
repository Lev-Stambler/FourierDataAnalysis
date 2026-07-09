"""Categorical ("q-ary") Fourier over a product of per-position alphabets, with
an orthonormal per-position basis built by a Householder reflection.

For a vocab of size V (uniform base measure), we want an orthonormal function
basis {psi_0..psi_{V-1}} of L^2(V symbols): psi_0 = 1 (constant) and the rest
orthonormal contrasts.  Build Psi = sqrt(V) * H where H = I - 2 v v^T is the
Householder reflection sending e_0 -> 1/sqrt(V) (so row 0 of H is 1/sqrt(V),
i.e. psi_0 = 1).  Then (1/V) Psi Psi^T = I.

This is non-redundant (a genuine orthonormal basis over the alphabet -- NO
one-hot-binary subvariety aliasing) and works for ANY V (unlike the 2-bit
Walsh basis, which is the V=4 special case, provided here as `walsh4`).

The on-dataset spectrum is the tensor transform of the lift by Psi along each of
the w positions; a coefficient is fhat_D(alpha) = E_{x~D}[ f(x) * prod_p psi_{alpha_p}(x_p) ].
"""

from __future__ import annotations

import numpy as np


def householder_basis(V: int) -> np.ndarray:
    """Psi (V x V): rows are the orthonormal basis functions, psi_0 = constant."""
    u = np.ones(V) / np.sqrt(V)
    e0 = np.zeros(V); e0[0] = 1.0
    d = e0 - u
    nrm = np.linalg.norm(d)
    H = np.eye(V) if nrm < 1e-12 else np.eye(V) - 2.0 * np.outer(d / nrm, d / nrm)
    return np.sqrt(V) * H


def walsh4() -> np.ndarray:
    """The V=4 two-bit Walsh basis {1, chi_bit0, chi_bit1, chi_both} as a 4x4 Psi."""
    s = np.arange(4)
    return np.stack([
        np.ones(4),
        1 - 2 * (s & 1),
        1 - 2 * ((s >> 1) & 1),
        1 - 2 * ((s & 1) ^ ((s >> 1) & 1)),
    ]).astype(np.float64)


def qary_spectrum(Cd: np.ndarray, f: np.ndarray, Psi: np.ndarray) -> np.ndarray:
    """All V^w dataset coefficients of f over distinct contexts Cd (ids 0..V-1)."""
    V = Psi.shape[0]
    m, w = Cd.shape
    flat = np.ravel_multi_index(Cd.T, (V,) * w)           # C-order flat index
    g = np.zeros(V ** w, dtype=np.float64)
    np.add.at(g, flat, np.asarray(f, dtype=np.float64) / m)
    G = g.reshape((V,) * w)
    for p in range(w):                                    # apply Psi along each axis
        G = np.moveaxis(G, p, 0)
        G = np.tensordot(Psi, G, axes=([1], [0]))
        G = np.moveaxis(G, 0, p)
    return G.reshape(-1)


def qary_reconstruct(Cte: np.ndarray, order: np.ndarray, coeffs: np.ndarray,
                     Psi: np.ndarray) -> np.ndarray:
    """g(x) = sum_{alpha in order} coeff_alpha * prod_p psi_{alpha_p}(x_p) on test contexts."""
    V = Psi.shape[0]
    mte, w = Cte.shape
    alpha = np.array(np.unravel_index(np.asarray(order), (V,) * w)).T   # (K, w)
    g = np.zeros(mte)
    for k in range(len(order)):
        char = np.ones(mte)
        for p in range(w):
            char *= Psi[alpha[k, p], Cte[:, p]]
        g += coeffs[k] * char
    return g


def analyze_qary(Cd_tr, f_tr, Cd_te, f_te, Psi, ks):
    """Held-out top-K correlation curve (basis-fair sparsity measure)."""
    fhat = qary_spectrum(Cd_tr, f_tr, Psi)
    order = np.argsort(-np.abs(fhat))
    fc = f_te - f_te.mean()
    corrs = []
    for K in ks:
        sel = order[:K]
        g = qary_reconstruct(Cd_te, sel, fhat[sel], Psi)
        gc = g - g.mean()
        den = np.sqrt((gc ** 2).sum() * (fc ** 2).sum()) + 1e-12
        corrs.append(float((gc * fc).sum() / den))
    ceil = max(corrs)
    kstar = (next((ks[i] for i, c in enumerate(corrs) if c >= 0.95 * ceil), ks[-1])
             if ceil > 0.05 else ks[-1])
    return dict(corrs=corrs, ceil=ceil, kstar=kstar)
