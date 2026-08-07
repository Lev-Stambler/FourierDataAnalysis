"""Enumerated profiles (PLAN §4.2): brute-force Fourier ground truth.

The next-token function f: context -> Delta(q) of a family depends only on its
support coordinates (family.support_positions()).  Its vector Fourier
coefficients under the uniform context measure vanish off the support subspace,
so the full profile is obtained by one FFT over the q^s support assignments:

    fhat(alpha) = E_ctx[f(ctx) conj(chi_alpha(ctx))],   supp(alpha) subset support
    W^k         = sum_{|alpha|=k} ||fhat(alpha)||_2^2.

Cost is q^s, not q^L.
"""

from __future__ import annotations

import numpy as np

from ..families.base import Family

#: refuse support subspaces larger than this (S1 tier stays cheap)
ENUMERATE_CAP = 1 << 24


def enumerated_profile(family: Family, return_coeffs: bool = False):
    """Brute-force level weights of the family's next-token function.

    Returns W (length L+1); with return_coeffs also the coefficient table of
    shape (q,)*s + (q,) over the support coordinates.
    """
    support = list(family.support_positions())
    s = len(support)
    q, L = family.q, family.L
    if s == 0:
        # constant law: all mass at level 0
        W = np.zeros(L + 1, dtype=np.float64)
        val = family.next_token_dist(np.zeros(L, dtype=np.int64))
        W[0] = float(np.sum(np.abs(val) ** 2))
        if return_coeffs:
            return W, val.reshape((1,) * 0 + (q,))
        return W
    if q**s > ENUMERATE_CAP:
        raise ValueError(f"support subspace too large: q^s = {q**s} > cap {ENUMERATE_CAP}")

    # Build the value table f(y) for all support assignments y in Z_q^s.
    ctx = np.zeros(L, dtype=np.int64)
    table = np.empty((q,) * s + (q,), dtype=np.float64)
    for flat in range(q**s):
        digits = np.array(np.unravel_index(flat, (q,) * s))
        for k, pos in enumerate(support):
            ctx[pos] = digits[k]
        table[tuple(digits)] = family.next_token_dist(ctx)

    # Vector FFT along the s support axes; normalize by q^s (uniform measure).
    coeffs = np.fft.fftn(table, axes=range(s)) / (q**s)

    # Level weights: degrees are taken over the support multi-index; coordinates
    # outside the support carry alpha=0 and contribute nothing to the degree.
    idx = np.indices((q,) * s).reshape(s, -1)
    deg = np.count_nonzero(idx, axis=0)
    mass = (np.abs(coeffs.reshape(-1, q)) ** 2).sum(axis=1)
    W = np.zeros(L + 1, dtype=np.float64)
    np.add.at(W, deg, mass)

    if return_coeffs:
        return W, coeffs
    return W
