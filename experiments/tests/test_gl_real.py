"""Regression tests for the GL search + reconstruction path and its bit convention.

Guards the two bugs fixed in commit 03d5ee5: (1) the 2^p vs reversed-C-order mask
convention between gl_torch/_fwht and the design builder, and (2) that GL + direct
reconstruction actually recovers a planted sparse Walsh function.  Also pins the
SAMP-vs-CSAMP blindness on a planted high-order (parity) coefficient.
"""

import numpy as np

from fda_exp.gl_real import _design_2p, _fwht, _recon
from fda_exp.gl_torch import gl_search_torch


def _full_cube(n):
    X = ((np.arange(1 << n)[:, None] >> np.arange(n)) & 1).astype(np.int64)   # X[i,p] = bit p of i
    idx = np.arange(1 << n, dtype=np.int64)                                   # encode(X) == i
    s = 1 - 2 * X
    def chi(mask):
        c = np.ones(len(X))
        for p in range(n):
            if (mask >> p) & 1:
                c *= s[:, p]
        return c
    return X, idx, chi


def test_gl_recovers_sparse_walsh_and_convention():
    """f = 1.0*chi_{0,2} + 0.7*chi_{3}: GL must recover masks {5, 8} (2^p convention:
    positions {0,2} -> 2^0+2^2 = 5, NOT the reversed mask), and the direct
    reconstruction from the recovered support must match f."""
    n = 8
    X, idx, chi = _full_cube(n)
    f = 1.0 * chi(0b101) + 0.7 * chi(0b1000)              # deg-2 at {0,2} (mask 5) + deg-1 at {3} (mask 8)
    r = gl_search_torch(idx, f, n, tau=1.2, n_exp=20000, device="cpu", mode="csamp", seed=0)
    rec = set(r["L"]) - {0}
    assert 5 in rec, f"convention broken: expected mask 5 for positions {{0,2}}, got {sorted(rec)}"
    assert 8 in rec
    fhat = _fwht(f) / (1 << n)                            # f is indexed by idx=arange, so f IS the value vector
    masks = np.array(sorted(rec))
    g = _recon(X, masks, fhat, 0.0, n)
    corr = np.corrcoef(g, f)[0, 1]
    assert corr > 0.98


def test_design_matches_fwht_convention():
    """A single Householder/Walsh character built by _design_2p at mask m equals the
    row the FWHT would assign to m (both 2^p): reconstructing chi_m from its own
    coefficient returns chi_m."""
    n = 7
    X, idx, chi = _full_cube(n)
    for m in (0b0001, 0b0101, 0b1010, 0b1111, 0b1000001):
        f = chi(m)
        fhat = _fwht(f) / (1 << n)
        assert abs(fhat[m] - 1.0) < 1e-9                 # all mass on mask m
        assert abs(fhat).sum() - 1.0 < 1e-9
        g = _recon(X, np.array([m]), fhat, 0.0, n)
        assert np.corrcoef(g, f)[0, 1] > 0.999


def test_blindness_csamp_vs_samp_on_parity():
    """f = full parity (a pure degree-n coefficient): every prefix has zero individual
    mass, so SAMP (context-blind pairs) prunes early and never reaches it, while CSAMP
    (context-conditioned) follows the heavy path and recovers it."""
    n = 8
    X, idx, chi = _full_cube(n)
    f = chi((1 << n) - 1)                                 # mask 255, degree 8
    rc = gl_search_torch(idx, f, n, tau=1.0, n_exp=40000, device="cpu", mode="csamp", seed=0)
    rs = gl_search_torch(idx, f, n, tau=1.0, n_exp=40000, device="cpu", mode="samp", seed=0)
    assert (1 << n) - 1 in set(rc["L"] or []), "CSAMP should recover the high-order parity coefficient"
    assert (1 << n) - 1 not in set(rs["L"] or []), "SAMP should be BLIND to the high-order coefficient"
