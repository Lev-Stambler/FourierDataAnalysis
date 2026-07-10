"""Correctness tests for the categorical (Householder) GL search."""
import numpy as np

from fda_exp.householder import householder_basis, qary_spectrum
from fda_exp.qary_gl import (
    _digits,
    _encode_qary,
    qary_coeffs_at,
    qary_gl_search,
    qary_recon,
)


def _cube(V, w):
    g = np.array(np.meshgrid(*([np.arange(V)] * w), indexing="ij")).reshape(w, -1).T
    return g.astype(np.int64)


def _char(C, alpha, Psi):
    chi = np.ones(len(C))
    for p, a in enumerate(alpha):
        if a:
            chi = chi * Psi[a, C[:, p]]
    return chi


def _code(alpha, V):
    return int(sum(a * V ** p for p, a in enumerate(alpha)))


def _msd(code, V, w):                                        # LSD code -> C-order flat (qary_spectrum)
    return int(np.ravel_multi_index(_digits([code], V, w)[0], (V,) * w))


def test_recovers_single_character_and_convention():
    V, w = 4, 6
    C, Psi = _cube(V, w), householder_basis(V)
    alpha = [1, 0, 2, 0, 0, 0]                                # degree-2 Householder char at positions 0,2
    f = _char(C, alpha, Psi)
    r = qary_gl_search(_encode_qary(C, V), f, w, V, Psi, tau=1.0, n_exp=20000, device="cpu", mode="csamp")
    code = _code(alpha, V)
    assert code in set(r["L"]), f"expected code {code}, got {r['L'][:10]}"
    assert _digits([code], V, w)[0].tolist() == alpha        # convention: code decodes to alpha
    g = qary_recon(C, np.array([code]), qary_coeffs_at(C, f, [code], V, Psi, w), V, Psi, w)
    assert np.corrcoef(g, f)[0, 1] > 0.98


def test_recall_vs_bruteforce_and_coeffs_match():
    V, w = 4, 6
    C, Psi = _cube(V, w), householder_basis(V)
    alphas = [[2, 0, 0, 0, 0, 0], [1, 0, 3, 0, 0, 0], [1, 2, 0, 3, 0, 0]]   # deg 1,2,3 (each |f_hat|=1)
    f = sum(_char(C, a, Psi) for a in alphas)
    fhat = qary_spectrum(C, f, Psi)                          # exact spectrum (C-order)
    tau = 1.0                                                # leaf kept iff |f_hat| >= 0.5 -> the 3 planted
    brute = set(int(s) for s in np.where(fhat ** 2 >= (tau / 2) ** 2)[0] if s != 0)
    r = qary_gl_search(_encode_qary(C, V), f, w, V, Psi, tau=tau, n_exp=40000, device="cpu", mode="csamp")
    rec_msd = set(_msd(c, V, w) for c in r["L"] if c != 0)
    assert len(rec_msd & brute) / max(len(brute), 1) >= 0.8
    for c in [c for c in r["L"] if c != 0][:20]:             # qary_coeffs_at == qary_spectrum
        assert abs(qary_coeffs_at(C, f, [c], V, Psi, w)[0] - fhat[_msd(c, V, w)]) < 1e-6


def test_blindness_csamp_vs_samp_full_degree():
    V, w = 4, 5
    C, Psi = _cube(V, w), householder_basis(V)
    alpha = [1, 2, 3, 1, 2]                                   # full-degree Householder character
    f = _char(C, alpha, Psi)
    idx = _encode_qary(C, V)
    code = _code(alpha, V)
    rc = qary_gl_search(idx, f, w, V, Psi, tau=1.0, n_exp=40000, device="cpu", mode="csamp")
    rs = qary_gl_search(idx, f, w, V, Psi, tau=1.0, n_exp=40000, device="cpu", mode="samp")
    assert code in set(rc["L"] or []), "CSAMP should recover the full-degree character"
    assert code not in set(rs["L"] or []), "SAMP should be BLIND to it"
