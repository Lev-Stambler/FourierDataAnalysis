import numpy as np

from fda_exp.householder import (
    analyze_qary,
    householder_basis,
    qary_spectrum,
    walsh4,
)


def test_orthonormal_householder():
    for V in [2, 3, 4, 5, 8, 20]:
        Psi = householder_basis(V)
        assert np.allclose(Psi @ Psi.T / V, np.eye(V), atol=1e-10)   # orthonormal over uniform
        assert np.allclose(Psi[0], 1.0)                              # row 0 is the constant


def test_orthonormal_walsh():
    Psi = walsh4()
    assert np.allclose(Psi @ Psi.T / 4, np.eye(4))
    assert np.allclose(Psi[0], 1.0)


def test_parseval_mass_identity():
    rng = np.random.default_rng(0)
    V, w = 4, 5
    Cd = np.unique(rng.integers(0, V, size=(600, w)), axis=0)
    f = rng.standard_normal(len(Cd))
    for Psi in (householder_basis(V), walsh4()):
        fhat = qary_spectrum(Cd, f, Psi)
        CD = V ** w / len(Cd)
        assert np.isclose((fhat ** 2).sum(), CD * np.mean(f ** 2), rtol=1e-9)


def test_junta_is_one_sparse():
    # f = a single character chi_alpha -> top-1 coefficient reconstructs it (corr 1)
    rng = np.random.default_rng(1)
    V, w = 4, 6
    Cd = np.unique(rng.integers(0, V, size=(4000, w)), axis=0)
    Psi = householder_basis(V)
    alpha = np.array([1, 0, 2, 0, 3, 0])                    # contrasts at positions 0,2,4
    f = np.prod([Psi[alpha[p], Cd[:, p]] for p in range(w)], axis=0)
    tr = np.arange(0, len(Cd), 2)
    te = np.arange(1, len(Cd), 2)
    r = analyze_qary(Cd[tr], f[tr], Cd[te], f[te], Psi, ks=(1, 4, 16))
    assert r["corrs"][0] > 0.98                             # recovered at K=1
