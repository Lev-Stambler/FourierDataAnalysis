import numpy as np

from fda_exp.audit import (
    _corr,
    _split3,
    degree_of,
    design_matrix,
    fourier_pick,
    logistic_auc,
    topk_fourier,
)
from fda_exp.householder import householder_basis


def _char(Cd, Psi, pos, cs):
    return np.prod([Psi[cs[i], Cd[:, pos[i]]] for i in range(len(pos))], axis=0)


def test_degree_of():
    V, w = 4, 3
    # alpha = (1,0,2) -> flat 1*16 + 0*4 + 2 = 18, degree 2
    assert degree_of([18], V, w).tolist() == [2]
    assert degree_of([0], V, w).tolist() == [0]            # constant
    assert degree_of([63], V, w).tolist() == [3]           # (3,3,3), full degree
    assert degree_of([16], V, w).tolist() == [1]           # (1,0,0)


def test_deg3_char_recovered_and_low_order_blind():
    """A single degree-3 Householder character: Fourier recovers it at K=1, and it
    is basis-exactly orthogonal to the degree-<=2 subspace, so ANY low-order linear
    model has ~0 correlation with it.  w=3 (V^w=8000 densely covered) => no aliasing."""
    rng = np.random.default_rng(0)
    V, w = 20, 3
    Cd = np.unique(rng.integers(0, V, size=(80000, w)), axis=0)
    Psi = householder_basis(V)
    f = _char(Cd, Psi, [0, 1, 2], [1, 1, 1])               # pure degree-3 character
    tr = np.arange(0, len(Cd), 2)
    te = np.arange(1, len(Cd), 2)
    corrs, order, _ = topk_fourier(Cd, f, tr, te, Psi, ks=(1, 4, 16))
    assert corrs[0] > 0.98                                 # Fourier top-1 nails it
    assert degree_of(order[:1], V, w)[0] == 3              # and it IS the degree-3 coefficient
    # low-order blindness: best degree-<=2 linear fit is ~orthogonal to a degree-3 char
    Phi_tr, Phi_te = design_matrix(Cd[tr], Psi, 2), design_matrix(Cd[te], Psi, 2)
    coef, *_ = np.linalg.lstsq(Phi_tr, f[tr], rcond=None)
    assert abs(_corr(Phi_te @ coef, f[te])) < 0.2


def test_deg1_char_recovered_at_k1():
    """Sanity: a single degree-1 character is 1-sparse -> Fourier corr 1 at K=1."""
    rng = np.random.default_rng(2)
    V, w = 20, 3
    Cd = np.unique(rng.integers(0, V, size=(80000, w)), axis=0)
    Psi = householder_basis(V)
    f = Psi[1, Cd[:, 1]]                                    # degree-1 char at position 1
    tr = np.arange(0, len(Cd), 2)
    te = np.arange(1, len(Cd), 2)
    corrs, order, _ = topk_fourier(Cd, f, tr, te, Psi, ks=(1, 4))
    assert corrs[0] > 0.98
    assert degree_of(order[:1], V, w)[0] == 1


def test_random_target_flat():
    rng = np.random.default_rng(1)
    V, w = 20, 3
    Cd = np.unique(rng.integers(0, V, size=(80000, w)), axis=0)
    Psi = householder_basis(V)
    f = rng.choice([-1.0, 1.0], size=len(Cd))
    tr, val, te = _split3(len(Cd), 1)
    fp = fourier_pick(Cd, f, tr, val, te, Psi, V, w, ks=(1, 16, 256))
    assert fp["auc"] < 0.6                                  # nothing generalizes
    assert logistic_auc(Cd, f, tr, te, Psi, 2) < 0.6
