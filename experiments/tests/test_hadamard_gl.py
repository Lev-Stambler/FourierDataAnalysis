"""Tests for the power-of-2 Walsh-Hadamard basis and the token-level CSAMP search that realizes
categorical Householder GL as binary Walsh GL over packed token bits."""
import numpy as np

from fda_exp.hadamard_gl import (
    hadamard_gl_search,
    pack_tokens,
    token_blocks,
    token_degree,
)
from fda_exp.householder import hadamard_basis, walsh4
from fda_exp.qary_gl import _encode_qary, degree_of_codes, qary_coeffs_at


def test_hadamard_basis_properties():
    """+/-1 (unit-modulus), orthonormal under uniform, row 0 constant, and == walsh4 at V=4."""
    for V in (2, 4, 8, 16, 32, 64):
        H = hadamard_basis(V)
        assert H.shape == (V, V)
        assert np.all(np.abs(H) == 1.0), f"V={V} not +/-1"                    # NO high-degree inflation
        assert np.allclose(H @ H.T / V, np.eye(V)), f"V={V} not orthonormal"  # (1/V) H H^T = I
        assert np.all(H[0] == 1.0), f"V={V} row0 not constant"                # psi_0 = 1
    assert np.array_equal(hadamard_basis(4), walsh4())                        # generalizes walsh4
    import pytest
    with pytest.raises(ValueError):
        hadamard_basis(12)                                                    # not a power of 2


def _char(C, alpha, H):
    chi = np.ones(len(C))
    for p, a in alpha.items():
        chi = chi * H[a, C[:, p]]
    return chi


def _code(alpha, V):
    return int(sum(a * V ** p for p, a in alpha.items()))


def test_pack_tokens_is_encode_qary():
    rng = np.random.default_rng(0)
    C = rng.integers(0, 8, size=(1000, 4)).astype(np.int64)
    assert np.array_equal(pack_tokens(C, 8), _encode_qary(C, 8))


def test_token_blocks_and_degree_roundtrip():
    """Decoding a base-V code into per-token contrasts and re-packing is the identity; token-degree
    == number of nonzero contrasts == degree_of_codes."""
    V, w = 8, 4
    rng = np.random.default_rng(1)
    codes = rng.integers(0, V ** w, size=200).astype(np.int64)
    a = token_blocks(codes, V, w)                                    # (K, w) contrasts
    repacked = (a * (V ** np.arange(w))).sum(1)
    assert np.array_equal(repacked, codes)                          # mask <-> contrasts is identity
    assert np.array_equal(token_degree(codes, V, w), (a != 0).sum(1))
    assert np.array_equal(token_degree(codes, V, w), degree_of_codes(codes, V, w))


def _recover(V, w, alpha, m=40000, seed=0):
    rng = np.random.default_rng(seed)
    C = rng.integers(0, V, size=(m, w)).astype(np.int64)
    H = hadamard_basis(V)
    f = _char(C, alpha, H)                                          # a PURE token-level character (+/-1)
    r = hadamard_gl_search(C, f, w, V, tau=1.0, device="cpu", mode="csamp", seed=0, max_width=5000)
    return C, f, H, r


def test_recovers_planted_degree2_character():
    V, w = 8, 4
    alpha = {0: 5, 2: 3}                                            # token-degree 2
    C, f, H, r = _recover(V, w, alpha)
    assert r["status"] == "ok", r
    want = _code(alpha, V)
    assert want in set(int(c) for c in r["codes"]), (want, r["codes"].tolist())
    i = list(r["codes"]).index(want)
    assert int(r["degrees"][i]) == 2                               # TOKEN-degree, not bit-degree
    assert list(r["contrasts"][i]) == [5, 0, 3, 0]
    # the recovered code is consumable by the V-ary utilities with hadamard_basis(V): coeff ~ 1
    coef = qary_coeffs_at(C, f, np.array([want]), V, H, w)[0]
    assert abs(abs(coef) - 1.0) < 0.05, coef


def test_recovers_planted_degree3_character():
    V, w = 8, 4
    alpha = {0: 5, 1: 2, 3: 7}                                      # token-degree 3 (bit-degree 6)
    C, f, H, r = _recover(V, w, alpha, seed=2)
    assert r["status"] == "ok", r
    want = _code(alpha, V)
    codes = set(int(c) for c in r["codes"])
    assert want in codes, (want, sorted(codes))
    i = list(r["codes"]).index(want)
    assert int(r["degrees"][i]) == 3
    assert list(r["contrasts"][i]) == [5, 2, 0, 7]


def test_collapse_equivalence():
    """Searching frequency-collapsed DISTINCT contexts (summed f, norm_m=orig m) recovers exactly the
    same heavy characters as searching every row -- the lever that makes exact-W tractable at scale.
    Exact for ANY f because the group-by only ever uses per-context sums."""
    V, w, m = 4, 3, 8000
    rng = np.random.default_rng(5)
    C = rng.integers(0, V, size=(m, w)).astype(np.int64)
    H = hadamard_basis(V)
    f = _char(C, {0: 1, 2: 3}, H) + 0.3 * rng.standard_normal(m)    # planted char + within-context noise
    r_full = hadamard_gl_search(C, f, w, V, tau=1.0, device="cpu", max_width=5000)
    Cd, inv = np.unique(C, axis=0, return_inverse=True)             # collapse to distinct contexts
    f_sum = np.zeros(len(Cd)); np.add.at(f_sum, inv, f)            # per-context weighted sum
    r_col = hadamard_gl_search(Cd, f_sum, w, V, tau=1.0, device="cpu", max_width=5000, norm_m=m)
    assert set(int(c) for c in r_full["codes"]) == set(int(c) for c in r_col["codes"])
    assert _code({0: 1, 2: 3}, V) in set(int(c) for c in r_col["codes"])
    assert len(Cd) < m                                             # collapse actually shrank the data


def test_no_high_degree_inflation():
    """The whole point of the Walsh basis: a pure degree-3 char has |coeff| ~ 1, NOT V^{deg/2}.
    (With householder_basis it would inflate to ~V^{3/2}.)"""
    V, w = 8, 4
    alpha = {0: 5, 1: 2, 3: 7}
    C, f, H, _ = _recover(V, w, alpha, seed=3)
    coef = qary_coeffs_at(C, f, np.array([_code(alpha, V)]), V, H, w)[0]
    assert abs(abs(coef) - 1.0) < 0.05, coef                       # bounded by ||f||_inf, no inflation


def test_recall_vs_bruteforce_nonuniform():
    """On NON-uniform data with several planted characters + noise, the Walsh CSAMP search recovers
    the exact heavy set (recall vs brute-force qary_spectrum) -- the correctness-at-scale check."""
    from fda_exp.householder import qary_spectrum
    V, w, m = 8, 4, 40000                                          # V^w = 4096, enumerable
    rng = np.random.default_rng(7)
    probs = [rng.dirichlet(np.ones(V)) for _ in range(w)]          # skewed (non-uniform) marginals
    C = np.stack([rng.choice(V, size=m, p=probs[p]) for p in range(w)], axis=1).astype(np.int64)
    H = hadamard_basis(V)
    planted = [{0: 3, 2: 5}, {1: 6, 3: 2, 0: 1}, {2: 4}]           # deg 2, 3, 1
    f = sum(_char(C, a, H) for a in planted) + 0.3 * rng.standard_normal(m)

    fhat = qary_spectrum(C, f, H)                                  # exact spectrum (C-order MSD flat)
    tau = 1.2
    brute = set(int(s) for s in np.where(fhat ** 2 >= (tau / 2) ** 2)[0] if s != 0)
    r = hadamard_gl_search(C, f, w, V, tau=tau, device="cpu", max_width=20000)
    assert r["status"] == "ok", r
    got = set(int(np.ravel_multi_index(d, (V,) * w)) for d in token_blocks(r["codes"], V, w))
    assert len(brute) >= 3 and len(got & brute) / len(brute) >= 0.8, (len(got & brute), len(brute))
    for a in planted:                                              # every planted char is recovered
        assert int(np.ravel_multi_index([a.get(p, 0) for p in range(w)], (V,) * w)) in got
