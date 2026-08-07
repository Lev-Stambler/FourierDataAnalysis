"""S0 tier tests: domains (character identities, FFT conventions)."""

import numpy as np

from dlx.domains import (
    character_matrix,
    fhat_enumerated,
    invert_enumerated,
    level_weights,
    orthogonality_check,
)


def test_orthogonality_composite_q():
    rng = np.random.default_rng(0)
    # composite q, per PLAN §12.4 test 1
    assert orthogonality_check(q=8, n=3, rng=rng)
    assert orthogonality_check(q=4, n=4, rng=rng)


def test_character_unit_modulus():
    rng = np.random.default_rng(1)
    q, n = 5, 3
    alphas = rng.integers(0, q, size=(6, n))
    xs = rng.integers(0, q, size=(10, n))
    M = character_matrix(q, alphas, xs)
    assert np.allclose(np.abs(M), 1.0)


def test_fft_matches_brute_force_and_inverts():
    rng = np.random.default_rng(2)
    q, n = 4, 3
    values = rng.standard_normal((q,) * n) + 1j * rng.standard_normal((q,) * n)
    fhat = fhat_enumerated(values)

    # brute-force one coefficient: fhat(k) = (1/q^n) sum_x f(x) conj(chi_k(x))
    k = np.array([1, 0, 3])
    total = 0.0 + 0.0j
    xs = np.array(np.unravel_index(np.arange(q**n), (q,) * n)).T
    chi = character_matrix(q, k[None], xs)[0]
    total = (values.reshape(-1) * np.conj(chi)).sum() / q**n
    assert abs(fhat[k[0], k[1], k[2]] - total) < 1e-10

    # exact inversion
    rec = invert_enumerated(fhat)
    assert np.allclose(rec, values, atol=1e-9)


def test_level_weights_parseval_and_degree():
    rng = np.random.default_rng(3)
    q, n = 4, 3
    values = rng.standard_normal((q,) * n)
    W = level_weights(fhat_enumerated(values))
    assert W.shape == (n + 1,)
    # Parseval under uniform law: sum_k W^k = E[f^2]
    assert abs(W.sum() - (np.abs(values) ** 2).mean()) < 1e-10

    # a single planted character lives entirely at its degree
    alpha = np.array([2, 0, 3])
    xs_grid = np.indices((q,) * n).reshape(n, -1).T  # (q^n, n)
    chi_vals = character_matrix(q, alpha[None], xs_grid)[0].reshape((q,) * n)
    Wp = level_weights(fhat_enumerated(chi_vals))
    assert abs(Wp[2] - 1.0) < 1e-10  # degree of (2,0,3) is 2
    assert abs(Wp.sum() - 1.0) < 1e-10
