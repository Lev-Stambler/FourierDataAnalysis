"""M2 tests: L0 exactness (noiseless enumerable cases) + aliasing regression."""

import numpy as np

from dlx.learners import SpectralLearner, low_degree_indices


def test_nd_count():
    # N_d = sum_{k<=d} C(n,k) (q-1)^k
    idx = low_degree_indices(q=4, n=6, d=2)
    assert idx.shape[0] == 1 + 6 * 3 + 15 * 9


def test_exact_recovery_planted_character_fullcube():
    q, n = 4, 4
    gamma = np.array([1, 0, 2, 3])
    xs = np.array(np.unravel_index(np.arange(q**n), (q,) * n)).T
    f = np.exp(2j * np.pi / q * (xs @ gamma % q))
    d = int(np.count_nonzero(gamma))
    lr = SpectralLearner(q=q, n=n, d=d).fit(xs, f, dataset_size=q**n)
    pred = lr.predict(xs)
    assert np.allclose(pred, f, atol=1e-9)


def test_paper_aliasing_halfcube_scaling():
    """Paper example: D = {x: x_0 = 1}, f = (-1)^{x_2}.

    Unscaled plug-in reconstruction returns 2f on D (aliased character counted
    once per alias); the C_D^{-1} scaling recovers f exactly.
    """
    q, n = 2, 4
    cube = np.array(np.unravel_index(np.arange(q**n), (q,) * n)).T
    D = cube[cube[:, 0] == 1]
    f = 1.0 - 2.0 * D[:, 2]

    lr = SpectralLearner(q=q, n=n, d=n).fit(D, f, dataset_size=len(D))
    assert abs(lr.C_D - 2.0) < 1e-12
    assert np.allclose(lr.predict(D), f, atol=1e-9)

    # unscaled variant: C_D := 1 gives 2f on D
    lr.C_D = 1.0
    assert np.allclose(lr.predict(D), 2.0 * f, atol=1e-9)


def test_exact_recovery_random_low_degree_polynomial():
    rng = np.random.default_rng(0)
    q, n, d = 4, 5, 2
    cube = np.array(np.unravel_index(np.arange(q**n), (q,) * n)).T
    idx = low_degree_indices(q, n, d)
    coeffs = rng.standard_normal(idx.shape[0]) + 1j * rng.standard_normal(idx.shape[0])
    phase = (idx @ cube.T) % q
    f = (np.exp(2j * np.pi / q) ** phase).T @ coeffs

    lr = SpectralLearner(q=q, n=n, d=d).fit(cube, f, dataset_size=q**n)
    assert np.allclose(lr.predict(cube), f, atol=1e-8)
