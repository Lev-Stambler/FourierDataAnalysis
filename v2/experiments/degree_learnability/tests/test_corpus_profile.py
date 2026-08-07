"""Tests for the oracle-free suffix-filtration profile estimator."""

import numpy as np

from dlx.families import F5IID, F1Markov
from dlx.profiles.corpus_profile import (
    suffix_filtration_profile,
    suffix_filtration_profile_crossfit,
)


def test_iid_profile_flat():
    fam = F5IID(q=8, L=16)
    x = fam.sample(400_000, np.random.default_rng(0))
    prof = suffix_filtration_profile(x, q=8, L=16, k_max=3)
    R = np.array(prof["R"])
    # iid: R_k ~ 1/q for all k (no suffix explains anything beyond marginal)
    assert np.allclose(R, 1.0 / 8, atol=0.01), R


def test_markov1_profile_concentrates_at_k1():
    """x_t = x_{t-1} + noise: suffix of length 1 explains (almost) everything."""
    fam = F1Markov(q=8, L=16, k=1, eta=0.1)
    x = fam.sample(400_000, np.random.default_rng(1))
    prof = suffix_filtration_profile(x, q=8, L=16, k_max=3)
    R = np.array(prof["R"])
    # R_1 should capture essentially all explainable mass; increments after ~0
    assert R[1] > 0.8, R
    assert R[2] - R[1] < 0.02, R
    assert R[3] - R[2] < 0.02, R


def test_markov3_needs_k3():
    fam = F1Markov(q=8, L=16, k=3, eta=0.1)
    x = fam.sample(400_000, np.random.default_rng(2))
    prof = suffix_filtration_profile(x, q=8, L=16, k_max=3)
    R = np.array(prof["R"])
    # the sum rule only becomes predictable once all three lags are seen:
    # little mass at k=1, the jump arrives at k=3
    assert R[1] < 0.3, R
    assert R[3] > R[2] + 0.1, R


def test_bounds_and_monotonicity():
    fam = F1Markov(q=8, L=16, k=2, eta=0.2)
    x = fam.sample(200_000, np.random.default_rng(3))
    prof = suffix_filtration_profile(x, q=8, L=16, k_max=3)
    R = np.array(prof["R"])
    assert R[0] <= R[1] <= R[2] <= R[3] <= 1.0 + 1e-9
    assert prof["coverage"][1] > 0.99  # q=8 suffixes collide heavily at 200k


def test_crossfit_avoids_sparse_suffix_singleton_bias():
    fam = F5IID(q=64, L=16)
    x = fam.sample(300_000, np.random.default_rng(4))
    prof = suffix_filtration_profile_crossfit(x, q=64, L=16, k_max=3)
    R = np.array(prof["R"])
    assert np.all(R < 0.03), R
    assert prof["estimator"].startswith("alternating-position cross-fit")
