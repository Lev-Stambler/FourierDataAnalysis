import numpy as np

from dlx.families import F2SubsetSum
from dlx.profiles.inverse_likelihood import (
    InverseLikelihoodTerm,
    fit_exact_inverse_likelihood,
    inverse_likelihood_column,
)
from dlx.profiles.text_anova import (
    _additive_fit,
    _score_pair_lookup,
    conditional_fourier_spectrum,
    inverse_likelihood_pair_profile,
    variance_concentration,
)


def test_chunked_pair_prior_scoring_is_chunk_size_invariant():
    rng = np.random.default_rng(91)
    q = 5
    a = rng.integers(q, size=4_000)
    b = rng.integers(q, size=4_000)
    y = (a + 2 * b + rng.integers(2, size=4_000)) % q
    fit = np.arange(len(y)) % 2 == 0
    intercept, g_a, g_b, _ = _additive_fit(
        a[fit], b[fit], y[fit], q, max_iter=100, tolerance=1e-10
    )
    args = (
        a[fit],
        b[fit],
        y[fit],
        a[~fit],
        b[~fit],
        y[~fit],
        q,
        8.0,
        intercept,
        g_a,
        g_b,
    )
    one_pair_at_a_time = _score_pair_lookup(*args, prior_chunk_pairs=1)
    all_pairs_at_once = _score_pair_lookup(*args, prior_chunk_pairs=q * q)
    assert np.isclose(one_pair_at_a_time, all_pairs_at_once, atol=1e-14)


def test_formula_19_constant_and_binary_contrasts():
    X = np.array([[0, 0], [0, 1], [1, 0]], dtype=np.int64)
    constant = inverse_likelihood_column(X, InverseLikelihoodTerm((), ()))
    first = inverse_likelihood_column(X, InverseLikelihoodTerm((0,), (0,)))
    second = inverse_likelihood_column(X, InverseLikelihoodTerm((1,), (0,)))
    assert np.array_equal(constant, np.ones(3))
    assert np.allclose(first, [1.5, 1.5, -3.0])
    assert np.allclose(second, [1.5, -3.0, 1.5])


def test_exact_rank_selected_basis_reconstructs_sparse_support():
    X = np.array([[0, 0], [0, 1], [1, 0]] * 5, dtype=np.int64)
    target = np.array([0.2, 0.7, -0.4] * 5)
    result = fit_exact_inverse_likelihood(X, target, (2, 2), max_degree=2)
    assert result["rank"] == 3
    assert result["support_size"] == 3
    assert result["weighted_mse"] < 1e-20


def test_text_pair_profile_separates_copy_from_xor():
    rng = np.random.default_rng(4)
    n = 80_000
    copy = F2SubsetSum(q=2, L=4, lags=(2,), eta=0.1).sample(n, rng)
    copy_profile = inverse_likelihood_pair_profile(copy, 2, 1, 2, max_tokens=60_000)
    assert copy_profile["degree1_gain"] > 0.35
    assert copy_profile["degree2_incremental_gain"] < 0.02

    xor = F2SubsetSum(q=2, L=4, lags=(1, 2), eta=0.1).sample(n, rng)
    xor_profile = inverse_likelihood_pair_profile(xor, 2, 1, 2, max_tokens=60_000)
    assert xor_profile["degree1_gain"] < 0.02
    assert xor_profile["degree2_incremental_gain"] > 0.4


def test_variance_concentration_uses_total_resolved_variance():
    additive = variance_concentration(1.0, 0.4, 0.45)
    assert additive["pair_function_variance"] == 0.6
    assert additive["degree1_variance"] == 0.6
    assert additive["concentration_leq_1"] == 1.0
    assert additive["tail_above_degree_1"] == 0.0
    assert additive["monotone_projection_applied"]

    interaction = variance_concentration(1.0, 1.02, 0.5)
    assert interaction["pair_function_variance"] == 0.5
    assert interaction["degree1_variance"] == 0.0
    assert interaction["concentration_leq_1"] == 0.0
    assert interaction["tail_above_degree_1"] == 1.0


def test_conditional_fourier_spectrum_uses_absolute_square_energy():
    spectrum = conditional_fourier_spectrum(0.8, 0.5, 0.3, q=4)
    assert np.allclose(spectrum["level_weights"], [0.2, 0.3, 0.2])
    assert spectrum["level_cardinalities"] == [1, 6, 9]
    assert np.isclose(spectrum["total_square_energy"], 0.7)
    assert np.isclose(sum(spectrum["level_weights"]), 0.7)
    assert np.isclose(spectrum["mean_nonconstant_spectral_degree"], 1.4)
