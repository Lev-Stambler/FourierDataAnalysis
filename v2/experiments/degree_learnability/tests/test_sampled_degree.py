from __future__ import annotations

import numpy as np

from dlx.profiles.sampled_degree import (
    crossfit_conditional_collision,
    invert_product_reference_degree_curve,
    sampled_nested_degree_profile,
)


def test_crossfit_collision_bounds_and_coverage() -> None:
    rng = np.random.default_rng(4)
    keys = rng.integers(0, 4, size=4_000, dtype=np.uint64)
    targets = keys.copy()
    folds = rng.integers(0, 2, size=len(keys), dtype=np.int8)
    result = crossfit_conditional_collision(keys, targets, folds)
    assert result["conditional_collision_energy"] == 1.0
    assert result["opposite_fold_context_coverage"] == 1.0
    assert result["evaluation_hoeffding_interval"][0] <= 1.0
    assert result["coverage_identification_interval"] == [1.0, 1.0]


def test_sampled_curve_detects_pure_degree_three_signal() -> None:
    rng = np.random.default_rng(12)
    contexts = rng.integers(0, 2, size=(12_000, 3), dtype=np.uint8)
    targets = np.bitwise_xor.reduce(contexts, axis=1)
    result = sampled_nested_degree_profile(
        contexts,
        targets,
        max_degree=3,
        n_chains=8,
        seed=9,
    )
    curve = result["degree_curve"]
    assert curve[2]["mean_conditional_collision_energy"] < 0.55
    assert curve[3]["mean_conditional_collision_energy"] > 0.95
    assert curve[3]["mean_increment_from_previous_degree"] > 0.4
    assert curve[3]["mean_context_coverage"] == 1.0
    weights = result["product_reference_inverted_level_weights"]
    assert abs(weights[1]) < 0.03
    assert abs(weights[2]) < 0.03
    assert weights[3] > 0.45


def test_product_reference_binomial_inversion() -> None:
    weights = invert_product_reference_degree_curve([0.5, 0.5, 0.5, 1.0], 3)
    np.testing.assert_allclose(weights, [0.5, 0.0, 0.0, 0.5])
