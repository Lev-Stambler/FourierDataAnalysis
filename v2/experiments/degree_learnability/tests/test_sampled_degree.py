from __future__ import annotations

import numpy as np

from dlx.profiles.sampled_degree import (
    crossfit_conditional_collision,
    geometric_sampled_features,
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
        include_product_reference=True,
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


def test_compact_default_omits_audit_and_product_diagnostic() -> None:
    rng = np.random.default_rng(7)
    contexts = rng.integers(0, 4, size=(2_000, 3), dtype=np.uint8)
    targets = contexts[:, 0]
    result = sampled_nested_degree_profile(
        contexts, targets, max_degree=3, n_chains=2, seed=1
    )
    assert "chains" not in result
    assert "product_reference_inverted_level_weights" not in result
    assert set(result["sampled_features"]) == {
        "sampled_nonconstant_energy_through_degree3",
        "sampled_mean_degree_through_degree3",
        "sampled_geometric_complexity_through_degree3",
    }


def test_geometric_summary_orders_matched_near_and_far_degree_three() -> None:
    chain = [
        {"conditional_collision_energy": 0.5, "support_columns": []},
        {"conditional_collision_energy": 0.5, "support_columns": [0]},
        {"conditional_collision_energy": 0.5, "support_columns": [0, 1]},
        {"conditional_collision_energy": 1.0, "support_columns": [0, 1, 2]},
    ]
    near = geometric_sampled_features([chain], (1, 2, 3))
    far = geometric_sampled_features([chain], (1, 2, 16))
    assert (
        near["sampled_nonconstant_energy_through_degree3"]
        == far["sampled_nonconstant_energy_through_degree3"]
    )
    assert near["sampled_mean_degree_through_degree3"] == 3.0
    assert (
        far["sampled_geometric_complexity_through_degree3"]
        > near["sampled_geometric_complexity_through_degree3"]
    )


def test_product_reference_binomial_inversion() -> None:
    weights = invert_product_reference_degree_curve([0.5, 0.5, 0.5, 1.0], 3)
    np.testing.assert_allclose(weights, [0.5, 0.0, 0.0, 0.5])
