from __future__ import annotations

import numpy as np

from dlx.analysis.paired_contrast import (
    association,
    blocked_spearman_permutation,
    fit_standardized_contrast,
    holm_adjust,
    predict_standardized_contrast,
    stratified_association_bootstrap,
)


def _rows() -> list[dict]:
    return [
        {
            "dataset": f"{stratum}_{index}",
            "stratum": stratum,
            "delta_overlap": float(index + offset),
            "delta_target": float(2 * (index + offset) + (-1) ** index * 0.1),
        }
        for stratum, offset in (("code", 0), ("prose", 5))
        for index in range(4)
    ]


def test_swapping_architecture_order_preserves_association_and_fit() -> None:
    x = np.asarray([0.1, 0.4, 0.7, 1.2])
    y = np.asarray([-0.2, 0.3, 0.8, 1.7])
    forward = association(x, y)
    reverse = association(-x, -y)
    assert forward == reverse
    model = fit_standardized_contrast(x, y)
    reversed_model = fit_standardized_contrast(-x, -y)
    np.testing.assert_allclose(
        predict_standardized_contrast(model, x),
        -predict_standardized_contrast(reversed_model, -x),
    )
    assert model["coefficient"] == reversed_model["coefficient"]


def test_stratified_resampling_is_deterministic() -> None:
    rows = _rows()
    first = stratified_association_bootstrap(
        rows,
        feature="delta_overlap",
        target="delta_target",
        samples=500,
        seed=33,
    )
    second = stratified_association_bootstrap(
        rows,
        feature="delta_overlap",
        target="delta_target",
        samples=500,
        seed=33,
    )
    assert first == second
    assert first["pearson_95_interval"][0] > 0.99


def test_blocked_permutation_is_deterministic_and_detects_alignment() -> None:
    rows = _rows()
    first = blocked_spearman_permutation(
        rows,
        feature="delta_overlap",
        target="delta_target",
        permutations=2_000,
        seed=34,
    )
    second = blocked_spearman_permutation(
        rows,
        feature="delta_overlap",
        target="delta_target",
        permutations=2_000,
        seed=34,
    )
    assert first == second
    assert first["mean_rho"] > 0.9
    assert first["two_sided_permutation_p"] < 0.05


def test_holm_adjustment_is_monotone_in_rank_order() -> None:
    adjusted = holm_adjust({"area": 0.03, "final": 0.01, "other": 0.2})
    assert adjusted == {"area": 0.06, "final": 0.03, "other": 0.2}
