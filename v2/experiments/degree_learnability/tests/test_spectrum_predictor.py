from __future__ import annotations

import numpy as np

from dlx.analysis.spectrum_predictor import (
    fit_frozen_ols,
    fit_frozen_ridge,
    nested_loco_predictions,
    paired_corpus_bootstrap_improvement,
    predict_frozen_ols,
    predict_frozen_ridge,
)


def _rows() -> list[dict]:
    rows = []
    configurations = ("absolute", "alibi")
    for corpus in range(8):
        for configuration in configurations:
            energy = 0.1 + 0.03 * corpus
            locality = 1.0 + corpus
            target = 0.2 + 0.4 * energy + 0.03 * locality
            target += 0.05 * (configuration == "alibi")
            rows.append(
                {
                    "dataset": f"corpus_{corpus}",
                    "configuration": configuration,
                    "features": {
                        "resolved_nonconstant_energy": energy,
                        "energy_weighted_log_radius": locality,
                    },
                    "difficulty": target,
                }
            )
    return rows


def test_nested_loco_ridge_recovers_grouped_signal() -> None:
    result = nested_loco_predictions(
        _rows(),
        target="difficulty",
        feature_names=("resolved_nonconstant_energy", "energy_weighted_log_radius"),
        kind="ridge",
    )
    assert result["r2"] > 0.95
    assert len(result["outer_selections"]) == 8


def test_nested_loco_ols_recovers_exact_linear_signal() -> None:
    result = nested_loco_predictions(
        _rows(),
        target="difficulty",
        feature_names=("resolved_nonconstant_energy", "energy_weighted_log_radius"),
        kind="ols",
    )
    assert result["r2"] > 0.99
    assert all(
        selection["parameters"] == {}
        for selection in result["outer_selections"].values()
    )


def test_frozen_ridge_round_trip_matches_manual_fit_predictions() -> None:
    rows = _rows()
    artifact = fit_frozen_ridge(
        rows,
        target="difficulty",
        feature_names=("resolved_nonconstant_energy", "energy_weighted_log_radius"),
    )
    predictions = predict_frozen_ridge(artifact, rows)
    assert predictions.shape == (len(rows),)
    assert (
        np.sqrt(np.mean((predictions - [row["difficulty"] for row in rows]) ** 2))
        < 0.01
    )


def test_frozen_ols_round_trip_and_intercept_model() -> None:
    rows = _rows()
    artifact = fit_frozen_ols(
        rows,
        target="difficulty",
        feature_names=("resolved_nonconstant_energy", "energy_weighted_log_radius"),
    )
    predictions = predict_frozen_ols(artifact, rows)
    assert (
        np.sqrt(np.mean((predictions - [row["difficulty"] for row in rows]) ** 2))
        < 1e-10
    )
    configuration_only = fit_frozen_ols(rows, target="difficulty", feature_names=())
    baseline = predict_frozen_ols(configuration_only, rows)
    for configuration in ("absolute", "alibi"):
        selected = np.asarray([row["configuration"] == configuration for row in rows])
        expected = np.mean(
            [row["difficulty"] for row in rows if row["configuration"] == configuration]
        )
        np.testing.assert_allclose(baseline[selected], expected)


def test_frozen_ols_rejects_constant_numeric_feature() -> None:
    rows = _rows()
    for row in rows:
        row["features"]["constant"] = 1.0
    with np.testing.assert_raises_regex(ValueError, "constant design"):
        fit_frozen_ols(rows, target="difficulty", feature_names=("constant",))
    historical = fit_frozen_ols(
        rows,
        target="difficulty",
        feature_names=("constant",),
        reject_constant_features=False,
    )
    assert historical["coefficients"][0] == 0.0
    assert np.all(np.isfinite(predict_frozen_ols(historical, rows)))


def test_paired_bootstrap_is_deterministic_and_detects_improvement() -> None:
    actual = np.arange(10, dtype=float)
    baseline = actual + np.linspace(-1.0, 1.0, 10)
    challenger = actual + 0.1 * np.linspace(-1.0, 1.0, 10)
    first = paired_corpus_bootstrap_improvement(
        actual, baseline, challenger, samples=500, seed=4
    )
    second = paired_corpus_bootstrap_improvement(
        actual, baseline, challenger, samples=500, seed=4
    )
    assert first == second
    assert first["relative_rmse_improvement"] > 0.8


def test_supported_low_data_model_families_run() -> None:
    rows = _rows()
    for kind in ("elastic_net", "pls", "svr_rbf", "gaussian_process"):
        result = nested_loco_predictions(
            rows,
            target="difficulty",
            feature_names=("resolved_nonconstant_energy", "energy_weighted_log_radius"),
            kind=kind,
        )
        assert np.isfinite(result["rmse"])
