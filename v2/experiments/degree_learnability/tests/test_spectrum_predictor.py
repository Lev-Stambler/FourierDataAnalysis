from __future__ import annotations

import numpy as np

from dlx.analysis.spectrum_predictor import (
    fit_frozen_ridge,
    nested_loco_predictions,
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


def test_frozen_ridge_round_trip_matches_manual_fit_predictions() -> None:
    rows = _rows()
    artifact = fit_frozen_ridge(
        rows,
        target="difficulty",
        feature_names=("resolved_nonconstant_energy", "energy_weighted_log_radius"),
    )
    predictions = predict_frozen_ridge(artifact, rows)
    assert predictions.shape == (len(rows),)
    assert np.sqrt(np.mean((predictions - [row["difficulty"] for row in rows]) ** 2)) < 0.01


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
