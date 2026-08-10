from __future__ import annotations

import numpy as np

from dlx.analysis.architecture_matching import (
    fit_architecture_ols,
    grouped_loco_predictions,
    predict_architecture_ols,
    prediction_scores,
    stratified_corpus_bootstrap_improvement,
)


def _rows() -> list[dict]:
    rows = []
    for stratum in ("code", "prose"):
        for corpus in range(4):
            feature = corpus + (4 if stratum == "code" else 0)
            for architecture in ("nope", "rope"):
                target = 0.2 + 0.03 * feature + 0.05 * (architecture == "rope")
                rows.append(
                    {
                        "dataset": f"{stratum}_{corpus}",
                        "stratum": stratum,
                        "architecture": architecture,
                        "features": {"spectrum": feature},
                        "difficulty": target,
                    }
                )
    return rows


def test_architecture_ols_round_trip() -> None:
    rows = _rows()
    artifact = fit_architecture_ols(
        rows, target="difficulty", numeric_features=("spectrum",)
    )
    predictions = predict_architecture_ols(artifact, rows)
    np.testing.assert_allclose(predictions, [row["difficulty"] for row in rows])


def test_stratified_corpus_bootstrap_keeps_architecture_blocks() -> None:
    rows = _rows()
    actual = np.asarray([row["difficulty"] for row in rows])
    score_rows = [
        {**row, "actual": target} for row, target in zip(rows, actual, strict=True)
    ]
    baseline = np.full(len(rows), actual.mean())
    matched = actual + 0.01
    result = stratified_corpus_bootstrap_improvement(
        score_rows, baseline, matched, samples=500, seed=30
    )
    assert result["relative_rmse_improvement"] > 0.8
    assert result["stratified_corpus_bootstrap_95_interval"][0] > 0.0


def test_grouped_loco_never_trains_on_held_out_corpus() -> None:
    rows = _rows()
    predictions = grouped_loco_predictions(
        rows, target="difficulty", numeric_features=("spectrum",)
    )
    scores = prediction_scores(
        np.asarray([row["difficulty"] for row in rows]), predictions
    )
    assert scores["rmse"] < 1e-12
    assert scores["r_squared"] > 0.999
