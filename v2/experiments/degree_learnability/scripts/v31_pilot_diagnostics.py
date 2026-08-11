"""Post-gate diagnostics for the Fourier-CE pilot."""

from __future__ import annotations

import json
from pathlib import Path

import numpy as np
from scipy.stats import pearsonr, spearmanr

from dlx.analysis.architecture_matching import (
    fit_architecture_ols,
    grouped_loco_predictions,
    predict_architecture_ols,
    prediction_scores,
    stratified_corpus_bootstrap_improvement,
)
from dlx.protocol.frozen import (
    file_sha256,
    verify_hash_lock,
    write_hash_once,
    write_json_once,
)

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs/local/v30_architecture_spectrum"


def _loco(rows: list[dict], target: str, features: tuple[str, ...]) -> tuple[dict, np.ndarray]:
    actual = np.asarray([row[target] for row in rows], dtype=float)
    predicted = grouped_loco_predictions(
        rows, target=target, numeric_features=features
    )
    return prediction_scores(actual, predicted), predicted


def _improvement(baseline: dict, matched: dict) -> float:
    return (baseline["rmse"] - matched["rmse"]) / baseline["rmse"]


def main() -> None:
    pilot_hash = verify_hash_lock(
        OUT / "pilot_analysis.json", OUT / "pilot_analysis.sha256"
    )
    pilot = json.loads((OUT / "pilot_analysis.json").read_text())
    if pilot["continuation_gate_passed"]:
        raise ValueError("diagnostic is intended for a failed pilot gate")
    rows = pilot["rows"]
    target = pilot["target"]
    overlap = "fourier_ce_overlap"
    ordinary = (
        "unigram_entropy_bits",
        "heldout_bigram_ce_bits",
        "lag1_mutual_information_bits",
        "zlib_bits_per_byte",
    )
    strong = tuple(pilot["strong_baseline_features"])

    comparisons = {}
    predictions = {}
    for label, features in (("ordinary_controls", ordinary), ("strong", strong)):
        baseline, baseline_predictions = _loco(rows, target, features)
        matched, matched_predictions = _loco(rows, target, features + (overlap,))
        comparisons[label] = {
            "baseline": baseline,
            "plus_fourier_ce_overlap": matched,
            "relative_rmse_improvement": _improvement(baseline, matched),
        }
        predictions[label] = (baseline_predictions, matched_predictions)

    actual = np.asarray([row[target] for row in rows], dtype=float)
    baseline_fit = fit_architecture_ols(
        rows, target=target, numeric_features=strong
    )
    matched_fit = fit_architecture_ols(
        rows, target=target, numeric_features=strong + (overlap,)
    )
    baseline_residual = actual - predict_architecture_ols(baseline_fit, rows)
    matched_residual = actual - predict_architecture_ols(matched_fit, rows)
    partial_r_squared = 1.0 - float(matched_residual @ matched_residual) / float(
        baseline_residual @ baseline_residual
    )

    datasets = sorted({row["dataset"] for row in rows})
    architectures = sorted({row["architecture"] for row in rows})
    outcome = np.asarray(
        [
            [
                next(
                    row[target]
                    for row in rows
                    if row["dataset"] == dataset
                    and row["architecture"] == architecture
                )
                for architecture in architectures
            ]
            for dataset in datasets
        ],
        dtype=float,
    )
    feature = np.asarray(
        [
            [
                next(
                    row["features"][overlap]
                    for row in rows
                    if row["dataset"] == dataset
                    and row["architecture"] == architecture
                )
                for architecture in architectures
            ]
            for dataset in datasets
        ],
        dtype=float,
    )

    def double_center(matrix: np.ndarray) -> np.ndarray:
        return (
            matrix
            - matrix.mean(axis=0, keepdims=True)
            - matrix.mean(axis=1, keepdims=True)
            + matrix.mean()
        )

    centered_outcome = double_center(outcome).ravel()
    centered_feature = double_center(feature).ravel()
    pearson = pearsonr(centered_feature, centered_outcome)
    spearman = spearmanr(centered_feature, centered_outcome)

    architecture_comparisons = {}
    for architecture in architectures:
        selected = [row for row in rows if row["architecture"] == architecture]
        baseline, _ = _loco(selected, target, strong)
        matched, _ = _loco(selected, target, strong + (overlap,))
        architecture_comparisons[architecture] = {
            "baseline": baseline,
            "plus_fourier_ce_overlap": matched,
            "relative_rmse_improvement": _improvement(baseline, matched),
        }

    bootstrap_rows = [
        {
            "dataset": row["dataset"],
            "stratum": row["stratum"],
            "actual": row[target],
        }
        for row in rows
    ]
    strong_baseline_predictions, strong_matched_predictions = predictions["strong"]
    bootstrap = stratified_corpus_bootstrap_improvement(
        bootstrap_rows,
        strong_baseline_predictions,
        strong_matched_predictions,
        samples=100_000,
        seed=3106,
    )
    result = {
        "status": "post-gate diagnostic; does not reopen confirmation",
        "pilot_analysis_hash": pilot_hash,
        "pilot_results_sha256": file_sha256(OUT / "pilot_results.json"),
        "target": target,
        "comparisons": comparisons,
        "strong_full_sample": {
            "standardized_overlap_coefficient": matched_fit["coefficients"][
                len(strong)
            ],
            "partial_r_squared": partial_r_squared,
        },
        "double_centered_dataset_architecture_interaction": {
            "pearson_r": float(pearson.statistic),
            "pearson_p": float(pearson.pvalue),
            "spearman_rho": float(spearman.statistic),
            "spearman_p": float(spearman.pvalue),
        },
        "architecture_comparisons": architecture_comparisons,
        "descriptive_stratified_corpus_bootstrap": bootstrap,
        "interpretation": "Fourier overlap helps relative to ordinary controls, but exact architecture-support matching adds no held-out information beyond the strong degree/energy/locality spectrum summaries.",
    }
    digest = write_json_once(OUT / "pilot_diagnostics.json", result)
    write_hash_once(OUT / "pilot_diagnostics.sha256", digest)
    print(
        json.dumps(
            {
                "comparisons": comparisons,
                "strong_full_sample": result["strong_full_sample"],
                "interaction": result[
                    "double_centered_dataset_architecture_interaction"
                ],
                "bootstrap": bootstrap,
                "artifact_sha256": digest,
            },
            indent=2,
        )
    )


if __name__ == "__main__":
    main()
