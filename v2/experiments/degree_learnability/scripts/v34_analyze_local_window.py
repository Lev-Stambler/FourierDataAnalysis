"""Evaluate the prospectively frozen v3.4 local-window transfer test."""

from __future__ import annotations

import json
from pathlib import Path

import numpy as np
from scipy.stats import pearsonr, spearmanr
from sklearn.linear_model import LinearRegression
from sklearn.preprocessing import StandardScaler

from dlx.analysis.architecture_matching import (
    prediction_scores,
    stratified_corpus_bootstrap_improvement,
)
from dlx.analysis.text_panel import median_cell_metrics
from dlx.protocol.frozen import (
    file_sha256,
    load_frozen_protocol,
    verify_hash_lock,
    write_hash_once,
    write_json_once,
)

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs/local/v34_local_window"
SOURCE_OUT = ROOT / "runs/local/v30_architecture_spectrum"


def _matrix(rows: list[dict], features: tuple[str, ...]) -> np.ndarray:
    windows = (8, 16, 32)
    return np.asarray(
        [
            [float(row[name]) for name in features]
            + [float(int(row["window"]) == value) for value in windows[1:]]
            for row in rows
        ],
        dtype=float,
    )


def _fit_predict(
    development: list[dict], confirmation: list[dict], target: str, features: tuple[str, ...]
) -> tuple[dict, np.ndarray]:
    x_train = _matrix(development, features)
    x_test = _matrix(confirmation, features)
    y_train = np.asarray([row[target] for row in development], dtype=float)
    scaler = StandardScaler().fit(x_train)
    model = LinearRegression().fit(scaler.transform(x_train), y_train)
    names = list(features) + ["window[16]", "window[32]"]
    return (
        {
            "target": target,
            "features": list(features),
            "columns": names,
            "scaler_mean": scaler.mean_.tolist(),
            "scaler_scale": scaler.scale_.tolist(),
            "intercept": float(model.intercept_),
            "coefficients": {
                name: float(value) for name, value in zip(names, model.coef_, strict=True)
            },
            "training_datasets": sorted({row["dataset"] for row in development}),
        },
        model.predict(scaler.transform(x_test)),
    )


def _correlations(rows: list[dict], target: str) -> dict:
    output = {}
    for window in (8, 16, 32):
        selected = [row for row in rows if int(row["window"]) == window]
        x = np.asarray([row["delta_uniform_overlap"] for row in selected])
        y = np.asarray([row[target] for row in selected])
        pearson = pearsonr(x, y)
        spearman = spearmanr(x, y)
        output[str(window)] = {
            "n": len(selected),
            "pearson_r": float(pearson.statistic),
            "pearson_p": float(pearson.pvalue),
            "spearman_rho": float(spearman.statistic),
            "spearman_p": float(spearman.pvalue),
        }
    return output


def main() -> dict:
    protocol = load_frozen_protocol(ROOT / "configs/protocol_v3.4.json")
    measurement = load_frozen_protocol(ROOT / "configs/protocol_v3.1.json")
    feature_hash = verify_hash_lock(
        OUT / "frozen_features.json", OUT / "frozen_features.sha256"
    )
    feature_artifact = json.loads((OUT / "frozen_features.json").read_text())
    feature_map = {
        (row["dataset"], int(row["window"])): row
        for row in feature_artifact["rows"]
    }

    natural_path = OUT / "natural_results.json"
    cells = json.loads(natural_path.read_text())
    expected = {
        (dataset, window, seed)
        for dataset in {key[0] for key in feature_map}
        for window in protocol["natural_training"]["new_windows"]
        for seed in protocol["natural_training"]["seeds"]
    }
    observed = {
        (
            row["dataset"],
            int(row["architecture"].removeprefix("rope_w")),
            int(row["seed"]),
        )
        for row in cells
    }
    if len(cells) != len(expected) or observed != expected:
        raise ValueError(f"natural grid incomplete: {len(cells)}/{len(expected)}")
    for cell in cells:
        if cell["analysis_protocol_hash"] != protocol["protocol_hash"]:
            raise ValueError(f"protocol mismatch in {cell['cell_id']}")
        if cell["prediction_lock_hash"] != feature_hash:
            raise ValueError(f"unfrozen features in {cell['cell_id']}")

    legacy_path = SOURCE_OUT / "expansion_results.json"
    if file_sha256(legacy_path) != protocol["natural_training"]["radius64_reuse"]["sha256"]:
        raise ValueError("legacy radius-64 outcomes changed after freeze")
    legacy = [
        row
        for row in json.loads(legacy_path.read_text())
        if row["architecture"] == "rope"
    ]
    legacy_by_dataset = {
        dataset: [row for row in legacy if row["dataset"] == dataset]
        for dataset in sorted({key[0] for key in feature_map})
    }
    if any(len(value) != 2 for value in legacy_by_dataset.values()):
        raise ValueError("legacy radius-64 outcomes are incomplete")

    rows = []
    for (dataset, window), features in sorted(feature_map.items()):
        selected = [
            row
            for row in cells
            if row["dataset"] == dataset
            and int(row["architecture"].removeprefix("rope_w")) == window
        ]
        validation_hashes = {
            row["validation_starts_sha256"]
            for row in selected + legacy_by_dataset[dataset]
        }
        if len(validation_hashes) != 1:
            raise ValueError(f"validation examples are not paired for {dataset}")
        current = median_cell_metrics(selected)
        radius64 = median_cell_metrics(legacy_by_dataset[dataset])
        rows.append(
            {
                **features,
                "normalized_curve_area": current["normalized_curve_area"],
                "radius64_normalized_curve_area": radius64["normalized_curve_area"],
                "delta_normalized_curve_area": current["normalized_curve_area"]
                - radius64["normalized_curve_area"],
                "final_ce_fraction": current["final_ce_fraction"],
                "radius64_final_ce_fraction": radius64["final_ce_fraction"],
                "delta_final_ce_fraction": current["final_ce_fraction"]
                - radius64["final_ce_fraction"],
                "validation_starts_sha256": validation_hashes.pop(),
            }
        )

    development = [row for row in rows if row["panel"] == "development"]
    confirmation = [row for row in rows if row["panel"] == "confirmation"]
    if len(development) != 72 or len(confirmation) != 72:
        raise ValueError("v3.4 requires 24 corpora x three windows in each panel")
    model_specs = {
        "window_baseline": (),
        "uniform_fingerprint": ("delta_uniform_overlap",),
        "direct_far_energy": ("direct_far_energy_fraction",),
        "unreachable_energy": ("unreachable_energy_fraction",),
        "unreachable_energy_plus_uniform": (
            "unreachable_energy_fraction",
            "delta_uniform_overlap",
        ),
    }
    targets = ("delta_normalized_curve_area", "delta_final_ce_fraction")
    analyses = {}
    bootstrap_rows = [
        {"dataset": row["dataset"], "stratum": row["stratum"], "actual": row[targets[0]]}
        for row in confirmation
    ]
    for target in targets:
        actual = np.asarray([row[target] for row in confirmation], dtype=float)
        models = {}
        predictions = {}
        for name, features in model_specs.items():
            model, prediction = _fit_predict(development, confirmation, target, features)
            models[name] = model
            predictions[name] = prediction
        comparisons = {
            "uniform_vs_window_baseline": {
                "baseline": prediction_scores(actual, predictions["window_baseline"]),
                "candidate": prediction_scores(actual, predictions["uniform_fingerprint"]),
            },
            "uniform_increment_beyond_far_energy": {
                "baseline": prediction_scores(
                    actual, predictions["unreachable_energy"]
                ),
                "candidate": prediction_scores(
                    actual, predictions["unreachable_energy_plus_uniform"]
                ),
            },
        }
        for value in comparisons.values():
            value["relative_rmse_improvement"] = (
                value["baseline"]["rmse"] - value["candidate"]["rmse"]
            ) / value["baseline"]["rmse"]
        if target == targets[0]:
            for label, left, right in (
                ("uniform_vs_window_baseline", "window_baseline", "uniform_fingerprint"),
                (
                    "uniform_increment_beyond_far_energy",
                    "unreachable_energy",
                    "unreachable_energy_plus_uniform",
                ),
            ):
                comparisons[label]["stratified_corpus_bootstrap"] = (
                    stratified_corpus_bootstrap_improvement(
                        bootstrap_rows,
                        predictions[left],
                        predictions[right],
                        samples=int(protocol["analysis"]["bootstrap"]["samples"]),
                        seed=int(protocol["analysis"]["bootstrap"]["seed"])
                        + int(label == "uniform_increment_beyond_far_energy"),
                    )
                )
        analyses[target] = {
            "models": models,
            "confirmation_scores": {
                name: prediction_scores(actual, prediction)
                for name, prediction in predictions.items()
            },
            "comparisons": comparisons,
            "confirmation_within_window_correlations": _correlations(
                confirmation, target
            ),
        }

    primary = analyses[targets[0]]
    coefficient = primary["models"]["uniform_fingerprint"]["coefficients"][
        "delta_uniform_overlap"
    ]
    interval = primary["comparisons"]["uniform_vs_window_baseline"][
        "stratified_corpus_bootstrap"
    ]["stratified_corpus_bootstrap_95_interval"]
    passed = coefficient > 0.0 and interval[0] > 0.0
    verdict = (
        "LOCAL_WINDOW_UNIFORM_FINGERPRINT_TRANSFERS"
        if passed
        else "LOCAL_WINDOW_UNIFORM_FINGERPRINT_DOES_NOT_TRANSFER"
    )
    result = {
        "status": "prospective held-out local-window analysis complete",
        "verdict": verdict,
        "protocol_hash": protocol["protocol_hash"],
        "measurement_protocol_hash": measurement["protocol_hash"],
        "frozen_features_sha256": feature_hash,
        "natural_results_sha256": file_sha256(natural_path),
        "primary_gate": {
            "criterion": protocol["analysis"]["success"],
            "delta_overlap_coefficient": coefficient,
            "confirmation_rmse_improvement_interval": interval,
            "passed": passed,
        },
        "analyses": analyses,
        "rows": rows,
    }
    digest = write_json_once(OUT / "analysis.json", result)
    write_hash_once(OUT / "analysis.sha256", digest)
    print(
        json.dumps(
            {
                "verdict": verdict,
                "primary_gate": result["primary_gate"],
                "primary_confirmation": primary["confirmation_scores"],
                "analysis_sha256": digest,
            },
            indent=2,
        )
    )
    return result


if __name__ == "__main__":
    main()
