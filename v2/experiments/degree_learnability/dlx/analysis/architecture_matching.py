"""Frozen low-dimensional predictors for architecture-spectrum matching."""

from __future__ import annotations

import math

import numpy as np
from sklearn.linear_model import LinearRegression
from sklearn.preprocessing import StandardScaler


def _numeric_value(row: dict, name: str) -> float:
    value = row.get(name, row.get("features", {}).get(name))
    result = float(value)
    if not math.isfinite(result):
        raise ValueError(f"row {row.get('dataset')} lacks finite feature {name}")
    return result


def architecture_design_matrix(
    rows: list[dict],
    *,
    numeric_features: tuple[str, ...],
    architectures: tuple[str, ...],
    strata: tuple[str, ...],
) -> np.ndarray:
    """Build numeric columns followed by frozen architecture/stratum indicators."""
    if not rows or not architectures or not strata:
        raise ValueError("rows, architectures, and strata cannot be empty")
    output = []
    for row in rows:
        architecture = str(row["architecture"])
        stratum = str(row["stratum"])
        if architecture not in architectures or stratum not in strata:
            raise ValueError("row contains an unknown architecture or stratum")
        output.append(
            [_numeric_value(row, name) for name in numeric_features]
            + [float(architecture == value) for value in architectures[1:]]
            + [float(stratum == value) for value in strata[1:]]
        )
    return np.asarray(output, dtype=float)


def fit_architecture_ols(
    rows: list[dict], *, target: str, numeric_features: tuple[str, ...]
) -> dict:
    """Fit and serialize a standardized OLS with fixed architecture/stratum effects."""
    architectures = tuple(sorted({str(row["architecture"]) for row in rows}))
    strata = tuple(sorted({str(row["stratum"]) for row in rows}))
    matrix = architecture_design_matrix(
        rows,
        numeric_features=numeric_features,
        architectures=architectures,
        strata=strata,
    )
    targets = np.asarray([float(row[target]) for row in rows], dtype=float)
    if np.any(~np.isfinite(targets)):
        raise ValueError("targets must be finite")
    raw_scale = matrix.std(axis=0)
    if np.any(raw_scale == 0.0):
        raise ValueError("constant design columns are not allowed")
    scaler = StandardScaler().fit(matrix)
    model = LinearRegression().fit(scaler.transform(matrix), targets)
    return {
        "kind": "architecture_stratum_ols",
        "target": target,
        "numeric_features": list(numeric_features),
        "architectures": list(architectures),
        "strata": list(strata),
        "columns": list(numeric_features)
        + [f"architecture[{value}]" for value in architectures[1:]]
        + [f"stratum[{value}]" for value in strata[1:]],
        "scaler_mean": scaler.mean_.tolist(),
        "scaler_scale": scaler.scale_.tolist(),
        "intercept": float(model.intercept_),
        "coefficients": np.asarray(model.coef_, dtype=float).tolist(),
        "training_corpora": sorted({str(row["dataset"]) for row in rows}),
    }


def predict_architecture_ols(artifact: dict, rows: list[dict]) -> np.ndarray:
    if artifact.get("kind") != "architecture_stratum_ols":
        raise ValueError("artifact is not an architecture-stratum OLS")
    matrix = architecture_design_matrix(
        rows,
        numeric_features=tuple(artifact["numeric_features"]),
        architectures=tuple(artifact["architectures"]),
        strata=tuple(artifact["strata"]),
    )
    mean = np.asarray(artifact["scaler_mean"], dtype=float)
    scale = np.asarray(artifact["scaler_scale"], dtype=float)
    coefficients = np.asarray(artifact["coefficients"], dtype=float)
    if np.any(scale <= 0.0) or matrix.shape[1] != len(coefficients):
        raise ValueError("frozen OLS dimensions or scales are invalid")
    return float(artifact["intercept"]) + ((matrix - mean) / scale) @ coefficients


def grouped_loco_predictions(
    rows: list[dict], *, target: str, numeric_features: tuple[str, ...]
) -> np.ndarray:
    """Predict each corpus from an OLS fit that excludes all its architecture rows."""
    datasets = sorted({str(row["dataset"]) for row in rows})
    if len(datasets) < 3:
        raise ValueError("grouped LOCO requires at least three corpora")
    predictions = np.empty(len(rows), dtype=float)
    for dataset in datasets:
        train = [row for row in rows if str(row["dataset"]) != dataset]
        test_indices = [
            index for index, row in enumerate(rows) if str(row["dataset"]) == dataset
        ]
        artifact = fit_architecture_ols(
            train, target=target, numeric_features=numeric_features
        )
        predictions[test_indices] = predict_architecture_ols(
            artifact, [rows[index] for index in test_indices]
        )
    return predictions


def prediction_scores(actual: np.ndarray, predicted: np.ndarray) -> dict:
    """Return finite RMSE, MAE, and ordinary held-out R-squared."""
    actual_values = np.asarray(actual, dtype=float)
    predicted_values = np.asarray(predicted, dtype=float)
    if actual_values.shape != predicted_values.shape or np.any(
        ~np.isfinite(actual_values) | ~np.isfinite(predicted_values)
    ):
        raise ValueError("actual and predicted values must be aligned and finite")
    residual = actual_values - predicted_values
    denominator = float(np.sum((actual_values - actual_values.mean()) ** 2))
    return {
        "rmse": math.sqrt(float(np.mean(residual**2))),
        "mae": float(np.mean(np.abs(residual))),
        "r_squared": 1.0 - float(np.sum(residual**2)) / denominator
        if denominator > 0.0
        else None,
    }


def stratified_corpus_bootstrap_improvement(
    rows: list[dict],
    baseline: np.ndarray,
    matched: np.ndarray,
    *,
    samples: int,
    seed: int,
) -> dict:
    """Resample corpora within strata while retaining every architecture row."""
    actual = np.asarray([float(row["actual"]) for row in rows], dtype=float)
    base = np.asarray(baseline, dtype=float)
    test = np.asarray(matched, dtype=float)
    if actual.shape != base.shape or actual.shape != test.shape or samples < 100:
        raise ValueError("aligned predictions and at least 100 samples are required")
    dataset_rows: dict[str, list[int]] = {}
    dataset_strata: dict[str, str] = {}
    for index, row in enumerate(rows):
        dataset = str(row["dataset"])
        stratum = str(row["stratum"])
        dataset_rows.setdefault(dataset, []).append(index)
        if dataset in dataset_strata and dataset_strata[dataset] != stratum:
            raise ValueError("one corpus cannot belong to multiple strata")
        dataset_strata[dataset] = stratum
    strata = sorted(set(dataset_strata.values()))
    blocks = [
        sorted(dataset for dataset, value in dataset_strata.items() if value == stratum)
        for stratum in strata
    ]
    if len({len(block) for block in blocks}) != 1:
        raise ValueError("the stratified bootstrap requires balanced corpus blocks")

    def rmse(predicted: np.ndarray, indices: np.ndarray) -> float:
        return math.sqrt(float(np.mean((actual[indices] - predicted[indices]) ** 2)))

    all_indices = np.arange(len(rows))
    point_base = rmse(base, all_indices)
    point_matched = rmse(test, all_indices)
    rng = np.random.default_rng(seed)
    improvements = np.empty(samples, dtype=float)
    for draw in range(samples):
        selected = []
        for block in blocks:
            sampled = rng.choice(block, size=len(block), replace=True)
            for dataset in sampled:
                selected.extend(dataset_rows[str(dataset)])
        indices = np.asarray(selected, dtype=int)
        sampled_base = rmse(base, indices)
        sampled_matched = rmse(test, indices)
        improvements[draw] = (sampled_base - sampled_matched) / sampled_base
    return {
        "baseline_rmse": point_base,
        "matched_rmse": point_matched,
        "relative_rmse_improvement": (point_base - point_matched) / point_base,
        "stratified_corpus_bootstrap_95_interval": [
            float(np.quantile(improvements, 0.025)),
            float(np.quantile(improvements, 0.975)),
        ],
        "samples": samples,
        "seed": seed,
        "strata": strata,
    }
