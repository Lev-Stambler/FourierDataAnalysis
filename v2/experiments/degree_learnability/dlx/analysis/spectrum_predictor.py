"""Corpus-grouped low-data predictors for Fourier learnability experiments."""

from __future__ import annotations

import math
from collections.abc import Callable

import numpy as np
from sklearn.base import RegressorMixin
from sklearn.cross_decomposition import PLSRegression
from sklearn.gaussian_process import GaussianProcessRegressor
from sklearn.gaussian_process.kernels import ConstantKernel, Matern, WhiteKernel
from sklearn.linear_model import ElasticNet, LinearRegression, Ridge
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler
from sklearn.svm import SVR

FOURIER_FEATURES = (
    "constant_energy",
    "resolved_nonconstant_energy",
    "low_degree_concentration_best_pair",
    "mean_nonconstant_degree_best_pair",
    "max_degree2_incremental_energy",
    "energy_weighted_log_radius",
    "locality_radius_50",
    "locality_radius_90",
    "radial_spectral_entropy_bits",
    "spectral_entropy_upper_bits_best_pair",
)
FOURIER_CORE_FEATURES = (
    "resolved_nonconstant_energy",
    "energy_weighted_log_radius",
    "mean_nonconstant_degree_best_pair",
    "radial_spectral_entropy_bits",
)


def _value(row: dict, name: str) -> float:
    value = row.get(name, row.get("features", {}).get(name))
    if value is None:
        raise ValueError(f"row {row.get('dataset')} lacks finite feature {name}")
    result = float(value)
    if not math.isfinite(result):
        raise ValueError(f"row {row.get('dataset')} has non-finite feature {name}")
    if name in {"locality_radius_50", "locality_radius_90"}:
        return math.log2(1.0 + result)
    return result


def design_matrix(
    rows: list[dict], feature_names: tuple[str, ...], configurations: tuple[str, ...]
) -> np.ndarray:
    """Build numeric dataset features followed by fixed configuration indicators."""
    values = []
    known = set(configurations)
    for row in rows:
        configuration = row["configuration"]
        if configuration not in known:
            raise ValueError(f"unknown configuration: {configuration}")
        values.append(
            [_value(row, name) for name in feature_names]
            + [float(configuration == candidate) for candidate in configurations[1:]]
        )
    return np.asarray(values, dtype=float)


def _candidate_models(
    kind: str, n_features: int
) -> list[tuple[dict, Callable[[], RegressorMixin]]]:
    if kind == "ols":
        return [({}, lambda: make_pipeline(StandardScaler(), LinearRegression()))]
    if kind == "ridge":
        return [
            (
                {"alpha": alpha},
                lambda alpha=alpha: make_pipeline(StandardScaler(), Ridge(alpha=alpha)),
            )
            for alpha in (0.01, 0.1, 1.0, 10.0, 100.0)
        ]
    if kind == "elastic_net":
        return [
            (
                {"alpha": alpha, "l1_ratio": ratio},
                lambda alpha=alpha, ratio=ratio: make_pipeline(
                    StandardScaler(),
                    ElasticNet(
                        alpha=alpha,
                        l1_ratio=ratio,
                        max_iter=20_000,
                        random_state=0,
                    ),
                ),
            )
            for alpha in (0.001, 0.01, 0.1)
            for ratio in (0.1, 0.5, 0.9)
        ]
    if kind == "pls":
        return [
            (
                {"n_components": components},
                lambda components=components: PLSRegression(
                    n_components=components, scale=True
                ),
            )
            for components in range(1, min(4, n_features + 1))
        ]
    if kind == "svr_rbf":
        return [
            (
                {"C": c, "gamma": gamma, "epsilon": epsilon},
                lambda c=c, gamma=gamma, epsilon=epsilon: make_pipeline(
                    StandardScaler(), SVR(C=c, gamma=gamma, epsilon=epsilon)
                ),
            )
            for c in (0.1, 1.0, 10.0)
            for gamma in ("scale", 0.1)
            for epsilon in (0.01, 0.05)
        ]
    if kind == "gaussian_process":
        kernel = ConstantKernel(1.0) * Matern(length_scale=1.0, nu=1.5) + WhiteKernel(
            noise_level=0.01
        )
        return [
            (
                {"kernel": "constant*matern32+white"},
                lambda: make_pipeline(
                    StandardScaler(),
                    GaussianProcessRegressor(
                        kernel=kernel,
                        normalize_y=True,
                        optimizer=None,
                        n_restarts_optimizer=0,
                        random_state=0,
                    ),
                ),
            )
        ]
    raise ValueError(f"unknown model kind: {kind}")


def _group_rmse(actual: np.ndarray, predicted: np.ndarray, groups: np.ndarray) -> float:
    per_group = []
    for group in sorted(set(groups.tolist())):
        selected = groups == group
        per_group.append(
            math.sqrt(mean_squared_error(actual[selected], predicted[selected]))
        )
    return float(np.mean(per_group))


def _select_candidate(
    x: np.ndarray, y: np.ndarray, groups: np.ndarray, kind: str
) -> tuple[dict, Callable[[], RegressorMixin], float]:
    unique_groups = sorted(set(groups.tolist()))
    if len(unique_groups) < 3:
        raise ValueError("nested grouped selection requires at least three corpora")
    candidates = _candidate_models(kind, x.shape[1])
    scored = []
    for order, (parameters, factory) in enumerate(candidates):
        actual_parts = []
        predicted_parts = []
        group_parts = []
        for held_out in unique_groups:
            train = groups != held_out
            score = ~train
            model = factory()
            model.fit(x[train], y[train])
            predicted = np.asarray(model.predict(x[score])).reshape(-1)
            actual_parts.append(y[score])
            predicted_parts.append(predicted)
            group_parts.append(groups[score])
        actual = np.concatenate(actual_parts)
        predicted = np.concatenate(predicted_parts)
        scored.append(
            (
                _group_rmse(actual, predicted, np.concatenate(group_parts)),
                order,
                parameters,
                factory,
            )
        )
    rmse, _, parameters, factory = min(scored, key=lambda item: (item[0], item[1]))
    return parameters, factory, rmse


def nested_loco_predictions(
    rows: list[dict],
    *,
    target: str,
    feature_names: tuple[str, ...],
    kind: str,
) -> dict:
    """Nested leave-one-corpus-out predictions with no seed/variant leakage."""
    if len({row["dataset"] for row in rows}) < 4:
        raise ValueError("outer LOCO evaluation requires at least four corpora")
    configurations = tuple(sorted({row["configuration"] for row in rows}))
    x = design_matrix(rows, feature_names, configurations)
    y = np.asarray([float(row[target]) for row in rows], dtype=float)
    groups = np.asarray([row["dataset"] for row in rows], dtype=object)
    predictions = np.empty(len(rows), dtype=float)
    selections = {}
    for held_out in sorted(set(groups.tolist())):
        train = groups != held_out
        score = ~train
        parameters, factory, inner_rmse = _select_candidate(
            x[train], y[train], groups[train], kind
        )
        model = factory()
        model.fit(x[train], y[train])
        predictions[score] = np.asarray(model.predict(x[score])).reshape(-1)
        selections[held_out] = {
            "parameters": parameters,
            "inner_group_balanced_rmse": inner_rmse,
        }
    return {
        "model_kind": kind,
        "target": target,
        "feature_names": list(feature_names),
        "configurations": list(configurations),
        "predictions": [
            {
                "dataset": row["dataset"],
                "configuration": row["configuration"],
                "actual": float(actual),
                "predicted": float(predicted),
            }
            for row, actual, predicted in zip(rows, y, predictions, strict=True)
        ],
        "rmse": float(math.sqrt(mean_squared_error(y, predictions))),
        "group_balanced_rmse": _group_rmse(y, predictions, groups),
        "mae": float(mean_absolute_error(y, predictions)),
        "r2": float(r2_score(y, predictions)),
        "outer_selections": selections,
    }


def fit_frozen_ridge(
    rows: list[dict], *, target: str, feature_names: tuple[str, ...]
) -> dict:
    """Fit and serialize the primary ridge predictor after grouped alpha selection."""
    configurations = tuple(sorted({row["configuration"] for row in rows}))
    x = design_matrix(rows, feature_names, configurations)
    y = np.asarray([float(row[target]) for row in rows], dtype=float)
    groups = np.asarray([row["dataset"] for row in rows], dtype=object)
    parameters, _, selection_rmse = _select_candidate(x, y, groups, "ridge")
    scaler = StandardScaler().fit(x)
    x_scaled = scaler.transform(x)
    model = Ridge(alpha=parameters["alpha"]).fit(x_scaled, y)
    return {
        "kind": "ridge",
        "target": target,
        "feature_names": list(feature_names),
        "configurations": list(configurations),
        "alpha": parameters["alpha"],
        "selection_group_balanced_rmse": selection_rmse,
        "scaler_mean": scaler.mean_.tolist(),
        "scaler_scale": scaler.scale_.tolist(),
        "intercept": float(model.intercept_),
        "coefficients": model.coef_.tolist(),
        "training_corpora": sorted(set(groups.tolist())),
    }


def predict_frozen_ridge(artifact: dict, rows: list[dict]) -> np.ndarray:
    if artifact.get("kind") != "ridge":
        raise ValueError("artifact is not a frozen ridge predictor")
    x = design_matrix(
        rows,
        tuple(artifact["feature_names"]),
        tuple(artifact["configurations"]),
    )
    mean = np.asarray(artifact["scaler_mean"], dtype=float)
    scale = np.asarray(artifact["scaler_scale"], dtype=float)
    coefficients = np.asarray(artifact["coefficients"], dtype=float)
    return artifact["intercept"] + ((x - mean) / scale) @ coefficients


def fit_frozen_ols(
    rows: list[dict],
    *,
    target: str,
    feature_names: tuple[str, ...],
    reject_constant_features: bool = True,
) -> dict:
    """Fit the preregistered standardized OLS model without model selection."""
    if not rows:
        raise ValueError("at least one training row is required")
    configurations = tuple(sorted({row["configuration"] for row in rows}))
    x = design_matrix(rows, feature_names, configurations)
    y = np.asarray([float(row[target]) for row in rows], dtype=float)
    if x.shape[1] == 0:
        mean = np.empty(0, dtype=float)
        scale = np.empty(0, dtype=float)
        coefficients = np.empty(0, dtype=float)
        intercept = float(y.mean())
    else:
        raw_scale = x.std(axis=0)
        if reject_constant_features and np.any(raw_scale == 0.0):
            indices = np.flatnonzero(raw_scale == 0.0).tolist()
            raise ValueError(f"constant design columns are not allowed: {indices}")
        scaler = StandardScaler().fit(x)
        model = LinearRegression().fit(scaler.transform(x), y)
        mean = scaler.mean_
        scale = scaler.scale_
        coefficients = np.asarray(model.coef_, dtype=float)
        intercept = float(model.intercept_)
    return {
        "kind": "ols",
        "target": target,
        "feature_names": list(feature_names),
        "configurations": list(configurations),
        "scaler_mean": mean.tolist(),
        "scaler_scale": scale.tolist(),
        "intercept": intercept,
        "coefficients": coefficients.tolist(),
        "training_corpora": sorted({row["dataset"] for row in rows}),
    }


def predict_frozen_ols(artifact: dict, rows: list[dict]) -> np.ndarray:
    """Apply a serialized OLS artifact without refitting or feature selection."""
    if artifact.get("kind") != "ols":
        raise ValueError("artifact is not a frozen OLS predictor")
    x = design_matrix(
        rows,
        tuple(artifact["feature_names"]),
        tuple(artifact["configurations"]),
    )
    coefficients = np.asarray(artifact["coefficients"], dtype=float)
    if x.shape[1] == 0:
        return np.full(len(rows), float(artifact["intercept"]))
    mean = np.asarray(artifact["scaler_mean"], dtype=float)
    scale = np.asarray(artifact["scaler_scale"], dtype=float)
    if np.any(scale <= 0.0):
        raise ValueError("frozen OLS artifact has invalid scaler values")
    return float(artifact["intercept"]) + ((x - mean) / scale) @ coefficients


def paired_corpus_bootstrap_improvement(
    actual: np.ndarray,
    baseline: np.ndarray,
    challenger: np.ndarray,
    *,
    samples: int = 10_000,
    seed: int = 2603,
) -> dict:
    """Bootstrap relative RMSE improvement for one independent row per corpus."""
    y = np.asarray(actual, dtype=float)
    base = np.asarray(baseline, dtype=float)
    test = np.asarray(challenger, dtype=float)
    if y.ndim != 1 or y.shape != base.shape or y.shape != test.shape or len(y) < 4:
        raise ValueError(
            "actual and predictions must be aligned vectors of length >= 4"
        )
    if samples < 100:
        raise ValueError("at least 100 bootstrap samples are required")

    def rmse(predicted: np.ndarray, indices: np.ndarray | None = None) -> float:
        errors = predicted - y
        if indices is not None:
            errors = errors[indices]
        return math.sqrt(float(np.mean(errors**2)))

    baseline_rmse = rmse(base)
    challenger_rmse = rmse(test)
    if baseline_rmse == 0.0:
        raise ValueError("relative improvement is undefined for zero baseline RMSE")
    rng = np.random.default_rng(seed)
    improvements = np.empty(samples, dtype=float)
    for index in range(samples):
        selected = rng.integers(0, len(y), size=len(y))
        sampled_baseline = rmse(base, selected)
        sampled_challenger = rmse(test, selected)
        improvements[index] = (
            (sampled_baseline - sampled_challenger) / sampled_baseline
            if sampled_baseline
            else 0.0
        )
    return {
        "baseline_rmse": baseline_rmse,
        "challenger_rmse": challenger_rmse,
        "relative_rmse_improvement": (baseline_rmse - challenger_rmse) / baseline_rmse,
        "corpus_bootstrap_95_interval": [
            float(np.quantile(improvements, 0.025)),
            float(np.quantile(improvements, 0.975)),
        ],
        "bootstrap_samples": samples,
        "bootstrap_seed": seed,
    }
