"""Low-dimensional paired architecture-contrast analysis utilities."""

from __future__ import annotations

import math

import numpy as np
from scipy.stats import pearsonr, rankdata, spearmanr


def association(x: np.ndarray, y: np.ndarray) -> dict:
    """Return two-sided Pearson and Spearman associations for aligned vectors."""
    feature = np.asarray(x, dtype=float)
    target = np.asarray(y, dtype=float)
    if feature.shape != target.shape or feature.ndim != 1 or len(feature) < 3:
        raise ValueError("association vectors must be aligned and contain 3+ rows")
    if np.any(~np.isfinite(feature) | ~np.isfinite(target)):
        raise ValueError("association vectors must be finite")
    if feature.std() == 0.0 or target.std() == 0.0:
        raise ValueError("association vectors must be nonconstant")
    pearson = pearsonr(feature, target)
    spearman = spearmanr(feature, target)
    slope = float(np.sum((feature - feature.mean()) * (target - target.mean()))) / float(
        np.sum((feature - feature.mean()) ** 2)
    )
    return {
        "pearson_r": float(pearson.statistic),
        "pearson_two_sided_p": float(pearson.pvalue),
        "spearman_rho": float(spearman.statistic),
        "spearman_two_sided_p": float(spearman.pvalue),
        "ols_slope_raw_units": slope,
        "expected_positive_sign": slope > 0.0,
    }


def _stratum_blocks(rows: list[dict]) -> list[np.ndarray]:
    strata = sorted({str(row["stratum"]) for row in rows})
    blocks = [
        np.asarray(
            [index for index, row in enumerate(rows) if row["stratum"] == stratum],
            dtype=int,
        )
        for stratum in strata
    ]
    if not blocks or min(map(len, blocks)) < 2:
        raise ValueError("each stratum needs at least two corpus rows")
    return blocks


def stratified_association_bootstrap(
    rows: list[dict],
    *,
    feature: str,
    target: str,
    samples: int,
    seed: int,
) -> dict:
    """Bootstrap Pearson correlation and raw OLS slope by corpus within stratum."""
    if samples < 100:
        raise ValueError("at least 100 bootstrap samples are required")
    x = np.asarray([float(row[feature]) for row in rows], dtype=float)
    y = np.asarray([float(row[target]) for row in rows], dtype=float)
    blocks = _stratum_blocks(rows)
    rng = np.random.default_rng(seed)
    correlations = np.empty(samples, dtype=float)
    slopes = np.empty(samples, dtype=float)
    chunk_size = 5_000
    for start in range(0, samples, chunk_size):
        stop = min(samples, start + chunk_size)
        count = stop - start
        selected = np.concatenate(
            [rng.choice(block, size=(count, len(block)), replace=True) for block in blocks],
            axis=1,
        )
        bx = x[selected]
        by = y[selected]
        bx_centered = bx - bx.mean(axis=1, keepdims=True)
        by_centered = by - by.mean(axis=1, keepdims=True)
        covariance = np.sum(bx_centered * by_centered, axis=1)
        x_square = np.sum(bx_centered**2, axis=1)
        y_square = np.sum(by_centered**2, axis=1)
        valid = (x_square > 0.0) & (y_square > 0.0)
        correlations[start:stop] = np.divide(
            covariance,
            np.sqrt(x_square * y_square),
            out=np.full(count, np.nan),
            where=valid,
        )
        slopes[start:stop] = np.divide(
            covariance,
            x_square,
            out=np.full(count, np.nan),
            where=x_square > 0.0,
        )
    if np.mean(np.isfinite(correlations) & np.isfinite(slopes)) < 0.99:
        raise ValueError("too many degenerate bootstrap samples")
    return {
        "pearson_95_interval": [
            float(np.nanquantile(correlations, 0.025)),
            float(np.nanquantile(correlations, 0.975)),
        ],
        "ols_slope_95_interval": [
            float(np.nanquantile(slopes, 0.025)),
            float(np.nanquantile(slopes, 0.975)),
        ],
        "samples": samples,
        "seed": seed,
        "resampling": "corpora within source stratum",
    }


def blocked_spearman_permutation(
    rows: list[dict],
    *,
    feature: str,
    target: str,
    permutations: int,
    seed: int,
) -> dict:
    """Two-sided permutation test for mean within-stratum Spearman correlation."""
    if permutations < 100:
        raise ValueError("at least 100 permutations are required")
    blocks = _stratum_blocks(rows)
    x = np.asarray([float(row[feature]) for row in rows], dtype=float)
    y = np.asarray([float(row[target]) for row in rows], dtype=float)
    strata = sorted({str(row["stratum"]) for row in rows})
    x_ranks = []
    y_ranks = []
    observed_by_stratum = {}
    for stratum, block in zip(strata, blocks, strict=True):
        xr = rankdata(x[block]).astype(float)
        yr = rankdata(y[block]).astype(float)
        xr -= xr.mean()
        yr -= yr.mean()
        denominator = math.sqrt(float(xr @ xr) * float(yr @ yr))
        if denominator == 0.0:
            raise ValueError("blocked Spearman requires nonconstant within-stratum data")
        rho = float(xr @ yr / denominator)
        x_ranks.append(xr)
        y_ranks.append(yr)
        observed_by_stratum[stratum] = rho
    observed = float(np.mean(list(observed_by_stratum.values())))
    rng = np.random.default_rng(seed)
    extreme = 0
    chunk_size = 5_000
    for start in range(0, permutations, chunk_size):
        count = min(chunk_size, permutations - start)
        block_null = []
        for xr, yr in zip(x_ranks, y_ranks, strict=True):
            orders = np.argsort(rng.random((count, len(yr))), axis=1)
            permuted = yr[orders]
            denominator = math.sqrt(float(xr @ xr) * float(yr @ yr))
            block_null.append((permuted @ xr) / denominator)
        null = np.mean(np.column_stack(block_null), axis=1)
        extreme += int(np.count_nonzero(np.abs(null) >= abs(observed)))
    return {
        "statistic": "mean within-stratum Spearman rho",
        "mean_rho": observed,
        "within_stratum_rhos": observed_by_stratum,
        "two_sided_permutation_p": float((extreme + 1) / (permutations + 1)),
        "permutations": permutations,
        "seed": seed,
    }


def fit_standardized_contrast(x: np.ndarray, y: np.ndarray) -> dict:
    """Fit an intercept plus one standardized contrast feature."""
    feature = np.asarray(x, dtype=float)
    target = np.asarray(y, dtype=float)
    if feature.shape != target.shape or feature.ndim != 1 or len(feature) < 3:
        raise ValueError("contrast fit requires aligned one-dimensional vectors")
    mean = float(feature.mean())
    scale = float(feature.std())
    if scale <= 0.0:
        raise ValueError("contrast feature must be nonconstant")
    z = (feature - mean) / scale
    coefficient = float(np.sum(z * (target - target.mean())) / np.sum(z**2))
    return {
        "kind": "standardized_one_feature_ols",
        "feature_mean": mean,
        "feature_scale": scale,
        "intercept": float(target.mean()),
        "coefficient": coefficient,
    }


def predict_standardized_contrast(model: dict, x: np.ndarray) -> np.ndarray:
    feature = np.asarray(x, dtype=float)
    if model.get("kind") != "standardized_one_feature_ols":
        raise ValueError("invalid standardized contrast model")
    return float(model["intercept"]) + float(model["coefficient"]) * (
        feature - float(model["feature_mean"])
    ) / float(model["feature_scale"])


def holm_adjust(p_values: dict[str, float]) -> dict[str, float]:
    """Return monotone Holm-adjusted p-values while retaining input labels."""
    if not p_values or any(not 0.0 <= float(value) <= 1.0 for value in p_values.values()):
        raise ValueError("Holm adjustment requires p-values in [0,1]")
    ordered = sorted(p_values, key=p_values.get)
    adjusted: dict[str, float] = {}
    running = 0.0
    count = len(ordered)
    for rank, label in enumerate(ordered):
        running = max(running, (count - rank) * float(p_values[label]))
        adjusted[label] = min(1.0, running)
    return {label: adjusted[label] for label in p_values}
