"""Sampled higher-order projection-energy curves with finite-sample bounds.

This module deliberately avoids enumerating q^k categorical coefficients.  It
samples nested supports, estimates the conditional collision energy

    E ||P(Y=. | X_A)||_2^2

by two-fold cross-fitting, and reports both a Hoeffding-style evaluation interval
and the mass of evaluation contexts observed in the opposite fold.  The curve is
a scalable, basis-invariant cumulative degree diagnostic.  Its increments are not
claimed to be the exact dependent-input Fourier level weights.
"""

from __future__ import annotations

import math

import numpy as np


def invert_product_reference_degree_curve(
    mean_projection_energies: list[float], n_coordinates: int
) -> list[float]:
    """Invert random-subset projection means into product-reference level weights.

    If coordinate subspaces are orthogonal (in particular under a product input
    measure), M_k for a uniform size-k support satisfies

        M_k = sum_{j<=k} binom(k,j) / binom(d,j) * W_j.

    Natural text violates the product assumption, so callers must retain the
    product-reference label and inspect negative levels rather than clipping them.
    """
    if not mean_projection_energies:
        raise ValueError("at least degree-zero energy is required")
    max_degree = len(mean_projection_energies) - 1
    if not max_degree <= n_coordinates:
        raise ValueError("curve degree exceeds n_coordinates")
    weights = []
    for degree, mean_energy in enumerate(mean_projection_energies):
        explained = sum(
            math.comb(degree, lower) / math.comb(n_coordinates, lower) * weights[lower]
            for lower in range(degree)
        )
        weights.append(math.comb(n_coordinates, degree) * (mean_energy - explained))
    return [float(value) for value in weights]


def _lookup_counts(
    sorted_values: np.ndarray, counts: np.ndarray, query: np.ndarray
) -> np.ndarray:
    indices = np.searchsorted(sorted_values, query)
    valid = indices < len(sorted_values)
    output = np.zeros(len(query), dtype=np.int64)
    if np.any(valid):
        valid_positions = np.flatnonzero(valid)
        matched = sorted_values[indices[valid]] == query[valid]
        positions = valid_positions[matched]
        output[positions] = counts[indices[positions]]
    return output


def _one_direction(
    keys: np.ndarray,
    targets: np.ndarray,
    train: np.ndarray,
    evaluate: np.ndarray,
) -> tuple[float, float, int]:
    train_keys = keys[train]
    train_targets = targets[train]
    eval_keys = keys[evaluate]
    eval_targets = targets[evaluate]
    unique_keys, key_counts = np.unique(train_keys, return_counts=True)
    train_pairs = (train_keys << np.uint64(8)) | train_targets.astype(np.uint64)
    unique_pairs, pair_counts = np.unique(train_pairs, return_counts=True)
    denominators = _lookup_counts(unique_keys, key_counts, eval_keys)
    eval_pairs = (eval_keys << np.uint64(8)) | eval_targets.astype(np.uint64)
    numerators = _lookup_counts(unique_pairs, pair_counts, eval_pairs)
    probabilities = np.divide(
        numerators,
        denominators,
        out=np.zeros(len(eval_keys), dtype=float),
        where=denominators > 0,
    )
    return float(probabilities.mean()), float(np.mean(denominators > 0)), len(eval_keys)


def crossfit_conditional_collision(
    keys: np.ndarray,
    targets: np.ndarray,
    fold_ids: np.ndarray,
    *,
    delta: float = 0.01,
) -> dict:
    """Estimate conditional square energy for one fixed support."""
    key_values = np.asarray(keys, dtype=np.uint64)
    y = np.asarray(targets, dtype=np.uint64)
    folds = np.asarray(fold_ids, dtype=np.int8)
    if key_values.shape != y.shape or y.shape != folds.shape:
        raise ValueError("keys, targets, and fold_ids must have matching shapes")
    if len(y) < 100 or set(np.unique(folds)) != {0, 1}:
        raise ValueError("two nonempty folds and at least 100 examples are required")
    if np.any(y > 255):
        raise ValueError("packed collision estimator requires targets in [0,255]")
    if not 0.0 < delta < 1.0:
        raise ValueError("delta must lie in (0,1)")

    directions = []
    for held_out in (0, 1):
        directions.append(
            _one_direction(key_values, y, folds != held_out, folds == held_out)
        )
    total = sum(item[2] for item in directions)
    estimate = sum(item[0] * item[2] for item in directions) / total
    coverage = sum(item[1] * item[2] for item in directions) / total
    # Conditional on the opposite-fold table, every evaluation score is in [0,1].
    # A union bound over the two held-out folds yields this weighted half-width.
    half_width = (
        sum(
            item[2] * math.sqrt(math.log(4.0 / delta) / (2.0 * item[2]))
            for item in directions
        )
        / total
    )
    return {
        "conditional_collision_energy": float(estimate),
        "opposite_fold_context_coverage": float(coverage),
        "evaluation_hoeffding_half_width": float(half_width),
        "evaluation_hoeffding_interval": [
            float(max(0.0, estimate - half_width)),
            float(min(1.0, estimate + half_width)),
        ],
        "coverage_identification_interval": [
            float(estimate),
            float(min(1.0, estimate + 1.0 - coverage)),
        ],
        "n_evaluation_scores": int(total),
    }


def geometric_sampled_features(
    chains: list[list[dict]],
    coordinate_radii: tuple[int, ...],
    *,
    feature_degree: int = 3,
    q: int = 256,
) -> dict[str, float]:
    """Summarize nested-chain energy, degree, and radius with a fixed formula."""
    if not chains or feature_degree < 1 or q < 2:
        raise ValueError("chains, feature_degree, and q must be positive")
    radii = tuple(int(value) for value in coordinate_radii)
    positive_energy = 0.0
    weighted_degree = 0.0
    weighted_geometry = 0.0
    total_square_energy = 0.0
    terminal_nonconstant = 0.0
    for chain in chains:
        if len(chain) <= feature_degree:
            raise ValueError("chain does not reach the requested feature degree")
        total_square_energy += chain[feature_degree]["conditional_collision_energy"]
        terminal_nonconstant += max(
            chain[feature_degree]["conditional_collision_energy"]
            - chain[0]["conditional_collision_energy"],
            0.0,
        )
        for degree in range(1, feature_degree + 1):
            increment = max(
                chain[degree]["conditional_collision_energy"]
                - chain[degree - 1]["conditional_collision_energy"],
                0.0,
            )
            support = chain[degree]["support_columns"]
            radius = max(radii[index] for index in support)
            if radius < degree:
                raise ValueError("support radius cannot be smaller than its degree")
            positive_energy += increment
            weighted_degree += degree * increment
            weighted_geometry += increment * math.log2(
                math.comb(radius, degree) * ((q - 1) ** degree)
            )
    return {
        "sampled_nonconstant_energy_through_degree3": float(
            terminal_nonconstant / len(chains)
        ),
        "sampled_mean_degree_through_degree3": float(
            weighted_degree / positive_energy if positive_energy else 0.0
        ),
        "sampled_geometric_complexity_through_degree3": float(
            weighted_geometry / total_square_energy if total_square_energy else 0.0
        ),
    }


def marginal_locality_features(
    chains: list[list[dict]],
    coordinate_radii: tuple[int, ...],
    *,
    feature_degree: int = 3,
) -> dict[str, float]:
    """Attribute clipped chain increments to the coordinate added at each step."""
    if not chains or feature_degree < 1:
        raise ValueError("chains and feature_degree must be positive")
    radii = tuple(int(value) for value in coordinate_radii)
    if not radii or any(value < 1 for value in radii):
        raise ValueError("coordinate_radii must be positive")
    radial_energy: dict[int, float] = {}
    for chain in chains:
        if len(chain) <= feature_degree:
            raise ValueError("chain does not reach the requested feature degree")
        for degree in range(1, feature_degree + 1):
            support = chain[degree]["support_columns"]
            if len(support) != degree:
                raise ValueError("chain support length does not equal its degree")
            coordinate = int(support[-1])
            if coordinate < 0 or coordinate >= len(radii):
                raise ValueError("chain support coordinate is out of range")
            increment = max(
                float(chain[degree]["conditional_collision_energy"])
                - float(chain[degree - 1]["conditional_collision_energy"]),
                0.0,
            )
            radius = radii[coordinate]
            radial_energy[radius] = radial_energy.get(radius, 0.0) + increment
    total = sum(radial_energy.values())
    if total <= 0.0:
        raise ValueError("marginal locality is undefined without positive energy")

    def quantile(probability: float) -> int:
        threshold = probability * total
        cumulative = 0.0
        for radius, energy in sorted(radial_energy.items()):
            cumulative += energy
            if cumulative >= threshold:
                return radius
        raise AssertionError("positive radial energy did not reach its quantile")

    return {
        "sampled_marginal_log1p_radius_through_degree3": float(
            sum(
                energy * math.log2(1.0 + radius)
                for radius, energy in radial_energy.items()
            )
            / total
        ),
        "sampled_marginal_radius50_through_degree3": float(quantile(0.5)),
        "sampled_marginal_radius90_through_degree3": float(quantile(0.9)),
        "sampled_marginal_positive_energy_through_degree3": float(total / len(chains)),
    }


def sampled_nested_degree_profile(
    contexts: np.ndarray,
    targets: np.ndarray,
    *,
    max_degree: int = 6,
    n_chains: int = 48,
    seed: int = 0,
    delta: float = 0.05,
    q: int = 256,
    coordinate_radii: tuple[int, ...] | None = None,
    include_chains: bool = False,
    include_product_reference: bool = False,
) -> dict:
    """Estimate an average cumulative projection curve over random support chains.

    The compact summary is the stable public artifact. ``include_chains`` retains
    the lossless per-support audit rows, while ``include_product_reference`` adds
    the product-measure-only binomial inversion.
    """
    x = np.asarray(contexts)
    y = np.asarray(targets)
    if x.ndim != 2 or len(x) != len(y):
        raise ValueError("contexts must be (n,d) and align with targets")
    if not 2 <= q <= 256:
        raise ValueError("q must lie in [2,256]")
    if x.dtype.kind not in "uib" or np.any(x < 0) or np.any(x >= q):
        raise ValueError("contexts must contain categorical values in [0,q)")
    if y.dtype.kind not in "uib" or np.any(y < 0) or np.any(y >= q):
        raise ValueError("targets must contain categorical values in [0,q)")
    if not 1 <= max_degree <= min(7, x.shape[1]):
        raise ValueError("max_degree must be between 1 and min(7,n_coordinates)")
    if n_chains < 2:
        raise ValueError("n_chains must be at least two")
    if not 0.0 < delta < 1.0:
        raise ValueError("delta must lie in (0,1)")
    radii = (
        tuple(range(1, x.shape[1] + 1))
        if coordinate_radii is None
        else tuple(int(value) for value in coordinate_radii)
    )
    if len(radii) != x.shape[1] or any(value < 1 for value in radii):
        raise ValueError("coordinate_radii must contain one positive value per column")

    rng = np.random.default_rng(seed)
    folds = np.zeros(len(y), dtype=np.int8)
    folds[rng.permutation(len(y))[len(y) // 2 :]] = 1
    chain_rows: list[list[dict]] = []
    per_support_delta = delta / ((max_degree + 1) * n_chains)
    for _ in range(n_chains):
        order = rng.permutation(x.shape[1])[:max_degree]
        keys = np.zeros(len(y), dtype=np.uint64)
        rows = [
            {
                "degree": 0,
                "support_columns": [],
                **crossfit_conditional_collision(
                    keys, y, folds, delta=per_support_delta
                ),
            }
        ]
        for degree, column in enumerate(order, start=1):
            keys |= x[:, column].astype(np.uint64) << np.uint64(8 * (degree - 1))
            rows.append(
                {
                    "degree": degree,
                    "support_columns": order[:degree].tolist(),
                    "support_radius": max(radii[index] for index in order[:degree]),
                    **crossfit_conditional_collision(
                        keys, y, folds, delta=per_support_delta
                    ),
                }
            )
        chain_rows.append(rows)

    degree_curve = []
    support_half_width = math.sqrt(
        math.log(2.0 * (max_degree + 1) / delta) / (2.0 * n_chains)
    )
    previous = None
    for degree in range(max_degree + 1):
        selected = [chain[degree] for chain in chain_rows]
        energies = np.asarray(
            [row["conditional_collision_energy"] for row in selected], dtype=float
        )
        coverages = np.asarray(
            [row["opposite_fold_context_coverage"] for row in selected], dtype=float
        )
        mean_energy = float(energies.mean())
        mean_coverage = float(coverages.mean())
        degree_curve.append(
            {
                "degree": degree,
                "mean_conditional_collision_energy": mean_energy,
                "mean_increment_from_previous_degree": (
                    None if previous is None else mean_energy - previous
                ),
                "mean_context_coverage": mean_coverage,
                "mean_coverage_identification_interval": [
                    mean_energy,
                    float(min(1.0, mean_energy + 1.0 - mean_coverage)),
                ],
                "support_sampling_hoeffding_interval": [
                    float(max(0.0, mean_energy - support_half_width)),
                    float(min(1.0, mean_energy + support_half_width)),
                ],
                "support_standard_deviation": float(energies.std()),
                "mean_evaluation_hoeffding_half_width": float(
                    np.mean(
                        [row["evaluation_hoeffding_half_width"] for row in selected]
                    )
                ),
            }
        )
        previous = mean_energy
    feature_degree = min(3, max_degree)
    sampled_features = geometric_sampled_features(
        chain_rows,
        radii,
        feature_degree=feature_degree,
        q=q,
    )
    result = {
        "schema_version": 2,
        "estimator": "random nested-support cross-fitted conditional collision energy",
        "interpretation": (
            "basis-invariant cumulative projection diagnostic; increments are not "
            "the exact dependent-input Fourier level weights"
        ),
        "n_examples": len(y),
        "n_coordinates": x.shape[1],
        "coordinate_radii": list(radii),
        "max_degree": max_degree,
        "n_chains": n_chains,
        "seed": seed,
        "delta": delta,
        "degree_curve": degree_curve,
        "sampled_features": sampled_features,
    }
    if include_product_reference:
        result["product_reference_inverted_level_weights"] = (
            invert_product_reference_degree_curve(
                [row["mean_conditional_collision_energy"] for row in degree_curve],
                x.shape[1],
            )
        )
        result["product_reference_warning"] = (
            "exact only for orthogonal coordinate subspaces, such as a product "
            "input measure; signed levels diagnose dependence or estimation bias"
        )
    if include_chains:
        result["chains"] = chain_rows
    return result


def sampled_token_degree_profile(
    tokens: np.ndarray,
    *,
    lags: tuple[int, ...] = tuple(range(1, 17)),
    max_degree: int = 6,
    n_chains: int = 48,
    max_positions: int = 100_000,
    seed: int = 0,
    delta: float = 0.05,
    q: int = 256,
    include_chains: bool = False,
    include_product_reference: bool = False,
) -> dict:
    """Convenience wrapper for a categorical token stream."""
    values = np.asarray(tokens)
    lag_values = tuple(sorted({int(lag) for lag in lags}))
    if len(lag_values) != len(lags) or lag_values[0] < 1:
        raise ValueError("lags must be unique positive integers")
    start = lag_values[-1]
    stop = min(len(values), start + max_positions)
    positions = np.arange(start, stop, dtype=np.int64)
    if len(positions) < 100:
        raise ValueError("token stream is too short")
    contexts = np.column_stack([values[positions - lag] for lag in lag_values])
    result = sampled_nested_degree_profile(
        contexts,
        values[positions],
        max_degree=max_degree,
        n_chains=n_chains,
        seed=seed,
        delta=delta,
        q=q,
        coordinate_radii=lag_values,
        include_chains=include_chains,
        include_product_reference=include_product_reference,
    )
    return {"lags": list(lag_values), "n_positions": len(positions), **result}
