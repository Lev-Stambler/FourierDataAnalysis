"""Resolved low-degree Fourier locality profiles for categorical token streams.

The complete degree-two space over a 64-byte context is too large to estimate
without a sketch.  This module instead freezes a small dyadic lag bank, computes
the exact cross-fitted degree-0/1/2 projection on every two-lag support, and forms
a monotone lower envelope over supports contained inside each radius.  The result
is a data-only, auditable *resolved* surface; it is not presented as the complete
64-position spectrum.
"""

from __future__ import annotations

import math
from itertools import combinations

import numpy as np

from dlx.profiles.text_anova import inverse_likelihood_pair_profile

DEFAULT_LAGS = (1, 2, 4, 8, 16, 32, 64)
DEFAULT_RADII = (1, 2, 4, 8, 16, 32, 64)


def _spectral_entropy_upper(weights: list[float], counts: list[int]) -> float:
    total = float(sum(weights))
    if total <= 0.0:
        return 0.0
    result = 0.0
    for weight, count in zip(weights, counts, strict=True):
        if weight <= 0.0:
            continue
        probability = weight / total
        result -= probability * math.log2(probability)
        result += probability * math.log2(count)
    return result


def _quantile_radius(
    radii: list[int], increments: np.ndarray, quantile: float
) -> int | None:
    total = float(increments.sum())
    if total <= 0.0:
        return None
    threshold = quantile * total
    cumulative = 0.0
    for radius, increment in zip(radii, increments, strict=True):
        cumulative += float(increment)
        if cumulative >= threshold:
            return radius
    return radii[-1]


def resolved_surface_from_pairs(
    pairs: list[dict], radii: tuple[int, ...] = DEFAULT_RADII
) -> dict:
    """Build a monotone support-bank lower envelope from pair profiles."""
    if not pairs:
        raise ValueError("at least one pair profile is required")
    ordered_radii = sorted({int(radius) for radius in radii})
    if ordered_radii != list(radii) or ordered_radii[0] < 1:
        raise ValueError("radii must be strictly increasing positive integers")

    constants = [
        pair["conditional_fourier_spectrum"]["level_weights"][0] for pair in pairs
    ]
    constant_energy = float(np.mean(constants))
    surface = []
    for radius in ordered_radii:
        eligible = [pair for pair in pairs if max(pair["lags"]) <= radius]
        degree1_candidates = [
            (
                sum(pair["conditional_fourier_spectrum"]["level_weights"][:2]),
                pair,
            )
            for pair in eligible
        ]
        degree2_candidates = [
            (pair["conditional_fourier_spectrum"]["total_square_energy"], pair)
            for pair in eligible
        ]
        best_degree1 = max(degree1_candidates, default=(constant_energy, None), key=lambda x: x[0])
        best_degree2 = max(degree2_candidates, default=(constant_energy, None), key=lambda x: x[0])
        surface.append(
            {
                "radius": radius,
                "energy_degree_leq_0": constant_energy,
                "energy_degree_leq_1": float(max(constant_energy, best_degree1[0])),
                "energy_degree_leq_2": float(max(constant_energy, best_degree2[0])),
                "argmax_degree_leq_1": best_degree1[1]["lags"] if best_degree1[1] else None,
                "argmax_degree_leq_2": best_degree2[1]["lags"] if best_degree2[1] else None,
            }
        )

    # Cross-fit noise can produce tiny reversals.  The support family is nested,
    # so its reported lower envelope must be cumulative and degree-monotone.
    previous1 = constant_energy
    previous2 = constant_energy
    for row in surface:
        previous1 = max(previous1, row["energy_degree_leq_1"])
        previous2 = max(previous2, row["energy_degree_leq_2"], previous1)
        row["energy_degree_leq_1"] = previous1
        row["energy_degree_leq_2"] = previous2

    cumulative = np.asarray(
        [row["energy_degree_leq_2"] - constant_energy for row in surface], dtype=float
    )
    increments = np.diff(np.concatenate(([0.0], cumulative)))
    increments = np.maximum(increments, 0.0)
    total_increment = float(increments.sum())
    radial_probabilities = (
        increments / total_increment if total_increment > 0.0 else np.zeros_like(increments)
    )
    radial_entropy = float(
        -sum(value * math.log2(value) for value in radial_probabilities if value > 0)
    )
    locality = (
        float(
            np.dot(
                radial_probabilities,
                np.log2(1.0 + np.asarray(ordered_radii, dtype=float)),
            )
        )
        if total_increment > 0.0
        else None
    )

    best_pair = max(
        pairs,
        key=lambda pair: pair["conditional_fourier_spectrum"]["nonconstant_energy"],
    )
    best_spectrum = best_pair["conditional_fourier_spectrum"]
    features = {
        "constant_energy": constant_energy,
        "resolved_nonconstant_energy": total_increment,
        "low_degree_concentration_best_pair": best_spectrum["cumulative_concentration"][1],
        "mean_nonconstant_degree_best_pair": best_spectrum[
            "mean_nonconstant_spectral_degree"
        ],
        "max_degree2_incremental_energy": float(
            max(
                pair["conditional_fourier_spectrum"]["level_weights"][2]
                for pair in pairs
            )
        ),
        "energy_weighted_log_radius": locality,
        "locality_radius_50": _quantile_radius(ordered_radii, increments, 0.5),
        "locality_radius_90": _quantile_radius(ordered_radii, increments, 0.9),
        "radial_spectral_entropy_bits": radial_entropy,
        "spectral_entropy_upper_bits_best_pair": _spectral_entropy_upper(
            best_spectrum["level_weights"], best_spectrum["level_cardinalities"]
        ),
    }
    return {
        "surface": surface,
        "radial_degree_leq_2_increments": increments.tolist(),
        "radial_degree_leq_2_mass": radial_probabilities.tolist(),
        "best_pair": best_pair["lags"],
        "features": features,
        "surface_kind": "dyadic two-support lower envelope",
        "scope": (
            "exact within each resolved pair support; lower bound rather than the "
            "complete degree-two 64-position spectrum"
        ),
    }


def resolved_locality_profile(
    tokens: np.ndarray,
    q: int = 256,
    lags: tuple[int, ...] = DEFAULT_LAGS,
    radii: tuple[int, ...] = DEFAULT_RADII,
    max_tokens: int = 96_000,
    smoothing: float = 8.0,
    positions: np.ndarray | None = None,
    fold_ids: np.ndarray | None = None,
) -> dict:
    """Profile all two-lag supports in a frozen lag bank."""
    lag_bank = tuple(sorted({int(lag) for lag in lags}))
    if len(lag_bank) < 2 or lag_bank[0] < 1:
        raise ValueError("lags must contain at least two positive values")
    tok = np.asarray(tokens)
    if positions is None:
        start = lag_bank[-1]
        stop = min(len(tok), start + int(max_tokens))
        selected_positions = np.arange(start, stop, dtype=np.int64)
    else:
        selected_positions = np.asarray(positions, dtype=np.int64)
    if len(selected_positions) < 100:
        raise ValueError("too few positions for a cross-fitted profile")
    if np.any(selected_positions < lag_bank[-1]) or np.any(selected_positions >= len(tok)):
        raise ValueError("positions do not support the full lag bank")
    selected_folds = (
        np.arange(len(selected_positions), dtype=np.int64) & 1
        if fold_ids is None
        else np.asarray(fold_ids, dtype=np.int64)
    )
    if selected_folds.shape != selected_positions.shape:
        raise ValueError("fold_ids must align with positions")

    pairs = [
        inverse_likelihood_pair_profile(
            tok,
            q,
            lag_a,
            lag_b,
            max_tokens=max_tokens,
            smoothing=smoothing,
            positions=selected_positions,
            fold_ids=selected_folds,
        )
        for lag_a, lag_b in combinations(lag_bank, 2)
    ]
    resolved = resolved_surface_from_pairs(pairs, radii=radii)
    return {
        "q": q,
        "lags": list(lag_bank),
        "radii": list(radii),
        "n_positions": len(selected_positions),
        "smoothing": smoothing,
        "pairs": pairs,
        **resolved,
    }
