"""Geometry-aware cardinalities for categorical Fourier level spectra.

The categorical basis contributes ``q_i - 1`` nonconstant directions for
variable ``i``.  A degree-k coefficient therefore has both an interaction
degree and a support-search geometry.  These helpers count that search space
without fitting a data-dependent tuning parameter.
"""

from __future__ import annotations

from math import comb, log

import numpy as np


def mixed_level_cardinalities(q_features: list[int] | tuple[int, ...]) -> list[int]:
    """Return exact coefficient counts by degree for mixed categorical inputs.

    The returned coefficients are those of
    ``prod_i (1 + (q_i - 1) z)``.  Thus level k counts every choice of k
    variables and every nonconstant basis direction on those variables.
    """

    cardinalities = [1]
    for q in q_features:
        if int(q) != q or q < 2:
            raise ValueError("every categorical alphabet size must be an integer >= 2")
        directions = int(q) - 1
        updated = cardinalities + [0]
        for degree, value in enumerate(cardinalities):
            updated[degree + 1] += value * directions
        cardinalities = updated
    return cardinalities


def local_ball_cardinalities(q: int, radius: int, max_degree: int) -> list[int]:
    """Coefficient counts through ``max_degree`` in a 1-D causal radius ball.

    There are ``comb(radius, k)`` possible k-position supports among the
    preceding ``radius`` positions and ``(q - 1)**k`` categorical directions
    on each support.
    """

    if int(q) != q or q < 2:
        raise ValueError("q must be an integer >= 2")
    if int(radius) != radius or radius < 0:
        raise ValueError("radius must be a nonnegative integer")
    if int(max_degree) != max_degree or max_degree < 0:
        raise ValueError("max_degree must be a nonnegative integer")
    q = int(q)
    radius = int(radius)
    max_degree = int(max_degree)
    return [
        comb(radius, degree) * (q - 1) ** degree if degree <= radius else 0
        for degree in range(max_degree + 1)
    ]


def spectral_search_complexity(
    level_weights: list[float] | tuple[float, ...] | np.ndarray,
    level_cardinalities: list[int] | tuple[int, ...],
    *,
    log_base: float = 2.0,
) -> float:
    """Energy-weighted log search volume, conditional on nonconstant energy.

    Degree zero is deliberately excluded, matching mean nonconstant spectral
    degree.  The result is in bits for ``log_base=2`` and in q-ary coordinate
    units for ``log_base=q``.
    """

    weights = np.asarray(level_weights, dtype=float)
    cardinalities = np.asarray(level_cardinalities, dtype=object)
    if weights.ndim != 1 or cardinalities.ndim != 1:
        raise ValueError("weights and cardinalities must be one-dimensional")
    if len(weights) != len(cardinalities):
        raise ValueError("weights and cardinalities must have equal length")
    if len(weights) < 2:
        raise ValueError("at least degree zero and degree one are required")
    if not np.all(np.isfinite(weights)) or np.any(weights < 0):
        raise ValueError("weights must be finite and nonnegative")
    if not np.isfinite(log_base) or log_base <= 0 or log_base == 1:
        raise ValueError("log_base must be positive and different from one")
    nonconstant = float(weights[1:].sum())
    if nonconstant <= 0:
        raise ValueError("positive nonconstant spectral energy is required")
    weighted_log_volume = 0.0
    for weight, cardinality in zip(weights[1:], cardinalities[1:]):
        if weight == 0:
            continue
        if not isinstance(cardinality, (int, np.integer)) or cardinality <= 0:
            raise ValueError("positive-weight levels need positive integer cardinality")
        weighted_log_volume += float(weight) * log(int(cardinality), log_base)
    return weighted_log_volume / nonconstant


__all__ = [
    "local_ball_cardinalities",
    "mixed_level_cardinalities",
    "spectral_search_complexity",
]
