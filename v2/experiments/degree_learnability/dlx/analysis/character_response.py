"""Empirical CE learnability of exact Fourier characters and dataset overlap."""

from __future__ import annotations

import itertools
import math
from collections.abc import Iterable

import numpy as np
import torch


def support_key(support: Iterable[int]) -> str:
    """Return the canonical JSON key for a nonempty unique lag support."""
    values = tuple(sorted(int(value) for value in support))
    if not values or values[0] < 1 or len(set(values)) != len(values):
        raise ValueError("support must contain unique positive lags")
    return ",".join(str(value) for value in values)


def parse_support_key(key: str) -> tuple[int, ...]:
    values = tuple(int(value) for value in key.split(","))
    if support_key(values) != key:
        raise ValueError(f"noncanonical support key: {key!r}")
    return values


def enumerate_supports(
    lags: Iterable[int], *, max_degree: int = 3
) -> tuple[tuple[int, ...], ...]:
    values = tuple(sorted(int(value) for value in lags))
    if not values or len(set(values)) != len(values) or values[0] < 1:
        raise ValueError("lags must be unique and positive")
    if not 1 <= max_degree <= len(values):
        raise ValueError("max_degree must lie in [1,len(lags)]")
    return tuple(
        support
        for degree in range(1, max_degree + 1)
        for support in itertools.combinations(values, degree)
    )


def walsh_character(contexts: torch.Tensor, support: Iterable[int]) -> torch.Tensor:
    """Evaluate a {-1,+1} Fourier character on binary sequence contexts."""
    if contexts.ndim != 2 or contexts.dtype not in {
        torch.uint8,
        torch.int8,
        torch.int16,
        torch.int32,
        torch.int64,
    }:
        raise ValueError("contexts must be a two-dimensional integer tensor")
    values = tuple(sorted(int(value) for value in support))
    if not values or values[0] < 1 or values[-1] > contexts.shape[1]:
        raise ValueError("support lags must lie inside the context")
    if torch.any((contexts < 0) | (contexts > 1)):
        raise ValueError("Fourier-character contexts must be binary")
    parity = torch.zeros(contexts.shape[0], device=contexts.device, dtype=torch.int64)
    for lag in values:
        parity ^= contexts[:, -lag].to(torch.int64)
    return 1.0 - 2.0 * parity.to(torch.float32)


def empirical_character_ce_kernel(
    cells: list[dict],
    *,
    architectures: Iterable[str],
    supports: Iterable[Iterable[int]],
    seeds: Iterable[int],
) -> dict[str, dict[str, float]]:
    """Reduce the complete character-training grid to median held-out CE hardness.

    Every value is a realized learning outcome: normalized held-out cross-entropy
    curve area for one architecture learning one exact Fourier character. No
    initialization proxy or model-gradient statistic enters this construction.
    """
    architecture_values = tuple(str(value) for value in architectures)
    support_values = tuple(tuple(int(lag) for lag in value) for value in supports)
    seed_values = tuple(int(value) for value in seeds)
    if not architecture_values or not support_values or not seed_values:
        raise ValueError("architectures, supports, and seeds cannot be empty")
    expected = {
        (architecture, support_key(support), seed)
        for architecture in architecture_values
        for support in support_values
        for seed in seed_values
    }
    observed = {
        (str(row["architecture"]), support_key(row["support"]), int(row["seed"]))
        for row in cells
    }
    if len(cells) != len(expected) or observed != expected:
        raise ValueError(
            f"Fourier-character CE grid is incomplete: {len(observed)}/{len(expected)}"
        )
    output: dict[str, dict[str, float]] = {}
    for architecture in architecture_values:
        output[architecture] = {}
        for support in support_values:
            key = support_key(support)
            values = [
                float(row["character_hardness"])
                for row in cells
                if row["architecture"] == architecture
                and support_key(row["support"]) == key
            ]
            if len(values) != len(seed_values) or any(
                not math.isfinite(value) or value < 0.0 for value in values
            ):
                raise ValueError("character CE hardness must be complete and finite")
            output[architecture][key] = float(np.median(values))
    return output


def architecture_spectrum_overlap(
    support_energy: dict[str, float], character_ce_hardness: dict[str, float]
) -> float:
    """Return Fourier-energy-weighted empirical character CE hardness."""
    if not support_energy:
        raise ValueError("support energy cannot be empty")
    total = 0.0
    weighted = 0.0
    for key, raw_energy in support_energy.items():
        parse_support_key(key)
        energy = float(raw_energy)
        if energy < 0.0 or not math.isfinite(energy):
            raise ValueError("support energy must be finite and nonnegative")
        if key not in character_ce_hardness:
            raise ValueError(f"character CE kernel lacks Fourier support {key}")
        hardness = float(character_ce_hardness[key])
        if hardness < 0.0 or not math.isfinite(hardness):
            raise ValueError("character CE hardness must be finite and nonnegative")
        total += energy
        weighted += energy * hardness
    if total <= 0.0:
        raise ValueError("architecture overlap requires positive Fourier energy")
    return weighted / total
