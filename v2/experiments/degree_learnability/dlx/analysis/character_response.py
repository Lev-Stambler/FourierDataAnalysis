"""Architecture response to exact Walsh characters and dataset overlap scores."""

from __future__ import annotations

import itertools
import math
from collections.abc import Iterable

import numpy as np
import torch
from scipy.stats import spearmanr


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
    """Evaluate a {-1,+1} Walsh character on binary sequence contexts."""
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
        raise ValueError("Walsh contexts must be binary")
    parity = torch.zeros(contexts.shape[0], device=contexts.device, dtype=torch.int64)
    for lag in values:
        parity ^= contexts[:, -lag].to(torch.int64)
    return 1.0 - 2.0 * parity.to(torch.float32)


def character_ntk_rayleigh(
    model: torch.nn.Module, contexts: torch.Tensor, support: Iterable[int]
) -> float:
    """Compute chi^T K chi / n without materializing the empirical NTK."""
    if contexts.shape[0] < 2:
        raise ValueError("at least two contexts are required")
    model.zero_grad(set_to_none=True)
    character = walsh_character(contexts, support)
    logits = model(contexts)[:, -1, :]
    if logits.shape[1] != 2:
        raise ValueError("character response requires a binary output head")
    contrast = logits[:, 1] - logits[:, 0]
    objective = torch.dot(character.to(contrast.dtype), contrast) / math.sqrt(
        contexts.shape[0]
    )
    gradients = torch.autograd.grad(objective, tuple(model.parameters()))
    value = sum(
        float(torch.sum(gradient.detach().float() ** 2)) for gradient in gradients
    )
    if not math.isfinite(value) or value <= 0.0:
        raise ValueError("character NTK Rayleigh quotient must be finite and positive")
    return value


def standardize_character_kernel(rows: list[dict]) -> dict[str, dict[str, float]]:
    """Aggregate initialization seeds and standardize log response per architecture."""
    architectures = sorted({str(row["architecture"]) for row in rows})
    output: dict[str, dict[str, float]] = {}
    for architecture in architectures:
        selected = [row for row in rows if row["architecture"] == architecture]
        keys = sorted({support_key(row["support"]) for row in selected})
        log_response = {}
        for key in keys:
            values = [
                float(row["ntk_rayleigh"])
                for row in selected
                if support_key(row["support"]) == key
            ]
            if not values or any(
                value <= 0.0 or not math.isfinite(value) for value in values
            ):
                raise ValueError(
                    "every kernel cell must have positive finite seed values"
                )
            log_response[key] = float(np.mean(np.log(values)))
        logs = np.asarray(list(log_response.values()), dtype=float)
        scale = float(logs.std())
        if scale <= 0.0:
            raise ValueError("architecture kernel has no support-dependent variation")
        output[architecture] = {
            key: float(-(value - logs.mean()) / scale)
            for key, value in log_response.items()
        }
    return output


def architecture_spectrum_overlap(
    support_energy: dict[str, float], architecture_kernel: dict[str, float]
) -> float:
    """Return the positive-energy-weighted architecture character hardness."""
    if not support_energy:
        raise ValueError("support energy cannot be empty")
    total = 0.0
    weighted = 0.0
    for key, raw_energy in support_energy.items():
        parse_support_key(key)
        energy = float(raw_energy)
        if energy < 0.0 or not math.isfinite(energy):
            raise ValueError("support energy must be finite and nonnegative")
        if key not in architecture_kernel:
            raise ValueError(f"architecture kernel lacks support {key}")
        response = float(architecture_kernel[key])
        if not math.isfinite(response):
            raise ValueError("architecture response must be finite")
        total += energy
        weighted += energy * response
    if total <= 0.0:
        raise ValueError("architecture overlap requires positive support energy")
    return weighted / total


def character_mechanism_analysis(
    character_cells: list[dict],
    ntk_cells: list[dict],
    *,
    bootstrap_samples: int,
    bootstrap_seed: int,
) -> dict:
    """Test whether initialization response predicts controlled learning hardness.

    Character seeds are reduced to a median for each architecture/support cell.
    The regression includes architecture and degree fixed effects, and the
    uncertainty calculation resamples complete five-architecture support blocks.
    """
    if bootstrap_samples < 100:
        raise ValueError("at least 100 bootstrap samples are required")
    grouped_hardness: dict[tuple[str, str], list[float]] = {}
    support_values: dict[str, tuple[int, ...]] = {}
    for row in character_cells:
        key = support_key(row["support"])
        support_values[key] = parse_support_key(key)
        value = float(row["character_hardness"])
        if not math.isfinite(value):
            raise ValueError("character hardness must be finite")
        grouped_hardness.setdefault((str(row["architecture"]), key), []).append(value)

    grouped_response: dict[tuple[str, str], list[float]] = {}
    for cell in ntk_cells:
        architecture = str(cell["architecture"])
        for row in cell["rows"]:
            key = support_key(row["support"])
            if key not in support_values:
                continue
            response = float(row["ntk_rayleigh"])
            if response <= 0.0 or not math.isfinite(response):
                raise ValueError("NTK responses must be positive and finite")
            grouped_response.setdefault((architecture, key), []).append(response)

    keys = sorted(grouped_hardness)
    if set(keys) != set(grouped_response):
        raise ValueError("controlled-learning and NTK grids do not match")
    architectures = sorted({architecture for architecture, _ in keys})
    supports = sorted({key for _, key in keys})
    if len(keys) != len(architectures) * len(supports):
        raise ValueError("the architecture/support grid must be complete")
    seed_counts = {len(values) for values in grouped_hardness.values()}
    ntk_seed_counts = {len(values) for values in grouped_response.values()}
    if len(seed_counts) != 1 or len(ntk_seed_counts) != 1:
        raise ValueError("each grid cell must contain the same number of seeds")

    rows = []
    for architecture, key in keys:
        support = support_values[key]
        rows.append(
            {
                "architecture": architecture,
                "support": key,
                "degree": len(support),
                "radius": max(support),
                "character_hardness": float(
                    np.median(grouped_hardness[(architecture, key)])
                ),
                "mean_log_ntk_response": float(
                    np.mean(np.log(grouped_response[(architecture, key)]))
                ),
            }
        )

    # Intercept, log response, architecture fixed effects, degree fixed effects.
    matrix = np.asarray(
        [
            [1.0, row["mean_log_ntk_response"]]
            + [float(row["architecture"] == value) for value in architectures[1:]]
            + [float(row["degree"] == value) for value in (2, 3)]
            for row in rows
        ],
        dtype=float,
    )
    target = np.asarray([row["character_hardness"] for row in rows], dtype=float)

    def coefficient(indices: np.ndarray) -> float:
        design = matrix[indices]
        outcome = target[indices]
        return float(np.linalg.lstsq(design, outcome, rcond=None)[0][1])

    point = coefficient(np.arange(len(rows)))
    support_rows = {
        key: np.asarray(
            [index for index, row in enumerate(rows) if row["support"] == key],
            dtype=int,
        )
        for key in supports
    }
    rng = np.random.default_rng(bootstrap_seed)
    bootstrap = np.empty(bootstrap_samples, dtype=float)
    for draw in range(bootstrap_samples):
        sampled = rng.choice(supports, size=len(supports), replace=True)
        indices = np.concatenate([support_rows[str(key)] for key in sampled])
        bootstrap[draw] = coefficient(indices)

    degree_one_radius_spearman = {}
    for architecture in architectures:
        selected = [
            row
            for row in rows
            if row["architecture"] == architecture and row["degree"] == 1
        ]
        correlation = spearmanr(
            np.log([row["radius"] for row in selected]),
            [row["character_hardness"] for row in selected],
        ).statistic
        degree_one_radius_spearman[architecture] = float(correlation)
    interval = [
        float(np.quantile(bootstrap, 0.025)),
        float(np.quantile(bootstrap, 0.975)),
    ]
    directional = (
        degree_one_radius_spearman.get("alibi", 0.0) > 0.0
        and degree_one_radius_spearman.get("reverse_alibi", 0.0) < 0.0
    )
    first_checkpoint_resolved = [
        float(row["floor_independent"]["half_best_learning_at"])
        > float(row["example_grid"][1])
        for row in character_cells
    ]
    return {
        "rows": rows,
        "architectures": architectures,
        "supports": supports,
        "character_seeds_per_cell": seed_counts.pop(),
        "ntk_seeds_per_cell": ntk_seed_counts.pop(),
        "regression": {
            "formula": "character_hardness ~ mean_log_ntk_response + architecture_FE + degree_FE",
            "mean_log_ntk_response_coefficient": point,
            "support_cluster_bootstrap_95_interval": interval,
            "bootstrap_samples": bootstrap_samples,
            "bootstrap_seed": bootstrap_seed,
        },
        "degree_one_radius_spearman": degree_one_radius_spearman,
        "resolution_diagnostics": {
            "fraction_cells_half_learning_after_first_checkpoint": float(
                np.mean(first_checkpoint_resolved)
            ),
            "median_cell_hardness": float(
                np.median([row["character_hardness"] for row in character_cells])
            ),
            "hardness_range": [
                float(min(row["character_hardness"] for row in character_cells)),
                float(max(row["character_hardness"] for row in character_cells)),
            ],
        },
        "directional_control_passed": directional,
        "mechanism_gate_passed": interval[1] < 0.0 and directional,
    }
