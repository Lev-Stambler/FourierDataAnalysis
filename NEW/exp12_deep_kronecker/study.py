"""Locked successive-halving policy for the Exp12 GPU campaign."""

from __future__ import annotations

import hashlib
import json
import math
from dataclasses import asdict, dataclass, replace
from typing import Any, Iterable


VARIANTS = ("deep-kron-r8", "transformer")
FAMILIES = ("adamw", "muon")
COARSE_SEEDS = (0,)
ROBUST_SEEDS = (0, 1)
SELECTION_SEEDS = (0, 1, 2)
COARSE_TOKENS = 1_000_000
ROBUST_TOKENS = 3_000_000
SELECTION_TOKENS = 10_000_000
FINAL_TOKENS = 40_000_000
WARMUP_TOKENS = 2_000_000
SCHEDULES = ("constant", "warmup-cosine")
MAX_BOUNDARY_EXPANSIONS = 2
ADAMW_LRS = {
    "deep-kron-r8": (0.001, 0.003, 0.01, 0.03),
    "transformer": (0.0003, 0.001, 0.003, 0.01),
}
MUON_LRS = {
    "deep-kron-r8": (0.01, 0.03, 0.1, 0.3),
    "transformer": (0.01, 0.03, 0.1, 0.3),
}
MUON_AUXILIARY_LR = {"deep-kron-r8": 0.003, "transformer": 0.001}


@dataclass(frozen=True)
class OptimizerRecipe:
    family: str
    body_lr: float
    auxiliary_lr: float
    schedule: str = "constant"
    weight_decay: float = 0.01
    beta2: float = 0.95
    momentum: float = 0.95
    nesterov: bool = True
    ns_steps: int = 5
    clip_norm: float = 1.0
    warmup_tokens: int = WARMUP_TOKENS
    horizon_tokens: int = FINAL_TOKENS
    minimum_lr_ratio: float = 0.1

    def __post_init__(self) -> None:
        if self.family not in FAMILIES or self.schedule not in SCHEDULES:
            raise ValueError("unsupported optimizer recipe")
        if min(self.body_lr, self.auxiliary_lr, self.clip_norm) <= 0:
            raise ValueError("learning rates and clipping norm must be positive")
        if self.weight_decay < 0 or not 0 <= self.minimum_lr_ratio <= 1:
            raise ValueError("invalid decay configuration")
        if not 0 <= self.beta2 < 1 or not 0 <= self.momentum < 1:
            raise ValueError("invalid optimizer momentum")


def recipe_slug(recipe: OptimizerRecipe) -> str:
    encoded = json.dumps(asdict(recipe), sort_keys=True, separators=(",", ":"))
    return hashlib.sha256(encoded.encode()).hexdigest()[:16]


def schedule_multiplier(recipe: OptimizerRecipe, tokens_seen: int) -> float:
    if recipe.schedule == "constant":
        return 1.0
    if tokens_seen <= recipe.warmup_tokens:
        return max(tokens_seen, 1) / recipe.warmup_tokens
    progress = min(
        1.0,
        (tokens_seen - recipe.warmup_tokens)
        / max(1, recipe.horizon_tokens - recipe.warmup_tokens),
    )
    cosine = 0.5 * (1.0 + math.cos(math.pi * progress))
    return recipe.minimum_lr_ratio + (1.0 - recipe.minimum_lr_ratio) * cosine


def coarse_recipes(variant: str) -> list[OptimizerRecipe]:
    if variant not in VARIANTS:
        raise ValueError(f"unknown variant: {variant}")
    recipes = [
        OptimizerRecipe("adamw", lr, lr) for lr in ADAMW_LRS[variant]
    ]
    recipes.extend(
        OptimizerRecipe("muon", lr, MUON_AUXILIARY_LR[variant])
        for lr in MUON_LRS[variant]
    )
    return recipes


def summarize(
    rows: Iterable[dict[str, Any]], required_seeds: tuple[int, ...]
) -> list[dict[str, Any]]:
    grouped: dict[str, list[dict[str, Any]]] = {}
    for row in rows:
        grouped.setdefault(str(row["recipe_slug"]), []).append(row)
    summaries: list[dict[str, Any]] = []
    for slug, cells in grouped.items():
        complete = {
            int(cell["seed"]): cell
            for cell in cells
            if cell.get("status") == "complete"
            and math.isfinite(float(cell["validation"]["nll"]))
        }
        if any(seed not in complete for seed in required_seeds):
            continue
        values = [float(complete[seed]["validation"]["nll"]) for seed in required_seeds]
        mean = sum(values) / len(values)
        variance = sum((value - mean) ** 2 for value in values) / len(values)
        summaries.append(
            {
                "recipe_slug": slug,
                "recipe": cells[0]["recipe"],
                "mean_validation_nll": mean,
                "std_validation_nll": math.sqrt(variance),
                "worst_validation_nll": max(values),
                "validation_nll_by_seed": dict(zip(required_seeds, values, strict=True)),
            }
        )
    return sorted(
        summaries,
        key=lambda row: (
            row["mean_validation_nll"],
            row["worst_validation_nll"],
            row["recipe_slug"],
        ),
    )


def boundary_extension(
    winner: OptimizerRecipe, recipes: Iterable[OptimizerRecipe]
) -> OptimizerRecipe | None:
    family = [
        recipe
        for recipe in recipes
        if recipe.family == winner.family
        and recipe.schedule == winner.schedule
        and recipe.auxiliary_lr == winner.auxiliary_lr
    ]
    learning_rates = sorted({recipe.body_lr for recipe in family})
    if winner.body_lr == learning_rates[-1]:
        return replace(winner, body_lr=winner.body_lr * 3.0)
    if winner.body_lr == learning_rates[0]:
        return replace(winner, body_lr=winner.body_lr / 3.0)
    return None


def robust_recipes(coarse_rows: Iterable[dict[str, Any]]) -> list[OptimizerRecipe]:
    summaries = summarize(coarse_rows, COARSE_SEEDS)
    selected: list[OptimizerRecipe] = []
    for family in FAMILIES:
        family_rows = [
            row for row in summaries if row["recipe"]["family"] == family
        ][:2]
        if len(family_rows) != 2:
            raise RuntimeError(f"coarse screen lacks two complete {family} recipes")
        bases = [OptimizerRecipe(**row["recipe"]) for row in family_rows]
        for base in bases:
            selected.extend(replace(base, schedule=schedule) for schedule in SCHEDULES)
        if family == "muon":
            best = bases[0]
            selected.extend(
                (
                    replace(best, auxiliary_lr=best.auxiliary_lr / 2.0),
                    replace(best, auxiliary_lr=best.auxiliary_lr * 2.0),
                )
            )
    return list({recipe_slug(recipe): recipe for recipe in selected}.values())


def finalist_recipes(robust_rows: Iterable[dict[str, Any]]) -> list[OptimizerRecipe]:
    summaries = summarize(robust_rows, ROBUST_SEEDS)
    if len(summaries) < 2:
        raise RuntimeError("robust screen lacks two complete recipes")
    return [OptimizerRecipe(**row["recipe"]) for row in summaries[:2]]


def selected_recipe(selection_rows: Iterable[dict[str, Any]]) -> dict[str, Any]:
    summaries = summarize(selection_rows, SELECTION_SEEDS)
    if not summaries:
        raise RuntimeError("selection screen has no three-seed complete recipe")
    return summaries[0]


def promotion_decision(
    deep_selection: dict[str, Any],
    transformer_selection: dict[str, Any],
    *,
    body_parameter_ratio: float,
    throughput_ratio: float,
) -> dict[str, Any]:
    deep = deep_selection["validation_nll_by_seed"]
    transformer = transformer_selection["validation_nll_by_seed"]
    deltas = [float(deep[seed]) - float(transformer[seed]) for seed in SELECTION_SEEDS]
    mean = sum(deltas) / len(deltas)
    passed = (
        body_parameter_ratio <= 1.0
        and throughput_ratio >= 0.4
        and mean <= 0.05
        and max(deltas) <= 0.15
    )
    return {
        "status": "pass" if passed else "fail",
        "paired_deep_minus_transformer_validation_nll": deltas,
        "mean_deep_minus_transformer_validation_nll": mean,
        "maximum_seed_delta": max(deltas),
        "body_parameter_ratio": body_parameter_ratio,
        "throughput_ratio": throughput_ratio,
        "limits": {
            "maximum_mean_validation_nll_delta": 0.05,
            "maximum_single_seed_validation_nll_delta": 0.15,
            "maximum_body_parameter_ratio": 1.0,
            "minimum_throughput_ratio": 0.4,
        },
        "test_split_may_be_opened": passed,
    }


def token_budget() -> dict[str, int]:
    # Maximum accounting assumes ten robust recipes per architecture. Constant
    # seed-0 cells for the selected coarse LRs resume from their 1M checkpoints.
    base_coarse = len(VARIANTS) * len(FAMILIES) * 4 * COARSE_TOKENS
    boundary_allowance = (
        len(VARIANTS)
        * len(FAMILIES)
        * MAX_BOUNDARY_EXPANSIONS
        * COARSE_TOKENS
    )
    coarse = base_coarse + boundary_allowance
    robust_cells = len(VARIANTS) * (4 + 6) * len(ROBUST_SEEDS)
    reused_coarse_cells = len(VARIANTS) * len(FAMILIES) * 2
    robust_increment = (
        robust_cells * ROBUST_TOKENS - reused_coarse_cells * COARSE_TOKENS
    )
    selection_increment = len(VARIANTS) * 2 * (
        2 * (SELECTION_TOKENS - ROBUST_TOKENS) + SELECTION_TOKENS
    )
    final_increment = len(VARIANTS) * len(SELECTION_SEEDS) * (
        FINAL_TOKENS - SELECTION_TOKENS
    )
    return {
        "base_coarse_tokens": base_coarse,
        "maximum_boundary_extension_tokens": boundary_allowance,
        "maximum_coarse_tokens": coarse,
        "robust_incremental_tokens": robust_increment,
        "selection_incremental_tokens": selection_increment,
        "pre_promotion_tokens": coarse + robust_increment + selection_increment,
        "post_promotion_final_incremental_tokens": final_increment,
        "maximum_total_tokens": coarse
        + robust_increment
        + selection_increment
        + final_increment,
    }
