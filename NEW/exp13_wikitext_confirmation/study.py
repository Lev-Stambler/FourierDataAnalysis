"""Locked search, matching, and verdict policy for Exp13."""

from __future__ import annotations

import hashlib
import json
import math
from dataclasses import asdict, dataclass, replace
from typing import Any, Iterable

from .model import STANDARD_CONTROLS


TUNING_SEEDS = (0, 1)
CONFIRMATION_SEEDS = (3, 4, 5, 6)
COARSE_TOKENS = 1_000_000
ROBUST_TOKENS = 3_000_000
CONFIRMATION_STEPS = 102
FINAL_STEPS = 407
CONTEXT_LENGTH = 256
COMMON_BATCH_CANDIDATES = (768, 640, 512, 384, 320, 256, 192, 128)
ADAMW_LRS = (0.0003, 0.001, 0.003, 0.01)
MUON_LRS = (0.01, 0.03, 0.1, 0.3)
SCHEDULES = ("constant", "warmup-cosine")


@dataclass(frozen=True)
class Recipe:
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
    warmup_tokens: int = 2_000_000
    horizon_tokens: int = 40_000_000
    minimum_lr_ratio: float = 0.1

    def __post_init__(self) -> None:
        if self.family not in ("adamw", "muon"):
            raise ValueError("optimizer must be AdamW or Muon")
        if self.schedule not in SCHEDULES:
            raise ValueError("invalid schedule")
        if min(self.body_lr, self.auxiliary_lr, self.clip_norm) <= 0:
            raise ValueError("learning rates and clipping must be positive")


FROZEN_CANDIDATE_RECIPE = Recipe("muon", 0.03, 0.003)
FROZEN_REPLAY_RECIPE = Recipe("muon", 0.1, 0.002)


def recipe_slug(recipe: Recipe) -> str:
    payload = json.dumps(asdict(recipe), sort_keys=True, separators=(",", ":"))
    return hashlib.sha256(payload.encode()).hexdigest()[:16]


def schedule_multiplier(recipe: Recipe, tokens_seen: int) -> float:
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


def coarse_recipes() -> list[Recipe]:
    return [*(Recipe("adamw", lr, lr) for lr in ADAMW_LRS), *(
        Recipe("muon", lr, 0.001) for lr in MUON_LRS
    )]


def boundary_extension(
    winner: Recipe, recipes: Iterable[Recipe]
) -> Recipe | None:
    family = sorted(
        {recipe.body_lr for recipe in recipes if recipe.family == winner.family}
    )
    if winner.body_lr == family[0]:
        return replace(winner, body_lr=winner.body_lr / 3.0)
    if winner.body_lr == family[-1]:
        return replace(winner, body_lr=winner.body_lr * 3.0)
    return None


def summarize(rows: Iterable[dict[str, Any]], seeds: tuple[int, ...]) -> list[dict[str, Any]]:
    groups: dict[tuple[str, str], list[dict[str, Any]]] = {}
    for row in rows:
        groups.setdefault((str(row["model"]), str(row["recipe_slug"])), []).append(row)
    result = []
    for (model, slug), cells in groups.items():
        complete = {
            int(cell["seed"]): cell
            for cell in cells
            if cell.get("status") == "complete"
            and math.isfinite(float(cell["validation"]["nll"]))
        }
        if any(seed not in complete for seed in seeds):
            continue
        values = [float(complete[seed]["validation"]["nll"]) for seed in seeds]
        mean = sum(values) / len(values)
        result.append(
            {
                "model": model,
                "recipe_slug": slug,
                "recipe": cells[0]["recipe"],
                "mean_validation_nll": mean,
                "worst_validation_nll": max(values),
                "validation_nll_by_seed": dict(zip(seeds, values, strict=True)),
            }
        )
    return sorted(
        result,
        key=lambda row: (
            row["mean_validation_nll"],
            row["worst_validation_nll"],
            row["model"],
            row["recipe_slug"],
        ),
    )


def robust_recipes(coarse_rows: Iterable[dict[str, Any]]) -> list[tuple[str, Recipe]]:
    summaries = summarize(coarse_rows, (0,))
    selected: list[tuple[str, Recipe]] = []
    for model in STANDARD_CONTROLS:
        rows = [row for row in summaries if row["model"] == model]
        for family in ("adamw", "muon"):
            candidates = [row for row in rows if row["recipe"]["family"] == family]
            if not candidates:
                raise RuntimeError(f"missing {model}/{family} coarse winner")
            base = Recipe(**candidates[0]["recipe"])
            selected.extend((model, replace(base, schedule=value)) for value in SCHEDULES)
            if family == "muon":
                selected.extend(
                    (
                        (model, replace(base, auxiliary_lr=0.0005)),
                        (model, replace(base, auxiliary_lr=0.002)),
                    )
                )
    unique = {(model, recipe_slug(recipe)): (model, recipe) for model, recipe in selected}
    return list(unique.values())


def finalist_recipes(robust_rows: Iterable[dict[str, Any]]) -> list[tuple[str, Recipe]]:
    rows = summarize(robust_rows, TUNING_SEEDS)
    if len(rows) < 2:
        raise RuntimeError("fewer than two complete standard Transformer finalists")
    return [(row["model"], Recipe(**row["recipe"])) for row in rows[:2]]


def paired_decision(
    candidate: dict[int, float], control: dict[int, float], *, minimum_mean_win: float
) -> dict[str, Any]:
    deltas = [float(candidate[seed]) - float(control[seed]) for seed in CONFIRMATION_SEEDS]
    mean = sum(deltas) / len(deltas)
    return {
        "status": (
            "pass"
            if all(delta < 0.0 for delta in deltas) and mean <= -minimum_mean_win
            else "fail"
        ),
        "paired_candidate_minus_transformer_nll": deltas,
        "mean_candidate_minus_transformer_nll": mean,
        "all_four_candidate_wins": all(delta < 0.0 for delta in deltas),
        "minimum_required_mean_win": minimum_mean_win,
    }


def fixed_tokens(batch: int, steps: int) -> int:
    return batch * CONTEXT_LENGTH * steps
