"""Cloud-only optimizer audit for the locked Exp14 matched pair.

Exp14 screened Muon for one seed at 1M tokens with the auxiliary AdamW rate
fixed at 0.001.  Its two global coarse winners were both AdamW, so Muon never
reached the multi-seed or longer-horizon stages.  This audit repairs that
selection-policy flaw without changing either architecture.
"""

from __future__ import annotations

import argparse
import math
import os
import statistics
import threading
from dataclasses import asdict, replace
from pathlib import Path
from typing import Any, Iterable

import torch

from exp13_wikitext_confirmation.study import Recipe, recipe_slug

from .campaign import (
    CANDIDATE,
    GPU_COUNT,
    _all_complete,
    _train_task,
    run_tasks,
    write_json,
)


SCHEMA = "exp14-optimizer-audit-v1"
CONTROL = "block-transformer-d3-w256"
MODELS = (CANDIDATE, CONTROL)
COARSE_SEED = 7
ROBUST_SEEDS = (8, 9)
FINAL_SEEDS = (10, 11, 12, 13)
COARSE_TOKENS = 3_000_000
ROBUST_TOKENS = 10_000_000
FINAL_TOKENS = 40_000_000
CANDIDATE_BATCH = 640
CONTROL_BATCH = 2048
CANDIDATE_EXECUTION_MODE = "default"
CONTROL_EXECUTION_MODE = "eager"
ADAMW_LRS = (0.0015, 0.003, 0.006, 0.01, 0.02)
MUON_BODY_LRS = (0.01, 0.03, 0.06, 0.1, 0.2, 0.3)
MUON_AUXILIARY_LRS = (0.001, 0.003, 0.01, 0.03)
SCHEDULES = ("constant", "warmup-cosine")
COARSE_ADVANCERS_PER_FAMILY = 2
MINIMUM_GPU_UTILIZATION = 85.0
CANDIDATE_PREFLIGHT_STEPS = 10
CONTROL_PREFLIGHT_STEPS = 40


def coarse_recipes() -> list[Recipe]:
    return [
        *(Recipe("adamw", lr, lr) for lr in ADAMW_LRS),
        *(
            Recipe("muon", body_lr, auxiliary_lr)
            for body_lr in MUON_BODY_LRS
            for auxiliary_lr in MUON_AUXILIARY_LRS
        ),
    ]


def _mean_nll(rows: Iterable[dict[str, Any]]) -> float:
    values = [float(row["validation"]["nll"]) for row in rows]
    if not values or any(not math.isfinite(value) for value in values):
        raise RuntimeError("cannot summarize missing or non-finite NLL values")
    return statistics.mean(values)


def summarize_recipes(
    rows: Iterable[dict[str, Any]],
    *,
    model: str,
    seeds: tuple[int, ...],
) -> list[dict[str, Any]]:
    selected = [row for row in rows if row.get("model") == model]
    grouped: dict[str, list[dict[str, Any]]] = {}
    for row in selected:
        grouped.setdefault(str(row["recipe_slug"]), []).append(row)
    summaries = []
    for slug, cells in grouped.items():
        by_seed = {int(cell["seed"]): cell for cell in cells}
        if set(by_seed) != set(seeds):
            continue
        ordered = [by_seed[seed] for seed in seeds]
        summaries.append(
            {
                "model": model,
                "recipe_slug": slug,
                "recipe": ordered[0]["recipe"],
                "mean_validation_block_nll": _mean_nll(ordered),
                "validation_block_nll_by_seed": {
                    str(seed): float(by_seed[seed]["validation"]["nll"])
                    for seed in seeds
                },
            }
        )
    return sorted(
        summaries,
        key=lambda row: (
            float(row["mean_validation_block_nll"]),
            str(row["recipe_slug"]),
        ),
    )


def top_by_family(
    summaries: Iterable[dict[str, Any]], count: int
) -> dict[str, list[Recipe]]:
    result: dict[str, list[Recipe]] = {}
    for family in ("adamw", "muon"):
        choices = [
            row for row in summaries if row["recipe"]["family"] == family
        ]
        if len(choices) < count:
            raise RuntimeError(f"missing {family} tuning choices")
        result[family] = [Recipe(**row["recipe"]) for row in choices[:count]]
    return result


def schedule_variants(recipes: Iterable[Recipe]) -> list[Recipe]:
    variants = {
        recipe_slug(replace(recipe, schedule=schedule)): replace(
            recipe, schedule=schedule
        )
        for recipe in recipes
        for schedule in SCHEDULES
    }
    return list(variants.values())


def best_by_family(summaries: Iterable[dict[str, Any]]) -> dict[str, Recipe]:
    return {
        family: Recipe(
            **next(
                row["recipe"]
                for row in summaries
                if row["recipe"]["family"] == family
            )
        )
        for family in ("adamw", "muon")
    }


def _execution(model: str) -> tuple[int, str]:
    if model == CANDIDATE:
        return CANDIDATE_BATCH, CANDIDATE_EXECUTION_MODE
    if model == CONTROL:
        return CONTROL_BATCH, CONTROL_EXECUTION_MODE
    raise ValueError(f"unknown audit model: {model}")


def training_task(
    model: str,
    recipe: Recipe,
    seed: int,
    target_tokens: int,
    evaluation_windows: int,
    output_root: Path,
    data_root: Path,
) -> dict[str, Any]:
    batch, execution_mode = _execution(model)
    return _train_task(
        model,
        recipe,
        seed,
        target_tokens,
        evaluation_windows,
        batch,
        output_root,
        data_root,
        execution_mode,
    )


def paired_deltas(
    rows: Iterable[dict[str, Any]],
    candidate_recipe: Recipe,
    control_recipe: Recipe,
) -> list[float]:
    candidate_slug = recipe_slug(candidate_recipe)
    control_slug = recipe_slug(control_recipe)
    selected = list(rows)
    candidate = {
        int(row["seed"]): float(row["validation"]["nll"])
        for row in selected
        if row["model"] == CANDIDATE
        and row["recipe_slug"] == candidate_slug
    }
    control = {
        int(row["seed"]): float(row["validation"]["nll"])
        for row in selected
        if row["model"] == CONTROL and row["recipe_slug"] == control_slug
    }
    if set(candidate) != set(FINAL_SEEDS) or set(control) != set(FINAL_SEEDS):
        raise RuntimeError("final paired seeds are incomplete")
    return [candidate[seed] - control[seed] for seed in FINAL_SEEDS]


def paid_preflight(data_root: Path) -> dict[str, Any]:
    if torch.cuda.device_count() != GPU_COUNT:
        raise RuntimeError("optimizer audit requires exactly eight visible GPUs")
    names = [torch.cuda.get_device_name(index) for index in range(GPU_COUNT)]
    memory = [
        torch.cuda.get_device_properties(index).total_memory / 2**30
        for index in range(GPU_COUNT)
    ]
    if any("H100" not in name for name in names) or any(value < 75 for value in memory):
        raise RuntimeError(f"optimizer audit requires 8xH100 80GB: {names} / {memory}")

    candidate_agreements = run_tasks(
        [
            {
                "kind": "loss-agreement",
                "model": CANDIDATE,
                "execution_mode": CANDIDATE_EXECUTION_MODE,
                "data_root": str(data_root),
            }
            for _ in range(GPU_COUNT)
        ]
    )
    _all_complete(candidate_agreements, "compiled candidate exact-loss agreement")
    control_agreements = run_tasks(
        [
            {
                "kind": "loss-agreement",
                "model": CONTROL,
                "execution_mode": CONTROL_EXECUTION_MODE,
                "data_root": str(data_root),
            }
            for _ in range(GPU_COUNT)
        ]
    )
    _all_complete(control_agreements, "control exact-loss agreement")

    representatives = {
        "adamw": Recipe("adamw", 0.003, 0.003),
        "muon": Recipe("muon", 0.1, 0.003),
    }
    # Eight candidate benchmarks keep the entire node occupied through the
    # expensive first compilation.  The locked batch 640 is inherited from the
    # completed 1024/896/768/640 OOM-downward compiled sweep.
    candidate_benchmarks = run_tasks(
        [
            {
                "kind": "benchmark",
                "model": CANDIDATE,
                "execution_mode": CANDIDATE_EXECUTION_MODE,
                "recipe": asdict(representatives[family]),
                "batch": CANDIDATE_BATCH,
                "measured_steps": CANDIDATE_PREFLIGHT_STEPS,
                "data_root": str(data_root),
            }
            for family in ("adamw", "muon")
            for _ in range(GPU_COUNT // 2)
        ]
    )
    _all_complete(candidate_benchmarks, "compiled candidate optimizer preflight")
    control_benchmarks = run_tasks(
        [
            {
                "kind": "benchmark",
                "model": CONTROL,
                "execution_mode": CONTROL_EXECUTION_MODE,
                "recipe": asdict(representatives[family]),
                "batch": CONTROL_BATCH,
                # Ten control steps last under three seconds and yielded only
                # six nvidia-smi samples in the first paid attempt.  Use a
                # longer window so its utilization gate measures steady state.
                "measured_steps": CONTROL_PREFLIGHT_STEPS,
                "data_root": str(data_root),
            }
            for family in ("adamw", "muon")
            for _ in range(GPU_COUNT // 2)
        ]
    )
    _all_complete(control_benchmarks, "control optimizer preflight")
    benchmarks = [*candidate_benchmarks, *control_benchmarks]
    utilization_summary: dict[str, dict[str, float]] = {}
    for model in MODELS:
        for family in ("adamw", "muon"):
            selected = [
                row
                for row in benchmarks
                if row["model"] == model and row["optimizer_family"] == family
            ]
            if len(selected) != GPU_COUNT // 2:
                raise RuntimeError(f"missing {model}/{family} preflight repeats")
            median_utilization = statistics.median(
                float(row["median_gpu_utilization_percent"]) for row in selected
            )
            minimum_samples = min(int(row["gpu_samples"]) for row in selected)
            if median_utilization < MINIMUM_GPU_UTILIZATION:
                raise RuntimeError(
                    f"{model}/{family} repeat-median utilization "
                    f"{median_utilization:.1f}% is below "
                    f"{MINIMUM_GPU_UTILIZATION:.1f}%"
                )
            utilization_summary[f"{model}/{family}"] = {
                "repeat_median_gpu_utilization_percent": median_utilization,
                "minimum_gpu_samples_per_repeat": minimum_samples,
                "median_tokens_per_second": statistics.median(
                    float(row["tokens_per_second"]) for row in selected
                ),
                "maximum_peak_allocated_gib": max(
                    float(row["peak_allocated_gib"]) for row in selected
                ),
                "maximum_peak_reserved_gib": max(
                    float(row["peak_reserved_gib"]) for row in selected
                ),
            }
    return {
        "status": "pass",
        "gpu_names": names,
        "gpu_memory_gib": memory,
        "candidate_execution_mode": CANDIDATE_EXECUTION_MODE,
        "control_execution_mode": CONTROL_EXECUTION_MODE,
        "candidate_batch": CANDIDATE_BATCH,
        "control_batch": CONTROL_BATCH,
        "candidate_global_tokens_per_step": CANDIDATE_BATCH * 256,
        "control_global_tokens_per_step": CONTROL_BATCH * 256,
        "gradient_accumulation": 1,
        "compiled_batch_sweep_lineage": "20t5mto4",
        "candidate_loss_agreement": candidate_agreements,
        "control_loss_agreement": control_agreements,
        "benchmarks": benchmarks,
        "utilization_summary": utilization_summary,
    }


def run_audit(
    output: str | Path,
    *,
    data_root: str | Path,
    heartbeat: str | Path | None = None,
) -> dict[str, Any]:
    if not os.environ.get("WANDB_API_KEY"):
        raise RuntimeError("WANDB_API_KEY is required before paid training")
    import wandb

    output_path = Path(output)
    cells = output_path.parent / "optimizer-audit-cells"
    data = Path(data_root)
    wandb.login(key=os.environ["WANDB_API_KEY"], verify=True)
    run = wandb.init(
        project="exp14-block-kronecker",
        name="exp14-muon-lr-optimizer-audit",
        config={
            "schema": SCHEMA,
            "models": MODELS,
            "coarse_tokens": COARSE_TOKENS,
            "robust_tokens": ROBUST_TOKENS,
            "final_tokens": FINAL_TOKENS,
            "adamw_lrs": ADAMW_LRS,
            "muon_body_lrs": MUON_BODY_LRS,
            "muon_auxiliary_lrs": MUON_AUXILIARY_LRS,
            "schedules": SCHEDULES,
            "gpu_count": GPU_COUNT,
        },
    )
    if not run.url:
        raise RuntimeError("W&B did not provide a direct run URL")
    print(f"WANDB_URL={run.url}", flush=True)
    write_json(output_path, {"schema": SCHEMA, "status": "running", "wandb_url": run.url})

    heartbeat_path = Path(heartbeat) if heartbeat else None
    stop = threading.Event()
    heartbeat_thread: threading.Thread | None = None
    if heartbeat_path:
        heartbeat_path.parent.mkdir(parents=True, exist_ok=True)
        heartbeat_path.touch()

        def pulse() -> None:
            while not stop.wait(30):
                heartbeat_path.touch()

        heartbeat_thread = threading.Thread(target=pulse, daemon=True)
        heartbeat_thread.start()

    log_step = 0

    def publish(stage: str, rows: Iterable[dict[str, Any]]) -> None:
        nonlocal log_step
        for row in rows:
            run.log(
                {
                    f"{stage}/model": row["model"],
                    f"{stage}/optimizer": row["recipe"]["family"],
                    f"{stage}/body_lr": row["recipe"]["body_lr"],
                    f"{stage}/auxiliary_lr": row["recipe"]["auxiliary_lr"],
                    f"{stage}/schedule": row["recipe"]["schedule"],
                    f"{stage}/seed": row["seed"],
                    f"{stage}/validation_block_nll": row["validation"]["nll"],
                    f"{stage}/tokens_per_second": row["performance"]["tokens_per_second"],
                    f"{stage}/global_tokens_per_step": row["global_tokens_per_step"],
                },
                step=log_step,
            )
            log_step += 1
        if heartbeat_path:
            heartbeat_path.touch()

    try:
        preflight = paid_preflight(data)
        write_json(
            output_path,
            {
                "schema": SCHEMA,
                "status": "preflight-complete",
                "wandb_url": run.url,
                "preflight": preflight,
            },
        )

        grid = coarse_recipes()
        coarse_tasks = [
            training_task(
                model,
                recipe,
                COARSE_SEED,
                COARSE_TOKENS,
                1024,
                cells,
                data,
            )
            # Candidate tasks first: every persistent worker compiles the same
            # graph and then reuses its local/disk compilation cache.
            for model in MODELS
            for recipe in grid
        ]
        coarse_rows = run_tasks(coarse_tasks)
        _all_complete(coarse_rows, "joint optimizer coarse grid")
        publish("coarse", coarse_rows)

        coarse_summary = {
            model: summarize_recipes(
                coarse_rows, model=model, seeds=(COARSE_SEED,)
            )
            for model in MODELS
        }
        coarse_advancers = {
            model: top_by_family(
                coarse_summary[model], COARSE_ADVANCERS_PER_FAMILY
            )
            for model in MODELS
        }
        robust_variants = {
            model: schedule_variants(
                [
                    recipe
                    for family in ("adamw", "muon")
                    for recipe in coarse_advancers[model][family]
                ]
            )
            for model in MODELS
        }
        robust_tasks = [
            training_task(
                model,
                recipe,
                seed,
                ROBUST_TOKENS,
                2048,
                cells,
                data,
            )
            for model in MODELS
            for recipe in robust_variants[model]
            for seed in ROBUST_SEEDS
        ]
        robust_rows = run_tasks(robust_tasks)
        _all_complete(robust_rows, "multi-seed optimizer robust stage")
        publish("robust", robust_rows)
        robust_summary = {
            model: summarize_recipes(
                robust_rows, model=model, seeds=ROBUST_SEEDS
            )
            for model in MODELS
        }
        finalists = {
            model: best_by_family(robust_summary[model]) for model in MODELS
        }
        primary = {
            model: Recipe(**robust_summary[model][0]["recipe"])
            for model in MODELS
        }

        # Exactly eight long candidate tasks occupy all H100s first.  The eight
        # much faster control tasks then occupy the full node as a second wave.
        final_tasks = [
            training_task(
                model,
                finalists[model][family],
                seed,
                FINAL_TOKENS,
                2048,
                cells,
                data,
            )
            for model in MODELS
            for family in ("adamw", "muon")
            for seed in FINAL_SEEDS
        ]
        final_rows = run_tasks(final_tasks)
        _all_complete(final_rows, "40M optimizer-family confirmation")
        publish("final", final_rows)
        final_summary = {
            model: summarize_recipes(
                final_rows, model=model, seeds=FINAL_SEEDS
            )
            for model in MODELS
        }
        family_deltas = {
            family: paired_deltas(
                final_rows,
                finalists[CANDIDATE][family],
                finalists[CONTROL][family],
            )
            for family in ("adamw", "muon")
        }
        primary_deltas = paired_deltas(
            final_rows, primary[CANDIDATE], primary[CONTROL]
        )
        result = {
            "schema": SCHEMA,
            "status": "complete",
            "wandb_url": run.url,
            "models": MODELS,
            "preflight": preflight,
            "grid": {
                "adamw_lrs": ADAMW_LRS,
                "muon_body_lrs": MUON_BODY_LRS,
                "muon_auxiliary_lrs": MUON_AUXILIARY_LRS,
                "schedules": SCHEDULES,
            },
            "coarse_tokens": COARSE_TOKENS,
            "robust_tokens": ROBUST_TOKENS,
            "final_tokens": FINAL_TOKENS,
            "coarse_rows": coarse_rows,
            "coarse_summary": coarse_summary,
            "coarse_advancers": {
                model: {
                    family: [asdict(recipe) for recipe in recipes]
                    for family, recipes in families.items()
                }
                for model, families in coarse_advancers.items()
            },
            "robust_rows": robust_rows,
            "robust_summary": robust_summary,
            "finalists_by_family": {
                model: {
                    family: asdict(recipe)
                    for family, recipe in recipes.items()
                }
                for model, recipes in finalists.items()
            },
            "primary_recipes_locked_at_10m": {
                model: asdict(recipe) for model, recipe in primary.items()
            },
            "final_rows": final_rows,
            "final_summary": final_summary,
            "paired_candidate_minus_control_by_optimizer_family": family_deltas,
            "mean_candidate_minus_control_by_optimizer_family": {
                family: statistics.mean(values)
                for family, values in family_deltas.items()
            },
            "primary_paired_candidate_minus_control": primary_deltas,
            "primary_mean_candidate_minus_control": statistics.mean(primary_deltas),
            "all_four_primary_candidate_wins": all(
                value < 0 for value in primary_deltas
            ),
            "cloud_only_training": True,
            "gpu_count": GPU_COUNT,
        }
        write_json(output_path, result)
        run.summary.update(
            {
                "candidate_primary_optimizer": primary[CANDIDATE].family,
                "control_primary_optimizer": primary[CONTROL].family,
                "primary_mean_candidate_minus_control": result[
                    "primary_mean_candidate_minus_control"
                ],
                "all_four_primary_candidate_wins": result[
                    "all_four_primary_candidate_wins"
                ],
                "adamw_mean_candidate_minus_control": result[
                    "mean_candidate_minus_control_by_optimizer_family"
                ]["adamw"],
                "muon_mean_candidate_minus_control": result[
                    "mean_candidate_minus_control_by_optimizer_family"
                ]["muon"],
            }
        )
        return result
    finally:
        stop.set()
        if heartbeat_thread is not None:
            heartbeat_thread.join(timeout=2)
        run.finish()


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", required=True)
    parser.add_argument("--data-root", required=True)
    parser.add_argument("--heartbeat")
    arguments = parser.parse_args()
    result = run_audit(
        arguments.output,
        data_root=arguments.data_root,
        heartbeat=arguments.heartbeat,
    )
    print(result["wandb_url"], flush=True)


if __name__ == "__main__":
    main()
