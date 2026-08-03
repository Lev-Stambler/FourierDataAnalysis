"""Robust, resumable optimizer tuning for the Experiment 11 WikiText pilot."""

from __future__ import annotations

import gc
import hashlib
import json
import math
import os
import time
from dataclasses import asdict, dataclass, replace
from pathlib import Path
from typing import Any, Callable

import numpy as np
import torch

from .lm import (
    batch_indices,
    bootstrap_interval,
    cross_entropy,
    evaluate,
    load_windows,
    optimizer_is_finite,
    preflight,
    tensor_batch,
    write_json,
)
from .model import LanguageModel, build_model, model_inventory


SCHEMA = "exp11-optimizer-tuning-v1"
TUNING_VARIANTS = ("order3-r8", "transformer")
LR_GRIDS = {
    "order3-r4": (0.01, 0.03, 0.06, 0.12),
    "order3-r8": (0.01, 0.03, 0.06, 0.12),
    "transformer": (0.0005, 0.0015, 0.003, 0.006),
}
SCHEDULES = ("constant", "warmup-cosine")
TUNING_SEEDS = (0, 1, 2)
SCREEN_TOKENS = 10_000_000
FINAL_TOKENS = 40_000_000
WARMUP_TOKENS = 2_000_000
MAX_BOUNDARY_EXPANSIONS = 2
TUNING_BATCH_CANDIDATES = (
    2048,
    1536,
    1280,
    1024,
    768,
    640,
    512,
    384,
    256,
)


@dataclass(frozen=True)
class OptimizerRecipe:
    peak_lr: float
    schedule: str
    weight_decay: float = 0.01
    beta2: float = 0.95
    clip_norm: float = 1.0
    warmup_tokens: int = WARMUP_TOKENS
    horizon_tokens: int = FINAL_TOKENS
    minimum_lr_ratio: float = 0.1

    def __post_init__(self) -> None:
        if self.schedule not in SCHEDULES:
            raise ValueError(f"unsupported schedule: {self.schedule}")
        if self.peak_lr <= 0 or self.weight_decay < 0 or self.clip_norm <= 0:
            raise ValueError("optimizer recipe values must be positive")
        if not 0 < self.beta2 < 1:
            raise ValueError("beta2 must be between zero and one")
        if not 0 <= self.minimum_lr_ratio <= 1:
            raise ValueError("minimum LR ratio must be in [0, 1]")


def learning_rate_at_tokens(recipe: OptimizerRecipe, tokens_seen: int) -> float:
    """Return a schedule value expressed entirely in prediction tokens."""
    if recipe.schedule == "constant":
        return recipe.peak_lr
    if tokens_seen <= recipe.warmup_tokens:
        return recipe.peak_lr * max(tokens_seen, 1) / recipe.warmup_tokens
    progress = min(
        1.0,
        (tokens_seen - recipe.warmup_tokens)
        / max(1, recipe.horizon_tokens - recipe.warmup_tokens),
    )
    cosine = 0.5 * (1.0 + math.cos(math.pi * progress))
    multiplier = recipe.minimum_lr_ratio + (1.0 - recipe.minimum_lr_ratio) * cosine
    return recipe.peak_lr * multiplier


def recipe_slug(recipe: OptimizerRecipe) -> str:
    payload = json.dumps(asdict(recipe), sort_keys=True, separators=(",", ":"))
    return hashlib.sha256(payload.encode()).hexdigest()[:16]


def recipe_directory(root: Path, variant: str, recipe: OptimizerRecipe, seed: int) -> Path:
    return root / "cells" / variant / f"recipe-{recipe_slug(recipe)}" / f"seed-{seed}"


def grouped_parameters(
    model: LanguageModel, weight_decay: float
) -> list[dict[str, Any]]:
    decay: list[torch.nn.Parameter] = []
    no_decay: list[torch.nn.Parameter] = []
    for parameter in model.parameters():
        if not parameter.requires_grad:
            continue
        (decay if parameter.ndim >= 2 else no_decay).append(parameter)
    return [
        {"params": decay, "weight_decay": weight_decay},
        {"params": no_decay, "weight_decay": 0.0},
    ]


def load_tuned_result(path: Path) -> dict[str, Any] | None:
    if not path.is_file():
        return None
    value = json.loads(path.read_text())
    return value if value.get("schema") == "exp11-tuned-cell-v1" else None


def train_tuned_cell(
    variant: str,
    recipe: OptimizerRecipe,
    seed: int,
    target_tokens: int,
    *,
    output_root: Path,
    train_windows: np.ndarray,
    validation_windows: np.ndarray,
    batch: int,
    eval_batch: int,
    device: torch.device,
    log: Callable[[dict[str, Any]], None],
    heartbeat: Path | None,
    use_compile: bool,
) -> dict[str, Any]:
    directory = recipe_directory(output_root, variant, recipe, seed)
    directory.mkdir(parents=True, exist_ok=True)
    result_path = directory / f"result-{target_tokens}.json"
    existing = load_tuned_result(result_path)
    if existing and existing.get("status") == "complete":
        return existing

    torch.manual_seed(seed)
    model = build_model(variant).to(device)
    wrapped = torch.compile(model) if use_compile else model
    optimizer = torch.optim.AdamW(
        grouped_parameters(model, recipe.weight_decay),
        lr=recipe.peak_lr,
        betas=(0.9, recipe.beta2),
    )
    identity = {"variant": variant, "recipe": asdict(recipe), "seed": seed}
    checkpoint_path = directory / "checkpoint.pt"
    completed_steps = 0
    if checkpoint_path.is_file():
        saved = torch.load(checkpoint_path, map_location="cpu", weights_only=True)
        if saved.get("identity") != identity:
            raise RuntimeError(f"checkpoint identity mismatch: {checkpoint_path}")
        model.load_state_dict(saved["model"])
        optimizer.load_state_dict(saved["optimizer"])
        completed_steps = int(saved["steps"])

    tokens_per_step = batch * 256
    target_steps = math.ceil(target_tokens / tokens_per_step)
    started = time.perf_counter()
    timed_seconds = 0.0
    timed_tokens = 0
    clipped_steps = 0
    measured_steps = 0
    last: dict[str, Any] = {}
    for step in range(completed_steps, target_steps):
        inputs, targets = tensor_batch(
            train_windows, batch_indices(len(train_windows), step, batch, seed), device
        )
        optimizer.zero_grad(set_to_none=True)
        lr = learning_rate_at_tokens(recipe, (step + 1) * tokens_per_step)
        for group in optimizer.param_groups:
            group["lr"] = lr
        torch.cuda.synchronize(device)
        step_started = time.perf_counter()
        with torch.autocast("cuda", dtype=torch.bfloat16):
            loss = cross_entropy(wrapped(inputs), targets)
        loss.backward()
        norm = torch.nn.utils.clip_grad_norm_(model.parameters(), recipe.clip_norm)
        if not torch.isfinite(loss) or not torch.isfinite(norm):
            raise RuntimeError(
                f"non-finite state in {variant} recipe={recipe_slug(recipe)} seed={seed}"
            )
        clipped_steps += int(float(norm.detach()) > recipe.clip_norm)
        measured_steps += 1
        optimizer.step()
        if not optimizer_is_finite(optimizer):
            raise RuntimeError(
                f"non-finite optimizer in {variant} recipe={recipe_slug(recipe)} seed={seed}"
            )
        torch.cuda.synchronize(device)
        duration = time.perf_counter() - step_started
        if step > completed_steps:
            timed_seconds += duration
            timed_tokens += tokens_per_step
        tokens_seen = (step + 1) * tokens_per_step
        last = {
            "train/variant": variant,
            "train/recipe": recipe_slug(recipe),
            "train/peak_lr": recipe.peak_lr,
            "train/current_lr": lr,
            "train/schedule": recipe.schedule,
            "train/weight_decay": recipe.weight_decay,
            "train/beta2": recipe.beta2,
            "train/seed": seed,
            "train/step": step + 1,
            "train/tokens_seen": tokens_seen,
            "train/global_examples_per_step": batch,
            "train/global_tokens_per_step": tokens_per_step,
            "train/gradient_accumulation": 1,
            "train/execution_mode": "compiled" if use_compile else "eager",
            "train/nll": float(loss.detach()),
            "train/grad_norm": float(norm.detach()),
            "train/clip_fraction_this_invocation": clipped_steps / measured_steps,
            "train/step_seconds": duration,
            "train/tokens_per_second": (
                timed_tokens / timed_seconds if timed_seconds else tokens_per_step / duration
            ),
            "train/peak_allocated_gib": torch.cuda.max_memory_allocated(device) / 2**30,
            "train/peak_reserved_gib": torch.cuda.max_memory_reserved(device) / 2**30,
        }
        if (step + 1) % 20 == 0 or step + 1 == target_steps:
            log(last)
            print(json.dumps(last, sort_keys=True), flush=True)
            if heartbeat:
                heartbeat.parent.mkdir(parents=True, exist_ok=True)
                heartbeat.touch()

    validation = evaluate(model, validation_windows, eval_batch, device)
    saved = {
        "schema": "exp11-tuned-checkpoint-v1",
        "identity": identity,
        "model": model.state_dict(),
        "optimizer": optimizer.state_dict(),
        "steps": target_steps,
        "tokens_seen": target_steps * tokens_per_step,
        "global_examples_per_step": batch,
        "global_tokens_per_step": tokens_per_step,
    }
    checkpoint_temporary = checkpoint_path.with_name(checkpoint_path.name + ".tmp")
    torch.save(saved, checkpoint_temporary)
    checkpoint_temporary.replace(checkpoint_path)
    result = {
        "schema": "exp11-tuned-cell-v1",
        "status": "complete",
        "variant": variant,
        "recipe": asdict(recipe),
        "recipe_slug": recipe_slug(recipe),
        "seed": seed,
        "target_tokens": target_tokens,
        "tokens_seen": target_steps * tokens_per_step,
        "inventory": model_inventory(model),
        "validation": validation,
        "performance": last,
        "elapsed_seconds_this_invocation": time.perf_counter() - started,
        "checkpoint": str(checkpoint_path),
    }
    write_json(result_path, result)
    log(
        {
            f"tune/{variant}/{recipe_slug(recipe)}/seed-{seed}/validation_nll": validation[
                "nll"
            ],
            f"tune/{variant}/{recipe_slug(recipe)}/seed-{seed}/tokens": result[
                "tokens_seen"
            ],
        }
    )
    del optimizer, wrapped, model, saved
    gc.collect()
    torch.cuda.empty_cache()
    return result


def safe_train_tuned_cell(*args: Any, **kwargs: Any) -> dict[str, Any]:
    """Turn expected optimizer divergence into an auditable tuning result."""
    try:
        return train_tuned_cell(*args, **kwargs)
    except RuntimeError as error:
        if "non-finite" not in str(error).lower():
            raise
        variant = str(args[0])
        recipe = args[1]
        seed = int(args[2])
        target_tokens = int(args[3])
        result = {
            "schema": "exp11-tuned-cell-v1",
            "status": "failed",
            "variant": variant,
            "recipe": asdict(recipe),
            "recipe_slug": recipe_slug(recipe),
            "seed": seed,
            "target_tokens": target_tokens,
            "failure": str(error),
        }
        log = kwargs["log"]
        log(
            {
                f"tune/{variant}/{recipe_slug(recipe)}/seed-{seed}/diverged": 1,
                f"tune/{variant}/{recipe_slug(recipe)}/seed-{seed}/tokens": target_tokens,
            }
        )
        gc.collect()
        torch.cuda.empty_cache()
        return result


def summarize_recipes(rows: list[dict[str, Any]]) -> list[dict[str, Any]]:
    grouped: dict[str, list[dict[str, Any]]] = {}
    for row in rows:
        grouped.setdefault(str(row["recipe_slug"]), []).append(row)
    summaries = []
    for slug, values in grouped.items():
        if {int(value["seed"]) for value in values} != set(TUNING_SEEDS):
            continue
        failed = [value for value in values if value.get("status") != "complete"]
        nlls = [float(value["validation"]["nll"]) for value in values if not failed]
        mean_nll = 1e30 if failed else float(np.mean(nlls))
        std_nll = 1e30 if failed else float(np.std(nlls))
        worst_nll = 1e30 if failed else max(nlls)
        summaries.append(
            {
                "recipe_slug": slug,
                "recipe": values[0]["recipe"],
                "validation_nll_by_seed": {
                    str(value["seed"]): (
                        float(value["validation"]["nll"])
                        if value.get("status") == "complete"
                        else None
                    )
                    for value in sorted(values, key=lambda item: item["seed"])
                },
                "failed_seeds": sorted(int(value["seed"]) for value in failed),
                "mean_validation_nll": mean_nll,
                "std_validation_nll": std_nll,
                "worst_validation_nll": worst_nll,
            }
        )
    return sorted(
        summaries,
        key=lambda item: (
            item["mean_validation_nll"],
            item["std_validation_nll"],
            item["worst_validation_nll"],
        ),
    )


def screen_recipes(variant: str) -> list[OptimizerRecipe]:
    return [
        OptimizerRecipe(peak_lr=lr, schedule=schedule)
        for schedule in SCHEDULES
        for lr in LR_GRIDS[variant]
    ]


def boundary_extension(
    best: OptimizerRecipe, observed: list[OptimizerRecipe]
) -> OptimizerRecipe | None:
    comparable = sorted(
        {
            recipe.peak_lr
            for recipe in observed
            if recipe.schedule == best.schedule
            and recipe.weight_decay == best.weight_decay
            and recipe.beta2 == best.beta2
            and recipe.clip_norm == best.clip_norm
        }
    )
    if best.peak_lr == comparable[0]:
        return replace(best, peak_lr=best.peak_lr / 2)
    if best.peak_lr == comparable[-1]:
        return replace(best, peak_lr=best.peak_lr * 2)
    return None


def projected_tuning_seconds(preflight_result: dict[str, Any]) -> float:
    seconds = 0.0
    initial_cells = len(SCHEDULES) * len(next(iter(LR_GRIDS.values()))) * len(TUNING_SEEDS)
    optimizer_ablation_cells = 2 * len(TUNING_SEEDS)
    boundary_reserve_cells = MAX_BOUNDARY_EXPANSIONS * len(TUNING_SEEDS)
    for variant in TUNING_VARIANTS:
        speed = float(preflight_result["selected"][variant]["tokens_per_second"])
        screen_tokens = (
            initial_cells + optimizer_ablation_cells + boundary_reserve_cells
        ) * SCREEN_TOKENS
        final_extension_tokens = len(TUNING_SEEDS) * (FINAL_TOKENS - SCREEN_TOKENS)
        seconds += (screen_tokens + final_extension_tokens) / speed
    return seconds


def load_checkpoint_model(result: dict[str, Any], device: torch.device) -> LanguageModel:
    saved = torch.load(result["checkpoint"], map_location="cpu", weights_only=True)
    model = build_model(result["variant"]).to(device)
    model.load_state_dict(saved["model"])
    return model


def matched_result(
    order3: list[dict[str, Any]], transformer: list[dict[str, Any]]
) -> dict[str, Any]:
    paired = [
        float(left["test"]["nll"] - right["test"]["nll"])
        for left, right in zip(order3, transformer, strict=True)
    ]
    interval = bootstrap_interval(paired)
    body_ratio = (
        int(order3[0]["inventory"]["body_parameters"])
        / int(transformer[0]["inventory"]["body_parameters"])
    )
    return {
        "paired_order3_minus_transformer_test_nll": paired,
        "mean_order3_minus_transformer_test_nll": float(np.mean(paired)),
        "bootstrap_95_percent_interval": interval,
        "order3_to_transformer_body_parameter_ratio": body_ratio,
        "maximum_allowed_upper_nll_bound": 0.02,
        "pass_quality": interval[1] <= 0.02,
    }


def run_tuning_campaign(
    output: str | Path,
    *,
    data_root: str | Path,
    heartbeat: str | Path | None = None,
    wall_limit_seconds: float = 1300.0,
) -> dict[str, Any]:
    if not torch.cuda.is_available() or torch.cuda.device_count() != 1:
        raise RuntimeError("optimizer tuning requires exactly one visible CUDA GPU")
    if not os.environ.get("WANDB_API_KEY"):
        raise RuntimeError("WANDB_API_KEY is required before the paid launch")
    import wandb

    wandb.login(key=os.environ["WANDB_API_KEY"], verify=True)
    run = wandb.init(
        project="exp11-kronecker-debug",
        name="exp11-robust-optimizer-tuning",
        config={
            "schema": SCHEMA,
            "variants": TUNING_VARIANTS,
            "lr_grids": LR_GRIDS,
            "schedules": SCHEDULES,
            "tuning_seeds": TUNING_SEEDS,
            "screen_tokens": SCREEN_TOKENS,
            "final_tokens": FINAL_TOKENS,
            "warmup_tokens": WARMUP_TOKENS,
            "wall_limit_seconds": wall_limit_seconds,
        },
    )
    if not run.url:
        raise RuntimeError("W&B did not provide a direct run URL")
    print(f"WANDB_URL={run.url}", flush=True)
    output_path = Path(output)
    root = output_path.parent / "tuning-cells"
    heartbeat_path = Path(heartbeat) if heartbeat else None
    write_json(
        output_path,
        {
            "schema": SCHEMA,
            "status": "running",
            "wandb_url": run.url,
            "started_at_unix_seconds": time.time(),
        },
    )
    log_index = 0

    def log(values: dict[str, Any]) -> None:
        nonlocal log_index
        log_index += 1
        run.log(values, step=log_index)
        if heartbeat_path:
            heartbeat_path.parent.mkdir(parents=True, exist_ok=True)
            heartbeat_path.touch()

    if heartbeat_path:
        heartbeat_path.parent.mkdir(parents=True, exist_ok=True)
        heartbeat_path.touch()

    train_windows = load_windows(Path(data_root), "train")
    validation_windows = load_windows(Path(data_root), "validation")
    device = torch.device("cuda:0")
    preflight_started = time.perf_counter()
    measured = preflight(
        train_windows,
        device,
        candidates=TUNING_BATCH_CANDIDATES,
        variants=TUNING_VARIANTS,
        log=log,
    )
    preflight_seconds = time.perf_counter() - preflight_started
    measured["elapsed_seconds"] = preflight_seconds
    write_json(root / "preflight.json", measured)
    projected = projected_tuning_seconds(measured)
    available = wall_limit_seconds * 0.96 - preflight_seconds
    if projected > available:
        result = {
            "schema": SCHEMA,
            "status": "complete",
            "verdict": "insufficient_measured_throughput_for_tuning",
            "projected_training_seconds": projected,
            "available_training_seconds": available,
            "preflight": measured,
            "wandb_url": run.url,
        }
        write_json(output_path, result)
        run.finish()
        return result

    batches = {
        variant: int(measured["selected"][variant]["batch"])
        for variant in TUNING_VARIANTS
    }
    compile_modes = {
        variant: measured["selected"][variant]["execution_mode"] == "compiled"
        for variant in TUNING_VARIANTS
    }
    eval_batches = {variant: min(512, batches[variant]) for variant in TUNING_VARIANTS}
    screens: dict[str, list[dict[str, Any]]] = {}
    screen_summaries: dict[str, list[dict[str, Any]]] = {}
    selected: dict[str, dict[str, Any]] = {}

    for variant in TUNING_VARIANTS:
        rows: list[dict[str, Any]] = []
        recipes = screen_recipes(variant)
        for recipe in recipes:
            for seed in TUNING_SEEDS:
                rows.append(
                    safe_train_tuned_cell(
                        variant,
                        recipe,
                        seed,
                        SCREEN_TOKENS,
                        output_root=root,
                        train_windows=train_windows,
                        validation_windows=validation_windows,
                        batch=batches[variant],
                        eval_batch=eval_batches[variant],
                        device=device,
                        log=log,
                        heartbeat=heartbeat_path,
                        use_compile=compile_modes[variant],
                    )
                )
        for _ in range(MAX_BOUNDARY_EXPANSIONS):
            summaries = summarize_recipes(rows)
            best = OptimizerRecipe(**summaries[0]["recipe"])
            extension = boundary_extension(best, recipes)
            if extension is None:
                break
            recipes.append(extension)
            for seed in TUNING_SEEDS:
                rows.append(
                    safe_train_tuned_cell(
                        variant,
                        extension,
                        seed,
                        SCREEN_TOKENS,
                        output_root=root,
                        train_windows=train_windows,
                        validation_windows=validation_windows,
                        batch=batches[variant],
                        eval_batch=eval_batches[variant],
                        device=device,
                        log=log,
                        heartbeat=heartbeat_path,
                        use_compile=compile_modes[variant],
                    )
                )
        base_summaries = summarize_recipes(rows)
        base_best = OptimizerRecipe(**base_summaries[0]["recipe"])
        ablations = [
            replace(base_best, weight_decay=0.0),
            replace(base_best, beta2=0.99),
        ]
        for recipe in ablations:
            if recipe in recipes:
                continue
            recipes.append(recipe)
            for seed in TUNING_SEEDS:
                rows.append(
                    safe_train_tuned_cell(
                        variant,
                        recipe,
                        seed,
                        SCREEN_TOKENS,
                        output_root=root,
                        train_windows=train_windows,
                        validation_windows=validation_windows,
                        batch=batches[variant],
                        eval_batch=eval_batches[variant],
                        device=device,
                        log=log,
                        heartbeat=heartbeat_path,
                        use_compile=compile_modes[variant],
                    )
                )
        summaries = summarize_recipes(rows)
        screens[variant] = rows
        screen_summaries[variant] = summaries
        selected[variant] = summaries[0]
        log(
            {
                f"selection/{variant}/mean_validation_nll": summaries[0][
                    "mean_validation_nll"
                ],
                f"selection/{variant}/std_validation_nll": summaries[0][
                    "std_validation_nll"
                ],
            }
        )

    finals: dict[str, list[dict[str, Any]]] = {variant: [] for variant in TUNING_VARIANTS}
    for variant in TUNING_VARIANTS:
        recipe = OptimizerRecipe(**selected[variant]["recipe"])
        for seed in TUNING_SEEDS:
            cell = safe_train_tuned_cell(
                variant,
                recipe,
                seed,
                FINAL_TOKENS,
                output_root=root,
                train_windows=train_windows,
                validation_windows=validation_windows,
                batch=batches[variant],
                eval_batch=eval_batches[variant],
                device=device,
                log=log,
                heartbeat=heartbeat_path,
                use_compile=compile_modes[variant],
            )
            finals[variant].append(cell)

    unstable_finals = {
        variant: [int(cell["seed"]) for cell in cells if cell.get("status") != "complete"]
        for variant, cells in finals.items()
    }
    if any(unstable_finals.values()):
        result = {
            "schema": SCHEMA,
            "status": "complete",
            "verdict": "optimizer_unstable_at_final_horizon",
            "wandb_url": run.url,
            "preflight": measured,
            "projected_training_seconds": projected,
            "selection_basis": "lowest three-seed mean validation NLL at 10M tokens",
            "test_opened_after_selection": False,
            "screens": screens,
            "screen_summaries": screen_summaries,
            "selected": selected,
            "finals": finals,
            "unstable_final_seeds": unstable_finals,
        }
        write_json(output_path, result)
        log({"gate/stable_final_horizon": 0})
        run.finish()
        return result

    # The held-out split is opened only after all optimizer recipes are locked.
    test_windows = load_windows(Path(data_root), "test")
    for variant in TUNING_VARIANTS:
        tested = []
        for cell in finals[variant]:
            model = load_checkpoint_model(cell, device)
            tested.append(
                {
                    **cell,
                    "test": evaluate(
                        model,
                        test_windows,
                        eval_batches[variant],
                        device,
                        include_mechanism=variant.startswith("order3"),
                    ),
                }
            )
            del model
            gc.collect()
            torch.cuda.empty_cache()
        finals[variant] = tested

    r8_comparison = matched_result(finals["order3-r8"], finals["transformer"])
    r8_pass = (
        r8_comparison["pass_quality"]
        and r8_comparison["order3_to_transformer_body_parameter_ratio"] <= 1.01
    )
    verdict = "pursue_order3_parameter_efficiency" if r8_pass else "do_not_scale_yet"
    result = {
        "schema": SCHEMA,
        "status": "complete",
        "verdict": verdict,
        "wandb_url": run.url,
        "preflight": measured,
        "projected_training_seconds": projected,
        "selection_basis": "lowest three-seed mean validation NLL at 10M tokens",
        "test_opened_after_selection": True,
        "screens": screens,
        "screen_summaries": screen_summaries,
        "selected": selected,
        "finals": finals,
        "matched_comparisons": {
            "order3-r8_vs_transformer": r8_comparison,
        },
    }
    write_json(output_path, result)
    log(
        {
            "gate/order3_r8": int(r8_pass),
            "gate/r8_upper_nll": r8_comparison["bootstrap_95_percent_interval"][1],
        }
    )
    run.finish()
    return result
