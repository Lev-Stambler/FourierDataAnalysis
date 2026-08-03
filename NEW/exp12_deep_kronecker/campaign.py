"""Resumable, successive-halving WikiText campaign for Experiment 12."""

from __future__ import annotations

import gc
import json
import math
import os
import time
from dataclasses import asdict
from pathlib import Path
from typing import Any, Callable, Iterable

import numpy as np
import torch

from exp11_kronecker_debug.lm import (
    batch_indices,
    bootstrap_interval,
    cross_entropy,
    evaluate,
    gpu_snapshot,
    load_windows,
    optimizer_is_finite,
    tensor_batch,
    write_json,
)
from exp11_kronecker_debug.muon_tuning import BatchedMuon, MuonWithAuxAdamW

from .model import DeepLanguageModel, build_model, model_inventory
from .study import (
    COARSE_SEEDS,
    COARSE_TOKENS,
    FAMILIES,
    FINAL_TOKENS,
    MAX_BOUNDARY_EXPANSIONS,
    ROBUST_SEEDS,
    ROBUST_TOKENS,
    SELECTION_SEEDS,
    SELECTION_TOKENS,
    VARIANTS,
    OptimizerRecipe,
    boundary_extension,
    coarse_recipes,
    finalist_recipes,
    promotion_decision,
    recipe_slug,
    robust_recipes,
    schedule_multiplier,
    selected_recipe,
    summarize,
    token_budget,
)


SCHEMA = "exp12-deep-kronecker-campaign-v1"
CELL_SCHEMA = "exp12-training-cell-v1"
BATCH_CANDIDATES = (
    2048,
    1536,
    1280,
    1024,
    768,
    640,
    512,
    384,
    320,
    256,
    192,
    160,
    128,
    96,
    64,
)


def grouped_parameters(
    parameters: Iterable[torch.nn.Parameter], weight_decay: float
) -> list[dict[str, Any]]:
    decay: list[torch.nn.Parameter] = []
    no_decay: list[torch.nn.Parameter] = []
    for parameter in parameters:
        (decay if parameter.ndim >= 2 else no_decay).append(parameter)
    groups: list[dict[str, Any]] = []
    if decay:
        groups.append({"params": decay, "weight_decay": weight_decay})
    if no_decay:
        groups.append({"params": no_decay, "weight_decay": 0.0})
    return groups


def split_muon_parameters(
    model: DeepLanguageModel,
) -> tuple[list[torch.nn.Parameter], list[torch.nn.Parameter], dict[str, Any]]:
    """Keep packed causal vectors/scalars/tied vocab out of matrix Muon."""
    muon: list[torch.nn.Parameter] = []
    auxiliary: list[torch.nn.Parameter] = []
    muon_names: list[str] = []
    auxiliary_names: list[str] = []
    for name, parameter in model.named_parameters():
        use_muon = name.startswith("blocks.") and parameter.ndim >= 2
        destination, names = (
            (muon, muon_names) if use_muon else (auxiliary, auxiliary_names)
        )
        destination.append(parameter)
        names.append(name)
    routed = [*muon, *auxiliary]
    if not muon or not auxiliary:
        raise RuntimeError("Muon routing must produce two nonempty groups")
    if len({id(parameter) for parameter in routed}) != len(routed):
        raise RuntimeError("optimizer parameter groups overlap")
    if sum(parameter.numel() for parameter in routed) != sum(
        parameter.numel() for parameter in model.parameters()
    ):
        raise RuntimeError("optimizer parameter routing is incomplete")
    return muon, auxiliary, {
        "muon_parameter_names": muon_names,
        "auxiliary_parameter_names": auxiliary_names,
        "muon_parameters": sum(parameter.numel() for parameter in muon),
        "auxiliary_parameters": sum(parameter.numel() for parameter in auxiliary),
        "muon_tensor_count": len(muon),
        "auxiliary_tensor_count": len(auxiliary),
    }


def create_optimizer(
    model: DeepLanguageModel, recipe: OptimizerRecipe
) -> tuple[Any, dict[str, Any]]:
    if recipe.family == "adamw":
        optimizer = torch.optim.AdamW(
            grouped_parameters(model.parameters(), recipe.weight_decay),
            lr=recipe.body_lr,
            betas=(0.9, recipe.beta2),
        )
        return optimizer, {
            "family": "adamw",
            "parameters": sum(parameter.numel() for parameter in model.parameters()),
        }
    muon_parameters, auxiliary_parameters, routing = split_muon_parameters(model)
    muon = BatchedMuon(
        muon_parameters,
        lr=recipe.body_lr,
        weight_decay=recipe.weight_decay,
        momentum=recipe.momentum,
        nesterov=recipe.nesterov,
        ns_steps=recipe.ns_steps,
    )
    auxiliary = torch.optim.AdamW(
        grouped_parameters(auxiliary_parameters, recipe.weight_decay),
        lr=recipe.auxiliary_lr,
        betas=(0.9, recipe.beta2),
    )
    return MuonWithAuxAdamW(muon, auxiliary), {"family": "muon", **routing}


def set_learning_rates(
    optimizer: Any, recipe: OptimizerRecipe, tokens_seen: int
) -> tuple[float, float]:
    multiplier = schedule_multiplier(recipe, tokens_seen)
    body_lr = recipe.body_lr * multiplier
    auxiliary_lr = recipe.auxiliary_lr * multiplier
    if recipe.family == "adamw":
        for group in optimizer.param_groups:
            group["lr"] = body_lr
        return body_lr, body_lr
    for group in optimizer.muon.param_groups:
        group["lr"] = body_lr
    for group in optimizer.auxiliary.param_groups:
        group["lr"] = auxiliary_lr
    return body_lr, auxiliary_lr


def benchmark_batch(
    variant: str,
    recipe: OptimizerRecipe,
    batch: int,
    windows: np.ndarray,
    device: torch.device,
    *,
    use_compile: bool,
    measured_steps: int = 3,
) -> dict[str, Any]:
    torch.manual_seed(1212)
    torch.cuda.empty_cache()
    model = build_model(variant).to(device)
    optimizer, _ = create_optimizer(model, recipe)
    wrapped = torch.compile(model) if use_compile else model
    torch.cuda.reset_peak_memory_stats(device)
    elapsed = 0.0
    warmup_seconds = 0.0
    last_loss = 0.0
    for step in range(measured_steps + 1):
        inputs, targets = tensor_batch(
            windows, batch_indices(len(windows), step, batch, 1212), device
        )
        optimizer.zero_grad(set_to_none=True)
        torch.cuda.synchronize(device)
        started = time.perf_counter()
        with torch.autocast("cuda", dtype=torch.bfloat16):
            loss = cross_entropy(wrapped(inputs), targets)
        loss.backward()
        norm = torch.nn.utils.clip_grad_norm_(model.parameters(), recipe.clip_norm)
        if not torch.isfinite(loss) or not torch.isfinite(norm):
            raise RuntimeError("non-finite forward/backward in paid preflight")
        optimizer.step()
        if not optimizer_is_finite(optimizer):
            raise RuntimeError("non-finite optimizer state in paid preflight")
        torch.cuda.synchronize(device)
        duration = time.perf_counter() - started
        if step:
            elapsed += duration
            last_loss = float(loss.detach())
        else:
            warmup_seconds = duration
    tokens = measured_steps * batch * 256
    result = {
        "variant": variant,
        "optimizer_family": recipe.family,
        "batch": batch,
        "global_examples_per_step": batch,
        "global_tokens_per_step": batch * 256,
        "gradient_accumulation": 1,
        "tokens_per_second": tokens / elapsed,
        "step_seconds": elapsed / measured_steps,
        "warmup_or_compile_seconds": warmup_seconds,
        "execution_mode": "compiled" if use_compile else "eager",
        "peak_allocated_gib": torch.cuda.max_memory_allocated(device) / 2**30,
        "peak_reserved_gib": torch.cuda.max_memory_reserved(device) / 2**30,
        "last_nll": last_loss,
        "finite_forward_backward_optimizer": True,
        **gpu_snapshot(device),
    }
    del optimizer, wrapped, model, inputs, targets, loss
    gc.collect()
    torch.cuda.empty_cache()
    return result


def paid_preflight(
    windows: np.ndarray,
    device: torch.device,
    log: Callable[[dict[str, Any]], None],
) -> dict[str, Any]:
    if torch.cuda.device_count() != 1:
        raise RuntimeError("Exp12 pilot requires exactly one visible GPU")
    selected: dict[str, dict[str, Any]] = {}
    curves: dict[str, Any] = {}
    defaults = {
        (variant, family): next(
            recipe
            for recipe in coarse_recipes(variant)
            if recipe.family == family
            and recipe.body_lr
            == (0.003 if family == "adamw" else 0.03)
        )
        for variant in VARIANTS
        for family in FAMILIES
    }
    for variant in VARIANTS:
        for family in FAMILIES:
            key = f"{variant}/{family}"
            stable: list[dict[str, Any]] = []
            failures: list[dict[str, Any]] = []
            recipe = defaults[(variant, family)]
            for batch in BATCH_CANDIDATES:
                try:
                    row = benchmark_batch(
                        variant, recipe, batch, windows, device, use_compile=False
                    )
                    stable.append(row)
                    log({f"preflight/{key}/{k}": v for k, v in row.items()})
                    if len(stable) == 3:
                        break
                except (torch.OutOfMemoryError, RuntimeError) as error:
                    if "out of memory" not in str(error).lower():
                        raise
                    failures.append({"batch": batch, "reason": "out_of_memory"})
                    gc.collect()
                    torch.cuda.empty_cache()
            if not stable:
                raise RuntimeError(f"no stable batch for {key}")
            eager = max(stable, key=lambda row: row["tokens_per_second"])
            candidates = [*stable]
            try:
                compiled = benchmark_batch(
                    variant,
                    recipe,
                    int(eager["batch"]),
                    windows,
                    device,
                    use_compile=True,
                )
                candidates.append(compiled)
                log({f"preflight/{key}/compiled/{k}": v for k, v in compiled.items()})
            except (torch.OutOfMemoryError, RuntimeError) as error:
                if "out of memory" not in str(error).lower():
                    raise
                failures.append(
                    {"batch": eager["batch"], "reason": "compiled_out_of_memory"}
                )
                gc.collect()
                torch.cuda.empty_cache()
            selected[key] = max(
                candidates, key=lambda row: row["tokens_per_second"]
            )
            curves[key] = {"measurements": candidates, "failures": failures}
    return {"selected": selected, "curves": curves}


def cell_directory(
    root: Path, variant: str, recipe: OptimizerRecipe, seed: int
) -> Path:
    return root / "cells" / variant / f"recipe-{recipe_slug(recipe)}" / f"seed-{seed}"


def train_cell(
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
    use_compile: bool,
    log: Callable[[dict[str, Any]], None],
    heartbeat: Path | None,
) -> dict[str, Any]:
    directory = cell_directory(output_root, variant, recipe, seed)
    directory.mkdir(parents=True, exist_ok=True)
    result_path = directory / f"result-{target_tokens}.json"
    if result_path.is_file():
        existing = json.loads(result_path.read_text())
        if existing.get("schema") == CELL_SCHEMA and existing.get("status") == "complete":
            return existing
    torch.manual_seed(seed)
    model = build_model(variant).to(device)
    optimizer, routing = create_optimizer(model, recipe)
    wrapped = torch.compile(model) if use_compile else model
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
        tokens_seen = (step + 1) * tokens_per_step
        body_lr, auxiliary_lr = set_learning_rates(optimizer, recipe, tokens_seen)
        torch.cuda.synchronize(device)
        step_started = time.perf_counter()
        with torch.autocast("cuda", dtype=torch.bfloat16):
            loss = cross_entropy(wrapped(inputs), targets)
        loss.backward()
        norm = torch.nn.utils.clip_grad_norm_(model.parameters(), recipe.clip_norm)
        if not torch.isfinite(loss) or not torch.isfinite(norm):
            raise RuntimeError("non-finite training state")
        clipped_steps += int(float(norm.detach()) > recipe.clip_norm)
        measured_steps += 1
        optimizer.step()
        if not optimizer_is_finite(optimizer):
            raise RuntimeError("non-finite optimizer state")
        torch.cuda.synchronize(device)
        duration = time.perf_counter() - step_started
        if step > completed_steps:
            timed_seconds += duration
            timed_tokens += tokens_per_step
        last = {
            "train/variant": variant,
            "train/optimizer_family": recipe.family,
            "train/recipe": recipe_slug(recipe),
            "train/body_lr": body_lr,
            "train/auxiliary_lr": auxiliary_lr,
            "train/schedule": recipe.schedule,
            "train/seed": seed,
            "train/step": step + 1,
            "train/tokens_seen": tokens_seen,
            "train/global_examples_per_step": batch,
            "train/global_tokens_per_step": tokens_per_step,
            "train/gradient_accumulation": 1,
            "train/execution_mode": "compiled" if use_compile else "eager",
            "train/nll": float(loss.detach()),
            "train/grad_norm": float(norm.detach()),
            "train/clip_fraction": clipped_steps / measured_steps,
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
        "schema": "exp12-checkpoint-v1",
        "identity": identity,
        "model": model.state_dict(),
        "optimizer": optimizer.state_dict(),
        "steps": target_steps,
        "tokens_seen": target_steps * tokens_per_step,
    }
    temporary = checkpoint_path.with_name(checkpoint_path.name + ".tmp")
    torch.save(saved, temporary)
    temporary.replace(checkpoint_path)
    result = {
        "schema": CELL_SCHEMA,
        "status": "complete",
        "variant": variant,
        "recipe": asdict(recipe),
        "recipe_slug": recipe_slug(recipe),
        "seed": seed,
        "target_tokens": target_tokens,
        "tokens_seen": target_steps * tokens_per_step,
        "inventory": model_inventory(model),
        "optimizer_routing": routing,
        "validation": validation,
        "performance": last,
        "elapsed_seconds_this_invocation": time.perf_counter() - started,
        "checkpoint": str(checkpoint_path),
    }
    write_json(result_path, result)
    log(
        {
            f"cell/{variant}/{recipe_slug(recipe)}/seed-{seed}/validation_nll": validation[
                "nll"
            ],
            f"cell/{variant}/{recipe_slug(recipe)}/seed-{seed}/tokens": result[
                "tokens_seen"
            ],
        }
    )
    del optimizer, wrapped, model, saved
    gc.collect()
    torch.cuda.empty_cache()
    return result


def safe_train_cell(*args: Any, **kwargs: Any) -> dict[str, Any]:
    try:
        return train_cell(*args, **kwargs)
    except (RuntimeError, torch.OutOfMemoryError) as error:
        variant, recipe, seed, target_tokens = args[:4]
        result = {
            "schema": CELL_SCHEMA,
            "status": "failed",
            "variant": variant,
            "recipe": asdict(recipe),
            "recipe_slug": recipe_slug(recipe),
            "seed": seed,
            "target_tokens": target_tokens,
            "failure": str(error)[:1000],
        }
        directory = cell_directory(kwargs["output_root"], variant, recipe, seed)
        write_json(directory / f"result-{target_tokens}.json", result)
        gc.collect()
        torch.cuda.empty_cache()
        return result


def load_checkpoint_model(cell: dict[str, Any], device: torch.device) -> DeepLanguageModel:
    model = build_model(str(cell["variant"])).to(device)
    saved = torch.load(cell["checkpoint"], map_location="cpu", weights_only=True)
    model.load_state_dict(saved["model"])
    return model


def run_campaign(
    output: str | Path,
    *,
    data_root: str | Path,
    heartbeat: str | Path | None = None,
    wall_limit_seconds: float = 3000.0,
) -> dict[str, Any]:
    if not torch.cuda.is_available() or torch.cuda.device_count() != 1:
        raise RuntimeError("Exp12 GPU campaign requires exactly one visible CUDA GPU")
    if not os.environ.get("WANDB_API_KEY"):
        raise RuntimeError("WANDB_API_KEY is required before paid training")
    import wandb

    wandb.login(key=os.environ["WANDB_API_KEY"], verify=True)
    run = wandb.init(
        project="exp12-deep-kronecker",
        name="exp12-successive-halving",
        config={
            "schema": SCHEMA,
            "variants": VARIANTS,
            "optimizer_families": FAMILIES,
            "token_budget": token_budget(),
            "selection_is_validation_only": True,
        },
    )
    if not run.url:
        raise RuntimeError("W&B did not provide a direct run URL")
    print(f"WANDB_URL={run.url}", flush=True)
    output_path = Path(output)
    root = output_path.parent / "campaign-cells"
    heartbeat_path = Path(heartbeat) if heartbeat else None
    write_json(
        output_path,
        {"schema": SCHEMA, "status": "running", "wandb_url": run.url},
    )
    log_step = 0

    def log(values: dict[str, Any]) -> None:
        nonlocal log_step
        log_step += 1
        run.log(values, step=log_step)
        if heartbeat_path:
            heartbeat_path.parent.mkdir(parents=True, exist_ok=True)
            heartbeat_path.touch()

    train_windows = load_windows(Path(data_root), "train")
    validation_windows = load_windows(Path(data_root), "validation")
    device = torch.device("cuda:0")
    preflight_started = time.perf_counter()
    preflight = paid_preflight(train_windows, device, log)
    preflight["elapsed_seconds"] = time.perf_counter() - preflight_started
    write_json(root / "preflight.json", preflight)
    minimum_throughput = min(
        row["tokens_per_second"] for row in preflight["selected"].values()
    )
    projected_pre_promotion = (
        token_budget()["pre_promotion_tokens"] / minimum_throughput * 1.25
    )
    available = wall_limit_seconds * 0.94 - preflight["elapsed_seconds"]
    if projected_pre_promotion > available:
        result = {
            "schema": SCHEMA,
            "status": "complete",
            "verdict": "insufficient_measured_throughput",
            "projected_pre_promotion_seconds": projected_pre_promotion,
            "available_seconds": available,
            "preflight": preflight,
            "wandb_url": run.url,
            "test_opened_after_selection": False,
        }
        write_json(output_path, result)
        run.finish()
        return result

    def train(
        variant: str, recipe: OptimizerRecipe, seed: int, tokens: int
    ) -> dict[str, Any]:
        key = f"{variant}/{recipe.family}"
        measured = preflight["selected"][key]
        return safe_train_cell(
            variant,
            recipe,
            seed,
            tokens,
            output_root=root,
            train_windows=train_windows,
            validation_windows=validation_windows,
            batch=int(measured["batch"]),
            eval_batch=min(256, int(measured["batch"])),
            device=device,
            use_compile=measured["execution_mode"] == "compiled",
            log=log,
            heartbeat=heartbeat_path,
        )

    stages: dict[str, Any] = {}
    selections: dict[str, dict[str, Any]] = {}
    selection_cells: dict[str, list[dict[str, Any]]] = {}
    for variant in VARIANTS:
        recipes = coarse_recipes(variant)
        coarse_rows = [
            train(variant, recipe, seed, COARSE_TOKENS)
            for recipe in recipes
            for seed in COARSE_SEEDS
        ]
        for family in FAMILIES:
            for _ in range(MAX_BOUNDARY_EXPANSIONS):
                family_rows = [
                    row for row in coarse_rows if row["recipe"]["family"] == family
                ]
                summaries = summarize(family_rows, COARSE_SEEDS)
                if not summaries:
                    break
                winner = OptimizerRecipe(**summaries[0]["recipe"])
                extension = boundary_extension(winner, recipes)
                if extension is None or extension in recipes:
                    break
                recipes.append(extension)
                coarse_rows.extend(
                    train(variant, extension, seed, COARSE_TOKENS)
                    for seed in COARSE_SEEDS
                )
        robust = robust_recipes(coarse_rows)
        robust_rows = [
            train(variant, recipe, seed, ROBUST_TOKENS)
            for recipe in robust
            for seed in ROBUST_SEEDS
        ]
        finalists = finalist_recipes(robust_rows)
        chosen_rows = [
            train(variant, recipe, seed, SELECTION_TOKENS)
            for recipe in finalists
            for seed in SELECTION_SEEDS
        ]
        selections[variant] = selected_recipe(chosen_rows)
        selection_cells[variant] = chosen_rows
        stages[variant] = {
            "coarse": coarse_rows,
            "robust": robust_rows,
            "selection": chosen_rows,
        }
        log(
            {
                f"selection/{variant}/validation_nll": selections[variant][
                    "mean_validation_nll"
                ]
            }
        )

    inventories = {variant: model_inventory(build_model(variant)) for variant in VARIANTS}
    selected_throughput = {
        variant: preflight["selected"][
            f"{variant}/{selections[variant]['recipe']['family']}"
        ]["tokens_per_second"]
        for variant in VARIANTS
    }
    promotion = promotion_decision(
        selections["deep-kron-r8"],
        selections["transformer"],
        body_parameter_ratio=(
            inventories["deep-kron-r8"]["body_parameters"]
            / inventories["transformer"]["body_parameters"]
        ),
        throughput_ratio=(
            selected_throughput["deep-kron-r8"]
            / selected_throughput["transformer"]
        ),
    )
    log(
        {
            "promotion/pass": int(promotion["status"] == "pass"),
            "promotion/mean_validation_delta": promotion[
                "mean_deep_minus_transformer_validation_nll"
            ],
        }
    )
    if promotion["status"] != "pass":
        result = {
            "schema": SCHEMA,
            "status": "complete",
            "verdict": "deep_kronecker_did_not_reach_final_promotion",
            "wandb_url": run.url,
            "preflight": preflight,
            "inventories": inventories,
            "stages": stages,
            "selections": selections,
            "promotion": promotion,
            "test_opened_after_selection": False,
        }
        write_json(output_path, result)
        run.finish()
        return result

    finals: dict[str, list[dict[str, Any]]] = {}
    test_windows = load_windows(Path(data_root), "test")
    for variant in VARIANTS:
        recipe = OptimizerRecipe(**selections[variant]["recipe"])
        cells = [
            train(variant, recipe, seed, FINAL_TOKENS)
            for seed in SELECTION_SEEDS
        ]
        tested: list[dict[str, Any]] = []
        key = f"{variant}/{recipe.family}"
        eval_batch = min(256, int(preflight["selected"][key]["batch"]))
        for cell in cells:
            model = load_checkpoint_model(cell, device)
            tested.append(
                {**cell, "test": evaluate(model, test_windows, eval_batch, device)}
            )
            del model
            gc.collect()
            torch.cuda.empty_cache()
        finals[variant] = tested
    deep_tests = [float(cell["test"]["nll"]) for cell in finals["deep-kron-r8"]]
    control_tests = [float(cell["test"]["nll"]) for cell in finals["transformer"]]
    deltas = [a - b for a, b in zip(deep_tests, control_tests, strict=True)]
    interval = bootstrap_interval(deltas)
    quality_pass = interval[1] <= 0.02
    result = {
        "schema": SCHEMA,
        "status": "complete",
        "verdict": (
            "deep_kronecker_frontier_win"
            if quality_pass
            else "deep_kronecker_failed_held_out_quality_gate"
        ),
        "wandb_url": run.url,
        "preflight": preflight,
        "inventories": inventories,
        "stages": stages,
        "selections": selections,
        "promotion": promotion,
        "finals": finals,
        "matched_test_comparison": {
            "paired_deep_minus_transformer_nll": deltas,
            "mean_deep_minus_transformer_nll": sum(deltas) / len(deltas),
            "bootstrap_95_percent_interval": interval,
            "maximum_allowed_upper_bound": 0.02,
            "pass": quality_pass,
        },
        "test_opened_after_selection": True,
    }
    write_json(output_path, result)
    run.finish()
    return result
