"""Cloud-only one/two-example memorization ladder for matched 5.4M models."""

from __future__ import annotations

import argparse
import gc
import hashlib
import json
import math
import multiprocessing as mp
import os
import queue
import statistics
import threading
import time
from dataclasses import asdict
from pathlib import Path
from typing import Any, Iterable

import numpy as np
import torch
import torch.nn.functional as F
from fla.modules import FusedLinearCrossEntropyLoss

import exp14_block_kronecker.campaign as base
import exp17_group_density.campaign as exp17
from exp14_block_kronecker.data import (
    CONTEXT_LENGTH,
    GROUP_SIZE,
    block_batch,
    load_windows,
)
from exp15_birouted_kronecker.model import CURRENT_R8
from exp17_group_density.campaign import Recipe
from exp17_group_density.model import (
    GROUP_DEEP,
    GROUP_HYBRID,
    GROUP_R1,
    GROUP_R2,
    GROUP_R4,
    NO_ROUTER_TOKEN,
)


SCHEMA = "exp18-memorization-ladder-v1"
CELL_SCHEMA = "exp18-memorization-cell-v1"
GPU_COUNT = 8
TRANSFORMER = exp17.TRANSFORMER
MODEL_NAMES = (
    CURRENT_R8,
    NO_ROUTER_TOKEN,
    GROUP_R1,
    GROUP_R2,
    GROUP_R4,
    GROUP_HYBRID,
    GROUP_DEEP,
    TRANSFORMER,
)
TARGET_PARAMETERS = exp17.TARGET_PARAMETERS
MAXIMUM_PARAMETER_MISMATCH = exp17.MAXIMUM_PARAMETER_MISMATCH
SAMPLE_INDICES = (17, 997)
SCREEN_SEED = 1801
CONFIRMATION_SEEDS = (1802, 1803)
SCREEN_STEPS = 64
CONFIRMATION_STEPS = 128
EVALUATION_INTERVAL = 4
SUCCESS_NLL = 0.01
STRICT_NLL = 0.001
LOSS_THRESHOLDS = (1.0, 0.1, SUCCESS_NLL, STRICT_NLL)
EARLY_STOP_CONFIRMATIONS = 2
MINIMUM_GLOBAL_TOKENS = 100_000
MINIMUM_UTILIZATION = 85.0
EXECUTION_MODE = "default"


def write_json(path: str | Path, value: Any) -> None:
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_text(json.dumps(value, indent=2, sort_keys=True))
    os.replace(temporary, path)


def recipe_slug(recipe: Recipe) -> str:
    return exp17.recipe_slug(recipe)


def memorization_recipes() -> tuple[Recipe, ...]:
    """Wide, constant-LR/no-regularization screen including unstable edges."""

    adamw = tuple(
        Recipe(
            "adamw",
            lr,
            lr,
            weight_decay=0.0,
            clip_norm=1.0,
            warmup_tokens=1,
        )
        for lr in (0.00075, 0.003, 0.012, 0.048)
    )
    muon = tuple(
        Recipe(
            "muon",
            body,
            auxiliary,
            weight_decay=0.0,
            clip_norm=1.0,
            warmup_tokens=1,
        )
        for body, auxiliary in (
            (0.015, 0.003),
            (0.06, 0.012),
            (0.24, 0.048),
            (0.96, 0.096),
        )
    )
    return (*adamw, *muon)


def balanced_sample_indices(
    unique_indices: tuple[int, ...], physical_batch: int
) -> np.ndarray:
    if not unique_indices or physical_batch < len(unique_indices):
        raise ValueError("physical batch must contain every memorized example")
    return np.resize(np.asarray(unique_indices, dtype=np.int64), physical_batch)


def sample_digest(inputs: torch.Tensor, targets: torch.Tensor) -> str:
    digest = hashlib.sha256()
    digest.update(inputs.detach().cpu().numpy().tobytes())
    digest.update(targets.detach().cpu().numpy().tobytes())
    return digest.hexdigest()


def cell_path(
    root: Path,
    model: str,
    sample_count: int,
    recipe: Recipe,
    seed: int,
) -> Path:
    return (
        root
        / "cells"
        / model
        / f"samples-{sample_count}"
        / f"recipe-{recipe_slug(recipe)}-seed-{seed}.json"
    )


@torch.inference_mode()
def exact_metrics(
    model: torch.nn.Module,
    inputs: torch.Tensor,
    targets: torch.Tensor,
) -> dict[str, Any]:
    model.eval()
    logits = model(inputs).float()
    losses = F.cross_entropy(
        logits.flatten(0, 1), targets.flatten(), reduction="none"
    ).reshape_as(targets)
    predictions = logits.argmax(-1)
    correct = predictions.eq(targets)
    example_nll = losses.mean(1)
    example_accuracy = correct.float().mean(1)
    group_nll = losses.reshape(inputs.shape[0], -1, GROUP_SIZE).mean((0, 2))
    group_accuracy = (
        correct.reshape(inputs.shape[0], -1, GROUP_SIZE).float().mean((0, 2))
    )
    return {
        "nll": float(losses.mean()),
        "token_accuracy": float(correct.float().mean()),
        "correct_tokens": int(correct.sum()),
        "total_tokens": int(correct.numel()),
        "worst_example_nll": float(example_nll.max()),
        "worst_example_token_accuracy": float(example_accuracy.min()),
        "example_nll": example_nll.cpu().tolist(),
        "example_token_accuracy": example_accuracy.cpu().tolist(),
        "target_group_nll": group_nll.cpu().tolist(),
        "target_group_token_accuracy": group_accuracy.cpu().tolist(),
    }


def threshold_key(value: float) -> str:
    return f"nll_le_{value:g}"


def first_threshold_hits(curve: Iterable[dict[str, Any]]) -> dict[str, Any]:
    hits: dict[str, Any] = {}
    for threshold in LOSS_THRESHOLDS:
        qualifying = [row for row in curve if float(row["nll"]) <= threshold]
        key = threshold_key(threshold)
        hits[key] = (
            {
                "step": int(qualifying[0]["step"]),
                "elapsed_seconds": float(qualifying[0]["elapsed_seconds"]),
            }
            if qualifying
            else None
        )
    perfect = [row for row in curve if float(row["token_accuracy"]) == 1.0]
    hits["perfect_token_accuracy"] = (
        {
            "step": int(perfect[0]["step"]),
            "elapsed_seconds": float(perfect[0]["elapsed_seconds"]),
        }
        if perfect
        else None
    )
    return hits


def fit_cell(task: dict[str, Any], device: torch.device) -> dict[str, Any]:
    model_name = str(task["model"])
    recipe = Recipe(**task["recipe"])
    seed = int(task["seed"])
    sample_indices = tuple(int(value) for value in task["sample_indices"])
    sample_count = len(sample_indices)
    physical_batch = int(task["batch"])
    maximum_steps = int(task["maximum_steps"])
    root = Path(task["output_root"])
    output = cell_path(root, model_name, sample_count, recipe, seed)
    if output.is_file():
        prior = json.loads(output.read_text())
        if prior.get("status") in {"complete", "unstable"}:
            return prior

    windows = load_windows(task["data_root"], "train")
    if max(sample_indices) >= len(windows) - 1:
        raise IndexError("memorization sample lacks a continuation window")
    torch.manual_seed(seed)
    torch.cuda.empty_cache()
    torch.cuda.reset_peak_memory_stats(device)
    model = exp17.build_model(model_name).to(device)
    hidden_function = exp17.compile_hidden(model, EXECUTION_MODE)
    optimizer, routing = exp17.create_optimizer(model, recipe)
    loss_function = FusedLinearCrossEntropyLoss()
    unique_inputs, unique_targets = block_batch(
        windows, np.asarray(sample_indices, dtype=np.int64), device
    )
    physical_indices = balanced_sample_indices(
        tuple(range(sample_count)), physical_batch
    )
    gather = torch.as_tensor(physical_indices, device=device)
    inputs = unique_inputs.index_select(0, gather)
    targets = unique_targets.index_select(0, gather)
    digest = sample_digest(unique_inputs, unique_targets)
    initial = exact_metrics(model, unique_inputs, unique_targets)
    curve: list[dict[str, Any]] = [{"step": 0, "elapsed_seconds": 0.0, **initial}]
    clipped = 0
    elapsed = 0.0
    confirmations = 0
    failure: str | None = None
    last_training_nll = math.nan
    last_grad_norm = math.nan

    with base.GpuSampler(device) as sampler:
        for step in range(1, maximum_steps + 1):
            model.train()
            optimizer.zero_grad(set_to_none=True)
            body_lr, auxiliary_lr = exp17.set_learning_rates(
                optimizer, recipe, step * physical_batch * CONTEXT_LENGTH
            )
            torch.cuda.synchronize(device)
            started = time.perf_counter()
            with torch.autocast("cuda", dtype=torch.bfloat16):
                loss = base.fused_loss(
                    model, inputs, targets, loss_function, hidden_function
                )
            loss.backward()
            norm = torch.nn.utils.clip_grad_norm_(
                model.parameters(), recipe.clip_norm
            )
            if not torch.isfinite(loss) or not torch.isfinite(norm):
                failure = "non-finite forward/backward"
                break
            clipped += int(float(norm.detach()) > recipe.clip_norm)
            optimizer.step()
            if not base.optimizer_is_finite(optimizer):
                failure = "non-finite optimizer state"
                break
            torch.cuda.synchronize(device)
            elapsed += time.perf_counter() - started
            last_training_nll = float(loss.detach())
            last_grad_norm = float(norm.detach())

            should_evaluate = (
                step == 1
                or step == maximum_steps
                or step % EVALUATION_INTERVAL == 0
                or last_training_nll <= SUCCESS_NLL
            )
            if not should_evaluate:
                continue
            measured = exact_metrics(model, unique_inputs, unique_targets)
            curve.append(
                {
                    "step": step,
                    "elapsed_seconds": elapsed,
                    "body_lr": body_lr,
                    "auxiliary_lr": auxiliary_lr,
                    "training_nll": last_training_nll,
                    "grad_norm": last_grad_norm,
                    **measured,
                }
            )
            successful = (
                float(measured["nll"]) <= SUCCESS_NLL
                and float(measured["token_accuracy"]) == 1.0
            )
            confirmations = confirmations + 1 if successful else 0
            if confirmations >= EARLY_STOP_CONFIRMATIONS:
                break

    final = exact_metrics(model, unique_inputs, unique_targets)
    if not curve or int(curve[-1]["step"]) != step:
        curve.append({"step": step, "elapsed_seconds": elapsed, **final})
    success = (
        failure is None
        and float(final["nll"]) <= SUCCESS_NLL
        and float(final["token_accuracy"]) == 1.0
    )
    strict_success = success and float(final["nll"]) <= STRICT_NLL
    result = {
        "schema": CELL_SCHEMA,
        "status": "unstable" if failure else "complete",
        "failure": failure,
        "model": model_name,
        "recipe": asdict(recipe),
        "recipe_slug": recipe_slug(recipe),
        "seed": seed,
        "sample_count": sample_count,
        "sample_indices": list(sample_indices),
        "sample_sha256": digest,
        "context_length": CONTEXT_LENGTH,
        "unique_tokens_per_optimizer_step": sample_count * CONTEXT_LENGTH,
        "unique_examples_per_optimizer_step": sample_count,
        "physical_batch": physical_batch,
        "global_examples_per_step": physical_batch,
        "global_tokens_per_step": physical_batch * CONTEXT_LENGTH,
        "duplicate_factor": physical_batch / sample_count,
        "gradient_accumulation": 1,
        "maximum_steps": maximum_steps,
        "steps_completed": int(step),
        "physical_tokens_processed": int(step * physical_batch * CONTEXT_LENGTH),
        "unique_example_exposures": int(step * sample_count),
        "success": success,
        "strict_success": strict_success,
        "success_definition": {
            "maximum_nll": SUCCESS_NLL,
            "token_accuracy": 1.0,
        },
        "initial": initial,
        "final": final,
        "threshold_hits": first_threshold_hits(curve),
        "curve": curve,
        "inventory": exp17.model_inventory(model),
        "optimizer_routing": routing,
        "performance": {
            "elapsed_seconds": elapsed,
            "physical_tokens_per_second": (
                step * physical_batch * CONTEXT_LENGTH / max(elapsed, 1e-12)
            ),
            "optimizer_steps_per_second": step / max(elapsed, 1e-12),
            "clip_fraction": clipped / max(step, 1),
            "last_training_nll": last_training_nll,
            "last_grad_norm": last_grad_norm,
            "peak_allocated_gib": torch.cuda.max_memory_allocated(device) / 2**30,
            "peak_reserved_gib": torch.cuda.max_memory_reserved(device) / 2**30,
            **sampler.summary(),
        },
        "execution_mode": EXECUTION_MODE,
        "loss_implementation": "fla-fused-linear-cross-entropy-exact",
    }
    write_json(output, result)
    del optimizer, model, loss_function, inputs, targets, unique_inputs, unique_targets
    gc.collect()
    torch.cuda.empty_cache()
    return result


def execute_task(task: dict[str, Any], device: torch.device) -> dict[str, Any]:
    if task["kind"] == "memorize":
        return fit_cell(task, device)
    return exp17.execute_task(task, device)


def worker_loop(gpu_id: int, tasks: mp.Queue, results: mp.Queue) -> None:
    exp17.activate_base()
    torch.cuda.set_device(gpu_id)
    device = torch.device(f"cuda:{gpu_id}")
    while True:
        item = tasks.get()
        if item is None:
            return
        index, task = item
        try:
            row = execute_task(task, device)
        except Exception as error:
            row = {
                "status": "failed",
                "kind": task.get("kind"),
                "model": task.get("model"),
                "recipe": task.get("recipe", {}),
                "seed": task.get("seed"),
                "sample_count": len(task.get("sample_indices", [])),
                "failure": (
                    "out_of_memory"
                    if isinstance(error, torch.OutOfMemoryError)
                    or "out of memory" in str(error).lower()
                    else f"{type(error).__name__}: {str(error)[:2000]}"
                ),
            }
            gc.collect()
            torch.cuda.empty_cache()
        results.put((index, row))


def run_tasks(
    tasks: list[dict[str, Any]], timeout: int = 21_600
) -> list[dict[str, Any]]:
    if not tasks:
        return []
    context = mp.get_context("spawn")
    task_queue, result_queue = context.Queue(), context.Queue()
    processes = [
        context.Process(target=worker_loop, args=(gpu, task_queue, result_queue))
        for gpu in range(min(GPU_COUNT, len(tasks)))
    ]
    for process in processes:
        process.start()
    for index, task in enumerate(tasks):
        task_queue.put((index, task))
    for _ in processes:
        task_queue.put(None)
    rows: list[dict[str, Any] | None] = [None] * len(tasks)
    try:
        for _ in tasks:
            index, row = result_queue.get(timeout=timeout)
            rows[index] = row
    except queue.Empty as error:
        raise TimeoutError("Exp18 GPU worker timed out") from error
    finally:
        for process in processes:
            process.join(timeout=10)
            if process.is_alive():
                process.terminate()
    if any(row is None for row in rows):
        raise RuntimeError("missing Exp18 task result")
    return [row for row in rows if row is not None]


def all_complete(rows: Iterable[dict[str, Any]], label: str) -> None:
    failures = [row for row in rows if row.get("status") != "complete"]
    if failures:
        raise RuntimeError(f"{label} failed: {failures}")


def paid_preflight(data: Path, cells: Path) -> dict[str, Any]:
    path = cells / "preflight.json"
    if path.is_file():
        prior = json.loads(path.read_text())
        if prior.get("status") == "pass":
            return prior
    if torch.cuda.device_count() != GPU_COUNT:
        raise RuntimeError("Exp18 requires exactly eight visible GPUs")
    names = [torch.cuda.get_device_name(index) for index in range(GPU_COUNT)]
    memory = [
        torch.cuda.get_device_properties(index).total_memory / 2**30
        for index in range(GPU_COUNT)
    ]
    if any("H100" not in name for name in names) or any(value < 75 for value in memory):
        raise RuntimeError(f"Exp18 requires 8xH100-80GB; found {names}/{memory}")
    inventories = {
        model: exp17.model_inventory(exp17.build_model(model))
        for model in MODEL_NAMES
    }
    mismatches = {
        model: abs(int(row["total_parameters"]) / TARGET_PARAMETERS - 1.0)
        for model, row in inventories.items()
    }
    if any(value > MAXIMUM_PARAMETER_MISMATCH for value in mismatches.values()):
        raise RuntimeError(f"parameter matching gate failed: {mismatches}")
    progress: dict[str, Any] = {
        "schema": "exp18-paid-preflight-v1",
        "status": "running",
        "gpu_names": names,
        "gpu_memory_gib": memory,
        "inventories": inventories,
        "parameter_mismatch_fractions": mismatches,
        "execution_mode": EXECUTION_MODE,
        "gradient_accumulation": 1,
    }
    write_json(path, progress)
    agreements = run_tasks(
        [
            {
                "kind": "loss-agreement",
                "model": model,
                "data_root": str(data),
                "execution_mode": EXECUTION_MODE,
            }
            for model in MODEL_NAMES
        ]
    )
    all_complete(agreements, "exact fused-loss agreement")
    progress["loss_agreement"] = dict(zip(MODEL_NAMES, agreements, strict=True))
    write_json(path, progress)
    sweeps = run_tasks(
        [
            {"kind": "batch-sweep", "model": model, "data_root": str(data)}
            for model in MODEL_NAMES
        ]
    )
    all_complete(sweeps, "ambitious physical batch sweep")
    selected = {str(row["model"]): row["selected"] for row in sweeps}
    for model, row in selected.items():
        if int(row["global_tokens_per_step"]) < MINIMUM_GLOBAL_TOKENS:
            raise RuntimeError(f"underfilled Exp18 batch for {model}: {row}")
        if float(row["median_gpu_utilization_percent"]) < MINIMUM_UTILIZATION:
            raise RuntimeError(f"low-utilization Exp18 batch for {model}: {row}")
    scaling = run_tasks(
        [
            {
                "kind": "benchmark",
                "model": model,
                "recipe": asdict(memorization_recipes()[0]),
                "batch": int(selected[model]["batch"]),
                "measured_steps": 10,
                "data_root": str(data),
                "execution_mode": EXECUTION_MODE,
            }
            for model in MODEL_NAMES
        ]
    )
    all_complete(scaling, "full-node heterogeneous utilization")
    progress.update(
        {
            "status": "pass",
            "batch_sweeps": {str(row["model"]): row for row in sweeps},
            "selected": selected,
            "full_node_workers": scaling,
            "full_node_aggregate_tokens_per_second": sum(
                float(row["tokens_per_second"]) for row in scaling
            ),
            "minimum_global_tokens_per_step": MINIMUM_GLOBAL_TOKENS,
            "minimum_utilization_percent": MINIMUM_UTILIZATION,
        }
    )
    write_json(path, progress)
    return progress


def memorization_task(
    model: str,
    recipe: Recipe,
    seed: int,
    sample_indices: tuple[int, ...],
    maximum_steps: int,
    batch: int,
    cells: Path,
    data: Path,
) -> dict[str, Any]:
    return {
        "kind": "memorize",
        "model": model,
        "recipe": asdict(recipe),
        "seed": seed,
        "sample_indices": list(sample_indices),
        "maximum_steps": maximum_steps,
        "batch": batch,
        "output_root": str(cells),
        "data_root": str(data),
    }


def row_score(row: dict[str, Any]) -> tuple[Any, ...]:
    hit = row.get("threshold_hits", {}).get(threshold_key(SUCCESS_NLL))
    return (
        not bool(row.get("success")),
        math.inf if hit is None else int(hit["step"]),
        float(row.get("final", {}).get("nll", math.inf)),
        float(row.get("performance", {}).get("elapsed_seconds", math.inf)),
    )


def promote_optimizer_winners(
    rows: list[dict[str, Any]],
) -> dict[str, dict[str, Recipe]]:
    promoted: dict[str, dict[str, Recipe]] = {}
    for model in MODEL_NAMES:
        promoted[model] = {}
        model_rows = [
            row
            for row in rows
            if row.get("model") == model and row.get("status") == "complete"
        ]
        for family in ("adamw", "muon"):
            family_rows = [
                row
                for row in model_rows
                if row.get("recipe", {}).get("family") == family
            ]
            if not family_rows:
                raise RuntimeError(f"no stable {family} memorization cell for {model}")
            promoted[model][family] = Recipe(**min(family_rows, key=row_score)["recipe"])
    return promoted


def summarize_confirmation(
    rows: list[dict[str, Any]],
) -> tuple[dict[str, Any], str]:
    summary: dict[str, Any] = {}
    for model in MODEL_NAMES:
        candidates: list[dict[str, Any]] = []
        for family in ("adamw", "muon"):
            values = [
                row
                for row in rows
                if row.get("model") == model
                and row.get("recipe", {}).get("family") == family
            ]
            if len(values) != len(CONFIRMATION_SEEDS):
                raise RuntimeError(f"incomplete Exp18 confirmation for {model}/{family}")
            successes = sum(bool(row.get("success")) for row in values)
            strict = sum(bool(row.get("strict_success")) for row in values)
            steps = []
            seconds = []
            for row in values:
                hit = row.get("threshold_hits", {}).get(threshold_key(SUCCESS_NLL))
                if hit is not None:
                    steps.append(int(hit["step"]))
                    seconds.append(float(hit["elapsed_seconds"]))
            candidate = {
                "family": family,
                "recipe": values[0]["recipe"],
                "successful_seeds": successes,
                "strict_successful_seeds": strict,
                "mean_final_nll": statistics.fmean(
                    float(row.get("final", {}).get("nll", math.inf))
                    for row in values
                ),
                "mean_steps_to_success_nll": (
                    statistics.fmean(steps) if len(steps) == len(values) else None
                ),
                "mean_seconds_to_success_nll": (
                    statistics.fmean(seconds) if len(seconds) == len(values) else None
                ),
            }
            candidates.append(candidate)
        winner = min(
            candidates,
            key=lambda row: (
                -int(row["successful_seeds"]),
                math.inf
                if row["mean_steps_to_success_nll"] is None
                else float(row["mean_steps_to_success_nll"]),
                float(row["mean_final_nll"]),
            ),
        )
        summary[model] = {"optimizer_results": candidates, "winner": winner}
    passing = [
        model
        for model, row in summary.items()
        if int(row["winner"]["successful_seeds"]) == len(CONFIRMATION_SEEDS)
    ]
    transformer_passed = TRANSFORMER in passing
    kron_passed = any(model.startswith("group-kron") for model in passing)
    if len(passing) == len(MODEL_NAMES):
        verdict = "all_matched_architectures_memorize_two_blocks"
    elif transformer_passed and not kron_passed:
        verdict = "debug_group_kronecker_before_generalization"
    elif kron_passed:
        verdict = "passing_kronecker_ready_for_small_data_ladder"
    else:
        verdict = "fix_shared_training_pipeline_before_generalization"
    return summary, verdict


def run_campaign(
    output: str | Path,
    *,
    data_root: str | Path,
    heartbeat: str | Path | None = None,
) -> dict[str, Any]:
    if not os.environ.get("WANDB_API_KEY"):
        raise RuntimeError("WANDB_API_KEY is required before paid training")
    import wandb

    exp17.activate_base()
    data = Path(data_root)
    output_path = Path(output)
    cells = output_path.parent / "memorization-cells"
    load_windows(data, "train")
    wandb.login(key=os.environ["WANDB_API_KEY"], verify=True)
    run = wandb.init(
        project="exp18-memorization",
        name="exp18-matched-5m-one-two-block-overfit",
        config={
            "schema": SCHEMA,
            "models": MODEL_NAMES,
            "sample_indices": SAMPLE_INDICES,
            "screen_steps": SCREEN_STEPS,
            "confirmation_steps": CONFIRMATION_STEPS,
            "success_nll": SUCCESS_NLL,
            "strict_nll": STRICT_NLL,
            "parameter_matching": "total-parameters",
            "cloud_only": True,
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

    def publish(stage: str, rows: list[dict[str, Any]]) -> None:
        nonlocal log_step
        for row in rows:
            for point in row.get("curve", []):
                log_step += 1
                run.log(
                    {
                        f"{stage}/model": row.get("model", ""),
                        f"{stage}/optimizer": row.get("recipe", {}).get("family", ""),
                        f"{stage}/body_lr": row.get("recipe", {}).get("body_lr", 0),
                        f"{stage}/auxiliary_lr": row.get("recipe", {}).get("auxiliary_lr", 0),
                        f"{stage}/seed": row.get("seed", 0),
                        f"{stage}/sample_count": row.get("sample_count", 0),
                        f"{stage}/optimizer_step": point.get("step", 0),
                        f"{stage}/nll": point.get("nll", float("nan")),
                        f"{stage}/token_accuracy": point.get("token_accuracy", 0),
                        f"{stage}/elapsed_seconds": point.get("elapsed_seconds", 0),
                        f"{stage}/global_tokens_per_step": row.get("global_tokens_per_step", 0),
                        f"{stage}/unique_tokens_per_step": row.get("unique_tokens_per_optimizer_step", 0),
                    },
                    step=log_step,
                )
        if heartbeat_path:
            heartbeat_path.touch()

    try:
        preflight = paid_preflight(data, cells)
        batches = {
            model: int(row["batch"]) for model, row in preflight["selected"].items()
        }
        recipes = memorization_recipes()
        # Model-major ordering gives each architecture eight simultaneous LR
        # cells, keeping the complete node occupied during the screen.
        screen = run_tasks(
            [
                memorization_task(
                    model,
                    recipe,
                    SCREEN_SEED,
                    (SAMPLE_INDICES[0],),
                    SCREEN_STEPS,
                    batches[model],
                    cells,
                    data,
                )
                for model in MODEL_NAMES
                for recipe in recipes
            ]
        )
        publish("one_sample", screen)
        promoted = promote_optimizer_winners(screen)
        confirmation = run_tasks(
            [
                memorization_task(
                    model,
                    promoted[model][family],
                    seed,
                    SAMPLE_INDICES,
                    CONFIRMATION_STEPS,
                    batches[model],
                    cells,
                    data,
                )
                for model in MODEL_NAMES
                for family in ("adamw", "muon")
                for seed in CONFIRMATION_SEEDS
            ]
        )
        publish("two_sample", confirmation)
        confirmation_summary, verdict = summarize_confirmation(confirmation)
        result = {
            "schema": SCHEMA,
            "status": "complete",
            "verdict": verdict,
            "wandb_url": run.url,
            "cloud_only_training": True,
            "gpu_count": GPU_COUNT,
            "preflight": preflight,
            "sample_indices": list(SAMPLE_INDICES),
            "screen": screen,
            "promoted_recipes": {
                model: {
                    family: asdict(recipe) for family, recipe in values.items()
                }
                for model, values in promoted.items()
            },
            "confirmation": confirmation,
            "confirmation_summary": confirmation_summary,
            "success_definition": {
                "maximum_nll": SUCCESS_NLL,
                "token_accuracy": 1.0,
                "required_confirmation_seeds": list(CONFIRMATION_SEEDS),
            },
            "next_step": (
                "advance passing recipes to 8, 32, 128, then full-corpus examples"
                if "ready" in verdict or verdict.startswith("all_")
                else "debug failed architecture before adding training examples"
            ),
        }
        write_json(output_path, result)
        passing = sum(
            int(row["winner"]["successful_seeds"]) == len(CONFIRMATION_SEEDS)
            for row in confirmation_summary.values()
        )
        run.summary.update(
            {
                "verdict": verdict,
                "passing_architectures": passing,
                "architecture_count": len(MODEL_NAMES),
            }
        )
        run.finish()
        stop.set()
        if heartbeat_thread:
            heartbeat_thread.join(timeout=2)
        return result
    except Exception:
        stop.set()
        if heartbeat_thread:
            heartbeat_thread.join(timeout=2)
        run.finish(exit_code=1)
        raise


def main() -> None:
    parser = argparse.ArgumentParser(description="Run Exp18 memorization ladder")
    parser.add_argument("--output", required=True)
    parser.add_argument("--data-root", required=True)
    parser.add_argument("--heartbeat")
    args = parser.parse_args()
    result = run_campaign(
        args.output, data_root=args.data_root, heartbeat=args.heartbeat
    )
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)


if __name__ == "__main__":
    main()
