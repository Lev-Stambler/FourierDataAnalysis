"""Evidence-gated, fully tuned eight-H100 Exp17 mechanism campaign."""

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
from dataclasses import asdict, dataclass, replace
from pathlib import Path
from collections.abc import Callable
from typing import Any, Iterable

import numpy as np
import torch
from fla.modules import FusedLinearCrossEntropyLoss

import exp14_block_kronecker.campaign as base
from exp11_kronecker_debug.muon_tuning import BatchedMuon, MuonWithAuxAdamW
from exp14_block_kronecker.data import CONTEXT_LENGTH, evaluate, load_windows
from exp14_block_kronecker.data import block_batch as data_block_batch
from exp14_block_kronecker.model import (
    build_model as build_exp14_model,
    model_inventory as exp14_inventory,
)
from exp15_birouted_kronecker.campaign import gpu_cached_block_batch
from exp15_birouted_kronecker.model import (
    CURRENT_R8,
    build_model as build_exp15_model,
    model_inventory as exp15_inventory,
)

from .model import (
    DENSE_GROUP,
    GROUP_DEEP,
    GROUP_HYBRID,
    GROUP_R1,
    GROUP_R2,
    GROUP_R4,
    MODEL_NAMES as GROUP_MODELS,
    NO_ROUTER_TOKEN,
    LanguageModel,
    build_model as build_group_model,
    model_inventory as group_inventory,
    rms_norm,
)


SCHEMA = "exp17-group-density-mechanism-v1"
CELL_SCHEMA = "exp17-group-density-cell-v1"
GPU_COUNT = 8
TRANSFORMER = "block-transformer-d3-w256"
MODEL_NAMES = (CURRENT_R8, *GROUP_MODELS, TRANSFORMER)
PROMOTABLE_GROUP_MODELS = (
    GROUP_R1,
    GROUP_R2,
    GROUP_R4,
    GROUP_HYBRID,
    GROUP_DEEP,
)
TARGET_PARAMETERS = 5_400_896
MAXIMUM_PARAMETER_MISMATCH = 0.001
COARSE_TOKENS = 2 * TARGET_PARAMETERS
ROBUST_TOKENS = 5 * TARGET_PARAMETERS
FINAL_TOKENS = 20 * TARGET_PARAMETERS
COARSE_SEED = 30
ROBUST_SEEDS = (35, 36)
FINAL_SEEDS = (31, 32, 33, 34)
MINIMUM_MECHANISM_WIN = 0.02
MINIMUM_GROUP_ABLATION = 0.01
MINIMUM_FINAL_WIN = 0.02
MINIMUM_UTILIZATION = 85.0
EXECUTION_MODE = "default"
BATCH_SEARCH = (4096, 3072, 2048, 1536, 1024, 768, 640, 512, 400)
UNDERFILLED_BATCH = 128
MAXIMUM_BOUNDARY_ROUNDS = 4
LR_LIMITS = {
    "adamw_body": (0.0001875, 0.192),
    "muon_body": (0.001875, 1.92),
    "muon_auxiliary": (0.0001875, 0.096),
}


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
    horizon_tokens: int = FINAL_TOKENS
    minimum_lr_ratio: float = 0.1

    def __post_init__(self) -> None:
        if self.family not in {"adamw", "muon"}:
            raise ValueError("Exp17 optimizer must be AdamW or Muon")
        if self.schedule not in {"constant", "warmup-cosine"}:
            raise ValueError("invalid schedule")
        if min(self.body_lr, self.auxiliary_lr, self.clip_norm) <= 0:
            raise ValueError("learning rates and clip norm must be positive")


def write_json(path: str | Path, value: Any) -> None:
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_text(json.dumps(value, indent=2, sort_keys=True))
    os.replace(temporary, path)


def recipe_slug(recipe: Recipe) -> str:
    payload = json.dumps(asdict(recipe), sort_keys=True, separators=(",", ":"))
    return hashlib.sha256(payload.encode()).hexdigest()[:16]


def schedule_multiplier(recipe: Recipe, tokens_seen: int) -> float:
    if recipe.schedule == "constant":
        return 1.0
    if tokens_seen <= recipe.warmup_tokens:
        return max(1, tokens_seen) / recipe.warmup_tokens
    progress = min(
        1.0,
        (tokens_seen - recipe.warmup_tokens)
        / max(1, recipe.horizon_tokens - recipe.warmup_tokens),
    )
    cosine = 0.5 * (1.0 + math.cos(math.pi * progress))
    return recipe.minimum_lr_ratio + (1.0 - recipe.minimum_lr_ratio) * cosine


def build_model(name: str) -> torch.nn.Module:
    if name == CURRENT_R8:
        return build_exp15_model(CURRENT_R8)
    if name == TRANSFORMER:
        return build_exp14_model(TRANSFORMER)
    return build_group_model(name)


def model_inventory(model: torch.nn.Module) -> dict[str, Any]:
    name = str(model.spec.name)
    if name == CURRENT_R8:
        return exp15_inventory(model)
    if name == TRANSFORMER:
        return exp14_inventory(model)
    return group_inventory(model)


def create_optimizer(
    model: torch.nn.Module, recipe: Recipe
) -> tuple[Any, dict[str, Any]]:
    if recipe.family == "adamw":
        return (
            torch.optim.AdamW(
                base.grouped_parameters(model.parameters(), recipe.weight_decay),
                lr=recipe.body_lr,
                betas=(0.9, recipe.beta2),
            ),
            {
                "family": "adamw",
                "parameters": sum(parameter.numel() for parameter in model.parameters()),
            },
        )
    muon, auxiliary, muon_names, auxiliary_names = [], [], [], []
    for name, parameter in model.named_parameters():
        use_muon = name.startswith("blocks.") and parameter.ndim >= 2
        target, names = (
            (muon, muon_names) if use_muon else (auxiliary, auxiliary_names)
        )
        target.append(parameter)
        names.append(name)
    combined = [*muon, *auxiliary]
    if not muon or not auxiliary:
        raise RuntimeError("Muon routing requires body matrices and auxiliaries")
    if len({id(parameter) for parameter in combined}) != len(combined):
        raise RuntimeError("optimizer parameter groups overlap")
    if {id(parameter) for parameter in combined} != {
        id(parameter) for parameter in model.parameters()
    }:
        raise RuntimeError("optimizer routing is incomplete")
    optimizer = MuonWithAuxAdamW(
        BatchedMuon(
            muon,
            lr=recipe.body_lr,
            weight_decay=recipe.weight_decay,
            momentum=recipe.momentum,
            nesterov=recipe.nesterov,
            ns_steps=recipe.ns_steps,
        ),
        torch.optim.AdamW(
            base.grouped_parameters(auxiliary, recipe.weight_decay),
            lr=recipe.auxiliary_lr,
            betas=(0.9, recipe.beta2),
        ),
    )
    return optimizer, {
        "family": "muon",
        "policy": "all-block-matrices-muon_embeddings-scalars-adamw",
        "muon_parameter_names": muon_names,
        "auxiliary_parameter_names": auxiliary_names,
        "muon_parameters": sum(parameter.numel() for parameter in muon),
        "auxiliary_parameters": sum(parameter.numel() for parameter in auxiliary),
    }


def set_learning_rates(
    optimizer: Any, recipe: Recipe, tokens_seen: int
) -> tuple[float, float]:
    multiplier = schedule_multiplier(recipe, tokens_seen)
    body = recipe.body_lr * multiplier
    auxiliary = recipe.auxiliary_lr * multiplier
    if recipe.family == "adamw":
        for group in optimizer.param_groups:
            group["lr"] = body
        return body, body
    for group in optimizer.muon.param_groups:
        group["lr"] = body
    for group in optimizer.auxiliary.param_groups:
        group["lr"] = auxiliary
    return body, auxiliary


def compile_hidden(
    model: torch.nn.Module, execution_mode: str
) -> Callable[[torch.Tensor], torch.Tensor]:
    """Compile once per model shape while keeping only batch symbolic.

    The physical-batch sweep intentionally exercises more than eight batch
    sizes. A static batch guard therefore exhausts TorchDynamo's per-code-object
    recompile cache and also wastes paid compile time. Context and model shapes
    remain static; only the leading batch dimension is dynamic.
    """

    if execution_mode == "eager":
        return model.hidden
    if execution_mode not in {
        "default",
        "reduce-overhead",
        "max-autotune-no-cudagraphs",
    }:
        raise ValueError(f"unknown execution mode: {execution_mode}")
    return torch.compile(
        model.hidden,
        backend="inductor",
        mode=execution_mode,
        fullgraph=True,
        dynamic=True,
    )


def activate_base() -> None:
    base.Recipe = Recipe
    base.recipe_slug = recipe_slug
    base.schedule_multiplier = schedule_multiplier
    base.create_optimizer = create_optimizer
    base.set_learning_rates = set_learning_rates
    base.compile_hidden = compile_hidden
    base.build_model = build_model
    base.model_inventory = model_inventory
    base.LanguageModel = LanguageModel
    if not hasattr(base, "data_block_batch"):
        base.data_block_batch = data_block_batch
    if not hasattr(base, "exp17_original_block_batch"):
        base.exp17_original_block_batch = base.block_batch
    base.block_batch = gpu_cached_block_batch


def cell_path(
    root: Path, model: str, recipe: Recipe, seed: int, tokens: int
) -> Path:
    return (
        root
        / "cells"
        / model
        / f"recipe-{recipe_slug(recipe)}"
        / f"seed-{seed}-tokens-{tokens}.json"
    )


@torch.inference_mode()
def group_telemetry(model: torch.nn.Module, inputs: torch.Tensor) -> dict[str, Any]:
    if str(model.spec.name) not in GROUP_MODELS:
        return {}
    value = torch.nn.functional.embedding(inputs, model.vocabulary)
    capture = {0, len(model.blocks) // 2, len(model.blocks) - 1}
    layers: list[dict[str, Any]] = []
    for index, block in enumerate(model.blocks):
        mixed = rms_norm(value + block.mixer_gain * block.mixer(value))
        if index in capture:
            branch = block.nonlinear_branch(mixed)
            row: dict[str, Any] = {
                "layer": index,
                "nonlinear_update_to_state_rms": float(
                    (block.ffn_gain * branch).float().square().mean().sqrt()
                    / mixed.float().square().mean().sqrt().clamp_min(1e-12)
                ),
            }
            if block.group_ffn is not None:
                groups = mixed.reshape(
                    mixed.shape[0],
                    model.spec.group_count,
                    model.spec.group_size,
                    model.spec.width,
                )
                hidden = block.group_ffn.hidden(groups).float()
                samples = hidden.flatten(0, 1).flatten(1)
                samples = samples[: min(64, samples.shape[0])]
                centered = samples - samples.mean(0, keepdim=True)
                variance = centered.square().mean(0)
                gram = centered @ centered.T / max(1, centered.shape[1])
                eigenvalues = torch.linalg.eigvalsh(gram).clamp_min(0)
                row.update(
                    {
                        "hidden_nonzero_variance_fraction": float(
                            (variance > 1e-7).float().mean()
                        ),
                        "hidden_variance_mean": float(variance.mean()),
                        "hidden_participation_ratio": float(
                            eigenvalues.sum().square()
                            / eigenvalues.square().sum().clamp_min(1e-12)
                        ),
                        "hidden_sites": int(hidden[0].numel()),
                    }
                )
            layers.append(row)
        value = block(value)
    return {"layers": layers}


def nonlinear_ablation(
    model: torch.nn.Module,
    validation_windows: np.ndarray,
    batch: int,
    device: torch.device,
) -> dict[str, Any]:
    if str(model.spec.name) not in GROUP_MODELS:
        return {}
    subset = validation_windows[: min(257, len(validation_windows))]
    normal = evaluate(model, subset, min(batch, 16), device)
    snapshots = [block.ffn_gain.detach().clone() for block in model.blocks]
    try:
        with torch.no_grad():
            for block in model.blocks:
                block.ffn_gain.zero_()
        ablated = evaluate(model, subset, min(batch, 16), device)
    finally:
        with torch.no_grad():
            for block, value in zip(model.blocks, snapshots, strict=True):
                block.ffn_gain.copy_(value)
    return {
        "normal": normal,
        "nonlinear_branch_off": ablated,
        "off_minus_normal_nll": float(ablated["nll"]) - float(normal["nll"]),
    }


def train_cell(task: dict[str, Any], device: torch.device) -> dict[str, Any]:
    name = str(task["model"])
    recipe = Recipe(**task["recipe"])
    seed = int(task["seed"])
    batch = int(task["batch"])
    target_tokens = int(task["target_tokens"])
    evaluation_windows = int(task["evaluation_windows"])
    output_root = Path(task["output_root"])
    result_path = cell_path(output_root, name, recipe, seed, target_tokens)
    if result_path.is_file():
        prior = json.loads(result_path.read_text())
        if prior.get("status") == "complete":
            return prior
    train_windows = load_windows(task["data_root"], "train")
    validation_windows = load_windows(task["data_root"], "validation")
    torch.manual_seed(seed)
    torch.cuda.empty_cache()
    torch.cuda.reset_peak_memory_stats(device)
    model = build_model(name).to(device)
    hidden_function = base.compile_hidden(model, EXECUTION_MODE)
    optimizer, routing = create_optimizer(model, recipe)
    loss_function = FusedLinearCrossEntropyLoss()
    steps = math.ceil(target_tokens / (batch * CONTEXT_LENGTH))
    timed_seconds, timed_tokens, clipped = 0.0, 0, 0
    last: dict[str, Any] = {}
    with base.GpuSampler(device) as sampler:
        for step in range(steps):
            indices = base._logical_indices(len(train_windows), step, batch, seed)
            inputs, targets = gpu_cached_block_batch(train_windows, indices, device)
            optimizer.zero_grad(set_to_none=True)
            tokens_seen = (step + 1) * batch * CONTEXT_LENGTH
            body_lr, auxiliary_lr = set_learning_rates(optimizer, recipe, tokens_seen)
            torch.cuda.synchronize(device)
            started = time.perf_counter()
            with torch.autocast("cuda", dtype=torch.bfloat16):
                loss = base.fused_loss(
                    model, inputs, targets, loss_function, hidden_function
                )
            loss.backward()
            norm = torch.nn.utils.clip_grad_norm_(model.parameters(), recipe.clip_norm)
            if not torch.isfinite(loss) or not torch.isfinite(norm):
                raise RuntimeError("non-finite Exp17 forward/backward")
            clipped += int(float(norm.detach()) > recipe.clip_norm)
            optimizer.step()
            if not base.optimizer_is_finite(optimizer):
                raise RuntimeError("non-finite Exp17 optimizer state")
            torch.cuda.synchronize(device)
            duration = time.perf_counter() - started
            if step:
                timed_seconds += duration
                timed_tokens += batch * CONTEXT_LENGTH
            last = {
                "step": step + 1,
                "tokens_seen": tokens_seen,
                "train_block_nll": float(loss.detach()),
                "grad_norm": float(norm.detach()),
                "body_lr": body_lr,
                "auxiliary_lr": auxiliary_lr,
            }
    validation_limit = min(evaluation_windows + 1, len(validation_windows))
    validation = evaluate(
        model,
        validation_windows[:validation_limit],
        min(16, batch),
        device,
    )
    telemetry_inputs, _ = gpu_cached_block_batch(
        validation_windows,
        np.arange(min(8, len(validation_windows) - 1)),
        device,
    )
    telemetry = group_telemetry(model, telemetry_inputs)
    ablation = nonlinear_ablation(model, validation_windows, batch, device)
    actual_tokens = steps * batch * CONTEXT_LENGTH
    result = {
        "schema": CELL_SCHEMA,
        "status": "complete",
        "model": name,
        "recipe": asdict(recipe),
        "recipe_slug": recipe_slug(recipe),
        "seed": seed,
        "batch": batch,
        "global_examples_per_step": batch,
        "global_tokens_per_step": batch * CONTEXT_LENGTH,
        "gradient_accumulation": 1,
        "target_tokens": target_tokens,
        "actual_tokens": actual_tokens,
        "steps": steps,
        "evaluation_windows": validation_limit - 1,
        "validation": validation,
        "inventory": model_inventory(model),
        "optimizer_routing": routing,
        "group_telemetry": telemetry,
        "nonlinear_ablation": ablation,
        "performance": {
            **last,
            "tokens_per_second": timed_tokens / max(timed_seconds, 1e-12),
            "clip_fraction": clipped / steps,
            "peak_allocated_gib": torch.cuda.max_memory_allocated(device) / 2**30,
            "peak_reserved_gib": torch.cuda.max_memory_reserved(device) / 2**30,
            **sampler.summary(),
        },
        "loss_implementation": "fla-fused-linear-cross-entropy-exact",
        "execution_mode": EXECUTION_MODE,
    }
    write_json(result_path, result)
    del optimizer, model, loss_function
    gc.collect()
    torch.cuda.empty_cache()
    return result


def batch_sweep(task: dict[str, Any], device: torch.device) -> dict[str, Any]:
    name = str(task["model"])
    recipe = Recipe("adamw", 0.006, 0.006, "warmup-cosine")
    curve: list[dict[str, Any]] = []
    stable_seen = 0
    for batch in BATCH_SEARCH:
        benchmark_task = {
            "kind": "benchmark",
            "model": name,
            "recipe": asdict(recipe),
            "batch": batch,
            "measured_steps": 10,
            "data_root": task["data_root"],
            "execution_mode": EXECUTION_MODE,
        }
        try:
            row = base.benchmark(benchmark_task, device)
        except Exception as error:
            row = {
                "status": "failed",
                "model": name,
                "batch": batch,
                "failure": (
                    "out_of_memory"
                    if isinstance(error, torch.OutOfMemoryError)
                    or "out of memory" in str(error).lower()
                    else f"{type(error).__name__}: {str(error)[:1200]}"
                ),
            }
            gc.collect()
            torch.cuda.empty_cache()
        curve.append(row)
        if row.get("status") == "complete":
            stable_seen += 1
            if stable_seen >= 3:
                break
    baseline = base.benchmark(
        {
            "kind": "benchmark",
            "model": name,
            "recipe": asdict(recipe),
            "batch": UNDERFILLED_BATCH,
            "measured_steps": 10,
            "data_root": task["data_root"],
            "execution_mode": EXECUTION_MODE,
        },
        device,
    )
    eligible = [
        row
        for row in curve
        if row.get("status") == "complete"
        and row.get("finite_forward_backward_optimizer") is True
        and int(row.get("global_tokens_per_step", 0)) >= 100_000
        and float(row.get("median_gpu_utilization_percent", 0))
        >= MINIMUM_UTILIZATION
    ]
    if not eligible:
        raise RuntimeError(f"no accepted physical batch for {name}: {curve}")
    selected = max(eligible, key=lambda row: float(row["tokens_per_second"]))
    return {
        "status": "complete",
        "kind": "batch-sweep",
        "model": name,
        "curve": curve,
        "underfilled_batch128": baseline,
        "selected": selected,
        "selected_over_underfilled_throughput": float(selected["tokens_per_second"])
        / float(baseline["tokens_per_second"]),
    }


def execute_task(task: dict[str, Any], device: torch.device) -> dict[str, Any]:
    if task["kind"] == "train":
        return train_cell(task, device)
    if task["kind"] == "batch-sweep":
        return batch_sweep(task, device)
    return base.execute_task(task, device)


def worker_loop(gpu_id: int, tasks: mp.Queue, results: mp.Queue) -> None:
    activate_base()
    torch.cuda.set_device(gpu_id)
    device = torch.device(f"cuda:{gpu_id}")
    while True:
        item = tasks.get()
        if item is None:
            return
        index, task = item
        try:
            result = execute_task(task, device)
        except Exception as error:
            result = {
                "status": "failed",
                "kind": task.get("kind"),
                "model": task.get("model"),
                "seed": task.get("seed"),
                "batch": task.get("batch"),
                "failure": (
                    "out_of_memory"
                    if isinstance(error, torch.OutOfMemoryError)
                    or "out of memory" in str(error).lower()
                    else f"{type(error).__name__}: {str(error)[:2000]}"
                ),
            }
            gc.collect()
            torch.cuda.empty_cache()
        results.put((index, result))


def run_tasks(tasks: list[dict[str, Any]], timeout: int = 14_400) -> list[dict[str, Any]]:
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
        raise TimeoutError("Exp17 GPU worker timed out") from error
    finally:
        for process in processes:
            process.join(timeout=10)
            if process.is_alive():
                process.terminate()
    if any(row is None for row in rows):
        raise RuntimeError("missing Exp17 task result")
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
        raise RuntimeError("Exp17 requires exactly eight visible GPUs")
    names = [torch.cuda.get_device_name(index) for index in range(GPU_COUNT)]
    memory = [
        torch.cuda.get_device_properties(index).total_memory / 2**30
        for index in range(GPU_COUNT)
    ]
    if any("H100" not in name for name in names) or any(size < 75 for size in memory):
        raise RuntimeError(f"Exp17 requires 8xH100-80GB; found {names}/{memory}")
    inventories = {name: model_inventory(build_model(name)) for name in MODEL_NAMES}
    mismatches = {
        name: abs(int(row["total_parameters"]) / TARGET_PARAMETERS - 1.0)
        for name, row in inventories.items()
    }
    if any(value > MAXIMUM_PARAMETER_MISMATCH for value in mismatches.values()):
        raise RuntimeError(f"parameter matching gate failed: {mismatches}")
    progress: dict[str, Any] = {
        "schema": "exp17-paid-preflight-v1",
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
                "model": name,
                "data_root": str(data),
                "execution_mode": EXECUTION_MODE,
            }
            for name in MODEL_NAMES
        ]
    )
    all_complete(agreements, "exact fused-loss agreement")
    progress["loss_agreement"] = dict(zip(MODEL_NAMES, agreements, strict=True))
    write_json(path, progress)
    sweeps = run_tasks(
        [
            {"kind": "batch-sweep", "model": name, "data_root": str(data)}
            for name in MODEL_NAMES
        ]
    )
    all_complete(sweeps, "ambitious physical batch sweep")
    selected = {
        row["model"]: row["selected"] for row in sweeps
    }
    anchor = selected[GROUP_R1]
    scaling_task = {
        "kind": "benchmark",
        "model": GROUP_R1,
        "recipe": asdict(Recipe("adamw", 0.006, 0.006, "warmup-cosine")),
        "batch": int(anchor["batch"]),
        "measured_steps": 10,
        "data_root": str(data),
        "execution_mode": EXECUTION_MODE,
    }
    scaling = run_tasks([dict(scaling_task) for _ in range(GPU_COUNT)])
    all_complete(scaling, "eight-worker scaling")
    efficiency = sum(float(row["tokens_per_second"]) for row in scaling) / (
        GPU_COUNT * float(anchor["tokens_per_second"])
    )
    if efficiency < 0.80:
        raise RuntimeError(f"eight-worker scaling efficiency too low: {efficiency}")
    progress.update(
        {
            "status": "pass",
            "batch_sweeps": {row["model"]: row for row in sweeps},
            "selected": selected,
            "full_node_workers": scaling,
            "full_node_aggregate_tokens_per_second": sum(
                float(row["tokens_per_second"]) for row in scaling
            ),
            "full_node_cell_scaling_efficiency": efficiency,
            "minimum_utilization_percent": MINIMUM_UTILIZATION,
            "minimum_global_tokens_per_step": 100_000,
        }
    )
    write_json(path, progress)
    return progress


def coarse_recipes() -> tuple[Recipe, ...]:
    adamw = tuple(Recipe("adamw", lr, lr) for lr in (0.0015, 0.003, 0.006, 0.012, 0.024))
    muon = tuple(
        Recipe("muon", body, auxiliary)
        for body in (0.015, 0.03, 0.06, 0.12)
        for auxiliary in (0.0015, 0.003, 0.006)
    )
    return (*adamw, *muon)


def boundary_extensions(rows: list[dict[str, Any]]) -> list[Recipe]:
    extensions: list[Recipe] = []
    by_family = {
        family: [row for row in rows if row["recipe"]["family"] == family]
        for family in ("adamw", "muon")
    }
    adamw_best = min(by_family["adamw"], key=lambda row: float(row["validation"]["nll"]))
    adamw_rates = sorted(
        {float(row["recipe"]["body_lr"]) for row in by_family["adamw"]}
    )
    adamw_lr = float(adamw_best["recipe"]["body_lr"])
    if adamw_lr in {adamw_rates[0], adamw_rates[-1]}:
        extension = adamw_lr / 2 if adamw_lr == adamw_rates[0] else adamw_lr * 2
        lower, upper = LR_LIMITS["adamw_body"]
        extension = min(upper, max(lower, extension))
        extensions.append(Recipe("adamw", extension, extension))
    muon_best = min(by_family["muon"], key=lambda row: float(row["validation"]["nll"]))
    body = float(muon_best["recipe"]["body_lr"])
    auxiliary = float(muon_best["recipe"]["auxiliary_lr"])
    body_rates = sorted(
        {float(row["recipe"]["body_lr"]) for row in by_family["muon"]}
    )
    auxiliary_rates = sorted(
        {float(row["recipe"]["auxiliary_lr"]) for row in by_family["muon"]}
    )
    if body in {body_rates[0], body_rates[-1]}:
        extension = body / 2 if body == body_rates[0] else body * 2
        lower, upper = LR_LIMITS["muon_body"]
        extensions.append(
            Recipe(
                "muon",
                min(upper, max(lower, extension)),
                auxiliary,
            )
        )
    if auxiliary in {auxiliary_rates[0], auxiliary_rates[-1]}:
        extension = (
            auxiliary / 2 if auxiliary == auxiliary_rates[0] else auxiliary * 2
        )
        lower, upper = LR_LIMITS["muon_auxiliary"]
        extensions.append(
            Recipe(
                "muon",
                body,
                min(upper, max(lower, extension)),
            )
        )
    unique = {recipe_slug(recipe): recipe for recipe in extensions}
    return list(unique.values())


def training_task(
    model: str,
    recipe: Recipe,
    seed: int,
    tokens: int,
    batch: int,
    cells: Path,
    data: Path,
    evaluation_windows: int,
) -> dict[str, Any]:
    return {
        "kind": "train",
        "model": model,
        "recipe": asdict(recipe),
        "seed": seed,
        "target_tokens": tokens,
        "evaluation_windows": evaluation_windows,
        "batch": batch,
        "output_root": str(cells),
        "data_root": str(data),
    }


def paired_interval(values: list[float], seed: int = 1701) -> dict[str, float]:
    generator = np.random.default_rng(seed)
    array = np.asarray(values, dtype=np.float64)
    samples = generator.choice(array, size=(20_000, len(array)), replace=True).mean(1)
    return {
        "mean": float(array.mean()),
        "lower_95": float(np.quantile(samples, 0.025)),
        "upper_95": float(np.quantile(samples, 0.975)),
    }


def run_campaign(
    output: str | Path,
    *,
    data_root: str | Path,
    heartbeat: str | Path | None = None,
) -> dict[str, Any]:
    if not os.environ.get("WANDB_API_KEY"):
        raise RuntimeError("WANDB_API_KEY is required before paid training")
    import wandb

    activate_base()
    data = Path(data_root)
    output_path = Path(output)
    cells = output_path.parent / "mechanism-cells"
    load_windows(data, "train")
    load_windows(data, "validation")
    wandb.login(key=os.environ["WANDB_API_KEY"], verify=True)
    run = wandb.init(
        project="exp17-group-density",
        name="exp17-5m-wikitext-mechanism",
        config={
            "schema": SCHEMA,
            "models": MODEL_NAMES,
            "coarse_tokens": COARSE_TOKENS,
            "robust_tokens": ROBUST_TOKENS,
            "final_tokens": FINAL_TOKENS,
            "parameter_matching": "total-parameters",
            "flops_gate": False,
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
            log_step += 1
            run.log(
                {
                    f"{stage}/model": row.get("model", ""),
                    f"{stage}/optimizer": row.get("recipe", {}).get("family", ""),
                    f"{stage}/body_lr": row.get("recipe", {}).get("body_lr", 0),
                    f"{stage}/auxiliary_lr": row.get("recipe", {}).get("auxiliary_lr", 0),
                    f"{stage}/schedule": row.get("recipe", {}).get("schedule", ""),
                    f"{stage}/validation_block_nll": row.get("validation", {}).get("nll", float("nan")),
                    f"{stage}/nonlinear_ablation_nll": row.get("nonlinear_ablation", {}).get("off_minus_normal_nll", 0),
                    f"{stage}/tokens_per_second": row.get("performance", {}).get("tokens_per_second", 0),
                    f"{stage}/global_tokens_per_step": row.get("global_tokens_per_step", 0),
                },
                step=log_step,
            )
        if heartbeat_path:
            heartbeat_path.touch()

    try:
        preflight = paid_preflight(data, cells)
        batches = {name: int(row["batch"]) for name, row in preflight["selected"].items()}
        recipes = coarse_recipes()
        coarse = run_tasks(
            [
                training_task(
                    model,
                    replace(recipe, horizon_tokens=COARSE_TOKENS),
                    COARSE_SEED,
                    COARSE_TOKENS,
                    batches[model],
                    cells,
                    data,
                    1024,
                )
                for model in MODEL_NAMES
                for recipe in recipes
            ]
        )
        all_complete(coarse, "coarse independent optimizer tuning")
        publish("coarse", coarse)
        all_coarse = list(coarse)
        boundary_rounds: list[list[dict[str, Any]]] = []
        boundary_failures: list[dict[str, Any]] = []
        attempted = {str(row["recipe_slug"]) for row in all_coarse}
        for boundary_round in range(MAXIMUM_BOUNDARY_ROUNDS):
            extensions = {
                model: [
                    recipe
                    for recipe in boundary_extensions(
                        [row for row in all_coarse if row["model"] == model]
                    )
                    if recipe_slug(replace(recipe, horizon_tokens=COARSE_TOKENS))
                    not in attempted
                ]
                for model in MODEL_NAMES
            }
            extension_tasks = [
                training_task(
                    model,
                    replace(recipe, horizon_tokens=COARSE_TOKENS),
                    COARSE_SEED,
                    COARSE_TOKENS,
                    batches[model],
                    cells,
                    data,
                    1024,
                )
                for model, values in extensions.items()
                for recipe in values
            ]
            if not extension_tasks:
                break
            attempted.update(
                recipe_slug(Recipe(**task["recipe"])) for task in extension_tasks
            )
            extension_rows = run_tasks(extension_tasks)
            for task, row in zip(extension_tasks, extension_rows, strict=True):
                row.setdefault("recipe", task["recipe"])
                row["boundary_round"] = boundary_round + 1
            complete_extensions = [
                row for row in extension_rows if row.get("status") == "complete"
            ]
            failed_extensions = [
                row for row in extension_rows if row.get("status") != "complete"
            ]
            all_coarse.extend(complete_extensions)
            boundary_rounds.append(extension_rows)
            boundary_failures.extend(failed_extensions)
            publish("boundary", extension_rows)
        promoted: dict[str, list[Recipe]] = {}
        for model in MODEL_NAMES:
            rows = [row for row in all_coarse if row["model"] == model]
            values: list[Recipe] = []
            for family in ("adamw", "muon"):
                family_rows = sorted(
                    (row for row in rows if row["recipe"]["family"] == family),
                    key=lambda row: float(row["validation"]["nll"]),
                )[:2]
                values.extend(Recipe(**row["recipe"]) for row in family_rows)
            promoted[model] = values
        robust_tasks = []
        for model, values in promoted.items():
            for recipe in values:
                for schedule in ("constant", "warmup-cosine"):
                    robust_recipe = replace(
                        recipe,
                        schedule=schedule,
                        warmup_tokens=max(1, ROBUST_TOKENS // 20),
                        horizon_tokens=ROBUST_TOKENS,
                    )
                    for seed in ROBUST_SEEDS:
                        robust_tasks.append(
                            training_task(
                                model,
                                robust_recipe,
                                seed,
                                ROBUST_TOKENS,
                                batches[model],
                                cells,
                                data,
                                2048,
                            )
                        )
        robust = run_tasks(robust_tasks)
        all_complete(robust, "robust schedule/seed tuning")
        publish("robust", robust)
        selected_recipes: dict[str, Recipe] = {}
        robust_means: dict[str, float] = {}
        for model in MODEL_NAMES:
            rows = [row for row in robust if row["model"] == model]
            by_recipe: dict[str, list[dict[str, Any]]] = {}
            for row in rows:
                by_recipe.setdefault(str(row["recipe_slug"]), []).append(row)
            complete_pairs = [values for values in by_recipe.values() if len(values) == 2]
            winner = min(
                complete_pairs,
                key=lambda values: statistics.fmean(
                    float(row["validation"]["nll"]) for row in values
                ),
            )
            selected_recipes[model] = Recipe(**winner[0]["recipe"])
            robust_means[model] = statistics.fmean(
                float(row["validation"]["nll"]) for row in winner
            )
        candidate = min(PROMOTABLE_GROUP_MODELS, key=robust_means.__getitem__)
        kron_baseline = min((CURRENT_R8, NO_ROUTER_TOKEN), key=robust_means.__getitem__)
        selected_candidate_rows = [
            row
            for row in robust
            if row["model"] == candidate
            and row["recipe_slug"] == recipe_slug(selected_recipes[candidate])
        ]
        ablation_effect = statistics.fmean(
            float(row["nonlinear_ablation"]["off_minus_normal_nll"])
            for row in selected_candidate_rows
        )
        candidate_minus_kron = robust_means[candidate] - robust_means[kron_baseline]
        mechanism_advance = (
            candidate_minus_kron <= -MINIMUM_MECHANISM_WIN
            and ablation_effect >= MINIMUM_GROUP_ABLATION
        )
        common: dict[str, Any] = {
            "schema": SCHEMA,
            "status": "complete",
            "wandb_url": run.url,
            "preflight": preflight,
            "coarse": all_coarse,
            "boundary_rounds": boundary_rounds,
            "boundary_failures": boundary_failures,
            "robust": robust,
            "selected_recipes": {
                model: asdict(recipe) for model, recipe in selected_recipes.items()
            },
            "robust_validation_nll": robust_means,
            "best_group_candidate": candidate,
            "strongest_kron_baseline": kron_baseline,
            "candidate_minus_kron_nll": candidate_minus_kron,
            "candidate_nonlinear_ablation_effect": ablation_effect,
            "mechanism_gates": {
                "minimum_candidate_minus_kron_nll": -MINIMUM_MECHANISM_WIN,
                "minimum_nonlinear_ablation_effect": MINIMUM_GROUP_ABLATION,
            },
            "cloud_only_training": True,
            "gpu_count": GPU_COUNT,
        }
        if not mechanism_advance:
            result = {
                **common,
                "verdict": "stop_group_density_no_mechanistic_win",
                "final": [],
                "advance_to_length": False,
            }
        else:
            final_models = (candidate, kron_baseline, TRANSFORMER)
            final = run_tasks(
                [
                    training_task(
                        model,
                        replace(
                            selected_recipes[model],
                            warmup_tokens=FINAL_TOKENS // 20,
                            horizon_tokens=FINAL_TOKENS,
                        ),
                        seed,
                        FINAL_TOKENS,
                        batches[model],
                        cells,
                        data,
                        4096,
                    )
                    for model in final_models
                    for seed in FINAL_SEEDS
                ]
            )
            all_complete(final, "four-seed 20-token-per-parameter final")
            publish("final", final)
            nll = {
                model: {
                    int(row["seed"]): float(row["validation"]["nll"])
                    for row in final
                    if row["model"] == model
                }
                for model in final_models
            }
            vs_kron = [
                nll[candidate][seed] - nll[kron_baseline][seed]
                for seed in FINAL_SEEDS
            ]
            vs_transformer = [
                nll[candidate][seed] - nll[TRANSFORMER][seed]
                for seed in FINAL_SEEDS
            ]
            kron_interval = paired_interval(vs_kron, 1702)
            transformer_interval = paired_interval(vs_transformer, 1703)
            win = (
                all(value < 0 for value in vs_kron)
                and all(value < 0 for value in vs_transformer)
                and kron_interval["mean"] <= -MINIMUM_FINAL_WIN
                and transformer_interval["mean"] <= -MINIMUM_FINAL_WIN
                and kron_interval["upper_95"] < 0
                and transformer_interval["upper_95"] < 0
            )
            result = {
                **common,
                "verdict": (
                    "promote_group_density_to_length_ladder"
                    if win
                    else "stop_group_density_did_not_beat_transformer"
                ),
                "final": final,
                "paired_candidate_minus_kron_nll": vs_kron,
                "paired_candidate_minus_transformer_nll": vs_transformer,
                "candidate_minus_kron_interval": kron_interval,
                "candidate_minus_transformer_interval": transformer_interval,
                "all_four_candidate_wins": all(value < 0 for value in [*vs_kron, *vs_transformer]),
                "minimum_final_win": MINIMUM_FINAL_WIN,
                "advance_to_length": win,
            }
        write_json(output_path, result)
        run.summary.update(
            {
                "verdict": result["verdict"],
                "best_group_candidate": candidate,
                "strongest_kron_baseline": kron_baseline,
                "candidate_minus_kron_nll": candidate_minus_kron,
                "candidate_nonlinear_ablation_effect": ablation_effect,
                "final_candidate_minus_kron_nll": result.get("candidate_minus_kron_interval", {}).get("mean"),
                "final_candidate_minus_transformer_nll": result.get("candidate_minus_transformer_interval", {}).get("mean"),
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
    parser = argparse.ArgumentParser(description="Run Exp17 group-density campaign")
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
