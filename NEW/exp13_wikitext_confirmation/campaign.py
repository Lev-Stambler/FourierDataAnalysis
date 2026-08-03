"""Eight-GPU cell-parallel confirmation campaign for Experiment 13."""

from __future__ import annotations

import gc
import json
import math
import multiprocessing as mp
import os
import queue
import statistics
import subprocess
import threading
import time
from dataclasses import asdict
from pathlib import Path
from typing import Any, Iterable

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

from .holdout import load_holdout, read_holdout_manifest
from .model import (
    CANDIDATE,
    MODEL_NAMES,
    REPLAY_CONTROL,
    STANDARD_CONTROLS,
    LanguageModel,
    build_model,
    model_inventory,
)
from .study import (
    CONFIRMATION_SEEDS,
    FROZEN_CANDIDATE_RECIPE,
    FROZEN_REPLAY_RECIPE,
    ROBUST_TOKENS,
    TUNING_SEEDS,
    Recipe,
    boundary_extension,
    coarse_recipes,
    finalist_recipes,
    paired_decision,
    recipe_slug,
    robust_recipes,
    schedule_multiplier,
    summarize,
)


SCHEMA = "exp13-wikitext-confirmation-v1"
CELL_SCHEMA = "exp13-training-cell-v1"
PREFLIGHT_BATCHES = (
    4096,
    3584,
    3072,
    2560,
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
    128,
)
GPU_COUNT = 8
MINIMUM_MEAN_WIN = 0.10


class GpuSampler:
    """Sample utilization and power strictly inside a timed GPU region."""

    def __init__(self, device: torch.device, interval: float = 0.05) -> None:
        self.device = int(device.index or 0)
        self.interval = interval
        self.rows: list[tuple[float, float, float]] = []
        self.stop = threading.Event()
        self.thread: threading.Thread | None = None

    def _sample(self) -> None:
        while not self.stop.is_set():
            try:
                result = subprocess.run(
                    [
                        "nvidia-smi",
                        "--query-gpu=utilization.gpu,power.draw,memory.used",
                        "--format=csv,noheader,nounits",
                        f"--id={self.device}",
                    ],
                    check=False,
                    capture_output=True,
                    text=True,
                    timeout=3,
                )
                values = tuple(
                    float(item.strip()) for item in result.stdout.strip().split(",")
                )
                if result.returncode == 0 and len(values) == 3:
                    self.rows.append(values)  # type: ignore[arg-type]
            except (OSError, subprocess.SubprocessError, ValueError):
                pass
            self.stop.wait(self.interval)

    def __enter__(self) -> "GpuSampler":
        self.thread = threading.Thread(target=self._sample, daemon=True)
        self.thread.start()
        return self

    def __exit__(self, *_: Any) -> None:
        self.stop.set()
        if self.thread is not None:
            self.thread.join(timeout=3)

    def summary(self) -> dict[str, float | int]:
        return {
            "gpu_samples": len(self.rows),
            "median_gpu_utilization_percent": (
                statistics.median(row[0] for row in self.rows) if self.rows else 0.0
            ),
            "median_power_watts": (
                statistics.median(row[1] for row in self.rows) if self.rows else 0.0
            ),
            "peak_nvidia_memory_mib": max((row[2] for row in self.rows), default=0.0),
        }


def grouped_parameters(
    parameters: Iterable[torch.nn.Parameter], weight_decay: float
) -> list[dict[str, Any]]:
    decay, no_decay = [], []
    for parameter in parameters:
        (decay if parameter.ndim >= 2 else no_decay).append(parameter)
    groups = []
    if decay:
        groups.append({"params": decay, "weight_decay": weight_decay})
    if no_decay:
        groups.append({"params": no_decay, "weight_decay": 0.0})
    return groups


def split_muon_parameters(
    model: LanguageModel,
) -> tuple[list[torch.nn.Parameter], list[torch.nn.Parameter], dict[str, Any]]:
    muon, auxiliary, muon_names, auxiliary_names = [], [], [], []
    for name, parameter in model.named_parameters():
        use_muon = name.startswith("blocks.") and parameter.ndim >= 2
        if use_muon:
            muon.append(parameter)
            muon_names.append(name)
        else:
            auxiliary.append(parameter)
            auxiliary_names.append(name)
    combined = [*muon, *auxiliary]
    if not muon or not auxiliary:
        raise RuntimeError("Muon routing requires nonempty body and auxiliary groups")
    if len({id(parameter) for parameter in combined}) != len(combined):
        raise RuntimeError("optimizer groups overlap")
    if sum(parameter.numel() for parameter in combined) != sum(
        parameter.numel() for parameter in model.parameters()
    ):
        raise RuntimeError("optimizer routing is incomplete")
    return muon, auxiliary, {
        "muon_parameter_names": muon_names,
        "auxiliary_parameter_names": auxiliary_names,
        "muon_parameters": sum(parameter.numel() for parameter in muon),
        "auxiliary_parameters": sum(parameter.numel() for parameter in auxiliary),
    }


def create_optimizer(model: LanguageModel, recipe: Recipe) -> tuple[Any, dict[str, Any]]:
    if recipe.family == "adamw":
        return (
            torch.optim.AdamW(
                grouped_parameters(model.parameters(), recipe.weight_decay),
                lr=recipe.body_lr,
                betas=(0.9, recipe.beta2),
            ),
            {"family": "adamw", "parameters": sum(p.numel() for p in model.parameters())},
        )
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
    optimizer: Any, recipe: Recipe, tokens_seen: int
) -> tuple[float, float]:
    multiplier = schedule_multiplier(recipe, tokens_seen)
    body_lr, auxiliary_lr = recipe.body_lr * multiplier, recipe.auxiliary_lr * multiplier
    if recipe.family == "adamw":
        for group in optimizer.param_groups:
            group["lr"] = body_lr
        return body_lr, body_lr
    for group in optimizer.muon.param_groups:
        group["lr"] = body_lr
    for group in optimizer.auxiliary.param_groups:
        group["lr"] = auxiliary_lr
    return body_lr, auxiliary_lr


def _windows(data_root: str | Path, split: str) -> np.ndarray:
    return np.load(Path(data_root) / f"{split}.npy", mmap_mode="r")


def benchmark(
    model_name: str,
    recipe: Recipe,
    batch: int,
    data_root: str,
    device: torch.device,
    *,
    compiled: bool,
    measured_steps: int = 3,
) -> dict[str, Any]:
    torch.manual_seed(1313)
    torch.cuda.empty_cache()
    windows = _windows(data_root, "train")
    model = build_model(model_name).to(device)
    optimizer, routing = create_optimizer(model, recipe)
    wrapped = torch.compile(model) if compiled else model
    torch.cuda.reset_peak_memory_stats(device)

    def update(step: int) -> tuple[float, float]:
        inputs, targets = tensor_batch(
            windows, batch_indices(len(windows), step, batch, 1313), device
        )
        optimizer.zero_grad(set_to_none=True)
        torch.cuda.synchronize(device)
        started = time.perf_counter()
        with torch.autocast("cuda", dtype=torch.bfloat16):
            loss = cross_entropy(wrapped(inputs), targets)
        loss.backward()
        norm = torch.nn.utils.clip_grad_norm_(model.parameters(), recipe.clip_norm)
        if not torch.isfinite(loss) or not torch.isfinite(norm):
            raise RuntimeError("non-finite benchmark state")
        optimizer.step()
        if not optimizer_is_finite(optimizer):
            raise RuntimeError("non-finite benchmark optimizer state")
        torch.cuda.synchronize(device)
        return time.perf_counter() - started, float(loss.detach())

    warmup, _ = update(0)
    elapsed, last_loss = 0.0, 0.0
    with GpuSampler(device) as sampler:
        for step in range(1, measured_steps + 1):
            duration, last_loss = update(step)
            elapsed += duration
    tokens = measured_steps * batch * 256
    result = {
        "status": "complete",
        "kind": "benchmark",
        "model": model_name,
        "optimizer_family": recipe.family,
        "batch": batch,
        "global_tokens_per_step": batch * 256,
        "gradient_accumulation": 1,
        "execution_mode": "compiled" if compiled else "eager",
        "tokens_per_second": tokens / elapsed,
        "step_seconds": elapsed / measured_steps,
        "warmup_or_compile_seconds": warmup,
        "last_nll": last_loss,
        "peak_allocated_gib": torch.cuda.max_memory_allocated(device) / 2**30,
        "peak_reserved_gib": torch.cuda.max_memory_reserved(device) / 2**30,
        "finite_forward_backward_optimizer": True,
        "optimizer_routing": routing,
        **sampler.summary(),
        **gpu_snapshot(device),
    }
    del optimizer, wrapped, model
    gc.collect()
    torch.cuda.empty_cache()
    return result


@torch.inference_mode()
def loss_agreement(
    model_name: str,
    data_root: str,
    device: torch.device,
    *,
    batch: int = 16,
) -> dict[str, Any]:
    """Compare the BF16 training fast path with an identical FP32 model."""

    torch.manual_seed(1314)
    windows = _windows(data_root, "train")
    reference = build_model(model_name).to(device=device, dtype=torch.float32)
    candidate = build_model(model_name).to(device=device, dtype=torch.float32)
    candidate.load_state_dict(reference.state_dict())
    inputs, targets = tensor_batch(
        windows, batch_indices(len(windows), 0, batch, 1314), device
    )
    fp32 = float(cross_entropy(reference(inputs), targets))
    with torch.autocast("cuda", dtype=torch.bfloat16):
        bf16 = float(cross_entropy(candidate(inputs), targets))
    relative = abs(fp32 - bf16) / max(abs(fp32), 1e-12)
    del reference, candidate, inputs, targets
    gc.collect()
    torch.cuda.empty_cache()
    return {
        "status": "complete" if relative <= 0.02 else "failed",
        "kind": "loss-agreement",
        "model": model_name,
        "batch": batch,
        "fp32_nll": fp32,
        "bf16_nll": bf16,
        "relative_error": relative,
        "maximum_relative_error": 0.02,
    }


def cell_directory(root: Path, model: str, recipe: Recipe, seed: int) -> Path:
    return root / "cells" / model / f"recipe-{recipe_slug(recipe)}" / f"seed-{seed}"


def train_cell(task: dict[str, Any], device: torch.device) -> dict[str, Any]:
    model_name = str(task["model"])
    recipe = Recipe(**task["recipe"])
    seed, batch, target_steps = int(task["seed"]), int(task["batch"]), int(task["target_steps"])
    root, data_root = Path(task["output_root"]), Path(task["data_root"])
    directory = cell_directory(root, model_name, recipe, seed)
    directory.mkdir(parents=True, exist_ok=True)
    result_path = directory / f"result-step{target_steps}.json"
    if result_path.is_file():
        existing = json.loads(result_path.read_text())
        if existing.get("status") == "complete":
            return existing
    torch.manual_seed(seed)
    train_windows = _windows(data_root, "train")
    validation_windows = _windows(data_root, "validation")
    model = build_model(model_name).to(device)
    optimizer, routing = create_optimizer(model, recipe)
    identity = {
        "model": model_name,
        "recipe": asdict(recipe),
        "seed": seed,
        "batch": batch,
    }
    latest = directory / "checkpoint.pt"
    completed_steps = 0
    cumulative_optimization_seconds = 0.0
    if latest.is_file():
        saved = torch.load(latest, map_location="cpu", weights_only=True)
        if saved.get("identity") != identity:
            raise RuntimeError(f"checkpoint identity mismatch: {latest}")
        model.load_state_dict(saved["model"])
        optimizer.load_state_dict(saved["optimizer"])
        completed_steps = int(saved["steps"])
        cumulative_optimization_seconds = float(
            saved.get("cumulative_optimization_seconds", 0.0)
        )
        if completed_steps > target_steps:
            raise RuntimeError("cannot rewind a cell checkpoint")
    wrapped = torch.compile(model) if bool(task.get("compiled", True)) else model
    started = time.perf_counter()
    timed_seconds, timed_tokens, clipped, measured = 0.0, 0, 0, 0
    last: dict[str, Any] = {}
    for step in range(completed_steps, target_steps):
        inputs, targets = tensor_batch(
            train_windows, batch_indices(len(train_windows), step, batch, seed), device
        )
        optimizer.zero_grad(set_to_none=True)
        tokens_seen = (step + 1) * batch * 256
        body_lr, auxiliary_lr = set_learning_rates(optimizer, recipe, tokens_seen)
        torch.cuda.synchronize(device)
        step_started = time.perf_counter()
        with torch.autocast("cuda", dtype=torch.bfloat16):
            loss = cross_entropy(wrapped(inputs), targets)
        loss.backward()
        norm = torch.nn.utils.clip_grad_norm_(model.parameters(), recipe.clip_norm)
        if not torch.isfinite(loss) or not torch.isfinite(norm):
            raise RuntimeError("non-finite training state")
        clipped += int(float(norm.detach()) > recipe.clip_norm)
        measured += 1
        optimizer.step()
        if not optimizer_is_finite(optimizer):
            raise RuntimeError("non-finite optimizer state")
        torch.cuda.synchronize(device)
        duration = time.perf_counter() - step_started
        if step > completed_steps:
            timed_seconds += duration
            timed_tokens += batch * 256
        last = {
            "model": model_name,
            "seed": seed,
            "step": step + 1,
            "tokens_seen": tokens_seen,
            "batch": batch,
            "global_tokens_per_step": batch * 256,
            "gradient_accumulation": 1,
            "optimizer_family": recipe.family,
            "body_lr": body_lr,
            "auxiliary_lr": auxiliary_lr,
            "schedule": recipe.schedule,
            "train_nll": float(loss.detach()),
            "grad_norm": float(norm.detach()),
            "clip_fraction": clipped / measured,
            "tokens_per_second": (
                timed_tokens / timed_seconds if timed_seconds else batch * 256 / duration
            ),
            "peak_allocated_gib": torch.cuda.max_memory_allocated(device) / 2**30,
            "peak_reserved_gib": torch.cuda.max_memory_reserved(device) / 2**30,
        }
    validation = evaluate(
        model, validation_windows, min(256, batch), device
    )
    checkpoint = directory / f"checkpoint-step{target_steps}.pt"
    saved = {
        "schema": "exp13-checkpoint-v1",
        "identity": identity,
        "model": model.state_dict(),
        "optimizer": optimizer.state_dict(),
        "steps": target_steps,
        "tokens_seen": target_steps * batch * 256,
        "cumulative_optimization_seconds": cumulative_optimization_seconds
        + timed_seconds,
    }
    temporary = checkpoint.with_name(checkpoint.name + ".tmp")
    torch.save(saved, temporary)
    temporary.replace(checkpoint)
    latest_temporary = latest.with_name(latest.name + ".tmp")
    torch.save(saved, latest_temporary)
    latest_temporary.replace(latest)
    result = {
        "schema": CELL_SCHEMA,
        "status": "complete",
        "model": model_name,
        "recipe": asdict(recipe),
        "recipe_slug": recipe_slug(recipe),
        "seed": seed,
        "batch": batch,
        "target_steps": target_steps,
        "tokens_seen": target_steps * batch * 256,
        "contexts_seen": target_steps * batch,
        "inventory": model_inventory(model),
        "optimizer_routing": routing,
        "validation": validation,
        "performance": last,
        "elapsed_seconds_this_invocation": time.perf_counter() - started,
        "optimization_seconds_this_invocation": timed_seconds,
        "cumulative_optimization_seconds": cumulative_optimization_seconds
        + timed_seconds,
        "checkpoint": str(checkpoint),
    }
    write_json(result_path, result)
    del optimizer, wrapped, model, saved
    gc.collect()
    torch.cuda.empty_cache()
    return result


def evaluate_checkpoint(task: dict[str, Any], device: torch.device) -> dict[str, Any]:
    model = build_model(str(task["model"])).to(device)
    saved = torch.load(task["checkpoint"], map_location="cpu", weights_only=True)
    model.load_state_dict(saved["model"])
    windows = load_holdout(task["holdout_root"], task["split"])
    value = evaluate(model, windows, int(task.get("batch", 256)), device)
    del model, saved
    gc.collect()
    torch.cuda.empty_cache()
    return {
        "status": "complete",
        "kind": "evaluation",
        "model": task["model"],
        "seed": task["seed"],
        "checkpoint": task["checkpoint"],
        "split": task["split"],
        "metrics": value,
    }


def execute_task(task: dict[str, Any], device: torch.device) -> dict[str, Any]:
    kind = task["kind"]
    if kind == "benchmark":
        return benchmark(
            task["model"],
            Recipe(**task["recipe"]),
            int(task["batch"]),
            task["data_root"],
            device,
            compiled=bool(task.get("compiled", False)),
            measured_steps=int(task.get("measured_steps", 3)),
        )
    if kind == "train":
        return train_cell(task, device)
    if kind == "loss-agreement":
        return loss_agreement(
            str(task["model"]),
            str(task["data_root"]),
            device,
            batch=int(task.get("batch", 16)),
        )
    if kind == "evaluate":
        return evaluate_checkpoint(task, device)
    raise ValueError(f"unknown task kind: {kind}")


def worker_loop(
    gpu_id: int, tasks: mp.Queue, results: mp.Queue
) -> None:
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
            if isinstance(error, torch.OutOfMemoryError) or "out of memory" in str(error).lower():
                reason = "out_of_memory"
            else:
                reason = f"{type(error).__name__}: {str(error)[:1000]}"
            result = {
                "status": "failed",
                "kind": task.get("kind"),
                "model": task.get("model"),
                "seed": task.get("seed"),
                "failure": reason,
            }
            gc.collect()
            torch.cuda.empty_cache()
        results.put((index, result))


def run_tasks(
    tasks: list[dict[str, Any]], *, workers: int = GPU_COUNT
) -> list[dict[str, Any]]:
    if not tasks:
        return []
    context = mp.get_context("spawn")
    task_queue, result_queue = context.Queue(), context.Queue()
    count = min(workers, len(tasks))
    processes = [
        context.Process(target=worker_loop, args=(gpu, task_queue, result_queue))
        for gpu in range(count)
    ]
    for process in processes:
        process.start()
    for index, task in enumerate(tasks):
        task_queue.put((index, task))
    for _ in processes:
        task_queue.put(None)
    values: list[dict[str, Any] | None] = [None] * len(tasks)
    for _ in tasks:
        try:
            index, result = result_queue.get(timeout=1800)
        except queue.Empty as error:
            for process in processes:
                process.terminate()
            raise RuntimeError("GPU worker result timeout") from error
        values[index] = result
    for process in processes:
        process.join(timeout=30)
        if process.exitcode:
            raise RuntimeError(f"GPU worker exited {process.exitcode}")
    return [value for value in values if value is not None]


def _benchmark_task(
    model: str, recipe: Recipe, batch: int, data_root: Path, *, compiled: bool
) -> dict[str, Any]:
    return {
        "kind": "benchmark",
        "model": model,
        "recipe": asdict(recipe),
        "batch": batch,
        "data_root": str(data_root),
        "compiled": compiled,
    }


def _pad_to_full_node(tasks: list[dict[str, Any]]) -> list[dict[str, Any]]:
    """Repeat independent benchmarks so short sweeps still occupy all GPUs."""
    if not tasks:
        return []
    return [
        *tasks,
        *(tasks[index % len(tasks)] for index in range(GPU_COUNT - len(tasks))),
    ]


def paid_preflight(data_root: Path) -> dict[str, Any]:
    if torch.cuda.device_count() != GPU_COUNT:
        raise RuntimeError("Exp13 requires exactly eight visible GPUs")
    names = [torch.cuda.get_device_name(index) for index in range(GPU_COUNT)]
    if any("H100" not in name for name in names):
        raise RuntimeError(f"Exp13 requires 8xH100 80GB, found: {names}")
    memory_gib = [
        torch.cuda.get_device_properties(index).total_memory / 2**30
        for index in range(GPU_COUNT)
    ]
    if any(value < 75.0 for value in memory_gib):
        raise RuntimeError(f"Exp13 requires 8xH100 80GB-class memory, found: {memory_gib}")
    curves: dict[str, Any] = {}
    selected: dict[str, Any] = {}
    representatives = {
        "adamw": Recipe("adamw", 0.003, 0.003),
        "muon": Recipe("muon", 0.03, 0.001),
    }
    for family, recipe in representatives.items():
        active = list(MODEL_NAMES)
        stable: dict[str, list[dict[str, Any]]] = {name: [] for name in MODEL_NAMES}
        failures: dict[str, list[dict[str, Any]]] = {name: [] for name in MODEL_NAMES}
        for batch in PREFLIGHT_BATCHES:
            if not active:
                break
            tasks = [_benchmark_task(name, recipe, batch, data_root, compiled=False) for name in active]
            padded = _pad_to_full_node(tasks)
            rows = run_tasks(padded)[: len(tasks)]
            next_active = []
            for name, row in zip(active, rows, strict=True):
                if row["status"] == "complete":
                    stable[name].append(row)
                    if len(stable[name]) < 3:
                        next_active.append(name)
                else:
                    failures[name].append({"batch": batch, "reason": row["failure"]})
                    next_active.append(name)
            active = next_active
        eager_by_name: dict[str, dict[str, Any]] = {}
        compiled_tasks = []
        for name in MODEL_NAMES:
            if not stable[name]:
                raise RuntimeError(f"no stable {family} batch for {name}")
            eager = max(stable[name], key=lambda row: row["tokens_per_second"])
            eager_by_name[name] = eager
            compiled_tasks.append(
                _benchmark_task(
                    name, recipe, int(eager["batch"]), data_root, compiled=True
                )
            )
        compiled_rows = run_tasks(_pad_to_full_node(compiled_tasks))[
            : len(compiled_tasks)
        ]
        underfilled_tasks = [
            _benchmark_task(name, recipe, 128, data_root, compiled=True)
            for name in MODEL_NAMES
        ]
        underfilled_rows = run_tasks(_pad_to_full_node(underfilled_tasks))[
            : len(underfilled_tasks)
        ]
        for name, compiled, underfilled in zip(
            MODEL_NAMES, compiled_rows, underfilled_rows, strict=True
        ):
            eager = eager_by_name[name]
            choices = [*stable[name]]
            if compiled["status"] == "complete":
                choices.append(compiled)
            else:
                failures[name].append(
                    {"batch": eager["batch"], "reason": "compiled_" + compiled["failure"]}
                )
            key = f"{name}/{family}"
            selected[key] = max(choices, key=lambda row: row["tokens_per_second"])
            curves[key] = {
                "measurements": choices,
                "underfilled_baseline": underfilled,
                "selected_over_underfilled_throughput": (
                    float(selected[key]["tokens_per_second"])
                    / float(underfilled["tokens_per_second"])
                    if underfilled["status"] == "complete"
                    else None
                ),
                "failures": failures[name],
            }
    candidate_batches = [
        int(selected[f"{name}/muon"]["batch"]) for name in MODEL_NAMES
    ]
    common_batch = min(candidate_batches)
    common_tasks = [
        _benchmark_task(
            name, representatives["muon"], common_batch, data_root, compiled=True
        )
        for name in MODEL_NAMES
    ]
    common_rows = run_tasks(_pad_to_full_node(common_tasks))[: len(common_tasks)]
    if any(row["status"] != "complete" for row in common_rows):
        raise RuntimeError("selected common batch is not stable for every model")
    common = dict(zip(MODEL_NAMES, common_rows, strict=True))
    scaling = {}
    for count in (1, 2, 4, 8):
        rows = run_tasks(
            [
                _benchmark_task(
                    CANDIDATE,
                    FROZEN_CANDIDATE_RECIPE,
                    common_batch,
                    data_root,
                    compiled=True,
                )
                for _ in range(count)
            ],
            workers=count,
        )
        aggregate = sum(float(row["tokens_per_second"]) for row in rows)
        scaling[str(count)] = {
            "aggregate_tokens_per_second": aggregate,
            "per_gpu_tokens_per_second": aggregate / count,
            "workers": rows,
        }
    return {
        "status": "pass",
        "gpu_names": names,
        "gpu_memory_gib": memory_gib,
        "gpu_count": GPU_COUNT,
        "curves": curves,
        "selected": selected,
        "common_batch": common_batch,
        "common_batch_measurements": common,
        "parallel_scaling": scaling,
        "gradient_accumulation": 1,
    }


def _train_task(
    model: str,
    recipe: Recipe,
    seed: int,
    target_steps: int,
    *,
    batch: int,
    output_root: Path,
    data_root: Path,
    compiled: bool,
) -> dict[str, Any]:
    return {
        "kind": "train",
        "model": model,
        "recipe": asdict(recipe),
        "seed": seed,
        "target_steps": target_steps,
        "batch": batch,
        "output_root": str(output_root),
        "data_root": str(data_root),
        "compiled": compiled,
    }


def _steps(tokens: int, batch: int) -> int:
    return math.ceil(tokens / (batch * 256))


def _all_complete(rows: Iterable[dict[str, Any]], label: str) -> None:
    failed = [row for row in rows if row.get("status") != "complete"]
    if failed:
        raise RuntimeError(f"{label} has failed cells: {failed}")


def _selection(
    rows: Iterable[dict[str, Any]], model: str, recipe: Recipe
) -> dict[str, Any]:
    slug = recipe_slug(recipe)
    values = summarize(
        (
            row
            for row in rows
            if row["model"] == model and row["recipe_slug"] == slug
        ),
        CONFIRMATION_SEEDS,
    )
    if not values:
        raise RuntimeError(f"missing complete four-seed selection for {model}")
    return values[0]


def _eval_tasks(
    cells: Iterable[dict[str, Any]], *, holdout_root: Path, split: str
) -> list[dict[str, Any]]:
    return [
        {
            "kind": "evaluate",
            "model": cell["model"],
            "seed": cell["seed"],
            "checkpoint": cell["checkpoint"],
            "holdout_root": str(holdout_root),
            "split": split,
            "batch": min(256, int(cell["batch"])),
        }
        for cell in cells
    ]


def _nll_by_seed(rows: Iterable[dict[str, Any]]) -> dict[int, float]:
    return {int(row["seed"]): float(row["metrics"]["nll"]) for row in rows}


def run_campaign(
    output: str | Path,
    *,
    data_root: str | Path,
    holdout_root: str | Path,
    heartbeat: str | Path | None = None,
) -> dict[str, Any]:
    if not os.environ.get("WANDB_API_KEY"):
        raise RuntimeError("WANDB_API_KEY is required before paid training")
    import wandb

    data, holdout, output_path = Path(data_root), Path(holdout_root), Path(output)
    # Training may verify only train/validation. Test-derived arrays are opened
    # exclusively by the explicit confirmation/final evaluator tasks.
    for split in ("train", "validation"):
        load_windows(data, split)
    holdout_manifest = read_holdout_manifest(holdout)
    wandb.login(key=os.environ["WANDB_API_KEY"], verify=True)
    run = wandb.init(
        project="exp13-wikitext-confirmation",
        name="exp13-adversarial-confirmation",
        config={
            "schema": SCHEMA,
            "models": MODEL_NAMES,
            "confirmation_seeds": CONFIRMATION_SEEDS,
            "holdout_sha256": holdout_manifest["source"]["test_sha256"],
        },
    )
    if not run.url:
        raise RuntimeError("W&B did not provide a direct run URL")
    print(f"WANDB_URL={run.url}", flush=True)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    write_json(output_path, {"schema": SCHEMA, "status": "running", "wandb_url": run.url})
    cells_root = output_path.parent / "campaign-cells"
    heartbeat_path = Path(heartbeat) if heartbeat else None
    heartbeat_stop = threading.Event()
    heartbeat_thread: threading.Thread | None = None
    if heartbeat_path:
        heartbeat_path.parent.mkdir(parents=True, exist_ok=True)
        heartbeat_path.touch()

        def pulse_heartbeat() -> None:
            while not heartbeat_stop.wait(30.0):
                heartbeat_path.touch()

        heartbeat_thread = threading.Thread(target=pulse_heartbeat, daemon=True)
        heartbeat_thread.start()
    log_step = 0

    def stop_heartbeat() -> None:
        heartbeat_stop.set()
        if heartbeat_thread is not None:
            heartbeat_thread.join(timeout=2.0)

    def publish(label: str, rows: list[dict[str, Any]]) -> None:
        nonlocal log_step
        for row in rows:
            log_step += 1
            values = {
                f"{label}/status": int(row.get("status") == "complete"),
                f"{label}/model": row.get("model", ""),
            }
            if row.get("validation"):
                values[f"{label}/validation_nll"] = row["validation"]["nll"]
            if row.get("performance"):
                values[f"{label}/tokens_per_second"] = row["performance"].get(
                    "tokens_per_second", 0
                )
            run.log(values, step=log_step)
        if heartbeat_path:
            heartbeat_path.parent.mkdir(parents=True, exist_ok=True)
            heartbeat_path.touch()

    try:
        preflight_started = time.perf_counter()
        preflight = paid_preflight(data)
        preflight["elapsed_seconds"] = time.perf_counter() - preflight_started
        write_json(cells_root / "preflight.json", preflight)
        common_batch = int(preflight["common_batch"])
        compiled = True

        coarse_tasks = []
        coarse_base = coarse_recipes()
        for model in STANDARD_CONTROLS:
            model_batch = int(preflight["selected"][f"{model}/muon"]["batch"])
            for recipe in coarse_base:
                key = f"{model}/{recipe.family}"
                measured = preflight["selected"][key]
                coarse_tasks.append(
                    _train_task(
                        model,
                        recipe,
                        0,
                        _steps(1_000_000, int(measured["batch"])),
                        batch=int(measured["batch"]),
                        output_root=cells_root,
                        data_root=data,
                        compiled=measured["execution_mode"] == "compiled",
                    )
                )
        coarse_rows = run_tasks(coarse_tasks)
        _all_complete(coarse_rows, "coarse screen")
        publish("coarse", coarse_rows)
        extensions = []
        summaries = summarize(coarse_rows, (0,))
        for model in STANDARD_CONTROLS:
            for family in ("adamw", "muon"):
                family_rows = [
                    row
                    for row in summaries
                    if row["model"] == model and row["recipe"]["family"] == family
                ]
                winner = Recipe(**family_rows[0]["recipe"])
                extension = boundary_extension(
                    winner, [recipe for recipe in coarse_base if recipe.family == family]
                )
                if extension is not None:
                    measured = preflight["selected"][f"{model}/{family}"]
                    extensions.append(
                        _train_task(
                            model,
                            extension,
                            0,
                            _steps(1_000_000, int(measured["batch"])),
                            batch=int(measured["batch"]),
                            output_root=cells_root,
                            data_root=data,
                            compiled=measured["execution_mode"] == "compiled",
                        )
                    )
        extension_rows = run_tasks(extensions)
        _all_complete(extension_rows, "boundary extension")
        coarse_rows.extend(extension_rows)
        publish("boundary", extension_rows)

        robust = robust_recipes(coarse_rows)
        robust_tasks = []
        for model, recipe in robust:
            measured = preflight["selected"][f"{model}/{recipe.family}"]
            for seed in TUNING_SEEDS:
                robust_tasks.append(
                    _train_task(
                        model,
                        recipe,
                        seed,
                        _steps(ROBUST_TOKENS, int(measured["batch"])),
                        batch=int(measured["batch"]),
                        output_root=cells_root,
                        data_root=data,
                        compiled=measured["execution_mode"] == "compiled",
                    )
                )
        robust_rows = run_tasks(robust_tasks)
        _all_complete(robust_rows, "robust screen")
        publish("robust", robust_rows)
        finalists = finalist_recipes(robust_rows)

        confirmation_steps = _steps(10_000_000, common_batch)
        confirmation_configs = [
            (CANDIDATE, FROZEN_CANDIDATE_RECIPE),
            (REPLAY_CONTROL, FROZEN_REPLAY_RECIPE),
            *finalists,
        ]
        selection_tasks = [
            _train_task(
                model,
                recipe,
                seed,
                confirmation_steps,
                batch=common_batch,
                output_root=cells_root,
                data_root=data,
                compiled=compiled,
            )
            for model, recipe in confirmation_configs
            for seed in CONFIRMATION_SEEDS
        ]
        selection_rows = run_tasks(selection_tasks)
        _all_complete(selection_rows, "10M selection")
        publish("selection", selection_rows)
        standard_summaries = [
            _selection(selection_rows, model, recipe) for model, recipe in finalists
        ]
        strongest = min(standard_summaries, key=lambda row: row["mean_validation_nll"])
        selected_model = str(strongest["model"])
        selected_recipe = Recipe(**strongest["recipe"])
        selected_slug = recipe_slug(selected_recipe)
        candidate_cells = [row for row in selection_rows if row["model"] == CANDIDATE]
        control_cells = [
            row
            for row in selection_rows
            if row["model"] == selected_model
            and row["recipe_slug"] == selected_slug
        ]
        confirmation_evaluations = run_tasks(
            _eval_tasks([*candidate_cells, *control_cells], holdout_root=holdout, split="confirmation")
        )
        _all_complete(confirmation_evaluations, "confirmation evaluation")
        candidate_confirmation = [
            row for row in confirmation_evaluations if row["model"] == CANDIDATE
        ]
        control_confirmation = [
            row for row in confirmation_evaluations if row["model"] == selected_model
        ]
        confirmation_decision = paired_decision(
            _nll_by_seed(candidate_confirmation),
            _nll_by_seed(control_confirmation),
            minimum_mean_win=MINIMUM_MEAN_WIN,
        )
        publish("confirmation", confirmation_evaluations)
        base_result = {
            "schema": SCHEMA,
            "status": "complete",
            "wandb_url": run.url,
            "preflight": preflight,
            "holdout_manifest": holdout_manifest,
            "inventories": {name: model_inventory(build_model(name)) for name in MODEL_NAMES},
            "coarse": coarse_rows,
            "robust": robust_rows,
            "finalists": [
                {"model": model, "recipe": asdict(recipe)} for model, recipe in finalists
            ],
            "selection": selection_rows,
            "selected_transformer": strongest,
            "confirmation_evaluations": confirmation_evaluations,
            "confirmation_decision": confirmation_decision,
            "confirmation_tokens_per_seed": confirmation_steps * common_batch * 256,
            "final_holdout_opened": False,
        }
        if confirmation_decision["status"] != "pass":
            result = {**base_result, "verdict": "not_confirmed"}
            write_json(output_path, result)
            stop_heartbeat()
            run.finish()
            return result

        final_steps = _steps(40_000_000, common_batch)
        secondary_model, secondary_recipe = next(
            (model, recipe)
            for model, recipe in finalists
            if (model, recipe_slug(recipe)) != (selected_model, selected_slug)
        )
        secondary_slug = recipe_slug(secondary_recipe)
        final_tasks = [
            _train_task(
                model,
                recipe,
                seed,
                final_steps,
                batch=common_batch,
                output_root=cells_root,
                data_root=data,
                compiled=compiled,
            )
            for model, recipe in (
                (CANDIDATE, FROZEN_CANDIDATE_RECIPE),
                (selected_model, selected_recipe),
                (secondary_model, secondary_recipe),
            )
            for seed in CONFIRMATION_SEEDS
        ]
        final_cells = run_tasks(final_tasks)
        _all_complete(final_cells, "40M final")
        publish("final", final_cells)
        candidate_final_cells = [row for row in final_cells if row["model"] == CANDIDATE]
        transformer_final_cells = [
            row
            for row in final_cells
            if row["model"] == selected_model
            and row["recipe_slug"] == selected_slug
        ]
        candidate_by_seed = {
            int(row["seed"]): row for row in candidate_final_cells
        }
        transformer_by_seed = {
            int(row["seed"]): row for row in transformer_final_cells
        }
        maximum_steps = len(_windows(data, "train")) // common_batch
        tokens_per_step = common_batch * 256
        compute_steps_by_seed: dict[int, int] = {}
        for seed in CONFIRMATION_SEEDS:
            candidate_seconds = float(
                candidate_by_seed[seed]["cumulative_optimization_seconds"]
            )
            transformer_seconds = float(
                transformer_by_seed[seed]["cumulative_optimization_seconds"]
            )
            transformer_step_seconds = tokens_per_step / float(
                transformer_by_seed[seed]["performance"]["tokens_per_second"]
            )
            additional_steps = math.ceil(
                max(0.0, candidate_seconds - transformer_seconds)
                / transformer_step_seconds
            )
            compute_steps_by_seed[seed] = min(
                maximum_steps, final_steps + additional_steps
            )
        compute_tasks = [
            _train_task(
                model,
                recipe,
                seed,
                compute_steps_by_seed[seed],
                batch=common_batch,
                output_root=cells_root,
                data_root=data,
                compiled=compiled,
            )
            for model, recipe in (
                (selected_model, selected_recipe),
                (secondary_model, secondary_recipe),
            )
            for seed in CONFIRMATION_SEEDS
        ]
        compute_and_secondary_cells = run_tasks(compute_tasks)
        compute_cells = [
            row
            for row in compute_and_secondary_cells
            if row["model"] == selected_model
            and row["recipe_slug"] == selected_slug
        ]
        secondary_compute_cells = [
            row
            for row in compute_and_secondary_cells
            if row["model"] == secondary_model
            and row["recipe_slug"] == secondary_slug
        ]
        _all_complete(compute_cells, "compute-matched extension")
        _all_complete(secondary_compute_cells, "secondary finalist extension")
        publish("compute_matched", compute_and_secondary_cells)

        final_evaluations = run_tasks(
            _eval_tasks(
                [*candidate_final_cells, *transformer_final_cells, *compute_cells],
                holdout_root=holdout,
                split="final",
            )
        )
        _all_complete(final_evaluations, "final evaluation")
        candidate_final = final_evaluations[:4]
        transformer_token_final = final_evaluations[4:8]
        transformer_compute_final = final_evaluations[8:12]
        token_decision = paired_decision(
            _nll_by_seed(candidate_final),
            _nll_by_seed(transformer_token_final),
            minimum_mean_win=MINIMUM_MEAN_WIN,
        )
        compute_decision = paired_decision(
            _nll_by_seed(candidate_final),
            _nll_by_seed(transformer_compute_final),
            minimum_mean_win=MINIMUM_MEAN_WIN,
        )
        token_interval = bootstrap_interval(
            token_decision["paired_candidate_minus_transformer_nll"]
        )
        compute_interval = bootstrap_interval(
            compute_decision["paired_candidate_minus_transformer_nll"]
        )
        compute_time_relative_errors = {
            seed: (
                float(
                    next(
                        row
                        for row in compute_cells
                        if int(row["seed"]) == seed
                    )["cumulative_optimization_seconds"]
                )
                / float(candidate_by_seed[seed]["cumulative_optimization_seconds"])
                - 1.0
            )
            for seed in CONFIRMATION_SEEDS
        }
        compute_match_valid = all(
            abs(value) <= 0.05 for value in compute_time_relative_errors.values()
        ) and all(
            compute_steps_by_seed[seed] < maximum_steps
            for seed in CONFIRMATION_SEEDS
        )
        if token_decision["status"] != "pass" or token_interval[1] >= 0:
            verdict = "token_advantage_did_not_survive_40m"
        elif (
            compute_match_valid
            and compute_decision["status"] == "pass"
            and compute_interval[1] < 0
        ):
            verdict = "confirmed_token_and_compute_advantage"
        else:
            verdict = "confirmed_token_efficiency_only"
        result = {
            **base_result,
            "verdict": verdict,
            "final_cells": final_cells,
            "compute_matched_cells": compute_cells,
            "secondary_finalist_cells": [
                row
                for row in final_cells
                if row["model"] == secondary_model
                and row["recipe_slug"] == secondary_slug
            ],
            "secondary_compute_matched_cells": secondary_compute_cells,
            "final_evaluations": final_evaluations,
            "token_matched_decision": {**token_decision, "bootstrap_95_percent_interval": token_interval},
            "compute_matched_decision": {
                **compute_decision,
                "bootstrap_95_percent_interval": compute_interval,
            },
            "final_tokens_per_seed": final_steps * common_batch * 256,
            "compute_matched_transformer_tokens_per_seed": {
                seed: steps * common_batch * 256
                for seed, steps in compute_steps_by_seed.items()
            },
            "compute_match": {
                "target_method": "paired_measured_40m_optimization_time",
                "projected_transformer_steps_by_seed": compute_steps_by_seed,
                "maximum_one_pass_steps": maximum_steps,
                "within_five_percent_and_not_capped": compute_match_valid,
                "paired_transformer_minus_candidate_relative_time_error": compute_time_relative_errors,
                "candidate_mean_cumulative_optimization_seconds": sum(
                    float(row["cumulative_optimization_seconds"])
                    for row in candidate_final_cells
                )
                / len(candidate_final_cells),
                "transformer_mean_cumulative_optimization_seconds": sum(
                    float(row["cumulative_optimization_seconds"])
                    for row in compute_cells
                )
                / len(compute_cells),
            },
            "final_holdout_opened": True,
        }
        write_json(output_path, result)
        stop_heartbeat()
        run.finish()
        return result
    except Exception:
        stop_heartbeat()
        run.finish(exit_code=1)
        raise
