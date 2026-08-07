"""Eight-H100 tuning pilot for the fully factorized block-causal candidate."""

from __future__ import annotations

import argparse
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
from dataclasses import asdict, replace
from pathlib import Path
from typing import Any, Callable, Iterable

import numpy as np
import torch
import torch.nn.functional as F
from fla.modules import FusedLinearCrossEntropyLoss

from exp11_kronecker_debug.lm import batch_indices
from exp11_kronecker_debug.muon_tuning import BatchedMuon, MuonWithAuxAdamW
from exp13_wikitext_confirmation.study import Recipe, recipe_slug, schedule_multiplier

from .data import CONTEXT_LENGTH, block_batch, evaluate, load_windows
from .model import (
    CANDIDATE,
    MODEL_NAMES,
    NO_WORKSPACE_PERMUTATION,
    STANDARD_CONTROLS,
    LanguageModel,
    build_model,
    model_inventory,
)


SCHEMA = "exp14-block-kronecker-pilot-v1"
CELL_SCHEMA = "exp14-block-training-cell-v1"
GPU_COUNT = 8
TUNED_MODELS = (CANDIDATE, *STANDARD_CONTROLS)
COARSE_SEED = 0
ROBUST_SEEDS = (1, 2)
FINAL_SEEDS = (3, 4, 5, 6)
COARSE_TOKENS = 1_000_000
ROBUST_TOKENS = 3_000_000
FINAL_TOKENS = 10_000_000
MINIMUM_MEAN_WIN = 0.05
MINIMUM_UTILIZATION = 85.0
MINIMUM_CELL_SCALING = 0.80
PREFLIGHT_BATCHES = (2048, 1536, 1024, 768, 640, 512, 384, 320, 256, 192, 128)
ADAMW_LRS = (0.0003, 0.001, 0.003, 0.01)
MUON_LRS = (0.01, 0.03, 0.1, 0.3)


def write_json(path: str | Path, value: Any) -> None:
    destination = Path(path)
    destination.parent.mkdir(parents=True, exist_ok=True)
    temporary = destination.with_name(destination.name + ".tmp")
    temporary.write_text(json.dumps(value, indent=2, sort_keys=True))
    temporary.replace(destination)


class GpuSampler:
    def __init__(self, device: torch.device, interval: float = 0.05) -> None:
        self.device = int(device.index or 0)
        self.interval = float(interval)
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
        raise RuntimeError("Muon routing requires body and auxiliary parameters")
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


def optimizer_is_finite(optimizer: Any) -> bool:
    values = (
        (optimizer.muon, optimizer.auxiliary)
        if hasattr(optimizer, "muon") and hasattr(optimizer, "auxiliary")
        else (optimizer,)
    )
    return all(
        torch.isfinite(item).all()
        for current in values
        for state in current.state.values()
        for item in state.values()
        if isinstance(item, torch.Tensor)
    )


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


def fused_loss(
    model: LanguageModel,
    inputs: torch.Tensor,
    targets: torch.Tensor,
    loss_function: FusedLinearCrossEntropyLoss,
    hidden_function: Callable[[torch.Tensor], torch.Tensor] | None = None,
) -> torch.Tensor:
    # ``block_batch`` returns a shifted view into the reconstructed stream.
    # FLA 0.5.2 flattens both arguments with ``view`` rather than ``reshape``,
    # so make the layout contract explicit at our integration boundary.
    hidden = (
        model.hidden(inputs)
        if hidden_function is None
        else hidden_function(inputs)
    ).contiguous()
    return loss_function(hidden, targets.contiguous(), model.vocabulary)


def compile_hidden(
    model: LanguageModel, execution_mode: str
) -> Callable[[torch.Tensor], torch.Tensor]:
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
        dynamic=False,
    )


def _logical_indices(size: int, step: int, batch: int, seed: int) -> np.ndarray:
    return batch_indices(size - 1, step, batch, seed)


def loss_agreement(task: dict[str, Any], device: torch.device) -> dict[str, Any]:
    name = str(task["model"])
    execution_mode = str(task.get("execution_mode", "eager"))
    windows = load_windows(task["data_root"], "train")
    torch.manual_seed(1410)
    model = build_model(name).to(device)
    hidden_function = compile_hidden(model, execution_mode)
    inputs, targets = block_batch(
        windows, _logical_indices(len(windows), 0, 2, 1410), device
    )
    with torch.no_grad():
        reference = F.cross_entropy(
            model(inputs).float().flatten(0, 1), targets.flatten()
        )
        with torch.autocast("cuda", dtype=torch.bfloat16):
            fused = fused_loss(
                model,
                inputs,
                targets,
                FusedLinearCrossEntropyLoss(),
                hidden_function,
            )
    relative = float((reference - fused).abs() / reference.abs().clamp_min(1e-12))
    result = {
        "status": "complete" if relative <= 0.02 else "failed",
        "kind": "loss-agreement",
        "model": name,
        "fp32_materialized_nll": float(reference),
        "bf16_fused_linear_nll": float(fused),
        "relative_error": relative,
        "maximum_relative_error": 0.02,
        "execution_mode": execution_mode,
    }
    del model, inputs, targets, reference, fused
    gc.collect()
    torch.cuda.empty_cache()
    return result


def benchmark(task: dict[str, Any], device: torch.device) -> dict[str, Any]:
    name = str(task["model"])
    execution_mode = str(task.get("execution_mode", "eager"))
    recipe = Recipe(**task["recipe"])
    batch = int(task["batch"])
    measured_steps = int(task.get("measured_steps", 10))
    windows = load_windows(task["data_root"], "train")
    torch.manual_seed(1411)
    torch.cuda.empty_cache()
    model = build_model(name).to(device)
    hidden_function = compile_hidden(model, execution_mode)
    optimizer, routing = create_optimizer(model, recipe)
    loss_function = FusedLinearCrossEntropyLoss()
    torch.cuda.reset_peak_memory_stats(device)

    def update(step: int) -> tuple[float, float]:
        inputs, targets = block_batch(
            windows, _logical_indices(len(windows), step, batch, 1411), device
        )
        optimizer.zero_grad(set_to_none=True)
        torch.cuda.synchronize(device)
        started = time.perf_counter()
        with torch.autocast("cuda", dtype=torch.bfloat16):
            loss = fused_loss(model, inputs, targets, loss_function, hidden_function)
        loss.backward()
        norm = torch.nn.utils.clip_grad_norm_(model.parameters(), recipe.clip_norm)
        if not torch.isfinite(loss) or not torch.isfinite(norm):
            raise RuntimeError("non-finite benchmark forward/backward")
        optimizer.step()
        if not optimizer_is_finite(optimizer):
            raise RuntimeError("non-finite benchmark optimizer")
        torch.cuda.synchronize(device)
        return time.perf_counter() - started, float(loss.detach())

    warmup, _ = update(0)
    elapsed, last_nll = 0.0, 0.0
    with GpuSampler(device) as sampler:
        for step in range(1, measured_steps + 1):
            duration, last_nll = update(step)
            elapsed += duration
    tokens = measured_steps * batch * CONTEXT_LENGTH
    result = {
        "status": "complete",
        "kind": "benchmark",
        "model": name,
        "optimizer_family": recipe.family,
        "batch": batch,
        "global_examples_per_step": batch,
        "global_tokens_per_step": batch * CONTEXT_LENGTH,
        "gradient_accumulation": 1,
        "tokens_per_second": tokens / elapsed,
        "step_seconds": elapsed / measured_steps,
        "warmup_seconds": warmup,
        "last_nll": last_nll,
        "peak_allocated_gib": torch.cuda.max_memory_allocated(device) / 2**30,
        "peak_reserved_gib": torch.cuda.max_memory_reserved(device) / 2**30,
        "finite_forward_backward_optimizer": True,
        "optimizer_routing": routing,
        "loss_implementation": "fla-fused-linear-cross-entropy-exact",
        "execution_mode": execution_mode,
        **sampler.summary(),
    }
    del optimizer, model, loss_function
    gc.collect()
    torch.cuda.empty_cache()
    return result


def _cell_path(
    root: Path,
    model: str,
    recipe: Recipe,
    seed: int,
    tokens: int,
    execution_mode: str = "eager",
) -> Path:
    base = root / "cells" / model / f"recipe-{recipe_slug(recipe)}"
    if execution_mode != "eager":
        base = base / f"execution-{execution_mode}"
    return base / f"seed-{seed}-tokens-{tokens}.json"


def train_cell(task: dict[str, Any], device: torch.device) -> dict[str, Any]:
    name = str(task["model"])
    execution_mode = str(task.get("execution_mode", "eager"))
    recipe = Recipe(**task["recipe"])
    seed = int(task["seed"])
    batch = int(task["batch"])
    target_tokens = int(task["target_tokens"])
    evaluation_windows = int(task["evaluation_windows"])
    output_root = Path(task["output_root"])
    result_path = _cell_path(
        output_root, name, recipe, seed, target_tokens, execution_mode
    )
    if result_path.is_file():
        existing = json.loads(result_path.read_text())
        if existing.get("status") == "complete":
            return existing
    train_windows = load_windows(task["data_root"], "train")
    validation_windows = load_windows(task["data_root"], "validation")
    torch.manual_seed(seed)
    torch.cuda.empty_cache()
    torch.cuda.reset_peak_memory_stats(device)
    model = build_model(name).to(device)
    hidden_function = compile_hidden(model, execution_mode)
    optimizer, routing = create_optimizer(model, recipe)
    loss_function = FusedLinearCrossEntropyLoss()
    steps = math.ceil(target_tokens / (batch * CONTEXT_LENGTH))
    timed_seconds, timed_tokens = 0.0, 0
    clipped = 0
    last: dict[str, Any] = {}
    with GpuSampler(device) as sampler:
        for step in range(steps):
            inputs, targets = block_batch(
                train_windows,
                _logical_indices(len(train_windows), step, batch, seed),
                device,
            )
            optimizer.zero_grad(set_to_none=True)
            tokens_seen = (step + 1) * batch * CONTEXT_LENGTH
            body_lr, auxiliary_lr = set_learning_rates(optimizer, recipe, tokens_seen)
            torch.cuda.synchronize(device)
            started = time.perf_counter()
            with torch.autocast("cuda", dtype=torch.bfloat16):
                loss = fused_loss(
                    model, inputs, targets, loss_function, hidden_function
                )
            loss.backward()
            norm = torch.nn.utils.clip_grad_norm_(model.parameters(), recipe.clip_norm)
            if not torch.isfinite(loss) or not torch.isfinite(norm):
                raise RuntimeError("non-finite training forward/backward")
            clipped += int(float(norm.detach()) > recipe.clip_norm)
            optimizer.step()
            if not optimizer_is_finite(optimizer):
                raise RuntimeError("non-finite training optimizer")
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
        "performance": {
            **last,
            "tokens_per_second": (
                timed_tokens / timed_seconds
                if timed_seconds
                else actual_tokens / max(1e-12, timed_seconds)
            ),
            "clip_fraction": clipped / steps,
            "peak_allocated_gib": torch.cuda.max_memory_allocated(device) / 2**30,
            "peak_reserved_gib": torch.cuda.max_memory_reserved(device) / 2**30,
            **sampler.summary(),
        },
        "loss_implementation": "fla-fused-linear-cross-entropy-exact",
        "execution_mode": execution_mode,
    }
    write_json(result_path, result)
    del optimizer, model, loss_function
    gc.collect()
    torch.cuda.empty_cache()
    return result


def execute_task(task: dict[str, Any], device: torch.device) -> dict[str, Any]:
    if task["kind"] == "loss-agreement":
        return loss_agreement(task, device)
    if task["kind"] == "benchmark":
        return benchmark(task, device)
    if task["kind"] == "train":
        return train_cell(task, device)
    raise ValueError(f"unknown task kind: {task['kind']}")


def worker_loop(gpu_id: int, tasks: mp.Queue, results: mp.Queue) -> None:
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
            reason = (
                "out_of_memory"
                if isinstance(error, torch.OutOfMemoryError)
                or "out of memory" in str(error).lower()
                else f"{type(error).__name__}: {str(error)[:1500]}"
            )
            result = {
                "status": "failed",
                "kind": task.get("kind"),
                "model": task.get("model"),
                "seed": task.get("seed"),
                "batch": task.get("batch"),
                "failure": reason,
            }
            gc.collect()
            torch.cuda.empty_cache()
        results.put((index, result))


def run_tasks(tasks: list[dict[str, Any]], workers: int = GPU_COUNT) -> list[dict[str, Any]]:
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
    rows: list[dict[str, Any] | None] = [None] * len(tasks)
    try:
        for _ in tasks:
            index, row = result_queue.get(timeout=3600)
            rows[index] = row
    except queue.Empty as error:
        raise TimeoutError("GPU worker task timed out") from error
    finally:
        for process in processes:
            process.join(timeout=10)
            if process.is_alive():
                process.terminate()
    if any(row is None for row in rows):
        raise RuntimeError("missing GPU worker result")
    return [row for row in rows if row is not None]


def pad_to_full_node(tasks: list[dict[str, Any]]) -> list[dict[str, Any]]:
    if not tasks:
        return []
    result = list(tasks)
    while len(result) < GPU_COUNT:
        result.append(dict(tasks[(len(result) - len(tasks)) % len(tasks)]))
    return result


def coarse_recipes() -> list[Recipe]:
    return [
        *(Recipe("adamw", lr, lr) for lr in ADAMW_LRS),
        *(Recipe("muon", lr, 0.001) for lr in MUON_LRS),
    ]


def _train_task(
    model: str,
    recipe: Recipe,
    seed: int,
    target_tokens: int,
    evaluation_windows: int,
    batch: int,
    output_root: Path,
    data_root: Path,
    execution_mode: str = "eager",
) -> dict[str, Any]:
    return {
        "kind": "train",
        "model": model,
        "recipe": asdict(recipe),
        "seed": seed,
        "target_tokens": target_tokens,
        "evaluation_windows": evaluation_windows,
        "batch": batch,
        "output_root": str(output_root),
        "data_root": str(data_root),
        "execution_mode": execution_mode,
    }


def _all_complete(rows: Iterable[dict[str, Any]], label: str) -> None:
    failed = [row for row in rows if row.get("status") != "complete"]
    if failed:
        raise RuntimeError(f"{label} has failed cells: {failed}")


def _mean_nll(rows: Iterable[dict[str, Any]]) -> float:
    values = [float(row["validation"]["nll"]) for row in rows]
    if not values:
        raise RuntimeError("cannot summarize an empty cell set")
    return sum(values) / len(values)


def _recipe_rows(
    rows: Iterable[dict[str, Any]], model: str, recipe: Recipe
) -> list[dict[str, Any]]:
    slug = recipe_slug(recipe)
    return [
        row
        for row in rows
        if row["model"] == model and row["recipe_slug"] == slug
    ]


def paid_preflight(data_root: Path, output_root: Path) -> dict[str, Any]:
    progress_path = output_root / "preflight.json"
    if progress_path.is_file():
        existing = json.loads(progress_path.read_text())
        if existing.get("status") == "pass":
            return existing
    if torch.cuda.device_count() != GPU_COUNT:
        raise RuntimeError("Exp14 requires exactly eight visible GPUs")
    names = [torch.cuda.get_device_name(index) for index in range(GPU_COUNT)]
    memory = [
        torch.cuda.get_device_properties(index).total_memory / 2**30
        for index in range(GPU_COUNT)
    ]
    if any("H100" not in name for name in names) or any(value < 75 for value in memory):
        raise RuntimeError(f"Exp14 requires 8xH100 80GB; found {names} / {memory}")
    progress: dict[str, Any] = {
        "schema": "exp14-paid-preflight-v1",
        "status": "running",
        "gpu_names": names,
        "gpu_memory_gib": memory,
        "batch_curves": {},
        "selected": {},
        "gradient_accumulation": 1,
        "loss_implementation": "fla-fused-linear-cross-entropy-exact",
    }
    write_json(progress_path, progress)
    agreement_tasks = [
        {"kind": "loss-agreement", "model": name, "data_root": str(data_root)}
        for name in MODEL_NAMES
    ]
    agreements = run_tasks(pad_to_full_node(agreement_tasks))[: len(agreement_tasks)]
    _all_complete(agreements, "BF16 fused-loss agreement")
    progress["loss_agreement"] = dict(zip(MODEL_NAMES, agreements, strict=True))
    write_json(progress_path, progress)
    representatives = {
        "adamw": Recipe("adamw", 0.003, 0.003),
        "muon": Recipe("muon", 0.03, 0.001),
    }
    for family, recipe in representatives.items():
        active = list(MODEL_NAMES)
        stable = {name: [] for name in MODEL_NAMES}
        failures = {name: [] for name in MODEL_NAMES}
        for batch in PREFLIGHT_BATCHES:
            if not active:
                break
            tasks = [
                {
                    "kind": "benchmark",
                    "model": name,
                    "recipe": asdict(recipe),
                    "batch": batch,
                    "measured_steps": 10,
                    "data_root": str(data_root),
                }
                for name in active
            ]
            rows = run_tasks(pad_to_full_node(tasks))[: len(tasks)]
            next_active = []
            for name, row in zip(active, rows, strict=True):
                valid = (
                    row.get("status") == "complete"
                    and row.get("finite_forward_backward_optimizer") is True
                    and int(row.get("gpu_samples", 0)) > 0
                    and float(row.get("median_gpu_utilization_percent", 0.0))
                    >= MINIMUM_UTILIZATION
                )
                if valid:
                    stable[name].append(row)
                    if len(stable[name]) < 3:
                        next_active.append(name)
                else:
                    failures[name].append(row)
                    next_active.append(name)
            active = next_active
            progress["last_batch"] = {"family": family, "batch": batch}
            write_json(progress_path, progress)
        baseline_tasks = [
            {
                "kind": "benchmark",
                "model": name,
                "recipe": asdict(recipe),
                "batch": 128,
                "measured_steps": 10,
                "data_root": str(data_root),
            }
            for name in MODEL_NAMES
        ]
        baselines = run_tasks(pad_to_full_node(baseline_tasks))[: len(MODEL_NAMES)]
        for name, baseline in zip(MODEL_NAMES, baselines, strict=True):
            if not stable[name]:
                raise RuntimeError(f"no stable high-utilization batch for {name}/{family}")
            key = f"{name}/{family}"
            selected = max(stable[name], key=lambda row: float(row["tokens_per_second"]))
            progress["selected"][key] = selected
            progress["batch_curves"][key] = {
                "stable": stable[name],
                "failures": failures[name],
                "underfilled_baseline": baseline,
                "selected_over_batch128_throughput": (
                    float(selected["tokens_per_second"])
                    / float(baseline["tokens_per_second"])
                    if baseline.get("status") == "complete"
                    else None
                ),
            }
        write_json(progress_path, progress)
    candidate_selected = progress["selected"][f"{CANDIDATE}/muon"]
    scaling_task = {
        "kind": "benchmark",
        "model": CANDIDATE,
        "recipe": asdict(representatives["muon"]),
        "batch": int(candidate_selected["batch"]),
        "measured_steps": 10,
        "data_root": str(data_root),
    }
    full_node = run_tasks([dict(scaling_task) for _ in range(GPU_COUNT)])
    _all_complete(full_node, "full-node cell scaling")
    aggregate = sum(float(row["tokens_per_second"]) for row in full_node)
    expected = GPU_COUNT * float(candidate_selected["tokens_per_second"])
    efficiency = aggregate / expected
    if efficiency < MINIMUM_CELL_SCALING:
        raise RuntimeError(
            f"full-node cell scaling efficiency {efficiency:.3f} is below "
            f"{MINIMUM_CELL_SCALING:.3f}"
        )
    progress.update(
        {
            "status": "pass",
            "full_node_candidate_workers": full_node,
            "full_node_aggregate_tokens_per_second": aggregate,
            "full_node_cell_scaling_efficiency": efficiency,
            "minimum_cell_scaling_efficiency": MINIMUM_CELL_SCALING,
            "minimum_gpu_utilization_percent": MINIMUM_UTILIZATION,
            "inventories": {
                name: model_inventory(build_model(name)) for name in MODEL_NAMES
            },
        }
    )
    write_json(progress_path, progress)
    return progress


def run_campaign(
    output: str | Path,
    *,
    data_root: str | Path,
    heartbeat: str | Path | None = None,
) -> dict[str, Any]:
    if not os.environ.get("WANDB_API_KEY"):
        raise RuntimeError("WANDB_API_KEY is required before paid training")
    import wandb

    data = Path(data_root)
    output_path = Path(output)
    cells_root = output_path.parent / "campaign-cells"
    load_windows(data, "train")
    load_windows(data, "validation")
    wandb.login(key=os.environ["WANDB_API_KEY"], verify=True)
    run = wandb.init(
        project="exp14-block-kronecker",
        name="exp14-fully-factorized-block-causal-pilot",
        config={
            "schema": SCHEMA,
            "candidate": CANDIDATE,
            "models": MODEL_NAMES,
            "block_size": 16,
            "context_length": CONTEXT_LENGTH,
            "coarse_tokens": COARSE_TOKENS,
            "robust_tokens": ROBUST_TOKENS,
            "final_tokens": FINAL_TOKENS,
        },
    )
    if not run.url:
        raise RuntimeError("W&B did not provide a direct run URL")
    print(f"WANDB_URL={run.url}", flush=True)
    write_json(
        output_path,
        {"schema": SCHEMA, "status": "running", "wandb_url": run.url},
    )
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
                    f"{stage}/status": int(row.get("status") == "complete"),
                    f"{stage}/validation_block_nll": row.get("validation", {}).get(
                        "nll", float("nan")
                    ),
                    f"{stage}/tokens_per_second": row.get("performance", {}).get(
                        "tokens_per_second", 0.0
                    ),
                    f"{stage}/batch": row.get("batch", 0),
                    f"{stage}/global_tokens_per_step": row.get(
                        "global_tokens_per_step", 0
                    ),
                },
                step=log_step,
            )
        if heartbeat_path:
            heartbeat_path.touch()

    try:
        preflight = paid_preflight(data, cells_root)
        batches = {
            key: int(value["batch"]) for key, value in preflight["selected"].items()
        }
        coarse_grid = coarse_recipes()
        coarse_tasks = [
            _train_task(
                model,
                recipe,
                COARSE_SEED,
                COARSE_TOKENS,
                512,
                batches[f"{model}/{recipe.family}"],
                cells_root,
                data,
            )
            for model in TUNED_MODELS
            for recipe in coarse_grid
        ]
        coarse_rows = run_tasks(coarse_tasks)
        _all_complete(coarse_rows, "coarse tuning")
        publish("coarse", coarse_rows)

        robust_candidates: dict[str, list[Recipe]] = {}
        robust_tasks = []
        for model in TUNED_MODELS:
            ranked = sorted(
                (row for row in coarse_rows if row["model"] == model),
                key=lambda row: float(row["validation"]["nll"]),
            )
            top = [Recipe(**row["recipe"]) for row in ranked[:2]]
            variants = {
                recipe_slug(replace(recipe, schedule=schedule)): replace(
                    recipe, schedule=schedule
                )
                for recipe in top
                for schedule in ("constant", "warmup-cosine")
            }
            robust_candidates[model] = list(variants.values())
            for recipe in variants.values():
                for seed in ROBUST_SEEDS:
                    robust_tasks.append(
                        _train_task(
                            model,
                            recipe,
                            seed,
                            ROBUST_TOKENS,
                            1024,
                            batches[f"{model}/{recipe.family}"],
                            cells_root,
                            data,
                        )
                    )
        if len(robust_tasks) % GPU_COUNT:
            raise RuntimeError("robust tuning did not fill the eight-GPU node")
        robust_rows = run_tasks(robust_tasks)
        _all_complete(robust_rows, "robust tuning")
        publish("robust", robust_rows)

        selected_recipes: dict[str, Recipe] = {}
        robust_summary: dict[str, Any] = {}
        for model in TUNED_MODELS:
            values = []
            for recipe in robust_candidates[model]:
                rows = _recipe_rows(robust_rows, model, recipe)
                if {int(row["seed"]) for row in rows} != set(ROBUST_SEEDS):
                    continue
                values.append(
                    {
                        "model": model,
                        "recipe": asdict(recipe),
                        "mean_validation_block_nll": _mean_nll(rows),
                        "nll_by_seed": {
                            str(row["seed"]): row["validation"]["nll"] for row in rows
                        },
                    }
                )
            values.sort(key=lambda row: float(row["mean_validation_block_nll"]))
            if not values:
                raise RuntimeError(f"no robust recipe for {model}")
            robust_summary[model] = values
            selected_recipes[model] = Recipe(**values[0]["recipe"])
        strongest_transformer = min(
            STANDARD_CONTROLS,
            key=lambda name: robust_summary[name][0]["mean_validation_block_nll"],
        )

        final_tasks = []
        for model in TUNED_MODELS:
            recipe = selected_recipes[model]
            for seed in FINAL_SEEDS:
                final_tasks.append(
                    _train_task(
                        model,
                        recipe,
                        seed,
                        FINAL_TOKENS,
                        2048,
                        batches[f"{model}/{recipe.family}"],
                        cells_root,
                        data,
                    )
                )
        candidate_recipe = selected_recipes[CANDIDATE]
        for seed in FINAL_SEEDS:
            final_tasks.append(
                _train_task(
                    NO_WORKSPACE_PERMUTATION,
                    candidate_recipe,
                    seed,
                    FINAL_TOKENS,
                    2048,
                    batches[
                        f"{NO_WORKSPACE_PERMUTATION}/{candidate_recipe.family}"
                    ],
                    cells_root,
                    data,
                )
            )
        if len(final_tasks) % GPU_COUNT:
            raise RuntimeError("final confirmation did not fill the eight-GPU node")
        final_rows = run_tasks(final_tasks)
        _all_complete(final_rows, "paired final")
        publish("final", final_rows)

        def nlls(model: str) -> dict[int, float]:
            return {
                int(row["seed"]): float(row["validation"]["nll"])
                for row in final_rows
                if row["model"] == model
            }

        candidate_nll = nlls(CANDIDATE)
        transformer_nll = nlls(strongest_transformer)
        ablation_nll = nlls(NO_WORKSPACE_PERMUTATION)
        paired = [
            candidate_nll[seed] - transformer_nll[seed] for seed in FINAL_SEEDS
        ]
        permutation_paired = [
            candidate_nll[seed] - ablation_nll[seed] for seed in FINAL_SEEDS
        ]
        mean_delta = sum(paired) / len(paired)
        verdict = (
            "block_kronecker_pilot_win"
            if all(value < 0 for value in paired) and mean_delta <= -MINIMUM_MEAN_WIN
            else "block_kronecker_not_yet_better"
        )
        result = {
            "schema": SCHEMA,
            "status": "complete",
            "verdict": verdict,
            "wandb_url": run.url,
            "objective": {
                "type": "16-token block-autoregressive language modeling",
                "input_tokens": 256,
                "prediction_shift_tokens": 16,
                "predicted_tokens_per_example": 256,
                "within_group_conditional_independence": True,
            },
            "preflight": preflight,
            "coarse": coarse_rows,
            "robust": robust_rows,
            "robust_summary": robust_summary,
            "selected_recipes": {
                model: asdict(recipe) for model, recipe in selected_recipes.items()
            },
            "strongest_transformer_locked_before_final": strongest_transformer,
            "final": final_rows,
            "paired_candidate_minus_transformer_block_nll": paired,
            "mean_candidate_minus_transformer_block_nll": mean_delta,
            "minimum_required_mean_win": MINIMUM_MEAN_WIN,
            "all_four_candidate_wins": all(value < 0 for value in paired),
            "paired_permuted_minus_unpermuted_candidate_block_nll": permutation_paired,
            "mean_permuted_minus_unpermuted_candidate_block_nll": (
                sum(permutation_paired) / len(permutation_paired)
            ),
            "inventories": {
                name: model_inventory(build_model(name)) for name in MODEL_NAMES
            },
            "cloud_only_training": True,
            "gpu_count": GPU_COUNT,
        }
        write_json(output_path, result)
        run.summary.update(
            {
                "verdict": verdict,
                "strongest_transformer": strongest_transformer,
                "mean_candidate_minus_transformer_block_nll": mean_delta,
                "mean_permuted_minus_unpermuted_candidate_block_nll": sum(
                    permutation_paired
                )
                / len(permutation_paired),
            }
        )
        run.finish()
        stop.set()
        if heartbeat_thread is not None:
            heartbeat_thread.join(timeout=2)
        return result
    except Exception:
        stop.set()
        if heartbeat_thread is not None:
            heartbeat_thread.join(timeout=2)
        run.finish(exit_code=1)
        raise


def main() -> None:
    parser = argparse.ArgumentParser(description="Run Exp14 on cloud 8xH100")
    parser.add_argument("--output", required=True)
    parser.add_argument("--data-root", required=True)
    parser.add_argument("--heartbeat")
    args = parser.parse_args()
    result = run_campaign(
        args.output,
        data_root=args.data_root,
        heartbeat=args.heartbeat,
    )
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)


if __name__ == "__main__":
    main()
