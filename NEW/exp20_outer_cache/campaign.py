"""Cloud-only eight-H100 campaign for Exp20 cached outer mixers."""

from __future__ import annotations

import argparse
import gc
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
from typing import Any, Iterable, Mapping, Sequence

import numpy as np
import torch
import torch.nn.functional as F
from fla.modules import FusedLinearCrossEntropyLoss

import exp14_block_kronecker.campaign as base
import exp17_group_density.campaign as exp17
from exp14_block_kronecker.data import CONTEXT_LENGTH, block_batch, load_windows
from exp17_group_density.campaign import Recipe
from exp19_norm_residual.campaign import (
    _tensor_comparison,
    balanced_repetition,
    exact_metrics,
    exposure_accounting,
    write_json,
)

from . import model as architectures


SCHEMA = "exp20-outer-cache-campaign-v1"
PREFLIGHT_SCHEMA = "exp20-outer-cache-preflight-v1"
CELL_SCHEMA = "exp20-outer-cache-cell-v1"
GPU_COUNT = 8
TARGET_PARAMETERS = 5_400_896
PARAMETER_TOLERANCE = 0.001
MINIMUM_GLOBAL_TOKENS = 100_000
MINIMUM_UTILIZATION = 85.0
BATCH_SEARCH = (4096, 3072, 2048, 1536, 1024, 768, 640, 512, 400)
UNDERFILLED_BATCH = 128
SCREEN_STEPS = 64
CONFIRMATION_STEPS = 128
SCREEN_SEED = 2001
CONFIRMATION_SEEDS = (2002, 2003, 2004)
SAMPLE_INDICES = (17, 997)
EVALUATION_INTERVAL = 4
SUCCESS_NLL = 0.01


EXECUTION_VARIANTS: tuple[dict[str, Any], ...] = (
    {
        "variant": "dense-r8-reference-dynamic",
        "model": architectures.DENSE_R8_REFERENCE,
        "dynamic": True,
        "compile_mode": "default",
    },
    {
        "variant": "dense-r8-reference-static",
        "model": architectures.DENSE_R8_REFERENCE,
        "dynamic": False,
        "compile_mode": "max-autotune",
    },
    {
        "variant": architectures.DENSE_R8_PACKED,
        "model": architectures.DENSE_R8_PACKED,
        "dynamic": False,
        "compile_mode": "max-autotune",
    },
    {
        "variant": architectures.DENSE_R4_PACKED,
        "model": architectures.DENSE_R4_PACKED,
        "dynamic": False,
        "compile_mode": "max-autotune",
    },
    {
        "variant": architectures.DENSE_R2_PACKED,
        "model": architectures.DENSE_R2_PACKED,
        "dynamic": False,
        "compile_mode": "max-autotune",
    },
    {
        "variant": architectures.DENSE_R1_PACKED,
        "model": architectures.DENSE_R1_PACKED,
        "dynamic": False,
        "compile_mode": "max-autotune",
    },
    {
        "variant": architectures.RECURRENT3_R8_PACKED,
        "model": architectures.RECURRENT3_R8_PACKED,
        "dynamic": False,
        "compile_mode": "max-autotune",
    },
    {
        "variant": architectures.TRANSFORMER_DEEP,
        "model": architectures.TRANSFORMER_DEEP,
        "dynamic": False,
        "compile_mode": "max-autotune",
    },
)


def screen_recipes() -> tuple[Recipe, ...]:
    adamw = tuple(
        Recipe("adamw", lr, lr, weight_decay=0.0, warmup_tokens=1)
        for lr in (0.003, 0.012, 0.048)
    )
    muon = tuple(
        Recipe(
            "muon", body, auxiliary, weight_decay=0.0, warmup_tokens=1
        )
        for body, auxiliary in ((0.06, 0.012), (0.24, 0.048), (0.96, 0.096))
    )
    return (*adamw, *muon)


def compile_hidden(
    model: torch.nn.Module, *, dynamic: bool, compile_mode: str
) -> Any:
    return torch.compile(
        model.hidden,
        backend="inductor",
        mode=compile_mode,
        fullgraph=True,
        dynamic=dynamic,
    )


def _variant(task: Mapping[str, Any]) -> str:
    return str(task.get("variant", task["model"]))


def _build_model(name: str, checkpointing: bool | None = None) -> torch.nn.Module:
    return architectures.build_model(name, activation_checkpointing=checkpointing)


def _gradient_map(model: torch.nn.Module) -> dict[str, torch.Tensor]:
    missing = [name for name, value in model.named_parameters() if value.grad is None]
    if missing:
        raise RuntimeError(f"parameters without gradients: {missing[:20]}")
    result = {
        name: value.grad.detach().clone() for name, value in model.named_parameters()
    }
    if not all(torch.isfinite(value).all() for value in result.values()):
        raise RuntimeError("non-finite gradient")
    return result


def _loss_backward(
    model: torch.nn.Module,
    inputs: torch.Tensor,
    targets: torch.Tensor,
    *,
    compiled: bool,
    bf16: bool,
    dynamic: bool = False,
    compile_mode: str = "default",
) -> tuple[float, dict[str, torch.Tensor]]:
    model.zero_grad(set_to_none=True)
    hidden = (
        compile_hidden(model, dynamic=dynamic, compile_mode=compile_mode)
        if compiled
        else model.hidden
    )
    with torch.autocast("cuda", dtype=torch.bfloat16, enabled=bf16):
        loss = base.fused_loss(
            model, inputs, targets, FusedLinearCrossEntropyLoss(), hidden
        )
    loss.backward()
    return float(loss.detach()), _gradient_map(model)


def correctness_cell(task: dict[str, Any], device: torch.device) -> dict[str, Any]:
    name = str(task["model"])
    windows = load_windows(task["data_root"], "train")
    inputs, targets = block_batch(windows, np.asarray(SAMPLE_INDICES), device)
    torch.manual_seed(2000)
    eager = _build_model(name, checkpointing=False).to(device)
    checkpointed = _build_model(name, checkpointing=True).to(device)
    checkpointed.load_state_dict(eager.state_dict())
    architecture = architectures.correctness_checks(name, device)
    if not architecture["pass"]:
        raise RuntimeError(f"architecture correctness failed: {architecture}")
    eager_loss, eager_gradients = _loss_backward(
        eager, inputs, targets, compiled=False, bf16=False
    )
    checkpoint_loss, checkpoint_gradients = _loss_backward(
        checkpointed, inputs, targets, compiled=False, bf16=False
    )
    checkpoint_parity = _tensor_comparison(eager_gradients, checkpoint_gradients)
    checkpoint_parity["absolute_loss_error"] = abs(eager_loss - checkpoint_loss)
    checkpoint_parity["pass"] = (
        checkpoint_parity["global_relative_error"] <= 1e-5
        and checkpoint_parity["maximum_absolute_error"] <= 1e-6
        and checkpoint_parity["absolute_loss_error"] <= 1e-6
    )

    compiled_model = _build_model(name, checkpointing=False).to(device)
    compiled_model.load_state_dict(eager.state_dict())
    eager_bf16_loss, eager_bf16_gradients = _loss_backward(
        eager, inputs, targets, compiled=False, bf16=True
    )
    compiled_loss, compiled_gradients = _loss_backward(
        compiled_model,
        inputs,
        targets,
        compiled=True,
        bf16=True,
        dynamic=bool(task.get("dynamic", False)),
        compile_mode=str(task.get("compile_mode", "default")),
    )
    compiled_parity = _tensor_comparison(eager_bf16_gradients, compiled_gradients)
    compiled_parity["relative_loss_error"] = abs(
        eager_bf16_loss - compiled_loss
    ) / max(abs(eager_bf16_loss), 1e-12)
    compiled_parity["pass"] = (
        compiled_parity["global_relative_error"] <= 0.02
        and compiled_parity["global_cosine"] >= 0.999
        and compiled_parity["maximum_absolute_error"] <= 0.002
        and compiled_parity["relative_loss_error"] <= 0.001
    )
    result = {
        "schema": CELL_SCHEMA,
        "status": (
            "complete"
            if architecture["pass"]
            and checkpoint_parity["pass"]
            and compiled_parity["pass"]
            else "failed"
        ),
        "kind": "correctness",
        "model": name,
        "variant": _variant(task),
        "architecture": architecture,
        "checkpoint_gradient_parity": checkpoint_parity,
        "compiled_bf16_gradient_parity": compiled_parity,
        "finite_forward_backward": True,
    }
    del eager, checkpointed, compiled_model, inputs, targets
    gc.collect()
    torch.cuda.empty_cache()
    return result


def _benchmark_update(
    *,
    model: torch.nn.Module,
    hidden: Any,
    optimizer: Any,
    loss_function: Any,
    windows: np.ndarray,
    batch: int,
    step: int,
    device: torch.device,
) -> tuple[float, float, float]:
    indices = base._logical_indices(len(windows), step, batch, 2000)
    inputs, targets = block_batch(windows, indices, device)
    optimizer.zero_grad(set_to_none=True)
    torch.cuda.synchronize(device)
    started = time.perf_counter()
    with torch.autocast("cuda", dtype=torch.bfloat16):
        loss = base.fused_loss(model, inputs, targets, loss_function, hidden)
    loss.backward()
    norm = torch.nn.utils.clip_grad_norm_(model.parameters(), 1.0)
    if not torch.isfinite(loss) or not torch.isfinite(norm):
        raise RuntimeError("non-finite benchmark forward/backward")
    optimizer.step()
    if not base.optimizer_is_finite(optimizer):
        raise RuntimeError("non-finite optimizer state")
    torch.cuda.synchronize(device)
    return time.perf_counter() - started, float(loss), float(norm)


def benchmark(task: dict[str, Any], device: torch.device) -> dict[str, Any]:
    name = str(task["model"])
    batch = int(task["batch"])
    measured_steps = int(task.get("measured_steps", 8))
    dynamic = bool(task.get("dynamic", False))
    compile_mode = str(task.get("compile_mode", "max-autotune"))
    windows = load_windows(task["data_root"], "train")
    torch.manual_seed(2000)
    torch.cuda.empty_cache()
    torch.cuda.reset_peak_memory_stats(device)
    model = _build_model(name).to(device)
    hidden = compile_hidden(model, dynamic=dynamic, compile_mode=compile_mode)
    recipe = Recipe("adamw", 0.003, 0.003, weight_decay=0.0, warmup_tokens=1)
    optimizer, routing = exp17.create_optimizer(model, recipe)
    loss_function = FusedLinearCrossEntropyLoss()
    warmup, _, _ = _benchmark_update(
        model=model,
        hidden=hidden,
        optimizer=optimizer,
        loss_function=loss_function,
        windows=windows,
        batch=batch,
        step=0,
        device=device,
    )
    elapsed = 0.0
    last_nll = last_norm = math.nan
    with base.GpuSampler(device) as sampler:
        for step in range(1, measured_steps + 1):
            duration, last_nll, last_norm = _benchmark_update(
                model=model,
                hidden=hidden,
                optimizer=optimizer,
                loss_function=loss_function,
                windows=windows,
                batch=batch,
                step=step,
                device=device,
            )
            elapsed += duration
    result = {
        "schema": CELL_SCHEMA,
        "status": "complete",
        "kind": "benchmark",
        "model": name,
        "variant": _variant(task),
        "dynamic": dynamic,
        "compile_mode": compile_mode,
        "batch": batch,
        "global_examples_per_step": batch,
        "global_tokens_per_step": batch * CONTEXT_LENGTH,
        "gradient_accumulation": 1,
        "tokens_per_second": measured_steps * batch * CONTEXT_LENGTH / elapsed,
        "step_seconds": elapsed / measured_steps,
        "warmup_seconds": warmup,
        "last_nll": last_nll,
        "last_grad_norm": last_norm,
        "finite_forward_backward_optimizer": True,
        "peak_allocated_gib": torch.cuda.max_memory_allocated(device) / 2**30,
        "peak_reserved_gib": torch.cuda.max_memory_reserved(device) / 2**30,
        "optimizer_routing": routing,
        **sampler.summary(),
    }
    del model, optimizer, loss_function, hidden
    gc.collect()
    torch.cuda.empty_cache()
    return result


def batch_sweep(task: dict[str, Any], device: torch.device) -> dict[str, Any]:
    curve: list[dict[str, Any]] = []
    stable = 0
    for batch in BATCH_SEARCH:
        try:
            row = benchmark({**task, "kind": "benchmark", "batch": batch}, device)
        except Exception as error:
            row = {
                "status": "failed",
                "kind": "benchmark",
                "model": task["model"],
                "variant": _variant(task),
                "batch": batch,
                "failure": (
                    "out_of_memory"
                    if isinstance(error, torch.OutOfMemoryError)
                    or "out of memory" in str(error).lower()
                    else f"{type(error).__name__}: {str(error)[:1600]}"
                ),
            }
            gc.collect()
            torch.cuda.empty_cache()
        curve.append(row)
        if row.get("status") == "complete":
            stable += 1
            if stable >= 3:
                break
    baseline = benchmark(
        {**task, "kind": "benchmark", "batch": UNDERFILLED_BATCH}, device
    )
    eligible = [
        row
        for row in curve
        if row.get("status") == "complete"
        and int(row["global_tokens_per_step"]) >= MINIMUM_GLOBAL_TOKENS
        and float(row.get("median_gpu_utilization_percent", 0)) >= MINIMUM_UTILIZATION
    ]
    if not eligible:
        raise RuntimeError(f"no stable utilized >=100k-token batch for {_variant(task)}")
    selected = max(eligible, key=lambda row: float(row["tokens_per_second"]))
    return {
        "schema": CELL_SCHEMA,
        "status": "complete",
        "kind": "batch-sweep",
        "model": task["model"],
        "variant": _variant(task),
        "curve": curve,
        "underfilled_baseline": baseline,
        "selected": selected,
        "selected_over_underfilled_throughput": float(selected["tokens_per_second"])
        / float(baseline["tokens_per_second"]),
    }


def operator_profile(task: dict[str, Any], device: torch.device) -> dict[str, Any]:
    trace_dir = Path(task["trace_dir"])
    trace_dir.mkdir(parents=True, exist_ok=True)
    trace_path = trace_dir / f"{_variant(task)}.json"
    name = str(task["model"])
    batch = int(task["batch"])
    windows = load_windows(task["data_root"], "train")
    model = _build_model(name).to(device)
    hidden = compile_hidden(
        model,
        dynamic=bool(task.get("dynamic", False)),
        compile_mode=str(task.get("compile_mode", "max-autotune")),
    )
    optimizer, _ = exp17.create_optimizer(
        model, Recipe("adamw", 0.003, 0.003, weight_decay=0.0, warmup_tokens=1)
    )
    loss_function = FusedLinearCrossEntropyLoss()
    _benchmark_update(
        model=model,
        hidden=hidden,
        optimizer=optimizer,
        loss_function=loss_function,
        windows=windows,
        batch=batch,
        step=0,
        device=device,
    )
    activities = [torch.profiler.ProfilerActivity.CPU, torch.profiler.ProfilerActivity.CUDA]
    with torch.profiler.profile(
        activities=activities,
        record_shapes=True,
        profile_memory=True,
        with_flops=True,
    ) as profile:
        duration, loss, norm = _benchmark_update(
            model=model,
            hidden=hidden,
            optimizer=optimizer,
            loss_function=loss_function,
            windows=windows,
            batch=batch,
            step=1,
            device=device,
        )
    profile.export_chrome_trace(str(trace_path))
    table = profile.key_averages().table(
        sort_by="self_cuda_time_total", row_limit=40
    )
    return {
        "schema": CELL_SCHEMA,
        "status": "complete",
        "kind": "operator-profile",
        "model": name,
        "variant": _variant(task),
        "batch": batch,
        "global_tokens_per_step": batch * CONTEXT_LENGTH,
        "step_seconds": duration,
        "loss": loss,
        "grad_norm": norm,
        "trace_path": str(trace_path),
        "top_cuda_operators": table,
    }


@torch.inference_mode()
def cache_benchmark(task: dict[str, Any], device: torch.device) -> dict[str, Any]:
    name = str(task["model"])
    batch = int(task["batch"])
    repeats = int(task.get("repeats", 3))
    torch.manual_seed(2010)
    model = _build_model(name, checkpointing=False).to(device).eval()
    if not isinstance(model, architectures.CachedLanguageModel):
        raise TypeError("cache benchmark requires an Exp20 cached model")
    spec = model.spec
    tokens = torch.randint(
        0, spec.vocab_size, (batch, spec.context_length), device=device
    )

    # Warm both paths. Future padding is deliberately computed by the naive
    # baseline, matching a full-prefix rerun while scoring only the newest group.
    cache = model.init_block_cache(batch, device=device, dtype=model.vocabulary.dtype)
    model.forward_block(tokens[:, : spec.group_size], cache)
    hidden = model.hidden(tokens)
    F.linear(hidden[:, : spec.group_size], model.vocabulary)
    torch.cuda.synchronize(device)

    cached_times = [0.0] * spec.group_count
    naive_times = [0.0] * spec.group_count
    torch.cuda.reset_peak_memory_stats(device)
    for _ in range(repeats):
        cache = model.init_block_cache(
            batch, device=device, dtype=model.vocabulary.dtype
        )
        for position, group in enumerate(tokens.split(spec.group_size, 1)):
            torch.cuda.synchronize(device)
            started = time.perf_counter()
            model.forward_block(group, cache)
            torch.cuda.synchronize(device)
            cached_times[position] += time.perf_counter() - started
        for position in range(spec.group_count):
            torch.cuda.synchronize(device)
            started = time.perf_counter()
            full_hidden = model.hidden(tokens)
            start = position * spec.group_size
            F.linear(
                full_hidden[:, start : start + spec.group_size], model.vocabulary
            )
            torch.cuda.synchronize(device)
            naive_times[position] += time.perf_counter() - started
    cached = [value / repeats for value in cached_times]
    naive = [value / repeats for value in naive_times]
    cache_bytes = 0
    for layer in cache.layers:
        for value in vars(layer).values():
            if isinstance(value, torch.Tensor):
                cache_bytes += value.numel() * value.element_size()
    return {
        "schema": CELL_SCHEMA,
        "status": "complete",
        "kind": "cache-benchmark",
        "model": name,
        "variant": _variant(task),
        "batch": batch,
        "group_size": spec.group_size,
        "groups": spec.group_count,
        "cache_bytes": cache_bytes,
        "cache_mib_per_sequence": cache_bytes / batch / 2**20,
        "cached_group_seconds": cached,
        "naive_group_seconds": naive,
        "final_group_speedup": naive[-1] / cached[-1],
        "end_to_end_speedup": sum(naive) / sum(cached),
        "peak_allocated_gib": torch.cuda.max_memory_allocated(device) / 2**30,
    }


def _threshold_hits(curve: Sequence[Mapping[str, Any]]) -> dict[str, Any]:
    result: dict[str, Any] = {}
    for threshold in (1.0, 0.1, SUCCESS_NLL, 0.001):
        hits = [row for row in curve if float(row["nll"]) <= threshold]
        result[f"nll_le_{threshold:g}"] = (
            {
                "step": int(hits[0]["step"]),
                "elapsed_seconds": float(hits[0]["elapsed_seconds"]),
            }
            if hits
            else None
        )
    return result


def fit_cell(task: dict[str, Any], device: torch.device) -> dict[str, Any]:
    name = str(task["model"])
    recipe = Recipe(**task["recipe"])
    seed = int(task["seed"])
    indices = tuple(int(index) for index in task["sample_indices"])
    unique_count = len(indices)
    physical_batch = int(task["batch"])
    maximum_steps = int(task["maximum_steps"])
    result_path = (
        Path(task["output_root"])
        / "fit-cells"
        / str(task["stage"])
        / name
        / (
            f"n{unique_count}-{exp17.recipe_slug(recipe)}-seed{seed}"
            f"-filler{int(task.get('full_node_filler_replica', 0))}.json"
        )
    )
    if result_path.is_file():
        prior = json.loads(result_path.read_text())
        if prior.get("status") == "complete":
            return prior
    train_windows = load_windows(task["data_root"], "train")
    torch.manual_seed(seed)
    model = _build_model(name).to(device)
    hidden = compile_hidden(model, dynamic=False, compile_mode="max-autotune")
    optimizer, routing = exp17.create_optimizer(model, recipe)
    loss_function = FusedLinearCrossEntropyLoss()
    unique_inputs, unique_targets = block_batch(
        train_windows, np.asarray(indices), device
    )
    gather = torch.as_tensor(
        balanced_repetition(unique_count, physical_batch), device=device
    )
    inputs = unique_inputs.index_select(0, gather)
    targets = unique_targets.index_select(0, gather)
    initial = exact_metrics(model, unique_inputs, unique_targets)
    curve: list[dict[str, Any]] = [{"step": 0, "elapsed_seconds": 0.0, **initial}]
    elapsed = 0.0
    clipped = 0
    torch.cuda.reset_peak_memory_stats(device)
    with base.GpuSampler(device) as sampler:
        for step in range(1, maximum_steps + 1):
            model.train()
            optimizer.zero_grad(set_to_none=True)
            exp17.set_learning_rates(
                optimizer, recipe, step * unique_count * CONTEXT_LENGTH
            )
            torch.cuda.synchronize(device)
            started = time.perf_counter()
            with torch.autocast("cuda", dtype=torch.bfloat16):
                loss = base.fused_loss(
                    model, inputs, targets, loss_function, hidden
                )
            loss.backward()
            norm = torch.nn.utils.clip_grad_norm_(model.parameters(), recipe.clip_norm)
            if not torch.isfinite(loss) or not torch.isfinite(norm):
                raise RuntimeError("non-finite fit state")
            clipped += int(float(norm) > recipe.clip_norm)
            optimizer.step()
            if not base.optimizer_is_finite(optimizer):
                raise RuntimeError("non-finite optimizer state")
            torch.cuda.synchronize(device)
            elapsed += time.perf_counter() - started
            if step % EVALUATION_INTERVAL == 0 or step == maximum_steps:
                measured = exact_metrics(model, unique_inputs, unique_targets)
                curve.append({"step": step, "elapsed_seconds": elapsed, **measured})
                if float(measured["nll"]) <= 0.001:
                    break
    final = curve[-1]
    hits = _threshold_hits(curve)
    result = {
        "schema": CELL_SCHEMA,
        "status": "complete",
        "kind": "fit",
        "stage": task["stage"],
        "model": name,
        "variant": name,
        "recipe": asdict(recipe),
        "seed": seed,
        "unique_count": unique_count,
        **exposure_accounting(
            unique_count=unique_count,
            physical_batch=physical_batch,
            steps=int(final["step"]),
        ),
        "curve": curve,
        "threshold_hits": hits,
        "success": hits["nll_le_0.01"] is not None,
        "final": final,
        "optimizer_routing": routing,
        "performance": {
            "elapsed_seconds": elapsed,
            "physical_tokens_per_second": (
                int(final["step"]) * physical_batch * CONTEXT_LENGTH
                / max(elapsed, 1e-12)
            ),
            "clip_fraction": clipped / max(int(final["step"]), 1),
            "peak_allocated_gib": torch.cuda.max_memory_allocated(device) / 2**30,
            "peak_reserved_gib": torch.cuda.max_memory_reserved(device) / 2**30,
            **sampler.summary(),
        },
    }
    write_json(result_path, result)
    del model, optimizer, inputs, targets, unique_inputs, unique_targets
    gc.collect()
    torch.cuda.empty_cache()
    return result


def execute_task(task: dict[str, Any], device: torch.device) -> dict[str, Any]:
    kind = str(task["kind"])
    if kind == "correctness":
        return correctness_cell(task, device)
    if kind == "batch-sweep":
        return batch_sweep(task, device)
    if kind == "benchmark":
        return benchmark(task, device)
    if kind == "operator-profile":
        return operator_profile(task, device)
    if kind == "cache-benchmark":
        return cache_benchmark(task, device)
    if kind == "fit":
        return fit_cell(task, device)
    raise ValueError(f"unknown Exp20 task kind: {kind}")


def worker_loop(gpu: int, tasks: mp.Queue, results: mp.Queue) -> None:
    torch.cuda.set_device(gpu)
    device = torch.device(f"cuda:{gpu}")
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
                "variant": _variant(task),
                "stage": task.get("stage"),
                "seed": task.get("seed"),
                "failure": (
                    "out_of_memory"
                    if isinstance(error, torch.OutOfMemoryError)
                    or "out of memory" in str(error).lower()
                    else f"{type(error).__name__}: {str(error)[:2400]}"
                ),
            }
            gc.collect()
            torch.cuda.empty_cache()
        results.put((index, row))


def run_tasks(tasks: Sequence[dict[str, Any]], timeout: int = 21_600) -> list[dict[str, Any]]:
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
        raise TimeoutError("Exp20 GPU worker timed out") from error
    finally:
        for process in processes:
            process.join(timeout=10)
            if process.is_alive():
                process.terminate()
    if any(row is None for row in rows):
        raise RuntimeError("missing Exp20 task result")
    return [row for row in rows if row is not None]


def fill_full_waves(tasks: Sequence[dict[str, Any]]) -> tuple[list[dict[str, Any]], int]:
    filled = [dict(task) for task in tasks]
    original = len(filled)
    if not filled:
        return filled, original
    replica = 0
    while len(filled) % GPU_COUNT:
        filler = dict(filled[replica % original])
        replica += 1
        filler["full_node_filler_replica"] = replica
        filled.append(filler)
    return filled, original


def all_complete(rows: Iterable[Mapping[str, Any]], label: str) -> None:
    failed = [row for row in rows if row.get("status") != "complete"]
    if failed:
        raise RuntimeError(f"{label} failed: {failed[:8]}")


def paid_preflight(data: Path, cells: Path) -> dict[str, Any]:
    path = cells / "preflight.json"
    if torch.cuda.device_count() != GPU_COUNT:
        raise RuntimeError("Exp20 requires exactly eight visible GPUs")
    gpu_names = [torch.cuda.get_device_name(index) for index in range(GPU_COUNT)]
    gpu_memory = [
        torch.cuda.get_device_properties(index).total_memory / 2**30
        for index in range(GPU_COUNT)
    ]
    if any("H100" not in name for name in gpu_names) or any(
        memory < 75 for memory in gpu_memory
    ):
        raise RuntimeError(f"Exp20 requires 8xH100-80GB: {gpu_names}/{gpu_memory}")
    inventories = {
        name: architectures.model_inventory(architectures.build_model(name))
        for name in architectures.MODEL_NAMES
    }
    mismatches = {
        name: abs(int(row["total_parameters"]) / TARGET_PARAMETERS - 1.0)
        for name, row in inventories.items()
    }
    if any(value > PARAMETER_TOLERANCE for value in mismatches.values()):
        raise RuntimeError(f"parameter matching failed: {mismatches}")
    progress: dict[str, Any] = {
        "schema": PREFLIGHT_SCHEMA,
        "status": "running",
        "gpu_names": gpu_names,
        "gpu_memory_gib": gpu_memory,
        "inventories": inventories,
        "parameter_mismatch_fractions": mismatches,
        "execution_variants": EXECUTION_VARIANTS,
        "gradient_accumulation": 1,
    }
    write_json(path, progress)
    correctness_tasks = []
    seen = set()
    for variant in EXECUTION_VARIANTS:
        if variant["model"] in seen:
            continue
        seen.add(variant["model"])
        correctness_tasks.append({"kind": "correctness", "data_root": str(data), **variant})
    filled, count = fill_full_waves(correctness_tasks)
    correctness_all = run_tasks(filled)
    correctness = correctness_all[:count]
    all_complete(correctness, "correctness/parity")
    progress["correctness"] = correctness
    write_json(path, progress)

    sweep_tasks = [
        {"kind": "batch-sweep", "data_root": str(data), **variant}
        for variant in EXECUTION_VARIANTS
    ]
    sweeps = run_tasks(sweep_tasks)
    all_complete(sweeps, "ambitious batch sweep")
    selected = {
        str(row["variant"]): row["selected"] for row in sweeps
    }
    progress["batch_sweeps"] = sweeps
    progress["selected"] = selected
    write_json(path, progress)

    full_node = run_tasks(
        [
            {
                "kind": "benchmark",
                "data_root": str(data),
                "batch": int(selected[str(variant["variant"])]["batch"]),
                "measured_steps": 10,
                **variant,
            }
            for variant in EXECUTION_VARIANTS
        ]
    )
    all_complete(full_node, "full-node benchmark")
    if any(
        float(row.get("median_gpu_utilization_percent", 0)) < MINIMUM_UTILIZATION
        or int(row.get("global_tokens_per_step", 0)) < MINIMUM_GLOBAL_TOKENS
        for row in full_node
    ):
        raise RuntimeError("full-node utilization/token-batch gate failed")
    by_variant = {str(row["variant"]): row for row in full_node}
    reference = float(
        by_variant["dense-r8-reference-dynamic"]["tokens_per_second"]
    )
    packed = float(by_variant[architectures.DENSE_R8_PACKED]["tokens_per_second"])
    implementation_speedup = packed / reference
    progress.update(
        {
            "status": "pass",
            "full_node_workers": full_node,
            "full_node_aggregate_tokens_per_second": sum(
                float(row["tokens_per_second"]) for row in full_node
            ),
            "dense_r8_packed_over_reference_speedup": implementation_speedup,
            "packed_gemm_two_x_gate": implementation_speedup >= 2.0,
            "requires_fused_triton": implementation_speedup < 2.0,
            "minimum_global_tokens_per_step": MINIMUM_GLOBAL_TOKENS,
            "minimum_utilization_percent": MINIMUM_UTILIZATION,
        }
    )
    write_json(path, progress)
    return progress


def _fit_task(
    *,
    stage: str,
    model: str,
    recipe: Recipe,
    seed: int,
    indices: Sequence[int],
    maximum_steps: int,
    batch: int,
    cells: Path,
    data: Path,
) -> dict[str, Any]:
    return {
        "kind": "fit",
        "stage": stage,
        "model": model,
        "recipe": asdict(recipe),
        "seed": seed,
        "sample_indices": list(indices),
        "maximum_steps": maximum_steps,
        "batch": batch,
        "output_root": str(cells),
        "data_root": str(data),
    }


def _cell_score(row: Mapping[str, Any]) -> tuple[Any, ...]:
    hit = row.get("threshold_hits", {}).get("nll_le_0.01")
    return (
        hit is None,
        math.inf if hit is None else int(hit["step"]),
        float(row.get("final", {}).get("nll", math.inf)),
        float(row.get("performance", {}).get("elapsed_seconds", math.inf)),
    )


def promote_recipes(rows: Sequence[dict[str, Any]]) -> dict[str, dict[str, Recipe]]:
    promoted: dict[str, dict[str, Recipe]] = {}
    for name in (architectures.DENSE_R8_PACKED, architectures.RECURRENT3_R8_PACKED):
        promoted[name] = {}
        for family in ("adamw", "muon"):
            candidates = [
                row
                for row in rows
                if row.get("model") == name
                and row.get("recipe", {}).get("family") == family
                and row.get("status") == "complete"
            ]
            if not candidates:
                raise RuntimeError(f"missing tuned cells for {name}/{family}")
            promoted[name][family] = Recipe(**min(candidates, key=_cell_score)["recipe"])
    return promoted


def tuning_extensions(rows: Sequence[dict[str, Any]]) -> list[tuple[str, Recipe]]:
    """Expand a winning LR boundary geometrically, with explicit safety caps."""

    limits = {
        "adamw_body": (0.0001875, 0.192),
        "muon_body": (0.001875, 1.92),
        "muon_auxiliary": (0.0001875, 0.096),
    }
    extensions: dict[tuple[str, str], tuple[str, Recipe]] = {}
    for name in (architectures.DENSE_R8_PACKED, architectures.RECURRENT3_R8_PACKED):
        for family in ("adamw", "muon"):
            candidates = [
                row
                for row in rows
                if row.get("status") == "complete"
                and row.get("model") == name
                and row.get("recipe", {}).get("family") == family
            ]
            if not candidates:
                continue
            best = min(candidates, key=_cell_score)
            recipe = Recipe(**best["recipe"])
            bodies = sorted({float(row["recipe"]["body_lr"]) for row in candidates})
            auxiliaries = sorted(
                {float(row["recipe"]["auxiliary_lr"]) for row in candidates}
            )
            if recipe.body_lr in {bodies[0], bodies[-1]}:
                proposed = (
                    recipe.body_lr / 4
                    if recipe.body_lr == bodies[0]
                    else recipe.body_lr * 4
                )
                lower, upper = limits[f"{family}_body"]
                proposed = min(upper, max(lower, proposed))
                if proposed != recipe.body_lr:
                    extension = Recipe(
                        family,
                        proposed,
                        proposed if family == "adamw" else recipe.auxiliary_lr,
                        weight_decay=0.0,
                        warmup_tokens=1,
                    )
                    extensions[(name, exp17.recipe_slug(extension))] = (name, extension)
            if family == "muon" and recipe.auxiliary_lr in {
                auxiliaries[0],
                auxiliaries[-1],
            }:
                proposed = (
                    recipe.auxiliary_lr / 4
                    if recipe.auxiliary_lr == auxiliaries[0]
                    else recipe.auxiliary_lr * 4
                )
                lower, upper = limits["muon_auxiliary"]
                proposed = min(upper, max(lower, proposed))
                if proposed != recipe.auxiliary_lr:
                    extension = Recipe(
                        "muon",
                        recipe.body_lr,
                        proposed,
                        weight_decay=0.0,
                        warmup_tokens=1,
                    )
                    extensions[(name, exp17.recipe_slug(extension))] = (name, extension)
    existing = {
        (str(row["model"]), exp17.recipe_slug(Recipe(**row["recipe"]))) for row in rows
    }
    return [value for key, value in extensions.items() if key not in existing]


def confirmation_summary(rows: Sequence[dict[str, Any]]) -> dict[str, Any]:
    summary: dict[str, Any] = {}
    for name in (architectures.DENSE_R8_PACKED, architectures.RECURRENT3_R8_PACKED):
        families = []
        for family in ("adamw", "muon"):
            candidates = [
                row
                for row in rows
                if row["model"] == name and row["recipe"]["family"] == family
            ]
            hits = [
                row["threshold_hits"].get("nll_le_0.01") for row in candidates
            ]
            families.append(
                {
                    "family": family,
                    "recipe": candidates[0]["recipe"],
                    "successful_seeds": sum(hit is not None for hit in hits),
                    "mean_steps_to_success": (
                        statistics.fmean(int(hit["step"]) for hit in hits)
                        if all(hit is not None for hit in hits)
                        else None
                    ),
                    "mean_seconds_to_success": (
                        statistics.fmean(float(hit["elapsed_seconds"]) for hit in hits)
                        if all(hit is not None for hit in hits)
                        else None
                    ),
                }
            )
        winner = min(
            families,
            key=lambda row: (
                -int(row["successful_seeds"]),
                math.inf
                if row["mean_steps_to_success"] is None
                else float(row["mean_steps_to_success"]),
                math.inf
                if row["mean_seconds_to_success"] is None
                else float(row["mean_seconds_to_success"]),
            ),
        )
        summary[name] = {"families": families, "winner": winner}
    dense = summary[architectures.DENSE_R8_PACKED]["winner"]
    recurrent = summary[architectures.RECURRENT3_R8_PACKED]["winner"]
    step_ratio = (
        float(recurrent["mean_steps_to_success"])
        / float(dense["mean_steps_to_success"])
        if recurrent["mean_steps_to_success"] is not None
        and dense["mean_steps_to_success"] is not None
        else math.inf
    )
    time_ratio = (
        float(recurrent["mean_seconds_to_success"])
        / float(dense["mean_seconds_to_success"])
        if recurrent["mean_seconds_to_success"] is not None
        and dense["mean_seconds_to_success"] is not None
        else math.inf
    )
    summary["promotion"] = {
        "recurrent_over_dense_step_ratio": step_ratio,
        "recurrent_over_dense_wall_time_ratio": time_ratio,
        "within_25_percent_steps": step_ratio <= 1.25,
        "faster_wall_clock": time_ratio < 1.0,
        "pass": step_ratio <= 1.25 and time_ratio < 1.0,
    }
    return summary


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
    cells = output_path.parent / "exp20-cells"
    traces = cells / "traces"
    load_windows(data, "train")
    wandb.login(key=os.environ["WANDB_API_KEY"], verify=True)
    run = wandb.init(
        project="exp20-outer-cache",
        name="exp20-cache-kernel-rank-exploration",
        config={
            "schema": SCHEMA,
            "models": architectures.MODEL_NAMES,
            "execution_variants": EXECUTION_VARIANTS,
            "hardware": "8xH100-80GB",
            "cloud_only_training": True,
            "minimum_global_tokens_per_step": MINIMUM_GLOBAL_TOKENS,
            "gradient_accumulation": 1,
            "ordinary_autoregressive_claim": False,
        },
    )
    if not run.url:
        raise RuntimeError("W&B did not provide a direct run URL")
    print(f"WANDB_URL={run.url}", flush=True)
    write_json(output_path, {"schema": SCHEMA, "status": "running", "wandb_url": run.url})
    heartbeat_path = Path(heartbeat) if heartbeat else None
    stop = threading.Event()
    heartbeat_thread = None
    if heartbeat_path:
        heartbeat_path.parent.mkdir(parents=True, exist_ok=True)
        heartbeat_path.touch()

        def pulse() -> None:
            while not stop.wait(30):
                heartbeat_path.touch()

        heartbeat_thread = threading.Thread(target=pulse, daemon=True)
        heartbeat_thread.start()

    try:
        preflight = paid_preflight(data, cells)
        preflight_artifact = wandb.Artifact(
            f"exp20-preflight-{run.id}", type="preflight"
        )
        preflight_artifact.add_file(str(cells / "preflight.json"))
        run.log_artifact(preflight_artifact)
        selected = preflight["selected"]
        throughput_columns = [
            "variant",
            "model",
            "batch",
            "global_tokens_per_step",
            "tokens_per_second",
            "step_seconds",
            "gpu_utilization_percent",
            "power_watts",
            "peak_allocated_gib",
            "peak_reserved_gib",
        ]
        throughput_data = []
        for row in preflight["full_node_workers"]:
            throughput_data.append(
                [
                    row["variant"],
                    row["model"],
                    row["batch"],
                    row["global_tokens_per_step"],
                    row["tokens_per_second"],
                    row["step_seconds"],
                    row["median_gpu_utilization_percent"],
                    row["median_power_watts"],
                    row["peak_allocated_gib"],
                    row["peak_reserved_gib"],
                ]
            )
            run.summary[f"throughput/{row['variant']}/tokens_per_second"] = row[
                "tokens_per_second"
            ]
            run.summary[f"throughput/{row['variant']}/global_tokens_per_step"] = row[
                "global_tokens_per_step"
            ]
        run.log(
            {
                "preflight/full_node_throughput": wandb.Table(
                    columns=throughput_columns, data=throughput_data
                )
            }
        )

        profile_tasks = [
            {
                "kind": "operator-profile",
                "data_root": str(data),
                "trace_dir": str(traces),
                # Profiler bookkeeping can consume substantial memory. This
                # remains above the 100k-token floor while throughput itself
                # is measured at the selected ambitious batch.
                "batch": min(
                    512, int(selected[str(variant["variant"])]["batch"])
                ),
                **variant,
            }
            for variant in EXECUTION_VARIANTS
        ]
        profiles = run_tasks(profile_tasks)
        all_complete(profiles, "operator profiles")
        profile_path = cells / "operator-profiles.json"
        write_json(profile_path, profiles)
        profile_artifact = wandb.Artifact(
            f"exp20-operator-profiles-{run.id}", type="profiler"
        )
        profile_artifact.add_file(str(profile_path))
        for trace in traces.glob("*.json"):
            profile_artifact.add_file(str(trace), name=f"traces/{trace.name}")
        run.log_artifact(profile_artifact)
        run.log(
            {
                "profiles/summary": wandb.Table(
                    columns=[
                        "variant",
                        "batch",
                        "global_tokens_per_step",
                        "step_seconds",
                        "trace_path",
                    ],
                    data=[
                        [
                            row["variant"],
                            row["batch"],
                            row["global_tokens_per_step"],
                            row["step_seconds"],
                            row["trace_path"],
                        ]
                        for row in profiles
                    ],
                )
            }
        )

        cache_models = (
            architectures.DENSE_R8_PACKED,
            architectures.DENSE_R4_PACKED,
            architectures.DENSE_R2_PACKED,
            architectures.DENSE_R1_PACKED,
            architectures.RECURRENT3_R8_PACKED,
        )
        cache_tasks = [
            {"kind": "cache-benchmark", "model": name, "variant": name, "batch": batch}
            for name in cache_models
            for batch in (1, 8, 64)
        ]
        filled_cache_tasks, cache_count = fill_full_waves(cache_tasks)
        cache_all = run_tasks(filled_cache_tasks)
        cache_rows = cache_all[:cache_count]
        all_complete(cache_rows, "cache benchmarks")
        cache_path = cells / "cache-benchmarks.json"
        write_json(cache_path, cache_rows)
        cache_artifact = wandb.Artifact(
            f"exp20-cache-benchmarks-{run.id}", type="benchmark"
        )
        cache_artifact.add_file(str(cache_path))
        run.log_artifact(cache_artifact)
        run.log(
            {
                "cache/summary": wandb.Table(
                    columns=[
                        "model",
                        "batch",
                        "cache_mib_per_sequence",
                        "final_group_speedup",
                        "end_to_end_speedup",
                        "peak_allocated_gib",
                    ],
                    data=[
                        [
                            row["model"],
                            row["batch"],
                            row["cache_mib_per_sequence"],
                            row["final_group_speedup"],
                            row["end_to_end_speedup"],
                            row["peak_allocated_gib"],
                        ]
                        for row in cache_rows
                    ],
                )
            }
        )

        # The implementation-only 2x gate is explicit. If packed GEMMs miss it,
        # stop before learning claims: the preregistered next action is a fused
        # Triton forward/backward kernel, not spending on an invalid screen.
        screen: list[dict[str, Any]] = []
        confirmation: list[dict[str, Any]] = []
        summary: dict[str, Any] | None = None
        verdict = "requires-fused-triton"
        if preflight["packed_gemm_two_x_gate"]:
            batches = {
                name: int(selected[name]["batch"])
                for name in (
                    architectures.DENSE_R8_PACKED,
                    architectures.RECURRENT3_R8_PACKED,
                )
            }
            screen_tasks = [
                _fit_task(
                        stage="one-example-lr-screen",
                        model=name,
                        recipe=recipe,
                        seed=SCREEN_SEED,
                        indices=(SAMPLE_INDICES[0],),
                        maximum_steps=SCREEN_STEPS,
                        batch=batches[name],
                        cells=cells,
                        data=data,
                    )
                for name in batches
                for recipe in screen_recipes()
            ]
            filled_screen, screen_count = fill_full_waves(screen_tasks)
            screen = run_tasks(filled_screen)[:screen_count]
            all_complete(screen, "independent LR screen")
            boundary_rounds = 0
            for boundary_round in range(4):
                extensions = tuning_extensions(screen)
                if not extensions:
                    break
                extension_tasks = [
                    _fit_task(
                        stage=f"one-example-lr-boundary-{boundary_round + 1}",
                        model=name,
                        recipe=recipe,
                        seed=SCREEN_SEED,
                        indices=(SAMPLE_INDICES[0],),
                        maximum_steps=SCREEN_STEPS,
                        batch=batches[name],
                        cells=cells,
                        data=data,
                    )
                    for name, recipe in extensions
                ]
                filled_extensions, extension_count = fill_full_waves(extension_tasks)
                extension_rows = run_tasks(filled_extensions)[:extension_count]
                all_complete(extension_rows, "LR boundary expansion")
                screen.extend(extension_rows)
                boundary_rounds = boundary_round + 1
            unresolved_lr_boundary = bool(tuning_extensions(screen))
            screen_path = cells / "one-example-lr-screen.json"
            write_json(
                screen_path,
                {
                    "status": "complete",
                    "boundary_rounds": boundary_rounds,
                    "unresolved_lr_boundary": unresolved_lr_boundary,
                    "rows": screen,
                },
            )
            screen_artifact = wandb.Artifact(
                f"exp20-lr-screen-{run.id}", type="tuning"
            )
            screen_artifact.add_file(str(screen_path))
            run.log_artifact(screen_artifact)
            run.log(
                {
                    "tuning/one_example": wandb.Table(
                        columns=[
                            "model",
                            "optimizer",
                            "body_lr",
                            "auxiliary_lr",
                            "success",
                            "steps_to_nll_0.01",
                            "seconds_to_nll_0.01",
                            "final_nll",
                        ],
                        data=[
                            [
                                row["model"],
                                row["recipe"]["family"],
                                row["recipe"]["body_lr"],
                                row["recipe"]["auxiliary_lr"],
                                row["success"],
                                (
                                    row["threshold_hits"]["nll_le_0.01"] or {}
                                ).get("step"),
                                (
                                    row["threshold_hits"]["nll_le_0.01"] or {}
                                ).get("elapsed_seconds"),
                                row["final"]["nll"],
                            ]
                            for row in screen
                        ],
                    )
                }
            )
            promoted = promote_recipes(screen)
            confirmation_tasks = [
                _fit_task(
                        stage="two-example-confirmation",
                        model=name,
                        recipe=promoted[name][family],
                        seed=seed,
                        indices=SAMPLE_INDICES,
                        maximum_steps=CONFIRMATION_STEPS,
                        batch=batches[name],
                        cells=cells,
                        data=data,
                    )
                for name in batches
                for family in ("adamw", "muon")
                for seed in CONFIRMATION_SEEDS
            ]
            filled_confirmation, confirmation_count = fill_full_waves(
                confirmation_tasks
            )
            confirmation = run_tasks(filled_confirmation)[:confirmation_count]
            all_complete(confirmation, "two-example confirmation")
            summary = confirmation_summary(confirmation)
            summary["tuning"] = {
                "boundary_rounds": boundary_rounds,
                "unresolved_lr_boundary": unresolved_lr_boundary,
            }
            confirmation_path = cells / "two-example-confirmation.json"
            write_json(
                confirmation_path,
                {"status": "complete", "summary": summary, "rows": confirmation},
            )
            confirmation_artifact = wandb.Artifact(
                f"exp20-two-example-confirmation-{run.id}", type="confirmation"
            )
            confirmation_artifact.add_file(str(confirmation_path))
            run.log_artifact(confirmation_artifact)
            run.log(
                {
                    "confirmation/two_example": wandb.Table(
                        columns=[
                            "model",
                            "optimizer",
                            "seed",
                            "steps_to_nll_0.01",
                            "seconds_to_nll_0.01",
                            "physical_tokens_per_second",
                        ],
                        data=[
                            [
                                row["model"],
                                row["recipe"]["family"],
                                row["seed"],
                                (
                                    row["threshold_hits"]["nll_le_0.01"] or {}
                                ).get("step"),
                                (
                                    row["threshold_hits"]["nll_le_0.01"] or {}
                                ).get("elapsed_seconds"),
                                row["performance"]["physical_tokens_per_second"],
                            ]
                            for row in confirmation
                        ],
                    )
                }
            )
            verdict = (
                "recurrent-promoted"
                if summary["promotion"]["pass"] and not unresolved_lr_boundary
                else "recurrent-not-promoted"
            )

        result = {
            "schema": SCHEMA,
            "status": "complete",
            "verdict": verdict,
            "wandb_url": run.url,
            "preflight": preflight,
            "operator_profiles": profiles,
            "cache_benchmarks": cache_rows,
            "screen": screen,
            "confirmation": confirmation,
            "confirmation_summary": summary,
            "stopped_before_corpus": True,
            "ordinary_autoregressive_claim": False,
        }
        write_json(output_path, result)
        result_artifact = wandb.Artifact(
            f"exp20-result-{run.id}", type="result", metadata={"verdict": verdict}
        )
        result_artifact.add_file(str(output_path), name="result.json")
        run.log_artifact(result_artifact)
        run.summary.update(
            {
                "verdict": verdict,
                "dense_r8_packed_over_reference_speedup": preflight[
                    "dense_r8_packed_over_reference_speedup"
                ],
                "packed_gemm_two_x_gate": preflight["packed_gemm_two_x_gate"],
            }
        )
        run.finish()
        stop.set()
        if heartbeat_thread:
            heartbeat_thread.join(timeout=2)
        return result
    except Exception as error:
        stop.set()
        if heartbeat_thread:
            heartbeat_thread.join(timeout=2)
        write_json(
            output_path,
            {
                "schema": SCHEMA,
                "status": "failed",
                "wandb_url": run.url,
                "failure": f"{type(error).__name__}: {str(error)[:4000]}",
            },
        )
        run.finish(exit_code=1)
        raise


def main() -> None:
    parser = argparse.ArgumentParser(description="Run Exp20 outer-cache exploration")
    parser.add_argument("--output", required=True)
    parser.add_argument("--data-root", required=True)
    parser.add_argument("--heartbeat")
    arguments = parser.parse_args()
    result = run_campaign(
        arguments.output,
        data_root=arguments.data_root,
        heartbeat=arguments.heartbeat,
    )
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)


if __name__ == "__main__":
    main()
