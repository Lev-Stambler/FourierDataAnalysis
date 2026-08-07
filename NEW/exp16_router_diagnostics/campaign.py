"""Eight-H100 router-learning diagnostic with conditional confirmation."""

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
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Any, Iterable

import numpy as np
import torch
import torch.nn.functional as F
from fla.modules import FusedLinearCrossEntropyLoss

import exp15_birouted_kronecker.campaign as exp15
from exp11_kronecker_debug.lm import batch_indices
from exp14_block_kronecker.campaign import GpuSampler, compile_hidden, optimizer_is_finite
from exp14_block_kronecker.data import CONTEXT_LENGTH, evaluate, load_windows
from exp15_birouted_kronecker.model import (
    BI_DECOUPLED_R8,
    BI_R8,
    CURRENT_R8,
    DENSE_WORKSPACE_R8,
    LanguageModel,
    build_model,
    rms_norm,
)


SCHEMA = "exp16-router-diagnostics-v1"
GPU_COUNT = 8
EXECUTION_MODE = "default"
SCREEN_SEED = 23
SCREEN_TOKENS = 10_000_000
FINAL_SEEDS = (24, 25, 26, 27)
FINAL_TOKENS = 40_000_000
BASE_LR = 0.006
MINIMUM_SCREEN_LOSS_WIN = 0.005
MINIMUM_ABLATION_EFFECT = 0.002
MINIMUM_GATE_STD = 0.02
MINIMUM_FINAL_WIN = 0.01
MINIMUM_UTILIZATION = 85.0


@dataclass(frozen=True)
class Track:
    name: str
    model: str
    router_lr_multiplier: float
    batch: int

    def validate(self) -> None:
        if self.model not in {
            CURRENT_R8,
            DENSE_WORKSPACE_R8,
            BI_R8,
            BI_DECOUPLED_R8,
        }:
            raise ValueError(f"unsupported diagnostic model: {self.model}")
        if self.router_lr_multiplier < 0 or self.batch <= 0:
            raise ValueError("invalid track hyperparameters")
        if self.batch * CONTEXT_LENGTH < 100_000:
            raise ValueError("track violates the physical token-batch floor")


TRACKS = (
    Track("current-router-1x", CURRENT_R8, 1.0, 640),
    Track("dense-router-1x", DENSE_WORKSPACE_R8, 1.0, 640),
    Track("bidec-router-0x", BI_DECOUPLED_R8, 0.0, 512),
    Track("bidec-router-1x", BI_DECOUPLED_R8, 1.0, 512),
    Track("bidec-router-3x", BI_DECOUPLED_R8, 3.0, 512),
    Track("bidec-router-10x", BI_DECOUPLED_R8, 10.0, 512),
    Track("bidec-router-30x", BI_DECOUPLED_R8, 30.0, 512),
    Track("bi-router-10x", BI_R8, 10.0, 512),
)
TRACK_BY_NAME = {track.name: track for track in TRACKS}
for _track in TRACKS:
    _track.validate()


def write_json(path: str | Path, value: Any) -> None:
    exp15.write_json(path, value)


def is_router_parameter(name: str) -> bool:
    return ".source_router." in name or ".destination_router." in name


def parameter_categories(
    model: LanguageModel,
) -> dict[str, list[torch.nn.Parameter]]:
    categories: dict[str, list[torch.nn.Parameter]] = {
        "router": [],
        "token_factors": [],
        "channel_factors": [],
        "ffn": [],
        "other": [],
    }
    for name, parameter in model.named_parameters():
        if is_router_parameter(name):
            category = "router"
        elif any(marker in name for marker in (".outer.", ".workspace")):
            category = "token_factors"
        elif any(marker in name for marker in (".channel1.", ".channel2.")):
            category = "channel_factors"
        elif ".ffn." in name:
            category = "ffn"
        else:
            category = "other"
        categories[category].append(parameter)
    if not categories["router"]:
        raise RuntimeError("diagnostic track has no router parameters")
    return categories


def create_optimizer(
    model: LanguageModel, track: Track
) -> tuple[torch.optim.AdamW, dict[str, Any]]:
    groups: list[dict[str, Any]] = []
    routed_ids: set[int] = set()
    inventory: dict[str, Any] = {}
    for role, router in (("body", False), ("router", True)):
        selected = [
            (name, parameter)
            for name, parameter in model.named_parameters()
            if is_router_parameter(name) is router
        ]
        decay = [parameter for _, parameter in selected if parameter.ndim >= 2]
        no_decay = [parameter for _, parameter in selected if parameter.ndim < 2]
        lr = BASE_LR * (track.router_lr_multiplier if router else 1.0)
        if decay:
            groups.append(
                {"params": decay, "weight_decay": 0.01, "lr": lr, "role": role}
            )
        if no_decay:
            groups.append(
                {"params": no_decay, "weight_decay": 0.0, "lr": lr, "role": role}
            )
        inventory[f"{role}_parameter_names"] = [name for name, _ in selected]
        inventory[f"{role}_parameters"] = sum(item.numel() for item in decay + no_decay)
        routed_ids.update(id(item) for item in decay + no_decay)
    parameters = list(model.parameters())
    if len(routed_ids) != len(parameters) or routed_ids != {id(item) for item in parameters}:
        raise RuntimeError("router optimizer routing is incomplete or overlapping")
    optimizer = torch.optim.AdamW(groups, betas=(0.9, 0.95))
    return optimizer, {
        "family": "adamw-router-multiplier",
        "base_lr": BASE_LR,
        "router_lr_multiplier": track.router_lr_multiplier,
        **inventory,
    }


def schedule_multiplier(tokens_seen: int, horizon: int) -> float:
    warmup = 2_000_000
    if tokens_seen <= warmup:
        return max(tokens_seen, 1) / warmup
    progress = min(1.0, (tokens_seen - warmup) / max(1, horizon - warmup))
    cosine = 0.5 * (1.0 + math.cos(math.pi * progress))
    return 0.1 + 0.9 * cosine


def set_learning_rates(
    optimizer: torch.optim.AdamW, track: Track, tokens_seen: int, horizon: int
) -> tuple[float, float]:
    multiplier = schedule_multiplier(tokens_seen, horizon)
    body_lr = BASE_LR * multiplier
    router_lr = body_lr * track.router_lr_multiplier
    for group in optimizer.param_groups:
        group["lr"] = router_lr if group["role"] == "router" else body_lr
    return body_lr, router_lr


def gradient_norm(parameters: Iterable[torch.nn.Parameter]) -> float:
    total = torch.zeros((), dtype=torch.float32)
    device: torch.device | None = None
    for parameter in parameters:
        if parameter.grad is not None:
            if device is None:
                device = parameter.grad.device
                total = total.to(device)
            total = total + parameter.grad.detach().float().square().sum()
    return float(total.sqrt())


def summarize_values(values: list[float]) -> dict[str, float | int]:
    return {
        "samples": len(values),
        "mean": statistics.fmean(values) if values else 0.0,
        "median": statistics.median(values) if values else 0.0,
        "maximum": max(values, default=0.0),
    }


@torch.inference_mode()
def router_telemetry(
    model: LanguageModel, inputs: torch.Tensor
) -> dict[str, Any]:
    value = F.embedding(inputs, model.vocabulary)
    per_layer: list[dict[str, Any]] = []
    path_correlations: list[float] = []
    capture = {0, 7, 15, 23, len(model.blocks) - 1}
    for index, block in enumerate(model.blocks):
        row: dict[str, Any] = {"layer": index}
        normalized = rms_norm(value)
        for side in ("source", "destination"):
            router = getattr(block, f"{side}_router", None)
            if router is None:
                continue
            gates = block._gate(router, value).float()
            row[f"{side}_gate_mean"] = float(gates.mean())
            row[f"{side}_gate_std"] = float(gates.std())
            row[f"{side}_gate_min"] = float(gates.min())
            row[f"{side}_gate_max"] = float(gates.max())
            row[f"{side}_gate_saturation_fraction"] = float(
                ((gates < 0.05) | (gates > 1.95)).float().mean()
            )
            row[f"{side}_router_weight_rms"] = float(
                router.weight.float().square().mean().sqrt()
            )
        branch = block.branch(value)
        row["mixer_update_to_state_rms"] = float(
            (block.mixer_gain * branch).float().square().mean().sqrt()
            / value.float().square().mean().sqrt().clamp_min(1e-12)
        )
        if index in capture:
            ranked = block.rank_outputs(value[:2]).float().flatten(2)
            ranked = F.normalize(ranked, dim=-1)
            correlation = torch.einsum("brd,bsd->brs", ranked, ranked).abs()
            rank = correlation.shape[-1]
            off_diagonal = ~torch.eye(rank, dtype=torch.bool, device=value.device)
            score = float(correlation[:, off_diagonal].mean())
            row["absolute_path_cosine"] = score
            path_correlations.append(score)
        per_layer.append(row)
        value = block(value)

    def side_summary(side: str) -> dict[str, float]:
        rows = [row for row in per_layer if f"{side}_gate_std" in row]
        return {
            "mean_gate_mean": statistics.fmean(
                float(row[f"{side}_gate_mean"]) for row in rows
            ) if rows else 1.0,
            "mean_gate_std": statistics.fmean(
                float(row[f"{side}_gate_std"]) for row in rows
            ) if rows else 0.0,
            "maximum_layer_gate_std": max(
                (float(row[f"{side}_gate_std"]) for row in rows), default=0.0
            ),
            "mean_saturation_fraction": statistics.fmean(
                float(row[f"{side}_gate_saturation_fraction"]) for row in rows
            ) if rows else 0.0,
            "mean_router_weight_rms": statistics.fmean(
                float(row[f"{side}_router_weight_rms"]) for row in rows
            ) if rows else 0.0,
        }

    return {
        "source": side_summary("source"),
        "destination": side_summary("destination"),
        "mean_mixer_update_to_state_rms": statistics.fmean(
            float(row["mixer_update_to_state_rms"]) for row in per_layer
        ),
        "mean_absolute_path_cosine": statistics.fmean(path_correlations),
        "per_layer": per_layer,
    }


def router_ablation_evaluations(
    model: LanguageModel,
    validation: np.ndarray,
    batch: int,
    device: torch.device,
) -> dict[str, Any]:
    subset = validation[: min(257, len(validation))]
    normal = evaluate(model, subset, min(batch, 16), device)
    routers = {
        side: [
            getattr(block, f"{side}_router")
            for block in model.blocks
            if hasattr(block, f"{side}_router")
        ]
        for side in ("source", "destination")
    }
    snapshots = {
        side: [router.weight.detach().clone() for router in values]
        for side, values in routers.items()
    }

    def zero(side: str) -> None:
        with torch.no_grad():
            for router in routers[side]:
                router.weight.zero_()

    def restore() -> None:
        with torch.no_grad():
            for side, values in routers.items():
                for router, weight in zip(values, snapshots[side], strict=True):
                    router.weight.copy_(weight)

    variants: dict[str, Any] = {"normal": normal}
    try:
        if routers["source"]:
            zero("source")
            variants["source_neutral"] = evaluate(
                model, subset, min(batch, 16), device
            )
            restore()
        if routers["destination"]:
            zero("destination")
            variants["destination_neutral"] = evaluate(
                model, subset, min(batch, 16), device
            )
            restore()
        if routers["source"] and routers["destination"]:
            zero("source")
            zero("destination")
            variants["both_neutral"] = evaluate(
                model, subset, min(batch, 16), device
            )
    finally:
        restore()
    normal_nll = float(normal["nll"])
    return {
        "evaluations": variants,
        "neutral_minus_normal_nll": {
            name: float(row["nll"]) - normal_nll
            for name, row in variants.items()
            if name != "normal"
        },
    }


def train_track(task: dict[str, Any], device: torch.device) -> dict[str, Any]:
    track = Track(**task["track"])
    track.validate()
    seed = int(task["seed"])
    target_tokens = int(task["target_tokens"])
    output_root = Path(task["output_root"])
    result_path = (
        output_root
        / "cells"
        / track.name
        / f"seed-{seed}-tokens-{target_tokens}.json"
    )
    if result_path.is_file():
        prior = json.loads(result_path.read_text())
        if prior.get("status") == "complete":
            return prior
    exp15._activate_base()
    train_windows = load_windows(task["data_root"], "train")
    validation_windows = load_windows(task["data_root"], "validation")
    torch.manual_seed(seed)
    torch.cuda.empty_cache()
    torch.cuda.reset_peak_memory_stats(device)
    model = build_model(track.model).to(device)
    hidden_function = compile_hidden(model, EXECUTION_MODE)
    optimizer, optimizer_inventory = create_optimizer(model, track)
    categories = parameter_categories(model)
    loss_function = FusedLinearCrossEntropyLoss()
    steps = math.ceil(target_tokens / (track.batch * CONTEXT_LENGTH))
    timed_seconds = 0.0
    timed_tokens = 0
    clipped = 0
    gradients = {name: [] for name in categories}
    last: dict[str, Any] = {}
    with GpuSampler(device) as sampler:
        for step in range(steps):
            indices = batch_indices(len(train_windows) - 1, step, track.batch, seed)
            inputs, targets = exp15.gpu_cached_block_batch(
                train_windows, indices, device
            )
            optimizer.zero_grad(set_to_none=True)
            tokens_seen = (step + 1) * track.batch * CONTEXT_LENGTH
            body_lr, router_lr = set_learning_rates(
                optimizer, track, tokens_seen, FINAL_TOKENS
            )
            torch.cuda.synchronize(device)
            started = time.perf_counter()
            with torch.autocast("cuda", dtype=torch.bfloat16):
                hidden = hidden_function(inputs).contiguous()
                loss = loss_function(
                    hidden, targets.contiguous(), model.vocabulary
                )
            loss.backward()
            if step % 4 == 0 or step + 1 == steps:
                for name, parameters in categories.items():
                    gradients[name].append(gradient_norm(parameters))
            norm = torch.nn.utils.clip_grad_norm_(model.parameters(), 1.0)
            if not torch.isfinite(loss) or not torch.isfinite(norm):
                raise RuntimeError("non-finite diagnostic forward/backward")
            clipped += int(float(norm.detach()) > 1.0)
            optimizer.step()
            if not optimizer_is_finite(optimizer):
                raise RuntimeError("non-finite diagnostic optimizer state")
            torch.cuda.synchronize(device)
            duration = time.perf_counter() - started
            if step:
                timed_seconds += duration
                timed_tokens += track.batch * CONTEXT_LENGTH
            last = {
                "step": step + 1,
                "tokens_seen": tokens_seen,
                "train_block_nll": float(loss.detach()),
                "global_grad_norm": float(norm.detach()),
                "body_lr": body_lr,
                "router_lr": router_lr,
            }
    validation_limit = min(513, len(validation_windows))
    validation = evaluate(
        model,
        validation_windows[:validation_limit],
        min(track.batch, 16),
        device,
    )
    telemetry_inputs, _ = exp15.gpu_cached_block_batch(
        validation_windows,
        np.arange(min(8, len(validation_windows) - 1)),
        device,
    )
    telemetry = router_telemetry(model, telemetry_inputs)
    ablations = router_ablation_evaluations(
        model, validation_windows, track.batch, device
    )
    actual_tokens = steps * track.batch * CONTEXT_LENGTH
    result = {
        "schema": "exp16-router-training-cell-v1",
        "status": "complete",
        "track": asdict(track),
        "model": track.model,
        "seed": seed,
        "target_tokens": target_tokens,
        "actual_tokens": actual_tokens,
        "steps": steps,
        "batch": track.batch,
        "global_examples_per_step": track.batch,
        "global_tokens_per_step": track.batch * CONTEXT_LENGTH,
        "gradient_accumulation": 1,
        "validation": validation,
        "router_telemetry": telemetry,
        "router_ablations": ablations,
        "gradient_norms": {
            name: summarize_values(values) for name, values in gradients.items()
        },
        "optimizer_routing": optimizer_inventory,
        "performance": {
            **last,
            "tokens_per_second": timed_tokens / max(timed_seconds, 1e-12),
            "clip_fraction": clipped / steps,
            "peak_allocated_gib": torch.cuda.max_memory_allocated(device) / 2**30,
            "peak_reserved_gib": torch.cuda.max_memory_reserved(device) / 2**30,
            **sampler.summary(),
        },
        "execution_mode": EXECUTION_MODE,
        "loss_implementation": "fla-fused-linear-cross-entropy-exact",
    }
    write_json(result_path, result)
    del optimizer, model, loss_function
    gc.collect()
    torch.cuda.empty_cache()
    return result


def worker_loop(gpu_id: int, tasks: mp.Queue, results: mp.Queue) -> None:
    torch.cuda.set_device(gpu_id)
    device = torch.device(f"cuda:{gpu_id}")
    while True:
        item = tasks.get()
        if item is None:
            return
        index, task = item
        try:
            result = train_track(task, device)
        except Exception as error:
            result = {
                "status": "failed",
                "track": task.get("track", {}),
                "seed": task.get("seed"),
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


def run_tasks(tasks: list[dict[str, Any]]) -> list[dict[str, Any]]:
    context = mp.get_context("spawn")
    task_queue, result_queue = context.Queue(), context.Queue()
    count = min(GPU_COUNT, len(tasks))
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
        raise TimeoutError("Exp16 worker timed out") from error
    finally:
        for process in processes:
            process.join(timeout=10)
            if process.is_alive():
                process.terminate()
    if any(row is None for row in rows):
        raise RuntimeError("missing Exp16 result")
    return [row for row in rows if row is not None]


def _all_complete(rows: Iterable[dict[str, Any]], label: str) -> None:
    failures = [row for row in rows if row.get("status") != "complete"]
    if failures:
        raise RuntimeError(f"{label} failed: {failures}")


def paid_preflight(data: Path) -> dict[str, Any]:
    if torch.cuda.device_count() != GPU_COUNT:
        raise RuntimeError("Exp16 requires exactly eight visible GPUs")
    gpu_names = [torch.cuda.get_device_name(index) for index in range(GPU_COUNT)]
    if any("H100" not in name for name in gpu_names):
        raise RuntimeError(f"Exp16 requires 8xH100; found {gpu_names}")
    exp15._activate_base()
    agreement_tasks = [
        {
            "kind": "loss-agreement",
            "model": track.model,
            "data_root": str(data),
            "execution_mode": EXECUTION_MODE,
        }
        for track in TRACKS
    ]
    agreements = exp15.run_tasks(agreement_tasks)
    _all_complete(agreements, "loss agreement")
    recipe = exp15.Recipe("adamw", BASE_LR, BASE_LR, "warmup-cosine")
    benchmark_tasks = [
        {
            "kind": "benchmark",
            "model": track.model,
            "recipe": asdict(recipe),
            "batch": track.batch,
            "measured_steps": 10,
            "data_root": str(data),
            "execution_mode": EXECUTION_MODE,
        }
        for track in TRACKS
    ]
    benchmarks = exp15.run_tasks(benchmark_tasks)
    _all_complete(benchmarks, "full-node selected-track benchmarks")
    invalid = [
        row
        for row in benchmarks
        if int(row.get("global_tokens_per_step", 0)) < 100_000
        or float(row.get("median_gpu_utilization_percent", 0))
        < MINIMUM_UTILIZATION
        or row.get("finite_forward_backward_optimizer") is not True
    ]
    if invalid:
        raise RuntimeError(f"Exp16 utilization/finite-state gate failed: {invalid}")
    return {
        "status": "pass",
        "gpu_names": gpu_names,
        "gpu_count": GPU_COUNT,
        "loss_agreement": dict(
            zip((track.name for track in TRACKS), agreements, strict=True)
        ),
        "benchmarks": dict(
            zip((track.name for track in TRACKS), benchmarks, strict=True)
        ),
        "minimum_utilization_percent": MINIMUM_UTILIZATION,
        "gradient_accumulation": 1,
        "input_pipeline": "immutable-int32-corpus-on-each-gpu_indexed-gather",
    }


def task_for(
    track: Track,
    seed: int,
    tokens: int,
    cells: Path,
    data: Path,
) -> dict[str, Any]:
    return {
        "track": asdict(track),
        "seed": seed,
        "target_tokens": tokens,
        "output_root": str(cells),
        "data_root": str(data),
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

    data = Path(data_root)
    output_path = Path(output)
    cells = output_path.parent / "router-diagnostic-cells"
    load_windows(data, "train")
    load_windows(data, "validation")
    wandb.login(key=os.environ["WANDB_API_KEY"], verify=True)
    run = wandb.init(
        project="exp16-router-diagnostics",
        name="exp16-router-learning-causal-use",
        config={
            "schema": SCHEMA,
            "tracks": [asdict(track) for track in TRACKS],
            "screen_tokens": SCREEN_TOKENS,
            "final_tokens": FINAL_TOKENS,
            "screen_loss_gate": MINIMUM_SCREEN_LOSS_WIN,
            "ablation_gate": MINIMUM_ABLATION_EFFECT,
            "gate_std_gate": MINIMUM_GATE_STD,
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
            telemetry = row.get("router_telemetry", {})
            ablation = row.get("router_ablations", {}).get(
                "neutral_minus_normal_nll", {}
            )
            run.log(
                {
                    f"{stage}/track": row.get("track", {}).get("name", ""),
                    f"{stage}/validation_block_nll": row.get("validation", {}).get("nll", float("nan")),
                    f"{stage}/source_gate_std": telemetry.get("source", {}).get("mean_gate_std", 0),
                    f"{stage}/destination_gate_std": telemetry.get("destination", {}).get("mean_gate_std", 0),
                    f"{stage}/both_neutral_minus_normal_nll": ablation.get("both_neutral", ablation.get("destination_neutral", 0)),
                    f"{stage}/router_grad_norm": row.get("gradient_norms", {}).get("router", {}).get("mean", 0),
                    f"{stage}/tokens_per_second": row.get("performance", {}).get("tokens_per_second", 0),
                },
                step=log_step,
            )
        if heartbeat_path:
            heartbeat_path.touch()

    try:
        preflight = paid_preflight(data)
        screen = run_tasks(
            [task_for(track, SCREEN_SEED, SCREEN_TOKENS, cells, data) for track in TRACKS]
        )
        _all_complete(screen, "router diagnostic screen")
        publish("screen", screen)
        by_track = {row["track"]["name"]: row for row in screen}
        current_nll = float(by_track["current-router-1x"]["validation"]["nll"])
        bidec = [row for row in screen if row["model"] == BI_DECOUPLED_R8]
        best = min(bidec, key=lambda row: float(row["validation"]["nll"]))
        best_name = str(best["track"]["name"])
        loss_delta = float(best["validation"]["nll"]) - current_nll
        ablation_deltas = best["router_ablations"]["neutral_minus_normal_nll"]
        causal_delta = float(
            ablation_deltas.get(
                "both_neutral", ablation_deltas.get("destination_neutral", 0.0)
            )
        )
        gate_std = max(
            float(best["router_telemetry"]["source"]["mean_gate_std"]),
            float(best["router_telemetry"]["destination"]["mean_gate_std"]),
        )
        advance = (
            loss_delta <= -MINIMUM_SCREEN_LOSS_WIN
            and causal_delta >= MINIMUM_ABLATION_EFFECT
            and gate_std >= MINIMUM_GATE_STD
        )
        common: dict[str, Any] = {
            "schema": SCHEMA,
            "status": "complete",
            "wandb_url": run.url,
            "preflight": preflight,
            "screen": screen,
            "best_bidec_track": best_name,
            "best_bidec_minus_current_nll": loss_delta,
            "best_router_neutral_minus_normal_nll": causal_delta,
            "best_router_gate_std": gate_std,
            "screen_gates": {
                "minimum_loss_win": MINIMUM_SCREEN_LOSS_WIN,
                "minimum_ablation_effect": MINIMUM_ABLATION_EFFECT,
                "minimum_gate_std": MINIMUM_GATE_STD,
            },
            "cloud_only_training": True,
            "gpu_count": GPU_COUNT,
        }
        if not advance:
            result = {
                **common,
                "verdict": "stop_router_not_causally_useful",
                "final": [],
            }
        else:
            winning_track = Track(**best["track"])
            current_track = TRACK_BY_NAME["current-router-1x"]
            final = run_tasks(
                [
                    task_for(track, seed, FINAL_TOKENS, cells, data)
                    for track in (current_track, winning_track)
                    for seed in FINAL_SEEDS
                ]
            )
            _all_complete(final, "paired router confirmation")
            publish("final", final)
            nll = {
                track.name: {
                    int(row["seed"]): float(row["validation"]["nll"])
                    for row in final
                    if row["track"]["name"] == track.name
                }
                for track in (current_track, winning_track)
            }
            paired = [
                nll[winning_track.name][seed] - nll[current_track.name][seed]
                for seed in FINAL_SEEDS
            ]
            mean_delta = statistics.fmean(paired)
            win = all(value < 0 for value in paired) and mean_delta <= -MINIMUM_FINAL_WIN
            result = {
                **common,
                "verdict": "promote_router_configuration" if win else "router_signal_did_not_confirm",
                "final": final,
                "paired_winner_minus_current_nll": paired,
                "mean_winner_minus_current_nll": mean_delta,
                "minimum_final_win": MINIMUM_FINAL_WIN,
                "all_four_winner_wins": all(value < 0 for value in paired),
            }
        write_json(output_path, result)
        run.summary.update(
            {
                "verdict": result["verdict"],
                "best_bidec_track": best_name,
                "best_bidec_minus_current_nll": loss_delta,
                "best_router_neutral_minus_normal_nll": causal_delta,
                "best_router_gate_std": gate_std,
                "mean_winner_minus_current_nll": result.get("mean_winner_minus_current_nll"),
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
    parser = argparse.ArgumentParser(description="Run Exp16 router diagnostics")
    parser.add_argument("--output", required=True)
    parser.add_argument("--data-root", required=True)
    parser.add_argument("--heartbeat")
    args = parser.parse_args()
    result = run_campaign(args.output, data_root=args.data_root, heartbeat=args.heartbeat)
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)


if __name__ == "__main__":
    main()
