"""Eight-GPU, token-matched Muon comparison for the corrected ExpV2-2 tasks.

The sweep deliberately treats learning rate as part of each architecture's
recipe.  Four Transformer and four deep rank-one Kronecker cells run in
parallel, after an architecture-specific physical-batch sweep.  The selected
recipes are then evaluated on four fresh paired initialization seeds.
"""

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

import torch

from expv2.exp1.model import build_model, model_inventory
from expv2.exp1.utils import atomic_json, finite_tree, seed_everything

from .budget import CalibrationBudget
from .config import ID_ACCURACY_THRESHOLD
from .synthetic import make_batch, masked_loss_and_accuracy
from .training import CalibrationRecipe, _optimizer, train_cell
from .two_hop_debug import DebugKind, _evaluate, _train_mixed_phase


SCHEMA = "expv2-2-matched-muon-comparison-v1"
CELL_SCHEMA = "expv2-2-matched-muon-cell-v1"
WANDB_PROJECT = "expv2-2-matched-muon"
GPU_COUNT = 8
CONTEXT_LENGTH = 128
VARIANTS = ("transformer", "kron-r1-d66")
LEARNING_RATES = (0.01, 0.03, 0.10, 0.30)
CONFIRMATION_SEEDS = (11, 22, 33, 44)
# This is exactly the context budget used by the valid AdamW Transformer
# control: 10,240 physical contexts x 1,000 updates per phase.
TARGET_CONTEXTS_PER_PHASE = 10_240_000
MINIMUM_UPDATES = 1_000
BATCH_CANDIDATES = (
    16_384,
    12_288,
    10_240,
    8_192,
    6_144,
    4_096,
    3_072,
    2_048,
    1_024,
)
UNDERFILLED_BATCH = 128
MINIMUM_UTILIZATION = 85.0
MAXIMUM_BF16_LOSS_RELATIVE_ERROR = 0.02


def _slug(value: float) -> str:
    return f"{value:.6g}".replace(".", "p")


def _budget(
    batch_contexts: int,
    target_contexts: int = TARGET_CONTEXTS_PER_PHASE,
) -> CalibrationBudget:
    return CalibrationBudget(
        batch_contexts=batch_contexts,
        optimizer_updates=MINIMUM_UPDATES,
        minimum_contexts=target_contexts,
    )


def _mean(values: Iterable[float]) -> float:
    rows = list(values)
    return statistics.fmean(rows) if rows else 0.0


class NodeSampler:
    """Collect per-GPU utilization, power, and memory without hiding outliers."""

    def __init__(self, interval: float = 0.1) -> None:
        self.interval = interval
        self.rows: dict[int, list[tuple[float, float, float]]] = {
            index: [] for index in range(GPU_COUNT)
        }
        self.stop = threading.Event()
        self.thread: threading.Thread | None = None

    def _sample(self) -> None:
        while not self.stop.is_set():
            try:
                output = subprocess.check_output(
                    [
                        "nvidia-smi",
                        "--query-gpu=index,utilization.gpu,power.draw,memory.used",
                        "--format=csv,noheader,nounits",
                    ],
                    text=True,
                    timeout=3,
                )
                for line in output.splitlines():
                    values = [item.strip() for item in line.split(",")]
                    if len(values) != 4:
                        continue
                    index = int(values[0])
                    if index in self.rows:
                        self.rows[index].append(tuple(float(item) for item in values[1:]))
            except (OSError, subprocess.SubprocessError, ValueError):
                pass
            self.stop.wait(self.interval)

    def __enter__(self) -> "NodeSampler":
        self.thread = threading.Thread(target=self._sample, daemon=True)
        self.thread.start()
        return self

    def __exit__(self, *_: Any) -> None:
        self.stop.set()
        if self.thread is not None:
            self.thread.join(timeout=3)

    def summary(self) -> dict[str, Any]:
        per_gpu: dict[str, Any] = {}
        for index, rows in self.rows.items():
            per_gpu[str(index)] = {
                "samples": len(rows),
                "median_gpu_utilization_percent": (
                    statistics.median(row[0] for row in rows) if rows else 0.0
                ),
                "median_power_watts": (
                    statistics.median(row[1] for row in rows) if rows else 0.0
                ),
                "peak_nvidia_memory_mib": max((row[2] for row in rows), default=0.0),
            }
        return {
            "per_gpu": per_gpu,
            "minimum_median_gpu_utilization_percent": min(
                row["median_gpu_utilization_percent"] for row in per_gpu.values()
            ),
            "mean_median_gpu_utilization_percent": _mean(
                float(row["median_gpu_utilization_percent"])
                for row in per_gpu.values()
            ),
            "mean_median_power_watts": _mean(
                float(row["median_power_watts"]) for row in per_gpu.values()
            ),
        }


class DeviceSampler:
    """Sample one physical GPU strictly inside a timed benchmark region."""

    def __init__(self, gpu_index: int, interval: float = 0.05) -> None:
        self.gpu_index = int(gpu_index)
        self.interval = interval
        self.rows: list[tuple[float, float, float]] = []
        self.stop = threading.Event()
        self.thread: threading.Thread | None = None

    def _sample(self) -> None:
        while not self.stop.is_set():
            try:
                output = subprocess.check_output(
                    [
                        "nvidia-smi",
                        "-i",
                        str(self.gpu_index),
                        "--query-gpu=utilization.gpu,power.draw,memory.used",
                        "--format=csv,noheader,nounits",
                    ],
                    text=True,
                    timeout=3,
                )
                values = tuple(float(item.strip()) for item in output.strip().split(","))
                if len(values) == 3:
                    self.rows.append(values)  # type: ignore[arg-type]
            except (OSError, subprocess.SubprocessError, ValueError):
                pass
            self.stop.wait(self.interval)

    def __enter__(self) -> "DeviceSampler":
        self.thread = threading.Thread(target=self._sample, daemon=True)
        self.thread.start()
        return self

    def __exit__(self, *_: Any) -> None:
        self.stop.set()
        if self.thread is not None:
            self.thread.join(timeout=3)

    def summary(self) -> dict[str, float | int]:
        return {
            "samples": len(self.rows),
            "median_gpu_utilization_percent": (
                statistics.median(row[0] for row in self.rows) if self.rows else 0.0
            ),
            "median_power_watts": (
                statistics.median(row[1] for row in self.rows) if self.rows else 0.0
            ),
            "peak_nvidia_memory_mib": max((row[2] for row in self.rows), default=0.0),
        }


def _benchmark(
    variant: str,
    batch_contexts: int,
    device: torch.device,
    *,
    measured_updates: int = 8,
) -> dict[str, Any]:
    seed_everything(411)
    torch.cuda.empty_cache()
    torch.cuda.reset_peak_memory_stats(device)
    model = build_model(variant, vocab_size=128).to(device=device, dtype=torch.bfloat16)
    recipe = CalibrationRecipe("muon", 0.03)
    optimizer, routing = _optimizer(model, recipe)

    def update(index: int) -> tuple[float, float]:
        batch = make_batch(
            "associative-recall",
            batch_contexts,
            seed=880_000 + index,
            split="train",
            device=device,
        )
        optimizer.zero_grad(set_to_none=True)
        loss, _ = masked_loss_and_accuracy(model(batch.inputs), batch.targets, batch.mask)
        loss.backward()
        norm = torch.nn.utils.clip_grad_norm_(model.parameters(), 1.0)
        optimizer.step()
        return float(loss), float(norm)

    finite = True
    try:
        for index in range(3):
            loss, norm = update(index)
            finite = finite and math.isfinite(loss) and math.isfinite(norm)
        torch.cuda.synchronize(device)
        with DeviceSampler(int(device.index or 0)) as sampler:
            started = time.monotonic()
            for index in range(measured_updates):
                loss, norm = update(index + 3)
                finite = finite and math.isfinite(loss) and math.isfinite(norm)
            torch.cuda.synchronize(device)
        elapsed = time.monotonic() - started
        result = {
            "status": "pass",
            "variant": variant,
            "batch_contexts": batch_contexts,
            "global_token_batch": batch_contexts * CONTEXT_LENGTH,
            "measured_updates": measured_updates,
            "elapsed_seconds": elapsed,
            "tokens_per_second": (
                batch_contexts * CONTEXT_LENGTH * measured_updates / elapsed
            ),
            "peak_allocated_gib": torch.cuda.max_memory_allocated(device) / 2**30,
            "peak_reserved_gib": torch.cuda.max_memory_reserved(device) / 2**30,
            "finite_forward_backward_optimizer": bool(
                finite and finite_tree(optimizer.state)
            ),
            "optimizer_routing": routing,
            "gradient_accumulation_steps": 1,
            **sampler.summary(),
        }
    finally:
        del optimizer, model
        gc.collect()
        torch.cuda.empty_cache()
    return result


@torch.inference_mode()
def _loss_agreement(variant: str, device: torch.device) -> dict[str, Any]:
    seed_everything(991)
    reference = build_model(variant, vocab_size=128).to(device=device, dtype=torch.float32)
    candidate = build_model(variant, vocab_size=128).to(device=device, dtype=torch.bfloat16)
    candidate.load_state_dict(reference.state_dict())
    batch = make_batch(
        "associative-recall", 128, seed=440_001, split="train", device=device
    )
    fp32_loss, _ = masked_loss_and_accuracy(
        reference(batch.inputs), batch.targets, batch.mask
    )
    bf16_loss, _ = masked_loss_and_accuracy(
        candidate(batch.inputs), batch.targets, batch.mask
    )
    fp32, bf16 = float(fp32_loss), float(bf16_loss)
    relative = abs(fp32 - bf16) / max(abs(fp32), 1e-12)
    del reference, candidate
    torch.cuda.empty_cache()
    return {
        "status": "pass" if relative <= MAXIMUM_BF16_LOSS_RELATIVE_ERROR else "fail",
        "variant": variant,
        "fp32_loss": fp32,
        "bf16_loss": bf16,
        "relative_error": relative,
        "maximum_relative_error": MAXIMUM_BF16_LOSS_RELATIVE_ERROR,
    }


def _cell_directory(root: Path, variant: str, lr: float, seed: int, stage: str) -> Path:
    return root / stage / variant / f"lr-{_slug(lr)}" / f"seed-{seed}"


def _train_full_cell(task: dict[str, Any], device: torch.device) -> dict[str, Any]:
    variant, lr, seed = str(task["variant"]), float(task["lr"]), int(task["seed"])
    batch_contexts = int(task["batch_contexts"])
    target_contexts = int(task.get("target_contexts", TARGET_CONTEXTS_PER_PHASE))
    output = _cell_directory(
        Path(task["output_root"]), variant, lr, seed, str(task["stage"])
    )
    output.mkdir(parents=True, exist_ok=True)
    result_path = output / "result.json"
    if result_path.is_file():
        existing = json.loads(result_path.read_text())
        if existing.get("status") == "complete":
            return existing
    key = os.environ.get("WANDB_API_KEY")
    if not key:
        raise RuntimeError("WANDB_API_KEY is required before every paid cell")
    import wandb

    wandb.login(key=key, relogin=True, verify=True)
    run = wandb.init(
        project=WANDB_PROJECT,
        name=f"{task['stage']}-{variant}-muon-lr{lr:g}-seed{seed}",
        job_type=str(task["stage"]),
        dir=str(output),
        reinit="finish_previous",
        config={
            "schema": CELL_SCHEMA,
            "variant": variant,
            "optimizer": "pure-batched-muon",
            "lr": lr,
            "seed": seed,
            "batch_contexts": batch_contexts,
            "global_token_batch": batch_contexts * CONTEXT_LENGTH,
            "target_contexts_per_phase": target_contexts,
            "minimum_updates_per_phase": MINIMUM_UPDATES,
            "gradient_accumulation_steps": 1,
            "cloud_only_training": True,
        },
    )
    wandb_url = str(run.url or run.get_url() or "")
    if not wandb_url.startswith("http"):
        run.finish(exit_code=1)
        raise RuntimeError("W&B did not provide a direct cell URL")
    atomic_json(
        output / "wandb-launch.json",
        {"schema": "expv2-2-wandb-launch-v1", "status": "complete", "wandb_url": wandb_url},
    )
    recipe = CalibrationRecipe("muon", lr)
    budget = _budget(batch_contexts, target_contexts)
    started = time.monotonic()
    try:
        standard = {
            name: train_cell(
                variant,
                name,  # type: ignore[arg-type]
                recipe,
                budget,
                seed=seed,
                output=output / f"{name}.json",
                train_split="train",
                device=device,
                run=run,
            )
            for name in ("delay-copy", "associative-recall")
        }
        seed_everything(seed)
        model = build_model(variant, vocab_size=128).to(
            device=device, dtype=torch.bfloat16
        )
        optimizer, routing = _optimizer(model, recipe)
        schedule: tuple[tuple[str, tuple[DebugKind, ...]], ...] = (
            ("concurrent-edges", ("first-edge", "second-edge")),
            (
                "concurrent-edges-and-two-hop",
                ("first-edge", "second-edge", "two-hop"),
            ),
            ("two-hop-finetune", ("two-hop",)),
        )
        phases = []
        for label, kinds in schedule:
            phases.append(
                _train_mixed_phase(
                    model,
                    optimizer,
                    kinds=kinds,
                    label=label,
                    budget=budget,
                    device=device,
                    run=run,
                    train_split="train",
                    probe_split="id",
                )
            )
            atomic_json(
                output / "two-hop-progress.json",
                {"status": "running", "phases": phases},
            )
        two_hop = {
            split: _evaluate(model, "two-hop", device=device, split=split)
            for split in ("id", "ood-cardinality", "ood-position")
        }
        inventory = model_inventory(model)
        id_accuracies = {
            "delay-copy": standard["delay-copy"]["evaluations"]["id"]["accuracy"],
            "associative-recall": standard["associative-recall"]["evaluations"]["id"]["accuracy"],
            "two-hop-recall": two_hop["id"]["accuracy"],
        }
        ood_accuracies = {
            "delay-copy/ood-composition": standard["delay-copy"]["evaluations"]["ood-composition"]["accuracy"],
            "delay-copy/ood-position": standard["delay-copy"]["evaluations"]["ood-position"]["accuracy"],
            "associative-recall/ood-cardinality": standard["associative-recall"]["evaluations"]["ood-cardinality"]["accuracy"],
            "associative-recall/ood-position": standard["associative-recall"]["evaluations"]["ood-position"]["accuracy"],
            "two-hop-recall/ood-cardinality": two_hop["ood-cardinality"]["accuracy"],
            "two-hop-recall/ood-position": two_hop["ood-position"]["accuracy"],
        }
        id_pass = all(value >= ID_ACCURACY_THRESHOLD for value in id_accuracies.values())
        result = {
            "schema": CELL_SCHEMA,
            "status": "complete",
            "stage": task["stage"],
            "wandb_url": wandb_url,
            "variant": variant,
            "recipe": asdict(recipe),
            "seed": seed,
            "batch_contexts": batch_contexts,
            "global_token_batch": batch_contexts * CONTEXT_LENGTH,
            "budget_per_phase": budget.as_dict(),
            "inventory": inventory,
            "optimizer_routing": routing,
            "standard_tasks": standard,
            "two_hop": {"phases": phases, "evaluations": two_hop},
            "id_accuracies": id_accuracies,
            "ood_accuracies": ood_accuracies,
            "id_pass": id_pass,
            "minimum_id_accuracy": min(id_accuracies.values()),
            "mean_id_accuracy": _mean(id_accuracies.values()),
            "mean_ood_accuracy": _mean(ood_accuracies.values()),
            "finite_training_state": all(
                row["finite_training_state"] for row in standard.values()
            )
            and all(row["finite_training_state"] for row in phases)
            and finite_tree(optimizer.state),
            "elapsed_seconds": time.monotonic() - started,
        }
        atomic_json(output / "two-hop-progress.json", result["two_hop"])
        atomic_json(result_path, result)
        run.summary.update(
            {
                "id_pass": id_pass,
                "minimum_id_accuracy": result["minimum_id_accuracy"],
                "mean_id_accuracy": result["mean_id_accuracy"],
                "mean_ood_accuracy": result["mean_ood_accuracy"],
            }
        )
        for name, value in id_accuracies.items():
            run.summary[f"id_accuracy/{name}"] = value
        for name, value in ood_accuracies.items():
            run.summary[f"ood_accuracy/{name}"] = value
        del optimizer, model
        gc.collect()
        torch.cuda.empty_cache()
        return result
    except Exception as error:
        atomic_json(
            result_path,
            {
                "schema": CELL_SCHEMA,
                "status": "failed",
                "wandb_url": wandb_url,
                "variant": variant,
                "recipe": asdict(recipe),
                "seed": seed,
                "reason": f"{type(error).__name__}: {error}",
                "elapsed_seconds": time.monotonic() - started,
            },
        )
        raise
    finally:
        run.finish()


def _execute_task(task: dict[str, Any], device: torch.device) -> dict[str, Any]:
    if task["kind"] == "benchmark":
        return _benchmark(
            str(task["variant"]),
            int(task["batch_contexts"]),
            device,
            measured_updates=int(task.get("measured_updates", 8)),
        )
    if task["kind"] == "loss-agreement":
        return _loss_agreement(str(task["variant"]), device)
    if task["kind"] == "train":
        return _train_full_cell(task, device)
    raise ValueError(f"unknown matched-comparison task: {task['kind']}")


def _worker_loop(gpu_id: int, tasks: mp.Queue, results: mp.Queue) -> None:
    torch.cuda.set_device(gpu_id)
    device = torch.device(f"cuda:{gpu_id}")
    while True:
        item = tasks.get()
        if item is None:
            return
        index, task = item
        try:
            value = _execute_task(task, device)
        except Exception as error:
            reason = (
                "out_of_memory"
                if isinstance(error, torch.OutOfMemoryError)
                or "out of memory" in str(error).lower()
                else f"{type(error).__name__}: {str(error)[:2000]}"
            )
            value = {
                "status": "failed",
                "kind": task.get("kind"),
                "variant": task.get("variant"),
                "lr": task.get("lr"),
                "seed": task.get("seed"),
                "reason": reason,
            }
            gc.collect()
            torch.cuda.empty_cache()
        results.put((index, value))


def _run_tasks(
    tasks: list[dict[str, Any]],
    *,
    workers: int | None = None,
    timeout_seconds: int = 10_800,
) -> list[dict[str, Any]]:
    if not tasks:
        return []
    count = min(workers or GPU_COUNT, len(tasks))
    context = mp.get_context("spawn")
    task_queue, result_queue = context.Queue(), context.Queue()
    processes = [
        context.Process(target=_worker_loop, args=(index, task_queue, result_queue))
        for index in range(count)
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
            index, value = result_queue.get(timeout=timeout_seconds)
        except queue.Empty as error:
            for process in processes:
                process.terminate()
            raise RuntimeError("matched comparison GPU worker timeout") from error
        values[index] = value
    for process in processes:
        process.join(timeout=30)
        if process.exitcode:
            raise RuntimeError(f"matched comparison worker exited {process.exitcode}")
    return [value for value in values if value is not None]


def _padded_variant_tasks(kind: str, **values: Any) -> list[dict[str, Any]]:
    return [
        {"kind": kind, "variant": VARIANTS[index % len(VARIANTS)], **values}
        for index in range(GPU_COUNT)
    ]


def _hardware_inventory() -> dict[str, Any]:
    if not torch.cuda.is_available() or torch.cuda.device_count() != GPU_COUNT:
        raise RuntimeError(
            f"matched comparison requires exactly {GPU_COUNT} cloud GPUs; "
            f"found {torch.cuda.device_count()}"
        )
    names = [torch.cuda.get_device_name(index) for index in range(GPU_COUNT)]
    if any(not ("H100" in name.upper() or "H200" in name.upper()) for name in names):
        raise RuntimeError(f"requires H100/H200 accelerators, found {names}")
    memory = [
        torch.cuda.get_device_properties(index).total_memory / 2**30
        for index in range(GPU_COUNT)
    ]
    if any(value < 75.0 for value in memory):
        raise RuntimeError(f"requires 80GB-class accelerators, found {memory}")
    return {"gpu_count": GPU_COUNT, "gpu_names": names, "gpu_memory_gib": memory}


def _aggregate_replicates(rows: list[dict[str, Any]]) -> dict[str, Any]:
    passed = [row for row in rows if row.get("status") == "pass"]
    if not passed:
        return {"status": "failed", "replicates": rows}
    return {
        "status": "pass" if len(passed) == len(rows) else "partial",
        "batch_contexts": passed[0]["batch_contexts"],
        "global_token_batch": passed[0]["global_token_batch"],
        "tokens_per_second_per_gpu_median": statistics.median(
            row["tokens_per_second"] for row in passed
        ),
        "peak_allocated_gib_max": max(row["peak_allocated_gib"] for row in passed),
        "peak_reserved_gib_max": max(row["peak_reserved_gib"] for row in passed),
        "finite_forward_backward_optimizer": all(
            row["finite_forward_backward_optimizer"] for row in passed
        ),
        "minimum_median_gpu_utilization_percent": min(
            row["median_gpu_utilization_percent"] for row in passed
        ),
        "mean_median_power_watts": _mean(
            row["median_power_watts"] for row in passed
        ),
        "replicates": rows,
    }


def paid_preflight() -> dict[str, Any]:
    hardware = _hardware_inventory()
    agreement_rows = _run_tasks(_padded_variant_tasks("loss-agreement"))
    agreements = {
        variant: [row for row in agreement_rows if row.get("variant") == variant]
        for variant in VARIANTS
    }
    if any(
        not rows or any(row.get("status") != "pass" for row in rows)
        for rows in agreements.values()
    ):
        raise RuntimeError(f"BF16/FP32 loss agreement failed: {agreements}")

    curves: dict[str, list[dict[str, Any]]] = {variant: [] for variant in VARIANTS}
    stable_counts = {variant: 0 for variant in VARIANTS}
    for batch in BATCH_CANDIDATES:
        tasks = _padded_variant_tasks("benchmark", batch_contexts=batch)
        with NodeSampler() as sampler:
            rows = _run_tasks(tasks, timeout_seconds=900)
        telemetry = sampler.summary()
        for variant in VARIANTS:
            replicas = [row for row in rows if row.get("variant") == variant]
            aggregate = _aggregate_replicates(replicas)
            aggregate["node_telemetry"] = telemetry
            saturated = (
                aggregate["status"] == "pass"
                and aggregate["finite_forward_backward_optimizer"]
                and aggregate["global_token_batch"] >= 100_000
                and aggregate["minimum_median_gpu_utilization_percent"]
                >= MINIMUM_UTILIZATION
            )
            aggregate["stable_and_saturated"] = saturated
            curves[variant].append(aggregate)
            stable_counts[variant] += int(saturated)
        if all(count >= 3 for count in stable_counts.values()):
            break

    selected: dict[str, Any] = {}
    for variant in VARIANTS:
        stable = [row for row in curves[variant] if row.get("stable_and_saturated")]
        if not stable:
            raise RuntimeError(f"no finite saturated physical batch for {variant}")
        selected[variant] = max(
            stable, key=lambda row: row["tokens_per_second_per_gpu_median"]
        )

    with NodeSampler() as sampler:
        underfilled_rows = _run_tasks(
            _padded_variant_tasks("benchmark", batch_contexts=UNDERFILLED_BATCH),
            timeout_seconds=900,
        )
    underfilled_telemetry = sampler.summary()
    underfilled = {
        variant: _aggregate_replicates(
            [row for row in underfilled_rows if row.get("variant") == variant]
        )
        for variant in VARIANTS
    }
    for variant in VARIANTS:
        selected[variant]["underfilled_throughput_multiplier"] = (
            selected[variant]["tokens_per_second_per_gpu_median"]
            / underfilled[variant]["tokens_per_second_per_gpu_median"]
        )

    scaling: dict[str, Any] = {}
    for variant in VARIANTS:
        batch = int(selected[variant]["batch_contexts"])
        scaling[variant] = {}
        for count in (1, 2, 4, 8):
            rows = _run_tasks(
                [
                    {
                        "kind": "benchmark",
                        "variant": variant,
                        "batch_contexts": batch,
                        "measured_updates": 5,
                    }
                    for _ in range(count)
                ],
                workers=count,
                timeout_seconds=900,
            )
            aggregate = sum(
                float(row["tokens_per_second"])
                for row in rows
                if row.get("status") == "pass"
            )
            scaling[variant][str(count)] = {
                "workers": rows,
                "aggregate_tokens_per_second": aggregate,
                "per_gpu_tokens_per_second": aggregate / count,
            }

    return {
        "schema": "expv2-2-matched-muon-preflight-v1",
        "status": "pass",
        "hardware": hardware,
        "loss_agreement": agreements,
        "curves": curves,
        "selected": selected,
        "underfilled": underfilled,
        "underfilled_node_telemetry": underfilled_telemetry,
        "parallel_scaling": scaling,
        "gradient_accumulation_steps": 1,
    }


def _all_complete(rows: Iterable[dict[str, Any]], label: str) -> None:
    failures = [row for row in rows if row.get("status") != "complete"]
    if failures:
        raise RuntimeError(f"{label} failed cells: {failures}")


def select_recipe(rows: Iterable[dict[str, Any]], variant: str) -> dict[str, Any]:
    candidates = [row for row in rows if row.get("variant") == variant]
    if len(candidates) != len(LEARNING_RATES):
        raise RuntimeError(f"missing LR candidates for {variant}")
    # OOD is intentionally absent from the selection key.  It remains a held-out
    # architecture metric, not a hyperparameter-tuning signal.
    return max(
        candidates,
        key=lambda row: (
            bool(row["id_pass"]),
            float(row["minimum_id_accuracy"]),
            float(row["mean_id_accuracy"]),
            -float(row["recipe"]["lr"]),
        ),
    )


def _metric_summary(rows: list[dict[str, Any]], variant: str) -> dict[str, Any]:
    selected = [row for row in rows if row["variant"] == variant]
    id_names = tuple(selected[0]["id_accuracies"])
    ood_names = tuple(selected[0]["ood_accuracies"])
    return {
        "variant": variant,
        "seeds": [row["seed"] for row in selected],
        "id_passes": sum(bool(row["id_pass"]) for row in selected),
        "id_accuracy_mean": {
            name: _mean(row["id_accuracies"][name] for row in selected)
            for name in id_names
        },
        "ood_accuracy_mean": {
            name: _mean(row["ood_accuracies"][name] for row in selected)
            for name in ood_names
        },
        "macro_id_accuracy_mean": _mean(
            value for row in selected for value in row["id_accuracies"].values()
        ),
        "macro_ood_accuracy_mean": _mean(
            value for row in selected for value in row["ood_accuracies"].values()
        ),
        "elapsed_seconds_mean": _mean(row["elapsed_seconds"] for row in selected),
        "parameters": selected[0]["inventory"]["total_parameters"],
        "body_parameters": selected[0]["inventory"]["body_parameters"],
        "wandb_urls": [row["wandb_url"] for row in selected],
    }


def run_matched_comparison(
    *,
    output_root: str | Path,
    result_path: str | Path,
    heartbeat_path: str | Path | None = None,
) -> dict[str, Any]:
    key = os.environ.get("WANDB_API_KEY")
    if not key:
        raise RuntimeError("WANDB_API_KEY is required before paid comparison")
    import wandb

    output = Path(output_root)
    output.mkdir(parents=True, exist_ok=True)
    wandb.login(key=key, relogin=True, verify=True)
    run = wandb.init(
        project=WANDB_PROJECT,
        name="expv2-2-eight-gpu-token-matched-muon",
        job_type="matched-comparison-controller",
        dir=str(output),
        config={
            "schema": SCHEMA,
            "variants": VARIANTS,
            "learning_rates": LEARNING_RATES,
            "confirmation_seeds": CONFIRMATION_SEEDS,
            "minimum_target_contexts_per_phase": TARGET_CONTEXTS_PER_PHASE,
            "minimum_target_tokens_per_phase": TARGET_CONTEXTS_PER_PHASE * CONTEXT_LENGTH,
            "gpu_count": GPU_COUNT,
            "cloud_only_training": True,
        },
    )
    wandb_url = str(run.url or run.get_url() or "")
    if not wandb_url.startswith("http"):
        run.finish(exit_code=1)
        raise RuntimeError("W&B did not provide a direct controller URL")
    print(f"WANDB_URL={wandb_url}", flush=True)
    atomic_json(
        output / "wandb-launch.json",
        {"schema": "expv2-2-wandb-launch-v1", "status": "complete", "wandb_url": wandb_url},
    )
    heartbeat = Path(heartbeat_path) if heartbeat_path else None
    stop = threading.Event()
    thread: threading.Thread | None = None
    if heartbeat:
        heartbeat.parent.mkdir(parents=True, exist_ok=True)
        heartbeat.touch()

        def pulse() -> None:
            while not stop.wait(30):
                heartbeat.touch()

        thread = threading.Thread(target=pulse, daemon=True)
        thread.start()
    started = time.monotonic()
    try:
        atomic_json(
            result_path,
            {"schema": SCHEMA, "status": "running", "wandb_url": wandb_url},
        )
        preflight = paid_preflight()
        atomic_json(output / "paid-preflight.json", preflight)
        matched_contexts = max(
            TARGET_CONTEXTS_PER_PHASE,
            max(
                int(preflight["selected"][name]["batch_contexts"])
                for name in VARIANTS
            )
            * MINIMUM_UPDATES,
        )
        tuning_wall_seconds_per_phase = max(
            (
                int(preflight["selected"][name]["batch_contexts"])
                * CONTEXT_LENGTH
                * MINIMUM_UPDATES
                / float(
                    preflight["selected"][name][
                        "tokens_per_second_per_gpu_median"
                    ]
                )
            )
            for name in VARIANTS
        )
        tuning_contexts = {
            name: (
                math.ceil(
                    (
                        float(
                            preflight["selected"][name][
                                "tokens_per_second_per_gpu_median"
                            ]
                        )
                        * tuning_wall_seconds_per_phase
                        / CONTEXT_LENGTH
                    )
                    / int(preflight["selected"][name]["batch_contexts"])
                )
                * int(preflight["selected"][name]["batch_contexts"])
            )
            for name in VARIANTS
        }
        run.config.update(
            {
                "tuning_contexts_per_phase": tuning_contexts,
                "tuning_predicted_wall_seconds_per_phase": tuning_wall_seconds_per_phase,
                "target_contexts_per_phase": matched_contexts,
                "target_tokens_per_phase": matched_contexts * CONTEXT_LENGTH,
            },
            allow_val_change=True,
        )
        run.log(
            {
                "preflight/gpu_count": GPU_COUNT,
                **{
                    f"preflight/{variant}/batch_contexts": preflight["selected"][variant]["batch_contexts"]
                    for variant in VARIANTS
                },
                **{
                    f"preflight/{variant}/tokens_per_second": preflight["selected"][variant]["tokens_per_second_per_gpu_median"]
                    for variant in VARIANTS
                },
            }
        )
        sweep_tasks = [
            {
                "kind": "train",
                "stage": "lr-sweep",
                "variant": variant,
                "lr": lr,
                "seed": 0,
                "batch_contexts": int(preflight["selected"][variant]["batch_contexts"]),
                "target_contexts": tuning_contexts[variant],
                "output_root": str(output),
            }
            for variant in VARIANTS
            for lr in LEARNING_RATES
        ]
        with NodeSampler() as sampler:
            sweep = _run_tasks(sweep_tasks)
        sweep_utilization = sampler.summary()
        _all_complete(sweep, "LR sweep")
        selection = {variant: select_recipe(sweep, variant) for variant in VARIANTS}
        atomic_json(
            output / "lr-selection.json",
            {
                "schema": "expv2-2-matched-muon-lr-selection-v1",
                "status": "complete",
                "selection_uses_ood": False,
                "rows": sweep,
                "selected": selection,
                "node_telemetry": sweep_utilization,
            },
        )
        confirmation_tasks = [
            {
                "kind": "train",
                "stage": "confirmation",
                "variant": variant,
                "lr": float(selection[variant]["recipe"]["lr"]),
                "seed": seed,
                "batch_contexts": int(preflight["selected"][variant]["batch_contexts"]),
                "target_contexts": matched_contexts,
                "output_root": str(output),
            }
            for variant in VARIANTS
            for seed in CONFIRMATION_SEEDS
        ]
        with NodeSampler() as sampler:
            confirmation = _run_tasks(confirmation_tasks)
        confirmation_utilization = sampler.summary()
        _all_complete(confirmation, "four-seed confirmation")
        summaries = {
            variant: _metric_summary(confirmation, variant) for variant in VARIANTS
        }
        transformer = summaries["transformer"]
        kronecker = summaries["kron-r1-d66"]
        parameter_mismatch = abs(
            float(kronecker["parameters"]) - float(transformer["parameters"])
        ) / float(transformer["parameters"])
        ood_delta = (
            kronecker["macro_ood_accuracy_mean"]
            - transformer["macro_ood_accuracy_mean"]
        )
        id_delta = (
            kronecker["macro_id_accuracy_mean"]
            - transformer["macro_id_accuracy_mean"]
        )
        both_reliable = (
            transformer["id_passes"] == len(CONFIRMATION_SEEDS)
            and kronecker["id_passes"] == len(CONFIRMATION_SEEDS)
        )
        verdict = (
            "deep_kronecker_wins_matched_confirmation"
            if both_reliable and ood_delta > 0.01
            else "architectures_tie_within_one_point"
            if both_reliable and abs(ood_delta) <= 0.01
            else "transformer_wins_matched_confirmation"
            if transformer["id_passes"] == len(CONFIRMATION_SEEDS)
            else "comparison_inconclusive_id_reliability"
        )
        result = {
            "schema": SCHEMA,
            "status": "complete",
            "wandb_url": wandb_url,
            "verdict": verdict,
            "preflight": preflight,
            "sweep": sweep,
            "selection": selection,
            "selection_uses_ood": False,
            "sweep_node_telemetry": sweep_utilization,
            "confirmation": confirmation,
            "confirmation_node_telemetry": confirmation_utilization,
            "summaries": summaries,
            "matched_basis": {
                "parameter_mismatch_fraction": parameter_mismatch,
                "target_contexts_per_phase": matched_contexts,
                "target_tokens_per_phase": matched_contexts * CONTEXT_LENGTH,
                "minimum_updates_per_phase": MINIMUM_UPDATES,
                "optimizer": "pure-batched-muon",
                "task_curriculum": [
                    "delay-copy",
                    "associative-recall",
                    "concurrent-edges",
                    "concurrent-edges-and-two-hop",
                    "two-hop-finetune",
                ],
            },
            "paired_deltas_kronecker_minus_transformer": {
                "macro_id_accuracy": id_delta,
                "macro_ood_accuracy": ood_delta,
            },
            "elapsed_seconds": time.monotonic() - started,
        }
        atomic_json(result_path, result)
        run.summary.update(
            {
                "verdict": verdict,
                "parameter_mismatch_fraction": parameter_mismatch,
                "kronecker_minus_transformer_macro_id_accuracy": id_delta,
                "kronecker_minus_transformer_macro_ood_accuracy": ood_delta,
                "transformer_id_passes": transformer["id_passes"],
                "kronecker_id_passes": kronecker["id_passes"],
            }
        )
        return result
    except Exception as error:
        atomic_json(
            result_path,
            {
                "schema": SCHEMA,
                "status": "failed",
                "wandb_url": wandb_url,
                "reason": f"{type(error).__name__}: {error}",
                "elapsed_seconds": time.monotonic() - started,
            },
        )
        raise
    finally:
        stop.set()
        if thread is not None:
            thread.join(timeout=3)
        run.finish()
