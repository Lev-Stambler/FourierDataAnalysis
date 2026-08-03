"""H100 batch and numerical preflight for the corrected positive control."""

from __future__ import annotations

import statistics
import subprocess
import threading
import time
from pathlib import Path
from typing import Any

import torch

from expv2.exp1.model import build_model
from expv2.exp1.utils import atomic_json, finite_tree, seed_everything

from .synthetic import make_batch, masked_loss_and_accuracy
from .training import CalibrationRecipe, _optimizer


BATCH_CANDIDATES = (12_288, 10_240, 8_192, 4_096, 2_048, 1_024)
UNDERFILLED_BATCH = 128
MINIMUM_UTILIZATION = 85.0
MAXIMUM_BF16_LOSS_RELATIVE_ERROR = 0.02


class NvidiaSampler:
    def __init__(self, interval: float = 0.1) -> None:
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

    def __enter__(self) -> "NvidiaSampler":
        self.thread = threading.Thread(target=self._sample, daemon=True)
        self.thread.start()
        return self

    def __exit__(self, *_: Any) -> None:
        self.stop.set()
        if self.thread is not None:
            self.thread.join(timeout=3)

    def summary(self) -> dict[str, float | int]:
        if not self.rows:
            return {
                "samples": 0,
                "median_gpu_utilization_percent": 0.0,
                "median_power_watts": 0.0,
                "peak_nvidia_memory_mib": 0.0,
            }
        return {
            "samples": len(self.rows),
            "median_gpu_utilization_percent": statistics.median(row[0] for row in self.rows),
            "median_power_watts": statistics.median(row[1] for row in self.rows),
            "peak_nvidia_memory_mib": max(row[2] for row in self.rows),
        }


def benchmark_batch(
    batch_contexts: int,
    *,
    warmup_updates: int = 3,
    measured_updates: int = 10,
) -> dict[str, Any]:
    if not torch.cuda.is_available() or torch.cuda.device_count() != 1:
        raise RuntimeError("preflight requires exactly one visible CUDA device")
    seed_everything(441)
    device = torch.device("cuda")
    torch.cuda.empty_cache()
    torch.cuda.reset_peak_memory_stats(device)
    model = build_model("transformer", vocab_size=128).to(
        device=device, dtype=torch.bfloat16
    )
    recipe = CalibrationRecipe("adamw", 3e-4)
    optimizer, routing = _optimizer(model, recipe)
    finite = True

    def update(index: int) -> tuple[float, float]:
        batch = make_batch(
            "associative-recall",
            batch_contexts,
            seed=90_000 + index,
            split="train",
            device=device,
        )
        optimizer.zero_grad(set_to_none=True)
        loss, _ = masked_loss_and_accuracy(
            model(batch.inputs), batch.targets, batch.mask
        )
        loss.backward()
        norm = torch.nn.utils.clip_grad_norm_(model.parameters(), 1.0)
        optimizer.step()
        return float(loss), float(norm)

    try:
        for index in range(warmup_updates):
            loss, norm = update(index)
            finite = finite and torch.isfinite(torch.tensor([loss, norm])).all().item()
        torch.cuda.synchronize(device)
        with NvidiaSampler() as sampler:
            started = time.monotonic()
            for index in range(measured_updates):
                loss, norm = update(warmup_updates + index)
                finite = finite and torch.isfinite(torch.tensor([loss, norm])).all().item()
            torch.cuda.synchronize(device)
            elapsed = time.monotonic() - started
        result = {
            "status": "pass",
            "batch_contexts": batch_contexts,
            "global_token_batch": batch_contexts * 128,
            "measured_updates": measured_updates,
            "elapsed_seconds": elapsed,
            "tokens_per_second": batch_contexts * 128 * measured_updates / elapsed,
            "updates_per_second": measured_updates / elapsed,
            "peak_allocated_gib": torch.cuda.max_memory_allocated(device) / 2**30,
            "peak_reserved_gib": torch.cuda.max_memory_reserved(device) / 2**30,
            "finite_training_state": bool(finite and finite_tree(optimizer.state)),
            "optimizer_routing": routing,
            **sampler.summary(),
        }
    finally:
        del model, optimizer
        torch.cuda.empty_cache()
    return result


@torch.inference_mode()
def loss_agreement(examples: int = 256) -> dict[str, Any]:
    """Compare the intended BF16 fast path with an FP32 reference."""

    if not torch.cuda.is_available() or torch.cuda.device_count() != 1:
        raise RuntimeError("loss agreement requires exactly one CUDA device")
    seed_everything(992)
    device = torch.device("cuda")
    reference = build_model("transformer", vocab_size=128).to(
        device=device, dtype=torch.float32
    )
    candidate = build_model("transformer", vocab_size=128).to(
        device=device, dtype=torch.bfloat16
    )
    candidate.load_state_dict(reference.state_dict())
    batch = make_batch(
        "associative-recall", examples, seed=73_001, split="train", device=device
    )
    reference_loss, _ = masked_loss_and_accuracy(
        reference(batch.inputs), batch.targets, batch.mask
    )
    candidate_loss, _ = masked_loss_and_accuracy(
        candidate(batch.inputs), batch.targets, batch.mask
    )
    fp32 = float(reference_loss)
    bf16 = float(candidate_loss)
    relative = abs(fp32 - bf16) / max(abs(fp32), 1e-12)
    result = {
        "examples": examples,
        "fp32_loss": fp32,
        "bf16_loss": bf16,
        "relative_error": relative,
        "maximum_relative_error": MAXIMUM_BF16_LOSS_RELATIVE_ERROR,
        "status": (
            "pass" if relative <= MAXIMUM_BF16_LOSS_RELATIVE_ERROR else "fail"
        ),
    }
    del reference, candidate
    torch.cuda.empty_cache()
    return result


def paid_preflight(output: str | Path, *, wandb_url: str) -> dict[str, Any]:
    failures: list[str] = []
    if not wandb_url.startswith("http"):
        failures.append("missing direct W&B URL")
    if not torch.cuda.is_available() or torch.cuda.device_count() != 1:
        failures.append("exactly one CUDA GPU is required")
        device_name = "unavailable"
    else:
        device_name = torch.cuda.get_device_name(0)
        if "H100" not in device_name.upper():
            failures.append(f"expected H100, got {device_name}")
    if failures:
        result = {
            "schema": "expv2-2-positive-control-preflight-v1",
            "status": "fail",
            "wandb_url": wandb_url,
            "device": device_name,
            "failures": failures,
        }
        atomic_json(output, result)
        return result

    agreement = loss_agreement()
    if agreement["status"] != "pass":
        failures.append("BF16 loss does not agree with FP32 reference")
    baseline = benchmark_batch(UNDERFILLED_BATCH)
    rows: list[dict[str, Any]] = []
    for batch in BATCH_CANDIDATES:
        try:
            row = benchmark_batch(batch)
        except torch.cuda.OutOfMemoryError:
            torch.cuda.empty_cache()
            row = {
                "status": "oom",
                "batch_contexts": batch,
                "global_token_batch": batch * 128,
            }
        rows.append(row)
    stable = [
        row
        for row in rows
        if row["status"] == "pass"
        and row["finite_training_state"]
        and row["median_gpu_utilization_percent"] >= MINIMUM_UTILIZATION
        and row["global_token_batch"] >= 100_000
    ]
    if not stable:
        failures.append("no stable, finite, saturated >=100k-token physical batch")
        selected = None
    else:
        selected = max(stable, key=lambda row: row["tokens_per_second"])
        selected["underfilled_throughput_multiplier"] = (
            selected["tokens_per_second"] / baseline["tokens_per_second"]
        )
    result = {
        "schema": "expv2-2-positive-control-preflight-v1",
        "status": "pass" if not failures else "fail",
        "wandb_url": wandb_url,
        "device": device_name,
        "device_count": torch.cuda.device_count(),
        "loss_agreement": agreement,
        "underfilled": baseline,
        "ambitious": rows,
        "selected": selected,
        "gradient_accumulation_steps": 1,
        "failures": failures,
    }
    atomic_json(output, result)
    return result
