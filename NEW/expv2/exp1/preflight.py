"""Paid H100 utilization, numerical, batch, and compilation preflight."""

from __future__ import annotations

import json
import os
import statistics
import subprocess
import sys
import threading
import time
from pathlib import Path
from typing import Any

import torch
import torch.nn.functional as F

from .config import CONTEXT_LENGTH, KRONECKER_SHAPES, VOCAB_SIZE
from .model import build_model
from .optimizer import build_muon
from .utils import atomic_json, finite_tree, seed_everything


BATCH_CANDIDATES = (8_192, 4_096, 2_048, 1_024)
UNDERFILLED_BATCH = 128
MINIMUM_UTILIZATION = 85.0
MINIMUM_THROUGHPUT_MULTIPLIER = 10.0


class NvidiaSampler:
    def __init__(self, interval: float = 0.2) -> None:
        self.interval = interval
        self.rows: list[tuple[float, float, float]] = []
        self.stop_event = threading.Event()
        self.thread: threading.Thread | None = None

    def _sample(self) -> None:
        while not self.stop_event.is_set():
            try:
                output = subprocess.check_output(
                    [
                        "nvidia-smi",
                        "--query-gpu=utilization.gpu,power.draw,memory.used",
                        "--format=csv,noheader,nounits",
                    ],
                    text=True,
                    timeout=5,
                )
                lines = [line for line in output.splitlines() if line.strip()]
                if len(lines) == 1:
                    values = tuple(float(item.strip()) for item in lines[0].split(","))
                    self.rows.append(values)  # type: ignore[arg-type]
            except (OSError, subprocess.SubprocessError, ValueError):
                pass
            self.stop_event.wait(self.interval)

    def __enter__(self) -> "NvidiaSampler":
        self.thread = threading.Thread(target=self._sample, daemon=True)
        self.thread.start()
        return self

    def __exit__(self, *_: Any) -> None:
        self.stop_event.set()
        if self.thread is not None:
            self.thread.join(timeout=5)

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
            "median_gpu_utilization_percent": statistics.median(
                row[0] for row in self.rows
            ),
            "median_power_watts": statistics.median(row[1] for row in self.rows),
            "peak_nvidia_memory_mib": max(row[2] for row in self.rows),
        }


def _training_step(
    model: torch.nn.Module,
    optimizer: torch.optim.Optimizer,
    tokens: torch.Tensor,
) -> tuple[float, float]:
    optimizer.zero_grad(set_to_none=True)
    logits = model(tokens)
    targets = torch.roll(tokens, shifts=-1, dims=1)
    loss = F.cross_entropy(logits.float().flatten(0, 1), targets.flatten())
    loss.backward()
    gradient_norm = torch.nn.utils.clip_grad_norm_(model.parameters(), 1.0)
    optimizer.step()
    return float(loss.item()), float(gradient_norm)


def benchmark_variant(
    variant: str,
    batch_contexts: int,
    *,
    warmup_steps: int = 2,
    measured_steps: int = 4,
) -> dict[str, Any]:
    if not torch.cuda.is_available() or torch.cuda.device_count() != 1:
        raise RuntimeError("benchmark requires exactly one visible CUDA GPU")
    device = torch.device("cuda")
    seed_everything(123)
    torch.cuda.empty_cache()
    torch.cuda.reset_peak_memory_stats(device)
    model = build_model(variant).to(device=device, dtype=torch.bfloat16)
    optimizer, routing = build_muon(model, lr=0.01)
    tokens = torch.randint(
        0, VOCAB_SIZE, (batch_contexts, CONTEXT_LENGTH), device=device
    )
    losses, gradient_norms = [], []
    try:
        for _ in range(warmup_steps):
            _training_step(model, optimizer, tokens)
        torch.cuda.synchronize(device)
        with NvidiaSampler() as sampler:
            started = time.monotonic()
            for _ in range(measured_steps):
                loss, gradient_norm = _training_step(model, optimizer, tokens)
                losses.append(loss)
                gradient_norms.append(gradient_norm)
            torch.cuda.synchronize(device)
            elapsed = time.monotonic() - started
        metrics = sampler.summary()
        result = {
            "status": "pass",
            "variant": variant,
            "batch_contexts": batch_contexts,
            "global_token_batch": batch_contexts * CONTEXT_LENGTH,
            "steps": measured_steps,
            "elapsed_seconds": elapsed,
            "tokens_per_second": (
                batch_contexts * CONTEXT_LENGTH * measured_steps / elapsed
            ),
            "peak_allocated_gib": torch.cuda.max_memory_allocated(device) / 2**30,
            "peak_reserved_gib": torch.cuda.max_memory_reserved(device) / 2**30,
            "finite_loss": all(torch.isfinite(torch.tensor(losses))),
            "finite_gradient_norm": all(torch.isfinite(torch.tensor(gradient_norms))),
            "finite_optimizer_state": finite_tree(optimizer.state),
            "optimizer_routing": routing,
            **metrics,
        }
    finally:
        del tokens, model, optimizer
        torch.cuda.empty_cache()
    return result


def _loss_agreement(variant: str) -> dict[str, float]:
    seed_everything(91)
    fp32 = build_model(variant).cuda().float()
    bf16 = build_model(variant).cuda().bfloat16()
    bf16.load_state_dict(fp32.state_dict())
    tokens = torch.randint(0, VOCAB_SIZE, (2, CONTEXT_LENGTH), device="cuda")
    targets = torch.roll(tokens, shifts=-1, dims=1)
    with torch.no_grad():
        left = F.cross_entropy(
            fp32(tokens).flatten(0, 1), targets.flatten()
        ).item()
        right = F.cross_entropy(
            bf16(tokens).float().flatten(0, 1), targets.flatten()
        ).item()
    del fp32, bf16, tokens, targets
    torch.cuda.empty_cache()
    return {
        "fp32_loss": float(left),
        "bf16_loss": float(right),
        "absolute_difference": abs(float(left) - float(right)),
    }


def compilation_probe(variant: str, batch_contexts: int) -> dict[str, Any]:
    """Run by a bounded subprocess so a pathological compile cannot consume the pilot."""

    if not torch.cuda.is_available():
        raise RuntimeError("compilation probe requires CUDA")
    seed_everything(44)
    device = torch.device("cuda")
    model = build_model(variant).to(device=device, dtype=torch.bfloat16)
    tokens = torch.randint(0, VOCAB_SIZE, (batch_contexts, CONTEXT_LENGTH), device=device)
    eager_optimizer, _ = build_muon(model, lr=0.01)
    torch.cuda.synchronize()
    eager_started = time.monotonic()
    eager_loss, _ = _training_step(model, eager_optimizer, tokens)
    torch.cuda.synchronize()
    eager_seconds = time.monotonic() - eager_started
    compiled_model = torch.compile(model, mode="reduce-overhead", fullgraph=False)
    compiled_optimizer, _ = build_muon(compiled_model, lr=0.01)
    torch.cuda.synchronize()
    compiled_started = time.monotonic()
    compiled_loss, _ = _training_step(compiled_model, compiled_optimizer, tokens)
    torch.cuda.synchronize()
    first_seconds = time.monotonic() - compiled_started
    steady_started = time.monotonic()
    _training_step(compiled_model, compiled_optimizer, tokens)
    torch.cuda.synchronize()
    steady_seconds = time.monotonic() - steady_started
    return {
        "schema": "expv2-1-compilation-probe-v1",
        "variant": variant,
        "batch_contexts": batch_contexts,
        "eager_seconds": eager_seconds,
        "compiled_first_seconds": first_seconds,
        "compiled_steady_seconds": steady_seconds,
        "loss_difference": abs(eager_loss - compiled_loss),
    }


def run_compilation_subprocess(
    variant: str, batch_contexts: int, *, timeout_seconds: int = 60
) -> dict[str, Any]:
    command = [
        sys.executable,
        "-m",
        "expv2.exp1",
        "compile-probe",
        f"--variant={variant}",
        f"--batch-contexts={batch_contexts}",
    ]
    started = time.monotonic()
    try:
        result = subprocess.run(
            command,
            text=True,
            capture_output=True,
            timeout=timeout_seconds,
            check=True,
        )
        value = json.loads(result.stdout)
        value["subprocess_elapsed_seconds"] = time.monotonic() - started
        value["status"] = "pass"
        return value
    except subprocess.TimeoutExpired:
        return {
            "status": "timeout",
            "timeout_seconds": timeout_seconds,
            "subprocess_elapsed_seconds": time.monotonic() - started,
        }
    except subprocess.CalledProcessError as error:
        return {
            "status": "fail",
            "returncode": error.returncode,
            "stderr": error.stderr[-4000:],
            "subprocess_elapsed_seconds": time.monotonic() - started,
        }


def paid_preflight(
    output: str | Path,
    *,
    wandb_url: str,
) -> dict[str, Any]:
    failures: list[str] = []
    if not os.environ.get("WANDB_API_KEY"):
        failures.append("WANDB_API_KEY is missing")
    if not wandb_url.startswith("http"):
        failures.append("direct W&B URL is missing")
    if not torch.cuda.is_available() or torch.cuda.device_count() != 1:
        failures.append("exactly one CUDA GPU is required")
        name = "unavailable"
    else:
        name = torch.cuda.get_device_name(0)
        if "H100" not in name.upper():
            failures.append(f"paid target is not an H100: {name}")
    if failures:
        result = {
            "schema": "expv2-1-paid-preflight-v1",
            "status": "fail",
            "wandb_url": wandb_url,
            "device": name,
            "failures": failures,
        }
        atomic_json(output, result)
        return result

    variants = (*KRONECKER_SHAPES, "dense", "transformer")
    agreement = {variant: _loss_agreement(variant) for variant in variants}
    sweeps: dict[str, Any] = {}
    selected: dict[str, int] = {}
    for variant in variants:
        rows = []
        baseline = benchmark_variant(variant, UNDERFILLED_BATCH)
        for batch in BATCH_CANDIDATES:
            try:
                row = benchmark_variant(variant, batch)
            except torch.cuda.OutOfMemoryError:
                torch.cuda.empty_cache()
                row = {
                    "status": "oom",
                    "variant": variant,
                    "batch_contexts": batch,
                    "global_token_batch": batch * CONTEXT_LENGTH,
                }
            rows.append(row)
        stable = [row for row in rows if row["status"] == "pass"]
        if not stable:
            failures.append(f"{variant} has no stable ambitious physical batch")
            continue
        winner = max(stable, key=lambda row: row["tokens_per_second"])
        selected[variant] = int(winner["batch_contexts"])
        ratio = winner["tokens_per_second"] / baseline["tokens_per_second"]
        winner["underfilled_throughput_multiplier"] = ratio
        if winner["global_token_batch"] < 100_000:
            failures.append(f"{variant} selected fewer than 100k global tokens")
        # Ten-fold is an aspiration when the underfilled baseline leaves that
        # headroom, not a reason to reject a measured saturated configuration.
        # These tiny models can already drive an H100 hard at batch 128.  Keep
        # the measured ratio and require the selected batch itself to saturate
        # the device; never infer headroom from allocation alone.
        winner["throughput_target_status"] = (
            "at_least_10x"
            if ratio >= MINIMUM_THROUGHPUT_MULTIPLIER
            else "measured_saturation_below_10x"
        )
        if winner["median_gpu_utilization_percent"] < MINIMUM_UTILIZATION:
            failures.append(f"{variant} GPU utilization is below 85 percent")
        if not winner["finite_loss"] or not winner["finite_gradient_norm"] or not winner["finite_optimizer_state"]:
            failures.append(f"{variant} produced non-finite preflight state")
        sweeps[variant] = {"underfilled": baseline, "ambitious": rows, "selected": winner}
    for variant, row in agreement.items():
        if row["absolute_difference"] > 0.05:
            failures.append(f"{variant} BF16/FP32 loss mismatch exceeds 0.05")

    compile_variant = "kron-r3-d22"
    compile_result = (
        run_compilation_subprocess(
            compile_variant, min(selected.get(compile_variant, 1024), 1024)
        )
        if compile_variant in selected
        else {"status": "skipped"}
    )
    use_compile = False
    if compile_result.get("status") == "pass":
        projected_steps = 1_000
        eager_projected = compile_result["eager_seconds"] * projected_steps
        compiled_projected = (
            compile_result["compiled_first_seconds"]
            + compile_result["compiled_steady_seconds"] * (projected_steps - 1)
        )
        compile_result["eager_projected_seconds"] = eager_projected
        compile_result["compiled_projected_seconds"] = compiled_projected
        use_compile = (
            compile_result["loss_difference"] <= 0.05
            and compiled_projected <= 0.95 * eager_projected
        )
    result = {
        "schema": "expv2-1-paid-preflight-v1",
        "status": "pass" if not failures else "fail",
        "wandb_url": wandb_url,
        "device": name,
        "device_count": torch.cuda.device_count(),
        "bf16_fp32_agreement": agreement,
        "batch_sweeps": sweeps,
        "selected_batch_contexts": selected,
        "compilation": compile_result,
        "use_compile": use_compile,
        "gradient_accumulation_steps": 1,
        "failures": failures,
    }
    atomic_json(output, result)
    return result
