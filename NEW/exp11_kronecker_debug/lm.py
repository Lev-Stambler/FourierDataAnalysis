"""Measured, resumable one-GPU WikiText pilot for Experiment 11."""

from __future__ import annotations

import gc
import hashlib
import json
import math
import os
import subprocess
import time
from dataclasses import asdict
from pathlib import Path
from typing import Any, Callable

import numpy as np
import torch
import torch.nn.functional as F

from .diagnostics import participation_ratio, rank_utilization
from .model import CanonicalOrder3Block, LanguageModel, VARIANTS, build_model, model_inventory


SCHEMA = "exp11-wikitext-pilot-v1"
LR_GRIDS = {
    "exp10-replica": (0.003, 0.01, 0.03),
    "order2-balanced": (0.003, 0.01, 0.03),
    "order3-r4": (0.003, 0.01, 0.03),
    "order3-r8": (0.003, 0.01, 0.03),
    "transformer": (0.0003, 0.001, 0.003),
}
SCREEN_TOKENS = 2_000_000
PROMOTION_TOKENS = 10_000_000
FINAL_TOKENS = 40_000_000
FINAL_VARIANTS = ("order3-r4", "transformer")
FINAL_SEEDS = (0, 1, 2)
BATCH_CANDIDATES = (512, 384, 256, 192, 128, 96, 64, 48, 32, 24, 16, 8)


def write_json(path: Path, value: Any) -> None:
    """Atomically publish an artifact so interruption cannot truncate it."""
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(path.name + ".tmp")
    temporary.write_text(json.dumps(value, indent=2, sort_keys=True))
    temporary.replace(path)


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(8 << 20), b""):
            digest.update(chunk)
    return digest.hexdigest()


def load_windows(data_root: Path, split: str, *, verify: bool = True) -> np.ndarray:
    manifest_path = data_root / "manifest.json"
    manifest = json.loads(manifest_path.read_text())
    if manifest.get("schema") != "exp10-tokenized-corpus-v1":
        raise RuntimeError("invalid WikiText corpus manifest")
    if int(manifest.get("context_length", 0)) != 256:
        raise RuntimeError("Experiment 11 requires 256-token WikiText windows")
    path = data_root / f"{split}.npy"
    expected = manifest["files"].get(path.name)
    if not expected:
        raise RuntimeError(f"split is absent from corpus manifest: {split}")
    if verify and sha256(path) != expected["sha256"]:
        raise RuntimeError(f"checksum mismatch: {path}")
    values = np.load(path, mmap_mode="r")
    if values.ndim != 2 or values.shape[1] != 257:
        raise RuntimeError(f"unexpected WikiText window shape: {values.shape}")
    return values


def permutation_multiplier(size: int, seed: int) -> int:
    candidate = 2 * (seed + 1) + 1
    while math.gcd(candidate, size) != 1:
        candidate += 2
    return candidate


def batch_indices(size: int, step: int, batch: int, seed: int) -> np.ndarray:
    """Map a logical stream to windows without storing a shuffled corpus."""
    positions = np.arange(step * batch, (step + 1) * batch, dtype=np.int64)
    multiplier = permutation_multiplier(size, seed)
    offset = int.from_bytes(hashlib.sha256(str(seed).encode()).digest()[:8], "big") % size
    return (positions * multiplier + offset) % size


def tensor_batch(windows: np.ndarray, indices: np.ndarray, device: torch.device) -> tuple[torch.Tensor, torch.Tensor]:
    values = torch.as_tensor(
        np.asarray(windows[indices], dtype=np.int64), device=device
    )
    return values[:, :-1], values[:, 1:]


def cross_entropy(logits: torch.Tensor, targets: torch.Tensor) -> torch.Tensor:
    return F.cross_entropy(logits.float().flatten(0, 1), targets.flatten())


def gpu_snapshot(device: torch.device) -> dict[str, Any]:
    row: dict[str, Any] = {}
    try:
        query = subprocess.run(
            [
                "nvidia-smi",
                "--query-gpu=utilization.gpu,power.draw",
                "--format=csv,noheader,nounits",
                f"--id={device.index or 0}",
            ],
            check=False,
            capture_output=True,
            text=True,
            timeout=5,
        )
        if query.returncode == 0 and query.stdout.strip():
            fields = query.stdout.strip().splitlines()[0].split(",")
            row["nvidia_smi_utilization_percent"] = float(fields[0].strip())
            row["power_draw_watts"] = float(fields[1].strip())
    except (OSError, subprocess.SubprocessError, ValueError):
        pass
    return row


def optimizer_is_finite(optimizer: torch.optim.Optimizer) -> bool:
    return all(
        torch.isfinite(value).all()
        for state in optimizer.state.values()
        for value in state.values()
        if isinstance(value, torch.Tensor)
    )


def benchmark_batch(
    variant: str,
    batch: int,
    windows: np.ndarray,
    device: torch.device,
    *,
    measured_steps: int = 3,
    use_compile: bool = False,
    optimizer_factory: Callable[[str, LanguageModel], Any] | None = None,
) -> dict[str, Any]:
    torch.manual_seed(913)
    torch.cuda.empty_cache()
    model = build_model(variant).to(device)
    torch.cuda.reset_peak_memory_stats()
    wrapped = torch.compile(model) if use_compile else model
    optimizer = (
        optimizer_factory(variant, model)
        if optimizer_factory is not None
        else torch.optim.AdamW(
            model.parameters(),
            lr=LR_GRIDS[variant][1],
            betas=(0.9, 0.95),
            weight_decay=0.0,
        )
    )
    losses: list[float] = []
    elapsed = 0.0
    warmup_seconds = 0.0
    for step in range(measured_steps + 1):
        inputs, targets = tensor_batch(
            windows, batch_indices(len(windows), step, batch, 913), device
        )
        optimizer.zero_grad(set_to_none=True)
        torch.cuda.synchronize(device)
        started = time.perf_counter()
        with torch.autocast("cuda", dtype=torch.bfloat16):
            loss = cross_entropy(wrapped(inputs), targets)
        loss.backward()
        norm = torch.nn.utils.clip_grad_norm_(model.parameters(), 1.0)
        if not torch.isfinite(loss) or not torch.isfinite(norm):
            raise RuntimeError("non-finite forward/backward state in batch sweep")
        optimizer.step()
        if not optimizer_is_finite(optimizer):
            raise RuntimeError("non-finite optimizer state in batch sweep")
        torch.cuda.synchronize(device)
        if step:
            elapsed += time.perf_counter() - started
            losses.append(float(loss.detach()))
        else:
            warmup_seconds = time.perf_counter() - started
    tokens = measured_steps * batch * 256
    row = {
        "batch": batch,
        "global_examples_per_step": batch,
        "global_tokens_per_step": batch * 256,
        "tokens_per_second": tokens / elapsed,
        "step_seconds": elapsed / measured_steps,
        "warmup_or_compile_seconds": warmup_seconds,
        "execution_mode": "compiled" if use_compile else "eager",
        "peak_allocated_gib": torch.cuda.max_memory_allocated(device) / 2**30,
        "peak_reserved_gib": torch.cuda.max_memory_reserved(device) / 2**30,
        "last_nll": losses[-1],
        "finite_forward_backward_optimizer": True,
        **gpu_snapshot(device),
    }
    del optimizer, wrapped, model, inputs, targets, loss
    gc.collect()
    torch.cuda.empty_cache()
    return row


def preflight(
    windows: np.ndarray,
    device: torch.device,
    *,
    candidates: tuple[int, ...] = BATCH_CANDIDATES,
    variants: tuple[str, ...] = VARIANTS,
    log: Callable[[dict[str, Any]], None] | None = None,
    optimizer_factory: Callable[[str, LanguageModel], Any] | None = None,
) -> dict[str, Any]:
    if torch.cuda.device_count() != 1:
        raise RuntimeError("pilot preflight requires exactly one visible CUDA GPU")
    rows: dict[str, list[dict[str, Any]]] = {}
    selected: dict[str, dict[str, Any]] = {}
    for variant in variants:
        stable: list[dict[str, Any]] = []
        failures: list[dict[str, Any]] = []
        for batch in candidates:
            try:
                row = benchmark_batch(
                    variant,
                    batch,
                    windows,
                    device,
                    optimizer_factory=optimizer_factory,
                )
                stable.append(row)
                if log:
                    log({f"preflight/{variant}/{key}": value for key, value in row.items()})
                # Three stable sizes characterize the local throughput curve.
                if len(stable) == 3:
                    break
            except torch.OutOfMemoryError as error:
                failures.append({"batch": batch, "reason": "out_of_memory", "detail": str(error)[:300]})
                gc.collect()
                torch.cuda.empty_cache()
            except RuntimeError as error:
                if "out of memory" not in str(error).lower():
                    raise
                failures.append(
                    {"batch": batch, "reason": "out_of_memory", "detail": str(error)[:300]}
                )
                gc.collect()
                torch.cuda.empty_cache()
        if not stable:
            raise RuntimeError(f"no stable physical batch for {variant}: {failures}")
        eager_winner = max(stable, key=lambda item: item["tokens_per_second"])
        compiled: dict[str, Any] | None = None
        try:
            compiled = benchmark_batch(
                variant,
                int(eager_winner["batch"]),
                windows,
                device,
                use_compile=True,
                optimizer_factory=optimizer_factory,
            )
            if log:
                log(
                    {
                        f"preflight/{variant}/compiled/{key}": value
                        for key, value in compiled.items()
                    }
                )
        except (torch.OutOfMemoryError, RuntimeError) as error:
            failures.append(
                {
                    "batch": eager_winner["batch"],
                    "execution_mode": "compiled",
                    "reason": "compiled_path_unstable",
                    "detail": str(error)[:300],
                }
            )
            gc.collect()
            torch.cuda.empty_cache()
        choices = [eager_winner, *([compiled] if compiled is not None else [])]
        winner = max(choices, key=lambda item: item["tokens_per_second"])
        candidates_with_compile = [
            *stable,
            *([compiled] if compiled is not None else []),
        ]
        rows[variant] = [
            {**item, "selected": item is winner} for item in candidates_with_compile
        ] + failures
        selected[variant] = winner

    # Numerical agreement is checked independently from the throughput timing.
    numerical_variant = (
        "order3-r4"
        if "order3-r4" in selected
        else next(variant for variant in variants if variant.startswith("order3"))
    )
    torch.manual_seed(71)
    model = build_model(numerical_variant).to(device)
    inputs, targets = tensor_batch(windows, np.arange(2), device)
    with torch.no_grad(), torch.autocast("cuda", dtype=torch.bfloat16):
        bf16_loss = cross_entropy(model(inputs), targets)
        compiled_agreement: float | None = None
        if selected[numerical_variant]["execution_mode"] == "compiled":
            compiled_model = torch.compile(model)
            compiled_loss = cross_entropy(compiled_model(inputs), targets)
            compiled_agreement = abs(float(compiled_loss) - float(bf16_loss))
            if compiled_agreement > 0.02:
                raise RuntimeError(
                    f"compiled/eager BF16 loss disagreement is too large: {compiled_agreement}"
                )
    with torch.no_grad():
        fp32_loss = cross_entropy(model(inputs), targets)
    agreement = abs(float(bf16_loss) - float(fp32_loss))
    if agreement > 0.05:
        raise RuntimeError(f"BF16 loss disagreement is too large: {agreement}")
    del model, inputs, targets
    torch.cuda.empty_cache()
    return {
        "schema": "exp11-preflight-v1",
        "device_count": torch.cuda.device_count(),
        "device_name": torch.cuda.get_device_name(device),
        "precision": "bfloat16",
        "compilation": "benchmarked per architecture; selected by measured steady throughput",
        "gradient_accumulation": 1,
        "batch_sweeps": rows,
        "selected": selected,
        "bf16_fp32_nll_absolute_difference": agreement,
        "compiled_eager_bf16_nll_absolute_difference": compiled_agreement,
        "status": "pass",
    }


@torch.inference_mode()
def evaluate(
    model: LanguageModel,
    windows: np.ndarray,
    batch: int,
    device: torch.device,
    *,
    include_mechanism: bool = False,
) -> dict[str, Any]:
    model.eval()
    total_loss = 0.0
    total_tokens = 0
    position_loss = torch.zeros(256, dtype=torch.float64, device=device)
    for start in range(0, len(windows), batch):
        indices = np.arange(start, min(start + batch, len(windows)))
        inputs, targets = tensor_batch(windows, indices, device)
        with torch.autocast("cuda", dtype=torch.bfloat16):
            logits = model(inputs)
        token_loss = F.cross_entropy(
            logits.float().flatten(0, 1), targets.flatten(), reduction="none"
        ).reshape_as(targets)
        total_loss += float(token_loss.sum())
        total_tokens += targets.numel()
        position_loss += token_loss.sum(0).double()
    nll = total_loss / total_tokens
    result: dict[str, Any] = {
        "nll": nll,
        "perplexity": math.exp(min(nll, 20)),
        "tokens": total_tokens,
        "position_nll": (position_loss / len(windows)).cpu().tolist(),
    }
    if include_mechanism:
        inputs, _ = tensor_batch(windows, np.arange(min(16, len(windows))), device)
        hidden = model.hidden(inputs)
        result["hidden_participation_ratio"] = participation_ratio(hidden)
        first = model.blocks[0]
        if isinstance(first, CanonicalOrder3Block):
            result["first_block_rank_utilization"] = rank_utilization(
                first, F.embedding(inputs, model.vocabulary)
            )
    model.train()
    return result


def cell_directory(root: Path, variant: str, lr: float, seed: int) -> Path:
    lr_slug = f"{lr:.8g}".replace(".", "p")
    return root / "cells" / variant / f"lr-{lr_slug}" / f"seed-{seed}"


def load_cell_result(path: Path) -> dict[str, Any] | None:
    if not path.is_file():
        return None
    value = json.loads(path.read_text())
    return value if value.get("schema") == "exp11-cell-v1" else None


def train_cell(
    variant: str,
    lr: float,
    seed: int,
    target_tokens: int,
    *,
    output_root: Path,
    train_windows: np.ndarray,
    validation_windows: np.ndarray,
    batch: int,
    eval_batch: int,
    device: torch.device,
    log: Callable[[dict[str, Any]], None],
    heartbeat: Path | None,
    use_compile: bool,
) -> dict[str, Any]:
    directory = cell_directory(output_root, variant, lr, seed)
    directory.mkdir(parents=True, exist_ok=True)
    result_path = directory / f"result-{target_tokens}.json"
    existing = load_cell_result(result_path)
    if existing and existing.get("status") == "complete":
        return existing

    torch.manual_seed(seed)
    model = build_model(variant).to(device)
    wrapped = torch.compile(model) if use_compile else model
    optimizer = torch.optim.AdamW(
        model.parameters(), lr=lr, betas=(0.9, 0.95), weight_decay=0.01
    )
    checkpoint_path = directory / "checkpoint.pt"
    completed_steps = 0
    if checkpoint_path.is_file():
        saved = torch.load(checkpoint_path, map_location="cpu", weights_only=True)
        identity = {"variant": variant, "lr": lr, "seed": seed}
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
    last: dict[str, Any] = {}
    for step in range(completed_steps, target_steps):
        inputs, targets = tensor_batch(
            train_windows, batch_indices(len(train_windows), step, batch, seed), device
        )
        optimizer.zero_grad(set_to_none=True)
        torch.cuda.synchronize(device)
        step_started = time.perf_counter()
        with torch.autocast("cuda", dtype=torch.bfloat16):
            loss = cross_entropy(wrapped(inputs), targets)
        loss.backward()
        norm = torch.nn.utils.clip_grad_norm_(model.parameters(), 1.0)
        if not torch.isfinite(loss) or not torch.isfinite(norm):
            raise RuntimeError(f"non-finite state in {variant} lr={lr} seed={seed}")
        optimizer.step()
        if not optimizer_is_finite(optimizer):
            raise RuntimeError(f"non-finite optimizer in {variant} lr={lr} seed={seed}")
        torch.cuda.synchronize(device)
        duration = time.perf_counter() - step_started
        if step > completed_steps:
            timed_seconds += duration
            timed_tokens += tokens_per_step
        tokens_seen = (step + 1) * tokens_per_step
        last = {
            "train/variant": variant,
            "train/lr": lr,
            "train/seed": seed,
            "train/step": step + 1,
            "train/tokens_seen": tokens_seen,
            "train/global_examples_per_step": batch,
            "train/global_tokens_per_step": tokens_per_step,
            "train/gradient_accumulation": 1,
            "train/execution_mode": "compiled" if use_compile else "eager",
            "train/nll": float(loss.detach()),
            "train/grad_norm": float(norm.detach()),
            "train/step_seconds": duration,
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
        "schema": "exp11-checkpoint-v1",
        "identity": {"variant": variant, "lr": lr, "seed": seed},
        "config": asdict(model.config),
        "model": model.state_dict(),
        "optimizer": optimizer.state_dict(),
        "steps": target_steps,
        "tokens_seen": target_steps * tokens_per_step,
        "global_examples_per_step": batch,
        "global_tokens_per_step": tokens_per_step,
    }
    checkpoint_temporary = checkpoint_path.with_name(checkpoint_path.name + ".tmp")
    torch.save(saved, checkpoint_temporary)
    checkpoint_temporary.replace(checkpoint_path)
    result = {
        "schema": "exp11-cell-v1",
        "status": "complete",
        "variant": variant,
        "lr": lr,
        "seed": seed,
        "target_tokens": target_tokens,
        "tokens_seen": target_steps * tokens_per_step,
        "inventory": model_inventory(model),
        "validation": validation,
        "performance": last,
        "elapsed_seconds_this_invocation": time.perf_counter() - started,
        "checkpoint": str(checkpoint_path),
    }
    write_json(result_path, result)
    log(
        {
            f"cell/{variant}/lr-{lr}/seed-{seed}/validation_nll": validation["nll"],
            f"cell/{variant}/lr-{lr}/seed-{seed}/tokens": result["tokens_seen"],
        }
    )
    del optimizer, wrapped, model, saved
    gc.collect()
    torch.cuda.empty_cache()
    return result


def load_checkpoint_model(result: dict[str, Any], device: torch.device) -> LanguageModel:
    saved = torch.load(result["checkpoint"], map_location="cpu", weights_only=True)
    model = build_model(result["variant"]).to(device)
    if saved.get("config") != asdict(model.config):
        raise RuntimeError("checkpoint model configuration mismatch")
    model.load_state_dict(saved["model"])
    return model


def bootstrap_interval(values: list[float], *, samples: int = 50_000) -> list[float]:
    generator = np.random.default_rng(1103)
    array = np.asarray(values, dtype=np.float64)
    means = array[generator.integers(0, len(array), size=(samples, len(array)))].mean(1)
    return [float(np.quantile(means, 0.025)), float(np.quantile(means, 0.975))]


def projected_training_seconds(preflight_result: dict[str, Any]) -> float:
    speeds = {
        variant: float(preflight_result["selected"][variant]["tokens_per_second"])
        for variant in VARIANTS
    }
    seconds = 0.0
    for variant in VARIANTS:
        seconds += 3 * SCREEN_TOKENS / speeds[variant]
        seconds += (PROMOTION_TOKENS - SCREEN_TOKENS) / speeds[variant]
    for variant in FINAL_VARIANTS:
        seconds += (FINAL_TOKENS - PROMOTION_TOKENS) / speeds[variant]
        seconds += 2 * FINAL_TOKENS / speeds[variant]
    return seconds


def run_campaign(
    output: str | Path,
    *,
    data_root: str | Path,
    heartbeat: str | Path | None = None,
    wall_limit_seconds: float = 3600.0,
) -> dict[str, Any]:
    if not torch.cuda.is_available() or torch.cuda.device_count() != 1:
        raise RuntimeError("Experiment 11 pilot requires exactly one visible CUDA GPU")
    if not os.environ.get("WANDB_API_KEY"):
        raise RuntimeError("WANDB_API_KEY is required before the paid launch")
    import wandb

    # Authentication and direct URL acquisition precede all paid model work.
    wandb.login(key=os.environ["WANDB_API_KEY"], verify=True)
    run = wandb.init(
        project="exp11-kronecker-debug",
        name="exp11-wikitext-one-gpu-pilot",
        config={
            "schema": SCHEMA,
            "variants": VARIANTS,
            "lr_grids": LR_GRIDS,
            "screen_tokens": SCREEN_TOKENS,
            "promotion_tokens": PROMOTION_TOKENS,
            "final_tokens": FINAL_TOKENS,
            "final_seeds": FINAL_SEEDS,
            "wall_limit_seconds": wall_limit_seconds,
        },
    )
    if not run.url:
        raise RuntimeError("W&B did not provide a direct run URL")
    print(f"WANDB_URL={run.url}", flush=True)
    output_path = Path(output)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    write_json(
        output_path,
        {
            "schema": SCHEMA,
            "status": "running",
            "wandb_url": run.url,
            "started_at_unix_seconds": time.time(),
        },
    )
    root = output_path.parent / "wikitext-cells"
    root.mkdir(parents=True, exist_ok=True)
    heartbeat_path = Path(heartbeat) if heartbeat else None
    log_index = 0

    def log(values: dict[str, Any]) -> None:
        nonlocal log_index
        log_index += 1
        run.log(values, step=log_index)
        if heartbeat_path:
            heartbeat_path.parent.mkdir(parents=True, exist_ok=True)
            heartbeat_path.touch()

    if heartbeat_path:
        heartbeat_path.parent.mkdir(parents=True, exist_ok=True)
        heartbeat_path.touch()

    train_windows = load_windows(Path(data_root), "train")
    validation_windows = load_windows(Path(data_root), "validation")
    test_windows = load_windows(Path(data_root), "test")
    device = torch.device("cuda:0")
    preflight_started = time.perf_counter()
    measured = preflight(train_windows, device, log=log)
    preflight_seconds = time.perf_counter() - preflight_started
    measured["elapsed_seconds"] = preflight_seconds
    write_json(root / "preflight.json", measured)
    projected = projected_training_seconds(measured)
    # Reserve 12% for evaluation, checkpoint I/O, W&B, and lifecycle shutdown.
    available_training_seconds = wall_limit_seconds * 0.88 - preflight_seconds
    if projected > available_training_seconds:
        result = {
            "schema": SCHEMA,
            "status": "complete",
            "verdict": "insufficient_measured_throughput_for_locked_pilot",
            "projected_training_seconds": projected,
            "available_training_seconds": available_training_seconds,
            "preflight": measured,
            "wandb_url": run.url,
        }
        write_json(output_path, result)
        log({"gate/throughput_fit": 0, "projected_training_seconds": projected})
        run.finish()
        return result

    batches = {
        variant: int(measured["selected"][variant]["batch"]) for variant in VARIANTS
    }
    compile_modes = {
        variant: measured["selected"][variant]["execution_mode"] == "compiled"
        for variant in VARIANTS
    }
    eval_batches = {variant: min(64, batches[variant]) for variant in VARIANTS}
    screens: dict[str, list[dict[str, Any]]] = {}
    winners: dict[str, dict[str, Any]] = {}
    for variant in VARIANTS:
        screens[variant] = []
        for lr in LR_GRIDS[variant]:
            screens[variant].append(
                train_cell(
                    variant,
                    lr,
                    0,
                    SCREEN_TOKENS,
                    output_root=root,
                    train_windows=train_windows,
                    validation_windows=validation_windows,
                    batch=batches[variant],
                    eval_batch=eval_batches[variant],
                    device=device,
                    log=log,
                    heartbeat=heartbeat_path,
                    use_compile=compile_modes[variant],
                )
            )
        winner = min(screens[variant], key=lambda row: row["validation"]["nll"])
        winners[variant] = train_cell(
            variant,
            float(winner["lr"]),
            0,
            PROMOTION_TOKENS,
            output_root=root,
            train_windows=train_windows,
            validation_windows=validation_windows,
            batch=batches[variant],
            eval_batch=eval_batches[variant],
            device=device,
            log=log,
            heartbeat=heartbeat_path,
            use_compile=compile_modes[variant],
        )
        # Results remain auditable; only unpromoted optimizer checkpoints are removed.
        for row in screens[variant]:
            if float(row["lr"]) != float(winner["lr"]):
                checkpoint = Path(row["checkpoint"])
                if checkpoint.is_file():
                    checkpoint.unlink()

    improvement = (
        winners["exp10-replica"]["validation"]["nll"]
        - winners["order3-r4"]["validation"]["nll"]
    )
    mechanism_pass = improvement >= 0.05
    if not mechanism_pass:
        result = {
            "schema": SCHEMA,
            "status": "complete",
            "verdict": "debug_mechanism_before_scaling",
            "preflight": measured,
            "screens": screens,
            "promoted": winners,
            "mechanism_gate": {
                "required_nll_improvement_over_exp10_replica": 0.05,
                "observed_nll_improvement": improvement,
                "pass": False,
            },
            "wandb_url": run.url,
        }
        write_json(output_path, result)
        log({"gate/mechanism": 0, "gate/mechanism_nll_improvement": improvement})
        run.finish()
        return result

    finals: dict[str, list[dict[str, Any]]] = {variant: [] for variant in FINAL_VARIANTS}
    for variant in FINAL_VARIANTS:
        lr = float(winners[variant]["lr"])
        for seed in FINAL_SEEDS:
            cell = train_cell(
                variant,
                lr,
                seed,
                FINAL_TOKENS,
                output_root=root,
                train_windows=train_windows,
                validation_windows=validation_windows,
                batch=batches[variant],
                eval_batch=eval_batches[variant],
                device=device,
                log=log,
                heartbeat=heartbeat_path,
                use_compile=compile_modes[variant],
            )
            model = load_checkpoint_model(cell, device)
            cell = {
                **cell,
                "test": evaluate(
                    model,
                    test_windows,
                    eval_batches[variant],
                    device,
                    include_mechanism=True,
                ),
            }
            del model
            finals[variant].append(cell)

    paired = [
        float(kron["test"]["nll"] - transformer["test"]["nll"])
        for kron, transformer in zip(
            finals["order3-r4"], finals["transformer"], strict=True
        )
    ]
    interval = bootstrap_interval(paired)
    kron_body = int(finals["order3-r4"][0]["inventory"]["body_parameters"])
    transformer_body = int(finals["transformer"][0]["inventory"]["body_parameters"])
    body_ratio = kron_body / transformer_body
    quality_body_pass = interval[1] <= 0.02 and body_ratio <= 0.5
    result = {
        "schema": SCHEMA,
        "status": "complete",
        "verdict": "promote_order3_scaling" if quality_body_pass else "do_not_scale_yet",
        "preflight": measured,
        "screens": screens,
        "promoted": winners,
        "finals": finals,
        "mechanism_gate": {
            "required_nll_improvement_over_exp10_replica": 0.05,
            "observed_nll_improvement": improvement,
            "pass": True,
        },
        "matched_comparison": {
            "primary_basis": "equal prediction tokens, quality versus trainable body parameters",
            "all_models_tuned": True,
            "paired_order3_minus_transformer_test_nll": paired,
            "mean_order3_minus_transformer_test_nll": float(np.mean(paired)),
            "bootstrap_95_percent_interval": interval,
            "maximum_allowed_upper_nll_bound": 0.02,
            "order3_to_transformer_body_parameter_ratio": body_ratio,
            "maximum_allowed_body_parameter_ratio": 0.5,
            "pass": quality_body_pass,
        },
        "projected_training_seconds": projected,
        "wandb_url": run.url,
    }
    write_json(output_path, result)
    log(
        {
            "gate/mechanism": 1,
            "gate/quality_body": int(quality_body_pass),
            "comparison/mean_order3_minus_transformer_test_nll": float(np.mean(paired)),
            "comparison/bootstrap_upper": interval[1],
            "comparison/body_parameter_ratio": body_ratio,
        }
    )
    run.finish()
    return result
