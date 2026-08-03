"""Token-budgeted, checkpointable pure-Muon training helpers."""

from __future__ import annotations

import gc
import hashlib
import json
import math
import time
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Any, Literal

import numpy as np
import torch
import torch.nn.functional as F

from .config import CONTEXT_LENGTH, SYNTHETIC_VOCAB_SIZE
from .model import LanguageModel, build_model, model_inventory
from .optimizer import BatchedMuon, build_muon, set_lr
from .synthetic import Task, make_batch, masked_loss_and_accuracy
from .utils import atomic_json, canonical_hash, finite_tree, seed_everything


Schedule = Literal["constant", "warmup-cosine"]


@dataclass(frozen=True)
class TrainingRecipe:
    lr: float
    schedule: Schedule = "constant"
    weight_decay: float = 0.01
    momentum: float = 0.95
    nesterov: bool = True
    ns_steps: int = 5
    clip_norm: float = 1.0
    warmup_fraction: float = 0.05
    minimum_lr_ratio: float = 0.1

    def validate(self) -> None:
        if self.lr <= 0 or self.weight_decay < 0 or self.clip_norm <= 0:
            raise ValueError("invalid training recipe")
        if self.schedule not in ("constant", "warmup-cosine"):
            raise ValueError("invalid schedule")
        if not 0 <= self.momentum < 1 or self.ns_steps <= 0:
            raise ValueError("invalid Muon recipe")
        if not 0 <= self.warmup_fraction < 1 or not 0 <= self.minimum_lr_ratio <= 1:
            raise ValueError("invalid LR schedule")


def schedule_multiplier(
    recipe: TrainingRecipe, tokens_seen: int, horizon_tokens: int
) -> float:
    if recipe.schedule == "constant":
        return 1.0
    warmup = max(CONTEXT_LENGTH, int(horizon_tokens * recipe.warmup_fraction))
    if tokens_seen < warmup:
        return max(CONTEXT_LENGTH, tokens_seen) / warmup
    progress = min(1.0, (tokens_seen - warmup) / max(1, horizon_tokens - warmup))
    cosine = 0.5 * (1.0 + math.cos(math.pi * progress))
    return recipe.minimum_lr_ratio + (1.0 - recipe.minimum_lr_ratio) * cosine


def _device_dtype(device: torch.device) -> torch.dtype:
    return torch.bfloat16 if device.type == "cuda" else torch.float32


def _cell_slug(value: dict[str, Any]) -> str:
    return canonical_hash(value)[:20]


def _save_checkpoint(
    path: Path,
    *,
    model: LanguageModel,
    optimizer: BatchedMuon,
    tokens_seen: int,
    metadata: dict[str, Any],
) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(path.name + ".tmp")
    torch.save(
        {
            "schema": "expv2-1-checkpoint-v1",
            "model": model.state_dict(),
            "optimizer": optimizer.state_dict(),
            "tokens_seen": tokens_seen,
            "torch_rng_state": torch.get_rng_state(),
            "cuda_rng_state": (
                torch.cuda.get_rng_state_all() if torch.cuda.is_available() else None
            ),
            "metadata": metadata,
        },
        temporary,
    )
    temporary.replace(path)


def _load_checkpoint(
    path: Path,
    *,
    model: LanguageModel,
    optimizer: BatchedMuon,
    device: torch.device,
) -> int:
    value = torch.load(path, map_location=device, weights_only=False)
    if value.get("schema") != "expv2-1-checkpoint-v1":
        raise RuntimeError("checkpoint schema mismatch")
    model.load_state_dict(value["model"])
    optimizer.load_state_dict(value["optimizer"])
    torch.set_rng_state(value["torch_rng_state"].cpu())
    if device.type == "cuda" and value.get("cuda_rng_state") is not None:
        torch.cuda.set_rng_state_all(value["cuda_rng_state"])
    return int(value["tokens_seen"])


def _log(run: Any | None, values: dict[str, Any], step: int) -> None:
    if run is not None:
        # Each independently tuned cell has its own local step counter.  Let
        # W&B own the monotonically increasing run step and preserve the cell
        # counter as an ordinary metric instead of dropping later-cell logs.
        run.log({**values, "cell_step": step})


@torch.inference_mode()
def evaluate_synthetic(
    model: LanguageModel,
    task: Task,
    *,
    split: str,
    batch_size: int,
    examples: int,
    seed: int,
    device: torch.device,
) -> dict[str, Any]:
    model.eval()
    losses: list[float] = []
    accuracies: list[float] = []
    weights: list[int] = []
    for start in range(0, examples, batch_size):
        current = min(batch_size, examples - start)
        batch = make_batch(
            task, current, seed=seed + start, split=split, device=device
        )
        logits = model(batch.inputs)
        loss, accuracy = masked_loss_and_accuracy(logits, batch.targets, batch.mask)
        count = int(batch.mask.sum().item())
        losses.append(float(loss.item()))
        accuracies.append(float(accuracy.item()))
        weights.append(count)
    total = sum(weights)
    return {
        "loss": sum(value * weight for value, weight in zip(losses, weights, strict=True)) / total,
        "accuracy": sum(value * weight for value, weight in zip(accuracies, weights, strict=True)) / total,
        "targets": total,
    }


def train_synthetic_cell(
    variant: str,
    task: Task,
    recipe: TrainingRecipe,
    *,
    seed: int,
    training_tokens: int,
    batch_contexts: int,
    output_root: str | Path,
    eval_examples: int = 4_096,
    run: Any | None = None,
    device: str | torch.device | None = None,
) -> dict[str, Any]:
    recipe.validate()
    if training_tokens <= 0 or training_tokens % CONTEXT_LENGTH:
        raise ValueError("synthetic training tokens must be positive and divide by context")
    target_contexts = training_tokens // CONTEXT_LENGTH
    device_value = torch.device(
        device or ("cuda" if torch.cuda.is_available() else "cpu")
    )
    metadata = {
        "kind": "synthetic",
        "variant": variant,
        "task": task,
        "recipe": asdict(recipe),
        "seed": seed,
        "training_tokens": training_tokens,
        "batch_contexts": batch_contexts,
    }
    root = Path(output_root) / "synthetic" / _cell_slug(metadata)
    result_path, checkpoint_path = root / "result.json", root / "checkpoint.pt"
    if result_path.is_file():
        return json.loads(result_path.read_text())
    seed_everything(seed)
    model = build_model(variant, vocab_size=SYNTHETIC_VOCAB_SIZE).to(
        device_value, dtype=_device_dtype(device_value)
    )
    optimizer, routing = build_muon(
        model,
        lr=recipe.lr,
        weight_decay=recipe.weight_decay,
        momentum=recipe.momentum,
        nesterov=recipe.nesterov,
        ns_steps=recipe.ns_steps,
    )
    contexts_seen = 0
    if checkpoint_path.is_file():
        contexts_seen = _load_checkpoint(
            checkpoint_path, model=model, optimizer=optimizer, device=device_value
        ) // CONTEXT_LENGTH
    model.train()
    if device_value.type == "cuda":
        torch.cuda.reset_peak_memory_stats(device_value)
        torch.cuda.synchronize(device_value)
    started = time.monotonic()
    step = math.ceil(contexts_seen / max(1, batch_contexts))
    while contexts_seen < target_contexts:
        current = min(batch_contexts, target_contexts - contexts_seen)
        batch = make_batch(
            task,
            current,
            seed=seed * 1_000_000_000 + contexts_seen,
            split="train",
            device=device_value,
        )
        optimizer.zero_grad(set_to_none=True)
        logits = model(batch.inputs)
        loss, accuracy = masked_loss_and_accuracy(logits, batch.targets, batch.mask)
        loss.backward()
        gradient_norm = torch.nn.utils.clip_grad_norm_(
            model.parameters(), recipe.clip_norm
        )
        tokens_after = (contexts_seen + current) * CONTEXT_LENGTH
        multiplier = schedule_multiplier(recipe, tokens_after, training_tokens)
        set_lr(optimizer, recipe.lr * multiplier)
        optimizer.step()
        contexts_seen += current
        step += 1
        _log(
            run,
            {
                f"synthetic/{variant}/{task}/train_loss": float(loss.item()),
                f"synthetic/{variant}/{task}/train_accuracy": float(accuracy.item()),
                f"synthetic/{variant}/{task}/gradient_norm": float(gradient_norm),
                f"synthetic/{variant}/{task}/lr": recipe.lr * multiplier,
                f"synthetic/{variant}/{task}/tokens": contexts_seen * CONTEXT_LENGTH,
            },
            step,
        )
    if device_value.type == "cuda":
        torch.cuda.synchronize(device_value)
    elapsed = time.monotonic() - started
    id_metrics = evaluate_synthetic(
        model,
        task,
        split="id",
        batch_size=min(batch_contexts, 512),
        examples=eval_examples,
        seed=10_000 + seed,
        device=device_value,
    )
    ood_metrics = evaluate_synthetic(
        model,
        task,
        split="ood",
        batch_size=min(batch_contexts, 512),
        examples=eval_examples,
        seed=20_000 + seed,
        device=device_value,
    )
    _save_checkpoint(
        checkpoint_path,
        model=model,
        optimizer=optimizer,
        tokens_seen=training_tokens,
        metadata=metadata,
    )
    result = {
        "schema": "expv2-1-synthetic-cell-v1",
        "status": "complete",
        **metadata,
        "inventory": model_inventory(model),
        "optimizer_routing": routing,
        "id": id_metrics,
        "ood": ood_metrics,
        "elapsed_seconds": elapsed,
        "tokens_per_second": training_tokens / max(elapsed, 1e-9),
        "peak_allocated_gib": (
            torch.cuda.max_memory_allocated(device_value) / 2**30
            if device_value.type == "cuda"
            else 0.0
        ),
        "checkpoint": str(checkpoint_path),
        "optimizer_state_finite": finite_tree(optimizer.state),
    }
    atomic_json(result_path, result)
    del model, optimizer
    gc.collect()
    if device_value.type == "cuda":
        torch.cuda.empty_cache()
    return result


@torch.inference_mode()
def evaluate_language_model(
    model: LanguageModel,
    windows: np.ndarray,
    *,
    batch_contexts: int,
    device: torch.device,
    return_window_nll: bool = False,
) -> dict[str, Any]:
    model.eval()
    total_loss, total_tokens = 0.0, 0
    window_losses: list[float] = []
    for start in range(0, len(windows), batch_contexts):
        values = torch.from_numpy(
            np.asarray(windows[start : start + batch_contexts]).astype(np.int64)
        ).to(device, non_blocking=True)
        inputs, targets = values[:, :-1], values[:, 1:]
        logits = model(inputs)
        token_loss = F.cross_entropy(
            logits.float().flatten(0, 1),
            targets.flatten(),
            reduction="none",
        ).reshape(targets.shape)
        loss = token_loss.sum()
        total_loss += float(loss.item())
        total_tokens += targets.numel()
        if return_window_nll:
            window_losses.extend(token_loss.mean(1).cpu().tolist())
    nll = total_loss / total_tokens
    result: dict[str, Any] = {
        "nll": nll,
        "perplexity": math.exp(min(nll, 20)),
        "tokens": total_tokens,
    }
    if return_window_nll:
        result["window_nll"] = window_losses
    return result


def train_language_cell(
    variant: str,
    recipe: TrainingRecipe,
    train_windows: np.ndarray,
    validation_windows: np.ndarray,
    *,
    seed: int,
    training_tokens: int,
    batch_contexts: int,
    evaluation_batch_contexts: int,
    output_root: str | Path,
    run: Any | None = None,
    device: str | torch.device | None = None,
) -> dict[str, Any]:
    recipe.validate()
    if training_tokens <= 0 or training_tokens % CONTEXT_LENGTH:
        raise ValueError("LM training tokens must be positive and divide by context")
    target_contexts = training_tokens // CONTEXT_LENGTH
    device_value = torch.device(
        device or ("cuda" if torch.cuda.is_available() else "cpu")
    )
    metadata = {
        "kind": "tinystories",
        "variant": variant,
        "recipe": asdict(recipe),
        "seed": seed,
        "training_tokens": training_tokens,
        "batch_contexts": batch_contexts,
        "evaluation_batch_contexts": evaluation_batch_contexts,
    }
    root = Path(output_root) / "tinystories" / _cell_slug(metadata)
    result_path, checkpoint_path = root / "result.json", root / "checkpoint.pt"
    if result_path.is_file():
        return json.loads(result_path.read_text())
    seed_everything(seed)
    generator = np.random.default_rng(seed)
    model = build_model(variant).to(device_value, dtype=_device_dtype(device_value))
    optimizer, routing = build_muon(
        model,
        lr=recipe.lr,
        weight_decay=recipe.weight_decay,
        momentum=recipe.momentum,
        nesterov=recipe.nesterov,
        ns_steps=recipe.ns_steps,
    )
    contexts_seen = 0
    if checkpoint_path.is_file():
        contexts_seen = _load_checkpoint(
            checkpoint_path, model=model, optimizer=optimizer, device=device_value
        ) // CONTEXT_LENGTH
        generator = np.random.default_rng(seed + contexts_seen)
    model.train()
    if device_value.type == "cuda":
        torch.cuda.reset_peak_memory_stats(device_value)
        torch.cuda.synchronize(device_value)
    started = time.monotonic()
    step = math.ceil(contexts_seen / max(1, batch_contexts))
    while contexts_seen < target_contexts:
        current = min(batch_contexts, target_contexts - contexts_seen)
        indices = generator.integers(0, len(train_windows), size=current)
        array = np.asarray(train_windows[indices]).astype(np.int64)
        values = torch.from_numpy(array).to(device_value, non_blocking=True)
        inputs, targets = values[:, :-1], values[:, 1:]
        optimizer.zero_grad(set_to_none=True)
        logits = model(inputs)
        loss = F.cross_entropy(logits.float().flatten(0, 1), targets.flatten())
        loss.backward()
        gradient_norm = torch.nn.utils.clip_grad_norm_(
            model.parameters(), recipe.clip_norm
        )
        tokens_after = (contexts_seen + current) * CONTEXT_LENGTH
        multiplier = schedule_multiplier(recipe, tokens_after, training_tokens)
        set_lr(optimizer, recipe.lr * multiplier)
        optimizer.step()
        contexts_seen += current
        step += 1
        _log(
            run,
            {
                f"tinystories/{variant}/train_nll": float(loss.item()),
                f"tinystories/{variant}/gradient_norm": float(gradient_norm),
                f"tinystories/{variant}/lr": recipe.lr * multiplier,
                f"tinystories/{variant}/tokens": contexts_seen * CONTEXT_LENGTH,
                f"tinystories/{variant}/global_context_batch": current,
                f"tinystories/{variant}/global_token_batch": current * CONTEXT_LENGTH,
            },
            step,
        )
    if device_value.type == "cuda":
        torch.cuda.synchronize(device_value)
    elapsed = time.monotonic() - started
    validation = evaluate_language_model(
        model,
        validation_windows,
        batch_contexts=evaluation_batch_contexts,
        device=device_value,
    )
    _save_checkpoint(
        checkpoint_path,
        model=model,
        optimizer=optimizer,
        tokens_seen=training_tokens,
        metadata=metadata,
    )
    result = {
        "schema": "expv2-1-lm-cell-v1",
        "status": "complete",
        **metadata,
        "inventory": model_inventory(model),
        "optimizer_routing": routing,
        "validation": validation,
        "elapsed_seconds": elapsed,
        "tokens_per_second": training_tokens / max(elapsed, 1e-9),
        "peak_allocated_gib": (
            torch.cuda.max_memory_allocated(device_value) / 2**30
            if device_value.type == "cuda"
            else 0.0
        ),
        "peak_reserved_gib": (
            torch.cuda.max_memory_reserved(device_value) / 2**30
            if device_value.type == "cuda"
            else 0.0
        ),
        "checkpoint": str(checkpoint_path),
        "optimizer_state_finite": finite_tree(optimizer.state),
    }
    atomic_json(result_path, result)
    del model, optimizer
    gc.collect()
    if device_value.type == "cuda":
        torch.cuda.empty_cache()
    return result


def evaluate_checkpoint_on_test(
    result: dict[str, Any],
    test_windows: np.ndarray,
    *,
    batch_contexts: int,
    return_window_nll: bool = False,
    device: str | torch.device | None = None,
) -> dict[str, Any]:
    device_value = torch.device(
        device or ("cuda" if torch.cuda.is_available() else "cpu")
    )
    model = build_model(result["variant"]).to(
        device_value, dtype=_device_dtype(device_value)
    )
    checkpoint = torch.load(result["checkpoint"], map_location=device_value, weights_only=False)
    model.load_state_dict(checkpoint["model"])
    metrics = evaluate_language_model(
        model,
        test_windows,
        batch_contexts=batch_contexts,
        device=device_value,
        return_window_nll=return_window_nll,
    )
    del model
    return metrics
