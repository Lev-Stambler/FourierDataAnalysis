"""Update-budgeted calibration cells for the corrected tasks."""

from __future__ import annotations

import gc
import time
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Any, Literal

import torch

from expv2.exp1.model import build_model, model_inventory
from expv2.exp1.optimizer import build_muon
from expv2.exp1.utils import atomic_json, finite_tree, seed_everything

from .baselines import diagnose_shortcuts
from .budget import CalibrationBudget
from .config import VALID_SPLITS
from .synthetic import Task, make_batch, masked_loss_and_accuracy


OptimizerKind = Literal["adamw", "muon"]


@dataclass(frozen=True)
class CalibrationRecipe:
    optimizer: OptimizerKind
    lr: float
    weight_decay: float = 0.01
    clip_norm: float = 1.0

    def validate(self) -> None:
        if self.optimizer not in ("adamw", "muon"):
            raise ValueError(f"unsupported optimizer: {self.optimizer}")
        if self.lr <= 0 or self.weight_decay < 0 or self.clip_norm <= 0:
            raise ValueError("invalid calibration recipe")


def _optimizer(
    model: torch.nn.Module, recipe: CalibrationRecipe
) -> tuple[torch.optim.Optimizer, dict[str, Any]]:
    if recipe.optimizer == "muon":
        return build_muon(
            model, lr=recipe.lr, weight_decay=recipe.weight_decay
        )
    parameters = list(model.parameters())
    optimizer = torch.optim.AdamW(
        parameters,
        lr=recipe.lr,
        weight_decay=recipe.weight_decay,
        fused=bool(parameters and parameters[0].is_cuda),
    )
    return optimizer, {
        "optimizer": "adamw-positive-control",
        "parameters": sum(parameter.numel() for parameter in model.parameters()),
        "parameter_tensors": len(list(model.parameters())),
        "lr": recipe.lr,
        "weight_decay": recipe.weight_decay,
    }


@torch.inference_mode()
def evaluate(
    model: torch.nn.Module,
    task: Task,
    split: str,
    *,
    examples: int,
    batch_contexts: int,
    seed: int,
    device: torch.device,
) -> dict[str, Any]:
    model.eval()
    total_loss = 0.0
    total_correct = 0.0
    total_targets = 0
    shortcut_rows: list[tuple[int, dict[str, Any]]] = []
    for start in range(0, examples, batch_contexts):
        current = min(batch_contexts, examples - start)
        batch = make_batch(
            task, current, seed=seed + start, split=split, device=device
        )
        loss, accuracy = masked_loss_and_accuracy(
            model(batch.inputs), batch.targets, batch.mask
        )
        count = int(batch.mask.sum())
        total_loss += float(loss) * count
        total_correct += float(accuracy) * count
        total_targets += count
        shortcut_rows.append((count, diagnose_shortcuts(batch)))
    common = set.intersection(*(set(row) for _, row in shortcut_rows))
    shortcuts = {
        key: sum(weight * float(row[key]) for weight, row in shortcut_rows)
        / total_targets
        for key in common
        if key.endswith("accuracy")
        and isinstance(shortcut_rows[0][1][key], (int, float))
    }
    if all("training_fixed_lag_accuracies" in row for _, row in shortcut_rows):
        lags = shortcut_rows[0][1]["training_fixed_lag_accuracies"]
        lag_rows = {
            lag: sum(
                weight * float(row["training_fixed_lag_accuracies"][lag])
                for weight, row in shortcut_rows
            )
            / total_targets
            for lag in lags
        }
        best_lag, best_accuracy = max(lag_rows.items(), key=lambda item: item[1])
        shortcuts.update(
            {
                "training_fixed_lag_accuracies": lag_rows,
                "best_training_fixed_lag": int(best_lag),
                "best_training_fixed_lag_accuracy": best_accuracy,
            }
        )
    return {
        "split": split,
        "loss": total_loss / total_targets,
        "accuracy": total_correct / total_targets,
        "targets": total_targets,
        "shortcut_baselines": shortcuts,
    }


def train_cell(
    variant: str,
    task: Task,
    recipe: CalibrationRecipe,
    budget: CalibrationBudget,
    *,
    seed: int,
    output: str | Path,
    train_split: str = "train",
    evaluation_examples: int = 4_096,
    evaluation_batch_contexts: int = 512,
    device: str | torch.device | None = None,
    run: Any | None = None,
) -> dict[str, Any]:
    output_path = Path(output)
    if output_path.is_file():
        import json

        existing = json.loads(output_path.read_text())
        if existing.get("status") == "complete":
            return existing
    recipe.validate()
    budget.validate()
    if train_split not in ("sanity", "train"):
        raise ValueError("training split must be sanity or train")
    device_value = torch.device(
        device or ("cuda" if torch.cuda.is_available() else "cpu")
    )
    seed_everything(seed)
    model = build_model(variant, vocab_size=128).to(
        device=device_value,
        dtype=torch.bfloat16 if device_value.type == "cuda" else torch.float32,
    )
    optimizer, routing = _optimizer(model, recipe)
    finite = True
    model.train()
    if device_value.type == "cuda":
        torch.cuda.reset_peak_memory_stats(device_value)
        torch.cuda.synchronize(device_value)
    started = time.monotonic()
    for update in range(budget.actual_updates):
        batch = make_batch(
            task,
            budget.batch_contexts,
            seed=seed * 1_000_000_000 + update,
            split=train_split,
            device=device_value,
        )
        optimizer.zero_grad(set_to_none=True)
        loss, accuracy = masked_loss_and_accuracy(
            model(batch.inputs), batch.targets, batch.mask
        )
        loss.backward()
        gradient_norm = torch.nn.utils.clip_grad_norm_(
            model.parameters(), recipe.clip_norm
        )
        optimizer.step()
        finite = finite and bool(torch.isfinite(loss)) and bool(
            torch.isfinite(gradient_norm)
        )
        if run is not None:
            run.log(
                {
                    f"calibration/{variant}/{task}/loss": float(loss),
                    f"calibration/{variant}/{task}/accuracy": float(accuracy),
                    f"calibration/{variant}/{task}/gradient_norm": float(gradient_norm),
                    "optimizer_update": update + 1,
                    "contexts_seen": (update + 1) * budget.batch_contexts,
                    "tokens_seen": (update + 1) * budget.batch_contexts * 128,
                    "global_context_batch": budget.batch_contexts,
                    "global_token_batch": budget.batch_contexts * 128,
                }
            )
    if device_value.type == "cuda":
        torch.cuda.synchronize(device_value)
    elapsed = time.monotonic() - started
    evaluation_splits = (
        ("sanity",)
        if train_split == "sanity"
        else tuple(split for split in VALID_SPLITS[task] if split not in ("sanity", "train"))
    )
    evaluations = {
        split: evaluate(
            model,
            task,
            split,
            examples=evaluation_examples,
            batch_contexts=evaluation_batch_contexts,
            seed=10_000 * (index + 1) + seed,
            device=device_value,
        )
        for index, split in enumerate(evaluation_splits)
    }
    result = {
        "schema": "expv2-2-calibration-cell-v1",
        "status": "complete" if finite and finite_tree(optimizer.state) else "failed",
        "variant": variant,
        "task": task,
        "train_split": train_split,
        "seed": seed,
        "recipe": asdict(recipe),
        "budget": budget.as_dict(),
        "inventory": model_inventory(model),
        "optimizer_routing": routing,
        "evaluations": evaluations,
        "elapsed_seconds": elapsed,
        "tokens_per_second": budget.training_tokens / max(elapsed, 1e-9),
        "finite_training_state": finite and finite_tree(optimizer.state),
        "peak_allocated_gib": (
            torch.cuda.max_memory_allocated(device_value) / 2**30
            if device_value.type == "cuda"
            else 0.0
        ),
    }
    atomic_json(output_path, result)
    del model, optimizer
    gc.collect()
    if device_value.type == "cuda":
        torch.cuda.empty_cache()
    return result
