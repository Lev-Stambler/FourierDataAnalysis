"""One-GPU synthetic probe suite with a single directly logged W&B run."""

from __future__ import annotations

import json
import os
from pathlib import Path

import torch

from .model import LanguageModel, ModelConfig
from .synthetic import (
    associative_recall_batch,
    delayed_copy_batch,
    probe_accuracy,
    probe_loss,
)


PROBE_VARIANTS = (
    "exp10-replica",
    "order2-balanced",
    "order3-r4",
    "order3-r8",
    "transformer",
)


def write_json(path: Path, value: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(path.name + ".tmp")
    temporary.write_text(json.dumps(value, indent=2, sort_keys=True))
    temporary.replace(path)


def probe_config(variant: str, context_length: int) -> ModelConfig:
    ranks = {
        "exp10-replica": 4,
        "order2-balanced": 3,
        "order3-r4": 4,
        "order3-r8": 8,
        "transformer": 0,
    }
    return ModelConfig(
        variant,
        context_length=context_length,
        vocab_size=32,
        width=32,
        depth=4,
        mode1=4,
        mode2=8,
        rank=ranks[variant],
        heads=4,
        mlp_width=96,
    )


def train_dynamic_probe(
    variant: str,
    task: str,
    *,
    steps: int,
    device: torch.device,
) -> dict[str, float]:
    context = 16 if task == "delayed-copy" else 9
    torch.manual_seed(17)
    model = LanguageModel(probe_config(variant, context)).to(device)
    learning_rate = 0.01 if variant != "transformer" else 0.001
    optimizer = torch.optim.AdamW(
        model.parameters(), lr=learning_rate, weight_decay=0.0
    )

    def batch(seed: int):
        if task == "delayed-copy":
            return delayed_copy_batch(
                64, context, 32, 4, seed=seed, device=device
            )
        return associative_recall_batch(
            64, 4, 16, 16, seed=seed, device=device
        )

    for step in range(steps):
        optimizer.zero_grad(set_to_none=True)
        loss = probe_loss(model, batch(step))
        loss.backward()
        torch.nn.utils.clip_grad_norm_(model.parameters(), 1.0)
        optimizer.step()
    validation = batch(1_000_000)
    return {
        "loss": float(probe_loss(model, validation).detach()),
        "accuracy": probe_accuracy(model, validation),
    }


def run_suite(output: str | Path, *, steps: int = 600) -> dict:
    if not torch.cuda.is_available():
        raise RuntimeError("paid synthetic suite requires one CUDA GPU")
    if torch.cuda.device_count() != 1:
        raise RuntimeError("pilot hard gate requires exactly one GPU")
    if not os.environ.get("WANDB_API_KEY"):
        raise RuntimeError("WANDB_API_KEY is required")
    import wandb

    wandb.login(key=os.environ["WANDB_API_KEY"], verify=True)
    run = wandb.init(
        project="exp11-kronecker-debug",
        name="exp11-synthetic-mechanism-gate",
        config={"variants": PROBE_VARIANTS, "steps_per_task": steps},
    )
    if not run.url:
        raise RuntimeError("W&B did not provide a direct run URL")
    path = Path(output)
    path.parent.mkdir(parents=True, exist_ok=True)
    write_json(
        path,
        {
            "schema": "exp11-synthetic-probes-v1",
            "status": "running",
            "wandb_url": run.url,
        },
    )
    device = torch.device("cuda")
    rows = {}
    for variant in PROBE_VARIANTS:
        rows[variant] = {
            task: train_dynamic_probe(variant, task, steps=steps, device=device)
            for task in ("delayed-copy", "associative-recall")
        }
        run.log(
            {
                f"{variant}/{task}/{metric}": value
                for task, metrics in rows[variant].items()
                for metric, value in metrics.items()
            }
        )
    delayed_pass = rows["order3-r4"]["delayed-copy"]["accuracy"] >= 0.99
    recall_gap = (
        rows["transformer"]["associative-recall"]["accuracy"]
        - rows["order3-r4"]["associative-recall"]["accuracy"]
    )
    passed = delayed_pass and recall_gap <= 0.02
    result = {
        "schema": "exp11-synthetic-probes-v1",
        "status": "complete" if passed else "failed",
        "verdict": "promote" if passed else "debug_before_language_model",
        "rows": rows,
        "gates": {
            "delayed_copy_accuracy_minimum": 0.99,
            "associative_recall_max_gap_to_transformer": 0.02,
            "observed_associative_recall_gap": recall_gap,
        },
        "wandb_url": run.url,
    }
    write_json(path, result)
    run.log({"gate/pass": int(passed)})
    run.finish()
    return result
