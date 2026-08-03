"""Full-distribution Transformer control using the debugged curriculum."""

from __future__ import annotations

import gc
import os
from pathlib import Path
from typing import Any

import torch

from expv2.exp1.model import build_model, model_inventory
from expv2.exp1.utils import atomic_json, seed_everything

from .budget import CalibrationBudget
from .campaign import Clock
from .config import ID_ACCURACY_THRESHOLD
from .preflight import paid_preflight
from .training import CalibrationRecipe, _optimizer, train_cell
from .two_hop_debug import DEBUG_LR, DebugKind, _evaluate, _train_mixed_phase


WANDB_PROJECT = "expv2-2-full-control"


def _standard_cell(
    task: str,
    *,
    batch_contexts: int,
    output: Path,
    run: Any,
    clock: Clock,
    throughput: float,
) -> dict[str, Any]:
    budget = CalibrationBudget(batch_contexts=batch_contexts)
    clock.require(budget.training_tokens / throughput * 1.35 + 10, task)
    row = train_cell(
        "transformer",
        task,  # type: ignore[arg-type]
        CalibrationRecipe("adamw", DEBUG_LR),
        budget,
        seed=0,
        output=output,
        train_split="train",
        run=run,
    )
    for split, metrics in row["evaluations"].items():
        run.log(
            {
                f"full-evaluation/{task}/{split}/accuracy": metrics["accuracy"],
                f"full-evaluation/{task}/{split}/loss": metrics["loss"],
            }
        )
    return row


def run_full_control(
    *,
    output_root: str | Path,
    result_path: str | Path,
    preflight_path: str | Path | None = None,
) -> dict[str, Any]:
    output = Path(output_root)
    output.mkdir(parents=True, exist_ok=True)
    key = os.environ.get("WANDB_API_KEY")
    if not key:
        raise RuntimeError("WANDB_API_KEY is required before paid calibration")
    import wandb

    wandb.login(key=key, relogin=True, verify=True)
    run = wandb.init(
        project=WANDB_PROJECT,
        name="expv2-2-transformer-full-id-control",
        job_type="full-positive-control",
        dir=str(output),
        config={
            "optimizer": "adamw",
            "lr": DEBUG_LR,
            "updates_per_phase": 1_000,
            "cloud_only_training": True,
            "id_gate": ID_ACCURACY_THRESHOLD,
            "two_hop_recipe": [
                "concurrent-edges",
                "concurrent-edges-and-two-hop",
                "two-hop-finetune",
            ],
        },
    )
    wandb_url = str(run.url or run.get_url() or "")
    if not wandb_url.startswith("http"):
        run.finish(exit_code=1)
        raise RuntimeError("W&B did not provide a direct run URL")
    atomic_json(
        output / "wandb-launch.json",
        {
            "schema": "expv2-2-wandb-launch-v1",
            "status": "complete",
            "wandb_url": wandb_url,
        },
    )
    clock = Clock()
    try:
        preflight = paid_preflight(
            preflight_path or output / "paid-preflight.json", wandb_url=wandb_url
        )
        if preflight["status"] != "pass":
            raise RuntimeError("paid preflight failed: " + "; ".join(preflight["failures"]))
        batch_contexts = int(preflight["selected"]["batch_contexts"])
        throughput = float(preflight["selected"]["tokens_per_second"])
        run.config.update(
            {
                "global_context_batch": batch_contexts,
                "global_token_batch": batch_contexts * 128,
                "gradient_accumulation_steps": 1,
                "measured_tokens_per_second": throughput,
            },
            allow_val_change=True,
        )
        standard = {
            task: _standard_cell(
                task,
                batch_contexts=batch_contexts,
                output=output / f"full-{task}.json",
                run=run,
                clock=clock,
                throughput=throughput,
            )
            for task in ("delay-copy", "associative-recall")
        }

        seed_everything(0)
        device = torch.device("cuda")
        model = build_model("transformer", vocab_size=128).to(
            device=device, dtype=torch.bfloat16
        )
        optimizer, routing = _optimizer(model, CalibrationRecipe("adamw", DEBUG_LR))
        budget = CalibrationBudget(batch_contexts=batch_contexts)
        schedule: tuple[tuple[str, tuple[DebugKind, ...]], ...] = (
            ("full-concurrent-edges", ("first-edge", "second-edge")),
            (
                "full-concurrent-edges-and-two-hop",
                ("first-edge", "second-edge", "two-hop"),
            ),
            ("full-two-hop-finetune", ("two-hop",)),
        )
        phases: list[dict[str, Any]] = []
        for label, kinds in schedule:
            clock.require(
                budget.training_tokens / throughput * 1.35 + 10,
                label,
            )
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
                output / "full-two-hop.json",
                {
                    "schema": "expv2-2-full-two-hop-v1",
                    "status": "running",
                    "phases": phases,
                },
            )
        two_hop_evaluations = {
            split: _evaluate(
                model,
                "two-hop",
                device=device,
                split=split,
            )
            for split in ("id", "ood-cardinality", "ood-position")
        }
        inventory = model_inventory(model)
        id_accuracies = {
            "delay-copy": standard["delay-copy"]["evaluations"]["id"]["accuracy"],
            "associative-recall": standard["associative-recall"]["evaluations"]["id"]["accuracy"],
            "two-hop-recall": two_hop_evaluations["id"]["accuracy"],
        }
        control_valid = all(
            accuracy >= ID_ACCURACY_THRESHOLD for accuracy in id_accuracies.values()
        )
        result = {
            "schema": "expv2-2-full-control-result-v1",
            "status": "complete",
            "wandb_url": wandb_url,
            "verdict": (
                "full_positive_control_pass"
                if control_valid
                else "full_positive_control_failed_id"
            ),
            "control_valid": control_valid,
            "ood_interpretable": control_valid,
            "id_accuracies": id_accuracies,
            "preflight": preflight,
            "standard_tasks": standard,
            "two_hop": {
                "optimizer_routing": routing,
                "inventory": inventory,
                "phases": phases,
                "evaluations": two_hop_evaluations,
            },
            "elapsed_seconds": clock.elapsed,
        }
        atomic_json(output / "full-two-hop.json", result["two_hop"])
        atomic_json(result_path, result)
        run.summary["verdict"] = result["verdict"]
        run.summary["control_valid"] = control_valid
        for task, accuracy in id_accuracies.items():
            run.summary[f"id_accuracy/{task}"] = accuracy
        del model, optimizer
        gc.collect()
        torch.cuda.empty_cache()
        return result
    except Exception as error:
        atomic_json(
            result_path,
            {
                "schema": "expv2-2-full-control-result-v1",
                "status": "failed",
                "wandb_url": wandb_url,
                "verdict": "infrastructure_or_budget_failure",
                "reason": repr(error),
                "elapsed_seconds": clock.elapsed,
            },
        )
        raise
    finally:
        run.finish()
