"""Bounded Transformer/AdamW positive-control calibration campaign."""

from __future__ import annotations

import os
import time
from pathlib import Path
from typing import Any

from expv2.exp1.utils import atomic_json

from .budget import CalibrationBudget
from .config import ID_ACCURACY_THRESHOLD, TASKS
from .preflight import paid_preflight
from .training import CalibrationRecipe, train_cell


WANDB_PROJECT = "expv2-2-calibrated-ood"
INITIAL_LR = 3e-4
FALLBACK_LRS = (1e-3, 1e-4, 3e-3)
MAXIMUM_CAMPAIGN_SECONDS = 3_000
STOP_RESERVE_SECONDS = 30


class CampaignBudgetError(RuntimeError):
    pass


class Clock:
    def __init__(self) -> None:
        self.started = time.monotonic()
        self.maximum = int(
            os.environ.get("EXPV2_2_MAX_WALL_SECONDS", MAXIMUM_CAMPAIGN_SECONDS)
        )

    @property
    def elapsed(self) -> float:
        return time.monotonic() - self.started

    @property
    def remaining(self) -> float:
        return self.maximum - self.elapsed

    def require(self, seconds: float, label: str) -> None:
        if seconds + STOP_RESERVE_SECONDS > self.remaining:
            raise CampaignBudgetError(
                f"insufficient campaign wall budget for {label}: "
                f"need {seconds + STOP_RESERVE_SECONDS:.1f}s, "
                f"have {self.remaining:.1f}s"
            )


def _wandb_start(output_root: Path) -> tuple[Any, str]:
    key = os.environ.get("WANDB_API_KEY")
    if not key:
        raise RuntimeError("WANDB_API_KEY is required before paid calibration")
    import wandb

    wandb.login(key=key, relogin=True, verify=True)
    run = wandb.init(
        project=WANDB_PROJECT,
        name="expv2-2-transformer-adamw-positive-control",
        job_type="positive-control",
        dir=str(output_root),
        config={
            "stage": "positive-control-only",
            "minimum_optimizer_updates": 1_000,
            "id_accuracy_gate": ID_ACCURACY_THRESHOLD,
            "ood_interpretation_before_id_pass": False,
        },
    )
    url = str(run.url or run.get_url() or "")
    if not url.startswith("http"):
        run.finish(exit_code=1)
        raise RuntimeError("W&B did not provide a direct run URL")
    atomic_json(
        output_root / "wandb-launch.json",
        {
            "schema": "expv2-2-wandb-launch-v1",
            "status": "complete",
            "wandb_url": url,
        },
    )
    return run, url


def _cell(
    *,
    task: str,
    lr: float,
    train_split: str,
    batch_contexts: int,
    output_root: Path,
    run: Any,
    clock: Clock,
    measured_tokens_per_second: float,
) -> dict[str, Any]:
    budget = CalibrationBudget(batch_contexts=batch_contexts)
    projected = budget.training_tokens / measured_tokens_per_second * 1.35 + 10
    clock.require(projected, f"{train_split}/{task}/lr-{lr}")
    result = train_cell(
        "transformer",
        task,  # type: ignore[arg-type]
        CalibrationRecipe("adamw", lr),
        budget,
        seed=0,
        output=(
            output_root
            / "cells"
            / f"{train_split}-{task}-lr-{lr}-batch-{batch_contexts}.json"
        ),
        train_split=train_split,
        run=run,
    )
    for split, metrics in result["evaluations"].items():
        run.log(
            {
                f"evaluation/{task}/{train_split}/{split}/accuracy": metrics["accuracy"],
                f"evaluation/{task}/{train_split}/{split}/loss": metrics["loss"],
                "cell_lr": lr,
                "cell_elapsed_seconds": result["elapsed_seconds"],
            }
        )
    return result


def _best(rows: list[dict[str, Any]], split: str) -> dict[str, Any]:
    return max(rows, key=lambda row: row["evaluations"][split]["accuracy"])


def run_positive_control(
    *,
    output_root: str | Path,
    result_path: str | Path,
    preflight_path: str | Path | None = None,
) -> dict[str, Any]:
    output = Path(output_root)
    output.mkdir(parents=True, exist_ok=True)
    run, wandb_url = _wandb_start(output)
    clock = Clock()
    result: dict[str, Any]
    try:
        preflight = paid_preflight(
            preflight_path or output / "paid-preflight.json",
            wandb_url=wandb_url,
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
                "measured_gpu_utilization_percent": preflight["selected"][
                    "median_gpu_utilization_percent"
                ],
            },
            allow_val_change=True,
        )
        sanity: dict[str, list[dict[str, Any]]] = {}
        selected_lrs: dict[str, float] = {}
        for task in TASKS:
            sanity[task] = []
            for lr in (INITIAL_LR, *FALLBACK_LRS):
                row = _cell(
                    task=task,
                    lr=lr,
                    train_split="sanity",
                    batch_contexts=batch_contexts,
                    output_root=output,
                    run=run,
                    clock=clock,
                    measured_tokens_per_second=throughput,
                )
                sanity[task].append(row)
                if row["evaluations"]["sanity"]["accuracy"] >= ID_ACCURACY_THRESHOLD:
                    break
            winner = _best(sanity[task], "sanity")
            selected_lrs[task] = float(winner["recipe"]["lr"])

        sanity_pass = all(
            _best(rows, "sanity")["evaluations"]["sanity"]["accuracy"]
            >= ID_ACCURACY_THRESHOLD
            for rows in sanity.values()
        )
        id_rows: dict[str, list[dict[str, Any]]] = {}
        if sanity_pass:
            for task in TASKS:
                id_rows[task] = []
                ordered = (selected_lrs[task],) + tuple(
                    lr for lr in (INITIAL_LR, *FALLBACK_LRS) if lr != selected_lrs[task]
                )
                for lr in ordered:
                    row = _cell(
                        task=task,
                        lr=lr,
                        train_split="train",
                        batch_contexts=batch_contexts,
                        output_root=output,
                        run=run,
                        clock=clock,
                        measured_tokens_per_second=throughput,
                    )
                    id_rows[task].append(row)
                    if row["evaluations"]["id"]["accuracy"] >= ID_ACCURACY_THRESHOLD:
                        break
                selected_lrs[task] = float(_best(id_rows[task], "id")["recipe"]["lr"])
        control_valid = sanity_pass and all(
            _best(rows, "id")["evaluations"]["id"]["accuracy"]
            >= ID_ACCURACY_THRESHOLD
            for rows in id_rows.values()
        )
        verdict = (
            "positive_control_pass"
            if control_valid
            else "invalid_task_or_recipe_at_sanity"
            if not sanity_pass
            else "positive_control_failed_id"
        )
        result = {
            "schema": "expv2-2-positive-control-result-v1",
            "status": "complete",
            "wandb_url": wandb_url,
            "verdict": verdict,
            "control_valid": control_valid,
            "selected_lrs": selected_lrs,
            "preflight": preflight,
            "sanity": sanity,
            "id": id_rows,
            "elapsed_seconds": clock.elapsed,
        }
        atomic_json(result_path, result)
        run.summary["verdict"] = verdict
        run.summary["control_valid"] = control_valid
        return result
    except CampaignBudgetError as error:
        result = {
            "schema": "expv2-2-positive-control-result-v1",
            "status": "complete",
            "wandb_url": wandb_url,
            "verdict": "compute_inconclusive",
            "reason": str(error),
            "elapsed_seconds": clock.elapsed,
        }
        atomic_json(result_path, result)
        run.summary["verdict"] = "compute_inconclusive"
        return result
    except Exception as error:
        result = {
            "schema": "expv2-2-positive-control-result-v1",
            "status": "failed",
            "wandb_url": wandb_url,
            "verdict": "infrastructure_or_preflight_failure",
            "reason": repr(error),
            "elapsed_seconds": clock.elapsed,
        }
        atomic_json(result_path, result)
        run.summary["verdict"] = result["verdict"]
        raise
    finally:
        run.finish()
