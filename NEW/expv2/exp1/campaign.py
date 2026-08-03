"""Strict two-gate ExpV2-1 campaign orchestration."""

from __future__ import annotations

import json
import math
import os
import time
from dataclasses import asdict
from pathlib import Path
from typing import Any, Callable

import numpy as np

from .config import KRONECKER_SHAPES, WANDB_PROJECT
from .data import load_document_ids, load_windows, validate_manifest
from .preflight import paid_preflight
from .synthetic import TASKS
from .training import (
    TrainingRecipe,
    evaluate_checkpoint_on_test,
    train_language_cell,
    train_synthetic_cell,
)
from .utils import atomic_json, canonical_hash


BASE_LRS = (0.003, 0.01, 0.03, 0.1)
COARSE_SYNTHETIC_TOKENS = 5_000_064
ROBUST_SYNTHETIC_TOKENS = 20_000_000
COARSE_LM_TOKENS = 10_000_000
ROBUST_LM_TOKENS = 30_000_000
FINAL_LM_TOKENS = 100_000_000
SYNTHETIC_SEEDS = (0, 1, 2, 3, 4)
ROBUST_LM_SEEDS = (0, 1)
FINAL_LM_SEEDS = (3, 4, 5)
MAX_BOUNDARY_EXTENSIONS = 2
MAX_WALL_SECONDS = 3_300
TEARDOWN_RESERVE_SECONDS = 30


class CampaignBudgetError(RuntimeError):
    pass


class CampaignClock:
    def __init__(self, maximum_seconds: int | None = None) -> None:
        self.started = time.monotonic()
        self.maximum_seconds = int(
            maximum_seconds
            if maximum_seconds is not None
            else os.environ.get("EXPV2_MAX_WALL_SECONDS", MAX_WALL_SECONDS)
        )

    @property
    def elapsed(self) -> float:
        return time.monotonic() - self.started

    @property
    def remaining(self) -> float:
        return self.maximum_seconds - self.elapsed

    def require(self, projected_seconds: float, label: str) -> None:
        if projected_seconds + TEARDOWN_RESERVE_SECONDS > self.remaining:
            raise CampaignBudgetError(
                f"insufficient wall budget for {label}: need approximately "
                f"{projected_seconds + TEARDOWN_RESERVE_SECONDS:.1f}s, "
                f"have {self.remaining:.1f}s"
            )


def _wandb_start(output_root: Path) -> tuple[Any, str]:
    if not os.environ.get("WANDB_API_KEY"):
        raise RuntimeError("WANDB_API_KEY is required before paid training")
    import wandb

    wandb.login(key=os.environ["WANDB_API_KEY"], relogin=True, verify=True)
    run = wandb.init(
        project=WANDB_PROJECT,
        name="expv2-1-strict-two-gate",
        job_type="pilot",
        dir=str(output_root),
        config={
            "optimizer": "pure-batched-muon",
            "context_length": 128,
            "parameter_matching": "body-and-total-below-one-percent",
            "maximum_h100_hours": 1.0,
        },
    )
    url = str(run.get_url() or "")
    if not url.startswith("http"):
        run.finish(exit_code=1)
        raise RuntimeError("W&B did not return a direct run URL")
    return run, url


def _projected_seconds(tokens: int, throughput: float, multiplier: float = 1.5) -> float:
    return max(5.0, tokens / max(throughput, 1.0) * multiplier + 2.0)


def _select_with_boundary(
    initial_rows: list[dict[str, Any]],
    run_lr: Callable[[float], dict[str, Any]],
    objective: Callable[[dict[str, Any]], float],
) -> tuple[dict[str, Any], list[dict[str, Any]], bool]:
    rows = list(initial_rows)
    for _ in range(MAX_BOUNDARY_EXTENSIONS + 1):
        rows.sort(key=objective)
        best = rows[0]
        observed = sorted({float(row["recipe"]["lr"]) for row in rows})
        value = float(best["recipe"]["lr"])
        if observed[0] < value < observed[-1]:
            return best, rows, True
        if len(rows) >= len(initial_rows) + MAX_BOUNDARY_EXTENSIONS:
            return best, rows, False
        next_lr = value / 3.0 if value == observed[0] else value * 3.0
        if next_lr in observed:
            return best, rows, False
        rows.append(run_lr(next_lr))
    return min(rows, key=objective), rows, False


def _synthetic_gate(
    *,
    output_root: Path,
    run: Any,
    batches: dict[str, int],
    throughputs: dict[str, float],
    clock: CampaignClock,
) -> dict[str, Any]:
    variants = (*KRONECKER_SHAPES, "dense", "transformer")
    selected_recipes: dict[str, dict[str, TrainingRecipe]] = {}
    coarse_rows: dict[str, dict[str, list[dict[str, Any]]]] = {}
    boundary_locked = True
    for variant in variants:
        selected_recipes[variant] = {}
        coarse_rows[variant] = {}
        for task in TASKS:
            def run_lr(lr: float) -> dict[str, Any]:
                clock.require(
                    _projected_seconds(COARSE_SYNTHETIC_TOKENS, throughputs[variant]),
                    f"synthetic coarse {variant}/{task}/{lr}",
                )
                return train_synthetic_cell(
                    variant,
                    task,
                    TrainingRecipe(lr),
                    seed=0,
                    training_tokens=COARSE_SYNTHETIC_TOKENS,
                    batch_contexts=batches[variant],
                    output_root=output_root,
                    run=run,
                )

            initial = [run_lr(lr) for lr in BASE_LRS]
            best, observed, locked = _select_with_boundary(
                initial,
                run_lr,
                lambda row: float(row["id"]["loss"] + row["ood"]["loss"]),
            )
            coarse_rows[variant][task] = observed
            selected_recipes[variant][task] = TrainingRecipe(**best["recipe"])
            boundary_locked = boundary_locked and locked

    robust: dict[str, dict[str, list[dict[str, Any]]]] = {}
    for variant in variants:
        robust[variant] = {}
        for task in TASKS:
            rows = []
            for seed in SYNTHETIC_SEEDS:
                clock.require(
                    _projected_seconds(ROBUST_SYNTHETIC_TOKENS, throughputs[variant]),
                    f"synthetic robust {variant}/{task}/seed-{seed}",
                )
                rows.append(
                    train_synthetic_cell(
                        variant,
                        task,
                        selected_recipes[variant][task],
                        seed=seed,
                        training_tokens=ROBUST_SYNTHETIC_TOKENS,
                        batch_contexts=batches[variant],
                        output_root=output_root,
                        run=run,
                    )
                )
            robust[variant][task] = rows

    thresholds = {
        "delay-copy": {"id": 0.99, "ood": 0.95},
        "associative-recall": {"id": 0.95, "ood": 0.90},
        "two-hop-recall": {"id": 0.90, "ood": 0.80},
    }
    summaries: dict[str, Any] = {}
    for variant in variants:
        task_rows = {}
        for task in TASKS:
            task_rows[task] = {
                split: {
                    "mean_accuracy": float(
                        np.mean([row[split]["accuracy"] for row in robust[variant][task]])
                    ),
                    "accuracies": [
                        row[split]["accuracy"] for row in robust[variant][task]
                    ],
                    "mean_loss": float(
                        np.mean([row[split]["loss"] for row in robust[variant][task]])
                    ),
                }
                for split in ("id", "ood")
            }
        summaries[variant] = task_rows

    control_valid = all(
        summaries["transformer"][task][split]["mean_accuracy"]
        >= thresholds[task][split]
        for task in TASKS
        for split in ("id", "ood")
    )
    passing = []
    decisions = {}
    for variant in KRONECKER_SHAPES:
        absolute = all(
            summaries[variant][task][split]["mean_accuracy"]
            >= thresholds[task][split]
            for task in TASKS
            for split in ("id", "ood")
        )
        close_to_transformer = all(
            summaries[variant][task]["id"]["mean_accuracy"]
            >= summaries["transformer"][task]["id"]["mean_accuracy"] - 0.02
            and summaries[variant][task]["ood"]["mean_accuracy"]
            >= summaries["transformer"][task]["ood"]["mean_accuracy"] - 0.05
            for task in TASKS
        )
        dense_seed_wins = 0
        for seed_index in range(len(SYNTHETIC_SEEDS)):
            candidate = np.mean(
                [robust[variant][task][seed_index]["ood"]["accuracy"] for task in TASKS]
            )
            dense = np.mean(
                [robust["dense"][task][seed_index]["ood"]["accuracy"] for task in TASKS]
            )
            dense_seed_wins += int(candidate > dense)
        passed = boundary_locked and control_valid and absolute and close_to_transformer and dense_seed_wins >= 4
        decisions[variant] = {
            "absolute_thresholds": absolute,
            "close_to_transformer": close_to_transformer,
            "dense_seed_wins": dense_seed_wins,
            "pass": passed,
        }
        if passed:
            passing.append(variant)
    passing.sort(
        key=lambda variant: (
            -min(summaries[variant][task]["ood"]["mean_accuracy"] for task in TASKS),
            -np.mean(
                [summaries[variant][task]["ood"]["mean_accuracy"] for task in TASKS]
            ),
        )
    )
    result = {
        "schema": "expv2-1-synthetic-gate-v1",
        "status": "pass" if passing else "fail",
        "control_valid": control_valid,
        "boundary_locked": boundary_locked,
        "selected_recipes": {
            variant: {
                task: asdict(recipe) for task, recipe in tasks.items()
            }
            for variant, tasks in selected_recipes.items()
        },
        "summaries": summaries,
        "decisions": decisions,
        "passing_kronecker_variants": passing,
        "promotion_variants": passing[:2],
        "failure_class": (
            None
            if passing
            else (
                "invalid_control"
                if not control_valid
                else "tuning_inconclusive"
                if not boundary_locked
                else "capability_failure"
            )
        ),
    }
    atomic_json(output_root / "synthetic-gate.json", result)
    return result


def _document_bootstrap(
    candidate_rows: list[np.ndarray],
    control_rows: list[np.ndarray],
    document_ids: np.ndarray,
    *,
    replicates: int = 10_000,
    seed: int = 9821,
) -> dict[str, Any]:
    unique, inverse = np.unique(np.asarray(document_ids), return_inverse=True)
    document_differences = np.zeros((len(candidate_rows), len(unique)), dtype=np.float64)
    counts = np.bincount(inverse)
    for seed_index, (candidate, control) in enumerate(
        zip(candidate_rows, control_rows, strict=True)
    ):
        difference = np.asarray(candidate) - np.asarray(control)
        sums = np.bincount(inverse, weights=difference, minlength=len(unique))
        document_differences[seed_index] = sums / counts
    rng = np.random.default_rng(seed)
    samples = np.empty(replicates, dtype=np.float64)
    for index in range(replicates):
        seed_indices = rng.integers(0, len(candidate_rows), len(candidate_rows))
        document_indices = rng.integers(0, len(unique), len(unique))
        samples[index] = document_differences[seed_indices][:, document_indices].mean()
    return {
        "mean": float(document_differences.mean()),
        "lower_95": float(np.quantile(samples, 0.025)),
        "upper_95": float(np.quantile(samples, 0.975)),
        "documents": len(unique),
        "replicates": replicates,
    }


def _lm_gate(
    *,
    synthetic: dict[str, Any],
    data_root: Path,
    output_root: Path,
    run: Any,
    batches: dict[str, int],
    throughputs: dict[str, float],
    clock: CampaignClock,
) -> dict[str, Any]:
    train = load_windows(data_root, "train")
    validation = load_windows(data_root, "validation")
    variants = (*synthetic["promotion_variants"], "dense", "transformer")
    coarse: dict[str, list[dict[str, Any]]] = {}
    locked = True
    selected_lrs: dict[str, list[float]] = {}
    for variant in variants:
        def run_lr(lr: float) -> dict[str, Any]:
            clock.require(
                _projected_seconds(COARSE_LM_TOKENS, throughputs[variant]),
                f"LM coarse {variant}/{lr}",
            )
            return train_language_cell(
                variant,
                TrainingRecipe(lr),
                train,
                validation,
                seed=0,
                training_tokens=COARSE_LM_TOKENS,
                batch_contexts=batches[variant],
                evaluation_batch_contexts=min(512, batches[variant]),
                output_root=output_root,
                run=run,
            )

        initial = [run_lr(lr) for lr in BASE_LRS]
        best, rows, boundary_locked = _select_with_boundary(
            initial, run_lr, lambda row: float(row["validation"]["nll"])
        )
        coarse[variant] = rows
        locked = locked and boundary_locked
        selected_lrs[variant] = [
            float(row["recipe"]["lr"])
            for row in sorted(rows, key=lambda row: row["validation"]["nll"])[:2]
        ]

    robust: dict[str, list[dict[str, Any]]] = {}
    selected: dict[str, dict[str, Any]] = {}
    for variant in variants:
        rows = []
        for lr in selected_lrs[variant]:
            for schedule in ("constant", "warmup-cosine"):
                for seed in ROBUST_LM_SEEDS:
                    clock.require(
                        _projected_seconds(ROBUST_LM_TOKENS, throughputs[variant]),
                        f"LM robust {variant}/{lr}/{schedule}/seed-{seed}",
                    )
                    rows.append(
                        train_language_cell(
                            variant,
                            TrainingRecipe(lr, schedule=schedule),
                            train,
                            validation,
                            seed=seed,
                            training_tokens=ROBUST_LM_TOKENS,
                            batch_contexts=batches[variant],
                            evaluation_batch_contexts=min(512, batches[variant]),
                            output_root=output_root,
                            run=run,
                        )
                    )
        robust[variant] = rows
        grouped: dict[str, list[dict[str, Any]]] = {}
        for row in rows:
            key = canonical_hash(row["recipe"])
            grouped.setdefault(key, []).append(row)
        complete = [values for values in grouped.values() if len(values) == len(ROBUST_LM_SEEDS)]
        winner = min(
            complete,
            key=lambda values: np.mean([row["validation"]["nll"] for row in values]),
        )
        selected[variant] = {
            "recipe": winner[0]["recipe"],
            "mean_validation_nll": float(
                np.mean([row["validation"]["nll"] for row in winner])
            ),
        }

    candidate = min(
        synthetic["promotion_variants"],
        key=lambda variant: selected[variant]["mean_validation_nll"],
    )
    final_variants = (candidate, "dense", "transformer")
    finals: dict[str, list[dict[str, Any]]] = {}
    for variant in final_variants:
        finals[variant] = []
        recipe = TrainingRecipe(**selected[variant]["recipe"])
        for seed in FINAL_LM_SEEDS:
            clock.require(
                _projected_seconds(FINAL_LM_TOKENS, throughputs[variant]),
                f"LM final {variant}/seed-{seed}",
            )
            finals[variant].append(
                train_language_cell(
                    variant,
                    recipe,
                    train,
                    validation,
                    seed=seed,
                    training_tokens=FINAL_LM_TOKENS,
                    batch_contexts=batches[variant],
                    evaluation_batch_contexts=min(512, batches[variant]),
                    output_root=output_root,
                    run=run,
                )
            )

    # The sealed split is opened only after every recipe and final checkpoint exists.
    test = load_windows(data_root, "test", allow_test=True)
    document_ids = load_document_ids(data_root, "test", allow_test=True)
    test_metrics: dict[str, list[dict[str, Any]]] = {}
    window_rows: dict[str, list[np.ndarray]] = {}
    window_root = output_root / "test-window-nll"
    window_root.mkdir(parents=True, exist_ok=True)
    for variant in final_variants:
        test_metrics[variant], window_rows[variant] = [], []
        for row in finals[variant]:
            metrics = evaluate_checkpoint_on_test(
                row,
                test,
                batch_contexts=min(512, batches[variant]),
                return_window_nll=True,
            )
            values = np.asarray(metrics.pop("window_nll"), dtype=np.float32)
            path = window_root / f"{variant}-seed-{row['seed']}.npy"
            np.save(path, values)
            metrics["window_nll_path"] = str(path)
            test_metrics[variant].append(metrics)
            window_rows[variant].append(values)

    versus_transformer = _document_bootstrap(
        window_rows[candidate], window_rows["transformer"], document_ids
    )
    versus_dense = _document_bootstrap(
        window_rows[candidate], window_rows["dense"], document_ids, seed=9822
    )
    paired_transformer = [
        test_metrics[candidate][index]["nll"]
        - test_metrics["transformer"][index]["nll"]
        for index in range(len(FINAL_LM_SEEDS))
    ]
    paired_dense = [
        test_metrics[candidate][index]["nll"] - test_metrics["dense"][index]["nll"]
        for index in range(len(FINAL_LM_SEEDS))
    ]
    throughput_ratio = np.mean(
        [row["tokens_per_second"] for row in finals[candidate]]
    ) / np.mean([row["tokens_per_second"] for row in finals["transformer"]])
    success = (
        locked
        and all(value < 0 for value in paired_transformer)
        and all(value < 0 for value in paired_dense)
        and float(np.mean(paired_transformer)) <= -0.02
        and versus_transformer["upper_95"] < 0
        and throughput_ratio >= 0.5
    )
    result = {
        "schema": "expv2-1-tinystories-gate-v1",
        "status": "pass" if success else "fail",
        "boundary_locked": locked,
        "candidate": candidate,
        "selected": selected,
        "finals": finals,
        "test": test_metrics,
        "paired_candidate_minus_transformer_nll": paired_transformer,
        "paired_candidate_minus_dense_nll": paired_dense,
        "candidate_minus_transformer_bootstrap": versus_transformer,
        "candidate_minus_dense_bootstrap": versus_dense,
        "candidate_training_throughput_ratio": float(throughput_ratio),
        "success": success,
        "failure_class": (
            None
            if success
            else "tuning_inconclusive"
            if not locked
            else "throughput_failure"
            if throughput_ratio < 0.5
            else "quality_failure"
        ),
    }
    atomic_json(output_root / "tinystories-gate.json", result)
    return result


def run_campaign(
    *,
    data_root: str | Path,
    output_root: str | Path,
    result_path: str | Path,
    preflight_path: str | Path | None = None,
) -> dict[str, Any]:
    data = Path(data_root)
    output = Path(output_root)
    output.mkdir(parents=True, exist_ok=True)
    validate_manifest(data)
    run, wandb_url = _wandb_start(output)
    clock = CampaignClock()
    result: dict[str, Any]
    try:
        preflight_output = Path(preflight_path or output / "paid-preflight.json")
        preflight = paid_preflight(preflight_output, wandb_url=wandb_url)
        if preflight["status"] != "pass":
            raise RuntimeError("paid preflight failed: " + "; ".join(preflight["failures"]))
        batches = {
            variant: int(value)
            for variant, value in preflight["selected_batch_contexts"].items()
        }
        throughputs = {
            variant: float(preflight["batch_sweeps"][variant]["selected"]["tokens_per_second"])
            for variant in batches
        }
        synthetic = _synthetic_gate(
            output_root=output,
            run=run,
            batches=batches,
            throughputs=throughputs,
            clock=clock,
        )
        lm = None
        if synthetic["status"] == "pass":
            lm = _lm_gate(
                synthetic=synthetic,
                data_root=data,
                output_root=output,
                run=run,
                batches=batches,
                throughputs=throughputs,
                clock=clock,
            )
        verdict = (
            "advance_kronecker"
            if lm is not None and lm["success"]
            else "stop_at_synthetic_gate"
            if synthetic["status"] != "pass"
            else "do_not_advance_at_this_scale"
        )
        result = {
            "schema": "expv2-1-campaign-result-v1",
            "status": "complete",
            "wandb_url": wandb_url,
            "verdict": verdict,
            "preflight": preflight,
            "synthetic": synthetic,
            "tinystories": lm,
            "elapsed_seconds": clock.elapsed,
        }
        run.summary["verdict"] = verdict
        run.summary["synthetic_pass"] = synthetic["status"] == "pass"
        run.summary["tinystories_pass"] = bool(lm and lm["success"])
        atomic_json(result_path, result)
        return result
    except CampaignBudgetError as error:
        result = {
            "schema": "expv2-1-campaign-result-v1",
            "status": "complete",
            "wandb_url": wandb_url,
            "verdict": "compute_inconclusive",
            "failure_class": "wall_budget",
            "reason": str(error),
            "elapsed_seconds": clock.elapsed,
        }
        atomic_json(result_path, result)
        run.summary["verdict"] = "compute_inconclusive"
        return result
    except Exception as error:
        result = {
            "schema": "expv2-1-campaign-result-v1",
            "status": "failed",
            "wandb_url": wandb_url,
            "verdict": "infrastructure_or_preflight_failure",
            "reason": repr(error),
            "elapsed_seconds": clock.elapsed,
        }
        atomic_json(result_path, result)
        run.summary["verdict"] = "infrastructure_or_preflight_failure"
        raise
    finally:
        run.finish()


def run_synthetic_only(
    *,
    output_root: str | Path,
    result_path: str | Path,
) -> dict[str, Any]:
    """Public debug entry point that never opens or prepares LM data."""

    output = Path(output_root)
    output.mkdir(parents=True, exist_ok=True)
    run, wandb_url = _wandb_start(output)
    clock = CampaignClock()
    try:
        preflight = paid_preflight(
            output / "paid-preflight.json", wandb_url=wandb_url
        )
        if preflight["status"] != "pass":
            raise RuntimeError("paid preflight failed: " + "; ".join(preflight["failures"]))
        batches = {
            variant: int(value)
            for variant, value in preflight["selected_batch_contexts"].items()
        }
        throughputs = {
            variant: float(preflight["batch_sweeps"][variant]["selected"]["tokens_per_second"])
            for variant in batches
        }
        synthetic = _synthetic_gate(
            output_root=output,
            run=run,
            batches=batches,
            throughputs=throughputs,
            clock=clock,
        )
        result = {
            "schema": "expv2-1-synthetic-only-result-v1",
            "status": "complete",
            "wandb_url": wandb_url,
            "synthetic": synthetic,
            "elapsed_seconds": clock.elapsed,
        }
        atomic_json(result_path, result)
        return result
    finally:
        run.finish()
