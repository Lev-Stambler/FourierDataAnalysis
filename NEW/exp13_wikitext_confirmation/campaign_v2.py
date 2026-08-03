"""Compute-corrected Exp13 runner after the v1 compile-preflight timeout."""

from __future__ import annotations

import argparse
import json
from dataclasses import asdict
from pathlib import Path
from typing import Any

import torch

from . import campaign as v1
from .model import CANDIDATE, MODEL_NAMES
from .study import FROZEN_CANDIDATE_RECIPE, Recipe


SCHEMA = "exp13-wikitext-confirmation-v2"
GPU_COUNT = 8
V1_TIMEOUT_SECONDS = 1800
MEASURED_BENCHMARK_STEPS = 10
MINIMUM_UTILIZATION_PERCENT = 85.0
MINIMUM_EIGHT_WAY_SCALING_EFFICIENCY = 0.80
_progress_path: Path | None = None


def _persist(value: dict[str, Any]) -> None:
    if _progress_path is not None:
        v1.write_json(_progress_path, value)


def paid_preflight_v2(data_root: Path) -> dict[str, Any]:
    """Benchmark end-to-end eager cells; never compile a fresh process per cell."""
    if _progress_path is not None and _progress_path.is_file():
        existing = json.loads(_progress_path.read_text())
        if existing.get("status") == "pass":
            return existing
    if torch.cuda.device_count() != GPU_COUNT:
        raise RuntimeError("Exp13 v2 requires exactly eight visible GPUs")
    names = [torch.cuda.get_device_name(index) for index in range(GPU_COUNT)]
    memory_gib = [
        torch.cuda.get_device_properties(index).total_memory / 2**30
        for index in range(GPU_COUNT)
    ]
    if any("H100" not in name for name in names) or any(
        value < 75.0 for value in memory_gib
    ):
        raise RuntimeError(
            f"Exp13 v2 requires exactly 8xH100 80GB; found {names} / {memory_gib}"
        )
    representatives = {
        "adamw": Recipe("adamw", 0.003, 0.003),
        "muon": Recipe("muon", 0.03, 0.001),
    }
    curves: dict[str, Any] = {}
    selected: dict[str, Any] = {}
    progress: dict[str, Any] = {
        "schema": "exp13-eager-preflight-v2",
        "status": "running",
        "gpu_names": names,
        "gpu_memory_gib": memory_gib,
        "compilation_policy": {
            "status": "rejected_for_campaign",
            "reason": (
                "v1 spent its full 1,800-second eight-H100 budget repeatedly "
                "compiling fresh benchmark processes and never completed preflight"
            ),
            "v1_wall_seconds": V1_TIMEOUT_SECONDS,
            "training_execution_mode": "eager",
        },
        "curves": curves,
        "selected": selected,
    }
    _persist(progress)
    agreement_tasks = [
        {"kind": "loss-agreement", "model": name, "data_root": str(data_root)}
        for name in MODEL_NAMES
    ]
    agreement_rows = v1.run_tasks(v1._pad_to_full_node(agreement_tasks))[
        : len(agreement_tasks)
    ]
    v1._all_complete(agreement_rows, "v2 BF16/FP32 loss agreement")
    progress["loss_agreement"] = dict(zip(MODEL_NAMES, agreement_rows, strict=True))
    _persist(progress)

    def eager_benchmark_task(
        name: str, recipe: Recipe, batch: int
    ) -> dict[str, Any]:
        task = v1._benchmark_task(
            name, recipe, batch, data_root, compiled=False
        )
        task["measured_steps"] = MEASURED_BENCHMARK_STEPS
        return task

    for family, recipe in representatives.items():
        active = list(MODEL_NAMES)
        stable: dict[str, list[dict[str, Any]]] = {
            name: [] for name in MODEL_NAMES
        }
        failures: dict[str, list[dict[str, Any]]] = {
            name: [] for name in MODEL_NAMES
        }
        for batch in v1.PREFLIGHT_BATCHES:
            if not active:
                break
            tasks = [
                eager_benchmark_task(name, recipe, batch)
                for name in active
            ]
            rows = v1.run_tasks(v1._pad_to_full_node(tasks))[: len(tasks)]
            next_active = []
            for name, row in zip(active, rows, strict=True):
                if (
                    row["status"] == "complete"
                    and row["finite_forward_backward_optimizer"]
                    and row.get("gpu_samples", 0) > 0
                    and row.get("median_gpu_utilization_percent", 0.0)
                    >= MINIMUM_UTILIZATION_PERCENT
                ):
                    stable[name].append(row)
                    if len(stable[name]) < 3:
                        next_active.append(name)
                else:
                    failures[name].append({
                        "batch": batch,
                        "reason": row.get(
                            "failure", "finite_or_utilization_gate_failed"
                        ),
                        "median_gpu_utilization_percent": row.get(
                            "median_gpu_utilization_percent"
                        ),
                    })
                    next_active.append(name)
            active = next_active
            progress["last_completed_batch"] = {"family": family, "batch": batch}
            _persist(progress)
        baseline_tasks = [
            eager_benchmark_task(name, recipe, 128)
            for name in MODEL_NAMES
        ]
        baselines = v1.run_tasks(v1._pad_to_full_node(baseline_tasks))[
            : len(baseline_tasks)
        ]
        for name, baseline in zip(MODEL_NAMES, baselines, strict=True):
            if not stable[name]:
                raise RuntimeError(f"no stable eager {family} batch for {name}")
            key = f"{name}/{family}"
            selected[key] = max(
                stable[name], key=lambda row: row["tokens_per_second"]
            )
            curves[key] = {
                "measurements": stable[name],
                "underfilled_baseline": baseline,
                "selected_over_underfilled_throughput": (
                    float(selected[key]["tokens_per_second"])
                    / float(baseline["tokens_per_second"])
                    if baseline["status"] == "complete"
                    else None
                ),
                "failures": failures[name],
            }
        _persist(progress)
    common_batch = min(
        int(selected[f"{name}/muon"]["batch"]) for name in MODEL_NAMES
    )
    common_tasks = [
        eager_benchmark_task(name, representatives["muon"], common_batch)
        for name in MODEL_NAMES
    ]
    common_rows = v1.run_tasks(v1._pad_to_full_node(common_tasks))[
        : len(common_tasks)
    ]
    v1._all_complete(common_rows, "v2 common-batch preflight")
    common = dict(zip(MODEL_NAMES, common_rows, strict=True))
    scaling = {}
    for count in (1, 2, 4, 8):
        rows = v1.run_tasks(
            [
                eager_benchmark_task(
                    CANDIDATE, FROZEN_CANDIDATE_RECIPE, common_batch
                )
                for _ in range(count)
            ],
            workers=count,
        )
        v1._all_complete(rows, f"v2 {count}-worker scaling")
        aggregate = sum(float(row["tokens_per_second"]) for row in rows)
        scaling[str(count)] = {
            "aggregate_tokens_per_second": aggregate,
            "per_gpu_tokens_per_second": aggregate / count,
            "workers": rows,
        }
        progress["parallel_scaling"] = scaling
        _persist(progress)
    scaling_efficiency = (
        float(scaling["8"]["aggregate_tokens_per_second"])
        / (8.0 * float(scaling["1"]["aggregate_tokens_per_second"]))
    )
    if scaling_efficiency < MINIMUM_EIGHT_WAY_SCALING_EFFICIENCY:
        raise RuntimeError(
            f"eight-way cell-parallel scaling efficiency {scaling_efficiency:.3f} "
            f"is below {MINIMUM_EIGHT_WAY_SCALING_EFFICIENCY:.3f}"
        )
    result = {
        **progress,
        "status": "pass",
        "gpu_count": GPU_COUNT,
        "common_batch": common_batch,
        "common_batch_measurements": common,
        "parallel_scaling": scaling,
        "eight_way_scaling_efficiency": scaling_efficiency,
        "minimum_eight_way_scaling_efficiency": MINIMUM_EIGHT_WAY_SCALING_EFFICIENCY,
        "minimum_gpu_utilization_percent": MINIMUM_UTILIZATION_PERCENT,
        "gradient_accumulation": 1,
        "execution_mode": "eager",
    }
    _persist(result)
    return result


def _eager_train_task(*args: Any, **kwargs: Any) -> dict[str, Any]:
    kwargs["compiled"] = False
    return _original_train_task(*args, **kwargs)


_original_preflight = v1.paid_preflight
_original_train_task = v1._train_task
_original_schema = v1.SCHEMA


def run_campaign_v2(
    output: str | Path,
    *,
    data_root: str | Path,
    holdout_root: str | Path,
    heartbeat: str | Path | None = None,
) -> dict[str, Any]:
    global _progress_path
    output_path = Path(output)
    _progress_path = output_path.parent / "preflight-v2-progress.json"
    v1.paid_preflight = paid_preflight_v2
    v1._train_task = _eager_train_task
    v1.SCHEMA = SCHEMA
    try:
        return v1.run_campaign(
            output_path,
            data_root=data_root,
            holdout_root=holdout_root,
            heartbeat=heartbeat,
        )
    finally:
        v1.paid_preflight = _original_preflight
        v1._train_task = _original_train_task
        v1.SCHEMA = _original_schema
        _progress_path = None


def main() -> None:
    parser = argparse.ArgumentParser(description="Compute-corrected Exp13 v2")
    parser.add_argument("--output", required=True)
    parser.add_argument("--data-root", required=True)
    parser.add_argument("--holdout-root", required=True)
    parser.add_argument("--heartbeat")
    args = parser.parse_args()
    result = run_campaign_v2(
        args.output,
        data_root=args.data_root,
        holdout_root=args.holdout_root,
        heartbeat=args.heartbeat,
    )
    print(json.dumps(result, indent=2, sort_keys=True))


if __name__ == "__main__":
    main()
