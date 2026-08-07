"""Eight-H100 compiled physical-batch sweep for the Exp14 candidate."""

from __future__ import annotations

import argparse
import os
import statistics
import threading
from dataclasses import asdict
from pathlib import Path
from typing import Any

import torch

from exp13_wikitext_confirmation.study import Recipe

from .campaign import CANDIDATE, GPU_COUNT, _all_complete, run_tasks, write_json


SCHEMA = "exp14-compiled-batch-probe-v1"
EXECUTION_MODE = "default"
BATCHES = (1024, 896, 768, 640)
REPEATS = 2
MEASURED_STEPS = 10
MINIMUM_UTILIZATION = 85.0


def summarize_batches(rows: list[dict[str, Any]]) -> dict[str, Any]:
    curves: dict[str, Any] = {}
    stable: list[dict[str, Any]] = []
    for batch in BATCHES:
        selected = [row for row in rows if int(row.get("batch", -1)) == batch]
        complete = [row for row in selected if row.get("status") == "complete"]
        valid = (
            len(complete) == REPEATS
            and all(row.get("finite_forward_backward_optimizer") for row in complete)
            and min(
                float(row["median_gpu_utilization_percent"]) for row in complete
            )
            >= MINIMUM_UTILIZATION
        )
        curve = {
            "batch": batch,
            "global_tokens_per_step": batch * 256,
            "valid": valid,
            "rows": selected,
        }
        if valid:
            curve.update(
                {
                    "median_tokens_per_second": statistics.median(
                        float(row["tokens_per_second"]) for row in complete
                    ),
                    "maximum_peak_allocated_gib": max(
                        float(row["peak_allocated_gib"]) for row in complete
                    ),
                    "maximum_peak_reserved_gib": max(
                        float(row["peak_reserved_gib"]) for row in complete
                    ),
                    "median_gpu_utilization_percent": statistics.median(
                        float(row["median_gpu_utilization_percent"])
                        for row in complete
                    ),
                    "maximum_warmup_seconds": max(
                        float(row["warmup_seconds"]) for row in complete
                    ),
                }
            )
            stable.append(curve)
        curves[str(batch)] = curve
    if not stable:
        raise RuntimeError("no stable compiled batch above the eager selection")
    selected = max(stable, key=lambda row: float(row["median_tokens_per_second"]))
    return {"curves": curves, "selected": selected}


def run_probe(
    output: str | Path,
    *,
    data_root: str | Path,
    heartbeat: str | Path | None = None,
) -> dict[str, Any]:
    if not os.environ.get("WANDB_API_KEY"):
        raise RuntimeError("WANDB_API_KEY is required before paid benchmarking")
    if torch.cuda.device_count() != GPU_COUNT:
        raise RuntimeError("compiled batch probe requires exactly eight GPUs")
    import wandb

    output_path = Path(output)
    wandb.login(key=os.environ["WANDB_API_KEY"], verify=True)
    run = wandb.init(
        project="exp14-block-kronecker",
        name="exp14-compiled-candidate-batch-sweep",
        config={
            "schema": SCHEMA,
            "candidate": CANDIDATE,
            "execution_mode": EXECUTION_MODE,
            "batches": BATCHES,
            "repeats": REPEATS,
            "measured_steps": MEASURED_STEPS,
        },
    )
    if not run.url:
        raise RuntimeError("W&B did not provide a direct run URL")
    print(f"WANDB_URL={run.url}", flush=True)
    write_json(
        output_path,
        {"schema": SCHEMA, "status": "running", "wandb_url": run.url},
    )
    heartbeat_path = Path(heartbeat) if heartbeat else None
    stop = threading.Event()
    heartbeat_thread: threading.Thread | None = None
    if heartbeat_path:
        heartbeat_path.parent.mkdir(parents=True, exist_ok=True)
        heartbeat_path.touch()

        def pulse() -> None:
            while not stop.wait(30):
                heartbeat_path.touch()

        heartbeat_thread = threading.Thread(target=pulse, daemon=True)
        heartbeat_thread.start()
    try:
        agreements = run_tasks(
            [
                {
                    "kind": "loss-agreement",
                    "model": CANDIDATE,
                    "execution_mode": EXECUTION_MODE,
                    "data_root": str(data_root),
                }
                for _ in range(GPU_COUNT)
            ]
        )
        _all_complete(agreements, "compiled exact-loss agreement")
        recipe = Recipe("adamw", 0.003, 0.003)
        benchmarks = run_tasks(
            [
                {
                    "kind": "benchmark",
                    "model": CANDIDATE,
                    "execution_mode": EXECUTION_MODE,
                    "recipe": asdict(recipe),
                    "batch": batch,
                    "measured_steps": MEASURED_STEPS,
                    "data_root": str(data_root),
                }
                for batch in BATCHES
                for _ in range(REPEATS)
            ]
        )
        summary = summarize_batches(benchmarks)
        for index, row in enumerate(benchmarks):
            run.log(
                {
                    "batch_sweep/batch": row.get("batch", 0),
                    "batch_sweep/status": int(row.get("status") == "complete"),
                    "batch_sweep/tokens_per_second": row.get(
                        "tokens_per_second", 0.0
                    ),
                    "batch_sweep/gpu_utilization_percent": row.get(
                        "median_gpu_utilization_percent", 0.0
                    ),
                    "batch_sweep/peak_allocated_gib": row.get(
                        "peak_allocated_gib", 0.0
                    ),
                    "batch_sweep/global_tokens_per_step": row.get(
                        "global_tokens_per_step", 0
                    ),
                },
                step=index,
            )
        result = {
            "schema": SCHEMA,
            "status": "complete",
            "wandb_url": run.url,
            "gpu_count": GPU_COUNT,
            "model": CANDIDATE,
            "execution_mode": EXECUTION_MODE,
            "loss_agreement": agreements,
            "benchmarks": benchmarks,
            "summary": summary,
        }
        write_json(output_path, result)
        run.summary.update(
            {
                "selected_batch": summary["selected"]["batch"],
                "selected_global_tokens_per_step": summary["selected"][
                    "global_tokens_per_step"
                ],
                "selected_tokens_per_second": summary["selected"][
                    "median_tokens_per_second"
                ],
            }
        )
        return result
    finally:
        stop.set()
        if heartbeat_thread is not None:
            heartbeat_thread.join(timeout=2)
        run.finish()


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", required=True)
    parser.add_argument("--data-root", required=True)
    parser.add_argument("--heartbeat")
    arguments = parser.parse_args()
    result = run_probe(
        arguments.output,
        data_root=arguments.data_root,
        heartbeat=arguments.heartbeat,
    )
    print(result["wandb_url"], flush=True)


if __name__ == "__main__":
    main()
