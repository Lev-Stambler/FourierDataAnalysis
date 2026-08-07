"""Cloud-only compiled-versus-eager throughput probe for the Exp14 candidate."""

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

from .campaign import (
    CANDIDATE,
    GPU_COUNT,
    _all_complete,
    run_tasks,
    write_json,
)


SCHEMA = "exp14-compile-probe-v1"
EXECUTION_MODES = (
    "eager",
    "default",
    "reduce-overhead",
)
MODE_REPEATS = {"eager": 2, "default": 3, "reduce-overhead": 3}
BATCH = 512
MEASURED_STEPS = 10
MINIMUM_COMPILED_SPEEDUP = 1.05


def mode_summary(rows: list[dict[str, Any]]) -> dict[str, dict[str, float]]:
    summary: dict[str, dict[str, float]] = {}
    for mode in EXECUTION_MODES:
        selected = [row for row in rows if row.get("execution_mode") == mode]
        if len(selected) != MODE_REPEATS[mode]:
            raise RuntimeError(f"missing {mode} probe repeats")
        _all_complete(selected, f"{mode} compile benchmark")
        summary[mode] = {
            "median_tokens_per_second": statistics.median(
                float(row["tokens_per_second"]) for row in selected
            ),
            "median_gpu_utilization_percent": statistics.median(
                float(row["median_gpu_utilization_percent"]) for row in selected
            ),
            "maximum_peak_allocated_gib": max(
                float(row["peak_allocated_gib"]) for row in selected
            ),
            "maximum_peak_reserved_gib": max(
                float(row["peak_reserved_gib"]) for row in selected
            ),
            "maximum_warmup_seconds": max(
                float(row["warmup_seconds"]) for row in selected
            ),
        }
    eager = summary["eager"]["median_tokens_per_second"]
    for values in summary.values():
        values["speedup_over_eager"] = values["median_tokens_per_second"] / eager
    return summary


def selected_mode(summary: dict[str, dict[str, float]]) -> str:
    compiled = max(
        EXECUTION_MODES[1:],
        key=lambda mode: summary[mode]["median_tokens_per_second"],
    )
    return (
        compiled
        if summary[compiled]["speedup_over_eager"] >= MINIMUM_COMPILED_SPEEDUP
        else "eager"
    )


def run_probe(
    output: str | Path,
    *,
    data_root: str | Path,
    heartbeat: str | Path | None = None,
) -> dict[str, Any]:
    if not os.environ.get("WANDB_API_KEY"):
        raise RuntimeError("WANDB_API_KEY is required before paid benchmarking")
    if torch.cuda.device_count() != GPU_COUNT:
        raise RuntimeError("the compile probe requires exactly eight visible GPUs")
    import wandb

    output_path = Path(output)
    wandb.login(key=os.environ["WANDB_API_KEY"], verify=True)
    run = wandb.init(
        project="exp14-block-kronecker",
        name="exp14-candidate-compile-probe",
        config={
            "schema": SCHEMA,
            "candidate": CANDIDATE,
            "execution_modes": EXECUTION_MODES,
            "repeats": MODE_REPEATS,
            "batch": BATCH,
            "global_tokens_per_step": BATCH * 256,
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
                    "execution_mode": mode,
                    "data_root": str(data_root),
                }
                for mode in EXECUTION_MODES
                for _ in range(MODE_REPEATS[mode])
            ]
        )
        _all_complete(agreements, "compiled BF16 fused-loss agreement")
        recipe = Recipe("muon", 0.03, 0.001)
        benchmarks = run_tasks(
            [
                {
                    "kind": "benchmark",
                    "model": CANDIDATE,
                    "execution_mode": mode,
                    "recipe": asdict(recipe),
                    "batch": BATCH,
                    "measured_steps": MEASURED_STEPS,
                    "data_root": str(data_root),
                }
                for mode in EXECUTION_MODES
                for _ in range(MODE_REPEATS[mode])
            ]
        )
        summary = mode_summary(benchmarks)
        choice = selected_mode(summary)
        for index, row in enumerate(benchmarks):
            run.log(
                {
                    "compile/mode": row["execution_mode"],
                    "compile/tokens_per_second": row["tokens_per_second"],
                    "compile/speedup_over_eager": summary[row["execution_mode"]][
                        "speedup_over_eager"
                    ],
                    "compile/gpu_utilization_percent": row[
                        "median_gpu_utilization_percent"
                    ],
                    "compile/peak_allocated_gib": row["peak_allocated_gib"],
                    "compile/global_tokens_per_step": row["global_tokens_per_step"],
                },
                step=index,
            )
        result = {
            "schema": SCHEMA,
            "status": "complete",
            "wandb_url": run.url,
            "model": CANDIDATE,
            "gpu_count": GPU_COUNT,
            "batch": BATCH,
            "global_tokens_per_step": BATCH * 256,
            "loss_agreement": agreements,
            "benchmarks": benchmarks,
            "summary": summary,
            "selected_execution_mode": choice,
            "minimum_compiled_speedup": MINIMUM_COMPILED_SPEEDUP,
        }
        write_json(output_path, result)
        run.summary.update(
            {
                "selected_execution_mode": choice,
                "selected_speedup_over_eager": summary[choice][
                    "speedup_over_eager"
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
