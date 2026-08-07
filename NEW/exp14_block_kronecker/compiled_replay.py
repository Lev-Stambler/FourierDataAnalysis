"""Compiled eight-H100 replay of the tuned Exp14 candidate and ablation."""

from __future__ import annotations

import argparse
import os
import threading
from dataclasses import asdict
from pathlib import Path
from typing import Any

import torch

from exp13_wikitext_confirmation.study import Recipe

from .campaign import (
    CANDIDATE,
    FINAL_SEEDS,
    FINAL_TOKENS,
    GPU_COUNT,
    NO_WORKSPACE_PERMUTATION,
    _all_complete,
    _train_task,
    run_tasks,
    write_json,
)


SCHEMA = "exp14-compiled-replay-v1"
EXECUTION_MODE = "default"
BATCH = 640
RECIPE = Recipe("adamw", 0.003, 0.003)
FROZEN_EAGER_CANDIDATE = {
    3: 7.391438030374461,
    4: 7.390633726824681,
    5: 7.388037331116023,
    6: 7.388819355330444,
}
FROZEN_LOCKED_TRANSFORMER = {
    3: 7.411661307799992,
    4: 7.4141100465370515,
    5: 7.411387792126885,
    6: 7.414455000402892,
}


def nll_by_seed(rows: list[dict[str, Any]], model: str) -> dict[int, float]:
    return {
        int(row["seed"]): float(row["validation"]["nll"])
        for row in rows
        if row["model"] == model
    }


def run_replay(
    output: str | Path,
    *,
    data_root: str | Path,
    heartbeat: str | Path | None = None,
) -> dict[str, Any]:
    if not os.environ.get("WANDB_API_KEY"):
        raise RuntimeError("WANDB_API_KEY is required before paid training")
    if torch.cuda.device_count() != GPU_COUNT:
        raise RuntimeError("compiled replay requires exactly eight GPUs")
    import wandb

    output_path = Path(output)
    cells = output_path.parent / "compiled-replay-cells"
    wandb.login(key=os.environ["WANDB_API_KEY"], verify=True)
    run = wandb.init(
        project="exp14-block-kronecker",
        name="exp14-compiled-final-replay",
        config={
            "schema": SCHEMA,
            "models": (CANDIDATE, NO_WORKSPACE_PERMUTATION),
            "execution_mode": EXECUTION_MODE,
            "batch": BATCH,
            "global_tokens_per_step": BATCH * 256,
            "target_tokens": FINAL_TOKENS,
            "seeds": FINAL_SEEDS,
            "recipe": asdict(RECIPE),
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
                    "model": model,
                    "execution_mode": EXECUTION_MODE,
                    "data_root": str(data_root),
                }
                for model in (CANDIDATE, NO_WORKSPACE_PERMUTATION)
                for _ in FINAL_SEEDS
            ]
        )
        _all_complete(agreements, "compiled replay exact-loss agreement")
        tasks = [
            _train_task(
                model,
                RECIPE,
                seed,
                FINAL_TOKENS,
                2048,
                BATCH,
                cells,
                Path(data_root),
                EXECUTION_MODE,
            )
            for model in (CANDIDATE, NO_WORKSPACE_PERMUTATION)
            for seed in FINAL_SEEDS
        ]
        rows = run_tasks(tasks)
        _all_complete(rows, "compiled final replay")
        candidate = nll_by_seed(rows, CANDIDATE)
        ablation = nll_by_seed(rows, NO_WORKSPACE_PERMUTATION)
        compiled_minus_eager = [
            candidate[seed] - FROZEN_EAGER_CANDIDATE[seed] for seed in FINAL_SEEDS
        ]
        compiled_minus_transformer = [
            candidate[seed] - FROZEN_LOCKED_TRANSFORMER[seed]
            for seed in FINAL_SEEDS
        ]
        permutation_delta = [
            candidate[seed] - ablation[seed] for seed in FINAL_SEEDS
        ]
        for index, row in enumerate(rows):
            run.log(
                {
                    "replay/model": row["model"],
                    "replay/seed": row["seed"],
                    "replay/validation_block_nll": row["validation"]["nll"],
                    "replay/tokens_per_second": row["performance"][
                        "tokens_per_second"
                    ],
                    "replay/global_tokens_per_step": row[
                        "global_tokens_per_step"
                    ],
                    "replay/gpu_utilization_percent": row["performance"][
                        "median_gpu_utilization_percent"
                    ],
                },
                step=index,
            )
        result = {
            "schema": SCHEMA,
            "status": "complete",
            "wandb_url": run.url,
            "gpu_count": GPU_COUNT,
            "execution_mode": EXECUTION_MODE,
            "batch": BATCH,
            "global_tokens_per_step": BATCH * 256,
            "target_tokens": FINAL_TOKENS,
            "recipe": asdict(RECIPE),
            "loss_agreement": agreements,
            "rows": rows,
            "compiled_candidate_minus_frozen_eager_candidate": compiled_minus_eager,
            "compiled_candidate_minus_frozen_locked_transformer": (
                compiled_minus_transformer
            ),
            "compiled_permuted_minus_compiled_unpermuted": permutation_delta,
            "all_four_compiled_candidate_wins": all(
                delta < 0 for delta in compiled_minus_transformer
            ),
            "mean_compiled_candidate_minus_transformer": sum(
                compiled_minus_transformer
            )
            / len(compiled_minus_transformer),
        }
        write_json(output_path, result)
        run.summary.update(
            {
                "all_four_compiled_candidate_wins": result[
                    "all_four_compiled_candidate_wins"
                ],
                "mean_compiled_candidate_minus_transformer": result[
                    "mean_compiled_candidate_minus_transformer"
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
    result = run_replay(
        arguments.output,
        data_root=arguments.data_root,
        heartbeat=arguments.heartbeat,
    )
    print(result["wandb_url"], flush=True)


if __name__ == "__main__":
    main()
