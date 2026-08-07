"""Build a hash-verified Exp19 resume bundle from immutable W&B evidence."""

from __future__ import annotations

import argparse
import hashlib
import math
import shutil
from dataclasses import asdict
from pathlib import Path
from typing import Any

import wandb

from exp14_block_kronecker.remote_runner import RESUME_BUNDLE_SCHEMA

from .campaign import (
    MAXIMUM_BRANCH_STATE_RATIO,
    MAXIMUM_LAYER_IMBALANCE,
    MINIMUM_EFFECTIVE_UPDATE_FRACTION,
    SAMPLE_INDICES,
    SCREEN_SEED,
    SCREEN_STEPS,
    STAGE_SNAPSHOT_SCHEMA,
    SUCCESS_NLL,
    TRAINING_MODEL_NAMES,
    _threshold_hits,
    exposure_accounting,
    screen_recipes,
    write_json,
)


def recover_screen(run_path: str) -> list[dict[str, Any]]:
    run = wandb.Api().run(run_path)
    prefix = "one_example/"
    curves: dict[tuple[str, str, float, float], list[dict[str, Any]]] = {}
    performance: dict[tuple[str, str, float, float], dict[str, Any]] = {}
    current: tuple[str, str, float, float] | None = None
    for history in run.scan_history(page_size=1000):
        model = history.get(prefix + "model")
        optimizer = history.get(prefix + "optimizer")
        body_lr = history.get(prefix + "body_lr")
        auxiliary_lr = history.get(prefix + "auxiliary_lr")
        if model and optimizer and body_lr is not None:
            current = (
                str(model),
                str(optimizer),
                float(body_lr),
                float(auxiliary_lr),
            )
            if prefix + "nll" in history:
                point = {
                    "step": int(history[prefix + "optimizer_step"]),
                    "elapsed_seconds": float(history[prefix + "elapsed_seconds"]),
                    "nll": float(history[prefix + "nll"]),
                    "token_accuracy": float(history[prefix + "token_accuracy"]),
                }
                telemetry = {
                    "maximum_scaled_update_to_state_ratio": float(
                        history.get(prefix + "max_branch_state_ratio", math.nan)
                    ),
                    "first_to_last_hidden_variance_ratio": float(
                        history.get(
                            prefix + "hidden_variance_first_over_last", math.nan
                        )
                    ),
                    "layer_gradient_imbalance": float(
                        history.get(prefix + "gradient_layer_imbalance", math.nan)
                    ),
                    "minimum_effective_normalized_update_fraction": float(
                        history.get(
                            prefix + "minimum_effective_factor_update", math.nan
                        )
                    ),
                }
                point["recovered_telemetry"] = telemetry
                curves.setdefault(current, []).append(point)
        elif (
            current is not None
            and model == current[0]
            and prefix + "physical_tokens_per_second" in history
        ):
            performance[current] = {
                "elapsed_seconds": curves[current][-1]["elapsed_seconds"],
                "physical_tokens_per_second": float(
                    history[prefix + "physical_tokens_per_second"]
                ),
                "median_gpu_utilization_percent": float(
                    history.get(prefix + "median_gpu_utilization_percent", math.nan)
                ),
                "peak_allocated_gib": float(
                    history.get(prefix + "peak_allocated_gib", math.nan)
                ),
                "peak_reserved_gib": float(
                    history.get(prefix + "peak_reserved_gib", math.nan)
                ),
            }

    recipes = {
        (recipe.family, recipe.body_lr, recipe.auxiliary_lr): recipe
        for recipe in screen_recipes()
    }
    rows: list[dict[str, Any]] = []
    for model in TRAINING_MODEL_NAMES:
        for recipe in screen_recipes():
            key = (model, recipe.family, recipe.body_lr, recipe.auxiliary_lr)
            points = sorted(curves.get(key, []), key=lambda point: point["step"])
            if not points:
                raise RuntimeError(f"W&B screen curve is missing: {key}")
            final = points[-1]
            telemetry = final.pop("recovered_telemetry")
            for point in points[:-1]:
                point.pop("recovered_telemetry", None)
            accounting = exposure_accounting(
                unique_count=1,
                physical_batch=4096,
                steps=int(final["step"]),
            )
            branch = telemetry["maximum_scaled_update_to_state_ratio"]
            variance = telemetry["first_to_last_hidden_variance_ratio"]
            gradient = telemetry["layer_gradient_imbalance"]
            effective = telemetry[
                "minimum_effective_normalized_update_fraction"
            ]
            failures = []
            if not math.isfinite(branch) or branch > MAXIMUM_BRANCH_STATE_RATIO:
                failures.append("branch_state_ratio")
            if not math.isfinite(variance) or variance > MAXIMUM_LAYER_IMBALANCE:
                failures.append("hidden_variance_imbalance")
            if not math.isfinite(gradient) or gradient > MAXIMUM_LAYER_IMBALANCE:
                failures.append("gradient_imbalance")
            if not math.isfinite(effective) or effective < MINIMUM_EFFECTIVE_UPDATE_FRACTION:
                failures.append("ineffective_normalized_factor_update")
            success = (
                float(final["nll"]) <= SUCCESS_NLL
                and float(final["token_accuracy"]) == 1.0
            )
            rows.append(
                {
                    "status": "complete",
                    "stage": "one-example-screen",
                    "model": model,
                    "recipe": asdict(recipes[(recipe.family, recipe.body_lr, recipe.auxiliary_lr)]),
                    "seed": SCREEN_SEED,
                    "unique_count": 1,
                    "sample_indices": [SAMPLE_INDICES[0]],
                    **accounting,
                    "maximum_steps": SCREEN_STEPS,
                    "success": success,
                    "final": {
                        "nll": float(final["nll"]),
                        "token_accuracy": float(final["token_accuracy"]),
                    },
                    "threshold_hits": _threshold_hits(points),
                    "curve": points,
                    "telemetry_curve": [],
                    "conditioning_gate": {
                        "pass": not failures,
                        "failures": failures,
                        **telemetry,
                    },
                    "performance": performance.get(
                        key, {"elapsed_seconds": float(final["elapsed_seconds"])}
                    ),
                    "recovered_from_wandb": run.url,
                }
            )
    if len(rows) != len(TRAINING_MODEL_NAMES) * len(screen_recipes()):
        raise RuntimeError("recovered screen has an invalid row count")
    return rows


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--run", required=True)
    parser.add_argument("--preflight", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--state-digest", required=True)
    args = parser.parse_args()
    output = args.output.resolve()
    preflight_target = output / "norm-cells" / "preflight.json"
    preflight_target.parent.mkdir(parents=True, exist_ok=True)
    shutil.copyfile(args.preflight, preflight_target)
    screen_target = (
        output
        / "norm-cells"
        / "stage-snapshots"
        / "one-example-screen.json"
    )
    write_json(
        screen_target,
        {
            "schema": STAGE_SNAPSHOT_SCHEMA,
            "status": "complete",
            "stage": "one-example-screen",
            "row_count": len(TRAINING_MODEL_NAMES) * len(screen_recipes()),
            "provenance": {"kind": "recovered-wandb-history", "run": args.run},
            "rows": recover_screen(args.run),
        },
    )
    relative_files = (
        "norm-cells/preflight.json",
        "norm-cells/stage-snapshots/one-example-screen.json",
    )
    files = {
        relative: hashlib.sha256((output / relative).read_bytes()).hexdigest()
        for relative in relative_files
    }
    write_json(
        output / "manifest.json",
        {
            "schema": RESUME_BUNDLE_SCHEMA,
            "state_digest": args.state_digest,
            "source_wandb_run": args.run,
            "files": files,
        },
    )


if __name__ == "__main__":
    main()
