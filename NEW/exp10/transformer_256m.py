"""Tune and run the locked 255M-parameter WikiText Transformer reference."""

from __future__ import annotations

import argparse
import json
import math
import os
import statistics
import subprocess
from pathlib import Path

from architecture_verdict import CONTEXT_LENGTH, PRESETS, parameter_inventory, LanguageModel


LRS = (0.0001, 0.0002, 0.0004, 0.0008, 0.0016)
FRACTIONS = {"screen": 0.10, "promotion": 0.30, "final": 1.0}
SEEDS = (0, 1, 2)


def slug(value: float) -> str:
    return f"{value:.8g}".replace(".", "p")


def windows(data_root: Path) -> int:
    manifest = json.loads((data_root / "manifest.json").read_text())
    return int(manifest["files"]["train.npy"]["windows"])


def output_path(root: Path, phase: str, lr: float, seed: int) -> Path:
    return root / phase / f"lr-{slug(lr)}" / f"seed-{seed}"


def command(args: argparse.Namespace, phase: str, lr: float, seed: int, mode: str = "train") -> list[str]:
    steps = max(
        1,
        math.floor(
            windows(Path(args.data_root))
            / (args.local_batch * 8)
            * FRACTIONS[phase]
        ),
    )
    output = output_path(Path(args.output_root), phase, lr, seed)
    return [
        "uv", "run", "--no-sync", "torchrun", "--standalone", "--nproc-per-node=8",
        "exp10/architecture_verdict.py",
        f"--mode={mode}",
        "--architecture=transformer",
        "--scale=xlarge",
        f"--phase={phase}",
        f"--lr={lr}",
        f"--seed={seed}",
        f"--steps={steps}",
        f"--local-batch={args.local_batch}",
        f"--eval-batch={args.eval_batch}",
        "--no-compile",
        f"--data-root={args.data_root}",
        f"--output-dir={output}",
        f"--run-name=exp10-wikitext-256m-transformer-{phase}-lr{slug(lr)}-seed{seed}",
    ]


def launch(args: argparse.Namespace, phase: str, lr: float, seed: int, mode: str = "train") -> dict:
    output = output_path(Path(args.output_root), phase, lr, seed)
    result_path = output / "result.json"
    if result_path.is_file():
        result = json.loads(result_path.read_text())
        complete = mode == "train" or mode == "evaluate" and "test" in result or mode == "benchmark" and "benchmark" in result
        if complete:
            print(f"SKIP_COMPLETE mode={mode} output={output}", flush=True)
            return result
    subprocess.run(command(args, phase, lr, seed, mode), check=True, env={**os.environ, "PYTHONUNBUFFERED": "1"})
    result = json.loads(result_path.read_text())
    if not result.get("wandb_url"):
        raise RuntimeError(f"missing direct W&B URL in {result_path}")
    return result


def best(rows: dict[float, dict], count: int = 1) -> list[float]:
    return [
        lr
        for lr, _ in sorted(
            rows.items(), key=lambda item: item[1]["validation"]["nll"]
        )[:count]
    ]


def run(args: argparse.Namespace) -> dict:
    if not os.environ.get("WANDB_API_KEY"):
        raise RuntimeError("WANDB_API_KEY is required")
    spec = PRESETS[("transformer", "xlarge")]
    inventory = parameter_inventory(LanguageModel(spec, activation_checkpointing=False))
    if inventory["total"] != 255_000_576:
        raise RuntimeError(f"unexpected parameter inventory: {inventory}")
    root = Path(args.output_root)
    root.mkdir(parents=True, exist_ok=True)
    plan = {
        "schema": "exp10-transformer-256m-plan-v1",
        "architecture": spec.__dict__,
        "inventory": inventory,
        "learning_rates": LRS,
        "phase_fractions": FRACTIONS,
        "seeds": SEEDS,
        "world_size": 8,
        "local_batch_per_gpu": args.local_batch,
        "global_token_batch": args.local_batch * 8 * CONTEXT_LENGTH,
        "train_prediction_tokens": windows(Path(args.data_root)) * CONTEXT_LENGTH,
    }
    (root / "plan.json").write_text(json.dumps(plan, indent=2))
    screens = {lr: launch(args, "screen", lr, 0) for lr in LRS}
    promoted_lrs = best(screens, 2)
    promotions = {lr: launch(args, "promotion", lr, 0) for lr in promoted_lrs}
    selected_lr = best(promotions)[0]
    finals = [launch(args, "final", selected_lr, seed) for seed in SEEDS]
    finals = [launch(args, "final", selected_lr, seed, "evaluate") for seed in SEEDS]
    finals[0] = launch(args, "final", selected_lr, 0, "benchmark")
    audit = {
        "schema": "exp10-transformer-256m-audit-v1",
        "status": "complete",
        "selected_lr": selected_lr,
        "parameters": inventory["total"],
        "global_token_batch": plan["global_token_batch"],
        "validation_nll": [row["validation"]["nll"] for row in finals],
        "test_nll": [row["test"]["nll"] for row in finals],
        "mean_test_nll": statistics.fmean(row["test"]["nll"] for row in finals),
        "sustained_tokens_per_second": [row["performance"]["sustained_tokens_per_second"] for row in finals],
        "peak_allocated_gib": [row["performance"]["peak_allocated_gib"] for row in finals],
        "wandb_urls": [row["wandb_url"] for row in finals],
        "benchmark": finals[0]["benchmark"],
    }
    (root / "audit.json").write_text(json.dumps(audit, indent=2, sort_keys=True))
    print(json.dumps(audit, indent=2), flush=True)
    return audit


def parser() -> argparse.ArgumentParser:
    value = argparse.ArgumentParser()
    value.add_argument("--data-root", default="/cache/exp10/data/wikitext")
    value.add_argument("--output-root", default="/cache/exp10/runs/wikitext/transformer-256m")
    value.add_argument("--local-batch", type=int, required=True)
    value.add_argument("--eval-batch", type=int, required=True)
    return value


if __name__ == "__main__":
    run(parser().parse_args())
