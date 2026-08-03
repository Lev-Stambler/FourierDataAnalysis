#!/usr/bin/env python3
"""Promote the symmetric screen, run locked finals, confirm, and audit."""

from __future__ import annotations

import json
import os
import subprocess
import sys
from pathlib import Path


ROOT = Path("/cache/expv6-kiss/matched-standard/symmetric-v2-h100")
DATA = Path(
    "/cache/qwen_fullwidth_distill/context16-fineweb-edu-next-token-4m-v1"
)
CONFIRMATION = ROOT / "confirmation-v1"
SCRIPT = "expv6/matched-standard/matched_standard.py"
ARCHITECTURES = ("kronecker", "transformer")


def slug(architecture: str, seed: int, lr: float) -> str:
    lr_text = f"{lr:.8g}".replace(".", "p")
    return f"{architecture}-seed{seed}-lr{lr_text}"


def result(architecture: str, seed: int, lr: float) -> dict:
    return json.loads((ROOT / slug(architecture, seed, lr) / "result.json").read_text())


def launch_train(
    architecture: str, lr: float, steps: int, *, seed: int = 0, resume: bool
) -> None:
    lr_text = f"{lr:.8g}".replace(".", "p")
    stage = "promote" if steps == 48 else "final"
    command = [
        "uv", "run", "--no-sync", "torchrun", "--standalone", "--nproc-per-node=8",
        SCRIPT,
        f"--architecture={architecture}",
        f"--seed={seed}",
        f"--lr={lr}",
        f"--steps={steps}",
        "--checkpoint_every=16",
        "--local_batch=32832",
        "--teacher_microbatch=2048",
        "--evaluation_batch=256",
        "--evaluate_after_train=true",
        f"--resume={'true' if resume else 'false'}",
        "--legacy_root_layout=false",
        "--symmetric_campaign=true",
        f"--output_dir={ROOT}",
        f"--run_name=expv6-symmetric-v2-h100-{architecture}-{stage}-lr{lr_text}-seed{seed}",
    ]
    subprocess.run(command, check=True, env={**os.environ, "PYTHONUNBUFFERED": "1"})


def validation_row(architecture: str, lr: float, step: int) -> dict:
    value = result(architecture, 0, lr)
    return {
        "lr": lr,
        "kl": float(value["validation_history"][str(step)]["kl"]),
        "tokens_per_second": float(
            value["performance_history"][str(step)]["tokens_per_second"]
        ),
    }


def selected_lr(rows: list[dict]) -> float:
    best_kl = min(row["kl"] for row in rows)
    tied = [row for row in rows if row["kl"] <= best_kl + 0.01]
    return float(max(tied, key=lambda row: row["tokens_per_second"])["lr"])


def evaluate_confirmation(architecture: str, lr: float, seed: int) -> None:
    command = [
        "uv", "run", "--no-sync", "torchrun", "--standalone", "--nproc-per-node=8",
        SCRIPT,
        "--mode=evaluate",
        "--evaluation_split=confirmation",
        f"--architecture={architecture}",
        f"--seed={seed}",
        f"--lr={lr}",
        "--steps=256",
        "--local_batch=32832",
        "--teacher_microbatch=2048",
        "--evaluation_batch=256",
        "--legacy_root_layout=false",
        "--symmetric_campaign=true",
        f"--output_dir={ROOT}",
        f"--confirmation_root={CONFIRMATION}",
    ]
    subprocess.run(command, check=True)


def main() -> None:
    lr_grid = (0.025, 0.05, 0.1, 0.2, 0.4, 0.8)
    promoted: dict[str, list[float]] = {}
    for architecture in ARCHITECTURES:
        rows = [validation_row(architecture, lr, 16) for lr in lr_grid]
        promoted[architecture] = [
            float(row["lr"]) for row in sorted(rows, key=lambda row: row["kl"])[:2]
        ]
        print(f"PROMOTED {architecture}: {promoted[architecture]}", flush=True)
        for lr in promoted[architecture]:
            launch_train(architecture, lr, 48, resume=True)

    selected: dict[str, float] = {}
    for architecture in ARCHITECTURES:
        rows = [validation_row(architecture, lr, 48) for lr in promoted[architecture]]
        selected[architecture] = selected_lr(rows)
        print(f"SELECTED {architecture}: {selected[architecture]}", flush=True)
        launch_train(architecture, selected[architecture], 256, seed=0, resume=True)
        for seed in (1, 2):
            launch_train(
                architecture, selected[architecture], 256, seed=seed, resume=False
            )

    subprocess.run(
        [
            "uv", "run", "--no-sync",
            "expv6/matched-standard/prepare_confirmation.py",
            f"--data-root={DATA}",
            f"--output-root={CONFIRMATION}",
        ],
        check=True,
    )
    for architecture in ARCHITECTURES:
        for seed in (0, 1, 2):
            evaluate_confirmation(architecture, selected[architecture], seed)

    subprocess.run(
        [
            "uv", "run", "--no-sync", SCRIPT,
            "--mode=audit",
            "--steps=256",
            "--local_batch=32832",
            "--legacy_root_layout=false",
            "--symmetric_campaign=true",
            f"--output_dir={ROOT}",
            f"--confirmation_root={CONFIRMATION}",
        ],
        check=True,
    )


if __name__ == "__main__":
    try:
        main()
    except Exception as error:
        print(f"CAMPAIGN_FAILED: {error}", file=sys.stderr, flush=True)
        raise
