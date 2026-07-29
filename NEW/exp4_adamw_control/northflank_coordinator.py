from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
import time
from pathlib import Path

from qwen_adamw_control.control import (
    COMPARISON_SCHEMA,
    DEFAULT_DATA_ROOT,
    DEFAULT_OUTPUT_ROOT,
    PREFLIGHT_SCHEMA,
    atomic_json,
    control_root,
    load_control_plan,
    status,
)

POLL_SECONDS = 60
HARD_MEMORY_RATIO = 0.92


def runtime_env(microbatch: int) -> dict[str, str]:
    return {
        **os.environ,
        "CUDA_VISIBLE_DEVICES": "0",
        "QWEN_NORMUON_KRONECKER_BACKEND": "cutensor",
        "QWEN_NORMUON_RANK_CHUNK": "512",
        "QWEN_NORMUON_MICROBATCH": str(microbatch),
        "PYTHONUNBUFFERED": "1",
    }


def write_status(output_root: str, value: dict) -> None:
    atomic_json(
        control_root(output_root) / "coordinator-status.json",
        {
            "schema": "qwen-adamw-control-coordinator-v1",
            **value,
        },
    )


def wait_for_reference(output_root: str) -> tuple[dict, int]:
    root = Path(output_root)
    while True:
        try:
            plan, _, _, _ = load_control_plan(output_root)
            preflight_path = root / "preflight.json"
            if not preflight_path.is_file():
                raise RuntimeError("NorMuon preflight is unavailable")
            microbatch = int(
                json.loads(preflight_path.read_text())[
                    "selected_microbatch"
                ]
            )
            if microbatch <= 0:
                raise RuntimeError("invalid matched microbatch")
            atomic_json(control_root(output_root) / "plan.json", plan)
            return plan, microbatch
        except RuntimeError as error:
            write_status(output_root, {
                "status": "queued",
                "stage": "waiting_for_normuon_winner",
                "detail": str(error),
                "poll_seconds": POLL_SECONDS,
            })
            time.sleep(POLL_SECONDS)


def run_logged(
    command: list[str],
    log_path: Path,
    *,
    env: dict[str, str],
) -> int:
    log_path.parent.mkdir(parents=True, exist_ok=True)
    with log_path.open("ab") as log:
        completed = subprocess.run(
            command,
            stdout=log,
            stderr=subprocess.STDOUT,
            env=env,
            check=False,
        )
    return completed.returncode


def queue_control(
    *,
    data_root: str,
    output_root: str,
) -> dict:
    if not os.environ.get("WANDB_API_KEY"):
        raise RuntimeError("WANDB_API_KEY is missing on the node")
    plan, microbatch = wait_for_reference(output_root)
    root = control_root(output_root)
    env = runtime_env(microbatch)
    preflight_path = root / "preflight.json"
    write_status(output_root, {
        "status": "running",
        "stage": "adamw_preflight",
        "plan_sha256": plan["plan_sha256"],
        "microbatch": microbatch,
    })
    preflight_returncode = run_logged(
        [
            sys.executable,
            "-m",
            "qwen_adamw_control",
            "preflight",
            "--data-root",
            data_root,
            "--output-root",
            output_root,
            "--result-path",
            str(preflight_path),
        ],
        root / "preflight.log",
        env=env,
    )
    if preflight_returncode != 0 or not preflight_path.is_file():
        raise RuntimeError("AdamW preflight failed")
    preflight_result = json.loads(preflight_path.read_text())
    if (
        preflight_result.get("schema") != PREFLIGHT_SCHEMA
        or not preflight_result.get("factor_gradient_nonzero")
        or not preflight_result.get("embedding_gradient_nonzero")
        or float(preflight_result["memory_ratio"])
            > HARD_MEMORY_RATIO
    ):
        raise RuntimeError(
            f"AdamW preflight rejected: {preflight_result}"
        )

    result_path = root / plan["label"] / "result.json"
    for attempt in range(1, 4):
        write_status(output_root, {
            "status": "running",
            "stage": "adamw_control",
            "attempt": attempt,
            "plan_sha256": plan["plan_sha256"],
            "microbatch": microbatch,
            "log": str(root / "control.log"),
        })
        returncode = run_logged(
            [
                sys.executable,
                "-m",
                "qwen_adamw_control",
                "run",
                "--data-root",
                data_root,
                "--output-root",
                output_root,
            ],
            root / "control.log",
            env=env,
        )
        if returncode == 0 and result_path.is_file():
            result = json.loads(result_path.read_text())
            if result.get("status") == "complete":
                break
    else:
        raise RuntimeError("AdamW control exhausted three retries")

    comparison_path = root / "comparison.json"
    if not comparison_path.is_file():
        raise RuntimeError("AdamW comparison artifact is missing")
    comparison = json.loads(comparison_path.read_text())
    if comparison.get("schema") != COMPARISON_SCHEMA:
        raise RuntimeError("AdamW comparison artifact is invalid")
    final_status = {
        "status": "complete",
        "stage": "complete",
        "plan_sha256": plan["plan_sha256"],
        "microbatch": microbatch,
        "comparison": str(comparison_path),
        "comparison_sha256": comparison["comparison_sha256"],
    }
    write_status(output_root, final_status)
    print(json.dumps(final_status, indent=2, sort_keys=True), flush=True)
    return final_status


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("command", choices=("queue", "status"))
    parser.add_argument("--data-root", default=DEFAULT_DATA_ROOT)
    parser.add_argument("--output-root", default=DEFAULT_OUTPUT_ROOT)
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    if args.command == "status":
        status(args.output_root)
        return
    try:
        queue_control(
            data_root=args.data_root,
            output_root=args.output_root,
        )
    except BaseException as error:
        write_status(args.output_root, {
            "status": "failed",
            "stage": "failed",
            "error": repr(error),
        })
        raise


if __name__ == "__main__":
    main()
