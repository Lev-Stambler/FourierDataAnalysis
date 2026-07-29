from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
import time
from pathlib import Path

from qwen_normuon_pretrain.config import final_trials, screen_trials
from qwen_normuon_pretrain.pretrain import (
    DEFAULT_DATA_ROOT,
    DEFAULT_OUTPUT_ROOT,
    FINAL_SUMMARY_SCHEMA,
    RESULT_SCHEMA,
    SCREEN_SUMMARY_SCHEMA,
    study_plan,
)

PREFLIGHT_CANDIDATES = (
    256,
    128,
    64,
)


def atomic_json(path: Path, value: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_text(json.dumps(value, indent=2, sort_keys=True))
    os.replace(temporary, path)


def runtime_env(gpu: int, microbatch: int) -> dict[str, str]:
    return {
        **os.environ,
        "CUDA_VISIBLE_DEVICES": str(gpu),
        "QWEN_NORMUON_KRONECKER_BACKEND": "cutensor",
        "QWEN_NORMUON_RANK_CHUNK": "512",
        "QWEN_NORMUON_MICROBATCH": str(microbatch),
        "PYTHONUNBUFFERED": "1",
    }


def preflight_search(data_root: str, output_root: str) -> dict:
    output = Path(output_root)
    output.mkdir(parents=True, exist_ok=True)
    attempts = []
    selected = None
    for candidate in PREFLIGHT_CANDIDATES:
        result_path = output / f"preflight-{candidate}.json"
        log_path = output / f"preflight-{candidate}.log"
        result_path.unlink(missing_ok=True)
        command = [
            sys.executable,
            "-m",
            "qwen_normuon_pretrain",
            "preflight",
            "--data-root",
            data_root,
            "--microbatch",
            str(candidate),
            "--result-path",
            str(result_path),
        ]
        with log_path.open("ab") as log:
            completed = subprocess.run(
                command,
                stdout=log,
                stderr=subprocess.STDOUT,
                env=runtime_env(0, candidate),
                check=False,
            )
        if completed.returncode != 0 or not result_path.is_file():
            attempts.append({
                "microbatch": candidate,
                "status": "failed",
                "returncode": completed.returncode,
                "log": str(log_path),
            })
            continue
        result = json.loads(result_path.read_text())
        memory_ratio = float(result["memory_ratio"])
        status = "viable" if memory_ratio <= 0.92 else "over_hard_cap"
        attempts.append({
            "microbatch": candidate,
            "status": status,
            "memory_ratio": memory_ratio,
            "peak_allocated_gib": result["peak_allocated_gib"],
            "peak_reserved_gib": result["peak_reserved_gib"],
            "step_seconds": result["step_seconds"],
            "timings": result["timings"],
            "loss": result["loss"],
            "log": str(log_path),
        })
        if status == "viable":
            selected = result
            break
    if selected is None:
        raise RuntimeError(
            f"no preflight candidate survived: {attempts}"
        )
    artifact = {
        "schema": "qwen-normuon-preflight-search-v1",
        "status": "complete",
        "hard_memory_ratio": 0.92,
        "selected_microbatch": selected["microbatch"],
        "selected": selected,
        "attempts": attempts,
    }
    atomic_json(output / "preflight.json", artifact)
    print(json.dumps(artifact, indent=2, sort_keys=True), flush=True)
    return artifact


def completed_cell(
    output_root: str,
    stage: str,
    label: str,
) -> bool:
    path = Path(output_root) / stage / label
    try:
        result = json.loads((path / "result.json").read_text())
    except (FileNotFoundError, json.JSONDecodeError, OSError):
        return False
    if (
        result.get("schema") != RESULT_SCHEMA
        or result.get("status") != "complete"
        or result.get("label") != label
    ):
        return False
    return stage != "final" or (path / "student.pt").is_file()


def write_running_status(
    output_root: str,
    *,
    stage: str,
    attempt: int,
    processes: list,
) -> None:
    atomic_json(Path(output_root) / "coordinator-status.json", {
        "schema": "qwen-normuon-coordinator-v1",
        "status": "running",
        "stage": stage,
        "attempt": attempt,
        "processes": [
            {
                "cell_index": index,
                "label": trial.label,
                "pid": process.pid,
                "log": str(log_path),
            }
            for index, trial, process, _, log_path in processes
        ],
    })


def run_stage(
    stage: str,
    trials: list,
    *,
    data_root: str,
    output_root: str,
    microbatch: int,
) -> list[dict]:
    if stage not in ("screen", "final"):
        raise ValueError(f"unsupported stage {stage}")
    pending = list(range(len(trials)))
    statuses = []
    for attempt in range(1, 4):
        processes = []
        for gpu, index in enumerate(pending):
            trial = trials[index]
            if completed_cell(output_root, stage, trial.label):
                statuses.append({
                    "stage": stage,
                    "cell_index": index,
                    "label": trial.label,
                    "status": "cached",
                })
                continue
            log_path = (
                Path(output_root)
                / "logs"
                / f"{stage}-cell-{index}.log"
            )
            log_path.parent.mkdir(parents=True, exist_ok=True)
            log = log_path.open("ab")
            command = [
                sys.executable,
                "-m",
                "qwen_normuon_pretrain",
                f"run-{stage}",
                "--cell-index",
                str(index),
                "--data-root",
                data_root,
                "--output-root",
                output_root,
            ]
            process = subprocess.Popen(
                command,
                stdout=log,
                stderr=subprocess.STDOUT,
                env=runtime_env(gpu, microbatch),
            )
            processes.append(
                (index, trial, process, log, log_path)
            )
        if not processes:
            break
        write_running_status(
            output_root,
            stage=stage,
            attempt=attempt,
            processes=processes,
        )
        failures = []
        while processes:
            remaining = []
            for index, trial, process, log, log_path in processes:
                returncode = process.poll()
                if returncode is None:
                    remaining.append(
                        (index, trial, process, log, log_path)
                    )
                    continue
                log.close()
                complete = completed_cell(
                    output_root,
                    stage,
                    trial.label,
                )
                statuses.append({
                    "stage": stage,
                    "cell_index": index,
                    "label": trial.label,
                    "returncode": returncode,
                    "status": "complete" if complete else "failed",
                    "log": str(log_path),
                })
                if not complete:
                    failures.append(index)
            processes = remaining
            if processes:
                time.sleep(15)
        pending = failures
        if not pending:
            break
    if pending:
        raise RuntimeError(
            f"{stage} exhausted retries for cells {pending}"
        )
    return statuses


def run_collect(
    command: str,
    *,
    data_root: str,
    output_root: str,
    microbatch: int,
) -> None:
    log_path = Path(output_root) / "logs" / f"{command}.log"
    with log_path.open("ab") as log:
        completed = subprocess.run(
            [
                sys.executable,
                "-m",
                "qwen_normuon_pretrain",
                command,
                "--data-root",
                data_root,
                "--output-root",
                output_root,
            ],
            stdout=log,
            stderr=subprocess.STDOUT,
            env=runtime_env(0, microbatch),
            check=False,
        )
    if completed.returncode != 0:
        raise RuntimeError(
            f"{command} failed; see {log_path}"
        )


def launch_study(
    *,
    data_root: str,
    output_root: str,
    microbatch: int,
) -> dict:
    output = Path(output_root)
    output.mkdir(parents=True, exist_ok=True)
    atomic_json(output / "study-plan.json", study_plan())
    if not os.environ.get("WANDB_API_KEY"):
        raise RuntimeError("WANDB_API_KEY is missing on the node")
    if not (Path(data_root) / "manifest.json").is_file():
        raise RuntimeError(f"prepared dataset is missing at {data_root}")
    statuses = run_stage(
        "screen",
        screen_trials(),
        data_root=data_root,
        output_root=output_root,
        microbatch=microbatch,
    )
    run_collect(
        "collect-screen",
        data_root=data_root,
        output_root=output_root,
        microbatch=microbatch,
    )
    screen_summary = json.loads(
        (output / "screen-summary.json").read_text()
    )
    if screen_summary.get("schema") != SCREEN_SUMMARY_SCHEMA:
        raise RuntimeError("screen collection wrote invalid summary")
    finalists = final_trials(tuple(screen_summary["top_lrs"]))
    statuses.extend(run_stage(
        "final",
        finalists,
        data_root=data_root,
        output_root=output_root,
        microbatch=microbatch,
    ))
    run_collect(
        "collect-final",
        data_root=data_root,
        output_root=output_root,
        microbatch=microbatch,
    )
    final_summary = json.loads(
        (output / "study-summary.json").read_text()
    )
    if final_summary.get("schema") != FINAL_SUMMARY_SCHEMA:
        raise RuntimeError("final collection wrote invalid summary")
    result = {
        "schema": "qwen-normuon-coordinator-v1",
        "status": "complete",
        "microbatch": microbatch,
        "cells": statuses,
        "summary": str(output / "study-summary.json"),
        "winner": final_summary["winner"],
    }
    atomic_json(output / "coordinator-status.json", result)
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)
    return result


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "command",
        choices=("preflight", "launch", "status"),
    )
    parser.add_argument("--data-root", default=DEFAULT_DATA_ROOT)
    parser.add_argument("--output-root", default=DEFAULT_OUTPUT_ROOT)
    parser.add_argument("--microbatch", type=int, default=0)
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    if args.command == "preflight":
        preflight_search(args.data_root, args.output_root)
        return
    if args.command == "status":
        from qwen_normuon_pretrain.pretrain import status

        status(args.output_root)
        return
    microbatch = args.microbatch
    if microbatch <= 0:
        path = Path(args.output_root) / "preflight.json"
        if not path.is_file():
            raise RuntimeError("run preflight before launch")
        microbatch = int(
            json.loads(path.read_text())["selected_microbatch"]
        )
    launch_study(
        data_root=args.data_root,
        output_root=args.output_root,
        microbatch=microbatch,
    )


if __name__ == "__main__":
    main()
