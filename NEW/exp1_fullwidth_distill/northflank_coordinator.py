from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
import time
from pathlib import Path

from qwen_fullwidth_distill.pretrain import (
    DEFAULT_DATA_ROOT,
    DEFAULT_OUTPUT_ROOT,
    pretrain_trials,
    study_plan,
)


PREFLIGHT_CANDIDATES = (384, 352, 320, 288, 256, 224, 192, 160, 128, 96, 64)


def atomic_json(path: Path, value: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_text(json.dumps(value, indent=2, sort_keys=True))
    os.replace(temporary, path)


def runtime_env(gpu: int, microbatch: int) -> dict[str, str]:
    return {
        **os.environ,
        "CUDA_VISIBLE_DEVICES": str(gpu),
        "QWEN_KRONECKER_BACKEND": "cutensor",
        "QWEN_KRONECKER_RANK_CHUNK": "512",
        "QWEN_KRONECKER_MICROBATCH": str(microbatch),
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
        command = [
            sys.executable,
            "-m",
            "qwen_fullwidth_distill.pretrain",
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
        attempt = {
            "microbatch": candidate,
            "status": (
                "viable"
                if float(result["memory_ratio"]) <= 0.92
                else "over_hard_cap"
            ),
            "memory_ratio": result["memory_ratio"],
            "peak_allocated_gib": result["peak_allocated_gib"],
            "peak_reserved_gib": result["peak_reserved_gib"],
            "step_seconds": result["step_seconds"],
            "loss": result["loss"],
            "log": str(log_path),
        }
        attempts.append(attempt)
        if attempt["status"] == "viable":
            selected = result
            break
    if selected is None:
        raise RuntimeError(f"no preflight candidate survived: {attempts}")
    artifact = {
        "schema": "qwen-fullwidth-next-token-preflight-search-v1",
        "status": "complete",
        "hard_memory_ratio": 0.92,
        "selected_microbatch": selected["microbatch"],
        "selected": selected,
        "attempts": attempts,
    }
    atomic_json(output / "preflight.json", artifact)
    print(json.dumps(artifact, indent=2, sort_keys=True), flush=True)
    return artifact


def _cell_complete(output_root: str, label: str) -> bool:
    try:
        result = json.loads(
            (Path(output_root) / label / "result.json").read_text()
        )
    except (FileNotFoundError, json.JSONDecodeError, OSError):
        return False
    return (
        result.get("status") == "complete"
        and result.get("label") == label
        and (Path(output_root) / label / "student.pt").is_file()
    )


def run_wave(
    indices: tuple[int, ...],
    *,
    wave: int,
    data_root: str,
    output_root: str,
    microbatch: int,
) -> list[dict]:
    trials = pretrain_trials()
    pending = list(indices)
    statuses = []
    for attempt in range(1, 4):
        processes = []
        for gpu, index in enumerate(pending):
            trial = trials[index]
            if _cell_complete(output_root, trial.label):
                statuses.append({
                    "cell_index": index,
                    "label": trial.label,
                    "status": "cached",
                })
                continue
            log_path = Path(output_root) / "logs" / f"cell-{index}.log"
            log_path.parent.mkdir(parents=True, exist_ok=True)
            log = log_path.open("ab")
            command = [
                sys.executable,
                "-m",
                "qwen_fullwidth_distill.pretrain",
                "run-cell",
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
            processes.append((index, trial, process, log, log_path))
        if not processes:
            break
        atomic_json(Path(output_root) / "coordinator-status.json", {
            "schema": "qwen-fullwidth-next-token-coordinator-v1",
            "status": "running",
            "wave": wave,
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
                complete = _cell_complete(output_root, trial.label)
                status = {
                    "cell_index": index,
                    "label": trial.label,
                    "returncode": returncode,
                    "status": "complete" if complete else "failed",
                    "log": str(log_path),
                }
                statuses.append(status)
                if not complete:
                    failures.append(index)
            processes = remaining
            if processes:
                time.sleep(15)
        pending = failures
        if not pending:
            break
    if pending:
        raise RuntimeError(f"wave {wave} exhausted retries for cells {pending}")
    return statuses


def launch_study(
    *,
    data_root: str,
    output_root: str,
    microbatch: int,
) -> dict:
    output = Path(output_root)
    output.mkdir(parents=True, exist_ok=True)
    plan = study_plan()
    atomic_json(output / "study-plan.json", plan)
    if not os.environ.get("WANDB_API_KEY"):
        raise RuntimeError("WANDB_API_KEY is not present on the Northflank node")
    if not (Path(data_root) / "manifest.json").is_file():
        raise RuntimeError(f"prepared dataset is missing at {data_root}")
    statuses = []
    statuses.extend(run_wave(
        (0, 1, 2, 3),
        wave=1,
        data_root=data_root,
        output_root=output_root,
        microbatch=microbatch,
    ))
    statuses.extend(run_wave(
        (4, 5, 6, 7),
        wave=2,
        data_root=data_root,
        output_root=output_root,
        microbatch=microbatch,
    ))
    collect_log_path = output / "logs" / "collect.log"
    with collect_log_path.open("ab") as log:
        collect = subprocess.run(
            [
                sys.executable,
                "-m",
                "qwen_fullwidth_distill.pretrain",
                "collect",
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
    if collect.returncode != 0:
        raise RuntimeError(
            f"study collection failed; see {collect_log_path}"
        )
    result = {
        "schema": "qwen-fullwidth-next-token-coordinator-v1",
        "status": "complete",
        "microbatch": microbatch,
        "cells": statuses,
        "summary": str(output / "study-summary.json"),
    }
    atomic_json(output / "coordinator-status.json", result)
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)
    return result


def status(output_root: str) -> dict:
    output = Path(output_root)
    try:
        coordinator = json.loads(
            (output / "coordinator-status.json").read_text()
        )
    except (FileNotFoundError, json.JSONDecodeError, OSError):
        coordinator = {"status": "not_started"}
    cells = []
    for index, trial in enumerate(pretrain_trials()):
        result_path = output / trial.label / "result.json"
        try:
            result = json.loads(result_path.read_text())
            cell_status = result.get("status", "unknown")
            validation = result.get("validation", {})
        except (FileNotFoundError, json.JSONDecodeError, OSError):
            cell_status = (
                "checkpointed"
                if (output / trial.label / "progress.pt").is_file()
                else "pending"
            )
            validation = {}
        cells.append({
            "cell_index": index,
            "label": trial.label,
            "status": cell_status,
            "validation_cross_entropy":
                validation.get("cross_entropy"),
        })
    value = {"coordinator": coordinator, "cells": cells}
    print(json.dumps(value, indent=2, sort_keys=True))
    return value


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
        status(args.output_root)
        return
    microbatch = args.microbatch
    if microbatch <= 0:
        preflight_path = Path(args.output_root) / "preflight.json"
        if not preflight_path.is_file():
            raise RuntimeError("run preflight before launch")
        microbatch = int(
            json.loads(preflight_path.read_text())["selected_microbatch"]
        )
    launch_study(
        data_root=args.data_root,
        output_root=args.output_root,
        microbatch=microbatch,
    )


if __name__ == "__main__":
    main()
