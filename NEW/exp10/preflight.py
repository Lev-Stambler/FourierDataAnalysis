"""Downward OOM search and measured full-node batch selection for Exp10."""

from __future__ import annotations

import argparse
import json
import os
import subprocess
import threading
import time
from pathlib import Path


CANDIDATES = (512, 384, 256, 192, 128, 96, 64, 32, 16, 8, 4, 2, 1)


def gpu_sampler(stop: threading.Event, samples: list[list[dict]]) -> None:
    while not stop.wait(0.5):
        completed = subprocess.run(
            [
                "nvidia-smi",
                "--query-gpu=index,utilization.gpu,memory.used,power.draw",
                "--format=csv,noheader,nounits",
            ],
            text=True,
            capture_output=True,
            check=False,
        )
        if completed.returncode:
            continue
        rows = []
        for line in completed.stdout.strip().splitlines():
            index, utilization, memory, power = (part.strip() for part in line.split(","))
            rows.append(
                {
                    "index": int(index),
                    "utilization_percent": float(utilization),
                    "memory_mib": float(memory),
                    "power_w": float(power),
                }
            )
        if len(rows) == 8:
            samples.append(rows)


def trial(args: argparse.Namespace, architecture: str, batch: int, steps: int) -> dict:
    output = Path(args.output_root) / architecture / f"batch-{batch}-steps-{steps}"
    command = [
        "uv", "run", "--no-sync", "torchrun", "--standalone", "--nproc-per-node=8",
        "exp10/architecture_verdict.py",
        f"--architecture={architecture}",
        f"--scale={args.scale}",
        "--phase=screen",
        "--lr=0.0004",
        f"--steps={steps}",
        f"--local-batch={batch}",
        "--eval-batch=1",
        "--validation-windows=64",
        "--no-compile",
        f"--data-root={args.data_root}",
        f"--output-dir={output}",
        f"--run-name=exp10-preflight-{args.scale}-{architecture}-batch{batch}-steps{steps}",
    ]
    stop, samples = threading.Event(), []
    sampler = threading.Thread(target=gpu_sampler, args=(stop, samples), daemon=True)
    sampler.start()
    started = time.time()
    completed = subprocess.run(command, env={**os.environ, "PYTHONUNBUFFERED": "1"})
    stop.set()
    sampler.join()
    value = {
        "architecture": architecture,
        "local_batch": batch,
        "steps": steps,
        "returncode": completed.returncode,
        "wall_seconds": time.time() - started,
        "gpu_samples": samples,
    }
    result_path = output / "result.json"
    if completed.returncode == 0 and result_path.is_file():
        value["result"] = json.loads(result_path.read_text())
    return value


def run(args: argparse.Namespace) -> dict:
    if not os.environ.get("WANDB_API_KEY"):
        raise RuntimeError("WANDB_API_KEY is required")
    inventory = subprocess.run(
        ["nvidia-smi", "--query-gpu=name,memory.total", "--format=csv,noheader"],
        text=True,
        capture_output=True,
        check=True,
    ).stdout.strip().splitlines()
    if len(inventory) != 8 or any("H100" not in value for value in inventory):
        raise RuntimeError(f"Exp10 preflight requires 8xH100; found {inventory}")

    trials = []
    first_stable = None
    for batch in CANDIDATES:
        value = trial(args, args.architectures[0], batch, 1)
        trials.append(value)
        if value["returncode"] == 0:
            first_stable = batch
            break
    if first_stable is None:
        raise RuntimeError(f"no stable {args.architectures[0]} batch")
    candidates = [first_stable]
    lower = [value for value in CANDIDATES if value < first_stable][:2]
    candidates.extend(lower)
    sustained = []
    for batch in candidates:
        for architecture in args.architectures:
            value = trial(args, architecture, batch, 4)
            trials.append(value)
            if value["returncode"] == 0:
                sustained.append(value)
    common = []
    for batch in candidates:
        rows = [
            value
            for value in sustained
            if value["local_batch"] == batch and "result" in value
        ]
        if len(rows) == len(args.architectures):
            common.append(
                (
                    min(
                        row["result"]["performance"]["sustained_tokens_per_second"]
                        for row in rows
                    ),
                    batch,
                )
            )
    if not common:
        raise RuntimeError("no common sustained batch for both architectures")
    _, selected = max(common)
    result = {
        "schema": "exp10-h100-preflight-v1",
        "scale": args.scale,
        "architectures": args.architectures,
        "gpu_inventory": inventory,
        "selected_local_batch": selected,
        "global_token_batch": selected * 8 * 256,
        "trials": trials,
    }
    output = Path(args.output_root)
    output.mkdir(parents=True, exist_ok=True)
    (output / "preflight.json").write_text(json.dumps(result, indent=2))
    print(json.dumps(result, indent=2))
    return result


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--data-root", default="/cache/exp10/data/tinystories")
    parser.add_argument("--output-root", default="/cache/exp10/preflight")
    parser.add_argument("--scale", default="large")
    parser.add_argument(
        "--architectures", nargs="+", default=["kronecker", "transformer"]
    )
    run(parser.parse_args())


if __name__ == "__main__":
    main()
