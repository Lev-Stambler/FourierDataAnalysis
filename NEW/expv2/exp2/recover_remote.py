"""Recover an orphaned durable ExpV2-2 job after CLI reauthentication."""

from __future__ import annotations

import argparse
import json
import time
from pathlib import Path

from expv2.exp1.utils import atomic_json

from .infra import delete_service
from .remote_runner import _download, _exec


def recover(
    *,
    remote_output: str,
    job_name: str,
    result_filename: str,
    local_output: Path,
    result_path: Path,
    timeout_seconds: int,
) -> dict:
    if not remote_output.startswith("/cache/expv2/exp2/"):
        raise ValueError(f"refusing unexpected remote output: {remote_output}")
    if not job_name.startswith("expv2-2-") or "/" in job_name:
        raise ValueError(f"refusing unexpected job name: {job_name}")
    done = f"/root/{job_name}.done"
    log = f"/root/{job_name}.log"
    deadline = time.monotonic() + timeout_seconds
    try:
        while time.monotonic() < deadline:
            try:
                _exec(f"test -s {done}", capture=True)
                break
            except Exception:
                tail = _exec(f"tail -5 {log} 2>/dev/null || true", capture=True)
                if tail.stdout.strip():
                    print(tail.stdout[-2_000:], flush=True)
                time.sleep(10)
        else:
            raise TimeoutError(f"orphaned job did not finish within {timeout_seconds}s")
        code_output = _exec(f"cat {done}", capture=True).stdout
        code_lines = [line.strip() for line in code_output.splitlines() if line.strip().isdigit()]
        remote_code = int(code_lines[-1]) if code_lines else -1
        local_output.mkdir(parents=True, exist_ok=True)
        for filename in (result_filename, "paid-preflight.json", "wandb-launch.json"):
            _download(
                f"{remote_output}/{filename}",
                local_output / filename,
                required=filename == result_filename,
            )
        result = json.loads((local_output / result_filename).read_text())
        result["recovered_after_cli_reauthentication"] = True
        result["remote_return_code"] = remote_code
        atomic_json(result_path, result)
        if remote_code or result.get("status") == "failed":
            raise RuntimeError(
                f"recovered cloud job failed with remote code {remote_code}: "
                f"{result.get('reason')}"
            )
        return result
    finally:
        delete_service()


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--remote-output", required=True)
    parser.add_argument("--job-name", required=True)
    parser.add_argument("--result-filename", required=True)
    parser.add_argument("--local-output", type=Path, required=True)
    parser.add_argument("--result", type=Path, required=True)
    parser.add_argument("--timeout-seconds", type=int, default=2400)
    args = parser.parse_args()
    result = recover(
        remote_output=args.remote_output,
        job_name=args.job_name,
        result_filename=args.result_filename,
        local_output=args.local_output,
        result_path=args.result,
        timeout_seconds=args.timeout_seconds,
    )
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)


if __name__ == "__main__":
    main()
