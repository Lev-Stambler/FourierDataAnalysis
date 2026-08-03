"""Durable Northflank launcher for the eight-GPU matched Muon campaign."""

from __future__ import annotations

import base64
import hashlib
import json
import os
import re
import shlex
import subprocess
import tempfile
import time
from pathlib import Path
from typing import Any

from expv2.exp1.utils import atomic_json


PROJECT_ID = "fda-test"
SERVICE_ID = "fda-node8h2"
JOB_NAME = "expv2-2-matched-muon"
REMOTE_RETURN_CODE = "__EXPV2_2_MATCHED_RC__="


def _target() -> tuple[str, str]:
    project = os.environ.get("RC_NF_PROJECT", PROJECT_ID)
    service = os.environ.get("RC_NF_SERVICE", SERVICE_ID)
    if (project, service) != (PROJECT_ID, SERVICE_ID):
        raise RuntimeError(
            f"matched campaign is pinned to {PROJECT_ID}/{SERVICE_ID}; "
            f"got {project}/{service}"
        )
    return project, service


def _target_args() -> list[str]:
    project, service = _target()
    return ["--projectId", project, "--serviceId", service]


def _run(arguments: list[str], **kwargs: Any) -> subprocess.CompletedProcess[str]:
    return subprocess.run(arguments, check=True, text=True, **kwargs)


def _exec(command: str, *, capture: bool = False) -> subprocess.CompletedProcess[str]:
    payload = base64.b64encode(command.encode()).decode()
    wrapper = (
        "script=$(mktemp /tmp/expv2-2-matched.XXXXXX) && "
        f"printf %s {shlex.quote(payload)} | base64 -d > \"$script\" && "
        "bash \"$script\"; code=$?; rm -f \"$script\"; "
        f"printf '\\n{REMOTE_RETURN_CODE}%s\\n' \"$code\"; exit 0"
    )
    result = _run(
        [
            "northflank",
            "exec",
            "service",
            *_target_args(),
            "--shell-cmd",
            "bash -lc",
            "--cmd",
            wrapper,
        ],
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
    )
    output = result.stdout or ""
    matches = re.findall(re.escape(REMOTE_RETURN_CODE) + r"(\d+)", output)
    if not matches:
        raise RuntimeError("Northflank exec lacked return-code sentinel:\n" + output[-4000:])
    code = int(matches[-1])
    cleaned = re.sub(
        rf"^\s*{re.escape(REMOTE_RETURN_CODE)}\d+\s*$",
        "",
        output,
        flags=re.MULTILINE,
    )
    result.stdout = cleaned
    if not capture and cleaned:
        print(cleaned, end="" if cleaned.endswith("\n") else "\n", flush=True)
    if code:
        raise subprocess.CalledProcessError(code, result.args, output=cleaned)
    return result


def _service() -> dict[str, Any]:
    result = _run(
        ["northflank", "get", "service", *_target_args(), "-o", "json"],
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
    )
    return json.loads(result.stdout)


def _deployment_status(service: dict[str, Any]) -> str:
    status = service.get("status", {})
    if isinstance(status, dict):
        deployment = status.get("deployment", {})
        if isinstance(deployment, dict) and deployment.get("status"):
            return str(deployment["status"])
    return str(status)


def _containers() -> list[dict[str, Any]]:
    result = _run(
        [
            "northflank",
            "get",
            "service",
            "containers",
            *_target_args(),
            "-o",
            "json",
        ],
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
    )
    return list(json.loads(result.stdout).get("containers", []))


def _ensure_running(timeout_seconds: int = 1200) -> dict[str, Any]:
    service = _service()
    gpu = (
        service.get("deployment", {})
        .get("gpu", {})
        .get("configuration", {})
    )
    if int(gpu.get("gpuCount", 0)) != 8:
        raise RuntimeError(f"expected eight GPUs, got service GPU config {gpu}")
    if str(gpu.get("gpuType", "")) not in ("h100-80", "h200-141"):
        raise RuntimeError(f"expected H100/H200 service, got {gpu}")
    try:
        initial_containers = _containers()
    except (subprocess.CalledProcessError, json.JSONDecodeError):
        initial_containers = []
    resumed = False
    if not any(row.get("status") == "TASK_RUNNING" for row in initial_containers):
        _run(
            [
                "northflank",
                "resume",
                "service",
                *_target_args(),
                "-i",
                '{"instances":1}',
            ]
        )
        resumed = True
    deadline = time.monotonic() + timeout_seconds
    while time.monotonic() < deadline:
        service = _service()
        try:
            rows = _containers()
            if any(row.get("status") == "TASK_RUNNING" for row in rows):
                return {"service": service, "resumed": resumed, "containers": rows}
        except (subprocess.CalledProcessError, json.JSONDecodeError):
            pass
        time.sleep(5)
    raise TimeoutError(f"service did not become runnable within {timeout_seconds}s")


def pause_service() -> None:
    _target()
    _run(["northflank", "pause", "service", *_target_args()])


def _upload(local: Path, remote: str) -> None:
    _run(
        [
            "northflank",
            "upload",
            "service",
            "file",
            *_target_args(),
            "--localPath",
            str(local),
            "--remotePath",
            remote,
        ]
    )


def _download(remote: str, local: Path, *, required: bool = True) -> bool:
    local.parent.mkdir(parents=True, exist_ok=True)
    result = subprocess.run(
        [
            "northflank",
            "download",
            "service",
            "file",
            *_target_args(),
            "--remotePath",
            remote,
            "--localPath",
            str(local),
        ],
        text=True,
    )
    if required and result.returncode:
        raise RuntimeError(f"failed to download required artifact: {remote}")
    return result.returncode == 0


def _digest(root: Path) -> str:
    value = hashlib.sha256()
    for path in sorted((root / "expv2").rglob("*.py")):
        value.update(str(path.relative_to(root)).encode())
        value.update(path.read_bytes())
    return value.hexdigest()


def _remote_job(command: str) -> None:
    log, done = f"/root/{JOB_NAME}.log", f"/root/{JOB_NAME}.done"
    payload = base64.b64encode(command.encode()).decode()
    worker = (
        f"printf %s {shlex.quote(payload)} | base64 -d | bash > {log} 2>&1; "
        f"code=$?; printf '%s\\n' \"$code\" > {done}"
    )
    _exec(
        f"rm -f {shlex.quote(log)} {shlex.quote(done)}; "
        f"nohup bash -c {shlex.quote(worker)} >/dev/null 2>&1 &",
        capture=True,
    )


def run_remote(
    *,
    repo_root: str | Path,
    local_output: str | Path,
    result_path: str | Path,
    timeout_seconds: int = 10_800,
) -> dict[str, Any]:
    key = os.environ.get("WANDB_API_KEY")
    if not key:
        raise RuntimeError("WANDB_API_KEY is required before resuming a paid node")
    root, local = Path(repo_root).resolve(), Path(local_output).resolve()
    local.mkdir(parents=True, exist_ok=True)
    _target()
    lifecycle_started = time.time()
    running = _ensure_running()
    digest = _digest(root)
    source = f"/cache/expv2/exp2/source-{digest[:12]}"
    remote_output = f"/cache/expv2/exp2/matched-muon-{digest[:12]}"
    remote_result = remote_output + "/result.json"
    launch = remote_output + "/wandb-launch.json"
    credential_dir = f"/root/{JOB_NAME}-credentials"
    campaign_error: Exception | None = None
    try:
        inventory = _exec(
            "nvidia-smi --query-gpu=index,name,memory.total --format=csv,noheader",
            capture=True,
        ).stdout
        gpu_lines = [line for line in inventory.splitlines() if line.strip()]
        if len(gpu_lines) != 8 or any(
            "H100" not in line.upper() and "H200" not in line.upper()
            for line in gpu_lines
        ):
            raise RuntimeError(f"invalid full-node GPU allocation:\n{inventory}")
        busy = _exec(
            "test -z \"$(nvidia-smi --query-compute-apps=pid "
            "--format=csv,noheader,nounits | sed '/^[[:space:]]*$/d')\"",
            capture=True,
        )
        if busy.returncode:
            raise RuntimeError("eight-GPU node has active compute processes")
        _exec(f"mkdir -p {shlex.quote(source)} {shlex.quote(remote_output)}")
        _upload(root / "expv2", source + "/expv2")
        _exec(f"mkdir -p {credential_dir}")
        with tempfile.TemporaryDirectory(prefix="expv2-2-matched-credentials-") as tmp:
            credential = Path(tmp) / "env.sh"
            credential.write_text(
                f"export WANDB_API_KEY={shlex.quote(key)}\n"
                "export PYTORCH_ALLOC_CONF=expandable_segments:True\n"
                "export TORCHINDUCTOR_CACHE_DIR=/cache/torchinductor/expv2-2-matched\n"
            )
            os.chmod(credential, 0o600)
            _upload(credential, credential_dir)
        remote_credential = credential_dir + "/env.sh"
        _exec(f"chmod 600 {remote_credential}; test -s {remote_credential}")
        command = " && ".join(
            [
                f"source {shlex.quote(remote_credential)}",
                f"export PYTHONPATH={shlex.quote(source)}:${{PYTHONPATH:-}}",
                "cd /root/fda/NEW",
                "command -v uv >/dev/null",
                "uv run --no-sync python -m expv2.exp2 matched-muon "
                f"--output-root={shlex.quote(remote_output)} "
                f"--result={shlex.quote(remote_result)} "
                f"--heartbeat={shlex.quote(remote_output + '/heartbeat')}",
            ]
        )
        _remote_job(command)
        deadline = time.monotonic() + timeout_seconds
        announced = False
        while time.monotonic() < deadline:
            try:
                _exec(f"test -s /root/{JOB_NAME}.done", capture=True)
                break
            except subprocess.CalledProcessError:
                if not announced:
                    try:
                        launch_text = _exec(f"cat {shlex.quote(launch)}", capture=True).stdout
                    except subprocess.CalledProcessError:
                        launch_text = ""
                    match = re.search(r'https://[^"\s]+', launch_text)
                    if match:
                        print(f"W&B run: {match.group(0)}", flush=True)
                        announced = True
                time.sleep(10)
        else:
            tail = _exec(f"tail -200 /root/{JOB_NAME}.log", capture=True).stdout
            raise TimeoutError(f"matched campaign exceeded {timeout_seconds}s:\n{tail}")
        code = _exec(f"cat /root/{JOB_NAME}.done", capture=True).stdout.strip()
        if code != "0":
            tail = _exec(f"tail -300 /root/{JOB_NAME}.log", capture=True).stdout
            raise RuntimeError(f"matched remote campaign failed rc={code}:\n{tail[-16000:]}")
        _download(remote_result, Path(result_path))
        for name in ("paid-preflight.json", "lr-selection.json", "wandb-launch.json"):
            _download(remote_output + "/" + name, local / name)
        result = json.loads(Path(result_path).read_text())
        result["remote_execution"] = {
            "project": PROJECT_ID,
            "service": SERVICE_ID,
            "source_digest": digest,
            "source_root": source,
            "remote_output": remote_output,
            "job_name": JOB_NAME,
            "gpu_inventory": gpu_lines,
            "lifecycle_started_epoch": lifecycle_started,
            "retrieved_epoch": time.time(),
        }
        atomic_json(result_path, result)
        return result
    except Exception as error:
        campaign_error = error
        raise
    finally:
        try:
            _exec(f"rm -rf {credential_dir}", capture=True)
        except Exception:
            pass
        try:
            pause_service()
        except Exception as pause_error:
            if campaign_error is None:
                raise
            print(f"WARNING: failed to pause matched service: {pause_error}", flush=True)
