"""Durable cloud-only launcher and retriever for the Exp13 v2 verdict run."""

from __future__ import annotations

import argparse
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

from exp11_kronecker_debug.lm import write_json


PROJECT_ID = "fda-race-us-central"
SERVICE_ID = "gpu-h100-8"
JOB_NAME = "exp13-wikitext-confirmation-v2b"
REMOTE_SENTINEL = "__EXP13_V2_REMOTE_RC__="
EXPERIMENT_ID = "exp13-wikitext-confirmation-8xh100-v2"
EXPECTED_GPU_COUNT = 8


def _target_args() -> list[str]:
    project = os.environ.get("RC_NF_PROJECT", PROJECT_ID)
    service = os.environ.get("RC_NF_SERVICE", SERVICE_ID)
    if (project, service) != (PROJECT_ID, SERVICE_ID):
        raise RuntimeError(
            f"Exp13 v2 is pinned to {PROJECT_ID}/{SERVICE_ID}; got "
            f"{project}/{service}"
        )
    return ["--projectId", project, "--serviceId", service]


def _run(arguments: list[str], **kwargs: Any) -> subprocess.CompletedProcess[str]:
    return subprocess.run(arguments, check=True, text=True, **kwargs)


def _json(arguments: list[str]) -> dict[str, Any]:
    result = _run(arguments, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    return json.loads(result.stdout)


def _service() -> dict[str, Any]:
    return _json(["northflank", "get", "service", *_target_args(), "-o", "json"])


def _containers() -> list[dict[str, Any]]:
    value = _json(
        [
            "northflank",
            "get",
            "service",
            "containers",
            *_target_args(),
            "-o",
            "json",
        ]
    )
    return list(value.get("containers", []))


def _validate_service(value: dict[str, Any]) -> None:
    gpu = (
        value.get("deployment", {})
        .get("gpu", {})
        .get("configuration", {})
    )
    if gpu.get("gpuType") != "h100-80" or int(gpu.get("gpuCount", 0)) != 8:
        raise RuntimeError(f"Exp13 requires the exact 8xH100 service, found {gpu}")


def _ensure_running(timeout_seconds: int = 1200) -> dict[str, Any]:
    service = _service()
    _validate_service(service)
    try:
        containers = _containers()
    except (subprocess.CalledProcessError, json.JSONDecodeError):
        containers = []
    resumed = not any(row.get("status") == "TASK_RUNNING" for row in containers)
    if resumed:
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
    deadline = time.monotonic() + timeout_seconds
    while time.monotonic() < deadline:
        try:
            containers = _containers()
        except (subprocess.CalledProcessError, json.JSONDecodeError):
            containers = []
        if any(row.get("status") == "TASK_RUNNING" for row in containers):
            return {"resumed": resumed, "service": service, "containers": containers}
        time.sleep(5)
    raise TimeoutError("8xH100 service did not reach TASK_RUNNING")


def pause_service() -> None:
    _run(["northflank", "pause", "service", *_target_args()])


def _exec(command: str, *, capture: bool = False) -> subprocess.CompletedProcess[str]:
    payload = base64.b64encode(command.encode()).decode()
    wrapper = (
        "script=$(mktemp /tmp/exp13-v2.XXXXXX) && "
        f"printf %s {shlex.quote(payload)} | base64 -d > \"$script\" && "
        "bash \"$script\"; code=$?; rm -f \"$script\"; "
        f"printf '\\n{REMOTE_SENTINEL}%s\\n' \"$code\"; exit 0"
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
    codes = re.findall(re.escape(REMOTE_SENTINEL) + r"(\d+)", output)
    if not codes:
        raise RuntimeError("Northflank exec lacked remote sentinel:\n" + output[-4000:])
    cleaned = re.sub(
        rf"^\s*{re.escape(REMOTE_SENTINEL)}\d+\s*$",
        "",
        output,
        flags=re.MULTILINE,
    )
    result.stdout = cleaned
    if not capture and cleaned:
        print(cleaned, end="" if cleaned.endswith("\n") else "\n", flush=True)
    if int(codes[-1]):
        raise subprocess.CalledProcessError(int(codes[-1]), result.args, output=cleaned)
    return result


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
        raise RuntimeError(f"failed to retrieve required artifact {remote}")
    return result.returncode == 0


def _source_digest(root: Path) -> str:
    manifest = json.loads(
        (root / "exp13_wikitext_confirmation" / "experiment_v2.json").read_text()
    )
    digest = hashlib.sha256()
    for artifact in manifest["source_artifacts"]:
        path = root / artifact["path"]
        actual = hashlib.sha256(path.read_bytes()).hexdigest()
        if actual != artifact["sha256"]:
            raise RuntimeError(f"source hash mismatch before launch: {path}")
        digest.update(artifact["path"].encode())
        digest.update(actual.encode())
    return digest.hexdigest()


def _start_job(command: str) -> None:
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


def _remote_command(source: str, state: str, credential: str) -> str:
    python = "/root/fda/NEW/.venv/bin/python"
    manifest = source + "/exp13_wikitext_confirmation/experiment_v2.json"
    campaign_result = state + "/stages/wikitext-confirmation-v2/result.json"
    commands = [
        f"source {shlex.quote(credential)}",
        f"export PYTHONPATH={shlex.quote(source)}:${{PYTHONPATH:-}}",
        f"test -x {python}",
        "test -s /cache/exp10/data/wikitext/manifest.json",
        "test -s /cache/exp10/data/wikitext/train.npy",
        "test -s /cache/exp10/data/wikitext/validation.npy",
        "test -s /cache/exp10/data/wikitext/test.npy",
        f"{python} -m research_control run {shlex.quote(manifest)} "
        f"--stage=local-audit --state-root={shlex.quote(state)}",
        f"{python} -m research_control run {shlex.quote(manifest)} "
        f"--stage=prepare-holdout --state-root={shlex.quote(state)}",
        f"{python} -m research_control run {shlex.quote(manifest)} "
        f"--stage=wikitext-confirmation-v2 --state-root={shlex.quote(state)}",
        f"{python} -m exp13_wikitext_confirmation audit "
        f"--campaign-result={shlex.quote(campaign_result)} "
        f"--output={shlex.quote(state + '/campaign-audit.json')}",
        f"{python} -m research_control audit {shlex.quote(manifest)} "
        f"--state-root={shlex.quote(state)}",
    ]
    return " && ".join(commands)


def run_remote(
    *,
    repo_root: str | Path,
    local_state: str | Path,
    timeout_seconds: int = 2400,
) -> dict[str, Any]:
    if not os.environ.get("WANDB_API_KEY"):
        raise RuntimeError("WANDB_API_KEY is required before paid allocation")
    root, local = Path(repo_root).resolve(), Path(local_state).resolve()
    local.mkdir(parents=True, exist_ok=True)
    _target_args()
    lifecycle_started = time.time()
    allocation = _ensure_running()
    digest = _source_digest(root)
    source = f"/cache/exp13/source-{digest[:12]}"
    state = f"/cache/research-control/{EXPERIMENT_ID}-v2b-{digest[:12]}"
    credential_dir = f"/root/{JOB_NAME}-credentials"
    campaign_error: Exception | None = None
    try:
        inventory = _exec(
            "nvidia-smi --query-gpu=index,name,memory.total,utilization.gpu "
            "--format=csv,noheader",
            capture=True,
        ).stdout
        rows = [line for line in inventory.splitlines() if line.strip()]
        if len(rows) != EXPECTED_GPU_COUNT or any("H100" not in row.upper() for row in rows):
            raise RuntimeError(f"invalid Exp13 allocation:\n{inventory}")
        _exec(
            "test -z \"$(nvidia-smi --query-compute-apps=pid "
            "--format=csv,noheader,nounits | sed '/^[[:space:]]*$/d')\"",
            capture=True,
        )
        _exec(f"mkdir -p {shlex.quote(source)} {shlex.quote(state)} {credential_dir}")
        for name in (
            "exp11_kronecker_debug",
            "exp12_deep_kronecker",
            "exp13_wikitext_confirmation",
            "research_control",
        ):
            _upload(root / name, source + "/" + name)
        for name in ("pyproject.toml", "uv.lock"):
            _upload(root / name, source + "/" + name)
        with tempfile.TemporaryDirectory(prefix="exp13-v2-credentials-") as tmp:
            credential = Path(tmp) / "env.sh"
            credential.write_text(
                f"export WANDB_API_KEY={shlex.quote(os.environ['WANDB_API_KEY'])}\n"
                f"export RC_NF_PROJECT={shlex.quote(PROJECT_ID)}\n"
                f"export RC_NF_SERVICE={shlex.quote(SERVICE_ID)}\n"
                "export PYTORCH_ALLOC_CONF=expandable_segments:True\n"
            )
            os.chmod(credential, 0o600)
            _upload(credential, credential_dir)
        remote_credential = credential_dir + "/env.sh"
        _exec(f"chmod 600 {remote_credential}; test -s {remote_credential}")
        _start_job(_remote_command(source, state, remote_credential))
        deadline = time.monotonic() + timeout_seconds
        announced = False
        campaign_result = state + "/stages/wikitext-confirmation-v2/result.json"
        while time.monotonic() < deadline:
            try:
                _exec(f"test -s /root/{JOB_NAME}.done", capture=True)
                break
            except subprocess.CalledProcessError:
                if not announced:
                    try:
                        running = _exec(
                            f"cat {shlex.quote(campaign_result)}", capture=True
                        ).stdout
                    except subprocess.CalledProcessError:
                        running = ""
                    match = re.search(r'https://[^"\s]+', running)
                    if match:
                        print(f"W&B run: {match.group(0)}", flush=True)
                        announced = True
                time.sleep(10)
        else:
            tail = _exec(f"tail -300 /root/{JOB_NAME}.log", capture=True).stdout
            raise TimeoutError(f"Exp13 durable job exceeded {timeout_seconds}s:\n{tail}")
        code = _exec(f"cat /root/{JOB_NAME}.done", capture=True).stdout.strip()
        artifact_map = {
            "result.json": campaign_result,
            "campaign-audit.json": state + "/campaign-audit.json",
            "controller-audit.json": state + "/audit.json",
            "ledger.jsonl": state + "/ledger.jsonl",
            "plan.json": state + "/plan.json",
            "preflight.json": state + "/stages/wikitext-confirmation-v2/campaign-cells/preflight.json",
            "preflight-v2-progress.json": state + "/stages/wikitext-confirmation-v2/preflight-v2-progress.json",
        }
        for local_name, remote_name in artifact_map.items():
            _download(remote_name, local / local_name, required=local_name == "result.json")
        if code != "0":
            tail = _exec(f"tail -400 /root/{JOB_NAME}.log", capture=True).stdout
            raise RuntimeError(f"Exp13 remote job failed rc={code}:\n{tail[-20000:]}")
        result = json.loads((local / "result.json").read_text())
        result["remote_execution"] = {
            "project": PROJECT_ID,
            "service": SERVICE_ID,
            "source_digest": digest,
            "source_root": source,
            "remote_state": state,
            "gpu_inventory": rows,
            "allocation": allocation,
            "lifecycle_started_epoch": lifecycle_started,
            "retrieved_epoch": time.time(),
        }
        write_json(local / "result.json", result)
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
            print(f"WARNING: failed to pause Exp13 service: {pause_error}", flush=True)


def main() -> None:
    parser = argparse.ArgumentParser(description="Launch Exp13 v2 on cloud 8xH100")
    parser.add_argument("--repo-root", default=".")
    parser.add_argument("--local-state", required=True)
    parser.add_argument("--timeout-seconds", type=int, default=2400)
    args = parser.parse_args()
    result = run_remote(
        repo_root=args.repo_root,
        local_state=args.local_state,
        timeout_seconds=args.timeout_seconds,
    )
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)


if __name__ == "__main__":
    main()
