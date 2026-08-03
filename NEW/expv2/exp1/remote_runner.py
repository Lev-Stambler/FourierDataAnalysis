"""Upload a clean ExpV2-1 snapshot, run it remotely, and retrieve evidence."""

from __future__ import annotations

import argparse
import base64
import json
import os
import re
import shlex
import subprocess
import tempfile
import time
from pathlib import Path
from typing import Any

from .utils import atomic_json


REMOTE_REPO = "/root/fda/NEW"
REMOTE_DATA = "/cache/expv2/exp1/data/tinystories"
REMOTE_OUTPUT = "/cache/expv2/exp1/run"
REMOTE_RETURN_CODE = "__EXPV2_REMOTE_RC__="


def _target_args() -> list[str]:
    project = os.environ.get("RC_NF_PROJECT")
    service = os.environ.get("RC_NF_SERVICE")
    if not project or not service:
        raise RuntimeError("RC_NF_PROJECT and RC_NF_SERVICE are required")
    if "expv2-1" not in service:
        raise RuntimeError(f"refusing non-ExpV2-1 remote service: {service}")
    return ["--projectId", project, "--serviceId", service]


def _run(arguments: list[str], **kwargs: Any) -> subprocess.CompletedProcess[str]:
    return subprocess.run(arguments, check=True, text=True, **kwargs)


def _exec(command: str, *, capture: bool = False) -> subprocess.CompletedProcess[str]:
    # The Northflank CLI currently exits zero even when the command in the
    # container exits nonzero.  Execute an encoded script and parse our own
    # return-code sentinel so a failed remote command can never pass preflight.
    payload = base64.b64encode(command.encode()).decode()
    wrapper = (
        "script=$(mktemp /tmp/expv2-command.XXXXXX) && "
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
        raise RuntimeError(
            "Northflank exec ended without a remote return-code sentinel:\n"
            + output[-4_000:]
        )
    remote_return_code = int(matches[-1])
    cleaned = re.sub(
        rf"^\s*{re.escape(REMOTE_RETURN_CODE)}\d+\s*$", "", output,
        flags=re.MULTILINE,
    )
    result.stdout = cleaned
    if not capture and cleaned:
        print(cleaned, end="" if cleaned.endswith("\n") else "\n", flush=True)
    if remote_return_code:
        raise subprocess.CalledProcessError(
            remote_return_code,
            result.args,
            output=cleaned,
        )
    return result


def _remote_job(
    command: str,
    *,
    name: str,
    timeout_seconds: int,
    poll_seconds: int = 5,
) -> None:
    """Run a durable remote job and verify its actual exit status."""

    if not re.fullmatch(r"[a-z0-9-]+", name):
        raise ValueError(f"unsafe remote job name: {name}")
    log = f"/root/{name}.log"
    done = f"/root/{name}.done"
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
    deadline = time.monotonic() + timeout_seconds
    while time.monotonic() < deadline:
        try:
            _exec(f"test -s {shlex.quote(done)}", capture=True)
            break
        except subprocess.CalledProcessError:
            time.sleep(poll_seconds)
    else:
        tail = _exec(f"tail -100 {shlex.quote(log)} 2>/dev/null || true", capture=True)
        raise TimeoutError(
            f"remote job {name} exceeded {timeout_seconds}s:\n{tail.stdout[-8_000:]}"
        )
    try:
        _exec(
            f"code=$(cat {shlex.quote(done)}); "
            "test \"$code\" = 0",
            capture=True,
        )
    except subprocess.CalledProcessError as error:
        tail = _exec(f"tail -200 {shlex.quote(log)} 2>/dev/null || true", capture=True)
        raise RuntimeError(
            f"remote job {name} failed:\n{tail.stdout[-12_000:]}"
        ) from error


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
        raise RuntimeError(f"failed to download required remote artifact: {remote}")
    return result.returncode == 0


def run_remote(
    *,
    repo_root: Path,
    data_root: Path,
    local_output: Path,
    result_path: Path,
) -> dict[str, Any]:
    if not os.environ.get("WANDB_API_KEY"):
        raise RuntimeError("WANDB_API_KEY must be present before remote launch")
    if not (data_root / "manifest.json").is_file():
        raise RuntimeError(f"prepared TinyStories data is missing: {data_root}")
    _exec(
        "mkdir -p /root/fda/NEW /cache/expv2/exp1/data && "
        "rm -rf /root/fda/NEW/expv2 "
        f"{shlex.quote(REMOTE_OUTPUT)} && mkdir -p {shlex.quote(REMOTE_OUTPUT)}"
    )
    _exec(f"mkdir -p {shlex.quote(REMOTE_REPO + '/expv2')}")
    _upload(repo_root / "expv2", REMOTE_REPO + "/expv2")
    staged_data = True
    try:
        _exec(f"test -f {shlex.quote(REMOTE_DATA + '/manifest.json')}", capture=True)
    except subprocess.CalledProcessError:
        staged_data = False
    if not staged_data:
        _exec(
            f"rm -rf {shlex.quote(REMOTE_DATA)} && "
            f"mkdir -p {shlex.quote(REMOTE_DATA)}"
        )
        _upload(data_root, REMOTE_DATA)
    remote_credential_dir = "/root/expv2-credentials"
    _exec(
        f"rm -rf {remote_credential_dir} && mkdir -p {remote_credential_dir}"
    )
    with tempfile.TemporaryDirectory(prefix="expv2-credentials-") as temporary:
        credential_path = Path(temporary) / "env.sh"
        credential_path.write_text(
            f"export WANDB_API_KEY={shlex.quote(os.environ['WANDB_API_KEY'])}\n"
            "export HF_HOME=/cache/hf\n"
            "export PYTORCH_ALLOC_CONF=expandable_segments:True\n"
            "export TORCHINDUCTOR_CACHE_DIR=/cache/torchinductor/expv2-1\n"
        )
        os.chmod(credential_path, 0o600)
        _upload(credential_path, remote_credential_dir)
    remote_credential = f"{remote_credential_dir}/env.sh"
    _exec(
        f"test -s {shlex.quote(remote_credential)} && "
        f"chmod 600 {shlex.quote(remote_credential)}"
    )
    bootstrap = " && ".join(
        [
            "export DEBIAN_FRONTEND=noninteractive",
            "export UV_CACHE_DIR=/cache/uv/expv2-1",
            "apt-get update -qq",
            "apt-get install -y -qq --no-install-recommends curl ca-certificates python3 python3-venv >/dev/null",
            "if ! command -v uv >/dev/null; then curl -LsSf https://astral.sh/uv/install.sh | sh >/dev/null; fi",
            "export PATH=/root/.local/bin:$PATH",
            "uv venv --allow-existing /root/fda/venv --python python3",
        ]
    )
    _remote_job(
        bootstrap,
        name="expv2-bootstrap",
        timeout_seconds=600,
    )
    _remote_job(
        "export PATH=/root/.local/bin:$PATH; export UV_CACHE_DIR=/cache/uv/expv2-1; "
        "uv pip install --python /root/fda/venv/bin/python "
        "--index-url https://download.pytorch.org/whl/cu128 "
        "torch==2.7.1",
        name="expv2-torch-install",
        timeout_seconds=900,
    )
    _remote_job(
        "export PATH=/root/.local/bin:$PATH; export UV_CACHE_DIR=/cache/uv/expv2-1; "
        "uv pip install --python /root/fda/venv/bin/python "
        "datasets==4.0.0 tokenizers==0.21.4 wandb==0.28.1 "
        "numpy==2.1.2 psutil==7.0.0",
        name="expv2-dependency-install",
        timeout_seconds=600,
    )
    verification = (
        "/root/fda/venv/bin/python -c "
        + shlex.quote(
            "import torch,datasets,tokenizers,wandb,numpy,psutil; "
            "assert torch.__version__.startswith('2.7.1+cu128'); "
            "assert torch.cuda.is_available(); assert torch.cuda.device_count()==1; "
            "assert 'H100' in torch.cuda.get_device_name(0); "
            "x=torch.randn((4096,4096),device='cuda',dtype=torch.bfloat16); "
            "y=x@x; torch.cuda.synchronize(); assert y.isfinite().all(); "
            "print(torch.__version__,torch.cuda.get_device_name(0),y.dtype,flush=True)"
        )
    )
    _remote_job(
        verification,
        name="expv2-gpu-verification",
        timeout_seconds=60,
        poll_seconds=2,
    )
    verified = _exec(
        "cat /root/expv2-gpu-verification.log",
        capture=True,
    ).stdout
    if "2.7.1+cu128" not in verified or "H100" not in verified:
        raise RuntimeError(f"invalid remote GPU verification output: {verified[-2_000:]}")
    remote_result = f"{REMOTE_OUTPUT}/campaign-result.json"
    campaign = " && ".join(
        [
            f"source {shlex.quote(remote_credential)}",
            "export EXPV2_MAX_WALL_SECONDS=1300",
            f"export PYTHONPATH={REMOTE_REPO}",
            "cd /root/fda/NEW",
            "/root/fda/venv/bin/python -m expv2.exp1 run-campaign "
            f"--data-root={shlex.quote(REMOTE_DATA)} "
            f"--output-root={shlex.quote(REMOTE_OUTPUT)} "
            f"--result={shlex.quote(remote_result)} "
            f"--preflight={shlex.quote(REMOTE_OUTPUT + '/paid-preflight.json')}",
        ]
    )
    campaign_error: Exception | None = None
    try:
        _remote_job(
            campaign,
            name="expv2-campaign",
            timeout_seconds=1_450,
            poll_seconds=10,
        )
    except Exception as error:
        campaign_error = error
    local_output.mkdir(parents=True, exist_ok=True)
    for filename in (
        "campaign-result.json",
        "paid-preflight.json",
        "synthetic-gate.json",
        "tinystories-gate.json",
    ):
        _download(
            f"{REMOTE_OUTPUT}/{filename}",
            local_output / filename,
            required=filename == "campaign-result.json",
        )
    remote_campaign = json.loads((local_output / "campaign-result.json").read_text())
    atomic_json(result_path, remote_campaign)
    if campaign_error is not None or remote_campaign.get("status") == "failed":
        raise RuntimeError(
            f"remote campaign failed: {remote_campaign.get('reason', campaign_error)}"
        )
    return remote_campaign


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--repo-root", type=Path, required=True)
    parser.add_argument("--data-root", type=Path, required=True)
    parser.add_argument("--local-output", type=Path, required=True)
    parser.add_argument("--result", type=Path, required=True)
    args = parser.parse_args()
    result = run_remote(
        repo_root=args.repo_root,
        data_root=args.data_root,
        local_output=args.local_output,
        result_path=args.result,
    )
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)


if __name__ == "__main__":
    main()
