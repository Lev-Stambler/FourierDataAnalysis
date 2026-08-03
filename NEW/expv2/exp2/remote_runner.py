"""Provision, execute, retrieve, and tear down the ExpV2-2 H100 control."""

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

from expv2.exp1.utils import atomic_json

from .infra import PROJECT_ID, SERVICE_ID, delete_service, provision


REMOTE_REPO = "/root/fda/NEW"
REMOTE_RETURN_CODE = "__EXPV2_2_REMOTE_RC__="


def _target_args() -> list[str]:
    project = os.environ.get("RC_NF_PROJECT")
    service = os.environ.get("RC_NF_SERVICE")
    if project != PROJECT_ID or service != SERVICE_ID:
        raise RuntimeError(
            f"expected controller target {PROJECT_ID}/{SERVICE_ID}, got "
            f"{project}/{service}"
        )
    return ["--projectId", project, "--serviceId", service]


def _run(arguments: list[str], **kwargs: Any) -> subprocess.CompletedProcess[str]:
    return subprocess.run(arguments, check=True, text=True, **kwargs)


def _exec(command: str, *, capture: bool = False) -> subprocess.CompletedProcess[str]:
    """Execute remotely while working around the CLI's lossy exit status."""

    payload = base64.b64encode(command.encode()).decode()
    wrapper = (
        "script=$(mktemp /tmp/expv2-2-command.XXXXXX) && "
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
            "Northflank exec lacked a remote return-code sentinel:\n" + output[-4_000:]
        )
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


def _remote_job(
    command: str,
    *,
    name: str,
    timeout_seconds: int,
    poll_seconds: int = 5,
    wandb_launch_path: str | None = None,
) -> None:
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
    announced_wandb = False
    polls = 0
    while time.monotonic() < deadline:
        try:
            _exec(f"test -s {shlex.quote(done)}", capture=True)
            break
        except subprocess.CalledProcessError:
            polls += 1
            if wandb_launch_path and not announced_wandb:
                try:
                    launch = _exec(
                        f"cat {shlex.quote(wandb_launch_path)}", capture=True
                    ).stdout
                except subprocess.CalledProcessError:
                    launch = ""
                match = re.search(r'https://[^"\s]+', launch)
                if match:
                    print(f"W&B run: {match.group(0)}", flush=True)
                    announced_wandb = True
            if polls % max(1, int(30 / poll_seconds)) == 0:
                tail = _exec(
                    f"tail -30 {shlex.quote(log)} 2>/dev/null || true",
                    capture=True,
                ).stdout
                if tail.strip():
                    print(f"[{name}]\n{tail[-6_000:]}", flush=True)
            time.sleep(poll_seconds)
    else:
        tail = _exec(
            f"tail -100 {shlex.quote(log)} 2>/dev/null || true", capture=True
        )
        raise TimeoutError(f"remote job {name} exceeded {timeout_seconds}s:\n{tail.stdout}")
    try:
        _exec(
            f"code=$(cat {shlex.quote(done)}); test \"$code\" = 0",
            capture=True,
        )
    except subprocess.CalledProcessError as error:
        tail = _exec(
            f"tail -200 {shlex.quote(log)} 2>/dev/null || true", capture=True
        )
        raise RuntimeError(f"remote job {name} failed:\n{tail.stdout[-12_000:]}") from error


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


def _source_digest(root: Path) -> str:
    digest = hashlib.sha256()
    for path in sorted((root / "expv2").rglob("*.py")):
        digest.update(str(path.relative_to(root)).encode())
        digest.update(path.read_bytes())
    return digest.hexdigest()


def run_remote(
    *,
    repo_root: Path,
    local_output: Path,
    result_path: Path,
    campaign_kind: str = "positive-control",
) -> dict[str, Any]:
    if campaign_kind not in (
        "positive-control",
        "two-hop-debug",
        "joint-two-hop",
        "full-control",
    ):
        raise ValueError(f"unknown campaign kind: {campaign_kind}")
    if not os.environ.get("WANDB_API_KEY"):
        raise RuntimeError("WANDB_API_KEY must be present before provisioning")
    _target_args()
    local_output.mkdir(parents=True, exist_ok=True)
    provision(local_output / "target.json")
    digest = _source_digest(repo_root)
    remote_output = f"/cache/expv2/exp2/{campaign_kind}-{digest[:12]}"
    result_filename = f"{campaign_kind}-result.json"
    remote_result = f"{remote_output}/{result_filename}"
    campaign_error: Exception | None = None
    try:
        _exec(
            "mkdir -p /root/fda/NEW && "
            "rm -rf /root/fda/NEW/expv2 && mkdir -p /root/fda/NEW/expv2"
        )
        _upload(repo_root / "expv2", REMOTE_REPO + "/expv2")
        _exec(
            f"mkdir -p {shlex.quote(remote_output)} && "
            f"rm -f {shlex.quote(remote_result)} "
            f"{shlex.quote(remote_output + '/paid-preflight.json')} "
            f"{shlex.quote(remote_output + '/wandb-launch.json')}"
        )
        credential_dir = "/root/expv2-2-credentials"
        _exec(f"rm -rf {credential_dir} && mkdir -p {credential_dir}")
        with tempfile.TemporaryDirectory(prefix="expv2-2-credentials-") as temporary:
            credential = Path(temporary) / "env.sh"
            credential.write_text(
                f"export WANDB_API_KEY={shlex.quote(os.environ['WANDB_API_KEY'])}\n"
                "export PYTORCH_ALLOC_CONF=expandable_segments:True\n"
                "export TORCHINDUCTOR_CACHE_DIR=/cache/torchinductor/expv2-2\n"
            )
            os.chmod(credential, 0o600)
            _upload(credential, credential_dir)
        remote_credential = credential_dir + "/env.sh"
        _exec(
            f"test -s {shlex.quote(remote_credential)} && "
            f"chmod 600 {shlex.quote(remote_credential)}"
        )
        bootstrap = " && ".join(
            [
                "export DEBIAN_FRONTEND=noninteractive",
                "export UV_CACHE_DIR=/cache/uv/expv2-2",
                "apt-get update -qq",
                "apt-get install -y -qq --no-install-recommends curl ca-certificates python3 python3-venv >/dev/null",
                "if ! command -v uv >/dev/null; then curl -LsSf https://astral.sh/uv/install.sh | sh >/dev/null; fi",
                "export PATH=/root/.local/bin:$PATH",
                "uv venv --allow-existing /cache/expv2/exp2/venv --python python3",
            ]
        )
        _remote_job(bootstrap, name="expv2-2-bootstrap", timeout_seconds=600)
        python = "/cache/expv2/exp2/venv/bin/python"
        _remote_job(
            "export PATH=/root/.local/bin:$PATH; export UV_CACHE_DIR=/cache/uv/expv2-2; "
            f"uv pip install --python {python} "
            "--index-url https://download.pytorch.org/whl/cu128 torch==2.7.1",
            name="expv2-2-torch-install",
            timeout_seconds=900,
        )
        _remote_job(
            "export PATH=/root/.local/bin:$PATH; export UV_CACHE_DIR=/cache/uv/expv2-2; "
            f"uv pip install --python {python} wandb==0.28.1 numpy==2.1.2 psutil==7.0.0",
            name="expv2-2-dependency-install",
            timeout_seconds=600,
        )
        verification = python + " -c " + shlex.quote(
            "import torch,wandb,numpy,psutil; "
            "assert torch.__version__.startswith('2.7.1+cu128'); "
            "assert torch.cuda.is_available() and torch.cuda.device_count()==1; "
            "assert 'H100' in torch.cuda.get_device_name(0); "
            "x=torch.randn((4096,4096),device='cuda',dtype=torch.bfloat16); "
            "y=x@x; torch.cuda.synchronize(); assert y.isfinite().all(); "
            "print(torch.__version__,torch.cuda.get_device_name(0),y.dtype,flush=True)"
        )
        _remote_job(
            verification,
            name="expv2-2-gpu-verification",
            timeout_seconds=90,
            poll_seconds=2,
        )
        verified = _exec("cat /root/expv2-2-gpu-verification.log", capture=True).stdout
        if "2.7.1+cu128" not in verified or "H100" not in verified:
            raise RuntimeError(f"invalid H100 verification: {verified[-2_000:]}")
        campaign_command = (
            f"{python} -m expv2.exp2 {campaign_kind} "
            f"--output-root={shlex.quote(remote_output)} "
            f"--result={shlex.quote(remote_result)} "
            f"--preflight={shlex.quote(remote_output + '/paid-preflight.json')}"
        )
        campaign = " && ".join(
            [
                f"source {shlex.quote(remote_credential)}",
                "export EXPV2_2_MAX_WALL_SECONDS=2400",
                f"export PYTHONPATH={REMOTE_REPO}",
                f"cd {REMOTE_REPO}",
                campaign_command,
            ]
        )
        try:
            _remote_job(
                campaign,
                name=f"expv2-2-{campaign_kind}",
                timeout_seconds=2_550,
                poll_seconds=10,
                wandb_launch_path=remote_output + "/wandb-launch.json",
            )
        except Exception as error:
            campaign_error = error
        for filename in (
            result_filename,
            "paid-preflight.json",
            "wandb-launch.json",
        ):
            _download(
                f"{remote_output}/{filename}",
                local_output / filename,
                required=filename == result_filename,
            )
        result = json.loads((local_output / result_filename).read_text())
        result["source_sha256"] = digest
        result["target"] = json.loads((local_output / "target.json").read_text())
        atomic_json(result_path, result)
        if campaign_error is not None or result.get("status") == "failed":
            raise RuntimeError(
                f"remote campaign failed: {result.get('reason', campaign_error)}"
            )
        return result
    finally:
        delete_service()


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--repo-root", type=Path, required=True)
    parser.add_argument("--local-output", type=Path, required=True)
    parser.add_argument("--result", type=Path, required=True)
    parser.add_argument(
        "--campaign",
        choices=(
            "positive-control",
            "two-hop-debug",
            "joint-two-hop",
            "full-control",
        ),
        default="positive-control",
    )
    args = parser.parse_args()
    result = run_remote(
        repo_root=args.repo_root,
        local_output=args.local_output,
        result_path=args.result,
        campaign_kind=args.campaign,
    )
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)


if __name__ == "__main__":
    main()
