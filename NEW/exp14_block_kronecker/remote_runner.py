"""Durable cloud-only launcher for the Exp14 eight-H100 campaign."""

from __future__ import annotations

import argparse
import base64
import gzip
import hashlib
import json
import os
import re
import shlex
import shutil
import subprocess
import tempfile
import time
from pathlib import Path
from typing import Any, Mapping

from exp13_wikitext_confirmation import remote_runner_v2 as infrastructure

from .campaign import write_json


PROJECT_ID = "fda-race-us-central"
TEAM_ID = "tearedcoder"
# The durable service id predates the in-place switch from H200 to H100.  The
# launch gate validates the actual configured and allocated GPU type, not this
# historical id string.
SERVICE_ID = "gpu-h200-8"
JOB_NAME = "exp14-block-kronecker-v1"
COMPILE_JOB_NAME = "exp14-compile-probe-v1"
COMPILED_BATCH_JOB_NAME = "exp14-compiled-batch-probe-v1"
COMPILED_REPLAY_JOB_NAME = "exp14-compiled-replay-v1"
OPTIMIZER_AUDIT_JOB_NAME = "exp14-optimizer-audit-v1"
EXP15_JOB_NAME = "exp15-birouted-kronecker-v1"
EXP16_JOB_NAME = "exp16-router-diagnostics-v1"
EXP17_JOB_NAME = "exp17-group-density-v1"
EXP18_JOB_NAME = "exp18-memorization-v1"
EXP19_JOB_NAME = "exp19-norm-residual-v1"
EXP20_JOB_NAME = "exp20-outer-cache-v1"
EXPECTED_GPU_COUNT = 8
CACHE_ROOT = "/cache"
JOB_HEARTBEAT_STALE_SECONDS = 180
MAXIMUM_JOB_ATTEMPTS = 4
RESUME_BUNDLE_SCHEMA = "exp14-cloud-resume-bundle-v1"
# Northflank treats ``--remotePath`` as a directory and preserves the local
# basename beneath it.
REMOTE_BOOTSTRAP_UV_DIR = "/root/exp14-bootstrap"
REMOTE_BOOTSTRAP_UV = REMOTE_BOOTSTRAP_UV_DIR + "/uv"


def _target_args() -> list[str]:
    return [
        "--teamId",
        os.environ.get("RC_NF_TEAM", TEAM_ID),
        "--projectId",
        os.environ.get("RC_NF_PROJECT", PROJECT_ID),
        "--serviceId",
        os.environ.get("RC_NF_SERVICE", SERVICE_ID),
    ]


# Reuse the hardened upload/exec/lifecycle implementation with explicit org
# token scoping.  Exp13's team-scoped context did not need --teamId.
infrastructure._target_args = _target_args
infrastructure.PROJECT_ID = PROJECT_ID
infrastructure.SERVICE_ID = SERVICE_ID
_RAW_REMOTE_EXEC = infrastructure._exec


def resilient_remote_exec(
    command: str, *, capture: bool = False, attempts: int = 24
) -> subprocess.CompletedProcess[str]:
    """Retry only Northflank's intermittent missing-sentinel transport error."""

    last_error = ""
    for attempt in range(1, attempts + 1):
        try:
            return _RAW_REMOTE_EXEC(command, capture=capture)
        except RuntimeError as error:
            last_error = str(error)
            if "lacked remote sentinel" not in last_error or attempt == attempts:
                raise
            time.sleep(5)
    raise RuntimeError(last_error)  # pragma: no cover - loop always returns or raises


# Keep the existing hardened sentinel/return-code implementation, but make its
# transport robust for every lifecycle call after a pod starts.
infrastructure._exec = resilient_remote_exec


def ensure_running_without_staging_race(
    timeout_seconds: int = 1200,
) -> dict[str, Any]:
    """Treat TASK_STAGING as an active allocation instead of resuming again."""

    service = infrastructure._service()
    infrastructure._validate_service(service)
    try:
        containers = infrastructure._containers()
    except (subprocess.CalledProcessError, json.JSONDecodeError):
        containers = []
    active = any(
        row.get("status") in {"TASK_RUNNING", "TASK_STAGING"}
        for row in containers
    )
    resumed = False
    if not active:
        try:
            infrastructure._run(
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
        except subprocess.CalledProcessError:
            # A concurrent controller can move the service out of PAUSED
            # between the status read and resume request. Accept only a newly
            # visible running/staging task; every other 409 remains fatal.
            containers = infrastructure._containers()
            if not any(
                row.get("status") in {"TASK_RUNNING", "TASK_STAGING"}
                for row in containers
            ):
                raise
    deadline = time.monotonic() + timeout_seconds
    while time.monotonic() < deadline:
        try:
            containers = infrastructure._containers()
        except (subprocess.CalledProcessError, json.JSONDecodeError):
            containers = []
        if any(row.get("status") == "TASK_RUNNING" for row in containers):
            return {
                "resumed": resumed,
                "service": service,
                "containers": containers,
            }
        time.sleep(5)
    raise TimeoutError("8xH100 service did not reach TASK_RUNNING")


infrastructure._ensure_running = ensure_running_without_staging_race


def wait_for_remote_shell(timeout_seconds: int = 300) -> dict[str, Any]:
    """Wait beyond TASK_RUNNING until Northflank exec reaches the container.

    The control plane can briefly advertise a running task before the exec
    stream is ready.  In that window the CLI exits successfully but never
    returns our remote sentinel.  Treat only that specific condition as a
    readiness retry; real remote command failures still surface immediately.
    """

    deadline = time.monotonic() + timeout_seconds
    attempts = 0
    last_error = ""
    while time.monotonic() < deadline:
        attempts += 1
        try:
            infrastructure._exec("true", capture=True)
            return {"ready": True, "attempts": attempts}
        except RuntimeError as error:
            last_error = str(error)
            if "lacked remote sentinel" not in last_error:
                raise
            time.sleep(5)
    raise TimeoutError(
        f"Northflank task ran but remote exec was not ready after {attempts} attempts: "
        f"{last_error[-2000:]}"
    )


def wait_for_gpu_inventory(timeout_seconds: int = 600) -> tuple[str, list[str], int]:
    """Run one detached driver probe until all eight H100s are responsive.

    A newly scheduled pod can expose device nodes before the NVIDIA driver call
    returns.  Run the potentially blocking probe in one bounded background
    loop and poll only its result file, avoiding a pile-up of stuck nvidia-smi
    processes and avoiding Northflank's short foreground exec attachment.
    """

    inventory_path = "/root/exp14-gpu-inventory.ready"
    candidate_path = "/root/exp14-gpu-inventory.candidate"
    log_path = "/root/exp14-gpu-inventory.log"
    command = (
        "nvidia-smi --query-gpu=index,name,memory.total,utilization.gpu "
        "--format=csv,noheader"
    )
    worker = (
        "for attempt in $(seq 1 60); do "
        f"if timeout --kill-after=5 30 {command} > {candidate_path} 2>&1 && "
        f"test \"$(grep -c '^[[:space:]]*[0-9][[:space:]]*,' {candidate_path})\" -eq 8 && "
        f"test \"$(grep -ci 'H100' {candidate_path})\" -eq 8; then "
        f"mv {candidate_path} {inventory_path}; exit 0; fi; "
        "sleep 5; done; exit 1"
    )
    infrastructure._exec(
        f"rm -f {inventory_path} {candidate_path} {log_path}; "
        f"nohup bash -c {shlex.quote(worker)} > {log_path} 2>&1 &",
        capture=True,
    )
    deadline = time.monotonic() + timeout_seconds
    attempts = 0
    last_inventory = ""
    while time.monotonic() < deadline:
        attempts += 1
        last_inventory = infrastructure._exec(
            f"cat {inventory_path} 2>/dev/null || true", capture=True
        ).stdout
        rows = [
            line.strip()
            for line in last_inventory.splitlines()
            if re.match(r"^\s*\d+\s*,", line)
        ]
        if len(rows) == EXPECTED_GPU_COUNT and all(
            "H100" in row.upper() for row in rows
        ) and all(re.search(r",\s*0\s*%\s*$", row) for row in rows):
            return last_inventory, rows, attempts
        time.sleep(5)
    raise RuntimeError(
        "NVIDIA driver did not expose a complete 8xH100 inventory after the "
        f"bounded health probe; last inventory:\n{last_inventory}"
    )


def source_digest(root: Path) -> str:
    digest = hashlib.sha256()
    for directory in (
        "exp10",
        "exp11_kronecker_debug",
        "exp13_wikitext_confirmation",
        "exp14_block_kronecker",
        "exp15_birouted_kronecker",
        "exp16_router_diagnostics",
        "exp17_group_density",
        "exp18_memorization",
        "exp19_norm_residual",
        "exp20_outer_cache",
    ):
        for path in sorted((root / directory).rglob("*")):
            if not path.is_file() or path.suffix not in {".py", ".json", ".md"}:
                continue
            relative = path.relative_to(root)
            if any(
                part
                in {
                    "cloud_state",
                    "compile_state",
                    "compiled_batch_state",
                    "compiled_replay_state",
                    "optimizer_audit_state",
                    "cache_snapshot",
                    "__pycache__",
                    "wandb",
                }
                for part in relative.parts
            ):
                continue
            digest.update(str(relative).encode())
            digest.update(hashlib.sha256(path.read_bytes()).digest())
    for name in ("ARCHITECTURE.md", "pyproject.toml", "uv.lock"):
        path = root / name
        digest.update(name.encode())
        digest.update(hashlib.sha256(path.read_bytes()).digest())
    return digest.hexdigest()


def validate_resume_bundle(bundle: Path, state_digest: str) -> dict[str, Any]:
    """Verify a local scientific-state bundle before allocating paid hardware."""

    root = bundle.resolve()
    manifest_path = root / "manifest.json"
    if not manifest_path.is_file():
        raise ValueError("resume bundle lacks manifest.json")
    manifest = json.loads(manifest_path.read_text())
    files = manifest.get("files")
    if (
        manifest.get("schema") != RESUME_BUNDLE_SCHEMA
        or manifest.get("state_digest") != state_digest
        or not isinstance(files, dict)
        or not files
    ):
        raise ValueError("invalid resume bundle manifest")
    for relative, expected in files.items():
        candidate = (root / relative).resolve()
        if not candidate.is_relative_to(root) or not candidate.is_file():
            raise ValueError(f"invalid resume bundle path: {relative}")
        actual = hashlib.sha256(candidate.read_bytes()).hexdigest()
        if actual != expected:
            raise ValueError(f"resume bundle hash mismatch: {relative}")
    return manifest


def upload_experiment_source(root: Path, source: str) -> None:
    for name in (
        "exp10",
        "exp11_kronecker_debug",
        "exp13_wikitext_confirmation",
        "exp14_block_kronecker",
        "exp15_birouted_kronecker",
        "exp16_router_diagnostics",
        "exp17_group_density",
        "exp18_memorization",
        "exp19_norm_residual",
        "exp20_outer_cache",
    ):
        infrastructure._upload(root / name, source + "/" + name)
    for name in ("pyproject.toml", "uv.lock"):
        infrastructure._upload(root / name, source + "/" + name)


def upload_resume_bundle(
    bundle: Path, state: str, manifest: Mapping[str, Any]
) -> None:
    infrastructure._upload(bundle, state)
    checks = " ".join(
        shlex.quote(f"{digest}  {state}/{relative}")
        for relative, digest in manifest["files"].items()
    )
    infrastructure._exec(
        f"printf '%s\\n' {checks} | sha256sum -c -",
        capture=True,
    )


def verify_wandb_before_allocation() -> tuple[str, dict[str, str]]:
    import wandb

    api = wandb.Api()
    credential = os.environ.get("WANDB_API_KEY") or getattr(api, "api_key", None)
    if not credential:
        raise RuntimeError("a stored or exported W&B credential is required")
    if not wandb.login(key=credential, verify=True, relogin=True):
        raise RuntimeError("W&B credential verification failed")
    viewer = wandb.Api(api_key=credential).viewer
    if not viewer:
        raise RuntimeError("W&B authenticated viewer is missing")
    return str(credential), {"status": "verified", "viewer": str(viewer)}


def upload_remote_credential(credential_value: str, credential_dir: str) -> str:
    """Recreate the ephemeral credential after every container replacement."""

    infrastructure._exec(
        f"mkdir -p {shlex.quote(credential_dir)}; "
        f"find {shlex.quote(credential_dir)} -maxdepth 1 -type f -delete",
        capture=True,
    )
    with tempfile.TemporaryDirectory(prefix="exp14-credentials-") as temporary:
        credential = Path(temporary) / "env.sh"
        credential.write_text(
            f"export WANDB_API_KEY={shlex.quote(credential_value)}\n"
            f"export RC_NF_PROJECT={shlex.quote(PROJECT_ID)}\n"
            f"export RC_NF_SERVICE={shlex.quote(SERVICE_ID)}\n"
            "export PYTORCH_ALLOC_CONF=expandable_segments:True\n"
            "export TOKENIZERS_PARALLELISM=false\n"
            f"export HF_HOME={shlex.quote(CACHE_ROOT + '/hf')}\n"
            f"export TORCHINDUCTOR_CACHE_DIR="
            f"{shlex.quote(CACHE_ROOT + '/torchinductor')}\n"
        )
        os.chmod(credential, 0o600)
        infrastructure._upload(credential, credential_dir + "/env.sh")
    remote = credential_dir + "/env.sh"
    infrastructure._exec(
        f"chmod 600 {shlex.quote(remote)}; test -s {shlex.quote(remote)}",
        capture=True,
    )
    return remote


def _job_paths(state: str, job_name: str, attempt: int) -> dict[str, str | int]:
    root = f"{state}/runner/{job_name}"
    return {
        "root": root,
        "done": root + "/done",
        "heartbeat": state + "/heartbeat",
        "log": root + f"/attempt-{attempt}.log",
        "attempt": attempt,
    }


def _start_job(
    command: str, job_name: str, state: str, attempt: int
) -> dict[str, str | int]:
    paths = _job_paths(state, job_name, attempt)
    log = str(paths["log"])
    done = str(paths["done"])
    heartbeat = str(paths["heartbeat"])
    payload = base64.b64encode(command.encode()).decode()
    worker = (
        f"printf %s {shlex.quote(payload)} | base64 -d | bash > {log} 2>&1; "
        f"code=$?; printf '%s\\n' \"$code\" > {done}"
    )
    infrastructure._exec(
        f"mkdir -p {shlex.quote(str(paths['root']))}; "
        f"rm -f {shlex.quote(log)} {shlex.quote(done)}; "
        f"touch {shlex.quote(heartbeat)}; "
        f"nohup bash -c {shlex.quote(worker)} >/dev/null 2>&1 &",
        capture=True,
    )
    return paths


def remote_job_status(paths: dict[str, str | int]) -> dict[str, Any]:
    """Read persistent completion or detect a stale campaign heartbeat."""

    done = shlex.quote(str(paths["done"]))
    heartbeat = shlex.quote(str(paths["heartbeat"]))
    command = (
        f"if test -s {done}; then "
        f"code=$(tail -1 {done}); echo EXP14_JOB_DONE $code; "
        f"elif test -e {heartbeat}; then "
        f"now=$(date +%s); modified=$(stat -c %Y {heartbeat}); age=$((now-modified)); "
        f"if test $age -gt {JOB_HEARTBEAT_STALE_SECONDS}; then "
        "echo EXP14_JOB_STALE $age; else echo EXP14_JOB_RUNNING $age; fi; "
        "else echo EXP14_JOB_MISSING; fi"
    )
    output = infrastructure._exec(command, capture=True).stdout
    matches = re.findall(
        r"^EXP14_JOB_(DONE|STALE|RUNNING|MISSING)(?:\s+(\d+))?\s*$",
        output,
        flags=re.MULTILINE,
    )
    if not matches:
        raise RuntimeError(f"could not parse persistent job status: {output[-2000:]}")
    status, value = matches[-1]
    result: dict[str, Any] = {"status": status.lower(), "raw": output[-2000:]}
    if status == "DONE":
        result["return_code"] = int(value)
    elif status in {"RUNNING", "STALE"}:
        result["heartbeat_age_seconds"] = int(value)
    return result


def run_remote_command_detached(
    command: str, *, operation: str, timeout_seconds: int = 900
) -> dict[str, Any]:
    """Run a long setup command without relying on the short exec attachment."""

    if not re.fullmatch(r"[a-z0-9-]+", operation):
        raise ValueError(f"unsafe detached operation name: {operation}")
    log = f"/root/exp14-{operation}.log"
    done = f"/root/exp14-{operation}.done"
    payload = base64.b64encode(command.encode()).decode()
    worker = (
        f"printf %s {shlex.quote(payload)} | base64 -d | bash > {log} 2>&1; "
        f"code=$?; printf '%s\\n' \"$code\" > {done}"
    )
    infrastructure._exec(
        f"rm -f {shlex.quote(log)} {shlex.quote(done)}; "
        f"nohup bash -c {shlex.quote(worker)} >/dev/null 2>&1 &",
        capture=True,
    )
    deadline = time.monotonic() + timeout_seconds
    polls = 0
    while time.monotonic() < deadline:
        polls += 1
        raw = infrastructure._exec(
            f"cat {shlex.quote(done)} 2>/dev/null || true", capture=True
        ).stdout
        codes = re.findall(r"^\s*(\d+)\s*$", raw, flags=re.MULTILINE)
        if codes:
            code = int(codes[-1])
            if code:
                tail = infrastructure._exec(
                    f"tail -200 {shlex.quote(log)}", capture=True
                ).stdout
                raise subprocess.CalledProcessError(code, command, output=tail)
            return {"operation": operation, "polls": polls, "status": "complete"}
        time.sleep(2)
    tail = infrastructure._exec(
        f"tail -200 {shlex.quote(log)} 2>/dev/null || true", capture=True
    ).stdout
    raise TimeoutError(f"remote {operation} timed out:\n{tail[-10000:]}")


def retrieve_artifact(remote: str, local: Path, *, required: bool = True) -> bool:
    """Retrieve a file, falling back when Northflank returns a corrupt tar."""

    if infrastructure._download(remote, local, required=False):
        return True
    try:
        transport = infrastructure._exec(
            f"test -s {shlex.quote(remote)}; "
            f"gzip -c {shlex.quote(remote)} | base64 -w0",
            capture=True,
        ).stdout
        candidates = re.findall(
            r"^[A-Za-z0-9+/=]{16,}$", transport, flags=re.MULTILINE
        )
        if not candidates:
            raise RuntimeError("artifact fallback returned no base64 payload")
        encoded = max(candidates, key=len)
        payload = gzip.decompress(base64.b64decode(encoded, validate=True))
        if not payload:
            raise RuntimeError("retrieved empty artifact")
        local.parent.mkdir(parents=True, exist_ok=True)
        temporary = local.with_suffix(local.suffix + ".tmp")
        temporary.write_bytes(payload)
        temporary.replace(local)
        return True
    except Exception:
        if required:
            raise RuntimeError(f"failed to retrieve required artifact {remote}")
        return False


def _remote_command(
    source: str, state: str, credential: str, program: str
) -> str:
    runtime = CACHE_ROOT + "/exp14/runtime"
    uv = runtime + "/bin/uv"
    python = runtime + "/venv/bin/python"
    result = state + "/result.json"
    heartbeat = state + "/heartbeat"
    bootstrap = (
        f"mkdir -p {shlex.quote(runtime + '/bin')} {shlex.quote(runtime + '/uv-cache')} && "
        f"if test ! -x {shlex.quote(uv)}; then "
        f"cp {shlex.quote(REMOTE_BOOTSTRAP_UV)} {shlex.quote(uv)}; "
        f"chmod 755 {shlex.quote(uv)}; "
        "fi; "
        f"if test ! -x {shlex.quote(python)}; then "
        f"UV_CACHE_DIR={shlex.quote(runtime + '/uv-cache')} "
        f"{shlex.quote(uv)} python install 3.13; "
        f"{shlex.quote(uv)} venv --python 3.13 {shlex.quote(runtime + '/venv')}; "
        f"UV_CACHE_DIR={shlex.quote(runtime + '/uv-cache')} "
        f"{shlex.quote(uv)} pip install --python {shlex.quote(python)} "
        "torch==2.10.0 --index-url https://download.pytorch.org/whl/cu128; "
        "fi; "
        f"if ! {shlex.quote(python)} -c "
        + shlex.quote(
            "import datasets, numpy, tokenizers, wandb; "
            "from fla.modules import FusedLinearCrossEntropyLoss"
        )
        + " >/dev/null 2>&1; then "
        f"UV_CACHE_DIR={shlex.quote(runtime + '/uv-cache')} "
        f"{shlex.quote(uv)} pip install --python {shlex.quote(python)} "
        "numpy==2.5.1 wandb==0.28.1 flash-linear-attention==0.5.2 "
        "datasets==4.4.0 tokenizers==0.22.2; "
        "fi"
    )
    prepare_data = (
        f"if test ! -s {CACHE_ROOT}/exp10/data/wikitext/manifest.json; then "
        "echo 'EXP14_DATA_PREP_START' $(date -u +%s); "
        f"mkdir -p {CACHE_ROOT}/exp10/data; "
        f"cd {shlex.quote(source)}; "
        f"{shlex.quote(python)} exp10/prepare_data.py tokenizer "
        f"--output={CACHE_ROOT}/exp10/tokenizer --documents-per-dataset=100000; "
        f"{shlex.quote(python)} exp10/prepare_data.py corpus "
        f"--dataset=wikitext --tokenizer-root={CACHE_ROOT}/exp10/tokenizer "
        f"--output={CACHE_ROOT}/exp10/data/wikitext --train-tokens=0 --eval-tokens=0; "
        "echo 'EXP14_DATA_PREP_DONE' $(date -u +%s); "
        "fi"
    )
    verify_data = (
        "printf '%s  %s\\n' "
        "aef005c8fa4a86f1033cf0a0f0524873f8dfd35368d638f618afa8022e5ed2fe "
        f"{CACHE_ROOT}/exp10/data/wikitext/manifest.json "
        "0840f947e9b4a86e96d9f24e23db9f6d204f5c8e356dff9cb3f917e3db494419 "
        f"{CACHE_ROOT}/exp10/data/wikitext/train.npy "
        "c9dca5e3979aa7c80084c39ac7eac964845da76897694539887678ee4860e09c "
        f"{CACHE_ROOT}/exp10/data/wikitext/validation.npy "
        "9bb7235fa485da526b56fa2e6033c82e601c94c95e596bba65e6fb87180ee7d1 "
        f"{CACHE_ROOT}/exp10/data/wikitext/test.npy | sha256sum -c -"
    )
    modules = {
        "exp15_campaign": "exp15_birouted_kronecker.campaign",
        "exp16_campaign": "exp16_router_diagnostics.campaign",
        "exp17_campaign": "exp17_group_density.campaign",
        "exp18_campaign": "exp18_memorization.campaign",
        "exp19_campaign": "exp19_norm_residual.campaign",
        "exp20_campaign": "exp20_outer_cache.campaign",
    }
    module = modules.get(program, "exp14_block_kronecker." + program)
    commands = [
        f"source {shlex.quote(credential)}",
        f"export PYTHONPATH={shlex.quote(source)}:${{PYTHONPATH:-}}",
        bootstrap,
        f"{shlex.quote(python)} -c "
        + shlex.quote(
            "import torch, wandb; from fla.modules import FusedLinearCrossEntropyLoss; "
            "assert torch.cuda.device_count() == 8; "
            "print('EXP14_RUNTIME_READY', torch.__version__, torch.cuda.device_count(), flush=True)"
        ),
        prepare_data,
        verify_data,
        f"{python} -m {module} "
        f"--output={shlex.quote(result)} "
        f"--data-root={CACHE_ROOT}/exp10/data/wikitext "
        f"--heartbeat={shlex.quote(heartbeat)}",
    ]
    return " && ".join(commands)


def run_remote(
    *,
    repo_root: str | Path,
    local_state: str | Path,
    timeout_seconds: int = 7200,
    program: str = "campaign",
    resume_state_digest: str | None = None,
    resume_bundle: str | Path | None = None,
) -> dict[str, Any]:
    root = Path(repo_root).resolve()
    local = Path(local_state).resolve()
    local.mkdir(parents=True, exist_ok=True)
    jobs = {
        "campaign": JOB_NAME,
        "compile_probe": COMPILE_JOB_NAME,
        "compiled_batch_probe": COMPILED_BATCH_JOB_NAME,
        "compiled_replay": COMPILED_REPLAY_JOB_NAME,
        "optimizer_audit": OPTIMIZER_AUDIT_JOB_NAME,
        "exp15_campaign": EXP15_JOB_NAME,
        "exp16_campaign": EXP16_JOB_NAME,
        "exp17_campaign": EXP17_JOB_NAME,
        "exp18_campaign": EXP18_JOB_NAME,
        "exp19_campaign": EXP19_JOB_NAME,
        "exp20_campaign": EXP20_JOB_NAME,
    }
    if program not in jobs:
        raise ValueError(f"unknown Exp14 cloud program: {program}")
    job_name = jobs[program]
    if resume_state_digest is not None and not re.fullmatch(
        r"[0-9a-f]{12}", resume_state_digest
    ):
        raise ValueError("resume state digest must be exactly 12 lowercase hex digits")
    if resume_bundle is not None and resume_state_digest is None:
        raise ValueError("resume bundle requires an explicit resume state digest")
    bundle = Path(resume_bundle).resolve() if resume_bundle is not None else None
    resume_manifest = (
        validate_resume_bundle(bundle, resume_state_digest)
        if bundle is not None and resume_state_digest is not None
        else None
    )
    if not (root / "exp14_block_kronecker" / "campaign.py").is_file():
        raise RuntimeError(f"invalid Exp14 repository root: {root}")
    target_team = os.environ.get("RC_NF_TEAM", TEAM_ID)
    target_project = os.environ.get("RC_NF_PROJECT", PROJECT_ID)
    target_service = os.environ.get("RC_NF_SERVICE", SERVICE_ID)
    if not all(
        re.fullmatch(r"[a-z0-9][a-z0-9-]{1,62}", value)
        for value in (target_team, target_project, target_service)
    ):
        raise RuntimeError("invalid Northflank target identifier")
    wandb_credential, wandb_preflight = verify_wandb_before_allocation()
    lifecycle_started = time.time()
    allocation = infrastructure._ensure_running(timeout_seconds=1200)
    allocation["remote_shell"] = wait_for_remote_shell()
    digest = source_digest(root)
    source = f"/cache/exp14/source-{digest[:12]}"
    state_digest = resume_state_digest or digest[:12]
    state = f"{CACHE_ROOT}/exp14/state-{state_digest}"
    credential_dir = f"/root/{job_name}-credentials"
    remote_snapshot = "/root/exp14-frozen-data.tar"
    local_snapshot = root / ".cloud_cache" / "exp14" / "exp10.tar"
    campaign_error: BaseException | None = None
    paused = False
    try:
        inventory, rows, inventory_attempts = wait_for_gpu_inventory()
        allocation["gpu_inventory_attempts"] = inventory_attempts
        infrastructure._exec(
            f"mkdir -p {shlex.quote(source)} {shlex.quote(state)} "
            f"{shlex.quote(credential_dir)}",
            capture=True,
        )
        infrastructure._exec(
            f"find {shlex.quote(credential_dir)} -maxdepth 1 -type f -delete",
            capture=True,
        )
        if local_snapshot.is_file():
            infrastructure._upload(local_snapshot, remote_snapshot)
            allocation["cache_restore"] = run_remote_command_detached(
                f"mkdir -p {CACHE_ROOT}; "
                f"tar -C {CACHE_ROOT} -xf {shlex.quote(remote_snapshot)}",
                operation="cache-restore",
            )
        upload_experiment_source(root, source)
        if bundle is not None and resume_manifest is not None:
            upload_resume_bundle(bundle, state, resume_manifest)
            allocation["resume_bundle"] = {
                "path": str(bundle),
                "manifest": resume_manifest,
            }
        local_uv = shutil.which("uv")
        if not local_uv:
            raise RuntimeError("local uv binary is required for remote bootstrap")
        infrastructure._upload(Path(local_uv), REMOTE_BOOTSTRAP_UV_DIR)
        infrastructure._exec(
            f"chmod 755 {shlex.quote(REMOTE_BOOTSTRAP_UV)}; "
            f"{shlex.quote(REMOTE_BOOTSTRAP_UV)} --version",
            capture=True,
        )
        remote_credential = upload_remote_credential(
            wandb_credential, credential_dir
        )
        attempt = 1
        job_paths = _start_job(
            _remote_command(source, state, remote_credential, program),
            job_name,
            state,
            attempt,
        )
        job_attempts = [dict(job_paths)]
        allocation["job_attempts"] = job_attempts
        result_path = state + "/result.json"
        deadline = time.monotonic() + timeout_seconds
        announced_url = ""
        code: str | None = None
        status_transport_errors = 0
        while time.monotonic() < deadline:
            try:
                status = remote_job_status(job_paths)
                status_transport_errors = 0
            except (subprocess.CalledProcessError, RuntimeError) as error:
                status_transport_errors += 1
                allocation["last_job_status_transport_error"] = {
                    "count": status_transport_errors,
                    "type": type(error).__name__,
                    "message": str(error)[-2000:],
                }
                if status_transport_errors >= 30:
                    raise RuntimeError(
                        "remote job status remained unreadable for 30 polls"
                    ) from error
                time.sleep(10)
                continue
            if status["status"] == "done":
                code = str(status["return_code"])
                break
            if status["status"] in {"stale", "missing"}:
                if attempt >= MAXIMUM_JOB_ATTEMPTS:
                    raise RuntimeError(
                        "remote campaign heartbeat remained stale after "
                        f"{attempt} attempts: {status}"
                    )
                replacement = wait_for_remote_shell()
                replacement_inventory, replacement_rows, replacement_checks = (
                    wait_for_gpu_inventory()
                )
                infrastructure._exec(
                    f"mkdir -p {shlex.quote(source)} {shlex.quote(state)}",
                    capture=True,
                )
                if local_snapshot.is_file():
                    infrastructure._upload(local_snapshot, remote_snapshot)
                    run_remote_command_detached(
                        f"mkdir -p {CACHE_ROOT}; "
                        f"tar -C {CACHE_ROOT} -xf {shlex.quote(remote_snapshot)}",
                        operation=f"cache-restore-attempt-{attempt + 1}",
                    )
                upload_experiment_source(root, source)
                infrastructure._upload(Path(local_uv), REMOTE_BOOTSTRAP_UV_DIR)
                infrastructure._exec(
                    f"chmod 755 {shlex.quote(REMOTE_BOOTSTRAP_UV)}",
                    capture=True,
                )
                if bundle is not None and resume_manifest is not None:
                    upload_resume_bundle(bundle, state, resume_manifest)
                remote_credential = upload_remote_credential(
                    wandb_credential, credential_dir
                )
                attempt += 1
                job_paths = _start_job(
                    _remote_command(source, state, remote_credential, program),
                    job_name,
                    state,
                    attempt,
                )
                job_paths.update(
                    {
                        "restart_reason": status,
                        "remote_shell": replacement,
                        "gpu_inventory": replacement_rows,
                        "gpu_inventory_attempts": replacement_checks,
                        "inventory": replacement_inventory,
                    }
                )
                job_attempts.append(dict(job_paths))
                announced_url = ""
                continue
            if not announced_url:
                try:
                    running = infrastructure._exec(
                        f"cat {shlex.quote(result_path)}", capture=True
                    ).stdout
                except subprocess.CalledProcessError:
                    running = ""
                match = re.search(r'https://[^"\s]+', running)
                if match:
                    announced_url = match.group(0)
                    print(f"W&B run: {announced_url}", flush=True)
            time.sleep(10)
        else:
            tail = infrastructure._exec(
                f"tail -300 {shlex.quote(str(job_paths['log']))}", capture=True
            ).stdout
            raise TimeoutError(f"Exp14 exceeded {timeout_seconds}s:\n{tail[-20000:]}")
        if code is None:
            raise RuntimeError("remote campaign ended without a return code")
        if program in {
            "campaign",
            "exp15_campaign",
            "exp16_campaign",
            "exp17_campaign",
            "exp18_campaign",
            "exp19_campaign",
            "exp20_campaign",
        }:
            preflight_directory = (
                "memorization-cells"
                if program == "exp18_campaign"
                else "exp20-cells"
                if program == "exp20_campaign"
                else "norm-cells"
                if program == "exp19_campaign"
                else "mechanism-cells"
                if program == "exp17_campaign"
                else "campaign-cells"
            )
            retrieve_artifact(
                state + f"/{preflight_directory}/preflight.json",
                local / "preflight.json",
                required=False,
            )
        for record in job_attempts:
            retrieve_artifact(
                str(record["log"]),
                local / f"remote-attempt-{int(record['attempt'])}.log",
                required=False,
            )
        retrieve_artifact(
            str(job_paths["log"]), local / "remote.log", required=True
        )
        if code != "0":
            tail = (local / "remote.log").read_text()[-20000:]
            raise RuntimeError(f"Exp14 remote campaign failed rc={code}:\n{tail}")
        retrieve_artifact(result_path, local / "result.json", required=True)
        result = json.loads((local / "result.json").read_text())
        if not result.get("wandb_url"):
            raise RuntimeError("completed paid run lacks a direct W&B URL")
        result["remote_execution"] = {
            "project": target_project,
            "team": target_team,
            "service": target_service,
            "source_digest": digest,
            "state_digest": state_digest,
            "resumed_prior_state": resume_state_digest is not None,
            "source_root": source,
            "remote_state": state,
            "program": program,
            "gpu_inventory": rows,
            "allocation": allocation,
            "wandb_preflight": wandb_preflight,
            "job_attempts": job_attempts,
            "cache_strategy": (
                "verified corpus snapshot and external SHA-256 resume bundle "
                "rehydrated into instance-ephemeral /cache; credentials remain ephemeral"
            ),
            "lifecycle_started_epoch": lifecycle_started,
            "retrieved_epoch": time.time(),
        }
        write_json(local / "result.json", result)
        return result
    except BaseException as error:
        campaign_error = error
        raise
    finally:
        try:
            local_snapshot.parent.mkdir(parents=True, exist_ok=True)
            if not local_snapshot.is_file():
                run_remote_command_detached(
                    f"test -s {CACHE_ROOT}/exp10/data/wikitext/manifest.json; "
                    f"tar -C {CACHE_ROOT} -cf {shlex.quote(remote_snapshot)} exp10",
                    operation="cache-snapshot",
                )
                candidate_snapshot = local_snapshot.with_suffix(".candidate.tar")
                if infrastructure._download(
                    remote_snapshot, candidate_snapshot, required=False
                ):
                    validation = subprocess.run(
                        ["tar", "-tf", str(candidate_snapshot)],
                        check=False,
                        stdout=subprocess.DEVNULL,
                        stderr=subprocess.DEVNULL,
                    )
                    if validation.returncode == 0:
                        candidate_snapshot.replace(local_snapshot)
                    else:
                        candidate_snapshot.unlink(missing_ok=True)
                        print(
                            "WARNING: rejected incomplete Exp14 corpus cache snapshot",
                            flush=True,
                        )
        except Exception:
            pass
        try:
            infrastructure._exec(
                f"find {shlex.quote(credential_dir)} -maxdepth 1 -type f -delete; "
                f"rmdir {shlex.quote(credential_dir)} 2>/dev/null || true; "
                f"rm -f {shlex.quote(REMOTE_BOOTSTRAP_UV)}; "
                f"rmdir {shlex.quote(REMOTE_BOOTSTRAP_UV_DIR)} 2>/dev/null || true",
                capture=True,
            )
        except Exception:
            pass
        try:
            infrastructure.pause_service()
            paused = True
        except Exception as pause_error:
            if campaign_error is None:
                raise
            print(f"WARNING: failed to pause Exp14 service: {pause_error}", flush=True)
        finally:
            write_json(
                local / "lifecycle.json",
                {
                    "project": target_project,
                    "team": target_team,
                    "service": target_service,
                    "paused": paused,
                    "finished_epoch": time.time(),
                    "had_campaign_error": campaign_error is not None,
                },
            )


def main() -> None:
    parser = argparse.ArgumentParser(description="Launch Exp14 on cloud 8xH100")
    parser.add_argument("--repo-root", default=".")
    parser.add_argument("--local-state", required=True)
    parser.add_argument("--timeout-seconds", type=int, default=7200)
    parser.add_argument(
        "--resume-state-digest",
        help="Reuse a verified /cache/exp14/state-<12 hex> cell directory",
    )
    parser.add_argument(
        "--resume-bundle",
        help="Upload a locally verified scientific-state bundle before launch",
    )
    parser.add_argument(
        "--program",
        choices=(
            "campaign",
            "compile_probe",
            "compiled_batch_probe",
            "compiled_replay",
            "optimizer_audit",
            "exp15_campaign",
            "exp16_campaign",
            "exp17_campaign",
            "exp18_campaign",
            "exp19_campaign",
            "exp20_campaign",
        ),
        default="campaign",
    )
    args = parser.parse_args()
    result = run_remote(
        repo_root=args.repo_root,
        local_state=args.local_state,
        timeout_seconds=args.timeout_seconds,
        program=args.program,
        resume_state_digest=args.resume_state_digest,
        resume_bundle=args.resume_bundle,
    )
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)


if __name__ == "__main__":
    main()
