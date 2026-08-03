"""Provision exactly one disposable H100 service for ExpV2-1."""

from __future__ import annotations

import json
import subprocess
import time
from pathlib import Path
from typing import Any

from .utils import atomic_json


PROJECT_ID = "fda-test"
SERVICE_ID = "expv2-1-h100"
REGION = "us-central"
GPU_TYPE = "h100-80"
GPU_COUNT = 1
IMAGE = "nvidia/cuda:12.8.1-devel-ubuntu22.04"
VOLUME_ID = "fda-cache"


def _run(*arguments: str, check: bool = True) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        ["northflank", *arguments],
        check=check,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )


def _json(*arguments: str) -> dict[str, Any]:
    return json.loads(_run(*arguments, "-o", "json").stdout)


def service_payload() -> dict[str, Any]:
    return {
        "name": SERVICE_ID,
        "description": "Disposable one-H100 ExpV2-1 strict pilot",
        "billing": {"deploymentPlan": "nf-gpu-h100-80-1g"},
        "deployment": {
            "instances": 1,
            "external": {"imagePath": IMAGE},
            "docker": {
                "configType": "customCommand",
                "customCommand": "sleep infinity",
            },
            "gpu": {
                "enabled": True,
                "configuration": {"gpuType": GPU_TYPE, "gpuCount": GPU_COUNT},
            },
            "ssh": {"enabled": True},
            "storage": {
                "ephemeralStorage": {"storageSize": 512_000},
                "shmSize": 32_768,
            },
        },
        "runtimeEnvironment": {
            "HF_HOME": "/cache/hf",
            "HF_XET_HIGH_PERFORMANCE": "1",
            "PYTORCH_ALLOC_CONF": "expandable_segments:True",
            "TORCHINDUCTOR_CACHE_DIR": "/cache/torchinductor/expv2-1",
        },
    }


def _service() -> dict[str, Any] | None:
    result = _run(
        "get",
        "service",
        "--projectId",
        PROJECT_ID,
        "--serviceId",
        SERVICE_ID,
        "-o",
        "json",
        check=False,
    )
    return json.loads(result.stdout) if result.returncode == 0 else None


def _validate_service(service: dict[str, Any]) -> None:
    gpu = service.get("deployment", {}).get("gpu", {}).get("configuration", {})
    if gpu.get("gpuType") != GPU_TYPE or int(gpu.get("gpuCount", 0)) != GPU_COUNT:
        raise RuntimeError(
            f"existing {PROJECT_ID}/{SERVICE_ID} is not exactly one H100"
        )


def _attach_cache(*, timeout_seconds: int = 120) -> None:
    deadline = time.monotonic() + timeout_seconds
    payload = {"nfObject": {"id": SERVICE_ID, "type": "service"}}
    last_error = "service or volume not ready"
    while time.monotonic() < deadline:
        volume = _json(
            "get", "volume", "--projectId", PROJECT_ID, "--volumeId", VOLUME_ID
        )
        attached = {item["id"] for item in volume.get("attachedObjects", [])}
        if SERVICE_ID in attached:
            return
        result = _run(
            "attach",
            "volume",
            "--projectId",
            PROJECT_ID,
            "--volumeId",
            VOLUME_ID,
            "-i",
            json.dumps(payload),
            check=False,
        )
        if result.returncode == 0:
            return
        last_error = result.stderr.strip() or result.stdout.strip()
        time.sleep(3)
    raise RuntimeError(f"failed to attach {VOLUME_ID}: {last_error}")


def _container_status() -> str:
    value = _json(
        "get",
        "service",
        "containers",
        "--projectId",
        PROJECT_ID,
        "--serviceId",
        SERVICE_ID,
    )
    containers = value.get("containers", [])
    return containers[0].get("status", "NO_CONTAINER") if containers else "NO_CONTAINER"


def provision(
    output: str | Path,
    *,
    timeout_seconds: int = 900,
) -> dict[str, Any]:
    service = _service()
    created = service is None
    if service is None:
        _run(
            "create",
            "service",
            "deployment",
            "--projectId",
            PROJECT_ID,
            "-i",
            json.dumps(service_payload()),
        )
    else:
        _validate_service(service)
        _run(
            "resume",
            "service",
            "--projectId",
            PROJECT_ID,
            "--serviceId",
            SERVICE_ID,
            "-i",
            '{"instances":1}',
            check=False,
        )
    _attach_cache()
    deadline = time.monotonic() + timeout_seconds
    status = "REQUESTED"
    while time.monotonic() < deadline:
        status = _container_status()
        if status == "TASK_RUNNING":
            break
        time.sleep(5)
    if status != "TASK_RUNNING":
        _run(
            "delete",
            "service",
            "--projectId",
            PROJECT_ID,
            "--serviceId",
            SERVICE_ID,
            "--force",
            check=False,
        )
        raise RuntimeError(f"one-H100 service failed to start: {status}")
    service = _service()
    assert service is not None
    _validate_service(service)
    result = {
        "schema": "expv2-1-h100-target-v1",
        "status": "complete",
        "project_id": PROJECT_ID,
        "service_id": SERVICE_ID,
        "region": REGION,
        "gpu_type": GPU_TYPE,
        "gpu_count": GPU_COUNT,
        "price_per_gpu_hour": 2.74,
        "created": created,
        "container_status": status,
        "cache_volume": VOLUME_ID,
    }
    atomic_json(output, result)
    return result
