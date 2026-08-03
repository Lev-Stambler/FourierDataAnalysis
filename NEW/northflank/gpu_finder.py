#!/usr/bin/env python3
"""Race Northflank H100/H200 capacity across regions and keep one winner."""

from __future__ import annotations

import argparse
import json
import subprocess
import time
from dataclasses import asdict, dataclass
from pathlib import Path


GPU_PRIORITY = ("h200-141", "h100-80")
EPHEMERAL_MB_PER_GPU = {"h200-141": 1_280_000, "h100-80": 512_000}
IMAGE = "nvidia/cuda:12.8.1-devel-ubuntu22.04"


@dataclass(frozen=True)
class Candidate:
    region: str
    gpu_type: str
    gpu_count: int
    price_cents_per_gpu_hour: int
    project_id: str
    service_id: str
    existing: bool = False
    spot: bool = False


def nf(*arguments: str, check: bool = True) -> str:
    result = subprocess.run(
        ["northflank", *arguments],
        check=check,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    return result.stdout


def nf_json(*arguments: str) -> dict:
    return json.loads(nf(*arguments, "-o", "json"))


def managed_candidates(
    regions: dict,
    *,
    count: int,
    project_prefix: str,
    allowed_regions: set[str] | None = None,
) -> list[Candidate]:
    result = []
    for region in regions["regions"]:
        region_id = region["id"]
        if allowed_regions and region_id not in allowed_regions:
            continue
        devices = {value["id"]: value for value in region.get("gpuDevices", [])}
        for gpu_type in GPU_PRIORITY:
            device = devices.get(gpu_type)
            if not device or count not in device["countOptions"]:
                continue
            short = "h200" if gpu_type.startswith("h200") else "h100"
            result.append(
                Candidate(
                    region=region_id,
                    gpu_type=gpu_type,
                    gpu_count=count,
                    price_cents_per_gpu_hour=int(
                        device.get("pricing", {}).get("onDemand", 0)
                    ),
                    project_id=f"{project_prefix}-{region_id}",
                    service_id=f"gpu-{short}-{count}",
                )
            )
    priority = {gpu: index for index, gpu in enumerate(GPU_PRIORITY)}
    return sorted(result, key=lambda value: (priority[value.gpu_type], value.region))


def spot_inventory(team: str) -> list[dict]:
    """Return configured BYOC preemptible pools; an empty list is definitive."""
    clusters = nf_json("list", "cloud", "clusters").get(
        "clusters", []
    )
    result = []
    for cluster in clusters:
        for pool in cluster.get("nodePools") or []:
            if pool.get("preemptible"):
                result.append(
                    {
                        "cluster_id": cluster["id"],
                        "cluster_name": cluster.get("name", cluster["id"]),
                        "node_pool": pool,
                    }
                )
    return result


def parse_existing(values: list[str], team: str) -> list[Candidate]:
    result = []
    for value in values:
        project_id, service_id = value.split("/", 1)
        service = nf_json(
            "get",
            "service",
            "--projectId",
            project_id,
            "--serviceId",
            service_id,
        )
        gpu = service["deployment"]["gpu"]["configuration"]
        if gpu["gpuType"] not in GPU_PRIORITY or int(gpu["gpuCount"]) != 8:
            raise RuntimeError(f"{value} is not an 8x H100/H200 service")
        result.append(
            Candidate(
                region=service["cluster"]["id"].removeprefix("nf-"),
                gpu_type=gpu["gpuType"],
                gpu_count=int(gpu["gpuCount"]),
                price_cents_per_gpu_hour=0,
                project_id=project_id,
                service_id=service_id,
                existing=True,
            )
        )
    return result


def ensure_project(candidate: Candidate, team: str) -> None:
    result = subprocess.run(
        [
            "northflank",
            "get",
            "project",
            "--projectId",
            candidate.project_id,
            "-o",
            "json",
        ],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
    if result.returncode == 0:
        return
    payload = {
        "name": candidate.project_id,
        "region": candidate.region,
        "description": "Reusable H100/H200 capacity race",
    }
    nf("create", "project", "-i", json.dumps(payload))


def service_payload(candidate: Candidate) -> dict:
    return {
        "name": candidate.service_id,
        "description": "H100/H200 capacity racer managed by gpu_finder.py",
        "billing": {
            "deploymentPlan": (
                f"nf-gpu-{candidate.gpu_type}-{candidate.gpu_count}g"
            )
        },
        "deployment": {
            "instances": 1,
            "external": {"imagePath": IMAGE},
            "docker": {
                "configType": "customCommand",
                "customCommand": "sleep infinity",
            },
            "gpu": {
                "enabled": True,
                "configuration": {
                    "gpuType": candidate.gpu_type,
                    "gpuCount": candidate.gpu_count,
                },
            },
            "ssh": {"enabled": True},
            "storage": {
                "ephemeralStorage": {
                    "storageSize": (
                        EPHEMERAL_MB_PER_GPU[candidate.gpu_type]
                        * candidate.gpu_count
                    )
                },
                "shmSize": 16_384,
            },
        },
        "runtimeEnvironment": {
            "HF_HOME": "/cache/hf",
            "HF_XET_HIGH_PERFORMANCE": "1",
            "PYTORCH_ALLOC_CONF": "expandable_segments:True",
            "TORCHINDUCTOR_CACHE_DIR": "/cache/torchinductor",
        },
    }


def ensure_service(candidate: Candidate, team: str) -> None:
    ensure_project(candidate, team)
    result = subprocess.run(
        [
            "northflank",
            "get",
            "service",
            "--projectId",
            candidate.project_id,
            "--serviceId",
            candidate.service_id,
            "-o",
            "json",
        ],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
    if result.returncode:
        nf(
            "create",
            "service",
            "deployment",
            "--projectId",
            candidate.project_id,
            "-i",
            json.dumps(service_payload(candidate)),
        )
    else:
        nf(
            "resume",
            "service",
            "--projectId",
            candidate.project_id,
            "--serviceId",
            candidate.service_id,
            "-i",
            '{"instances":1}',
            check=False,
        )


def status(candidate: Candidate, team: str) -> str:
    data = nf_json(
        "get",
        "service",
        "containers",
        "--projectId",
        candidate.project_id,
        "--serviceId",
        candidate.service_id,
    )
    containers = data.get("containers", [])
    return containers[0]["status"] if containers else "NO_CONTAINER"


def pause(candidate: Candidate, team: str) -> None:
    nf(
        "pause",
        "service",
        "--projectId",
        candidate.project_id,
        "--serviceId",
        candidate.service_id,
        check=False,
    )


def race(args: argparse.Namespace, candidates: list[Candidate]) -> None:
    for candidate in candidates:
        print(json.dumps({"requesting": asdict(candidate)}), flush=True)
        ensure_service(candidate, args.team)
    deadline = time.monotonic() + args.timeout_minutes * 60
    winner = None
    snapshot = {
        f"{candidate.project_id}/{candidate.service_id}": "REQUESTED"
        for candidate in candidates
    }
    poll_index = 0
    try:
        while time.monotonic() < deadline:
            # Poll one candidate per tick. Polling every candidate per tick can
            # exhaust Northflank's hourly API allowance before scarce GPU
            # capacity arrives.
            candidate = candidates[poll_index % len(candidates)]
            key = f"{candidate.project_id}/{candidate.service_id}"
            try:
                state = status(candidate, args.team)
            except Exception as error:
                state = f"ERROR:{error}"
            snapshot[key] = state
            poll_index += 1
            if state == "TASK_RUNNING":
                winner = candidate
            if winner or poll_index % len(candidates) == 0:
                print(json.dumps({"status": snapshot}), flush=True)
            if winner:
                break
            time.sleep(args.poll_seconds)
    except BaseException:
        for candidate in candidates:
            pause(candidate, args.team)
        raise
    if winner is None:
        for candidate in candidates:
            pause(candidate, args.team)
        raise RuntimeError("no H100/H200 candidate reached TASK_RUNNING before timeout")
    for candidate in candidates:
        if candidate != winner:
            pause(candidate, args.team)
    output = {
        "schema": "northflank-gpu-finder-v1",
        "winner": asdict(winner),
        "all_candidates": [asdict(value) for value in candidates],
    }
    Path(args.winner_file).write_text(json.dumps(output, indent=2))
    print(json.dumps(output, indent=2), flush=True)


def arguments() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("command", choices=("inventory", "race"))
    parser.add_argument("--team", default="tearedcoder")
    parser.add_argument("--count", type=int, default=8)
    parser.add_argument("--project-prefix", default="fda-race")
    parser.add_argument("--regions", default="")
    parser.add_argument("--gpu-types", default="h200-141,h100-80")
    parser.add_argument("--existing", action="append", default=[])
    parser.add_argument("--prefer-spot", action="store_true")
    parser.add_argument("--spot-only", action="store_true")
    parser.add_argument("--poll-seconds", type=int, default=5)
    parser.add_argument("--timeout-minutes", type=float, default=30)
    parser.add_argument("--winner-file", default="/tmp/northflank-gpu-winner.json")
    return parser.parse_args()


def main() -> None:
    args = arguments()
    allowed = {value for value in args.regions.split(",") if value} or None
    candidates = managed_candidates(
        nf_json("list", "regions"),
        count=args.count,
        project_prefix=args.project_prefix,
        allowed_regions=allowed,
    )
    allowed_gpu_types = {value for value in args.gpu_types.split(",") if value}
    candidates = [value for value in candidates if value.gpu_type in allowed_gpu_types]
    spots = spot_inventory(args.team)
    existing = parse_existing(args.existing, args.team)
    inventory = {
        "managed_h100_h200": [asdict(value) for value in candidates],
        "configured_byoc_spot_pools": spots,
        "spot_available_to_account": bool(spots),
    }
    print(json.dumps(inventory, indent=2), flush=True)
    if args.command == "inventory":
        return
    if args.spot_only and not spots:
        raise RuntimeError("spot-only requested but the account has no BYOC spot pool")
    if args.prefer_spot and not spots:
        print(
            "No configured BYOC spot pool; racing managed H100/H200 fallback.",
            flush=True,
        )
    unique = {
        (value.project_id, value.service_id): value
        for value in [*existing, *candidates]
    }
    race(args, list(unique.values()))


if __name__ == "__main__":
    main()
