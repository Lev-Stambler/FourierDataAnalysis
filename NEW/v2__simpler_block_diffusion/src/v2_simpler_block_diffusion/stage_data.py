from __future__ import annotations

import argparse
import concurrent.futures
import hashlib
import json
import os
from pathlib import Path
from urllib.parse import quote

import requests

from .data import DataSource, load_and_validate_manifest, manifest_sha256


def _download(url: str, destination: Path) -> dict[str, str | int | bool]:
    destination.parent.mkdir(parents=True, exist_ok=True)
    receipt = destination.with_suffix(destination.suffix + ".download.json")
    if destination.is_file() and receipt.is_file():
        prior = json.loads(receipt.read_text())
        if prior.get("url") == url and prior.get("bytes") == destination.stat().st_size:
            return {**prior, "reused": True}

    temporary = destination.with_suffix(destination.suffix + f".part-{os.getpid()}")
    digest = hashlib.sha256()
    byte_count = 0
    with requests.get(
        url,
        stream=True,
        timeout=(30, 600),
        headers={"Accept-Encoding": "identity"},
    ) as response:
        response.raise_for_status()
        with temporary.open("wb") as output:
            for chunk in response.iter_content(chunk_size=8 << 20):
                if chunk:
                    output.write(chunk)
                    digest.update(chunk)
                    byte_count += len(chunk)
    temporary.replace(destination)
    result: dict[str, str | int | bool] = {
        "url": url,
        "path": str(destination),
        "bytes": byte_count,
        "sha256": digest.hexdigest(),
        "reused": False,
    }
    receipt.write_text(json.dumps(result, indent=2, sort_keys=True) + "\n")
    return result


def stage_shards(
    *,
    data_manifest_path: str | Path,
    resolved_shards_path: str | Path,
    output_root: str | Path,
    output_manifest: str | Path,
    world_size: int,
    stage: str,
    workers: int,
) -> dict:
    manifest = load_and_validate_manifest(data_manifest_path)
    resolved = json.loads(Path(resolved_shards_path).read_text())
    if resolved.get("schema") != "v2-sbd-resolved-shards-v1":
        raise ValueError("unsupported resolved-shard manifest schema")
    if resolved.get("data_manifest_sha256") != manifest_sha256(data_manifest_path):
        raise ValueError("resolved shards do not match the locked data manifest")

    sources = {raw["source_id"]: DataSource(**raw) for raw in manifest["sources"]}
    root = Path(output_root)
    jobs: list[tuple[str, Path]] = []
    selected_by_source: dict[str, list[Path]] = {}
    for raw in resolved["sources"]:
        source = sources[raw["source_id"]]
        active = (
            stage == "all"
            or (stage == "0" and source.stage0_weight > 0)
            or (stage == "1" and source.stage1_weight > 0)
        )
        if not active:
            continue
        ranked_files = sorted(
            raw["files"],
            key=lambda value: hashlib.sha256(
                f"{raw['source_id']}\0{value}".encode()
            ).digest(),
        )
        selected = sorted(ranked_files[: min(world_size, len(ranked_files))])
        local_paths = []
        for index, relative in enumerate(selected):
            url = (
                f"https://huggingface.co/datasets/{raw['dataset']}/resolve/"
                f"{raw['revision']}/{quote(relative)}"
            )
            suffix = "".join(Path(relative).suffixes)
            destination = root / raw["source_id"] / f"shard-{index:02d}{suffix}"
            jobs.append((url, destination))
            local_paths.append(destination)
        selected_by_source[raw["source_id"]] = local_paths

    receipts = []
    with concurrent.futures.ThreadPoolExecutor(max_workers=workers) as pool:
        future_to_path = {
            pool.submit(_download, url, destination): destination
            for url, destination in jobs
        }
        for future in concurrent.futures.as_completed(future_to_path):
            result = future.result()
            receipts.append(result)
            print(json.dumps(result, sort_keys=True), flush=True)

    staged = dict(resolved)
    staged["schema"] = "v2-sbd-resolved-shards-v1"
    staged["staging"] = {
        "schema": "v2-sbd-local-staging-v1",
        "stage": stage,
        "world_size": world_size,
        "root": str(root),
        "downloaded_bytes": sum(int(item["bytes"]) for item in receipts if not item["reused"]),
        "reused_bytes": sum(int(item["bytes"]) for item in receipts if item["reused"]),
    }
    for raw in staged["sources"]:
        local = selected_by_source.get(raw["source_id"])
        if local is not None:
            raw["local_files"] = [str(path) for path in local]
    destination = Path(output_manifest)
    destination.parent.mkdir(parents=True, exist_ok=True)
    destination.write_text(json.dumps(staged, indent=2, sort_keys=True) + "\n")
    return staged


def main() -> None:
    parser = argparse.ArgumentParser(description="Stage immutable corpus shards on persistent disk")
    parser.add_argument("--manifest", required=True)
    parser.add_argument("--resolved-shards", required=True)
    parser.add_argument("--output-root", required=True)
    parser.add_argument("--output-manifest", required=True)
    parser.add_argument("--world-size", type=int, default=8)
    parser.add_argument("--stage", choices=("0", "1", "all"), default="0")
    parser.add_argument("--workers", type=int, default=8)
    args = parser.parse_args()
    stage_shards(
        data_manifest_path=args.manifest,
        resolved_shards_path=args.resolved_shards,
        output_root=args.output_root,
        output_manifest=args.output_manifest,
        world_size=args.world_size,
        stage=args.stage,
        workers=args.workers,
    )


if __name__ == "__main__":
    main()
