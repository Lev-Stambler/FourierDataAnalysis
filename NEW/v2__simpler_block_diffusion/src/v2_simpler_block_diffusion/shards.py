from __future__ import annotations

import hashlib
import json
from pathlib import Path
from typing import Any
from urllib.parse import quote

from .data import DataSource, load_and_validate_manifest, manifest_sha256


SHARD_SUFFIXES = (".parquet", ".jsonl", ".jsonl.gz", ".jsonl.zst")


def _eligible_path(source: DataSource, path: str) -> bool:
    if not path.endswith(SHARD_SUFFIXES):
        return False
    if source.subset and source.subset != "default":
        return path.startswith(f"{source.subset}/")
    return path.startswith("data/")


def _select_paths(source: DataSource, paths: list[str], max_files: int) -> list[str]:
    eligible = [path for path in paths if _eligible_path(source, path)]
    if not eligible:
        raise RuntimeError(f"no supported data files found for {source.source_id}")
    if source.kind == "conversation" or len(eligible) <= max_files:
        return sorted(eligible)
    # A stable hash sample avoids taking only one chronological/domain prefix.
    ranked = sorted(
        eligible,
        key=lambda path: hashlib.sha256(f"{source.source_id}\0{path}".encode()).digest(),
    )
    return sorted(ranked[:max_files])


def resolve_shards(
    data_manifest_path: str | Path,
    output_path: str | Path,
    *,
    max_files_per_document_source: int = 1024,
) -> dict[str, Any]:
    from huggingface_hub import HfApi

    manifest = load_and_validate_manifest(data_manifest_path)
    api = HfApi()
    resolved_sources = []
    for raw in manifest["sources"]:
        source = DataSource(**raw)
        repo_files = api.list_repo_files(
            source.dataset, repo_type="dataset", revision=source.revision
        )
        selected = _select_paths(source, repo_files, max_files_per_document_source)
        resolved_sources.append(
            {
                "source_id": source.source_id,
                "dataset": source.dataset,
                "revision": source.revision,
                "format": "parquet" if selected[0].endswith(".parquet") else "json",
                "files": selected,
            }
        )
    result = {
        "schema": "v2-sbd-resolved-shards-v1",
        "data_manifest_sha256": manifest_sha256(data_manifest_path),
        "selection": "all conversation shards; stable SHA-256 sample for document sources",
        "max_files_per_document_source": max_files_per_document_source,
        "sources": resolved_sources,
    }
    destination = Path(output_path)
    destination.parent.mkdir(parents=True, exist_ok=True)
    destination.write_text(json.dumps(result, indent=2, sort_keys=True) + "\n")
    return result


def load_resolved_shards(
    path: str | Path, data_manifest_path: str | Path
) -> dict[str, dict[str, Any]]:
    value = json.loads(Path(path).read_text())
    if value.get("schema") != "v2-sbd-resolved-shards-v1":
        raise ValueError("unsupported resolved-shard manifest schema")
    expected = manifest_sha256(data_manifest_path)
    if value.get("data_manifest_sha256") != expected:
        raise ValueError("resolved shards do not match the locked data manifest")
    result: dict[str, dict[str, Any]] = {}
    for source in value.get("sources", []):
        files = source.get("files", [])
        if not files:
            raise ValueError(f"resolved source {source.get('source_id')} contains no files")
        revision = source["revision"]
        dataset = source["dataset"]
        local_files = source.get("local_files")
        if local_files is not None:
            missing = [path for path in local_files if not Path(path).is_file()]
            if missing:
                raise ValueError(
                    f"staged source {source['source_id']} has missing files: {missing[:3]}"
                )
        result[source["source_id"]] = {
            "format": source["format"],
            "urls": list(local_files) if local_files is not None else [
                f"https://huggingface.co/datasets/{dataset}/resolve/{revision}/{quote(file)}"
                for file in files
            ],
        }
    return result


def main() -> None:
    import argparse

    parser = argparse.ArgumentParser(description="Resolve corpus files once before a paid run")
    parser.add_argument("--manifest", required=True)
    parser.add_argument("--output", required=True)
    parser.add_argument("--max-document-files", type=int, default=1024)
    args = parser.parse_args()
    result = resolve_shards(
        args.manifest,
        args.output,
        max_files_per_document_source=args.max_document_files,
    )
    print(json.dumps({source["source_id"]: len(source["files"]) for source in result["sources"]}, sort_keys=True))


if __name__ == "__main__":
    main()
