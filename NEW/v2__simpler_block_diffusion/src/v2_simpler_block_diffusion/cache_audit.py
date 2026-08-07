from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path

from safetensors.torch import load_file

from .data import manifest_sha256


def audit_cache(
    root: str | Path,
    *,
    data_manifest_path: str | Path,
    stage: int,
    world_size: int,
) -> dict:
    root = Path(root)
    all_hashes: set[bytes] = set()
    rank_reports = []
    cross_rank_duplicates = 0
    total_contexts = 0
    source_totals: dict[str, int] = {}
    for rank in range(world_size):
        metadata = json.loads((root / f"rank-{rank:02d}.json").read_text())
        if metadata.get("data_manifest_sha256") != manifest_sha256(data_manifest_path):
            raise ValueError(f"rank {rank} cache does not match data manifest")
        if metadata.get("stage") != stage or metadata.get("world_size") != world_size:
            raise ValueError(f"rank {rank} cache stage/world size mismatch")
        rank_hashes: set[bytes] = set()
        contexts = 0
        for filename in metadata["files"]:
            tensors = load_file(root / filename)
            ids = tensors["input_ids"].contiguous().numpy()
            eligible = tensors["eligible_mask"].contiguous().numpy()
            for index in range(ids.shape[0]):
                digest = hashlib.sha256(ids[index].tobytes() + eligible[index].tobytes()).digest()
                rank_hashes.add(digest)
            contexts += ids.shape[0]
        if contexts != metadata["contexts"]:
            raise ValueError(f"rank {rank} metadata says {metadata['contexts']} contexts, found {contexts}")
        for source, count in metadata["source_contexts"].items():
            source_totals[source] = source_totals.get(source, 0) + int(count)
        rank_reports.append(
            {
                "rank": rank,
                "contexts": contexts,
                "unique_contexts": len(rank_hashes),
                "within_rank_duplicates": contexts - len(rank_hashes),
            }
        )
        cross_rank_duplicates += len(rank_hashes & all_hashes)
        all_hashes.update(rank_hashes)
        total_contexts += contexts
    result = {
        "schema": "v2-sbd-token-cache-audit-v1",
        "stage": stage,
        "world_size": world_size,
        "total_contexts": total_contexts,
        "unique_contexts": len(all_hashes),
        "cross_rank_duplicates": cross_rank_duplicates,
        "source_contexts": source_totals,
        "ranks": rank_reports,
    }
    (root / "audit.json").write_text(json.dumps(result, indent=2, sort_keys=True) + "\n")
    return result


def main() -> None:
    parser = argparse.ArgumentParser(description="Audit a persistent token cache")
    parser.add_argument("--root", required=True)
    parser.add_argument("--manifest", required=True)
    parser.add_argument("--stage", type=int, choices=(0, 1), required=True)
    parser.add_argument("--world-size", type=int, default=8)
    args = parser.parse_args()
    print(json.dumps(audit_cache(
        args.root,
        data_manifest_path=args.manifest,
        stage=args.stage,
        world_size=args.world_size,
    ), sort_keys=True))


if __name__ == "__main__":
    main()
