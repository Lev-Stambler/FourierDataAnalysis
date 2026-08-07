from __future__ import annotations

import hashlib
import json
import random
from pathlib import Path

import torch
from safetensors.torch import load_file, save_file

from .data import DataSource, load_and_validate_manifest, manifest_sha256
from .shards import load_resolved_shards
from .streaming_data import PackedTrainingExample, WeightedMixture


class TokenCacheMixture:
    def __init__(
        self,
        root: str | Path,
        *,
        rank: int,
        data_manifest_path: str | Path,
        stage: int,
    ) -> None:
        self.root = Path(root)
        metadata_path = self.root / f"rank-{rank:02d}.json"
        metadata = json.loads(metadata_path.read_text())
        if metadata.get("schema") != "v2-sbd-token-cache-v1":
            raise ValueError("unsupported token-cache schema")
        if metadata.get("data_manifest_sha256") != manifest_sha256(data_manifest_path):
            raise ValueError("token cache does not match the locked data manifest")
        if metadata.get("stage") != stage:
            raise ValueError("token cache has the wrong curriculum stage")
        self.files = [self.root / name for name in metadata["files"]]
        if not self.files:
            raise ValueError("token cache has no files")
        self.file_index = 0
        self.row_index = 0
        self.current_file_index: int | None = None
        self.current: dict[str, torch.Tensor] | None = None

    def __iter__(self) -> "TokenCacheMixture":
        return self

    def __next__(self) -> PackedTrainingExample:
        if self.current is None or self.row_index >= self.current["input_ids"].shape[0]:
            self.current_file_index = self.file_index
            self.current = load_file(self.files[self.current_file_index])
            self.file_index = (self.file_index + 1) % len(self.files)
            self.row_index = 0
        index = self.row_index
        self.row_index += 1
        return PackedTrainingExample(
            self.current["input_ids"][index].long(),
            self.current["eligible_mask"][index].bool(),
            "token-cache",
        )

    def state_dict(self) -> dict[str, int | None]:
        return {
            "next_file_index": self.file_index,
            "current_file_index": self.current_file_index,
            "row_index": self.row_index,
        }

    def load_state_dict(self, state: dict[str, int | None]) -> None:
        next_file = int(state["next_file_index"])
        current_file = state["current_file_index"]
        row_index = int(state["row_index"])
        if not 0 <= next_file < len(self.files):
            raise ValueError("invalid next token-cache file index")
        if current_file is None:
            if row_index != 0:
                raise ValueError("cache row index requires a current file")
            self.current = None
            self.current_file_index = None
        else:
            current_file = int(current_file)
            if not 0 <= current_file < len(self.files):
                raise ValueError("invalid current token-cache file index")
            self.current = load_file(self.files[current_file])
            if not 0 <= row_index <= self.current["input_ids"].shape[0]:
                raise ValueError("invalid token-cache row index")
            self.current_file_index = current_file
        self.file_index = next_file
        self.row_index = row_index


def _context_quotas(sources: list[DataSource], *, stage: int, contexts: int) -> dict[str, int]:
    weighted = []
    for source in sources:
        weight = source.stage0_weight if stage == 0 else source.stage1_weight
        if weight > 0:
            weighted.append((source, float(weight)))
    total = sum(weight for _, weight in weighted)
    raw = [(source, contexts * weight / total) for source, weight in weighted]
    quotas = {source.source_id: int(value) for source, value in raw}
    remaining = contexts - sum(quotas.values())
    order = sorted(raw, key=lambda item: (-(item[1] - int(item[1])), item[0].source_id))
    for source, _ in order[:remaining]:
        quotas[source.source_id] += 1
    return quotas


def build_rank_cache(
    *,
    data_manifest_path: str | Path,
    resolved_shards_path: str | Path,
    output: str | Path,
    rank: int,
    world_size: int,
    contexts: int,
    skip_contexts: int,
    chunk_contexts: int,
    stage: int,
    seed: int,
) -> dict:
    from transformers import AutoTokenizer

    from .teacher import TeacherSpec

    manifest = load_and_validate_manifest(data_manifest_path)
    sources = [DataSource(**raw) for raw in manifest["sources"]]
    resolved = load_resolved_shards(resolved_shards_path, data_manifest_path)
    spec = TeacherSpec()
    tokenizer = AutoTokenizer.from_pretrained(
        spec.model_id, revision=spec.revision, trust_remote_code=True
    )
    quotas = _context_quotas(sources, stage=stage, contexts=contexts)
    skip_quotas = _context_quotas(sources, stage=stage, contexts=skip_contexts)
    examples: list[PackedTrainingExample] = []
    seen_contexts: set[bytes] = set()
    for source in sources:
        quota = quotas.get(source.source_id, 0)
        if quota == 0:
            continue
        iterator = iter(
            WeightedMixture(
                [source],
                tokenizer,
                stage=stage,
                context_length=2048,
                rank=rank,
                world_size=world_size,
                seed=seed,
                resolved_shards=resolved,
            )
        )
        for _ in range(skip_quotas.get(source.source_id, 0)):
            next(iterator)
        accepted = 0
        while accepted < quota:
            example = next(iterator)
            digest = hashlib.sha256(
                example.input_ids.numpy().tobytes()
                + example.eligible_mask.numpy().tobytes()
            ).digest()
            if digest in seen_contexts:
                continue
            seen_contexts.add(digest)
            examples.append(example)
            accepted += 1
        print(
            json.dumps({"rank": rank, "source": source.source_id, "contexts": quota}),
            flush=True,
        )
    random.Random(seed + rank * 1_000_003).shuffle(examples)
    destination = Path(output)
    destination.mkdir(parents=True, exist_ok=True)
    files = []
    produced = 0
    while produced < contexts:
        count = min(chunk_contexts, contexts - produced)
        chunk = examples[produced : produced + count]
        filename = f"rank-{rank:02d}-chunk-{len(files):05d}.safetensors"
        save_file(
            {
                "input_ids": torch.stack([item.input_ids for item in chunk]).to(torch.int32),
                "eligible_mask": torch.stack([item.eligible_mask for item in chunk]),
            },
            destination / filename,
        )
        files.append(filename)
        produced += count
        print(json.dumps({"rank": rank, "contexts": produced}), flush=True)
    metadata = {
        "schema": "v2-sbd-token-cache-v1",
        "data_manifest_sha256": manifest_sha256(data_manifest_path),
        "teacher_model": spec.model_id,
        "teacher_revision": spec.revision,
        "stage": stage,
        "rank": rank,
        "world_size": world_size,
        "contexts": contexts,
        "clean_tokens": contexts * 2048,
        "source_contexts": quotas,
        "source_skipped_contexts": skip_quotas,
        "files": files,
    }
    (destination / f"rank-{rank:02d}.json").write_text(
        json.dumps(metadata, indent=2, sort_keys=True) + "\n"
    )
    return metadata


def main() -> None:
    import argparse

    parser = argparse.ArgumentParser(description="Build one rank of the persistent token cache")
    parser.add_argument("--manifest", required=True)
    parser.add_argument("--resolved-shards", required=True)
    parser.add_argument("--output", required=True)
    parser.add_argument("--rank", type=int, required=True)
    parser.add_argument("--world-size", type=int, default=8)
    parser.add_argument("--contexts", type=int, required=True)
    parser.add_argument("--skip-contexts", type=int, default=0)
    parser.add_argument("--chunk-contexts", type=int, default=1024)
    parser.add_argument("--stage", type=int, choices=(0, 1), default=0)
    parser.add_argument("--seed", type=int, default=194)
    args = parser.parse_args()
    build_rank_cache(
        data_manifest_path=args.manifest,
        resolved_shards_path=args.resolved_shards,
        output=args.output,
        rank=args.rank,
        world_size=args.world_size,
        contexts=args.contexts,
        skip_contexts=args.skip_contexts,
        chunk_contexts=args.chunk_contexts,
        stage=args.stage,
        seed=args.seed,
    )


if __name__ == "__main__":
    main()
