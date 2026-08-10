"""Materialize the hash-pinned v2.6 corpus panel without opening outcomes."""

from __future__ import annotations

import argparse
import hashlib
import json
import sys
import tarfile
import urllib.request
from pathlib import Path

import numpy as np

ROOT = Path(__file__).parent.parent
sys.path.insert(0, str(ROOT))

from dlx.protocol.frozen import (
    file_sha256,
    load_frozen_protocol,
    write_hash_once,
    write_json_once,
)

PROTOCOL_PATH = ROOT / "configs/protocol_v2.6.json"
OUT = ROOT / "runs/local/v26_sampled_locality"
DATA_DIR = OUT / "data"
CACHE = ROOT / "dlx/data_cache/v26_sources"

PRIOR_REPOSITORIES = {
    "mdn/content",
    "kubernetes/website",
    "rust-lang/rust",
    "golang/go",
    "microsoft/TypeScript",
    "llvm/llvm-project",
    "rocq-prover/rocq",
    "JuliaLang/julia",
    "ghc/ghc",
    "postgres/postgres",
    "leanprover-community/mathlib4",
    "python/cpython",
    "torvalds/linux",
}


def validate_registry(protocol: dict) -> None:
    sources = protocol["corpora"]["sources"]
    if len(sources) != 32 or len({row["id"] for row in sources}) != 32:
        raise ValueError("v2.6 requires exactly 32 uniquely named corpora")
    if len({row["repo"] for row in sources}) != 32:
        raise ValueError("v2.6 sources must use distinct repositories")
    counts = {}
    for row in sources:
        counts[row["stratum"]] = counts.get(row["stratum"], 0) + 1
        if row["repo"] in PRIOR_REPOSITORIES:
            raise ValueError(f"source repository was used previously: {row['repo']}")
        if len(row["commit"]) != 40:
            raise ValueError(f"source is not commit-pinned: {row['id']}")
    if set(counts.values()) != {8} or len(counts) != 4:
        raise ValueError(f"expected four strata of eight corpora, got {counts}")


def _download(source: dict) -> tuple[Path, str]:
    CACHE.mkdir(parents=True, exist_ok=True)
    archive = CACHE / f"{source['id']}-{source['commit']}.tar.gz"
    url = f"https://codeload.github.com/{source['repo']}/tar.gz/{source['commit']}"
    if not archive.exists():
        partial = Path(f"{archive}.partial")
        request = urllib.request.Request(url, headers={"User-Agent": "dlx-v2.6/1.0"})
        with (
            urllib.request.urlopen(request, timeout=300) as response,
            partial.open("wb") as out,
        ):
            for chunk in iter(lambda: response.read(1024 * 1024), b""):
                out.write(chunk)
        partial.replace(archive)
    return archive, url


def materialize(source: dict, n_bytes: int) -> dict:
    DATA_DIR.mkdir(parents=True, exist_ok=True)
    output = ROOT / f"dlx/data_cache/v26_{source['id']}_bytes_n{n_bytes}.npy"
    metadata_path = DATA_DIR / f"{source['id']}.json"
    if output.exists() or metadata_path.exists():
        if not output.exists() or not metadata_path.exists():
            raise FileExistsError(f"partial cached corpus state for {source['id']}")
        metadata = json.loads(metadata_path.read_text())
        if metadata["output_file_sha256"] != file_sha256(output):
            raise ValueError(f"cached output hash mismatch: {source['id']}")
        return metadata

    archive, url = _download(source)
    payload = bytearray()
    records = []
    suffixes = tuple(value.lower() for value in source["suffixes"])
    with tarfile.open(archive, "r:gz") as handle:
        members = sorted(
            (
                member
                for member in handle.getmembers()
                if member.isfile()
                and member.name.lower().endswith(suffixes)
                and any(path in f"/{member.name}" for path in source["paths"])
            ),
            key=lambda member: member.name,
        )
        for member in members:
            extracted = handle.extractfile(member)
            if extracted is None:
                continue
            value = extracted.read()
            records.append(
                {"path": member.name, "sha256": hashlib.sha256(value).hexdigest()}
            )
            payload.extend(f"\n\nFILE {member.name}\n".encode())
            payload.extend(value)
            if len(payload) >= n_bytes:
                del payload[n_bytes:]
                break
    if len(payload) != n_bytes:
        raise RuntimeError(
            f"{source['id']} yielded {len(payload)} selected bytes; require {n_bytes}"
        )
    tokens = np.frombuffer(payload, dtype=np.uint8).copy()
    np.save(output, tokens)
    counts = np.bincount(tokens.astype(np.int64), minlength=256)
    probabilities = counts[counts > 0].astype(float) / counts.sum()
    metadata = {
        **source,
        "source_kind": "hash-pinned GitHub repository archive",
        "source_url": url,
        "source_archive_sha256": file_sha256(archive),
        "source_set_sha256": hashlib.sha256(
            json.dumps(records, sort_keys=True).encode()
        ).hexdigest(),
        "documents_consumed": len(records),
        "canonicalization": "lexicographic archive member order, explicit FILE separators, exact prefix",
        "output": str(output.relative_to(ROOT)),
        "output_file_sha256": file_sha256(output),
        "byte_stream_sha256": hashlib.sha256(tokens.tobytes()).hexdigest(),
        "byte_counts_sha256": hashlib.sha256(counts.tobytes()).hexdigest(),
        "n_bytes": len(tokens),
        "unigram_entropy_bits": float(-np.dot(probabilities, np.log2(probabilities))),
    }
    write_json_once(metadata_path, metadata)
    return metadata


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--dataset")
    parser.add_argument("--validate-only", action="store_true")
    args = parser.parse_args()
    protocol = load_frozen_protocol(PROTOCOL_PATH)
    validate_registry(protocol)
    sources = protocol["corpora"]["sources"]
    if args.validate_only:
        print(f"valid frozen registry: {len(sources)} sources")
        return
    if args.dataset:
        sources = [row for row in sources if row["id"] == args.dataset]
        if not sources:
            parser.error(f"unknown dataset: {args.dataset}")
    n_bytes = int(protocol["corpora"]["bytes_per_corpus"])
    for source in sources:
        metadata = materialize(source, n_bytes)
        print(f"{source['id']}: {metadata['output_file_sha256'][:12]}", flush=True)
    complete = []
    for source in protocol["corpora"]["sources"]:
        path = DATA_DIR / f"{source['id']}.json"
        if path.exists():
            complete.append(json.loads(path.read_text()))
    if len(complete) == 32:
        manifest = {
            "protocol_hash": protocol["protocol_hash"],
            "corpora": complete,
        }
        digest = write_json_once(OUT / "data_manifest.json", manifest)
        write_hash_once(OUT / "data_manifest.sha256", digest)
        print(f"froze complete data manifest: {digest}")


if __name__ == "__main__":
    main()
