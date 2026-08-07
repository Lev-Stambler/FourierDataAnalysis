"""Materialize fresh hard-domain corpora for the prospective v2.2 stress test."""

from __future__ import annotations

import hashlib
import json
import tarfile
import urllib.error
import urllib.request
from pathlib import Path

import numpy as np

ROOT = Path(__file__).parent.parent
CACHE = ROOT / "dlx/data_cache"
SOURCE_DIR = CACHE / "v22_sources"
OUT = ROOT / "runs/local/v22_hard_h100"
N_BYTES = 8_000_000

ARCHIVES = {
    "mathlib_lean": {
        "url": "https://github.com/leanprover-community/mathlib4/archive/refs/tags/v4.32.1.tar.gz",
        "archive": "mathlib4-v4.32.1.tar.gz",
        "suffix": ".lean",
        "required_path": "/Mathlib/",
    },
    "rust_source": {
        "url": "https://github.com/rust-lang/rust/archive/refs/tags/1.89.0.tar.gz",
        "archive": "rust-1.89.0.tar.gz",
        "suffix": ".rs",
        "required_path": "/",
    },
}


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _download(url: str, path: Path) -> None:
    if path.exists():
        return
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".partial")
    request = urllib.request.Request(url, headers={"User-Agent": "dlx-v2.2/1.0"})
    with (
        urllib.request.urlopen(request, timeout=120) as response,
        temporary.open("wb") as handle,
    ):
        while chunk := response.read(1024 * 1024):
            handle.write(chunk)
    temporary.replace(path)


def _append(payload: bytearray, value: bytes) -> bool:
    remaining = N_BYTES - len(payload)
    if remaining <= 0:
        return True
    payload.extend(value[:remaining])
    return len(payload) == N_BYTES


def _archive_payload(spec: dict) -> tuple[bytes, int, dict]:
    archive_path = SOURCE_DIR / spec["archive"]
    _download(spec["url"], archive_path)
    payload = bytearray()
    documents = 0
    with tarfile.open(archive_path, mode="r:gz") as archive:
        members = sorted(
            (
                member
                for member in archive.getmembers()
                if member.isfile()
                and member.name.endswith(spec["suffix"])
                and spec["required_path"] in member.name
            ),
            key=lambda member: member.name,
        )
        for member in members:
            extracted = archive.extractfile(member)
            if extracted is None:
                continue
            documents += 1
            header = f"\n\n-- FILE: {member.name}\n".encode()
            if _append(payload, header) or _append(payload, extracted.read()):
                break
    provenance = {
        "source_url": spec["url"],
        "source_archive": str(archive_path.relative_to(ROOT)),
        "source_archive_sha256": sha256_file(archive_path),
        "documents_consumed": documents,
    }
    return bytes(payload), documents, provenance


def _rfc_payload() -> tuple[bytes, int, dict]:
    rfc_dir = SOURCE_DIR / "rfc_text"
    rfc_dir.mkdir(parents=True, exist_ok=True)
    payload = bytearray()
    records = []
    for number in range(9000, 10000):
        name = f"rfc{number}.txt"
        path = rfc_dir / name
        url = f"https://www.rfc-editor.org/rfc/{name}"
        if not path.exists():
            try:
                _download(url, path)
            except urllib.error.HTTPError as error:
                if error.code == 404:
                    continue
                raise
        value = path.read_bytes()
        records.append(
            {"rfc": number, "sha256": hashlib.sha256(value).hexdigest()}
        )
        if _append(payload, f"\n\nRFC {number}\n".encode()) or _append(payload, value):
            break
    source_set_hash = hashlib.sha256(
        json.dumps(records, sort_keys=True).encode()
    ).hexdigest()
    provenance = {
        "source_url_pattern": "https://www.rfc-editor.org/rfc/rfc{number}.txt",
        "source_range_start": 9000,
        "source_range_end_inclusive": records[-1]["rfc"],
        "source_documents": records,
        "source_set_sha256": source_set_hash,
        "documents_consumed": len(records),
    }
    return bytes(payload), len(records), provenance


def _prepare(dataset: str) -> dict:
    output = CACHE / f"v22_{dataset}_bytes_n{N_BYTES}.npy"
    metadata_path = OUT / "data" / f"{dataset}.json"
    if output.exists():
        if not metadata_path.exists():
            raise FileExistsError(
                f"refusing to reuse an unverified prospective payload: {output}"
            )
        metadata = json.loads(metadata_path.read_text())
        if metadata["output_file_sha256"] != sha256_file(output):
            raise ValueError(f"cached prospective payload hash mismatch: {output}")
        return metadata
    if dataset == "rfc_technical":
        raw, documents, provenance = _rfc_payload()
        canonicalization = "ordered immutable RFC text documents"
    else:
        raw, documents, provenance = _archive_payload(ARCHIVES[dataset])
        canonicalization = f"lexicographically ordered {ARCHIVES[dataset]['suffix']} files"
    if len(raw) != N_BYTES:
        raise RuntimeError(f"{dataset} produced {len(raw)} bytes; need {N_BYTES}")
    tokens = np.frombuffer(raw, dtype=np.uint8).copy()
    np.save(output, tokens)
    counts = np.bincount(tokens.astype(np.int64), minlength=256)
    probabilities = counts[counts > 0].astype(float) / counts.sum()
    metadata = {
        "dataset": dataset,
        **provenance,
        "canonicalization": canonicalization,
        "documents_consumed": documents,
        "output": str(output.relative_to(ROOT)),
        "output_file_sha256": sha256_file(output),
        "byte_stream_sha256": hashlib.sha256(tokens.tobytes()).hexdigest(),
        "byte_counts_sha256": hashlib.sha256(counts.tobytes()).hexdigest(),
        "n_bytes": len(tokens),
        "dtype": str(tokens.dtype),
        "unigram_entropy_bits": float(
            -(probabilities * np.log2(probabilities)).sum()
        ),
        "divisible_by_16": len(tokens) % 16 == 0,
    }
    metadata_path.parent.mkdir(parents=True, exist_ok=True)
    metadata_path.write_text(json.dumps(metadata, indent=2))
    return metadata


def main() -> None:
    records = []
    for dataset in (*ARCHIVES, "rfc_technical"):
        metadata = _prepare(dataset)
        records.append(metadata)
        print(
            f"{dataset}: bytes={metadata['n_bytes']} "
            f"H1={metadata['unigram_entropy_bits']:.4f} "
            f"sha={metadata['output_file_sha256'][:12]}",
            flush=True,
        )
    OUT.mkdir(parents=True, exist_ok=True)
    (OUT / "data_manifest.json").write_text(json.dumps(records, indent=2))


if __name__ == "__main__":
    main()
