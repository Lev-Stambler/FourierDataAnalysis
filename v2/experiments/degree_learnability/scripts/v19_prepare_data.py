"""Prepare the small hash-pinned enwik8 byte payload for the v1.9 remote pilot."""

from __future__ import annotations

import hashlib
import json
from pathlib import Path

import numpy as np

ROOT = Path(__file__).parent.parent
SOURCE = ROOT / "dlx/data_cache/r4_enwik8_n20000000.npy"
OUTPUT = ROOT / "dlx/data_cache/v19_enwik8_bytes_n5500000.npy"
META = ROOT / "dlx/data_cache/v19_enwik8_bytes_n5500000.json"
N_BYTES = 5_500_000


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def main() -> None:
    if not OUTPUT.exists():
        source = np.load(SOURCE, mmap_mode="r")
        payload = np.asarray(source[:N_BYTES], dtype=np.uint8)
        if len(payload) != N_BYTES:
            raise ValueError("source corpus is shorter than the frozen payload")
        np.save(OUTPUT, payload)
    payload = np.load(OUTPUT, mmap_mode="r")
    counts = np.bincount(np.asarray(payload, dtype=np.int64), minlength=256)
    metadata = {
        "source": str(SOURCE.relative_to(ROOT)),
        "source_sha256": sha256_file(SOURCE),
        "output": str(OUTPUT.relative_to(ROOT)),
        "output_sha256": sha256_file(OUTPUT),
        "n_bytes": len(payload),
        "dtype": str(payload.dtype),
        "byte_counts_sha256": hashlib.sha256(counts.tobytes()).hexdigest(),
        "divisible_by_all_strides": all(
            len(payload) % stride == 0 for stride in (1, 2, 4, 8)
        ),
    }
    META.write_text(json.dumps(metadata, indent=2))
    print(json.dumps(metadata, indent=2))


if __name__ == "__main__":
    main()
