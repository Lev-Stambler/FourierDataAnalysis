"""Freeze the corpus registry reused by the v2.8 random-window repair."""

from __future__ import annotations

import hashlib
import json
import sys
from pathlib import Path

import numpy as np

ROOT = Path(__file__).parent.parent
sys.path.insert(0, str(ROOT))

from dlx.analysis.text_panel import v24_profile_data_files
from dlx.protocol.frozen import file_sha256, write_hash_once, write_json_once
from scripts.v26_freeze_predictions import _prior_rows

OUT = ROOT / "runs/local/v28_random_windows"


def _byte_hash(path: Path) -> str:
    values = np.load(path, mmap_mode="r")
    digest = hashlib.sha256()
    for start in range(0, len(values), 1_000_000):
        digest.update(np.asarray(values[start : start + 1_000_000]).tobytes())
    return digest.hexdigest()


def prepare() -> dict:
    historical_paths = {
        **v24_profile_data_files(ROOT, "development"),
        **v24_profile_data_files(ROOT, "confirmation"),
    }
    prior = {row["dataset"] for row in _prior_rows()}
    v26 = json.loads(
        (ROOT / "runs/local/v26_sampled_locality/data_manifest.json").read_text()
    )
    v27 = json.loads(
        (ROOT / "runs/local/v27_marginal_locality/data_manifest.json").read_text()
    )
    confirmation_strata = {row["id"]: row["stratum"] for row in v27["corpora"]}
    records = []
    for dataset in sorted(prior):
        path = historical_paths[dataset]
        records.append(
            {
                "dataset": dataset,
                "panel": "development",
                "path": str(path.relative_to(ROOT)),
                "n_bytes": len(np.load(path, mmap_mode="r")),
                "file_sha256": file_sha256(path),
                "byte_stream_sha256": _byte_hash(path),
            }
        )
    for row in v26["corpora"]:
        path = ROOT / row["output"]
        records.append(
            {
                "dataset": row["id"],
                "panel": "development",
                "path": row["output"],
                "n_bytes": row["n_bytes"],
                "file_sha256": row["output_file_sha256"],
                "byte_stream_sha256": row["byte_stream_sha256"],
            }
        )
    for row in v27["corpora"]:
        path = ROOT / row["output"]
        records.append(
            {
                "dataset": row["id"],
                "panel": "confirmation",
                "stratum": confirmation_strata[row["id"]],
                "path": row["output"],
                "n_bytes": row["n_bytes"],
                "file_sha256": row["output_file_sha256"],
                "byte_stream_sha256": row["byte_stream_sha256"],
            }
        )
    if len(records) != 78 or len({row["dataset"] for row in records}) != 78:
        raise ValueError("v2.8 requires 54 development and 24 confirmation corpora")
    if sum(row["panel"] == "development" for row in records) != 54:
        raise ValueError("development panel size changed")
    result = {
        "status": "frozen source reuse registry before v2.8 profiling or training",
        "sampling_repair": "all corpora reused byte-for-byte; only position sampling changes",
        "corpora": records,
    }
    digest = write_json_once(OUT / "data_manifest.json", result)
    write_hash_once(OUT / "data_manifest.sha256", digest)
    print(digest)
    return result


if __name__ == "__main__":
    prepare()
