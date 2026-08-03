"""One-time sealing of the untouched official WikiText test windows."""

from __future__ import annotations

import hashlib
import json
import os
from pathlib import Path
from typing import Any

import numpy as np


SCHEMA = "exp13-wikitext-holdout-v1"
SOURCE_SCHEMA = "exp10-tokenized-corpus-v1"
CONFIRMATION_WINDOWS = 581
FINAL_WINDOWS = 582


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(8 << 20), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _write_json(path: Path, value: Any) -> None:
    temporary = path.with_name(path.name + ".tmp")
    temporary.write_text(json.dumps(value, indent=2, sort_keys=True))
    os.replace(temporary, path)


def read_holdout_manifest(root: Path) -> dict[str, Any]:
    manifest_path = root / "manifest.json"
    if not manifest_path.is_file():
        raise RuntimeError(f"missing holdout manifest: {manifest_path}")
    value = json.loads(manifest_path.read_text())
    if value.get("schema") != SCHEMA:
        raise RuntimeError("invalid Exp13 holdout schema")
    if value["partition"]["confirmation_index_range"] != [0, 580]:
        raise RuntimeError("confirmation range changed")
    if value["partition"]["final_index_range"] != [581, 1162]:
        raise RuntimeError("final range changed")
    if value["partition"].get("overlap_windows") != 0:
        raise RuntimeError("holdout partitions overlap")
    return value


def validate_holdout(
    root: Path, splits: tuple[str, ...] = ("confirmation", "final")
) -> dict[str, Any]:
    value = read_holdout_manifest(root)
    expected_shapes = {
        "confirmation": (CONFIRMATION_WINDOWS, 257),
        "final": (FINAL_WINDOWS, 257),
    }
    if any(split not in expected_shapes for split in splits):
        raise ValueError("invalid holdout split")
    for split in splits:
        name = f"{split}.npy"
        metadata = value["files"][name]
        path = root / name
        if not path.is_file() or sha256(path) != metadata["sha256"]:
            raise RuntimeError(f"holdout checksum mismatch: {path}")
        values = np.load(path, mmap_mode="r")
        if values.shape != expected_shapes[split]:
            raise RuntimeError(f"unexpected {split} shape")
    return value


def prepare_holdout(data_root: str | Path, output_root: str | Path) -> dict[str, Any]:
    source, output = Path(data_root), Path(output_root)
    if (output / "manifest.json").is_file():
        return validate_holdout(output)
    source_manifest = json.loads((source / "manifest.json").read_text())
    if source_manifest.get("schema") != SOURCE_SCHEMA:
        raise RuntimeError("invalid source corpus manifest")
    source_test = source / "test.npy"
    expected = source_manifest["files"]["test.npy"]
    if sha256(source_test) != expected["sha256"]:
        raise RuntimeError("source WikiText test checksum mismatch")
    windows = np.load(source_test, mmap_mode="r")
    if windows.shape != (CONFIRMATION_WINDOWS + FINAL_WINDOWS, 257):
        raise RuntimeError(f"unexpected source test shape: {windows.shape}")
    output.mkdir(parents=True, exist_ok=False)
    confirmation = output / "confirmation.npy"
    final = output / "final.npy"
    np.save(confirmation, np.asarray(windows[:CONFIRMATION_WINDOWS]))
    np.save(final, np.asarray(windows[CONFIRMATION_WINDOWS:]))
    value = {
        "schema": SCHEMA,
        "source": {
            "dataset": "Salesforce/wikitext",
            "dataset_revision": source_manifest["dataset_revision"],
            "test_path": str(source_test),
            "test_sha256": expected["sha256"],
            "test_windows": int(expected["windows"]),
        },
        "partition": {
            "method": "contiguous_before_any_exp13_evaluation",
            "confirmation_index_range": [0, 580],
            "final_index_range": [581, 1162],
            "overlap_windows": 0,
        },
        "files": {
            "confirmation.npy": {
                "sha256": sha256(confirmation),
                "windows": CONFIRMATION_WINDOWS,
                "prediction_tokens": CONFIRMATION_WINDOWS * 256,
            },
            "final.npy": {
                "sha256": sha256(final),
                "windows": FINAL_WINDOWS,
                "prediction_tokens": FINAL_WINDOWS * 256,
            },
        },
    }
    _write_json(output / "manifest.json", value)
    return validate_holdout(output)


def load_holdout(root: str | Path, split: str) -> np.ndarray:
    if split not in ("confirmation", "final"):
        raise ValueError("split must be confirmation or final")
    path = Path(root)
    validate_holdout(path, (split,))
    return np.load(path / f"{split}.npy", mmap_mode="r")
