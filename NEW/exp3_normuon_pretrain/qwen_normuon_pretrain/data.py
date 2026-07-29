from __future__ import annotations

import hashlib
import json
import os
from pathlib import Path

import numpy as np

from .config import (
    CONTEXT_LENGTH,
    FINEWEB_EDU_CONFIG,
    FINEWEB_EDU_ID,
    FINEWEB_EDU_REVISION,
    MODEL_ID,
    MODEL_REVISION,
    TEST_EXAMPLES,
    TRAIN_EXAMPLES,
    VALIDATION_EXAMPLES,
)

SCHEMA = "qwen-fullwidth-context16-data-v2"
MAX_WINDOWS_PER_DOCUMENT = 32
SPLIT_SIZES = {
    "train": TRAIN_EXAMPLES,
    "validation": VALIDATION_EXAMPLES,
    "test": TEST_EXAMPLES,
}


def document_digest(text: str) -> bytes:
    return hashlib.sha256(text.encode("utf-8", "ignore")).digest()


def split_for_digest(digest: bytes) -> str:
    bucket = int.from_bytes(digest[:8], "big") % 100
    if bucket < 80:
        return "train"
    if bucket < 90:
        return "validation"
    return "test"


def deterministic_spans(
    token_ids,
    digest: bytes,
    *,
    length: int = CONTEXT_LENGTH + 1,
    maximum: int = MAX_WINDOWS_PER_DOCUMENT,
):
    ids = np.asarray(token_ids, dtype=np.int32)
    count = min(maximum, len(ids) // length)
    if count == 0:
        return ()
    slack = len(ids) - count * length
    start = int.from_bytes(digest[8:16], "big") % (slack + 1)
    return tuple(
        ids[start + index * length : start + (index + 1) * length]
        for index in range(count)
    )


def _manifest(
    root: Path,
    expected_sizes: dict | None = None,
) -> dict | None:
    path = root / "manifest.json"
    if not path.exists():
        return None
    value = json.loads(path.read_text())
    expected = {
        "schema": SCHEMA,
        "model_id": MODEL_ID,
        "model_revision": MODEL_REVISION,
        "fineweb_id": FINEWEB_EDU_ID,
        "fineweb_config": FINEWEB_EDU_CONFIG,
        "fineweb_revision": FINEWEB_EDU_REVISION,
        "context_length": CONTEXT_LENGTH,
    }
    if not all(value.get(key) == expected_value for key, expected_value in expected.items()):
        return None
    if expected_sizes is not None and value.get("split_sizes") != expected_sizes:
        return None
    return value


def load_manifest(root: str) -> dict:
    path = Path(root)
    manifest = _manifest(path)
    if manifest is None:
        raise RuntimeError(f"missing or invalid dataset manifest at {path}")
    return manifest


def prepare_dataset(
    root: str,
    tokenizer,
    *,
    batch_documents: int = 512,
    split_sizes: dict | None = None,
) -> dict:
    from datasets import load_dataset

    root_path = Path(root)
    sizes = dict(SPLIT_SIZES if split_sizes is None else split_sizes)
    if set(sizes) != {"train", "validation", "test"} or min(sizes.values()) <= 0:
        raise ValueError(
            "split_sizes must contain positive train/validation/test sizes"
        )
    existing = _manifest(root_path, sizes)
    if existing is not None:
        return existing
    temporary = root_path.with_name(root_path.name + ".tmp")
    temporary.mkdir(parents=True, exist_ok=True)
    arrays = {}
    for split, size in sizes.items():
        arrays[(split, "contexts")] = np.lib.format.open_memmap(
            temporary / f"{split}_contexts.npy",
            mode="w+",
            dtype=np.int32,
            shape=(size, CONTEXT_LENGTH),
        )
        arrays[(split, "targets")] = np.lib.format.open_memmap(
            temporary / f"{split}_targets.npy",
            mode="w+",
            dtype=np.int32,
            shape=(size,),
        )
        arrays[(split, "hashes")] = np.lib.format.open_memmap(
            temporary / f"{split}_hashes.npy",
            mode="w+",
            dtype="V32",
            shape=(size,),
        )
    counts = {split: 0 for split in sizes}
    seen: set[bytes] = set()
    stream = load_dataset(
        FINEWEB_EDU_ID,
        name=FINEWEB_EDU_CONFIG,
        split="train",
        streaming=True,
        revision=FINEWEB_EDU_REVISION,
    )
    texts: list[str] = []
    digests: list[bytes] = []

    def flush() -> None:
        if not texts:
            return
        tokenized = tokenizer(texts, add_special_tokens=False)["input_ids"]
        for ids, digest in zip(tokenized, digests, strict=True):
            split = split_for_digest(digest)
            if counts[split] >= sizes[split]:
                continue
            for span in deterministic_spans(ids, digest):
                index = counts[split]
                if index >= sizes[split]:
                    break
                arrays[(split, "contexts")][index] = span[:-1]
                arrays[(split, "targets")][index] = span[-1]
                arrays[(split, "hashes")][index] = np.void(digest)
                counts[split] += 1
        texts.clear()
        digests.clear()

    for record in stream:
        text = record.get("text") or ""
        if not text:
            continue
        digest = document_digest(text)
        if digest in seen:
            continue
        seen.add(digest)
        split = split_for_digest(digest)
        if counts[split] >= sizes[split]:
            continue
        texts.append(text)
        digests.append(digest)
        if len(texts) >= batch_documents:
            flush()
            if all(counts[name] >= sizes[name] for name in sizes):
                break
    flush()
    if counts != sizes:
        raise RuntimeError(f"FineWeb-Edu ended before dataset filled: {counts}")
    for array in arrays.values():
        array.flush()
    manifest = {
        "schema": SCHEMA,
        "model_id": MODEL_ID,
        "model_revision": MODEL_REVISION,
        "fineweb_id": FINEWEB_EDU_ID,
        "fineweb_config": FINEWEB_EDU_CONFIG,
        "fineweb_revision": FINEWEB_EDU_REVISION,
        "context_length": CONTEXT_LENGTH,
        "sampling": {
            "kind": "deterministic_nonoverlapping_windows",
            "maximum_per_document": MAX_WINDOWS_PER_DOCUMENT,
        },
        "split_sizes": sizes,
    }
    (temporary / "manifest.json").write_text(
        json.dumps(manifest, sort_keys=True)
    )
    if root_path.exists():
        raise RuntimeError(
            f"refusing to replace existing invalid dataset at {root_path}"
        )
    os.replace(temporary, root_path)
    return manifest


def load_split(root: str, split: str, *, mmap_mode: str = "r"):
    if split not in SPLIT_SIZES:
        raise ValueError(f"unknown split {split}")
    path = Path(root)
    load_manifest(root)
    return (
        np.load(path / f"{split}_contexts.npy", mmap_mode=mmap_mode),
        np.load(path / f"{split}_targets.npy", mmap_mode=mmap_mode),
        np.load(path / f"{split}_hashes.npy", mmap_mode=mmap_mode),
    )
