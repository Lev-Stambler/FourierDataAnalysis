from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path

import numpy as np

from qwen_normuon_pretrain.config import (
    FINEWEB_EDU_CONFIG,
    FINEWEB_EDU_ID,
    FINEWEB_EDU_REVISION,
    MODEL_ID,
    MODEL_REVISION,
)
from qwen_normuon_pretrain.data import deterministic_spans, load_manifest


SCHEMA = "expv6-confirmation-v1"
EXAMPLES = 8_192
NAMESPACE = b"expv6-symmetric-v2-confirmation"


def confirmation_bucket(digest: bytes) -> int:
    return int.from_bytes(hashlib.sha256(NAMESPACE + digest).digest()[:8], "big") % 100


def existing_document_hashes(data_root: Path) -> set[bytes]:
    hashes: set[bytes] = set()
    for split in ("train", "validation", "test"):
        values = np.load(data_root / f"{split}_hashes.npy", mmap_mode="r")
        hashes.update(bytes(value) for value in np.unique(values))
    return hashes


def array_sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(2**20), b""):
            digest.update(chunk)
    return digest.hexdigest()


def prepare(data_root: Path, output_root: Path, examples: int = EXAMPLES) -> dict:
    manifest_path = output_root / "manifest.json"
    if manifest_path.is_file():
        value = json.loads(manifest_path.read_text())
        if value.get("schema") == SCHEMA and value.get("examples") == examples:
            return value
        raise RuntimeError(f"refusing to replace invalid confirmation data at {output_root}")

    source_manifest = load_manifest(str(data_root))
    excluded = existing_document_hashes(data_root)
    temporary = output_root.with_name(output_root.name + ".tmp")
    if temporary.exists():
        raise RuntimeError(f"stale temporary confirmation directory: {temporary}")
    temporary.mkdir(parents=True)

    contexts = np.lib.format.open_memmap(
        temporary / "confirmation_contexts.npy",
        mode="w+",
        dtype=np.int32,
        shape=(examples, 16),
    )
    targets = np.lib.format.open_memmap(
        temporary / "confirmation_targets.npy",
        mode="w+",
        dtype=np.int32,
        shape=(examples,),
    )
    hashes = np.lib.format.open_memmap(
        temporary / "confirmation_hashes.npy",
        mode="w+",
        dtype="V32",
        shape=(examples,),
    )

    from datasets import load_dataset
    from transformers import AutoTokenizer

    tokenizer = AutoTokenizer.from_pretrained(MODEL_ID, revision=MODEL_REVISION)
    stream = load_dataset(
        FINEWEB_EDU_ID,
        name=FINEWEB_EDU_CONFIG,
        split="train",
        streaming=True,
        revision=FINEWEB_EDU_REVISION,
    )
    seen: set[bytes] = set()
    selected_documents = 0
    count = 0
    texts: list[str] = []
    digests: list[bytes] = []

    def flush() -> None:
        nonlocal count, selected_documents
        if not texts:
            return
        tokenized = tokenizer(texts, add_special_tokens=False)["input_ids"]
        for token_ids, digest in zip(tokenized, digests, strict=True):
            spans = deterministic_spans(token_ids, digest)
            if not spans:
                continue
            selected_documents += 1
            for span in spans:
                if count >= examples:
                    break
                contexts[count] = span[:-1]
                targets[count] = span[-1]
                hashes[count] = np.void(digest)
                count += 1
        texts.clear()
        digests.clear()

    for record in stream:
        text = record.get("text") or ""
        if not text:
            continue
        digest = hashlib.sha256(text.encode("utf-8", "ignore")).digest()
        if digest in seen:
            continue
        seen.add(digest)
        if digest in excluded or confirmation_bucket(digest) != 0:
            continue
        texts.append(text)
        digests.append(digest)
        if len(texts) >= 256:
            flush()
            if count >= examples:
                break
    flush()
    if count != examples:
        raise RuntimeError(f"FineWeb-Edu ended after {count}/{examples} confirmation examples")

    for array in (contexts, targets, hashes):
        array.flush()
    del contexts, targets, hashes
    files = {
        name: array_sha256(temporary / name)
        for name in (
            "confirmation_contexts.npy",
            "confirmation_targets.npy",
            "confirmation_hashes.npy",
        )
    }
    value = {
        "schema": SCHEMA,
        "examples": examples,
        "selected_documents": selected_documents,
        "selection": "namespace_sha256_mod_100_eq_0_excluding_existing_documents",
        "namespace": NAMESPACE.decode(),
        "model_id": MODEL_ID,
        "model_revision": MODEL_REVISION,
        "fineweb_id": FINEWEB_EDU_ID,
        "fineweb_config": FINEWEB_EDU_CONFIG,
        "fineweb_revision": FINEWEB_EDU_REVISION,
        "source_manifest_sha256": hashlib.sha256(
            json.dumps(source_manifest, sort_keys=True).encode()
        ).hexdigest(),
        "files": files,
    }
    (temporary / "manifest.json").write_text(json.dumps(value, indent=2, sort_keys=True))
    if output_root.exists():
        raise RuntimeError(f"refusing to replace existing path {output_root}")
    os.replace(temporary, output_root)
    return value


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--data-root", required=True, type=Path)
    parser.add_argument("--output-root", required=True, type=Path)
    parser.add_argument("--examples", default=EXAMPLES, type=int)
    args = parser.parse_args()
    print(json.dumps(prepare(args.data_root, args.output_root, args.examples), indent=2))


if __name__ == "__main__":
    main()
