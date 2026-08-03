"""Independent, document-disjoint TinyStories preparation for ExpV2-1."""

from __future__ import annotations

import hashlib
import json
import os
from pathlib import Path
from typing import Any, Iterator

import numpy as np

from .config import (
    CONTEXT_LENGTH,
    TINY_STORIES_ID,
    TINY_STORIES_REVISION,
    VOCAB_SIZE,
)
from .utils import atomic_json, sha256


TOKENIZER_SCHEMA = "expv2-1-tokenizer-v1"
CORPUS_SCHEMA = "expv2-1-tinystories-v1"
DEFAULT_DATA_ROOT = Path("/cache/expv2/exp1/data/tinystories")


def _records(split: str) -> Iterator[dict[str, Any]]:
    from datasets import load_dataset

    yield from load_dataset(
        TINY_STORIES_ID,
        split=split,
        streaming=True,
        revision=TINY_STORIES_REVISION,
    )


def _nonempty_text(split: str) -> Iterator[str]:
    for record in _records(split):
        text = str(record.get("text") or "").strip()
        if text:
            yield text


def train_tokenizer(
    output_root: str | Path,
    *,
    training_documents: int = 100_000,
) -> dict[str, Any]:
    from tokenizers import Tokenizer, decoders, models, normalizers, pre_tokenizers, trainers

    root = Path(output_root)
    root.mkdir(parents=True, exist_ok=True)
    tokenizer_path = root / "tokenizer.json"
    manifest_path = root / "tokenizer-manifest.json"
    if tokenizer_path.is_file() and manifest_path.is_file():
        existing = json.loads(manifest_path.read_text())
        if (
            existing.get("schema") == TOKENIZER_SCHEMA
            and existing.get("tokenizer_sha256") == sha256(tokenizer_path)
        ):
            return existing
        raise RuntimeError(f"invalid cached tokenizer at {root}")

    tokenizer = Tokenizer(models.BPE(unk_token="<unk>"))
    tokenizer.normalizer = normalizers.NFC()
    tokenizer.pre_tokenizer = pre_tokenizers.ByteLevel(add_prefix_space=False)
    tokenizer.decoder = decoders.ByteLevel()
    trainer = trainers.BpeTrainer(
        vocab_size=VOCAB_SIZE,
        min_frequency=2,
        special_tokens=["<pad>", "<unk>", "<bos>", "<eos>"],
        show_progress=True,
    )

    def documents() -> Iterator[str]:
        for index, text in enumerate(_nonempty_text("train")):
            if index >= training_documents:
                break
            yield text

    tokenizer.train_from_iterator(documents(), trainer=trainer)
    if tokenizer.get_vocab_size() != VOCAB_SIZE:
        raise RuntimeError(
            f"tokenizer has {tokenizer.get_vocab_size()} entries; expected {VOCAB_SIZE}"
        )
    temporary = tokenizer_path.with_name(tokenizer_path.name + ".tmp")
    tokenizer.save(str(temporary))
    os.replace(temporary, tokenizer_path)
    result = {
        "schema": TOKENIZER_SCHEMA,
        "dataset_id": TINY_STORIES_ID,
        "dataset_revision": TINY_STORIES_REVISION,
        "training_split": "train",
        "training_documents": training_documents,
        "vocab_size": VOCAB_SIZE,
        "special_tokens": ["<pad>", "<unk>", "<bos>", "<eos>"],
        "tokenizer_sha256": sha256(tokenizer_path),
    }
    atomic_json(manifest_path, result)
    return result


def _keep_validation_document(text: str, split: str) -> bool:
    if split not in ("validation", "test"):
        return True
    parity = 0 if split == "validation" else 1
    return hashlib.sha256(text.encode()).digest()[0] % 2 == parity


def _write_split(
    split: str,
    *,
    tokenizer: Any,
    output: Path,
    token_limit: int,
) -> dict[str, Any]:
    source_split = "train" if split == "train" else "validation"
    raw = output.with_suffix(".raw")
    document_raw = output.with_name(output.stem + "-documents.raw")
    windows = 0
    documents = 0
    document_hashes: list[str] = []
    eos = tokenizer.token_to_id("<eos>")
    if eos is None:
        raise RuntimeError("tokenizer lacks <eos>")
    with raw.open("wb") as handle, document_raw.open("wb") as document_handle:
        for text in _nonempty_text(source_split):
            if not _keep_validation_document(text, split):
                continue
            document_digest = hashlib.sha256(text.encode()).hexdigest()
            tokens = [*tokenizer.encode(text).ids, eos]
            written_for_document = False
            for start in range(0, len(tokens) - CONTEXT_LENGTH, CONTEXT_LENGTH + 1):
                window = tokens[start : start + CONTEXT_LENGTH + 1]
                if len(window) != CONTEXT_LENGTH + 1:
                    continue
                values = np.asarray(window, dtype=np.uint16)
                handle.write(values.tobytes())
                document_value = np.asarray(
                    [int(document_digest[:16], 16)], dtype=np.uint64
                )
                document_handle.write(document_value.tobytes())
                windows += 1
                written_for_document = True
                if token_limit and windows * CONTEXT_LENGTH >= token_limit:
                    break
            if written_for_document:
                documents += 1
                document_hashes.append(document_digest)
            if token_limit and windows * CONTEXT_LENGTH >= token_limit:
                break
    if windows == 0:
        raise RuntimeError(f"no document-contained windows produced for {split}")
    raw_array = np.memmap(
        raw, mode="r", dtype=np.uint16, shape=(windows, CONTEXT_LENGTH + 1)
    )
    temporary = output.with_name(output.name + ".tmp")
    destination = np.lib.format.open_memmap(
        temporary,
        mode="w+",
        dtype=np.uint16,
        shape=(windows, CONTEXT_LENGTH + 1),
    )
    for start in range(0, windows, 16_384):
        destination[start : start + 16_384] = raw_array[start : start + 16_384]
    destination.flush()
    del destination, raw_array
    os.replace(temporary, output)
    raw.unlink()
    document_output = output.with_name(output.stem + "-documents.npy")
    raw_documents = np.memmap(
        document_raw, mode="r", dtype=np.uint64, shape=(windows,)
    )
    document_temporary = document_output.with_name(document_output.name + ".tmp")
    saved_documents = np.lib.format.open_memmap(
        document_temporary, mode="w+", dtype=np.uint64, shape=(windows,)
    )
    saved_documents[:] = raw_documents[:]
    saved_documents.flush()
    del saved_documents, raw_documents
    os.replace(document_temporary, document_output)
    document_raw.unlink()
    document_set_hash = hashlib.sha256(
        "\n".join(sorted(document_hashes)).encode()
    ).hexdigest()
    return {
        "path": output.name,
        "sha256": sha256(output),
        "windows": windows,
        "prediction_tokens": windows * CONTEXT_LENGTH,
        "documents": documents,
        "document_set_sha256": document_set_hash,
        "source_split": source_split,
        "document_ids_path": document_output.name,
        "document_ids_sha256": sha256(document_output),
        "selection": (
            "all"
            if split == "train"
            else f"sha256-first-byte-parity-{0 if split == 'validation' else 1}"
        ),
    }


def prepare_tinystories(
    output_root: str | Path = DEFAULT_DATA_ROOT,
    *,
    tokenizer_documents: int = 100_000,
    train_tokens: int = 150_000_000,
    eval_tokens: int = 5_000_000,
) -> dict[str, Any]:
    from tokenizers import Tokenizer

    root = Path(output_root)
    root.mkdir(parents=True, exist_ok=True)
    tokenizer_manifest = train_tokenizer(
        root, training_documents=tokenizer_documents
    )
    manifest_path = root / "manifest.json"
    if manifest_path.is_file():
        existing = json.loads(manifest_path.read_text())
        validate_manifest(root, existing)
        return existing
    tokenizer = Tokenizer.from_file(str(root / "tokenizer.json"))
    files = {}
    for split in ("train", "validation", "test"):
        limit = train_tokens if split == "train" else eval_tokens
        files[split] = _write_split(
            split,
            tokenizer=tokenizer,
            output=root / f"{split}.npy",
            token_limit=limit,
        )
    if files["validation"]["document_set_sha256"] == files["test"]["document_set_sha256"]:
        raise RuntimeError("validation and test document sets are not disjoint")
    result = {
        "schema": CORPUS_SCHEMA,
        "dataset_id": TINY_STORIES_ID,
        "dataset_revision": TINY_STORIES_REVISION,
        "context_length": CONTEXT_LENGTH,
        "vocab_size": VOCAB_SIZE,
        "window_policy": "nonoverlapping-within-document-no-cross-document-packing",
        "validation_test_policy": "official-validation-documents-split-by-sha256-parity",
        "tokenizer_sha256": tokenizer_manifest["tokenizer_sha256"],
        "files": files,
    }
    atomic_json(manifest_path, result)
    return result


def validate_manifest(root: str | Path, manifest: dict[str, Any] | None = None) -> dict[str, Any]:
    base = Path(root)
    value = manifest or json.loads((base / "manifest.json").read_text())
    if value.get("schema") != CORPUS_SCHEMA:
        raise RuntimeError("invalid ExpV2-1 corpus schema")
    if value.get("dataset_revision") != TINY_STORIES_REVISION:
        raise RuntimeError("TinyStories revision mismatch")
    if value.get("context_length") != CONTEXT_LENGTH or value.get("vocab_size") != VOCAB_SIZE:
        raise RuntimeError("corpus dimensions mismatch")
    if value.get("tokenizer_sha256") != sha256(base / "tokenizer.json"):
        raise RuntimeError("tokenizer checksum mismatch")
    for split, metadata in value.get("files", {}).items():
        path = base / metadata["path"]
        if sha256(path) != metadata["sha256"]:
            raise RuntimeError(f"corrupt corpus split: {split}")
        array = np.load(path, mmap_mode="r")
        if array.shape != (metadata["windows"], CONTEXT_LENGTH + 1):
            raise RuntimeError(f"corpus split shape mismatch: {split}")
        if array.dtype != np.uint16:
            raise RuntimeError(f"corpus split dtype mismatch: {split}")
        document_ids_path = base / metadata["document_ids_path"]
        if sha256(document_ids_path) != metadata["document_ids_sha256"]:
            raise RuntimeError(f"corrupt corpus document IDs: {split}")
        document_ids = np.load(document_ids_path, mmap_mode="r")
        if document_ids.shape != (metadata["windows"],) or document_ids.dtype != np.uint64:
            raise RuntimeError(f"corpus document ID shape/dtype mismatch: {split}")
    if value["files"]["validation"]["document_set_sha256"] == value["files"]["test"]["document_set_sha256"]:
        raise RuntimeError("validation/test document-set hash collision")
    return value


def load_windows(
    root: str | Path,
    split: str,
    *,
    allow_test: bool = False,
) -> np.ndarray:
    if split not in ("train", "validation", "test"):
        raise ValueError("unknown TinyStories split")
    if split == "test" and not allow_test:
        raise PermissionError("sealed test split cannot be loaded before final recipes lock")
    base = Path(root)
    manifest = validate_manifest(base)
    return np.load(base / manifest["files"][split]["path"], mmap_mode="r")


def load_document_ids(
    root: str | Path,
    split: str,
    *,
    allow_test: bool = False,
) -> np.ndarray:
    if split not in ("train", "validation", "test"):
        raise ValueError("unknown TinyStories split")
    if split == "test" and not allow_test:
        raise PermissionError("sealed test document IDs cannot be loaded before lock")
    base = Path(root)
    manifest = validate_manifest(base)
    return np.load(
        base / manifest["files"][split]["document_ids_path"], mmap_mode="r"
    )
