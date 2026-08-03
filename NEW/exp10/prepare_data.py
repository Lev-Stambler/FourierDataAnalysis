"""Prepare the shared 16K BPE and deterministic Exp10 token windows."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path

import numpy as np

from architecture_verdict import CONTEXT_LENGTH, VOCAB_SIZE, sha256


SCHEMA = "exp10-tokenized-corpus-v1"
TOKENIZER_SCHEMA = "exp10-shared-bpe-v1"
DATASETS = {
    "tinystories": {
        "id": "roneneldan/TinyStories",
        "config": None,
        "revision": "f54c09fd23315a6f9c86f9dc80f725de7d8f9c64",
        "splits": {"train": "train", "validation": "validation", "test": "validation"},
    },
    "wikitext": {
        "id": "Salesforce/wikitext",
        "config": "wikitext-103-raw-v1",
        "revision": "b08601e04326c79dfdd32d625aee71d232d685c3",
        "splits": {"train": "train", "validation": "validation", "test": "test"},
    },
}


def atomic_json(path: Path, value: dict) -> None:
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_text(json.dumps(value, indent=2, sort_keys=True))
    os.replace(temporary, path)


def records(dataset: str, split: str, revision: str = ""):
    from datasets import load_dataset

    source = DATASETS[dataset]
    kwargs = {
        "path": source["id"],
        "split": source["splits"][split],
        "streaming": True,
        "revision": revision or source["revision"],
    }
    if source["config"]:
        kwargs["name"] = source["config"]
    yield from load_dataset(**kwargs)


def tokenizer_training_text(documents_per_dataset: int):
    for dataset in DATASETS:
        count = 0
        for record in records(dataset, "train"):
            text = (record.get("text") or "").strip()
            if not text:
                continue
            yield text
            count += 1
            if count >= documents_per_dataset:
                break


def train_tokenizer(output: Path, documents_per_dataset: int) -> dict:
    from tokenizers import Tokenizer, decoders, models, normalizers, pre_tokenizers, trainers

    output.mkdir(parents=True, exist_ok=True)
    tokenizer_path = output / "tokenizer.json"
    manifest_path = output / "manifest.json"
    if tokenizer_path.is_file() and manifest_path.is_file():
        value = json.loads(manifest_path.read_text())
        if value.get("tokenizer_sha256") == sha256(tokenizer_path):
            return value
        raise RuntimeError(f"invalid tokenizer artifact at {output}")

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
    tokenizer.train_from_iterator(
        tokenizer_training_text(documents_per_dataset), trainer=trainer
    )
    if tokenizer.get_vocab_size() != VOCAB_SIZE:
        raise RuntimeError(
            f"tokenizer has {tokenizer.get_vocab_size()} entries, expected {VOCAB_SIZE}"
        )
    tokenizer.save(str(tokenizer_path))
    value = {
        "schema": TOKENIZER_SCHEMA,
        "vocab_size": VOCAB_SIZE,
        "training_sources": list(DATASETS),
        "documents_per_dataset": documents_per_dataset,
        "dataset_revisions": {
            name: source["revision"] for name, source in DATASETS.items()
        },
        "tokenizer_sha256": sha256(tokenizer_path),
    }
    atomic_json(manifest_path, value)
    return value


def split_limits(dataset: str, split: str, train_tokens: int, eval_tokens: int) -> int:
    if split == "train":
        return train_tokens
    if dataset == "tinystories" and split == "test":
        return eval_tokens
    return eval_tokens


def write_windows(
    dataset: str,
    split: str,
    tokenizer,
    raw_path: Path,
    token_limit: int,
    revision: str,
) -> int:
    eos = tokenizer.token_to_id("<eos>")
    pending: list[int] = []
    windows = 0
    source_split = DATASETS[dataset]["splits"][split]
    validation_parity = 0 if split == "validation" else 1
    with raw_path.open("wb") as handle:
        for index, record in enumerate(records(dataset, split, revision)):
            if dataset == "tinystories" and source_split == "validation":
                digest = hashlib.sha256((record.get("text") or "").encode()).digest()
                if digest[0] % 2 != validation_parity:
                    continue
            text = (record.get("text") or "").strip()
            if not text:
                continue
            pending.extend(tokenizer.encode(text).ids)
            pending.append(eos)
            while len(pending) >= CONTEXT_LENGTH + 1:
                values = np.asarray(pending[: CONTEXT_LENGTH + 1], dtype=np.uint16)
                handle.write(values.tobytes())
                del pending[: CONTEXT_LENGTH + 1]
                windows += 1
                if token_limit and windows * CONTEXT_LENGTH >= token_limit:
                    return windows
    return windows


def raw_to_npy(raw_path: Path, npy_path: Path, windows: int) -> None:
    raw = np.memmap(
        raw_path, mode="r", dtype=np.uint16, shape=(windows, CONTEXT_LENGTH + 1)
    )
    output = np.lib.format.open_memmap(
        npy_path,
        mode="w+",
        dtype=np.uint16,
        shape=(windows, CONTEXT_LENGTH + 1),
    )
    for start in range(0, windows, 16_384):
        output[start : start + 16_384] = raw[start : start + 16_384]
    output.flush()
    del output, raw
    raw_path.unlink()


def prepare_corpus(
    dataset: str,
    tokenizer_root: Path,
    output: Path,
    revision: str,
    train_tokens: int,
    eval_tokens: int,
) -> dict:
    from tokenizers import Tokenizer

    tokenizer_path = tokenizer_root / "tokenizer.json"
    tokenizer_manifest = json.loads((tokenizer_root / "manifest.json").read_text())
    if tokenizer_manifest.get("tokenizer_sha256") != sha256(tokenizer_path):
        raise RuntimeError("shared tokenizer checksum mismatch")
    manifest_path = output / "manifest.json"
    if manifest_path.is_file():
        value = json.loads(manifest_path.read_text())
        for name, metadata in value.get("files", {}).items():
            if sha256(output / name) != metadata["sha256"]:
                raise RuntimeError(f"corrupt cached corpus file: {name}")
        return value

    output.mkdir(parents=True, exist_ok=True)
    tokenizer = Tokenizer.from_file(str(tokenizer_path))
    files = {}
    for split in ("train", "validation", "test"):
        raw_path = output / f"{split}.raw"
        npy_path = output / f"{split}.npy"
        limit = split_limits(dataset, split, train_tokens, eval_tokens)
        windows = write_windows(dataset, split, tokenizer, raw_path, limit, revision)
        if windows == 0:
            raise RuntimeError(f"no {dataset}/{split} windows were produced")
        raw_to_npy(raw_path, npy_path, windows)
        files[npy_path.name] = {
            "sha256": sha256(npy_path),
            "windows": windows,
            "prediction_tokens": windows * CONTEXT_LENGTH,
        }
    value = {
        "schema": SCHEMA,
        "dataset": dataset,
        "dataset_id": DATASETS[dataset]["id"],
        "dataset_config": DATASETS[dataset]["config"],
        "dataset_revision": revision or DATASETS[dataset]["revision"],
        "context_length": CONTEXT_LENGTH,
        "vocab_size": VOCAB_SIZE,
        "tokenizer_sha256": tokenizer_manifest["tokenizer_sha256"],
        "files": files,
    }
    atomic_json(manifest_path, value)
    return value


def parser() -> argparse.ArgumentParser:
    value = argparse.ArgumentParser()
    subparsers = value.add_subparsers(dest="command", required=True)
    tokenizer = subparsers.add_parser("tokenizer")
    tokenizer.add_argument("--output", type=Path, required=True)
    tokenizer.add_argument("--documents-per-dataset", type=int, default=100_000)
    corpus = subparsers.add_parser("corpus")
    corpus.add_argument("--dataset", choices=DATASETS, required=True)
    corpus.add_argument("--tokenizer-root", type=Path, required=True)
    corpus.add_argument("--output", type=Path, required=True)
    corpus.add_argument("--revision", default="")
    corpus.add_argument("--train-tokens", type=int, default=0)
    corpus.add_argument("--eval-tokens", type=int, default=5_000_000)
    return value


def main() -> None:
    args = parser().parse_args()
    if args.command == "tokenizer":
        result = train_tokenizer(args.output, args.documents_per_dataset)
    else:
        result = prepare_corpus(
            args.dataset,
            args.tokenizer_root,
            args.output,
            args.revision,
            args.train_tokens,
            args.eval_tokens,
        )
    print(json.dumps(result, indent=2), flush=True)
    # datasets/pyarrow can leave background finalizers waiting indefinitely
    # after a streaming iterator has already produced a durable artifact.
    # All files above are flushed and atomically manifested before this point.
    os._exit(0)


if __name__ == "__main__":
    main()
