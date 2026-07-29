"""Revision-bound storage for cached teacher final hidden states.

NumPy has no native bfloat16 dtype.  Cache shards therefore store the exact
16-bit bfloat16 bit patterns as little-endian unsigned integers.  A final
manifest is only published after every shard has passed size and checksum
validation.
"""

from __future__ import annotations

import hashlib
import json
import os
import re
import uuid
from bisect import bisect_right
from collections.abc import Mapping, Sequence
from pathlib import Path
from typing import Any

import numpy as np
import torch


SCHEMA = "qwen-fullwidth-teacher-hidden-v1"
SHARD_SIDECAR_SCHEMA = "qwen-fullwidth-teacher-hidden-shard-v1"
HIDDEN_SIZE = 1_024
STORAGE_DTYPE = "uint16_le"
TENSOR_DTYPE = "bfloat16"
MANIFEST_FILENAME = "manifest.json"
_SPLIT_PATTERN = re.compile(r"^[A-Za-z0-9_.-]+$")
_SHA256_PATTERN = re.compile(r"^[0-9a-f]{64}$")


def canonical_json_sha256(value: Mapping[str, Any]) -> str:
    """Hash a JSON mapping independent of insertion order or whitespace."""
    encoded = json.dumps(
        value,
        sort_keys=True,
        separators=(",", ":"),
        ensure_ascii=True,
    ).encode("utf-8")
    return hashlib.sha256(encoded).hexdigest()


def make_cache_identity(
    *,
    model_id: str,
    model_revision: str,
    embedding_sha256: str,
    dataset_manifest: Mapping[str, Any],
    context_length: int,
) -> dict[str, Any]:
    """Build the immutable teacher and dataset identity for one cache."""
    if not model_id or not model_revision:
        raise ValueError("model_id and model_revision must be non-empty")
    _validate_sha256(embedding_sha256, "embedding_sha256")
    if (
        isinstance(context_length, bool)
        or not isinstance(context_length, int)
        or context_length <= 0
    ):
        raise ValueError("context_length must be a positive integer")
    normalized_dataset = _normalize_mapping(
        dataset_manifest, "dataset_manifest"
    )
    return {
        "model_id": model_id,
        "model_revision": model_revision,
        "embedding_sha256": embedding_sha256,
        "dataset_manifest": normalized_dataset,
        "dataset_manifest_sha256":
            canonical_json_sha256(normalized_dataset),
        "context_length": int(context_length),
        "hidden_size": HIDDEN_SIZE,
        "tensor_dtype": TENSOR_DTYPE,
        "storage_dtype": STORAGE_DTYPE,
    }


def write_hidden_state_shard(
    root: str | Path,
    split: str,
    shard_index: int,
    row_start: int,
    hidden_states: torch.Tensor,
    *,
    cache_identity: Mapping[str, Any] | None = None,
    replace_existing: bool = False,
) -> dict[str, Any]:
    """Atomically write one raw BF16 shard and return its manifest record.

    ``replace_existing`` is intentionally opt-in.  Normal retries validate an
    existing same-name shard against the newly computed bytes and reject a
    mismatch.  Cache preparation may opt in after recomputing the exact shard
    under a validated identity, allowing recovery from a corrupt partial file
    without ever treating it as a cache hit.
    """
    _validate_split(split)
    _validate_nonnegative_int(shard_index, "shard_index")
    _validate_nonnegative_int(row_start, "row_start")
    if hidden_states.ndim != 2 or hidden_states.shape[1] != HIDDEN_SIZE:
        raise ValueError(
            f"hidden_states must have shape [rows, {HIDDEN_SIZE}]"
        )
    if hidden_states.shape[0] <= 0:
        raise ValueError("hidden_states must contain at least one row")
    if not torch.is_floating_point(hidden_states):
        raise ValueError("hidden_states must be floating point")

    root_path = Path(root)
    root_path.mkdir(parents=True, exist_ok=True)
    filename = f"{split}-{shard_index:06d}.bf16"
    destination = root_path / filename
    sidecar_path = root_path / _shard_sidecar_filename(split, shard_index)
    temporary = root_path / f".{filename}.tmp-{uuid.uuid4().hex}"
    normalized_identity = (
        None
        if cache_identity is None
        else _validate_identity(cache_identity)
    )
    bits = (
        hidden_states.detach()
        .to(device="cpu", dtype=torch.bfloat16)
        .contiguous()
        .view(torch.uint16)
        .numpy()
    )
    raw = np.asarray(bits, dtype=np.dtype("<u2"))
    try:
        mapped = np.memmap(
            temporary,
            mode="w+",
            dtype=np.dtype("<u2"),
            shape=raw.shape,
        )
        mapped[:] = raw
        mapped.flush()
        del mapped
        _fsync_file(temporary)
        digest = _file_sha256(temporary)
        record = {
            "index": shard_index,
            "row_start": row_start,
            "rows": int(raw.shape[0]),
            "hidden_size": HIDDEN_SIZE,
            "tensor_dtype": TENSOR_DTYPE,
            "storage_dtype": STORAGE_DTYPE,
            "file": filename,
            "bytes": int(raw.nbytes),
            "sha256": digest,
        }
        if destination.exists():
            try:
                validate_shard(root_path, record)
            except RuntimeError:
                if not replace_existing:
                    raise
                os.replace(temporary, destination)
                _fsync_directory(root_path)
        else:
            os.replace(temporary, destination)
            _fsync_directory(root_path)
        validate_shard(root_path, record)
        if normalized_identity is not None:
            sidecar = {
                "schema": SHARD_SIDECAR_SCHEMA,
                "identity": normalized_identity,
                "record": record,
            }
            sidecar["sidecar_sha256"] = canonical_json_sha256(sidecar)
            _atomic_write_json(sidecar_path, sidecar)
        return record
    finally:
        temporary.unlink(missing_ok=True)


def validate_shard(
    root: str | Path,
    record: Mapping[str, Any],
    *,
    checksum: bool = True,
) -> Path:
    """Validate shard metadata, byte length, and content checksum."""
    value = _normalize_mapping(record, "shard record")
    required = {
        "index",
        "row_start",
        "rows",
        "hidden_size",
        "tensor_dtype",
        "storage_dtype",
        "file",
        "bytes",
        "sha256",
    }
    if set(value) != required:
        raise RuntimeError(
            "invalid shard record keys: "
            f"expected {sorted(required)}, got {sorted(value)}"
        )
    _validate_nonnegative_int(value["index"], "shard index", RuntimeError)
    _validate_nonnegative_int(
        value["row_start"], "shard row_start", RuntimeError
    )
    _validate_positive_int(value["rows"], "shard rows", RuntimeError)
    if value["hidden_size"] != HIDDEN_SIZE:
        raise RuntimeError(
            f"shard hidden_size must be {HIDDEN_SIZE}"
        )
    if value["tensor_dtype"] != TENSOR_DTYPE:
        raise RuntimeError(f"shard tensor_dtype must be {TENSOR_DTYPE}")
    if value["storage_dtype"] != STORAGE_DTYPE:
        raise RuntimeError(f"shard storage_dtype must be {STORAGE_DTYPE}")
    _validate_sha256(value["sha256"], "shard sha256", RuntimeError)
    filename = value["file"]
    if (
        not isinstance(filename, str)
        or not filename
        or Path(filename).name != filename
    ):
        raise RuntimeError("shard file must be a safe basename")
    expected_bytes = value["rows"] * HIDDEN_SIZE * 2
    if value["bytes"] != expected_bytes:
        raise RuntimeError(
            f"shard byte metadata mismatch: {value['bytes']} "
            f"!= {expected_bytes}"
        )
    path = Path(root) / filename
    if not path.is_file():
        raise RuntimeError(f"missing teacher cache shard {path}")
    if path.stat().st_size != expected_bytes:
        raise RuntimeError(
            f"teacher cache shard has wrong size: {path.stat().st_size} "
            f"!= {expected_bytes}"
        )
    if checksum:
        digest = _file_sha256(path)
        if digest != value["sha256"]:
            raise RuntimeError(
                f"teacher cache shard checksum mismatch for {filename}"
            )
    return path


def load_hidden_state_shard_record(
    root: str | Path,
    split: str,
    shard_index: int,
    *,
    expected_identity: Mapping[str, Any],
    expected_row_start: int | None = None,
    expected_rows: int | None = None,
    validate_checksum: bool = True,
) -> dict[str, Any] | None:
    """Load an identity-bound completed-shard sidecar.

    A missing sidecar is an incomplete shard, even if a same-name raw file is
    present.  This is what makes a crash between data publication and sidecar
    publication safe: the next worker recomputes instead of trusting an
    unbound file.
    """
    _validate_split(split)
    _validate_nonnegative_int(shard_index, "shard_index")
    if expected_row_start is not None:
        _validate_nonnegative_int(expected_row_start, "expected_row_start")
    if expected_rows is not None:
        _validate_positive_int(expected_rows, "expected_rows")
    identity = _validate_identity(expected_identity)
    path = Path(root) / _shard_sidecar_filename(split, shard_index)
    if not path.is_file():
        return None
    try:
        value = json.loads(path.read_text())
    except (OSError, json.JSONDecodeError) as error:
        raise RuntimeError(
            f"invalid teacher cache shard sidecar at {path}"
        ) from error
    value = _normalize_mapping(value, "teacher cache shard sidecar")
    digest = value.get("sidecar_sha256")
    _validate_sha256(digest, "sidecar_sha256", RuntimeError)
    unsigned = dict(value)
    del unsigned["sidecar_sha256"]
    if canonical_json_sha256(unsigned) != digest:
        raise RuntimeError(
            f"teacher cache shard sidecar checksum mismatch at {path}"
        )
    if value.get("schema") != SHARD_SIDECAR_SCHEMA:
        raise RuntimeError(
            f"teacher cache shard sidecar schema mismatch at {path}"
        )
    try:
        stored_identity = _validate_identity(value.get("identity"))
    except ValueError as error:
        raise RuntimeError(
            f"invalid teacher cache shard identity at {path}"
        ) from error
    if stored_identity != identity:
        raise RuntimeError(
            f"teacher cache shard identity mismatch at {path}"
        )
    record = _normalize_mapping(
        value.get("record"), "teacher cache shard record"
    )
    expected_file = f"{split}-{shard_index:06d}.bf16"
    if (
        record.get("index") != shard_index
        or record.get("file") != expected_file
    ):
        raise RuntimeError(
            f"teacher cache shard sidecar names the wrong shard at {path}"
        )
    if (
        expected_row_start is not None
        and record.get("row_start") != expected_row_start
    ):
        raise RuntimeError(
            f"teacher cache shard row_start mismatch at {path}"
        )
    if expected_rows is not None and record.get("rows") != expected_rows:
        raise RuntimeError(f"teacher cache shard row count mismatch at {path}")
    validate_shard(Path(root), record, checksum=validate_checksum)
    return record


def open_hidden_state_shard(
    root: str | Path,
    record: Mapping[str, Any],
    *,
    validate: bool = True,
) -> np.memmap:
    """Open a raw shard as a read-only ``[rows, 1024]`` uint16 memmap."""
    value = _normalize_mapping(record, "shard record")
    path = (
        validate_shard(root, value)
        if validate
        else Path(root) / str(value["file"])
    )
    return np.memmap(
        path,
        mode="r",
        dtype=np.dtype("<u2"),
        shape=(int(value["rows"]), HIDDEN_SIZE),
    )


def read_hidden_state_shard(
    root: str | Path,
    record: Mapping[str, Any],
    *,
    device: str | torch.device | None = None,
    validate: bool = True,
) -> torch.Tensor:
    """Read one shard into an owning bfloat16 torch tensor."""
    mapped = open_hidden_state_shard(root, record, validate=validate)
    # Copy out of the read-only mapping before exposing the tensor.  This
    # avoids a non-writable NumPy view and decouples its lifetime from memmap.
    native_bits = np.array(mapped, dtype=np.uint16, copy=True)
    value = torch.from_numpy(native_bits).view(torch.bfloat16)
    return value.to(device=device) if device is not None else value


def finalize_teacher_cache(
    root: str | Path,
    identity: Mapping[str, Any],
    shards: Mapping[str, Sequence[Mapping[str, Any]]],
    *,
    expected_split_rows: Mapping[str, int] | None = None,
    records_checksum_validated: bool = False,
) -> dict[str, Any]:
    """Validate all shards and atomically publish the final manifest.

    ``records_checksum_validated`` is for coordinators that receive every
    record directly from workers which have just checksum-validated the
    corresponding shard.  It skips a redundant serial content pass while
    retaining all structural, size, contiguity, and manifest checks.
    """
    root_path = Path(root)
    root_path.mkdir(parents=True, exist_ok=True)
    normalized_identity = _validate_identity(identity)
    if not shards:
        raise ValueError("at least one cache split is required")
    expected_rows = (
        None
        if expected_split_rows is None
        else {
            str(split): _positive_int(rows, f"{split} expected rows")
            for split, rows in expected_split_rows.items()
        }
    )
    if expected_rows is not None and set(expected_rows) != set(shards):
        raise ValueError(
            "expected_split_rows must name exactly the cached splits"
        )

    split_values: dict[str, Any] = {}
    for split, records in sorted(shards.items()):
        _validate_split(split)
        normalized_records = [
            _normalize_mapping(record, f"{split} shard record")
            for record in records
        ]
        normalized_records.sort(key=lambda record: record["row_start"])
        if not normalized_records:
            raise ValueError(f"cache split {split} has no shards")
        cursor = 0
        seen_indices: set[int] = set()
        for record in normalized_records:
            validate_shard(
                root_path,
                record,
                checksum=not records_checksum_validated,
            )
            if record["index"] in seen_indices:
                raise RuntimeError(
                    f"duplicate shard index {record['index']} in {split}"
                )
            seen_indices.add(record["index"])
            if record["row_start"] != cursor:
                raise RuntimeError(
                    f"non-contiguous {split} shard rows: expected "
                    f"row_start {cursor}, got {record['row_start']}"
                )
            cursor += record["rows"]
        if expected_rows is not None and cursor != expected_rows[split]:
            raise RuntimeError(
                f"{split} cache has {cursor} rows, expected "
                f"{expected_rows[split]}"
            )
        split_values[split] = {
            "rows": cursor,
            "shards": normalized_records,
        }

    payload = {
        "schema": SCHEMA,
        **normalized_identity,
        "splits": split_values,
    }
    payload["manifest_sha256"] = canonical_json_sha256(payload)
    # Every shard's contents were checksum-validated in the construction loop
    # above.  Revalidate the assembled manifest structure and file sizes here,
    # but do not read and hash the full cache a second time.
    _validate_manifest_value(
        root_path,
        payload,
        expected_identity=normalized_identity,
        validate_shards=False,
    )

    manifest_path = root_path / MANIFEST_FILENAME
    if manifest_path.exists():
        existing = load_teacher_cache_manifest(
            root_path,
            expected_identity=normalized_identity,
            validate_shards=True,
        )
        if existing != payload:
            raise RuntimeError(
                f"refusing to replace incompatible cache manifest at "
                f"{manifest_path}"
            )
        return existing
    _atomic_write_json(manifest_path, payload)
    return payload


def load_teacher_cache_manifest(
    root: str | Path,
    *,
    expected_identity: Mapping[str, Any] | None = None,
    validate_shards: bool = True,
) -> dict[str, Any]:
    """Load a complete cache, rejecting stale identities or partial shards."""
    root_path = Path(root)
    path = root_path / MANIFEST_FILENAME
    if not path.is_file():
        raise RuntimeError(f"missing teacher cache manifest at {path}")
    try:
        value = json.loads(path.read_text())
    except (OSError, json.JSONDecodeError) as error:
        raise RuntimeError(
            f"invalid teacher cache manifest at {path}"
        ) from error
    return _validate_manifest_value(
        root_path,
        value,
        expected_identity=expected_identity,
        validate_shards=validate_shards,
    )


def reconstruct_logits(
    final_hidden_states: torch.Tensor,
    tied_embedding: torch.Tensor,
    *,
    vocab_size: int | None = None,
) -> torch.Tensor:
    """Reconstruct tied-head logits from cached final hidden states."""
    if final_hidden_states.ndim != 2:
        raise ValueError("final_hidden_states must be a 2D tensor")
    if (
        final_hidden_states.shape[1] != HIDDEN_SIZE
        or tied_embedding.ndim != 2
        or tied_embedding.shape[1] != HIDDEN_SIZE
    ):
        raise ValueError(
            f"hidden states and embedding must have width {HIDDEN_SIZE}"
        )
    if tied_embedding.requires_grad:
        raise ValueError("tied_embedding must be frozen")
    if final_hidden_states.device != tied_embedding.device:
        raise ValueError("hidden states and tied embedding must share a device")
    if final_hidden_states.dtype != tied_embedding.dtype:
        raise ValueError("hidden states and tied embedding must share a dtype")
    rows = tied_embedding.shape[0]
    if vocab_size is not None:
        if (
            isinstance(vocab_size, bool)
            or not isinstance(vocab_size, int)
            or not 0 < vocab_size <= rows
        ):
            raise ValueError(
                "vocab_size must be between one and the embedding row count"
            )
        rows = vocab_size
    return torch.nn.functional.linear(
        final_hidden_states,
        tied_embedding[:rows],
    )


class TeacherHiddenCache:
    """Random-access reader over validated contiguous hidden-state shards."""

    def __init__(
        self,
        root: str | Path,
        *,
        expected_identity: Mapping[str, Any] | None = None,
        validate_shards: bool = False,
    ) -> None:
        self.root = Path(root)
        self.manifest = load_teacher_cache_manifest(
            self.root,
            expected_identity=expected_identity,
            validate_shards=validate_shards,
        )
        self._splits: dict[str, tuple[list[int], list[np.memmap], int]] = {}
        for split, value in self.manifest["splits"].items():
            records = value["shards"]
            starts = [int(record["row_start"]) for record in records]
            mappings = [
                open_hidden_state_shard(
                    self.root,
                    record,
                    validate=False,
                )
                for record in records
            ]
            self._splits[split] = (
                starts,
                mappings,
                int(value["rows"]),
            )

    def take(
        self,
        split: str,
        indices,
        *,
        device: str | torch.device | None = None,
    ) -> torch.Tensor:
        if split not in self._splits:
            raise ValueError(f"teacher cache has no split {split}")
        source = np.asarray(indices)
        if source.ndim != 1:
            raise ValueError("teacher cache indices must be one-dimensional")
        if source.size == 0:
            result = torch.empty((0, HIDDEN_SIZE), dtype=torch.bfloat16)
            return result.to(device=device) if device is not None else result
        if source.dtype.kind not in ("i", "u"):
            raise ValueError("teacher cache indices must be integers")
        values = source.astype(np.int64, copy=False)
        starts, mappings, rows = self._splits[split]
        if len(values) and (int(values.min()) < 0 or int(values.max()) >= rows):
            raise IndexError("teacher cache index is out of range")
        result = np.empty((len(values), HIDDEN_SIZE), dtype=np.uint16)
        groups: dict[int, list[tuple[int, int]]] = {}
        for output_index, row in enumerate(values.tolist()):
            shard_index = bisect_right(starts, row) - 1
            groups.setdefault(shard_index, []).append(
                (output_index, row - starts[shard_index])
            )
        for shard_index, pairs in groups.items():
            output_indices = np.fromiter(
                (pair[0] for pair in pairs),
                dtype=np.int64,
            )
            local_indices = np.fromiter(
                (pair[1] for pair in pairs),
                dtype=np.int64,
            )
            result[output_indices] = mappings[shard_index][local_indices]
        tensor = torch.from_numpy(result).view(torch.bfloat16)
        return tensor.to(device=device) if device is not None else tensor


def _validate_manifest_value(
    root: Path,
    manifest: Mapping[str, Any],
    *,
    expected_identity: Mapping[str, Any] | None,
    validate_shards: bool,
) -> dict[str, Any]:
    value = _normalize_mapping(manifest, "teacher cache manifest")
    digest = value.get("manifest_sha256")
    _validate_sha256(digest, "manifest_sha256", RuntimeError)
    unsigned = dict(value)
    del unsigned["manifest_sha256"]
    if canonical_json_sha256(unsigned) != digest:
        raise RuntimeError("teacher cache manifest checksum mismatch")
    if value.get("schema") != SCHEMA:
        raise RuntimeError(
            f"teacher cache schema mismatch: {value.get('schema')!r}"
        )
    identity = {
        key: value.get(key)
        for key in _identity_keys()
    }
    try:
        normalized_identity = _validate_identity(identity)
    except ValueError as error:
        raise RuntimeError("invalid teacher cache identity") from error
    if expected_identity is not None:
        expected = _validate_identity(expected_identity)
        if normalized_identity != expected:
            raise RuntimeError("teacher cache identity mismatch")
    splits = value.get("splits")
    if not isinstance(splits, dict) or not splits:
        raise RuntimeError("teacher cache manifest has no splits")
    for split, split_value in splits.items():
        _validate_split(split, RuntimeError)
        if not isinstance(split_value, dict):
            raise RuntimeError(f"invalid manifest entry for split {split}")
        if set(split_value) != {"rows", "shards"}:
            raise RuntimeError(f"invalid manifest keys for split {split}")
        _validate_positive_int(
            split_value["rows"], f"{split} rows", RuntimeError
        )
        records = split_value["shards"]
        if not isinstance(records, list) or not records:
            raise RuntimeError(f"manifest split {split} has no shards")
        cursor = 0
        seen_indices: set[int] = set()
        for record in records:
            normalized_record = _normalize_mapping(
                record, f"{split} shard record"
            )
            if normalized_record.get("index") in seen_indices:
                raise RuntimeError(
                    f"duplicate shard index in manifest split {split}"
                )
            seen_indices.add(normalized_record.get("index"))
            if normalized_record.get("row_start") != cursor:
                raise RuntimeError(
                    f"non-contiguous shard rows in manifest split {split}"
                )
            # Structural validation and file-size checks are cheap and always
            # mandatory.  ``validate_shards`` only controls the O(total bytes)
            # checksum pass, which training can skip after finalization.
            validate_shard(
                root,
                normalized_record,
                checksum=validate_shards,
            )
            cursor += normalized_record["rows"]
        if cursor != split_value["rows"]:
            raise RuntimeError(f"manifest row count mismatch for split {split}")
    return value


def _validate_identity(identity: Mapping[str, Any]) -> dict[str, Any]:
    value = _normalize_mapping(identity, "cache identity")
    if set(value) != set(_identity_keys()):
        raise ValueError(
            "cache identity must contain exactly "
            f"{sorted(_identity_keys())}"
        )
    if (
        not isinstance(value["model_id"], str)
        or not isinstance(value["model_revision"], str)
        or not value["model_id"]
        or not value["model_revision"]
    ):
        raise ValueError("cache identity has an empty model id or revision")
    _validate_sha256(
        value["embedding_sha256"], "embedding_sha256", ValueError
    )
    dataset = _normalize_mapping(
        value["dataset_manifest"], "dataset_manifest"
    )
    expected_dataset_hash = canonical_json_sha256(dataset)
    if value["dataset_manifest_sha256"] != expected_dataset_hash:
        raise ValueError("dataset manifest hash mismatch in cache identity")
    _validate_positive_int(
        value["context_length"], "context_length", ValueError
    )
    if value["hidden_size"] != HIDDEN_SIZE:
        raise ValueError(f"cache hidden_size must be {HIDDEN_SIZE}")
    if value["tensor_dtype"] != TENSOR_DTYPE:
        raise ValueError(f"cache tensor_dtype must be {TENSOR_DTYPE}")
    if value["storage_dtype"] != STORAGE_DTYPE:
        raise ValueError(f"cache storage_dtype must be {STORAGE_DTYPE}")
    value["dataset_manifest"] = dataset
    return value


def _identity_keys() -> tuple[str, ...]:
    return (
        "model_id",
        "model_revision",
        "embedding_sha256",
        "dataset_manifest",
        "dataset_manifest_sha256",
        "context_length",
        "hidden_size",
        "tensor_dtype",
        "storage_dtype",
    )


def _shard_sidecar_filename(split: str, shard_index: int) -> str:
    return f"{split}-{shard_index:06d}.json"


def _normalize_mapping(
    value: Mapping[str, Any],
    name: str,
) -> dict[str, Any]:
    if not isinstance(value, Mapping):
        raise ValueError(f"{name} must be a mapping")
    try:
        normalized = json.loads(
            json.dumps(
                dict(value),
                sort_keys=True,
                separators=(",", ":"),
                ensure_ascii=True,
            )
        )
    except (TypeError, ValueError) as error:
        raise ValueError(f"{name} must be JSON serializable") from error
    if not isinstance(normalized, dict):
        raise ValueError(f"{name} must be a JSON object")
    return normalized


def _validate_split(
    split: Any,
    error_type: type[Exception] = ValueError,
) -> None:
    if not isinstance(split, str) or not _SPLIT_PATTERN.fullmatch(split):
        raise error_type(f"invalid cache split name {split!r}")


def _validate_sha256(
    value: Any,
    name: str,
    error_type: type[Exception] = ValueError,
) -> None:
    if not isinstance(value, str) or not _SHA256_PATTERN.fullmatch(value):
        raise error_type(f"{name} must be a lowercase SHA-256 hex digest")


def _validate_nonnegative_int(
    value: Any,
    name: str,
    error_type: type[Exception] = ValueError,
) -> None:
    if isinstance(value, bool) or not isinstance(value, int) or value < 0:
        raise error_type(f"{name} must be a non-negative integer")


def _validate_positive_int(
    value: Any,
    name: str,
    error_type: type[Exception] = ValueError,
) -> None:
    if isinstance(value, bool) or not isinstance(value, int) or value <= 0:
        raise error_type(f"{name} must be a positive integer")


def _positive_int(value: Any, name: str) -> int:
    _validate_positive_int(value, name)
    return int(value)


def _file_sha256(path: Path, chunk_bytes: int = 8 * 1024 * 1024) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        while chunk := stream.read(chunk_bytes):
            digest.update(chunk)
    return digest.hexdigest()


def _atomic_write_json(path: Path, value: Mapping[str, Any]) -> None:
    temporary = path.parent / f".{path.name}.tmp-{uuid.uuid4().hex}"
    try:
        encoded = json.dumps(
            value,
            sort_keys=True,
            separators=(",", ":"),
        ).encode("utf-8")
        with temporary.open("xb") as stream:
            stream.write(encoded)
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(temporary, path)
        _fsync_directory(path.parent)
    finally:
        temporary.unlink(missing_ok=True)


def _fsync_file(path: Path) -> None:
    with path.open("rb") as stream:
        os.fsync(stream.fileno())


def _fsync_directory(path: Path) -> None:
    descriptor = os.open(path, os.O_RDONLY)
    try:
        os.fsync(descriptor)
    finally:
        os.close(descriptor)
