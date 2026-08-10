"""Hash-lock helpers for prospective experiment protocols and artifacts."""

from __future__ import annotations

import hashlib
import json
from pathlib import Path


def canonical_json_hash(value: dict, *, omit: str | None = None) -> str:
    payload = dict(value)
    if omit is not None:
        payload.pop(omit, None)
    return hashlib.sha256(json.dumps(payload, sort_keys=True).encode()).hexdigest()


def file_sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with Path(path).open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def load_frozen_protocol(path: Path) -> dict:
    protocol = json.loads(Path(path).read_text())
    recorded = protocol.get("protocol_hash")
    expected = canonical_json_hash(protocol, omit="protocol_hash")
    if recorded != expected:
        raise ValueError(f"protocol hash mismatch: {recorded} != {expected}")
    return protocol


def write_json_once(path: Path, value: dict) -> str:
    """Write canonical human-readable JSON, refusing a different existing lock."""
    output = Path(path)
    payload = (json.dumps(value, indent=2) + "\n").encode()
    if output.exists() and output.read_bytes() != payload:
        raise FileExistsError(f"refusing to overwrite a different lock: {output}")
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_bytes(payload)
    return hashlib.sha256(payload).hexdigest()


def write_hash_once(path: Path, digest: str) -> None:
    """Write a SHA-256 sidecar while refusing to repair or replace a lock."""
    if len(digest) != 64 or any(
        character not in "0123456789abcdef" for character in digest
    ):
        raise ValueError("digest must be a lowercase SHA-256 hexadecimal string")
    output = Path(path)
    payload = digest + "\n"
    if output.exists() and output.read_text() != payload:
        raise FileExistsError(f"refusing to overwrite a different hash lock: {output}")
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(payload)


def verify_hash_lock(path: Path, hash_path: Path) -> str:
    actual = file_sha256(path)
    recorded = Path(hash_path).read_text().strip()
    if actual != recorded:
        raise ValueError(f"artifact hash mismatch: {actual} != {recorded}")
    return actual
