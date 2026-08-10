from __future__ import annotations

import gzip
import hashlib
import json
from pathlib import Path

import pytest

from dlx.analysis.text_panel import (
    load_v24_confirmation_rows,
    load_v24_development_rows,
)
from dlx.protocol.confirmation import confirmation_cells
from dlx.protocol.frozen import (
    load_frozen_protocol,
    verify_hash_lock,
    write_hash_once,
    write_json_once,
)

ROOT = Path(__file__).parent.parent


def test_v26_protocol_is_hash_valid_and_enumerates_exact_grid() -> None:
    protocol = load_frozen_protocol(ROOT / "configs/protocol_v2.6.json")
    cells = confirmation_cells(protocol)
    assert len(protocol["corpora"]["sources"]) == 32
    assert len(cells) == 64
    assert len({(row["dataset"], row["seed"]) for row in cells}) == 64
    assert {source["stratum"] for source in protocol["corpora"]["sources"]} == {
        "documentation",
        "language",
        "additional_code",
        "formal",
    }


def test_shared_v24_loaders_recover_expected_natural_training_panel() -> None:
    development = load_v24_development_rows(ROOT)
    confirmation = load_v24_confirmation_rows(ROOT)
    comparable = [
        row
        for row in development + confirmation
        if row["stride"] == 1 and row["configuration"] == "learned_absolute_d64_l2"
    ]
    assert len(comparable) == 22
    assert len({row["dataset"] for row in comparable}) == 22


def test_v25_compact_profiles_have_lossless_hash_locked_audits() -> None:
    manifest = json.loads(
        (ROOT / "runs/local/v25_kiss_diagnostic/manifest.json").read_text()
    )
    assert len(manifest["profiles"]) == 25
    for row in manifest["profiles"]:
        summary = ROOT / row["summary"]
        audit = ROOT / row["audit"]
        assert hashlib.sha256(summary.read_bytes()).hexdigest() == row["summary_sha256"]
        assert hashlib.sha256(audit.read_bytes()).hexdigest() == row["audit_sha256"]
        details = json.loads(gzip.decompress(audit.read_bytes()))
        assert details["summary_sha256"] == row["summary_sha256"]
        assert details["chains"]


def test_hash_locks_refuse_overwrite_and_detect_tampering(tmp_path: Path) -> None:
    artifact = tmp_path / "artifact.json"
    digest = write_json_once(artifact, {"value": 1})
    assert write_json_once(artifact, {"value": 1}) == digest
    with pytest.raises(FileExistsError):
        write_json_once(artifact, {"value": 2})
    lock = tmp_path / "artifact.sha256"
    write_hash_once(lock, digest)
    write_hash_once(lock, digest)
    with pytest.raises(FileExistsError):
        write_hash_once(lock, "0" * 64)
    assert verify_hash_lock(artifact, lock) == digest
    artifact.write_text("tampered")
    with pytest.raises(ValueError, match="hash mismatch"):
        verify_hash_lock(artifact, lock)
