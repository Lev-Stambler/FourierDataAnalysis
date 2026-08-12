from __future__ import annotations

import json
from pathlib import Path

from dlx.analysis.character_response import enumerate_supports
from dlx.protocol.frozen import file_sha256, load_frozen_protocol

ROOT = Path(__file__).parent.parent


def test_v34_protocol_and_panel_are_frozen() -> None:
    protocol = load_frozen_protocol(ROOT / "configs/protocol_v3.4.json")
    assert protocol["protocol_hash"] == (
        "06ae1e6cbc9cfe35b35bfff8280da11f6b12e0e61879bfde3ce9f834851bc6f7"
    )
    panel_path = ROOT / protocol["panel"]["path"]
    assert file_sha256(panel_path) == protocol["panel"]["sha256"]
    panel = json.loads(panel_path.read_text())
    assert len(panel["strata"]) == 6
    for value in panel["strata"].values():
        assert len(value["development"]) == len(value["confirmation"]) == 4
        assert set(value["development"]).isdisjoint(value["confirmation"])


def test_v34_grids_are_exact_and_single_h100() -> None:
    protocol = load_frozen_protocol(ROOT / "configs/protocol_v3.4.json")
    supports = enumerate_supports(
        protocol["uniform_character_probe"]["lags"], max_degree=2
    )
    assert len(supports) == 28
    assert 3 * 28 * 2 == protocol["uniform_character_probe"]["new_training_cells"]
    assert 3 * 48 * 2 == protocol["natural_training"]["new_cells"]
    assert protocol["compute"]["maximum_concurrent_h100s"] == 1


def test_v34_analysis_is_locked_when_complete() -> None:
    analysis_path = ROOT / "runs/local/v34_local_window/analysis.json"
    if not analysis_path.exists():
        return
    out = analysis_path.parent
    assert file_sha256(analysis_path) == (out / "analysis.sha256").read_text().strip()
    assert file_sha256(out / "audit.json") == (out / "audit.sha256").read_text().strip()
    analysis = json.loads(analysis_path.read_text())
    audit = json.loads((out / "audit.json").read_text())
    assert len(analysis["rows"]) == 144
    assert audit["status"] == "PASS"
