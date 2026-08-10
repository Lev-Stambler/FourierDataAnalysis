from __future__ import annotations

import json
from pathlib import Path

from dlx.protocol.frozen import file_sha256, load_frozen_protocol

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs/local/v28_random_windows"


def test_v28_protocol_and_manifests_are_frozen() -> None:
    protocol = load_frozen_protocol(ROOT / "configs/protocol_v2.8.json")
    manifest = json.loads((OUT / "data_manifest.json").read_text())
    profiles = json.loads((OUT / "profile_manifest.json").read_text())

    assert protocol["protocol_hash"] == (
        "a9043972453a18e17d559c8d5839de08a4445ff4680ea480452e0caddfaabbc1"
    )
    assert (
        file_sha256(OUT / "data_manifest.json") == protocol["data"]["manifest_sha256"]
    )
    assert len(manifest["corpora"]) == 78
    assert sum(row["panel"] == "development" for row in manifest["corpora"]) == 54
    assert sum(row["panel"] == "confirmation" for row in manifest["corpora"]) == 24
    assert len(profiles["profiles"]) == 78
    assert protocol["sampling"]["block_size"] == 16_384
    assert protocol["sampling"]["unit"].startswith("contiguous next-token window")
    assert protocol["training"]["ctx_len"] + 1 == 65


def test_v28_predictions_were_frozen_with_positive_locality_direction() -> None:
    predictions = json.loads((OUT / "predictions.json").read_text())

    assert (
        file_sha256(OUT / "predictions.json")
        == (OUT / "predictions.sha256").read_text().strip()
    )
    assert predictions["development_locality_coefficient"] > 0.0
    assert len(predictions["predictions"]) == 24
    assert all(
        set(row["predictions"]) == {"intercept_only", "marginal_locality"}
        for row in predictions["predictions"]
    )


def test_v28_completed_random_window_repair_passes_audit() -> None:
    development = json.loads((OUT / "development_results.json").read_text())
    confirmation = json.loads((OUT / "confirmation_results.json").read_text())
    analysis = json.loads((OUT / "analysis.json").read_text())
    audit = json.loads((OUT / "audit.json").read_text())

    assert len(development) == 108
    assert len(confirmation) == 48
    assert analysis["verdict"] == "PREDICTIVE_ONLY"
    assert analysis["primary_gates"] == {
        "predictive_rmse_gate": True,
        "blocked_rank_gate": False,
    }
    assert audit["status"] == "PASS"
    assert all(audit["checks"].values())
