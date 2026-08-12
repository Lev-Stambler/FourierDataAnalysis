from __future__ import annotations

import json
from pathlib import Path

from dlx.analysis.character_response import enumerate_supports
from dlx.protocol.frozen import file_sha256, load_frozen_protocol

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs/local/v30_architecture_spectrum"


def test_v31_fourier_only_protocol_and_source_panel_are_frozen() -> None:
    protocol = load_frozen_protocol(ROOT / "configs/protocol_v3.1.json")
    assert protocol["protocol_hash"] == (
        "a33dac6859746c7ea6e5aab8b36842a686bfb37e2ba4f6e53b02ff4ee18d298a"
    )
    serialized = json.dumps(protocol).lower()
    assert ("n" + "tk") not in serialized
    for name in ("candidate_registry", "source_feasibility", "manifest"):
        assert (
            file_sha256(ROOT / protocol["data"][name])
            == protocol["data"][f"{name}_sha256"]
        )
    manifest = json.loads((OUT / "data_manifest.json").read_text())
    assert len(manifest["corpora"]) == 72
    assert sum(row["panel"] == "pilot" for row in manifest["corpora"]) == 24
    confirmation = [
        row for row in manifest["corpora"] if row["panel"] == "confirmation"
    ]
    assert len(confirmation) == 48
    assert {
        sum(row["stratum"] == stratum for row in confirmation)
        for stratum in {row["stratum"] for row in confirmation}
    } == {8}


def test_v31_trains_every_fourier_support_using_heldout_ce() -> None:
    protocol = load_frozen_protocol(ROOT / "configs/protocol_v3.1.json")
    spec = protocol["fourier_character_training"]
    supports = enumerate_supports(spec["lags"], max_degree=spec["max_degree"])
    assert len(supports) == 63
    assert len(protocol["architectures"]) == 5
    assert len(spec["seeds"]) == 3
    assert protocol["compute"]["fourier_character_training_cells"] == 945
    assert "cross-entropy" in spec["hardness"]
    assert spec["kernel"].startswith("one raw empirical CE-hardness value")


def test_v31_degree_three_sentinel_bank_is_frozen() -> None:
    path = ROOT / "configs/degree3_sentinels_v3.1.json"
    expected_hash = (
        ROOT / "configs/degree3_sentinels_v3.1.sha256"
    ).read_text().strip()
    assert file_sha256(path) == expected_hash
    config = json.loads(path.read_text())
    supports = [
        tuple(support)
        for group in (
            "high_energy_unmeasured_supports",
            "geometric_and_boundary_stress_supports",
        )
        for support in config["selection"][group]
    ]
    assert len(supports) == len(set(supports)) == 12
    assert all(len(support) == 3 for support in supports)
    assert config["cells"] == len(supports) * 5 * 3 == 180


def test_v32_expansion_predictions_are_frozen_for_48_unseen_corpora() -> None:
    protocol = load_frozen_protocol(ROOT / "configs/protocol_v3.2.json")
    assert protocol["protocol_hash"] == (
        "a7dd59ecaddeb5d7edc02ec139f84459171c87ae57b5e2d79a6133651b81f5a1"
    )
    prediction_path = OUT / "expansion_predictions.json"
    assert file_sha256(prediction_path) == (
        OUT / "expansion_predictions.sha256"
    ).read_text().strip()
    artifact = json.loads(prediction_path.read_text())
    assert artifact["protocol_hash"] == protocol["protocol_hash"]
    assert artifact["status"].endswith("before any v3.2 expansion training outcome")
    assert len(artifact["predictions"]) == 48 * 5 == 240
    assert len(
        {
            (row["dataset"], row["architecture"])
            for row in artifact["predictions"]
        }
    ) == 240
    assert set(artifact["models"]) == {
        "ordinary_baseline",
        "ordinary_plus_fourier",
        "strong_baseline",
        "strong_plus_fourier",
    }


def test_v32_expansion_result_and_audit_are_complete() -> None:
    analysis_path = OUT / "expansion_analysis.json"
    audit_path = OUT / "expansion_audit.json"
    assert file_sha256(analysis_path) == (
        OUT / "expansion_analysis.sha256"
    ).read_text().strip()
    assert file_sha256(audit_path) == (
        OUT / "expansion_audit.sha256"
    ).read_text().strip()
    analysis = json.loads(analysis_path.read_text())
    audit = json.loads(audit_path.read_text())
    assert analysis["verdict"] == "EXPANDED_FOURIER_SIGNAL_DOES_NOT_REPLICATE"
    assert len(analysis["rows"]) == 48 * 5 == 240
    assert not analysis["primary_gate"]["interval_strictly_positive"]
    assert audit["status"] == "PASS"
    assert audit["locks"]["results"] == file_sha256(OUT / "expansion_results.json")


def test_v33_paired_contrast_is_hash_locked_and_exploratory() -> None:
    protocol = load_frozen_protocol(ROOT / "configs/protocol_v3.3.json")
    assert protocol["protocol_hash"] == (
        "c0a97d1fb7f72ec66deb9b8a095f9c6bfeb5effe3be8107dd36f2c2a61c74975"
    )
    analysis_path = OUT / "paired_contrast_analysis.json"
    audit_path = OUT / "paired_contrast_audit.json"
    assert file_sha256(analysis_path) == (
        OUT / "paired_contrast_analysis.sha256"
    ).read_text().strip()
    assert file_sha256(audit_path) == (
        OUT / "paired_contrast_audit.sha256"
    ).read_text().strip()
    analysis = json.loads(analysis_path.read_text())
    audit = json.loads(audit_path.read_text())
    assert analysis["interpretation"]["confirmatory_verdict"] is None
    assert analysis["interpretation"]["bridge_status"] == "DIRECTIONALLY_INVERTED"
    assert len(analysis["rows"]) == 72
    assert len(analysis["all_architecture_pair_diagnostic"]) == 10
    assert audit["status"] == "PASS"
