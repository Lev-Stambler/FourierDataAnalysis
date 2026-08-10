from __future__ import annotations

import json
from pathlib import Path

from dlx.analysis.character_response import enumerate_supports
from dlx.protocol.frozen import file_sha256, load_frozen_protocol

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs/local/v30_architecture_spectrum"


def test_v30_protocol_and_source_panel_are_frozen() -> None:
    protocol = load_frozen_protocol(ROOT / "configs/protocol_v3.0.json")
    assert protocol["protocol_hash"] == (
        "724b19290dbb78b4d70c552785cec1649e3e8e3ed4309484f30681bc50f8b0c3"
    )
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


def test_v30_grids_have_the_frozen_sizes() -> None:
    protocol = load_frozen_protocol(ROOT / "configs/protocol_v3.0.json")
    kernel = protocol["character_kernel"]
    supports = enumerate_supports(kernel["lags"], max_degree=kernel["max_degree"])
    assert len(supports) == 63
    assert len(protocol["character_training"]["validation_supports"]) == 18
    assert len(protocol["architectures"]) == 5
    assert protocol["compute"] == {
        "profiles": "Modal CPU",
        "training_gpu": "H100",
        "maximum_concurrent_h100s": 1,
        "character_training_cells": 270,
        "pilot_training_cells": 240,
        "confirmation_training_cells": 480,
        "network_during_remote_cells": False,
    }
