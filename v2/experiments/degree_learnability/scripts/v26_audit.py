"""Mechanical provenance audit for the frozen v2.6 confirmation."""

from __future__ import annotations

import json
import sys
from pathlib import Path

ROOT = Path(__file__).parent.parent
sys.path.insert(0, str(ROOT))

from dlx.protocol.confirmation import confirmation_cells
from dlx.protocol.frozen import load_frozen_protocol, verify_hash_lock

OUT = ROOT / "runs/local/v26_sampled_locality"


def audit() -> dict:
    protocol = load_frozen_protocol(ROOT / "configs/protocol_v2.6.json")
    locks = {
        "data_manifest": verify_hash_lock(
            OUT / "data_manifest.json", OUT / "data_manifest.sha256"
        ),
        "profile_manifest": verify_hash_lock(
            OUT / "profile_manifest.json", OUT / "profile_manifest.sha256"
        ),
        "predictions": verify_hash_lock(
            OUT / "predictions.json", OUT / "predictions.sha256"
        ),
    }
    manifest = json.loads((OUT / "data_manifest.json").read_text())
    profiles = json.loads((OUT / "profile_manifest.json").read_text())
    prediction_lock = json.loads((OUT / "predictions.json").read_text())
    cells = json.loads((OUT / "remote_results.json").read_text())
    expected = confirmation_cells(protocol)
    expected_ids = {f"V26/{row['dataset']}/s{row['seed']}" for row in expected}
    checks = {
        "protocol_hashes_match": all(
            value["protocol_hash"] == protocol["protocol_hash"]
            for value in (manifest, profiles, prediction_lock)
        )
        and all(row["protocol_hash"] == protocol["protocol_hash"] for row in cells),
        "32_unique_source_hashes": len(
            {row["byte_stream_sha256"] for row in manifest["corpora"]}
        )
        == 32,
        "32_profiles": len(profiles["profiles"]) == 32,
        "32_predictions": len(prediction_lock["predictions"]) == 32,
        "64_exact_cells": len(cells) == 64
        and {row["cell_id"] for row in cells} == expected_ids,
        "prediction_lock_precedes_every_cell": all(
            row["prediction_lock_hash"] == locks["predictions"] for row in cells
        ),
        "single_declared_h100_concurrency": protocol["compute"][
            "maximum_concurrent_h100s"
        ]
        == 1,
    }
    result = {
        "status": "PASS" if all(checks.values()) else "FAIL",
        "protocol_hash": protocol["protocol_hash"],
        "locks": locks,
        "checks": checks,
    }
    (OUT / "audit.json").write_text(json.dumps(result, indent=2) + "\n")
    return result


if __name__ == "__main__":
    print(json.dumps(audit(), indent=2))
