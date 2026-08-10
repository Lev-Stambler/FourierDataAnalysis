"""Mechanical provenance audit for the frozen v2.7 confirmation."""

from __future__ import annotations

import json
import sys
from pathlib import Path

ROOT = Path(__file__).parent.parent
sys.path.insert(0, str(ROOT))

from dlx.protocol.confirmation import confirmation_cells
from dlx.protocol.frozen import (
    file_sha256,
    load_frozen_protocol,
    verify_hash_lock,
)

OUT = ROOT / "runs/local/v27_marginal_locality"
PROTOCOL_PATH = ROOT / "configs/protocol_v2.7.json"


def audit() -> dict:
    protocol = load_frozen_protocol(PROTOCOL_PATH)
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
    development_path = ROOT / protocol["development"]["manifest"]
    source_registry_path = ROOT / protocol["corpora"]["source_registry"]
    feasibility_path = ROOT / protocol["corpora"]["source_feasibility"]
    manifest = json.loads((OUT / "data_manifest.json").read_text())
    profiles = json.loads((OUT / "profile_manifest.json").read_text())
    prediction_lock = json.loads((OUT / "predictions.json").read_text())
    cells = json.loads((OUT / "remote_results.json").read_text())
    expected = confirmation_cells(protocol)
    expected_ids = {f"V27/{row['dataset']}/s{row['seed']}" for row in expected}
    data_by_id = {row["id"]: row for row in manifest["corpora"]}
    profile_by_id = {row["dataset"]: row for row in profiles["profiles"]}
    expected_datasets = {source["id"] for source in protocol["corpora"]["sources"]}

    profile_files_match = all(
        file_sha256(OUT / "profiles" / f"{dataset}.json")
        == profile_by_id[dataset]["profile_sha256"]
        and file_sha256(OUT / "audit_chains" / f"{dataset}.json.gz")
        == profile_by_id[dataset]["audit_sha256"]
        for dataset in expected_datasets
    )
    cell_hashes_match = all(
        row["protocol_hash"] == protocol["protocol_hash"]
        and row["data_manifest_hash"] == locks["data_manifest"]
        and row["profile_manifest_hash"] == locks["profile_manifest"]
        and row["prediction_lock_hash"] == locks["predictions"]
        and row["data_sha256"] == data_by_id[row["dataset"]]["byte_stream_sha256"]
        for row in cells
    )
    checks = {
        "development_manifest_matches_frozen_hash": file_sha256(development_path)
        == protocol["development"]["manifest_sha256"],
        "source_registry_matches_frozen_hash": file_sha256(source_registry_path)
        == protocol["corpora"]["source_registry_sha256"],
        "source_feasibility_matches_frozen_hash": file_sha256(feasibility_path)
        == protocol["corpora"]["source_feasibility_sha256"],
        "all_artifact_protocol_hashes_match": all(
            value["protocol_hash"] == protocol["protocol_hash"]
            for value in (manifest, profiles, prediction_lock)
        ),
        "24_unique_byte_streams": len(manifest["corpora"]) == 24
        and len({row["byte_stream_sha256"] for row in manifest["corpora"]}) == 24,
        "24_exact_profiles": set(profile_by_id) == expected_datasets,
        "profile_and_audit_files_match_manifest": profile_files_match,
        "24_exact_predictions": {
            row["dataset"] for row in prediction_lock["predictions"]
        }
        == expected_datasets
        and len(prediction_lock["predictions"]) == 24,
        "48_exact_cells": len(cells) == 48
        and {row["cell_id"] for row in cells} == expected_ids,
        "every_cell_matches_pretraining_locks": cell_hashes_match,
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
