"""Mechanical audit for the v2.8 random-window repair."""

from __future__ import annotations

import json
import sys
from pathlib import Path

ROOT = Path(__file__).parent.parent
sys.path.insert(0, str(ROOT))

from dlx.protocol.frozen import file_sha256, load_frozen_protocol, verify_hash_lock

OUT = ROOT / "runs/local/v28_random_windows"


def audit() -> dict:
    protocol = load_frozen_protocol(ROOT / "configs/protocol_v2.8.json")
    locks = {
        name: verify_hash_lock(OUT / f"{name}.json", OUT / f"{name}.sha256")
        for name in ("data_manifest", "profile_manifest", "predictions")
    }
    manifest = json.loads((OUT / "data_manifest.json").read_text())
    profiles = json.loads((OUT / "profile_manifest.json").read_text())
    development = json.loads((OUT / "development_results.json").read_text())
    confirmation = json.loads((OUT / "confirmation_results.json").read_text())
    predictions = json.loads((OUT / "predictions.json").read_text())
    profile_rows = {row["dataset"]: row for row in profiles["profiles"]}
    datasets = {row["dataset"] for row in manifest["corpora"]}
    manifest_rows = {row["dataset"]: row for row in manifest["corpora"]}
    profile_contents = {
        dataset: json.loads((OUT / "profiles" / f"{dataset}.json").read_text())
        for dataset in datasets
    }
    expected_development = {
        f"V28D/{row['dataset']}/s{seed}"
        for row in manifest["corpora"]
        if row["panel"] == "development"
        for seed in protocol["training"]["seeds"]
    }
    expected_confirmation = {
        f"V28C/{row['dataset']}/s{seed}"
        for row in manifest["corpora"]
        if row["panel"] == "confirmation"
        for seed in protocol["training"]["seeds"]
    }
    checks = {
        "frozen_manifest_matches_protocol": locks["data_manifest"]
        == protocol["data"]["manifest_sha256"],
        "78_unique_corpora": len(datasets) == len(manifest["corpora"]) == 78,
        "source_files_match": all(
            file_sha256(ROOT / row["path"]) == row["file_sha256"]
            for row in manifest["corpora"]
        ),
        "78_exact_profiles": set(profile_rows) == datasets,
        "profile_files_match": all(
            file_sha256(OUT / "profiles" / f"{dataset}.json")
            == profile_rows[dataset]["profile_sha256"]
            for dataset in datasets
        ),
        "profile_content_locks_match": all(
            profile_contents[dataset]["protocol_hash"] == protocol["protocol_hash"]
            and profile_contents[dataset]["data_sha256"]
            == manifest_rows[dataset]["byte_stream_sha256"]
            for dataset in datasets
        ),
        "profile_audits_match": all(
            file_sha256(OUT / "audit_chains" / f"{dataset}.json.gz")
            == profile_rows[dataset]["audit_sha256"]
            for dataset in datasets
        ),
        "108_development_cells": len(development) == 108
        and {row["cell_id"] for row in development} == expected_development,
        "24_predictions": len(predictions["predictions"]) == 24
        and {row["dataset"] for row in predictions["predictions"]}
        == {
            row["dataset"]
            for row in manifest["corpora"]
            if row["panel"] == "confirmation"
        },
        "48_confirmation_cells": len(confirmation) == 48
        and {row["cell_id"] for row in confirmation} == expected_confirmation,
        "confirmation_uses_frozen_predictions": all(
            row["prediction_lock_hash"] == locks["predictions"] for row in confirmation
        ),
        "all_training_locks_match": all(
            row["protocol_hash"] == protocol["protocol_hash"]
            and row["data_manifest_hash"] == locks["data_manifest"]
            and row["profile_manifest_hash"] == locks["profile_manifest"]
            and row["data_sha256"]
            == manifest_rows[row["dataset"]]["byte_stream_sha256"]
            for row in development + confirmation
        ),
        "fixed_validation_within_corpus": all(
            len(
                {
                    row["validation_starts_sha256"]
                    for row in development + confirmation
                    if row["dataset"] == dataset
                }
            )
            == 1
            for dataset in datasets
        ),
        "single_h100_concurrency": protocol["compute"]["maximum_concurrent_h100s"] == 1,
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
