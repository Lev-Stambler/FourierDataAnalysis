"""Integrity audit for the complete v2.4 predictive-spectrum experiment."""

from __future__ import annotations

import hashlib
import json
from pathlib import Path

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs/local/v24_spectrum_predictor"


def _hash(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def audit() -> dict:
    failures = []
    protocol_path = ROOT / "configs/protocol_v2.4.json"
    protocol = json.loads(protocol_path.read_text())
    recorded = protocol["protocol_hash"]
    unhashed = dict(protocol)
    unhashed.pop("protocol_hash")
    expected = hashlib.sha256(json.dumps(unhashed, sort_keys=True).encode()).hexdigest()
    if recorded != expected:
        failures.append("protocol hash mismatch")

    manifest_path = OUT / "confirmatory_data_manifest.json"
    manifest = json.loads(manifest_path.read_text())
    manifest_hash = hashlib.sha256(
        json.dumps(manifest, sort_keys=True).encode()
    ).hexdigest()
    if len(manifest) != 12:
        failures.append("confirmatory manifest does not contain 12 corpora")
    if manifest_hash != (OUT / "confirmatory_data_manifest.sha256").read_text().strip():
        failures.append("confirmatory manifest hash mismatch")

    counts = {
        "development_text_profiles": len(list((OUT / "text_profiles").glob("*.json"))),
        "confirmatory_text_profiles": len(
            list((OUT / "confirmatory_profiles").glob("*.json"))
        ),
        "image_profiles": len(list((OUT / "image_profiles").glob("*.json"))),
    }
    for key, wanted in (
        ("development_text_profiles", 52),
        ("confirmatory_text_profiles", 30),
        ("image_profiles", 6),
    ):
        if counts[key] != wanted:
            failures.append(f"{key}: {counts[key]} != {wanted}")

    primary_path = OUT / "confirmatory_predictions.json"
    baseline_path = OUT / "confirmatory_baseline_predictions.json"
    lock_hashes = {
        "primary": _hash(primary_path),
        "baseline": _hash(baseline_path),
    }
    if lock_hashes["primary"] != (
        OUT / "confirmatory_predictions.sha256"
    ).read_text().strip():
        failures.append("primary prediction lock mismatch")
    if lock_hashes["baseline"] != (
        OUT / "confirmatory_baseline_predictions.sha256"
    ).read_text().strip():
        failures.append("baseline prediction lock mismatch")
    if json.loads(primary_path.read_text())["n_prediction_rows"] != 90:
        failures.append("primary prediction lock does not contain 90 rows")

    confirm = json.loads((OUT / "confirmatory_remote_results.json").read_text())
    factorial = json.loads((OUT / "factorial_remote_results.json").read_text())
    images = json.loads((OUT / "image_remote_results.json").read_text())
    if len(confirm) != 270:
        failures.append(f"confirmatory H100 cells: {len(confirm)} != 270")
    if len(factorial) != 108:
        failures.append(f"factorial H100 cells: {len(factorial)} != 108")
    if len(images) != 18:
        failures.append(f"image H100 cells: {len(images)} != 18")
    all_cells = confirm + factorial + images
    if any(row["protocol_hash"] != recorded for row in all_cells):
        failures.append("remote cell protocol hash mismatch")
    if any("H100" not in row["remote"]["gpu"] for row in all_cells):
        failures.append("non-H100 training cell found")
    if any(row["prediction_lock_hash"] != lock_hashes["primary"] for row in confirm):
        failures.append("confirmatory cell prediction lock mismatch")
    if any(row["data_manifest_hash"] != manifest_hash for row in confirm):
        failures.append("confirmatory cell data manifest mismatch")

    analyses = {
        name: json.loads((OUT / name).read_text())
        for name in (
            "development_analysis.json",
            "confirmatory_analysis.json",
            "factorial_analysis.json",
            "image_analysis.json",
        )
    }
    if analyses["image_analysis.json"]["old_invalid_m8_curves_reused"]:
        failures.append("invalid historical image curves were reused")
    if not all(
        profile["scope"].startswith("exact within each resolved pair support")
        for profile in (
            json.loads(path.read_text())
            for path in (OUT / "confirmatory_profiles").glob("*.json")
        )
    ):
        failures.append("resolved-surface scope disclosure missing")

    result = {
        "status": "PASS" if not failures else "FAIL",
        "failures": failures,
        "protocol_hash": recorded,
        "data_manifest_hash": manifest_hash,
        "prediction_lock_hashes": lock_hashes,
        "counts": counts,
        "training_cells": {
            "confirmatory": len(confirm),
            "exact_factorial": len(factorial),
            "corrected_image": len(images),
            "total": len(all_cells),
            "maximum_concurrent_h100s": 1,
            "local_training_cells": 0,
        },
        "confirmatory_verdict": analyses["confirmatory_analysis.json"]["verdict"],
    }
    (OUT / "audit.json").write_text(json.dumps(result, indent=2))
    print(f"v2.4 audit {result['status']}")
    if failures:
        for failure in failures:
            print(f"- {failure}")
        raise SystemExit(1)
    return result


if __name__ == "__main__":
    audit()
