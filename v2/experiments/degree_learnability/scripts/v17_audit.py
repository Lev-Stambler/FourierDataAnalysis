"""Fail-closed audit for protocol v1.7 geometry-aware spectrum analysis."""

from __future__ import annotations

import json
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent))
sys.path.insert(0, str(Path(__file__).parent))

from v17_local_geometry_analysis import (
    H5_SOURCE,
    M6_TABLE,
    OUT,
    TEXT_SOURCE,
    _sha256,
    load_protocol,
)

from dlx.analysis.local_geometry import local_ball_cardinalities


def main() -> None:
    protocol = load_protocol()
    result = json.loads((OUT / "integrated_analysis.json").read_text())
    text_audit = json.loads((TEXT_SOURCE / "audit.json").read_text())
    h5_audit = json.loads((H5_SOURCE / "audit.json").read_text())
    failures = []

    if result["protocol_hash"] != protocol["protocol_hash"]:
        failures.append("result protocol hash mismatch")
    if text_audit["status"] != "PASS":
        failures.append("source v1.6 audit is not PASS")
    if h5_audit["status"] != "PASS":
        failures.append("source matched-H5 audit is not PASS")
    sources = protocol["source_protocol_hashes"]
    if text_audit["protocol_hash"] != sources["conditional_spectrum_v1_6"]:
        failures.append("source v1.6 protocol hash mismatch")
    if h5_audit["protocol_hash"] != sources["matched_h5_v1_3"]:
        failures.append("source matched-H5 protocol hash mismatch")

    text_path = TEXT_SOURCE / "integrated_analysis.json"
    text_source = json.loads(text_path.read_text())
    if result["text"]["input_sha256"] != _sha256(text_path):
        failures.append("text input content hash mismatch")
    h5_spectra = H5_SOURCE / "part_a" / "part_a_results.json"
    h5_curves = H5_SOURCE / "integrated_analysis.json"
    if result["matched_real_tabular"]["input_sha256"] != {
        "spectra": _sha256(h5_spectra),
        "curves": _sha256(h5_curves),
    }:
        failures.append("tabular input content hash mismatch")
    if result["m6_locality_controls"]["input_sha256"] != _sha256(M6_TABLE):
        failures.append("M6 input content hash mismatch")

    text_rows = result["text"]["rungs"]
    if len(text_rows) != 4:
        failures.append("expected four text rungs")
    if local_ball_cardinalities(256, 2, 2) != [1, 510, 65025]:
        failures.append("categorical pair cardinality identity failed")
    for row in text_rows:
        low, high = row["search_complexity_bits_interval"]
        if not (0 < low <= high):
            failures.append(f"{row['rung']}: invalid geometry interval")
        source_weights = next(
            source_row
            for source_row in text_source["rungs"]
            if source_row["rung"] == row["rung"]
        )["spectrum"]["level_weights"]
        if len(row["level_weights"]) != len(source_weights) or any(
            abs(left - right) > 1e-12
            for left, right in zip(row["level_weights"], source_weights)
        ):
            failures.append(f"{row['rung']}: text spectrum changed")

    tabular = result["matched_real_tabular"]
    if tabular["n_real_datasets"] != 9:
        failures.append("expected nine real tabular datasets")
    expected_features = {"f4_n500": 4, "f6_n500": 6, "f9_n500": 9}
    for row in tabular["rows"]:
        if row["n_rows"] != 500:
            failures.append(f"{row['dataset']}: row matching failed")
        if row["n_features"] != expected_features[row["band"]]:
            failures.append(f"{row['dataset']}: feature matching failed")
        if len(row["level_cardinalities"]) != row["n_features"] + 1:
            failures.append(f"{row['dataset']}: incomplete level cardinalities")
        if abs(sum(row["level_weights"]) - 1.0) > 1e-9:
            failures.append(
                f"{row['dataset']}: normalized spectrum does not sum to one"
            )

    controls = result["m6_locality_controls"]
    if len(controls["rows"]) != 8:
        failures.append("expected eight M6 locality-control families")
    if not controls["common_bayes_floor"]:
        failures.append("M6 locality controls do not share a Bayes floor")
    if any(row["n_seeds"] != 3 for row in controls["rows"]):
        failures.append("M6 locality controls are not three-seed complete")
    for row in controls["rows"]:
        for seed in range(3):
            manifest_path = (
                M6_TABLE.parent / f"M6__{row['family']}__s{seed}" / "manifest.json"
            )
            manifest = json.loads(manifest_path.read_text())
            if manifest["protocol_hash"] != sources["m6_v1"]:
                failures.append(
                    f"{row['family']}/s{seed}: M6 source protocol hash mismatch"
                )
    if not controls["locality_contrasts"]["ordering_by_geometry_and_final_gap"]:
        failures.append("reported exact M6 geometry ordering is false")
    geometry_rho = controls["correlations"]["geometric_complexity_vs_final_gap"]["rho"]
    if abs(geometry_rho - 1.0) > 1e-12:
        failures.append("M6 geometry correlation is not exactly one")

    if result["real_dataset_count"] != 11:
        failures.append("expected eleven real datasets total")
    if result["training_or_profile_recomputation"] != 0:
        failures.append("unexpected training or profile recomputation")

    audit = {
        "status": "PASS" if not failures else "FAIL",
        "protocol_hash": protocol["protocol_hash"],
        "source_protocol_hashes": sources,
        "failures": failures,
        "text_rungs": len(text_rows),
        "real_tabular_datasets": tabular["n_real_datasets"],
        "real_datasets_total": result["real_dataset_count"],
        "m6_matched_control_families": len(controls["rows"]),
        "training_cells_recomputed": 0,
        "profiles_recomputed": 0,
        "image_curves_misused_as_spectra": 0,
    }
    (OUT / "audit.json").write_text(json.dumps(audit, indent=2))
    if failures:
        raise SystemExit("v1.7 audit FAIL: " + "; ".join(failures))
    print("v1.7 audit PASS")


if __name__ == "__main__":
    main()
