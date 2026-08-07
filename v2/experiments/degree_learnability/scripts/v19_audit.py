"""Fail-closed audit for the v1.9 Parseval scaling and remote stride pilot."""

from __future__ import annotations

import hashlib
import json
import sys
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).parent.parent))
sys.path.insert(0, str(Path(__file__).parent))

from v19_analyze import (
    DATA,
    OUT,
    PROFILE_DIR,
    _sha256_tokens,
    load_protocol,
    parseval_metrics,
    stride_interleave,
)
from v19_prepare_data import META, OUTPUT, sha256_file

ROOT = Path(__file__).parent.parent


def _strict_json(path: Path) -> dict | list:
    return json.loads(
        path.read_text(),
        parse_constant=lambda value: (_ for _ in ()).throw(
            ValueError(f"non-finite JSON constant {value} in {path}")
        ),
    )


def main() -> None:
    protocol = load_protocol()
    result = _strict_json(OUT / "integrated_analysis.json")
    remote = _strict_json(OUT / "remote_results.json")
    metadata = _strict_json(META)
    source_audit = _strict_json(ROOT / "runs/local/v18_real_natural/audit.json")
    failures = []

    if result["protocol_hash"] != protocol["protocol_hash"]:
        failures.append("result protocol hash mismatch")
    source_hash = protocol["source_protocol_hashes"]["real_natural_v1_8"]
    if source_audit["status"] != "PASS" or source_audit["protocol_hash"] != source_hash:
        failures.append("v1.8 source audit/hash mismatch")
    if metadata["output_sha256"] != sha256_file(OUTPUT) or DATA != OUTPUT:
        failures.append("remote payload content hash mismatch")
    if not metadata["divisible_by_all_strides"]:
        failures.append("payload does not divide evenly across frozen strides")

    spec = protocol["stride_pilot"]
    expected_cells = {
        (stride, seed) for stride in spec["strides"] for seed in spec["seeds"]
    }
    observed_cells = {(row["stride"], row["seed"]) for row in remote}
    if observed_cells != expected_cells or len(remote) != len(expected_cells):
        failures.append("remote cell grid is incomplete or duplicated")

    original = np.load(DATA, mmap_mode="r")
    original_counts = np.bincount(original.astype(np.int64), minlength=256)
    count_hash = hashlib.sha256(original_counts.tobytes()).hexdigest()
    for row in remote:
        stride = row["stride"]
        transformed = stride_interleave(original, stride)
        if row["protocol_hash"] != protocol["protocol_hash"]:
            failures.append(f"stride{stride}/s{row['seed']}: protocol mismatch")
        if row["data_sha256"] != _sha256_tokens(transformed):
            failures.append(f"stride{stride}/s{row['seed']}: transformed data mismatch")
        if row["byte_counts_sha256"] != count_hash:
            failures.append(f"stride{stride}/s{row['seed']}: byte multiset mismatch")
        if row["n_bytes"] != len(original):
            failures.append(f"stride{stride}/s{row['seed']}: byte count mismatch")
        if "A10" not in row["remote"]["gpu"]:
            failures.append(f"stride{stride}/s{row['seed']}: expected remote A10 GPU")
        if row["token_grid"][0] != 0 or len(row["token_grid"]) != len(
            row["val_ce_bits"]
        ):
            failures.append(f"stride{stride}/s{row['seed']}: invalid held-out curve")
        if row["initial_val_ce_bits"] is None:
            failures.append(f"stride{stride}/s{row['seed']}: missing zero-step CE")

    analysis_rows = {row["stride"]: row for row in result["stride_pilot"]["rows"]}
    for stride in spec["strides"]:
        profile = _strict_json(PROFILE_DIR / f"stride{stride}.json")
        if profile["protocol_hash"] != protocol["protocol_hash"]:
            failures.append(f"stride{stride}: profile protocol mismatch")
        if not profile["original_byte_counts_preserved"]:
            failures.append(f"stride{stride}: profile byte multiset changed")
        if len(profile["pairs"]) != 10 or any(
            pair["n_positions"] != spec["profile_max_positions"]
            for pair in profile["pairs"]
        ):
            failures.append(f"stride{stride}: incomplete profile")
        expected_pair = [stride, 2 * stride]
        if profile["best_pair_by_function_variance"] != expected_pair:
            failures.append(f"stride{stride}: dependency lags did not move as planted")

        row = analysis_rows[stride]
        if row["best_pair"] != expected_pair:
            failures.append(f"stride{stride}: analysis pair mismatch")
        recomputed = parseval_metrics(row["level_weights"], max(expected_pair))
        for key in (
            "total_square_energy",
            "geometric_moment_absolute",
            "geometric_moment_total",
            "geometric_moment_nonconstant",
            "spectral_entropy_uniform_within_level_upper_bits",
        ):
            if abs(row[key] - recomputed[key]) > 1e-12:
                failures.append(f"stride{stride}: {key} recomputation mismatch")

    g_values = [
        analysis_rows[stride]["geometric_moment_total"] for stride in spec["strides"]
    ]
    if not np.all(np.diff(g_values) > 0):
        failures.append("Parseval geometric moment is not strictly increasing")
    correlations = result["stride_pilot"]["correlations"]["geometric_moment_total"]
    if abs(correlations["vs_curve_area"]["rho"] - 0.8) > 1e-12:
        failures.append("curve-area geometry correlation mismatch")
    if abs(correlations["vs_final_ce_fraction"]["rho"] - 1.0) > 1e-12:
        failures.append("final-CE geometry correlation mismatch")
    if result["compute"]["remote_gpu_cells"] != 8:
        failures.append("remote GPU compute accounting mismatch")
    if result["compute"]["local_training_cells"] != 0:
        failures.append("unexpected local training")

    audit = {
        "status": "PASS" if not failures else "FAIL",
        "protocol_hash": protocol["protocol_hash"],
        "source_protocol_hash": source_hash,
        "failures": failures,
        "remote_gpu_cells": len(remote),
        "local_training_cells": 0,
        "same_byte_multiset_all_cells": not any(
            "multiset" in failure for failure in failures
        ),
        "profile_pairs_per_stride": 10,
        "profile_positions_per_pair": spec["profile_max_positions"],
        "dependency_pairs_recovered": not any(
            "dependency lags" in failure for failure in failures
        ),
        "curve_area_geometry_rho": correlations["vs_curve_area"]["rho"],
        "final_ce_fraction_geometry_rho": correlations["vs_final_ce_fraction"]["rho"],
    }
    (OUT / "audit.json").write_text(json.dumps(audit, indent=2))
    if failures:
        raise SystemExit("v1.9 audit FAIL: " + "; ".join(failures))
    print("v1.9 audit PASS")


if __name__ == "__main__":
    main()
