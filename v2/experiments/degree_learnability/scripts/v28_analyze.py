"""Analyze the frozen confirmation phase of the v2.8 sampling repair."""

from __future__ import annotations

import json
import sys
from pathlib import Path

import numpy as np

ROOT = Path(__file__).parent.parent
sys.path.insert(0, str(ROOT))

from dlx.analysis.floor_independent import normalized_learning_time
from dlx.analysis.text_panel import median_cell_metrics
from dlx.protocol.frozen import load_frozen_protocol, verify_hash_lock
from scripts.v27_analyze import _blocked_rank_test, _score, _stratified_rmse_bootstrap

OUT = ROOT / "runs/local/v28_random_windows"


def analyze() -> dict:
    protocol = load_frozen_protocol(ROOT / "configs/protocol_v2.8.json")
    locks = {
        "data_manifest_hash": verify_hash_lock(
            OUT / "data_manifest.json", OUT / "data_manifest.sha256"
        ),
        "profile_manifest_hash": verify_hash_lock(
            OUT / "profile_manifest.json", OUT / "profile_manifest.sha256"
        ),
        "prediction_lock_hash": verify_hash_lock(
            OUT / "predictions.json", OUT / "predictions.sha256"
        ),
    }
    manifest = json.loads((OUT / "data_manifest.json").read_text())
    cells = json.loads((OUT / "confirmation_results.json").read_text())
    sources = [row for row in manifest["corpora"] if row["panel"] == "confirmation"]
    expected = {
        f"V28C/{row['dataset']}/s{seed}"
        for row in sources
        for seed in protocol["training"]["seeds"]
    }
    if len(cells) != 48 or {row["cell_id"] for row in cells} != expected:
        raise ValueError("corrected confirmation grid is incomplete")
    prediction_lock = json.loads((OUT / "predictions.json").read_text())
    predictions = {
        row["dataset"]: row["predictions"] for row in prediction_lock["predictions"]
    }
    rows = []
    threshold = protocol["target"]["maximum_identifiable_final_ce_fraction"]
    for source in sources:
        selected = [row for row in cells if row["dataset"] == source["dataset"]]
        metrics = median_cell_metrics(selected)
        identifiable = metrics["final_ce_fraction"] < threshold
        row = {
            "dataset": source["dataset"],
            "stratum": source["stratum"],
            **metrics,
            "target_identifiable": identifiable,
            "predictions": predictions[source["dataset"]],
        }
        row["normalized_learning_time"] = (
            normalized_learning_time(
                metrics["normalized_curve_area"],
                metrics["final_ce_fraction"],
                maximum_final_fraction=threshold,
            )
            if identifiable
            else None
        )
        rows.append(row)
    unidentified = [row["dataset"] for row in rows if not row["target_identifiable"]]
    if unidentified:
        result = {
            "status": "PAIRED_SAMPLING_REPAIR_NOT_NEW_SOURCE_DISJOINT_CONFIRMATION",
            "protocol_hash": protocol["protocol_hash"],
            **locks,
            "verdict": "TARGET_UNIDENTIFIABLE",
            "unidentifiable_corpora": unidentified,
            "rows": rows,
        }
        (OUT / "analysis.json").write_text(
            json.dumps(result, indent=2, allow_nan=False) + "\n"
        )
        return result
    actual = np.asarray([row["normalized_learning_time"] for row in rows])
    predicted = {
        label: np.asarray([row["predictions"][label] for row in rows])
        for label in protocol["prediction"]["feature_sets"]
    }
    bootstrap = _stratified_rmse_bootstrap(
        rows,
        samples=protocol["decision"]["rmse_bootstrap"]["samples"],
        seed=protocol["decision"]["rmse_bootstrap"]["seed"],
    )
    rank = _blocked_rank_test(
        rows,
        permutations=protocol["decision"]["rank_test"]["permutations"],
        seed=protocol["decision"]["rank_test"]["seed"],
    )
    predictive = (
        bootstrap["relative_rmse_improvement"] > 0
        and bootstrap["stratified_bootstrap_95_interval"][0] > 0
    )
    association = (
        rank["mean_rho"] > 0
        and rank["one_sided_permutation_p"]
        <= protocol["decision"]["rank_test"]["maximum_p"]
    )
    verdict = (
        "CONFIRMED"
        if predictive and association
        else "PREDICTIVE_ONLY"
        if predictive
        else "ASSOCIATION_ONLY"
        if association
        else "NOT_CONFIRMED"
    )
    result = {
        "status": "PAIRED_SAMPLING_REPAIR_NOT_NEW_SOURCE_DISJOINT_CONFIRMATION",
        "protocol_hash": protocol["protocol_hash"],
        **locks,
        "verdict": verdict,
        "primary_gates": {
            "predictive_rmse_gate": predictive,
            "blocked_rank_gate": association,
        },
        "scores": {
            label: _score(actual, values) for label, values in predicted.items()
        },
        "stratified_paired_bootstrap": bootstrap,
        "blocked_rank_test": rank,
        "rows": rows,
    }
    (OUT / "analysis.json").write_text(
        json.dumps(result, indent=2, allow_nan=False) + "\n"
    )
    return result


if __name__ == "__main__":
    result = analyze()
    keys = ["verdict"]
    keys.extend(
        key
        for key in (
            "primary_gates",
            "scores",
            "stratified_paired_bootstrap",
            "blocked_rank_test",
            "unidentifiable_corpora",
        )
        if key in result
    )
    print(json.dumps({key: result[key] for key in keys}, indent=2))
