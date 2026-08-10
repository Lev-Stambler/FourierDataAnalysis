"""Score the locked v3.0 architecture-matching confirmation panel."""

from __future__ import annotations

import json
from pathlib import Path

import numpy as np

from dlx.analysis.architecture_matching import (
    prediction_scores,
    stratified_corpus_bootstrap_improvement,
)
from dlx.analysis.text_panel import median_cell_metrics
from dlx.protocol.frozen import (
    file_sha256,
    load_frozen_protocol,
    verify_hash_lock,
    write_hash_once,
    write_json_once,
)

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs/local/v30_architecture_spectrum"


def main() -> dict:
    protocol = load_frozen_protocol(ROOT / "configs/protocol_v3.0.json")
    locks = {
        name: verify_hash_lock(OUT / f"{name}.json", OUT / f"{name}.sha256")
        for name in (
            "data_manifest",
            "profile_manifest",
            "character_kernel",
            "mechanism_analysis",
            "pilot_analysis",
            "predictions",
        )
    }
    manifest = json.loads((OUT / "data_manifest.json").read_text())
    prediction_artifact = json.loads((OUT / "predictions.json").read_text())
    prediction_map = {
        (row["dataset"], row["architecture"]): row["predictions"]
        for row in prediction_artifact["predictions"]
    }
    sources = [row for row in manifest["corpora"] if row["panel"] == "confirmation"]
    architectures = [row["id"] for row in protocol["architectures"]]
    cells_path = OUT / "confirmation_results.json"
    cells = json.loads(cells_path.read_text())
    expected = {
        f"V30C/{source['dataset']}/{architecture}/s{seed}"
        for source in sources
        for architecture in architectures
        for seed in protocol["natural_training"]["seeds"]
    }
    if len(cells) != len(expected) or {row["cell_id"] for row in cells} != expected:
        raise ValueError(
            f"confirmation grid is incomplete: {len(cells)}/{len(expected)}"
        )
    for cell in cells:
        if cell["prediction_lock_hash"] != locks["predictions"]:
            raise ValueError(f"unlocked prediction in {cell['cell_id']}")
        if cell["protocol_hash"] != protocol["protocol_hash"]:
            raise ValueError(f"protocol mismatch in {cell['cell_id']}")

    target = protocol["natural_training"]["primary_target"]
    rows = []
    for source in sources:
        validation_hashes = {
            row["validation_starts_sha256"]
            for row in cells
            if row["dataset"] == source["dataset"]
        }
        if len(validation_hashes) != 1:
            raise ValueError(
                f"validation chunks are not paired for {source['dataset']}"
            )
        for architecture in architectures:
            selected = [
                row
                for row in cells
                if row["dataset"] == source["dataset"]
                and row["architecture"] == architecture
            ]
            rows.append(
                {
                    "dataset": source["dataset"],
                    "stratum": source["stratum"],
                    "architecture": architecture,
                    **median_cell_metrics(selected),
                    "predictions": prediction_map[(source["dataset"], architecture)],
                }
            )
    actual = np.asarray([row[target] for row in rows], dtype=float)
    baseline = np.asarray(
        [row["predictions"]["strong_baseline"] for row in rows], dtype=float
    )
    matched = np.asarray(
        [row["predictions"]["architecture_matched"] for row in rows], dtype=float
    )
    score_rows = [{**row, "actual": row[target]} for row in rows]
    decision = protocol["decision"]["confirmation_bootstrap"]
    bootstrap = stratified_corpus_bootstrap_improvement(
        score_rows,
        baseline,
        matched,
        samples=int(decision["samples"]),
        seed=int(decision["seed"]),
    )
    per_architecture = {}
    for architecture in architectures:
        indices = np.asarray(
            [
                index
                for index, row in enumerate(rows)
                if row["architecture"] == architecture
            ],
            dtype=int,
        )
        base_score = prediction_scores(actual[indices], baseline[indices])
        matched_score = prediction_scores(actual[indices], matched[indices])
        per_architecture[architecture] = {
            "strong_baseline": base_score,
            "architecture_matched": matched_score,
            "relative_rmse_improvement": (base_score["rmse"] - matched_score["rmse"])
            / base_score["rmse"],
        }
    architectures_improved = sum(
        result["relative_rmse_improvement"] > 0.0
        for result in per_architecture.values()
    )
    interval_passed = bootstrap["stratified_corpus_bootstrap_95_interval"][0] > 0.0
    transfer_passed = interval_passed and architectures_improved >= 3
    mechanism_passed = json.loads((OUT / "mechanism_analysis.json").read_text())[
        "mechanism_gate_passed"
    ]
    if mechanism_passed and transfer_passed:
        verdict = "MECHANISM_SUPPORTED_AND_TRANSFERS"
    elif mechanism_passed:
        verdict = "MECHANISM_SUPPORTED_NO_TRANSFER"
    elif transfer_passed:
        verdict = "PREDICTION_WITHOUT_MECHANISM"
    else:
        verdict = "NOT_SUPPORTED"
    result = {
        "status": "source-disjoint frozen confirmation complete",
        "protocol_hash": protocol["protocol_hash"],
        **{f"{name}_hash": value for name, value in locks.items()},
        "confirmation_results_sha256": file_sha256(cells_path),
        "verdict": verdict,
        "primary_target": target,
        "gates": {
            "mechanism_gate_passed": mechanism_passed,
            "paired_bootstrap_interval_strictly_positive": interval_passed,
            "architectures_with_positive_point_improvement": architectures_improved,
            "transfer_gate_passed": transfer_passed,
        },
        "scores": {
            "strong_baseline": prediction_scores(actual, baseline),
            "architecture_matched": prediction_scores(actual, matched),
        },
        "stratified_paired_corpus_bootstrap": bootstrap,
        "per_architecture": per_architecture,
        "rows": rows,
    }
    digest = write_json_once(OUT / "analysis.json", result)
    write_hash_once(OUT / "analysis.sha256", digest)
    print(
        json.dumps(
            {
                "verdict": verdict,
                "gates": result["gates"],
                "scores": result["scores"],
                "bootstrap": bootstrap,
                "per_architecture": per_architecture,
                "analysis_sha256": digest,
            },
            indent=2,
        )
    )
    return result


if __name__ == "__main__":
    main()
