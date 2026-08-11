"""Score the prospectively frozen 48-corpus Fourier expansion."""

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


def _scores(actual: np.ndarray, baseline: np.ndarray, fourier: np.ndarray) -> dict:
    baseline_score = prediction_scores(actual, baseline)
    fourier_score = prediction_scores(actual, fourier)
    return {
        "baseline": baseline_score,
        "plus_fourier_ce_overlap": fourier_score,
        "relative_rmse_improvement": (
            baseline_score["rmse"] - fourier_score["rmse"]
        )
        / baseline_score["rmse"],
    }


def main() -> dict:
    protocol = load_frozen_protocol(ROOT / "configs/protocol_v3.2.json")
    measurement = load_frozen_protocol(ROOT / "configs/protocol_v3.1.json")
    prediction_hash = verify_hash_lock(
        OUT / "expansion_predictions.json", OUT / "expansion_predictions.sha256"
    )
    predictions = json.loads((OUT / "expansion_predictions.json").read_text())
    if predictions["protocol_hash"] != protocol["protocol_hash"]:
        raise ValueError("expansion predictions use the wrong analysis protocol")
    if predictions["measurement_protocol_hash"] != measurement["protocol_hash"]:
        raise ValueError("expansion predictions use the wrong measurement protocol")
    prediction_map = {
        (row["dataset"], row["architecture"]): row["predictions"]
        for row in predictions["predictions"]
    }

    manifest = json.loads((OUT / "data_manifest.json").read_text())
    sources = [row for row in manifest["corpora"] if row["panel"] == "confirmation"]
    architectures = [row["id"] for row in measurement["architectures"]]
    seeds = measurement["natural_training"]["seeds"]
    cells_path = OUT / "expansion_results.json"
    cells = json.loads(cells_path.read_text())
    expected = {
        f"V32E/{source['dataset']}/{architecture}/s{seed}"
        for source in sources
        for architecture in architectures
        for seed in seeds
    }
    observed = {row["cell_id"] for row in cells}
    if len(cells) != len(expected) or observed != expected:
        raise ValueError(f"expansion grid is incomplete: {len(cells)}/{len(expected)}")
    for cell in cells:
        if cell["prediction_lock_hash"] != prediction_hash:
            raise ValueError(f"unlocked prediction in {cell['cell_id']}")
        if cell["analysis_protocol_hash"] != protocol["protocol_hash"]:
            raise ValueError(f"analysis protocol mismatch in {cell['cell_id']}")
        if cell["protocol_hash"] != measurement["protocol_hash"]:
            raise ValueError(f"measurement protocol mismatch in {cell['cell_id']}")

    target = protocol["primary_comparison"]["target"]
    rows = []
    for source in sources:
        dataset_cells = [
            row for row in cells if row["dataset"] == source["dataset"]
        ]
        validation_hashes = {
            row["validation_starts_sha256"] for row in dataset_cells
        }
        if len(validation_hashes) != 1:
            raise ValueError(f"validation chunks are not paired for {source['dataset']}")
        for architecture in architectures:
            selected = [
                row
                for row in dataset_cells
                if row["architecture"] == architecture
            ]
            key = (source["dataset"], architecture)
            if key not in prediction_map:
                raise ValueError(f"missing frozen prediction for {key}")
            rows.append(
                {
                    "dataset": source["dataset"],
                    "stratum": source["stratum"],
                    "architecture": architecture,
                    **median_cell_metrics(selected),
                    "predictions": prediction_map[key],
                }
            )

    actual = np.asarray([row[target] for row in rows], dtype=float)
    comparison_specs = {
        "primary_ordinary_controls": (
            "ordinary_baseline",
            "ordinary_plus_fourier",
        ),
        "secondary_strong_controls": (
            "strong_baseline",
            "strong_plus_fourier",
        ),
    }
    bootstrap_rows = [
        {
            "dataset": row["dataset"],
            "stratum": row["stratum"],
            "actual": row[target],
        }
        for row in rows
    ]
    comparisons = {}
    for label, (baseline_name, fourier_name) in comparison_specs.items():
        baseline = np.asarray(
            [row["predictions"][baseline_name] for row in rows], dtype=float
        )
        fourier = np.asarray(
            [row["predictions"][fourier_name] for row in rows], dtype=float
        )
        bootstrap = stratified_corpus_bootstrap_improvement(
            bootstrap_rows,
            baseline,
            fourier,
            samples=int(protocol["decision"]["bootstrap_samples"]),
            seed=int(protocol["decision"]["bootstrap_seed"]),
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
            per_architecture[architecture] = _scores(
                actual[indices], baseline[indices], fourier[indices]
            )
        comparisons[label] = {
            **_scores(actual, baseline, fourier),
            "stratified_paired_corpus_bootstrap": bootstrap,
            "per_architecture": per_architecture,
        }

    primary = comparisons["primary_ordinary_controls"]
    interval = primary["stratified_paired_corpus_bootstrap"][
        "stratified_corpus_bootstrap_95_interval"
    ]
    passed = interval[0] > 0.0
    verdict = (
        "EXPANDED_FOURIER_SIGNAL_REPLICATES"
        if passed
        else "EXPANDED_FOURIER_SIGNAL_DOES_NOT_REPLICATE"
    )
    result = {
        "status": "prospective source-disjoint 48-corpus expansion complete",
        "protocol_hash": protocol["protocol_hash"],
        "measurement_protocol_hash": measurement["protocol_hash"],
        "prediction_lock_hash": prediction_hash,
        "expansion_results_sha256": file_sha256(cells_path),
        "verdict": verdict,
        "primary_target": target,
        "primary_gate": {
            "criterion": protocol["decision"]["success"],
            "interval_strictly_positive": passed,
            "interval": interval,
        },
        "comparisons": comparisons,
        "rows": rows,
    }
    digest = write_json_once(OUT / "expansion_analysis.json", result)
    write_hash_once(OUT / "expansion_analysis.sha256", digest)
    print(
        json.dumps(
            {
                "verdict": verdict,
                "primary_gate": result["primary_gate"],
                "comparisons": comparisons,
                "analysis_sha256": digest,
            },
            indent=2,
        )
    )
    return result


if __name__ == "__main__":
    main()
