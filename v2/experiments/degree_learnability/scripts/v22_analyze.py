"""Analyze the frozen v2.2 hard-domain single-H100 stress test."""

from __future__ import annotations

import hashlib
import json
import math
import sys
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).parent.parent))
sys.path.insert(0, str(Path(__file__).parent))

from v19_analyze import parseval_metrics
from v20_analyze import (
    _median_cell_metrics,
    _rank_correlation,
    _seed_specific_rhos,
    exact_blocked_permutation,
)

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs/local/v22_hard_h100"
PROFILE_DIR = OUT / "profiles"


def load_protocol() -> dict:
    protocol = json.loads((ROOT / "configs/protocol_v2.2.json").read_text())
    recorded = protocol["protocol_hash"]
    unhashed = dict(protocol)
    unhashed.pop("protocol_hash")
    expected = hashlib.sha256(json.dumps(unhashed, sort_keys=True).encode()).hexdigest()
    if recorded != expected:
        raise ValueError(f"protocol hash mismatch: {recorded} != {expected}")
    return protocol


def analyze(protocol: dict) -> dict:
    remote = json.loads((OUT / "remote_results.json").read_text())
    datasets = [entry["id"] for entry in protocol["datasets"]]
    strides = protocol["intervention"]["strides"]
    rows = []
    rows_by_dataset = {}
    for dataset in datasets:
        dataset_rows = []
        for stride in strides:
            profile = json.loads(
                (PROFILE_DIR / f"{dataset}__stride{stride}.json").read_text()
            )
            spectrum = profile["pairs"][0]["conditional_fourier_spectrum"]
            cells = [
                cell
                for cell in remote
                if cell["dataset"] == dataset and cell["stride"] == stride
            ]
            radius = 2 * stride
            row = {
                "dataset": dataset,
                "stride": stride,
                "pair": [stride, radius],
                "radius": radius,
                "log2_radius": math.log2(radius),
                "level_weights": spectrum["level_weights"],
                **parseval_metrics(spectrum["level_weights"], radius),
                **_median_cell_metrics(cells),
            }
            rows.append(row)
            dataset_rows.append(row)
        rows_by_dataset[dataset] = dataset_rows

    exact = exact_blocked_permutation(rows_by_dataset)
    seed_specific = _seed_specific_rhos(
        rows_by_dataset, remote, protocol["replication"]["seeds"]
    )
    final_rhos = exact["rho_by_dataset"]
    area_rhos = {
        dataset: _rank_correlation(
            [row["geometric_moment_total"] for row in dataset_rows],
            [row["normalized_curve_area"] for row in dataset_rows],
        )
        for dataset, dataset_rows in rows_by_dataset.items()
    }
    mean_area_rho = float(np.mean(list(area_rhos.values())))
    directions = {
        dataset: dataset_rows[-1]["final_ce_fraction"]
        - dataset_rows[0]["final_ce_fraction"]
        for dataset, dataset_rows in rows_by_dataset.items()
    }
    positive_directions = sum(value > 0 for value in directions.values())
    all_seed_positive = all(
        row["mean_within_dataset_rho"] > 0 for row in seed_specific.values()
    )
    supported = (
        exact["observed_rho"] >= 0.80
        and exact["exact_two_sided_p"] < 0.05
        and positive_directions == 3
        and all_seed_positive
    )
    if supported:
        verdict = "SUPPORTED"
    elif exact["observed_rho"] <= 0 or positive_directions < 2:
        verdict = "REFUTED"
    else:
        verdict = "INCONCLUSIVE"

    diagnostic = protocol["cleanliness_diagnostic"]
    cleaner = (
        all(value >= 0.80 for value in final_rhos.values())
        and exact["observed_rho"] > diagnostic["v2_1_final_rho"]
        and mean_area_rho > diagnostic["v2_1_area_rho"]
    )
    result = {
        "protocol_hash": protocol["protocol_hash"],
        "verdict": verdict,
        "support_rule": protocol["confirmatory_test"]["support_rule"],
        "primary_exact_blocked_test": exact,
        "stride16_minus_stride1_final_ce_fraction": directions,
        "positive_endpoint_directions": positive_directions,
        "seed_specific_blocked_rhos": seed_specific,
        "all_seed_specific_blocked_rhos_positive": all_seed_positive,
        "secondary_curve_area_rho_by_dataset": area_rhos,
        "secondary_mean_curve_area_rho": mean_area_rho,
        "cleanliness_diagnostic": {
            "cleaner_than_v2_1": cleaner,
            "frozen_rule": diagnostic["cleaner_rule"],
            "v2_1_final_rho": diagnostic["v2_1_final_rho"],
            "v2_2_final_rho": exact["observed_rho"],
            "v2_1_area_rho": diagnostic["v2_1_area_rho"],
            "v2_2_area_rho": mean_area_rho,
            "interpretation": "comparison-only because both domains and learner changed",
        },
        "dataset_stride_rows": rows,
        "spectrum_invariance": {
            dataset: {
                "nonconstant_energy_range": [
                    min(row["nonconstant_energy"] for row in dataset_rows),
                    max(row["nonconstant_energy"] for row in dataset_rows),
                ],
                "nonconstant_degree_range": [
                    min(row["mean_degree_nonconstant"] for row in dataset_rows),
                    max(row["mean_degree_nonconstant"] for row in dataset_rows),
                ],
            }
            for dataset, dataset_rows in rows_by_dataset.items()
        },
        "compute": {
            "remote_h100_training_cells": len(remote),
            "maximum_concurrent_h100s": 1,
            "remote_cpu_profile_cells": len(list(PROFILE_DIR.glob("*.json"))),
            "local_training_cells": 0,
            "local_profile_cells": 0,
            "training_cell_wallclock_seconds": float(
                sum(row["remote"]["wallclock_seconds"] for row in remote)
            ),
            "profile_cell_wallclock_seconds": float(
                sum(
                    json.loads(path.read_text())["remote"]["wallclock_seconds"]
                    for path in PROFILE_DIR.glob("*.json")
                )
            ),
            "gpu_names": sorted({row["remote"]["gpu"] for row in remote}),
        },
        "scope": protocol["scope"],
    }
    (OUT / "integrated_analysis.json").write_text(json.dumps(result, indent=2))
    print(
        json.dumps(
            {
                "verdict": verdict,
                "primary": exact,
                "directions": directions,
                "seed_specific": seed_specific,
                "area_rhos": area_rhos,
                "mean_area_rho": mean_area_rho,
                "cleanliness": result["cleanliness_diagnostic"],
                "compute": result["compute"],
            },
            indent=2,
        )
    )
    return result


if __name__ == "__main__":
    analyze(load_protocol())
