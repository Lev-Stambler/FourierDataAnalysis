"""Analyze the frozen v2.3 Transformer-configuration robustness test."""

from __future__ import annotations

import hashlib
import itertools
import json
import math
import sys
from collections import Counter
from pathlib import Path

import numpy as np
from scipy.stats import rankdata

sys.path.insert(0, str(Path(__file__).parent.parent))
sys.path.insert(0, str(Path(__file__).parent))

from v20_analyze import _median_cell_metrics, _rank_correlation

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs/local/v23_transformer_robustness"
SOURCE = ROOT / "runs/local/v21_predictor_selection"
BASELINE = "learned_absolute_d64_l2"


def load_protocol() -> dict:
    protocol = json.loads((ROOT / "configs/protocol_v2.3.json").read_text())
    recorded = protocol["protocol_hash"]
    unhashed = dict(protocol)
    unhashed.pop("protocol_hash")
    expected = hashlib.sha256(json.dumps(unhashed, sort_keys=True).encode()).hexdigest()
    if recorded != expected:
        raise ValueError(f"protocol hash mismatch: {recorded} != {expected}")
    return protocol


def load_cells(protocol: dict) -> list[dict]:
    new_cells = json.loads((OUT / "remote_results.json").read_text())
    source_cells = json.loads((SOURCE / "remote_results.json").read_text())
    strides = set(protocol["intervention"]["strides"])
    baseline = []
    for row in source_cells:
        if row["stride"] not in strides:
            continue
        copied = dict(row)
        copied.update(
            {
                "configuration": BASELINE,
                "position_encoding": "learned_absolute",
                "d_model": 64,
                "n_layers": 2,
                "n_heads": 4,
                "source": "reused v2.1",
            }
        )
        baseline.append(copied)
    return baseline + new_cells


def exact_blocked_spearman(
    rows_by_dataset: dict[str, list[dict]], outcome: str
) -> dict:
    """Exact two-sided blocked Spearman test via distance convolution."""

    n = len(next(iter(rows_by_dataset.values())))
    if any(len(rows) != n for rows in rows_by_dataset.values()):
        raise ValueError("every block must contain the same number of rows")
    denominator = n * (n * n - 1) // 6
    observed_by_dataset = {}
    observed_distance_sum = 0
    for dataset, rows in rows_by_dataset.items():
        x_rank = rankdata([row["log2_radius"] for row in rows])
        y_rank = rankdata([row[outcome] for row in rows])
        if len(np.unique(x_rank)) != n or len(np.unique(y_rank)) != n:
            raise ValueError(f"{dataset}: exact Spearman test does not permit ties")
        distance = int(np.sum((x_rank - y_rank) ** 2))
        observed_distance_sum += distance
        observed_by_dataset[dataset] = 1.0 - distance / denominator

    one_block = Counter()
    reference = np.arange(1, n + 1)
    for permutation in itertools.permutations(range(1, n + 1)):
        distance = int(np.sum((reference - permutation) ** 2))
        one_block[distance] += 1
    convolved = Counter({0: 1})
    for _ in rows_by_dataset:
        updated = Counter()
        for left, left_count in convolved.items():
            for right, right_count in one_block.items():
                updated[left + right] += left_count * right_count
        convolved = updated

    blocks = len(rows_by_dataset)
    centered_observed = abs(denominator * blocks - observed_distance_sum)
    extreme = sum(
        count
        for distance, count in convolved.items()
        if abs(denominator * blocks - distance) >= centered_observed
    )
    total = math.factorial(n) ** blocks
    if sum(convolved.values()) != total:
        raise AssertionError("exact permutation convolution lost probability mass")
    return {
        "statistic": f"mean within-corpus Spearman rho for {outcome}",
        "observed_rho": float(np.mean(list(observed_by_dataset.values()))),
        "rho_by_dataset": observed_by_dataset,
        "exact_two_sided_p": extreme / total,
        "extreme_permutations": str(extreme),
        "total_permutations": str(total),
        "method": f"exact convolution of {blocks} independent {n}! rank permutations",
    }


def _seed_specific(
    datasets: list[str], strides: list[int], cells: list[dict], seeds: list[int]
) -> dict:
    output = {}
    locality = [math.log2(2 * stride) for stride in strides]
    for seed in seeds:
        by_dataset = {}
        for dataset in datasets:
            ordered = [
                next(
                    row
                    for row in cells
                    if row["dataset"] == dataset
                    and row["stride"] == stride
                    and row["seed"] == seed
                )
                for stride in strides
            ]
            difficulty = [
                row["floor_independent"]["final_ce_bits"]
                / row["floor_independent"]["initial_ce_bits"]
                for row in ordered
            ]
            by_dataset[dataset] = _rank_correlation(locality, difficulty)
        output[str(seed)] = {
            "mean_within_dataset_rho": float(np.mean(list(by_dataset.values()))),
            "rho_by_dataset": by_dataset,
        }
    return output


def analyze(protocol: dict) -> dict:
    all_cells = load_cells(protocol)
    datasets = [row["id"] for row in protocol["datasets"]]
    strides = protocol["intervention"]["strides"]
    seeds = protocol["replication"]["seeds"]
    by_configuration = {}
    all_rows = []
    for configuration in protocol["configurations"]:
        config_id = configuration["id"]
        cells = [row for row in all_cells if row["configuration"] == config_id]
        rows_by_dataset = {}
        for dataset in datasets:
            dataset_rows = []
            for stride in strides:
                selected = [
                    row
                    for row in cells
                    if row["dataset"] == dataset and row["stride"] == stride
                ]
                row = {
                    "configuration": config_id,
                    "dataset": dataset,
                    "stride": stride,
                    "radius": 2 * stride,
                    "log2_radius": math.log2(2 * stride),
                    **_median_cell_metrics(selected),
                }
                dataset_rows.append(row)
                all_rows.append(row)
            rows_by_dataset[dataset] = dataset_rows

        final_test = exact_blocked_spearman(rows_by_dataset, "final_ce_fraction")
        area_test = exact_blocked_spearman(rows_by_dataset, "normalized_curve_area")
        endpoints = {
            dataset: rows[-1]["final_ce_fraction"] - rows[0]["final_ce_fraction"]
            for dataset, rows in rows_by_dataset.items()
        }
        seed_specific = _seed_specific(datasets, strides, cells, seeds)
        positive_endpoints = sum(value > 0 for value in endpoints.values())
        all_seed_positive = all(
            row["mean_within_dataset_rho"] > 0 for row in seed_specific.values()
        )
        supported = (
            final_test["observed_rho"] >= 0.60
            and final_test["exact_two_sided_p"] < 0.05
            and positive_endpoints >= 4
            and all_seed_positive
        )
        by_configuration[config_id] = {
            "supported": supported,
            "configuration": configuration,
            "primary_final_ce_test": final_test,
            "secondary_curve_area_test": area_test,
            "stride16_minus_stride1_final_ce_fraction": endpoints,
            "positive_endpoint_directions": positive_endpoints,
            "seed_specific_rhos": seed_specific,
            "all_seed_specific_rhos_positive": all_seed_positive,
            "n_params": sorted({row.get("n_params") for row in cells if row.get("n_params")}),
        }

    supported_count = sum(row["supported"] for row in by_configuration.values())
    all_positive = all(
        row["primary_final_ce_test"]["observed_rho"] > 0
        for row in by_configuration.values()
    )
    if supported_count == len(by_configuration):
        verdict = "SUPPORTED"
    elif supported_count >= 2 and all_positive:
        verdict = "MIXED"
    else:
        verdict = "REFUTED"
    new_cells = [
        row for row in all_cells if row.get("source") != "reused v2.1"
    ]
    result = {
        "protocol_hash": protocol["protocol_hash"],
        "verdict": verdict,
        "frozen_rules": protocol["test"],
        "supported_configurations": supported_count,
        "total_configurations": len(by_configuration),
        "configuration_results": by_configuration,
        "configuration_dataset_stride_rows": all_rows,
        "compute": {
            "reused_v2_1_cells": len(all_cells) - len(new_cells),
            "new_remote_h100_cells": len(new_cells),
            "maximum_concurrent_h100s": 1,
            "new_h100_aggregate_cell_seconds": float(
                sum(row["remote"]["wallclock_seconds"] for row in new_cells)
            ),
            "local_training_cells": 0,
            "gpu_names": sorted({row["remote"]["gpu"] for row in new_cells}),
        },
        "scope": protocol["scope"],
    }
    OUT.mkdir(parents=True, exist_ok=True)
    (OUT / "integrated_analysis.json").write_text(json.dumps(result, indent=2))
    print(
        json.dumps(
            {
                "verdict": verdict,
                "configurations": {
                    key: {
                        "supported": value["supported"],
                        "rho": value["primary_final_ce_test"]["observed_rho"],
                        "p": value["primary_final_ce_test"]["exact_two_sided_p"],
                        "endpoints": value["positive_endpoint_directions"],
                        "area_rho": value["secondary_curve_area_test"]["observed_rho"],
                    }
                    for key, value in by_configuration.items()
                },
                "compute": result["compute"],
            },
            indent=2,
        )
    )
    return result


if __name__ == "__main__":
    analyze(load_protocol())
