"""Analyze the frozen v2.1 unseen-corpus predictor-selection experiment."""

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

from v19_analyze import parseval_metrics

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs/local/v21_predictor_selection"
PROFILE_DIR = OUT / "profiles"

PREDICTORS = (
    "mean_degree_total",
    "mean_degree_nonconstant",
    "log2_radius",
    "geometric_moment_absolute",
    "geometric_moment_nonconstant",
    "geometric_moment_total",
    "spectral_entropy_uniform_within_level_upper_bits",
)
PRIMARY = "geometric_moment_total"


def load_protocol() -> dict:
    protocol = json.loads((ROOT / "configs/protocol_v2.1.json").read_text())
    recorded = protocol["protocol_hash"]
    unhashed = dict(protocol)
    unhashed.pop("protocol_hash")
    expected = hashlib.sha256(json.dumps(unhashed, sort_keys=True).encode()).hexdigest()
    if recorded != expected:
        raise ValueError(f"protocol hash mismatch: {recorded} != {expected}")
    return protocol


def _rank_correlation(x: list[float], y: list[float]) -> float:
    return float(np.corrcoef(rankdata(x), rankdata(y))[0, 1])


def _median_cell_metrics(cells: list[dict]) -> dict:
    summaries = [row["floor_independent"] for row in cells]
    keys = (
        "initial_ce_bits",
        "final_ce_bits",
        "learning_amount_bits",
        "fractional_learning",
        "normalized_curve_area",
    )
    values = {
        key: float(np.median([summary[key] for summary in summaries])) for key in keys
    }
    values["final_ce_fraction"] = float(
        np.median(
            [
                summary["final_ce_bits"] / summary["initial_ce_bits"]
                for summary in summaries
            ]
        )
    )
    return values


def exact_blocked_spearman(rows_by_dataset: dict[str, list[dict]]) -> dict:
    """Exact two-sided null by convolution of n=8 permutation distributions."""
    n = len(next(iter(rows_by_dataset.values())))
    if n != 8 or any(len(rows) != n for rows in rows_by_dataset.values()):
        raise ValueError("frozen exact test requires eight rows per dataset")
    denominator = n * (n * n - 1) // 6
    observed_by_dataset = {}
    observed_distance_sum = 0
    for dataset, rows in rows_by_dataset.items():
        x_rank = rankdata([row[PRIMARY] for row in rows])
        y_rank = rankdata([row["final_ce_fraction"] for row in rows])
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
    observed = float(np.mean(list(observed_by_dataset.values())))
    return {
        "statistic": "mean within-dataset Spearman rho",
        "observed_rho": observed,
        "rho_by_dataset": observed_by_dataset,
        "exact_two_sided_p": extreme / total,
        "extreme_permutations": str(extreme),
        "total_permutations": str(total),
        "method": "exact convolution of six 8! Spearman rank-permutation distributions",
    }


def _seed_specific_rhos(
    rows_by_dataset: dict[str, list[dict]], remote: list[dict], seeds: list[int]
) -> dict:
    output = {}
    for seed in seeds:
        by_dataset = {}
        for dataset, rows in rows_by_dataset.items():
            cells = sorted(
                (
                    cell
                    for cell in remote
                    if cell["dataset"] == dataset and cell["seed"] == seed
                ),
                key=lambda cell: cell["stride"],
            )
            by_dataset[dataset] = _rank_correlation(
                [row[PRIMARY] for row in rows],
                [
                    cell["floor_independent"]["final_ce_bits"]
                    / cell["floor_independent"]["initial_ce_bits"]
                    for cell in cells
                ],
            )
        output[str(seed)] = {
            "mean_within_dataset_rho": float(np.mean(list(by_dataset.values()))),
            "rho_by_dataset": by_dataset,
        }
    return output


def _loco_scores(
    rows_by_dataset: dict[str, list[dict]], outcome: str
) -> dict[str, dict]:
    scores = {}
    for predictor in PREDICTORS:
        per_dataset = {}
        squared_errors = []
        for held_out, held_rows in rows_by_dataset.items():
            train_x, train_y = [], []
            for dataset, rows in rows_by_dataset.items():
                if dataset == held_out:
                    continue
                x0, y0 = rows[0][predictor], rows[0][outcome]
                train_x.extend(row[predictor] - x0 for row in rows[1:])
                train_y.extend(row[outcome] - y0 for row in rows[1:])
            train_x_array = np.asarray(train_x)
            train_y_array = np.asarray(train_y)
            denominator = float(np.dot(train_x_array, train_x_array))
            slope = (
                float(np.dot(train_x_array, train_y_array) / denominator)
                if denominator > 0
                else 0.0
            )
            x0, y0 = held_rows[0][predictor], held_rows[0][outcome]
            held_x = np.asarray([row[predictor] - x0 for row in held_rows[1:]])
            held_y = np.asarray([row[outcome] - y0 for row in held_rows[1:]])
            errors = held_y - slope * held_x
            squared_errors.extend((errors**2).tolist())
            per_dataset[held_out] = {
                "fitted_slope": slope,
                "rmse": float(np.sqrt(np.mean(errors**2))),
                "mae": float(np.mean(np.abs(errors))),
            }
        scores[predictor] = {
            "loco_rmse": float(np.sqrt(np.mean(squared_errors))),
            "loco_mae": float(
                np.mean(
                    [
                        row["mae"]
                        for row in per_dataset.values()
                    ]
                )
            ),
            "per_held_out_dataset": per_dataset,
        }
    return scores


def _centered_predictor_correlations(
    rows_by_dataset: dict[str, list[dict]],
) -> dict[str, float]:
    deltas = {}
    for predictor in PREDICTORS:
        deltas[predictor] = np.asarray(
            [
                row[predictor] - rows[0][predictor]
                for rows in rows_by_dataset.values()
                for row in rows[1:]
            ]
        )
    return {
        predictor: float(np.corrcoef(deltas[PRIMARY], deltas[predictor])[0, 1])
        for predictor in PREDICTORS
        if predictor != PRIMARY
    }


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
            dataset_rows.append(row)
            rows.append(row)
        rows_by_dataset[dataset] = dataset_rows

    exact = exact_blocked_spearman(rows_by_dataset)
    seed_specific = _seed_specific_rhos(
        rows_by_dataset, remote, protocol["replication"]["seeds"]
    )
    directions = {
        dataset: dataset_rows[-1]["final_ce_fraction"]
        - dataset_rows[0]["final_ce_fraction"]
        for dataset, dataset_rows in rows_by_dataset.items()
    }
    positive_directions = sum(value > 0 for value in directions.values())
    all_seed_positive = all(
        row["mean_within_dataset_rho"] > 0 for row in seed_specific.values()
    )
    geometry_replicated = (
        exact["observed_rho"] >= 0.70
        and exact["exact_two_sided_p"] < 0.01
        and positive_directions >= 5
        and all_seed_positive
    )

    primary_scores = _loco_scores(rows_by_dataset, "final_ce_fraction")
    secondary_scores = _loco_scores(rows_by_dataset, "normalized_curve_area")
    ordered = sorted(primary_scores, key=lambda key: primary_scores[key]["loco_rmse"])
    winner, runner_up = ordered[:2]
    g_rmse = primary_scores[PRIMARY]["loco_rmse"]
    runner_up_rmse = primary_scores[runner_up]["loco_rmse"]
    relative_improvement = (runner_up_rmse - g_rmse) / runner_up_rmse
    beats_radius_datasets = sum(
        primary_scores[PRIMARY]["per_held_out_dataset"][dataset]["rmse"]
        < primary_scores["log2_radius"]["per_held_out_dataset"][dataset]["rmse"]
        for dataset in datasets
    )
    centered_correlations = _centered_predictor_correlations(rows_by_dataset)
    identifiability_pass = all(
        abs(value) < 0.995 for value in centered_correlations.values()
    )
    selected = (
        geometry_replicated
        and winner == PRIMARY
        and relative_improvement >= 0.05
        and beats_radius_datasets >= 5
        and identifiability_pass
    )
    if selected:
        verdict = "G_TOTAL_SELECTED"
    elif geometry_replicated:
        verdict = "GEOMETRY_SUPPORTED_METRIC_UNRESOLVED"
    else:
        verdict = "GEOMETRY_NOT_REPLICATED"

    mean_rhos = {
        predictor: float(
            np.mean(
                [
                    _rank_correlation(
                        [row[predictor] for row in dataset_rows],
                        [row["final_ce_fraction"] for row in dataset_rows],
                    )
                    for dataset_rows in rows_by_dataset.values()
                ]
            )
        )
        for predictor in PREDICTORS
    }
    area_rhos = {
        dataset: _rank_correlation(
            [row[PRIMARY] for row in dataset_rows],
            [row["normalized_curve_area"] for row in dataset_rows],
        )
        for dataset, dataset_rows in rows_by_dataset.items()
    }
    result = {
        "protocol_hash": protocol["protocol_hash"],
        "verdict": verdict,
        "geometry_replication": {
            "supported": geometry_replicated,
            "frozen_support_rule": protocol["geometry_replication_test"][
                "support_rule"
            ],
            "primary_exact_blocked_test": exact,
            "stride16_minus_stride1_final_ce_fraction": directions,
            "positive_endpoint_directions": positive_directions,
            "seed_specific_blocked_rhos": seed_specific,
            "all_seed_specific_blocked_rhos_positive": all_seed_positive,
            "secondary_curve_area_rho_by_dataset": area_rhos,
            "secondary_mean_curve_area_rho": float(np.mean(list(area_rhos.values()))),
            "mean_within_dataset_rho_by_predictor": mean_rhos,
        },
        "predictor_selection": {
            "selected": selected,
            "frozen_selection_rule": protocol["predictor_selection_test"][
                "selection_rule"
            ],
            "primary_loco_final_ce_fraction": primary_scores,
            "secondary_loco_curve_area": secondary_scores,
            "rmse_order": ordered,
            "winner": winner,
            "runner_up": runner_up,
            "g_total_relative_rmse_improvement_over_runner_up": relative_improvement,
            "g_total_beats_log2_radius_held_out_datasets": beats_radius_datasets,
            "centered_predictor_correlations_with_g_total": centered_correlations,
            "identifiability_gate_pass": identifiability_pass,
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
            "remote_gpu_training_cells": len(remote),
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
                "geometry": result["geometry_replication"],
                "predictor_selection": {
                    "rmse_order": ordered,
                    "scores": {
                        key: primary_scores[key]["loco_rmse"] for key in ordered
                    },
                    "relative_improvement": relative_improvement,
                    "beats_radius_datasets": beats_radius_datasets,
                    "identifiability_pass": identifiability_pass,
                    "centered_correlations": centered_correlations,
                },
            },
            indent=2,
        )
    )
    return result


if __name__ == "__main__":
    analyze(load_protocol())
