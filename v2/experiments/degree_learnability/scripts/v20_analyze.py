"""Profile and perform the exact blocked v2.0 confirmation test."""

from __future__ import annotations

import argparse
import hashlib
import itertools
import json
import sys
from pathlib import Path

import numpy as np
from scipy.stats import rankdata

sys.path.insert(0, str(Path(__file__).parent.parent))
sys.path.insert(0, str(Path(__file__).parent))

from v19_analyze import parseval_metrics, stride_interleave

from dlx.profiles.text_anova import text_inverse_likelihood_profile

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs/local/v20_confirmatory_geometry"
PROFILE_DIR = OUT / "profiles"
DATA_FILES = {
    "enwik8": ROOT / "dlx/data_cache/v19_enwik8_bytes_n5500000.npy",
    "tinystories": ROOT / "dlx/data_cache/h5_tinystories_bytes_n5500000.npy",
    "wikitext2": ROOT / "dlx/data_cache/v18_wikitext2_bytes_n5500000.npy",
    "codeparrot_python": ROOT
    / "dlx/data_cache/v18_codeparrot_python_bytes_n5500000.npy",
}
PILOT_DATASET = "enwik8"


def load_protocol() -> dict:
    protocol = json.loads((ROOT / "configs/protocol_v2.0.json").read_text())
    recorded = protocol["protocol_hash"]
    unhashed = dict(protocol)
    unhashed.pop("protocol_hash")
    expected = hashlib.sha256(json.dumps(unhashed, sort_keys=True).encode()).hexdigest()
    if recorded != expected:
        raise ValueError(f"protocol hash mismatch: {recorded} != {expected}")
    return protocol


def _sha256_tokens(tokens: np.ndarray) -> str:
    return hashlib.sha256(np.ascontiguousarray(tokens).tobytes()).hexdigest()


def _profile_positions(n_tokens: int, stride: int, n_positions: int) -> tuple:
    n_times = n_positions // stride
    first_time = 16 // stride + 1
    last_time = n_tokens // stride - 1
    sampled_times = np.linspace(first_time, last_time, num=n_times, dtype=np.int64)
    if len(np.unique(sampled_times)) != len(sampled_times):
        raise ValueError("profile time sampling produced duplicate targets")
    positions = (sampled_times[:, None] * stride + np.arange(stride)[None, :]).reshape(
        -1
    )
    fold_ids = np.repeat(np.arange(n_times) & 1, stride)
    return positions, fold_ids


def profile_cell(dataset: str, stride: int, protocol: dict) -> dict:
    original = np.load(DATA_FILES[dataset], mmap_mode="r")
    tokens = stride_interleave(original, stride)
    spec = protocol["spectrum"]
    positions, fold_ids = _profile_positions(
        len(tokens), stride, spec["profile_positions"]
    )
    profile = text_inverse_likelihood_profile(
        tokens,
        q=256,
        lags=tuple(spec["profile_lags"]),
        max_tokens=spec["profile_positions"],
        positions=positions,
        fold_ids=fold_ids,
    )
    original_counts = np.bincount(original.astype(np.int64), minlength=256)
    transformed_counts = np.bincount(tokens.astype(np.int64), minlength=256)
    profile.update(
        {
            "protocol_hash": protocol["protocol_hash"],
            "dataset": dataset,
            "stride": stride,
            "data_sha256": _sha256_tokens(tokens),
            "byte_counts_preserved": bool(
                np.array_equal(original_counts, transformed_counts)
            ),
            "position_sampling": spec["position_sampling"],
            "position_sha256": hashlib.sha256(positions.tobytes()).hexdigest(),
        }
    )
    PROFILE_DIR.mkdir(parents=True, exist_ok=True)
    path = PROFILE_DIR / f"{dataset}__stride{stride}.json"
    path.write_text(json.dumps(profile, indent=2))
    print(
        f"{dataset} stride {stride}: best={profile['best_pair_by_function_variance']} "
        f"V={profile['pair_function_variance_best_pair']:.6f}",
        flush=True,
    )
    return profile


def _estimated_best(profile: dict) -> tuple[list[int], list[float]]:
    candidates = [
        (pair["lags"], pair["conditional_fourier_spectrum"])
        for pair in profile["pairs"]
    ]
    pair, spectrum = max(candidates, key=lambda item: item[1]["nonconstant_energy"])
    return pair, spectrum["level_weights"]


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


def _rank_correlation(x: list[float], y: list[float]) -> float:
    x_rank = rankdata(x)
    y_rank = rankdata(y)
    return float(np.corrcoef(x_rank, y_rank)[0, 1])


def exact_blocked_permutation(rows_by_dataset: dict[str, list[dict]]) -> dict:
    observed_by_dataset = {
        dataset: _rank_correlation(
            [row["geometric_moment_total"] for row in rows],
            [row["final_ce_fraction"] for row in rows],
        )
        for dataset, rows in rows_by_dataset.items()
    }
    observed = float(np.mean(list(observed_by_dataset.values())))
    null_values = []
    for rows in rows_by_dataset.values():
        x = [row["geometric_moment_total"] for row in rows]
        y = [row["final_ce_fraction"] for row in rows]
        null_values.append(
            [
                _rank_correlation(x, permutation)
                for permutation in itertools.permutations(y)
            ]
        )
    extreme = 0
    total = 0
    for combination in itertools.product(*null_values):
        statistic = float(np.mean(combination))
        total += 1
        if abs(statistic) >= abs(observed) - 1e-12:
            extreme += 1
    return {
        "statistic": "mean within-dataset Spearman rho",
        "observed_rho": observed,
        "rho_by_dataset": observed_by_dataset,
        "exact_two_sided_p": extreme / total,
        "extreme_permutations": extreme,
        "total_permutations": total,
    }


def _seed_specific_rhos(
    rows_by_dataset: dict[str, list[dict]], remote: list[dict], seeds: list[int]
) -> dict:
    output = {}
    for seed in seeds:
        correlations = []
        for dataset, rows in rows_by_dataset.items():
            cells = sorted(
                [
                    cell
                    for cell in remote
                    if cell["dataset"] == dataset and cell["seed"] == seed
                ],
                key=lambda cell: cell["stride"],
            )
            correlations.append(
                _rank_correlation(
                    [row["geometric_moment_total"] for row in rows],
                    [
                        cell["floor_independent"]["final_ce_bits"]
                        / cell["floor_independent"]["initial_ce_bits"]
                        for cell in cells
                    ],
                )
            )
        output[str(seed)] = {
            "mean_within_dataset_rho": float(np.mean(correlations)),
            "rho_by_dataset": correlations,
        }
    return output


def analyze(protocol: dict) -> dict:
    remote = json.loads((OUT / "remote_results.json").read_text())
    strides = protocol["intervention"]["strides"]
    datasets = [row["id"] for row in protocol["datasets"]]
    rows = []
    rows_by_dataset = {}
    for dataset in datasets:
        dataset_rows = []
        for stride in strides:
            profile = json.loads(
                (PROFILE_DIR / f"{dataset}__stride{stride}.json").read_text()
            )
            pair, weights = _estimated_best(profile)
            cells = [
                cell
                for cell in remote
                if cell["dataset"] == dataset and cell["stride"] == stride
            ]
            row = {
                "dataset": dataset,
                "stride": stride,
                "expected_pair": [stride, 2 * stride],
                "best_pair": pair,
                "radius": max(pair),
                "level_weights": weights,
                **parseval_metrics(weights, max(pair)),
                **_median_cell_metrics(cells),
            }
            dataset_rows.append(row)
            rows.append(row)
        rows_by_dataset[dataset] = dataset_rows

    exact = exact_blocked_permutation(rows_by_dataset)
    fresh_only = exact_blocked_permutation(
        {
            dataset: rows
            for dataset, rows in rows_by_dataset.items()
            if dataset != PILOT_DATASET
        }
    )
    seed_specific = _seed_specific_rhos(
        rows_by_dataset, remote, protocol["replication"]["seeds"]
    )
    directions = {
        dataset: (
            dataset_rows[-1]["final_ce_fraction"] - dataset_rows[0]["final_ce_fraction"]
        )
        for dataset, dataset_rows in rows_by_dataset.items()
    }
    positive_directions = sum(value > 0 for value in directions.values())
    all_seed_rhos_positive = all(
        row["mean_within_dataset_rho"] > 0 for row in seed_specific.values()
    )
    if (
        exact["observed_rho"] >= 0.70
        and exact["exact_two_sided_p"] < 0.05
        and positive_directions >= 3
        and all_seed_rhos_positive
    ):
        decision = "SUPPORTED"
    elif exact["observed_rho"] <= 0 or positive_directions < 3:
        decision = "REFUTED"
    else:
        decision = "INCONCLUSIVE"

    secondary_rhos = {
        dataset: _rank_correlation(
            [row["geometric_moment_total"] for row in dataset_rows],
            [row["normalized_curve_area"] for row in dataset_rows],
        )
        for dataset, dataset_rows in rows_by_dataset.items()
    }
    comparator_names = (
        "mean_degree_total",
        "mean_degree_nonconstant",
        "geometric_moment_absolute",
        "geometric_moment_nonconstant",
        "geometric_moment_total",
        "spectral_entropy_uniform_within_level_upper_bits",
    )
    comparator_blocked_rhos = {
        name: float(
            np.mean(
                [
                    _rank_correlation(
                        [row[name] for row in dataset_rows],
                        [row["final_ce_fraction"] for row in dataset_rows],
                    )
                    for dataset_rows in rows_by_dataset.values()
                ]
            )
        )
        for name in comparator_names
    }
    result = {
        "protocol_hash": protocol["protocol_hash"],
        "confirmatory_verdict": decision,
        "support_rule": protocol["confirmatory_test"]["support_rule"],
        "primary_exact_blocked_test": exact,
        "fresh_corpora_only_sensitivity": {
            "status": "post-hoc confirmatory-purity sensitivity; not the frozen primary test",
            "excluded_pilot_dataset": PILOT_DATASET,
            **fresh_only,
        },
        "stride8_minus_stride1_final_ce_fraction": directions,
        "positive_endpoint_directions": positive_directions,
        "seed_specific_blocked_rhos": seed_specific,
        "all_seed_specific_blocked_rhos_positive": all_seed_rhos_positive,
        "secondary_curve_area_rho_by_dataset": secondary_rhos,
        "secondary_mean_curve_area_rho": float(np.mean(list(secondary_rhos.values()))),
        "comparator_mean_within_dataset_rhos": comparator_blocked_rhos,
        "dataset_stride_rows": rows,
        "remote_cells": remote,
        "spectrum_invariance": {
            dataset: {
                "total_energy_range": [
                    min(row["total_square_energy"] for row in dataset_rows),
                    max(row["total_square_energy"] for row in dataset_rows),
                ],
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
            "remote_gpu_cells": len(remote),
            "local_training_cells": 0,
            "total_remote_cell_wallclock_seconds": float(
                sum(row["remote"]["wallclock_seconds"] for row in remote)
            ),
            "gpu_names": sorted({row["remote"]["gpu"] for row in remote}),
        },
        "scope": protocol["scope"],
    }
    (OUT / "integrated_analysis.json").write_text(json.dumps(result, indent=2))
    print(
        json.dumps(
            {
                "verdict": decision,
                "primary": exact,
                "directions": directions,
                "seed_specific": seed_specific,
                "secondary_area_mean_rho": result["secondary_mean_curve_area_rho"],
                "comparators": comparator_blocked_rhos,
            },
            indent=2,
        )
    )
    return result


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("stage", choices=("profile", "analyze"))
    parser.add_argument("--dataset", choices=tuple(DATA_FILES))
    parser.add_argument("--stride", type=int)
    args = parser.parse_args()
    protocol = load_protocol()
    if args.stage == "profile":
        if (
            args.dataset is None
            or args.stride not in protocol["intervention"]["strides"]
        ):
            parser.error("profile requires a frozen --dataset and --stride")
        profile_cell(args.dataset, args.stride, protocol)
    else:
        analyze(protocol)


if __name__ == "__main__":
    main()
