"""Profile and analyze the v1.9 Parseval/stride pilot."""

from __future__ import annotations

import argparse
import hashlib
import json
import math
import sys
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).parent.parent))
sys.path.insert(0, str(Path(__file__).parent))

from v17_local_geometry_analysis import _spearman

from dlx.analysis.local_geometry import (
    local_ball_cardinalities,
    spectral_search_complexity,
)
from dlx.profiles.text_anova import text_inverse_likelihood_profile

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs/local/v19_parseval_stride"
DATA = ROOT / "dlx/data_cache/v19_enwik8_bytes_n5500000.npy"
PROFILE_DIR = OUT / "profiles"


def load_protocol() -> dict:
    protocol = json.loads((ROOT / "configs/protocol_v1.9.json").read_text())
    recorded = protocol["protocol_hash"]
    unhashed = dict(protocol)
    unhashed.pop("protocol_hash")
    expected = hashlib.sha256(json.dumps(unhashed, sort_keys=True).encode()).hexdigest()
    if recorded != expected:
        raise ValueError(f"protocol hash mismatch: {recorded} != {expected}")
    return protocol


def stride_interleave(tokens: np.ndarray, stride: int) -> np.ndarray:
    """Round-robin contiguous lanes so original lag one becomes lag ``stride``."""

    values = np.asarray(tokens)
    if stride < 1 or len(values) % stride:
        raise ValueError("positive stride must divide token count exactly")
    lanes = values.reshape(stride, -1)
    return np.ascontiguousarray(lanes.T.reshape(-1))


def _sha256_tokens(tokens: np.ndarray) -> str:
    return hashlib.sha256(np.ascontiguousarray(tokens).tobytes()).hexdigest()


def profile_stride(stride: int, protocol: dict) -> dict:
    original = np.load(DATA, mmap_mode="r")
    tokens = stride_interleave(original, stride)
    spec = protocol["stride_pilot"]
    n_positions = int(spec["profile_max_positions"])
    n_times = n_positions // stride
    first_time = max(spec["profile_lags"]) // stride + 1
    last_time = len(tokens) // stride - 1
    sampled_times = np.linspace(first_time, last_time, num=n_times, dtype=np.int64)
    if len(np.unique(sampled_times)) != len(sampled_times):
        raise ValueError("profile time sampling produced duplicate targets")
    positions = (sampled_times[:, None] * stride + np.arange(stride)[None, :]).reshape(
        -1
    )
    fold_ids = np.repeat(np.arange(n_times) & 1, stride)
    profile = text_inverse_likelihood_profile(
        tokens,
        q=spec["q"],
        lags=tuple(spec["profile_lags"]),
        max_tokens=spec["profile_max_positions"],
        positions=positions,
        fold_ids=fold_ids,
    )
    profile.update(
        {
            "protocol_hash": protocol["protocol_hash"],
            "stride": stride,
            "data_sha256": _sha256_tokens(tokens),
            "position_sampling": "uniform corpus-wide time indices; every lane represented in both folds",
            "position_sha256": hashlib.sha256(positions.tobytes()).hexdigest(),
            "original_byte_counts_preserved": bool(
                np.array_equal(
                    np.bincount(tokens.astype(np.int64), minlength=256),
                    np.bincount(original.astype(np.int64), minlength=256),
                )
            ),
        }
    )
    PROFILE_DIR.mkdir(parents=True, exist_ok=True)
    (PROFILE_DIR / f"stride{stride}.json").write_text(json.dumps(profile, indent=2))
    print(
        f"stride {stride}: best={profile['best_pair_by_function_variance']} "
        f"V={profile['pair_function_variance_best_pair']:.6f}",
        flush=True,
    )
    return profile


def parseval_metrics(weights: list[float], radius: int, q: int = 256) -> dict:
    weights_array = np.asarray(weights, dtype=float)
    cardinalities = local_ball_cardinalities(q, radius, len(weights) - 1)
    costs = np.asarray(
        [math.log2(value) if value > 0 else 0.0 for value in cardinalities]
    )
    total = float(weights_array.sum())
    nonconstant = float(weights_array[1:].sum())
    probabilities = weights_array / total
    moment = float(np.dot(weights_array, costs))
    level_entropy = float(
        -sum(value * math.log2(value) for value in probabilities if value > 0)
    )
    return {
        "total_square_energy": total,
        "nonconstant_energy": nonconstant,
        "level_mass_normalized_by_total": probabilities.tolist(),
        "mean_degree_total": float(
            np.dot(weights_array, np.arange(len(weights_array))) / total
        ),
        "mean_degree_nonconstant": float(
            np.dot(weights_array[1:], np.arange(1, len(weights_array))) / nonconstant
        ),
        "geometric_moment_absolute": moment,
        "geometric_moment_total": moment / total,
        "geometric_moment_nonconstant": spectral_search_complexity(
            weights, cardinalities
        ),
        "level_entropy_total_bits": level_entropy,
        "spectral_entropy_uniform_within_level_upper_bits": (
            level_entropy + moment / total
        ),
        "effective_support_upper_log2": level_entropy + moment / total,
        "level_cardinalities": cardinalities,
        "level_search_cost_bits": costs.tolist(),
        "low_degree_concentration_total_leq_1": float(weights_array[:2].sum() / total),
    }


def _estimated_best(profile: dict) -> tuple[list[int], list[float]]:
    candidates = [
        (pair["lags"], pair["conditional_fourier_spectrum"])
        for pair in profile["pairs"]
    ]
    pair, spectrum = max(candidates, key=lambda item: item[1]["nonconstant_energy"])
    return pair, spectrum["level_weights"]


def _median_by_stride(remote: list[dict], stride: int) -> dict:
    rows = [row for row in remote if row["stride"] == stride]
    keys = (
        "initial_ce_bits",
        "final_ce_bits",
        "learning_amount_bits",
        "fractional_learning",
        "normalized_curve_area",
    )
    medians = {
        key: float(np.median([row["floor_independent"][key] for row in rows]))
        for key in keys
    }
    medians["final_ce_fraction"] = medians["final_ce_bits"] / medians["initial_ce_bits"]
    return medians


def existing_real_reanalysis() -> dict:
    source = json.loads(
        (ROOT / "runs/local/v18_real_natural/integrated_analysis.json").read_text()
    )
    rows = []
    for source_row in source["datasets"]:
        radius = max(source_row["best_pair"])
        metrics = parseval_metrics(source_row["level_weights"], radius)
        rows.append(
            {
                "dataset": source_row["dataset"],
                "radius": radius,
                "normalized_curve_area": source_row["normalized_curve_area"],
                "final_ce_fraction": (
                    source_row["final_ce_bits"] / source_row["initial_ce_bits"]
                ),
                **metrics,
            }
        )
    area = [row["normalized_curve_area"] for row in rows]
    final_fraction = [row["final_ce_fraction"] for row in rows]
    metric_names = (
        "mean_degree_nonconstant",
        "mean_degree_total",
        "geometric_moment_absolute",
        "geometric_moment_nonconstant",
        "geometric_moment_total",
        "level_entropy_total_bits",
        "spectral_entropy_uniform_within_level_upper_bits",
        "total_square_energy",
    )
    correlations = {
        name: {
            "vs_curve_area": _spearman([row[name] for row in rows], area),
            "vs_final_ce_fraction": _spearman(
                [row[name] for row in rows], final_fraction
            ),
        }
        for name in metric_names
    }
    return {
        "status": "exploratory post-v1.8 rescaling",
        "rows": rows,
        "correlations": correlations,
    }


def analyze(protocol: dict) -> dict:
    remote = json.loads((OUT / "remote_results.json").read_text())
    spec = protocol["stride_pilot"]
    rows = []
    for stride in spec["strides"]:
        profile = json.loads((PROFILE_DIR / f"stride{stride}.json").read_text())
        pair, weights = _estimated_best(profile)
        radius = max(pair)
        rows.append(
            {
                "stride": stride,
                "expected_pair": [stride, 2 * stride],
                "best_pair": pair,
                "radius": radius,
                "level_weights": weights,
                **parseval_metrics(weights, radius, q=spec["q"]),
                **_median_by_stride(remote, stride),
            }
        )

    area = [row["normalized_curve_area"] for row in rows]
    final_fraction = [row["final_ce_fraction"] for row in rows]
    metric_names = (
        "stride",
        "mean_degree_nonconstant",
        "mean_degree_total",
        "geometric_moment_absolute",
        "geometric_moment_nonconstant",
        "geometric_moment_total",
        "spectral_entropy_uniform_within_level_upper_bits",
    )
    correlations = {
        name: {
            "vs_curve_area": _spearman([row[name] for row in rows], area),
            "vs_final_ce_fraction": _spearman(
                [row[name] for row in rows], final_fraction
            ),
        }
        for name in metric_names
    }
    geometry = correlations["geometric_moment_total"]
    monotone_area = bool(np.all(np.diff(area) >= 0))
    monotone_final = bool(np.all(np.diff(final_fraction) >= 0))
    if (
        monotone_area
        and monotone_final
        and geometry["vs_curve_area"]["rho"] >= 0.8
        and geometry["vs_final_ce_fraction"]["rho"] >= 0.8
    ):
        decision = "SUPPORTED"
    elif (
        geometry["vs_curve_area"]["rho"] <= 0
        or geometry["vs_final_ce_fraction"]["rho"] <= 0
    ):
        decision = "REFUTED"
    else:
        decision = "INCONCLUSIVE"

    result = {
        "protocol_hash": protocol["protocol_hash"],
        "existing_real_reanalysis": existing_real_reanalysis(),
        "stride_pilot": {
            "decision": decision,
            "support_rule": spec["support_rule"],
            "rows": rows,
            "seed_cells": remote,
            "correlations": correlations,
            "area_nondecreasing_all_strides": monotone_area,
            "final_ce_fraction_nondecreasing_all_strides": monotone_final,
            "interpretation": (
                "same real bytes become sharply harder once local dependencies are "
                "dilated; the response then largely saturates and is not perfectly monotone"
            ),
        },
        "compute": {
            "remote_gpu_cells": len(remote),
            "local_training_cells": 0,
            "gpu_names": sorted({row["remote"]["gpu"] for row in remote}),
            "total_remote_cell_wallclock_seconds": float(
                sum(row["remote"]["wallclock_seconds"] for row in remote)
            ),
        },
    }
    (OUT / "integrated_analysis.json").write_text(json.dumps(result, indent=2))
    print(
        json.dumps(
            {
                "decision": decision,
                "rows": [
                    {
                        "stride": row["stride"],
                        "pair": row["best_pair"],
                        "G_total": row["geometric_moment_total"],
                        "area": row["normalized_curve_area"],
                        "final_fraction": row["final_ce_fraction"],
                    }
                    for row in rows
                ],
                "correlations": correlations,
            },
            indent=2,
        )
    )
    return result


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("stage", choices=("profile", "analyze"))
    parser.add_argument("--stride", type=int)
    args = parser.parse_args()
    protocol = load_protocol()
    if args.stage == "profile":
        if args.stride not in protocol["stride_pilot"]["strides"]:
            parser.error("profile requires a frozen --stride")
        profile_stride(args.stride, protocol)
    else:
        analyze(protocol)


if __name__ == "__main__":
    main()
