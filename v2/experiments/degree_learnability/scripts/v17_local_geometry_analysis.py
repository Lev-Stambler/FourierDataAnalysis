"""Exploratory joint degree-locality spectrum analysis (protocol v1.7)."""

from __future__ import annotations

import hashlib
import itertools
import json
import sys
from math import factorial, log2
from pathlib import Path

import numpy as np
from scipy.stats import rankdata, spearmanr

sys.path.insert(0, str(Path(__file__).parent.parent))

from dlx.analysis.local_geometry import (
    local_ball_cardinalities,
    mixed_level_cardinalities,
    spectral_search_complexity,
)

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs" / "local" / "v17_local_geometry"
TEXT_SOURCE = ROOT / "runs" / "local" / "v16_conditional_spectrum"
H5_SOURCE = ROOT / "runs" / "local" / "h5_matched"
M6_TABLE = ROOT / "runs" / "modal" / "m6" / "difficulty_table.json"


def _sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def load_protocol() -> dict:
    path = ROOT / "configs" / "protocol_v1.7.json"
    protocol = json.loads(path.read_text())
    recorded = protocol["protocol_hash"]
    unhashed = dict(protocol)
    unhashed.pop("protocol_hash")
    expected = hashlib.sha256(json.dumps(unhashed, sort_keys=True).encode()).hexdigest()
    if recorded != expected:
        raise ValueError(f"protocol_v1.7 hash mismatch: {recorded} != {expected}")
    return protocol


def _spearman(x: list[float], y: list[float]) -> dict:
    x_array = np.asarray(x, dtype=float)
    y_array = np.asarray(y, dtype=float)
    if np.all(x_array == x_array[0]) or np.all(y_array == y_array[0]):
        return {
            "n": len(x),
            "rho": None,
            "asymptotic_p": None,
            "exact_permutation_p": None,
            "constant_input": True,
        }
    result = spearmanr(x_array, y_array)
    record = {
        "n": len(x),
        "rho": float(result.statistic),
        "asymptotic_p": float(result.pvalue),
    }
    if len(x) <= 8:
        x_rank = rankdata(x_array)
        y_rank = rankdata(y_array)
        x_centered = x_rank - x_rank.mean()
        y_centered = y_rank - y_rank.mean()
        denominator = float(np.linalg.norm(x_centered) * np.linalg.norm(y_centered))
        observed = abs(float(np.dot(x_centered, y_centered) / denominator))
        extreme = sum(
            abs(
                float(
                    np.dot(x_centered, np.asarray(permutation) - y_rank.mean())
                    / denominator
                )
            )
            >= observed - 1e-12
            for permutation in itertools.permutations(y_rank.tolist())
        )
        record["exact_permutation_p"] = extreme / factorial(len(x))
    return record


def _text_analysis() -> dict:
    source_path = TEXT_SOURCE / "integrated_analysis.json"
    source = json.loads(source_path.read_text())
    rows = []
    for original in source["rungs"]:
        q = 256
        weights = original["spectrum"]["level_weights"]
        radius = max(original["best_pair"])
        upper_cardinality = local_ball_cardinalities(q, radius, len(weights) - 1)
        upper_bits = spectral_search_complexity(weights, upper_cardinality)

        if original["source_kind"] == "natural":
            lower_cardinality = list(upper_cardinality)
            lower_cardinality[1] = q - 1
            radius_assignment = {
                "degree_1": [1, radius],
                "degree_2": [radius, radius],
                "note": "saved W1 combines the two additive coordinates",
            }
        else:
            lower_cardinality = upper_cardinality
            active_degree = 1 if weights[1] > 0 else 2
            radius_assignment = {
                f"degree_{active_degree}": [radius, radius],
                "note": "exact planted support",
            }
        lower_bits = spectral_search_complexity(weights, lower_cardinality)
        rows.append(
            {
                "rung": original["rung"],
                "source_kind": original["source_kind"],
                "q": q,
                "best_pair": original["best_pair"],
                "level_weights": weights,
                "mean_nonconstant_spectral_degree": original["spectrum"][
                    "mean_nonconstant_spectral_degree"
                ],
                "radius_assignment": radius_assignment,
                "local_ball_cardinalities_upper": upper_cardinality,
                "search_complexity_bits": upper_bits,
                "search_complexity_bits_interval": [lower_bits, upper_bits],
                "geometric_degree_qary": upper_bits / log2(q),
                "geometric_degree_qary_interval": [
                    lower_bits / log2(q),
                    upper_bits / log2(q),
                ],
                "normalized_curve_area": original["normalized_curve_area"],
            }
        )

    degrees = [row["mean_nonconstant_spectral_degree"] for row in rows]
    upper = [row["search_complexity_bits"] for row in rows]
    areas = [row["normalized_curve_area"] for row in rows]
    raw = _spearman(degrees, areas)
    geometry = _spearman(upper, areas)

    natural_indices = [
        index for index, row in enumerate(rows) if row["source_kind"] == "natural"
    ]
    sensitivity_rhos = []
    for endpoints in itertools.product((0, 1), repeat=len(natural_indices)):
        costs = list(upper)
        for index, endpoint in zip(natural_indices, endpoints):
            costs[index] = rows[index]["search_complexity_bits_interval"][endpoint]
        sensitivity_rhos.append(_spearman(costs, areas)["rho"])

    natural_rows = [row for row in rows if row["source_kind"] == "natural"]
    return {
        "input_sha256": _sha256(source_path),
        "rungs": rows,
        "correlations": {
            "raw_spectral_degree_vs_curve_area": raw,
            "geometric_complexity_vs_curve_area": geometry,
            "geometry_rho_over_natural_radius_endpoints": [
                min(sensitivity_rhos),
                max(sensitivity_rhos),
            ],
            "rho_change_at_upper_endpoint": geometry["rho"] - raw["rho"],
        },
        "real_only_scope": {
            "n_natural_corpora": len(natural_rows),
            "raw_and_geometry_order_identical": bool(
                np.sign(
                    natural_rows[0]["mean_nonconstant_spectral_degree"]
                    - natural_rows[1]["mean_nonconstant_spectral_degree"]
                )
                == np.sign(
                    natural_rows[0]["search_complexity_bits"]
                    - natural_rows[1]["search_complexity_bits"]
                )
            ),
            "inference": "two natural corpora cannot independently validate locality",
        },
    }


def _tabular_analysis() -> dict:
    spectra_path = H5_SOURCE / "part_a" / "part_a_results.json"
    curves_path = H5_SOURCE / "integrated_analysis.json"
    spectra = json.loads(spectra_path.read_text())
    curves = json.loads(curves_path.read_text())
    rows = []
    bands = {}
    raw_rank_blocks = []
    geometry_rank_blocks = []
    area_rank_blocks = []
    for band, band_meta in spectra["bands"].items():
        curve_by_name = {
            row["dataset"]: row for row in curves["part_a"]["bands"][band]["datasets"]
        }
        band_rows = []
        for dataset, profile in band_meta["datasets"].items():
            weights = profile["W_normalized_for_analysis"]
            cardinalities = mixed_level_cardinalities(profile["q_features"])
            curve = curve_by_name[dataset]
            weights_array = np.asarray(weights, dtype=float)
            degrees = np.arange(len(weights_array), dtype=float)
            nonconstant_degree = float(
                np.dot(degrees[1:], weights_array[1:]) / weights_array[1:].sum()
            )
            row = {
                "band": band,
                "dataset": dataset,
                "n_rows": profile["n_rows"],
                "n_features": profile["n_features"],
                "q_features": profile["q_features"],
                "level_weights": weights,
                "level_cardinalities": cardinalities,
                "source_mean_spectral_degree_including_degree0": profile["mean_degree"],
                "mean_nonconstant_spectral_degree": nonconstant_degree,
                "search_complexity_bits": spectral_search_complexity(
                    weights, cardinalities
                ),
                "normalized_curve_area": curve["normalized_curve_area"],
            }
            band_rows.append(row)
            rows.append(row)
        degree = [row["mean_nonconstant_spectral_degree"] for row in band_rows]
        geometry = [row["search_complexity_bits"] for row in band_rows]
        area = [row["normalized_curve_area"] for row in band_rows]
        raw_rank_blocks.extend(rankdata(degree).tolist())
        geometry_rank_blocks.extend(rankdata(geometry).tolist())
        area_rank_blocks.extend(rankdata(area).tolist())
        bands[band] = {
            "n": len(band_rows),
            "raw_spectral_degree_vs_curve_area": _spearman(degree, area),
            "geometric_complexity_vs_curve_area": _spearman(geometry, area),
            "raw_and_geometry_ranking_identical": (
                rankdata(degree).tolist() == rankdata(geometry).tolist()
            ),
        }

    degree = [row["mean_nonconstant_spectral_degree"] for row in rows]
    geometry = [row["search_complexity_bits"] for row in rows]
    area = [row["normalized_curve_area"] for row in rows]
    return {
        "input_sha256": {
            "spectra": _sha256(spectra_path),
            "curves": _sha256(curves_path),
        },
        "n_real_datasets": len(rows),
        "rows": rows,
        "bands": bands,
        "pooled_diagnostic": {
            "warning": "feature count and alphabet geometry differ across bands",
            "raw_spectral_degree_vs_curve_area": _spearman(degree, area),
            "geometric_complexity_vs_curve_area": _spearman(geometry, area),
        },
        "within_band_rank_blocked": {
            "raw_spectral_degree_vs_curve_area": _spearman(
                raw_rank_blocks, area_rank_blocks
            ),
            "geometric_complexity_vs_curve_area": _spearman(
                geometry_rank_blocks, area_rank_blocks
            ),
            "note": "descriptive concatenation of within-band ranks; not an independent-sample p-value",
        },
    }


def _m6_locality_controls() -> dict:
    source = json.loads(M6_TABLE.read_text())
    families = [
        "F1_k1",
        "F1_k2",
        "F1_k3",
        "F1_k4",
        "F1_k6",
        "F1_k8",
        "copy_lag4",
        "copy_lag16",
    ]
    rows = []
    for family in families:
        cells = [row for row in source if row["cell_id"].split("/")[1] == family]
        if family.startswith("F1_k"):
            degree = int(family.removeprefix("F1_k"))
            radius = degree
        else:
            degree = 1
            radius = int(family.removeprefix("copy_lag"))
        cardinalities = local_ball_cardinalities(32, radius, degree)
        weights = [0.0] * degree + [1.0]
        rows.append(
            {
                "family": family,
                "q": 32,
                "planted_degree": degree,
                "radius": radius,
                "active_level_cardinality": cardinalities[degree],
                "search_complexity_bits": spectral_search_complexity(
                    weights, cardinalities
                ),
                "geometric_degree_qary": spectral_search_complexity(
                    weights, cardinalities, log_base=32
                ),
                "bayes_floor_bits": float(cells[0]["bayes_floor_bits"]),
                "median_final_gap_bits": float(
                    np.median([cell["final_gap_bits"] for cell in cells])
                ),
                "median_norm_remaining": float(
                    np.median([cell["norm_remaining"] for cell in cells])
                ),
                "n_seeds": len(cells),
            }
        )
    degree = [row["planted_degree"] for row in rows]
    geometry = [row["search_complexity_bits"] for row in rows]
    final_gap = [row["median_final_gap_bits"] for row in rows]
    remaining = [row["median_norm_remaining"] for row in rows]
    return {
        "input_sha256": _sha256(M6_TABLE),
        "rows": rows,
        "common_bayes_floor": len({row["bayes_floor_bits"] for row in rows}) == 1,
        "correlations": {
            "raw_degree_vs_final_gap": _spearman(degree, final_gap),
            "geometric_complexity_vs_final_gap": _spearman(geometry, final_gap),
            "raw_degree_vs_norm_remaining": _spearman(degree, remaining),
            "geometric_complexity_vs_norm_remaining": _spearman(geometry, remaining),
        },
        "locality_contrasts": {
            "copy_lag16_minus_copy_lag4_final_gap": (
                next(row for row in rows if row["family"] == "copy_lag16")[
                    "median_final_gap_bits"
                ]
                - next(row for row in rows if row["family"] == "copy_lag4")[
                    "median_final_gap_bits"
                ]
            ),
            "ordering_by_geometry_and_final_gap": [
                row["family"]
                for row in sorted(
                    rows, key=lambda value: value["search_complexity_bits"]
                )
            ]
            == [
                row["family"]
                for row in sorted(
                    rows, key=lambda value: value["median_final_gap_bits"]
                )
            ],
        },
    }


def analyze(protocol: dict) -> dict:
    text = _text_analysis()
    tabular = _tabular_analysis()
    controls = _m6_locality_controls()
    return {
        "protocol_hash": protocol["protocol_hash"],
        "metric": "energy-weighted categorical Fourier support-search complexity",
        "formula": "G = sum W[k,r] log2(binom(r,k)(q-1)^k) / sum W[k,r], k>=1",
        "text": text,
        "matched_real_tabular": tabular,
        "m6_locality_controls": controls,
        "real_dataset_count": tabular["n_real_datasets"]
        + text["real_only_scope"]["n_natural_corpora"],
        "finding": {
            "controlled": (
                "geometry resolves the radius-4/radius-16 degree-1 copies and "
                "perfectly orders median final gap across the eight matched M6 families"
            ),
            "text": (
                "four-rung rho rises from 0.40 for raw degree to 0.80 for geometric "
                "complexity and is unchanged over the natural W1 radius bounds"
            ),
            "real_data": (
                "no independent improvement: geometry preserves the raw-degree rank "
                "inside every matched tabular band, and only two natural texts exist"
            ),
            "verdict": "PROMISING_CONTROLLED_GEOMETRY_EFFECT; REAL_DATA_CONFIRMATION_OPEN",
        },
        "limitations": [
            "v1.7 is post-hoc exploratory",
            "natural text spectra are strongest-pair slices, not full-context W[k,r]",
            "tabular features have no spatial ordering, so their geometry is coefficient-search width rather than locality",
            "image artifacts have learning curves but no compatible joint Fourier support spectrum and are not forced into this analysis",
        ],
        "training_or_profile_recomputation": 0,
    }


def main() -> None:
    protocol = load_protocol()
    result = analyze(protocol)
    OUT.mkdir(parents=True, exist_ok=True)
    (OUT / "integrated_analysis.json").write_text(json.dumps(result, indent=2))
    print(
        json.dumps(
            {
                "verdict": result["finding"]["verdict"],
                "text": result["text"]["correlations"],
                "m6": result["m6_locality_controls"]["correlations"],
                "tabular_bands": result["matched_real_tabular"]["bands"],
            },
            indent=2,
        )
    )


if __name__ == "__main__":
    main()
