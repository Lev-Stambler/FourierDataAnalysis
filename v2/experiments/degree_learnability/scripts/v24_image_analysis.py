"""Analyze DCT/Hermite descriptors against corrected image learning curves."""

from __future__ import annotations

import json
import math
import sys
from pathlib import Path

from scipy.stats import spearmanr

sys.path.insert(0, str(Path(__file__).parent))

from v20_analyze import _median_cell_metrics

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs/local/v24_spectrum_predictor"
FEATURES = (
    "dct_non_dc_frequency_centroid",
    "dct_low_frequency_concentration",
    "dct_high_frequency_tail",
    "dct_non_dc_spectral_entropy",
    "hermite_degree2_energy_log",
    "hermite_energy_weighted_log_distance",
    "hermite_spectral_entropy",
    "hermite_correlation_effective_rank",
)


def _features(profile: dict) -> dict:
    dct = profile["dct"]
    hermite = profile["gaussian_hermite"]
    return {
        "dct_non_dc_frequency_centroid": dct["non_dc_frequency_centroid"],
        "dct_low_frequency_concentration": dct[
            "low_frequency_concentration_r_leq_quarter"
        ],
        "dct_high_frequency_tail": dct["high_frequency_tail_r_gt_half"],
        "dct_non_dc_spectral_entropy": dct["non_dc_spectral_entropy_bits"],
        "hermite_degree2_energy_log": math.log2(
            1.0 + hermite["degree2_correlation_energy"]
        ),
        "hermite_energy_weighted_log_distance": hermite[
            "energy_weighted_log_distance"
        ],
        "hermite_spectral_entropy": hermite["spectral_entropy_bits"],
        "hermite_correlation_effective_rank": hermite[
            "correlation_effective_rank"
        ],
    }


def analyze() -> dict:
    cells = json.loads((OUT / "image_remote_results.json").read_text())
    datasets = sorted({row["dataset"] for row in cells})
    if len(cells) != 18 or len(datasets) != 6:
        raise ValueError("corrected image grid is incomplete")
    rows = []
    for dataset in datasets:
        selected = [row for row in cells if row["dataset"] == dataset]
        profile = json.loads((OUT / "image_profiles" / f"{dataset}.json").read_text())
        rows.append(
            {
                "dataset": dataset,
                "features": _features(profile),
                "marginal_code_entropy_bits": selected[0][
                    "marginal_code_entropy_bits"
                ],
                **_median_cell_metrics(selected),
            }
        )
    correlations = {
        target: {
            feature: {
                "spearman_rho": float(
                    spearmanr(
                        [row["features"][feature] for row in rows],
                        [row[target] for row in rows],
                    ).statistic
                ),
                "two_sided_p": float(
                    spearmanr(
                        [row["features"][feature] for row in rows],
                        [row[target] for row in rows],
                    ).pvalue
                ),
            }
            for feature in FEATURES
        }
        for target in ("final_ce_fraction", "normalized_curve_area")
    }
    result = {
        "status": "development-only six-dataset image panel",
        "basis_scope": {
            "dct": "orthonormal spatial cosine basis",
            "hermite": "rank-Gaussianized degree-two patch dependence",
            "not_pooled_with_text": True,
        },
        "old_invalid_m8_curves_reused": False,
        "rows": rows,
        "correlations": correlations,
    }
    (OUT / "image_analysis.json").write_text(json.dumps(result, indent=2))
    print(json.dumps(correlations, indent=2))
    return result


if __name__ == "__main__":
    analyze()
