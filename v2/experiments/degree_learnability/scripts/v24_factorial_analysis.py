"""Analyze exact Fourier energy × degree × support-radius interventions."""

from __future__ import annotations

import json
import math
import sys
from pathlib import Path

import numpy as np
from scipy.stats import spearmanr

sys.path.insert(0, str(Path(__file__).parent))

from v20_analyze import _median_cell_metrics

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs/local/v24_spectrum_predictor"
TARGETS = ("final_ce_fraction", "normalized_curve_area")


def analyze() -> dict:
    cells = json.loads((OUT / "factorial_remote_results.json").read_text())
    if len(cells) != 108:
        raise ValueError(f"exact factorial incomplete: {len(cells)} != 108")
    keys = sorted(
        {
            (
                row["configuration"],
                row["degree"],
                row["radius"],
                row["signal_probability"],
            )
            for row in cells
        }
    )
    rows = []
    for configuration, degree, radius, probability in keys:
        selected = [
            row
            for row in cells
            if row["configuration"] == configuration
            and row["degree"] == degree
            and row["radius"] == radius
            and row["signal_probability"] == probability
        ]
        if len(selected) != 3:
            raise ValueError(f"missing factorial seeds: {keys}")
        rows.append(
            {
                "configuration": configuration,
                "degree": degree,
                "radius": radius,
                "signal_probability": probability,
                "exact_nonconstant_energy": probability**2 * (1.0 - 1.0 / 16.0),
                "log2_radius": math.log2(radius),
                **_median_cell_metrics(selected),
            }
        )

    configurations = sorted({row["configuration"] for row in rows})
    regressions = {}
    effects = {}
    for target in TARGETS:
        x = []
        y = []
        for row in rows:
            log_radius = row["log2_radius"]
            config_indicators = [
                float(row["configuration"] == config)
                for config in configurations[1:]
            ]
            x.append(
                [
                    1.0,
                    float(row["degree"] == 2),
                    row["exact_nonconstant_energy"],
                    log_radius,
                    *config_indicators,
                    *[indicator * log_radius for indicator in config_indicators],
                ]
            )
            y.append(row[target])
        x_array = np.asarray(x)
        y_array = np.asarray(y)
        coefficients, *_ = np.linalg.lstsq(x_array, y_array, rcond=None)
        predicted = x_array @ coefficients
        regressions[target] = {
            "terms": [
                "intercept",
                "degree2_indicator",
                "exact_nonconstant_energy",
                "log2_radius",
                *[f"config[{config}]" for config in configurations[1:]],
                *[
                    f"config[{config}]*log2_radius"
                    for config in configurations[1:]
                ],
            ],
            "coefficients": coefficients.tolist(),
            "r2": float(
                1.0
                - np.sum((y_array - predicted) ** 2)
                / np.sum((y_array - y_array.mean()) ** 2)
            ),
        }
        locality_rhos = []
        endpoint_directions = []
        degree_differences = []
        for configuration in configurations:
            for degree in (1, 2):
                for probability in (0.35, 0.7):
                    selected = sorted(
                        (
                            row
                            for row in rows
                            if row["configuration"] == configuration
                            and row["degree"] == degree
                            and row["signal_probability"] == probability
                        ),
                        key=lambda row: row["radius"],
                    )
                    locality_rhos.append(
                        float(
                            spearmanr(
                                [row["radius"] for row in selected],
                                [row[target] for row in selected],
                            ).statistic
                        )
                    )
                    endpoint_directions.append(selected[-1][target] > selected[0][target])
            for radius in (2, 8, 32):
                for probability in (0.35, 0.7):
                    degree1 = next(
                        row
                        for row in rows
                        if row["configuration"] == configuration
                        and row["degree"] == 1
                        and row["radius"] == radius
                        and row["signal_probability"] == probability
                    )
                    degree2 = next(
                        row
                        for row in rows
                        if row["configuration"] == configuration
                        and row["degree"] == 2
                        and row["radius"] == radius
                        and row["signal_probability"] == probability
                    )
                    degree_differences.append(degree2[target] - degree1[target])
        effects[target] = {
            "mean_within_cell_locality_spearman": float(np.mean(locality_rhos)),
            "locality_rhos": locality_rhos,
            "positive_radius32_minus_radius2": int(sum(endpoint_directions)),
            "total_radius_endpoints": len(endpoint_directions),
            "mean_degree2_minus_degree1": float(np.mean(degree_differences)),
        }
    result = {
        "status": "exact controlled categorical mechanism test",
        "exact_spectrum": (
            "nonconstant energy=p^2(1-1/q), pure degree k, support radius r"
        ),
        "rows": rows,
        "regressions": regressions,
        "effects": effects,
    }
    (OUT / "factorial_analysis.json").write_text(json.dumps(result, indent=2))
    print(json.dumps({"regressions": regressions, "effects": effects}, indent=2))
    return result


if __name__ == "__main__":
    analyze()
