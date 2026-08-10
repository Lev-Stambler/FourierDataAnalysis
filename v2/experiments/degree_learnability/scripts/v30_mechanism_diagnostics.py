"""Clearly labeled post-gate diagnostics for the v3.0 mechanism result."""

from __future__ import annotations

import json
from pathlib import Path

import numpy as np
from sklearn.linear_model import LinearRegression

from dlx.protocol.frozen import verify_hash_lock, write_hash_once, write_json_once

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs/local/v30_architecture_spectrum"


def _rmse(actual: np.ndarray, predicted: np.ndarray) -> float:
    return float(np.sqrt(np.mean((actual - predicted) ** 2)))


def main() -> dict:
    mechanism_hash = verify_hash_lock(
        OUT / "mechanism_analysis.json", OUT / "mechanism_analysis.sha256"
    )
    mechanism = json.loads((OUT / "mechanism_analysis.json").read_text())
    rows = mechanism["rows"]
    architectures = sorted({row["architecture"] for row in rows})

    def features(row: dict, *, include_ntk: bool) -> list[float]:
        result = [
            float(row["architecture"] == value) for value in architectures[1:]
        ] + [float(row["degree"] == value) for value in (2, 3)]
        if include_ntk:
            result.append(float(row["mean_log_ntk_response"]))
        return result

    actual = np.asarray([row["character_hardness"] for row in rows], dtype=float)
    r_squared = {}
    for label, include_ntk in (("fixed_effects", False), ("plus_ntk", True)):
        matrix = np.asarray(
            [features(row, include_ntk=include_ntk) for row in rows], dtype=float
        )
        model = LinearRegression().fit(matrix, actual)
        r_squared[label] = float(model.score(matrix, actual))

    supports = sorted({row["support"] for row in rows})
    support_predictions = {
        "fixed_effects": np.empty(len(rows), dtype=float),
        "plus_ntk": np.empty(len(rows), dtype=float),
    }
    for support in supports:
        train = [index for index, row in enumerate(rows) if row["support"] != support]
        test = [index for index, row in enumerate(rows) if row["support"] == support]
        for label, include_ntk in (("fixed_effects", False), ("plus_ntk", True)):
            matrix = np.asarray(
                [features(rows[index], include_ntk=include_ntk) for index in train],
                dtype=float,
            )
            model = LinearRegression().fit(matrix, actual[train])
            support_predictions[label][test] = model.predict(
                np.asarray(
                    [features(rows[index], include_ntk=include_ntk) for index in test],
                    dtype=float,
                )
            )
    base_rmse = _rmse(actual, support_predictions["fixed_effects"])
    ntk_rmse = _rmse(actual, support_predictions["plus_ntk"])
    log_response = np.asarray(
        [row["mean_log_ntk_response"] for row in rows], dtype=float
    )
    degree_medians = {
        architecture: {
            str(degree): float(
                np.median(
                    [
                        row["character_hardness"]
                        for row in rows
                        if row["architecture"] == architecture
                        and row["degree"] == degree
                    ]
                )
            )
            for degree in (1, 2, 3)
        }
        for architecture in architectures
    }
    result = {
        "status": "EXPLORATORY_POST_GATE_DIAGNOSTIC",
        "mechanism_analysis_hash": mechanism_hash,
        "in_sample_r_squared": r_squared,
        "partial_r_squared_of_ntk": (r_squared["plus_ntk"] - r_squared["fixed_effects"])
        / (1.0 - r_squared["fixed_effects"]),
        "standardized_log_ntk_coefficient": mechanism["regression"][
            "mean_log_ntk_response_coefficient"
        ]
        * float(log_response.std())
        / float(actual.std()),
        "grouped_leave_one_support_out": {
            "fixed_effects_rmse": base_rmse,
            "plus_ntk_rmse": ntk_rmse,
            "relative_rmse_improvement": (base_rmse - ntk_rmse) / base_rmse,
        },
        "median_character_hardness_by_architecture_and_degree": degree_medians,
        "degree_one_radius_hardness": {
            architecture: {
                str(row["radius"]): row["character_hardness"]
                for row in rows
                if row["architecture"] == architecture and row["degree"] == 1
            }
            for architecture in architectures
        },
    }
    digest = write_json_once(OUT / "mechanism_diagnostics.json", result)
    write_hash_once(OUT / "mechanism_diagnostics.sha256", digest)
    print(json.dumps({**result, "artifact_sha256": digest}, indent=2))
    return result


if __name__ == "__main__":
    main()
