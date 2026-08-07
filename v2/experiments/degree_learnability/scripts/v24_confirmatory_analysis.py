"""Score the frozen v2.4 predictions after every confirmatory cell completes."""

from __future__ import annotations

import hashlib
import json
import math
import sys
from pathlib import Path

import numpy as np
from scipy.stats import rankdata
from sklearn.metrics import mean_squared_error, r2_score

sys.path.insert(0, str(Path(__file__).parent))

from v20_analyze import _median_cell_metrics
from v24_freeze_predictions import (
    CONFIGURATIONS,
    DATASETS,
    INTERVENTION_DATASETS,
    OUT,
    TARGETS,
)

ROOT = Path(__file__).parent.parent
RESULT_PATH = OUT / "confirmatory_remote_results.json"
PRIMARY_LOCK = OUT / "confirmatory_predictions.json"
BASELINE_LOCK = OUT / "confirmatory_baseline_predictions.json"


def _prediction_map() -> tuple[dict, dict]:
    primary = json.loads(PRIMARY_LOCK.read_text())
    baseline = json.loads(BASELINE_LOCK.read_text())
    mapping = {}
    for source in (primary, baseline):
        for row in source["predictions"]:
            key = (row["dataset"], int(row["stride"]), row["configuration"])
            mapping.setdefault(key, {}).update(row["predictions"])
    return mapping, {
        "primary": hashlib.sha256(PRIMARY_LOCK.read_bytes()).hexdigest(),
        "baseline": hashlib.sha256(BASELINE_LOCK.read_bytes()).hexdigest(),
    }


def _actual_rows() -> list[dict]:
    cells = json.loads(RESULT_PATH.read_text())
    expected = 30 * len(CONFIGURATIONS) * 3
    if len(cells) != expected:
        raise ValueError(f"confirmatory grid incomplete: {len(cells)} != {expected}")
    rows = []
    for dataset in DATASETS:
        strides = (1, 4, 8, 16) if dataset in INTERVENTION_DATASETS else (1,)
        for stride in strides:
            for configuration in CONFIGURATIONS:
                selected = [
                    row
                    for row in cells
                    if row["dataset"] == dataset
                    and row["stride"] == stride
                    and row["configuration"] == configuration
                ]
                if len(selected) != 3:
                    raise ValueError(
                        f"missing seeds: {dataset}/stride{stride}/{configuration}"
                    )
                rows.append(
                    {
                        "dataset": dataset,
                        "stride": stride,
                        "configuration": configuration,
                        **_median_cell_metrics(selected),
                    }
                )
    return rows


def _score(rows: list[dict], predictions: dict, feature_set: str, target: str) -> dict:
    actual = np.asarray([row[target] for row in rows], dtype=float)
    predicted = np.asarray(
        [
            predictions[(row["dataset"], row["stride"], row["configuration"])][
                feature_set
            ][target]
            for row in rows
        ],
        dtype=float,
    )
    per_corpus = {}
    for dataset in sorted({row["dataset"] for row in rows}):
        selected = np.asarray([row["dataset"] == dataset for row in rows])
        per_corpus[dataset] = math.sqrt(
            mean_squared_error(actual[selected], predicted[selected])
        )
    return {
        "rmse": math.sqrt(mean_squared_error(actual, predicted)),
        "corpus_balanced_rmse": float(np.mean(list(per_corpus.values()))),
        "r2": float(r2_score(actual, predicted)),
        "per_corpus_rmse": per_corpus,
        "errors": (predicted - actual).tolist(),
    }


def _bootstrap_improvement(
    rows: list[dict], predictions: dict, target: str, samples: int = 10_000
) -> dict:
    datasets = sorted({row["dataset"] for row in rows})
    by_dataset = {dataset: [row for row in rows if row["dataset"] == dataset] for dataset in datasets}
    rng = np.random.default_rng(2407)
    values = []
    for _ in range(samples):
        sampled = rng.choice(datasets, size=len(datasets), replace=True)
        baseline_errors = []
        combined_errors = []
        for dataset in sampled:
            for row in by_dataset[dataset]:
                key = (row["dataset"], row["stride"], row["configuration"])
                actual = row[target]
                baseline_errors.append(
                    (predictions[key]["non_fourier_only"][target] - actual) ** 2
                )
                combined_errors.append(
                    (predictions[key]["combined_compact"][target] - actual) ** 2
                )
        baseline_rmse = math.sqrt(float(np.mean(baseline_errors)))
        combined_rmse = math.sqrt(float(np.mean(combined_errors)))
        values.append((baseline_rmse - combined_rmse) / baseline_rmse)
    observed_baseline = _score(rows, predictions, "non_fourier_only", target)["rmse"]
    observed_combined = _score(rows, predictions, "combined_compact", target)["rmse"]
    return {
        "relative_rmse_improvement": (
            observed_baseline - observed_combined
        )
        / observed_baseline,
        "corpus_bootstrap_95_interval": [
            float(np.quantile(values, 0.025)),
            float(np.quantile(values, 0.975)),
        ],
        "bootstrap_samples": samples,
    }


def _intervention(rows: list[dict], predictions: dict, target: str) -> dict:
    actual_deltas = []
    predicted_deltas = []
    locality = []
    sign_hits = []
    primary = json.loads(PRIMARY_LOCK.read_text())
    profile_by_key = {
        (row["dataset"], row["stride"], row["configuration"]): row
        for row in primary["predictions"]
    }
    for row in rows:
        if row["dataset"] not in INTERVENTION_DATASETS or row["stride"] == 1:
            continue
        baseline = next(
            candidate
            for candidate in rows
            if candidate["dataset"] == row["dataset"]
            and candidate["configuration"] == row["configuration"]
            and candidate["stride"] == 1
        )
        key = (row["dataset"], row["stride"], row["configuration"])
        base_key = (baseline["dataset"], 1, baseline["configuration"])
        actual_delta = row[target] - baseline[target]
        predicted_delta = (
            predictions[key]["combined_compact"][target]
            - predictions[base_key]["combined_compact"][target]
        )
        locality_delta = (
            profile_by_key[key]["features"]["energy_weighted_log_radius"]
            - profile_by_key[base_key]["features"]["energy_weighted_log_radius"]
        )
        actual_deltas.append(actual_delta)
        predicted_deltas.append(predicted_delta)
        locality.append(locality_delta)
        sign_hits.append((actual_delta > 0) == (predicted_delta > 0))
    actual_array = np.asarray(actual_deltas)
    predicted_array = np.asarray(predicted_deltas)
    rho = float(
        np.corrcoef(rankdata(locality), rankdata(actual_deltas))[0, 1]
    )
    return {
        "n_paired_predictions": len(actual_deltas),
        "rmse": math.sqrt(mean_squared_error(actual_array, predicted_array)),
        "r2": float(r2_score(actual_array, predicted_array)),
        "sign_accuracy": float(np.mean(sign_hits)),
        "measured_locality_spearman": rho,
    }


def analyze() -> dict:
    rows = _actual_rows()
    predictions, lock_hashes = _prediction_map()
    natural = [row for row in rows if row["stride"] == 1]
    feature_sets = (
        "configuration_only",
        "non_fourier_only",
        "fourier_compact",
        "combined_compact",
    )
    scores = {
        target: {
            feature_set: _score(natural, predictions, feature_set, target)
            for feature_set in feature_sets
        }
        for target in TARGETS
    }
    improvements = {
        target: _bootstrap_improvement(natural, predictions, target)
        for target in TARGETS
    }
    interventions = {
        target: _intervention(rows, predictions, target) for target in TARGETS
    }
    gates = {
        target: (
            scores[target]["combined_compact"]["r2"] > 0.0
            and improvements[target]["relative_rmse_improvement"] >= 0.05
            and improvements[target]["corpus_bootstrap_95_interval"][0] > 0.0
        )
        for target in TARGETS
    }
    predictive = {
        target: (
            scores[target]["fourier_compact"]["r2"] > 0.0
            and scores[target]["combined_compact"]["rmse"]
            < scores[target]["configuration_only"]["rmse"]
        )
        for target in TARGETS
    }
    if all(gates.values()):
        verdict = "STRONG_SUPPORT_WITH_BASELINE_LOCK_TIMING_CAVEAT"
    elif all(predictive.values()):
        verdict = "PREDICTIVE_NOT_UNIQUELY_FOURIER"
    elif any(predictive.values()):
        verdict = "MIXED"
    else:
        verdict = "REFUTED"
    result = {
        "protocol_hash": json.loads(
            (ROOT / "configs/protocol_v2.4.json").read_text()
        )["protocol_hash"],
        "verdict": verdict,
        "prediction_lock_hashes": lock_hashes,
        "baseline_lock_timing_caveat": (
            "control-only predictions were locked after three of 270 cells began, "
            "before aggregate analysis; Fourier and combined predictions were locked "
            "before every cell"
        ),
        "n_unseen_corpora": len(DATASETS),
        "n_natural_prediction_rows": len(natural),
        "natural_scores": scores,
        "incremental_fourier_improvement": improvements,
        "intervention_prediction": interventions,
        "outcome_gates": gates,
        "rows": rows,
    }
    (OUT / "confirmatory_analysis.json").write_text(json.dumps(result, indent=2))
    print(
        json.dumps(
            {
                "verdict": verdict,
                "scores": {
                    target: {
                        feature_set: {
                            "rmse": values["rmse"],
                            "r2": values["r2"],
                        }
                        for feature_set, values in by_feature.items()
                    }
                    for target, by_feature in scores.items()
                },
                "improvements": improvements,
                "interventions": interventions,
            },
            indent=2,
        )
    )
    return result


if __name__ == "__main__":
    analyze()
