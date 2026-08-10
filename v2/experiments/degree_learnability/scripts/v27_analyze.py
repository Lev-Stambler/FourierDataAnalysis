"""Score the frozen v2.7 predictions after the 48 confirmation cells finish."""

from __future__ import annotations

import itertools
import json
import math
import sys
from pathlib import Path

import numpy as np
from scipy.stats import pearsonr, spearmanr

ROOT = Path(__file__).parent.parent
sys.path.insert(0, str(ROOT))

from dlx.analysis.floor_independent import normalized_learning_time
from dlx.analysis.text_panel import median_cell_metrics
from dlx.protocol.confirmation import confirmation_cells
from dlx.protocol.frozen import load_frozen_protocol, verify_hash_lock

OUT = ROOT / "runs/local/v27_marginal_locality"
PROTOCOL_PATH = ROOT / "configs/protocol_v2.7.json"


def _score(actual: np.ndarray, predicted: np.ndarray) -> dict:
    residual = actual - predicted
    denominator = float(np.sum((actual - actual.mean()) ** 2))
    constant_prediction = float(np.std(predicted)) == 0.0
    rho = None if constant_prediction else spearmanr(actual, predicted)
    correlation = None if constant_prediction else pearsonr(actual, predicted)
    return {
        "rmse": float(np.sqrt(np.mean(residual**2))),
        "r2": float(1.0 - np.sum(residual**2) / denominator),
        "spearman_rho": None if rho is None else float(rho.statistic),
        "spearman_two_sided_p": None if rho is None else float(rho.pvalue),
        "pearson_r": None if correlation is None else float(correlation.statistic),
        "pearson_two_sided_p": (
            None if correlation is None else float(correlation.pvalue)
        ),
    }


def _confirmation_rows(protocol: dict, prediction_lock: dict) -> list[dict]:
    cells = json.loads((OUT / "remote_results.json").read_text())
    expected = confirmation_cells(protocol)
    expected_ids = {f"V27/{row['dataset']}/s{row['seed']}" for row in expected}
    observed_ids = {row["cell_id"] for row in cells}
    if len(cells) != len(expected) or observed_ids != expected_ids:
        missing = sorted(expected_ids - observed_ids)
        extra = sorted(observed_ids - expected_ids)
        raise ValueError(f"incomplete training grid; missing={missing}, extra={extra}")
    predictions = {
        row["dataset"]: row["predictions"] for row in prediction_lock["predictions"]
    }
    rows = []
    threshold = protocol["target"]["maximum_identifiable_final_ce_fraction"]
    for source in protocol["corpora"]["sources"]:
        selected = [row for row in cells if row["dataset"] == source["id"]]
        if len(selected) != 2 or {row["seed"] for row in selected} != {0, 1}:
            raise ValueError(f"missing exact seed pair for {source['id']}")
        medians = median_cell_metrics(selected)
        final_fraction = medians["final_ce_fraction"]
        row = {
            "dataset": source["id"],
            "stratum": source["stratum"],
            **medians,
            "target_identifiable": final_fraction < threshold,
            "predictions": predictions[source["id"]],
        }
        if row["target_identifiable"]:
            row["normalized_learning_time"] = normalized_learning_time(
                medians["normalized_curve_area"],
                final_fraction,
                maximum_final_fraction=threshold,
            )
        else:
            row["normalized_learning_time"] = None
        rows.append(row)
    return rows


def _stratified_rmse_bootstrap(rows: list[dict], *, samples: int, seed: int) -> dict:
    strata = sorted({row["stratum"] for row in rows})
    blocks = [
        [index for index, row in enumerate(rows) if row["stratum"] == name]
        for name in strata
    ]
    if {len(block) for block in blocks} != {4}:
        raise ValueError("the locked bootstrap requires four corpora per stratum")
    actual = np.asarray([row["normalized_learning_time"] for row in rows])
    baseline = np.asarray([row["predictions"]["intercept_only"] for row in rows])
    locality = np.asarray([row["predictions"]["marginal_locality"] for row in rows])
    rng = np.random.default_rng(seed)
    choices = rng.integers(0, 4, size=(samples, len(blocks), 4))
    block_array = np.asarray(blocks)
    sampled = np.take_along_axis(
        np.broadcast_to(block_array, choices.shape), choices, axis=2
    ).reshape(samples, -1)
    actual_sampled = actual[sampled]
    rmse_baseline = np.sqrt(np.mean((actual_sampled - baseline[sampled]) ** 2, axis=1))
    rmse_locality = np.sqrt(np.mean((actual_sampled - locality[sampled]) ** 2, axis=1))
    improvements = (rmse_baseline - rmse_locality) / rmse_baseline
    point_baseline = float(np.sqrt(np.mean((actual - baseline) ** 2)))
    point_locality = float(np.sqrt(np.mean((actual - locality) ** 2)))
    return {
        "relative_rmse_improvement": (point_baseline - point_locality) / point_baseline,
        "stratified_bootstrap_95_interval": [
            float(np.quantile(improvements, 0.025)),
            float(np.quantile(improvements, 0.975)),
        ],
        "samples": samples,
        "seed": seed,
        "strata": strata,
    }


def _blocked_rank_test(rows: list[dict], *, permutations: int, seed: int) -> dict:
    strata = sorted({row["stratum"] for row in rows})
    observed = []
    null_values = []
    for name in strata:
        block = [row for row in rows if row["stratum"] == name]
        feature = np.asarray([row["predictions"]["marginal_locality"] for row in block])
        target = np.asarray([row["normalized_learning_time"] for row in block])
        observed.append(float(spearmanr(feature, target).statistic))
        null_values.append(
            np.asarray(
                [
                    spearmanr(feature, target[list(order)]).statistic
                    for order in itertools.permutations(range(4))
                ],
                dtype=float,
            )
        )
    statistic = float(np.mean(observed))
    rng = np.random.default_rng(seed)
    draws = rng.integers(0, math.factorial(4), size=(permutations, len(strata)))
    null = np.mean(
        np.column_stack(
            [values[draws[:, index]] for index, values in enumerate(null_values)]
        ),
        axis=1,
    )
    p_value = float((1 + np.count_nonzero(null >= statistic)) / (permutations + 1))
    return {
        "statistic": "mean within-stratum Spearman rho",
        "mean_rho": statistic,
        "within_stratum_rhos": dict(zip(strata, observed, strict=True)),
        "one_sided_permutation_p": p_value,
        "permutations": permutations,
        "seed": seed,
        "alternative": "greater",
    }


def analyze() -> dict:
    protocol = load_frozen_protocol(PROTOCOL_PATH)
    locks = {
        "data_manifest_hash": verify_hash_lock(
            OUT / "data_manifest.json", OUT / "data_manifest.sha256"
        ),
        "profile_manifest_hash": verify_hash_lock(
            OUT / "profile_manifest.json", OUT / "profile_manifest.sha256"
        ),
        "prediction_lock_hash": verify_hash_lock(
            OUT / "predictions.json", OUT / "predictions.sha256"
        ),
    }
    prediction_lock = json.loads((OUT / "predictions.json").read_text())
    rows = _confirmation_rows(protocol, prediction_lock)
    unidentified = [row["dataset"] for row in rows if not row["target_identifiable"]]
    if unidentified:
        result = {
            "protocol_hash": protocol["protocol_hash"],
            **locks,
            "verdict": "TARGET_UNIDENTIFIABLE",
            "unidentifiable_corpora": unidentified,
            "rows": rows,
        }
    else:
        actual = np.asarray([row["normalized_learning_time"] for row in rows])
        predictions = {
            label: np.asarray([row["predictions"][label] for row in rows])
            for label in protocol["prediction"]["feature_sets"]
        }
        scores = {
            label: _score(actual, values) for label, values in predictions.items()
        }
        bootstrap_config = protocol["decision"]["rmse_bootstrap"]
        bootstrap = _stratified_rmse_bootstrap(
            rows,
            samples=bootstrap_config["samples"],
            seed=bootstrap_config["seed"],
        )
        rank_config = protocol["decision"]["rank_test"]
        rank = _blocked_rank_test(
            rows,
            permutations=rank_config["permutations"],
            seed=rank_config["seed"],
        )
        predictive_gate = (
            bootstrap["relative_rmse_improvement"] > 0.0
            and bootstrap["stratified_bootstrap_95_interval"][0] > 0.0
        )
        rank_gate = (
            rank["mean_rho"] > 0.0
            and rank["one_sided_permutation_p"] <= rank_config["maximum_p"]
        )
        if predictive_gate and rank_gate:
            verdict = "CONFIRMED"
        elif predictive_gate:
            verdict = "PREDICTIVE_ONLY"
        elif rank_gate:
            verdict = "ASSOCIATION_ONLY"
        else:
            verdict = "NOT_CONFIRMED"
        result = {
            "protocol_hash": protocol["protocol_hash"],
            **locks,
            "verdict": verdict,
            "n_confirmation_corpora": len(rows),
            "target": protocol["target"],
            "primary_gates": {
                "predictive_rmse_gate": predictive_gate,
                "blocked_rank_gate": rank_gate,
            },
            "scores": scores,
            "stratified_paired_bootstrap": bootstrap,
            "blocked_rank_test": rank,
            "rows": rows,
        }
    (OUT / "analysis.json").write_text(
        json.dumps(result, indent=2, allow_nan=False) + "\n"
    )
    return result


if __name__ == "__main__":
    analyzed = analyze()
    print(
        json.dumps(
            {
                key: analyzed[key]
                for key in ("verdict", "primary_gates", "scores")
                if key in analyzed
            },
            indent=2,
        )
    )
