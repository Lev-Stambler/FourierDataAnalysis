"""Score the frozen v2.6 corpus predictions after all 64 cells complete."""

from __future__ import annotations

import json
import math
import sys
from pathlib import Path

import numpy as np
from scipy.stats import spearmanr
from sklearn.metrics import mean_squared_error, r2_score

ROOT = Path(__file__).parent.parent
sys.path.insert(0, str(ROOT))

from dlx.analysis.spectrum_predictor import paired_corpus_bootstrap_improvement
from dlx.analysis.text_panel import TARGETS, median_cell_metrics
from dlx.protocol.confirmation import confirmation_cells
from dlx.protocol.frozen import load_frozen_protocol, verify_hash_lock

OUT = ROOT / "runs/local/v26_sampled_locality"


def _rows(protocol: dict) -> list[dict]:
    cells = json.loads((OUT / "remote_results.json").read_text())
    expected = confirmation_cells(protocol)
    expected_ids = {f"V26/{row['dataset']}/s{row['seed']}" for row in expected}
    if len(cells) != 64 or {row["cell_id"] for row in cells} != expected_ids:
        raise ValueError("v2.6 training grid is incomplete or contains extra cells")
    rows = []
    for source in protocol["corpora"]["sources"]:
        selected = [row for row in cells if row["dataset"] == source["id"]]
        if len(selected) != 2 or {row["seed"] for row in selected} != {0, 1}:
            raise ValueError(f"missing seeds for {source['id']}")
        rows.append(
            {
                "dataset": source["id"],
                "stratum": source["stratum"],
                **median_cell_metrics(selected),
            }
        )
    return rows


def _score(actual: np.ndarray, predicted: np.ndarray) -> dict:
    rho, p_value = spearmanr(actual, predicted)
    return {
        "rmse": math.sqrt(mean_squared_error(actual, predicted)),
        "r2": float(r2_score(actual, predicted)),
        "spearman_rho": float(rho),
        "spearman_two_sided_p": float(p_value),
    }


def analyze() -> dict:
    protocol = load_frozen_protocol(ROOT / "configs/protocol_v2.6.json")
    data_hash = verify_hash_lock(
        OUT / "data_manifest.json", OUT / "data_manifest.sha256"
    )
    profile_hash = verify_hash_lock(
        OUT / "profile_manifest.json", OUT / "profile_manifest.sha256"
    )
    prediction_hash = verify_hash_lock(
        OUT / "predictions.json", OUT / "predictions.sha256"
    )
    prediction_lock = json.loads((OUT / "predictions.json").read_text())
    predictions = {
        row["dataset"]: row["predictions"] for row in prediction_lock["predictions"]
    }
    rows = _rows(protocol)
    scores = {}
    improvements = {}
    for target in TARGETS:
        actual = np.asarray([row[target] for row in rows], dtype=float)
        by_model = {
            label: np.asarray(
                [predictions[row["dataset"]][label][target] for row in rows],
                dtype=float,
            )
            for label in protocol["prediction"]["feature_sets"]
        }
        scores[target] = {
            label: _score(actual, values) for label, values in by_model.items()
        }
        improvements[target] = {
            "energy_degree_vs_controls": paired_corpus_bootstrap_improvement(
                actual,
                by_model["controls"],
                by_model["controls_energy_degree"],
                seed=2603,
            ),
            "locality_increment": paired_corpus_bootstrap_improvement(
                actual,
                by_model["controls_energy_degree"],
                by_model["controls_energy_degree_locality"],
                seed=2604,
            ),
        }
    primary = "final_ce_fraction"
    locality = improvements[primary]["locality_increment"]
    energy = improvements[primary]["energy_degree_vs_controls"]
    spectral_r2 = scores[primary]["controls_energy_degree_locality"]["r2"]
    if (
        locality["relative_rmse_improvement"] > 0
        and locality["corpus_bootstrap_95_interval"][0] > 0
    ):
        verdict = "LOCALITY_SUPPORTED"
    elif energy["relative_rmse_improvement"] > 0:
        verdict = "SPECTRUM_SUPPORTED_LOCALITY_UNRESOLVED"
    elif spectral_r2 > 0:
        verdict = "PREDICTIVE_NOT_INCREMENTAL"
    else:
        verdict = "NOT_SUPPORTED"
    result = {
        "protocol_hash": protocol["protocol_hash"],
        "data_manifest_hash": data_hash,
        "profile_manifest_hash": profile_hash,
        "prediction_lock_hash": prediction_hash,
        "verdict": verdict,
        "n_unseen_corpora": len(rows),
        "primary_target": primary,
        "secondary_target": "normalized_curve_area",
        "scores": scores,
        "paired_corpus_bootstrap_improvements": improvements,
        "rows": rows,
    }
    (OUT / "analysis.json").write_text(json.dumps(result, indent=2) + "\n")
    return result


if __name__ == "__main__":
    result = analyze()
    print(
        json.dumps({"verdict": result["verdict"], "scores": result["scores"]}, indent=2)
    )
