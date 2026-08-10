"""Reduce corrected development cells and freeze v2.8 confirmation predictions."""

from __future__ import annotations

import json
import sys
from pathlib import Path

ROOT = Path(__file__).parent.parent
sys.path.insert(0, str(ROOT))

from dlx.analysis.floor_independent import normalized_learning_time
from dlx.analysis.spectrum_predictor import fit_frozen_ols, predict_frozen_ols
from dlx.analysis.text_panel import median_cell_metrics
from dlx.protocol.frozen import (
    load_frozen_protocol,
    verify_hash_lock,
    write_hash_once,
    write_json_once,
)

OUT = ROOT / "runs/local/v28_random_windows"


def main() -> dict:
    protocol = load_frozen_protocol(ROOT / "configs/protocol_v2.8.json")
    data_hash = verify_hash_lock(
        OUT / "data_manifest.json", OUT / "data_manifest.sha256"
    )
    profile_hash = verify_hash_lock(
        OUT / "profile_manifest.json", OUT / "profile_manifest.sha256"
    )
    manifest = json.loads((OUT / "data_manifest.json").read_text())
    cells = json.loads((OUT / "development_results.json").read_text())
    development = [row for row in manifest["corpora"] if row["panel"] == "development"]
    expected = {
        f"V28D/{row['dataset']}/s{seed}"
        for row in development
        for seed in protocol["training"]["seeds"]
    }
    if len(cells) != 108 or {row["cell_id"] for row in cells} != expected:
        raise ValueError("corrected development grid is incomplete")
    rows = []
    for source in development:
        selected = [row for row in cells if row["dataset"] == source["dataset"]]
        metrics = median_cell_metrics(selected)
        target = normalized_learning_time(
            metrics["normalized_curve_area"],
            metrics["final_ce_fraction"],
            maximum_final_fraction=protocol["target"][
                "maximum_identifiable_final_ce_fraction"
            ],
        )
        profile = json.loads(
            (OUT / "profiles" / f"{source['dataset']}.json").read_text()
        )
        rows.append(
            {
                "dataset": source["dataset"],
                "configuration": "random_windows_learned_absolute_d64_l2",
                "features": profile["features"],
                **metrics,
                "normalized_learning_time": target,
            }
        )
    development_result = {"protocol_hash": protocol["protocol_hash"], "rows": rows}
    development_hash = write_json_once(
        OUT / "development_analysis.json", development_result
    )
    write_hash_once(OUT / "development_analysis.sha256", development_hash)
    confirmation = []
    for source in manifest["corpora"]:
        if source["panel"] != "confirmation":
            continue
        profile = json.loads(
            (OUT / "profiles" / f"{source['dataset']}.json").read_text()
        )
        confirmation.append(
            {
                "dataset": source["dataset"],
                "stratum": source["stratum"],
                "configuration": "random_windows_learned_absolute_d64_l2",
                "features": profile["features"],
            }
        )
    predictions = [dict(row) for row in confirmation]
    artifacts = {}
    for label, names in protocol["prediction"]["feature_sets"].items():
        artifact = fit_frozen_ols(
            rows, target="normalized_learning_time", feature_names=tuple(names)
        )
        artifact.update(
            {
                "protocol_hash": protocol["protocol_hash"],
                "data_manifest_hash": data_hash,
                "profile_manifest_hash": profile_hash,
                "development_analysis_hash": development_hash,
            }
        )
        artifacts[label] = artifact
        write_json_once(OUT / "models" / f"{label}.json", artifact)
        values = predict_frozen_ols(artifact, confirmation)
        for row, value in zip(predictions, values, strict=True):
            row.setdefault("predictions", {})[label] = float(value)
    result = {
        "status": "v2.8 predictions frozen before corrected confirmation training",
        "protocol_hash": protocol["protocol_hash"],
        "data_manifest_hash": data_hash,
        "profile_manifest_hash": profile_hash,
        "development_analysis_hash": development_hash,
        "development_locality_coefficient": artifacts["marginal_locality"][
            "coefficients"
        ][0],
        "predictions": predictions,
    }
    digest = write_json_once(OUT / "predictions.json", result)
    write_hash_once(OUT / "predictions.sha256", digest)
    print(
        json.dumps(
            {
                "prediction_hash": digest,
                "coefficient": result["development_locality_coefficient"],
            },
            indent=2,
        )
    )
    return result


if __name__ == "__main__":
    main()
