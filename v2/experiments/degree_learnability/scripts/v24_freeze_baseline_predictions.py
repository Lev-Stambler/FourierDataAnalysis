"""Lock omitted v2.4 control-only baselines before confirmatory analysis."""

from __future__ import annotations

import hashlib
import json
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
sys.path.insert(0, str(Path(__file__).parent.parent))

from v24_development_analysis import CONTROL_FEATURES, TARGETS, build_rows
from v24_freeze_predictions import CONFIGURATIONS, OUT, PREDICTION_PATH

from dlx.analysis.spectrum_predictor import fit_frozen_ridge, predict_frozen_ridge

PATH = OUT / "confirmatory_baseline_predictions.json"
HASH_PATH = OUT / "confirmatory_baseline_predictions.sha256"


def main() -> None:
    development = build_rows()
    prediction_rows = json.loads(PREDICTION_PATH.read_text())["predictions"]
    artifacts = {}
    predictions = [
        {
            "dataset": row["dataset"],
            "stride": row["stride"],
            "configuration": row["configuration"],
            "predictions": {},
        }
        for row in prediction_rows
    ]
    feature_sets = {"configuration_only": (), "non_fourier_only": CONTROL_FEATURES}
    for target in TARGETS:
        artifacts[target] = {}
        for feature_set, features in feature_sets.items():
            artifact = fit_frozen_ridge(
                development, target=target, feature_names=features
            )
            artifacts[target][feature_set] = artifact
            values = predict_frozen_ridge(artifact, prediction_rows)
            for row, value in zip(predictions, values, strict=True):
                row["predictions"].setdefault(feature_set, {})[target] = float(value)
    result = {
        "status": (
            "supplementary deterministic baseline lock; created after three of 270 "
            "planned cells had begun and before any aggregate outcome analysis"
        ),
        "configurations": list(CONFIGURATIONS),
        "primary_prediction_lock_hash": hashlib.sha256(
            PREDICTION_PATH.read_bytes()
        ).hexdigest(),
        "artifacts": artifacts,
        "predictions": predictions,
    }
    payload = (json.dumps(result, indent=2) + "\n").encode()
    if PATH.exists() and PATH.read_bytes() != payload:
        raise FileExistsError("refusing to overwrite a different baseline lock")
    digest = hashlib.sha256(payload).hexdigest()
    PATH.write_bytes(payload)
    HASH_PATH.write_text(digest + "\n")
    print(f"froze {len(predictions)} baseline predictions: sha256={digest}")


if __name__ == "__main__":
    main()
