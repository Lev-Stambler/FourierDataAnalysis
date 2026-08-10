"""Fit old-corpus OLS models and freeze v2.6 predictions before training."""

from __future__ import annotations

import json
import sys
from pathlib import Path

ROOT = Path(__file__).parent.parent
sys.path.insert(0, str(ROOT))

from dlx.analysis.spectrum_predictor import fit_frozen_ols, predict_frozen_ols
from dlx.analysis.text_panel import (
    TARGETS,
    attach_geometric_sampled_features,
    load_v24_confirmation_rows,
    load_v24_development_rows,
)
from dlx.protocol.frozen import (
    load_frozen_protocol,
    verify_hash_lock,
    write_hash_once,
    write_json_once,
)

OUT = ROOT / "runs/local/v26_sampled_locality"
V25_PROFILES = ROOT / "runs/local/v25_kiss_diagnostic/profiles"
PROTOCOL_PATH = ROOT / "configs/protocol_v2.6.json"
PREDICTION_PATH = OUT / "predictions.json"
PREDICTION_HASH_PATH = OUT / "predictions.sha256"


def _prior_rows() -> list[dict]:
    rows = load_v24_development_rows(ROOT) + load_v24_confirmation_rows(ROOT)
    selected = [
        row
        for row in rows
        if int(row["stride"]) == 1 and row["configuration"] == "learned_absolute_d64_l2"
    ]
    if len(selected) != 22 or len({row["dataset"] for row in selected}) != 22:
        raise ValueError("expected 22 independent comparable prior corpora")
    return attach_geometric_sampled_features(selected, V25_PROFILES)


def _new_rows(protocol: dict) -> list[dict]:
    rows = []
    for source in protocol["corpora"]["sources"]:
        profile = json.loads((OUT / "profiles" / f"{source['id']}.json").read_text())
        if profile["protocol_hash"] != protocol["protocol_hash"]:
            raise ValueError(f"stale profile: {source['id']}")
        rows.append(
            {
                "dataset": source["id"],
                "configuration": "learned_absolute_d64_l2",
                "features": profile["features"],
            }
        )
    return rows


def main() -> None:
    protocol = load_frozen_protocol(PROTOCOL_PATH)
    data_hash = verify_hash_lock(
        OUT / "data_manifest.json", OUT / "data_manifest.sha256"
    )
    profile_hash = verify_hash_lock(
        OUT / "profile_manifest.json", OUT / "profile_manifest.sha256"
    )
    train_rows = _prior_rows()
    new_rows = _new_rows(protocol)
    artifacts = {}
    artifact_hashes = {}
    predictions = [dict(row) for row in new_rows]
    model_dir = OUT / "models"
    for target in TARGETS:
        artifacts[target] = {}
        artifact_hashes[target] = {}
        for label, names in protocol["prediction"]["feature_sets"].items():
            artifact = fit_frozen_ols(
                train_rows, target=target, feature_names=tuple(names)
            )
            artifact.update(
                {
                    "protocol_hash": protocol["protocol_hash"],
                    "data_manifest_hash": data_hash,
                    "profile_manifest_hash": profile_hash,
                }
            )
            path = model_dir / f"{label}__{target}.json"
            artifact_hashes[target][label] = write_json_once(path, artifact)
            artifacts[target][label] = artifact
            values = predict_frozen_ols(artifact, new_rows)
            for row, value in zip(predictions, values, strict=True):
                row.setdefault("predictions", {}).setdefault(label, {})[target] = float(
                    value
                )
    result = {
        "status": "predictions frozen before any v2.6 training",
        "protocol_hash": protocol["protocol_hash"],
        "data_manifest_hash": data_hash,
        "profile_manifest_hash": profile_hash,
        "n_training_corpora": len(train_rows),
        "n_prediction_corpora": len(new_rows),
        "artifact_hashes": artifact_hashes,
        "predictions": predictions,
    }
    digest = write_json_once(PREDICTION_PATH, result)
    write_hash_once(PREDICTION_HASH_PATH, digest)
    print(f"froze {len(predictions)} corpus predictions: {digest}")


if __name__ == "__main__":
    main()
