"""Fit the v2.7 one-feature OLS and freeze predictions before training."""

from __future__ import annotations

import json
import sys
from pathlib import Path

ROOT = Path(__file__).parent.parent
sys.path.insert(0, str(ROOT))

from dlx.analysis.spectrum_predictor import fit_frozen_ols, predict_frozen_ols
from dlx.protocol.frozen import (
    file_sha256,
    load_frozen_protocol,
    verify_hash_lock,
    write_hash_once,
    write_json_once,
)

OUT = ROOT / "runs/local/v27_marginal_locality"
PROTOCOL_PATH = ROOT / "configs/protocol_v2.7.json"
TARGET = "normalized_learning_time"


def main() -> dict:
    protocol = load_frozen_protocol(PROTOCOL_PATH)
    development_path = ROOT / protocol["development"]["manifest"]
    if file_sha256(development_path) != protocol["development"]["manifest_sha256"]:
        raise ValueError("development manifest changed after protocol freeze")
    development = json.loads(development_path.read_text())["rows"]
    data_hash = verify_hash_lock(
        OUT / "data_manifest.json", OUT / "data_manifest.sha256"
    )
    profile_hash = verify_hash_lock(
        OUT / "profile_manifest.json", OUT / "profile_manifest.sha256"
    )
    rows = []
    for source in protocol["corpora"]["sources"]:
        profile = json.loads((OUT / "profiles" / f"{source['id']}.json").read_text())
        rows.append(
            {
                "dataset": source["id"],
                "stratum": source["stratum"],
                "configuration": "learned_absolute_d64_l2",
                "features": profile["features"],
            }
        )
    artifacts = {}
    artifact_hashes = {}
    predictions = [dict(row) for row in rows]
    for label, names in protocol["prediction"]["feature_sets"].items():
        artifact = fit_frozen_ols(
            development, target=TARGET, feature_names=tuple(names)
        )
        artifact.update(
            {
                "protocol_hash": protocol["protocol_hash"],
                "development_manifest_hash": protocol["development"]["manifest_sha256"],
                "data_manifest_hash": data_hash,
                "profile_manifest_hash": profile_hash,
            }
        )
        path = OUT / "models" / f"{label}__{TARGET}.json"
        artifact_hashes[label] = write_json_once(path, artifact)
        artifacts[label] = artifact
        values = predict_frozen_ols(artifact, rows)
        for row, value in zip(predictions, values, strict=True):
            row.setdefault("predictions", {})[label] = float(value)
    coefficient = artifacts["marginal_locality"]["coefficients"][0]
    if coefficient <= 0.0:
        raise ValueError("development locality coefficient is not positive")
    result = {
        "status": "v2.7 predictions frozen before any confirmation training",
        "protocol_hash": protocol["protocol_hash"],
        "development_manifest_hash": protocol["development"]["manifest_sha256"],
        "data_manifest_hash": data_hash,
        "profile_manifest_hash": profile_hash,
        "target": TARGET,
        "positive_development_coefficient": coefficient,
        "artifact_hashes": artifact_hashes,
        "predictions": predictions,
    }
    digest = write_json_once(OUT / "predictions.json", result)
    write_hash_once(OUT / "predictions.sha256", digest)
    print(f"froze {len(predictions)} predictions: {digest}")
    return result


if __name__ == "__main__":
    main()
