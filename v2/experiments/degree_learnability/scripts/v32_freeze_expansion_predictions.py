"""Freeze 48-corpus predictions for the prospective v3.2 expansion."""

from __future__ import annotations

import json
import math
from pathlib import Path

from dlx.analysis.architecture_matching import (
    fit_architecture_ols,
    predict_architecture_ols,
)
from dlx.analysis.character_response import architecture_spectrum_overlap
from dlx.protocol.frozen import (
    file_sha256,
    load_frozen_protocol,
    verify_hash_lock,
    write_hash_once,
    write_json_once,
)

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs/local/v30_architecture_spectrum"


def _profile_row(source: dict, architecture: str, response: dict) -> dict:
    profile = json.loads((OUT / "profiles" / f"{source['dataset']}.json").read_text())
    overlap = architecture_spectrum_overlap(
        profile["support_energy"], response["architecture_hardness"][architecture]
    )
    if profile["data_sha256"] != source["byte_stream_sha256"] or not math.isfinite(
        overlap
    ):
        raise ValueError(f"invalid profile for {source['dataset']}")
    return {
        "dataset": source["dataset"],
        "stratum": source["stratum"],
        "architecture": architecture,
        "features": {**profile["features"], "fourier_ce_overlap": overlap},
    }


def main() -> None:
    protocol = load_frozen_protocol(ROOT / "configs/protocol_v3.2.json")
    measurement = load_frozen_protocol(ROOT / "configs/protocol_v3.1.json")
    expected = protocol["locked_inputs"]
    locks = {
        "data_manifest_sha256": verify_hash_lock(
            OUT / "data_manifest.json", OUT / "data_manifest.sha256"
        ),
        "profile_manifest_sha256": verify_hash_lock(
            OUT / "profile_manifest.json", OUT / "profile_manifest.sha256"
        ),
        "fourier_ce_response_sha256": verify_hash_lock(
            OUT / "fourier_ce_kernel.json", OUT / "fourier_ce_kernel.sha256"
        ),
        "pilot_analysis_sha256": verify_hash_lock(
            OUT / "pilot_analysis.json", OUT / "pilot_analysis.sha256"
        ),
        "pilot_diagnostics_sha256": verify_hash_lock(
            OUT / "pilot_diagnostics.json", OUT / "pilot_diagnostics.sha256"
        ),
        "pilot_results_sha256": file_sha256(OUT / "pilot_results.json"),
    }
    if locks != expected:
        raise ValueError("v3.2 locked input mismatch")
    if measurement["protocol_hash"] != protocol["measurement_protocol"]["protocol_hash"]:
        raise ValueError("measurement protocol mismatch")

    pilot = json.loads((OUT / "pilot_analysis.json").read_text())
    development_rows = pilot["rows"]
    target = protocol["primary_comparison"]["target"]
    ordinary = tuple(protocol["primary_comparison"]["baseline_features"])
    strong = tuple(protocol["secondary_comparison"]["baseline_features"])
    specifications = {
        "ordinary_baseline": ordinary,
        "ordinary_plus_fourier": ordinary + ("fourier_ce_overlap",),
        "strong_baseline": strong,
        "strong_plus_fourier": strong + ("fourier_ce_overlap",),
    }
    model_dir = OUT / "expansion_models"
    model_dir.mkdir(parents=True, exist_ok=True)
    models = {}
    for label, features in specifications.items():
        model = fit_architecture_ols(
            development_rows, target=target, numeric_features=features
        )
        model.update(
            {
                "expansion_protocol_hash": protocol["protocol_hash"],
                "measurement_protocol_hash": measurement["protocol_hash"],
                "locked_inputs": locks,
            }
        )
        path = model_dir / f"{label}.json"
        write_json_once(path, model)
        models[label] = model

    manifest = json.loads((OUT / "data_manifest.json").read_text())
    response = json.loads((OUT / "fourier_ce_kernel.json").read_text())
    architectures = [row["id"] for row in measurement["architectures"]]
    expansion_rows = [
        _profile_row(source, architecture, response)
        for source in manifest["corpora"]
        if source["panel"] == "confirmation"
        for architecture in architectures
    ]
    if len(expansion_rows) != 240:
        raise ValueError("expected 240 expansion corpus-architecture rows")
    predictions = [
        {
            "dataset": row["dataset"],
            "stratum": row["stratum"],
            "architecture": row["architecture"],
            "predictions": {},
        }
        for row in expansion_rows
    ]
    for label, model in models.items():
        values = predict_architecture_ols(model, expansion_rows)
        for prediction, value in zip(predictions, values, strict=True):
            prediction["predictions"][label] = float(value)
    artifact = {
        "status": "predictions frozen before any v3.2 expansion training outcome",
        "protocol_hash": protocol["protocol_hash"],
        "measurement_protocol_hash": measurement["protocol_hash"],
        "locked_inputs": locks,
        "target": target,
        "models": {
            label: file_sha256(model_dir / f"{label}.json") for label in models
        },
        "predictions": predictions,
    }
    digest = write_json_once(OUT / "expansion_predictions.json", artifact)
    write_hash_once(OUT / "expansion_predictions.sha256", digest)
    print(
        json.dumps(
            {
                "protocol_hash": protocol["protocol_hash"],
                "prediction_hash": digest,
                "predictions": len(predictions),
                "outcomes_observed": 0,
            },
            indent=2,
        )
    )


if __name__ == "__main__":
    main()
