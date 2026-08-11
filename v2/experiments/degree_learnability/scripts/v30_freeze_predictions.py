"""Analyze the Fourier-CE pilot and freeze confirmation predictions."""

from __future__ import annotations

import json
import math
from pathlib import Path

import numpy as np

from dlx.analysis.architecture_matching import (
    fit_architecture_ols,
    grouped_loco_predictions,
    predict_architecture_ols,
    prediction_scores,
)
from dlx.analysis.character_response import architecture_spectrum_overlap
from dlx.analysis.text_panel import median_cell_metrics
from dlx.protocol.frozen import (
    file_sha256,
    load_frozen_protocol,
    verify_hash_lock,
    write_hash_once,
    write_json_once,
)

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs/local/v30_architecture_spectrum"


def _profile_row(source: dict, architecture: str, kernel: dict) -> dict:
    profile = json.loads((OUT / "profiles" / f"{source['dataset']}.json").read_text())
    if profile["data_sha256"] != source["byte_stream_sha256"]:
        raise ValueError(f"profile/data mismatch for {source['dataset']}")
    overlap = architecture_spectrum_overlap(
        profile["support_energy"], kernel["architecture_hardness"][architecture]
    )
    if not math.isfinite(overlap):
        raise ValueError("architecture-spectrum overlap must be finite")
    return {
        "dataset": source["dataset"],
        "stratum": source["stratum"],
        "architecture": architecture,
        "features": {
            **profile["features"],
            "fourier_ce_overlap": overlap,
        },
    }


def main() -> dict:
    protocol = load_frozen_protocol(ROOT / "configs/protocol_v3.1.json")
    locks = {
        "data_manifest_hash": verify_hash_lock(
            OUT / "data_manifest.json", OUT / "data_manifest.sha256"
        ),
        "profile_manifest_hash": verify_hash_lock(
            OUT / "profile_manifest.json", OUT / "profile_manifest.sha256"
        ),
        "fourier_ce_kernel_hash": verify_hash_lock(
            OUT / "fourier_ce_kernel.json", OUT / "fourier_ce_kernel.sha256"
        ),
    }
    manifest = json.loads((OUT / "data_manifest.json").read_text())
    kernel = json.loads((OUT / "fourier_ce_kernel.json").read_text())
    cells_path = OUT / "pilot_results.json"
    cells = json.loads(cells_path.read_text())
    pilot_sources = [row for row in manifest["corpora"] if row["panel"] == "pilot"]
    architectures = [row["id"] for row in protocol["architectures"]]
    expected = {
        f"V31P/{source['dataset']}/{architecture}/s{seed}"
        for source in pilot_sources
        for architecture in architectures
        for seed in protocol["natural_training"]["seeds"]
    }
    if len(cells) != len(expected) or {row["cell_id"] for row in cells} != expected:
        raise ValueError(f"pilot grid is incomplete: {len(cells)}/{len(expected)}")

    rows = []
    for source in pilot_sources:
        for architecture in architectures:
            selected = [
                row
                for row in cells
                if row["dataset"] == source["dataset"]
                and row["architecture"] == architecture
            ]
            rows.append(
                {
                    **_profile_row(source, architecture, kernel),
                    **median_cell_metrics(selected),
                }
            )
    target = protocol["natural_training"]["primary_target"]
    baseline_features = tuple(protocol["prediction"]["strong_baseline_features"])
    matched_features = baseline_features + ("fourier_ce_overlap",)
    actual = np.asarray([row[target] for row in rows], dtype=float)
    baseline_loco = grouped_loco_predictions(
        rows, target=target, numeric_features=baseline_features
    )
    matched_loco = grouped_loco_predictions(
        rows, target=target, numeric_features=matched_features
    )
    baseline_score = prediction_scores(actual, baseline_loco)
    matched_score = prediction_scores(actual, matched_loco)
    improvement = (baseline_score["rmse"] - matched_score["rmse"]) / baseline_score[
        "rmse"
    ]
    continuation = improvement > 0.0
    for row, baseline_value, matched_value in zip(
        rows, baseline_loco, matched_loco, strict=True
    ):
        row["grouped_loco_predictions"] = {
            "strong_baseline": float(baseline_value),
            "fourier_ce_matched": float(matched_value),
        }
    pilot_artifact = {
        "status": "pilot evaluated before confirmation training",
        "protocol_hash": protocol["protocol_hash"],
        **locks,
        "pilot_results_sha256": file_sha256(cells_path),
        "target": target,
        "strong_baseline_features": list(baseline_features),
        "matched_features": list(matched_features),
        "grouped_loco": {
            "strong_baseline": baseline_score,
            "fourier_ce_matched": matched_score,
            "relative_rmse_improvement": improvement,
        },
        "continuation_gate_passed": continuation,
        "rows": rows,
    }
    pilot_hash = write_json_once(OUT / "pilot_analysis.json", pilot_artifact)
    write_hash_once(OUT / "pilot_analysis.sha256", pilot_hash)
    if not continuation:
        print(json.dumps(pilot_artifact["grouped_loco"], indent=2))
        print("continuation gate: FAILED")
        return pilot_artifact

    artifacts = {}
    for label, features in (
        ("strong_baseline", baseline_features),
        ("fourier_ce_matched", matched_features),
    ):
        artifact = fit_architecture_ols(rows, target=target, numeric_features=features)
        artifact.update(
            {
                "protocol_hash": protocol["protocol_hash"],
                **locks,
                "pilot_analysis_hash": pilot_hash,
            }
        )
        artifacts[label] = artifact
        write_json_once(OUT / "models" / f"{label}.json", artifact)

    confirmation_rows = []
    for source in manifest["corpora"]:
        if source["panel"] != "confirmation":
            continue
        for architecture in architectures:
            confirmation_rows.append(_profile_row(source, architecture, kernel))
    predictions = [
        {
            "dataset": row["dataset"],
            "stratum": row["stratum"],
            "architecture": row["architecture"],
            "predictions": {},
        }
        for row in confirmation_rows
    ]
    for label, artifact in artifacts.items():
        values = predict_architecture_ols(artifact, confirmation_rows)
        for row, value in zip(predictions, values, strict=True):
            row["predictions"][label] = float(value)
    result = {
        "status": "source-disjoint confirmation predictions frozen before training",
        "protocol_hash": protocol["protocol_hash"],
        **locks,
        "pilot_analysis_hash": pilot_hash,
        "models": {
            label: file_sha256(OUT / "models" / f"{label}.json") for label in artifacts
        },
        "predictions": predictions,
    }
    digest = write_json_once(OUT / "predictions.json", result)
    write_hash_once(OUT / "predictions.sha256", digest)
    print(
        json.dumps(
            {
                "grouped_loco": pilot_artifact["grouped_loco"],
                "continuation_gate_passed": continuation,
                "prediction_hash": digest,
                "confirmation_predictions": len(predictions),
            },
            indent=2,
        )
    )
    return result


if __name__ == "__main__":
    main()
