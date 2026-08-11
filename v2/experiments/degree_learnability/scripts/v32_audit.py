"""Independent audit for the prospective v3.2 corpus expansion."""

from __future__ import annotations

import json
from pathlib import Path

from dlx.protocol.frozen import (
    file_sha256,
    load_frozen_protocol,
    verify_hash_lock,
    write_hash_once,
    write_json_once,
)

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs/local/v30_architecture_spectrum"


def _check(condition: bool, label: str, checks: list[dict]) -> None:
    checks.append({"check": label, "passed": bool(condition)})


def main() -> dict:
    protocol = load_frozen_protocol(ROOT / "configs/protocol_v3.2.json")
    measurement = load_frozen_protocol(ROOT / "configs/protocol_v3.1.json")
    checks: list[dict] = []
    prediction_hash = verify_hash_lock(
        OUT / "expansion_predictions.json", OUT / "expansion_predictions.sha256"
    )
    analysis_hash = verify_hash_lock(
        OUT / "expansion_analysis.json", OUT / "expansion_analysis.sha256"
    )
    predictions = json.loads((OUT / "expansion_predictions.json").read_text())
    analysis = json.loads((OUT / "expansion_analysis.json").read_text())
    manifest = json.loads((OUT / "data_manifest.json").read_text())
    expansion = [row for row in manifest["corpora"] if row["panel"] == "confirmation"]
    pilot = [row for row in manifest["corpora"] if row["panel"] == "pilot"]
    architectures = [row["id"] for row in measurement["architectures"]]
    seeds = measurement["natural_training"]["seeds"]

    _check(
        protocol["measurement_protocol"]["protocol_hash"]
        == measurement["protocol_hash"],
        "v3.2 locks the v3.1 measurement protocol",
        checks,
    )
    _check(len(expansion) == 48, "48 expansion corpora", checks)
    _check(
        {row["dataset"] for row in pilot}.isdisjoint(
            {row["dataset"] for row in expansion}
        ),
        "development and expansion source IDs disjoint",
        checks,
    )
    _check(
        {row["byte_stream_sha256"] for row in pilot}.isdisjoint(
            {row["byte_stream_sha256"] for row in expansion}
        ),
        "development and expansion byte streams disjoint",
        checks,
    )
    strata = sorted({row["stratum"] for row in expansion})
    _check(
        len(strata) == 6
        and all(
            sum(row["stratum"] == stratum for row in expansion) == 8
            for stratum in strata
        ),
        "six balanced source strata",
        checks,
    )
    _check(
        predictions["status"].endswith(
            "before any v3.2 expansion training outcome"
        ),
        "predictions state pre-outcome freeze",
        checks,
    )
    _check(
        predictions["protocol_hash"] == protocol["protocol_hash"],
        "predictions use v3.2 protocol",
        checks,
    )
    _check(
        predictions["locked_inputs"] == protocol["locked_inputs"],
        "predictions retain all frozen input hashes",
        checks,
    )
    _check(
        len(predictions["predictions"]) == 240
        and len(
            {
                (row["dataset"], row["architecture"])
                for row in predictions["predictions"]
            }
        )
        == 240,
        "240 unique corpus-architecture predictions",
        checks,
    )
    _check(
        all(
            file_sha256(OUT / "expansion_models" / f"{name}.json") == digest
            for name, digest in predictions["models"].items()
        ),
        "all frozen OLS model hashes match",
        checks,
    )

    results_path = OUT / "expansion_results.json"
    cells = json.loads(results_path.read_text())
    expected = {
        f"V32E/{source['dataset']}/{architecture}/s{seed}"
        for source in expansion
        for architecture in architectures
        for seed in seeds
    }
    _check(
        len(cells) == 480 and {row["cell_id"] for row in cells} == expected,
        "complete unique 480-cell expansion grid",
        checks,
    )
    _check(
        all(row["prediction_lock_hash"] == prediction_hash for row in cells),
        "every outcome references frozen predictions",
        checks,
    )
    _check(
        all(row["analysis_protocol_hash"] == protocol["protocol_hash"] for row in cells),
        "every outcome references v3.2 analysis protocol",
        checks,
    )
    _check(
        all(row["protocol_hash"] == measurement["protocol_hash"] for row in cells),
        "every outcome uses frozen v3.1 measurement protocol",
        checks,
    )
    _check(
        all("H100" in row["remote"]["gpu"] for row in cells),
        "every expansion cell ran on H100",
        checks,
    )
    _check(
        all(
            len(
                {
                    row["validation_starts_sha256"]
                    for row in cells
                    if row["dataset"] == source["dataset"]
                }
            )
            == 1
            for source in expansion
        ),
        "validation chunks paired across architecture and seed",
        checks,
    )
    _check(
        analysis["protocol_hash"] == protocol["protocol_hash"]
        and analysis["prediction_lock_hash"] == prediction_hash,
        "analysis references frozen protocol and predictions",
        checks,
    )
    _check(
        analysis["expansion_results_sha256"] == file_sha256(results_path),
        "analysis locks the complete outcome grid",
        checks,
    )
    _check(
        len(analysis["rows"]) == 240
        and len(
            {(row["dataset"], row["architecture"]) for row in analysis["rows"]}
        )
        == 240,
        "analysis contains 240 unique seed-median rows",
        checks,
    )
    interval = analysis["primary_gate"]["interval"]
    expected_verdict = (
        "EXPANDED_FOURIER_SIGNAL_REPLICATES"
        if interval[0] > 0.0
        else "EXPANDED_FOURIER_SIGNAL_DOES_NOT_REPLICATE"
    )
    _check(
        analysis["verdict"] == expected_verdict
        and analysis["primary_gate"]["interval_strictly_positive"]
        == (interval[0] > 0.0),
        "verdict follows frozen interval rule",
        checks,
    )

    result = {
        "status": "PASS" if all(row["passed"] for row in checks) else "FAIL",
        "completed_stage": "v3.2 prospective expansion",
        "protocol_hash": protocol["protocol_hash"],
        "measurement_protocol_hash": measurement["protocol_hash"],
        "locks": {
            "predictions": prediction_hash,
            "results": file_sha256(results_path),
            "analysis": analysis_hash,
        },
        "checks": checks,
        "timing": {
            "expansion_gpu_cell_seconds": sum(
                row["remote"]["wallclock_seconds"] for row in cells
            )
        },
    }
    digest = write_json_once(OUT / "expansion_audit.json", result)
    write_hash_once(OUT / "expansion_audit.sha256", digest)
    print(
        json.dumps(
            {
                "status": result["status"],
                "completed_stage": result["completed_stage"],
                "timing": result["timing"],
                "audit_sha256": digest,
            },
            indent=2,
        )
    )
    return result


if __name__ == "__main__":
    main()
