"""Mechanical provenance and leakage audit for v3.4."""

from __future__ import annotations

import json
import math
from pathlib import Path

from dlx.protocol.frozen import (
    file_sha256,
    load_frozen_protocol,
    verify_hash_lock,
    write_hash_once,
    write_json_once,
)

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs/local/v34_local_window"
SOURCE_OUT = ROOT / "runs/local/v30_architecture_spectrum"


def _check(value: bool, label: str, checks: list[dict]) -> None:
    checks.append({"check": label, "passed": bool(value)})


def main() -> dict:
    protocol = load_frozen_protocol(ROOT / "configs/protocol_v3.4.json")
    analysis_hash = verify_hash_lock(OUT / "analysis.json", OUT / "analysis.sha256")
    feature_hash = verify_hash_lock(
        OUT / "frozen_features.json", OUT / "frozen_features.sha256"
    )
    response_hash = verify_hash_lock(
        OUT / "uniform_response.json", OUT / "uniform_response.sha256"
    )
    analysis = json.loads((OUT / "analysis.json").read_text())
    features = json.loads((OUT / "frozen_features.json").read_text())
    natural = json.loads((OUT / "natural_results.json").read_text())
    uniform = json.loads((OUT / "uniform_results.json").read_text())
    checks: list[dict] = []

    _check(
        file_sha256(ROOT / protocol["panel"]["path"])
        == protocol["panel"]["sha256"],
        "corpus split matches preregistered hash",
        checks,
    )
    _check(
        len(uniform) == 168
        and len({row["cell_id"] for row in uniform}) == 168,
        "exact 168-cell uniform grid",
        checks,
    )
    _check(
        all(
            row["analysis_protocol_hash"] == protocol["protocol_hash"]
            and int(row["degree"]) <= 2
            and int(row["seed"]) in {0, 1}
            for row in uniform
        ),
        "uniform grid uses frozen protocol, degrees, and seeds",
        checks,
    )
    _check(
        features["uniform_response_sha256"] == response_hash
        and len(features["rows"]) == 144,
        "144 predictors derive from locked uniform response",
        checks,
    )
    _check(
        len(natural) == 288
        and len({row["cell_id"] for row in natural}) == 288,
        "exact 288-cell natural grid",
        checks,
    )
    _check(
        all(
            row["prediction_lock_hash"] == feature_hash
            and row["analysis_protocol_hash"] == protocol["protocol_hash"]
            for row in natural
        ),
        "every natural cell consumes the pre-outcome feature lock",
        checks,
    )
    _check(
        analysis["frozen_features_sha256"] == feature_hash
        and analysis["natural_results_sha256"]
        == file_sha256(OUT / "natural_results.json"),
        "analysis input hashes match exact artifacts",
        checks,
    )
    rows = analysis["rows"]
    _check(
        len(rows) == 144
        and sum(row["panel"] == "development" for row in rows) == 72
        and sum(row["panel"] == "confirmation" for row in rows) == 72,
        "balanced development and confirmation outcome panels",
        checks,
    )
    development = {row["dataset"] for row in rows if row["panel"] == "development"}
    confirmation = {row["dataset"] for row in rows if row["panel"] == "confirmation"}
    _check(
        len(development) == len(confirmation) == 24
        and development.isdisjoint(confirmation),
        "24 source-disjoint corpora per panel",
        checks,
    )
    _check(
        all(
            set(model["training_datasets"]) == development
            for target in analysis["analyses"].values()
            for model in target["models"].values()
        ),
        "all fitted models train only on development corpora",
        checks,
    )
    _check(
        all(
            math.isclose(
                row["delta_normalized_curve_area"],
                row["normalized_curve_area"]
                - row["radius64_normalized_curve_area"],
            )
            and math.isclose(
                row["delta_final_ce_fraction"],
                row["final_ce_fraction"] - row["radius64_final_ce_fraction"],
            )
            for row in rows
        ),
        "paired target contrasts recompute exactly",
        checks,
    )
    result = {
        "status": "PASS" if all(row["passed"] for row in checks) else "FAIL",
        "protocol_hash": protocol["protocol_hash"],
        "uniform_response_hash": response_hash,
        "frozen_features_hash": feature_hash,
        "analysis_hash": analysis_hash,
        "checks": checks,
    }
    digest = write_json_once(OUT / "audit.json", result)
    write_hash_once(OUT / "audit.sha256", digest)
    print(json.dumps({**result, "audit_hash": digest}, indent=2))
    return result


if __name__ == "__main__":
    main()
