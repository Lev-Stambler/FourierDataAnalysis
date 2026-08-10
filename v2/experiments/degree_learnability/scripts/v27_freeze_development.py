"""Freeze the 54-corpus development panel for v2.7."""

from __future__ import annotations

import gzip
import json
import sys
from pathlib import Path

ROOT = Path(__file__).parent.parent
sys.path.insert(0, str(ROOT))

from dlx.analysis.floor_independent import normalized_learning_time
from dlx.profiles.sampled_degree import marginal_locality_features
from dlx.protocol.frozen import file_sha256, write_hash_once, write_json_once
from scripts.v26_freeze_predictions import _prior_rows

OUT = ROOT / "runs/local/v27_marginal_locality"
HISTORICAL = ROOT / "runs/local/v26_lag_grid_repair"
V26 = ROOT / "runs/local/v26_sampled_locality"
FEATURE = "sampled_marginal_log1p_radius_through_degree3"
PROFILE_SPEC = {
    "lags": [1, 2, 4, 8, 16, 32, 64],
    "max_degree": 3,
    "chains": 64,
    "positions": 250_000,
    "seed": 2603,
    "delta": 0.05,
}


def _profile_features(base: Path, dataset: str) -> tuple[dict, dict]:
    profile_path = base / "profiles" / f"{dataset}.json"
    audit_path = base / "audit_chains" / f"{dataset}.json.gz"
    profile = json.loads(profile_path.read_text())
    audit = json.loads(gzip.decompress(audit_path.read_bytes()))
    if audit["summary_sha256"] != file_sha256(profile_path):
        raise ValueError(f"profile/audit hash mismatch: {dataset}")
    observed = {
        "lags": profile["lags"],
        "max_degree": profile["max_degree"],
        "chains": profile["n_chains"],
        "positions": profile["n_examples"],
        "seed": profile["seed"],
        "delta": profile["delta"],
    }
    if observed != PROFILE_SPEC:
        raise ValueError(f"incompatible development profile: {dataset}: {observed}")
    features = marginal_locality_features(
        audit["chains"], tuple(profile["coordinate_radii"]), feature_degree=3
    )
    provenance = {
        "profile": str(profile_path.relative_to(ROOT)),
        "profile_sha256": file_sha256(profile_path),
        "audit": str(audit_path.relative_to(ROOT)),
        "audit_sha256": file_sha256(audit_path),
        "data_sha256": profile["data_sha256"],
    }
    return features, provenance


def _target(row: dict) -> float:
    return normalized_learning_time(
        row["normalized_curve_area"], row["final_ce_fraction"]
    )


def freeze() -> dict:
    rows = []
    for source in _prior_rows():
        features, provenance = _profile_features(HISTORICAL, source["dataset"])
        rows.append(
            {
                "dataset": source["dataset"],
                "panel": "historical_22_reprofiled",
                "configuration": "learned_absolute_d64_l2",
                "features": features,
                "normalized_curve_area": source["normalized_curve_area"],
                "final_ce_fraction": source["final_ce_fraction"],
                "normalized_learning_time": _target(source),
                **provenance,
            }
        )
    v26_rows = {
        row["dataset"]: row
        for row in json.loads((V26 / "analysis.json").read_text())["rows"]
    }
    for dataset in sorted(v26_rows):
        source = v26_rows[dataset]
        features, provenance = _profile_features(V26, dataset)
        rows.append(
            {
                "dataset": dataset,
                "panel": "v2.6_32",
                "configuration": "learned_absolute_d64_l2",
                "features": features,
                "normalized_curve_area": source["normalized_curve_area"],
                "final_ce_fraction": source["final_ce_fraction"],
                "normalized_learning_time": _target(source),
                **provenance,
            }
        )
    if len(rows) != 54 or len({row["dataset"] for row in rows}) != 54:
        raise ValueError("development panel must contain 54 unique corpora")
    result = {
        "status": "frozen v2.7 development data; confirmation outcomes absent",
        "feature": FEATURE,
        "target": "normalized_learning_time",
        "profile_spec": PROFILE_SPEC,
        "n_corpora": len(rows),
        "rows": rows,
    }
    digest = write_json_once(OUT / "development_manifest.json", result)
    write_hash_once(OUT / "development_manifest.sha256", digest)
    return result


if __name__ == "__main__":
    result = freeze()
    print(f"froze {result['n_corpora']} development rows")
