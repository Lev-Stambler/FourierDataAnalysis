"""Generate and hash v2.4 predictions before confirmatory training is permitted."""

from __future__ import annotations

import hashlib
import json
import sys
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).parent.parent))

from dlx.analysis.spectrum_predictor import predict_frozen_ridge
from dlx.profiles.simple_controls import simple_text_controls

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs/local/v24_spectrum_predictor"
PROFILE_DIR = OUT / "confirmatory_profiles"
CONTROL_DIR = OUT / "confirmatory_controls"
PREDICTION_PATH = OUT / "confirmatory_predictions.json"
PREDICTION_HASH_PATH = OUT / "confirmatory_predictions.sha256"
MANIFEST_HASH_PATH = OUT / "confirmatory_data_manifest.sha256"
DATASETS = (
    "mdn_docs",
    "kubernetes_docs",
    "rust_reference_docs",
    "rfc_legacy",
    "pubmed_independent",
    "go_source",
    "typescript_source",
    "llvm_cpp_source",
    "coq_source",
    "julia_source",
    "ghc_haskell_source",
    "postgresql_source",
)
INTERVENTION_DATASETS = {
    "mdn_docs",
    "rfc_legacy",
    "pubmed_independent",
    "go_source",
    "llvm_cpp_source",
    "coq_source",
}
CONFIGURATIONS = (
    "learned_absolute_d64_l2",
    "sinusoidal_d64_l2",
    "alibi_d64_l2",
)
TARGETS = ("final_ce_fraction", "normalized_curve_area")
FEATURE_SETS = ("fourier_compact", "combined_compact")


def _controls(dataset: str, stride: int, data_hash: str) -> dict:
    CONTROL_DIR.mkdir(parents=True, exist_ok=True)
    path = CONTROL_DIR / f"{dataset}__stride{stride}.json"
    if path.exists():
        result = json.loads(path.read_text())
        if result["data_sha256"] != data_hash:
            raise ValueError(f"stale confirmatory controls: {dataset}/stride{stride}")
        return result
    original = np.load(
        ROOT / f"dlx/data_cache/v24_{dataset}_bytes_n2000000.npy", mmap_mode="r"
    )
    tokens = np.ascontiguousarray(
        np.asarray(original, dtype=np.uint8).reshape(stride, -1).T.reshape(-1)
    )
    actual_hash = hashlib.sha256(tokens.tobytes()).hexdigest()
    if actual_hash != data_hash:
        raise ValueError(f"profile/control data mismatch: {dataset}/stride{stride}")
    result = {
        "dataset": dataset,
        "stride": stride,
        "data_sha256": actual_hash,
        **simple_text_controls(tokens, q=256, max_tokens=1_000_000),
    }
    path.write_text(json.dumps(result, indent=2))
    return result


def main() -> None:
    rows = []
    for dataset in DATASETS:
        strides = (1, 4, 8, 16) if dataset in INTERVENTION_DATASETS else (1,)
        for stride in strides:
            profile = json.loads(
                (PROFILE_DIR / f"{dataset}__stride{stride}.json").read_text()
            )
            controls = _controls(dataset, stride, profile["data_sha256"])
            for configuration in CONFIGURATIONS:
                rows.append(
                    {
                        "dataset": dataset,
                        "stride": stride,
                        "configuration": configuration,
                        "data_sha256": profile["data_sha256"],
                        "features": {**profile["features"], **controls},
                    }
                )

    artifacts = {}
    artifact_hashes = {}
    for target in TARGETS:
        artifacts[target] = {}
        artifact_hashes[target] = {}
        for feature_set in FEATURE_SETS:
            path = OUT / f"frozen_ridge__{feature_set}__{target}.json"
            artifacts[target][feature_set] = json.loads(path.read_text())
            artifact_hashes[target][feature_set] = hashlib.sha256(
                path.read_bytes()
            ).hexdigest()

    predictions = [dict(row) for row in rows]
    for target in TARGETS:
        for feature_set in FEATURE_SETS:
            values = predict_frozen_ridge(artifacts[target][feature_set], rows)
            for row, value in zip(predictions, values, strict=True):
                row.setdefault("predictions", {}).setdefault(feature_set, {})[
                    target
                ] = float(value)
    result = {
        "protocol_hash": json.loads(
            (ROOT / "configs/protocol_v2.4.json").read_text()
        )["protocol_hash"],
        "data_manifest_hash": MANIFEST_HASH_PATH.read_text().strip(),
        "status": "predictions frozen before confirmatory training",
        "artifact_hashes": artifact_hashes,
        "n_prediction_rows": len(predictions),
        "predictions": predictions,
    }
    payload = (json.dumps(result, indent=2) + "\n").encode()
    digest = hashlib.sha256(payload).hexdigest()
    if PREDICTION_PATH.exists() and PREDICTION_PATH.read_bytes() != payload:
        raise FileExistsError("refusing to overwrite a different prediction lock")
    PREDICTION_PATH.write_bytes(payload)
    PREDICTION_HASH_PATH.write_text(digest + "\n")
    print(f"froze {len(predictions)} predictions: sha256={digest}")


if __name__ == "__main__":
    main()
