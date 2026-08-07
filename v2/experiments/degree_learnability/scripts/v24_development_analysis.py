"""Fit the v2.4 low-data model zoo on pre-confirmatory development data."""

from __future__ import annotations

import hashlib
import json
import math
import sys
import warnings
from pathlib import Path

import numpy as np
from sklearn.exceptions import ConvergenceWarning

sys.path.insert(0, str(Path(__file__).parent.parent))
sys.path.insert(0, str(Path(__file__).parent))

from v20_analyze import _median_cell_metrics

from dlx.analysis.spectrum_predictor import (
    FOURIER_CORE_FEATURES,
    FOURIER_FEATURES,
    fit_frozen_ridge,
    nested_loco_predictions,
)
from dlx.profiles.simple_controls import simple_text_controls

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs/local/v24_spectrum_predictor"
PROFILE_DIR = OUT / "text_profiles"
CONTROL_DIR = OUT / "text_controls"
CONTROL_FEATURES = (
    "unigram_entropy_bits",
    "heldout_bigram_ce_bits",
    "lag1_mutual_information_bits",
    "zlib_bits_per_byte",
)
MODEL_KINDS = ("ridge", "elastic_net", "pls", "svr_rbf", "gaussian_process")
TARGETS = ("final_ce_fraction", "normalized_curve_area")

DATA_FILES = {
    "enwik8": ROOT / "dlx/data_cache/v19_enwik8_bytes_n5500000.npy",
    "tinystories": ROOT / "dlx/data_cache/h5_tinystories_bytes_n5500000.npy",
    "wikitext2": ROOT / "dlx/data_cache/v18_wikitext2_bytes_n5500000.npy",
    "codeparrot_python": ROOT / "dlx/data_cache/v18_codeparrot_python_bytes_n5500000.npy",
    "gutenberg_books": ROOT / "dlx/data_cache/v21_gutenberg_books_bytes_n5499984.npy",
    "reuters_news": ROOT / "dlx/data_cache/v21_reuters_news_bytes_n5499984.npy",
    "brown_balanced": ROOT / "dlx/data_cache/v21_brown_balanced_bytes_n5499984.npy",
    "pubmed_abstracts": ROOT / "dlx/data_cache/v21_pubmed_abstracts_bytes_n5499984.npy",
    "cpython_source": ROOT / "dlx/data_cache/v21_cpython_source_bytes_n5499984.npy",
    "linux_c_source": ROOT / "dlx/data_cache/v21_linux_c_source_bytes_n5499984.npy",
    "mathlib_lean": ROOT / "dlx/data_cache/v22_mathlib_lean_bytes_n8000000.npy",
    "rust_source": ROOT / "dlx/data_cache/v22_rust_source_bytes_n8000000.npy",
    "rfc_technical": ROOT / "dlx/data_cache/v22_rfc_technical_bytes_n8000000.npy",
}


def load_protocol() -> dict:
    protocol = json.loads((ROOT / "configs/protocol_v2.4.json").read_text())
    recorded = protocol["protocol_hash"]
    unhashed = dict(protocol)
    unhashed.pop("protocol_hash")
    expected = hashlib.sha256(json.dumps(unhashed, sort_keys=True).encode()).hexdigest()
    if recorded != expected:
        raise ValueError(f"protocol hash mismatch: {recorded} != {expected}")
    return protocol


def _training_cells() -> list[dict]:
    v20 = json.loads(
        (ROOT / "runs/local/v20_confirmatory_geometry/remote_results.json").read_text()
    )
    for row in v20:
        row["configuration"] = "learned_absolute_d64_l2"

    selected_strides = {1, 4, 8, 16}
    v21 = [
        row
        for row in json.loads(
            (ROOT / "runs/local/v21_predictor_selection/remote_results.json").read_text()
        )
        if row["stride"] in selected_strides
    ]
    for row in v21:
        row["configuration"] = "learned_absolute_d64_l2"

    v23 = json.loads(
        (ROOT / "runs/local/v23_transformer_robustness/remote_results.json").read_text()
    )
    v22 = json.loads((ROOT / "runs/local/v22_hard_h100/remote_results.json").read_text())
    for row in v22:
        row["configuration"] = "learned_absolute_d256_l4_8m"
    return v20 + v21 + v23 + v22


def _controls(dataset: str, stride: int) -> dict:
    CONTROL_DIR.mkdir(parents=True, exist_ok=True)
    path = CONTROL_DIR / f"{dataset}__stride{stride}.json"
    if path.exists():
        return json.loads(path.read_text())
    original = np.load(DATA_FILES[dataset], mmap_mode="r")
    tokens = np.ascontiguousarray(
        np.asarray(original, dtype=np.uint8).reshape(stride, -1).T.reshape(-1)
    )
    result = {
        "dataset": dataset,
        "stride": stride,
        "data_sha256": hashlib.sha256(tokens.tobytes()).hexdigest(),
        **simple_text_controls(tokens, q=256, max_tokens=1_000_000),
    }
    path.write_text(json.dumps(result, indent=2))
    return result


def build_rows() -> list[dict]:
    cells = _training_cells()
    keys = sorted(
        {
            (row["dataset"], int(row["stride"]), row["configuration"])
            for row in cells
        }
    )
    rows = []
    for dataset, stride, configuration in keys:
        selected = [
            row
            for row in cells
            if row["dataset"] == dataset
            and row["stride"] == stride
            and row["configuration"] == configuration
        ]
        if len(selected) != 3:
            raise ValueError(f"expected three seeds for {dataset}/{stride}/{configuration}")
        profile = json.loads(
            (PROFILE_DIR / f"{dataset}__stride{stride}.json").read_text()
        )
        controls = _controls(dataset, stride)
        if profile["data_sha256"] != selected[0]["data_sha256"]:
            raise ValueError(f"profile/training data mismatch: {dataset}/stride{stride}")
        if controls["data_sha256"] != profile["data_sha256"]:
            raise ValueError(f"control/profile data mismatch: {dataset}/stride{stride}")
        rows.append(
            {
                "dataset": dataset,
                "stride": stride,
                "configuration": configuration,
                "features": {**profile["features"], **controls},
                "log2_stride": math.log2(stride),
                **_median_cell_metrics(selected),
            }
        )
    return rows


def _delta_rows(rows: list[dict]) -> list[dict]:
    output = []
    for row in rows:
        if row["stride"] == 1:
            continue
        baseline = next(
            candidate
            for candidate in rows
            if candidate["dataset"] == row["dataset"]
            and candidate["configuration"] == row["configuration"]
            and candidate["stride"] == 1
        )
        output.append(
            {
                "dataset": row["dataset"],
                "stride": row["stride"],
                "configuration": row["configuration"],
                "features": {
                    name: row["features"][name] - baseline["features"][name]
                    for name in (*FOURIER_FEATURES, *CONTROL_FEATURES)
                },
                "log2_stride": row["log2_stride"],
                **{
                    target: row[target] - baseline[target]
                    for target in TARGETS
                },
            }
        )
    return output


def analyze() -> dict:
    protocol = load_protocol()
    rows = build_rows()
    feature_sets = {
        "configuration_only": (),
        "non_fourier_only": CONTROL_FEATURES,
        "fourier_compact": FOURIER_CORE_FEATURES,
        "fourier_plus_configuration": FOURIER_FEATURES,
        "combined_compact": (*FOURIER_CORE_FEATURES, *CONTROL_FEATURES),
        "combined": (*FOURIER_FEATURES, *CONTROL_FEATURES),
    }
    model_comparison = {}
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", category=ConvergenceWarning)
        warnings.filterwarnings("ignore", message="y residual is constant.*")
        for target in TARGETS:
            model_comparison[target] = {}
            for feature_set, feature_names in feature_sets.items():
                model_comparison[target][feature_set] = {
                    kind: nested_loco_predictions(
                        rows,
                        target=target,
                        feature_names=feature_names,
                        kind=kind,
                    )
                    for kind in MODEL_KINDS
                }

    delta_rows = _delta_rows(rows)
    delta_models = {
        target: {
            "configuration_only": nested_loco_predictions(
                delta_rows, target=target, feature_names=(), kind="ridge"
            ),
            "stride_only": nested_loco_predictions(
                delta_rows,
                target=target,
                feature_names=("log2_stride",),
                kind="ridge",
            ),
            "fourier": nested_loco_predictions(
                delta_rows,
                target=target,
                feature_names=("energy_weighted_log_radius",),
                kind="ridge",
            ),
            "combined": nested_loco_predictions(
                delta_rows,
                target=target,
                feature_names=(*FOURIER_FEATURES, *CONTROL_FEATURES),
                kind="ridge",
            ),
        }
        for target in TARGETS
    }
    frozen = {
        target: {
            "fourier_compact": fit_frozen_ridge(
                rows,
                target=target,
                feature_names=FOURIER_CORE_FEATURES,
            ),
            "combined_compact": fit_frozen_ridge(
                rows,
                target=target,
                feature_names=(*FOURIER_CORE_FEATURES, *CONTROL_FEATURES),
            ),
        }
        for target in TARGETS
    }
    summary = {
        target: {
            feature_set: {
                kind: {
                    "group_balanced_rmse": result["group_balanced_rmse"],
                    "r2": result["r2"],
                }
                for kind, result in by_kind.items()
            }
            for feature_set, by_kind in model_comparison[target].items()
        }
        for target in TARGETS
    }
    result = {
        "protocol_hash": protocol["protocol_hash"],
        "status": "development only; no confirmatory outcomes used",
        "n_independent_corpora": len({row["dataset"] for row in rows}),
        "n_dataset_configuration_stride_rows": len(rows),
        "feature_sets": {key: list(value) for key, value in feature_sets.items()},
        "summary": summary,
        "model_comparison": model_comparison,
        "intervention_delta_models": delta_models,
        "frozen_primary_ridge": frozen,
        "rows": rows,
    }
    OUT.mkdir(parents=True, exist_ok=True)
    (OUT / "development_analysis.json").write_text(json.dumps(result, indent=2))
    for target, artifacts in frozen.items():
        for feature_set, artifact in artifacts.items():
            (OUT / f"frozen_ridge__{feature_set}__{target}.json").write_text(
                json.dumps(artifact, indent=2)
            )
    print(json.dumps({"summary": summary, "delta": {
        target: {
            name: {
                "rmse": model["group_balanced_rmse"],
                "r2": model["r2"],
            }
            for name, model in models.items()
        }
        for target, models in delta_models.items()
    }}, indent=2))
    return result


if __name__ == "__main__":
    analyze()
