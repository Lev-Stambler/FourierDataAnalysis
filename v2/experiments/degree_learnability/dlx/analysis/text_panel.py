"""Stable corpus-row loaders shared by post-v2.4 text analyses."""

from __future__ import annotations

import json
from pathlib import Path

import numpy as np

CONTROL_FEATURES = (
    "unigram_entropy_bits",
    "heldout_bigram_ce_bits",
    "lag1_mutual_information_bits",
    "zlib_bits_per_byte",
)
TARGETS = ("final_ce_fraction", "normalized_curve_area")
V24_OUT = Path("runs/local/v24_spectrum_predictor")

V24_DEVELOPMENT_DATA = {
    "enwik8": "dlx/data_cache/v19_enwik8_bytes_n5500000.npy",
    "tinystories": "dlx/data_cache/h5_tinystories_bytes_n5500000.npy",
    "wikitext2": "dlx/data_cache/v18_wikitext2_bytes_n5500000.npy",
    "codeparrot_python": "dlx/data_cache/v18_codeparrot_python_bytes_n5500000.npy",
    "gutenberg_books": "dlx/data_cache/v21_gutenberg_books_bytes_n5499984.npy",
    "reuters_news": "dlx/data_cache/v21_reuters_news_bytes_n5499984.npy",
    "brown_balanced": "dlx/data_cache/v21_brown_balanced_bytes_n5499984.npy",
    "pubmed_abstracts": "dlx/data_cache/v21_pubmed_abstracts_bytes_n5499984.npy",
    "cpython_source": "dlx/data_cache/v21_cpython_source_bytes_n5499984.npy",
    "linux_c_source": "dlx/data_cache/v21_linux_c_source_bytes_n5499984.npy",
    "mathlib_lean": "dlx/data_cache/v22_mathlib_lean_bytes_n8000000.npy",
    "rust_source": "dlx/data_cache/v22_rust_source_bytes_n8000000.npy",
    "rfc_technical": "dlx/data_cache/v22_rfc_technical_bytes_n8000000.npy",
}


def v24_profile_data_files(root: Path, panel: str) -> dict[str, Path]:
    if panel == "development":
        return {
            dataset: root / relative
            for dataset, relative in V24_DEVELOPMENT_DATA.items()
        }
    if panel != "confirmation":
        raise ValueError("panel must be development or confirmation")
    manifest = json.loads(
        (root / V24_OUT / "confirmatory_data_manifest.json").read_text()
    )
    return {row["dataset"]: root / row["output"] for row in manifest}


def median_cell_metrics(cells: list[dict]) -> dict:
    """Reduce repeated training seeds to one corpus/configuration outcome row."""
    if not cells:
        raise ValueError("at least one training cell is required")
    summaries = [row["floor_independent"] for row in cells]
    keys = (
        "initial_ce_bits",
        "final_ce_bits",
        "learning_amount_bits",
        "fractional_learning",
        "normalized_curve_area",
    )
    values = {
        key: float(np.median([summary[key] for summary in summaries])) for key in keys
    }
    values["final_ce_fraction"] = float(
        np.median(
            [
                summary["final_ce_bits"] / summary["initial_ce_bits"]
                for summary in summaries
            ]
        )
    )
    return values


def sampled_features(profile: dict) -> dict[str, float]:
    """Read schema-v2 summaries or derive the historical v2.5 summaries."""
    if "sampled_features" in profile:
        return {
            name: float(value) for name, value in profile["sampled_features"].items()
        }
    curve = profile["degree_curve"]
    energies = np.asarray(
        [curve[degree]["mean_conditional_collision_energy"] for degree in range(4)],
        dtype=float,
    )
    increments = np.maximum(np.diff(energies), 0.0)
    total = float(increments.sum())
    return {
        "sampled_nonconstant_energy_through_degree3": total,
        "sampled_mean_degree_through_degree3": (
            float(np.dot(np.arange(1, 4), increments) / total) if total else 0.0
        ),
        "sampled_degree3_context_coverage": float(curve[3]["mean_context_coverage"]),
    }


def attach_sampled_features(rows: list[dict], profile_dir: Path) -> list[dict]:
    output = []
    for row in rows:
        profile = json.loads((profile_dir / f"{row['dataset']}.json").read_text())
        output.append(
            {
                **row,
                "features": {**row["features"], **sampled_features(profile)},
            }
        )
    return output


def attach_geometric_sampled_features(
    rows: list[dict], profile_dir: Path
) -> list[dict]:
    """Attach the fixed v2.6 energy/degree/locality summaries to prior rows."""
    output = []
    for row in rows:
        profile = json.loads((profile_dir / f"{row['dataset']}.json").read_text())
        features = profile.get("sampled_features") or profile.get(
            "geometric_sampled_features"
        )
        if features is None:
            raise ValueError(f"profile lacks geometric summaries: {row['dataset']}")
        output.append({**row, "features": {**row["features"], **features}})
    return output


def load_v24_development_rows(root: Path) -> list[dict]:
    """Load the historical development panel without importing experiment scripts."""
    sources = []
    for relative, configuration, strides in (
        (
            "runs/local/v20_confirmatory_geometry/remote_results.json",
            "learned_absolute_d64_l2",
            None,
        ),
        (
            "runs/local/v21_predictor_selection/remote_results.json",
            "learned_absolute_d64_l2",
            {1, 4, 8, 16},
        ),
        ("runs/local/v23_transformer_robustness/remote_results.json", None, None),
        (
            "runs/local/v22_hard_h100/remote_results.json",
            "learned_absolute_d256_l4_8m",
            None,
        ),
    ):
        cells = json.loads((root / relative).read_text())
        for cell in cells:
            if strides is not None and int(cell["stride"]) not in strides:
                continue
            copied = dict(cell)
            if configuration is not None:
                copied["configuration"] = configuration
            sources.append(copied)
    output_dir = root / V24_OUT
    keys = sorted(
        {(row["dataset"], int(row["stride"]), row["configuration"]) for row in sources}
    )
    rows = []
    for dataset, stride, configuration in keys:
        selected = [
            row
            for row in sources
            if row["dataset"] == dataset
            and int(row["stride"]) == stride
            and row["configuration"] == configuration
        ]
        if len(selected) != 3:
            raise ValueError(
                f"expected three seeds for {dataset}/stride{stride}/{configuration}"
            )
        profile = json.loads(
            (
                output_dir / "text_profiles" / f"{dataset}__stride{stride}.json"
            ).read_text()
        )
        controls = json.loads(
            (
                output_dir / "text_controls" / f"{dataset}__stride{stride}.json"
            ).read_text()
        )
        if profile["data_sha256"] != selected[0]["data_sha256"]:
            raise ValueError(f"profile/training mismatch: {dataset}/stride{stride}")
        if controls["data_sha256"] != profile["data_sha256"]:
            raise ValueError(f"control/profile mismatch: {dataset}/stride{stride}")
        rows.append(
            {
                "dataset": dataset,
                "stride": stride,
                "configuration": configuration,
                "features": {**profile["features"], **controls},
                **median_cell_metrics(selected),
            }
        )
    return rows


def load_v24_confirmation_rows(root: Path) -> list[dict]:
    """Load the 12 natural stride-one confirmation corpora and frozen features."""
    output_dir = root / V24_OUT
    feature_rows = json.loads(
        (output_dir / "confirmatory_predictions.json").read_text()
    )["predictions"]
    natural_features = {
        (row["dataset"], row["configuration"]): row["features"]
        for row in feature_rows
        if int(row["stride"]) == 1
    }
    cells = json.loads((output_dir / "confirmatory_remote_results.json").read_text())
    rows = []
    for dataset, configuration in sorted(natural_features):
        selected = [
            row
            for row in cells
            if row["dataset"] == dataset
            and int(row["stride"]) == 1
            and row["configuration"] == configuration
        ]
        if len(selected) != 3:
            raise ValueError(
                f"expected three confirmation seeds: {dataset}/{configuration}"
            )
        rows.append(
            {
                "dataset": dataset,
                "stride": 1,
                "configuration": configuration,
                "features": natural_features[(dataset, configuration)],
                **median_cell_metrics(selected),
            }
        )
    return rows
