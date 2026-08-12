"""Freeze v3.4 uniform-character response and natural-corpus predictors."""

from __future__ import annotations

import json
from collections import defaultdict
from pathlib import Path

import numpy as np

from dlx.analysis.character_response import enumerate_supports
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


def _key(support: tuple[int, ...]) -> str:
    return ",".join(str(value) for value in support)


def _r_squared(y: np.ndarray, prediction: np.ndarray) -> float | None:
    total_ss = float(np.sum((y - y.mean()) ** 2))
    return (
        1.0 - float(np.sum((y - prediction) ** 2)) / total_ss
        if total_ss > 0.0
        else None
    )


def _panel_rows(protocol: dict) -> list[dict]:
    panel = json.loads((ROOT / protocol["panel"]["path"]).read_text())
    return [
        {"dataset": dataset, "stratum": stratum, "panel": split}
        for stratum, value in panel["strata"].items()
        for split in ("development", "confirmation")
        for dataset in value[split]
    ]


def _uniform_geometry(
    hardness: dict[str, dict[str, float]], supports: list[tuple[int, ...]]
) -> dict:
    """Describe how far each architecture is from degree-only Fourier isotropy."""
    output = {}
    for window, values in hardness.items():
        y = np.asarray([values[_key(tuple(support))] for support in supports])
        degree = np.asarray([len(support) for support in supports])
        radius = np.asarray([max(support) for support in supports], dtype=float)
        degree_fit = np.asarray(
            [y[degree == value].mean() for value in degree], dtype=float
        )
        design = np.column_stack(
            (
                np.ones(len(y)),
                degree == 2,
                np.log2(radius),
            )
        )
        geometry_fit = design @ np.linalg.lstsq(design, y, rcond=None)[0]
        output[window] = {
            "support_mean": float(y.mean()),
            "support_sd": float(y.std()),
            "support_range": [float(y.min()), float(y.max())],
            "degree_only_r_squared": _r_squared(y, degree_fit),
            "degree_plus_log2_radius_r_squared": _r_squared(y, geometry_fit),
            "within_degree": {
                str(value): {
                    "mean": float(y[degree == value].mean()),
                    "sd": float(y[degree == value].std()),
                    "range": [
                        float(y[degree == value].min()),
                        float(y[degree == value].max()),
                    ],
                }
                for value in (1, 2)
            },
            "interpretation": "zero within-degree SD would be Fourier-isotropic with respect to exact support at fixed degree",
        }
    return output


def main() -> dict:
    protocol = load_frozen_protocol(ROOT / "configs/protocol_v3.4.json")
    measurement = load_frozen_protocol(ROOT / "configs/protocol_v3.1.json")
    panel_hash = verify_hash_lock(
        ROOT / protocol["panel"]["path"],
        ROOT / "configs/local_window_panel_v3.4.sha256",
    )
    supports = enumerate_supports(
        protocol["uniform_character_probe"]["lags"], max_degree=2
    )
    seeds = set(protocol["uniform_character_probe"]["seeds"])
    expected_new = {
        (int(window), tuple(support), int(seed))
        for window in protocol["intervention"]["new_attention_windows"]
        for support in supports
        for seed in seeds
    }
    new_path = OUT / "uniform_results.json"
    new_cells = json.loads(new_path.read_text())
    observed_new = {
        (
            int(row["architecture"].removeprefix("rope_w")),
            tuple(int(value) for value in row["support"]),
            int(row["seed"]),
        )
        for row in new_cells
    }
    if len(new_cells) != len(expected_new) or observed_new != expected_new:
        raise ValueError(f"uniform grid incomplete: {len(new_cells)}/{len(expected_new)}")
    if any(row["analysis_protocol_hash"] != protocol["protocol_hash"] for row in new_cells):
        raise ValueError("uniform cells do not use the frozen v3.4 protocol")

    legacy_path = SOURCE_OUT / "fourier_character_results.json"
    if file_sha256(legacy_path) != protocol["uniform_character_probe"]["radius64_reuse"]["sha256"]:
        raise ValueError("legacy radius-64 character cells changed after freeze")
    legacy = [
        row
        for row in json.loads(legacy_path.read_text())
        if row["architecture"] == "rope"
        and int(row["degree"]) <= 2
        and int(row["seed"]) in seeds
    ]
    expected_legacy = {(tuple(support), seed) for support in supports for seed in seeds}
    observed_legacy = {
        (tuple(int(value) for value in row["support"]), int(row["seed"]))
        for row in legacy
    }
    if len(legacy) != len(expected_legacy) or observed_legacy != expected_legacy:
        raise ValueError("legacy radius-64 character grid is incomplete")

    hardness_values: dict[tuple[int, tuple[int, ...]], list[float]] = defaultdict(list)
    for row in new_cells:
        window = int(row["architecture"].removeprefix("rope_w"))
        hardness_values[(window, tuple(row["support"]))].append(
            float(row["character_hardness"])
        )
    for row in legacy:
        hardness_values[(64, tuple(row["support"]))].append(
            float(row["character_hardness"])
        )
    hardness = {
        str(window): {
            _key(tuple(support)): float(np.median(hardness_values[(window, tuple(support))]))
            for support in supports
        }
        for window in protocol["intervention"]["attention_windows"]
    }
    response = {
        "status": "frozen before any limited-window natural-training outcome",
        "protocol_hash": protocol["protocol_hash"],
        "measurement_protocol_hash": measurement["protocol_hash"],
        "uniform_results_sha256": file_sha256(new_path),
        "legacy_results_sha256": file_sha256(legacy_path),
        "definition": "median normalized held-out CE curve area over seeds 0 and 1",
        "hardness": hardness,
        "uniform_geometry": _uniform_geometry(hardness, supports),
    }
    response_digest = write_json_once(OUT / "uniform_response.json", response)
    write_hash_once(OUT / "uniform_response.sha256", response_digest)

    profiles = {}
    for row in _panel_rows(protocol):
        profile_path = SOURCE_OUT / "profiles" / f"{row['dataset']}.json"
        profile = json.loads(profile_path.read_text())
        raw = {
            key: float(value)
            for key, value in profile["support_energy"].items()
            if len(key.split(",")) <= 2
        }
        total = sum(raw.values())
        if total <= 0.0 or set(raw) != {_key(tuple(support)) for support in supports}:
            raise ValueError(f"invalid degree<=2 support energy for {row['dataset']}")
        profiles[row["dataset"]] = {
            **row,
            "profile_sha256": file_sha256(profile_path),
            "energy": {key: value / total for key, value in raw.items()},
            "raw_degree12_energy": total,
        }

    feature_rows = []
    for dataset in sorted(profiles):
        profile = profiles[dataset]
        overlaps = {
            window: sum(
                energy * hardness[str(window)][support]
                for support, energy in profile["energy"].items()
            )
            for window in protocol["intervention"]["attention_windows"]
        }
        for window in protocol["intervention"]["new_attention_windows"]:
            direct_far_energy = sum(
                energy
                for support, energy in profile["energy"].items()
                if max(int(value) for value in support.split(",")) > window
            )
            effective_reach = 1 + int(protocol["intervention"]["layers"]) * (
                window - 1
            )
            unreachable_energy = sum(
                energy
                for support, energy in profile["energy"].items()
                if max(int(value) for value in support.split(",")) > effective_reach
            )
            feature_rows.append(
                {
                    "dataset": dataset,
                    "stratum": profile["stratum"],
                    "panel": profile["panel"],
                    "window": window,
                    "uniform_overlap": overlaps[window],
                    "radius64_uniform_overlap": overlaps[64],
                    "delta_uniform_overlap": overlaps[window] - overlaps[64],
                    "direct_far_energy_fraction": direct_far_energy,
                    "effective_target_lag_reach": effective_reach,
                    "unreachable_energy_fraction": unreachable_energy,
                    "raw_degree12_energy": profile["raw_degree12_energy"],
                    "profile_sha256": profile["profile_sha256"],
                }
            )
    artifact = {
        "status": "all predictors frozen before any limited-window natural-training outcome",
        "protocol_hash": protocol["protocol_hash"],
        "panel_sha256": panel_hash,
        "uniform_response_sha256": response_digest,
        "profile_manifest_sha256": verify_hash_lock(
            SOURCE_OUT / "profile_manifest.json", SOURCE_OUT / "profile_manifest.sha256"
        ),
        "rows": feature_rows,
    }
    digest = write_json_once(OUT / "frozen_features.json", artifact)
    write_hash_once(OUT / "frozen_features.sha256", digest)
    print(
        json.dumps(
            {
                "uniform_response_sha256": response_digest,
                "frozen_features_sha256": digest,
                "feature_rows": len(feature_rows),
            },
            indent=2,
        )
    )
    return artifact


if __name__ == "__main__":
    main()
