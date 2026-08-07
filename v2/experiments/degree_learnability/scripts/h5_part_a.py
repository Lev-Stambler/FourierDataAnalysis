"""Part A: exact-row/exact-feature OpenML bands with fixed-MLP cells."""

from __future__ import annotations

import json
import sys
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).parent.parent))
sys.path.insert(0, str(Path(__file__).parent))

from h5_common import OUT, load_v13, set_bounded_threads

from dlx.data.tabular import dataset_spectrum, load_tabular
from dlx.protocol import load_manifest
from dlx.training.matched_tabular import matched_tabular_run


def degree_features(spectrum: dict) -> dict:
    w = np.asarray(spectrum["W"], dtype=float)
    w /= max(float(w.sum()), 1e-12)
    degrees = np.arange(len(w))
    degree90 = int(np.searchsorted(np.cumsum(w), 0.90, side="left"))
    return {
        "W_normalized_for_analysis": w.tolist(),
        "mean_degree": float(np.dot(degrees, w)),
        "tail_above_2": float(w[3:].sum()),
        "degree_90": degree90,
        "grid_size": spectrum["grid_size"],
        "distinct_rows": spectrum["distinct_rows"],
    }


def main() -> None:
    proto = load_v13()
    set_bounded_threads(proto)
    spec = proto["matched_h5"]["part_a_tabular"]
    root = OUT / "part_a"
    root.mkdir(parents=True, exist_ok=True)
    report = {"protocol_hash": proto["protocol_hash"], "bands": {}, "cells": []}

    for band, names in spec["bands"].items():
        expected_features = int(band.split("_")[0][1:])
        report["bands"][band] = {"datasets": {}, "expected_features": expected_features,
                                  "row_target": spec["row_target"]}
        prepared = {}
        for name in names:
            data = load_tabular(name, max_rows=spec["row_target"],
                                seed=spec["selection_seed"])
            if len(data["X"]) != spec["row_target"]:
                raise ValueError(f"{name}: {len(data['X'])} rows != frozen target")
            if data["X"].shape[1] != expected_features:
                raise ValueError(f"{name}: {data['X'].shape[1]} features != {expected_features}")
            spectrum = dataset_spectrum(data["X"], data["y"], data["n_classes"])
            prepared[name] = data
            report["bands"][band]["datasets"][name] = {
                "openml": data["openml"], "n_rows": len(data["X"]),
                "n_features": data["X"].shape[1], "n_classes": data["n_classes"],
                "q_features": data["q_features"], **degree_features(spectrum),
            }

        for name, data in prepared.items():
            for seed in spec["seeds"]:
                cell_id = f"H5A/{band}/{name}/s{seed}"
                cell_dir = root / cell_id.replace("/", "__")
                if (cell_dir / "manifest.json").exists():
                    metrics = json.loads((cell_dir / "metrics.json").read_text())
                else:
                    metrics = matched_tabular_run(
                        data, seed, cell_dir, cell_id, proto["protocol_hash"], band,
                        spec["selection_seed"], device="cpu")
                load_manifest(cell_dir)
                report["cells"].append({
                    "cell_id": cell_id, "band": band, "dataset": name, "seed": seed,
                    **{k: metrics[k] for k in (
                        "initial_ce_bits", "final_ce_bits", "best_ce_bits",
                        "learning_amount_bits", "fractional_learning",
                        "normalized_curve_area", "label_entropy_bits",
                        "majority_fraction")},
                })
    (root / "part_a_results.json").write_text(json.dumps(report, indent=2))
    print(f"Part A complete: {len(report['cells'])} cells -> {root / 'part_a_results.json'}")


if __name__ == "__main__":
    main()
