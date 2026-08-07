"""Part C: floor-independent re-score of the 18 existing image cells."""

from __future__ import annotations

import json
import sys
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).parent.parent))
sys.path.insert(0, str(Path(__file__).parent))

from h5_common import (
    OUT,
    load_v13,
    reconstruct_initial_ce,
    set_bounded_threads,
)

from dlx.analysis.floor_independent import curve_metrics
from dlx.protocol import load_manifest

DATASETS = ["gaussian_noise_control", "MNIST", "FashionMNIST", "SVHN", "CIFAR10",
            "STL10_downsampled"]


def main() -> None:
    proto = load_v13()
    set_bounded_threads(proto)
    root = OUT / "part_c"
    root.mkdir(parents=True, exist_ok=True)
    cache = Path("dlx/data_cache/images")
    report = {"protocol_hash": proto["protocol_hash"], "training_performed": False,
              "floor_policy": "historical marginal-code floor excluded from all primary metrics",
              "cells": []}

    for name in DATASETS:
        codes = np.load(cache / f"{name}_codes.npy").astype(np.int64, copy=False)
        tok_meta = json.loads((cache / f"{name}_tokmeta.json").read_text())
        data_version = str(tok_meta["data_sha"])
        floor = float(tok_meta["code_prior_floor_bits"])
        for seed in (0, 1, 2):
            src = Path(f"runs/local/m8/M8__{name}__s{seed}")
            load_manifest(src)
            met = json.loads((src / "metrics.json").read_text())
            init = reconstruct_initial_ce(codes, 512, name, floor, seed, proto,
                                          data_version)
            summary = curve_metrics([0, *met["token_grid"]],
                                    [init, *met["val_ce_bits"]], init)
            report["cells"].append({
                "cell_id": f"H5C/{name}/s{seed}", "dataset": name, "seed": seed,
                "source_cell": met["cell_id"], **summary,
                "historical_metadata": {
                    "marginal_code_entropy_bits": floor,
                    "old_final_gap_bits": met["final_gap_bits"],
                    "old_gap_used_in_revised_metrics": False,
                },
            })
        del codes

    if len(report["cells"]) != 18:
        raise AssertionError(f"expected 18 image cells, got {len(report['cells'])}")
    (root / "part_c_results.json").write_text(json.dumps(report, indent=2))
    print(f"Part C complete: {len(report['cells'])} cells re-scored, zero retrained")


if __name__ == "__main__":
    main()
