"""M9: R3 tabular ladder — resolve datasets, compute spectra + influences where
enumerable, run the fixed MLP across seeds, write manifests.

Runs locally on CPU (small models); Modal path available via --modal later if needed.
"""

from __future__ import annotations

import json
import sys
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent))

from dlx.data.tabular import LADDER, dataset_spectrum, load_tabular, spectral_influences
from dlx.grid import load_protocol
from dlx.training.tabular_run import tabular_run

OUT = Path("runs/local/m9")
SEEDS = (0, 1, 2)


def main():
    proto = load_protocol()
    OUT.mkdir(parents=True, exist_ok=True)
    names = [nm for nm, _ in LADDER]
    report = {"datasets": {}, "cells": []}

    for name, enumerable_expected in LADDER:
        t0 = time.time()
        print(f"[{name}] loading...", flush=True)
        try:
            data = load_tabular(name, max_rows=20_000)
        except Exception as e:
            print(f"  LOAD FAILED: {e}", flush=True)
            report["datasets"][name] = {"error": str(e)[:200]}
            continue
        grid_ok = None
        spectrum = None
        try:
            spectrum = dataset_spectrum(data["X"], data["y"], data["n_classes"])
            grid_ok = True
        except ValueError as e:
            grid_ok = False
        if spectrum is not None:
            sens = spectral_influences(data["X"], data["y"], data["n_classes"])
            spectrum["spectral_influences"] = sens.tolist()
        report["datasets"][name] = {
            "openml": data["openml"], "n_rows_used": len(data["X"]),
            "n_features": int(data["X"].shape[1]), "n_classes": data["n_classes"],
            "q_features": data["q_features"], "enumerable": grid_ok,
            "enumerable_expected": enumerable_expected,
            "spectrum_W": spectrum["W"] if spectrum else None,
            "load_seconds": round(time.time() - t0, 1),
        }
        print(f"  rows={len(data['X'])} feats={data['X'].shape[1]} "
              f"classes={data['n_classes']} enumerable={grid_ok}", flush=True)

        for seed in SEEDS:
            cell_id = f"M9/{name}/s{seed}"
            m = tabular_run(data, seed, OUT / cell_id.replace("/", "__"), cell_id,
                            proto["protocol_hash"])
            report["cells"].append({
                "cell_id": cell_id, "T_star_rows": m["T_star_rows"],
                "best_val_ce": round(m["best_val_ce_bits"], 4),
                "learning_amount": round(m["learning_amount_bits"], 4),
                "total_influence_perm": round(m["total_influence_perm"], 4),
            })
            print(f"  {cell_id}: T*={m['T_star_rows']} best={m['best_val_ce_bits']:.4f}",
                  flush=True)

    (OUT / "m9_report.json").write_text(json.dumps(report, indent=2, default=str))
    print("wrote", OUT / "m9_report.json")


if __name__ == "__main__":
    main()
