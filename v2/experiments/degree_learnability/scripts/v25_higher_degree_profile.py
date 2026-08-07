"""Run the bounded sampled degree diagnostic on the v2.4 natural corpora."""

from __future__ import annotations

import argparse
import hashlib
import json
import sys
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).parent.parent))
sys.path.insert(0, str(Path(__file__).parent))

from v24_development_analysis import DATA_FILES
from v24_freeze_predictions import DATASETS, OUT

from dlx.profiles.sampled_degree import (
    invert_product_reference_degree_curve,
    sampled_token_degree_profile,
)

ROOT = Path(__file__).parent.parent
PROFILE_DIR = OUT / "higher_degree_profiles"


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--panel", choices=("confirmation", "development"), default="confirmation"
    )
    parser.add_argument("--dataset", default="all")
    parser.add_argument("--max-positions", type=int, default=100_000)
    parser.add_argument("--chains", type=int, default=48)
    parser.add_argument("--max-degree", type=int, default=6)
    parser.add_argument("--inversion-only", action="store_true")
    args = parser.parse_args()
    paths = (
        {dataset: ROOT / f"dlx/data_cache/v24_{dataset}_bytes_n2000000.npy" for dataset in DATASETS}
        if args.panel == "confirmation"
        else DATA_FILES
    )
    if args.dataset != "all" and args.dataset not in paths:
        parser.error(f"unknown {args.panel} dataset: {args.dataset}")
    selected = tuple(paths) if args.dataset == "all" else (args.dataset,)
    PROFILE_DIR.mkdir(parents=True, exist_ok=True)
    for dataset in selected:
        output_path = PROFILE_DIR / f"{dataset}.json"
        if args.inversion_only:
            result = json.loads(output_path.read_text())
            result["product_reference_inverted_level_weights"] = (
                invert_product_reference_degree_curve(
                    [
                        row["mean_conditional_collision_energy"]
                        for row in result["degree_curve"]
                    ],
                    result["n_coordinates"],
                )
            )
            result["product_reference_warning"] = (
                "exact only for orthogonal coordinate subspaces, such as a product "
                "input measure; signed levels diagnose dependence or estimation bias"
            )
            output_path.write_text(json.dumps(result, indent=2))
            print(f"{dataset}: inversion added", flush=True)
            continue
        path = paths[dataset]
        tokens = np.load(path, mmap_mode="r")
        result = {
            "dataset": dataset,
            "data_sha256": hashlib.sha256(np.asarray(tokens).tobytes()).hexdigest(),
            **sampled_token_degree_profile(
                tokens,
                lags=tuple(range(1, 17)),
                max_degree=args.max_degree,
                n_chains=args.chains,
                max_positions=args.max_positions,
                seed=2500,
                delta=0.05,
            ),
        }
        output_path.write_text(json.dumps(result, indent=2))
        compact = [
            {
                "degree": row["degree"],
                "energy": round(row["mean_conditional_collision_energy"], 6),
                "increment": (
                    None
                    if row["mean_increment_from_previous_degree"] is None
                    else round(row["mean_increment_from_previous_degree"], 6)
                ),
                "coverage": round(row["mean_context_coverage"], 4),
            }
            for row in result["degree_curve"]
        ]
        print(f"{dataset}: {json.dumps(compact)}", flush=True)


if __name__ == "__main__":
    main()
