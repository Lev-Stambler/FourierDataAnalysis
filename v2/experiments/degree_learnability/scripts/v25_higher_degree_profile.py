"""Run the bounded sampled degree diagnostic on the v2.4 natural corpora."""

from __future__ import annotations

import argparse
import gzip
import hashlib
import json
import sys
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).parent.parent))
from dlx.analysis.text_panel import v24_profile_data_files
from dlx.profiles.sampled_degree import sampled_token_degree_profile

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs/local/v25_kiss_diagnostic"
PROFILE_DIR = OUT / "profiles"
AUDIT_DIR = OUT / "audit_chains"


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--panel", choices=("confirmation", "development"), default="confirmation"
    )
    parser.add_argument("--dataset", default="all")
    parser.add_argument("--max-positions", type=int, default=100_000)
    parser.add_argument("--chains", type=int, default=48)
    parser.add_argument("--max-degree", type=int, default=6)
    parser.add_argument("--audit-chains", action="store_true")
    parser.add_argument("--product-reference", action="store_true")
    args = parser.parse_args()
    paths = v24_profile_data_files(ROOT, args.panel)
    if args.dataset != "all" and args.dataset not in paths:
        parser.error(f"unknown {args.panel} dataset: {args.dataset}")
    selected = tuple(paths) if args.dataset == "all" else (args.dataset,)
    PROFILE_DIR.mkdir(parents=True, exist_ok=True)
    for dataset in selected:
        output_path = PROFILE_DIR / f"{dataset}.json"
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
                include_chains=args.audit_chains,
                include_product_reference=args.product_reference,
            ),
        }
        chains = result.pop("chains", None)
        output_path.write_text(json.dumps(result, indent=2))
        if chains is not None:
            AUDIT_DIR.mkdir(parents=True, exist_ok=True)
            audit_payload = json.dumps(
                {
                    "dataset": dataset,
                    "data_sha256": result["data_sha256"],
                    "summary_sha256": hashlib.sha256(
                        output_path.read_bytes()
                    ).hexdigest(),
                    "chains": chains,
                },
                separators=(",", ":"),
            ).encode()
            (AUDIT_DIR / f"{dataset}.json.gz").write_bytes(
                gzip.compress(audit_payload, compresslevel=9, mtime=0)
            )
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
