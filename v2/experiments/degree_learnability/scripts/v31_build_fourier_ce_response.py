"""Build the exact-degree-1/2 and pooled-degree-3 Fourier CE response surface."""

from __future__ import annotations

import json
from pathlib import Path

from dlx.analysis.character_response import (
    empirical_character_ce_kernel,
    enumerate_supports,
    support_key,
)
from dlx.protocol.frozen import (
    file_sha256,
    load_frozen_protocol,
    verify_hash_lock,
    write_hash_once,
    write_json_once,
)

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs/local/v30_architecture_spectrum"


def main() -> None:
    protocol = load_frozen_protocol(ROOT / "configs/protocol_v3.1.json")
    sentinel_hash = verify_hash_lock(
        OUT / "degree3_sentinel_analysis.json",
        OUT / "degree3_sentinel_analysis.sha256",
    )
    sentinel = json.loads((OUT / "degree3_sentinel_analysis.json").read_text())
    if not sentinel["pooling_gate_passed"]:
        raise ValueError("degree-three pooling gate did not pass")

    result_path = OUT / "fourier_character_results.json"
    cells = json.loads(result_path.read_text())
    spec = protocol["fourier_character_training"]
    supports = enumerate_supports(spec["lags"], max_degree=spec["max_degree"])
    architectures = [row["id"] for row in protocol["architectures"]]
    pooled_degree_three = {
        architecture: sentinel["architecture_results"][architecture][
            "pooled_median"
        ]
        for architecture in architectures
    }
    response = empirical_character_ce_kernel(
        cells,
        architectures=architectures,
        supports=supports,
        seeds=spec["seeds"],
        pooled_hardness_by_degree={3: pooled_degree_three},
    )

    observed_supports = {
        support_key(row["support"])
        for row in cells
        if len(
            [
                candidate
                for candidate in cells
                if candidate["architecture"] == row["architecture"]
                and candidate["support"] == row["support"]
            ]
        )
        == len(spec["seeds"])
    }
    exact = sorted(
        support_key(support)
        for support in supports
        if support_key(support) in observed_supports
    )
    imputed = sorted(
        support_key(support)
        for support in supports
        if support_key(support) not in observed_supports
    )
    if len(exact) != 45 or len(imputed) != 18:
        raise ValueError(
            f"expected 45 exact and 18 pooled supports, got {len(exact)}/{len(imputed)}"
        )
    if any(len(key.split(",")) != 3 for key in imputed):
        raise ValueError("only degree-three supports may be pooled")

    artifact = {
        "protocol_hash": protocol["protocol_hash"],
        "definition": "median held-out normalized CE curve area for each exact Fourier character; unmeasured degree-three values use the frozen architecture-specific sentinel median",
        "character_results_sha256": file_sha256(result_path),
        "degree3_sentinel_analysis_hash": sentinel_hash,
        "character_cells": len(cells),
        "exact_supports": exact,
        "pooled_degree_three_supports": imputed,
        "pooled_degree_three_hardness": pooled_degree_three,
        "architecture_hardness": response,
    }
    digest = write_json_once(OUT / "fourier_ce_kernel.json", artifact)
    write_hash_once(OUT / "fourier_ce_kernel.sha256", digest)
    print(
        json.dumps(
            {
                "character_cells": len(cells),
                "exact_supports": len(exact),
                "pooled_degree_three_supports": len(imputed),
                "artifact_sha256": digest,
            },
            indent=2,
        )
    )


if __name__ == "__main__":
    main()
