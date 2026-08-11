"""Apply the frozen degree-three Fourier-character pooling gate."""

from __future__ import annotations

import hashlib
import json
from collections import defaultdict
from pathlib import Path

import numpy as np

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
    config_path = ROOT / "configs/degree3_sentinels_v3.1.json"
    config_hash = verify_hash_lock(
        config_path, ROOT / "configs/degree3_sentinels_v3.1.sha256"
    )
    config = json.loads(config_path.read_text())
    sentinel_supports = {
        tuple(int(value) for value in support)
        for group in (
            "high_energy_unmeasured_supports",
            "geometric_and_boundary_stress_supports",
        )
        for support in config["selection"][group]
    }
    result_path = OUT / "fourier_character_results.json"
    rows = json.loads(result_path.read_text())
    sentinel_rows = [
        row for row in rows if tuple(row["support"]) in sentinel_supports
    ]
    if len(sentinel_rows) != config["cells"]:
        raise ValueError(
            f"sentinel grid incomplete: {len(sentinel_rows)}/{config['cells']}"
        )
    pre_sentinel = [
        row for row in rows if tuple(row["support"]) not in sentinel_supports
    ]
    pre_sentinel_payload = (
        json.dumps(
            sorted(pre_sentinel, key=lambda row: row["cell_id"]), indent=2
        )
        + "\n"
    ).encode()
    if (
        hashlib.sha256(pre_sentinel_payload).hexdigest()
        != config["character_results_sha256_before_sentinels"]
    ):
        raise ValueError("pre-sentinel result lock mismatch")

    seeds = protocol["fourier_character_training"]["seeds"]
    grouped: dict[tuple[str, tuple[int, ...]], list[float]] = defaultdict(list)
    for row in rows:
        if int(row["degree"]) == 3:
            grouped[(row["architecture"], tuple(row["support"]))].append(
                float(row["character_hardness"])
            )

    minimum_required = config["pooling_gate"][
        "minimum_median_normalized_ce_area_for_every_sampled_architecture_support"
    ]
    maximum_range = config["pooling_gate"][
        "maximum_within_architecture_range_across_all_17_sampled_degree3_supports"
    ]
    architecture_results = {}
    for architecture in (row["id"] for row in protocol["architectures"]):
        medians = {
            ",".join(str(value) for value in support): float(np.median(values))
            for (cell_architecture, support), values in grouped.items()
            if cell_architecture == architecture and len(values) == len(seeds)
        }
        values = list(medians.values())
        if len(values) != 17:
            raise ValueError(
                f"expected 17 sampled degree-three supports for {architecture}"
            )
        observed_range = max(values) - min(values)
        architecture_results[architecture] = {
            "support_medians": dict(sorted(medians.items())),
            "sampled_supports": len(values),
            "pooled_median": float(np.median(values)),
            "minimum_support_median": min(values),
            "maximum_support_median": max(values),
            "support_range": observed_range,
            "minimum_gate_passed": min(values) >= minimum_required,
            "range_gate_passed": observed_range <= maximum_range,
        }
    passed = all(
        row["minimum_gate_passed"] and row["range_gate_passed"]
        for row in architecture_results.values()
    )
    result = {
        "protocol_hash": protocol["protocol_hash"],
        "sentinel_config_hash": config_hash,
        "character_results_sha256": file_sha256(result_path),
        "sentinel_cells": len(sentinel_rows),
        "architecture_results": architecture_results,
        "pooling_gate_passed": passed,
        "action": (
            config["pooling_gate"]["pass_action"]
            if passed
            else config["pooling_gate"]["fail_action"]
        ),
    }
    digest = write_json_once(OUT / "degree3_sentinel_analysis.json", result)
    write_hash_once(OUT / "degree3_sentinel_analysis.sha256", digest)
    print(
        json.dumps(
            {
                "pooling_gate_passed": passed,
                "architecture_summary": {
                    architecture: {
                        key: row[key]
                        for key in (
                            "pooled_median",
                            "minimum_support_median",
                            "support_range",
                        )
                    }
                    for architecture, row in architecture_results.items()
                },
                "artifact_sha256": digest,
            },
            indent=2,
        )
    )


if __name__ == "__main__":
    main()
