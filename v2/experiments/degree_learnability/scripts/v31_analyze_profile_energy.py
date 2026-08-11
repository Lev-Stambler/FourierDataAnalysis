"""Summarize where the profiled dataset Fourier energy lies."""

from __future__ import annotations

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


def _summary(values: list[float]) -> dict[str, float]:
    array = np.asarray(values, dtype=np.float64)
    return {
        "mean": float(array.mean()),
        "median": float(np.median(array)),
        "minimum": float(array.min()),
        "maximum": float(array.max()),
    }


def main() -> None:
    protocol = load_frozen_protocol(ROOT / "configs/protocol_v3.1.json")
    profile_manifest_hash = verify_hash_lock(
        OUT / "profile_manifest.json", OUT / "profile_manifest.sha256"
    )
    manifest = json.loads((OUT / "profile_manifest.json").read_text())
    profiles = [
        json.loads((OUT / "profiles" / f"{row['dataset']}.json").read_text())
        for row in manifest["profiles"]
    ]
    if len(profiles) != 72:
        raise ValueError("expected 72 corpus Fourier profiles")

    cells_path = OUT / "fourier_character_results.json"
    cells = json.loads(cells_path.read_text())
    expected_per_support = len(protocol["architectures"]) * len(
        protocol["fourier_character_training"]["seeds"]
    )
    observed: dict[tuple[int, ...], set[tuple[str, int]]] = defaultdict(set)
    hardness: dict[tuple[str, tuple[int, ...]], list[float]] = defaultdict(list)
    for row in cells:
        support = tuple(int(value) for value in row["support"])
        observed[support].add((row["architecture"], int(row["seed"])))
        hardness[(row["architecture"], support)].append(
            float(row["character_hardness"])
        )
    complete_supports = {
        support
        for support, observations in observed.items()
        if len(observations) == expected_per_support
    }

    degree_shares: dict[str, dict[int, list[float]]] = {
        panel: defaultdict(list) for panel in ("all", "pilot", "confirmation")
    }
    normalized_support_energy: dict[str, list[float]] = defaultdict(list)
    current_coverage = []
    for profile in profiles:
        by_degree: dict[int, float] = defaultdict(float)
        total = float(sum(profile["support_energy"].values()))
        covered = 0.0
        for key, raw_energy in profile["support_energy"].items():
            support = tuple(int(value) for value in key.split(","))
            energy = float(raw_energy)
            by_degree[len(support)] += energy
            normalized_support_energy[key].append(energy / total)
            if support in complete_supports:
                covered += energy
        for panel in ("all", profile["panel"]):
            for degree in (1, 2, 3):
                degree_shares[panel][degree].append(by_degree[degree] / total)
        current_coverage.append(covered / total)

    degree_three_spread = {}
    for architecture in (row["id"] for row in protocol["architectures"]):
        values = [
            float(np.median(cell_values))
            for (cell_architecture, support), cell_values in hardness.items()
            if cell_architecture == architecture
            and len(support) == 3
            and len(cell_values)
            == len(protocol["fourier_character_training"]["seeds"])
        ]
        degree_three_spread[architecture] = {
            "measured_supports": len(values),
            **_summary(values),
            "standard_deviation": float(np.std(values)),
        }

    ranked_supports = sorted(
        (
            {
                "support": key,
                "degree": len(key.split(",")),
                "mean_normalized_energy": float(np.mean(values)),
                "measured_for_all_architectures": tuple(
                    int(value) for value in key.split(",")
                )
                in complete_supports,
            }
            for key, values in normalized_support_energy.items()
        ),
        key=lambda row: (-row["mean_normalized_energy"], row["support"]),
    )
    result = {
        "protocol_hash": protocol["protocol_hash"],
        "profile_manifest_hash": profile_manifest_hash,
        "character_results_sha256": file_sha256(cells_path),
        "corpora": len(profiles),
        "character_cells": len(cells),
        "complete_supports_all_architectures": len(complete_supports),
        "degree_energy_share": {
            panel: {
                str(degree): _summary(values)
                for degree, values in sorted(by_degree.items())
            }
            for panel, by_degree in degree_shares.items()
        },
        "current_exact_support_energy_coverage": _summary(current_coverage),
        "degree_three_character_hardness": degree_three_spread,
        "supports_by_mean_normalized_energy": ranked_supports,
        "decision": {
            "drop_degree_three": False,
            "reason": "degree three carries substantial measured Fourier energy, while the sampled character tasks are currently at the CE-area ceiling",
            "next_test": "measure a frozen geometrically diverse degree-three sentinel bank before choosing pooled estimation or exhaustive completion",
        },
    }
    digest = write_json_once(OUT / "profile_energy_analysis.json", result)
    write_hash_once(OUT / "profile_energy_analysis.sha256", digest)
    print(
        json.dumps(
            {
                "degree_energy_share_all": result["degree_energy_share"]["all"],
                "current_coverage": result[
                    "current_exact_support_energy_coverage"
                ],
                "decision": result["decision"],
                "artifact_sha256": digest,
            },
            indent=2,
        )
    )


if __name__ == "__main__":
    main()
