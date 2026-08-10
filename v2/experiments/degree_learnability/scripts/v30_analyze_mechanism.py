"""Run the frozen v3.0 controlled architecture-mechanism analysis."""

from __future__ import annotations

import json
from pathlib import Path

from dlx.analysis.character_response import character_mechanism_analysis
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
    protocol = load_frozen_protocol(ROOT / "configs/protocol_v3.0.json")
    kernel_hash = verify_hash_lock(
        OUT / "character_kernel.json", OUT / "character_kernel.sha256"
    )
    ntk_path = OUT / "ntk_cells.json"
    character_path = OUT / "character_results.json"
    ntk_cells = json.loads(ntk_path.read_text())
    character_cells = json.loads(character_path.read_text())
    expected = (
        len(protocol["architectures"])
        * len(protocol["character_training"]["validation_supports"])
        * len(protocol["character_training"]["seeds"])
    )
    if len(character_cells) != expected:
        raise ValueError(
            f"controlled character grid is incomplete: {len(character_cells)}/{expected}"
        )
    decision = protocol["decision"]["mechanism_bootstrap"]
    analysis = character_mechanism_analysis(
        character_cells,
        ntk_cells,
        bootstrap_samples=int(decision["samples"]),
        bootstrap_seed=int(decision["seed"]),
    )
    artifact = {
        "protocol_hash": protocol["protocol_hash"],
        "character_kernel_hash": kernel_hash,
        "ntk_cells_sha256": file_sha256(ntk_path),
        "character_results_sha256": file_sha256(character_path),
        **analysis,
    }
    digest = write_json_once(OUT / "mechanism_analysis.json", artifact)
    write_hash_once(OUT / "mechanism_analysis.sha256", digest)
    regression = analysis["regression"]
    print(
        json.dumps(
            {
                "mechanism_gate_passed": analysis["mechanism_gate_passed"],
                "log_ntk_coefficient": regression["mean_log_ntk_response_coefficient"],
                "bootstrap_95_interval": regression[
                    "support_cluster_bootstrap_95_interval"
                ],
                "degree_one_radius_spearman": analysis["degree_one_radius_spearman"],
                "artifact_sha256": digest,
            },
            indent=2,
        )
    )


if __name__ == "__main__":
    main()
