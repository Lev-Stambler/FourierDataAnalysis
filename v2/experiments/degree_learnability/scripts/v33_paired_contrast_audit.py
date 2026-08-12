"""Independent integrity audit for the v3.3 paired contrast diagnostic."""

from __future__ import annotations

import json
import math
from pathlib import Path

from dlx.analysis.paired_contrast import holm_adjust
from dlx.protocol.frozen import (
    file_sha256,
    load_frozen_protocol,
    verify_hash_lock,
    write_hash_once,
    write_json_once,
)

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs/local/v30_architecture_spectrum"


def _check(condition: bool, label: str, checks: list[dict]) -> None:
    checks.append({"check": label, "passed": bool(condition)})


def main() -> dict:
    protocol = load_frozen_protocol(ROOT / "configs/protocol_v3.3.json")
    measurement = load_frozen_protocol(ROOT / "configs/protocol_v3.1.json")
    analysis_hash = verify_hash_lock(
        OUT / "paired_contrast_analysis.json",
        OUT / "paired_contrast_analysis.sha256",
    )
    analysis = json.loads((OUT / "paired_contrast_analysis.json").read_text())
    manifest = json.loads((OUT / "data_manifest.json").read_text())
    checks: list[dict] = []

    _check(
        analysis["protocol_hash"] == protocol["protocol_hash"],
        "analysis uses v3.3 protocol",
        checks,
    )
    _check(
        analysis["measurement_protocol_hash"] == measurement["protocol_hash"],
        "analysis uses v3.1 measurements",
        checks,
    )
    _check(
        analysis["locked_inputs"] == protocol["locked_inputs"],
        "analysis retains exact locked input hashes",
        checks,
    )
    file_map = {
        "data_manifest_sha256": "data_manifest.json",
        "profile_manifest_sha256": "profile_manifest.json",
        "fourier_ce_kernel_sha256": "fourier_ce_kernel.json",
        "pilot_results_sha256": "pilot_results.json",
        "pilot_analysis_sha256": "pilot_analysis.json",
        "expansion_results_sha256": "expansion_results.json",
        "expansion_analysis_sha256": "expansion_analysis.json",
        "expansion_audit_sha256": "expansion_audit.json",
    }
    _check(
        all(
            file_sha256(OUT / path) == protocol["locked_inputs"][name]
            for name, path in file_map.items()
        ),
        "all locked input files match",
        checks,
    )

    rows = analysis["rows"]
    development = [row for row in rows if row["panel"] == "development"]
    expansion = [row for row in rows if row["panel"] == "expansion"]
    expected_development = {
        row["dataset"] for row in manifest["corpora"] if row["panel"] == "pilot"
    }
    expected_expansion = {
        row["dataset"]
        for row in manifest["corpora"]
        if row["panel"] == "confirmation"
    }
    _check(
        len(development) == 24
        and {row["dataset"] for row in development} == expected_development,
        "exact 24-corpus development panel",
        checks,
    )
    _check(
        len(expansion) == 48
        and {row["dataset"] for row in expansion} == expected_expansion,
        "exact 48-corpus expansion panel",
        checks,
    )
    _check(
        expected_development.isdisjoint(expected_expansion),
        "development and expansion corpus IDs disjoint",
        checks,
    )
    targets = protocol["co_primary_targets"] + protocol["diagnostic_targets"]
    _check(
        all(
            row["left_architecture"] == "rope"
            and row["right_architecture"] == "nope"
            and math.isclose(
                row["delta_fourier_ce_overlap"],
                row["left_fourier_ce_overlap"] - row["right_fourier_ce_overlap"],
            )
            and all(
                math.isclose(
                    row[f"delta_{target}"],
                    row[f"left_{target}"] - row[f"right_{target}"],
                )
                for target in targets
            )
            for row in rows
        ),
        "all paired contrasts use RoPE minus NoPE orientation",
        checks,
    )

    pilot_cells = json.loads((OUT / "pilot_results.json").read_text())
    expansion_cells = json.loads((OUT / "expansion_results.json").read_text())
    _check(
        all(
            sum(
                cell["dataset"] == dataset
                and cell["architecture"] == architecture
                for cell in pilot_cells
            )
            == 2
            for dataset in expected_development
            for architecture in ("rope", "nope")
        ),
        "two pilot seeds per paired architecture",
        checks,
    )
    _check(
        all(
            sum(
                cell["dataset"] == dataset
                and cell["architecture"] == architecture
                for cell in expansion_cells
            )
            == 2
            for dataset in expected_expansion
            for architecture in ("rope", "nope")
        ),
        "two expansion seeds per paired architecture",
        checks,
    )

    transfers = analysis["development_to_expansion_transfer"]
    _check(
        all(
            set(transfers[target]["primary_mean_baseline"]["model"]["training_datasets"])
            == expected_development
            for target in targets
        ),
        "primary models train only on development corpora",
        checks,
    )
    _check(
        all(
            set(
                transfers[target]["stratum_fixed_effect_sensitivity"][model][
                    "training_datasets"
                ]
            )
            == expected_development
            for target in targets
            for model in ("baseline_model", "fourier_model")
        ),
        "sensitivity models train only on development corpora",
        checks,
    )
    _check(
        all(
            transfers[target]["primary_mean_baseline"][
                "stratified_paired_corpus_bootstrap"
            ]["seed"]
            == protocol["inference"]["transfer_bootstrap_seed"] + index
            for index, target in enumerate(targets)
        ),
        "transfer bootstrap seeds follow protocol",
        checks,
    )

    primary = analysis["primary_inference"]
    raw = {
        target: analysis["associations"][target]["expansion"][
            "pearson_two_sided_p"
        ]
        for target in protocol["co_primary_targets"]
    }
    adjusted = holm_adjust(raw)
    _check(
        all(
            math.isclose(primary[target]["holm_adjusted_p"], adjusted[target])
            and primary[target]["directionally_aligned"]
            == (
                analysis["associations"][target]["expansion"][
                    "ols_slope_raw_units"
                ]
                > 0.0
            )
            for target in protocol["co_primary_targets"]
        ),
        "Holm correction and directional labels recompute exactly",
        checks,
    )
    _check(
        len(analysis["all_architecture_pair_diagnostic"]) == 10,
        "all ten unordered architecture pairs reported",
        checks,
    )
    _check(
        analysis["interpretation"]["confirmatory_verdict"] is None,
        "post-outcome diagnostic makes no confirmatory verdict",
        checks,
    )

    result = {
        "status": "PASS" if all(row["passed"] for row in checks) else "FAIL",
        "protocol_hash": protocol["protocol_hash"],
        "analysis_hash": analysis_hash,
        "checks": checks,
    }
    digest = write_json_once(OUT / "paired_contrast_audit.json", result)
    write_hash_once(OUT / "paired_contrast_audit.sha256", digest)
    print(json.dumps({**result, "audit_hash": digest}, indent=2))
    return result


if __name__ == "__main__":
    main()
