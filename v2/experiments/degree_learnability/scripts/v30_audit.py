"""Independent artifact and pairing audit for the v3.0 experiment."""

from __future__ import annotations

import gzip
import json
from pathlib import Path

from dlx.analysis.character_response import enumerate_supports
from dlx.protocol.frozen import (
    file_sha256,
    load_frozen_protocol,
    verify_hash_lock,
    write_json_once,
)

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs/local/v30_architecture_spectrum"


def _check(condition: bool, label: str, checks: list[dict]) -> None:
    checks.append({"check": label, "passed": bool(condition)})


def main() -> dict:
    protocol = load_frozen_protocol(ROOT / "configs/protocol_v3.0.json")
    checks: list[dict] = []
    locks = {}
    for name in ("data_manifest", "character_kernel", "mechanism_analysis"):
        locks[name] = verify_hash_lock(OUT / f"{name}.json", OUT / f"{name}.sha256")
        _check(True, f"{name} hash lock", checks)
    manifest = json.loads((OUT / "data_manifest.json").read_text())
    corpora = manifest["corpora"]
    _check(len(corpora) == 72, "72 unique corpus rows", checks)
    _check(len({row["dataset"] for row in corpora}) == 72, "dataset IDs unique", checks)
    _check(
        len({row["byte_stream_sha256"] for row in corpora}) == 72,
        "byte streams unique",
        checks,
    )
    pilot_sources = {row["dataset"] for row in corpora if row["panel"] == "pilot"}
    confirmation_sources = {
        row["dataset"] for row in corpora if row["panel"] == "confirmation"
    }
    _check(
        pilot_sources.isdisjoint(confirmation_sources),
        "pilot and confirmation source IDs disjoint",
        checks,
    )
    strata = sorted({row["stratum"] for row in corpora})
    by_panel_stratum = {
        (panel, stratum): sum(
            row["panel"] == panel and row["stratum"] == stratum for row in corpora
        )
        for panel, expected in (("pilot", 4), ("confirmation", 8))
        for stratum in strata
    }
    _check(
        all(
            value == (4 if panel == "pilot" else 8)
            for (panel, _), value in by_panel_stratum.items()
        ),
        "balanced six-stratum panels",
        checks,
    )
    _check(len(strata) == protocol["data"]["confirmation_strata"], "six strata", checks)

    ntk = json.loads((OUT / "ntk_cells.json").read_text())
    expected_supports = len(
        enumerate_supports(
            protocol["character_kernel"]["lags"],
            max_degree=protocol["character_kernel"]["max_degree"],
        )
    )
    _check(len(ntk) == 40, "40 NTK cells", checks)
    _check(
        all(len(cell["rows"]) == expected_supports for cell in ntk),
        "63 supports in every NTK cell",
        checks,
    )
    _check(
        all("H100" in cell["remote"]["gpu"] for cell in ntk),
        "NTK cells ran on H100",
        checks,
    )
    character = json.loads((OUT / "character_results.json").read_text())
    _check(len(character) == 270, "270 controlled character cells", checks)
    _check(
        len({cell["cell_id"] for cell in character}) == len(character),
        "controlled cell IDs unique",
        checks,
    )
    _check(
        all(cell["kernel_hash"] == locks["character_kernel"] for cell in character),
        "controlled cells reference locked kernel",
        checks,
    )

    mechanism = json.loads((OUT / "mechanism_analysis.json").read_text())
    completed_stage = "mechanism"
    timing = {
        "ntk_gpu_cell_seconds": sum(
            cell["remote"]["wallclock_seconds"] for cell in ntk
        ),
        "character_gpu_cell_seconds": sum(
            cell["remote"]["wallclock_seconds"] for cell in character
        ),
    }
    if mechanism["mechanism_gate_passed"]:
        for name in ("profile_manifest", "pilot_analysis"):
            locks[name] = verify_hash_lock(OUT / f"{name}.json", OUT / f"{name}.sha256")
            _check(True, f"{name} hash lock", checks)
        profiles = json.loads((OUT / "profile_manifest.json").read_text())["profiles"]
        _check(len(profiles) == 72, "72 profile summaries and audits", checks)
        for row in profiles:
            profile = OUT / "profiles" / f"{row['dataset']}.json"
            audit = OUT / "audit_chains" / f"{row['dataset']}.json.gz"
            _check(
                file_sha256(profile) == row["profile_sha256"],
                f"{row['dataset']} profile hash",
                checks,
            )
            _check(
                file_sha256(audit) == row["audit_sha256"],
                f"{row['dataset']} chain-audit hash",
                checks,
            )
            decoded = json.loads(gzip.decompress(audit.read_bytes()))
            _check(
                decoded["summary_sha256"] == row["profile_sha256"],
                f"{row['dataset']} audit links summary",
                checks,
            )
        pilot = json.loads((OUT / "pilot_results.json").read_text())
        _check(len(pilot) == 240, "240 pilot training cells", checks)
        _check(
            all(cell["prediction_lock_hash"] is None for cell in pilot),
            "pilot preceded prediction lock",
            checks,
        )
        _check(
            all("H100" in cell["remote"]["gpu"] for cell in pilot),
            "pilot training cells ran on H100",
            checks,
        )
        for dataset in {cell["dataset"] for cell in pilot}:
            selected = [cell for cell in pilot if cell["dataset"] == dataset]
            _check(
                len({cell["validation_starts_sha256"] for cell in selected}) == 1,
                f"{dataset} validation chunks paired",
                checks,
            )
        timing["pilot_gpu_cell_seconds"] = sum(
            cell["remote"]["wallclock_seconds"] for cell in pilot
        )
        completed_stage = "pilot"
        pilot_analysis = json.loads((OUT / "pilot_analysis.json").read_text())
        if pilot_analysis["continuation_gate_passed"]:
            for name in ("predictions", "analysis"):
                locks[name] = verify_hash_lock(
                    OUT / f"{name}.json", OUT / f"{name}.sha256"
                )
                _check(True, f"{name} hash lock", checks)
            confirmation = json.loads((OUT / "confirmation_results.json").read_text())
            _check(len(confirmation) == 480, "480 confirmation training cells", checks)
            _check(
                all(
                    cell["prediction_lock_hash"] == locks["predictions"]
                    for cell in confirmation
                ),
                "confirmation cells reference frozen predictions",
                checks,
            )
            _check(
                all("H100" in cell["remote"]["gpu"] for cell in confirmation),
                "confirmation training cells ran on H100",
                checks,
            )
            for dataset in {cell["dataset"] for cell in confirmation}:
                selected = [cell for cell in confirmation if cell["dataset"] == dataset]
                _check(
                    len({cell["validation_starts_sha256"] for cell in selected}) == 1,
                    f"{dataset} validation chunks paired",
                    checks,
                )
            timing["confirmation_gpu_cell_seconds"] = sum(
                cell["remote"]["wallclock_seconds"] for cell in confirmation
            )
            completed_stage = "confirmation"
    result = {
        "protocol_hash": protocol["protocol_hash"],
        "status": "PASS" if all(row["passed"] for row in checks) else "FAIL",
        "completed_stage": completed_stage,
        "mechanism_gate_passed": mechanism["mechanism_gate_passed"],
        "locks": locks,
        "checks": checks,
        "timing": timing,
    }
    write_json_once(OUT / "audit.json", result)
    print(
        json.dumps(
            {
                "status": result["status"],
                "completed_stage": completed_stage,
                "checks": len(checks),
                "timing": timing,
            },
            indent=2,
        )
    )
    return result


if __name__ == "__main__":
    main()
