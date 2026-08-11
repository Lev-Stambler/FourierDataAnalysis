"""Independent artifact and pairing audit for the Fourier-only experiment."""

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
    protocol = load_frozen_protocol(ROOT / "configs/protocol_v3.1.json")
    checks: list[dict] = []
    locks = {
        name: verify_hash_lock(OUT / f"{name}.json", OUT / f"{name}.sha256")
        for name in ("data_manifest", "fourier_ce_kernel")
    }
    manifest = json.loads((OUT / "data_manifest.json").read_text())
    corpora = manifest["corpora"]
    _check(len(corpora) == 72, "72 corpus rows", checks)
    _check(len({row["dataset"] for row in corpora}) == 72, "dataset IDs unique", checks)
    _check(
        len({row["byte_stream_sha256"] for row in corpora}) == 72,
        "byte streams unique",
        checks,
    )
    pilot = {row["dataset"] for row in corpora if row["panel"] == "pilot"}
    confirmation = {row["dataset"] for row in corpora if row["panel"] == "confirmation"}
    _check(
        pilot.isdisjoint(confirmation), "pilot/confirmation sources disjoint", checks
    )
    strata = sorted({row["stratum"] for row in corpora})
    _check(len(strata) == 6, "six source strata", checks)
    _check(
        all(
            sum(row["panel"] == panel and row["stratum"] == stratum for row in corpora)
            == expected
            for panel, expected in (("pilot", 4), ("confirmation", 8))
            for stratum in strata
        ),
        "balanced source panels",
        checks,
    )

    spec = protocol["fourier_character_training"]
    supports = enumerate_supports(spec["lags"], max_degree=spec["max_degree"])
    character = json.loads((OUT / "fourier_character_results.json").read_text())
    expected_character = (
        len(protocol["architectures"]) * len(supports) * len(spec["seeds"])
    )
    _check(len(character) == expected_character == 945, "945 Fourier CE cells", checks)
    _check(
        len({row["cell_id"] for row in character}) == expected_character,
        "Fourier CE cell IDs unique",
        checks,
    )
    _check(
        all(row["protocol_hash"] == protocol["protocol_hash"] for row in character),
        "Fourier CE cells use corrected protocol",
        checks,
    )
    _check(
        all("H100" in row["remote"]["gpu"] for row in character),
        "Fourier CE cells ran on H100",
        checks,
    )
    kernel = json.loads((OUT / "fourier_ce_kernel.json").read_text())
    _check(
        kernel["results_sha256"] == file_sha256(OUT / "fourier_character_results.json"),
        "CE kernel locks complete Fourier result grid",
        checks,
    )
    completed_stage = "fourier_character_ce"
    timing = {
        "fourier_character_gpu_cell_seconds": sum(
            row["remote"]["wallclock_seconds"] for row in character
        )
    }

    if (OUT / "profile_manifest.sha256").exists():
        locks["profile_manifest"] = verify_hash_lock(
            OUT / "profile_manifest.json", OUT / "profile_manifest.sha256"
        )
        profiles = json.loads((OUT / "profile_manifest.json").read_text())["profiles"]
        _check(len(profiles) == 72, "72 Fourier profile summaries and audits", checks)
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
                f"{row['dataset']} chain hash",
                checks,
            )
            decoded = json.loads(gzip.decompress(audit.read_bytes()))
            _check(
                decoded["summary_sha256"] == row["profile_sha256"],
                f"{row['dataset']} chain audit links profile",
                checks,
            )
        completed_stage = "profile"

    if (OUT / "pilot_analysis.sha256").exists():
        locks["pilot_analysis"] = verify_hash_lock(
            OUT / "pilot_analysis.json", OUT / "pilot_analysis.sha256"
        )
        pilot_cells = json.loads((OUT / "pilot_results.json").read_text())
        _check(len(pilot_cells) == 240, "240 pilot training cells", checks)
        _check(
            all(
                row["fourier_ce_kernel_hash"] == locks["fourier_ce_kernel"]
                for row in pilot_cells
            ),
            "pilot cells reference Fourier CE kernel",
            checks,
        )
        timing["pilot_gpu_cell_seconds"] = sum(
            row["remote"]["wallclock_seconds"] for row in pilot_cells
        )
        completed_stage = "pilot"
        pilot_analysis = json.loads((OUT / "pilot_analysis.json").read_text())
        if pilot_analysis["continuation_gate_passed"]:
            for name in ("predictions", "analysis"):
                locks[name] = verify_hash_lock(
                    OUT / f"{name}.json", OUT / f"{name}.sha256"
                )
            confirmation_cells = json.loads(
                (OUT / "confirmation_results.json").read_text()
            )
            _check(len(confirmation_cells) == 480, "480 confirmation cells", checks)
            _check(
                all(
                    row["prediction_lock_hash"] == locks["predictions"]
                    for row in confirmation_cells
                ),
                "confirmation cells use frozen predictions",
                checks,
            )
            timing["confirmation_gpu_cell_seconds"] = sum(
                row["remote"]["wallclock_seconds"] for row in confirmation_cells
            )
            completed_stage = "confirmation"

    result = {
        "protocol_hash": protocol["protocol_hash"],
        "status": "PASS" if all(row["passed"] for row in checks) else "FAIL",
        "completed_stage": completed_stage,
        "locks": locks,
        "checks": checks,
        "timing": timing,
    }
    write_json_once(OUT / "audit.json", result)
    print(
        json.dumps(
            {key: result[key] for key in ("status", "completed_stage", "timing")},
            indent=2,
        )
    )
    return result


if __name__ == "__main__":
    main()
