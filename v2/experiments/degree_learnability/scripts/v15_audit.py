"""Fail-closed audit for the analysis-only v1.5 variance correction."""

from __future__ import annotations

import json
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))

from v15_variance_analysis import OUT, SOURCE, load_protocol


def main() -> None:
    protocol = load_protocol()
    source_audit = json.loads((SOURCE / "audit.json").read_text())
    result = json.loads((OUT / "integrated_analysis.json").read_text())
    failures = []
    if source_audit["status"] != "PASS":
        failures.append("source v1.4 audit is not PASS")
    if source_audit["protocol_hash"] != protocol["source_v1_4_protocol_hash"]:
        failures.append("source v1.4 protocol hash mismatch")
    if result["protocol_hash"] != protocol["protocol_hash"]:
        failures.append("v1.5 protocol hash mismatch")
    if len(result["rungs"]) != 4:
        failures.append("expected four text rungs")
    for rung in result["rungs"]:
        if len(rung["pair_profiles"]) != 10:
            failures.append(f"{rung['rung']}: expected ten pair profiles")
        for pair in rung["pair_profiles"]:
            c1 = pair["concentration_leq_1"]
            c2 = pair["concentration_leq_2"]
            if c1 is not None and not 0.0 <= c1 <= 1.0:
                failures.append(f"{rung['rung']}/{pair['lags']}: C<=1 out of range")
            if c2 is not None and c2 != 1.0:
                failures.append(f"{rung['rung']}/{pair['lags']}: C<=2 is not one")
    if result["training_or_profile_recomputation"] != 0:
        failures.append("v1.5 unexpectedly reports recomputation")
    if result["controlled_consistency_check"]["status"] != "PASS":
        failures.append("controlled concentration consistency check failed")
    audit = {
        "status": "PASS" if not failures else "FAIL",
        "protocol_hash": protocol["protocol_hash"],
        "source_v1_4_protocol_hash": protocol["source_v1_4_protocol_hash"],
        "failures": failures,
        "rungs": len(result["rungs"]),
        "pair_profiles": sum(len(row["pair_profiles"]) for row in result["rungs"]),
        "training_cells_recomputed": 0,
        "profiles_recomputed": 0,
    }
    (OUT / "audit.json").write_text(json.dumps(audit, indent=2))
    if failures:
        raise SystemExit("v1.5 audit FAIL: " + "; ".join(failures))
    print("v1.5 audit PASS")


if __name__ == "__main__":
    main()
