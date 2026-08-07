"""Fail-closed audit for corrected conditional Fourier spectrum v1.6."""

from __future__ import annotations

import json
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))

from v16_conditional_spectrum import OUT, SOURCE, load_protocol


def main() -> None:
    protocol = load_protocol()
    source_audit = json.loads((SOURCE / "audit.json").read_text())
    result = json.loads((OUT / "integrated_analysis.json").read_text())
    failures = []
    if source_audit["status"] != "PASS":
        failures.append("source v1.4 audit is not PASS")
    if source_audit["protocol_hash"] != protocol["source_v1_4_protocol_hash"]:
        failures.append("source protocol hash mismatch")
    if result["protocol_hash"] != protocol["protocol_hash"]:
        failures.append("v1.6 protocol hash mismatch")
    if len(result["rungs"]) != 4:
        failures.append("expected four rungs")
    for row in result["rungs"]:
        spectrum = row["spectrum"]
        if len(spectrum["level_weights"]) != 3:
            failures.append(f"{row['rung']}: expected levels zero through two")
        if any(weight < 0.0 for weight in spectrum["level_weights"]):
            failures.append(f"{row['rung']}: negative level weight")
        if abs(sum(spectrum["level_weights"]) - spectrum["total_square_energy"]) > 1e-12:
            failures.append(f"{row['rung']}: Parseval sum mismatch")
        if spectrum["level_cardinalities"] != [1, 510, 65025]:
            failures.append(f"{row['rung']}: coefficient cardinality mismatch")
    if result["controlled"]["copy_spectral_degree"] != 1.0:
        failures.append("copy planted spectral degree mismatch")
    if result["controlled"]["markov2_spectral_degree"] != 2.0:
        failures.append("Markov-2 planted spectral degree mismatch")
    if result["training_or_profile_recomputation"] != 0:
        failures.append("unexpected recomputation")
    audit = {
        "status": "PASS" if not failures else "FAIL",
        "protocol_hash": protocol["protocol_hash"],
        "source_v1_4_protocol_hash": protocol["source_v1_4_protocol_hash"],
        "failures": failures,
        "rungs": len(result["rungs"]),
        "degrees": [0, 1, 2],
        "invalid_sparse_lift_artifacts_reused": 0,
        "training_cells_recomputed": 0,
    }
    (OUT / "audit.json").write_text(json.dumps(audit, indent=2))
    if failures:
        raise SystemExit("v1.6 audit FAIL: " + "; ".join(failures))
    print("v1.6 audit PASS")


if __name__ == "__main__":
    main()
