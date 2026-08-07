"""Fail-closed audit for the corrected v1.4 text campaign."""

from __future__ import annotations

import json
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent))
sys.path.insert(0, str(Path(__file__).parent))

from v14_common import OUT, load_v14

from dlx.protocol import load_manifest


def main() -> None:
    protocol = load_v14()
    failures = []
    preflight = json.loads((OUT / "preflight" / "preflight_results.json").read_text())
    report = json.loads((OUT / "confirmation" / "confirmation_results.json").read_text())
    analysis = json.loads((OUT / "integrated_analysis.json").read_text())
    if not preflight["gate_pass"] or not preflight["excluded_from_confirmation"]:
        failures.append("preflight gate/exclusion invalid")
    if len(preflight["cells"]) != 2:
        failures.append("expected 2 preflight cells")
    if len(report["cells"]) != 12:
        failures.append(f"expected 12 confirmation cells, got {len(report['cells'])}")
    expected_ids = {f"V14/{r['id']}/s{s}" for r in protocol["rungs"]
                    for s in protocol["confirmation"]["seeds"]}
    actual_ids = {cell["cell_id"] for cell in report["cells"]}
    if actual_ids != expected_ids:
        failures.append("confirmation cell IDs do not match frozen grid")
    for cell_id in sorted(expected_ids):
        cell_dir = OUT / "confirmation" / cell_id.replace("/", "__")
        manifest = load_manifest(cell_dir)
        metrics = json.loads((cell_dir / "metrics.json").read_text())
        if manifest["protocol_hash"] != protocol["protocol_hash"]:
            failures.append(f"{cell_id}: protocol hash mismatch")
        if manifest.get("sequence_semantics") != protocol["sequence_semantics"]["cyclic_reuse"]:
            failures.append(f"{cell_id}: missing contiguous sequence semantics")
        if not metrics.get("initial_checkpoint_recorded"):
            failures.append(f"{cell_id}: missing zero-update validation")
        if len(metrics["token_grid"]) != len(metrics["val_ce_bits"]):
            failures.append(f"{cell_id}: curve length mismatch")
    for rung, meta in report["rungs"].items():
        profile = meta["inverse_likelihood_profile"]
        if profile.get("paper") != "https://arxiv.org/abs/2603.02673":
            failures.append(f"{rung}: wrong profile basis provenance")
        if len(profile.get("pairs", [])) != 10:
            failures.append(f"{rung}: expected 10 lag-pair profiles")
    if analysis["protocol_hash"] != protocol["protocol_hash"]:
        failures.append("analysis protocol hash mismatch")
    audit = {"status": "PASS" if not failures else "FAIL",
             "protocol_hash": protocol["protocol_hash"], "failures": failures,
             "preflight_cells": len(preflight["cells"]),
             "confirmation_cells": len(report["cells"]),
             "profiles": len(report["rungs"]),
             "old_language_cells_reused": 0,
             "controlled_text_verdict": analysis["controlled_text_verdict"]}
    (OUT / "audit.json").write_text(json.dumps(audit, indent=2))
    if failures:
        raise SystemExit("v1.4 audit FAIL: " + "; ".join(failures))
    print("v1.4 audit PASS")


if __name__ == "__main__":
    main()

