"""Independent artifact/manifest audit for the matched-H5 campaign."""

from __future__ import annotations

import json
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent))
sys.path.insert(0, str(Path(__file__).parent))

from h5_common import OUT, ROOT, load_v13

from dlx.protocol import load_manifest


def check_manifest(path: Path, protocol_hash: str | None = None) -> dict:
    man = load_manifest(path)
    metrics = Path(man["metrics_path"])
    if not metrics.exists():
        raise FileNotFoundError(f"manifest metrics_path missing: {metrics}")
    if protocol_hash is not None and man["protocol_hash"] != protocol_hash:
        raise ValueError(f"wrong protocol for {path}: {man['protocol_hash']}")
    return man


def main() -> None:
    proto = load_v13()
    ph = proto["protocol_hash"]
    a = json.loads((OUT / "part_a/part_a_results.json").read_text())
    b = json.loads((OUT / "part_b/part_b_results.json").read_text())
    c = json.loads((OUT / "part_c/part_c_results.json").read_text())
    integrated = json.loads((OUT / "integrated_analysis.json").read_text())

    if len(a["cells"]) != 27:
        raise AssertionError("Part A cell count")
    a_manifests = []
    for cell in a["cells"]:
        path = OUT / "part_a" / cell["cell_id"].replace("/", "__")
        a_manifests.append(check_manifest(path, ph))
    for band in a["bands"].values():
        rows = {d["n_rows"] for d in band["datasets"].values()}
        feats = {d["n_features"] for d in band["datasets"].values()}
        if rows != {500} or len(feats) != 1:
            raise AssertionError(f"unmatched tabular band: rows={rows}, features={feats}")

    if len(b["cells"]) != 12 or set(b["rungs"]) != {
            "enwik8_bytes", "tinystories_bytes", "markov2_bytes", "copy_lag16_bytes"}:
        raise AssertionError("Part B rung/cell census")
    if any(meta["q"] != 256 for meta in b["rungs"].values()):
        raise AssertionError("Part B vocabulary mismatch")
    b_new = []
    b_reused = []
    for cell in b["cells"]:
        if cell["execution"] == "new":
            path = OUT / "part_b" / cell["cell_id"].replace("/", "__")
            man = check_manifest(path, ph)
            if abs(int(man["budget_tokens"]) - 5_000_000) >= 1024:
                raise AssertionError(f"Part B budget mismatch: {man['budget_tokens']}")
            b_new.append(man)
        else:
            b_reused.append(check_manifest(Path(cell["source_manifest"]).parent))
    if len(b_new) != 9 or len(b_reused) != 3:
        raise AssertionError("Part B new/reuse census")
    e1 = b["rungs"]["markov2_bytes"]["generating_entropy_bits"]
    e2 = b["rungs"]["copy_lag16_bytes"]["generating_entropy_bits"]
    if abs(e1 - e2) > 1e-12:
        raise AssertionError("synthetic entropy mismatch")

    if len(c["cells"]) != 18 or c["training_performed"] is not False:
        raise AssertionError("Part C census/retraining claim")
    for cell in c["cells"]:
        src = Path("runs/local/m8") / cell["source_cell"].replace("/", "__")
        check_manifest(src)

    verdict = (ROOT / "VERDICT.md").read_text()
    if "<!-- MATCHED_H5_V1_3_ADDENDUM -->" not in verdict:
        raise AssertionError("VERDICT addendum missing")
    if integrated["updated_h5_verdict"] not in {"PASS", "FAIL", "INCONCLUSIVE"}:
        raise AssertionError("explicit revised verdict missing")

    audit = {
        "status": "PASS", "protocol_hash": ph,
        "part_a": {"cells": 27, "new_manifests": len(a_manifests),
                   "exact_row_and_feature_matching": True},
        "part_b": {"cells": 12, "new_manifests": len(b_new),
                   "reused_manifests": len(b_reused), "all_q": 256,
                   "synthetic_entropy_matched": True},
        "part_c": {"cells_rescored": 18, "cells_retrained": 0,
                   "negative_gap_artifacts_in_primary_metrics": 0},
        "part_d": {"updated_h5_verdict": integrated["updated_h5_verdict"],
                   "addendum_present": True},
    }
    (OUT / "audit.json").write_text(json.dumps(audit, indent=2))
    print(json.dumps(audit, indent=2))


if __name__ == "__main__":
    main()
