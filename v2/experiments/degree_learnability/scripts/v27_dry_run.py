"""Read-only validation of the frozen v2.7 protocol and pretraining locks."""

from __future__ import annotations

import json
import sys
from pathlib import Path

ROOT = Path(__file__).parent.parent
sys.path.insert(0, str(ROOT))

from dlx.protocol.confirmation import confirmation_cells
from dlx.protocol.frozen import file_sha256, load_frozen_protocol, verify_hash_lock

OUT = ROOT / "runs/local/v27_marginal_locality"


def dry_run() -> dict:
    protocol = load_frozen_protocol(ROOT / "configs/protocol_v2.7.json")
    cells = confirmation_cells(protocol)
    checks = {
        "development_manifest": file_sha256(ROOT / protocol["development"]["manifest"])
        == protocol["development"]["manifest_sha256"],
        "data_manifest": bool(
            verify_hash_lock(OUT / "data_manifest.json", OUT / "data_manifest.sha256")
        ),
        "profile_manifest": bool(
            verify_hash_lock(
                OUT / "profile_manifest.json", OUT / "profile_manifest.sha256"
            )
        ),
        "prediction_lock": bool(
            verify_hash_lock(OUT / "predictions.json", OUT / "predictions.sha256")
        ),
        "48_sequential_cells": len(cells) == 48
        and protocol["compute"]["maximum_concurrent_h100s"] == 1,
    }
    return {
        "status": "PASS" if all(checks.values()) else "FAIL",
        "protocol_hash": protocol["protocol_hash"],
        "checks": checks,
        "next_cells": cells,
    }


if __name__ == "__main__":
    print(json.dumps(dry_run(), indent=2))
