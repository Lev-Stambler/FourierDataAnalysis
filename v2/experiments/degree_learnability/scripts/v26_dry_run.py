"""Validate and print the frozen v2.6 execution grid without launching work."""

from __future__ import annotations

import json
import sys
from pathlib import Path

ROOT = Path(__file__).parent.parent
sys.path.insert(0, str(ROOT))

from dlx.protocol.confirmation import confirmation_cells
from dlx.protocol.frozen import load_frozen_protocol


def main() -> None:
    protocol = load_frozen_protocol(ROOT / "configs/protocol_v2.6.json")
    cells = confirmation_cells(protocol)
    print(
        json.dumps(
            {
                "protocol_hash": protocol["protocol_hash"],
                "corpora": len(protocol["corpora"]["sources"]),
                "training_cells": len(cells),
                "seeds": protocol["training"]["seeds"],
                "maximum_concurrent_h100s": protocol["compute"][
                    "maximum_concurrent_h100s"
                ],
                "first_cell": cells[0],
                "last_cell": cells[-1],
                "launches_work": False,
            },
            indent=2,
        )
    )


if __name__ == "__main__":
    main()
