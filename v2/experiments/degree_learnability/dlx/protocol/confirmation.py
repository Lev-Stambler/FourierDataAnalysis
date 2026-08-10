"""Pure validation and cell enumeration for frozen corpus confirmations."""

from __future__ import annotations


def confirmation_cells(protocol: dict) -> list[dict]:
    cells = [
        {"dataset": source["id"], "seed": int(seed)}
        for source in protocol["corpora"]["sources"]
        for seed in protocol["training"]["seeds"]
    ]
    ids = [f"V26/{row['dataset']}/s{row['seed']}" for row in cells]
    if len(ids) != len(set(ids)):
        raise ValueError("confirmation grid contains duplicate cell IDs")
    expected = int(protocol["compute"]["expected_training_cells"])
    if len(cells) != expected:
        raise ValueError(
            f"confirmation grid has {len(cells)} cells, expected {expected}"
        )
    if int(protocol["compute"]["maximum_concurrent_h100s"]) != 1:
        raise ValueError("confirmation protocol must cap H100 concurrency at one")
    return cells
