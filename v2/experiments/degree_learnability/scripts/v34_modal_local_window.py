"""Single-H100 runner for the frozen v3.4 causal attention-window test."""

from __future__ import annotations

import json
import os
from pathlib import Path

import modal

from dlx.analysis.character_response import enumerate_supports
from dlx.protocol.frozen import load_frozen_protocol, verify_hash_lock
from scripts import v30_modal as base

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs/local/v34_local_window"
SOURCE_OUT = ROOT / "runs/local/v30_architecture_spectrum"
MEASUREMENT = load_frozen_protocol(ROOT / "configs/protocol_v3.1.json")
PROTOCOL = load_frozen_protocol(ROOT / "configs/protocol_v3.4.json")
PANEL = json.loads((ROOT / PROTOCOL["panel"]["path"]).read_text())

app = modal.App("dlx-v34-local-window")
app.include(base.app)


def _architecture(window: int) -> dict:
    return {
        "id": f"rope_w{window}",
        "position_encoding": "rope",
        "rope_base": 10_000.0,
        "attention_window": int(window),
    }


def _panel_lookup() -> dict[str, str]:
    return {
        dataset: panel
        for value in PANEL["strata"].values()
        for panel in ("development", "confirmation")
        for dataset in value[panel]
    }


def _expansion_corpora() -> list[dict]:
    lookup = _panel_lookup()
    rows = [
        row
        for row in base.CORPORA
        if row["panel"] == "confirmation" and row["dataset"] in lookup
    ]
    if len(rows) != 48 or set(lookup) != {row["dataset"] for row in rows}:
        raise ValueError("v3.4 panel does not exactly match the 48 expansion corpora")
    return [{**row, "v34_panel": lookup[row["dataset"]]} for row in rows]


def _run_uniform(limit: int) -> None:
    spec = MEASUREMENT["fourier_character_training"]
    supports = [
        support
        for support in enumerate_supports(spec["lags"], max_degree=2)
        if len(support) <= 2
    ]
    windows = PROTOCOL["intervention"]["new_attention_windows"]
    seeds = PROTOCOL["uniform_character_probe"]["seeds"]
    cells = [
        (window, support, seed)
        for window in windows
        for support in supports
        for seed in seeds
    ]
    if len(cells) != PROTOCOL["uniform_character_probe"]["new_training_cells"]:
        raise ValueError("unexpected v3.4 uniform-probe grid")
    if limit:
        cells = cells[:limit]
    result_path = OUT / "uniform_results.json"
    existing = json.loads(result_path.read_text()) if result_path.exists() else []
    completed = {row["cell_id"] for row in existing}
    for window, support, seed in cells:
        support_id = "-".join(str(value) for value in support)
        cell_id = f"V34U/w{window}/A{support_id}/s{seed}"
        if cell_id in completed:
            continue
        result = base.character_train_cell.remote(
            {
                "protocol": MEASUREMENT,
                "analysis_protocol_hash": PROTOCOL["protocol_hash"],
                "architecture": _architecture(window),
                "data_architecture_id": "rope",
                "support": support,
                "seed": seed,
                "cell_id": cell_id,
            }
        )
        base._save_cells(result_path, [result])
        print(cell_id, result["character_hardness"], flush=True)


def _run_natural(limit: int) -> None:
    # This lock is deliberately unavailable until all uniform-probe-derived
    # predictors have been frozen without access to limited-window outcomes.
    feature_hash = verify_hash_lock(
        OUT / "frozen_features.json", OUT / "frozen_features.sha256"
    )
    data_hash = verify_hash_lock(
        SOURCE_OUT / "data_manifest.json", SOURCE_OUT / "data_manifest.sha256"
    )
    profile_hash = verify_hash_lock(
        SOURCE_OUT / "profile_manifest.json", SOURCE_OUT / "profile_manifest.sha256"
    )
    kernel_hash = verify_hash_lock(
        SOURCE_OUT / "fourier_ce_kernel.json", SOURCE_OUT / "fourier_ce_kernel.sha256"
    )
    cells = [
        (row, window, seed)
        for row in _expansion_corpora()
        for window in PROTOCOL["intervention"]["new_attention_windows"]
        for seed in PROTOCOL["natural_training"]["seeds"]
    ]
    if len(cells) != PROTOCOL["natural_training"]["new_cells"]:
        raise ValueError("unexpected v3.4 natural-training grid")
    if limit:
        cells = cells[:limit]
    result_path = Path(
        os.environ.get("DLX_V34_CHECKPOINT", OUT / "natural_results.json")
    )
    existing = json.loads(result_path.read_text()) if result_path.exists() else []
    completed = {row["cell_id"] for row in existing}
    for row, window, seed in cells:
        cell_id = f"V34N/{row['dataset']}/w{window}/s{seed}"
        if cell_id in completed:
            continue
        result = base.natural_train_cell.remote(
            {
                "protocol": MEASUREMENT,
                "analysis_protocol_hash": PROTOCOL["protocol_hash"],
                "corpus": row,
                "architecture": _architecture(window),
                "seed": seed,
                "data_manifest_hash": data_hash,
                "profile_manifest_hash": profile_hash,
                "fourier_ce_kernel_hash": kernel_hash,
                "prediction_lock_hash": feature_hash,
                "cell_id": cell_id,
            }
        )
        result["v34_panel"] = row["v34_panel"]
        base._save_cells(result_path, [result])
        print(
            cell_id,
            result["floor_independent"]["normalized_curve_area"],
            flush=True,
        )


@app.local_entrypoint()
def main(stage: str = "uniform", limit: int = 0) -> None:
    verify_hash_lock(
        ROOT / PROTOCOL["panel"]["path"],
        ROOT / "configs/local_window_panel_v3.4.sha256",
    )
    if stage == "uniform":
        _run_uniform(limit)
    elif stage == "natural":
        _run_natural(limit)
    else:
        raise ValueError("stage must be uniform or natural")
