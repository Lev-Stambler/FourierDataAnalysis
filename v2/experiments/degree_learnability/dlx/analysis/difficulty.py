"""Difficulty scalars from a run directory (manifest-validated).

A run without a complete manifest does not exist: load_run refuses it.
"""

from __future__ import annotations

import json
from pathlib import Path

from ..protocol import load_manifest
from ..training.metrics import tokens_to_threshold


def load_run(run_dir: Path) -> tuple[dict, dict]:
    """Returns (manifest, metrics); raises if the manifest is missing/incomplete."""
    run_dir = Path(run_dir)
    manifest = load_manifest(run_dir)
    metrics_path = Path(manifest["metrics_path"])
    if not metrics_path.exists():
        # manifest may store a path relative to another cwd; fall back to run_dir
        metrics_path = run_dir / Path(manifest["metrics_path"]).name
    metrics = json.loads(metrics_path.read_text())
    return manifest, metrics


def difficulty_from_run(run_dir: Path, budget_grid: list[int],
                        theta: float) -> dict:
    """T* at every budget in the grid (read off the checkpointed val curve),
    final gap, and the floor. Budgets above the run's budget_tokens are None.

    Secondary difficulty scalars (protocol v1.1 amendment; uniform across cells):
      final_gap_bits     — val CE minus floor at the max budget;
      norm_remaining     — (final CE - floor) / (init CE - floor), fraction of the
                           reducible gap left (init CE = first checkpoint, ~uniform);
      T_half             — first checkpoint where CE <= floor + (init CE - floor)/2.
    """
    manifest, metrics = load_run(run_dir)
    grid = metrics["token_grid"]
    curve = metrics["val_ce_bits"]
    floor = metrics["bayes_floor_bits"]
    run_budget = manifest["budget_tokens"]
    out = {"cell_id": manifest["cell_id"], "family_version": manifest["family_version"],
           "seed": manifest["seed"], "bayes_floor_bits": floor,
           "final_gap_bits": metrics["final_gap_bits"],
           "run_budget_tokens": run_budget, "T_star_by_budget": {}}
    for B in budget_grid:
        sub_grid = [t for t in grid if t <= B]
        sub_curve = curve[: len(sub_grid)]
        # tolerance: a run's actual token count is truncated to whole steps, so it
        # can land just under the nominal budget; treat <=0.1% shortfall as covered
        covered = B <= run_budget * 1.001
        out["T_star_by_budget"][str(B)] = (
            tokens_to_threshold(sub_grid, sub_curve, floor, theta) if covered else None
        )
    out["T_star"] = out["T_star_by_budget"][str(max(budget_grid))]
    init_ce = curve[0]
    reducible = max(init_ce - floor, 1e-9)
    out["init_ce_bits"] = init_ce
    out["norm_remaining"] = float((curve[-1] - floor) / reducible)
    half_level = floor + reducible / 2.0
    out["T_half"] = tokens_to_threshold(grid, curve, floor, half_level - floor)
    return out
