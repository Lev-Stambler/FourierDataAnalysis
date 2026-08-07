"""Difficulty metrics (PLAN §6, all preregistered)."""

from __future__ import annotations

import numpy as np

__all__ = ["tokens_to_threshold", "final_gap", "grokking_jump"]


def tokens_to_threshold(token_grid: list[int], val_ce: list[float], floor: float,
                        theta: float) -> int | None:
    """First checkpoint token count where val CE <= floor + theta."""
    for t, ce in zip(token_grid, val_ce):
        if ce <= floor + theta:
            return int(t)
    return None


def final_gap(val_ce: list[float], floor: float) -> float:
    return float(val_ce[-1] - floor)


def grokking_jump(token_grid: list[int], val_ce: list[float], min_drop_bits: float = 0.5,
                  window: int = 3) -> int | None:
    """Token count of the largest val-CE drop >= min_drop_bits within `window`
    checkpoints (logged if present; secondary metric)."""
    best = None
    best_drop = min_drop_bits
    ce = np.asarray(val_ce, dtype=float)
    for i in range(len(ce) - window):
        drop = ce[i] - ce[i + window]
        if drop >= best_drop:
            best_drop = drop
            best = int(token_grid[i + window])
    return best
