"""Pure configuration and selection helpers for the KISS LR sweep."""

from __future__ import annotations

import math


LR_SWEEP_GRID = tuple(
    (ste_lr, coefficient_lr)
    for ste_lr in (0.03, 0.1, 0.3)
    for coefficient_lr in (0.01, 0.03)
)


def lr_label(value: float) -> str:
    return f"{value:g}".replace(".", "p")


def select_best_trial(results):
    """Select the finite result with the lowest final fresh exact KL."""
    valid = [
        result for result in results
        if math.isfinite(float(result.get("final_kl", math.nan)))
    ]
    if not valid:
        raise RuntimeError("no LR sweep trial produced a finite final KL")
    return min(
        valid,
        key=lambda result: (
            float(result["final_kl"]),
            float(result["ste_lr"]),
            float(result["coefficient_lr"]),
        ),
    )
