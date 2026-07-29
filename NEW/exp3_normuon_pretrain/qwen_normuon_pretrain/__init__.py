"""Self-contained next-token pretraining with batched NorMuon factors."""

from .config import (
    AUX_ADAMW_LR,
    DEFAULT_NORMUON_LR,
    FINAL_EXAMPLES,
    NORMUON_LR_GRID,
    SCREEN_EXAMPLES,
    Architecture,
    Trial,
    default_optimizer_policy,
    final_trials,
    screen_trials,
)

__all__ = [
    "AUX_ADAMW_LR",
    "DEFAULT_NORMUON_LR",
    "FINAL_EXAMPLES",
    "NORMUON_LR_GRID",
    "SCREEN_EXAMPLES",
    "Architecture",
    "Trial",
    "default_optimizer_policy",
    "final_trials",
    "screen_trials",
]
