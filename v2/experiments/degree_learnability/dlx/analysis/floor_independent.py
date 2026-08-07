"""Floor-independent learning-curve metrics used by matched H5 protocol v1.3."""

from __future__ import annotations

import numpy as np


def curve_metrics(token_grid: list[int], val_ce_bits: list[float],
                  initial_ce_bits: float | None = None) -> dict:
    """Summarize a CE curve without a Bayes-floor estimate.

    ``normalized_curve_area`` is CE/init CE integrated over normalized log-token
    time. Higher values mean the model stayed near its initial loss for longer and
    are therefore interpreted as harder. Learning amounts have the opposite sign:
    higher means easier/more was learned.
    """
    t = np.asarray(token_grid, dtype=np.float64)
    ce = np.asarray(val_ce_bits, dtype=np.float64)
    if t.ndim != 1 or ce.ndim != 1 or len(t) != len(ce) or len(t) < 2:
        raise ValueError("token_grid and val_ce_bits must be aligned curves of length >= 2")
    if np.any(~np.isfinite(ce)) or np.any(np.diff(t) < 0):
        raise ValueError("curve must be finite and token_grid non-decreasing")

    init = float(ce[0] if initial_ce_bits is None else initial_ce_bits)
    final = float(ce[-1])
    best = float(ce.min())
    learning = init - final
    best_learning = init - best

    max_t = max(float(t[-1]), 1.0)
    x = np.log1p(t) / np.log1p(max_t)
    if x[0] > 0.0:
        x = np.concatenate(([0.0], x))
        ce_for_auc = np.concatenate(([init], ce))
    else:
        ce_for_auc = ce
    area = float(np.trapezoid(ce_for_auc / max(abs(init), 1e-12), x))

    half_at = None
    if best_learning > 0.0:
        threshold = init - 0.5 * best_learning
        hit = np.flatnonzero(ce <= threshold)
        if len(hit):
            half_at = int(t[int(hit[0])])

    return {
        "initial_ce_bits": init,
        "final_ce_bits": final,
        "best_ce_bits": best,
        "learning_amount_bits": learning,
        "best_learning_amount_bits": best_learning,
        "fractional_learning": learning / max(abs(init), 1e-12),
        "normalized_curve_area": area,
        "half_best_learning_at": half_at,
    }
