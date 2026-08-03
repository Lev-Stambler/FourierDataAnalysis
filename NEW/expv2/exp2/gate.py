"""Positive-control-first interpretation for capability and OOD metrics."""

from __future__ import annotations

from typing import Any

from .config import ID_ACCURACY_THRESHOLD, OOD_ACCURACY_THRESHOLD


def normalized_above_shortcut(accuracy: float, shortcut: float) -> float:
    return (accuracy - shortcut) / max(1e-12, 1.0 - shortcut)


def interpret_control(
    *,
    id_accuracy: float,
    id_shortcut_accuracy: float,
    ood_accuracies: dict[str, float],
    ood_shortcuts: dict[str, float],
) -> dict[str, Any]:
    if set(ood_accuracies) != set(ood_shortcuts):
        raise ValueError("OOD metrics and shortcut baselines must have identical axes")
    id_normalized = normalized_above_shortcut(
        id_accuracy, id_shortcut_accuracy
    )
    if id_accuracy < ID_ACCURACY_THRESHOLD:
        return {
            "status": "invalid_control",
            "interpret_ood": False,
            "id_accuracy": id_accuracy,
            "id_shortcut_accuracy": id_shortcut_accuracy,
            "id_normalized_above_shortcut": id_normalized,
            "reason": "positive control did not learn the ID algorithm",
        }
    axes = {
        name: {
            "accuracy": accuracy,
            "shortcut_accuracy": ood_shortcuts[name],
            "normalized_above_shortcut": normalized_above_shortcut(
                accuracy, ood_shortcuts[name]
            ),
            "pass": accuracy >= OOD_ACCURACY_THRESHOLD,
        }
        for name, accuracy in ood_accuracies.items()
    }
    return {
        "status": "pass" if all(row["pass"] for row in axes.values()) else "ood_failure",
        "interpret_ood": True,
        "id_accuracy": id_accuracy,
        "id_shortcut_accuracy": id_shortcut_accuracy,
        "id_normalized_above_shortcut": id_normalized,
        "ood_axes": axes,
    }
