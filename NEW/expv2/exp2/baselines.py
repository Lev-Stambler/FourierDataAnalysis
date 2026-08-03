"""Analytical shortcut baselines that every learned result must report."""

from __future__ import annotations

from typing import Any

import torch

from .config import TRAIN_DELAYS, VOCAB_SIZE
from .synthetic import SyntheticBatch


def _selected_accuracy(batch: SyntheticBatch, predictions: torch.Tensor) -> float:
    rows = torch.arange(batch.inputs.shape[0], device=batch.inputs.device)[:, None]
    targets = batch.targets[rows, batch.query_positions]
    return float((predictions == targets).float().mean().item())


def visible_value_accuracy(batch: SyntheticBatch) -> float:
    if batch.candidate_values is None:
        raise ValueError("visible-value baseline requires a recall batch")
    prediction = batch.candidate_values[:, :1].expand_as(batch.query_positions)
    return _selected_accuracy(batch, prediction)


def fixed_lag_accuracy(batch: SyntheticBatch, lag: int) -> float:
    if batch.delays is None:
        raise ValueError("fixed-lag baseline requires a delay batch")
    positions = batch.query_positions - int(lag)
    if int(positions.min()) < 0:
        raise ValueError("fixed lag reaches before the context")
    rows = torch.arange(batch.inputs.shape[0], device=batch.inputs.device)[:, None]
    return _selected_accuracy(batch, batch.inputs[rows, positions])


def diagnose_shortcuts(batch: SyntheticBatch) -> dict[str, Any]:
    result: dict[str, Any] = {"uniform_vocabulary_accuracy": 1.0 / VOCAB_SIZE}
    if batch.candidate_values is not None:
        result.update(
            {
                "cardinality": batch.cardinality,
                "visible_value_accuracy": visible_value_accuracy(batch),
                "visible_value_theoretical_accuracy": 1.0 / int(batch.cardinality),
            }
        )
    if batch.delays is not None:
        rows = {str(lag): fixed_lag_accuracy(batch, lag) for lag in TRAIN_DELAYS}
        best_lag, best = max(rows.items(), key=lambda item: item[1])
        result.update(
            {
                "training_fixed_lag_accuracies": rows,
                "best_training_fixed_lag": int(best_lag),
                "best_training_fixed_lag_accuracy": best,
            }
        )
    return result
