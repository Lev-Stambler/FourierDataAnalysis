"""Local proofs that the corrected tasks isolate their advertised shift."""

from __future__ import annotations

from pathlib import Path
from typing import Any

import torch

from expv2.exp1.utils import atomic_json

from .baselines import diagnose_shortcuts
from .config import (
    ASSOCIATIVE_HELDOUT_QUERY_SLOTS,
    ASSOCIATIVE_QUERY_SLOTS,
    DELAY_HELDOUT_QUERY_SLOTS,
    DELAY_QUERY_SLOTS,
    HELDOUT_COMPOSITION_DELAYS,
    TASKS,
    TRAIN_DELAYS,
    TWO_HOP_HELDOUT_QUERY_SLOTS,
    TWO_HOP_QUERY_SLOTS,
    VALID_SPLITS,
)
from .synthetic import SyntheticBatch, make_batch


def _oracle_accuracy(task: str, batch: SyntheticBatch) -> float:
    rows = torch.arange(batch.inputs.shape[0])[:, None]
    query_tokens = batch.inputs[rows, batch.query_positions]
    target_tokens = batch.targets[rows, batch.query_positions]
    if task == "delay-copy":
        oracle = batch.inputs[rows, batch.support_positions]
    elif task == "associative-recall":
        keys = batch.inputs[rows, batch.support_positions]
        values = batch.inputs[rows, batch.support_positions + 1]
        matches = query_tokens[:, :, None] == keys[:, None, :]
        oracle = (matches.long() * values[:, None, :]).sum(-1)
    else:
        first_slots = batch.support_positions[:, 0]
        second_slots = batch.support_positions[:, 1]
        first_keys = batch.inputs[rows, first_slots]
        middles = batch.inputs[rows, first_slots + 1]
        second_keys = batch.inputs[rows, second_slots]
        finals = batch.inputs[rows, second_slots + 1]
        first_match = query_tokens[:, :, None] == first_keys[:, None, :]
        selected_middle = (first_match.long() * middles[:, None, :]).sum(-1)
        second_match = selected_middle[:, :, None] == second_keys[:, None, :]
        oracle = (second_match.long() * finals[:, None, :]).sum(-1)
    return float((oracle == target_tokens).float().mean().item())


def local_audit(
    output: str | Path | None = None,
    *,
    examples: int = 4_096,
) -> dict[str, Any]:
    failures: list[str] = []
    rows: dict[str, Any] = {}
    for task in TASKS:
        rows[task] = {}
        for index, split in enumerate(VALID_SPLITS[task]):
            left = make_batch(task, examples, seed=1700 + index, split=split)
            right = make_batch(task, examples, seed=1700 + index, split=split)
            oracle = _oracle_accuracy(task, left)
            shortcuts = diagnose_shortcuts(left)
            deterministic = (
                torch.equal(left.inputs, right.inputs)
                and torch.equal(left.targets, right.targets)
                and torch.equal(left.mask, right.mask)
            )
            if not deterministic:
                failures.append(f"{task}/{split} is nondeterministic")
            if oracle != 1.0:
                failures.append(f"{task}/{split} oracle accuracy is {oracle}")
            if left.candidate_values is not None:
                empirical = float(shortcuts["visible_value_accuracy"])
                theoretical = float(shortcuts["visible_value_theoretical_accuracy"])
                if abs(empirical - theoretical) > 0.02:
                    failures.append(
                        f"{task}/{split} visible-value baseline mismatch: "
                        f"{empirical} versus {theoretical}"
                    )
            rows[task][split] = {
                "oracle_accuracy": oracle,
                "deterministic": deterministic,
                "targets_per_context": int(left.mask.sum(1).unique().item()),
                "minimum_query_position": int(left.query_positions.min()),
                "maximum_support_position": int(left.support_positions.max()),
                "all_support_before_own_query": bool(
                    (left.support_positions < left.query_positions).all()
                    if task == "delay-copy"
                    else left.support_positions.max() < left.query_positions.min()
                ),
                "shortcut_baselines": shortcuts,
            }

    train_bits = {
        bit: {(delay >> bit) & 1 for delay in TRAIN_DELAYS} for bit in range(5)
    }
    if any(values != {0, 1} for values in train_bits.values()):
        failures.append("not every delay bit sees both symbols during training")
    if set(TRAIN_DELAYS) & set(HELDOUT_COMPOSITION_DELAYS):
        failures.append("train and compositional-OOD delays overlap")
    if not set(ASSOCIATIVE_QUERY_SLOTS).isdisjoint(
        ASSOCIATIVE_HELDOUT_QUERY_SLOTS
    ):
        failures.append("associative position splits overlap")
    if not set(TWO_HOP_QUERY_SLOTS).isdisjoint(TWO_HOP_HELDOUT_QUERY_SLOTS):
        failures.append("two-hop position splits overlap")
    if not set(DELAY_QUERY_SLOTS).isdisjoint(DELAY_HELDOUT_QUERY_SLOTS):
        failures.append("delay position splits overlap")

    result = {
        "schema": "expv2-2-task-audit-v1",
        "status": "pass" if not failures else "fail",
        "rows": rows,
        "delay_instruction": {
            "train_delays": TRAIN_DELAYS,
            "heldout_composition_delays": HELDOUT_COMPOSITION_DELAYS,
            "training_bit_coverage": {
                str(bit): sorted(values) for bit, values in train_bits.items()
            },
            "unseen_instruction_tokens": False,
        },
        "failures": failures,
    }
    if output is not None:
        atomic_json(output, result)
    return result
