from __future__ import annotations

import math
from collections.abc import Iterable

from .config import (
    EXTENSION_2X_EXAMPLES,
    EXTENSION_4X_EXAMPLES,
    FINAL_EXAMPLES,
    MID_EXAMPLES,
    MIN_CONTINUATION_IMPROVEMENT,
    MIN_EXTENSION_IMPROVEMENT,
    RESULT_SCHEMA,
    SCREEN_EXAMPLES,
    TARGET_VALIDATION_KL,
    Architecture,
    Cell,
)


def _validated_results(
    results: Iterable[dict],
    *,
    target_examples: int,
) -> list[dict]:
    values = list(results)
    for value in values:
        if (
            value.get("schema") != RESULT_SCHEMA
            or value.get("status") != "complete"
            or int(value.get("examples_seen", -1)) != target_examples
        ):
            raise RuntimeError("selection received an incomplete result")
        metric = float(value.get("validation", {}).get("kl", math.nan))
        if not math.isfinite(metric):
            raise RuntimeError("selection requires finite validation KL")
    return values


def _architecture(result: dict) -> Architecture:
    value = result["cell"]["architecture"]
    architecture = Architecture(**value)
    architecture.validate()
    return architecture


def select_depths(results: Iterable[dict]) -> dict[int, dict]:
    values = _validated_results(
        results,
        target_examples=SCREEN_EXAMPLES,
    )
    selected = {}
    for order in (2, 3):
        candidates = [
            value
            for value in values
            if (
                _architecture(value).factor_order == order
                and _architecture(value).rank == 1
            )
        ]
        if len(candidates) != 6:
            raise RuntimeError(
                f"depth selection needs six order-{order} rank-one cells"
            )
        selected[order] = min(
            candidates,
            key=lambda value: (
                float(value["validation"]["kl"]),
                _architecture(value).depth,
            ),
        )
    return selected


def select_ranks(
    depth_winners: dict[int, dict],
    rank_results: Iterable[dict],
) -> dict[int, dict]:
    rank_values = _validated_results(
        rank_results,
        target_examples=SCREEN_EXAMPLES,
    )
    if set(depth_winners) != {2, 3}:
        raise RuntimeError("rank selection needs both depth winners")
    selected = {}
    for order in (2, 3):
        depth_result = depth_winners[order]
        depth_architecture = _architecture(depth_result)
        candidates = [depth_result]
        candidates.extend(
            value for value in rank_values if _architecture(value).factor_order == order
        )
        architectures = [_architecture(value) for value in candidates]
        if (
            len(candidates) != 4
            or {value.rank for value in architectures} != {1, 2, 4, 8}
            or {value.depth for value in architectures} != {depth_architecture.depth}
        ):
            raise RuntimeError(f"rank selection needs ranks 1/2/4/8 at order {order}")
        selected[order] = min(
            candidates,
            key=lambda value: (
                float(value["validation"]["kl"]),
                _architecture(value).rank,
                int(value["inventory"]["trainable_parameters"]),
            ),
        )
    return selected


def continuation_cell(
    source: dict,
    *,
    stage: str,
    target_examples: int,
) -> Cell:
    stage_targets = {
        "mid": MID_EXAMPLES,
        "final": FINAL_EXAMPLES,
        "extend2x": EXTENSION_2X_EXAMPLES,
        "extend4x": EXTENSION_4X_EXAMPLES,
    }
    if stage not in stage_targets:
        raise ValueError("invalid continuation stage")
    if target_examples != stage_targets[stage]:
        raise ValueError("invalid continuation target")
    architecture = _architecture(source)
    cell = Cell(
        stage=stage,
        architecture=architecture,
        target_examples=target_examples,
        factor_lr=float(source["cell"]["factor_lr"]),
        auxiliary_lr=float(source["cell"]["auxiliary_lr"]),
    )
    cell.validate()
    return cell


def select_overall(results: Iterable[dict]) -> dict:
    values = _validated_results(
        results,
        target_examples=MID_EXAMPLES,
    )
    if len(values) != 2 or {_architecture(value).factor_order for value in values} != {
        2,
        3,
    }:
        raise RuntimeError("overall selection needs one result per factor order")
    return min(
        values,
        key=lambda value: (
            float(value["validation"]["kl"]),
            int(value["inventory"]["trainable_parameters"]),
            _architecture(value).rank,
            _architecture(value).depth,
        ),
    )


def continuation_improved(source: dict, continuation: dict) -> bool:
    before = float(source["validation"]["kl"])
    after = float(continuation["validation"]["kl"])
    return (
        math.isfinite(before)
        and math.isfinite(after)
        and before - after >= MIN_CONTINUATION_IMPROVEMENT
    )


def should_extend(source: dict, continuation: dict) -> bool:
    before = float(source["validation"]["kl"])
    after = float(continuation["validation"]["kl"])
    return (
        math.isfinite(before)
        and math.isfinite(after)
        and after > TARGET_VALIDATION_KL
        and before - after >= MIN_EXTENSION_IMPROVEMENT
    )
