from __future__ import annotations

import math

import pytest
import torch

from expv2.exp2.audit import _oracle_accuracy, local_audit
from expv2.exp2.baselines import diagnose_shortcuts, fixed_lag_accuracy
from expv2.exp2.budget import CalibrationBudget, reject_underupdated_budget
from expv2.exp2.config import (
    ASSOCIATIVE_HELDOUT_QUERY_SLOTS,
    ASSOCIATIVE_QUERY_SLOTS,
    BIT_ONE,
    BIT_ZERO,
    DELAY_HELDOUT_QUERY_SLOTS,
    DELAY_QUERY_SLOTS,
    HELDOUT_COMPOSITION_DELAYS,
    MINIMUM_CALIBRATION_UPDATES,
    TASKS,
    TRAIN_DELAYS,
    TWO_HOP_HELDOUT_QUERY_SLOTS,
    TWO_HOP_QUERY_SLOTS,
    VALID_SPLITS,
)
from expv2.exp2.gate import interpret_control, normalized_above_shortcut
from expv2.exp2.matched_comparison import (
    LEARNING_RATES,
    TARGET_CONTEXTS_PER_PHASE,
    _budget as matched_budget,
    select_recipe,
)
from expv2.exp2.synthetic import make_batch
from expv2.exp2.training import CalibrationRecipe, _optimizer, evaluate
from expv2.exp2.two_hop_debug import (
    debug_batch,
    local_debug_audit,
    mixed_debug_batch,
)
from expv2.exp1.model import build_model


@pytest.mark.parametrize(
    ("task", "split"),
    [
        (task, split)
        for task in TASKS
        for split in VALID_SPLITS[task]
    ],
)
def test_generators_are_deterministic_causal_and_oracle_correct(
    task: str, split: str
) -> None:
    first = make_batch(task, 64, seed=88, split=split)
    second = make_batch(task, 64, seed=88, split=split)
    assert torch.equal(first.inputs, second.inputs)
    assert torch.equal(first.targets, second.targets)
    assert torch.equal(first.mask, second.mask)
    assert _oracle_accuracy(task, first) == 1.0
    if task == "delay-copy":
        assert bool((first.support_positions < first.query_positions).all())
    else:
        assert int(first.support_positions.max()) < int(first.query_positions.min())


@pytest.mark.parametrize("task", ["associative-recall", "two-hop-recall"])
@pytest.mark.parametrize("split", ["sanity", "id", "ood-cardinality"])
def test_visible_value_shortcut_is_explicit(task: str, split: str) -> None:
    batch = make_batch(task, 16_384, seed=912, split=split)
    row = diagnose_shortcuts(batch)
    assert row["visible_value_accuracy"] == pytest.approx(
        row["visible_value_theoretical_accuracy"], abs=0.01
    )
    assert row["visible_value_theoretical_accuracy"] == pytest.approx(
        1.0 / int(batch.cardinality)
    )


def test_ood_cardinality_does_not_change_query_position_domain() -> None:
    for task, allowed in (
        ("associative-recall", set(ASSOCIATIVE_QUERY_SLOTS)),
        ("two-hop-recall", set(TWO_HOP_QUERY_SLOTS)),
    ):
        batch = make_batch(task, 1024, seed=42, split="ood-cardinality")
        assert set(batch.query_positions.flatten().tolist()) <= allowed
        assert batch.cardinality == 8


def test_position_ood_changes_only_query_position_domain() -> None:
    rows = (
        ("associative-recall", ASSOCIATIVE_QUERY_SLOTS, ASSOCIATIVE_HELDOUT_QUERY_SLOTS),
        ("two-hop-recall", TWO_HOP_QUERY_SLOTS, TWO_HOP_HELDOUT_QUERY_SLOTS),
        ("delay-copy", DELAY_QUERY_SLOTS, DELAY_HELDOUT_QUERY_SLOTS),
    )
    for task, trained, heldout in rows:
        batch = make_batch(task, 2048, seed=64, split="ood-position")
        positions = set(batch.query_positions.flatten().tolist())
        assert positions <= set(heldout)
        assert positions.isdisjoint(trained)
        if batch.cardinality is not None:
            assert batch.cardinality in (2, 3, 4)
        if batch.delays is not None:
            assert set(batch.delays.tolist()) <= set(TRAIN_DELAYS)


def test_delay_composition_uses_trained_symbols_not_unseen_embeddings() -> None:
    train = make_batch("delay-copy", 4096, seed=2, split="train")
    ood = make_batch("delay-copy", 4096, seed=3, split="ood-composition")
    assert set(train.delays.tolist()) <= set(TRAIN_DELAYS)
    assert set(ood.delays.tolist()) <= set(HELDOUT_COMPOSITION_DELAYS)
    assert set(train.delays.tolist()).isdisjoint(set(ood.delays.tolist()))
    assert set(train.inputs[:, :5].unique().tolist()) == {BIT_ZERO, BIT_ONE}
    assert set(ood.inputs[:, :5].unique().tolist()) == {BIT_ZERO, BIT_ONE}


def test_fixed_lag_shortcut_cannot_solve_composition_ood() -> None:
    batch = make_batch("delay-copy", 16_384, seed=99, split="ood-composition")
    best = max(fixed_lag_accuracy(batch, lag) for lag in TRAIN_DELAYS)
    assert best < 0.02


def test_budget_rejects_old_few_update_configuration() -> None:
    old_contexts = 20_000_000 // 128
    with pytest.raises(ValueError, match="optimizer updates"):
        reject_underupdated_budget(
            training_contexts=old_contexts, batch_contexts=4096
        )
    budget = CalibrationBudget(batch_contexts=4096)
    assert budget.actual_updates == MINIMUM_CALIBRATION_UPDATES
    assert budget.training_contexts == 4_096_000
    assert budget.training_tokens == 524_288_000


def test_gate_refuses_to_interpret_ood_until_control_learns_id() -> None:
    failed = interpret_control(
        id_accuracy=0.25,
        id_shortcut_accuracy=0.25,
        ood_accuracies={"cardinality": 0.125},
        ood_shortcuts={"cardinality": 0.125},
    )
    assert failed["status"] == "invalid_control"
    assert not failed["interpret_ood"]
    passed = interpret_control(
        id_accuracy=0.99,
        id_shortcut_accuracy=0.25,
        ood_accuracies={"cardinality": 0.94, "position": 0.91},
        ood_shortcuts={"cardinality": 0.125, "position": 0.25},
    )
    assert passed["status"] == "pass"
    assert passed["interpret_ood"]
    assert normalized_above_shortcut(0.25, 0.25) == 0


def test_full_local_task_audit_passes() -> None:
    result = local_audit(examples=2048)
    assert result["status"] == "pass", result["failures"]
    assert not result["delay_instruction"]["unseen_instruction_tokens"]


def test_adamw_positive_control_route_and_evaluation_are_finite() -> None:
    model = build_model("transformer", vocab_size=128)
    recipe = CalibrationRecipe("adamw", 3e-4)
    optimizer, routing = _optimizer(model, recipe)
    assert isinstance(optimizer, torch.optim.AdamW)
    assert routing["optimizer"] == "adamw-positive-control"
    metrics = evaluate(
        model,
        "associative-recall",
        "id",
        examples=8,
        batch_contexts=4,
        seed=21,
        device=torch.device("cpu"),
    )
    assert math.isfinite(metrics["loss"])
    assert 0 <= metrics["accuracy"] <= 1
    assert "visible_value_accuracy" in metrics["shortcut_baselines"]


def test_muon_calibration_route_is_available() -> None:
    model = build_model("transformer", vocab_size=128)
    optimizer, routing = _optimizer(model, CalibrationRecipe("muon", 0.03))
    assert routing["optimizer"] == "pure-batched-muon"
    assert routing["parameters"] == sum(p.numel() for p in model.parameters())
    assert optimizer.param_groups[0]["lr"] == pytest.approx(0.03)


@pytest.mark.parametrize(
    "kind", ["first-edge", "second-edge", "marked-two-hop", "two-hop"]
)
def test_two_hop_debug_generators_are_deterministic_and_candidate_correct(
    kind: str,
) -> None:
    first = debug_batch(kind, 512, seed=101)
    second = debug_batch(kind, 512, seed=101)
    assert torch.equal(first.inputs, second.inputs)
    assert torch.equal(first.targets, second.targets)
    rows = torch.arange(512)[:, None]
    target = first.targets[rows, first.query_positions]
    assert first.candidate_values is not None
    assert bool(
        (target[:, :, None] == first.candidate_values[:, None, :]).any(-1).all()
    )


def test_two_hop_debug_audit_passes() -> None:
    result = local_debug_audit()
    assert result["status"] == "pass", result["failures"]
    assert result["markers"]["all_observed"]


def test_mixed_debug_batch_packs_each_objective_without_changing_batch_size() -> None:
    batch = mixed_debug_batch(
        ("first-edge", "second-edge", "two-hop"), 101, seed=404
    )
    assert batch.inputs.shape == (101, 128)
    assert int(batch.mask.sum()) == 101
    rows = torch.arange(101)[:, None]
    target = batch.targets[rows, batch.query_positions]
    assert batch.candidate_values is not None
    assert bool(
        (target[:, :, None] == batch.candidate_values[:, None, :]).any(-1).all()
    )


@pytest.mark.parametrize("split", ["train", "id", "ood-cardinality", "ood-position"])
def test_mixed_debug_batch_supports_full_two_hop_splits(split: str) -> None:
    batch = mixed_debug_batch(
        ("first-edge", "second-edge", "two-hop"),
        99,
        seed=505,
        split=split,
    )
    assert batch.inputs.shape == (99, 128)
    expected = 8 if split == "ood-cardinality" else {2, 3, 4}
    if isinstance(expected, set):
        assert batch.cardinality in expected
    else:
        assert batch.cardinality == expected


def test_matched_budget_holds_examples_constant_across_physical_batches() -> None:
    large = matched_budget(10_240)
    small = matched_budget(2_048)
    assert large.training_contexts == TARGET_CONTEXTS_PER_PHASE
    assert small.training_contexts == TARGET_CONTEXTS_PER_PHASE
    assert large.actual_updates == 1_000
    assert small.actual_updates == 5_000
    assert large.training_tokens == small.training_tokens


def test_matched_lr_selection_never_uses_ood() -> None:
    rows = []
    for index, lr in enumerate(LEARNING_RATES):
        rows.append(
            {
                "variant": "transformer",
                "recipe": {"lr": lr},
                "id_pass": index == 1,
                "minimum_id_accuracy": 0.99 if index == 1 else 0.5,
                "mean_id_accuracy": 0.995 if index == 1 else 0.6,
                # The ID winner deliberately has the worst OOD value.
                "mean_ood_accuracy": 0.0 if index == 1 else 1.0,
            }
        )
    assert select_recipe(rows, "transformer")["recipe"]["lr"] == 0.03
