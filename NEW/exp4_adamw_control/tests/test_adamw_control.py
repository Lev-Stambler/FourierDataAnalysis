import json

import pytest
import torch

from qwen_adamw_control.control import (
    ADAMW_CONTROL_LR,
    CHECKPOINT_SCHEMA,
    COMPARISON_SCHEMA,
    build_control_plan,
    comparison_value,
    load_checkpoint,
    load_reference,
    make_adamw_optimizers,
    save_checkpoint,
    set_learning_rates,
)
from qwen_normuon_pretrain.config import Architecture, Trial
from qwen_normuon_pretrain.model import NextTokenStudent
from qwen_normuon_pretrain.pretrain import SCREEN_SUMMARY_SCHEMA


def tiny_trial() -> Trial:
    return Trial(
        stage="screen",
        normuon_lr=3e-3,
        effective_batch=2_048,
        examples=262_144,
        warmup_steps=8,
        stable_steps=108,
        cooldown_steps=12,
        architecture=Architecture(
            context_length=4,
            embedding_width=4,
            depth=1,
            expansion=4,
            repetitions=1,
            rank=2,
            rank_chunk=1,
        ),
    )


def reference_values(trial: Trial) -> tuple[dict, dict]:
    result = {
        "status": "complete",
        "label": "reference-normuon",
        "trial": trial.to_dict(),
        "initial_hashes": {
            "embedding": "a" * 64,
            "factors": "b" * 64,
        },
        "validation": {
            "cross_entropy": 2.0,
            "perplexity": 7.389,
        },
        "test": {},
        "dataset_manifest": {"schema": "tiny-data"},
        "wall_seconds": 10.0,
        "wandb_url": "https://wandb.invalid/reference",
    }
    summary = {
        "schema": SCREEN_SUMMARY_SCHEMA,
        "status": "complete",
        "summary_sha256": "c" * 64,
        "ranking": [{"label": result["label"]}],
    }
    return summary, result


def test_control_plan_matches_reference_except_factor_optimizer():
    trial = tiny_trial()
    summary, result = reference_values(trial)
    plan = build_control_plan(summary, result, trial)
    assert plan["optimizer"]["factor_optimizer"] == "fused_adamw"
    assert plan["optimizer"]["factor_lr"] == ADAMW_CONTROL_LR
    assert plan["optimizer"]["auxiliary_lr"] == trial.aux_adamw_lr
    assert plan["matched"]["architecture"] == trial.to_dict()[
        "architecture"
    ]
    assert plan["matched"]["seed"] == trial.seed
    assert plan["matched"]["effective_batch"] == trial.effective_batch
    assert plan["matched"]["examples"] == trial.examples
    assert plan["matched"]["initializer_hashes"] == (
        result["initial_hashes"]
    )
    assert len(plan["plan_sha256"]) == 64


def test_load_reference_requires_completed_winner_with_test(tmp_path):
    trial = tiny_trial()
    summary, result = reference_values(trial)
    (tmp_path / "screen-summary.json").write_text(json.dumps(summary))
    result_path = tmp_path / "screen" / result["label"] / "result.json"
    result_path.parent.mkdir(parents=True)
    result_path.write_text(json.dumps(result))
    loaded_summary, loaded_result, loaded_trial = load_reference(
        str(tmp_path)
    )
    assert loaded_summary == summary
    assert loaded_result == result
    assert loaded_trial == trial


def test_adamw_routes_all_factor_and_auxiliary_parameters():
    trial = tiny_trial()
    student = NextTokenStudent(
        trial.architecture,
        torch.randn(11, 4),
        vocab_size=11,
    )
    factor_optimizer, aux_optimizer, metadata = (
        make_adamw_optimizers(student, device="cpu")
    )
    factor_ids = {
        id(parameter)
        for group in factor_optimizer.param_groups
        for parameter in group["params"]
    }
    auxiliary_ids = {
        id(parameter)
        for group in aux_optimizer.param_groups
        for parameter in group["params"]
    }
    assert not (factor_ids & auxiliary_ids)
    assert factor_ids | auxiliary_ids == {
        id(parameter) for parameter in student.parameters()
    }
    assert metadata["factor_optimizer"] == "fused_adamw"
    assert metadata["factor_matrices"] == 12


def test_shared_wsd_sets_direct_adamw_lr():
    trial = tiny_trial()
    student = NextTokenStudent(
        trial.architecture,
        torch.randn(11, 4),
        vocab_size=11,
    )
    factor_optimizer, aux_optimizer, _ = make_adamw_optimizers(
        student,
        device="cpu",
    )
    multiplier, lr = set_learning_rates(
        factor_optimizer,
        aux_optimizer,
        trial,
        step=9,
    )
    assert multiplier == 1.0
    assert lr == ADAMW_CONTROL_LR
    assert {
        group["lr"]
        for optimizer in (factor_optimizer, aux_optimizer)
        for group in optimizer.param_groups
    } == {ADAMW_CONTROL_LR}


def test_checkpoint_restores_model_and_both_adamw_states(tmp_path):
    trial = tiny_trial()
    summary, result = reference_values(trial)
    plan = build_control_plan(summary, result, trial)
    student = NextTokenStudent(
        trial.architecture,
        torch.randn(11, 4),
        vocab_size=11,
    )
    factor_optimizer, aux_optimizer, _ = make_adamw_optimizers(
        student,
        device="cpu",
    )
    ids = torch.tensor([[0, 1, 2, 3], [3, 4, 5, 6]])
    student(ids).sum().backward()
    factor_optimizer.step()
    aux_optimizer.step()
    expected = {
        name: value.detach().clone()
        for name, value in student.state_dict().items()
    }
    initial_hashes = result["initial_hashes"]
    path = tmp_path / "progress.pt"
    save_checkpoint(
        path,
        student,
        factor_optimizer,
        aux_optimizer,
        trial,
        plan,
        step=1,
        examples_seen=2_048,
        initial_hashes=initial_hashes,
        initial_validation={"cross_entropy": 3.0},
        elapsed_wall_seconds=1.5,
    )
    checkpoint = torch.load(path, weights_only=True)
    assert checkpoint["schema"] == CHECKPOINT_SCHEMA
    for parameter in student.parameters():
        parameter.data.zero_()
    factor_optimizer.state.clear()
    aux_optimizer.state.clear()
    resumed = load_checkpoint(
        path,
        student,
        factor_optimizer,
        aux_optimizer,
        trial,
        plan,
        initial_hashes,
    )
    assert resumed["step"] == 1
    assert resumed["examples_seen"] == 2_048
    for name, value in student.state_dict().items():
        torch.testing.assert_close(value, expected[name])
    assert factor_optimizer.state
    assert aux_optimizer.state


def test_comparison_reports_signed_adamw_minus_normuon_deltas():
    trial = tiny_trial()
    summary, reference = reference_values(trial)
    plan = build_control_plan(summary, reference, trial)
    adamw = {
        "status": "complete",
        "trial": trial.to_dict(),
        "validation": {
            "cross_entropy": 1.8,
            "perplexity": 6.05,
        },
        "test": {},
        "wall_seconds": 12.0,
        "wandb_url": "https://wandb.invalid/adamw",
    }
    comparison = comparison_value(plan, reference, adamw)
    assert comparison["schema"] == COMPARISON_SCHEMA
    assert comparison["adamw_minus_normuon"][
        "validation_cross_entropy"
    ] == pytest.approx(-0.2)
    assert comparison["adamw_minus_normuon"][
        "test_cross_entropy"
    ] is None
    assert len(comparison["comparison_sha256"]) == 64
