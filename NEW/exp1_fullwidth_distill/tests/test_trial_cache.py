import json
from dataclasses import replace

import pytest
import torch

from qwen_fullwidth_distill.config import ArchitectureConfig, TrialConfig
from qwen_fullwidth_distill.model import FullWidthStudent
from qwen_fullwidth_distill.train import (
    _final_checkpoint_optimizer,
    _load_completed_result,
    _load_progress_checkpoint,
    _load_warm_start_checkpoint,
    _save_checkpoint,
    _save_progress_checkpoint,
)


def _result(trial: TrialConfig) -> dict:
    return {
        "label": trial.label,
        "architecture": trial.architecture.to_dict(),
        "lr": trial.lr,
        "seed": trial.seed,
        "steps": trial.steps,
        "validation": {"kl": 1.25},
        "test": {},
        "embedding_sha256": "a" * 64,
    }


def test_completed_screen_result_is_reused(tmp_path):
    trial = TrialConfig(
        ArchitectureConfig("monarch", "sequential", 1),
        stage="screen",
    )
    expected = _result(trial)
    (tmp_path / "result.json").write_text(json.dumps(expected))
    assert _load_completed_result(tmp_path, trial) == expected


def test_mismatched_or_malformed_result_is_not_reused(tmp_path):
    trial = TrialConfig(ArchitectureConfig("dense", "sequential", 1))
    value = _result(trial)
    value["steps"] += 1
    (tmp_path / "result.json").write_text(json.dumps(value))
    assert _load_completed_result(tmp_path, trial) is None
    (tmp_path / "result.json").write_text("{")
    assert _load_completed_result(tmp_path, trial) is None


def test_final_requires_test_metrics_and_atomic_checkpoint(tmp_path):
    trial = TrialConfig(
        ArchitectureConfig("monarch", "residual_ffn", 1, 4),
        stage="final",
        steps=4_000,
    )
    value = _result(trial)
    (tmp_path / "result.json").write_text(json.dumps(value))
    assert _load_completed_result(tmp_path, trial) is None

    value["test"] = {"kl": 1.5}
    (tmp_path / "result.json").write_text(json.dumps(value))
    assert _load_completed_result(tmp_path, trial) is None

    (tmp_path / "student.pt").write_bytes(b"complete")
    assert _load_completed_result(tmp_path, trial) == value


def test_progress_checkpoint_restores_exact_student_state(tmp_path):
    config = ArchitectureConfig(
        "monarch",
        "sequential",
        1,
        context_length=4,
        embedding_width=4,
        monarch_blocks=4,
    )
    trial = TrialConfig(config, stage="final", steps=4_000)
    device = "cuda" if torch.cuda.is_available() else "cpu"
    student = FullWidthStudent(
        config, torch.randn(7, 4), vocab_size=7
    ).to(device)
    optimizer = torch.optim.AdamW(
        student.parameters(),
        lr=1e-3,
        fused=torch.cuda.is_available(),
    )
    optimizer.zero_grad(set_to_none=True)
    sum(parameter.square().sum() for parameter in student.parameters()).backward()
    optimizer.step()
    expected = {
        key: value.detach().clone()
        for key, value in student.state_dict().items()
    }
    expected_optimizer = optimizer.state_dict()
    path = tmp_path / "progress.pt"
    _save_progress_checkpoint(
        path,
        student,
        optimizer,
        trial,
        "c" * 64,
        1_000,
        {"kl": 3.0},
        12.5,
    )
    with torch.no_grad():
        for parameter in student.parameters():
            parameter.zero_()
    optimizer.state.clear()
    resume = _load_progress_checkpoint(
        path, student, optimizer, trial, "c" * 64
    )
    assert resume == {
        "step": 1_000,
        "examples_seen": 1_024_000,
        "input_tokens_seen": 4_096_000,
        "optimizer_steps": 1_000,
        "initial_validation": {"kl": 3.0},
        "elapsed_wall_seconds": 12.5,
        "stable_validation": None,
        "optimizer_state_resumed": True,
    }
    for key, value in student.state_dict().items():
        torch.testing.assert_close(value, expected[key])
    restored_optimizer = optimizer.state_dict()
    assert restored_optimizer["param_groups"] == expected_optimizer["param_groups"]
    for key, state in expected_optimizer["state"].items():
        for name, value in state.items():
            if isinstance(value, torch.Tensor):
                torch.testing.assert_close(
                    restored_optimizer["state"][key][name], value
                )
            else:
                assert restored_optimizer["state"][key][name] == value

    with pytest.raises(RuntimeError, match="embedding_sha256"):
        _load_progress_checkpoint(
            path, student, optimizer, trial, "d" * 64
        )

    safer_cadence = replace(trial, checkpoint_every_examples=64)
    assert _load_progress_checkpoint(
        path, student, optimizer, safer_cadence, "c" * 64
    )["step"] == 1_000


def test_progress_v3_counters_and_backward_v2_loading(tmp_path):
    config = ArchitectureConfig(
        "monarch",
        "sequential",
        1,
        context_length=4,
        embedding_width=4,
        monarch_blocks=4,
    )
    trial = TrialConfig(config, steps=100, effective_batch=32)
    student = FullWidthStudent(config, torch.randn(7, 4), vocab_size=7)
    optimizer = torch.optim.AdamW(student.parameters())
    sum(parameter.square().sum() for parameter in student.parameters()).backward()
    optimizer.step()
    path = tmp_path / "progress.pt"
    _save_progress_checkpoint(
        path,
        student,
        optimizer,
        trial,
        "a" * 64,
        25,
        {"kl": 2.0},
        1.0,
    )
    value = torch.load(path, weights_only=True)
    assert value["schema"] == "qwen-fullwidth-checkpoint-v3"
    assert value["examples_seen"] == 800
    assert value["input_tokens_seen"] == 3_200
    assert value["optimizer_steps"] == 25

    value["schema"] = "qwen-fullwidth-progress-v2"
    for key in ("examples_seen", "input_tokens_seen", "optimizer_steps"):
        value.pop(key)
    torch.save(value, path)
    resume = _load_progress_checkpoint(
        path, student, optimizer, trial, "a" * 64
    )
    assert resume["examples_seen"] == 800
    assert resume["input_tokens_seen"] == 3_200
    assert resume["optimizer_steps"] == 25


def test_weights_only_warm_start_keeps_cursor_and_resets_adam(tmp_path):
    config = ArchitectureConfig(
        "monarch",
        "sequential",
        1,
        context_length=4,
        embedding_width=4,
        monarch_blocks=4,
    )
    source = TrialConfig(
        config,
        lr=5e-4,
        seed=2,
        steps=96,
        stage="source",
        effective_batch=32,
    )
    source_student = FullWidthStudent(
        config, torch.randn(7, 4), vocab_size=7
    )
    source_state = {
        key: value.detach().clone()
        for key, value in source_student.state_dict().items()
    }
    path = tmp_path / source.label / "student.pt"
    _save_checkpoint(
        path,
        source_student,
        source,
        "b" * 64,
        {
            "steps_completed": 96,
            "examples_seen": 3_072,
            "input_tokens_seen": 12_288,
            "optimizer_steps": 96,
            "validation": {"kl": 1.5},
        },
        optimizer=None,
    )
    target = TrialConfig(
        config,
        lr=1e-3,
        seed=2,
        steps=8,
        stage="fresh",
        effective_batch=32,
        warm_start_stage=source.stage,
        warm_start_label=source.label,
        warm_start_step=96,
        warm_start_lr_override=True,
        warm_start_weights_only=True,
    )
    target_student = FullWidthStudent(
        config, torch.randn(7, 4), vocab_size=7
    )
    target_optimizer = torch.optim.AdamW(
        target_student.parameters(), lr=target.lr
    )
    sum(
        parameter.square().sum()
        for parameter in target_student.parameters()
    ).backward()
    target_optimizer.step()
    assert target_optimizer.state

    resume = _load_warm_start_checkpoint(
        path,
        target_student,
        target_optimizer,
        target,
        "b" * 64,
    )

    assert resume["step"] == 0
    assert resume["examples_seen"] == 3_072
    assert resume["input_tokens_seen"] == 12_288
    assert resume["optimizer_steps"] == 0
    assert resume["optimizer_state_resumed"] is False
    assert not target_optimizer.state
    for key, value in target_student.state_dict().items():
        torch.testing.assert_close(value, source_state[key])

    legacy = torch.load(path, weights_only=True)
    legacy["schema"] = "qwen-fullwidth-student-v1"
    legacy["metrics"].update({
        "label": source.label,
        "architecture": source.architecture.to_dict(),
        "lr": source.lr,
        "seed": source.seed,
        "steps_completed": 96,
    })
    for key in (
        "step",
        "examples_seen",
        "input_tokens_seen",
        "optimizer_steps",
    ):
        legacy.pop(key)
    torch.save(legacy, path)
    legacy_resume = _load_warm_start_checkpoint(
        path,
        target_student,
        target_optimizer,
        target,
        "b" * 64,
    )
    assert legacy_resume["step"] == 0
    assert legacy_resume["examples_seen"] == 3_072
    assert legacy_resume["input_tokens_seen"] == 12_288


def test_stable_checkpoint_contains_branchable_state_and_validation(tmp_path):
    config = ArchitectureConfig(
        "monarch",
        "sequential",
        1,
        context_length=4,
        embedding_width=4,
        monarch_blocks=4,
    )
    trial = TrialConfig(
        config,
        steps=20,
        lr_schedule="wsd",
        warmup_steps=4,
        cooldown_steps=5,
    )
    student = FullWidthStudent(config, torch.randn(7, 4), vocab_size=7)
    optimizer = torch.optim.AdamW(student.parameters())
    sum(parameter.square().sum() for parameter in student.parameters()).backward()
    optimizer.step()
    path = tmp_path / trial.label / "stable.pt"
    _save_progress_checkpoint(
        path,
        student,
        optimizer,
        trial,
        "c" * 64,
        15,
        {"kl": 2.0},
        1.0,
        examples_seen=10_000,
        input_tokens_seen=40_000,
        optimizer_steps=15,
        stable_validation={"kl": 1.25},
    )
    value = torch.load(path, weights_only=True)
    assert value["schema"] == "qwen-fullwidth-checkpoint-v3"
    assert value["step"] == 15
    assert value["optimizer_state_included"] is True
    assert value["optimizer_state_dict"]["state"]
    assert value["examples_seen"] == 10_000
    assert value["stable_validation"] == {"kl": 1.25}

    target = TrialConfig(
        config,
        steps=25,
        effective_batch=64,
        warm_start_stage=trial.stage,
        warm_start_label=trial.label,
        warm_start_step=15,
        warm_start_resume_step=15,
        warm_start_from_stable=True,
    )
    target_student = FullWidthStudent(
        config, torch.randn(7, 4), vocab_size=7
    )
    target_optimizer = torch.optim.AdamW(target_student.parameters())
    resume = _load_warm_start_checkpoint(
        path,
        target_student,
        target_optimizer,
        target,
        "c" * 64,
    )
    assert resume["step"] == resume["optimizer_steps"] == 15
    assert resume["examples_seen"] == 10_000
    assert resume["input_tokens_seen"] == 40_000
    assert resume["stable_validation"] == {"kl": 1.25}
    assert resume["optimizer_state_resumed"] is True
    assert target_optimizer.state


def test_progress_checkpoint_can_seed_a_longer_stage(tmp_path):
    config = ArchitectureConfig(
        "monarch",
        "sequential",
        1,
        context_length=4,
        embedding_width=4,
        monarch_blocks=4,
    )
    source = TrialConfig(
        config,
        lr=5e-4,
        seed=3,
        steps=128,
        stage="rank_probe",
    )
    target = TrialConfig(
        config,
        lr=5e-4,
        seed=3,
        steps=512,
        stage="rank_continue",
        warm_start_stage="rank_checkpoint",
        warm_start_label=source.label,
        warm_start_step=96,
    )
    student = FullWidthStudent(
        config, torch.randn(7, 4), vocab_size=7
    )
    optimizer = torch.optim.AdamW(student.parameters(), lr=5e-4)
    optimizer.zero_grad(set_to_none=True)
    sum(parameter.square().sum() for parameter in student.parameters()).backward()
    optimizer.step()
    expected = {
        key: value.detach().clone()
        for key, value in student.state_dict().items()
    }
    path = tmp_path / source.label / "student.pt"
    _save_progress_checkpoint(
        path,
        student,
        optimizer,
        source,
        "e" * 64,
        96,
        {"kl": 8.0},
        100.0,
    )
    with torch.no_grad():
        for parameter in student.parameters():
            parameter.zero_()
    optimizer.state.clear()
    resume = _load_warm_start_checkpoint(
        path,
        student,
        optimizer,
        target,
        "e" * 64,
    )
    assert resume == {
        "step": 96,
        "examples_seen": 98_304,
        "input_tokens_seen": 393_216,
        "optimizer_steps": 96,
        "initial_validation": {"kl": 8.0},
        "elapsed_wall_seconds": 0.0,
        "warm_started": True,
        "optimizer_state_resumed": True,
        "stable_validation": None,
    }
    for key, value in student.state_dict().items():
        torch.testing.assert_close(value, expected[key])


def test_warm_start_can_preserve_moments_and_override_lr(tmp_path):
    config = ArchitectureConfig(
        "monarch",
        "sequential",
        1,
        context_length=4,
        embedding_width=4,
        monarch_blocks=4,
    )
    source = TrialConfig(
        config,
        lr=5e-4,
        seed=3,
        steps=128,
        stage="source",
    )
    target = TrialConfig(
        config,
        lr=2e-4,
        seed=3,
        steps=256,
        stage="decay",
        warm_start_stage=source.stage,
        warm_start_label=source.label,
        warm_start_step=96,
        warm_start_lr_override=True,
    )
    source_student = FullWidthStudent(
        config, torch.randn(7, 4), vocab_size=7
    )
    source_optimizer = torch.optim.AdamW(
        source_student.parameters(), lr=source.lr
    )
    source_optimizer.zero_grad(set_to_none=True)
    sum(
        parameter.square().sum()
        for parameter in source_student.parameters()
    ).backward()
    source_optimizer.step()
    path = tmp_path / source.label / "student.pt"
    _save_progress_checkpoint(
        path,
        source_student,
        source_optimizer,
        source,
        "f" * 64,
        96,
        {"kl": 2.0},
        10.0,
    )

    target_student = FullWidthStudent(
        config, torch.randn(7, 4), vocab_size=7
    )
    target_optimizer = torch.optim.AdamW(
        target_student.parameters(),
        lr=target.lr,
        weight_decay=0.123,
    )
    resume = _load_warm_start_checkpoint(
        path,
        target_student,
        target_optimizer,
        target,
        "f" * 64,
    )
    assert resume["lr_overridden"] is True
    assert target_optimizer.state
    assert {
        group["lr"] for group in target_optimizer.param_groups
    } == {target.lr}
    assert {
        group["weight_decay"] for group in target_optimizer.param_groups
    } == {0.123}

    disallowed = TrialConfig(
        config,
        lr=target.lr,
        seed=target.seed,
        steps=target.steps,
        stage=target.stage,
        warm_start_stage=source.stage,
        warm_start_label=source.label,
        warm_start_step=96,
    )
    with pytest.raises(RuntimeError, match="warm-start LR mismatch"):
        _load_warm_start_checkpoint(
            path,
            target_student,
            target_optimizer,
            disallowed,
            "f" * 64,
        )


def test_every_final_stage_retains_optimizer_state():
    config = ArchitectureConfig(
        "monarch",
        "sequential",
        1,
        context_length=4,
        embedding_width=4,
        monarch_blocks=4,
    )
    student = FullWidthStudent(config, torch.randn(7, 4), vocab_size=7)
    optimizer = torch.optim.AdamW(student.parameters())
    final_trial = TrialConfig(
        config,
        stage="tensor_kron_rank_monarch_capacity_checkpoint",
    )
    screen_trial = TrialConfig(config, stage="screen")
    wsd_large_batch_trial = TrialConfig(
        config,
        stage="tensor_kron_edu_wsd_large_batch_lr",
    )
    assert final_trial.is_final
    assert wsd_large_batch_trial.is_final
    assert _final_checkpoint_optimizer(final_trial, optimizer) is optimizer
    assert (
        _final_checkpoint_optimizer(wsd_large_batch_trial, optimizer)
        is optimizer
    )
    assert _final_checkpoint_optimizer(screen_trial, optimizer) is None


def test_warm_start_can_change_batch_without_changing_example_cursor(
    tmp_path,
):
    config = ArchitectureConfig(
        "monarch",
        "sequential",
        1,
        context_length=4,
        embedding_width=4,
        monarch_blocks=4,
    )
    source = TrialConfig(
        config,
        lr=1e-3,
        seed=4,
        steps=128,
        stage="source",
        effective_batch=512,
    )
    target = TrialConfig(
        config,
        lr=1e-3,
        seed=4,
        steps=512,
        stage="target",
        effective_batch=128,
        warm_start_stage=source.stage,
        warm_start_label=source.label,
        warm_start_step=96,
        warm_start_resume_step=96,
    )
    source_student = FullWidthStudent(
        config, torch.randn(7, 4), vocab_size=7
    )
    source_optimizer = torch.optim.AdamW(
        source_student.parameters(), lr=source.lr
    )
    source_optimizer.zero_grad(set_to_none=True)
    sum(
        parameter.square().sum()
        for parameter in source_student.parameters()
    ).backward()
    source_optimizer.step()
    path = tmp_path / source.label / "student.pt"
    _save_progress_checkpoint(
        path,
        source_student,
        source_optimizer,
        source,
        "b" * 64,
        96,
        {"kl": 1.5},
        10.0,
    )
    target_student = FullWidthStudent(
        config, torch.randn(7, 4), vocab_size=7
    )
    target_optimizer = torch.optim.AdamW(
        target_student.parameters(), lr=target.lr
    )
    resume = _load_warm_start_checkpoint(
        path,
        target_student,
        target_optimizer,
        target,
        "b" * 64,
    )
    assert resume["step"] == resume["optimizer_steps"] == 96
    assert resume["examples_seen"] == 49_152
    assert target.to_dict()["warm_start_resume_step"] == 96

    invalid = TrialConfig(
        config,
        lr=target.lr,
        seed=target.seed,
        steps=target.steps,
        stage=target.stage,
        effective_batch=target.effective_batch,
        warm_start_stage=source.stage,
        warm_start_label=source.label,
        warm_start_step=96,
        warm_start_resume_step=95,
    )
    with pytest.raises(RuntimeError, match="optimizer age"):
        _load_warm_start_checkpoint(
            path,
            target_student,
            target_optimizer,
            invalid,
            "b" * 64,
        )
