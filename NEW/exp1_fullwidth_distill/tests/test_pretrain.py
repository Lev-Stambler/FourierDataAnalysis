import math

import pytest
import torch
import torch.nn.functional as F

from qwen_fullwidth_distill.config import ArchitectureConfig, TrialConfig
from qwen_fullwidth_distill.model import FullWidthStudent
from qwen_fullwidth_distill.pretrain import (
    EXPECTED_TRAINABLE_PARAMETERS,
    LR_GRID,
    BATCH_VARIANTS,
    load_pretrain_checkpoint,
    next_token_cross_entropy_rows,
    pretrain_trials,
    save_pretrain_checkpoint,
    study_plan,
)


def tiny_architecture() -> ArchitectureConfig:
    return ArchitectureConfig(
        "monarch",
        "sequential",
        1,
        context_length=4,
        embedding_width=4,
        monarch_blocks=4,
    )


def test_next_token_cross_entropy_matches_pytorch():
    logits = torch.randn(7, 13)
    target = torch.tensor([0, 1, 2, 3, 4, 5, 6])
    actual = next_token_cross_entropy_rows(logits, target)
    expected = F.cross_entropy(logits.float(), target, reduction="none")
    torch.testing.assert_close(actual, expected)
    with pytest.raises(ValueError, match="rank"):
        next_token_cross_entropy_rows(logits[0], target)


def test_trainable_embedding_is_tied_once_and_receives_both_gradients():
    embedding = torch.randn(11, 4)
    model = FullWidthStudent(
        tiny_architecture(),
        embedding,
        vocab_size=11,
        trainable_embedding=True,
    )
    assert model.tied_embedding.requires_grad
    assert list(model.state_dict()).count("tied_embedding") == 1
    assert dict(model.named_parameters())["tied_embedding"].data_ptr() == (
        model.tied_embedding.data_ptr()
    )
    ids = torch.tensor([[0, 1, 2, 3], [3, 4, 5, 6]])
    target = torch.tensor([7, 8])
    next_token_cross_entropy_rows(model(ids), target).mean().backward()
    assert model.tied_embedding.grad is not None
    assert float(model.tied_embedding.grad.norm()) > 0
    assert float(model.tied_embedding.grad[0].norm()) > 0
    assert float(model.tied_embedding.grad[7].norm()) > 0


def test_pretrain_grid_is_exact_and_teacher_free():
    trials = pretrain_trials()
    assert len(trials) == 8
    assert len({trial.label for trial in trials}) == 8
    assert {
        (trial.effective_batch, trial.lr)
        for trial in trials
    } == {
        (batch, lr)
        for batch in BATCH_VARIANTS
        for lr in LR_GRID
    }
    assert {trial.steps * trial.effective_batch for trial in trials} == {
        4_194_304
    }
    assert {trial.objective for trial in trials} == {"next_token_ce"}
    assert {
        trial.embedding_initialization for trial in trials
    } == {"trainable_random"}
    assert not any(trial.use_teacher_cache for trial in trials)
    assert all(trial.is_final for trial in trials)
    assert EXPECTED_TRAINABLE_PARAMETERS == 338_552_032
    plan = study_plan()
    assert plan["teacher_used"] is False
    assert plan["teacher_cache_used"] is False
    assert len(plan["cells"]) == 8
    assert len(plan["plan_sha256"]) == 64
    assert (
        EXPECTED_TRAINABLE_PARAMETERS
        - 248_320 * 1_024
        == 84_272_352
    )


def test_next_token_checkpoint_restores_embedding_and_adam(tmp_path):
    config = tiny_architecture()
    trial = TrialConfig(
        config,
        lr=1e-3,
        steps=4,
        effective_batch=2,
        objective="next_token_ce",
        embedding_initialization="trainable_random",
    )
    student = FullWidthStudent(
        config,
        torch.randn(11, 4),
        vocab_size=11,
        trainable_embedding=True,
    )
    optimizer = torch.optim.AdamW(student.parameters(), lr=trial.lr)
    ids = torch.tensor([[0, 1, 2, 3], [3, 4, 5, 6]])
    target = torch.tensor([7, 8])
    next_token_cross_entropy_rows(student(ids), target).mean().backward()
    optimizer.step()
    expected_state = {
        key: value.detach().clone()
        for key, value in student.state_dict().items()
    }
    expected_optimizer = optimizer.state_dict()
    initial_hash = "a" * 64
    path = tmp_path / "progress.pt"
    save_pretrain_checkpoint(
        path,
        student,
        optimizer,
        trial,
        step=2,
        examples_seen=4,
        initial_embedding_sha256=initial_hash,
        initial_validation={
            "cross_entropy": math.log(11),
            "perplexity": 11.0,
        },
        elapsed_wall_seconds=1.25,
        stable_validation=None,
    )
    with torch.no_grad():
        for parameter in student.parameters():
            parameter.zero_()
    optimizer.state.clear()
    resumed = load_pretrain_checkpoint(
        path,
        student,
        optimizer,
        trial,
        initial_hash,
    )
    assert resumed["step"] == 2
    assert resumed["examples_seen"] == 4
    assert resumed["elapsed_wall_seconds"] == pytest.approx(1.25)
    for key, value in student.state_dict().items():
        torch.testing.assert_close(value, expected_state[key])
    restored_optimizer = optimizer.state_dict()
    assert restored_optimizer["param_groups"] == expected_optimizer["param_groups"]
    assert restored_optimizer["state"].keys() == expected_optimizer["state"].keys()


def test_legacy_trial_identity_remains_implicit():
    trial = TrialConfig(tiny_architecture())
    assert trial.objective == "forward_kl"
    assert trial.embedding_initialization == "frozen_qwen"
    assert "objective" not in trial.to_dict()
    assert "embedding_initialization" not in trial.to_dict()
    assert "-ce" not in trial.label
    assert "-erand" not in trial.label
