import json

import pytest
import torch
from qwen_lsh_monarch.config import ArchitectureConfig, TrialConfig
from qwen_lsh_monarch.model import LSHMonarchStudent
from qwen_lsh_monarch.train import (
    ADAMW,
    _load_completed_result,
    _load_progress_checkpoint,
    _save_progress_checkpoint,
    distribution_rows,
)


def signed_codes(vocab=19):
    generator = torch.Generator().manual_seed(5)
    return (
        torch.randint(0, 2, (vocab, 18), generator=generator).float() * 2 - 1
    )


def result_payload(trial):
    return {
        "label": trial.label,
        "architecture": trial.architecture.to_dict(),
        "lr": trial.lr,
        "seed": trial.seed,
        "steps": trial.steps,
        "validation": {"kl": 1.0},
        "test": {},
        "embedding_sha256": "a" * 64,
        "codebook_sha256": "b" * 64,
    }


def test_loss_and_optimizer_constants_match_protocol():
    logits = torch.randn(4, 13)
    rows = distribution_rows(logits, logits, torch.tensor([0, 1, 2, 3]))
    torch.testing.assert_close(rows["kl"], torch.zeros(4), atol=2e-7, rtol=0)
    assert ADAMW == {
        "betas": (0.9, 0.999),
        "eps": 1e-8,
        "weight_decay": 0.01,
        "fused": True,
    }


def test_completed_result_requires_matching_codebook(tmp_path):
    trial = TrialConfig(ArchitectureConfig("sequential", 1))
    value = result_payload(trial)
    (tmp_path / "result.json").write_text(json.dumps(value))
    assert _load_completed_result(tmp_path, trial, "b" * 64) == value
    assert _load_completed_result(tmp_path, trial, "c" * 64) is None


def test_final_cache_requires_test_and_checkpoint(tmp_path):
    trial = TrialConfig(
        ArchitectureConfig("residual_ffn", 1, 4),
        stage="final",
        steps=4_000,
    )
    value = result_payload(trial)
    (tmp_path / "result.json").write_text(json.dumps(value))
    assert _load_completed_result(tmp_path, trial, "b" * 64) is None
    value["test"] = {"kl": 1.1}
    (tmp_path / "result.json").write_text(json.dumps(value))
    assert _load_completed_result(tmp_path, trial, "b" * 64) is None
    (tmp_path / "student.pt").write_bytes(b"complete")
    assert _load_completed_result(tmp_path, trial, "b" * 64) == value


def test_progress_restores_model_optimizer_and_checks_both_hashes(tmp_path):
    trial = TrialConfig(
        ArchitectureConfig("sequential", 1),
        stage="final",
        steps=4_000,
    )
    student = LSHMonarchStudent(trial.architecture, signed_codes())
    optimizer = torch.optim.AdamW(student.parameters(), lr=1e-3)
    optimizer.zero_grad(set_to_none=True)
    sum(parameter.square().sum() for parameter in student.parameters()).backward()
    optimizer.step()
    expected = {
        key: value.detach().clone()
        for key, value in student.state_dict().items()
    }
    path = tmp_path / "progress.pt"
    _save_progress_checkpoint(
        path,
        student,
        optimizer,
        trial,
        "a" * 64,
        "b" * 64,
        1_000,
        {"kl": 3.0},
        12.5,
    )
    with torch.no_grad():
        for parameter in student.parameters():
            parameter.zero_()
    optimizer.state.clear()
    resume = _load_progress_checkpoint(
        path, student, optimizer, trial, "a" * 64, "b" * 64
    )
    assert resume == {
        "step": 1_000,
        "initial_validation": {"kl": 3.0},
        "elapsed_wall_seconds": 12.5,
    }
    for key, value in student.state_dict().items():
        torch.testing.assert_close(value, expected[key])
    assert optimizer.state_dict()["state"]
    with pytest.raises(RuntimeError, match="codebook_sha256"):
        _load_progress_checkpoint(
            path, student, optimizer, trial, "a" * 64, "c" * 64
        )
