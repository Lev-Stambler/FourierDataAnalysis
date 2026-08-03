from __future__ import annotations

import importlib.util
import math
from pathlib import Path

import torch


MODULE_PATH = Path(__file__).with_name("matched_standard.py")
SPEC = importlib.util.spec_from_file_location("expv6_matched_standard", MODULE_PATH)
assert SPEC is not None and SPEC.loader is not None
expv6 = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(expv6)


def test_parameter_matched_architectures() -> None:
    torch.manual_seed(0)
    kronecker = expv6.Student("kronecker")
    transformer = expv6.Student("transformer")
    assert expv6.assert_parameter_match(kronecker) == {
        "trainable_parameters": 1_177_920,
        "vocabulary_parameters": 63_808,
        "body_parameters": 1_114_112,
    }
    assert expv6.parameter_inventory(transformer) == expv6.parameter_inventory(
        kronecker
    )


def test_transformer_is_causal() -> None:
    torch.manual_seed(1)
    prior = expv6.CONFIG["activation_checkpointing"]
    expv6.CONFIG["activation_checkpointing"] = False
    try:
        model = expv6.Student("transformer").eval()
        tokens = torch.randint(
            0,
            math.prod(expv6.CONFIG["vocab_modes"]),
            (1, expv6.CONFIG["context_length"]),
        )
        changed = tokens.clone()
        changed[:, 8:] = torch.randint(
            0,
            math.prod(expv6.CONFIG["vocab_modes"]),
            (1, expv6.CONFIG["context_length"] - 8),
        )
        original_hidden = model.sequence(tokens)
        changed_hidden = model.sequence(changed)
        torch.testing.assert_close(
            original_hidden[:, :8],
            changed_hidden[:, :8],
            atol=0,
            rtol=0,
        )
    finally:
        expv6.CONFIG["activation_checkpointing"] = prior


def test_batch_indices_are_deterministic_and_rank_disjoint() -> None:
    prior = expv6.CONFIG["local_batch"]
    expv6.CONFIG["local_batch"] = 8
    try:
        first = expv6.batch_indices(1_000, 7, 0, 2)
        repeated = expv6.batch_indices(1_000, 7, 0, 2)
        second_rank = expv6.batch_indices(1_000, 7, 1, 2)
        assert first.tolist() == repeated.tolist()
        assert set(first).isdisjoint(second_rank)
    finally:
        expv6.CONFIG["local_batch"] = prior


def test_checkpoint_identity_excludes_expandable_target() -> None:
    original = expv6.CONFIG["steps"]
    try:
        expv6.CONFIG["steps"] = 16
        screen = expv6.checkpoint_identity(8)
        expv6.CONFIG["steps"] = 144
        full = expv6.checkpoint_identity(8)
        assert screen == full
        assert screen["global_token_batch"] == 7_471_104
    finally:
        expv6.CONFIG["steps"] = original


def test_rope_preserves_shape_and_position_zero() -> None:
    value = torch.randn(2, 4, 16, 16)
    rotated = expv6.apply_rope(value)
    assert rotated.shape == value.shape
    torch.testing.assert_close(rotated[:, :, 0], value[:, :, 0])


def test_clip_grad_norm_returns_pre_clip_norm() -> None:
    parameter = torch.nn.Parameter(torch.zeros(2))
    parameter.grad = torch.tensor([3.0, 4.0])
    norm = torch.nn.utils.clip_grad_norm_([parameter], 1.0)
    assert float(norm) == 5.0
    assert parameter.grad.norm() <= 1.0


def test_comparison_summary_applies_screen_and_replication_rules() -> None:
    results = []
    for architecture in expv6.ARCHITECTURES:
        for lr, kl, speed in ((0.1, 3.1, 9.0), (0.2, 3.105, 11.0)):
            results.append(
                {
                    "architecture": architecture,
                    "seed": 0,
                    "lr": lr,
                    "step": 48,
                    "validation_history": {
                        "16": {"kl": kl + 1},
                        "48": {"kl": kl},
                    },
                    "performance_history": {
                        "16": {"tokens_per_second": speed - 1},
                        "48": {"tokens_per_second": speed},
                    },
                }
            )
        for seed in (0, 1, 2):
            results.append(
                {
                    "architecture": architecture,
                    "seed": seed,
                    "lr": 0.2,
                    "step": 144,
                    "validation": {"kl": 2.9 + 0.03 * (architecture == "transformer")},
                    "confirmation": {"kl": 3.0 + 0.04 * (architecture == "transformer")},
                }
            )
    summary = expv6.comparison_summary(results)
    assert summary["selected"]["transformer"]["lr"] == 0.2
    assert summary["selected"]["kronecker"]["lr"] == 0.2
    assert summary["replication_status"] == "complete"
    assert summary["confirmation_evaluation_complete"] is True
    assert math.isclose(summary["paired_validation_kl_deltas"]["0"], 0.03)
    assert math.isclose(summary["paired_confirmation_kl_deltas"]["0"], 0.04)
