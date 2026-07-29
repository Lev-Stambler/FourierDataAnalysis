import pytest
import torch

from qwen_fullwidth_distill.config import ArchitectureConfig, TrialConfig
from qwen_fullwidth_distill.train import (
    ADAMW,
    _microbatch_chunks,
    _training_order,
    _validation_examples_for_step,
    checkpoint_due,
    distribution_rows,
    forward_kl_rows,
    lr_schedule_multiplier,
    normalized_hidden_mse_rows,
    stable_checkpoint_step,
    tensor_sha256,
)


def test_exact_kl_is_zero_for_identical_logits():
    logits = torch.randn(4, 11)
    target = torch.tensor([0, 1, 2, 3])
    rows = distribution_rows(logits, logits, target)
    torch.testing.assert_close(rows["kl"], torch.zeros(4), atol=2e-7, rtol=0)
    torch.testing.assert_close(
        rows["teacher_cross_entropy"], rows["teacher_nll"]
    )
    torch.testing.assert_close(
        rows["student_cross_entropy"], rows["student_nll"]
    )


def test_softened_kl_is_zero_and_hidden_mse_is_scale_normalized():
    logits = torch.randn(4, 11)
    torch.testing.assert_close(
        forward_kl_rows(logits, logits, temperature=2.0),
        torch.zeros(4),
        atol=2e-7,
        rtol=0,
    )
    teacher = torch.tensor([[1.0, -1.0], [2.0, -2.0]])
    student = torch.zeros_like(teacher)
    torch.testing.assert_close(
        normalized_hidden_mse_rows(teacher, student),
        torch.ones(2),
    )
    with pytest.raises(ValueError, match="shapes"):
        normalized_hidden_mse_rows(teacher, student[:, :1])


def test_standard_adamw_constants():
    assert ADAMW == {
        "betas": (0.9, 0.999),
        "eps": 1e-8,
        "weight_decay": 0.01,
        "fused": True,
    }


def test_warmup_cosine_schedule_restarts_at_warm_start_boundary():
    trial = TrialConfig(
        ArchitectureConfig(
            "monarch",
            "sequential",
            1,
            context_length=4,
            embedding_width=4,
            monarch_blocks=4,
        ),
        steps=200,
        warm_start_step=100,
        lr_schedule="warmup_cosine",
        warmup_steps=10,
        min_lr_ratio=0.1,
        gradient_clip_norm=1.0,
    )
    assert lr_schedule_multiplier(trial, 101) == pytest.approx(0.1)
    assert lr_schedule_multiplier(trial, 110) == pytest.approx(1.0)
    assert lr_schedule_multiplier(trial, 200) == pytest.approx(0.1)
    assert 0.1 < lr_schedule_multiplier(trial, 155) < 1.0


def test_example_based_warmup_is_batch_invariant():
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
        steps=200,
        effective_batch=32,
        lr_schedule="warmup_cosine",
        warmup_examples=320,
        min_lr_ratio=0.1,
    )
    assert lr_schedule_multiplier(trial, 1) == pytest.approx(0.1)
    assert lr_schedule_multiplier(trial, 10) == pytest.approx(1.0)


def test_warmup_hold_ramps_from_floor_then_stays_at_peak():
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
        lr_schedule="warmup_hold",
        warmup_steps=5,
        min_lr_ratio=0.2,
    )
    assert lr_schedule_multiplier(trial, 1) == pytest.approx(0.2)
    assert lr_schedule_multiplier(trial, 3) == pytest.approx(0.6)
    assert lr_schedule_multiplier(trial, 5) == pytest.approx(1.0)
    assert lr_schedule_multiplier(trial, 6) == pytest.approx(1.0)
    assert lr_schedule_multiplier(trial, 20) == pytest.approx(1.0)


def test_wsd_warms_holds_and_linearly_cools_to_zero():
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
    assert stable_checkpoint_step(trial) == 15
    assert lr_schedule_multiplier(trial, 1) == pytest.approx(0.25)
    assert lr_schedule_multiplier(trial, 4) == pytest.approx(1.0)
    assert lr_schedule_multiplier(trial, 15) == pytest.approx(1.0)
    assert lr_schedule_multiplier(trial, 16) == pytest.approx(0.8)
    assert lr_schedule_multiplier(trial, 20) == pytest.approx(0.0)
    assert _validation_examples_for_step(trial, 15, 8_192) == 8_192
    assert _validation_examples_for_step(trial, 14, 8_192) < 8_192


def test_tail_microbatch_partition_has_exact_weighted_mean():
    chunks = _microbatch_chunks(10, 4)
    assert chunks == (4, 4, 2)
    values = torch.arange(10, dtype=torch.float32)
    cursor = 0
    weighted = 0.0
    for chunk in chunks:
        weighted += float(values[cursor : cursor + chunk].mean()) * chunk / 10
        cursor += chunk
    assert cursor == 10
    assert weighted == pytest.approx(float(values.mean()))


def test_progress_checkpoint_cadence_uses_local_continuation_steps():
    trial = TrialConfig(
        ArchitectureConfig(
            "monarch",
            "sequential",
            1,
            context_length=4,
            embedding_width=4,
            monarch_blocks=4,
        ),
        steps=100,
        effective_batch=256,
        checkpoint_every_examples=2_048,
    )
    assert checkpoint_due(trial, 8)
    assert checkpoint_due(trial, 16)
    assert not checkpoint_due(trial, 7)
    assert not checkpoint_due(trial, 100)


def test_invalid_schedule_contracts_fail_before_training():
    config = ArchitectureConfig(
        "monarch",
        "sequential",
        1,
        context_length=4,
        embedding_width=4,
        monarch_blocks=4,
    )
    with pytest.raises(ValueError, match="constant LR"):
        lr_schedule_multiplier(
            TrialConfig(config, steps=10, warmup_steps=2),
            10,
        )
    with pytest.raises(ValueError, match="stable scheduled"):
        lr_schedule_multiplier(
            TrialConfig(
                config,
                steps=10,
                lr_schedule="warmup_cosine",
                warmup_steps=10,
                min_lr_ratio=0.1,
            ),
            10,
        )


def test_tensor_hash_supports_bfloat16_and_tracks_raw_bits():
    value = torch.tensor([[1.0, -2.0], [3.0, 4.0]], dtype=torch.bfloat16)
    first = tensor_sha256(value)
    assert first == tensor_sha256(value.clone())
    value[0, 0] = 1.5
    assert first != tensor_sha256(value)


def test_long_run_training_order_reuses_data_deterministically():
    first_epoch = _training_order(11, 11, 7, False)
    repeated = _training_order(11, 27, 7, True)
    torch.testing.assert_close(
        torch.from_numpy(repeated[:11]),
        torch.from_numpy(first_epoch),
    )
    assert len(repeated) == 27
    assert set(repeated) == set(range(11))
    with pytest.raises(ValueError, match="only 11"):
        _training_order(11, 12, 7, False)
