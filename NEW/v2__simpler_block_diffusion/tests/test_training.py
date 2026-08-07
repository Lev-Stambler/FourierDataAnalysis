from __future__ import annotations

import pytest

from v2_simpler_block_diffusion.training import OptimizerConfig, token_lr_scale


def test_continuation_lr_schedule_uses_tokens_after_origin() -> None:
    config = OptimizerConfig(
        warmup_target_tokens=2_000_000,
        total_target_tokens=25_000_000,
        schedule_origin_target_tokens=100_133_258,
    )

    assert token_lr_scale(100_133_258, config) == pytest.approx(1 / 2_000_000)
    assert token_lr_scale(101_133_258, config) == pytest.approx(0.5)
    assert token_lr_scale(102_133_258, config) == pytest.approx(1.0)
    assert token_lr_scale(125_133_258, config) == pytest.approx(0.1)
    assert token_lr_scale(200_000_000, config) == pytest.approx(0.1)

