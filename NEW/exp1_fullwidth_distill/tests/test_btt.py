import math

import pytest
import torch

from qwen_fullwidth_distill.btt import BTTLinear, context_preserving_modes
from qwen_fullwidth_distill.config import ArchitectureConfig, TrialConfig
from qwen_fullwidth_distill.model import (
    FullWidthStack,
    optimizer_parameter_groups,
)


def test_context_preserving_layouts_are_exact_and_padding_free():
    assert context_preserving_modes(16_384, 16_384, 3) == (32, 16, 32)
    assert context_preserving_modes(16_384, 65_536, 3) == (64, 16, 64)
    assert context_preserving_modes(16_384, 16_384, 4) == (16, 8, 8, 16)
    assert context_preserving_modes(16_384, 65_536, 4) == (16, 16, 16, 16)
    with pytest.raises(ValueError, match="must be"):
        context_preserving_modes(16_384, 16_385, 4)


def test_btt_matches_independent_two_core_contraction_and_backpropagates():
    torch.manual_seed(3)
    layer = BTTLinear(
        6,
        6,
        input_modes=(2, 3),
        output_modes=(3, 2),
        rank=2,
        bias=False,
    ).double()
    value = torch.randn(5, 6, dtype=torch.double, requires_grad=True)
    first = layer.normalized_core(0)
    second = layer.normalized_core(1)
    expected = torch.einsum(
        "buv,rqiuv,srijv->bij",
        value.reshape(5, 2, 3),
        first,
        second,
    ).reshape(5, 6)
    actual = layer(value)
    torch.testing.assert_close(actual, expected)
    actual.square().mean().backward()
    assert value.grad is not None
    assert all(parameter.grad is not None for parameter in layer.parameters())
    assert all(
        torch.isfinite(parameter.grad).all()
        for parameter in layer.parameters()
    )


def test_btt_matches_its_materialized_dense_map():
    torch.manual_seed(4)
    layer = BTTLinear(
        8,
        8,
        input_modes=(2, 2, 2),
        output_modes=(2, 2, 2),
        rank=2,
        bias=False,
    ).double()
    basis = torch.eye(8, dtype=torch.double)
    materialized_rows = layer(basis)
    value = torch.randn(5, 8, dtype=torch.double, requires_grad=True)
    expected = value @ materialized_rows
    actual = layer(value)
    torch.testing.assert_close(actual, expected)


def test_btt_max_rms_normalization_caps_growth_but_permits_shrinkage():
    layer = BTTLinear(
        4,
        4,
        input_modes=(2, 2),
        output_modes=(2, 2),
        rank=1,
        bias=False,
    )
    target = float(layer.target_rms[0])
    with torch.no_grad():
        layer.cores[0].fill_(10 * target)
    assert math.isclose(
        float(layer.normalized_core(0).detach().square().mean().sqrt()),
        target,
        rel_tol=1e-5,
    )
    with torch.no_grad():
        layer.cores[0].fill_(target / 10)
    assert math.isclose(
        float(layer.normalized_core(0).detach().square().mean().sqrt()),
        target / 10,
        rel_tol=1e-5,
    )


def test_btt_gradcheck_includes_core_weight_normalization():
    torch.manual_seed(5)
    layer = BTTLinear(
        4,
        4,
        input_modes=(2, 2),
        output_modes=(2, 2),
        rank=1,
    ).double()
    value = torch.randn(2, 4, dtype=torch.double, requires_grad=True)
    assert torch.autograd.gradcheck(layer, (value,), fast_mode=True)


def test_btt_parameter_frontier_counts_are_exact():
    configurations = [
        (
            ArchitectureConfig(
                "btt", "residual_ffn", 11, 4, btt_cores=3, btt_rank=1
            ),
            81_821_762,
        ),
        (
            ArchitectureConfig(
                "btt", "residual_ffn", 20, 4, btt_cores=4, btt_rank=1
            ),
            85_852_320,
        ),
        (
            ArchitectureConfig(
                "btt", "residual_ffn", 7, 4, btt_cores=4, btt_rank=2
            ),
            81_428_536,
        ),
        (
            ArchitectureConfig(
                "btt", "residual_gated", 35, btt_cores=4, btt_rank=1
            ),
            84_869_540,
        ),
    ]
    with torch.device("meta"):
        for config, expected in configurations:
            stack = FullWidthStack(config)
            assert sum(parameter.numel() for parameter in stack.parameters()) == expected
            assert math.isclose(config.residual_multiplier, config.depth ** -0.5)


def test_btt_serialization_and_optimizer_parameterization_are_explicit():
    config = ArchitectureConfig(
        "btt", "residual_ffn", 20, 4, btt_cores=4, btt_rank=1
    )
    trial = TrialConfig(
        config,
        lr=3e-3,
        stage="tensor_param",
        lr_parameterization="mup",
    )
    assert config.label == "btt-c4-r1-residual_ffn-d20-x4"
    assert trial.label.endswith("-pmup-s0")
    assert trial.to_dict()["lr_parameterization"] == "mup"
    legacy = ArchitectureConfig("monarch", "residual_ffn", 1, 4)
    assert "btt_cores" not in legacy.to_dict()

    layer = BTTLinear(
        16,
        16,
        input_modes=(2, 2, 2, 2),
        output_modes=(2, 2, 2, 2),
        rank=1,
    )
    groups, metadata = optimizer_parameter_groups(layer, 1e-3, "mup")
    assert len(groups) == len(metadata)
    assert max(group["lr"] for group in groups) > 1e-3
    assert sum(row["parameters"] for row in metadata) == sum(
        parameter.numel() for parameter in layer.parameters()
    )
