import math

import pytest
import torch

from qwen_fullwidth_distill.config import ArchitectureConfig
from qwen_fullwidth_distill.kronecker import (
    KroneckerLinear,
    context_channel_modes,
)
from qwen_fullwidth_distill.model import FullWidthStack
from qwen_fullwidth_distill.monarch import MonarchLinear


def test_context_channel_modes_are_axis_aligned_and_padding_free():
    assert context_channel_modes(16, 1024, 16_384) == (16, 32, 32)
    assert context_channel_modes(16, 1024, 65_536) == (16, 64, 64)
    assert context_channel_modes(32, 1024, 32_768) == (32, 32, 32)
    assert context_channel_modes(
        16, 1024, 16_384, order=4
    ) == (16, 8, 8, 16)
    assert context_channel_modes(
        16, 1024, 65_536, order=4
    ) == (16, 16, 16, 16)
    with pytest.raises(ValueError, match="square"):
        context_channel_modes(16, 1024, 16 * 1000)
    with pytest.raises(ValueError, match="order three or four"):
        context_channel_modes(16, 1024, 16_384, order=5)


def test_runtime_rank_chunk_override_does_not_change_parameters(monkeypatch):
    monkeypatch.setenv("QWEN_KRONECKER_RANK_CHUNK", "1024")
    layer = KroneckerLinear(
        8,
        8,
        input_modes=(2, 2, 2),
        output_modes=(2, 2, 2),
        rank=3,
        rank_chunk=2,
    )
    assert layer.rank_chunk == 1024
    assert layer.rank == 3
    assert sum(parameter.numel() for parameter in layer.parameters()) == 56


def test_order_three_matches_explicit_kronecker_forward_and_gradients():
    torch.manual_seed(17)
    layer = KroneckerLinear(
        12,
        12,
        input_modes=(2, 2, 3),
        output_modes=(3, 2, 2),
        bias=True,
    ).double()
    value = torch.randn(4, 12, dtype=torch.double, requires_grad=True)
    factors = [layer.normalized_factor(index) for index in range(3)]
    weight = torch.kron(torch.kron(factors[0], factors[1]), factors[2])
    expected = torch.nn.functional.linear(value, weight, layer.bias)
    actual = layer(value)
    torch.testing.assert_close(actual, expected)

    upstream = torch.randn_like(actual)
    actual.backward(upstream, retain_graph=True)
    actual_input_gradient = value.grad.detach().clone()
    actual_factor_gradients = [
        factor.grad.detach().clone() for factor in layer.factors
    ]
    value.grad = None
    for factor in layer.factors:
        factor.grad = None
    expected.backward(upstream)
    torch.testing.assert_close(value.grad, actual_input_gradient)
    for factor, gradient in zip(
        layer.factors, actual_factor_gradients, strict=True
    ):
        torch.testing.assert_close(factor.grad, gradient)


def test_rank_sum_matches_explicit_kronecker_forward_and_gradients():
    torch.manual_seed(23)
    layer = KroneckerLinear(
        12,
        12,
        input_modes=(2, 2, 3),
        output_modes=(3, 2, 2),
        bias=True,
        rank=3,
        rank_chunk=2,
    ).double()
    value = torch.randn(5, 12, dtype=torch.double, requires_grad=True)
    factors = [layer.normalized_factor(index) for index in range(3)]
    weight = sum(
        layer.mixing[index]
        * torch.kron(
            torch.kron(factors[0][index], factors[1][index]),
            factors[2][index],
        )
        for index in range(layer.rank)
    )
    expected = torch.nn.functional.linear(value, weight, layer.bias)
    actual = layer(value)
    torch.testing.assert_close(actual, expected)

    upstream = torch.randn_like(actual)
    actual.backward(upstream, retain_graph=True)
    actual_input_gradient = value.grad.detach().clone()
    actual_factor_gradients = [
        factor.grad.detach().clone() for factor in layer.factors
    ]
    actual_mixing_gradient = layer.mixing.grad.detach().clone()
    value.grad = None
    layer.mixing.grad = None
    for factor in layer.factors:
        factor.grad = None
    expected.backward(upstream)
    torch.testing.assert_close(value.grad, actual_input_gradient)
    torch.testing.assert_close(layer.mixing.grad, actual_mixing_gradient)
    for factor, gradient in zip(
        layer.factors, actual_factor_gradients, strict=True
    ):
        torch.testing.assert_close(factor.grad, gradient)


def test_rank_sum_reduction_first_path_matches_explicit_weight():
    torch.manual_seed(29)
    layer = KroneckerLinear(
        24,
        8,
        input_modes=(2, 4, 3),
        output_modes=(2, 2, 2),
        bias=True,
        rank=3,
        rank_chunk=2,
    ).double()
    value = torch.randn(5, 24, dtype=torch.double, requires_grad=True)
    factors = [layer.normalized_factor(index) for index in range(3)]
    weight = sum(
        layer.mixing[index]
        * torch.kron(
            torch.kron(factors[0][index], factors[1][index]),
            factors[2][index],
        )
        for index in range(layer.rank)
    )
    expected = torch.nn.functional.linear(value, weight, layer.bias)
    actual = layer(value)
    torch.testing.assert_close(actual, expected)

    upstream = torch.randn_like(actual)
    actual.backward(upstream, retain_graph=True)
    actual_gradients = [
        value.grad.detach().clone(),
        layer.mixing.grad.detach().clone(),
        *(factor.grad.detach().clone() for factor in layer.factors),
    ]
    value.grad = None
    layer.mixing.grad = None
    for factor in layer.factors:
        factor.grad = None
    expected.backward(upstream)
    expected_gradients = [
        value.grad,
        layer.mixing.grad,
        *(factor.grad for factor in layer.factors),
    ]
    for actual_gradient, expected_gradient in zip(
        actual_gradients, expected_gradients, strict=True
    ):
        torch.testing.assert_close(actual_gradient, expected_gradient)


def test_order_four_rank_sum_matches_explicit_forward_and_all_gradients():
    torch.manual_seed(31)
    layer = KroneckerLinear(
        24,
        24,
        input_modes=(2, 2, 3, 2),
        output_modes=(3, 2, 2, 2),
        bias=True,
        rank=3,
        rank_chunk=2,
    ).double()
    value = torch.randn(2, 3, 24, dtype=torch.double, requires_grad=True)
    factors = [layer.normalized_factor(index) for index in range(4)]
    weight = 0
    for rank_index in range(layer.rank):
        term = factors[0][rank_index]
        for factor in factors[1:]:
            term = torch.kron(term, factor[rank_index])
        weight = weight + layer.mixing[rank_index] * term
    expected = torch.nn.functional.linear(value, weight, layer.bias)
    actual = layer(value)
    torch.testing.assert_close(actual, expected)

    upstream = torch.randn_like(actual)
    actual.backward(upstream, retain_graph=True)
    actual_input_gradient = value.grad.detach().clone()
    actual_parameter_gradients = {
        name: parameter.grad.detach().clone()
        for name, parameter in layer.named_parameters()
    }
    value.grad = None
    for parameter in layer.parameters():
        parameter.grad = None
    expected.backward(upstream)
    torch.testing.assert_close(value.grad, actual_input_gradient)
    for name, parameter in layer.named_parameters():
        torch.testing.assert_close(
            parameter.grad,
            actual_parameter_gradients[name],
        )


def test_order_four_rank_sum_validates_modes_and_preserves_batch_shape():
    layer = KroneckerLinear(
        16,
        24,
        input_modes=(2, 2, 2, 2),
        output_modes=(3, 2, 2, 2),
        rank=2,
        rank_chunk=1,
    )
    assert layer(torch.randn(2, 3, 16)).shape == (2, 3, 24)
    with pytest.raises(ValueError, match="expected final width 16"):
        layer(torch.randn(2, 15))
    with pytest.raises(ValueError, match="equal order"):
        KroneckerLinear(
            16,
            8,
            input_modes=(2, 2, 2, 2),
            output_modes=(2, 2, 2),
            rank=2,
        )
    with pytest.raises(ValueError, match="input modes"):
        KroneckerLinear(
            16,
            16,
            input_modes=(2, 2, 2, 3),
            output_modes=(2, 2, 2, 2),
            rank=2,
        )


def test_context_axis_basis_order_is_preserved():
    layer = KroneckerLinear(
        8,
        8,
        input_modes=(2, 2, 2),
        output_modes=(2, 2, 2),
        bias=False,
    )
    with torch.no_grad():
        for factor in layer.factors:
            factor.copy_(torch.eye(2))
        for gain in layer.gains:
            gain.fill_(1)
    basis = torch.eye(8)
    torch.testing.assert_close(layer(basis), basis)


def test_rectangular_context_aligned_map_has_exact_shape():
    layer = KroneckerLinear(
        16,
        64,
        input_modes=(2, 2, 4),
        output_modes=(2, 4, 8),
    )
    assert layer(torch.randn(5, 16)).shape == (5, 64)
    assert sum(factor.numel() for factor in layer.factors) == 44


def test_rank_one_monarch_is_not_a_plain_kronecker_product():
    kronecker = KroneckerLinear(
        16,
        16,
        input_modes=(4, 4),
        output_modes=(4, 4),
        bias=False,
    )
    monarch = MonarchLinear(16, 16, nblocks=4, rank=1, bias=False)
    assert sum(
        factor.numel() for factor in kronecker.factors
    ) == 32
    assert (
        monarch.factor1.numel() + monarch.factor2.numel()
    ) == 128


def test_parameter_matched_depth_788_count_is_exact():
    config = ArchitectureConfig(
        "kronecker",
        "residual_ffn",
        788,
        4,
    )
    config.validate()
    with torch.device("meta"):
        stack = FullWidthStack(config)
    assert sum(parameter.numel() for parameter in stack.parameters()) == (
        84_327_032
    )
    assert math.isclose(config.residual_multiplier, 788 ** -0.5)
    assert config.label == "kronecker-c3-residual_ffn-d788-x4"
    assert "kronecker_layout" in config.to_dict()


@pytest.mark.parametrize(
    ("depth", "rank", "expected"),
    (
        (16, 593, 84_232_320),
        (32, 291, 84_271_872),
        (64, 140, 84_350_976),
        (128, 64, 83_951_616),
    ),
)
def test_rank_sum_parameter_frontier_is_matched(depth, rank, expected):
    config = ArchitectureConfig(
        "kronecker",
        "residual_ffn",
        depth,
        4,
        kronecker_rank=rank,
        kronecker_rank_chunk=32,
    )
    config.validate()
    with torch.device("meta"):
        stack = FullWidthStack(config)
    assert sum(parameter.numel() for parameter in stack.parameters()) == expected
    assert config.label == (
        f"kronecker-c3-r{rank}-residual_ffn-d{depth}-x4"
    )
    assert config.to_dict()["kronecker_rank_chunk"] == 32
