import pytest
import torch
from qwen_kron_distill.kronecker import KroneckerSumLinear


def dense_reference(
    layer: KroneckerSumLinear,
    value: torch.Tensor,
) -> torch.Tensor:
    matrix = None
    for rank_index in range(layer.rank):
        factors = [
            (
                layer.normalized_factor(index)
                if layer.rank == 1
                else layer.normalized_factor(index)[rank_index]
            )
            for index in range(layer.order)
        ]
        term = factors[0]
        for factor in factors[1:]:
            term = torch.kron(term, factor)
        coefficient = (
            1.0
            if layer.mixing is None
            else layer.mixing.reshape(-1)[rank_index]
        )
        contribution = coefficient * term
        matrix = contribution if matrix is None else matrix + contribution
    output = value.flatten(1) @ matrix.mT
    if layer.bias is not None:
        output = output + layer.bias.flatten()
    return output.reshape(value.shape[0], *layer.output_modes)


@pytest.mark.parametrize(
    ("input_modes", "output_modes", "rank"),
    [
        ((3, 4), (2, 5), 1),
        ((3, 4), (2, 5), 4),
        ((3, 4), (1, 5), 1),
        ((3, 4), (1, 5), 4),
        ((3, 2, 3), (2, 3, 2), 1),
        ((3, 2, 3), (2, 3, 2), 4),
        ((3, 2, 3), (1, 3, 2), 1),
        ((3, 2, 3), (1, 3, 2), 4),
    ],
)
def test_tensor_native_matches_explicit_kronecker(
    input_modes,
    output_modes,
    rank,
):
    torch.manual_seed(7)
    layer = KroneckerSumLinear(
        input_modes,
        output_modes,
        rank=rank,
    ).double()
    value = torch.randn(2, *input_modes, dtype=torch.float64)

    actual = layer(value)
    expected = dense_reference(layer, value)

    torch.testing.assert_close(actual, expected, rtol=1e-10, atol=1e-10)


@pytest.mark.parametrize(
    ("input_modes", "output_modes", "rank"),
    [
        ((3, 4), (3, 4), 1),
        ((3, 4), (1, 4), 4),
        ((3, 2, 2), (3, 2, 2), 1),
        ((3, 2, 2), (1, 2, 2), 4),
    ],
)
def test_tensor_native_gradients_match_dense_reference(
    input_modes,
    output_modes,
    rank,
):
    torch.manual_seed(13)
    layer = KroneckerSumLinear(
        input_modes,
        output_modes,
        rank=rank,
    ).double()
    native_input = torch.randn(
        2,
        *input_modes,
        dtype=torch.float64,
        requires_grad=True,
    )
    dense_input = native_input.detach().clone().requires_grad_(True)
    weight = torch.randn(2, *output_modes, dtype=torch.float64)
    parameters = list(layer.parameters())

    native_gradients = torch.autograd.grad(
        (layer(native_input) * weight).sum(),
        [native_input, *parameters],
    )
    dense_gradients = torch.autograd.grad(
        (dense_reference(layer, dense_input) * weight).sum(),
        [dense_input, *parameters],
    )

    for actual, expected in zip(
        native_gradients,
        dense_gradients,
        strict=True,
    ):
        torch.testing.assert_close(
            actual,
            expected,
            rtol=2e-9,
            atol=2e-9,
        )


def test_terminal_operator_does_not_materialize_full_context_output():
    layer = KroneckerSumLinear(
        (16, 8, 8),
        (1, 8, 8),
        rank=8,
    )

    output = layer(torch.randn(2, 16, 8, 8))

    assert output.shape == (2, 1, 8, 8)
    assert layer.out_features == 64
    assert layer.in_features == 1024
