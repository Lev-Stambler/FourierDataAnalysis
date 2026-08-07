from __future__ import annotations

from dataclasses import asdict

import pytest
import torch
import torch.nn.functional as F

from exp17_group_density.model import (
    DENSE_GROUP,
    GROUP_DEEP,
    GROUP_HYBRID,
    GROUP_R1,
    GROUP_R2,
    GROUP_R4,
    MODEL_NAMES,
    NO_ROUTER_TOKEN,
    CausalToeplitzFactor,
    DilatedCausalPrefixFactor,
    KroneckerGroupSwiGLU,
    LocalGroupDecoder,
    ModelSpec,
    build_model,
    group_rms_norm,
    model_inventory,
)


TARGET_PARAMETERS = 5_400_896


def overrides(spec: ModelSpec) -> dict:
    values = asdict(spec)
    values.pop("name")
    return values


def tiny_spec(name: str, *, outer_kind: str = "dense", decoder: bool = False) -> ModelSpec:
    kinds = {
        NO_ROUTER_TOKEN: dict(ffn_kind="token", token_ffn_width=7),
        DENSE_GROUP: dict(ffn_kind="dense-group", dense_group_width=3),
        GROUP_R1: dict(
            ffn_kind="kron-group",
            group_rank=1,
            hidden_workspace=5,
            hidden_channel=6,
        ),
        GROUP_R2: dict(
            ffn_kind="kron-group",
            group_rank=2,
            hidden_workspace=4,
            hidden_channel=4,
        ),
        GROUP_R4: dict(
            ffn_kind="kron-group",
            group_rank=2,
            hidden_workspace=4,
            hidden_channel=4,
        ),
        GROUP_HYBRID: dict(
            ffn_kind="hybrid",
            token_ffn_width=4,
            group_rank=1,
            hidden_workspace=4,
            hidden_channel=4,
        ),
        GROUP_DEEP: dict(
            ffn_kind="kron-group",
            group_rank=1,
            hidden_workspace=5,
            hidden_channel=6,
        ),
    }
    return ModelSpec(
        name=name,
        context_length=8,
        vocab_size=32,
        width=8,
        depth=2,
        group_size=4,
        workspace1=2,
        workspace2=2,
        channel1=2,
        channel2=4,
        mixer_rank=2,
        outer_kind=outer_kind,
        local_decoder_layers=2 if decoder else 0,
        local_decoder_heads=2,
        local_decoder_ffn_width=12,
        **kinds[name],
    )


def test_all_primary_models_are_total_parameter_matched() -> None:
    for name in MODEL_NAMES:
        inventory = model_inventory(build_model(name))
        mismatch = abs(inventory["total_parameters"] / TARGET_PARAMETERS - 1.0)
        assert mismatch <= 0.001, (name, inventory)
        assert inventory["active_parameters"] == inventory["total_parameters"]


def test_expected_activation_fanout() -> None:
    token = model_inventory(build_model(NO_ROUTER_TOKEN))
    r1 = model_inventory(build_model(GROUP_R1))
    deep = model_inventory(build_model(GROUP_DEEP))
    dense = model_inventory(build_model(DENSE_GROUP))
    assert r1["nonlinear_activation_sites_per_example"] > 3.8 * token[
        "nonlinear_activation_sites_per_example"
    ]
    assert deep["nonlinear_activation_sites_per_example"] > 7.5 * token[
        "nonlinear_activation_sites_per_example"
    ]
    assert dense["nonlinear_activation_sites_per_example"] == 32 * 16 * 16


@pytest.mark.parametrize("name", MODEL_NAMES)
def test_tiny_models_have_finite_complete_gradients(name: str) -> None:
    torch.manual_seed(4)
    model = build_model(name, **overrides(tiny_spec(name)))
    tokens = torch.randint(0, 32, (2, 8))
    loss = F.cross_entropy(model(tokens).flatten(0, 1), tokens.flatten())
    loss.backward()
    assert torch.isfinite(loss)
    assert all(
        parameter.grad is not None and torch.isfinite(parameter.grad).all()
        for parameter in model.parameters()
    )
    assert not any("router" in name for name, _ in model.named_parameters())


@pytest.mark.parametrize("kind", ["prefix", "toeplitz"])
def test_outer_factors_match_materialized_application(kind: str) -> None:
    torch.manual_seed(7)
    factor = (
        DilatedCausalPrefixFactor(2, 7)
        if kind == "prefix"
        else CausalToeplitzFactor(2, 7)
    ).double()
    value = torch.randn(3, 2, 7, 5, dtype=torch.float64, requires_grad=True)
    actual = factor.apply(value)
    expected = torch.einsum("rsg,brgd->brsd", factor.matrix(), value)
    torch.testing.assert_close(actual, expected, rtol=1e-10, atol=1e-10)
    actual.square().sum().backward()
    assert value.grad is not None and torch.isfinite(value.grad).all()
    assert all(parameter.grad is not None for parameter in factor.parameters())


@pytest.mark.parametrize("kind", ["prefix", "toeplitz"])
def test_outer_factors_are_causal_and_globally_reaching(kind: str) -> None:
    torch.manual_seed(11)
    factor = (
        DilatedCausalPrefixFactor(2, 8)
        if kind == "prefix"
        else CausalToeplitzFactor(2, 8)
    )
    matrix = factor.matrix()
    assert torch.count_nonzero(torch.triu(matrix, diagonal=1)) == 0
    assert torch.all(matrix[:, -1, 0].abs() > 0)


@pytest.mark.parametrize("outer_kind", ["dense", "prefix", "toeplitz"])
def test_future_group_isolation(outer_kind: str) -> None:
    torch.manual_seed(13)
    spec = tiny_spec(GROUP_R1, outer_kind=outer_kind)
    model = build_model(GROUP_R1, **overrides(spec)).eval()
    first = torch.randint(0, 32, (1, 8))
    second = first.clone()
    second[:, 4:] = torch.randint(0, 32, (1, 4))
    with torch.no_grad():
        hidden_first = model.hidden(first)
        hidden_second = model.hidden(second)
    torch.testing.assert_close(hidden_first[:, :4], hidden_second[:, :4])


def test_group_ffn_has_cross_token_nonlinear_dependence() -> None:
    torch.manual_seed(17)
    module = KroneckerGroupSwiGLU(4, 8, 2, 5, 6).double()
    value = torch.randn(1, 1, 4, 8, dtype=torch.float64, requires_grad=True)
    output = module(value)
    derivative = torch.autograd.grad(output[0, 0, 3, 2], value)[0]
    assert derivative[0, 0, 0].abs().sum() > 1e-8


def test_group_ffn_matches_explicit_dense_kronecker_matrices() -> None:
    torch.manual_seed(19)
    module = KroneckerGroupSwiGLU(4, 6, 2, 3, 5).double()
    value = torch.randn(2, 1, 4, 6, dtype=torch.float64)
    normalized = group_rms_norm(value) * module.input_channel_scale
    flat = normalized.flatten(2)
    paths = []
    for rank in range(module.rank):
        gate_matrix = torch.kron(
            module.normalized(module.gate_workspace)[rank],
            module.normalized(module.gate_channel)[rank],
        )
        up_matrix = torch.kron(
            module.normalized(module.up_workspace)[rank],
            module.normalized(module.up_channel)[rank],
        )
        down_matrix = torch.kron(
            module.normalized(module.down_workspace)[rank],
            module.normalized(module.down_channel)[rank],
        )
        hidden = F.silu(F.linear(flat, gate_matrix)) * F.linear(flat, up_matrix)
        paths.append(F.linear(hidden, down_matrix))
    expected = torch.stack(paths, 2)
    expected = torch.einsum("bgrd,r->bgd", expected, module.path_amplitudes)
    expected = expected.reshape_as(value) * module.output_channel_scale
    torch.testing.assert_close(module(value), expected, rtol=1e-10, atol=1e-10)


def test_local_decoder_is_causal_inside_group() -> None:
    torch.manual_seed(23)
    spec = tiny_spec(GROUP_R1, decoder=True)
    decoder = LocalGroupDecoder(spec).eval()
    vocabulary = torch.randn(32, 8)
    context = torch.randn(1, 2, 4, 8)
    first = torch.randint(0, 32, (1, 2, 4))
    second = first.clone()
    second[:, :, 2:] = torch.randint(0, 32, (1, 2, 2))
    with torch.no_grad():
        output_first = decoder(context, first, vocabulary)
        output_second = decoder(context, second, vocabulary)
    # Token j consumes only target tokens strictly before j.
    torch.testing.assert_close(output_first[:, :, :3], output_second[:, :, :3])


def test_hierarchical_model_shape_and_gradient() -> None:
    torch.manual_seed(29)
    spec = tiny_spec(GROUP_R1, decoder=True)
    model = build_model(GROUP_R1, **overrides(spec))
    source = torch.randint(0, 32, (2, 8))
    targets = torch.randint(0, 32, (2, 2, 4))
    logits = model.hierarchical_logits(source, targets)
    assert logits.shape == (2, 2, 4, 32)
    F.cross_entropy(logits.flatten(0, 2), targets.flatten()).backward()
    assert all(
        parameter.grad is not None and torch.isfinite(parameter.grad).all()
        for parameter in model.parameters()
    )
