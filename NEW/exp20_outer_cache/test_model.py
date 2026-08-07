from __future__ import annotations

from dataclasses import replace

import pytest
import torch

from exp20_outer_cache.model import (
    DENSE_R2_PACKED,
    DENSE_R8_REFERENCE,
    RECURRENT3_R8_PACKED,
    CachedLanguageModel,
    CachedStructuredMixer,
    DenseCausalOuter,
    ModelSpec,
    SemiseparableOuter,
    build_model,
    model_inventory,
)


def tiny_spec(name: str, **overrides: object) -> ModelSpec:
    values = dict(
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
        hidden_workspace=5,
        hidden_channel=6,
        activation_checkpointing=False,
    )
    values.update(overrides)
    return replace(ModelSpec(name), **values)


def test_packed_inner_matches_reference_forward_and_backward() -> None:
    torch.manual_seed(20)
    reference = CachedStructuredMixer(
        tiny_spec(DENSE_R8_REFERENCE, mixer_backend="reference"), 1
    ).double()
    packed = CachedStructuredMixer(
        tiny_spec(DENSE_R2_PACKED, mixer_backend="packed-gemm"), 1
    ).double()
    packed.load_state_dict(reference.state_dict())
    left = torch.randn(2, 8, 8, dtype=torch.float64, requires_grad=True)
    right = left.detach().clone().requires_grad_(True)
    expected = reference(left)
    actual = packed(right)
    torch.testing.assert_close(actual, expected, rtol=1e-10, atol=1e-10)
    probe = torch.randn_like(actual)
    expected_gradients = torch.autograd.grad(
        expected, [left, *reference.parameters()], probe
    )
    actual_gradients = torch.autograd.grad(actual, [right, *packed.parameters()], probe)
    for result, target in zip(actual_gradients, expected_gradients, strict=True):
        torch.testing.assert_close(result, target, rtol=1e-9, atol=1e-9)


def test_reference_model_matches_frozen_exp19_clean_token_model() -> None:
    from exp19_norm_residual.model import (
        CLEAN_GROUP_R1_TOKEN,
        build_model as build_exp19,
    )

    torch.manual_seed(23)
    spec = tiny_spec(DENSE_R8_REFERENCE, mixer_backend="reference")
    exp20 = CachedLanguageModel(spec).double().eval()
    exp19 = build_exp19(
        CLEAN_GROUP_R1_TOKEN,
        context_length=spec.context_length,
        vocab_size=spec.vocab_size,
        width=spec.width,
        depth=spec.depth,
        group_size=spec.group_size,
        workspace1=spec.workspace1,
        workspace2=spec.workspace2,
        channel1=spec.channel1,
        channel2=spec.channel2,
        mixer_rank=spec.mixer_rank,
        hidden_workspace=spec.hidden_workspace,
        hidden_channel=spec.hidden_channel,
        activation_checkpointing=False,
    ).double().eval()
    source = exp19.state_dict()
    mapped = {}
    for key in exp20.state_dict():
        source_key = key.replace(".mixer.outer.factor.raw", ".mixer.outer.raw")
        mapped[key] = source[source_key]
    exp20.load_state_dict(mapped)
    tokens = torch.randint(0, spec.vocab_size, (2, spec.context_length))
    torch.testing.assert_close(exp20(tokens), exp19(tokens), rtol=1e-10, atol=1e-10)


def test_semiseparable_scan_matches_materialized_forward_and_backward() -> None:
    torch.manual_seed(21)
    outer = SemiseparableOuter(2, 5, 3).double()
    left = torch.randn(2, 2, 5, 4, 6, dtype=torch.float64, requires_grad=True)
    right = left.detach().clone().requires_grad_(True)
    expected = outer.apply_materialized(left)
    actual = outer.apply(right)
    torch.testing.assert_close(actual, expected, rtol=1e-9, atol=1e-9)
    probe = torch.randn_like(actual)
    expected_gradients = torch.autograd.grad(
        expected, [left, *outer.parameters()], probe, retain_graph=True
    )
    actual_gradients = torch.autograd.grad(actual, [right, *outer.parameters()], probe)
    for result, target in zip(actual_gradients, expected_gradients, strict=True):
        torch.testing.assert_close(result, target, rtol=1e-8, atol=1e-8)


def test_semiseparable_row_norm_stays_finite_near_component_cancellation() -> None:
    outer = SemiseparableOuter(1, 4, 3).float()
    with torch.no_grad():
        outer.input_scale.copy_(
            torch.tensor(
                [[[1.0, 1.0, 1.0], [1.0, -1.0, 1.0],
                  [1.0, 1.0, -1.0], [-1.0, 1.0, 1.0]]]
            )
        )
        outer.output_scale.copy_(
            torch.tensor(
                [[[1.0, -1.0, 1.0e-7], [1.0, 1.0, 1.0e-7],
                  [1.0, -1.0, 1.0e-7], [1.0, 1.0, 1.0e-7]]]
            )
        )
    value = torch.randn(2, 1, 4, 3, 5, requires_grad=True)
    output = outer.apply(value)
    gradients = torch.autograd.grad(
        output.square().mean(), [value, *outer.parameters()]
    )
    assert torch.isfinite(output).all()
    assert torch.isfinite(outer.matrix()).all()
    assert all(torch.isfinite(gradient).all() for gradient in gradients)


@pytest.mark.parametrize(
    ("name", "outer_kind"),
    [
        (DENSE_R2_PACKED, "dense-history"),
        (RECURRENT3_R8_PACKED, "semiseparable-3"),
    ],
)
def test_cached_blocks_match_full_forward(name: str, outer_kind: str) -> None:
    torch.manual_seed(22)
    spec = tiny_spec(
        name,
        outer_kind=outer_kind,
        mixer_backend="packed-gemm",
    )
    model = CachedLanguageModel(spec).double().eval()
    tokens = torch.randint(0, spec.vocab_size, (2, spec.context_length))
    with torch.inference_mode():
        expected = model(tokens)
        actual, cache = model.prefill_blocks(tokens)
    torch.testing.assert_close(actual, expected, rtol=1e-9, atol=1e-9)
    assert cache.position == spec.group_count


def test_cache_rejects_gradient_use_and_overflow() -> None:
    spec = tiny_spec(DENSE_R2_PACKED, mixer_backend="packed-gemm")
    model = CachedLanguageModel(spec).eval()
    cache = model.init_block_cache(1)
    group = torch.randint(0, spec.vocab_size, (1, spec.group_size))
    with pytest.raises(RuntimeError, match="inference-only"):
        model.forward_block(group, cache)
    with torch.inference_mode():
        for _ in range(spec.group_count):
            _, cache = model.forward_block(group, cache)
        with pytest.raises(ValueError, match="full"):
            model.forward_block(group, cache)


@pytest.mark.parametrize("outer", [DenseCausalOuter(2, 4), SemiseparableOuter(2, 4, 3)])
def test_outer_is_causal_and_rows_are_normalized(outer: torch.nn.Module) -> None:
    matrix = outer.matrix()
    assert torch.count_nonzero(torch.triu(matrix, diagonal=1)) == 0
    torch.testing.assert_close(
        matrix.float().square().sum(-1), torch.ones(2, 4), rtol=1e-5, atol=1e-5
    )


def test_rank_sweep_is_parameter_matched() -> None:
    from exp20_outer_cache.model import (
        DENSE_R1_PACKED,
        DENSE_R4_PACKED,
        DENSE_R8_PACKED,
        MODEL_NAMES,
        TRANSFORMER_DEEP,
    )

    target = model_inventory(build_model(DENSE_R8_PACKED))["total_parameters"]
    for name in (DENSE_R1_PACKED, DENSE_R2_PACKED, DENSE_R4_PACKED, RECURRENT3_R8_PACKED):
        total = model_inventory(build_model(name))["total_parameters"]
        assert abs(total - target) / target <= 0.001, (name, total, target)
    assert TRANSFORMER_DEEP in MODEL_NAMES
