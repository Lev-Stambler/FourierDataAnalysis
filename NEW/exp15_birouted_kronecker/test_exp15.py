from __future__ import annotations

import pytest
import torch

from exp14_block_kronecker.model import build_model as build_exp14

from .campaign import (
    Recipe,
    _activate_base,
    _finite_complete_benchmark,
    _valid_benchmark,
    create_optimizer,
    gpu_cached_block_batch,
)
from .model import (
    BI_DECOUPLED_R8,
    BI_R8,
    CURRENT_R8,
    FFN_ONLY,
    MODEL_NAMES,
    SOURCE_R8,
    build_model,
    model_inventory,
)


SMALL = dict(
    context_length=8,
    vocab_size=32,
    width=8,
    depth=1,
    ffn_width=12,
    group_count=2,
    workspace1=2,
    workspace2=2,
    channel1=2,
    channel2=4,
    rank=2,
)


@pytest.mark.parametrize("name", MODEL_NAMES)
def test_every_variant_has_finite_forward_backward(name: str) -> None:
    model = build_model(name, **SMALL)
    tokens = torch.randint(0, SMALL["vocab_size"], (2, SMALL["context_length"]))
    loss = model(tokens).float().square().mean()
    loss.backward()
    assert torch.isfinite(loss)
    assert all(
        parameter.grad is None or torch.isfinite(parameter.grad).all()
        for parameter in model.parameters()
    )


@pytest.mark.parametrize("name", (CURRENT_R8, SOURCE_R8, BI_R8, BI_DECOUPLED_R8))
def test_future_groups_cannot_change_prior_group(name: str) -> None:
    torch.manual_seed(15)
    model = build_model(name, **SMALL).eval()
    first = torch.randn(1, SMALL["context_length"], SMALL["width"])
    second = first.clone()
    second[:, 4:] = torch.randn_like(second[:, 4:])
    layer = model.blocks[0]
    with torch.no_grad():
        left = layer.branch(first)
        right = layer.branch(second)
    torch.testing.assert_close(left[:, :4], right[:, :4], rtol=1e-5, atol=1e-5)


def test_zero_initialized_router_is_exactly_neutral() -> None:
    torch.manual_seed(15)
    current = build_model(CURRENT_R8, **SMALL)
    bi = build_model(BI_R8, **SMALL)
    current_state = current.state_dict()
    bi.load_state_dict(
        {name: value for name, value in current_state.items() if name in bi.state_dict()},
        strict=False,
    )
    value = torch.randn(2, SMALL["context_length"], SMALL["width"])
    torch.testing.assert_close(
        current.blocks[0].branch(value), bi.blocks[0].branch(value), rtol=0, atol=0
    )


def test_source_router_changes_what_is_sent() -> None:
    torch.manual_seed(15)
    model = build_model(SOURCE_R8, **SMALL)
    layer = model.blocks[0]
    value = torch.randn(2, SMALL["context_length"], SMALL["width"])
    neutral = layer.branch(value)
    with torch.no_grad():
        layer.source_router.weight.normal_(0, 1)
    routed = layer.branch(value)
    assert not torch.allclose(neutral, routed)


def test_ffn_control_is_parameter_matched_but_mixer_inactive() -> None:
    current = model_inventory(build_model(CURRENT_R8, **SMALL))
    control = model_inventory(build_model(FFN_ONLY, **SMALL))
    assert current["total_parameters"] == control["total_parameters"]
    assert current["active_mixer"] is True
    assert control["active_mixer"] is False


def test_decoupling_exposes_rank_cross_product() -> None:
    inventory = model_inventory(build_model(BI_DECOUPLED_R8, **SMALL))
    assert inventory["effective_rank_pairings"] == SMALL["rank"] ** 2


def test_current_variant_exactly_reproduces_exp14_operator() -> None:
    torch.manual_seed(1500)
    old = build_exp14("block-kron-r8", **SMALL)
    torch.manual_seed(1500)
    new = build_model(CURRENT_R8, **SMALL)
    translated = {
        name.replace(".router.", ".destination_router."): value
        for name, value in old.state_dict().items()
    }
    new.load_state_dict(translated)
    tokens = torch.randint(0, SMALL["vocab_size"], (2, SMALL["context_length"]))
    torch.testing.assert_close(old(tokens), new(tokens), rtol=0, atol=0)


@pytest.mark.parametrize("family", ("adamw", "muon", "hybrid"))
def test_optimizer_policies_cover_every_parameter_once(family: str) -> None:
    model = build_model(BI_R8, **SMALL)
    optimizer, inventory = create_optimizer(model, Recipe(family, 0.03, 0.003))
    groups = (
        optimizer.param_groups
        if family == "adamw"
        else [*optimizer.muon.param_groups, *optimizer.auxiliary.param_groups]
    )
    parameters = [parameter for group in groups for parameter in group["params"]]
    assert len({id(parameter) for parameter in parameters}) == len(parameters)
    assert sum(parameter.numel() for parameter in parameters) == sum(
        parameter.numel() for parameter in model.parameters()
    )
    assert inventory["family"] == family


def test_underutilized_stable_batch_advances_upward_search() -> None:
    row = {
        "status": "complete",
        "finite_forward_backward_optimizer": True,
        "gpu_samples": 8,
        "median_gpu_utilization_percent": 61.0,
    }
    assert _finite_complete_benchmark(row)
    assert not _valid_benchmark(row)


def test_cached_batch_cpu_fallback_preserves_block_shift() -> None:
    _activate_base()
    windows = torch.arange(3 * 257).reshape(3, 257).numpy()
    inputs, targets = gpu_cached_block_batch(
        windows, torch.tensor([0]).numpy(), torch.device("cpu")
    )
    assert inputs.shape == targets.shape == (1, 256)
    torch.testing.assert_close(targets, torch.arange(16, 272)[None])
