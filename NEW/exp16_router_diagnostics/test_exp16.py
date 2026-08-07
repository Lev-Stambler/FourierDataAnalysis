from __future__ import annotations

import torch

from exp15_birouted_kronecker.model import BI_DECOUPLED_R8, build_model

from .campaign import (
    BASE_LR,
    TRACKS,
    Track,
    create_optimizer,
    is_router_parameter,
    parameter_categories,
    set_learning_rates,
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


def test_tracks_fill_node_and_respect_token_floor() -> None:
    assert len(TRACKS) == 8
    assert len({track.name for track in TRACKS}) == 8
    assert all(track.batch * 256 >= 100_000 for track in TRACKS)


def test_router_optimizer_covers_parameters_and_applies_multiplier() -> None:
    model = build_model(BI_DECOUPLED_R8, **SMALL)
    track = Track("test", BI_DECOUPLED_R8, 10.0, 512)
    optimizer, inventory = create_optimizer(model, track)
    grouped = [item for group in optimizer.param_groups for item in group["params"]]
    assert len({id(item) for item in grouped}) == len(grouped)
    assert {id(item) for item in grouped} == {id(item) for item in model.parameters()}
    body_lr, router_lr = set_learning_rates(optimizer, track, 2_000_000, 40_000_000)
    assert body_lr == BASE_LR
    assert router_lr == 10 * BASE_LR
    assert all(
        group["lr"] == (router_lr if group["role"] == "router" else body_lr)
        for group in optimizer.param_groups
    )
    assert inventory["router_parameters"] > 0


def test_frozen_router_track_has_zero_router_lr() -> None:
    model = build_model(BI_DECOUPLED_R8, **SMALL)
    track = Track("test", BI_DECOUPLED_R8, 0.0, 512)
    optimizer, _ = create_optimizer(model, track)
    _, router_lr = set_learning_rates(optimizer, track, 1_000_000, 40_000_000)
    assert router_lr == 0
    assert all(group["lr"] == 0 for group in optimizer.param_groups if group["role"] == "router")


def test_parameter_categories_are_complete() -> None:
    model = build_model(BI_DECOUPLED_R8, **SMALL)
    categories = parameter_categories(model)
    categorized = [item for values in categories.values() for item in values]
    assert {id(item) for item in categorized} == {id(item) for item in model.parameters()}
    router_ids = {id(parameter) for parameter in categories["router"]}
    router_names = [
        name for name, parameter in model.named_parameters() if id(parameter) in router_ids
    ]
    assert router_names
    assert all(is_router_parameter(name) for name in router_names)
