from __future__ import annotations

from dataclasses import replace

import numpy as np
import pytest
import torch

import exp14_block_kronecker.campaign as base
from exp17_group_density.campaign import (
    MODEL_NAMES,
    TARGET_PARAMETERS,
    Recipe,
    activate_base,
    boundary_extensions,
    build_model as build_campaign_model,
    compile_hidden,
    coarse_recipes,
    create_optimizer,
    model_inventory,
    paired_interval,
    recipe_slug,
    schedule_multiplier,
)
from exp17_group_density.model import GROUP_R1, build_model


def test_every_track_is_parameter_matched() -> None:
    for name in MODEL_NAMES:
        inventory = model_inventory(build_campaign_model(name))
        assert abs(inventory["total_parameters"] / TARGET_PARAMETERS - 1) <= 0.001


def test_coarse_grid_covers_both_optimizer_families() -> None:
    recipes = coarse_recipes()
    assert len(recipes) == 17
    assert sum(recipe.family == "adamw" for recipe in recipes) == 5
    assert sum(recipe.family == "muon" for recipe in recipes) == 12
    assert len({recipe_slug(recipe) for recipe in recipes}) == len(recipes)


def test_muon_and_adamw_route_every_parameter_exactly_once() -> None:
    model = build_model(
        GROUP_R1,
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
        group_rank=1,
        hidden_workspace=5,
        hidden_channel=6,
        activation_checkpointing=False,
    )
    adamw, adamw_routing = create_optimizer(model, Recipe("adamw", 0.01, 0.01))
    assert adamw_routing["parameters"] == sum(p.numel() for p in model.parameters())
    muon, muon_routing = create_optimizer(model, Recipe("muon", 0.02, 0.003))
    assert muon_routing["muon_parameters"] + muon_routing[
        "auxiliary_parameters"
    ] == sum(p.numel() for p in model.parameters())
    names = [
        *muon_routing["muon_parameter_names"],
        *muon_routing["auxiliary_parameter_names"],
    ]
    assert len(names) == len(set(names)) == len(list(model.parameters()))
    del adamw, muon


def test_boundary_extension_expands_only_a_winning_edge() -> None:
    rows = []
    for recipe in coarse_recipes():
        score = 2.0
        if recipe.family == "adamw" and recipe.body_lr == 0.024:
            score = 1.0
        if (
            recipe.family == "muon"
            and recipe.body_lr == 0.12
            and recipe.auxiliary_lr == 0.006
        ):
            score = 0.9
        rows.append({"recipe": recipe.__dict__, "validation": {"nll": score}})
    extensions = boundary_extensions(rows)
    assert Recipe("adamw", 0.048, 0.048) in extensions
    assert Recipe("muon", 0.24, 0.006) in extensions
    assert Recipe("muon", 0.12, 0.012) in extensions

    rows.extend(
        [
            {
                "recipe": Recipe("adamw", 0.048, 0.048).__dict__,
                "validation": {"nll": 0.8},
            },
            {
                "recipe": Recipe("muon", 0.24, 0.006).__dict__,
                "validation": {"nll": 0.7},
            },
        ]
    )
    next_extensions = boundary_extensions(rows)
    assert Recipe("adamw", 0.096, 0.096) in next_extensions
    assert Recipe("muon", 0.48, 0.006) in next_extensions


def test_schedule_and_paired_interval_are_deterministic() -> None:
    recipe = replace(
        Recipe("adamw", 0.01, 0.01),
        schedule="warmup-cosine",
        warmup_tokens=100,
        horizon_tokens=1000,
    )
    assert schedule_multiplier(recipe, 50) == pytest.approx(0.5)
    assert schedule_multiplier(recipe, 1000) == pytest.approx(0.1)
    first = paired_interval([-0.03, -0.02, -0.04, -0.025])
    second = paired_interval([-0.03, -0.02, -0.04, -0.025])
    assert first == second
    assert first["upper_95"] < 0


def test_base_batch_cpu_fallback_is_bound() -> None:
    activate_base()
    assert base.compile_hidden is compile_hidden
    windows = np.arange(5 * 257, dtype=np.int64).reshape(5, 257)
    inputs, targets = base.block_batch(windows, np.array([0, 1]), torch.device("cpu"))
    assert inputs.shape == targets.shape == (2, 256)
    np.testing.assert_array_equal(targets[:, 0].numpy(), windows[[0, 1], 16])
