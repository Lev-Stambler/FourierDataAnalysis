from __future__ import annotations

import json
from dataclasses import asdict

import pytest
import torch

from exp12_deep_kronecker.campaign import create_optimizer, split_muon_parameters
from exp12_deep_kronecker.gates import local_correctness, maximum_prefix_error
from exp12_deep_kronecker.model import (
    DeepKroneckerBlock,
    SharedCausalBasis,
    build_model,
    channel_layout,
    model_inventory,
)
from exp12_deep_kronecker.study import (
    COARSE_SEEDS,
    OptimizerRecipe,
    boundary_extension,
    coarse_recipes,
    finalist_recipes,
    promotion_decision,
    recipe_slug,
    robust_recipes,
    schedule_multiplier,
    token_budget,
)


def test_factored_content_routed_branch_matches_dense_reference() -> None:
    torch.manual_seed(12)
    basis = SharedCausalBasis(2, 5).double()
    block = DeepKroneckerBlock(2, 3, 2, 8, 4, 3, 0).double()
    value = torch.randn(2, 5, 6, dtype=torch.float64, requires_grad=True)
    positions = basis.matrix()
    factored = block.branch(value, positions)
    materialized = block.materialized_branch(value, positions)
    assert torch.allclose(factored, materialized, atol=1e-10, rtol=1e-10)
    differentiated = (
        value,
        basis.raw,
        block.channel1.raw,
        block.channel2.raw,
        block.rank_amplitudes,
        block.router.weight,
    )
    left = torch.autograd.grad(
        factored.square().sum(), differentiated, retain_graph=True
    )
    right = torch.autograd.grad(materialized.square().sum(), differentiated)
    assert all(
        torch.allclose(a, b, atol=1e-9, rtol=1e-9)
        for a, b in zip(left, right, strict=True)
    )


def test_shared_basis_is_causal_and_row_balanced() -> None:
    torch.manual_seed(13)
    matrix = SharedCausalBasis(3, 16).double().matrix()
    assert not torch.count_nonzero(torch.triu(matrix, diagonal=1))
    assert torch.allclose(
        matrix.square().sum(-1),
        torch.ones(3, 16, dtype=torch.float64),
        atol=1e-7,
    )


def test_both_models_are_prefix_invariant() -> None:
    assert maximum_prefix_error("deep-kron-r8") <= 1e-6
    assert maximum_prefix_error("transformer") <= 1e-6


def test_default_models_are_deep_and_parameter_matched() -> None:
    deep = model_inventory(build_model("deep-kron-r8"))
    control = model_inventory(build_model("transformer"))
    ratio = deep["body_parameters"] / control["body_parameters"]
    assert deep["config"]["depth"] == control["config"]["depth"] == 32
    assert deep["nonlinear_residual_updates"] == 64
    assert 0.9 <= ratio <= 1.0
    assert deep["causal_basis_parameters"] / deep["body_parameters"] <= 0.3


def test_basis_reuse_routing_and_layout_changes_are_real() -> None:
    model = build_model("deep-kron-r8")
    assert len(model.position_banks) == 4
    assert {block.basis_index for block in model.blocks} == {0, 1, 2, 3}
    layouts = {tuple(block.permutation.tolist()) for block in model.blocks}
    assert len(layouts) >= 16
    for layer in range(8):
        permutation = channel_layout(128, layer)
        assert torch.equal(torch.sort(permutation).values, torch.arange(128))
    block = model.blocks[0]
    value = torch.randn(2, 256, 128)
    assert torch.equal(block.routing_weights(value), torch.ones(2, 256, 8))
    with torch.no_grad():
        block.router.weight.normal_(0.0, 0.02)
    assert float(block.routing_weights(value).std().detach()) > 1e-3


def test_all_parameters_receive_finite_active_gradients() -> None:
    torch.manual_seed(14)
    model = build_model(
        "deep-kron-r8",
        context_length=6,
        vocab_size=12,
        width=12,
        depth=3,
        mode1=3,
        mode2=4,
        rank=2,
        basis_banks=2,
        ffn_width=24,
    )
    loss = model(torch.randint(0, 12, (4, 6))).square().mean()
    loss.backward()
    assert all(
        parameter.grad is not None
        and torch.isfinite(parameter.grad).all()
        and float(parameter.grad.norm()) > 0.0
        for parameter in model.parameters()
    )


def test_full_local_gate_passes_and_persists(tmp_path) -> None:
    path = tmp_path / "local-correctness.json"
    result = local_correctness(path)
    assert result["status"] == "pass"
    assert json.loads(path.read_text()) == result


def _rows(recipes, seeds, base):
    return [
        {
            "status": "complete",
            "recipe_slug": recipe_slug(recipe),
            "recipe": asdict(recipe),
            "seed": seed,
            "validation": {"nll": base + index * 0.1 + seed * 0.01},
        }
        for index, recipe in enumerate(recipes)
        for seed in seeds
    ]


def test_successive_halving_keeps_both_optimizer_families() -> None:
    coarse = coarse_recipes("deep-kron-r8")
    assert len(coarse) == 8
    robust = robust_recipes(_rows(coarse, COARSE_SEEDS, 4.0))
    assert {recipe.family for recipe in robust} == {"adamw", "muon"}
    assert {recipe.schedule for recipe in robust} == {
        "constant",
        "warmup-cosine",
    }
    assert len(robust) == 10
    finalists = finalist_recipes(_rows(robust, (0, 1), 4.0))
    assert len(finalists) == 2


def test_schedule_boundary_promotion_and_budget_are_locked() -> None:
    recipe = OptimizerRecipe("muon", 0.3, 0.003, "warmup-cosine")
    assert schedule_multiplier(recipe, 1_000_000) == 0.5
    assert schedule_multiplier(recipe, 2_000_000) == 1.0
    extended = boundary_extension(
        OptimizerRecipe("muon", 0.3, 0.003),
        [OptimizerRecipe("muon", lr, 0.003) for lr in (0.01, 0.03, 0.1, 0.3)],
    )
    assert extended is not None and extended.body_lr == pytest.approx(0.9)
    deep = {"validation_nll_by_seed": {0: 4.0, 1: 4.02, 2: 4.01}}
    control = {"validation_nll_by_seed": {0: 4.01, 1: 4.0, 2: 4.02}}
    assert promotion_decision(
        deep, control, body_parameter_ratio=0.968, throughput_ratio=0.6
    )["status"] == "pass"
    budget = token_budget()
    assert budget["pre_promotion_tokens"] == 232_000_000
    assert budget["maximum_total_tokens"] == 412_000_000


def test_optimizer_routing_is_semantic_complete_and_disjoint() -> None:
    for variant in ("deep-kron-r8", "transformer"):
        model = build_model(variant)
        muon, auxiliary, routing = split_muon_parameters(model)
        assert len({id(parameter) for parameter in [*muon, *auxiliary]}) == len(
            [*muon, *auxiliary]
        )
        assert routing["muon_parameters"] + routing["auxiliary_parameters"] == sum(
            parameter.numel() for parameter in model.parameters()
        )
        assert "vocabulary" in routing["auxiliary_parameter_names"]
        assert all(
            not name.startswith("position_banks.")
            for name in routing["muon_parameter_names"]
        )
        if variant == "deep-kron-r8":
            assert any(
                name.startswith("position_banks.")
                for name in routing["auxiliary_parameter_names"]
            )
            assert any("router.weight" in name for name in routing["muon_parameter_names"])
        recipe = OptimizerRecipe("muon", 0.03, 0.003)
        optimizer, created_routing = create_optimizer(model, recipe)
        assert optimizer.muon.param_groups[0]["lr"] == pytest.approx(0.03)
        assert created_routing == {"family": "muon", **routing}
