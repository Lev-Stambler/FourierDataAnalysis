from __future__ import annotations

import json
from dataclasses import asdict

import numpy as np
import pytest
import torch

from exp11_kronecker_debug.diagnostics import future_jacobian_max
from exp11_kronecker_debug.gates import local_correctness
from exp11_kronecker_debug.lm import batch_indices, bootstrap_interval, projected_training_seconds
from exp11_kronecker_debug.model import (
    CanonicalOrder3Block,
    PackedCausalRows,
    build_model,
    model_inventory,
)
from exp11_kronecker_debug.muon_tuning import (
    MuonRecipe,
    boundary_extension as muon_boundary_extension,
    create_optimizer as create_muon_optimizer,
    muon_correctness,
    optimizer_ablations,
    schedule_multiplier,
    split_muon_parameters,
)
from exp11_kronecker_debug.synthetic import associative_recall_batch, delayed_copy_batch
from exp11_kronecker_debug.tuning import (
    OptimizerRecipe,
    boundary_extension,
    learning_rate_at_tokens,
    summarize_recipes,
)


def test_canonical_factored_operator_matches_materialized_forward_and_backward() -> None:
    torch.manual_seed(4)
    block = CanonicalOrder3Block(2, 3, 2, 4, 1).double()
    value = torch.randn(2, 4, 6, dtype=torch.float64, requires_grad=True)
    factored = block.branch(value)
    dense = block.materialized_branch(value)
    assert torch.allclose(factored, dense, atol=1e-10, rtol=1e-10)
    factored_grad = torch.autograd.grad(factored.square().sum(), (value, *block.parameters()), retain_graph=True)
    dense_grad = torch.autograd.grad(dense.square().sum(), (value, *block.parameters()))
    assert all(
        torch.allclose(left, right, atol=1e-9, rtol=1e-9)
        for left, right in zip(factored_grad, dense_grad, strict=True)
    )


def test_canonical_operator_is_causal_and_row_balanced() -> None:
    torch.manual_seed(5)
    block = CanonicalOrder3Block(2, 2, 3, 6, 1).double()
    value = torch.randn(1, 6, 4, dtype=torch.float64)
    assert future_jacobian_max(block, value) == 0.0
    matrix = block.position.matrix()
    assert not torch.count_nonzero(torch.triu(matrix, diagonal=1))
    row_energy = matrix.square().sum(-1)
    assert torch.allclose(row_energy, torch.ones_like(row_energy), atol=1e-7)


def test_language_models_are_prefix_invariant_and_body_matched() -> None:
    torch.manual_seed(6)
    for variant in ("order3-r4", "transformer"):
        model = build_model(
            variant,
            context_length=8,
            vocab_size=32,
            width=16,
            depth=2,
            mode1=4,
            mode2=4,
            heads=4,
            mlp_width=32,
        ).eval()
        first = torch.randint(0, 32, (1, 8))
        second = first.clone()
        second[:, 5:] = torch.randint(0, 32, (1, 3))
        assert torch.allclose(model(first)[:, :5], model(second)[:, :5], atol=1e-6)
    kron = model_inventory(build_model("order3-r4"))
    transformer = model_inventory(build_model("transformer"))
    assert kron["body_parameters"] / transformer["body_parameters"] <= 0.5
    assert kron["vocabulary_parameters"] == transformer["vocabulary_parameters"]


def test_packed_rows_reproduce_bug_or_remove_it() -> None:
    torch.manual_seed(7)
    balanced = PackedCausalRows(2, 32, balanced=True).matrix().square().sum(-1)
    flawed = PackedCausalRows(2, 32, balanced=False).matrix().square().sum(-1)
    assert torch.allclose(balanced, torch.ones_like(balanced), atol=1e-6)
    assert float((flawed[:, -1].mean() / flawed[:, 0].mean()).detach()) > 8.0


def test_synthetic_batches_target_only_the_intended_positions() -> None:
    delayed = delayed_copy_batch(3, 8, 16, 3, seed=2)
    assert delayed.mask.sum().item() == 15
    assert torch.equal(delayed.targets[:, 3:], delayed.inputs[:, :-3])
    recall = associative_recall_batch(4, 3, 8, 8, seed=2)
    assert recall.inputs.shape == (4, 7)
    assert recall.mask.sum().item() == 4
    assert torch.all(recall.targets[recall.mask] >= 8)


def test_pilot_accounting_helpers_are_deterministic() -> None:
    first = batch_indices(101, 3, 8, 19)
    second = batch_indices(101, 3, 8, 19)
    assert np.array_equal(first, second)
    assert len(np.unique(first)) == len(first)
    interval = bootstrap_interval([-0.1, 0.0, 0.1], samples=1000)
    assert interval[0] <= 0 <= interval[1]
    selected = {variant: {"tokens_per_second": 100_000.0} for variant in (
        "exp10-replica", "order2-balanced", "order3-r4", "order3-r8", "transformer"
    )}
    assert projected_training_seconds({"selected": selected}) == pytest.approx(2900.0)


def test_full_local_gate_passes_and_persists(tmp_path) -> None:
    path = tmp_path / "gate.json"
    result = local_correctness(path)
    assert result["status"] == "pass"
    assert json.loads(path.read_text()) == result


def test_token_schedule_warms_and_decays() -> None:
    recipe = OptimizerRecipe(peak_lr=0.1, schedule="warmup-cosine")
    assert learning_rate_at_tokens(recipe, 1_000_000) == pytest.approx(0.05)
    assert learning_rate_at_tokens(recipe, 2_000_000) == pytest.approx(0.1)
    assert learning_rate_at_tokens(recipe, 40_000_000) == pytest.approx(0.01)
    constant = OptimizerRecipe(peak_lr=0.03, schedule="constant")
    assert learning_rate_at_tokens(constant, 1) == pytest.approx(0.03)
    assert learning_rate_at_tokens(constant, 40_000_000) == pytest.approx(0.03)


def test_tuning_selection_uses_all_seeds_and_expands_boundaries() -> None:
    first = OptimizerRecipe(peak_lr=0.01, schedule="constant")
    second = OptimizerRecipe(peak_lr=0.02, schedule="constant")
    rows = []
    for recipe, values in ((first, (4.0, 4.2, 4.1)), (second, (3.8, 3.9, 4.0))):
        for seed, nll in enumerate(values):
            rows.append(
                {
                    "status": "complete",
                    "recipe_slug": str(recipe.peak_lr),
                    "recipe": asdict(recipe),
                    "seed": seed,
                    "validation": {"nll": nll},
                }
            )
    summaries = summarize_recipes(rows)
    assert summaries[0]["recipe"]["peak_lr"] == 0.02
    extension = boundary_extension(second, [first, second])
    assert extension is not None
    assert extension.peak_lr == pytest.approx(0.04)


def test_muon_matches_torch_and_routes_every_parameter(tmp_path) -> None:
    result = muon_correctness(tmp_path / "muon.json")
    assert result["status"] == "pass"
    assert result["batched_vs_torch_muon_max_difference"] <= 0.003
    for variant in ("order3-r8", "transformer"):
        routing = result["routing"][variant]
        assert routing["muon_parameters"] > 0
        assert routing["auxiliary_parameters"] > 0


def test_muon_routing_keeps_vocabulary_and_vectors_on_adamw() -> None:
    for variant in ("order3-r8", "transformer"):
        model = build_model(variant)
        muon, auxiliary, routing = split_muon_parameters(model)
        assert len(muon) == routing["muon_tensor_count"]
        assert len(auxiliary) == routing["auxiliary_tensor_count"]
        assert "vocabulary" in routing["auxiliary_parameter_names"]
        assert all(name.startswith("blocks.") for name in routing["muon_parameter_names"])
        optimizer, _ = create_muon_optimizer(
            model, MuonRecipe(0.01, 0.001, "constant")
        )
        assert optimizer.muon.param_groups[0]["lr"] == pytest.approx(0.01)


def test_muon_schedule_boundary_and_ablations() -> None:
    recipe = MuonRecipe(0.03, 0.0015, "warmup-cosine")
    assert schedule_multiplier(recipe, 1_000_000) == pytest.approx(0.5)
    assert schedule_multiplier(recipe, 2_000_000) == pytest.approx(1.0)
    assert schedule_multiplier(recipe, 40_000_000) == pytest.approx(0.1)
    lower = MuonRecipe(0.003, 0.0015, "constant")
    upper = MuonRecipe(0.01, 0.0015, "constant")
    extension = muon_boundary_extension(upper, [lower, upper])
    assert extension is not None
    assert extension.muon_lr == pytest.approx(0.03)
    ablations = optimizer_ablations(recipe)
    assert {item.auxiliary_lr for item in ablations} >= {0.00075, 0.003}
    assert any(item.weight_decay == 0 for item in ablations)
    assert any(item.momentum == 0.9 for item in ablations)
