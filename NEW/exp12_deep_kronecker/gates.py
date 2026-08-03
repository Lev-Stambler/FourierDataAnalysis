"""Local falsification gates for the deep Kronecker successor."""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any

import torch
import torch.nn.functional as F

from exp11_kronecker_debug.model import build_model as build_exp11_model

from .model import DeepKroneckerBlock, SharedCausalBasis, build_model, model_inventory


def write_json(path: Path, value: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(path.name + ".tmp")
    temporary.write_text(json.dumps(value, indent=2, sort_keys=True))
    temporary.replace(path)


def reference_errors() -> tuple[float, float]:
    torch.manual_seed(1201)
    basis = SharedCausalBasis(2, 5).double()
    block = DeepKroneckerBlock(2, 3, 2, 8, 4, 3, 0).double()
    value = torch.randn(2, 5, 6, dtype=torch.float64, requires_grad=True)
    positions = basis.matrix()
    factored = block.branch(value, positions)
    materialized = block.materialized_branch(value, positions)
    forward_error = float((factored - materialized).abs().max().detach())
    differentiated = (
        value,
        basis.raw,
        block.channel1.raw,
        block.channel2.raw,
        block.rank_amplitudes,
        block.router.weight,
    )
    factored_gradients = torch.autograd.grad(
        factored.square().sum(), differentiated, retain_graph=True
    )
    materialized_gradients = torch.autograd.grad(
        materialized.square().sum(), differentiated
    )
    backward_error = max(
        float((left - right).abs().max().detach())
        for left, right in zip(
            factored_gradients, materialized_gradients, strict=True
        )
    )
    return forward_error, backward_error


def maximum_prefix_error(variant: str) -> float:
    torch.manual_seed(1202)
    model = build_model(
        variant,
        context_length=8,
        vocab_size=32,
        width=16,
        depth=4,
        mode1=4,
        mode2=4,
        rank=2,
        basis_banks=2,
        heads=4,
        ffn_width=32,
    ).eval()
    first = torch.randint(0, 32, (2, 8))
    second = first.clone()
    second[:, 5:] = torch.randint(0, 32, (2, 3))
    with torch.no_grad():
        return float((model(first)[:, :5] - model(second)[:, :5]).abs().max())


def tiny_overfit() -> tuple[float, float, bool]:
    torch.manual_seed(1203)
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
    inputs = torch.randint(0, 12, (8, 6))
    targets = torch.randint(0, 12, (8, 6))
    optimizer = torch.optim.AdamW(
        model.parameters(), lr=0.03, betas=(0.9, 0.95), weight_decay=0.0
    )
    initial = 0.0
    gradients_active = False
    for step in range(101):
        optimizer.zero_grad(set_to_none=True)
        loss = F.cross_entropy(model(inputs).flatten(0, 1), targets.flatten())
        if step == 0:
            initial = float(loss.detach())
        loss.backward()
        if step == 0:
            gradients_active = all(
                parameter.grad is not None
                and torch.isfinite(parameter.grad).all()
                and float(parameter.grad.norm()) > 0.0
                for parameter in model.parameters()
            )
        optimizer.step()
    return initial, float(loss.detach()), gradients_active


def local_correctness(output: str | Path | None = None) -> dict[str, Any]:
    forward_error, backward_error = reference_errors()
    torch.manual_seed(1204)
    basis = SharedCausalBasis(3, 16)
    matrix = basis.matrix()
    row_energy_error = float(
        (matrix.square().sum(-1) - 1.0).abs().max().detach()
    )
    future_nonzeros = int(torch.count_nonzero(torch.triu(matrix, diagonal=1)))

    deep = build_model("deep-kron-r8")
    control = build_model("transformer")
    deep_inventory = model_inventory(deep)
    control_inventory = model_inventory(control)
    old = build_exp11_model("order3-r8")
    old_position_parameters = sum(
        block.position.raw.numel() for block in old.blocks
    )
    old_body_parameters = sum(parameter.numel() for parameter in old.blocks.parameters())

    first_block = deep.blocks[0]
    neutral_router_error = float(
        (first_block.routing_weights(torch.randn(2, 256, 128)) - 1.0)
        .abs()
        .max()
        .detach()
    )
    with torch.no_grad():
        first_block.router.weight.normal_(0.0, 0.02)
    routed = first_block.routing_weights(torch.randn(2, 256, 128))
    content_router_std = float(routed.std().detach())
    distinct_layouts = len(
        {tuple(block.permutation.tolist()) for block in deep.blocks}
    )

    initial_nll, final_nll, gradients_active = tiny_overfit()
    checks = {
        "factored_materialized_forward_max_error": forward_error,
        "factored_materialized_backward_max_error": backward_error,
        "causal_basis_future_nonzeros": future_nonzeros,
        "causal_basis_row_energy_max_error": row_energy_error,
        "deep_kronecker_prefix_max_error": maximum_prefix_error("deep-kron-r8"),
        "transformer_prefix_max_error": maximum_prefix_error("transformer"),
        "depth": deep.config.depth,
        "nonlinear_residual_updates": deep_inventory["nonlinear_residual_updates"],
        "basis_banks": deep.config.basis_banks,
        "distinct_channel_layouts": distinct_layouts,
        "body_parameter_ratio_to_transformer": (
            deep_inventory["body_parameters"] / control_inventory["body_parameters"]
        ),
        "old_position_parameter_fraction": old_position_parameters
        / old_body_parameters,
        "new_shared_basis_parameter_fraction": (
            deep_inventory["causal_basis_parameters"]
            / deep_inventory["body_parameters"]
        ),
        "neutral_router_max_error_from_one": neutral_router_error,
        "content_router_output_std": content_router_std,
        "tiny_overfit_initial_nll": initial_nll,
        "tiny_overfit_final_nll": final_nll,
        "all_gradients_finite_and_active": gradients_active,
        "deep_kronecker_inventory": deep_inventory,
        "transformer_inventory": control_inventory,
    }
    passed = (
        forward_error <= 1e-8
        and backward_error <= 1e-8
        and future_nonzeros == 0
        and row_energy_error <= 1e-6
        and checks["deep_kronecker_prefix_max_error"] <= 1e-6
        and checks["transformer_prefix_max_error"] <= 1e-6
        and deep.config.depth >= 32
        and distinct_layouts >= 16
        and 0.9 <= checks["body_parameter_ratio_to_transformer"] <= 1.0
        and checks["new_shared_basis_parameter_fraction"] <= 0.3
        and neutral_router_error == 0.0
        and content_router_std >= 1e-3
        and final_nll <= 0.1
        and gradients_active
    )
    result = {
        "schema": "exp12-local-correctness-v1",
        "status": "pass" if passed else "fail",
        "checks": checks,
    }
    if output is not None:
        write_json(Path(output), result)
    if not passed:
        raise RuntimeError(f"Exp12 local correctness failed: {result}")
    return result
