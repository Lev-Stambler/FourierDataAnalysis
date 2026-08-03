"""Zero-paid-compute correctness gates for the canonical operator."""

from __future__ import annotations

import json
from pathlib import Path

import torch
import torch.nn.functional as F

from .diagnostics import future_jacobian_max, position_balance_ratio, rank_utilization
from .model import CanonicalOrder3Block, Exp10ReplicaBlock, LanguageModel, ModelConfig


def local_correctness(output: str | Path) -> dict:
    torch.manual_seed(11)
    reference_block = CanonicalOrder3Block(2, 2, 2, 4, 1).double()
    reference_input = torch.randn(
        1, 4, 4, dtype=torch.float64, requires_grad=True
    )
    factored = reference_block.branch(reference_input)
    materialized = reference_block.materialized_branch(reference_input)
    forward_error = float((factored - materialized).abs().max().detach())
    factored_gradients = torch.autograd.grad(
        factored.square().sum(),
        (reference_input, *reference_block.parameters()),
        retain_graph=True,
    )
    materialized_gradients = torch.autograd.grad(
        materialized.square().sum(),
        (reference_input, *reference_block.parameters()),
    )
    backward_error = max(
        float((left - right).abs().max().detach())
        for left, right in zip(
            factored_gradients, materialized_gradients, strict=True
        )
    )
    future_error = future_jacobian_max(reference_block, reference_input)

    batch = torch.randn(128, 64, 32)
    replica = Exp10ReplicaBlock(32, 4, 64, 4)
    corrected = CanonicalOrder3Block(4, 8, 4, 64, 4)
    replica_balance = position_balance_ratio(replica, batch)
    corrected_balance = position_balance_ratio(corrected, batch)
    utilization = rank_utilization(corrected, batch)

    config = ModelConfig(
        "order3-r4",
        context_length=8,
        vocab_size=16,
        width=16,
        depth=2,
        mode1=4,
        mode2=4,
        rank=2,
    )
    model = LanguageModel(config)
    values = torch.randint(0, config.vocab_size, (8, config.context_length))
    optimizer = torch.optim.AdamW(model.parameters(), lr=0.03, weight_decay=0.0)
    loss = torch.tensor(float("inf"))
    for _ in range(250):
        optimizer.zero_grad(set_to_none=True)
        loss = F.cross_entropy(
            model(values).flatten(0, 1), values.flatten()
        )
        loss.backward()
        optimizer.step()
    gradients_active = all(
        parameter.grad is not None
        and torch.isfinite(parameter.grad).all()
        and bool(torch.count_nonzero(parameter.grad))
        for parameter in model.parameters()
    )
    checks = {
        "forward_reference_error": forward_error,
        "backward_reference_error": backward_error,
        "future_jacobian_max": future_error,
        "exp10_replica_position_balance_ratio": replica_balance,
        "canonical_position_balance_ratio": corrected_balance,
        "effective_rank_fraction": utilization["effective_rank_fraction"],
        "identity_overfit_nll": float(loss.detach()),
        "finite_active_gradients": gradients_active,
    }
    passed = (
        forward_error <= 1e-8
        and backward_error <= 1e-8
        and future_error <= 1e-12
        and replica_balance >= 4.0
        and corrected_balance <= 2.0
        and utilization["effective_rank_fraction"] >= 0.5
        and float(loss.detach()) < 1e-3
        and gradients_active
    )
    result = {
        "schema": "exp11-local-correctness-v1",
        "status": "pass" if passed else "failed",
        "checks": checks,
    }
    path = Path(output)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(result, indent=2, sort_keys=True))
    return result
