"""Mechanistic diagnostics and local correctness gates."""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any

import torch
import torch.nn.functional as F

from .config import KRONECKER_SHAPES, SYNTHETIC_VOCAB_SIZE
from .model import (
    DenseCausalResidual,
    KroneckerResidual,
    build_model,
    inventory,
)
from .optimizer import build_muon
from .utils import atomic_json, finite_tree, seed_everything


def hidden_participation_ratio(hidden: torch.Tensor) -> float:
    values = hidden.float().reshape(-1, hidden.shape[-1])
    values = values - values.mean(0, keepdim=True)
    covariance = values.mT @ values / max(1, values.shape[0] - 1)
    eigenvalues = torch.linalg.eigvalsh(covariance).clamp_min(0)
    effective_rank = eigenvalues.sum().square() / (
        eigenvalues.square().sum() + 1e-12
    )
    return float((effective_rank / hidden.shape[-1]).item())


def prefix_error(model: torch.nn.Module, token_ids: torch.Tensor, prefix: int) -> float:
    changed = token_ids.clone()
    changed[:, prefix:] = torch.randint_like(changed[:, prefix:], 0, model.config.vocab_size)
    with torch.no_grad():
        left = model.hidden(token_ids)[:, :prefix]
        right = model.hidden(changed)[:, :prefix]
    return float((left - right).abs().max().item())


def future_jacobian_max(model: torch.nn.Module, token_ids: torch.Tensor, position: int) -> float:
    embedding = F.embedding(token_ids, model.vocabulary).detach().requires_grad_(True)
    if model.input_bridge is not None:
        value = model.input_bridge(embedding)
    else:
        value = embedding
    for block in model.blocks:
        value = block(value)
    if model.output_bridge is not None:
        value = model.output_bridge(value)
    scalar = value[:, position].float().sum()
    gradient = torch.autograd.grad(scalar, embedding)[0]
    if position + 1 >= token_ids.shape[1]:
        return 0.0
    return float(gradient[:, position + 1 :].abs().max().item())


def _operator_reference_gate() -> dict[str, float]:
    seed_everything(123)
    value = torch.randn(2, 7, 6, dtype=torch.float64, requires_grad=True)
    block = KroneckerResidual(6, 3, 7, 5).double()
    factored = block.branch(value)
    materialized = block.materialized_branch(value)
    forward = float((factored - materialized).abs().max().item())
    left = torch.autograd.grad(factored.square().sum(), value, retain_graph=True)[0]
    right = torch.autograd.grad(materialized.square().sum(), value)[0]
    backward = float((left - right).abs().max().item())

    dense_value = torch.randn(2, 6, 4, dtype=torch.float64, requires_grad=True)
    dense = DenseCausalResidual(6, 4, pair_chunk=5).double()
    dense_factored = dense.branch(dense_value)
    dense_reference = dense.materialized_branch(dense_value)
    dense_forward = float((dense_factored - dense_reference).abs().max().item())
    dense_left = torch.autograd.grad(
        dense_factored.square().sum(), dense_value, retain_graph=True
    )[0]
    dense_right = torch.autograd.grad(dense_reference.square().sum(), dense_value)[0]
    dense_backward = float((dense_left - dense_right).abs().max().item())
    return {
        "kronecker_forward_max_error": forward,
        "kronecker_backward_max_error": backward,
        "dense_forward_max_error": dense_forward,
        "dense_backward_max_error": dense_backward,
    }


def local_audit(output: str | Path | None = None) -> dict[str, Any]:
    seed_everything(7)
    operator = _operator_reference_gate()
    model_rows: dict[str, Any] = {}
    for variant in (*KRONECKER_SHAPES, "dense", "transformer"):
        model = build_model(variant, vocab_size=SYNTHETIC_VOCAB_SIZE)
        tokens = torch.randint(0, SYNTHETIC_VOCAB_SIZE, (2, 128))
        logits = model(tokens)
        loss = F.cross_entropy(logits.flatten(0, 1), tokens.flatten())
        loss.backward()
        gradients = {
            name: {
                "finite": bool(torch.isfinite(parameter.grad).all()),
                "nonzero": bool(parameter.grad.abs().max() > 0),
            }
            for name, parameter in model.named_parameters()
            if parameter.grad is not None
        }
        missing = [
            name for name, parameter in model.named_parameters() if parameter.grad is None
        ]
        optimizer, routing = build_muon(model, lr=0.01)
        optimizer.step()
        model_rows[variant] = {
            "finite_logits": bool(torch.isfinite(logits).all()),
            "finite_loss": bool(torch.isfinite(loss)),
            "prefix_error": prefix_error(model, tokens, 64),
            "future_jacobian_max": future_jacobian_max(model, tokens[:1], 31),
            "hidden_participation_ratio": hidden_participation_ratio(
                model.hidden(tokens)
            ),
            "missing_gradients": missing,
            "all_gradients_finite": all(row["finite"] for row in gradients.values()),
            "all_gradients_nonzero": all(row["nonzero"] for row in gradients.values()),
            "optimizer_routing": routing,
            "optimizer_state_finite": finite_tree(optimizer.state),
        }
        del model, optimizer, logits, loss
    counts = inventory()
    tolerance = 1e-10
    failures = []
    for name, value in operator.items():
        if value > tolerance:
            failures.append(f"{name}={value} exceeds {tolerance}")
    if counts["maximum_total_mismatch_fraction"] > 0.01:
        failures.append("total parameter mismatch exceeds one percent")
    if counts["maximum_body_mismatch_fraction"] > 0.01:
        failures.append("body parameter mismatch exceeds one percent")
    for variant, row in model_rows.items():
        if not row["finite_logits"] or not row["finite_loss"]:
            failures.append(f"{variant} produced non-finite values")
        if row["prefix_error"] > 1e-5 or row["future_jacobian_max"] > 1e-7:
            failures.append(f"{variant} violates causality")
        if row["missing_gradients"] or not row["all_gradients_finite"]:
            failures.append(f"{variant} has missing/non-finite gradients")
        if not row["optimizer_state_finite"]:
            failures.append(f"{variant} optimizer state is non-finite")
    result = {
        "schema": "expv2-1-local-audit-v1",
        "status": "pass" if not failures else "fail",
        "operator_reference": operator,
        "inventory": counts,
        "models": model_rows,
        "failures": failures,
    }
    if output is not None:
        atomic_json(output, result)
    return result
