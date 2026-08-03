"""Symmetric local falsification gates for Exp13."""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any

import numpy as np
import torch
import torch.nn.functional as F

from exp11_kronecker_debug.lm import batch_indices
from exp12_deep_kronecker.model import build_model as build_exp12_model

from .model import (
    CANDIDATE,
    MODEL_NAMES,
    STANDARD_CONTROLS,
    StandardTransformerBlock,
    apply_rope,
    build_model,
    model_inventory,
    rms_norm,
)


def _write_json(path: Path, value: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(path.name + ".tmp")
    temporary.write_text(json.dumps(value, indent=2, sort_keys=True))
    temporary.replace(path)


def tiny_overrides(name: str) -> dict[str, int]:
    values = {
        "context_length": 6,
        "vocab_size": 12,
        "width": 12,
        "depth": 3,
        "heads": 3,
        "ffn_width": 24,
        "mode1": 3,
        "mode2": 4,
        "rank": 2,
        "basis_banks": 2,
    }
    return values


def exp12_candidate_parity() -> dict[str, float | bool]:
    overrides = {
        "context_length": 8,
        "vocab_size": 32,
        "width": 16,
        "depth": 4,
        "heads": 4,
        "ffn_width": 32,
        "mode1": 4,
        "mode2": 4,
        "rank": 2,
        "basis_banks": 2,
    }
    torch.manual_seed(1301)
    old = build_exp12_model(CANDIDATE, **overrides)
    torch.manual_seed(1301)
    new = build_model(CANDIDATE, **overrides)
    keys_equal = old.state_dict().keys() == new.state_dict().keys()
    parameter_error = max(
        float((old.state_dict()[key] - new.state_dict()[key]).abs().max())
        for key in old.state_dict()
    )
    inputs = torch.randint(0, 32, (2, 8))
    output_error = float((old(inputs) - new(inputs)).abs().max().detach())
    return {
        "state_keys_equal": keys_equal,
        "parameter_max_error": parameter_error,
        "logit_max_error": output_error,
    }


def manual_attention_errors() -> tuple[float, float]:
    torch.manual_seed(1302)
    block = StandardTransformerBlock(8, 2, 16).double()
    value = torch.randn(2, 5, 8, dtype=torch.float64, requires_grad=True)
    actual = block.attention(value)
    query, key, content = block.qkv(rms_norm(value)).chunk(3, -1)

    def heads(item: torch.Tensor) -> torch.Tensor:
        return item.reshape(2, 5, 2, 4).transpose(1, 2)

    query, key, content = apply_rope(heads(query)), apply_rope(heads(key)), heads(content)
    scores = query @ key.transpose(-2, -1) / 2.0
    mask = torch.triu(torch.ones(5, 5, dtype=torch.bool), diagonal=1)
    scores = scores.masked_fill(mask[None, None], float("-inf"))
    attended = scores.softmax(-1) @ content
    expected = block.output(attended.transpose(1, 2).reshape(2, 5, 8))
    forward = float((actual - expected).abs().max().detach())
    differentiated = (value, block.qkv.weight, block.output.weight)
    actual_grad = torch.autograd.grad(
        actual.square().sum(), differentiated, retain_graph=True
    )
    expected_grad = torch.autograd.grad(expected.square().sum(), differentiated)
    backward = max(
        float((left - right).abs().max())
        for left, right in zip(actual_grad, expected_grad, strict=True)
    )
    return forward, backward


def prefix_error(name: str) -> float:
    torch.manual_seed(1303)
    overrides = tiny_overrides(name)
    overrides.update(context_length=8, width=16, mode1=4, mode2=4, heads=4)
    model = build_model(name, **overrides).eval()
    first = torch.randint(0, 12, (2, 8))
    second = first.clone()
    second[:, 5:] = torch.randint(0, 12, (2, 3))
    with torch.no_grad():
        return float((model(first)[:, :5] - model(second)[:, :5]).abs().max())


def overfit_and_updates(name: str) -> dict[str, Any]:
    torch.manual_seed(1304)
    model = build_model(name, **tiny_overrides(name))
    inputs = torch.randint(0, 12, (8, 6))
    targets = torch.randint(0, 12, (8, 6))
    optimizer = torch.optim.AdamW(
        model.parameters(), lr=0.03, betas=(0.9, 0.95), weight_decay=0.0
    )
    initial = 0.0
    all_gradients = False
    all_updates = False
    for step in range(151):
        optimizer.zero_grad(set_to_none=True)
        before = [parameter.detach().clone() for parameter in model.parameters()]
        loss = F.cross_entropy(model(inputs).flatten(0, 1), targets.flatten())
        if step == 0:
            initial = float(loss.detach())
        loss.backward()
        if step == 0:
            all_gradients = all(
                parameter.grad is not None
                and torch.isfinite(parameter.grad).all()
                and float(parameter.grad.norm()) > 0.0
                for parameter in model.parameters()
            )
        optimizer.step()
        if step == 0:
            all_updates = all(
                not torch.equal(previous, parameter.detach())
                for previous, parameter in zip(before, model.parameters(), strict=True)
            )
    return {
        "initial_nll": initial,
        "final_nll": float(loss.detach()),
        "all_gradients_finite_active": all_gradients,
        "all_parameters_updated": all_updates,
    }


def sampler_error() -> int:
    size, contexts, seed = 485_778, 40_320, 13

    def stream(batch: int) -> np.ndarray:
        return np.concatenate(
            [batch_indices(size, step, batch, seed) for step in range(contexts // batch)]
        )

    first, second = stream(384), stream(640)
    return int(np.count_nonzero(first != second))


def local_audit(output: str | Path | None = None) -> dict[str, Any]:
    parity = exp12_candidate_parity()
    attention_forward, attention_backward = manual_attention_errors()
    prefixes = {name: prefix_error(name) for name in MODEL_NAMES}
    learning = {name: overfit_and_updates(name) for name in MODEL_NAMES}
    inventories = {name: model_inventory(build_model(name)) for name in MODEL_NAMES}
    candidate_parameters = inventories[CANDIDATE]["total_parameters"]
    standard_parameter_ratios = {
        name: inventories[name]["total_parameters"] / candidate_parameters
        for name in STANDARD_CONTROLS
    }
    checks = {
        "exp12_candidate_parity": parity,
        "manual_attention_forward_max_error": attention_forward,
        "manual_attention_backward_max_error": attention_backward,
        "prefix_max_error": prefixes,
        "tiny_learning": learning,
        "logical_stream_mismatch_count": sampler_error(),
        "standard_parameter_ratios": standard_parameter_ratios,
        "inventories": inventories,
    }
    passed = (
        parity["state_keys_equal"]
        and parity["parameter_max_error"] == 0.0
        and parity["logit_max_error"] == 0.0
        and attention_forward <= 1e-10
        and attention_backward <= 1e-9
        and max(prefixes.values()) <= 1e-6
        and all(
            row["final_nll"] <= 0.25
            and row["final_nll"] <= 0.1 * row["initial_nll"]
            and row["all_gradients_finite_active"]
            and row["all_parameters_updated"]
            for row in learning.values()
        )
        and checks["logical_stream_mismatch_count"] == 0
        and max(standard_parameter_ratios.values()) <= 1.005
    )
    result = {
        "schema": "exp13-local-audit-v1",
        "status": "pass" if passed else "fail",
        "checks": checks,
    }
    if output is not None:
        _write_json(Path(output), result)
    if not passed:
        raise RuntimeError(f"Exp13 local audit failed: {result}")
    return result
