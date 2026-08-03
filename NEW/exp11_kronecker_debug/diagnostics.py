"""Mechanistic diagnostics for representation and mixing utilization."""

from __future__ import annotations

import torch

from .model import CanonicalOrder3Block, MixerBlock, rms


def participation_ratio(value: torch.Tensor) -> float:
    """Normalized covariance effective rank in the last dimension."""
    flattened = value.detach().float().reshape(-1, value.shape[-1])
    centered = flattened - flattened.mean(0, keepdim=True)
    covariance = centered.mT @ centered / max(1, len(centered) - 1)
    eigenvalues = torch.linalg.eigvalsh(covariance).clamp_min(0)
    effective = eigenvalues.sum().square() / (eigenvalues.square().sum() + 1e-12)
    return float(effective / value.shape[-1])


def position_branch_rms(block: MixerBlock, value: torch.Tensor) -> list[float]:
    branch = block.branch(value).detach()
    return [float(item) for item in rms(branch).square().mean(0).sqrt()]


def position_balance_ratio(block: MixerBlock, value: torch.Tensor) -> float:
    values = torch.tensor(position_branch_rms(block, value))
    return float(values.max() / values.clamp_min(1e-12).min())


def rank_utilization(block: CanonicalOrder3Block, value: torch.Tensor) -> dict[str, float]:
    ranked = block.rank_outputs(value).detach().float()
    norms = ranked.square().mean((0, 2, 3, 4)).sqrt()
    probabilities = norms.square() / (norms.square().sum() + 1e-12)
    effective = 1.0 / (probabilities.square().sum() + 1e-12)
    return {
        "rank_norm_min": float(norms.min()),
        "rank_norm_max": float(norms.max()),
        "effective_rank_terms": float(effective),
        "effective_rank_fraction": float(effective / block.rank),
    }


def future_jacobian_max(block: MixerBlock, value: torch.Tensor) -> float:
    """Return the largest derivative from a future input into a past output."""
    if value.shape[0] != 1:
        raise ValueError("Jacobian diagnostic requires batch size one")
    value = value.detach().requires_grad_(True)
    output = block(value)
    maximum = 0.0
    for position in range(output.shape[1] - 1):
        gradient = torch.autograd.grad(
            output[:, position].sum(), value, retain_graph=True
        )[0]
        maximum = max(maximum, float(gradient[:, position + 1 :].abs().max()))
    return maximum
