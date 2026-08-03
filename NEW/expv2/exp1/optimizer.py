"""Pure batched Muon used by every ExpV2-1 architecture."""

from __future__ import annotations

import math
from collections import defaultdict
from collections.abc import Iterable
from typing import Any, Callable

import torch


@torch.no_grad()
def zeropower_via_newton_schulz5(
    gradient: torch.Tensor, steps: int = 5
) -> torch.Tensor:
    """Orthogonalize independent matrices over their final two dimensions."""

    if gradient.ndim < 2:
        raise ValueError("Muon parameters must be matrices or batches of matrices")
    value = gradient.bfloat16()
    transposed = value.shape[-2] > value.shape[-1]
    if transposed:
        value = value.mT
    value = value / (value.norm(dim=(-2, -1), keepdim=True) + 1e-7)
    for _ in range(steps):
        gram = value @ value.mT
        value = 3.4445 * value + (-4.7750 * gram + 2.0315 * gram @ gram) @ value
    return value.mT if transposed else value


class BatchedMuon(torch.optim.Optimizer):
    """Muon with independent leading-dimension matrix slices.

    Every ExpV2-1 trainable tensor has at least two dimensions, so the campaign
    uses this optimizer exclusively.  Same-shaped matrices are stacked before
    Newton--Schulz to keep optimizer overhead small on the deep candidates.
    """

    def __init__(
        self,
        params: Iterable[torch.nn.Parameter],
        *,
        lr: float,
        weight_decay: float = 0.01,
        momentum: float = 0.95,
        nesterov: bool = True,
        ns_steps: int = 5,
    ) -> None:
        if lr <= 0 or weight_decay < 0 or not 0 <= momentum < 1 or ns_steps <= 0:
            raise ValueError("invalid Muon hyperparameters")
        super().__init__(
            params,
            {
                "lr": float(lr),
                "weight_decay": float(weight_decay),
                "momentum": float(momentum),
                "nesterov": bool(nesterov),
                "ns_steps": int(ns_steps),
            },
        )
        for group in self.param_groups:
            for parameter in group["params"]:
                if parameter.ndim < 2:
                    raise ValueError(
                        "pure ExpV2-1 Muon requires every parameter to be at least 2D"
                    )

    @torch.no_grad()
    def step(self, closure: Callable[[], torch.Tensor] | None = None) -> Any:
        loss = None
        if closure is not None:
            with torch.enable_grad():
                loss = closure()
        for group in self.param_groups:
            buckets: dict[tuple[Any, ...], list[torch.Tensor]] = defaultdict(list)
            for parameter in group["params"]:
                if parameter.grad is None:
                    continue
                key = (parameter.device, parameter.dtype, *parameter.shape[-2:])
                buckets[key].append(parameter)
            for parameters in buckets.values():
                self._step_bucket(parameters, group)
        return loss

    @torch.no_grad()
    def _step_bucket(
        self, parameters: list[torch.Tensor], group: dict[str, Any]
    ) -> None:
        rows, columns = parameters[0].shape[-2:]
        counts = [parameter.numel() // (rows * columns) for parameter in parameters]
        directions = torch.empty(
            sum(counts),
            rows,
            columns,
            device=parameters[0].device,
            dtype=torch.bfloat16,
        )
        offset = 0
        for parameter, count in zip(parameters, counts, strict=True):
            state = self.state[parameter]
            momentum_buffer = state.setdefault(
                "momentum_buffer", torch.zeros_like(parameter)
            )
            momentum_buffer.lerp_(parameter.grad, 1.0 - group["momentum"])
            direction = (
                parameter.grad.lerp(momentum_buffer, group["momentum"])
                if group["nesterov"]
                else momentum_buffer
            )
            directions[offset : offset + count].copy_(
                direction.reshape(count, rows, columns)
            )
            offset += count
        updates = zeropower_via_newton_schulz5(
            directions, steps=group["ns_steps"]
        )
        adjusted_lr = group["lr"] * math.sqrt(max(1.0, rows / columns))
        offset = 0
        for parameter, count in zip(parameters, counts, strict=True):
            parameter.mul_(1.0 - group["lr"] * group["weight_decay"])
            update = updates[offset : offset + count].reshape(parameter.shape)
            parameter.add_(update.to(parameter.dtype), alpha=-adjusted_lr)
            offset += count


def build_muon(
    model: torch.nn.Module,
    *,
    lr: float,
    weight_decay: float = 0.01,
    momentum: float = 0.95,
    nesterov: bool = True,
    ns_steps: int = 5,
) -> tuple[BatchedMuon, dict[str, Any]]:
    named = list(model.named_parameters())
    if not named:
        raise RuntimeError("model contains no trainable parameters")
    invalid = [name for name, parameter in named if parameter.ndim < 2]
    if invalid:
        raise RuntimeError(f"non-Muon parameters are forbidden: {invalid}")
    parameters = [parameter for _, parameter in named]
    if len({id(parameter) for parameter in parameters}) != len(parameters):
        raise RuntimeError("optimizer routing contains duplicate parameters")
    optimizer = BatchedMuon(
        parameters,
        lr=lr,
        weight_decay=weight_decay,
        momentum=momentum,
        nesterov=nesterov,
        ns_steps=ns_steps,
    )
    inventory = {
        "optimizer": "pure-batched-muon",
        "parameter_names": [name for name, _ in named],
        "parameter_tensors": len(named),
        "parameters": sum(parameter.numel() for parameter in parameters),
        "lr": lr,
        "weight_decay": weight_decay,
        "momentum": momentum,
        "nesterov": nesterov,
        "ns_steps": ns_steps,
    }
    return optimizer, inventory


def set_lr(optimizer: BatchedMuon, value: float) -> None:
    if value <= 0:
        raise ValueError("learning rate must be positive")
    for group in optimizer.param_groups:
        group["lr"] = float(value)
