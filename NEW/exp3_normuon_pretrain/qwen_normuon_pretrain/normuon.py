"""Single-device batched NorMuon.

Derived from the official implementation at commit
c6989a8354730695d9f5a9faa6c55eeb24865209:
https://github.com/zichongli5/NorMuon/blob/main/normuon.py

Copyright (c) 2025 zichongli5. Licensed under the MIT License; see
THIRD_PARTY_LICENSES.md.
"""

from __future__ import annotations

from collections import defaultdict

import torch

SOURCE_COMMIT = "c6989a8354730695d9f5a9faa6c55eeb24865209"
NS_COEFFICIENTS = (3.4445, -4.7750, 2.0315)


def zeropower_via_newton_schulz5(
    gradient: torch.Tensor,
    *,
    steps: int = 5,
) -> torch.Tensor:
    """Approximate the polar factor over the final two dimensions."""
    if gradient.ndim < 2:
        raise ValueError("NorMuon parameters must contain matrices")
    if steps <= 0:
        raise ValueError("Newton-Schulz steps must be positive")
    a, b, c = NS_COEFFICIENTS
    value = gradient.bfloat16()
    transposed = value.size(-2) > value.size(-1)
    if transposed:
        value = value.mT
    value = value / (
        value.norm(dim=(-2, -1), keepdim=True) + 1e-7
    )
    for _ in range(steps):
        gram = value @ value.mT
        polynomial = b * gram + c * gram @ gram
        value = a * value + polynomial @ value
    if transposed:
        value = value.mT
    return value


def normuon_update(
    gradient: torch.Tensor,
    momentum: torch.Tensor,
    second_momentum: torch.Tensor,
    *,
    beta1: float = 0.95,
    beta2: float = 0.95,
    ns_steps: int = 5,
    nesterov: bool = True,
    eps: float = 1e-10,
) -> torch.Tensor:
    """Return one neuron-normalized orthogonalized matrix update."""
    if gradient.ndim < 2:
        raise ValueError("NorMuon parameters must contain matrices")
    if momentum.shape != gradient.shape:
        raise ValueError("first-moment state shape mismatch")
    if second_momentum.shape != gradient.shape[:-1] + (1,):
        raise ValueError("row-moment state shape mismatch")
    if not 0.0 <= beta1 < 1.0 or not 0.0 <= beta2 < 1.0:
        raise ValueError("momentum coefficients must be in [0, 1)")
    if eps <= 0:
        raise ValueError("epsilon must be positive")

    momentum.lerp_(gradient, 1.0 - beta1)
    direction = (
        torch.lerp(gradient, momentum, beta1)
        if nesterov
        else momentum
    )
    update = zeropower_via_newton_schulz5(
        direction,
        steps=ns_steps,
    ).to(dtype=gradient.dtype)
    original_norm = update.norm(
        dim=(-2, -1),
        keepdim=True,
    )
    row_mean_square = torch.mean(
        update * update,
        dim=-1,
        keepdim=True,
    )
    second_momentum.lerp_(row_mean_square, 1.0 - beta2)
    update = update / (second_momentum.sqrt() + eps)
    normalized_norm = update.norm(
        dim=(-2, -1),
        keepdim=True,
    )
    update = update * (
        original_norm / (normalized_norm + eps)
    )
    aspect = max(
        1.0,
        gradient.size(-2) / gradient.size(-1),
    ) ** 0.5
    return update * aspect


class SingleDeviceNorMuon(torch.optim.Optimizer):
    """NorMuon with shape-bucketed NS5 and independent per-matrix state."""

    def __init__(
        self,
        params,
        *,
        lr: float,
        weight_decay: float = 0.01,
        beta1: float = 0.95,
        beta2: float = 0.95,
        ns_steps: int = 5,
        nesterov: bool = True,
        eps: float = 1e-10,
    ) -> None:
        if lr <= 0:
            raise ValueError("learning rate must be positive")
        defaults = {
            "lr": float(lr),
            "weight_decay": float(weight_decay),
            "beta1": float(beta1),
            "beta2": float(beta2),
            "ns_steps": int(ns_steps),
            "nesterov": bool(nesterov),
            "eps": float(eps),
        }
        super().__init__(params, defaults)
        for group in self.param_groups:
            for parameter in group["params"]:
                if parameter.ndim < 2:
                    raise ValueError(
                        "NorMuon groups may contain only matrix batches"
                    )

    @staticmethod
    def _shape_buckets(parameters) -> list[list[torch.Tensor]]:
        buckets = defaultdict(list)
        for parameter in parameters:
            if parameter.grad is None:
                continue
            key = (
                parameter.device,
                parameter.dtype,
                tuple(parameter.shape[-2:]),
            )
            buckets[key].append(parameter)
        return list(buckets.values())

    @torch.no_grad()
    def step(self, closure=None):
        loss = None
        if closure is not None:
            with torch.enable_grad():
                loss = closure()
        for group in self.param_groups:
            for bucket in self._shape_buckets(group["params"]):
                self._step_bucket(bucket, group)
        return loss

    @torch.no_grad()
    def _step_bucket(self, parameters, group) -> None:
        matrix_counts = [
            parameter.numel()
            // (parameter.shape[-2] * parameter.shape[-1])
            for parameter in parameters
        ]
        rows, columns = parameters[0].shape[-2:]
        direction_bank = torch.empty(
            sum(matrix_counts),
            rows,
            columns,
            device=parameters[0].device,
            dtype=torch.bfloat16,
        )
        offset = 0
        for parameter, count in zip(
            parameters,
            matrix_counts,
            strict=True,
        ):
            gradient = parameter.grad
            state = self.state[parameter]
            if not state:
                state["momentum_buffer"] = torch.zeros_like(parameter)
                state["second_momentum_buffer"] = torch.zeros_like(
                    parameter[..., 0:1]
                )
                state["step"] = 0
            state["step"] += 1
            momentum = state["momentum_buffer"]
            momentum.lerp_(gradient, 1.0 - group["beta1"])
            direction = (
                torch.lerp(gradient, momentum, group["beta1"])
                if group["nesterov"]
                else momentum
            )
            direction_bank[offset:offset + count].copy_(
                direction.reshape(count, rows, columns)
            )
            offset += count
        orthogonal_bank = zeropower_via_newton_schulz5(
            direction_bank,
            steps=group["ns_steps"],
        )
        del direction_bank

        offset = 0
        for parameter, count in zip(
            parameters,
            matrix_counts,
            strict=True,
        ):
            state = self.state[parameter]
            update = orthogonal_bank[
                offset:offset + count
            ].reshape(parameter.shape).to(dtype=parameter.dtype)
            offset += count
            original_norm = update.norm(
                dim=(-2, -1),
                keepdim=True,
            )
            row_mean_square = torch.mean(
                update * update,
                dim=-1,
                keepdim=True,
            )
            second = state["second_momentum_buffer"]
            second.lerp_(row_mean_square, 1.0 - group["beta2"])
            update = update / (second.sqrt() + group["eps"])
            normalized_norm = update.norm(
                dim=(-2, -1),
                keepdim=True,
            )
            update = update * (
                original_norm / (normalized_norm + group["eps"])
            )
            update = update * max(
                1.0,
                rows / columns,
            ) ** 0.5
            if group["weight_decay"]:
                parameter.mul_(
                    1.0 - group["lr"] * group["weight_decay"]
                )
            parameter.add_(update, alpha=-group["lr"])
