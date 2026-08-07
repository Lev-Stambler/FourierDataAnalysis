from __future__ import annotations

import math
from collections import defaultdict
from collections.abc import Iterable
from typing import Any

import torch


@torch.no_grad()
def zeropower_via_newton_schulz5(
    gradient: torch.Tensor, steps: int = 5
) -> torch.Tensor:
    """Orthogonalize every matrix over the final two dimensions independently."""
    if gradient.ndim < 2:
        raise ValueError("Muon parameters must contain matrices")
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
    """Canonical Muon; leading dimensions are independent matrix slices."""

    def __init__(
        self,
        params: Iterable[torch.nn.Parameter],
        *,
        lr: float,
        momentum: float = 0.95,
        nesterov: bool = True,
        ns_steps: int = 5,
    ) -> None:
        if lr <= 0 or not 0 <= momentum < 1 or ns_steps <= 0:
            raise ValueError("invalid Muon hyperparameters")
        super().__init__(
            params,
            {
                "lr": float(lr),
                "base_lr": float(lr),
                "momentum": float(momentum),
                "nesterov": bool(nesterov),
                "ns_steps": int(ns_steps),
                "weight_decay": 0.0,
            },
        )
        for group in self.param_groups:
            for parameter in group["params"]:
                if parameter.ndim < 2:
                    raise ValueError("Muon parameters must contain matrices")

    @torch.no_grad()
    def step(self, closure=None):
        loss = None
        if closure is not None:
            with torch.enable_grad():
                loss = closure()
        for group in self.param_groups:
            buckets: dict[tuple[Any, ...], list[torch.Tensor]] = defaultdict(list)
            for parameter in group["params"]:
                if parameter.grad is not None:
                    buckets[(parameter.device, parameter.dtype, *parameter.shape[-2:])].append(
                        parameter
                    )
            for parameters in buckets.values():
                self._step_bucket(parameters, group)
        return loss

    @torch.no_grad()
    def _step_bucket(self, parameters: list[torch.Tensor], group: dict[str, Any]) -> None:
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
            momentum = state.setdefault("momentum_buffer", torch.zeros_like(parameter))
            state["step"] = int(state.get("step", 0)) + 1
            momentum.lerp_(parameter.grad, 1 - group["momentum"])
            direction = (
                parameter.grad.lerp(momentum, group["momentum"])
                if group["nesterov"]
                else momentum
            )
            directions[offset : offset + count].copy_(
                direction.reshape(count, rows, columns)
            )
            offset += count
        updates = zeropower_via_newton_schulz5(directions, group["ns_steps"])
        adjusted_lr = group["lr"] * math.sqrt(max(1.0, rows / columns))
        offset = 0
        for parameter, count in zip(parameters, counts, strict=True):
            update = updates[offset : offset + count].reshape(parameter.shape)
            parameter.add_(update.to(parameter.dtype), alpha=-adjusted_lr)
            offset += count


class MuonWithAuxAdamW:
    def __init__(self, muon: BatchedMuon, auxiliary: torch.optim.AdamW) -> None:
        self.muon = muon
        self.auxiliary = auxiliary

    @property
    def param_groups(self) -> list[dict]:
        return self.muon.param_groups + self.auxiliary.param_groups

    @property
    def state(self) -> dict:
        return {**self.muon.state, **self.auxiliary.state}

    def zero_grad(self, set_to_none: bool = True) -> None:
        self.muon.zero_grad(set_to_none=set_to_none)
        self.auxiliary.zero_grad(set_to_none=set_to_none)

    def step(self) -> None:
        self.muon.step()
        self.auxiliary.step()

    def state_dict(self) -> dict:
        return {
            "schema": "v2-sbd-muon-aux-v1",
            "muon": self.muon.state_dict(),
            "auxiliary": self.auxiliary.state_dict(),
        }

    def load_state_dict(self, state: dict) -> None:
        if state.get("schema") != "v2-sbd-muon-aux-v1":
            raise ValueError("invalid split optimizer checkpoint")
        self.muon.load_state_dict(state["muon"])
        self.auxiliary.load_state_dict(state["auxiliary"])


def build_factorized_muon(
    model: torch.nn.Module,
    *,
    muon_lr: float,
    auxiliary_lr: float,
    auxiliary_weight_decay: float = 0.1,
) -> tuple[MuonWithAuxAdamW, dict[str, Any]]:
    muon_parameters: list[torch.nn.Parameter] = []
    auxiliary_decay: list[torch.nn.Parameter] = []
    auxiliary_no_decay: list[torch.nn.Parameter] = []
    names = {"muon": [], "auxiliary_decay": [], "auxiliary_no_decay": []}
    for name, parameter in model.named_parameters():
        if parameter.ndim >= 2 and name != "embed_tokens.weight":
            muon_parameters.append(parameter)
            names["muon"].append(name)
        elif parameter.ndim >= 2:
            auxiliary_decay.append(parameter)
            names["auxiliary_decay"].append(name)
        else:
            auxiliary_no_decay.append(parameter)
            names["auxiliary_no_decay"].append(name)
    routed = [*muon_parameters, *auxiliary_decay, *auxiliary_no_decay]
    if not muon_parameters or not auxiliary_decay or not auxiliary_no_decay:
        raise RuntimeError("split optimizer requires nonempty Muon/AdamW routes")
    if len({id(parameter) for parameter in routed}) != len(routed):
        raise RuntimeError("optimizer routes overlap")
    if sum(parameter.numel() for parameter in routed) != sum(
        parameter.numel() for parameter in model.parameters()
    ):
        raise RuntimeError("optimizer routing is incomplete")

    muon = BatchedMuon(muon_parameters, lr=muon_lr)
    auxiliary = torch.optim.AdamW(
        [
            {
                "params": auxiliary_decay,
                "weight_decay": auxiliary_weight_decay,
                "base_lr": auxiliary_lr,
            },
            {
                "params": auxiliary_no_decay,
                "weight_decay": 0.0,
                "base_lr": auxiliary_lr,
            },
        ],
        lr=auxiliary_lr,
        betas=(0.9, 0.95),
        eps=1e-8,
    )
    inventory = {
        "schema": "v2-sbd-optimizer-routing-v1",
        "muon_parameter_names": names["muon"],
        "auxiliary_parameter_names": [
            *names["auxiliary_decay"],
            *names["auxiliary_no_decay"],
        ],
        "muon_parameters": sum(parameter.numel() for parameter in muon_parameters),
        "auxiliary_parameters": sum(
            parameter.numel() for parameter in [*auxiliary_decay, *auxiliary_no_decay]
        ),
        "total_parameters": sum(parameter.numel() for parameter in routed),
    }
    return MuonWithAuxAdamW(muon, auxiliary), inventory


def set_lr_scale(optimizer: MuonWithAuxAdamW, scale: float) -> None:
    if scale < 0 or not math.isfinite(scale):
        raise ValueError("LR scale must be finite and non-negative")
    for group in optimizer.param_groups:
        group["lr"] = float(group["base_lr"]) * scale
