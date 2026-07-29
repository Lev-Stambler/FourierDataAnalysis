from __future__ import annotations

import math
from math import prod

import torch
from torch import nn


class KroneckerSumLinear(nn.Module):
    """Apply a small sum of Kronecker products in native tensor form."""

    def __init__(
        self,
        input_modes: tuple[int, ...],
        output_modes: tuple[int, ...],
        *,
        rank: int,
        bias: bool = True,
    ) -> None:
        super().__init__()
        if len(input_modes) not in (2, 3):
            raise ValueError("tensor-native operator supports order two or three")
        if len(input_modes) != len(output_modes):
            raise ValueError("input and output factor orders must match")
        if min(*input_modes, *output_modes, rank) <= 0:
            raise ValueError("Kronecker modes and rank must be positive")
        self.input_modes = tuple(int(value) for value in input_modes)
        self.output_modes = tuple(int(value) for value in output_modes)
        self.order = len(input_modes)
        self.rank = int(rank)

        factors = []
        gains = []
        targets = []
        for input_mode, output_mode in zip(
            self.input_modes,
            self.output_modes,
            strict=True,
        ):
            shape = (
                (output_mode, input_mode)
                if rank == 1
                else (rank, output_mode, input_mode)
            )
            factor = nn.Parameter(torch.empty(shape))
            factor.optimizer_role = "kronecker_factor"
            factors.append(factor)
            gain = nn.Parameter(
                torch.ones(1, 1 if rank == 1 else rank)
            )
            gain.optimizer_role = "kronecker_gain"
            gains.append(gain)
            targets.append(
                input_mode**-0.5 * min(1.0, math.sqrt(output_mode / input_mode))
            )
        self.factors = nn.ParameterList(factors)
        self.gains = nn.ParameterList(gains)
        self.target_rms_values = tuple(targets)
        self.register_buffer(
            "target_rms",
            torch.tensor(targets, dtype=torch.float32),
            persistent=False,
        )
        self.mixing = (
            None
            if rank == 1
            else nn.Parameter(torch.empty(1, rank))
        )
        if self.mixing is not None:
            self.mixing.optimizer_role = "kronecker_mixing"
        self.bias = nn.Parameter(torch.empty(*self.output_modes)) if bias else None
        if self.bias is not None:
            self.bias.optimizer_role = "bias"
        self.reset_parameters()

    @property
    def in_features(self) -> int:
        return prod(self.input_modes)

    @property
    def out_features(self) -> int:
        return prod(self.output_modes)

    def reset_parameters(self) -> None:
        for factor, target in zip(
            self.factors,
            self.target_rms_values,
            strict=True,
        ):
            nn.init.normal_(factor, std=target)
        for gain in self.gains:
            nn.init.ones_(gain)
        if self.mixing is not None:
            nn.init.constant_(self.mixing, self.rank**-0.5)
        if self.bias is not None:
            nn.init.zeros_(self.bias)

    def normalized_factor(self, index: int) -> torch.Tensor:
        factor = self.factors[index]
        reduction = None if self.rank == 1 else (-2, -1)
        rms = torch.sqrt(factor.float().square().mean(dim=reduction) + 1e-8)
        gain = self.gains[index].float().reshape(-1)
        if self.rank == 1:
            gain = gain[0]
        scale = gain / torch.clamp(
            rms / self.target_rms[index],
            min=1.0,
        )
        if self.rank > 1:
            scale = scale[:, None, None]
        return factor * scale.to(dtype=factor.dtype)

    def _order_two(self, value: torch.Tensor) -> torch.Tensor:
        context = self.normalized_factor(0)
        channel = self.normalized_factor(1)
        terminal = self.output_modes[0] == 1
        if self.rank == 1:
            if terminal:
                output = torch.einsum(
                    "bij,oi->boj",
                    value,
                    context,
                )
                output = torch.matmul(output, channel.mT)
            else:
                output = torch.matmul(value, channel.mT)
                output = torch.einsum(
                    "oi,bip->bop",
                    context,
                    output,
                )
            return output
        mixing = self.mixing.reshape(-1)
        if terminal:
            output = torch.einsum(
                "bij,roi->broj",
                value,
                context,
            )
            return torch.einsum(
                "broj,rpj,r->bop",
                output,
                channel,
                mixing,
            )
        output = torch.einsum(
            "bij,rpj->brip",
            value,
            channel,
        )
        return torch.einsum(
            "brip,roi,r->bop",
            output,
            context,
            mixing,
        )

    def _order_three(self, value: torch.Tensor) -> torch.Tensor:
        context = self.normalized_factor(0)
        channel0 = self.normalized_factor(1)
        channel1 = self.normalized_factor(2)
        terminal = self.output_modes[0] == 1
        if self.rank == 1:
            if terminal:
                output = torch.einsum(
                    "bijk,oi->bojk",
                    value,
                    context,
                )
            else:
                output = torch.einsum(
                    "bijk,qk->bijq",
                    value,
                    channel1,
                )
                output = torch.einsum(
                    "bijq,pj->bipq",
                    output,
                    channel0,
                )
                return torch.einsum(
                    "bipq,oi->bopq",
                    output,
                    context,
                )
            output = torch.einsum(
                "bojk,qk->bojq",
                output,
                channel1,
            )
            return torch.einsum(
                "bojq,pj->bopq",
                output,
                channel0,
            )
        mixing = self.mixing.reshape(-1)
        if terminal:
            output = torch.einsum(
                "bijk,roi->brojk",
                value,
                context,
            )
            output = torch.einsum(
                "brojk,rqk->brojq",
                output,
                channel1,
            )
            return torch.einsum(
                "brojq,rpj,r->bopq",
                output,
                channel0,
                mixing,
            )
        output = torch.einsum(
            "bijk,rqk->brijq",
            value,
            channel1,
        )
        output = torch.einsum(
            "brijq,rpj->bripq",
            output,
            channel0,
        )
        return torch.einsum(
            "bripq,roi,r->bopq",
            output,
            context,
            mixing,
        )

    def forward(self, value: torch.Tensor) -> torch.Tensor:
        expected = (value.shape[0], *self.input_modes)
        if value.ndim != self.order + 1 or tuple(value.shape) != expected:
            raise ValueError(
                f"expected tensor shape [batch,{self.input_modes}], "
                f"got {tuple(value.shape)}"
            )
        output = self._order_two(value) if self.order == 2 else self._order_three(value)
        if self.bias is not None:
            output = output + self.bias
        return output
