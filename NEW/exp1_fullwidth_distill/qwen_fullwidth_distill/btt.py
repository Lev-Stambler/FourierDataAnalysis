from __future__ import annotations

import math
from math import prod

import torch
from torch import nn


def context_preserving_modes(
    full_width: int,
    width: int,
    cores: int,
) -> tuple[int, ...]:
    """Return the pinned, padding-free tensorization used by the BTT study."""
    if full_width != 16_384:
        raise ValueError("context_preserving_v1 requires full width 16,384")
    if cores == 3:
        modes = {
            full_width: (32, 16, 32),
            4 * full_width: (64, 16, 64),
        }
    elif cores == 4:
        modes = {
            full_width: (16, 8, 8, 16),
            4 * full_width: (16, 16, 16, 16),
        }
    else:
        raise ValueError("context_preserving_v1 supports three or four cores")
    try:
        result = modes[width]
    except KeyError as error:
        raise ValueError(
            f"BTT width must be {full_width} or {4 * full_width}, got {width}"
        ) from error
    if prod(result) != width:
        raise RuntimeError("BTT layout does not exactly cover the logical width")
    return result


class BTTLinear(nn.Module):
    """Block Tensor-Train linear map with arbitrary tensor order.

    Unlike a conventional Tensor Train, each core is also indexed by all
    already-produced output blocks and all not-yet-consumed input blocks.
    This removes the corresponding parameter sharing and gives the BTT family
    used by Qiu et al. (ICML 2024). The logical operation remains a full-width
    linear map; tensorization changes only its parameterization.
    """

    def __init__(
        self,
        in_features: int,
        out_features: int,
        *,
        input_modes: tuple[int, ...],
        output_modes: tuple[int, ...],
        rank: int = 1,
        bias: bool = True,
        weight_norm: bool = True,
    ) -> None:
        super().__init__()
        if min(in_features, out_features, rank) <= 0:
            raise ValueError("BTT dimensions and rank must be positive")
        if len(input_modes) != len(output_modes) or len(input_modes) < 2:
            raise ValueError("BTT input/output modes must have equal order >= 2")
        if min(*input_modes, *output_modes) <= 0:
            raise ValueError("BTT modes must be positive")
        if prod(input_modes) != in_features:
            raise ValueError("input modes do not multiply to in_features")
        if prod(output_modes) != out_features:
            raise ValueError("output modes do not multiply to out_features")
        if not weight_norm:
            raise ValueError("BTT study requires core weight normalization")

        self.in_features = int(in_features)
        self.out_features = int(out_features)
        self.input_modes = tuple(int(value) for value in input_modes)
        self.output_modes = tuple(int(value) for value in output_modes)
        self.rank = int(rank)
        self.weight_norm = bool(weight_norm)
        self.order = len(self.input_modes)
        ranks = (1,) + (self.rank,) * (self.order - 1) + (1,)
        self.ranks = ranks

        cores = []
        gains = []
        target_rms = []
        for index, (input_mode, output_mode) in enumerate(
            zip(self.input_modes, self.output_modes, strict=True)
        ):
            rank_previous = ranks[index]
            rank_next = ranks[index + 1]
            shape = (
                rank_next,
                rank_previous,
                *self.output_modes[:index],
                output_mode,
                input_mode,
                *self.input_modes[index + 1 :],
            )
            core = nn.Parameter(torch.empty(shape))
            local_input = rank_previous * input_mode
            local_output = rank_next * output_mode
            core.lr_multiplier = (
                self.in_features / local_input / self.order
            )
            core.optimizer_role = "btt_core"
            cores.append(core)

            gain = nn.Parameter(torch.ones(()))
            gain.lr_multiplier = 1.0
            gain.optimizer_role = "btt_gain"
            gains.append(gain)
            target_rms.append(
                local_input ** -0.5
                * min(1.0, math.sqrt(local_output / local_input))
            )

        self.cores = nn.ParameterList(cores)
        self.gains = nn.ParameterList(gains)
        self.target_rms_values = tuple(float(value) for value in target_rms)
        self.register_buffer(
            "target_rms",
            torch.tensor(self.target_rms_values, dtype=torch.float32),
            persistent=False,
        )
        self.bias = (
            nn.Parameter(torch.empty(self.out_features)) if bias else None
        )
        if self.bias is not None:
            self.bias.lr_multiplier = 1.0
            self.bias.optimizer_role = "bias"
        self.reset_parameters()

    def reset_parameters(self) -> None:
        for index, core in enumerate(self.cores):
            nn.init.normal_(core, std=self.target_rms_values[index])
        for gain in self.gains:
            nn.init.ones_(gain)
        if self.bias is not None:
            nn.init.zeros_(self.bias)

    def normalized_core(self, index: int) -> torch.Tensor:
        core = self.cores[index]
        rms = torch.sqrt(core.float().square().mean() + 1e-8)
        # Qiu et al.'s transformer stabilization is a maximum-RMS
        # parametrization: it prevents a core from growing beyond its µP
        # initialization scale without forcing a core that has learned to
        # shrink back up to that scale. The learned scalar retains a separate
        # O(1)-LR amplitude degree of freedom.
        scale = self.gains[index].float() / torch.clamp(
            rms / self.target_rms[index],
            min=1.0,
        )
        return core * scale.to(dtype=core.dtype)

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        if x.shape[-1] != self.in_features:
            raise ValueError(
                f"expected final width {self.in_features}, got {x.shape[-1]}"
            )
        batch_shape = x.shape[:-1]
        batch = math.prod(batch_shape)
        value = x.reshape(batch, self.in_features)

        for index in range(self.order):
            rank_previous = self.ranks[index]
            rank_next = self.ranks[index + 1]
            output_prefix = prod(self.output_modes[:index])
            input_suffix = prod(self.input_modes[index + 1 :])
            input_mode = self.input_modes[index]
            output_mode = self.output_modes[index]
            value = value.reshape(
                batch,
                rank_previous,
                output_prefix,
                input_mode,
                input_suffix,
            )
            core = self.normalized_core(index).reshape(
                rank_next,
                rank_previous,
                output_prefix,
                output_mode,
                input_mode,
                input_suffix,
            )
            value = torch.einsum(
                "rtpmnq,btpnq->brpmq",
                core,
                value,
            )

        output = value.reshape(*batch_shape, self.out_features)
        if self.bias is not None:
            output = output + self.bias
        return output

    @property
    def saving(self) -> float:
        dense = self.in_features * self.out_features
        structured = sum(core.numel() for core in self.cores)
        return structured / dense

    def core_metrics(self, prefix: str) -> dict[str, float]:
        metrics: dict[str, float] = {}
        for index, (core, gain) in enumerate(
            zip(self.cores, self.gains, strict=True)
        ):
            metrics[f"{prefix}/core{index}_raw_rms"] = float(
                torch.sqrt(core.detach().float().square().mean())
            )
            metrics[f"{prefix}/core{index}_gain"] = float(gain.detach())
            metrics[f"{prefix}/core{index}_effective_lr_multiplier"] = float(
                core.lr_multiplier
            )
        return metrics

    def extra_repr(self) -> str:
        return (
            f"in_features={self.in_features}, out_features={self.out_features}, "
            f"input_modes={self.input_modes}, output_modes={self.output_modes}, "
            f"rank={self.rank}, bias={self.bias is not None}"
        )
