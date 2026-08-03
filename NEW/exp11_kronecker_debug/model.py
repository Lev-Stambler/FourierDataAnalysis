"""Readable reference models for the Kronecker debugging campaign.

The canonical block is deliberately expressible in two mathematical lines::

    Z = SiLU(RMS(H))
    H = RMS(H + alpha * sum_r g_r * Z x1 A_r x2 B_r x3 C_r)

``A`` is lower triangular, and the hidden width is tensorized as ``d1*d2``.
"""

from __future__ import annotations

import math
from dataclasses import asdict, dataclass

import torch
import torch.nn.functional as F
from torch import nn


VARIANTS = (
    "exp10-replica",
    "order2-balanced",
    "order3-r4",
    "order3-r8",
    "transformer",
)


@dataclass(frozen=True)
class ModelConfig:
    variant: str
    context_length: int = 256
    vocab_size: int = 16_384
    width: int = 128
    depth: int = 8
    mode1: int = 8
    mode2: int = 16
    rank: int = 0
    heads: int = 4
    mlp_width: int = 528

    def validate(self) -> None:
        if self.variant not in VARIANTS:
            raise ValueError(f"unknown variant: {self.variant}")
        if min(self.context_length, self.vocab_size, self.width, self.depth) <= 0:
            raise ValueError("model dimensions must be positive")
        if self.mode1 * self.mode2 != self.width:
            raise ValueError("mode1*mode2 must equal width")
        if self.variant.startswith("order3") and self.rank <= 0:
            raise ValueError("order-three variants require positive rank")
        if self.variant == "order2-balanced" and self.rank <= 0:
            raise ValueError("balanced order-two variant requires positive rank")
        if self.variant == "transformer" and self.width % self.heads:
            raise ValueError("Transformer width must divide evenly across heads")


def rms(value: torch.Tensor) -> torch.Tensor:
    return value.float().square().mean(-1).sqrt()


def rms_norm(value: torch.Tensor) -> torch.Tensor:
    scale = torch.rsqrt(value.float().square().mean(-1, keepdim=True) + 1e-6)
    return value * scale.to(value.dtype)


class PackedCausalRows(nn.Module):
    """Packed causal matrices with equal row energy at initialization/forward."""

    def __init__(self, rank: int, context_length: int, *, balanced: bool) -> None:
        super().__init__()
        self.rank = int(rank)
        self.context_length = int(context_length)
        self.balanced = bool(balanced)
        rows, columns = torch.tril_indices(context_length, context_length)
        self.register_buffer("rows", rows, persistent=False)
        self.register_buffer("columns", columns, persistent=False)
        self.raw = nn.Parameter(torch.randn(rank, rows.numel()))

    def matrix(self) -> torch.Tensor:
        values = self.raw
        if self.balanced:
            energy = torch.zeros(
                self.rank,
                self.context_length,
                device=values.device,
                dtype=torch.float32,
            )
            energy.scatter_add_(
                1,
                self.rows.expand(self.rank, -1),
                values.float().square(),
            )
            scale = torch.rsqrt(energy + 1e-8).to(values.dtype)
            values = values * scale.gather(1, self.rows.expand(self.rank, -1))
        else:
            # This reproduces Exp10's whole-triangle normalization bug.
            scale = torch.rsqrt(
                values.float().square().mean(1, keepdim=True)
                * self.context_length
                + 1e-12
            )
            values = values * scale.to(values.dtype)
        linear = self.rows * self.context_length + self.columns
        full = values.new_zeros(
            self.rank, self.context_length * self.context_length
        )
        return full.scatter(
            1, linear.expand(self.rank, -1), values
        ).reshape(self.rank, self.context_length, self.context_length)


class CanonicalFactor(nn.Module):
    """A per-rank factor with its radial gauge removed."""

    def __init__(self, rank: int, width: int) -> None:
        super().__init__()
        self.width = int(width)
        self.raw = nn.Parameter(torch.randn(rank, width, width) / math.sqrt(width))

    def value(self) -> torch.Tensor:
        target_rms = self.width**-0.5
        current = self.raw.float().square().mean((-2, -1), keepdim=True).sqrt()
        return self.raw * (target_rms / (current + 1e-8)).to(self.raw.dtype)


class MixerBlock(nn.Module):
    def branch(self, value: torch.Tensor) -> torch.Tensor:
        raise NotImplementedError

    def forward(self, value: torch.Tensor) -> torch.Tensor:
        return rms_norm(value + self.residual_scale * self.branch(value))


class Exp10ReplicaBlock(MixerBlock):
    """Exact structural replica of the flawed Exp10 order-two block."""

    def __init__(self, width: int, rank: int, context_length: int, depth: int) -> None:
        super().__init__()
        self.position = PackedCausalRows(rank, context_length, balanced=False)
        self.channel = nn.Parameter(
            torch.randn(rank, width, width) / math.sqrt(width)
        )
        self.rank = int(rank)
        self.residual_scale = 1.0 / math.sqrt(rank * depth)

    def branch(self, value: torch.Tensor) -> torch.Tensor:
        channel_scale = torch.rsqrt(
            self.channel.float().square().mean((-2, -1), keepdim=True)
            * self.channel.shape[-1]
            + 1e-12
        )
        channel = self.channel * channel_scale.to(self.channel.dtype)
        mixed = torch.einsum("btc,roc->brto", F.silu(rms_norm(value)), channel)
        return torch.einsum("brto,rst->bso", mixed, self.position.matrix())


class BalancedOrder2Block(MixerBlock):
    """Order-two ablation with causal-row balancing fixed."""

    def __init__(self, width: int, rank: int, context_length: int, depth: int) -> None:
        super().__init__()
        self.position = PackedCausalRows(rank, context_length, balanced=True)
        self.channel = CanonicalFactor(rank, width)
        self.mixing = nn.Parameter(torch.full((rank,), rank**-0.5))
        self.residual_scale = depth**-0.5

    def branch(self, value: torch.Tensor) -> torch.Tensor:
        mixed = torch.einsum(
            "btc,roc->brto", F.silu(rms_norm(value)), self.channel.value()
        )
        return torch.einsum(
            "brto,rst,r->bso", mixed, self.position.matrix(), self.mixing
        )


class CanonicalOrder3Block(MixerBlock):
    """Causal sequence x channel-mode-1 x channel-mode-2 Kronecker mixer."""

    def __init__(
        self,
        mode1: int,
        mode2: int,
        rank: int,
        context_length: int,
        depth: int,
    ) -> None:
        super().__init__()
        self.mode1 = int(mode1)
        self.mode2 = int(mode2)
        self.rank = int(rank)
        self.position = PackedCausalRows(rank, context_length, balanced=True)
        self.channel1 = CanonicalFactor(rank, mode1)
        self.channel2 = CanonicalFactor(rank, mode2)
        self.mixing = nn.Parameter(torch.full((rank,), rank**-0.5))
        self.residual_scale = depth**-0.5

    def rank_outputs(self, value: torch.Tensor) -> torch.Tensor:
        batch, tokens, width = value.shape
        if width != self.mode1 * self.mode2:
            raise ValueError("hidden width does not match tensor modes")
        tensor = F.silu(rms_norm(value)).reshape(
            batch, tokens, self.mode1, self.mode2
        )
        channels = torch.einsum(
            "btij,rpi,rqj->brtpq",
            tensor,
            self.channel1.value(),
            self.channel2.value(),
        )
        return torch.einsum(
            "brtpq,rst->brspq", channels, self.position.matrix()
        )

    def branch(self, value: torch.Tensor) -> torch.Tensor:
        ranked = self.rank_outputs(value)
        mixed = torch.einsum("brspq,r->bspq", ranked, self.mixing)
        return mixed.flatten(2)

    def materialized_branch(self, value: torch.Tensor) -> torch.Tensor:
        """Slow dense reference used only by correctness gates."""
        normalized = F.silu(rms_norm(value))
        output = torch.zeros_like(normalized)
        positions = self.position.matrix()
        first, second = self.channel1.value(), self.channel2.value()
        for index in range(self.rank):
            channel = torch.kron(first[index], second[index])
            output = output + self.mixing[index] * torch.einsum(
                "st,btc,oc->bso",
                positions[index],
                normalized,
                channel,
            )
        return output


def _rotate_half(value: torch.Tensor) -> torch.Tensor:
    first, second = value[..., 0::2], value[..., 1::2]
    return torch.stack((-second, first), dim=-1).flatten(-2)


def apply_rope(value: torch.Tensor) -> torch.Tensor:
    width = value.shape[-1]
    positions = torch.arange(value.shape[-2], device=value.device).float()
    frequencies = 1.0 / (
        10_000.0
        ** (torch.arange(0, width, 2, device=value.device).float() / width)
    )
    angles = torch.outer(positions, frequencies).repeat_interleave(2, -1)
    return value * angles.cos().to(value.dtype)[None, None] + _rotate_half(
        value
    ) * angles.sin().to(value.dtype)[None, None]


class TransformerBlock(nn.Module):
    def __init__(self, width: int, heads: int, mlp_width: int) -> None:
        super().__init__()
        self.width = int(width)
        self.heads = int(heads)
        self.qkv = nn.Linear(width, 3 * width, bias=False)
        self.output = nn.Linear(width, width, bias=False)
        self.gate_up = nn.Linear(width, 2 * mlp_width, bias=False)
        self.down = nn.Linear(mlp_width, width, bias=False)

    def forward(self, value: torch.Tensor) -> torch.Tensor:
        batch, tokens, width = value.shape
        head_width = width // self.heads
        query, key, content = self.qkv(rms_norm(value)).chunk(3, -1)

        def heads(item: torch.Tensor) -> torch.Tensor:
            return item.reshape(batch, tokens, self.heads, head_width).transpose(1, 2)

        query, key, content = apply_rope(heads(query)), apply_rope(heads(key)), heads(content)
        attended = F.scaled_dot_product_attention(
            query, key, content, dropout_p=0.0, is_causal=True
        )
        attended = attended.transpose(1, 2).reshape(batch, tokens, width)
        value = value + self.output(attended)
        gate, up = self.gate_up(rms_norm(value)).chunk(2, -1)
        return rms_norm(value + self.down(F.silu(gate) * up))


class LanguageModel(nn.Module):
    def __init__(self, config: ModelConfig) -> None:
        super().__init__()
        config.validate()
        self.config = config
        self.vocabulary = nn.Parameter(
            torch.randn(config.vocab_size, config.width) / math.sqrt(config.width)
        )
        self.blocks = nn.ModuleList(build_block(config) for _ in range(config.depth))

    def hidden(self, token_ids: torch.Tensor) -> torch.Tensor:
        if token_ids.ndim != 2 or token_ids.shape[1] != self.config.context_length:
            raise ValueError(
                f"expected token ids shaped [batch,{self.config.context_length}]"
            )
        value = F.embedding(token_ids, self.vocabulary)
        for block in self.blocks:
            value = block(value)
        return rms_norm(value)

    def forward(self, token_ids: torch.Tensor) -> torch.Tensor:
        return F.linear(self.hidden(token_ids), self.vocabulary)


def build_block(config: ModelConfig) -> nn.Module:
    if config.variant == "exp10-replica":
        return Exp10ReplicaBlock(
            config.width, config.rank or 8, config.context_length, config.depth
        )
    if config.variant == "order2-balanced":
        return BalancedOrder2Block(
            config.width, config.rank, config.context_length, config.depth
        )
    if config.variant.startswith("order3"):
        return CanonicalOrder3Block(
            config.mode1,
            config.mode2,
            config.rank,
            config.context_length,
            config.depth,
        )
    return TransformerBlock(config.width, config.heads, config.mlp_width)


def default_config(variant: str, **overrides: int) -> ModelConfig:
    ranks = {
        "exp10-replica": 8,
        "order2-balanced": 5,
        "order3-r4": 4,
        "order3-r8": 8,
        "transformer": 0,
    }
    return ModelConfig(variant=variant, rank=ranks[variant], **overrides)


def build_model(variant: str, **overrides: int) -> LanguageModel:
    return LanguageModel(default_config(variant, **overrides))


def model_inventory(model: LanguageModel) -> dict[str, int | float | dict]:
    vocabulary = model.vocabulary.numel()
    body = sum(parameter.numel() for parameter in model.blocks.parameters())
    return {
        "config": asdict(model.config),
        "vocabulary_parameters": vocabulary,
        "body_parameters": body,
        "total_parameters": vocabulary + body,
    }
