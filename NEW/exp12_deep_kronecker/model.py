"""Deep Kronecker mixer and a depth-matched Transformer control.

The Kronecker path deliberately spends parameters on compositional depth rather
than duplicating a dense causal table in every layer. Four causal basis banks
are shared cyclically by 32 blocks. Every block supplies its own channel
factors, content-dependent rank router, channel layout, and SwiGLU update.
"""

from __future__ import annotations

import math
from dataclasses import asdict, dataclass
from typing import Any

import torch
import torch.nn.functional as F
from torch import nn


VARIANTS = ("deep-kron-r8", "transformer")


@dataclass(frozen=True)
class ModelConfig:
    variant: str
    context_length: int = 256
    vocab_size: int = 16_384
    width: int = 128
    depth: int = 32
    mode1: int = 8
    mode2: int = 16
    rank: int = 8
    basis_banks: int = 4
    heads: int = 4
    ffn_width: int = 256

    def validate(self) -> None:
        if self.variant not in VARIANTS:
            raise ValueError(f"unknown variant: {self.variant}")
        dimensions = (
            self.context_length,
            self.vocab_size,
            self.width,
            self.depth,
            self.ffn_width,
        )
        if min(dimensions) <= 0:
            raise ValueError("model dimensions must be positive")
        if self.mode1 * self.mode2 != self.width:
            raise ValueError("mode1*mode2 must equal width")
        if self.variant == "deep-kron-r8":
            if self.rank <= 0 or not 0 < self.basis_banks <= self.depth:
                raise ValueError("Kronecker rank/basis-bank count is invalid")
        if self.variant == "transformer":
            if self.width % self.heads:
                raise ValueError("Transformer width must divide evenly across heads")
            if (self.width // self.heads) % 2:
                raise ValueError("RoPE requires an even head width")


def rms_norm(value: torch.Tensor) -> torch.Tensor:
    scale = torch.rsqrt(value.float().square().mean(-1, keepdim=True) + 1e-6)
    return value * scale.to(value.dtype)


class SharedCausalBasis(nn.Module):
    """A bank of packed causal matrices with unit energy in every output row."""

    def __init__(self, rank: int, context_length: int) -> None:
        super().__init__()
        self.rank = int(rank)
        self.context_length = int(context_length)
        rows, columns = torch.tril_indices(context_length, context_length)
        self.register_buffer("rows", rows, persistent=False)
        self.register_buffer("columns", columns, persistent=False)
        self.raw = nn.Parameter(torch.randn(rank, rows.numel()))

    def matrix(self) -> torch.Tensor:
        energy = torch.zeros(
            self.rank,
            self.context_length,
            device=self.raw.device,
            dtype=torch.float32,
        )
        energy.scatter_add_(
            1,
            self.rows.expand(self.rank, -1),
            self.raw.float().square(),
        )
        scale = torch.rsqrt(energy + 1e-8).to(self.raw.dtype)
        values = self.raw * scale.gather(1, self.rows.expand(self.rank, -1))
        linear = self.rows * self.context_length + self.columns
        full = values.new_zeros(
            self.rank, self.context_length * self.context_length
        )
        return full.scatter(1, linear.expand(self.rank, -1), values).reshape(
            self.rank, self.context_length, self.context_length
        )


class CanonicalFactor(nn.Module):
    """A matrix per Kronecker rank with its unidentifiable radial gauge removed."""

    def __init__(self, rank: int, width: int) -> None:
        super().__init__()
        self.width = int(width)
        self.raw = nn.Parameter(torch.randn(rank, width, width) / math.sqrt(width))

    def value(self) -> torch.Tensor:
        target_rms = self.width**-0.5
        current = self.raw.float().square().mean((-2, -1), keepdim=True).sqrt()
        return self.raw * (target_rms / (current + 1e-8)).to(self.raw.dtype)


def channel_layout(width: int, layer_index: int) -> torch.Tensor:
    """Return a bijection that changes the tensorized channel grouping by layer."""
    stride = 2 * (layer_index % max(1, width // 2)) + 1
    while math.gcd(stride, width) != 1:
        stride += 2
    offset = (layer_index * (layer_index + 1) // 2) % width
    return (torch.arange(width) * stride + offset) % width


class SwiGLU(nn.Module):
    def __init__(self, width: int, hidden_width: int) -> None:
        super().__init__()
        self.gate_up = nn.Linear(width, 2 * hidden_width, bias=False)
        self.down = nn.Linear(hidden_width, width, bias=False)

    def forward(self, value: torch.Tensor) -> torch.Tensor:
        gate, up = self.gate_up(value).chunk(2, -1)
        return self.down(F.silu(gate) * up)


class DeepKroneckerBlock(nn.Module):
    """Content-routed order-three mixer followed by a gated channel update."""

    def __init__(
        self,
        mode1: int,
        mode2: int,
        rank: int,
        ffn_width: int,
        depth: int,
        layer_index: int,
        basis_index: int,
    ) -> None:
        super().__init__()
        self.mode1 = int(mode1)
        self.mode2 = int(mode2)
        self.rank = int(rank)
        self.basis_index = int(basis_index)
        width = mode1 * mode2
        self.channel1 = CanonicalFactor(rank, mode1)
        self.channel2 = CanonicalFactor(rank, mode2)
        self.rank_amplitudes = nn.Parameter(torch.full((rank,), rank**-0.5))
        self.router = nn.Linear(width, rank, bias=False)
        nn.init.zeros_(self.router.weight)
        self.ffn = SwiGLU(width, ffn_width)
        initial_gain = (2.0 * depth) ** -0.5
        self.mixer_gain = nn.Parameter(torch.tensor(initial_gain))
        self.ffn_gain = nn.Parameter(torch.tensor(initial_gain))
        permutation = channel_layout(width, layer_index)
        self.register_buffer("permutation", permutation, persistent=False)
        self.register_buffer(
            "inverse_permutation", torch.argsort(permutation), persistent=False
        )

    def rank_outputs(
        self, value: torch.Tensor, positions: torch.Tensor
    ) -> torch.Tensor:
        batch, tokens, width = value.shape
        if width != self.mode1 * self.mode2:
            raise ValueError("hidden width does not match tensor modes")
        if positions.shape != (self.rank, tokens, tokens):
            raise ValueError("causal basis shape does not match the block")
        normalized = F.silu(rms_norm(value))[..., self.permutation]
        tensor = normalized.reshape(batch, tokens, self.mode1, self.mode2)
        channels = torch.einsum(
            "btij,rpi,rqj->brtpq",
            tensor,
            self.channel1.value(),
            self.channel2.value(),
        )
        return torch.einsum("brtpq,rst->brspq", channels, positions)

    def routing_weights(self, value: torch.Tensor) -> torch.Tensor:
        # Zero-initialized routing produces exactly-one gates while retaining a
        # nonzero derivative, so the content adaptation starts neutrally.
        return 2.0 * torch.sigmoid(self.router(rms_norm(value)))

    def branch(self, value: torch.Tensor, positions: torch.Tensor) -> torch.Tensor:
        ranked = self.rank_outputs(value, positions)
        gates = self.routing_weights(value).permute(0, 2, 1)[..., None, None]
        weighted = ranked * gates * self.rank_amplitudes[None, :, None, None, None]
        mixed = weighted.sum(1).flatten(2)
        return mixed[..., self.inverse_permutation]

    def materialized_branch(
        self, value: torch.Tensor, positions: torch.Tensor
    ) -> torch.Tensor:
        """Slow dense reference used by correctness tests only."""
        normalized = F.silu(rms_norm(value))[..., self.permutation]
        gates = self.routing_weights(value)
        output = torch.zeros_like(normalized)
        first, second = self.channel1.value(), self.channel2.value()
        for index in range(self.rank):
            channel = torch.kron(first[index], second[index])
            rank_output = torch.einsum(
                "st,btc,oc->bso", positions[index], normalized, channel
            )
            output = output + (
                self.rank_amplitudes[index]
                * gates[:, :, index, None]
                * rank_output
            )
        return output[..., self.inverse_permutation]

    def forward(self, value: torch.Tensor, positions: torch.Tensor) -> torch.Tensor:
        value = rms_norm(value + self.mixer_gain * self.branch(value, positions))
        return rms_norm(value + self.ffn_gain * self.ffn(rms_norm(value)))


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


class DeepTransformerBlock(nn.Module):
    """Depth-matched causal RoPE attention plus SwiGLU control block."""

    def __init__(
        self, width: int, heads: int, ffn_width: int, depth: int
    ) -> None:
        super().__init__()
        self.width = int(width)
        self.heads = int(heads)
        self.qkv = nn.Linear(width, 3 * width, bias=False)
        self.output = nn.Linear(width, width, bias=False)
        self.ffn = SwiGLU(width, ffn_width)
        initial_gain = (2.0 * depth) ** -0.5
        self.attention_gain = nn.Parameter(torch.tensor(initial_gain))
        self.ffn_gain = nn.Parameter(torch.tensor(initial_gain))

    def forward(self, value: torch.Tensor) -> torch.Tensor:
        batch, tokens, width = value.shape
        head_width = width // self.heads
        query, key, content = self.qkv(rms_norm(value)).chunk(3, -1)

        def heads(item: torch.Tensor) -> torch.Tensor:
            return item.reshape(batch, tokens, self.heads, head_width).transpose(1, 2)

        query = apply_rope(heads(query))
        key = apply_rope(heads(key))
        content = heads(content)
        attended = F.scaled_dot_product_attention(
            query, key, content, dropout_p=0.0, is_causal=True
        )
        attended = attended.transpose(1, 2).reshape(batch, tokens, width)
        value = rms_norm(value + self.attention_gain * self.output(attended))
        return rms_norm(value + self.ffn_gain * self.ffn(rms_norm(value)))


class DeepLanguageModel(nn.Module):
    def __init__(self, config: ModelConfig) -> None:
        super().__init__()
        config.validate()
        self.config = config
        self.vocabulary = nn.Parameter(
            torch.randn(config.vocab_size, config.width) / math.sqrt(config.width)
        )
        if config.variant == "deep-kron-r8":
            self.position_banks = nn.ModuleList(
                SharedCausalBasis(config.rank, config.context_length)
                for _ in range(config.basis_banks)
            )
            self.blocks = nn.ModuleList(
                DeepKroneckerBlock(
                    config.mode1,
                    config.mode2,
                    config.rank,
                    config.ffn_width,
                    config.depth,
                    layer_index,
                    layer_index % config.basis_banks,
                )
                for layer_index in range(config.depth)
            )
        else:
            self.position_banks = nn.ModuleList()
            self.blocks = nn.ModuleList(
                DeepTransformerBlock(
                    config.width, config.heads, config.ffn_width, config.depth
                )
                for _ in range(config.depth)
            )

    def hidden(self, token_ids: torch.Tensor) -> torch.Tensor:
        if token_ids.ndim != 2 or token_ids.shape[1] != self.config.context_length:
            raise ValueError(
                f"expected token ids shaped [batch,{self.config.context_length}]"
            )
        value = F.embedding(token_ids, self.vocabulary)
        if self.config.variant == "deep-kron-r8":
            positions = tuple(bank.matrix() for bank in self.position_banks)
            for block in self.blocks:
                value = block(value, positions[block.basis_index])
        else:
            for block in self.blocks:
                value = block(value)
        return rms_norm(value)

    def forward(self, token_ids: torch.Tensor) -> torch.Tensor:
        return F.linear(self.hidden(token_ids), self.vocabulary)


def default_config(variant: str, **overrides: int) -> ModelConfig:
    if variant not in VARIANTS:
        raise ValueError(f"unknown variant: {variant}")
    values: dict[str, Any] = {
        "variant": variant,
        "ffn_width": 256 if variant == "deep-kron-r8" else 192,
    }
    values.update(overrides)
    return ModelConfig(**values)


def build_model(variant: str, **overrides: int) -> DeepLanguageModel:
    return DeepLanguageModel(default_config(variant, **overrides))


def model_inventory(model: DeepLanguageModel) -> dict[str, Any]:
    vocabulary = model.vocabulary.numel()
    body = sum(
        parameter.numel()
        for name, parameter in model.named_parameters()
        if name != "vocabulary"
    )
    return {
        "config": asdict(model.config),
        "vocabulary_parameters": vocabulary,
        "body_parameters": body,
        "total_parameters": vocabulary + body,
        "nonlinear_residual_updates": 2 * model.config.depth,
        "causal_basis_parameters": sum(
            bank.raw.numel() for bank in model.position_banks
        ),
    }
