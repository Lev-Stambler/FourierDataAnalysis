"""Fresh reference models for the ExpV2-1 Kronecker reset.

The candidate is intentionally only a factorized whole-state linear map::

    X <- X + 1/sqrt(depth * rank) sum_r A_r SiLU(RMS(X)) B_r^T

The token factor ``A`` is lower triangular.  There are no queries, keys,
routers, FFNs, learned residual gains, or position-bank sharing mechanisms in
the Kronecker path.
"""

from __future__ import annotations

import math
from dataclasses import asdict
from typing import Any

import torch
import torch.nn.functional as F
from torch import nn

from .config import KRONECKER_SHAPES, ModelConfig, default_config


def rms_norm(value: torch.Tensor) -> torch.Tensor:
    scale = torch.rsqrt(value.float().square().mean(-1, keepdim=True) + 1e-6)
    return value * scale.to(value.dtype)


class PackedCausalFactors(nn.Module):
    """A sum of learned lower-triangular token factors."""

    def __init__(self, rank: int, context_length: int) -> None:
        super().__init__()
        self.rank = int(rank)
        self.context_length = int(context_length)
        rows, columns = torch.tril_indices(context_length, context_length)
        self.register_buffer("rows", rows, persistent=False)
        self.register_buffer("columns", columns, persistent=False)
        self.raw = nn.Parameter(torch.empty(rank, rows.numel()))
        self.reset_parameters()

    def reset_parameters(self) -> None:
        with torch.no_grad():
            self.raw.normal_()
            fan_in = (self.rows + 1).to(torch.float32).rsqrt()
            self.raw.mul_(fan_in[None])

    def matrix(self) -> torch.Tensor:
        linear = self.rows * self.context_length + self.columns
        full = self.raw.new_zeros(
            self.rank, self.context_length * self.context_length
        )
        return full.scatter(1, linear.expand(self.rank, -1), self.raw).reshape(
            self.rank, self.context_length, self.context_length
        )


class KroneckerResidual(nn.Module):
    def __init__(self, width: int, rank: int, context_length: int, depth: int) -> None:
        super().__init__()
        self.width = int(width)
        self.rank = int(rank)
        self.depth = int(depth)
        self.token = PackedCausalFactors(rank, context_length)
        self.channel = nn.Parameter(torch.empty(rank, width, width))
        nn.init.normal_(self.channel, std=width**-0.5)
        self.residual_scale = float((rank * depth) ** -0.5)

    def branch(self, value: torch.Tensor) -> torch.Tensor:
        normalized = F.silu(rms_norm(value))
        channel_mixed = torch.einsum(
            "btc,roc->brto", normalized, self.channel
        )
        return torch.einsum(
            "rst,brto->bso", self.token.matrix(), channel_mixed
        )

    def materialized_branch(self, value: torch.Tensor) -> torch.Tensor:
        normalized = F.silu(rms_norm(value))
        output = torch.zeros_like(normalized)
        token = self.token.matrix()
        for index in range(self.rank):
            output = output + torch.einsum(
                "st,btc,oc->bso",
                token[index],
                normalized,
                self.channel[index],
            )
        return output

    def forward(self, value: torch.Tensor) -> torch.Tensor:
        return value + self.residual_scale * self.branch(value)


class DenseCausalResidual(nn.Module):
    """Unfactorized channel block for every causal token pair."""

    def __init__(self, context_length: int, width: int, pair_chunk: int = 1024) -> None:
        super().__init__()
        self.context_length = int(context_length)
        self.width = int(width)
        self.pair_chunk = int(pair_chunk)
        rows, columns = torch.tril_indices(context_length, context_length)
        self.register_buffer("rows", rows, persistent=False)
        self.register_buffer("columns", columns, persistent=False)
        self.blocks = nn.Parameter(torch.empty(rows.numel(), width, width))
        self.reset_parameters()

    def reset_parameters(self) -> None:
        with torch.no_grad():
            self.blocks.normal_()
            fan_in = ((self.rows + 1) * self.width).to(torch.float32).rsqrt()
            self.blocks.mul_(fan_in[:, None, None])

    def branch(self, value: torch.Tensor) -> torch.Tensor:
        normalized = F.silu(rms_norm(value))
        output = normalized.new_zeros(normalized.shape)
        for start in range(0, self.rows.numel(), self.pair_chunk):
            stop = min(start + self.pair_chunk, self.rows.numel())
            source = normalized[:, self.columns[start:stop]]
            contribution = torch.einsum(
                "bni,noi->bno", source, self.blocks[start:stop]
            )
            output = torch.index_add(
                output, 1, self.rows[start:stop], contribution
            )
        return output

    def materialized_branch(self, value: torch.Tensor) -> torch.Tensor:
        normalized = F.silu(rms_norm(value))
        output = torch.zeros_like(normalized)
        for index in range(self.rows.numel()):
            source = normalized[:, self.columns[index]]
            output[:, self.rows[index]] += F.linear(source, self.blocks[index])
        return output

    def forward(self, value: torch.Tensor) -> torch.Tensor:
        return value + self.branch(value)


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
    def __init__(self, width: int, heads: int, ffn_width: int, depth: int) -> None:
        super().__init__()
        self.width = int(width)
        self.heads = int(heads)
        self.qkv = nn.Linear(width, 3 * width, bias=False)
        self.output = nn.Linear(width, width, bias=False)
        self.gate_up = nn.Linear(width, 2 * ffn_width, bias=False)
        self.down = nn.Linear(ffn_width, width, bias=False)
        self.reset_parameters(depth)

    def reset_parameters(self, depth: int) -> None:
        nn.init.normal_(self.qkv.weight, std=self.width**-0.5)
        nn.init.normal_(self.gate_up.weight, std=self.width**-0.5)
        residual_std = (self.width * 2 * depth) ** -0.5
        nn.init.normal_(self.output.weight, std=residual_std)
        nn.init.normal_(self.down.weight, std=residual_std)

    def forward(self, value: torch.Tensor) -> torch.Tensor:
        batch, tokens, width = value.shape
        head_width = width // self.heads
        query, key, content = self.qkv(rms_norm(value)).chunk(3, -1)

        def split_heads(item: torch.Tensor) -> torch.Tensor:
            return item.reshape(batch, tokens, self.heads, head_width).transpose(1, 2)

        query = apply_rope(split_heads(query))
        key = apply_rope(split_heads(key))
        content = split_heads(content)
        attended = F.scaled_dot_product_attention(
            query, key, content, dropout_p=0.0, is_causal=True
        )
        attended = attended.transpose(1, 2).reshape(batch, tokens, width)
        value = value + self.output(attended)
        gate, up = self.gate_up(rms_norm(value)).chunk(2, -1)
        return value + self.down(F.silu(gate) * up)


class LanguageModel(nn.Module):
    def __init__(self, config: ModelConfig) -> None:
        super().__init__()
        config.validate()
        self.config = config
        self.vocabulary = nn.Parameter(
            torch.empty(config.vocab_size, config.vocabulary_width)
        )
        nn.init.normal_(self.vocabulary, std=config.vocabulary_width**-0.5)
        if config.variant in KRONECKER_SHAPES:
            self.input_bridge = None
            self.output_bridge = None
            self.blocks = nn.ModuleList(
                KroneckerResidual(
                    config.width,
                    config.rank,
                    config.context_length,
                    config.depth,
                )
                for _ in range(config.depth)
            )
        elif config.variant == "dense":
            self.input_bridge = nn.Linear(
                config.vocabulary_width, config.dense_width, bias=False
            )
            self.output_bridge = nn.Linear(
                config.dense_width, config.vocabulary_width, bias=False
            )
            nn.init.normal_(
                self.input_bridge.weight, std=config.vocabulary_width**-0.5
            )
            nn.init.normal_(self.output_bridge.weight, std=config.dense_width**-0.5)
            self.blocks = nn.ModuleList(
                [DenseCausalResidual(config.context_length, config.dense_width)]
            )
        else:
            self.input_bridge = None
            self.output_bridge = None
            self.blocks = nn.ModuleList(
                TransformerBlock(
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
        if self.input_bridge is not None:
            value = self.input_bridge(value)
        for block in self.blocks:
            value = block(value)
        value = rms_norm(value)
        if self.output_bridge is not None:
            value = self.output_bridge(value)
        return rms_norm(value)

    def forward(self, token_ids: torch.Tensor) -> torch.Tensor:
        return F.linear(self.hidden(token_ids), self.vocabulary)


def build_model(
    variant: str,
    *,
    vocab_size: int | None = None,
    context_length: int | None = None,
) -> LanguageModel:
    kwargs: dict[str, int] = {}
    if vocab_size is not None:
        kwargs["vocab_size"] = int(vocab_size)
    if context_length is not None:
        kwargs["context_length"] = int(context_length)
    return LanguageModel(default_config(variant, **kwargs))


def model_inventory(model: LanguageModel) -> dict[str, Any]:
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
        "total_parameters": body + vocabulary,
        "trainable_parameters": sum(p.numel() for p in model.parameters()),
        "residual_updates": (
            2 * model.config.depth
            if model.config.variant == "transformer"
            else model.config.depth
        ),
    }


def inventory() -> dict[str, Any]:
    rows = {variant: model_inventory(build_model(variant)) for variant in (*KRONECKER_SHAPES, "dense", "transformer")}
    totals = [row["total_parameters"] for row in rows.values()]
    bodies = [row["body_parameters"] for row in rows.values()]
    return {
        "schema": "expv2-1-inventory-v1",
        "models": rows,
        "maximum_total_mismatch_fraction": (max(totals) - min(totals)) / min(totals),
        "maximum_body_mismatch_fraction": (max(bodies) - min(bodies)) / min(bodies),
    }
