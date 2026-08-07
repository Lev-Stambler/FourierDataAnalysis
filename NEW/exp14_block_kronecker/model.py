"""Fully factorized block-causal Kronecker and matched Transformer models.

The candidate treats a 256-token input as 16 ordered groups, each containing a
4 x 4 noncausal workspace.  Only the outer 16 x 16 factor is causal.  Every
rank path is therefore

    L_group x U_workspace1 x V_workspace2 x B_channel1 x C_channel2.

The loss is shifted by one complete 16-token group; see :mod:`data`.  That
shift is essential because the inner workspace factors are deliberately
noncausal.
"""

from __future__ import annotations

import math
from dataclasses import asdict, dataclass, replace
from typing import Any

import torch
import torch.nn.functional as F
from torch import nn


CANDIDATE = "block-kron-r8"
NO_WORKSPACE_PERMUTATION = "block-kron-r8-no-workspace-permutation"
STANDARD_CONTROLS = (
    "block-transformer-d32-w128",
    "block-transformer-d10-w160",
    "block-transformer-d6-w192",
    "block-transformer-d3-w256",
)
MODEL_NAMES = (CANDIDATE, NO_WORKSPACE_PERMUTATION, *STANDARD_CONTROLS)


@dataclass(frozen=True)
class ModelSpec:
    name: str
    family: str
    context_length: int = 256
    vocab_size: int = 16_384
    width: int = 128
    depth: int = 32
    heads: int = 4
    ffn_width: int = 256
    group_count: int = 16
    workspace1: int = 4
    workspace2: int = 4
    channel1: int = 8
    channel2: int = 16
    rank: int = 8
    permute_workspace: bool = True

    @property
    def workspace_size(self) -> int:
        return self.workspace1 * self.workspace2

    def validate(self) -> None:
        if self.name not in MODEL_NAMES:
            raise ValueError(f"unknown model: {self.name}")
        if self.family not in ("block-kronecker", "block-transformer"):
            raise ValueError(f"unknown family: {self.family}")
        if min(
            self.context_length,
            self.vocab_size,
            self.width,
            self.depth,
            self.ffn_width,
            self.group_count,
            self.workspace1,
            self.workspace2,
        ) <= 0:
            raise ValueError("model dimensions must be positive")
        if self.group_count * self.workspace_size != self.context_length:
            raise ValueError("group and workspace modes must multiply to context length")
        if self.family == "block-kronecker":
            if self.channel1 * self.channel2 != self.width:
                raise ValueError("channel modes must multiply to width")
            if self.rank <= 0:
                raise ValueError("Kronecker rank must be positive")
        elif self.width % self.heads or (self.width // self.heads) % 2:
            raise ValueError("RoPE requires an even integral head width")


SPECS = {
    CANDIDATE: ModelSpec(CANDIDATE, "block-kronecker"),
    NO_WORKSPACE_PERMUTATION: ModelSpec(
        NO_WORKSPACE_PERMUTATION,
        "block-kronecker",
        permute_workspace=False,
    ),
    "block-transformer-d32-w128": ModelSpec(
        "block-transformer-d32-w128", "block-transformer", ffn_width=98
    ),
    "block-transformer-d10-w160": ModelSpec(
        "block-transformer-d10-w160",
        "block-transformer",
        width=160,
        depth=10,
        heads=5,
        ffn_width=365,
        channel1=10,
    ),
    "block-transformer-d6-w192": ModelSpec(
        "block-transformer-d6-w192",
        "block-transformer",
        width=192,
        depth=6,
        heads=6,
        ffn_width=395,
        channel1=12,
    ),
    "block-transformer-d3-w256": ModelSpec(
        "block-transformer-d3-w256",
        "block-transformer",
        width=256,
        depth=3,
        heads=8,
        ffn_width=181,
        channel1=16,
    ),
}


def rms_norm(value: torch.Tensor) -> torch.Tensor:
    scale = torch.rsqrt(value.float().square().mean(-1, keepdim=True) + 1e-6)
    return value * scale.to(value.dtype)


class RowNormalizedFactor(nn.Module):
    """A dense rank bank with unit-norm output rows."""

    def __init__(self, rank: int, width: int) -> None:
        super().__init__()
        self.rank = int(rank)
        self.width = int(width)
        self.raw = nn.Parameter(torch.randn(rank, width, width) / math.sqrt(width))

    def value(self) -> torch.Tensor:
        norm = self.raw.float().square().sum(-1, keepdim=True).sqrt()
        return self.raw / (norm + 1e-8).to(self.raw.dtype)


class PackedLowerFactor(nn.Module):
    """Packed inclusive lower-triangular rank bank with unit-norm rows."""

    def __init__(self, rank: int, width: int) -> None:
        super().__init__()
        self.rank = int(rank)
        self.width = int(width)
        rows, columns = torch.tril_indices(width, width)
        self.register_buffer("rows", rows, persistent=False)
        self.register_buffer("columns", columns, persistent=False)
        self.raw = nn.Parameter(torch.randn(rank, rows.numel()))

    def value(self) -> torch.Tensor:
        row_index = self.rows.expand(self.rank, -1)
        energy = torch.zeros(
            self.rank,
            self.width,
            dtype=torch.float32,
            device=self.raw.device,
        )
        energy.scatter_add_(1, row_index, self.raw.float().square())
        packed = self.raw / torch.sqrt(energy + 1e-8).to(self.raw.dtype).gather(
            1, row_index
        )
        linear = self.rows * self.width + self.columns
        dense = packed.new_zeros(self.rank, self.width * self.width)
        return dense.scatter(1, linear.expand(self.rank, -1), packed).reshape(
            self.rank, self.width, self.width
        )


def affine_layout(width: int, layer_index: int) -> torch.Tensor:
    """Deterministic permutation used only inside a noncausal mode."""

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


class BlockKroneckerLayer(nn.Module):
    """One order-five block-causal Kronecker residual layer."""

    def __init__(self, spec: ModelSpec, layer_index: int) -> None:
        super().__init__()
        self.spec = spec
        self.outer = PackedLowerFactor(spec.rank, spec.group_count)
        self.workspace1 = RowNormalizedFactor(spec.rank, spec.workspace1)
        self.workspace2 = RowNormalizedFactor(spec.rank, spec.workspace2)
        self.channel1 = RowNormalizedFactor(spec.rank, spec.channel1)
        self.channel2 = RowNormalizedFactor(spec.rank, spec.channel2)
        self.rank_amplitudes = nn.Parameter(
            torch.full((spec.rank,), spec.rank**-0.5)
        )
        self.router = nn.Linear(spec.width, spec.rank, bias=False)
        nn.init.zeros_(self.router.weight)
        self.ffn = SwiGLU(spec.width, spec.ffn_width)
        initial_gain = (2.0 * spec.depth) ** -0.5
        self.mixer_gain = nn.Parameter(torch.tensor(initial_gain))
        self.ffn_gain = nn.Parameter(torch.tensor(initial_gain))
        workspace = (
            affine_layout(spec.workspace_size, layer_index)
            if spec.permute_workspace
            else torch.arange(spec.workspace_size)
        )
        channel = affine_layout(spec.width, layer_index)
        self.register_buffer("workspace_permutation", workspace, persistent=False)
        self.register_buffer(
            "inverse_workspace_permutation", torch.argsort(workspace), persistent=False
        )
        self.register_buffer("channel_permutation", channel, persistent=False)
        self.register_buffer(
            "inverse_channel_permutation", torch.argsort(channel), persistent=False
        )

    def rank_outputs(self, value: torch.Tensor) -> torch.Tensor:
        spec = self.spec
        batch, tokens, width = value.shape
        if (tokens, width) != (spec.context_length, spec.width):
            raise ValueError("hidden state does not match the block specification")
        normalized = F.silu(rms_norm(value)).reshape(
            batch, spec.group_count, spec.workspace_size, width
        )
        normalized = normalized[:, :, self.workspace_permutation]
        normalized = normalized[..., self.channel_permutation]
        tensor = normalized.reshape(
            batch,
            spec.group_count,
            spec.workspace1,
            spec.workspace2,
            spec.channel1,
            spec.channel2,
        )
        channels = torch.einsum(
            "bgijuv,rpu,rqv->brgijpq",
            tensor,
            self.channel1.value(),
            self.channel2.value(),
        )
        workspace = torch.einsum(
            "brgijpq,rxi,ryj->brgxypq",
            channels,
            self.workspace1.value(),
            self.workspace2.value(),
        )
        groups = torch.einsum(
            "brgxypq,rsg->brsxypq", workspace, self.outer.value()
        )
        ranked = groups.reshape(
            batch, spec.rank, spec.group_count, spec.workspace_size, spec.width
        )
        ranked = ranked[:, :, :, self.inverse_workspace_permutation]
        ranked = ranked[..., self.inverse_channel_permutation]
        return ranked.reshape(batch, spec.rank, tokens, width)

    def materialized_rank_outputs(self, value: torch.Tensor) -> torch.Tensor:
        """Slow exact reference used only by numerical tests."""

        spec = self.spec
        batch = value.shape[0]
        normalized = F.silu(rms_norm(value)).reshape(
            batch, spec.group_count, spec.workspace_size, spec.width
        )
        normalized = normalized[:, :, self.workspace_permutation]
        normalized = normalized[..., self.channel_permutation]
        flattened = normalized.reshape(batch, -1)
        outputs = []
        factors = (
            self.outer.value(),
            self.workspace1.value(),
            self.workspace2.value(),
            self.channel1.value(),
            self.channel2.value(),
        )
        for rank_index in range(spec.rank):
            matrix = factors[0][rank_index]
            for factor in factors[1:]:
                matrix = torch.kron(matrix, factor[rank_index])
            output = F.linear(flattened, matrix).reshape(
                batch, spec.group_count, spec.workspace_size, spec.width
            )
            output = output[:, :, self.inverse_workspace_permutation]
            output = output[..., self.inverse_channel_permutation]
            outputs.append(output.reshape(batch, spec.context_length, spec.width))
        return torch.stack(outputs, 1)

    def branch(self, value: torch.Tensor) -> torch.Tensor:
        ranked = self.rank_outputs(value)
        gates = (2.0 * torch.sigmoid(self.router(rms_norm(value)))).permute(0, 2, 1)
        weighted = ranked * gates[..., None] * self.rank_amplitudes[None, :, None, None]
        return weighted.sum(1)

    def forward(self, value: torch.Tensor) -> torch.Tensor:
        value = rms_norm(value + self.mixer_gain * self.branch(value))
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


class BlockTransformerLayer(nn.Module):
    """Matched Transformer using the same block-causal information boundary."""

    def __init__(self, spec: ModelSpec) -> None:
        super().__init__()
        self.spec = spec
        self.qkv = nn.Linear(spec.width, 3 * spec.width, bias=False)
        self.output = nn.Linear(spec.width, spec.width, bias=False)
        self.ffn = SwiGLU(spec.width, spec.ffn_width)
        groups = torch.arange(spec.context_length) // spec.workspace_size
        block_mask = groups[:, None] >= groups[None, :]
        self.register_buffer("block_mask", block_mask, persistent=False)

    def attention(self, value: torch.Tensor) -> torch.Tensor:
        spec = self.spec
        batch, tokens, width = value.shape
        head_width = width // spec.heads
        query, key, content = self.qkv(rms_norm(value)).chunk(3, -1)

        def heads(item: torch.Tensor) -> torch.Tensor:
            return item.reshape(batch, tokens, spec.heads, head_width).transpose(1, 2)

        attended = F.scaled_dot_product_attention(
            apply_rope(heads(query)),
            apply_rope(heads(key)),
            heads(content),
            attn_mask=self.block_mask,
            dropout_p=0.0,
        )
        return self.output(attended.transpose(1, 2).reshape(batch, tokens, width))

    def forward(self, value: torch.Tensor) -> torch.Tensor:
        value = value + self.attention(value)
        return rms_norm(value + self.ffn(rms_norm(value)))


class LanguageModel(nn.Module):
    def __init__(self, spec: ModelSpec) -> None:
        super().__init__()
        spec.validate()
        self.spec = spec
        self.vocabulary = nn.Parameter(
            torch.randn(spec.vocab_size, spec.width) / math.sqrt(spec.width)
        )
        if spec.family == "block-kronecker":
            self.blocks = nn.ModuleList(
                BlockKroneckerLayer(spec, layer) for layer in range(spec.depth)
            )
        else:
            self.blocks = nn.ModuleList(
                BlockTransformerLayer(spec) for _ in range(spec.depth)
            )

    def hidden(self, token_ids: torch.Tensor) -> torch.Tensor:
        if token_ids.shape != (token_ids.shape[0], self.spec.context_length):
            raise ValueError(
                f"expected token ids shaped [batch,{self.spec.context_length}]"
            )
        value = F.embedding(token_ids, self.vocabulary)
        for block in self.blocks:
            value = block(value)
        return rms_norm(value)

    def forward(self, token_ids: torch.Tensor) -> torch.Tensor:
        return F.linear(self.hidden(token_ids), self.vocabulary)


def build_model(name: str, **overrides: Any) -> LanguageModel:
    if name not in SPECS:
        raise ValueError(f"unknown model: {name}")
    return LanguageModel(replace(SPECS[name], **overrides))


def model_inventory(model: LanguageModel) -> dict[str, Any]:
    vocabulary = model.vocabulary.numel()
    body = sum(
        parameter.numel()
        for name, parameter in model.named_parameters()
        if name != "vocabulary"
    )
    token_factor_parameters = sum(
        parameter.numel()
        for name, parameter in model.named_parameters()
        if any(
            marker in name
            for marker in (".outer.raw", ".workspace1.raw", ".workspace2.raw")
        )
    )
    return {
        "spec": asdict(model.spec),
        "vocabulary_parameters": vocabulary,
        "body_parameters": body,
        "total_parameters": vocabulary + body,
        "token_factor_parameters": token_factor_parameters,
        "nonlinear_residual_updates": 2 * model.spec.depth,
        "token_parameter_scaling": "O(depth*rank*(groups^2+workspace1^2+workspace2^2))",
    }
