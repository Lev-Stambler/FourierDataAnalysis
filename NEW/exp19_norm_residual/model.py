"""Corrected pre-norm residual models for the Exp19 conditioning audit.

The clean Kronecker candidate preserves the Exp17 block-causal contractions,
permutations, and rank-eight structured mixer.  It changes only the residual
shell and the rank-one group FFN gauges::

    x <- x + a * mixer(norm_1(x))
    x <- x + a * group_ffn(norm_2(x))

where ``a = 1 / sqrt(2 * depth)`` is fixed.  Consequently a layer with zero
branches is exactly the identity.  The rank-one group FFN has no input,
output, path-amplitude, or residual-gain parameters.

Named audit variants deliberately retain intermediate redundant scales.  They
are separate identities and must not be presented as the clean candidate.
"""

from __future__ import annotations

import math
from dataclasses import asdict, dataclass, replace
from typing import Any, Literal

import torch
import torch.nn.functional as F
from torch import nn
from torch.utils.checkpoint import checkpoint


LEGACY_POSTNORM_R1 = "legacy-exp17-postnorm-r1"
PRENORM_AFFINE_FREE_SCALED = "prenorm-affine-free-redundant-r1"
PRENORM_LEARNED_SCALED = "prenorm-learned-mixed-redundant-r1"
CLEAN_GROUP_R1_TOKEN = "prenorm-clean-token-r1"
CLEAN_GROUP_R1_JOINT = "prenorm-clean-joint-r1"
CORRECTED_NO_ROUTER_TOKEN = "prenorm-no-router-token-ffn"
CORRECTED_TRANSFORMER_DEEP = "prenorm-transformer-d32-w128"
CORRECTED_TRANSFORMER_WIDE = "prenorm-transformer-d3-w256"

AUDIT_MODEL_NAMES = (
    LEGACY_POSTNORM_R1,
    PRENORM_AFFINE_FREE_SCALED,
    PRENORM_LEARNED_SCALED,
    CLEAN_GROUP_R1_JOINT,
    CLEAN_GROUP_R1_TOKEN,
    CORRECTED_NO_ROUTER_TOKEN,
    CORRECTED_TRANSFORMER_DEEP,
    CORRECTED_TRANSFORMER_WIDE,
)
MODEL_NAMES = AUDIT_MODEL_NAMES

Family = Literal["group-kron", "token-ffn", "transformer"]
NormKind = Literal[
    "affine-free-token",
    "affine-free-joint",
    "learned-token",
    "learned-joint",
]


@dataclass(frozen=True)
class ModelSpec:
    name: str
    family: Family = "group-kron"
    context_length: int = 256
    vocab_size: int = 16_384
    width: int = 128
    depth: int = 32
    group_size: int = 16
    workspace1: int = 4
    workspace2: int = 4
    channel1: int = 8
    channel2: int = 16
    mixer_rank: int = 8
    hidden_workspace: int = 64
    hidden_channel: int = 250
    token_ffn_width: int = 0
    heads: int = 4
    transformer_ffn_width: int = 0
    transformer_ffn_plus_one_layers: int = 0
    norm_kind: NormKind = "learned-token"
    ffn_norm_kind: NormKind | None = None
    redundant_group_scales: bool = False
    learned_residual_gains: bool = False
    scale_embedding_residual: bool = True
    activation_checkpointing: bool = True

    @property
    def group_count(self) -> int:
        return self.context_length // self.group_size

    @property
    def branch_scale(self) -> float:
        return (2.0 * self.depth) ** -0.5

    def validate(self) -> None:
        if self.name not in AUDIT_MODEL_NAMES[1:]:
            raise ValueError(f"unknown Exp19 model: {self.name}")
        if self.family not in {"group-kron", "token-ffn", "transformer"}:
            raise ValueError(f"unknown family: {self.family}")
        if self.norm_kind not in {
            "affine-free-token",
            "affine-free-joint",
            "learned-token",
            "learned-joint",
        }:
            raise ValueError(f"unknown norm kind: {self.norm_kind}")
        if self.ffn_norm_kind is not None and self.ffn_norm_kind not in {
            "affine-free-token",
            "affine-free-joint",
            "learned-token",
            "learned-joint",
        }:
            raise ValueError(f"unknown FFN norm kind: {self.ffn_norm_kind}")
        if self.context_length % self.group_size:
            raise ValueError("context length must contain complete groups")
        if self.workspace1 * self.workspace2 != self.group_size:
            raise ValueError("workspace modes must multiply to group size")
        if min(self.width, self.depth) <= 0:
            raise ValueError("width and depth must be positive")
        if self.family != "transformer":
            if self.channel1 * self.channel2 != self.width:
                raise ValueError("channel modes must multiply to width")
            if self.mixer_rank <= 0:
                raise ValueError("mixer rank must be positive")
        if self.family == "group-kron" and min(
            self.hidden_workspace, self.hidden_channel
        ) <= 0:
            raise ValueError("group FFN hidden modes must be positive")
        if self.family == "token-ffn" and self.token_ffn_width <= 0:
            raise ValueError("token FFN width must be positive")
        if self.family == "transformer":
            if self.transformer_ffn_width <= 0:
                raise ValueError("Transformer FFN width must be positive")
            if self.width % self.heads or (self.width // self.heads) % 2:
                raise ValueError("RoPE requires an even integral head width")
            if not 0 <= self.transformer_ffn_plus_one_layers <= self.depth:
                raise ValueError("invalid Transformer mixed-width FFN schedule")
        if self.learned_residual_gains and not self.redundant_group_scales:
            raise ValueError("learned residual gains are audit-only redundant scales")


SPECS = {
    PRENORM_AFFINE_FREE_SCALED: ModelSpec(
        PRENORM_AFFINE_FREE_SCALED,
        norm_kind="affine-free-token",
        ffn_norm_kind="affine-free-joint",
        redundant_group_scales=True,
        learned_residual_gains=True,
        scale_embedding_residual=False,
    ),
    PRENORM_LEARNED_SCALED: ModelSpec(
        PRENORM_LEARNED_SCALED,
        ffn_norm_kind="learned-joint",
        redundant_group_scales=True,
        learned_residual_gains=True,
        scale_embedding_residual=False,
    ),
    CLEAN_GROUP_R1_TOKEN: ModelSpec(CLEAN_GROUP_R1_TOKEN),
    CLEAN_GROUP_R1_JOINT: ModelSpec(
        CLEAN_GROUP_R1_JOINT,
        norm_kind="learned-token",
        ffn_norm_kind="learned-joint",
    ),
    CORRECTED_NO_ROUTER_TOKEN: ModelSpec(
        CORRECTED_NO_ROUTER_TOKEN,
        family="token-ffn",
        token_ffn_width=258,
    ),
    CORRECTED_TRANSFORMER_DEEP: ModelSpec(
        CORRECTED_TRANSFORMER_DEEP,
        family="transformer",
        transformer_ffn_width=97,
        transformer_ffn_plus_one_layers=17,
    ),
    CORRECTED_TRANSFORMER_WIDE: ModelSpec(
        CORRECTED_TRANSFORMER_WIDE,
        family="transformer",
        width=256,
        depth=3,
        channel1=16,
        heads=8,
        transformer_ffn_width=181,
        transformer_ffn_plus_one_layers=2,
    ),
}


def _rms_scale(value: torch.Tensor, dimensions: tuple[int, ...], eps: float) -> torch.Tensor:
    """Compute the normalization statistic in FP32 even under autocast."""

    return torch.rsqrt(value.float().square().mean(dimensions, keepdim=True) + eps)


class LearnedRMSNorm(nn.Module):
    """Learned RMSNorm with tokenwise or 16-token joint statistics.

    Joint normalization shares one RMS statistic across workspace and channel
    axes, but its learned gamma remains channelwise.  This keeps the audit
    variants parameter matched while testing only the reduction domain.
    """

    def __init__(
        self,
        width: int,
        *,
        kind: NormKind = "learned-token",
        group_size: int = 16,
        eps: float = 1e-6,
    ) -> None:
        super().__init__()
        self.width = int(width)
        self.kind = kind
        self.group_size = int(group_size)
        self.eps = float(eps)
        if kind in {"affine-free-token", "affine-free-joint"}:
            self.register_parameter("weight", None)
        elif kind in {"learned-token", "learned-joint"}:
            self.weight = nn.Parameter(torch.ones(width))
        else:
            raise ValueError(f"unknown RMSNorm kind: {kind}")

    def forward(self, value: torch.Tensor) -> torch.Tensor:
        if value.shape[-1] != self.width:
            raise ValueError(f"expected final width {self.width}, got {value.shape[-1]}")
        original_shape = value.shape
        if self.kind.endswith("joint"):
            if value.ndim != 3 or value.shape[1] % self.group_size:
                raise ValueError("joint RMSNorm requires [batch,complete-groups,width]")
            grouped = value.reshape(
                value.shape[0], value.shape[1] // self.group_size, self.group_size, self.width
            )
            normalized = grouped * _rms_scale(grouped, (-2, -1), self.eps).to(
                grouped.dtype
            )
            normalized = normalized.reshape(original_shape)
        else:
            normalized = value * _rms_scale(value, (-1,), self.eps).to(value.dtype)
        if self.weight is not None:
            normalized = normalized * self.weight.to(normalized.dtype)
        return normalized


class RowNormalizedFactor(nn.Module):
    """Dense rank bank whose output rows have unit Euclidean norm."""

    def __init__(self, rank: int, width: int) -> None:
        super().__init__()
        self.rank = int(rank)
        self.width = int(width)
        self.raw = nn.Parameter(torch.randn(rank, width, width) / math.sqrt(width))

    def value(self) -> torch.Tensor:
        norm = self.raw.float().square().sum(-1, keepdim=True).sqrt().clamp_min(1e-8)
        return self.raw / norm.to(self.raw.dtype)


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
            self.rank, self.width, dtype=torch.float32, device=self.raw.device
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


class RouterFreeStructuredMixer(nn.Module):
    """The exact Exp17 rank-eight, block-causal structured contraction.

    Input normalization is intentionally absent: the enclosing residual layer
    owns pre-normalization, so there is one explicit normalization boundary.
    """

    def __init__(self, spec: ModelSpec, layer_index: int) -> None:
        super().__init__()
        self.spec = spec
        self.outer = PackedLowerFactor(spec.mixer_rank, spec.group_count)
        self.workspace1 = RowNormalizedFactor(spec.mixer_rank, spec.workspace1)
        self.workspace2 = RowNormalizedFactor(spec.mixer_rank, spec.workspace2)
        self.channel1 = RowNormalizedFactor(spec.mixer_rank, spec.channel1)
        self.channel2 = RowNormalizedFactor(spec.mixer_rank, spec.channel2)
        self.rank_amplitudes = nn.Parameter(
            torch.full((spec.mixer_rank,), spec.mixer_rank**-0.5)
        )
        workspace = affine_layout(spec.group_size, layer_index)
        channel = affine_layout(spec.width, layer_index)
        self.register_buffer("workspace_permutation", workspace, persistent=False)
        self.register_buffer(
            "inverse_workspace_permutation", torch.argsort(workspace), persistent=False
        )
        self.register_buffer("channel_permutation", channel, persistent=False)
        self.register_buffer(
            "inverse_channel_permutation", torch.argsort(channel), persistent=False
        )

    def rank_outputs(self, normalized: torch.Tensor) -> torch.Tensor:
        spec = self.spec
        batch, tokens, width = normalized.shape
        if (tokens, width) != (spec.context_length, spec.width):
            raise ValueError("structured mixer received an invalid hidden shape")
        activated = F.silu(normalized).reshape(
            batch, spec.group_count, spec.group_size, spec.width
        )
        activated = activated[:, :, self.workspace_permutation]
        activated = activated[..., self.channel_permutation]
        tensor = activated.reshape(
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
            batch, spec.mixer_rank, spec.group_count, spec.group_size, spec.width
        )
        ranked = ranked[:, :, :, self.inverse_workspace_permutation]
        ranked = ranked[..., self.inverse_channel_permutation]
        return ranked.reshape(batch, spec.mixer_rank, tokens, width)

    def materialized_rank_outputs(self, normalized: torch.Tensor) -> torch.Tensor:
        """Slow explicit Kronecker reference for numerical audits."""

        spec = self.spec
        batch = normalized.shape[0]
        activated = F.silu(normalized).reshape(
            batch, spec.group_count, spec.group_size, spec.width
        )
        activated = activated[:, :, self.workspace_permutation]
        activated = activated[..., self.channel_permutation]
        flattened = activated.reshape(batch, -1)
        outputs = []
        factors = (
            self.outer.value(),
            self.workspace1.value(),
            self.workspace2.value(),
            self.channel1.value(),
            self.channel2.value(),
        )
        for rank_index in range(spec.mixer_rank):
            matrix = factors[0][rank_index]
            for factor in factors[1:]:
                matrix = torch.kron(matrix, factor[rank_index])
            output = F.linear(flattened, matrix).reshape(
                batch, spec.group_count, spec.group_size, spec.width
            )
            output = output[:, :, self.inverse_workspace_permutation]
            output = output[..., self.inverse_channel_permutation]
            outputs.append(output.reshape(batch, spec.context_length, spec.width))
        return torch.stack(outputs, 1)

    def forward(self, normalized: torch.Tensor) -> torch.Tensor:
        return torch.einsum(
            "brtd,r->btd", self.rank_outputs(normalized), self.rank_amplitudes
        )


def _normalize_rows(factor: torch.Tensor) -> torch.Tensor:
    norm = factor.float().square().sum(-1, keepdim=True).sqrt().clamp_min(1e-8)
    return factor / norm.to(factor.dtype)


class CleanRankOneGroupSwiGLU(nn.Module):
    """Rank-one group SwiGLU with no redundant scale gauges."""

    def __init__(
        self, group_size: int, width: int, hidden_workspace: int, hidden_channel: int
    ) -> None:
        super().__init__()
        self.group_size = int(group_size)
        self.width = int(width)
        self.hidden_workspace = int(hidden_workspace)
        self.hidden_channel = int(hidden_channel)

        def factor(output: int, input_: int) -> nn.Parameter:
            return nn.Parameter(torch.randn(output, input_) / math.sqrt(input_))

        self.gate_workspace = factor(hidden_workspace, group_size)
        self.gate_channel = factor(hidden_channel, width)
        self.up_workspace = factor(hidden_workspace, group_size)
        self.up_channel = factor(hidden_channel, width)
        self.down_workspace = factor(group_size, hidden_workspace)
        self.down_channel = factor(width, hidden_channel)

    def hidden(self, normalized: torch.Tensor) -> torch.Tensor:
        gate = torch.einsum(
            "bgwd,hw,kd->bghk",
            normalized,
            _normalize_rows(self.gate_workspace),
            _normalize_rows(self.gate_channel),
        )
        up = torch.einsum(
            "bgwd,hw,kd->bghk",
            normalized,
            _normalize_rows(self.up_workspace),
            _normalize_rows(self.up_channel),
        )
        return F.silu(gate) * up

    def forward(self, normalized: torch.Tensor) -> torch.Tensor:
        return torch.einsum(
            "bghk,wh,dk->bgwd",
            self.hidden(normalized),
            _normalize_rows(self.down_workspace),
            _normalize_rows(self.down_channel),
        )


class RedundantRankOneGroupSwiGLU(nn.Module):
    """Audit-only rank-one FFN retaining the Exp17 scale gauges."""

    def __init__(
        self, group_size: int, width: int, hidden_workspace: int, hidden_channel: int
    ) -> None:
        super().__init__()
        self.group_size = int(group_size)
        self.width = int(width)
        self.hidden_workspace = int(hidden_workspace)
        self.hidden_channel = int(hidden_channel)

        def factor(output: int, input_: int) -> nn.Parameter:
            return nn.Parameter(torch.randn(1, output, input_) / math.sqrt(input_))

        self.gate_workspace = factor(hidden_workspace, group_size)
        self.gate_channel = factor(hidden_channel, width)
        self.up_workspace = factor(hidden_workspace, group_size)
        self.up_channel = factor(hidden_channel, width)
        self.down_workspace = factor(group_size, hidden_workspace)
        self.down_channel = factor(width, hidden_channel)
        self.path_amplitudes = nn.Parameter(torch.ones(1))
        self.input_channel_scale = nn.Parameter(torch.ones(width))
        self.output_channel_scale = nn.Parameter(torch.ones(width))

    def hidden(self, normalized: torch.Tensor) -> torch.Tensor:
        value = normalized * self.input_channel_scale
        gate = torch.einsum(
            "bgwd,rhw,rkd->bgrhk",
            value,
            _normalize_rows(self.gate_workspace),
            _normalize_rows(self.gate_channel),
        )
        up = torch.einsum(
            "bgwd,rhw,rkd->bgrhk",
            value,
            _normalize_rows(self.up_workspace),
            _normalize_rows(self.up_channel),
        )
        return F.silu(gate) * up

    def forward(self, normalized: torch.Tensor) -> torch.Tensor:
        ranked = torch.einsum(
            "bgrhk,rwh,rdk->bgrwd",
            self.hidden(normalized),
            _normalize_rows(self.down_workspace),
            _normalize_rows(self.down_channel),
        )
        output = torch.einsum("bgrwd,r->bgwd", ranked, self.path_amplitudes)
        return output * self.output_channel_scale


class CorrectedKronLayer(nn.Module):
    def __init__(self, spec: ModelSpec, layer_index: int) -> None:
        super().__init__()
        self.spec = spec
        self.mixer_norm = LearnedRMSNorm(
            spec.width, kind=spec.norm_kind, group_size=spec.group_size
        )
        self.ffn_norm = LearnedRMSNorm(
            spec.width,
            kind=spec.ffn_norm_kind or spec.norm_kind,
            group_size=spec.group_size,
        )
        self.mixer = RouterFreeStructuredMixer(spec, layer_index)
        if spec.family == "group-kron":
            group_type = (
                RedundantRankOneGroupSwiGLU
                if spec.redundant_group_scales
                else CleanRankOneGroupSwiGLU
            )
            self.group_ffn: nn.Module | None = group_type(
                spec.group_size,
                spec.width,
                spec.hidden_workspace,
                spec.hidden_channel,
            )
            self.token_ffn: nn.Module | None = None
        else:
            self.group_ffn = None
            self.token_ffn = SwiGLU(spec.width, spec.token_ffn_width)
        if spec.learned_residual_gains:
            self.mixer_gain = nn.Parameter(torch.tensor(spec.branch_scale))
            self.ffn_gain = nn.Parameter(torch.tensor(spec.branch_scale))
        else:
            self.register_parameter("mixer_gain", None)
            self.register_parameter("ffn_gain", None)

    @property
    def mixer_scale(self) -> float | torch.Tensor:
        return self.mixer_gain if self.mixer_gain is not None else self.spec.branch_scale

    @property
    def ffn_scale(self) -> float | torch.Tensor:
        return self.ffn_gain if self.ffn_gain is not None else self.spec.branch_scale

    def nonlinear_branch(self, normalized: torch.Tensor) -> torch.Tensor:
        if self.group_ffn is not None:
            spec = self.spec
            groups = normalized.reshape(
                normalized.shape[0],
                spec.group_count,
                spec.group_size,
                spec.width,
            )
            return self.group_ffn(groups).reshape_as(normalized)
        assert self.token_ffn is not None
        return self.token_ffn(normalized)

    def residual_updates(self, value: torch.Tensor) -> tuple[torch.Tensor, torch.Tensor]:
        mixer_update = self.mixer_scale * self.mixer(self.mixer_norm(value))
        mixed = value + mixer_update
        ffn_update = self.ffn_scale * self.nonlinear_branch(self.ffn_norm(mixed))
        return mixer_update, ffn_update

    def forward(self, value: torch.Tensor) -> torch.Tensor:
        mixer_update = self.mixer_scale * self.mixer(self.mixer_norm(value))
        value = value + mixer_update
        return value + self.ffn_scale * self.nonlinear_branch(self.ffn_norm(value))


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


class CorrectedTransformerLayer(nn.Module):
    """Block-causal learned-pre-norm Transformer with fixed branch scales."""

    def __init__(self, spec: ModelSpec, layer_index: int = 0) -> None:
        super().__init__()
        self.spec = spec
        self.attention_norm = LearnedRMSNorm(
            spec.width, kind=spec.norm_kind, group_size=spec.group_size
        )
        self.ffn_norm = LearnedRMSNorm(
            spec.width, kind=spec.norm_kind, group_size=spec.group_size
        )
        self.qkv = nn.Linear(spec.width, 3 * spec.width, bias=False)
        self.output = nn.Linear(spec.width, spec.width, bias=False)
        self.ffn_width = spec.transformer_ffn_width + int(
            layer_index < spec.transformer_ffn_plus_one_layers
        )
        self.ffn = SwiGLU(spec.width, self.ffn_width)
        groups = torch.arange(spec.context_length) // spec.group_size
        self.register_buffer(
            "block_mask", groups[:, None] >= groups[None, :], persistent=False
        )

    def attention(self, normalized: torch.Tensor) -> torch.Tensor:
        spec = self.spec
        batch, tokens, width = normalized.shape
        head_width = width // spec.heads
        query, key, content = self.qkv(normalized).chunk(3, -1)

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
        value = value + self.spec.branch_scale * self.attention(
            self.attention_norm(value)
        )
        return value + self.spec.branch_scale * self.ffn(self.ffn_norm(value))


class LanguageModel(nn.Module):
    def __init__(self, spec: ModelSpec) -> None:
        super().__init__()
        spec.validate()
        self.spec = spec
        self.vocabulary = nn.Parameter(
            torch.randn(spec.vocab_size, spec.width) / math.sqrt(spec.width)
        )
        block_type = (
            CorrectedTransformerLayer
            if spec.family == "transformer"
            else CorrectedKronLayer
        )
        self.blocks = nn.ModuleList(block_type(spec, index) for index in range(spec.depth))
        self.final_norm = LearnedRMSNorm(
            spec.width, kind=spec.norm_kind, group_size=spec.group_size
        )

    def hidden(self, token_ids: torch.Tensor) -> torch.Tensor:
        if token_ids.ndim != 2 or token_ids.shape[1] != self.spec.context_length:
            raise ValueError(
                f"expected token IDs shaped [batch,{self.spec.context_length}]"
            )
        value = F.embedding(token_ids, self.vocabulary)
        if self.spec.scale_embedding_residual:
            # The tied vocabulary is initialized at RMS 1/sqrt(width).  Lift
            # only its residual-stream use to RMS~1 so the fixed branch scale
            # has the intended conditioning; the output projection remains
            # tied to the unscaled vocabulary parameter.
            value = value * math.sqrt(self.spec.width)
        for block in self.blocks:
            value = (
                checkpoint(block, value, use_reentrant=False)
                if (
                    self.training
                    and torch.is_grad_enabled()
                    and self.spec.activation_checkpointing
                )
                else block(value)
            )
        return self.final_norm(value)

    def forward(self, token_ids: torch.Tensor) -> torch.Tensor:
        return F.linear(self.hidden(token_ids), self.vocabulary)


def build_legacy_model(
    *, activation_checkpointing: bool | None = None, **overrides: Any
) -> nn.Module:
    """Build the immutable Exp17 model through its original implementation."""

    from exp17_group_density.model import GROUP_R1, build_model as build_exp17

    if activation_checkpointing is not None:
        overrides["activation_checkpointing"] = activation_checkpointing
    return build_exp17(GROUP_R1, **overrides)


def build_model(
    name: str, *, activation_checkpointing: bool | None = None, **overrides: Any
) -> nn.Module:
    if name == LEGACY_POSTNORM_R1:
        return build_legacy_model(
            activation_checkpointing=activation_checkpointing, **overrides
        )
    if name not in SPECS:
        raise ValueError(f"unknown Exp19 model: {name}")
    if activation_checkpointing is not None:
        overrides["activation_checkpointing"] = activation_checkpointing
    return LanguageModel(replace(SPECS[name], **overrides))


def model_inventory(model: nn.Module) -> dict[str, Any]:
    """Return exact counts without pretending audit intermediates are matched."""

    if not isinstance(model, LanguageModel):
        from exp17_group_density.model import model_inventory as legacy_inventory

        result = dict(legacy_inventory(model))
        result["architecture_identity"] = LEGACY_POSTNORM_R1
        result["implementation"] = "external-frozen-exp17"
        return result
    named = list(model.named_parameters())
    vocabulary = model.vocabulary.numel()
    total = sum(parameter.numel() for _, parameter in named)
    norms = sum(
        parameter.numel()
        for name, parameter in named
        if name.endswith("_norm.weight") or name == "final_norm.weight"
    )
    group_ffn = sum(
        parameter.numel() for name, parameter in named if ".group_ffn." in name
    )
    token_ffn = sum(
        parameter.numel() for name, parameter in named if ".token_ffn." in name
    )
    transformer = sum(
        parameter.numel()
        for name, parameter in named
        if any(marker in name for marker in (".qkv.", ".output.", ".ffn."))
        and model.spec.family == "transformer"
    )
    redundant = sum(
        parameter.numel()
        for name, parameter in named
        if any(
            marker in name
            for marker in (
                "path_amplitudes",
                "input_channel_scale",
                "output_channel_scale",
                "mixer_gain",
                "ffn_gain",
            )
        )
        and ".mixer.rank_amplitudes" not in name
    )
    return {
        "architecture_identity": model.spec.name,
        "implementation": "exp19",
        "spec": asdict(model.spec),
        "vocabulary_parameters": vocabulary,
        "body_parameters": total - vocabulary,
        "total_parameters": total,
        "norm_parameters": norms,
        "group_ffn_parameters": group_ffn,
        "token_ffn_parameters": token_ffn,
        "transformer_block_parameters": transformer,
        "redundant_scale_parameters": redundant,
        "fixed_branch_scale": model.spec.branch_scale,
        "residual_topology": "true-pre-norm-identity",
    }


def _correctness_overrides(name: str) -> dict[str, Any]:
    """Small shape preserving every relevant factorization and causal axis."""

    values: dict[str, Any] = {
        "context_length": 8,
        "vocab_size": 32,
        "width": 8,
        "depth": 2,
        "group_size": 4,
        "workspace1": 2,
        "workspace2": 2,
        "channel1": 2,
        "channel2": 4,
        "mixer_rank": 2,
        "hidden_workspace": 5,
        "hidden_channel": 6,
        "heads": 2,
        "activation_checkpointing": False,
    }
    if name == CORRECTED_NO_ROUTER_TOKEN:
        values["token_ffn_width"] = 7
    if name in {CORRECTED_TRANSFORMER_DEEP, CORRECTED_TRANSFORMER_WIDE}:
        values["transformer_ffn_width"] = 7
        values["transformer_ffn_plus_one_layers"] = 0
    if name == LEGACY_POSTNORM_R1:
        values.pop("heads")
    return values


def correctness_checks(name: str, device: torch.device) -> dict[str, Any]:
    """Numerically audit contractions, causality, and residual identity.

    Full-size materialization would create a 32768-square matrix, so the check
    uses a reduced model with the same five contraction axes and two causal
    groups.  Runtime parity for the full model is checked separately by the
    campaign's eager/checkpointed/compiled gate.
    """

    torch.manual_seed(1900)
    model = build_model(name, **_correctness_overrides(name)).to(device=device)
    model = model.to(dtype=torch.float64).eval()
    contraction: dict[str, Any] = {"applicable": False, "pass": True}
    if isinstance(model, LanguageModel) and isinstance(model.blocks[0], CorrectedKronLayer):
        mixer = model.blocks[0].mixer
        normalized = torch.randn(
            2,
            model.spec.context_length,
            model.spec.width,
            device=device,
            dtype=torch.float64,
            requires_grad=True,
        )
        factored = torch.einsum(
            "brtd,r->btd", mixer.rank_outputs(normalized), mixer.rank_amplitudes
        )
        explicit = torch.einsum(
            "brtd,r->btd",
            mixer.materialized_rank_outputs(normalized),
            mixer.rank_amplitudes,
        )
        probe = torch.randn_like(factored)
        parameters = [normalized, *mixer.parameters()]
        factored_gradients = torch.autograd.grad(
            factored, parameters, probe, retain_graph=True, allow_unused=False
        )
        explicit_gradients = torch.autograd.grad(
            explicit, parameters, probe, allow_unused=False
        )
        forward_error = float((factored - explicit).detach().abs().max())
        backward_error = max(
            float((left - right).detach().abs().max())
            for left, right in zip(factored_gradients, explicit_gradients, strict=True)
        )
        contraction = {
            "applicable": True,
            "forward_max_absolute_error": forward_error,
            "backward_max_absolute_error": backward_error,
            "tolerance": 1e-8,
            "pass": forward_error <= 1e-8 and backward_error <= 1e-8,
        }

    value = torch.randn(
        2,
        model.spec.context_length,
        model.spec.width,
        device=device,
        dtype=torch.float64,
        requires_grad=True,
    )
    hidden = value
    for block in model.blocks:
        hidden = block(hidden)
    if isinstance(model, LanguageModel):
        hidden = model.final_norm(hidden)
    else:
        from exp14_block_kronecker.model import rms_norm

        hidden = rms_norm(hidden)
    future_gradient = torch.autograd.grad(
        hidden[:, : model.spec.group_size].square().sum(), value
    )[0][:, model.spec.group_size :]
    future_gradient_error = float(future_gradient.abs().max())
    causality = {
        "future_group_gradient_max_absolute": future_gradient_error,
        "tolerance": 1e-8,
        "pass": future_gradient_error <= 1e-8,
    }

    identity: dict[str, Any]
    if isinstance(model, LanguageModel):
        block = model.blocks[0]
        with torch.no_grad():
            for parameter_name, parameter in block.named_parameters():
                if "norm.weight" not in parameter_name:
                    parameter.zero_()
        identity_input = torch.randn(
            2,
            model.spec.context_length,
            model.spec.width,
            device=device,
            dtype=torch.float64,
        )
        identity_error = float(
            (block(identity_input) - identity_input).detach().abs().max()
        )
        identity = {
            "applicable": True,
            "max_absolute_error": identity_error,
            "tolerance": 1e-6,
            "pass": identity_error <= 1e-6,
        }
    else:
        identity = {
            "applicable": False,
            "pass": True,
            "reason": "frozen legacy post-norm reference is intentionally non-identity",
        }
    passed = bool(contraction["pass"] and causality["pass"] and identity["pass"])
    return {
        "pass": passed,
        "reduced_shape": _correctness_overrides(name),
        "factored_materialized_contraction": contraction,
        "block_causality": causality,
        "zero_branch_identity": identity,
    }
