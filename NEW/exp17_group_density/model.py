"""Router-free block-causal KronMix with nonlinear 16-token workspaces.

The candidate replaces a token-local SwiGLU with a parameter-efficient dense
map over one complete noncausal workspace.  For one source group
``X in R[workspace,width]`` and group-FFN path ``r`` it computes

    H_r = silu(A_gate_r X B_gate_r^T) * (A_up_r X B_up_r^T)
    Y   = sum_r A_down_r H_r B_down_r^T.

Every hidden feature therefore depends on every token and channel in its
16-token source group, while factor storage grows with a sum of mode sizes.
The outer group operator remains inclusive block causal.
"""

from __future__ import annotations

import math
from dataclasses import asdict, dataclass, replace
from typing import Any, Literal

import torch
import torch.nn.functional as F
from torch import nn
from torch.utils.checkpoint import checkpoint

from exp14_block_kronecker.model import (
    PackedLowerFactor,
    RowNormalizedFactor,
    SwiGLU,
    affine_layout,
    rms_norm,
)


CURRENT_ROUTED = "current-routed-r8"
NO_ROUTER_TOKEN = "no-router-token"
DENSE_GROUP = "dense-group"
GROUP_R1 = "group-kron-r1"
GROUP_R2 = "group-kron-r2"
GROUP_R4 = "group-kron-r4"
GROUP_HYBRID = "group-kron-hybrid"
GROUP_DEEP = "group-kron-deep"
MODEL_NAMES = (
    NO_ROUTER_TOKEN,
    DENSE_GROUP,
    GROUP_R1,
    GROUP_R2,
    GROUP_R4,
    GROUP_HYBRID,
    GROUP_DEEP,
)

OuterKind = Literal["dense", "prefix", "toeplitz"]
FfnKind = Literal["token", "dense-group", "kron-group", "hybrid"]


@dataclass(frozen=True)
class ModelSpec:
    name: str
    context_length: int = 256
    vocab_size: int = 16_384
    width: int = 128
    depth: int = 32
    token_ffn_width: int = 0
    group_size: int = 16
    workspace1: int = 4
    workspace2: int = 4
    channel1: int = 8
    channel2: int = 16
    mixer_rank: int = 8
    group_rank: int = 0
    hidden_workspace: int = 0
    hidden_channel: int = 0
    dense_group_width: int = 0
    outer_kind: OuterKind = "dense"
    ffn_kind: FfnKind = "token"
    local_decoder_layers: int = 0
    local_decoder_heads: int = 4
    local_decoder_ffn_width: int = 256
    activation_checkpointing: bool = True

    @property
    def group_count(self) -> int:
        return self.context_length // self.group_size

    def validate(self) -> None:
        if self.name not in MODEL_NAMES:
            raise ValueError(f"unknown group-density model: {self.name}")
        if self.context_length % self.group_size:
            raise ValueError("context length must contain complete groups")
        if self.workspace1 * self.workspace2 != self.group_size:
            raise ValueError("workspace modes must multiply to group size")
        if self.channel1 * self.channel2 != self.width:
            raise ValueError("channel modes must multiply to model width")
        if self.outer_kind not in {"dense", "prefix", "toeplitz"}:
            raise ValueError(f"unknown outer factor: {self.outer_kind}")
        if self.ffn_kind not in {"token", "dense-group", "kron-group", "hybrid"}:
            raise ValueError(f"unknown FFN kind: {self.ffn_kind}")
        if min(self.width, self.depth, self.mixer_rank) <= 0:
            raise ValueError("width, depth, and mixer rank must be positive")
        if self.ffn_kind in {"token", "hybrid"} and self.token_ffn_width <= 0:
            raise ValueError("token/hybrid FFN requires token_ffn_width")
        if self.ffn_kind == "dense-group" and self.dense_group_width <= 0:
            raise ValueError("dense group FFN requires a hidden width")
        if self.ffn_kind in {"kron-group", "hybrid"} and min(
            self.group_rank, self.hidden_workspace, self.hidden_channel
        ) <= 0:
            raise ValueError("Kronecker group FFN dimensions must be positive")
        if self.local_decoder_layers and (
            self.width % self.local_decoder_heads
            or (self.width // self.local_decoder_heads) % 2
        ):
            raise ValueError("local decoder requires an even integral head width")


SPECS = {
    NO_ROUTER_TOKEN: ModelSpec(
        NO_ROUTER_TOKEN,
        token_ffn_width=259,
        ffn_kind="token",
    ),
    DENSE_GROUP: ModelSpec(
        DENSE_GROUP,
        dense_group_width=16,
        ffn_kind="dense-group",
    ),
    GROUP_R1: ModelSpec(
        GROUP_R1,
        group_rank=1,
        hidden_workspace=64,
        hidden_channel=250,
        ffn_kind="kron-group",
    ),
    GROUP_R2: ModelSpec(
        GROUP_R2,
        group_rank=2,
        hidden_workspace=64,
        hidden_channel=121,
        ffn_kind="kron-group",
    ),
    GROUP_R4: ModelSpec(
        GROUP_R4,
        group_rank=4,
        hidden_workspace=68,
        hidden_channel=56,
        ffn_kind="kron-group",
    ),
    GROUP_HYBRID: ModelSpec(
        GROUP_HYBRID,
        token_ffn_width=128,
        group_rank=1,
        hidden_workspace=32,
        hidden_channel=126,
        ffn_kind="hybrid",
    ),
    GROUP_DEEP: ModelSpec(
        GROUP_DEEP,
        width=80,
        depth=64,
        channel1=8,
        channel2=10,
        group_rank=1,
        hidden_workspace=67,
        hidden_channel=241,
        ffn_kind="kron-group",
        local_decoder_heads=5,
        local_decoder_ffn_width=160,
    ),
}


def group_rms_norm(value: torch.Tensor) -> torch.Tensor:
    """RMS-normalize jointly across workspace and channel axes."""

    scale = torch.rsqrt(value.float().square().mean((-2, -1), keepdim=True) + 1e-6)
    return value * scale.to(value.dtype)


class DilatedCausalPrefixFactor(nn.Module):
    """Product of sparse causal offset factors with global prefix reach."""

    def __init__(self, rank: int, groups: int) -> None:
        super().__init__()
        self.rank = int(rank)
        self.groups = int(groups)
        self.offsets = tuple(1 << index for index in range(math.ceil(math.log2(groups))))
        self.raw = nn.ParameterList(
            nn.Parameter(torch.randn(rank, groups - offset) / math.sqrt(2.0))
            for offset in self.offsets
        )

    @staticmethod
    def _coefficient(raw: torch.Tensor) -> torch.Tensor:
        return torch.tanh(raw)

    def apply(self, value: torch.Tensor) -> torch.Tensor:
        if value.shape[1:3] != (self.rank, self.groups):
            raise ValueError("prefix input must be [batch,rank,groups,...]")
        output = value
        for offset, raw in zip(self.offsets, self.raw, strict=True):
            coefficient = self._coefficient(raw)
            shape = (1, self.rank, self.groups - offset) + (1,) * (value.ndim - 3)
            mixed = (
                output[:, :, offset:]
                + coefficient.reshape(shape) * output[:, :, :-offset]
            ) / torch.sqrt(1.0 + coefficient.square()).reshape(shape)
            output = torch.cat((output[:, :, :offset], mixed), dim=2)
        return output

    def matrix(self) -> torch.Tensor:
        matrix = torch.eye(self.groups, device=self.raw[0].device, dtype=self.raw[0].dtype)
        matrix = matrix.expand(self.rank, -1, -1).clone()
        for offset, raw in zip(self.offsets, self.raw, strict=True):
            coefficient = self._coefficient(raw)
            factor = torch.eye(
                self.groups, device=raw.device, dtype=raw.dtype
            ).expand(self.rank, -1, -1).clone()
            rows = torch.arange(offset, self.groups, device=raw.device)
            norm = torch.sqrt(1.0 + coefficient.square())
            factor[:, rows, rows] = 1.0 / norm
            factor[:, rows, rows - offset] = coefficient / norm
            matrix = torch.bmm(factor, matrix)
        return matrix


class CausalToeplitzFactor(nn.Module):
    """Row-normalized lower-triangular Toeplitz rank bank."""

    def __init__(self, rank: int, groups: int) -> None:
        super().__init__()
        self.rank = int(rank)
        self.groups = int(groups)
        self.raw = nn.Parameter(torch.randn(rank, groups) / math.sqrt(groups))

    def kernel_and_norm(self) -> tuple[torch.Tensor, torch.Tensor]:
        kernel = self.raw
        prefix = kernel.float().square().cumsum(-1).sqrt().clamp_min(1e-8)
        return kernel, prefix.to(kernel.dtype)

    def matrix(self) -> torch.Tensor:
        kernel, prefix = self.kernel_and_norm()
        rows = torch.arange(self.groups, device=kernel.device)[:, None]
        columns = torch.arange(self.groups, device=kernel.device)[None, :]
        lag = rows - columns
        valid = lag >= 0
        lag = lag.clamp_min(0)
        values = kernel[:, lag] / prefix[:, rows]
        return values * valid[None]

    def apply(self, value: torch.Tensor) -> torch.Tensor:
        if value.shape[1:3] != (self.rank, self.groups):
            raise ValueError("Toeplitz input must be [batch,rank,groups,...]")
        # The direct path is faster for the approved 16/32/64-group ladder.
        return torch.einsum("rsg,brg...->brs...", self.matrix(), value)


class DenseCausalFactor(nn.Module):
    def __init__(self, rank: int, groups: int) -> None:
        super().__init__()
        self.factor = PackedLowerFactor(rank, groups)

    def matrix(self) -> torch.Tensor:
        return self.factor.value()

    def apply(self, value: torch.Tensor) -> torch.Tensor:
        return torch.einsum("rsg,brg...->brs...", self.matrix(), value)


def build_outer(kind: OuterKind, rank: int, groups: int) -> nn.Module:
    if kind == "dense":
        return DenseCausalFactor(rank, groups)
    if kind == "prefix":
        return DilatedCausalPrefixFactor(rank, groups)
    if kind == "toeplitz":
        return CausalToeplitzFactor(rank, groups)
    raise ValueError(f"unknown outer factor: {kind}")


class DenseGroupSwiGLU(nn.Module):
    def __init__(self, group_size: int, width: int, hidden_width: int) -> None:
        super().__init__()
        self.group_size = int(group_size)
        self.width = int(width)
        flattened = group_size * width
        self.gate_up = nn.Linear(flattened, 2 * hidden_width, bias=False)
        self.down = nn.Linear(hidden_width, flattened, bias=False)
        # This active group preconditioner consumes exactly the 1,024
        # parameters/layer freed by removing the old width-128 rank-8 router.
        # It strengthens the literal-dense control without reintroducing a
        # source/destination routing mechanism.
        self.input_channel_scale = nn.Parameter(torch.ones(width))
        self.output_channel_scale = nn.Parameter(torch.ones(width))
        self.workspace_preconditioner = nn.Parameter(
            torch.randn(group_size, group_size) / math.sqrt(group_size)
        )
        self.channel_lowrank_left = nn.Parameter(
            torch.randn(width, 2) / math.sqrt(width)
        )
        self.channel_lowrank_right = nn.Parameter(
            torch.randn(width, 2) / math.sqrt(2)
        )

    def forward(self, value: torch.Tensor) -> torch.Tensor:
        batch, groups, workspace, width = value.shape
        if (workspace, width) != (self.group_size, self.width):
            raise ValueError("dense group FFN received an invalid group shape")
        normalized = group_rms_norm(value) * self.input_channel_scale
        normalized = normalized + 0.1 * torch.einsum(
            "xy,bgyd->bgxd", self.workspace_preconditioner, normalized
        )
        projected = torch.einsum(
            "bgwd,dk,ek->bgwe",
            normalized,
            self.channel_lowrank_left,
            self.channel_lowrank_right,
        )
        flattened = (normalized + projected / math.sqrt(2.0)).flatten(2)
        gate, up = self.gate_up(flattened).chunk(2, -1)
        output = self.down(F.silu(gate) * up).reshape(
            batch, groups, workspace, width
        )
        return output * self.output_channel_scale


class KroneckerGroupSwiGLU(nn.Module):
    def __init__(
        self,
        group_size: int,
        width: int,
        rank: int,
        hidden_workspace: int,
        hidden_channel: int,
    ) -> None:
        super().__init__()
        self.group_size = int(group_size)
        self.width = int(width)
        self.rank = int(rank)
        self.hidden_workspace = int(hidden_workspace)
        self.hidden_channel = int(hidden_channel)

        def factor(output: int, input_: int) -> nn.Parameter:
            return nn.Parameter(torch.randn(rank, output, input_) / math.sqrt(input_))

        self.gate_workspace = factor(hidden_workspace, group_size)
        self.gate_channel = factor(hidden_channel, width)
        self.up_workspace = factor(hidden_workspace, group_size)
        self.up_channel = factor(hidden_channel, width)
        self.down_workspace = factor(group_size, hidden_workspace)
        self.down_channel = factor(width, hidden_channel)
        self.path_amplitudes = nn.Parameter(torch.full((rank,), rank**-0.5))
        self.input_channel_scale = nn.Parameter(torch.ones(width))
        self.output_channel_scale = nn.Parameter(torch.ones(width))

    @staticmethod
    def normalized(factor: torch.Tensor) -> torch.Tensor:
        norm = factor.float().square().sum(-1, keepdim=True).sqrt().clamp_min(1e-8)
        return factor / norm.to(factor.dtype)

    def hidden(self, value: torch.Tensor) -> torch.Tensor:
        normalized = group_rms_norm(value) * self.input_channel_scale
        gate = torch.einsum(
            "bgwd,rhw,rkd->bgrhk",
            normalized,
            self.normalized(self.gate_workspace),
            self.normalized(self.gate_channel),
        )
        up = torch.einsum(
            "bgwd,rhw,rkd->bgrhk",
            normalized,
            self.normalized(self.up_workspace),
            self.normalized(self.up_channel),
        )
        return F.silu(gate) * up

    def forward(self, value: torch.Tensor) -> torch.Tensor:
        hidden = self.hidden(value)
        ranked = torch.einsum(
            "bgrhk,rwh,rdk->bgrwd",
            hidden,
            self.normalized(self.down_workspace),
            self.normalized(self.down_channel),
        )
        output = torch.einsum("bgrwd,r->bgwd", ranked, self.path_amplitudes)
        return output * self.output_channel_scale


class RouterFreeStructuredMixer(nn.Module):
    def __init__(self, spec: ModelSpec, layer_index: int) -> None:
        super().__init__()
        self.spec = spec
        self.outer = build_outer(spec.outer_kind, spec.mixer_rank, spec.group_count)
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

    def rank_outputs(self, value: torch.Tensor) -> torch.Tensor:
        spec = self.spec
        batch, tokens, width = value.shape
        if (tokens, width) != (spec.context_length, spec.width):
            raise ValueError("structured mixer received an invalid hidden shape")
        normalized = F.silu(rms_norm(value)).reshape(
            batch, spec.group_count, spec.group_size, spec.width
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
        groups = self.outer.apply(workspace)
        ranked = groups.reshape(
            batch,
            spec.mixer_rank,
            spec.group_count,
            spec.group_size,
            spec.width,
        )
        ranked = ranked[:, :, :, self.inverse_workspace_permutation]
        ranked = ranked[..., self.inverse_channel_permutation]
        return ranked.reshape(batch, spec.mixer_rank, tokens, width)

    def forward(self, value: torch.Tensor) -> torch.Tensor:
        ranked = self.rank_outputs(value)
        return torch.einsum("brtd,r->btd", ranked, self.rank_amplitudes)


class GroupDensityLayer(nn.Module):
    def __init__(self, spec: ModelSpec, layer_index: int) -> None:
        super().__init__()
        self.spec = spec
        self.mixer = RouterFreeStructuredMixer(spec, layer_index)
        self.token_ffn = (
            SwiGLU(spec.width, spec.token_ffn_width)
            if spec.ffn_kind in {"token", "hybrid"}
            else None
        )
        self.dense_group_ffn = (
            DenseGroupSwiGLU(spec.group_size, spec.width, spec.dense_group_width)
            if spec.ffn_kind == "dense-group"
            else None
        )
        self.group_ffn = (
            KroneckerGroupSwiGLU(
                spec.group_size,
                spec.width,
                spec.group_rank,
                spec.hidden_workspace,
                spec.hidden_channel,
            )
            if spec.ffn_kind in {"kron-group", "hybrid"}
            else None
        )
        initial_gain = (2.0 * spec.depth) ** -0.5
        self.mixer_gain = nn.Parameter(torch.tensor(initial_gain))
        self.ffn_gain = nn.Parameter(torch.tensor(initial_gain))

    def nonlinear_branch(self, value: torch.Tensor) -> torch.Tensor:
        spec = self.spec
        groups = value.reshape(
            value.shape[0], spec.group_count, spec.group_size, spec.width
        )
        output = torch.zeros_like(value)
        if self.token_ffn is not None:
            output = output + self.token_ffn(rms_norm(value))
        if self.dense_group_ffn is not None:
            output = output + self.dense_group_ffn(groups).reshape_as(value)
        if self.group_ffn is not None:
            output = output + self.group_ffn(groups).reshape_as(value)
        return output

    def forward(self, value: torch.Tensor) -> torch.Tensor:
        value = rms_norm(value + self.mixer_gain * self.mixer(value))
        return rms_norm(value + self.ffn_gain * self.nonlinear_branch(value))


class LocalDecoderBlock(nn.Module):
    def __init__(self, width: int, heads: int, ffn_width: int) -> None:
        super().__init__()
        self.attention = nn.MultiheadAttention(width, heads, bias=False, batch_first=True)
        self.ffn = SwiGLU(width, ffn_width)

    def forward(self, value: torch.Tensor, causal_mask: torch.Tensor) -> torch.Tensor:
        normalized = rms_norm(value)
        attended, _ = self.attention(
            normalized, normalized, normalized, attn_mask=causal_mask, need_weights=False
        )
        value = rms_norm(value + attended)
        return rms_norm(value + self.ffn(rms_norm(value)))


class LocalGroupDecoder(nn.Module):
    """Shared causal decoder for ordinary likelihood inside target groups."""

    def __init__(self, spec: ModelSpec) -> None:
        super().__init__()
        self.spec = spec
        self.group_bos = nn.Parameter(torch.randn(spec.width) / math.sqrt(spec.width))
        self.blocks = nn.ModuleList(
            LocalDecoderBlock(
                spec.width, spec.local_decoder_heads, spec.local_decoder_ffn_width
            )
            for _ in range(spec.local_decoder_layers)
        )
        mask = torch.triu(
            torch.ones(spec.group_size, spec.group_size, dtype=torch.bool), diagonal=1
        )
        self.register_buffer("causal_mask", mask, persistent=False)

    def forward(
        self,
        context: torch.Tensor,
        target_tokens: torch.Tensor,
        vocabulary: torch.Tensor,
    ) -> torch.Tensor:
        spec = self.spec
        expected = (context.shape[0], spec.group_count, spec.group_size)
        if target_tokens.shape != expected or context.shape[1:] != (
            spec.group_count,
            spec.group_size,
            spec.width,
        ):
            raise ValueError("local decoder target/context shape mismatch")
        embedded = F.embedding(target_tokens, vocabulary)
        bos = self.group_bos.reshape(1, 1, 1, -1).expand(
            context.shape[0], spec.group_count, 1, -1
        )
        shifted = torch.cat((bos, embedded[:, :, :-1]), dim=2)
        value = (context + shifted).flatten(0, 1)
        for block in self.blocks:
            value = block(value, self.causal_mask)
        return value.reshape_as(context)


class LanguageModel(nn.Module):
    def __init__(self, spec: ModelSpec) -> None:
        super().__init__()
        spec.validate()
        self.spec = spec
        self.vocabulary = nn.Parameter(
            torch.randn(spec.vocab_size, spec.width) / math.sqrt(spec.width)
        )
        self.blocks = nn.ModuleList(
            GroupDensityLayer(spec, index) for index in range(spec.depth)
        )
        self.local_decoder = (
            LocalGroupDecoder(spec) if spec.local_decoder_layers else None
        )

    def hidden(self, token_ids: torch.Tensor) -> torch.Tensor:
        if token_ids.ndim != 2 or token_ids.shape[1] != self.spec.context_length:
            raise ValueError(
                f"expected token IDs shaped [batch,{self.spec.context_length}]"
            )
        value = F.embedding(token_ids, self.vocabulary)
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
        return rms_norm(value)

    def forward(self, token_ids: torch.Tensor) -> torch.Tensor:
        return F.linear(self.hidden(token_ids), self.vocabulary)

    def hierarchical_hidden(
        self, source_ids: torch.Tensor, target_groups: torch.Tensor
    ) -> torch.Tensor:
        if self.local_decoder is None:
            raise RuntimeError("this model has no local causal decoder")
        spec = self.spec
        context = self.hidden(source_ids).reshape(
            source_ids.shape[0], spec.group_count, spec.group_size, spec.width
        )
        return self.local_decoder(context, target_groups, self.vocabulary)

    def hierarchical_logits(
        self, source_ids: torch.Tensor, target_groups: torch.Tensor
    ) -> torch.Tensor:
        return F.linear(
            self.hierarchical_hidden(source_ids, target_groups), self.vocabulary
        )


def build_model(name: str, **overrides: Any) -> LanguageModel:
    if name not in SPECS:
        raise ValueError(f"unknown group-density model: {name}")
    return LanguageModel(replace(SPECS[name], **overrides))


def nonlinear_activation_sites(spec: ModelSpec) -> int:
    token = (
        spec.depth * spec.context_length * spec.token_ffn_width
        if spec.ffn_kind in {"token", "hybrid"}
        else 0
    )
    group = (
        spec.depth
        * spec.group_count
        * spec.group_rank
        * spec.hidden_workspace
        * spec.hidden_channel
        if spec.ffn_kind in {"kron-group", "hybrid"}
        else 0
    )
    dense = (
        spec.depth * spec.group_count * spec.dense_group_width
        if spec.ffn_kind == "dense-group"
        else 0
    )
    return token + group + dense


def model_inventory(model: LanguageModel) -> dict[str, Any]:
    spec = model.spec
    named = list(model.named_parameters())
    vocabulary = model.vocabulary.numel()
    decoder = sum(
        parameter.numel()
        for name, parameter in named
        if name.startswith("local_decoder.")
    )
    group_ffn = sum(
        parameter.numel()
        for name, parameter in named
        if ".group_ffn." in name or ".dense_group_ffn." in name
    )
    token_ffn = sum(
        parameter.numel() for name, parameter in named if ".token_ffn." in name
    )
    outer = sum(
        parameter.numel()
        for name, parameter in named
        if ".mixer.outer." in name
    )
    total = sum(parameter.numel() for _, parameter in named)
    return {
        "spec": asdict(spec),
        "vocabulary_parameters": vocabulary,
        "body_parameters": total - vocabulary,
        "total_parameters": total,
        "active_parameters": sum(
            parameter.numel() for _, parameter in named if parameter.requires_grad
        ),
        "token_ffn_parameters": token_ffn,
        "group_ffn_parameters": group_ffn,
        "outer_factor_parameters": outer,
        "local_decoder_parameters": decoder,
        "nonlinear_activation_sites_per_example": nonlinear_activation_sites(spec),
        "group_size": spec.group_size,
        "group_count": spec.group_count,
        "outer_parameter_scaling": {
            "dense": "O(depth*mixer_rank*groups^2)",
            "prefix": "O(depth*mixer_rank*groups*log(groups))",
            "toeplitz": "O(depth*mixer_rank*groups)",
        }[spec.outer_kind],
    }
