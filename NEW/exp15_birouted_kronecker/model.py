"""Mechanistic Exp15 variants of the block-causal Kronecker mixer.

The central experiment separates routing at the source of a structured linear
map (a key-like gate) from routing at its destination (a query-like gate):

    y_s = sum_r q_r(x_s) [T_r(k_r(x) C_r x)]_s.

The gates are token-local and ``T_r`` is block causal, so source routing does
not alter the information boundary or introduce a quadratic token interaction.
"""

from __future__ import annotations

import math
from dataclasses import asdict, dataclass, replace
from typing import Any

import torch
import torch.nn.functional as F
from torch import nn

from exp14_block_kronecker.model import (
    PackedLowerFactor,
    RowNormalizedFactor,
    SwiGLU,
    affine_layout,
    rms_norm,
)


CURRENT_R8 = "current-r8"
FFN_ONLY = "ffn-only"
SOURCE_R8 = "source-r8"
BI_R8 = "bi-r8"
DECOUPLED_R8 = "decoupled-r8"
BI_DECOUPLED_R8 = "bi-decoupled-r8"
DENSE_WORKSPACE_R8 = "dense-workspace-r8"
BI_R12 = "bi-r12"
MODEL_NAMES = (
    CURRENT_R8,
    FFN_ONLY,
    SOURCE_R8,
    BI_R8,
    DECOUPLED_R8,
    BI_DECOUPLED_R8,
    DENSE_WORKSPACE_R8,
    BI_R12,
)


@dataclass(frozen=True)
class ModelSpec:
    name: str
    context_length: int = 256
    vocab_size: int = 16_384
    width: int = 128
    depth: int = 32
    ffn_width: int = 256
    group_count: int = 16
    workspace1: int = 4
    workspace2: int = 4
    channel1: int = 8
    channel2: int = 16
    rank: int = 8
    routing: str = "destination"
    decoupled_ranks: bool = False
    dense_workspace: bool = False
    mixer_enabled: bool = True

    @property
    def workspace_size(self) -> int:
        return self.workspace1 * self.workspace2

    def validate(self) -> None:
        if self.name not in MODEL_NAMES:
            raise ValueError(f"unknown model: {self.name}")
        if self.routing not in {"none", "destination", "source", "bi"}:
            raise ValueError(f"unknown routing: {self.routing}")
        if self.group_count * self.workspace_size != self.context_length:
            raise ValueError("token modes must multiply to context length")
        if self.channel1 * self.channel2 != self.width:
            raise ValueError("channel modes must multiply to width")
        if min(self.rank, self.depth, self.ffn_width) <= 0:
            raise ValueError("rank, depth, and FFN width must be positive")


SPECS = {
    CURRENT_R8: ModelSpec(CURRENT_R8),
    # Parameters remain allocated so this is a parameter-matched causal
    # intervention.  The mixer branch is deliberately inactive.
    FFN_ONLY: ModelSpec(FFN_ONLY, mixer_enabled=False),
    SOURCE_R8: ModelSpec(SOURCE_R8, routing="source"),
    BI_R8: ModelSpec(BI_R8, routing="bi"),
    DECOUPLED_R8: ModelSpec(
        DECOUPLED_R8, routing="destination", decoupled_ranks=True
    ),
    BI_DECOUPLED_R8: ModelSpec(
        BI_DECOUPLED_R8, routing="bi", decoupled_ranks=True
    ),
    DENSE_WORKSPACE_R8: ModelSpec(
        DENSE_WORKSPACE_R8, routing="destination", dense_workspace=True
    ),
    BI_R12: ModelSpec(BI_R12, rank=12, routing="bi"),
}


class RoutedKroneckerLayer(nn.Module):
    def __init__(self, spec: ModelSpec, layer_index: int) -> None:
        super().__init__()
        self.spec = spec
        self.outer = PackedLowerFactor(spec.rank, spec.group_count)
        if spec.dense_workspace:
            self.workspace = RowNormalizedFactor(spec.rank, spec.workspace_size)
        else:
            self.workspace1 = RowNormalizedFactor(spec.rank, spec.workspace1)
            self.workspace2 = RowNormalizedFactor(spec.rank, spec.workspace2)
        self.channel1 = RowNormalizedFactor(spec.rank, spec.channel1)
        self.channel2 = RowNormalizedFactor(spec.rank, spec.channel2)
        self.rank_amplitudes = nn.Parameter(
            torch.full((spec.rank,), spec.rank**-0.5)
        )
        if spec.decoupled_ranks:
            self.channel_amplitudes = nn.Parameter(
                torch.full((spec.rank,), spec.rank**-0.5)
            )
        if spec.routing in {"source", "bi"}:
            self.source_router = nn.Linear(spec.width, spec.rank, bias=False)
            nn.init.zeros_(self.source_router.weight)
        if spec.routing in {"destination", "bi"}:
            self.destination_router = nn.Linear(spec.width, spec.rank, bias=False)
            nn.init.zeros_(self.destination_router.weight)
        self.ffn = SwiGLU(spec.width, spec.ffn_width)
        initial_gain = (2.0 * spec.depth) ** -0.5
        self.mixer_gain = nn.Parameter(torch.tensor(initial_gain))
        self.ffn_gain = nn.Parameter(torch.tensor(initial_gain))
        workspace = affine_layout(spec.workspace_size, layer_index)
        channel = affine_layout(spec.width, layer_index)
        self.register_buffer("workspace_permutation", workspace, persistent=False)
        self.register_buffer(
            "inverse_workspace_permutation", torch.argsort(workspace), persistent=False
        )
        self.register_buffer("channel_permutation", channel, persistent=False)
        self.register_buffer(
            "inverse_channel_permutation", torch.argsort(channel), persistent=False
        )

    @staticmethod
    def _gate(router: nn.Linear, value: torch.Tensor) -> torch.Tensor:
        # Neutral at initialization, bounded, and token-local.
        return 2.0 * torch.sigmoid(router(rms_norm(value)))

    def _input_tensor(self, value: torch.Tensor) -> torch.Tensor:
        spec = self.spec
        if value.shape[1:] != (spec.context_length, spec.width):
            raise ValueError("hidden state does not match the block specification")
        normalized = F.silu(rms_norm(value)).reshape(
            value.shape[0], spec.group_count, spec.workspace_size, spec.width
        )
        normalized = normalized[:, :, self.workspace_permutation]
        normalized = normalized[..., self.channel_permutation]
        return normalized.reshape(
            value.shape[0],
            spec.group_count,
            spec.workspace1,
            spec.workspace2,
            spec.channel1,
            spec.channel2,
        )

    def _source_gates(self, value: torch.Tensor) -> torch.Tensor | None:
        if not hasattr(self, "source_router"):
            return None
        spec = self.spec
        gates = self._gate(self.source_router, value).reshape(
            value.shape[0], spec.group_count, spec.workspace_size, spec.rank
        )
        gates = gates[:, :, self.workspace_permutation]
        return gates.reshape(
            value.shape[0],
            spec.group_count,
            spec.workspace1,
            spec.workspace2,
            spec.rank,
        ).permute(0, 4, 1, 2, 3)

    def _token_mix(self, ranked: torch.Tensor) -> torch.Tensor:
        spec = self.spec
        if spec.dense_workspace:
            flat = ranked.flatten(3, 4)
            workspace = torch.einsum(
                "brgwpq,rxw->brgxpq", flat, self.workspace.value()
            ).reshape(
                ranked.shape[0],
                spec.rank,
                spec.group_count,
                spec.workspace1,
                spec.workspace2,
                spec.channel1,
                spec.channel2,
            )
        else:
            workspace = torch.einsum(
                "brgijpq,rxi,ryj->brgxypq",
                ranked,
                self.workspace1.value(),
                self.workspace2.value(),
            )
        return torch.einsum(
            "brgxypq,rsg->brsxypq", workspace, self.outer.value()
        )

    def rank_outputs(self, value: torch.Tensor) -> torch.Tensor:
        """Return token-rank paths after source routing and structured mixing."""

        spec = self.spec
        tensor = self._input_tensor(value)
        source = self._source_gates(value)
        if spec.decoupled_ranks:
            # Sum channel ranks independently of token ranks.  Expanding the
            # two sums gives R_channel x R_token effective combinations while
            # retaining additive factor storage and application cost.
            channel_ranked = torch.einsum(
                "bgijuv,cpu,cqv->bcgijpq",
                tensor,
                self.channel1.value(),
                self.channel2.value(),
            )
            channels = torch.einsum(
                "bcgijpq,c->bgijpq", channel_ranked, self.channel_amplitudes
            )
            ranked = channels[:, None].expand(-1, spec.rank, -1, -1, -1, -1, -1)
        else:
            ranked = torch.einsum(
                "bgijuv,rpu,rqv->brgijpq",
                tensor,
                self.channel1.value(),
                self.channel2.value(),
            )
        if source is not None:
            ranked = ranked * source[..., None, None]
        groups = self._token_mix(ranked)
        groups = groups.reshape(
            value.shape[0], spec.rank, spec.group_count, spec.workspace_size, spec.width
        )
        groups = groups[:, :, :, self.inverse_workspace_permutation]
        groups = groups[..., self.inverse_channel_permutation]
        return groups.reshape(
            value.shape[0], spec.rank, spec.context_length, spec.width
        )

    def branch(self, value: torch.Tensor) -> torch.Tensor:
        ranked = self.rank_outputs(value)
        weights = self.rank_amplitudes[None, :, None, None]
        if hasattr(self, "destination_router"):
            destination = self._gate(self.destination_router, value).permute(0, 2, 1)
            weights = weights * destination[..., None]
        return (ranked * weights).sum(1)

    def forward(self, value: torch.Tensor) -> torch.Tensor:
        if self.spec.mixer_enabled:
            value = rms_norm(value + self.mixer_gain * self.branch(value))
        return rms_norm(value + self.ffn_gain * self.ffn(rms_norm(value)))


class LanguageModel(nn.Module):
    def __init__(self, spec: ModelSpec) -> None:
        super().__init__()
        spec.validate()
        self.spec = spec
        self.vocabulary = nn.Parameter(
            torch.randn(spec.vocab_size, spec.width) / math.sqrt(spec.width)
        )
        self.blocks = nn.ModuleList(
            RoutedKroneckerLayer(spec, layer) for layer in range(spec.depth)
        )

    def hidden(self, token_ids: torch.Tensor) -> torch.Tensor:
        if token_ids.ndim != 2 or token_ids.shape[1] != self.spec.context_length:
            raise ValueError(f"expected token ids shaped [batch,{self.spec.context_length}]")
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
    token_factors = sum(
        parameter.numel()
        for name, parameter in model.named_parameters()
        if any(marker in name for marker in (".outer.raw", ".workspace.raw", ".workspace1.raw", ".workspace2.raw"))
    )
    return {
        "spec": asdict(model.spec),
        "vocabulary_parameters": vocabulary,
        "body_parameters": body,
        "total_parameters": vocabulary + body,
        "token_factor_parameters": token_factors,
        "active_mixer": model.spec.mixer_enabled,
        "effective_rank_pairings": (
            model.spec.rank**2 if model.spec.decoupled_ranks else model.spec.rank
        ),
        "token_parameter_scaling": (
            "O(depth*rank*(groups^2+workspace^2))"
            if model.spec.dense_workspace
            else "O(depth*rank*(groups^2+workspace1^2+workspace2^2))"
        ),
    }
