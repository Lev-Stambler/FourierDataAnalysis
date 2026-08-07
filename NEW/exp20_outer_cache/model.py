"""Exact block caching and compact recurrent outers for the Exp19 Kron mixer.

The dense operator is scientifically identical to the clean-token Exp19 mixer.
Only its implementation is split into an inner transform and an outer causal
transform so completed 16-token groups can be cached at inference time.  The
semiseparable operator is a separately named architecture: it replaces the
arbitrary lower-triangular outer bank with a row-normalized order-three
recurrence while retaining every inner Kronecker factor.
"""

from __future__ import annotations

import math
from dataclasses import asdict, dataclass, replace
from typing import Any, Literal

import torch
import torch.nn.functional as F
from torch import nn
from torch.utils.checkpoint import checkpoint

from exp19_norm_residual.model import (
    CleanRankOneGroupSwiGLU,
    LearnedRMSNorm,
    PackedLowerFactor,
    RowNormalizedFactor,
    affine_layout,
)


DENSE_R8_REFERENCE = "dense-r8-reference"
DENSE_R8_PACKED = "dense-r8-packed"
DENSE_R4_PACKED = "dense-r4-packed"
DENSE_R2_PACKED = "dense-r2-packed"
DENSE_R1_PACKED = "dense-r1-packed"
RECURRENT3_R8_PACKED = "semiseparable3-r8-packed"
TRANSFORMER_DEEP = "transformer-d32-w128"

MODEL_NAMES = (
    DENSE_R8_REFERENCE,
    DENSE_R8_PACKED,
    DENSE_R4_PACKED,
    DENSE_R2_PACKED,
    DENSE_R1_PACKED,
    RECURRENT3_R8_PACKED,
    TRANSFORMER_DEEP,
)
KRON_MODEL_NAMES = MODEL_NAMES[:-1]

OuterKind = Literal["dense-history", "semiseparable-3"]
MixerBackend = Literal["reference", "packed-gemm"]


@dataclass(frozen=True)
class ModelSpec:
    name: str
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
    recurrent_order: int = 3
    outer_kind: OuterKind = "dense-history"
    mixer_backend: MixerBackend = "packed-gemm"
    activation_checkpointing: bool = True

    @property
    def group_count(self) -> int:
        return self.context_length // self.group_size

    @property
    def branch_scale(self) -> float:
        return (2.0 * self.depth) ** -0.5

    def validate(self) -> None:
        if self.name not in KRON_MODEL_NAMES:
            raise ValueError(f"unknown Exp20 Kron model: {self.name}")
        if self.context_length % self.group_size:
            raise ValueError("context length must contain complete groups")
        if self.workspace1 * self.workspace2 != self.group_size:
            raise ValueError("workspace modes must multiply to group size")
        if self.channel1 * self.channel2 != self.width:
            raise ValueError("channel modes must multiply to width")
        if min(
            self.width,
            self.depth,
            self.mixer_rank,
            self.hidden_workspace,
            self.hidden_channel,
        ) <= 0:
            raise ValueError("model dimensions and ranks must be positive")
        if self.outer_kind not in {"dense-history", "semiseparable-3"}:
            raise ValueError(f"unknown outer kind: {self.outer_kind}")
        if self.mixer_backend not in {"reference", "packed-gemm"}:
            raise ValueError(f"unknown mixer backend: {self.mixer_backend}")
        if self.outer_kind == "semiseparable-3" and self.recurrent_order != 3:
            raise ValueError("the preregistered recurrent outer has order three")


SPECS = {
    DENSE_R8_REFERENCE: ModelSpec(
        DENSE_R8_REFERENCE, mixer_backend="reference"
    ),
    DENSE_R8_PACKED: ModelSpec(DENSE_R8_PACKED),
    DENSE_R4_PACKED: ModelSpec(
        DENSE_R4_PACKED, mixer_rank=4, hidden_channel=255
    ),
    DENSE_R2_PACKED: ModelSpec(
        DENSE_R2_PACKED, mixer_rank=2, hidden_channel=258
    ),
    DENSE_R1_PACKED: ModelSpec(
        DENSE_R1_PACKED, mixer_rank=1, hidden_channel=259
    ),
    RECURRENT3_R8_PACKED: ModelSpec(
        RECURRENT3_R8_PACKED, outer_kind="semiseparable-3"
    ),
}


class DenseCausalOuter(nn.Module):
    """The exact Exp19 packed, row-normalized lower-triangular rank bank."""

    def __init__(self, rank: int, groups: int) -> None:
        super().__init__()
        self.rank = int(rank)
        self.groups = int(groups)
        self.factor = PackedLowerFactor(rank, groups)

    def matrix(self) -> torch.Tensor:
        return self.factor.value()

    def apply(self, value: torch.Tensor) -> torch.Tensor:
        return torch.einsum("rsg,brgwc->brswc", self.matrix(), value)

    def step(self, history: torch.Tensor, position: int) -> torch.Tensor:
        row = self.matrix()[:, position, : position + 1]
        return torch.einsum("rg,brgwc->brwc", row, history[:, :, : position + 1])


class SemiseparableOuter(nn.Module):
    """Order-three row-normalized causal recurrence.

    For each mixer rank and recurrent component, the feature state is

        h_s = a_s h_(s-1) + b_s z_s

    and the output is ``c_s @ h_s`` divided by the exact Euclidean norm of the
    corresponding materialized causal row.  The row itself is tracked when
    computing that norm.  Computing it as ``c.T @ gram @ c`` is algebraically
    equivalent but is numerically unsafe near component cancellation: finite
    precision can make the nominally non-negative quadratic form negative
    before ``sqrt``.  Decays are constrained to (0, 1), making the streaming
    state finite without changing row normalization.
    """

    def __init__(self, rank: int, groups: int, order: int = 3) -> None:
        super().__init__()
        self.rank = int(rank)
        self.groups = int(groups)
        self.order = int(order)
        self.raw_decay = nn.Parameter(
            torch.full((rank, groups, order), math.log(0.9 / 0.1))
        )
        self.input_scale = nn.Parameter(
            torch.randn(rank, groups, order) / math.sqrt(order)
        )
        self.output_scale = nn.Parameter(
            torch.randn(rank, groups, order) / math.sqrt(order)
        )

    def coefficients(self) -> tuple[torch.Tensor, torch.Tensor, torch.Tensor]:
        return torch.sigmoid(self.raw_decay), self.input_scale, self.output_scale

    def _advance_basis(
        self,
        basis: torch.Tensor,
        decay: torch.Tensor,
        injection: torch.Tensor,
        position: int,
    ) -> torch.Tensor:
        one_hot = F.one_hot(
            torch.tensor(position, device=basis.device), self.groups
        ).to(basis.dtype)
        return (
            decay[:, :, None] * basis
            + injection[:, :, None] * one_hot
        )

    @staticmethod
    def _row_norm(row: torch.Tensor) -> torch.Tensor:
        # Accumulate the small norm in FP32 even under BF16 autocast.  Taking
        # the norm of explicit coefficients is non-negative by construction
        # and has the same derivative as exact row normalization.
        return torch.linalg.vector_norm(row.float(), dim=-1).clamp_min(1e-8)

    def matrix(self) -> torch.Tensor:
        decay, injection, projection = self.coefficients()
        basis = injection.new_zeros(self.rank, self.order, self.groups)
        rows = []
        for position in range(self.groups):
            a = decay[:, position]
            b = injection[:, position]
            c = projection[:, position]
            basis = self._advance_basis(basis, a, b, position)
            row = torch.einsum("rm,rmg->rg", c, basis)
            norm = self._row_norm(row).to(row.dtype)
            rows.append(row / norm[:, None])
        return torch.stack(rows, 1)

    def apply_materialized(self, value: torch.Tensor) -> torch.Tensor:
        return torch.einsum("rsg,brgwc->brswc", self.matrix(), value)

    def apply(self, value: torch.Tensor) -> torch.Tensor:
        decay, injection, projection = self.coefficients()
        state = value.new_zeros(
            value.shape[0], self.rank, self.order, value.shape[-2], value.shape[-1]
        )
        basis = injection.new_zeros(self.rank, self.order, self.groups)
        outputs = []
        for position in range(self.groups):
            a = decay[:, position]
            b = injection[:, position]
            c = projection[:, position]
            state = a[None, :, :, None, None] * state + b[
                None, :, :, None, None
            ] * value[:, :, position, None]
            basis = self._advance_basis(basis, a, b, position)
            numerator = torch.einsum("brmwc,rm->brwc", state, c)
            row = torch.einsum("rm,rmg->rg", c, basis)
            denominator = self._row_norm(row).to(numerator.dtype)
            outputs.append(numerator / denominator[None, :, None, None])
        return torch.stack(outputs, 2)

    def step(
        self,
        value: torch.Tensor,
        state: torch.Tensor,
        basis: torch.Tensor,
        position: int,
    ) -> tuple[torch.Tensor, torch.Tensor, torch.Tensor]:
        decay, injection, projection = self.coefficients()
        a = decay[:, position]
        b = injection[:, position]
        c = projection[:, position]
        state = a[None, :, :, None, None] * state + b[
            None, :, :, None, None
        ] * value[:, :, None]
        basis = self._advance_basis(basis, a, b, position)
        numerator = torch.einsum("brmwc,rm->brwc", state, c)
        row = torch.einsum("rm,rmg->rg", c, basis)
        denominator = self._row_norm(row).to(numerator.dtype)
        return numerator / denominator[None, :, None, None], state, basis


@dataclass
class DenseLayerCache:
    history: torch.Tensor


@dataclass
class RecurrentLayerCache:
    state: torch.Tensor
    normalizer_basis: torch.Tensor


LayerCache = DenseLayerCache | RecurrentLayerCache


@dataclass
class BlockCache:
    """Mutable inference-only per-layer state for completed source groups."""

    layers: list[LayerCache]
    position: int
    batch_size: int
    device: torch.device
    dtype: torch.dtype
    outer_kind: OuterKind


class CachedStructuredMixer(nn.Module):
    def __init__(self, spec: ModelSpec, layer_index: int) -> None:
        super().__init__()
        self.spec = spec
        self.outer: DenseCausalOuter | SemiseparableOuter
        if spec.outer_kind == "dense-history":
            self.outer = DenseCausalOuter(spec.mixer_rank, spec.group_count)
        else:
            self.outer = SemiseparableOuter(
                spec.mixer_rank, spec.group_count, spec.recurrent_order
            )
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

    def _permuted_groups(self, normalized: torch.Tensor) -> torch.Tensor:
        spec = self.spec
        if normalized.ndim != 3 or normalized.shape[-1] != spec.width:
            raise ValueError("mixer input must be [batch,tokens,width]")
        if normalized.shape[1] % spec.group_size:
            raise ValueError("mixer input must contain complete groups")
        activated = F.silu(normalized).reshape(
            normalized.shape[0], -1, spec.group_size, spec.width
        )
        return activated[:, :, self.workspace_permutation][
            ..., self.channel_permutation
        ]

    def _inner_reference(self, activated: torch.Tensor) -> torch.Tensor:
        spec = self.spec
        tensor = activated.reshape(
            activated.shape[0],
            activated.shape[1],
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
        return workspace.reshape(
            activated.shape[0],
            spec.mixer_rank,
            activated.shape[1],
            spec.group_size,
            spec.width,
        )

    def _inner_packed(self, activated: torch.Tensor) -> torch.Tensor:
        spec = self.spec

        def batch_kron(first: torch.Tensor, second: torch.Tensor) -> torch.Tensor:
            return (
                first[:, :, None, :, None] * second[:, None, :, None, :]
            ).reshape(
                first.shape[0],
                first.shape[1] * second.shape[1],
                first.shape[2] * second.shape[2],
            )

        channel = batch_kron(self.channel1.value(), self.channel2.value())
        workspace = batch_kron(self.workspace1.value(), self.workspace2.value())
        channels = torch.matmul(
            activated[:, None],
            channel.transpose(-1, -2)[None, :, None],
        )
        mixed = torch.matmul(
            channels.transpose(-1, -2),
            workspace.transpose(-1, -2)[None, :, None],
        ).transpose(-1, -2)
        return mixed

    def inner(self, normalized: torch.Tensor) -> torch.Tensor:
        activated = self._permuted_groups(normalized)
        if self.spec.mixer_backend == "reference":
            return self._inner_reference(activated)
        return self._inner_packed(activated)

    def _restore(self, ranked: torch.Tensor) -> torch.Tensor:
        ranked = ranked[:, :, :, self.inverse_workspace_permutation]
        ranked = ranked[..., self.inverse_channel_permutation]
        return torch.einsum("brgwd,r->bgwd", ranked, self.rank_amplitudes).flatten(1, 2)

    def rank_outputs(self, normalized: torch.Tensor) -> torch.Tensor:
        inner = self.inner(normalized)
        return self.outer.apply(inner)

    def forward(self, normalized: torch.Tensor) -> torch.Tensor:
        if normalized.shape[1] != self.spec.context_length:
            raise ValueError("full mixer path requires the configured context length")
        return self._restore(self.rank_outputs(normalized))

    def forward_block(
        self, normalized: torch.Tensor, cache: LayerCache, position: int
    ) -> torch.Tensor:
        if normalized.shape[1:] != (self.spec.group_size, self.spec.width):
            raise ValueError("cached mixer requires exactly one complete group")
        inner = self.inner(normalized)[:, :, 0]
        if isinstance(cache, DenseLayerCache) and isinstance(self.outer, DenseCausalOuter):
            cache.history[:, :, position].copy_(inner)
            ranked = self.outer.step(cache.history, position)
        elif isinstance(cache, RecurrentLayerCache) and isinstance(
            self.outer, SemiseparableOuter
        ):
            ranked, state, normalizer_basis = self.outer.step(
                inner, cache.state, cache.normalizer_basis, position
            )
            cache.state.copy_(state)
            cache.normalizer_basis.copy_(normalizer_basis)
        else:
            raise TypeError("cache kind does not match mixer outer kind")
        return self._restore(ranked[:, :, None])


class CachedKronLayer(nn.Module):
    def __init__(self, spec: ModelSpec, layer_index: int) -> None:
        super().__init__()
        self.spec = spec
        self.mixer_norm = LearnedRMSNorm(
            spec.width, kind="learned-token", group_size=spec.group_size
        )
        self.ffn_norm = LearnedRMSNorm(
            spec.width, kind="learned-token", group_size=spec.group_size
        )
        self.mixer = CachedStructuredMixer(spec, layer_index)
        self.group_ffn = CleanRankOneGroupSwiGLU(
            spec.group_size,
            spec.width,
            spec.hidden_workspace,
            spec.hidden_channel,
        )

    def nonlinear_branch(self, normalized: torch.Tensor) -> torch.Tensor:
        groups = normalized.reshape(
            normalized.shape[0], -1, self.spec.group_size, self.spec.width
        )
        return self.group_ffn(groups).reshape_as(normalized)

    def forward(self, value: torch.Tensor) -> torch.Tensor:
        value = value + self.spec.branch_scale * self.mixer(self.mixer_norm(value))
        return value + self.spec.branch_scale * self.nonlinear_branch(
            self.ffn_norm(value)
        )

    def forward_block(
        self, value: torch.Tensor, cache: LayerCache, position: int
    ) -> torch.Tensor:
        value = value + self.spec.branch_scale * self.mixer.forward_block(
            self.mixer_norm(value), cache, position
        )
        return value + self.spec.branch_scale * self.nonlinear_branch(
            self.ffn_norm(value)
        )


class CachedLanguageModel(nn.Module):
    def __init__(self, spec: ModelSpec) -> None:
        super().__init__()
        spec.validate()
        self.spec = spec
        self.vocabulary = nn.Parameter(
            torch.randn(spec.vocab_size, spec.width) / math.sqrt(spec.width)
        )
        self.blocks = nn.ModuleList(
            CachedKronLayer(spec, layer_index) for layer_index in range(spec.depth)
        )
        self.final_norm = LearnedRMSNorm(
            spec.width, kind="learned-token", group_size=spec.group_size
        )

    def hidden(self, token_ids: torch.Tensor) -> torch.Tensor:
        if token_ids.shape[1:] != (self.spec.context_length,):
            raise ValueError(
                f"expected token IDs shaped [batch,{self.spec.context_length}]"
            )
        value = F.embedding(token_ids, self.vocabulary) * math.sqrt(self.spec.width)
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

    def init_block_cache(
        self,
        batch_size: int,
        *,
        device: torch.device | str | None = None,
        dtype: torch.dtype | None = None,
    ) -> BlockCache:
        if batch_size <= 0:
            raise ValueError("cache batch size must be positive")
        device = torch.device(device) if device is not None else self.vocabulary.device
        dtype = dtype or self.vocabulary.dtype
        spec = self.spec
        layers: list[LayerCache] = []
        for _ in self.blocks:
            if spec.outer_kind == "dense-history":
                layers.append(
                    DenseLayerCache(
                        torch.zeros(
                            batch_size,
                            spec.mixer_rank,
                            spec.group_count,
                            spec.group_size,
                            spec.width,
                            device=device,
                            dtype=dtype,
                        )
                    )
                )
            else:
                layers.append(
                    RecurrentLayerCache(
                        state=torch.zeros(
                            batch_size,
                            spec.mixer_rank,
                            spec.recurrent_order,
                            spec.group_size,
                            spec.width,
                            device=device,
                            dtype=dtype,
                        ),
                        normalizer_basis=torch.zeros(
                            spec.mixer_rank,
                            spec.recurrent_order,
                            spec.group_count,
                            device=device,
                            dtype=self.vocabulary.dtype,
                        ),
                    )
                )
        return BlockCache(
            layers=layers,
            position=0,
            batch_size=batch_size,
            device=device,
            dtype=dtype,
            outer_kind=spec.outer_kind,
        )

    def forward_block(
        self, token_ids: torch.Tensor, cache: BlockCache
    ) -> tuple[torch.Tensor, BlockCache]:
        if torch.is_grad_enabled():
            raise RuntimeError("block caches are inference-only; use torch.inference_mode()")
        spec = self.spec
        if token_ids.shape != (cache.batch_size, spec.group_size):
            raise ValueError("cached input must match cache batch and group size")
        if cache.position >= spec.group_count:
            raise ValueError("block cache is full")
        if token_ids.device != cache.device:
            raise ValueError("cached input and cache must be on the same device")
        value = F.embedding(token_ids, self.vocabulary) * math.sqrt(spec.width)
        for block, layer_cache in zip(self.blocks, cache.layers, strict=True):
            value = block.forward_block(value, layer_cache, cache.position)
        cache.position += 1
        hidden = self.final_norm(value)
        return F.linear(hidden, self.vocabulary), cache

    def prefill_blocks(
        self, token_ids: torch.Tensor, cache: BlockCache | None = None
    ) -> tuple[torch.Tensor, BlockCache]:
        if token_ids.ndim != 2 or token_ids.shape[1] % self.spec.group_size:
            raise ValueError("prefill input must contain complete groups")
        cache = cache or self.init_block_cache(
            token_ids.shape[0], device=token_ids.device, dtype=self.vocabulary.dtype
        )
        outputs = []
        for block in token_ids.split(self.spec.group_size, dim=1):
            logits, cache = self.forward_block(block, cache)
            outputs.append(logits)
        return torch.cat(outputs, dim=1), cache


def build_model(
    name: str, *, activation_checkpointing: bool | None = None, **overrides: Any
) -> nn.Module:
    if name == TRANSFORMER_DEEP:
        from exp19_norm_residual.model import (
            CORRECTED_TRANSFORMER_DEEP,
            build_model as build_exp19,
        )

        return build_exp19(
            CORRECTED_TRANSFORMER_DEEP,
            activation_checkpointing=activation_checkpointing,
            **overrides,
        )
    if name not in SPECS:
        raise ValueError(f"unknown Exp20 model: {name}")
    if activation_checkpointing is not None:
        overrides["activation_checkpointing"] = activation_checkpointing
    return CachedLanguageModel(replace(SPECS[name], **overrides))


def model_inventory(model: nn.Module) -> dict[str, Any]:
    if not isinstance(model, CachedLanguageModel):
        from exp19_norm_residual.model import model_inventory as exp19_inventory

        result = dict(exp19_inventory(model))
        result["architecture_identity"] = TRANSFORMER_DEEP
        result["implementation"] = "exp19-frozen-transformer-control"
        return result
    named = list(model.named_parameters())
    total = sum(parameter.numel() for _, parameter in named)
    vocabulary = model.vocabulary.numel()
    mixer = sum(
        parameter.numel() for name, parameter in named if ".mixer." in name
    )
    group_ffn = sum(
        parameter.numel() for name, parameter in named if ".group_ffn." in name
    )
    return {
        "architecture_identity": model.spec.name,
        "implementation": "exp20",
        "spec": asdict(model.spec),
        "vocabulary_parameters": vocabulary,
        "body_parameters": total - vocabulary,
        "mixer_parameters": mixer,
        "group_ffn_parameters": group_ffn,
        "total_parameters": total,
        "fixed_branch_scale": model.spec.branch_scale,
        "residual_topology": "true-pre-norm-identity",
        "cache_semantics": "16-token-block-shifted-backbone",
    }


def correctness_checks(name: str, device: torch.device) -> dict[str, Any]:
    """Audit packed contractions, causal isolation, and streaming parity."""

    if name == TRANSFORMER_DEEP:
        return {"pass": True, "applicable": False, "control": "frozen-exp19"}
    overrides: dict[str, Any] = {
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
        "activation_checkpointing": False,
    }
    torch.manual_seed(2000)
    model = build_model(name, **overrides).to(device=device, dtype=torch.float64).eval()
    assert isinstance(model, CachedLanguageModel)
    reference = CachedLanguageModel(
        replace(model.spec, mixer_backend="reference")
    ).to(device=device, dtype=torch.float64).eval()
    reference.load_state_dict(model.state_dict())
    left = torch.randint(0, model.spec.vocab_size, (2, model.spec.context_length), device=device)
    packed_output = model(left)
    reference_output = reference(left)
    forward_error = float((packed_output - reference_output).abs().max())
    probe = torch.randn_like(packed_output)
    packed_gradients = torch.autograd.grad(
        packed_output, list(model.parameters()), probe, retain_graph=True
    )
    reference_gradients = torch.autograd.grad(
        reference_output, list(reference.parameters()), probe
    )
    backward_error = max(
        float((packed - expected).abs().max())
        for packed, expected in zip(
            packed_gradients, reference_gradients, strict=True
        )
    )
    contraction = {
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
    future_gradient = torch.autograd.grad(
        hidden[:, : model.spec.group_size].square().sum(), value
    )[0][:, model.spec.group_size :]
    future_error = float(future_gradient.abs().max())
    causality = {
        "future_group_gradient_max_absolute": future_error,
        "tolerance": 1e-8,
        "pass": future_error <= 1e-8,
    }

    with torch.inference_mode():
        full = model(left)
        cached, cache = model.prefill_blocks(left)
    cache_error = float((full - cached).abs().max())
    cache_parity = {
        "max_absolute_error": cache_error,
        "completed_groups": cache.position,
        "tolerance": 1e-8,
        "pass": cache_error <= 1e-8 and cache.position == model.spec.group_count,
    }

    recurrent: dict[str, Any] = {"applicable": False, "pass": True}
    if isinstance(model.blocks[0].mixer.outer, SemiseparableOuter):
        outer = model.blocks[0].mixer.outer
        scanned_input = torch.randn(
            2,
            model.spec.mixer_rank,
            model.spec.group_count,
            model.spec.group_size,
            model.spec.width,
            device=device,
            dtype=torch.float64,
            requires_grad=True,
        )
        materialized_input = scanned_input.detach().clone().requires_grad_(True)
        scanned = outer.apply(scanned_input)
        materialized = outer.apply_materialized(materialized_input)
        recurrent_error = float((scanned - materialized).abs().max())
        recurrent_probe = torch.randn_like(scanned)
        scanned_gradients = torch.autograd.grad(
            scanned,
            [scanned_input, *outer.parameters()],
            recurrent_probe,
            retain_graph=True,
        )
        materialized_gradients = torch.autograd.grad(
            materialized,
            [materialized_input, *outer.parameters()],
            recurrent_probe,
        )
        recurrent_backward_error = max(
            float((left - right).abs().max())
            for left, right in zip(
                scanned_gradients, materialized_gradients, strict=True
            )
        )
        recurrent = {
            "applicable": True,
            "scan_materialized_max_absolute_error": recurrent_error,
            "scan_materialized_backward_max_absolute_error": recurrent_backward_error,
            "tolerance": 1e-8,
            "pass": recurrent_error <= 1e-8 and recurrent_backward_error <= 1e-8,
        }
    passed = all(
        row["pass"] for row in (contraction, causality, cache_parity, recurrent)
    )
    return {
        "pass": passed,
        "packed_reference_contraction": contraction,
        "block_causality": causality,
        "cached_full_parity": cache_parity,
        "semiseparable_scan_materialized": recurrent,
    }
