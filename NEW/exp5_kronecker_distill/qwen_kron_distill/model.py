from __future__ import annotations

import math

import torch
import torch.nn.functional as F
from torch import nn

from .config import (
    INITIALIZER_RANGE,
    VOCAB_SIZE,
    VOCABULARY_FACTOR_INITIALIZER_STD,
    Architecture,
)
from .kronecker import KroneckerSumLinear
from .objective import (
    khatri_rao_forward_kl,
    khatri_rao_logits,
    materialized_probability_linear_forward_kl,
)


def vocabulary_modes(vocab_size: int) -> tuple[int, int]:
    """Return the closest exact two-factor decomposition of a vocabulary."""

    first = math.isqrt(vocab_size)
    while vocab_size % first:
        first -= 1
    return first, vocab_size // first


class TensorResidual(nn.Module):
    def __init__(self, architecture: Architecture) -> None:
        super().__init__()
        self.architecture = architecture
        self.norm = nn.RMSNorm(
            (architecture.context_length, architecture.embedding_width),
            eps=1e-6,
        )
        self.linear = KroneckerSumLinear(
            architecture.input_modes,
            architecture.input_modes,
            rank=architecture.rank,
        )

    def forward(self, value: torch.Tensor) -> torch.Tensor:
        activated = F.silu(self.norm(value))
        modes = activated.reshape(
            activated.shape[0],
            *self.architecture.input_modes,
        )
        transformed = self.linear(modes).reshape_as(value)
        return value + self.architecture.residual_multiplier * transformed


class TensorTerminalResidual(nn.Module):
    """Compute only the context position consumed by the tied head."""

    def __init__(self, architecture: Architecture) -> None:
        super().__init__()
        self.architecture = architecture
        self.norm = nn.RMSNorm(
            (architecture.context_length, architecture.embedding_width),
            eps=1e-6,
        )
        output_modes = (1, *architecture.input_modes[1:])
        self.linear = KroneckerSumLinear(
            architecture.input_modes,
            output_modes,
            rank=architecture.rank,
        )

    def forward(self, value: torch.Tensor) -> torch.Tensor:
        activated = F.silu(self.norm(value))
        modes = activated.reshape(
            activated.shape[0],
            *self.architecture.input_modes,
        )
        transformed = self.linear(modes).reshape(
            value.shape[0],
            self.architecture.embedding_width,
        )
        return value[:, -1, :] + self.architecture.residual_multiplier * transformed


class TensorKroneckerStudent(nn.Module):
    def __init__(
        self,
        architecture: Architecture,
        *,
        vocab_size: int = VOCAB_SIZE,
        dtype: torch.dtype = torch.bfloat16,
        seed: int = 0,
    ) -> None:
        super().__init__()
        architecture.validate()
        if not 0 < vocab_size <= VOCAB_SIZE:
            raise ValueError("invalid vocabulary size")
        self.architecture = architecture
        self.vocab_size = int(vocab_size)
        self.dense_vocabulary = architecture.vocabulary_width is not None
        self.vocab_modes = (
            (self.vocab_size,)
            if self.dense_vocabulary
            else vocabulary_modes(self.vocab_size)
        )
        generator = torch.Generator(device="cpu")
        generator.manual_seed(seed)
        vocabulary_factors = []
        vocabulary_width = (
            architecture.vocabulary_width
            if self.dense_vocabulary
            else architecture.embedding_width
        )
        for mode in self.vocab_modes:
            value = torch.empty(
                mode,
                vocabulary_width,
                dtype=dtype,
                device="cpu",
            )
            value.normal_(
                mean=0.0,
                std=(
                    INITIALIZER_RANGE
                    if self.dense_vocabulary
                    else VOCABULARY_FACTOR_INITIALIZER_STD
                ),
                generator=generator,
            )
            factor = nn.Parameter(value)
            factor.optimizer_role = "tied_vocabulary_factor"
            vocabulary_factors.append(factor)
        self.vocabulary_factors = nn.ParameterList(vocabulary_factors)
        if (
            self.dense_vocabulary
            and vocabulary_width != architecture.embedding_width
        ):
            self.input_projection = nn.Parameter(
                torch.empty(
                    architecture.embedding_width,
                    vocabulary_width,
                    dtype=dtype,
                )
            )
            self.output_projection = nn.Parameter(
                torch.empty(
                    vocabulary_width,
                    architecture.embedding_width,
                    dtype=dtype,
                )
            )
            self.input_projection.optimizer_role = "vocabulary_bridge"
            self.output_projection.optimizer_role = "vocabulary_bridge"
            self.input_projection.data.normal_(
                mean=0.0,
                std=vocabulary_width**-0.5,
                generator=generator,
            )
            self.output_projection.data.normal_(
                mean=0.0,
                std=architecture.embedding_width**-0.5,
                generator=generator,
            )
        else:
            self.register_parameter("input_projection", None)
            self.register_parameter("output_projection", None)
        self.layers = nn.ModuleList(
            [TensorResidual(architecture) for _ in range(architecture.depth - 1)]
        )
        self.terminal = TensorTerminalResidual(architecture)

    def hidden(self, token_ids: torch.Tensor) -> torch.Tensor:
        if (
            token_ids.ndim != 2
            or token_ids.shape[1] != self.architecture.context_length
        ):
            raise ValueError("student requires token ids with shape [batch,16]")
        if self.dense_vocabulary:
            value = F.embedding(token_ids, self.vocabulary_factors[0])
            if self.input_projection is not None:
                value = F.linear(value, self.input_projection)
        else:
            first = torch.div(
                token_ids,
                self.vocab_modes[1],
                rounding_mode="floor",
            )
            second = token_ids.remainder(self.vocab_modes[1])
            value = (
                F.embedding(first, self.vocabulary_factors[0])
                * F.embedding(second, self.vocabulary_factors[1])
            )
        for layer in self.layers:
            value = layer(value)
        return self.terminal(value)

    def head_hidden(self, token_ids: torch.Tensor) -> torch.Tensor:
        hidden = self.hidden(token_ids)
        if self.output_projection is not None:
            hidden = F.linear(hidden, self.output_projection)
        return hidden

    def forward(
        self,
        token_ids: torch.Tensor,
        teacher_probability: torch.Tensor | None = None,
        teacher_entropy: torch.Tensor | None = None,
    ) -> torch.Tensor:
        hidden = self.head_hidden(token_ids)
        targets = (teacher_probability, teacher_entropy)
        if all(value is None for value in targets):
            if self.dense_vocabulary:
                return F.linear(hidden, self.vocabulary_factors[0])
            return khatri_rao_logits(
                hidden,
                self.vocabulary_factors[0],
                self.vocabulary_factors[1],
            )
        if any(value is None for value in targets):
            raise ValueError("teacher probability and entropy are both required")
        if self.dense_vocabulary:
            return materialized_probability_linear_forward_kl(
                hidden,
                self.vocabulary_factors[0],
                teacher_probability,
                teacher_entropy,
            )
        return khatri_rao_forward_kl(
            hidden,
            self.vocabulary_factors[0],
            self.vocabulary_factors[1],
            teacher_probability,
            teacher_entropy,
        )

    def vocabulary_parameters(self) -> list[nn.Parameter]:
        return list(self.vocabulary_factors)

    def factor_parameters(self) -> list[nn.Parameter]:
        result = list(self.vocabulary_factors)
        if self.input_projection is not None:
            result.extend(
                [self.input_projection, self.output_projection]
            )
        for module in self.modules():
            if isinstance(module, KroneckerSumLinear):
                result.extend(module.factors)
        if len({id(value) for value in result}) != len(result):
            raise RuntimeError("factor routing contains duplicate tensors")
        return result

    def auxiliary_parameters(self) -> list[nn.Parameter]:
        factor_ids = {id(value) for value in self.factor_parameters()}
        return [value for value in self.parameters() if id(value) not in factor_ids]

    def parameter_inventory(self) -> dict[str, int | list[int]]:
        factors = self.factor_parameters()
        auxiliary = self.auxiliary_parameters()
        return {
            "vocabulary_modes": list(self.vocab_modes),
            "vocabulary_factor_parameters": sum(
                value.numel() for value in self.vocabulary_parameters()
            ),
            "factor_tensors": len(factors),
            "factor_matrices": sum(math.prod(value.shape[:-2]) for value in factors),
            "factor_parameters": sum(value.numel() for value in factors),
            "auxiliary_parameters": sum(value.numel() for value in auxiliary),
            "trainable_parameters": sum(value.numel() for value in self.parameters()),
        }
