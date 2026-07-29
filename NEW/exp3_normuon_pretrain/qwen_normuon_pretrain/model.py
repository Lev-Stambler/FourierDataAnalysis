from __future__ import annotations

import math

import torch
import torch.nn.functional as F
from torch import nn

from .config import Architecture
from .kronecker import KroneckerLinear, context_channel_modes

EXPECTED_FACTOR_PARAMETERS = 83_802_112
EXPECTED_AUX_PARAMETERS = 254_749_920
EXPECTED_TRAINABLE_PARAMETERS = (
    EXPECTED_FACTOR_PARAMETERS + EXPECTED_AUX_PARAMETERS
)
EXPECTED_FACTOR_TENSORS = 24
EXPECTED_FACTOR_MATRICES = 57_768


class TwoMapResidual(nn.Module):
    def __init__(self, architecture: Architecture) -> None:
        super().__init__()
        width = architecture.full_width
        hidden = width * architecture.expansion
        self.norm = nn.RMSNorm(width, eps=1e-6)
        self.up = KroneckerLinear(
            width,
            hidden,
            input_modes=context_channel_modes(
                architecture.context_length,
                architecture.embedding_width,
                width,
            ),
            output_modes=context_channel_modes(
                architecture.context_length,
                architecture.embedding_width,
                hidden,
            ),
            rank=architecture.rank,
            rank_chunk=architecture.rank_chunk,
        )
        self.down = KroneckerLinear(
            hidden,
            width,
            input_modes=context_channel_modes(
                architecture.context_length,
                architecture.embedding_width,
                hidden,
            ),
            output_modes=context_channel_modes(
                architecture.context_length,
                architecture.embedding_width,
                width,
            ),
            rank=architecture.rank,
            rank_chunk=architecture.rank_chunk,
        )

    def forward(
        self,
        value: torch.Tensor,
        residual_multiplier: float,
    ) -> torch.Tensor:
        return value + residual_multiplier * self.down(
            F.silu(self.up(self.norm(value)))
        )


class KroneckerStack(nn.Module):
    def __init__(self, architecture: Architecture) -> None:
        super().__init__()
        architecture.validate()
        self.layers = nn.ModuleList([
            TwoMapResidual(architecture)
            for _ in range(architecture.depth)
        ])
        self.repetitions = architecture.repetitions
        self.residual_multiplier = architecture.residual_multiplier
        self.collect_activations = False
        self.last_activation_rms: list[float] = []

    def forward(self, value: torch.Tensor) -> torch.Tensor:
        if self.collect_activations:
            self.last_activation_rms = []
        for _ in range(self.repetitions):
            for layer in self.layers:
                value = layer(value, self.residual_multiplier)
                if self.collect_activations:
                    self.last_activation_rms.append(float(
                        torch.sqrt(
                            value.detach().float().square().mean()
                        )
                    ))
        return value


class NextTokenStudent(nn.Module):
    def __init__(
        self,
        architecture: Architecture,
        tied_embedding: torch.Tensor,
        *,
        vocab_size: int,
    ) -> None:
        super().__init__()
        architecture.validate()
        if tied_embedding.ndim != 2:
            raise ValueError("tied embedding must be a matrix")
        if tied_embedding.shape[1] != architecture.embedding_width:
            raise ValueError("embedding width mismatch")
        if not 0 < vocab_size <= tied_embedding.shape[0]:
            raise ValueError("invalid vocabulary size")
        self.architecture = architecture
        self.vocab_size = int(vocab_size)
        self.tied_embedding = nn.Parameter(tied_embedding.detach())
        self.stack = KroneckerStack(architecture)

    def hidden(self, token_ids: torch.Tensor) -> torch.Tensor:
        expected = (
            token_ids.shape[0],
            self.architecture.context_length,
        )
        if token_ids.ndim != 2 or token_ids.shape != expected:
            raise ValueError(
                f"expected [batch,{self.architecture.context_length}] ids"
            )
        embedded = F.embedding(token_ids, self.tied_embedding)
        flattened = embedded.reshape(
            token_ids.shape[0],
            self.architecture.full_width,
        )
        mixed = self.stack(flattened)
        return mixed.reshape(
            token_ids.shape[0],
            self.architecture.context_length,
            self.architecture.embedding_width,
        )[:, -1, :]

    def forward(self, token_ids: torch.Tensor) -> torch.Tensor:
        return F.linear(
            self.hidden(token_ids),
            self.tied_embedding[: self.vocab_size],
        )

    def factor_parameters(self) -> list[nn.Parameter]:
        parameters = []
        for module in self.modules():
            if isinstance(module, KroneckerLinear):
                parameters.extend(module.factors)
        if len({id(parameter) for parameter in parameters}) != len(parameters):
            raise RuntimeError("factor routing contains duplicate tensors")
        return parameters

    def auxiliary_parameters(self) -> list[nn.Parameter]:
        factor_ids = {
            id(parameter) for parameter in self.factor_parameters()
        }
        return [
            parameter
            for parameter in self.parameters()
            if id(parameter) not in factor_ids
        ]

    def parameter_inventory(self) -> dict[str, int]:
        factors = self.factor_parameters()
        auxiliary = self.auxiliary_parameters()
        factor_parameters = sum(
            parameter.numel() for parameter in factors
        )
        auxiliary_parameters = sum(
            parameter.numel() for parameter in auxiliary
        )
        factor_matrices = sum(
            math.prod(parameter.shape[:-2])
            for parameter in factors
        )
        return {
            "factor_tensors": len(factors),
            "factor_matrices": factor_matrices,
            "factor_parameters": factor_parameters,
            "auxiliary_tensors": len(auxiliary),
            "auxiliary_parameters": auxiliary_parameters,
            "trainable_parameters":
                factor_parameters + auxiliary_parameters,
        }

    def validate_study_inventory(self) -> dict[str, int]:
        inventory = self.parameter_inventory()
        expected = {
            "factor_tensors": EXPECTED_FACTOR_TENSORS,
            "factor_matrices": EXPECTED_FACTOR_MATRICES,
            "factor_parameters": EXPECTED_FACTOR_PARAMETERS,
            "auxiliary_parameters": EXPECTED_AUX_PARAMETERS,
            "trainable_parameters": EXPECTED_TRAINABLE_PARAMETERS,
        }
        for key, value in expected.items():
            if inventory[key] != value:
                raise RuntimeError(
                    f"unexpected {key}: {inventory[key]} != {value}"
                )
        return inventory

    def collect_activation_diagnostics(self, enabled: bool) -> None:
        self.stack.collect_activations = bool(enabled)

    def activation_metrics(self) -> dict[str, float]:
        values = self.stack.last_activation_rms
        if not values:
            return {}
        return {
            "diagnostic/activation_rms_max": max(values),
            "diagnostic/activation_rms_last": values[-1],
        }

    def factor_metrics(self) -> dict[str, float]:
        values = []
        for module in self.modules():
            if not isinstance(module, KroneckerLinear):
                continue
            for factor in module.factors:
                values.append(float(torch.sqrt(
                    factor.detach().float().square().mean()
                )))
        if not values:
            return {}
        return {
            "diagnostic/factor_rms_min": min(values),
            "diagnostic/factor_rms_max": max(values),
            "diagnostic/factor_rms_mean": sum(values) / len(values),
        }
