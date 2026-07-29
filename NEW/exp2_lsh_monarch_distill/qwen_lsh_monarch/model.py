from __future__ import annotations

import torch
import torch.nn.functional as F
from torch import nn

from .config import ArchitectureConfig
from .monarch import MonarchLinear


def _linear(
    config: ArchitectureConfig, input_width: int, output_width: int
) -> nn.Module:
    return MonarchLinear(
        input_width,
        output_width,
        nblocks=config.monarch_blocks,
        rank=config.monarch_rank,
        bias=True,
    )


class OneMapResidual(nn.Module):
    def __init__(self, config: ArchitectureConfig) -> None:
        super().__init__()
        self.norm = nn.RMSNorm(config.full_width, eps=1e-6)
        self.linear = _linear(config, config.full_width, config.full_width)

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        return x + self.linear(F.silu(self.norm(x)))


class TwoMapResidual(nn.Module):
    def __init__(self, config: ArchitectureConfig) -> None:
        super().__init__()
        hidden = config.full_width * config.expansion
        self.norm = nn.RMSNorm(config.full_width, eps=1e-6)
        self.up = _linear(config, config.full_width, hidden)
        self.down = _linear(config, hidden, config.full_width)

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        return x + self.down(F.silu(self.up(self.norm(x))))


class FullWidthStack(nn.Module):
    def __init__(self, config: ArchitectureConfig) -> None:
        super().__init__()
        config.validate()
        if config.form == "sequential":
            self.layers = nn.ModuleList([
                _linear(config, config.full_width, config.full_width)
                for _ in range(config.depth)
            ])
        elif config.form == "residual_one":
            self.layers = nn.ModuleList([
                OneMapResidual(config) for _ in range(config.depth)
            ])
        else:
            self.layers = nn.ModuleList([
                TwoMapResidual(config) for _ in range(config.depth)
            ])
        self.form = config.form

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        if self.form == "sequential":
            for index, layer in enumerate(self.layers):
                x = layer(x)
                if index + 1 < len(self.layers):
                    x = F.silu(x)
            return x
        for layer in self.layers:
            x = layer(x)
        return x


class LSHMonarchStudent(nn.Module):
    """Frozen LSH lookup -> 289-wide Monarch -> tied LSH unembedding."""

    def __init__(
        self,
        config: ArchitectureConfig,
        tied_codebook: torch.Tensor,
    ) -> None:
        super().__init__()
        config.validate()
        if tied_codebook.ndim != 2:
            raise ValueError("tied codebook must be a matrix")
        if tied_codebook.shape[1] != config.code_bits:
            raise ValueError("code width disagrees with architecture")
        if not tied_codebook.is_floating_point():
            tied_codebook = tied_codebook.float()
        if not bool(torch.all((tied_codebook == -1) | (tied_codebook == 1))):
            raise ValueError("tied codebook must contain only {-1,+1}")
        self.config = config
        self.vocab_size = int(tied_codebook.shape[0])
        self.register_buffer(
            "tied_codebook", tied_codebook.detach(), persistent=False
        )
        self.stack = FullWidthStack(config)

    def encode(self, token_ids: torch.Tensor) -> torch.Tensor:
        if (
            token_ids.ndim != 2
            or token_ids.shape[1] != self.config.context_length
        ):
            raise ValueError(
                f"expected [batch,{self.config.context_length}] token ids"
            )
        codes = F.embedding(token_ids, self.tied_codebook)
        flat = codes.reshape(token_ids.shape[0], self.config.active_width)
        return F.pad(flat, (0, 1), value=0.0)

    def hidden(self, token_ids: torch.Tensor) -> torch.Tensor:
        mixed = self.stack(self.encode(token_ids))
        start = (self.config.context_length - 1) * self.config.code_bits
        return mixed[:, start : start + self.config.code_bits]

    def forward(self, token_ids: torch.Tensor) -> torch.Tensor:
        return F.linear(self.hidden(token_ids), self.tied_codebook)

    def trainable_parameter_count(self) -> int:
        return sum(parameter.numel() for parameter in self.parameters())
