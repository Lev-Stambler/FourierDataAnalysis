from __future__ import annotations

import math

import torch
from torch import nn


class MonarchLinear(nn.Module):
    """Square or rectangular Monarch map with explicit parallel rank.

    The two factors use the fixed reshape-transpose permutation from the prior
    repository implementation. Padding is deliberately external.
    """

    def __init__(
        self,
        in_features: int,
        out_features: int,
        *,
        nblocks: int,
        rank: int = 1,
        bias: bool = False,
        rank_chunk: int = 8,
    ) -> None:
        super().__init__()
        if min(in_features, out_features, nblocks, rank, rank_chunk) <= 0:
            raise ValueError("Monarch dimensions and rank must be positive")
        if in_features % nblocks or out_features % nblocks:
            raise ValueError("Monarch widths must divide evenly by nblocks")
        self.in_features = int(in_features)
        self.out_features = int(out_features)
        self.nblocks = int(nblocks)
        self.rank = int(rank)
        self.rank_chunk = int(rank_chunk)
        self.in_block = self.in_features // self.nblocks
        self.out_block = self.out_features // self.nblocks
        self.middle_block = min(self.in_block, self.out_block)
        self.factor1 = nn.Parameter(
            torch.empty(self.rank, self.nblocks, self.middle_block, self.in_block)
        )
        self.factor2 = nn.Parameter(
            torch.empty(self.rank, self.nblocks, self.out_block, self.middle_block)
        )
        self.bias = nn.Parameter(torch.empty(self.out_features)) if bias else None
        self.reset_parameters()

    def reset_parameters(self) -> None:
        for factor in (self.factor1, self.factor2):
            nn.init.uniform_(factor, -1 / math.sqrt(factor.shape[-1]), 1 / math.sqrt(factor.shape[-1]))
        if self.bias is not None:
            nn.init.zeros_(self.bias)

    def _branch(self, x: torch.Tensor, lo: int, hi: int) -> torch.Tensor:
        shape = x.shape[:-1]
        batch = math.prod(shape)
        blocks = x.reshape(batch, self.nblocks, self.in_block)
        first = torch.einsum("bkp,tkqp->tbkq", blocks, self.factor1[lo:hi])
        first = first.reshape(hi - lo, batch, self.middle_block, self.nblocks)
        first = first.transpose(-1, -2).contiguous()
        second = torch.einsum("tbkr,tksr->tbks", first, self.factor2[lo:hi])
        return second.transpose(-1, -2).reshape(hi - lo, batch, self.out_features).sum(0).reshape(
            *shape, self.out_features
        )

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        if x.shape[-1] != self.in_features:
            raise ValueError(f"expected width {self.in_features}, got {x.shape[-1]}")
        output = None
        for lo in range(0, self.rank, self.rank_chunk):
            branch = self._branch(x, lo, min(lo + self.rank_chunk, self.rank))
            output = branch if output is None else output + branch
        assert output is not None
        output = output * (self.rank**-0.5)
        return output if self.bias is None else output + self.bias

    def dense_weight(self) -> torch.Tensor:
        """Materialize the equivalent weight for tests and diagnostics."""
        eye = torch.eye(self.in_features, device=self.factor1.device, dtype=self.factor1.dtype)
        return self(eye).transpose(0, 1)

    @property
    def structured_parameter_count(self) -> int:
        return self.factor1.numel() + self.factor2.numel() + (
            0 if self.bias is None else self.bias.numel()
        )

