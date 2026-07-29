from __future__ import annotations

import math

import torch
from torch import nn


class MonarchLinear(nn.Module):
    """Square/rectangular Monarch map with optional parallel rank."""

    def __init__(
        self,
        in_features: int,
        out_features: int,
        *,
        nblocks: int,
        rank: int = 1,
        bias: bool = True,
        rank_chunk: int = 8,
    ) -> None:
        super().__init__()
        if min(in_features, out_features, nblocks, rank, rank_chunk) <= 0:
            raise ValueError("Monarch dimensions, rank, and chunk must be positive")
        if in_features % nblocks or out_features % nblocks:
            raise ValueError("padding is external: widths must divide by nblocks")
        self.in_features = int(in_features)
        self.out_features = int(out_features)
        self.nblocks = int(nblocks)
        self.rank = int(rank)
        self.rank_chunk = int(rank_chunk)
        self.in_block = self.in_features // self.nblocks
        self.out_block = self.out_features // self.nblocks
        self.middle_block = min(self.in_block, self.out_block)
        self.factor1 = nn.Parameter(torch.empty(
            self.rank, self.nblocks, self.middle_block, self.in_block
        ))
        self.factor2 = nn.Parameter(torch.empty(
            self.rank, self.nblocks, self.out_block, self.middle_block
        ))
        self.bias = nn.Parameter(torch.empty(self.out_features)) if bias else None
        self.reset_parameters()

    def reset_parameters(self) -> None:
        for factor in (self.factor1, self.factor2):
            bound = 1 / math.sqrt(factor.shape[-1])
            nn.init.uniform_(factor, -bound, bound)
        if self.bias is not None:
            bound = 1 / math.sqrt(self.in_features)
            nn.init.uniform_(self.bias, -bound, bound)

    @property
    def saving(self) -> float:
        dense = self.in_features * self.out_features
        return (self.factor1.numel() + self.factor2.numel()) / dense

    def _branch_chunk(self, x: torch.Tensor, lo: int, hi: int) -> torch.Tensor:
        batch_shape = x.shape[:-1]
        batch = math.prod(batch_shape)
        x_blocks = x.reshape(batch, self.nblocks, self.in_block)
        first = torch.einsum(
            "bkp,tkqp->tbkq", x_blocks, self.factor1[lo:hi]
        )
        first = first.reshape(
            hi - lo, batch, self.middle_block, self.nblocks
        ).transpose(-1, -2).contiguous()
        second = torch.einsum(
            "tbkr,tksr->tbks", first, self.factor2[lo:hi]
        )
        second = second.transpose(-1, -2).reshape(
            hi - lo, batch, self.out_features
        )
        return second.sum(0).reshape(*batch_shape, self.out_features)

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        if x.shape[-1] != self.in_features:
            raise ValueError(
                f"expected final width {self.in_features}, got {x.shape[-1]}"
            )
        output = None
        for lo in range(0, self.rank, self.rank_chunk):
            value = self._branch_chunk(x, lo, min(lo + self.rank_chunk, self.rank))
            output = value if output is None else output + value
        output = output * (self.rank ** -0.5)
        if self.bias is not None:
            output = output + self.bias
        return output

    def extra_repr(self) -> str:
        return (
            f"in_features={self.in_features}, out_features={self.out_features}, "
            f"nblocks={self.nblocks}, rank={self.rank}, bias={self.bias is not None}"
        )
