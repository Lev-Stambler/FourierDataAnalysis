"""L2 — fixed MLP for the R3 tabular ladder (PLAN §3R): identical architecture
across datasets, 2 hidden layers of 256, early stopping. The learner is fixed by
the protocol; difficulty differences come from the data."""

from __future__ import annotations

import math

import torch
import torch.nn as nn
import torch.nn.functional as F


class FixedMLP(nn.Module):
    def __init__(self, n_features: int, q_features: list[int], n_classes: int,
                 width: int = 256, emb_dim: int = 8):
        super().__init__()
        self.embs = nn.ModuleList([nn.Embedding(q, emb_dim) for q in q_features])
        self.net = nn.Sequential(
            nn.Linear(n_features * emb_dim, width), nn.GELU(),
            nn.Linear(width, width), nn.GELU(),
            nn.Linear(width, n_classes),
        )

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        e = [emb(x[:, j]) for j, emb in enumerate(self.embs)]
        return self.net(torch.cat(e, dim=1))

    def n_params(self) -> int:
        return sum(p.numel() for p in self.parameters())


def cross_entropy_bits(logits: torch.Tensor, targets: torch.Tensor) -> float:
    return float(F.cross_entropy(logits, targets).item()) / math.log(2.0)
