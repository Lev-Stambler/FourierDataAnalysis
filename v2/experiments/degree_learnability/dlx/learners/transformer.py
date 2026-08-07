"""L1 — minimal causal Transformer (fixed config per PLAN §5; from scratch).

Deliberately small and standard: token embedding + learned absolute positions,
pre-norm blocks with multi-head causal self-attention (torch SDPA) and SwiGLU-free
GELU MLP, tied output head. No exotic tricks: the learner must stay a "standard
Transformer" so difficulty claims are about the data, not the architecture.
"""

from __future__ import annotations

import hashlib
import json
import math
from dataclasses import asdict, dataclass

import torch
import torch.nn as nn
import torch.nn.functional as F


@dataclass(frozen=True)
class TransformerConfig:
    vocab: int
    ctx_len: int
    d_model: int = 128
    n_layers: int = 4
    n_heads: int = 4
    mlp_mult: int = 4
    dropout: float = 0.0
    tie_weights: bool = True
    lr: float = 1e-3
    weight_decay: float = 0.1
    grad_clip: float = 1.0

    def to_json(self) -> str:
        return json.dumps(asdict(self), sort_keys=True)

    @property
    def config_hash(self) -> str:
        return hashlib.sha256(self.to_json().encode()).hexdigest()[:16]


class Block(nn.Module):
    def __init__(self, cfg: TransformerConfig):
        super().__init__()
        assert cfg.d_model % cfg.n_heads == 0
        self.n_heads = cfg.n_heads
        self.head_dim = cfg.d_model // cfg.n_heads
        self.ln1 = nn.LayerNorm(cfg.d_model)
        self.qkv = nn.Linear(cfg.d_model, 3 * cfg.d_model, bias=False)
        self.out = nn.Linear(cfg.d_model, cfg.d_model, bias=False)
        self.ln2 = nn.LayerNorm(cfg.d_model)
        w = cfg.mlp_mult * cfg.d_model
        self.mlp = nn.Sequential(
            nn.Linear(cfg.d_model, w, bias=False),
            nn.GELU(),
            nn.Linear(w, cfg.d_model, bias=False),
        )
        self.drop = nn.Dropout(cfg.dropout)

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        h = self.ln1(x)
        B, T, D = h.shape
        qkv = self.qkv(h).reshape(B, T, 3, self.n_heads, self.head_dim)
        q, k, v = [t.permute(0, 2, 1, 3) for t in qkv.unbind(dim=2)]
        att = F.scaled_dot_product_attention(q, k, v, is_causal=True,
                                             dropout_p=self.drop.p if self.training else 0.0)
        att = att.transpose(1, 2).reshape(B, T, D)
        x = x + self.drop(self.out(att))
        x = x + self.drop(self.mlp(self.ln2(x)))
        return x


class CausalTransformer(nn.Module):
    def __init__(self, cfg: TransformerConfig):
        super().__init__()
        self.cfg = cfg
        self.tok = nn.Embedding(cfg.vocab, cfg.d_model)
        self.pos = nn.Embedding(cfg.ctx_len, cfg.d_model)
        self.blocks = nn.ModuleList([Block(cfg) for _ in range(cfg.n_layers)])
        self.ln_f = nn.LayerNorm(cfg.d_model)
        self.head = nn.Linear(cfg.d_model, cfg.vocab, bias=False)
        if cfg.tie_weights:
            self.head.weight = self.tok.weight
        self.apply(self._init)

    @staticmethod
    def _init(m: nn.Module):
        if isinstance(m, nn.Linear):
            nn.init.normal_(m.weight, mean=0.0, std=0.02)
            if m.bias is not None:
                nn.init.zeros_(m.bias)
        elif isinstance(m, nn.Embedding):
            nn.init.normal_(m.weight, mean=0.0, std=0.02)

    def forward(self, inputs: torch.Tensor) -> torch.Tensor:
        """inputs (B, T) int -> logits (B, T, vocab)."""
        B, T = inputs.shape
        pos = torch.arange(T, device=inputs.device)
        x = self.tok(inputs) + self.pos(pos)[None, :, :]
        for block in self.blocks:
            x = block(x)
        return self.head(self.ln_f(x))

    def n_params(self) -> int:
        return sum(p.numel() for p in self.parameters())


def cross_entropy_bits(logits: torch.Tensor, targets: torch.Tensor) -> float:
    """Mean cross-entropy in bits per token."""
    B, T, V = logits.shape
    loss = F.cross_entropy(logits.reshape(B * T, V), targets.reshape(B * T),
                           reduction="mean")
    return float(loss.item()) / math.log(2.0)
