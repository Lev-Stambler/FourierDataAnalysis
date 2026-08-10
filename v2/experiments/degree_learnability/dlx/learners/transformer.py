"""L1 — minimal causal Transformer (fixed config per PLAN §5; from scratch).

Deliberately small and standard: token embedding, configurable positional
geometry, pre-norm blocks with multi-head causal self-attention (torch SDPA) and
SwiGLU-free GELU MLP, tied output head. Historical protocols use the default
learned absolute positions. Later robustness protocols may use fixed sinusoidal
positions or attention-relative variants without changing historical hashes.
"""

from __future__ import annotations

import hashlib
import json
import math
from dataclasses import asdict, dataclass

import torch
import torch.nn.functional as F
from torch import nn


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
    position_encoding: str = "learned_absolute"
    rope_base: float = 10_000.0
    attention_window: int | None = None

    def to_json(self) -> str:
        payload = asdict(self)
        # Preserve every historical config hash exactly.
        if self.position_encoding == "learned_absolute":
            payload.pop("position_encoding")
        if self.position_encoding != "rope":
            payload.pop("rope_base")
        if self.attention_window is None:
            payload.pop("attention_window")
        return json.dumps(payload, sort_keys=True)

    @property
    def config_hash(self) -> str:
        return hashlib.sha256(self.to_json().encode()).hexdigest()[:16]

    def __post_init__(self) -> None:
        allowed = {
            "learned_absolute",
            "sinusoidal",
            "rope",
            "nope",
            "alibi",
            "reverse_alibi",
        }
        if self.position_encoding not in allowed:
            raise ValueError(
                f"position_encoding must be one of {sorted(allowed)}, "
                f"got {self.position_encoding!r}"
            )
        if self.rope_base <= 1.0:
            raise ValueError("rope_base must be greater than one")
        if self.attention_window is not None and not (
            1 <= self.attention_window <= self.ctx_len
        ):
            raise ValueError("attention_window must lie in [1,ctx_len]")
        if self.position_encoding == "rope" and (self.d_model // self.n_heads) % 2:
            raise ValueError("RoPE requires an even attention head dimension")


def _alibi_slopes(n_heads: int) -> torch.Tensor:
    """Return the head slopes from the original ALiBi construction."""

    def slopes_power_of_two(count: int) -> list[float]:
        start = 2 ** (-(2 ** -(math.log2(count) - 3)))
        return [start ** (index + 1) for index in range(count)]

    if math.log2(n_heads).is_integer():
        values = slopes_power_of_two(n_heads)
    else:
        lower = 2 ** math.floor(math.log2(n_heads))
        values = slopes_power_of_two(lower)
        extra = slopes_power_of_two(2 * lower)[0::2][: n_heads - lower]
        values.extend(extra)
    return torch.tensor(values, dtype=torch.float32)


def _sinusoidal_positions(ctx_len: int, d_model: int) -> torch.Tensor:
    """Standard fixed sinusoidal position table."""

    position = torch.arange(ctx_len, dtype=torch.float32)[:, None]
    frequencies = torch.exp(
        torch.arange(0, d_model, 2, dtype=torch.float32)
        * (-math.log(10_000.0) / d_model)
    )
    table = torch.zeros(ctx_len, d_model, dtype=torch.float32)
    table[:, 0::2] = torch.sin(position * frequencies)
    if d_model > 1:
        table[:, 1::2] = torch.cos(position * frequencies[: table[:, 1::2].shape[1]])
    return table


def _apply_rope(values: torch.Tensor, *, base: float) -> torch.Tensor:
    """Rotate paired head coordinates by their absolute sequence positions."""
    head_dim = values.shape[-1]
    if head_dim % 2:
        raise ValueError("RoPE requires an even head dimension")
    positions = torch.arange(
        values.shape[-2], device=values.device, dtype=torch.float32
    )
    inverse_frequencies = torch.exp(
        -math.log(base)
        * torch.arange(0, head_dim, 2, device=values.device, dtype=torch.float32)
        / head_dim
    )
    angles = positions[:, None] * inverse_frequencies[None, :]
    cosine = angles.cos().to(dtype=values.dtype)[None, None, :, :]
    sine = angles.sin().to(dtype=values.dtype)[None, None, :, :]
    even = values[..., 0::2]
    odd = values[..., 1::2]
    return torch.stack(
        (even * cosine - odd * sine, even * sine + odd * cosine), dim=-1
    ).flatten(-2)


class Block(nn.Module):
    def __init__(self, cfg: TransformerConfig):
        super().__init__()
        assert cfg.d_model % cfg.n_heads == 0
        self.n_heads = cfg.n_heads
        self.head_dim = cfg.d_model // cfg.n_heads
        self.position_encoding = cfg.position_encoding
        self.rope_base = cfg.rope_base
        self.attention_window = cfg.attention_window
        if self.position_encoding in {"alibi", "reverse_alibi"}:
            self.register_buffer(
                "alibi_slopes",
                _alibi_slopes(cfg.n_heads).reshape(1, cfg.n_heads, 1, 1),
                persistent=False,
            )
        else:
            self.alibi_slopes = None
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
        if self.position_encoding == "rope":
            q = _apply_rope(q, base=self.rope_base)
            k = _apply_rope(k, base=self.rope_base)
        dropout_p = self.drop.p if self.training else 0.0
        positions = torch.arange(T, device=h.device)
        distances = positions[:, None] - positions[None, :]
        causal = distances >= 0
        if self.attention_window is not None:
            causal &= distances < self.attention_window
        if self.position_encoding in {"alibi", "reverse_alibi"}:
            direction = -1.0 if self.position_encoding == "alibi" else 1.0
            bias = (
                direction
                * self.alibi_slopes.to(dtype=h.dtype)
                * distances.clamp_min(0).to(dtype=h.dtype)[None, None, :, :]
            )
            bias = bias.masked_fill(~causal[None, None, :, :], float("-inf"))
            att = F.scaled_dot_product_attention(
                q, k, v, attn_mask=bias, is_causal=False, dropout_p=dropout_p
            )
        elif self.attention_window is not None:
            bias = torch.zeros((T, T), device=h.device, dtype=h.dtype)
            bias = bias.masked_fill(~causal, float("-inf"))
            att = F.scaled_dot_product_attention(
                q, k, v, attn_mask=bias, is_causal=False, dropout_p=dropout_p
            )
        else:
            att = F.scaled_dot_product_attention(
                q, k, v, is_causal=True, dropout_p=dropout_p
            )
        att = att.transpose(1, 2).reshape(B, T, D)
        x = x + self.drop(self.out(att))
        x = x + self.drop(self.mlp(self.ln2(x)))
        return x


class CausalTransformer(nn.Module):
    def __init__(self, cfg: TransformerConfig):
        super().__init__()
        self.cfg = cfg
        self.tok = nn.Embedding(cfg.vocab, cfg.d_model)
        if cfg.position_encoding == "learned_absolute":
            self.pos = nn.Embedding(cfg.ctx_len, cfg.d_model)
            self.register_buffer("positional_table", None, persistent=False)
        elif cfg.position_encoding == "sinusoidal":
            self.pos = None
            self.register_buffer(
                "positional_table",
                _sinusoidal_positions(cfg.ctx_len, cfg.d_model),
                persistent=False,
            )
        else:
            self.pos = None
            self.register_buffer("positional_table", None, persistent=False)
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
        _, T = inputs.shape
        x = self.tok(inputs)
        if self.pos is not None:
            positions = torch.arange(T, device=inputs.device)
            x = x + self.pos(positions)[None, :, :]
        elif self.positional_table is not None:
            x = x + self.positional_table[:T].to(dtype=x.dtype)[None, :, :]
        for block in self.blocks:
            x = block(x)
        return self.head(self.ln_f(x))

    def n_params(self) -> int:
        return sum(p.numel() for p in self.parameters())


def cross_entropy_bits(logits: torch.Tensor, targets: torch.Tensor) -> float:
    """Mean cross-entropy in bits per token."""
    B, T, V = logits.shape
    loss = F.cross_entropy(
        logits.reshape(B * T, V), targets.reshape(B * T), reduction="mean"
    )
    return float(loss.item()) / math.log(2.0)
