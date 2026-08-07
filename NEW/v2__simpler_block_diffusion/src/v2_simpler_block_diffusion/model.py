from __future__ import annotations

import json
import math
from dataclasses import dataclass
from pathlib import Path

import torch
import torch.nn.functional as F
from torch import nn

from .config import SimplerBlockDiffusionConfig
from .monarch import MonarchLinear


@dataclass
class StudentOutput:
    noisy_hidden: torch.Tensor
    clean_hidden: torch.Tensor
    selected_hidden: torch.Tensor | None = None
    selected_indices: torch.Tensor | None = None
    logits: torch.Tensor | None = None


@dataclass
class PrefixCache:
    keys: list[torch.Tensor]
    values: list[torch.Tensor]
    token_count: int


def _sinusoidal_noise_embedding(t: torch.Tensor, width: int) -> torch.Tensor:
    if width % 2:
        raise ValueError("noise embedding width must be even")
    frequencies = torch.exp(
        -math.log(10_000.0)
        * torch.arange(width // 2, device=t.device, dtype=torch.float32)
        / max(width // 2 - 1, 1)
    )
    angles = t.float().unsqueeze(-1) * frequencies * 1_000.0
    return torch.cat((torch.sin(angles), torch.cos(angles)), dim=-1).to(t.dtype)


def _apply_rope(x: torch.Tensor, positions: torch.Tensor, theta: float) -> torch.Tensor:
    """Apply RoPE to [batch, heads, tokens, head_dim]."""
    width = x.shape[-1]
    inv = theta ** (-torch.arange(0, width, 2, device=x.device, dtype=torch.float32) / width)
    angles = positions.float().unsqueeze(-1) * inv
    cos = angles.cos().to(x.dtype).unsqueeze(1)
    sin = angles.sin().to(x.dtype).unsqueeze(1)
    even, odd = x[..., 0::2], x[..., 1::2]
    return torch.stack((even * cos - odd * sin, even * sin + odd * cos), dim=-1).flatten(-2)


class NoiseConditioner(nn.Module):
    def __init__(self, config: SimplerBlockDiffusionConfig) -> None:
        super().__init__()
        self.width = config.noise_embedding_size
        self.up = nn.Linear(self.width, config.noise_mlp_size)
        self.down = nn.Linear(config.noise_mlp_size, config.hidden_size)

    def forward(self, rates: torch.Tensor) -> torch.Tensor:
        embedded = _sinusoidal_noise_embedding(rates, self.width).to(self.up.weight.dtype)
        return self.down(F.silu(self.up(embedded)))


class QKRMSNorm(nn.Module):
    def __init__(self, width: int, eps: float) -> None:
        super().__init__()
        self.weight = nn.Parameter(torch.ones(width))
        self.eps = eps

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        value = x.float() * torch.rsqrt(x.float().square().mean(-1, keepdim=True) + self.eps)
        return (value * self.weight.float()).to(x.dtype)


class CrossBlockAttention(nn.Module):
    def __init__(self, config: SimplerBlockDiffusionConfig) -> None:
        super().__init__()
        self.config = config
        d = config.hidden_size
        self.q_proj = nn.Linear(d, config.num_attention_heads * config.head_dim, bias=False)
        self.k_proj = nn.Linear(d, config.num_key_value_heads * config.head_dim, bias=False)
        self.v_proj = nn.Linear(d, config.num_key_value_heads * config.head_dim, bias=False)
        self.o_proj = nn.Linear(config.num_attention_heads * config.head_dim, d, bias=False)
        self.q_norm = QKRMSNorm(config.head_dim, config.rms_norm_eps)
        self.k_norm = QKRMSNorm(config.head_dim, config.rms_norm_eps)

    def _shape_q(self, x: torch.Tensor) -> torch.Tensor:
        b, t, _ = x.shape
        return self.q_proj(x).view(b, t, self.config.num_attention_heads, self.config.head_dim).transpose(1, 2)

    def _shape_kv(self, x: torch.Tensor, projection: nn.Linear) -> torch.Tensor:
        b, t, _ = x.shape
        value = projection(x).view(b, t, self.config.num_key_value_heads, self.config.head_dim)
        return value.transpose(1, 2)

    def project_kv(self, x: torch.Tensor, positions: torch.Tensor) -> tuple[torch.Tensor, torch.Tensor]:
        key = self.k_norm(self._shape_kv(x, self.k_proj))
        key = _apply_rope(key, positions, self.config.rope_theta)
        return key, self._shape_kv(x, self.v_proj)

    def attend(
        self,
        query_input: torch.Tensor,
        query_positions: torch.Tensor,
        key: torch.Tensor,
        value: torch.Tensor,
        allowed: torch.Tensor | None,
    ) -> torch.Tensor:
        b, q_len, _ = query_input.shape
        if key.shape[2] == 0:
            return query_input.new_zeros((b, q_len, self.config.hidden_size))
        query = self.q_norm(self._shape_q(query_input))
        query = _apply_rope(query, query_positions, self.config.rope_theta)
        repeats = self.config.num_attention_heads // self.config.num_key_value_heads
        key = key.repeat_interleave(repeats, dim=1)
        value = value.repeat_interleave(repeats, dim=1)
        mask = None if allowed is None else allowed[:, None, :, :]
        output = F.scaled_dot_product_attention(query, key, value, attn_mask=mask)
        output = output.transpose(1, 2).reshape(b, q_len, self.config.hidden_size)
        return self.o_proj(torch.nan_to_num(output))

    def forward_dual(self, x: torch.Tensor, token_mask: torch.Tensor | None) -> torch.Tensor:
        """Cross attention for [B, 2, blocks, block_size, D]."""
        b, streams, blocks, block_size, d = x.shape
        if streams != 2:
            raise ValueError("dual stream must contain [noisy, clean]")
        query_input = x.reshape(b, streams * blocks * block_size, d)
        clean = x[:, 1].reshape(b, blocks * block_size, d)
        positions = torch.arange(blocks * block_size, device=x.device).expand(b, -1)
        query_positions = positions[:, None, :].expand(-1, streams, -1).reshape(b, -1)
        key, value = self.project_kv(clean, positions)

        query_blocks = torch.arange(blocks, device=x.device).repeat_interleave(block_size)
        query_blocks = query_blocks.repeat(streams)
        key_blocks = torch.arange(blocks, device=x.device).repeat_interleave(block_size)
        # The structural mask is identical for every example. Preserve a
        # singleton batch dimension and let SDPA broadcast it instead of
        # cloning hundreds of MiB per layer at production batch sizes.
        allowed = (key_blocks.unsqueeze(0) < query_blocks.unsqueeze(1)).unsqueeze(0)
        if token_mask is not None:
            allowed = allowed.expand(b, -1, -1).clone()
            clean_mask = token_mask[:, 1].reshape(b, -1).bool()
            query_mask = token_mask.reshape(b, -1).bool()
            allowed &= clean_mask[:, None, :]
            allowed &= query_mask[:, :, None]
        output = self.attend(query_input, query_positions, key, value, allowed)
        return output.reshape(b, streams, blocks, block_size, d)


class BlockMonarchSwiGLU(nn.Module):
    def __init__(self, config: SimplerBlockDiffusionConfig) -> None:
        super().__init__()
        kwargs = {"nblocks": config.monarch_blocks, "rank": config.monarch_rank, "bias": False}
        self.gate = MonarchLinear(config.flat_block_size, config.local_hidden_size, **kwargs)
        self.up = MonarchLinear(config.flat_block_size, config.local_hidden_size, **kwargs)
        self.down = MonarchLinear(config.local_hidden_size, config.flat_block_size, **kwargs)
        self.block_size = config.block_size
        self.hidden_size = config.hidden_size

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        shape = x.shape
        if shape[-2:] != (self.block_size, self.hidden_size):
            raise ValueError("local mixer requires complete fixed-size blocks")
        flat = x.reshape(*shape[:-2], self.block_size * self.hidden_size)
        mixed = self.down(F.silu(self.gate(flat)) * self.up(flat))
        return mixed.reshape(shape)


class SimplerBlockLayer(nn.Module):
    def __init__(self, config: SimplerBlockDiffusionConfig) -> None:
        super().__init__()
        self.attn_norm = nn.RMSNorm(config.hidden_size, config.rms_norm_eps, elementwise_affine=False)
        self.local_norm = nn.RMSNorm(config.hidden_size, config.rms_norm_eps, elementwise_affine=False)
        self.attention = CrossBlockAttention(config)
        self.local = BlockMonarchSwiGLU(config)
        self.ada = nn.Linear(config.hidden_size, 6 * config.hidden_size)
        nn.init.zeros_(self.ada.weight)
        nn.init.zeros_(self.ada.bias)

    @staticmethod
    def _modulate(x: torch.Tensor, shift: torch.Tensor, scale: torch.Tensor) -> torch.Tensor:
        return x * (1 + scale.unsqueeze(-2)) + shift.unsqueeze(-2)

    def forward_dual(
        self, x: torch.Tensor, condition: torch.Tensor, token_mask: torch.Tensor | None
    ) -> torch.Tensor:
        shift_a, scale_a, gate_a, shift_m, scale_m, gate_m = self.ada(condition).chunk(6, dim=-1)
        attn_input = self._modulate(self.attn_norm(x), shift_a, scale_a)
        x = x + gate_a.unsqueeze(-2) * self.attention.forward_dual(attn_input, token_mask)
        local_input = self._modulate(self.local_norm(x), shift_m, scale_m)
        x = x + gate_m.unsqueeze(-2) * self.local(local_input)
        return x

    def forward_block(
        self,
        x: torch.Tensor,
        condition: torch.Tensor,
        positions: torch.Tensor,
        key: torch.Tensor,
        value: torch.Tensor,
    ) -> tuple[torch.Tensor, torch.Tensor, torch.Tensor]:
        shift_a, scale_a, gate_a, shift_m, scale_m, gate_m = self.ada(condition).chunk(6, dim=-1)
        attn_input = self._modulate(self.attn_norm(x), shift_a, scale_a)
        flat_attn = attn_input.squeeze(1)
        new_key, new_value = self.attention.project_kv(flat_attn, positions)
        attended = self.attention.attend(flat_attn, positions, key, value, None).unsqueeze(1)
        x = x + gate_a.unsqueeze(-2) * attended
        local_input = self._modulate(self.local_norm(x), shift_m, scale_m)
        x = x + gate_m.unsqueeze(-2) * self.local(local_input)
        return x, new_key, new_value


class SimplerBlockDiffusionForMaskedLM(nn.Module):
    config_class = SimplerBlockDiffusionConfig

    def __init__(self, config: SimplerBlockDiffusionConfig | None = None) -> None:
        super().__init__()
        self.config = config or SimplerBlockDiffusionConfig()
        self.embed_tokens = nn.Embedding(self.config.vocab_size, self.config.hidden_size)
        self.noise_conditioner = NoiseConditioner(self.config)
        self.layers = nn.ModuleList(
            SimplerBlockLayer(self.config) for _ in range(self.config.num_hidden_layers)
        )
        self.final_norm = nn.RMSNorm(
            self.config.hidden_size, self.config.rms_norm_eps, elementwise_affine=True
        )
        nn.init.normal_(self.embed_tokens.weight, std=self.config.initializer_range)

    def _validate_ids(self, ids: torch.Tensor) -> tuple[int, int]:
        if ids.ndim != 2:
            raise ValueError("token IDs must have shape [batch, sequence]")
        b, length = ids.shape
        if length > self.config.max_position_embeddings or length % self.config.block_size:
            raise ValueError("sequence must be block aligned and no longer than max context")
        return b, length

    def forward(
        self,
        noisy_ids: torch.Tensor,
        clean_ids: torch.Tensor,
        block_noise: torch.Tensor,
        *,
        token_mask: torch.Tensor | None = None,
        selected_indices: torch.Tensor | None = None,
        return_logits: bool = False,
    ) -> StudentOutput:
        b, length = self._validate_ids(noisy_ids)
        if clean_ids.shape != noisy_ids.shape:
            raise ValueError("clean and noisy IDs must have identical shapes")
        blocks = length // self.config.block_size
        if block_noise.shape != (b, blocks):
            raise ValueError(f"block_noise must have shape {(b, blocks)}")
        if token_mask is not None and token_mask.shape != (b, 2, length):
            raise ValueError(f"token_mask must have shape {(b, 2, length)}")

        ids = torch.stack((noisy_ids, clean_ids), dim=1)
        x = self.embed_tokens(ids).reshape(
            b, 2, blocks, self.config.block_size, self.config.hidden_size
        )
        rates = torch.stack((block_noise, torch.zeros_like(block_noise)), dim=1)
        condition = self.noise_conditioner(rates)
        shaped_mask = None
        if token_mask is not None:
            shaped_mask = token_mask.reshape(b, 2, blocks, self.config.block_size)
        for layer in self.layers:
            x = layer.forward_dual(x, condition, shaped_mask)
        x = self.final_norm(x)
        noisy_hidden = x[:, 0].reshape(b, length, self.config.hidden_size)
        clean_hidden = x[:, 1].reshape(b, length, self.config.hidden_size)

        selected_hidden = None
        logits = None
        if selected_indices is not None:
            flat = noisy_hidden.reshape(b * length, self.config.hidden_size)
            selected_hidden = flat.index_select(0, selected_indices.long())
            if return_logits:
                logits = F.linear(selected_hidden, self.embed_tokens.weight)
        elif return_logits:
            raise ValueError("full-sequence logits are forbidden; pass selected_indices")
        return StudentOutput(noisy_hidden, clean_hidden, selected_hidden, selected_indices, logits)

    def empty_cache(self, batch_size: int, *, device: torch.device, dtype: torch.dtype) -> PrefixCache:
        shape = (batch_size, self.config.num_key_value_heads, 0, self.config.head_dim)
        return PrefixCache(
            [torch.empty(shape, device=device, dtype=dtype) for _ in self.layers],
            [torch.empty(shape, device=device, dtype=dtype) for _ in self.layers],
            0,
        )

    def forward_block(
        self,
        token_ids: torch.Tensor,
        noise_rate: torch.Tensor,
        cache: PrefixCache,
        *,
        commit: bool = False,
    ) -> tuple[torch.Tensor, PrefixCache]:
        if token_ids.ndim != 2 or token_ids.shape[1] != self.config.block_size:
            raise ValueError("forward_block requires one complete block")
        b = token_ids.shape[0]
        if noise_rate.shape not in ((b,), (b, 1)):
            raise ValueError("noise_rate must contain one value per batch item")
        if cache.token_count % self.config.block_size:
            raise ValueError("prefix cache must end on a block boundary")
        positions = torch.arange(
            cache.token_count,
            cache.token_count + self.config.block_size,
            device=token_ids.device,
        ).expand(b, -1)
        condition = self.noise_conditioner(noise_rate.reshape(b, 1)).reshape(b, 1, self.config.hidden_size)
        x = self.embed_tokens(token_ids).unsqueeze(1)
        new_keys: list[torch.Tensor] = []
        new_values: list[torch.Tensor] = []
        for index, layer in enumerate(self.layers):
            x, key, value = layer.forward_block(
                x, condition, positions, cache.keys[index], cache.values[index]
            )
            new_keys.append(key)
            new_values.append(value)
        hidden = self.final_norm(x).squeeze(1)
        if not commit:
            return hidden, cache
        committed = PrefixCache(
            [torch.cat((old, new), dim=2) for old, new in zip(cache.keys, new_keys)],
            [torch.cat((old, new), dim=2) for old, new in zip(cache.values, new_values)],
            cache.token_count + self.config.block_size,
        )
        return hidden, committed

    def selected_logits(self, hidden: torch.Tensor) -> torch.Tensor:
        return F.linear(hidden, self.embed_tokens.weight)

    def num_parameters(self) -> int:
        return sum(parameter.numel() for parameter in self.parameters())

    def save_pretrained(self, directory: str | Path, metadata: dict | None = None) -> None:
        from safetensors.torch import save_file

        destination = Path(directory)
        destination.mkdir(parents=True, exist_ok=True)
        self.config.to_json_file(destination / "config.json")
        save_file(self.state_dict(), destination / "model.safetensors")
        if metadata is not None:
            (destination / "metadata.json").write_text(
                json.dumps(metadata, indent=2, sort_keys=True) + "\n"
            )

    @classmethod
    def from_pretrained(cls, directory: str | Path) -> "SimplerBlockDiffusionForMaskedLM":
        from safetensors.torch import load_file

        source = Path(directory)
        model = cls(SimplerBlockDiffusionConfig.from_json_file(source / "config.json"))
        model.load_state_dict(load_file(source / "model.safetensors"))
        return model
