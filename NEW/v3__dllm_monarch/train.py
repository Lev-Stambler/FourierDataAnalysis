from __future__ import annotations

import argparse
import copy
import hashlib
import json
import math
import os
import statistics
import threading
import time
from collections import defaultdict
from dataclasses import asdict, replace
from pathlib import Path
from types import SimpleNamespace
from typing import Any, Iterable

import torch
import torch.nn.functional as F
from torch import nn
from transformers import PretrainedConfig, PreTrainedModel

from CONFIG import (
    ARCHITECTURES,
    CONFIG,
    DEEP_CONFIG,
    DLLM_COMMIT,
    GLOBAL_KL_CONFIG,
    LIGER_VERSION,
    THINK_SFT_CONFIG,
    THROUGHPUT_CONFIG,
    Config,
)


_COMPILED_FLEX_ATTENTION = None
_FLEX_BLOCK_MASKS: dict[tuple[Any, ...], Any] = {}


def compiled_flex_attention(
    query: torch.Tensor,
    key: torch.Tensor,
    value: torch.Tensor,
    block_mask: Any,
) -> torch.Tensor:
    """Lazily compile PyTorch's maintained block-sparse attention kernel."""
    global _COMPILED_FLEX_ATTENTION
    if _COMPILED_FLEX_ATTENTION is None:
        from torch.nn.attention.flex_attention import flex_attention

        _COMPILED_FLEX_ATTENTION = torch.compile(flex_attention, fullgraph=True)
    return _COMPILED_FLEX_ATTENTION(
        query,
        key,
        value,
        block_mask=block_mask,
        enable_gqa=True,
    )


def write_json(path: str | Path, value: dict[str, Any]) -> None:
    destination = Path(path)
    destination.parent.mkdir(parents=True, exist_ok=True)
    temporary = destination.with_suffix(destination.suffix + ".part")
    temporary.write_text(json.dumps(value, indent=2, sort_keys=True) + "\n")
    temporary.replace(destination)


class MonarchConfig(PretrainedConfig):
    model_type = "v3_dllm_monarch"

    def __init__(self, source: Config | None = None, **kwargs: Any) -> None:
        source = source or CONFIG
        values = {
            "architecture_id": source.architecture_id,
            "vocab_size": source.vocab_size,
            "mask_token_id": source.mask_token_id,
            "hidden_size": source.hidden_size,
            "num_hidden_layers": source.layers,
            "num_attention_heads": source.attention_heads,
            "num_key_value_heads": source.kv_heads,
            "head_dim": source.head_dim,
            "block_size": source.block_size,
            "max_position_embeddings": source.max_position_embeddings,
            "rope_theta": source.rope_theta,
            "monarch_blocks": source.monarch_blocks,
            "monarch_rank": source.monarch_rank,
            "local_expansion": source.local_expansion,
            "rms_norm_eps": source.rms_eps,
            "noise_embedding_size": source.noise_width,
            "noise_mlp_size": source.noise_mlp_width,
            "initializer_range": source.initializer_range,
        }
        values.update(kwargs)
        super().__init__(tie_word_embeddings=True, **values)

    @property
    def flat_block_size(self) -> int:
        return self.block_size * self.hidden_size

    @property
    def local_hidden_size(self) -> int:
        return self.flat_block_size * self.local_expansion


class MonarchLinear(nn.Module):
    def __init__(self, in_features: int, out_features: int, *, nblocks: int, rank: int = 1) -> None:
        super().__init__()
        if min(in_features, out_features, nblocks, rank) <= 0:
            raise ValueError("Monarch dimensions must be positive")
        if in_features % nblocks or out_features % nblocks:
            raise ValueError("Monarch widths must divide by nblocks")
        self.in_features = in_features
        self.out_features = out_features
        self.nblocks = nblocks
        self.rank = rank
        self.in_block = in_features // nblocks
        self.out_block = out_features // nblocks
        self.middle_block = min(self.in_block, self.out_block)
        self.factor1 = nn.Parameter(torch.empty(rank, nblocks, self.middle_block, self.in_block))
        self.factor2 = nn.Parameter(torch.empty(rank, nblocks, self.out_block, self.middle_block))
        self.reset_parameters()

    def reset_parameters(self) -> None:
        for factor in (self.factor1, self.factor2):
            nn.init.uniform_(factor, -1 / math.sqrt(factor.shape[-1]), 1 / math.sqrt(factor.shape[-1]))

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        shape = x.shape[:-1]
        blocks = x.reshape(-1, self.nblocks, self.in_block)
        first = torch.einsum("bkp,tkqp->tbkq", blocks, self.factor1)
        first = first.reshape(self.rank, -1, self.middle_block, self.nblocks).transpose(-1, -2)
        second = torch.einsum("tbkr,tksr->tbks", first, self.factor2)
        output = second.transpose(-1, -2).reshape(self.rank, -1, self.out_features).sum(0)
        return output.reshape(*shape, self.out_features) * self.rank**-0.5


def sinusoidal_noise_embedding(t: torch.Tensor, width: int) -> torch.Tensor:
    frequencies = torch.exp(
        -math.log(10_000.0)
        * torch.arange(width // 2, device=t.device, dtype=torch.float32)
        / max(width // 2 - 1, 1)
    )
    angles = t.float().unsqueeze(-1) * frequencies * 1_000.0
    return torch.cat((angles.sin(), angles.cos()), dim=-1).to(t.dtype)


def apply_rope(x: torch.Tensor, positions: torch.Tensor, theta: float) -> torch.Tensor:
    width = x.shape[-1]
    inv = theta ** (-torch.arange(0, width, 2, device=x.device, dtype=torch.float32) / width)
    angles = positions.float().unsqueeze(-1) * inv
    cos, sin = angles.cos().to(x.dtype).unsqueeze(1), angles.sin().to(x.dtype).unsqueeze(1)
    even, odd = x[..., 0::2], x[..., 1::2]
    return torch.stack((even * cos - odd * sin, even * sin + odd * cos), dim=-1).flatten(-2)


class NoiseConditioner(nn.Module):
    def __init__(self, config: MonarchConfig) -> None:
        super().__init__()
        self.width = config.noise_embedding_size
        self.up = nn.Linear(self.width, config.noise_mlp_size)
        self.down = nn.Linear(config.noise_mlp_size, config.hidden_size)

    def forward(self, rates: torch.Tensor) -> torch.Tensor:
        embedded = sinusoidal_noise_embedding(rates, self.width).to(self.up.weight.dtype)
        return self.down(F.silu(self.up(embedded)))


class QKRMSNorm(nn.Module):
    def __init__(self, width: int, eps: float) -> None:
        super().__init__()
        self.weight = nn.Parameter(torch.ones(width))
        self.eps = eps

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        normalized = x.float() * torch.rsqrt(x.float().square().mean(-1, keepdim=True) + self.eps)
        return (normalized * self.weight.float()).to(x.dtype)


class CrossBlockAttention(nn.Module):
    def __init__(self, config: MonarchConfig) -> None:
        super().__init__()
        self.config = config
        d = config.hidden_size
        self.q_proj = nn.Linear(d, config.num_attention_heads * config.head_dim, bias=False)
        self.k_proj = nn.Linear(d, config.num_key_value_heads * config.head_dim, bias=False)
        self.v_proj = nn.Linear(d, config.num_key_value_heads * config.head_dim, bias=False)
        self.o_proj = nn.Linear(config.num_attention_heads * config.head_dim, d, bias=False)
        self.q_norm = QKRMSNorm(config.head_dim, config.rms_norm_eps)
        self.k_norm = QKRMSNorm(config.head_dim, config.rms_norm_eps)

    def shape_q(self, x: torch.Tensor) -> torch.Tensor:
        b, t, _ = x.shape
        return self.q_proj(x).view(b, t, self.config.num_attention_heads, self.config.head_dim).transpose(1, 2)

    def shape_kv(self, x: torch.Tensor, projection: nn.Linear) -> torch.Tensor:
        b, t, _ = x.shape
        return projection(x).view(b, t, self.config.num_key_value_heads, self.config.head_dim).transpose(1, 2)

    def project_kv(self, x: torch.Tensor, positions: torch.Tensor) -> tuple[torch.Tensor, torch.Tensor]:
        key = apply_rope(self.k_norm(self.shape_kv(x, self.k_proj)), positions, self.config.rope_theta)
        return key, self.shape_kv(x, self.v_proj)

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
        query = apply_rope(self.q_norm(self.shape_q(query_input)), query_positions, self.config.rope_theta)
        repeats = self.config.num_attention_heads // self.config.num_key_value_heads
        key = key.repeat_interleave(repeats, dim=1)
        value = value.repeat_interleave(repeats, dim=1)
        mask = None if allowed is None else allowed[:, None]
        output = F.scaled_dot_product_attention(query, key, value, attn_mask=mask)
        return self.o_proj(torch.nan_to_num(output.transpose(1, 2).reshape(b, q_len, -1)))

    def attend_flex(
        self,
        query_input: torch.Tensor,
        query_positions: torch.Tensor,
        key: torch.Tensor,
        value: torch.Tensor,
    ) -> torch.Tensor:
        from torch.nn.attention.flex_attention import create_block_mask

        batch, query_length, _ = query_input.shape
        key_length = key.shape[2]
        cache_key = (
            query_input.device.type,
            query_input.device.index,
            query_length,
            key_length,
            self.config.block_size,
        )
        block_mask = _FLEX_BLOCK_MASKS.get(cache_key)
        if block_mask is None:
            sequence_length = key_length
            block_size = self.config.block_size

            def prior_block_mask(_batch, _head, query_index, key_index):
                query_block = (query_index % sequence_length) // block_size
                key_block = key_index // block_size
                return key_block < query_block

            block_mask = create_block_mask(
                prior_block_mask,
                None,
                None,
                query_length,
                key_length,
                device=query_input.device,
                BLOCK_SIZE=128,
                _compile=True,
            )
            _FLEX_BLOCK_MASKS[cache_key] = block_mask
        query = apply_rope(self.q_norm(self.shape_q(query_input)), query_positions, self.config.rope_theta)
        output = compiled_flex_attention(query, key, value, block_mask)
        return self.o_proj(torch.nan_to_num(output.transpose(1, 2).reshape(batch, query_length, -1)))

    def forward_dual(
        self,
        x: torch.Tensor,
        positions: torch.Tensor | None = None,
        clean_key_valid: torch.Tensor | None = None,
    ) -> torch.Tensor:
        b, streams, blocks, block_size, d = x.shape
        query_input = x.reshape(b, streams * blocks * block_size, d)
        clean = x[:, 1].reshape(b, blocks * block_size, d)
        if positions is None:
            positions = torch.arange(blocks * block_size, device=x.device).expand(b, -1)
        elif positions.shape != (b, blocks * block_size):
            raise ValueError("positions must match the flattened block sequence")
        query_positions = positions[:, None].expand(-1, streams, -1).reshape(b, -1)
        key, value = self.project_kv(clean, positions)
        if (
            getattr(self.config, "use_flex_attention", False)
            and clean_key_valid is None
            and x.is_cuda
        ):
            return self.attend_flex(query_input, query_positions, key, value).reshape(x.shape)
        query_blocks = torch.arange(blocks, device=x.device).repeat_interleave(block_size).repeat(streams)
        key_blocks = torch.arange(blocks, device=x.device).repeat_interleave(block_size)
        allowed = (key_blocks.unsqueeze(0) < query_blocks.unsqueeze(1)).unsqueeze(0)
        if clean_key_valid is not None:
            if clean_key_valid.shape != (b, blocks * block_size):
                raise ValueError("clean key validity must match the flattened block sequence")
            allowed = allowed & clean_key_valid[:, None, :]
        return self.attend(query_input, query_positions, key, value, allowed).reshape(x.shape)


class BlockMonarchSwiGLU(nn.Module):
    def __init__(self, config: MonarchConfig) -> None:
        super().__init__()
        kwargs = {"nblocks": config.monarch_blocks, "rank": config.monarch_rank}
        self.gate = MonarchLinear(config.flat_block_size, config.local_hidden_size, **kwargs)
        self.up = MonarchLinear(config.flat_block_size, config.local_hidden_size, **kwargs)
        self.down = MonarchLinear(config.local_hidden_size, config.flat_block_size, **kwargs)
        self.block_size, self.hidden_size = config.block_size, config.hidden_size

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        shape = x.shape
        flat = x.reshape(*shape[:-2], self.block_size * self.hidden_size)
        return self.down(F.silu(self.gate(flat)) * self.up(flat)).reshape(shape)


class MonarchLayer(nn.Module):
    def __init__(self, config: MonarchConfig) -> None:
        super().__init__()
        self.attn_norm = nn.RMSNorm(config.hidden_size, config.rms_norm_eps, elementwise_affine=False)
        self.local_norm = nn.RMSNorm(config.hidden_size, config.rms_norm_eps, elementwise_affine=False)
        self.attention = CrossBlockAttention(config)
        self.local = BlockMonarchSwiGLU(config)
        self.ada = nn.Linear(config.hidden_size, 6 * config.hidden_size)
        nn.init.zeros_(self.ada.weight)
        nn.init.zeros_(self.ada.bias)

    @staticmethod
    def modulate(x: torch.Tensor, shift: torch.Tensor, scale: torch.Tensor) -> torch.Tensor:
        return x * (1 + scale.unsqueeze(-2)) + shift.unsqueeze(-2)

    def forward(
        self,
        x: torch.Tensor,
        condition: torch.Tensor,
        positions: torch.Tensor | None = None,
        token_valid: torch.Tensor | None = None,
    ) -> torch.Tensor:
        valid = None if token_valid is None else token_valid.unsqueeze(-1).to(x.dtype)
        shift_a, scale_a, gate_a, shift_m, scale_m, gate_m = self.ada(condition).chunk(6, dim=-1)
        attn_input = self.modulate(self.attn_norm(x), shift_a, scale_a)
        if valid is not None:
            attn_input = attn_input * valid
        clean_key_valid = None if token_valid is None else token_valid[:, 1].flatten(1)
        attn_update = self.attention.forward_dual(attn_input, positions, clean_key_valid)
        if valid is not None:
            attn_update = attn_update * valid
        x = x + gate_a.unsqueeze(-2) * attn_update
        if valid is not None:
            x = x * valid
        local_input = self.modulate(self.local_norm(x), shift_m, scale_m)
        if valid is not None:
            local_input = local_input * valid
        local_update = self.local(local_input)
        if valid is not None:
            local_update = local_update * valid
        output = x + gate_m.unsqueeze(-2) * local_update
        return output if valid is None else output * valid


class MonarchStudent(PreTrainedModel):
    config_class = MonarchConfig
    main_input_name = "input_ids"

    def __init__(self, config: MonarchConfig | None = None) -> None:
        super().__init__(config or MonarchConfig())
        self.embed_tokens = nn.Embedding(self.config.vocab_size, self.config.hidden_size)
        self.noise_conditioner = NoiseConditioner(self.config)
        self.layers = nn.ModuleList(MonarchLayer(self.config) for _ in range(self.config.num_hidden_layers))
        self.final_norm = nn.RMSNorm(self.config.hidden_size, self.config.rms_norm_eps, elementwise_affine=True)
        self._compiled_train_stack = None
        nn.init.normal_(self.embed_tokens.weight, std=self.config.initializer_range)

    @property
    def device(self) -> torch.device:
        return self.embed_tokens.weight.device

    def get_input_embeddings(self) -> nn.Module:
        return self.embed_tokens

    def set_input_embeddings(self, value: nn.Module) -> None:
        self.embed_tokens = value

    def get_output_embeddings(self) -> nn.Module:
        return self.embed_tokens

    def forward_features(
        self,
        noisy_ids: torch.Tensor,
        clean_ids: torch.Tensor,
        block_noise: torch.Tensor,
        position_ids: torch.Tensor | None = None,
        token_valid: torch.Tensor | None = None,
    ) -> torch.Tensor:
        b, length = noisy_ids.shape
        if clean_ids.shape != noisy_ids.shape or length % self.config.block_size:
            raise ValueError("training inputs must be matching block-aligned matrices")
        blocks = length // self.config.block_size
        ids = torch.stack((noisy_ids, clean_ids), dim=1)
        x = self.embed_tokens(ids).reshape(b, 2, blocks, self.config.block_size, self.config.hidden_size)
        if token_valid is not None:
            if token_valid.shape != noisy_ids.shape:
                raise ValueError("token validity must match input IDs")
            dual_valid = token_valid[:, None].expand(-1, 2, -1).reshape(
                b, 2, blocks, self.config.block_size
            )
            x = x * dual_valid.unsqueeze(-1).to(x.dtype)
        else:
            dual_valid = None
        rates = torch.stack((block_noise, torch.zeros_like(block_noise)), dim=1)
        condition = self.noise_conditioner(rates)
        for layer in self.layers:
            x = layer(x, condition, position_ids, dual_valid)
        return self.final_norm(x)[:, 0].reshape(b * length, self.config.hidden_size)

    def enable_compiled_train_stack(self) -> None:
        if self._compiled_train_stack is None:
            self._compiled_train_stack = torch.compile(
                self.forward_features,
                fullgraph=True,
                dynamic=False,
                backend="inductor",
                mode="default",
            )

    def forward_train(
        self,
        noisy_ids: torch.Tensor,
        clean_ids: torch.Tensor,
        block_noise: torch.Tensor,
        selected_indices: torch.Tensor,
        position_ids: torch.Tensor | None = None,
        token_valid: torch.Tensor | None = None,
    ) -> torch.Tensor:
        feature_function = (
            self._compiled_train_stack
            if self.training and self._compiled_train_stack is not None
            else self.forward_features
        )
        noisy_hidden = feature_function(
            noisy_ids,
            clean_ids,
            block_noise,
            position_ids,
            token_valid,
        )
        return noisy_hidden.index_select(0, selected_indices.long())

    def forward(
        self,
        input_ids: torch.Tensor | None = None,
        attention_mask: torch.Tensor | None = None,
        position_ids: torch.Tensor | None = None,
        past_key_values: torch.Tensor | None = None,
        use_cache: bool = False,
        labels: torch.Tensor | None = None,
        noisy_ids: torch.Tensor | None = None,
        clean_ids: torch.Tensor | None = None,
        block_noise: torch.Tensor | None = None,
        selected_indices: torch.Tensor | None = None,
        **_: Any,
    ) -> SimpleNamespace:
        if noisy_ids is not None:
            if clean_ids is None or block_noise is None or selected_indices is None:
                raise ValueError("student training forward is incomplete")
            return SimpleNamespace(
                selected_hidden=self.forward_train(noisy_ids, clean_ids, block_noise, selected_indices)
            )
        # Training never calls this path. dLLM generation uses small batches, so
        # storing token IDs and recomputing the prefix is the smallest correct
        # cache adapter; it preserves block causality without custom cache code.
        del labels
        if input_ids is None:
            raise ValueError("input_ids are required for generation")
        if past_key_values is None:
            full_ids = input_ids
            full_positions = position_ids
        else:
            if not isinstance(past_key_values, dict) or past_key_values.get("schema") != "v3-token-cache-v1":
                raise ValueError("generation cache is not a V3 token cache")
            full_ids = torch.cat((past_key_values["input_ids"], input_ids), dim=1)
            cached_positions = past_key_values.get("position_ids")
            if cached_positions is None or position_ids is None:
                full_positions = None
            else:
                full_positions = torch.cat((cached_positions, position_ids), dim=1)
        if full_ids.shape[1] % self.config.block_size:
            raise ValueError("generation input must be block aligned")
        b, length = full_ids.shape
        blocks = length // self.config.block_size
        if full_positions is None:
            full_positions = torch.arange(length, device=full_ids.device).expand(b, -1)
        if full_positions.shape != full_ids.shape:
            raise ValueError("generation position IDs must match the reconstructed sequence")
        pad_token_id = self.config.pad_token_id
        token_valid = torch.ones_like(full_ids, dtype=torch.bool) if pad_token_id is None else full_ids != pad_token_id
        if attention_mask is not None:
            if attention_mask.dtype != torch.bool:
                attention_mask = attention_mask != 0
            visible_keys = attention_mask.squeeze(1).any(dim=-2)
            if visible_keys.shape == token_valid.shape:
                token_valid = token_valid & visible_keys
        noisy = full_ids
        clean = full_ids
        rates = (full_ids == self.config.mask_token_id).reshape(b, blocks, -1).float().mean(-1)
        all_indices = torch.arange(b * length, device=full_ids.device)
        hidden = self.forward_train(
            noisy,
            clean,
            rates,
            all_indices,
            position_ids=full_positions,
            token_valid=token_valid,
        )
        logits = F.linear(hidden, self.embed_tokens.weight).reshape(b, length, self.config.vocab_size)
        # The distillation softmax intentionally removes this vocabulary row,
        # so its output logit is untrained and must never be sampled. The same
        # token remains valid as an input corruption state.
        logits[..., self.config.mask_token_id] = -torch.inf
        cache = {
            "schema": "v3-token-cache-v1",
            "input_ids": full_ids.detach(),
            "position_ids": full_positions.detach(),
        }
        return SimpleNamespace(
            logits=logits[:, -input_ids.shape[1] :],
            past_key_values=cache if use_cache else past_key_values,
        )


@torch.no_grad()
def zeropower_via_newton_schulz5(gradient: torch.Tensor, steps: int = 5) -> torch.Tensor:
    value = gradient.bfloat16()
    transposed = value.shape[-2] > value.shape[-1]
    if transposed:
        value = value.mT
    value = value / (value.norm(dim=(-2, -1), keepdim=True) + 1e-7)
    for _ in range(steps):
        gram = value @ value.mT
        value = 3.4445 * value + (-4.7750 * gram + 2.0315 * gram @ gram) @ value
    return value.mT if transposed else value


class BatchedMonarchMuon(torch.optim.Optimizer):
    """Muon over every final-two-dimensional factor slice independently."""

    def __init__(self, params: Iterable[nn.Parameter], lr: float, momentum: float = 0.95) -> None:
        super().__init__(params, dict(lr=lr, momentum=momentum, nesterov=True, ns_steps=5, weight_decay=0.0))
        if any(parameter.ndim < 3 for group in self.param_groups for parameter in group["params"]):
            raise ValueError("Monarch Muon expects batched matrices")

    @torch.no_grad()
    def step(self, closure=None):
        loss = None
        if closure is not None:
            with torch.enable_grad():
                loss = closure()
        for group in self.param_groups:
            buckets: dict[tuple[Any, ...], list[nn.Parameter]] = defaultdict(list)
            for parameter in group["params"]:
                if parameter.grad is not None:
                    buckets[(parameter.device, parameter.dtype, *parameter.shape[-2:])].append(parameter)
            for parameters in buckets.values():
                rows, columns = parameters[0].shape[-2:]
                counts = [p.numel() // (rows * columns) for p in parameters]
                directions = torch.empty(sum(counts), rows, columns, device=parameters[0].device, dtype=torch.bfloat16)
                offset = 0
                for parameter, count in zip(parameters, counts, strict=True):
                    state = self.state[parameter]
                    if "momentum_buffer" not in state:
                        state["momentum_buffer"] = torch.zeros_like(parameter)
                        state["step"] = 0
                    state["step"] += 1
                    momentum = state["momentum_buffer"]
                    momentum.lerp_(parameter.grad, 1 - group["momentum"])
                    direction = parameter.grad.lerp(momentum, group["momentum"])
                    directions[offset : offset + count].copy_(direction.reshape(count, rows, columns))
                    offset += count
                updates = zeropower_via_newton_schulz5(directions, group["ns_steps"])
                adjusted_lr = group["lr"] * math.sqrt(max(1.0, rows / columns))
                offset = 0
                for parameter, count in zip(parameters, counts, strict=True):
                    update = updates[offset : offset + count].reshape(parameter.shape)
                    parameter.add_(update.to(parameter.dtype), alpha=-adjusted_lr)
                    offset += count
        return loss


class CompositeOptimizer(torch.optim.Optimizer):
    def __init__(self, dense: torch.optim.Optimizer, monarch: torch.optim.Optimizer, auxiliary: torch.optim.Optimizer) -> None:
        parameters = [p for child in (dense, monarch, auxiliary) for group in child.param_groups for p in group["params"]]
        super().__init__(parameters, {})
        self.dense, self.monarch, self.auxiliary = dense, monarch, auxiliary
        self.param_groups = dense.param_groups + monarch.param_groups + auxiliary.param_groups
        self.supervised_tokens = 0
        self.contexts = 0
        self.profile_step_callback = None
        self.latest_gradient_metrics: dict[str, float] = {}

    @staticmethod
    def _route_gradient_metrics(name: str, optimizer: torch.optim.Optimizer) -> dict[str, float]:
        gradients = [
            parameter.grad
            for group in optimizer.param_groups
            for parameter in group["params"]
            if parameter.grad is not None
        ]
        if not gradients:
            return {
                f"train/grad_norm_{name}_postclip": 0.0,
                f"train/grad_rms_{name}_postclip": 0.0,
            }
        norms = torch._foreach_norm(gradients, 2)
        norm = torch.stack([value.float().square() for value in norms]).sum().sqrt()
        elements = sum(gradient.numel() for gradient in gradients)
        return {
            f"train/grad_norm_{name}_postclip": float(norm.item()),
            f"train/grad_rms_{name}_postclip": float((norm / math.sqrt(elements)).item()),
        }

    def step(self, closure=None):
        if self.profile_step_callback is not None:
            self.profile_step_callback("start")
        self.latest_gradient_metrics = {}
        for name, child in (
            ("dense_muon", self.dense),
            ("monarch_muon", self.monarch),
            ("adamw", self.auxiliary),
        ):
            self.latest_gradient_metrics.update(self._route_gradient_metrics(name, child))
        loss = self.dense.step(closure)
        self.monarch.step()
        self.auxiliary.step()
        if self.profile_step_callback is not None:
            self.profile_step_callback("end")
        return loss

    def zero_grad(self, set_to_none: bool = True) -> None:
        for child in (self.dense, self.monarch, self.auxiliary):
            child.zero_grad(set_to_none=set_to_none)

    def state_dict(self) -> dict[str, Any]:
        return {
            "schema": "v3-composite-muon-v1",
            "dense": self.dense.state_dict(),
            "monarch": self.monarch.state_dict(),
            "auxiliary": self.auxiliary.state_dict(),
            "supervised_tokens": self.supervised_tokens,
            "contexts": self.contexts,
        }

    def load_state_dict(self, state: dict[str, Any]) -> None:
        if state.get("schema") != "v3-composite-muon-v1":
            raise ValueError("invalid V3 optimizer state")
        self.dense.load_state_dict(state["dense"])
        self.monarch.load_state_dict(state["monarch"])
        self.auxiliary.load_state_dict(state["auxiliary"])
        self.supervised_tokens = int(state.get("supervised_tokens", 0))
        self.contexts = int(state.get("contexts", 0))


def apply_learning_rates(
    optimizer: torch.optim.Optimizer,
    scheduler: Any,
    config: Config,
) -> dict[str, float]:
    """Apply effective route LRs after checkpoint optimizer/scheduler restore."""
    base = getattr(optimizer, "optimizer", optimizer)
    if not isinstance(base, CompositeOptimizer):
        raise TypeError("learning-rate override requires CompositeOptimizer")
    for group in base.dense.param_groups:
        group["lr"] = config.muon_lr
        group["initial_lr"] = config.muon_lr
    for group in base.monarch.param_groups:
        group["lr"] = config.muon_lr
        group["initial_lr"] = config.muon_lr
    for group in base.auxiliary.param_groups:
        group["lr"] = config.adamw_lr
        group["initial_lr"] = config.adamw_lr

    inner_scheduler = getattr(scheduler, "scheduler", scheduler)
    effective = [group["lr"] for group in base.param_groups]
    if inner_scheduler is not None:
        inner_scheduler.base_lrs = list(effective)
        inner_scheduler._last_lr = list(effective)
    return {
        "dense_muon": float(base.dense.param_groups[0]["lr"]),
        "monarch_muon": float(base.monarch.param_groups[0]["lr"]),
        "adamw": float(base.auxiliary.param_groups[0]["lr"]),
    }


def build_optimizer(model: MonarchStudent, config: Config = CONFIG) -> tuple[CompositeOptimizer, dict[str, Any]]:
    routes: dict[str, list[tuple[str, nn.Parameter]]] = {"dense_muon": [], "monarch_muon": [], "adamw": []}
    for name, parameter in model.named_parameters():
        if ".local." in name and name.endswith(("factor1", "factor2")):
            routes["monarch_muon"].append((name, parameter))
        elif name == "embed_tokens.weight" or parameter.ndim == 1:
            routes["adamw"].append((name, parameter))
        elif parameter.ndim == 2:
            routes["dense_muon"].append((name, parameter))
        else:
            raise RuntimeError(f"unroutable parameter {name}: {tuple(parameter.shape)}")
    routed = [p for values in routes.values() for _, p in values]
    if len({id(p) for p in routed}) != len(routed) or len(routed) != len(list(model.parameters())):
        raise RuntimeError("optimizer routing is overlapping or incomplete")
    dense = torch.optim.Muon(
        [p for _, p in routes["dense_muon"]], lr=config.muon_lr, momentum=0.95,
        nesterov=True, ns_steps=5, adjust_lr_fn="original", weight_decay=0.0,
    )
    monarch = BatchedMonarchMuon([p for _, p in routes["monarch_muon"]], lr=config.muon_lr)
    adamw_parameters = [p for _, p in routes["adamw"]]
    use_fused = bool(adamw_parameters and adamw_parameters[0].is_cuda)
    auxiliary = torch.optim.AdamW(
        adamw_parameters, lr=config.adamw_lr, betas=(0.9, 0.95), eps=1e-8,
        weight_decay=config.adamw_weight_decay, fused=use_fused,
    )
    inventory = {
        key: {"tensors": len(values), "parameters": sum(p.numel() for _, p in values), "names": [n for n, _ in values]}
        for key, values in routes.items()
    }
    inventory["total"] = {"tensors": len(routed), "parameters": sum(p.numel() for p in routed)}
    return CompositeOptimizer(dense, monarch, auxiliary), inventory


def remove_mask_row(weight: torch.Tensor, mask_token_id: int) -> torch.Tensor:
    return torch.cat((weight[:mask_token_id], weight[mask_token_id + 1 :]), dim=0)


def remap_labels(labels: torch.Tensor, mask_token_id: int) -> torch.Tensor:
    if bool((labels == mask_token_id).any()):
        raise ValueError("mask token cannot be a hard label")
    return labels - (labels > mask_token_id).to(labels.dtype)


def right_shifted_selected_hidden(
    hidden: torch.Tensor,
    selected_indices: torch.Tensor,
) -> torch.Tensor:
    """Select Dream's raw h[i-1] for logical output position i."""
    if hidden.ndim != 3:
        raise ValueError("Dream hidden states must have shape [batch, length, width]")
    length = hidden.shape[1]
    if bool((selected_indices.remainder(length) == 0).any()):
        raise ValueError("Dream right shift cannot supervise position zero")
    return hidden.reshape(-1, hidden.shape[-1]).index_select(0, selected_indices.long() - 1)


def create_right_shifted_dream_bd3_mask(
    length: int,
    block_size: int,
    device: torch.device | str,
) -> torch.Tensor:
    """BD3 mask whose noisy raw query i-1 predicts logical Dream token i."""
    indices = torch.arange(2 * length, device=device)
    query = indices[:, None]
    key = indices[None, :]
    query_clean = query >= length
    key_clean = key >= length

    # Dream shifts raw logits right. A noisy query at physical position q is
    # therefore assigned to the block of logical output min(q + 1, L - 1).
    noisy_logical_query = (query + 1).clamp_max(length - 1)
    query_blocks = torch.where(
        query_clean,
        (query - length) // block_size,
        noisy_logical_query // block_size,
    )
    key_blocks = torch.where(key_clean, (key - length) // block_size, key // block_size)
    block_diagonal = (query_blocks == key_blocks) & (query_clean == key_clean)
    offset_block_causal = (query_blocks > key_blocks) & key_clean & ~query_clean
    clean_block_causal = (query_blocks >= key_blocks) & key_clean & query_clean
    return (block_diagonal | offset_block_causal | clean_block_causal)[None, None]


METRIC_NAMES = (
    "full_kl",
    "student_ce",
    "teacher_ce",
    "top1_agreement",
    "student_hard_top1",
    "teacher_hard_top1",
    "teacher_top1_probability",
    "teacher_entropy",
    "selected_t",
    "selected_block_mask_fraction",
)


def materialized_kl_reference(
    student_hidden: torch.Tensor,
    student_weight: torch.Tensor,
    teacher_hidden: torch.Tensor,
    teacher_weight: torch.Tensor,
    mask_token_id: int,
    chunk_size: int = CONFIG.diagnostic_chunk_size,
) -> torch.Tensor:
    """Tiny-test oracle; production KL always comes from the selected-row kernel."""
    student_weight = remove_mask_row(student_weight, mask_token_id)
    teacher_weight = remove_mask_row(teacher_weight, mask_token_id)
    values = []
    with torch.no_grad() if not student_hidden.requires_grad and not student_weight.requires_grad else torch.enable_grad():
        for start in range(0, student_hidden.shape[0], chunk_size):
            stop = min(start + chunk_size, student_hidden.shape[0])
            student_logits = F.linear(student_hidden[start:stop], student_weight).float()
            teacher_logits = F.linear(teacher_hidden[start:stop], teacher_weight).float()
            teacher_logp = teacher_logits.log_softmax(-1)
            values.append(
                (teacher_logp.exp() * (teacher_logp - student_logits.log_softmax(-1))).sum(-1)
            )
    return torch.cat(values)


def materialized_diagnostic_metrics(
    student_hidden: torch.Tensor,
    student_weight: torch.Tensor,
    teacher_hidden: torch.Tensor,
    teacher_weight: torch.Tensor,
    labels: torch.Tensor,
    mask_token_id: int,
    chunk_size: int = CONFIG.diagnostic_chunk_size,
) -> dict[str, torch.Tensor]:
    student_weight = remove_mask_row(student_weight, mask_token_id)
    teacher_weight = remove_mask_row(teacher_weight, mask_token_id)
    labels = remap_labels(labels, mask_token_id)
    sums = torch.zeros(7, device=student_hidden.device, dtype=torch.float64)
    rows = student_hidden.shape[0]
    with torch.no_grad():
        for start in range(0, rows, chunk_size):
            stop = min(start + chunk_size, rows)
            student_logits = F.linear(student_hidden[start:stop], student_weight).float()
            teacher_logits = F.linear(teacher_hidden[start:stop], teacher_weight).float()
            student_logp = student_logits.log_softmax(-1)
            teacher_logp = teacher_logits.log_softmax(-1)
            teacher_p = teacher_logp.exp()
            local_labels = labels[start:stop, None]
            sums[0] += -student_logp.gather(1, local_labels).sum().double()
            sums[1] += -teacher_logp.gather(1, local_labels).sum().double()
            sums[2] += (student_logits.argmax(-1) == teacher_logits.argmax(-1)).sum().double()
            sums[3] += (student_logits.argmax(-1, keepdim=True) == local_labels).sum().double()
            sums[4] += (teacher_logits.argmax(-1, keepdim=True) == local_labels).sum().double()
            sums[5] += teacher_p.max(-1).values.sum().double()
            sums[6] += -(teacher_p * teacher_logp).sum().double()
    denominator = max(rows, 1)
    return {
        "student_ce": (sums[0] / denominator).float(),
        "teacher_ce": (sums[1] / denominator).float(),
        "top1_agreement": (sums[2] / denominator).float(),
        "student_hard_top1": (sums[3] / denominator).float(),
        "teacher_hard_top1": (sums[4] / denominator).float(),
        "teacher_top1_probability": (sums[5] / denominator).float(),
        "teacher_entropy": (sums[6] / denominator).float(),
    }


class ForwardKLLoss(nn.Module):
    def __init__(self, mask_token_id: int) -> None:
        super().__init__()
        from liger_kernel.transformers import LigerFusedLinearJSD

        self.mask_token_id = mask_token_id
        self.loss = LigerFusedLinearJSD(jsd_beta=0.0, temperature=1.0)

    def forward(
        self,
        student_hidden: torch.Tensor,
        student_weight: torch.Tensor,
        teacher_hidden: torch.Tensor,
        teacher_weight: torch.Tensor,
        labels: torch.Tensor,
        padded_rows: int | None = None,
    ) -> torch.Tensor:
        if padded_rows is not None:
            rows = student_hidden.shape[0]
            if rows > padded_rows or teacher_hidden.shape[0] != rows or labels.shape[0] != rows:
                raise ValueError("invalid selected-row padding target")
            padding = padded_rows - rows
            if padding:
                student_hidden = torch.cat(
                    (student_hidden, student_hidden.new_zeros(padding, student_hidden.shape[1]))
                )
                teacher_hidden = torch.cat(
                    (teacher_hidden, teacher_hidden.new_zeros(padding, teacher_hidden.shape[1]))
                )
                labels = torch.cat((labels, labels.new_full((padding,), -100)))
        return self.loss(
            student_hidden,
            remove_mask_row(student_weight, self.mask_token_id),
            teacher_hidden,
            remove_mask_row(teacher_weight, self.mask_token_id),
            remap_labels(labels, self.mask_token_id),
        )


def vocabulary_slices(vocabulary: int, mask_token_id: int, chunk_size: int):
    for segment_start, segment_stop in ((0, mask_token_id), (mask_token_id + 1, vocabulary)):
        for start in range(segment_start, segment_stop, chunk_size):
            yield start, min(start + chunk_size, segment_stop)


class ChunkedExactKLFunction(torch.autograd.Function):
    """Exact forward KL with O(selected_rows * chunk_size) projection storage."""

    @staticmethod
    @torch.amp.custom_fwd(device_type="cuda")
    def forward(
        ctx,
        student_hidden: torch.Tensor,
        student_weight: torch.Tensor,
        teacher_hidden: torch.Tensor,
        teacher_weight: torch.Tensor,
        mask_token_id: int,
        chunk_size: int,
    ) -> torch.Tensor:
        if student_hidden.shape[0] != teacher_hidden.shape[0]:
            raise ValueError("teacher/student selected row counts differ")
        if student_weight.shape[0] != teacher_weight.shape[0]:
            raise ValueError("teacher/student vocabularies differ")
        rows = student_hidden.shape[0]
        slices = tuple(vocabulary_slices(student_weight.shape[0], mask_token_id, chunk_size))
        teacher_max = torch.full((rows,), -torch.inf, device=student_hidden.device)
        student_max = torch.full_like(teacher_max, -torch.inf)
        teacher_sum = torch.zeros_like(teacher_max)
        student_sum = torch.zeros_like(teacher_max)
        teacher_difference_sum = torch.zeros_like(teacher_max)
        with torch.no_grad():
            for start, stop in slices:
                teacher_logits = F.linear(teacher_hidden, teacher_weight[start:stop]).float()
                student_logits = F.linear(student_hidden, student_weight[start:stop]).float()
                next_teacher_max = torch.maximum(teacher_max, teacher_logits.max(-1).values)
                teacher_rescale = (teacher_max - next_teacher_max).exp()
                teacher_exponential = (teacher_logits - next_teacher_max[:, None]).exp()
                teacher_difference_sum = teacher_difference_sum * teacher_rescale + (
                    teacher_exponential * (teacher_logits - student_logits)
                ).sum(-1)
                teacher_sum = teacher_sum * teacher_rescale + teacher_exponential.sum(-1)
                teacher_max = next_teacher_max

                next_student_max = torch.maximum(student_max, student_logits.max(-1).values)
                student_sum = student_sum * (student_max - next_student_max).exp() + (
                    student_logits - next_student_max[:, None]
                ).exp().sum(-1)
                student_max = next_student_max
            teacher_logz = teacher_max + teacher_sum.log()
            student_logz = student_max + student_sum.log()
            kl_rows = teacher_difference_sum / teacher_sum + student_logz - teacher_logz
        ctx.mask_token_id = mask_token_id
        ctx.chunk_size = chunk_size
        ctx.save_for_backward(
            student_hidden,
            student_weight,
            teacher_hidden,
            teacher_weight,
            student_logz,
            teacher_logz,
        )
        return kl_rows

    @staticmethod
    @torch.amp.custom_bwd(device_type="cuda")
    def backward(ctx, grad_output: torch.Tensor):
        student_hidden, student_weight, teacher_hidden, teacher_weight, student_logz, teacher_logz = (
            ctx.saved_tensors
        )
        rows = student_hidden.shape[0]
        hidden_gradient = torch.zeros_like(student_hidden)
        weight_gradient = torch.zeros_like(student_weight)
        if grad_output.shape != (rows,):
            raise RuntimeError("chunked KL backward requires one gradient per selected row")
        with torch.no_grad():
            for start, stop in vocabulary_slices(
                student_weight.shape[0], ctx.mask_token_id, ctx.chunk_size
            ):
                teacher_logits = F.linear(teacher_hidden, teacher_weight[start:stop]).float()
                student_logits = F.linear(student_hidden, student_weight[start:stop]).float()
                logits_gradient = (
                    (student_logits - student_logz[:, None]).exp()
                    - (teacher_logits - teacher_logz[:, None]).exp()
                ) * grad_output.float()[:, None]
                local_gradient = logits_gradient.to(student_hidden.dtype)
                hidden_gradient.add_(
                    local_gradient @ student_weight[start:stop].to(local_gradient.dtype)
                )
                if student_weight.dtype == torch.float32 and local_gradient.dtype in {
                    torch.float16,
                    torch.bfloat16,
                }:
                    local_weight_gradient = torch.mm(
                        local_gradient.mT,
                        student_hidden,
                        out_dtype=torch.float32,
                    )
                else:
                    local_weight_gradient = local_gradient.mT @ student_hidden
                weight_gradient[start:stop].copy_(local_weight_gradient)
        return hidden_gradient, weight_gradient, None, None, None, None


class ChunkedForwardKLLoss(nn.Module):
    def __init__(self, mask_token_id: int, chunk_size: int, reduction: str = "mean") -> None:
        super().__init__()
        if reduction not in {"mean", "none"}:
            raise ValueError("chunked KL reduction must be 'mean' or 'none'")
        self.mask_token_id = mask_token_id
        self.chunk_size = chunk_size
        self.reduction = reduction

    def forward(
        self,
        student_hidden: torch.Tensor,
        student_weight: torch.Tensor,
        teacher_hidden: torch.Tensor,
        teacher_weight: torch.Tensor,
        labels: torch.Tensor,
        padded_rows: int | None = None,
    ) -> torch.Tensor:
        if padded_rows is not None:
            raise ValueError("chunked exact KL does not use selected-row padding")
        remap_labels(labels, self.mask_token_id)
        rows = ChunkedExactKLFunction.apply(
            student_hidden,
            student_weight,
            teacher_hidden,
            teacher_weight,
            self.mask_token_id,
            self.chunk_size,
        )
        return rows.mean() if self.reduction == "mean" else rows


def tiny_config() -> Config:
    return replace(
        CONFIG,
        vocab_size=97,
        mask_token_id=96,
        hidden_size=32,
        layers=2,
        attention_heads=4,
        kv_heads=2,
        head_dim=8,
        block_size=8,
        max_position_embeddings=32,
        monarch_blocks=8,
        noise_width=32,
        noise_mlp_width=64,
        max_supervised_positions=16,
    )


def run_self_tests() -> dict[str, Any]:
    torch.manual_seed(7)
    tokenizer_audit = audit_fast_tokenizer()
    tokenizer = load_tokenizer()
    converted = conversation_tokens(
        {
            "messages": [
                {"role": "system", "content": "Be concise."},
                {"role": "user", "content": "What is 2+2?"},
                {"role": "assistant", "content": "The answer is four."},
            ]
        },
        tokenizer,
    )
    assert converted is not None
    chat_ids, assistant_eligible = converted
    assert len(chat_ids) == len(assistant_eligible)
    assert any(assistant_eligible) and not assistant_eligible[0] and not assistant_eligible[-1]
    assistant_ids = [token for token, eligible in zip(chat_ids, assistant_eligible) if eligible]
    assert "answer is four" in tokenizer.decode(assistant_ids)
    assert row_source_allowed("ai2-adapt-dev/oasst1_converted")
    assert row_source_allowed("ai2-adapt-dev/coconot_converted")
    assert not row_source_allowed("ai2-adapt-dev/tulu_v3.9_wildchat_100k")
    assert not row_source_allowed("HuggingFaceFW/fineweb-edu")
    assert row_source_allowed("Dolci Instruct OpenThoughts3+ Science", THROUGHPUT_CONFIG)
    assert row_source_allowed("Tulu 3 Persona MATH", THROUGHPUT_CONFIG)
    assert not row_source_allowed("FLAN", THROUGHPUT_CONFIG)
    assert row_source_allowed("Nemotron Post-training code", THINK_SFT_CONFIG)
    assert row_source_allowed("OpenThoughts 3", THINK_SFT_CONFIG)
    assert not row_source_allowed("", THINK_SFT_CONFIG)
    step_budget = calculate_training_budget(
        dataset_contexts=4_000_000,
        microbatch=104,
        world_size=8,
        gradient_accumulation=1,
        sequence_length=512,
        maximum_epochs=10.0,
        max_steps=100,
        num_train_epochs=None,
    )
    assert step_budget["planned_epochs"] == 0.0208
    assert step_budget["global_contexts_update"] == 832
    try:
        calculate_training_budget(
            dataset_contexts=4_000_000,
            microbatch=104,
            world_size=8,
            gradient_accumulation=1,
            sequence_length=512,
            maximum_epochs=10.0,
            max_steps=48_077,
            num_train_epochs=None,
        )
    except ValueError as exc:
        assert "10" in str(exc)
    else:
        raise AssertionError("step budget above ten epochs was accepted")
    synthetic_maskable = torch.tensor([assistant_eligible], dtype=torch.bool)
    synthetic_masked = (torch.zeros_like(synthetic_maskable, dtype=torch.float32) < 1.0) & synthetic_maskable
    assert torch.equal(synthetic_masked, synthetic_maskable)
    assert not bool(synthetic_masked[0, 0]) and not bool(synthetic_masked[0, -1])
    production = MonarchStudent(MonarchConfig())
    parameter_count = sum(p.numel() for p in production.parameters())
    optimizer, inventory = build_optimizer(production)
    assert parameter_count == CONFIG.expected_parameters
    assert (inventory["dense_muon"]["tensors"], inventory["dense_muon"]["parameters"]) == (
        57,
        7_012_352,
    )
    assert (inventory["monarch_muon"]["tensors"], inventory["monarch_muon"]["parameters"]) == (
        66,
        51_904_512,
    )
    assert (inventory["adamw"]["tensors"], inventory["adamw"]["parameters"]) == (
        37,
        38_915_456,
    )
    assert (inventory["total"]["tensors"], inventory["total"]["parameters"]) == (
        160,
        CONFIG.expected_parameters,
    )
    del optimizer, production

    deep = MonarchStudent(MonarchConfig(DEEP_CONFIG))
    deep_parameter_count = sum(p.numel() for p in deep.parameters())
    deep_optimizer, deep_inventory = build_optimizer(deep, DEEP_CONFIG)
    assert deep_parameter_count == DEEP_CONFIG.expected_parameters == 95_572_992
    assert (deep_inventory["dense_muon"]["tensors"], deep_inventory["dense_muon"]["parameters"]) == (
        222,
        6_881_280,
    )
    assert (deep_inventory["monarch_muon"]["tensors"], deep_inventory["monarch_muon"]["parameters"]) == (
        264,
        69_206_016,
    )
    assert (deep_inventory["adamw"]["tensors"], deep_inventory["adamw"]["parameters"]) == (
        136,
        19_485_696,
    )
    assert deep_inventory["total"]["parameters"] == DEEP_CONFIG.expected_parameters
    assert GLOBAL_KL_CONFIG.expected_parameters == DEEP_CONFIG.expected_parameters
    assert GLOBAL_KL_CONFIG.kl_backend == "chunked-global"
    scheduler = torch.optim.lr_scheduler.LambdaLR(deep_optimizer, lr_lambda=lambda _: 1.0)
    override = replace(GLOBAL_KL_CONFIG, muon_lr=0.025, adamw_lr=3.75e-4)
    applied = apply_learning_rates(deep_optimizer, scheduler, override)
    deep_optimizer.step()
    scheduler.step()
    assert applied == {"dense_muon": 0.025, "monarch_muon": 0.025, "adamw": 3.75e-4}
    assert [group["lr"] for group in deep_optimizer.param_groups] == [0.025, 0.025, 3.75e-4]
    del deep_optimizer, deep

    cfg = tiny_config()
    model = MonarchStudent(MonarchConfig(cfg))
    with torch.no_grad():
        for layer in model.layers:
            d = cfg.hidden_size
            layer.ada.bias[2 * d : 3 * d].fill_(1)
            layer.ada.bias[5 * d : 6 * d].fill_(1)
    clean = torch.randint(0, cfg.vocab_size - 1, (2, 16))
    noisy = clean.clone()
    noisy[:, 1::3] = cfg.mask_token_id
    rates = (noisy == cfg.mask_token_id).reshape(2, 2, 8).float().mean(-1)
    selected = torch.nonzero((noisy == cfg.mask_token_id).reshape(-1), as_tuple=False).flatten()
    hidden = model.forward_train(noisy, clean, rates, selected)
    assert hidden.shape == (selected.numel(), cfg.hidden_size) and torch.isfinite(hidden).all()
    hidden.square().mean().backward()
    assert all(p.grad is None or torch.isfinite(p.grad).all() for p in model.parameters())

    rows, hs, ht, vocab, mask = 7, 5, 9, 23, 18
    sh = torch.randn(rows, hs, requires_grad=True)
    sw = torch.randn(vocab, hs, requires_grad=True)
    th, tw = torch.randn(rows, ht), torch.randn(vocab, ht)
    labels = torch.arange(1, rows + 1)
    diagnostics = materialized_diagnostic_metrics(sh, sw, th, tw, labels, mask, chunk_size=3)
    sw_trim, tw_trim = remove_mask_row(sw, mask), remove_mask_row(tw, mask)
    sl, tl = F.linear(sh, sw_trim).float(), F.linear(th, tw_trim).float()
    reference_rows = (
        tl.log_softmax(-1).exp() * (tl.log_softmax(-1) - sl.log_softmax(-1))
    ).sum(-1)
    oracle_rows = materialized_kl_reference(sh, sw, th, tw, mask)
    torch.testing.assert_close(oracle_rows, reference_rows)
    reference = reference_rows.mean()
    torch.testing.assert_close(
        diagnostics["student_ce"],
        -sl.log_softmax(-1).gather(1, remap_labels(labels, mask)[:, None]).mean(),
    )
    reference.backward()
    reference_hidden_gradient = sh.grad.detach().clone()
    reference_weight_gradient = sw.grad.detach().clone()
    sh.grad = None
    sw.grad = None
    chunked_rows = ChunkedForwardKLLoss(mask, chunk_size=7, reduction="none")(
        sh, sw, th, tw, labels
    )
    torch.testing.assert_close(chunked_rows.detach(), reference_rows.detach(), atol=2e-6, rtol=2e-6)
    chunked_rows.mean().backward()
    torch.testing.assert_close(sh.grad, reference_hidden_gradient, atol=2e-6, rtol=2e-6)
    torch.testing.assert_close(sw.grad, reference_weight_gradient, atol=2e-6, rtol=2e-6)

    # DDP averages rank gradients. Scaling each local sum by world/global rows
    # must therefore equal one global selected-token mean even for uneven ranks.
    rank_sizes = (2, rows - 2)
    partitioned_sh = sh.detach().clone().requires_grad_(True)
    partitioned_sw = sw.detach().clone().requires_grad_(True)
    start = 0
    local_losses = []
    for size in rank_sizes:
        stop = start + size
        local_rows = ChunkedForwardKLLoss(mask, chunk_size=7, reduction="none")(
            partitioned_sh[start:stop],
            partitioned_sw,
            th[start:stop],
            tw,
            labels[start:stop],
        )
        local_losses.append(len(rank_sizes) * local_rows.sum() / rows)
        start = stop
    # Simulate DDP's gradient average across the two rank-local objectives.
    (sum(local_losses) / len(rank_sizes)).backward()
    torch.testing.assert_close(partitioned_sh.grad, reference_hidden_gradient, atol=2e-6, rtol=2e-6)
    torch.testing.assert_close(partitioned_sw.grad, reference_weight_gradient, atol=2e-6, rtol=2e-6)

    # Dream trains and evaluates with logits shifted right. Selected hidden
    # projection must exactly equal selecting from the fully shifted logits,
    # without materializing full-vocabulary logits in production.
    dream_hidden = torch.randn(2, 16, 9)
    dream_weight = torch.randn(23, 9)
    dream_selected = torch.tensor([1, 7, 8, 15, 17, 24, 31])
    raw_logits = F.linear(dream_hidden, dream_weight)
    shifted_logits = torch.cat((raw_logits[:, :1], raw_logits[:, :-1]), dim=1)
    selected_logits = F.linear(
        right_shifted_selected_hidden(dream_hidden, dream_selected),
        dream_weight,
    )
    torch.testing.assert_close(
        selected_logits,
        shifted_logits.reshape(-1, shifted_logits.shape[-1]).index_select(0, dream_selected),
    )
    try:
        right_shifted_selected_hidden(dream_hidden, torch.tensor([0]))
    except ValueError:
        pass
    else:
        raise AssertionError("Dream position zero must not be supervised")

    # Raw Dream query 31 predicts logical token 32, so at the block boundary
    # it attends noisy block 1 and clean prior block 0, not noisy block 0.
    shifted_mask = create_right_shifted_dream_bd3_mask(64, 32, "cpu")[0, 0]
    assert shifted_mask[30, 0] and shifted_mask[30, 31]
    assert not shifted_mask[30, 32] and not shifted_mask[30, 64]
    assert shifted_mask[31, 32] and shifted_mask[31, 63]
    assert shifted_mask[31, 64] and not shifted_mask[31, 31]
    assert not shifted_mask[31, 96]

    capped = torch.ones(2, 32, dtype=torch.bool)
    first = select_masked_positions(
        capped,
        8,
        generator=torch.Generator().manual_seed(194),
    )
    second = select_masked_positions(
        capped,
        8,
        generator=torch.Generator().manual_seed(194),
    )
    torch.testing.assert_close(first, second)
    assert first.numel() == 16

    # The upstream BD3 sampler left-pads prompts to a block boundary, caches
    # that prefix, then repeatedly supplies one active block. Cached and direct
    # recomputation must agree, logical RoPE positions must survive the cache,
    # and padding must not leak through the dense within-block Monarch mixer.
    generation_config = MonarchConfig(cfg, pad_token_id=cfg.vocab_size - 2)
    generation_model = MonarchStudent(generation_config)
    with torch.no_grad():
        for layer in generation_model.layers:
            d = cfg.hidden_size
            layer.ada.bias[2 * d : 3 * d].fill_(1)
            layer.ada.bias[5 * d : 6 * d].fill_(1)
    pad = generation_config.pad_token_id
    prefix = torch.tensor([[pad, pad, pad, pad, 7, 8, 9, 10]])
    prefix_positions = torch.tensor([[0, 0, 0, 0, 0, 1, 2, 3]])
    prefix_valid = prefix != pad
    prefix_attention = (
        prefix_valid[:, None, :, None] & prefix_valid[:, None, None, :]
    )
    prefix_output = generation_model(
        input_ids=prefix,
        attention_mask=prefix_attention,
        position_ids=prefix_positions,
        use_cache=True,
    )
    assert prefix_output.past_key_values["schema"] == "v3-token-cache-v1"
    active = torch.full((1, cfg.block_size), cfg.mask_token_id)
    active_positions = torch.arange(4, 4 + cfg.block_size)[None]
    full_ids = torch.cat((prefix, active), dim=1)
    full_positions = torch.cat((prefix_positions, active_positions), dim=1)
    full_valid = full_ids != pad
    physical_blocks = torch.arange(full_ids.shape[1]) // cfg.block_size
    full_attention = (
        (physical_blocks[None, :, None] >= physical_blocks[None, None, :])[:, None]
        & full_valid[:, None, :, None]
        & full_valid[:, None, None, :]
    )
    cached_logits = generation_model(
        input_ids=active,
        attention_mask=full_attention[:, :, cfg.block_size :, :],
        position_ids=active_positions,
        past_key_values=copy.deepcopy(prefix_output.past_key_values),
    ).logits
    direct_logits = generation_model(
        input_ids=full_ids,
        attention_mask=full_attention,
        position_ids=full_positions,
    ).logits[:, -cfg.block_size :]
    torch.testing.assert_close(cached_logits, direct_logits)

    selected_active = torch.arange(cfg.block_size, 2 * cfg.block_size)
    generation_rates = (full_ids == cfg.mask_token_id).reshape(1, 2, cfg.block_size).float().mean(-1)
    hidden_before = generation_model.forward_train(
        full_ids,
        full_ids,
        generation_rates,
        selected_active,
        position_ids=full_positions,
        token_valid=full_valid,
    )
    with torch.no_grad():
        generation_model.embed_tokens.weight[pad].fill_(123)
    hidden_after = generation_model.forward_train(
        full_ids,
        full_ids,
        generation_rates,
        selected_active,
        position_ids=full_positions,
        token_valid=full_valid,
    )
    torch.testing.assert_close(hidden_before, hidden_after)

    # Exercise the actual pinned dLLM BD3 sampler API, including left prompt
    # padding, token-cache deepcopy, iterative unmasking, and mask suppression.
    from dllm.core.samplers import BD3LMSampler, BD3LMSamplerConfig

    sampler_tokenizer = SimpleNamespace(
        mask_token_id=cfg.mask_token_id,
        bos_token_id=1,
        pad_token_id=pad,
        eos_token_id=None,
    )
    tiny_sampler = BD3LMSampler(model=generation_model, tokenizer=sampler_tokenizer)
    tiny_sample = tiny_sampler.sample(
        [[7, 8, 9]],
        BD3LMSamplerConfig(
            max_new_tokens=cfg.block_size,
            block_size=cfg.block_size,
            steps=cfg.block_size,
            temperature=0.0,
            right_shift_logits=False,
            return_dict=True,
        ),
        return_dict=True,
    )
    assert tiny_sample.sequences.shape == (1, 2 * cfg.block_size)
    assert not bool((tiny_sample.sequences[:, -cfg.block_size :] == cfg.mask_token_id).any())

    batched = nn.Parameter(torch.randn(2, 16, 16))
    references = [nn.Parameter(x.clone()) for x in batched.detach()]
    ours = BatchedMonarchMuon([batched], lr=0.02)
    native = torch.optim.Muon(references, lr=0.02, momentum=0.95, nesterov=True, ns_steps=5, adjust_lr_fn="original", weight_decay=0.0)
    for _ in range(2):
        gradient = torch.randn_like(batched)
        batched.grad = gradient.clone()
        for parameter, local in zip(references, gradient, strict=True):
            parameter.grad = local.clone()
        ours.step()
        native.step()
    torch.testing.assert_close(batched, torch.stack(references), atol=2e-3, rtol=2e-2)

    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    step_model = MonarchStudent(MonarchConfig(cfg)).to(device)
    if device.type == "cuda":
        step_model.to(torch.bfloat16)
    step_optimizer, _ = build_optimizer(step_model, cfg)
    ids = torch.randint(0, cfg.vocab_size - 1, (2, 16), device=device)
    corrupted = ids.clone()
    corrupted[:, ::2] = cfg.mask_token_id
    rate = (corrupted == cfg.mask_token_id).reshape(2, 2, 8).float().mean(-1)
    indices = torch.nonzero((corrupted == cfg.mask_token_id).reshape(-1), as_tuple=False).flatten()
    output = step_model.forward_train(corrupted, ids, rate, indices)
    output.float().square().mean().backward()
    step_optimizer.step()
    assert all(torch.isfinite(p).all() for p in step_model.parameters())

    if device.type == "cuda":
        sh = torch.randn(11, 16, device=device, dtype=torch.bfloat16, requires_grad=True)
        sw = torch.randn(31, 16, device=device, dtype=torch.bfloat16, requires_grad=True)
        th = torch.randn(11, 24, device=device, dtype=torch.bfloat16)
        tw = torch.randn(31, 24, device=device, dtype=torch.bfloat16)
        labels = torch.arange(11, device=device) % 30
        fused = ForwardKLLoss(30)(sh, sw, th, tw, labels)
        reference_rows = materialized_kl_reference(sh, sw, th, tw, 30)
        torch.testing.assert_close(fused.detach(), reference_rows.mean(), atol=5e-3, rtol=5e-2)
        fused.backward()
        assert sh.grad is not None and sw.grad is not None

        padded_sh = sh.detach().clone().requires_grad_(True)
        padded_sw = sw.detach().clone().requires_grad_(True)
        padded = ForwardKLLoss(30)(
            padded_sh,
            padded_sw,
            th,
            tw,
            labels,
            padded_rows=17,
        )
        torch.testing.assert_close(padded.detach(), fused.detach(), atol=5e-3, rtol=5e-2)
        padded.backward()
        torch.testing.assert_close(padded_sh.grad, sh.grad, atol=5e-3, rtol=5e-2)
        torch.testing.assert_close(padded_sw.grad, sw.grad, atol=5e-3, rtol=5e-2)

        # Production keeps trainable student weights in FP32 while autocast
        # supplies BF16 hidden states. Exercise that exact mixed-dtype path: the
        # CPU reference above already proves gradient equality, while this check
        # catches unsupported H100 matmul/autocast combinations and non-finite
        # gradients before a paid launch.
        mixed_sh = torch.randn(13, 16, device=device, dtype=torch.bfloat16, requires_grad=True)
        mixed_sw = torch.randn(31, 16, device=device, dtype=torch.float32, requires_grad=True)
        mixed_th = torch.randn(13, 24, device=device, dtype=torch.bfloat16)
        mixed_tw = torch.randn(31, 24, device=device, dtype=torch.bfloat16)
        mixed_labels = torch.arange(13, device=device) % 30
        with torch.autocast("cuda", dtype=torch.bfloat16):
            mixed_chunked_rows = ChunkedForwardKLLoss(30, chunk_size=7, reduction="none")(
                mixed_sh,
                mixed_sw,
                mixed_th,
                mixed_tw,
                mixed_labels,
            )
            mixed_reference = materialized_kl_reference(
                mixed_sh,
                mixed_sw,
                mixed_th,
                mixed_tw,
                30,
            )
        torch.testing.assert_close(
            mixed_chunked_rows.detach(), mixed_reference.detach(), atol=5e-3, rtol=5e-2
        )
        mixed_chunked_rows.mean().backward()
        assert mixed_sh.grad is not None and torch.isfinite(mixed_sh.grad).all()
        assert mixed_sw.grad is not None and torch.isfinite(mixed_sw.grad).all()

    return {
        "status": "pass",
        "parameter_count": parameter_count,
        "deep_parameter_count": deep_parameter_count,
        "deep_optimizer": {
            key: {k: v for k, v in value.items() if k != "names"}
            for key, value in deep_inventory.items()
        },
        "optimizer": {key: {k: v for k, v in value.items() if k != "names"} for key, value in inventory.items()},
        "torch": torch.__version__,
        "cuda": torch.cuda.is_available(),
        "tokenizer": tokenizer_audit,
    }


def select_masked_positions(
    masked: torch.Tensor,
    cap: int,
    generator: torch.Generator | None = None,
) -> torch.Tensor:
    batch, length = masked.shape
    selected: list[torch.Tensor] = []
    for row in range(batch):
        local = torch.nonzero(masked[row], as_tuple=False).flatten()
        if local.numel() > cap:
            local = local[
                torch.randperm(local.numel(), device=local.device, generator=generator)[:cap]
            ]
        selected.append(local + row * length)
    nonempty = [value for value in selected if value.numel()]
    return torch.cat(nonempty) if nonempty else torch.empty(0, device=masked.device, dtype=torch.long)


class DreamTeacher:
    def __init__(self, device: torch.device) -> None:
        from dllm.pipelines.dream.models.modeling_dream import DreamModel
        from transformers import PreTrainedModel

        # dLLM's Dream wrapper assumes the base loader returns only a model and
        # crashes on the (model, loading_info) tuple. Generation config is not
        # used by the frozen teacher, so invoke the maintained base loader with
        # dLLM's exact Dream class and retain strict load auditing.
        self.model, loading_info = PreTrainedModel.from_pretrained.__func__(
            DreamModel,
            CONFIG.teacher_model,
            revision=CONFIG.teacher_revision,
            dtype=torch.bfloat16,
            attn_implementation="sdpa",
            low_cpu_mem_usage=True,
            output_loading_info=True,
        )
        incompatible = {
            key: loading_info.get(key, [])
            for key in ("missing_keys", "unexpected_keys", "mismatched_keys", "error_msgs")
            if loading_info.get(key)
        }
        if incompatible:
            raise RuntimeError(f"Dream checkpoint did not load exactly: {incompatible}")
        self.model = self.model.to(device).eval()
        self.model.requires_grad_(False)
        if self.model.config.vocab_size != CONFIG.vocab_size:
            raise RuntimeError(f"teacher vocabulary changed: {self.model.config.vocab_size}")
        if getattr(self.model.lm_head, "bias", None) is not None:
            raise RuntimeError("selected-hidden Dream projection assumes a bias-free lm_head")

    @property
    def weight(self) -> torch.Tensor:
        return self.model.lm_head.weight

    def selected_hidden(
        self,
        noisy_ids: torch.Tensor,
        clean_ids: torch.Tensor,
        attention_mask: torch.Tensor,
        selected_indices: torch.Tensor,
    ) -> torch.Tensor:
        batch, length = noisy_ids.shape
        packed = torch.cat((noisy_ids, clean_ids), dim=1)
        positions = torch.arange(length, device=packed.device).expand(batch, -1)
        positions = torch.cat((positions, positions), dim=1)
        with torch.inference_mode():
            output = self.model.model(
                input_ids=packed,
                attention_mask=attention_mask,
                position_ids=positions,
                use_cache=False,
                return_dict=True,
            )
            hidden = output.last_hidden_state[:, :length]
            selected = right_shifted_selected_hidden(hidden, selected_indices)
        return selected.clone()


class GpuMonitor:
    def __init__(self, local_rank: int) -> None:
        self.local_rank = local_rank
        self.samples: list[float] = []
        self.stop_event = threading.Event()
        self.thread: threading.Thread | None = None

    def start(self) -> None:
        try:
            import pynvml

            pynvml.nvmlInit()
            handle = pynvml.nvmlDeviceGetHandleByIndex(self.local_rank)

            def sample() -> None:
                while not self.stop_event.wait(0.25):
                    self.samples.append(float(pynvml.nvmlDeviceGetUtilizationRates(handle).gpu))

            self.thread = threading.Thread(target=sample, daemon=True)
            self.thread.start()
        except Exception:
            self.thread = None

    def stop(self) -> dict[str, float]:
        self.stop_event.set()
        if self.thread is not None:
            self.thread.join(timeout=2)
        return {
            "mean_gpu_utilization_percent": statistics.fmean(self.samples) if self.samples else 0.0,
            "median_gpu_utilization_percent": statistics.median(self.samples) if self.samples else 0.0,
            "gpu_samples": len(self.samples),
        }


def make_trainer_class():
    from dllm.core.trainers.bd3lm import BD3LMTrainer
    from transformers import TrainerCallback

    class CommitCallback(TrainerCallback):
        def __init__(self, owner) -> None:
            self.owner = owner

        def on_train_begin(self, args, state, control, **kwargs):
            # Trainer restores these cadence fields from trainer_state.json.
            # Honor the explicitly requested cadence for a new resume leg so
            # a short proof's eval-every-step setting does not dominate a
            # longer continuation.
            state.logging_steps = int(args.logging_steps)
            state.eval_steps = int(args.eval_steps) if args.eval_steps else 0
            state.save_steps = int(args.save_steps) if args.save_steps else 0
            optimizer = getattr(self.owner, "optimizer", None)
            base = getattr(optimizer, "optimizer", optimizer)
            if isinstance(base, CompositeOptimizer):
                self.owner.total_supervised_tokens = base.supervised_tokens
                self.owner.total_contexts = base.contexts
                self.owner.effective_learning_rates = apply_learning_rates(
                    optimizer,
                    self.owner.lr_scheduler,
                    self.owner.architecture,
                )

        def on_step_end(self, args, state, control, **kwargs):
            self.owner.commit_optimizer_step()
            target = self.owner.additional_supervised_tokens
            if target is not None and self.owner.session_supervised_tokens >= target:
                control.should_evaluate = args.eval_strategy != "no"
                control.should_save = True
                control.should_training_stop = True
            return control

    class DistillationTrainer(BD3LMTrainer):
        def __init__(
            self,
            *args,
            teacher: DreamTeacher,
            architecture: Config,
            profile_update: bool = False,
            additional_supervised_tokens: int | None = None,
            **kwargs,
        ) -> None:
            self.teacher = teacher
            self.architecture = architecture
            if architecture.kl_backend == "liger":
                self.forward_kl = ForwardKLLoss(architecture.mask_token_id)
            elif architecture.kl_backend == "chunked-recomputed":
                self.forward_kl = ChunkedForwardKLLoss(
                    architecture.mask_token_id,
                    architecture.vocab_chunk_size,
                )
            elif architecture.kl_backend == "chunked-global":
                self.forward_kl = ChunkedForwardKLLoss(
                    architecture.mask_token_id,
                    architecture.vocab_chunk_size,
                    reduction="none",
                )
            else:
                raise ValueError(f"unknown KL backend: {architecture.kl_backend}")
            if additional_supervised_tokens is not None and additional_supervised_tokens <= 0:
                raise ValueError("additional supervised-token target must be positive")
            self.additional_supervised_tokens = additional_supervised_tokens
            self.effective_learning_rates: dict[str, float] = {}
            self.pending_supervised_tokens = 0
            self.pending_contexts = 0
            self.total_supervised_tokens = 0
            self.session_supervised_tokens = 0
            self.total_contexts = 0
            self.last_global_supervised_tokens = 0
            self.last_update_seconds = 0.0
            self.last_commit_at = time.monotonic()
            self.latest_metrics: dict[str, float] = {}
            self.latest_metrics_step = -1
            self.eval_history: list[dict[str, float]] = []
            output_dir = Path(kwargs["args"].output_dir)
            prior_rows: dict[float, dict[str, float]] = {}
            for history_name in ("eval_history_first_leg.json", "eval_history.json"):
                history_path = output_dir / history_name
                if history_path.exists():
                    for row in json.loads(history_path.read_text())["rows"]:
                        prior_rows[float(row["step"])] = row
            self.eval_history = [prior_rows[step] for step in sorted(prior_rows)]
            self.started_at = time.monotonic()
            self._eval_generator: torch.Generator | None = None
            self._eval_metric_sums: torch.Tensor | None = None
            self._eval_metric_count = 0.0
            self.profile_update = profile_update
            self.profile_complete = False
            self.profile_totals: defaultdict[str, float] = defaultdict(float)
            self.profile_metrics: dict[str, float] = {}
            self._profile_backward_started: float | None = None
            self._profile_student_backward_started: float | None = None
            self._profile_optimizer_started: float | None = None
            self.grad_norm_observations = 0
            self.grad_clip_observations = 0
            super().__init__(*args, **kwargs)
            # The inherited dLLM NLL/PPL meter is updated only by dLLM's own
            # CE compute_loss. Our explicit KL/CE metrics replace it; retaining
            # the callback would emit misleading NaNs.
            self.remove_callback(type(self.meter))
            self.add_callback(CommitCallback(self))

        def create_optimizer(self):
            if self.optimizer is None:
                unwrapped = self.accelerator.unwrap_model(self.model)
                self.optimizer, self.optimizer_inventory = build_optimizer(unwrapped, self.architecture)
                if self.profile_update:
                    self.optimizer.profile_step_callback = self._profile_optimizer_event
                if self.optimizer_inventory["total"]["parameters"] != self.architecture.expected_parameters:
                    raise RuntimeError("production optimizer routing count changed")
            return self.optimizer

        def _profile_active(self) -> bool:
            return self.profile_update and not self.profile_complete and self.model.training

        def _profile_call(self, name: str, function):
            if not self._profile_active():
                return function()
            torch.cuda.synchronize()
            started = time.perf_counter()
            value = function()
            torch.cuda.synchronize()
            self.profile_totals[name] += (time.perf_counter() - started) * 1_000
            return value

        def _profile_optimizer_event(self, event: str) -> None:
            if not self._profile_active():
                return
            torch.cuda.synchronize()
            if event == "start":
                self._profile_optimizer_started = time.perf_counter()
            elif event == "end" and self._profile_optimizer_started is not None:
                self.profile_totals["optimizer"] += (
                    time.perf_counter() - self._profile_optimizer_started
                ) * 1_000
                self._profile_optimizer_started = None

        def training_step(self, model, inputs, num_items_in_batch=None):
            active = self._profile_active()
            if active:
                torch.cuda.synchronize()
                started = time.perf_counter()
            output = super().training_step(model, inputs, num_items_in_batch)
            if active:
                torch.cuda.synchronize()
                self.profile_totals["microbatch_total"] += (
                    time.perf_counter() - started
                ) * 1_000
                if self._profile_backward_started is None:
                    raise RuntimeError("profiled loss backward hook did not run")
                if self._profile_student_backward_started is None:
                    raise RuntimeError("profiled selected-hidden backward hook did not run")
                self.profile_totals["kl_backward"] += (
                    self._profile_student_backward_started - self._profile_backward_started
                ) * 1_000
                self.profile_totals["student_backward_ddp"] += (
                    time.perf_counter() - self._profile_student_backward_started
                ) * 1_000
                self.profile_totals["backward_ddp"] += (
                    time.perf_counter() - self._profile_backward_started
                ) * 1_000
                self._profile_backward_started = None
                self._profile_student_backward_started = None
            return output

        def get_train_dataloader(self):
            # Accelerate's dispatcher consumes the iterable only on rank 0.
            # PyTorch still constructs an iterator on every rank, so workers
            # would eagerly populate 64 unused 10k-context shuffle buffers.
            original = (
                self.args.dataloader_num_workers,
                self.args.dataloader_persistent_workers,
                self.args.dataloader_prefetch_factor,
            )
            self.args.dataloader_num_workers = CONFIG.train_dataloader_workers
            self.args.dataloader_persistent_workers = CONFIG.train_dataloader_workers > 0
            self.args.dataloader_prefetch_factor = (
                CONFIG.dataloader_prefetch_factor if CONFIG.train_dataloader_workers > 0 else None
            )
            try:
                return super().get_train_dataloader()
            finally:
                (
                    self.args.dataloader_num_workers,
                    self.args.dataloader_persistent_workers,
                    self.args.dataloader_prefetch_factor,
                ) = original

        def create_scheduler(self, num_training_steps: int, optimizer=None):
            if self.lr_scheduler is None:
                self.lr_scheduler = torch.optim.lr_scheduler.LambdaLR(
                    optimizer or self.optimizer, lr_lambda=lambda _: 1.0
                )
            return self.lr_scheduler

        def commit_optimizer_step(self) -> None:
            committed_at = time.monotonic()
            self.last_update_seconds = committed_at - self.last_commit_at
            self.last_commit_at = committed_at
            self.last_global_supervised_tokens = self.pending_supervised_tokens
            self.total_supervised_tokens += self.pending_supervised_tokens
            self.session_supervised_tokens += self.pending_supervised_tokens
            self.total_contexts += self.pending_contexts
            self.pending_supervised_tokens = 0
            self.pending_contexts = 0
            base = getattr(self.optimizer, "optimizer", self.optimizer)
            if isinstance(base, CompositeOptimizer):
                base.supervised_tokens = self.total_supervised_tokens
                base.contexts = self.total_contexts
            if self._profile_active():
                component_names = (
                    "teacher_forward",
                    "student_forward",
                    "kl_forward",
                    "kl_backward",
                    "student_backward_ddp",
                    "backward_ddp",
                    "optimizer",
                    "microbatch_total",
                )
                self.profile_totals["unattributed"] = max(
                    0.0,
                    self.profile_totals["microbatch_total"]
                    - sum(
                        self.profile_totals[name]
                        for name in (
                            "teacher_forward",
                            "student_forward",
                            "kl_forward",
                            "backward_ddp",
                        )
                    ),
                )
                self.profile_totals["profiled_update"] = (
                    self.profile_totals["microbatch_total"]
                    + self.profile_totals["optimizer"]
                )
                names = (*component_names, "unattributed", "profiled_update")
                local = torch.tensor(
                    [self.profile_totals[name] for name in names],
                    device=self.accelerator.device,
                    dtype=torch.float64,
                )
                maximum = local.clone()
                mean = local.clone()
                if torch.distributed.is_initialized():
                    torch.distributed.all_reduce(maximum, op=torch.distributed.ReduceOp.MAX)
                    torch.distributed.all_reduce(mean, op=torch.distributed.ReduceOp.SUM)
                    mean /= self.accelerator.num_processes
                self.profile_metrics = {
                    **{
                        f"profile/{name}_ms_max": float(maximum[index].item())
                        for index, name in enumerate(names)
                    },
                    **{
                        f"profile/{name}_ms_mean": float(mean[index].item())
                        for index, name in enumerate(names)
                    },
                }
                self.profile_complete = True
                if self.is_world_process_zero():
                    write_json(
                        Path(self.args.output_dir) / "update_profile.json",
                        {"schema": "v3-update-profile-v1", **self.profile_metrics},
                    )

        def _attention_mask(self, length: int, device: torch.device) -> torch.Tensor:
            return create_right_shifted_dream_bd3_mask(
                length,
                self.architecture.block_size,
                device,
            )

        def compute_loss(self, model, inputs, return_outputs=False, **kwargs):
            inputs = self._preprocess_inputs(inputs)
            clean_ids, labels = inputs["input_ids"], inputs["labels"]
            eligible = inputs.get("eligible_mask")
            if eligible is None or eligible.shape != labels.shape:
                raise RuntimeError("post-training batches require a token eligibility mask")
            batch, length = clean_ids.shape
            maskable = (labels != -100) & eligible.bool()
            # Dream's output convention predicts logical token i from raw
            # hidden state i-1. As in Dream's own eval path, position zero is
            # never corrupted or scored because it has no preceding state.
            maskable[:, 0] = False
            generator = self._eval_generator if not model.training else None
            t = self.time_epsilon + (1 - self.time_epsilon) * torch.rand(
                batch, device=clean_ids.device, generator=generator
            )
            p_mask = 1.0 - self.scheduler(t).unsqueeze(1)
            masked = (torch.rand(clean_ids.shape, device=clean_ids.device, generator=generator) < p_mask) & maskable
            noisy_ids = torch.where(masked, self.architecture.mask_token_id, clean_ids)
            selected_indices = select_masked_positions(
                masked,
                self.architecture.max_supervised_positions,
                generator=generator,
            )
            if selected_indices.numel() == 0:
                raise RuntimeError("BD3 corruption selected no supervised positions")
            selected_labels = clean_ids.reshape(-1).index_select(0, selected_indices)
            block_noise = masked.reshape(batch, length // self.architecture.block_size, -1).float().mean(-1)
            attention_mask = self._attention_mask(length, clean_ids.device)
            teacher_hidden = self._profile_call(
                "teacher_forward",
                lambda: self.teacher.selected_hidden(
                    noisy_ids,
                    clean_ids,
                    attention_mask,
                    selected_indices,
                ),
            )
            student_output = self._profile_call(
                "student_forward",
                lambda: model(
                    noisy_ids=noisy_ids,
                    clean_ids=clean_ids,
                    block_noise=block_noise,
                    selected_indices=selected_indices,
                ),
            )
            unwrapped = self.accelerator.unwrap_model(model)
            student_hidden = student_output.selected_hidden
            local_count = torch.tensor(selected_indices.numel(), device=clean_ids.device, dtype=torch.long)
            global_count_tensor = self.accelerator.reduce(local_count, reduction="sum")
            global_count = int(global_count_tensor.item())
            kl_output = self._profile_call(
                "kl_forward",
                lambda: self.forward_kl(
                    student_hidden,
                    unwrapped.embed_tokens.weight,
                    teacher_hidden,
                    self.teacher.weight,
                    selected_labels,
                ),
            )
            if self.architecture.kl_backend == "chunked-global":
                if kl_output.shape != (selected_indices.numel(),):
                    raise RuntimeError("canonical KL must return one value per selected token")
                # DDP averages rank gradients. Multiplying the local sum by R/N
                # makes that average exactly the global selected-token mean.
                loss = (
                    kl_output.sum()
                    * self.accelerator.num_processes
                    / global_count_tensor.to(kl_output.dtype)
                )
                canonical_kl_rows: torch.Tensor | None = kl_output
            else:
                if kl_output.ndim != 0:
                    raise RuntimeError("legacy KL backend must return a scalar")
                loss = kl_output
                canonical_kl_rows = None
            if self._profile_active():
                def mark_backward_start(gradient):
                    torch.cuda.synchronize()
                    self._profile_backward_started = time.perf_counter()
                    return gradient

                def mark_student_backward_start(gradient):
                    torch.cuda.synchronize()
                    self._profile_student_backward_started = time.perf_counter()
                    return gradient

                loss.register_hook(mark_backward_start)
                student_hidden.register_hook(mark_student_backward_start)
            if model.training:
                self.pending_supervised_tokens += global_count
                self.pending_contexts += batch * self.accelerator.num_processes

            # Canonical KL comes directly from the optimization kernel. CE and
            # top-1 diagnostics require a separate projection over 151,935
            # classes, so measure those only at the configured cadence.
            should_measure = (
                not model.training
                or (
                    not self._profile_active()
                    and self.architecture.train_diagnostic_steps > 0
                    and self.state.global_step % self.architecture.train_diagnostic_steps == 0
                )
            )
            if should_measure:
                metrics = materialized_diagnostic_metrics(
                    student_hidden,
                    unwrapped.embed_tokens.weight,
                    teacher_hidden,
                    self.teacher.weight,
                    selected_labels,
                    self.architecture.mask_token_id,
                )
                if canonical_kl_rows is not None:
                    metrics["full_kl"] = canonical_kl_rows.detach().mean().float()
                else:
                    metrics["full_kl"] = materialized_kl_reference(
                        student_hidden.detach(),
                        unwrapped.embed_tokens.weight.detach(),
                        teacher_hidden.detach(),
                        self.teacher.weight.detach(),
                        self.architecture.mask_token_id,
                    ).mean().float()
                selected_rows = selected_indices.div(length, rounding_mode="floor").long()
                selected_positions = selected_indices.remainder(length)
                selected_blocks = selected_positions.div(
                    self.architecture.block_size,
                    rounding_mode="floor",
                ).long()
                metrics["selected_t"] = t.index_select(0, selected_rows).mean()
                metrics["selected_block_mask_fraction"] = block_noise[
                    selected_rows,
                    selected_blocks,
                ].mean()
                values = torch.stack([metrics[key] for key in METRIC_NAMES])
                weighted = torch.cat((values.double() * local_count.double(), local_count.double()[None]))
                reduced = self.accelerator.reduce(weighted, reduction="sum")
                denominator = reduced[-1].clamp_min(1)
                self.latest_metrics = {
                    name: float((reduced[index] / denominator).item())
                    for index, name in enumerate(METRIC_NAMES)
                }
                self.latest_metrics["global_supervised_tokens_microbatch"] = global_count
                if model.training:
                    self.latest_metrics_step = self.state.global_step
                if not model.training and self._eval_metric_sums is not None:
                    self._eval_metric_sums += reduced[: len(METRIC_NAMES)].detach().cpu()
                    self._eval_metric_count += float(reduced[-1].item())
            outputs = SimpleNamespace(logits=torch.empty(0, device=clean_ids.device), selected_hidden=student_hidden)
            return (loss, outputs) if return_outputs else loss

        def log(self, logs: dict[str, float], *args, **kwargs) -> None:
            if "loss" in logs:
                if self.architecture.kl_backend == "chunked-global":
                    logs["train/full_kl"] = float(logs["loss"])
                if "grad_norm" in logs:
                    preclip = float(logs["grad_norm"])
                    self.grad_norm_observations += 1
                    if preclip > float(self.args.max_grad_norm):
                        self.grad_clip_observations += 1
                    logs["train/grad_norm_preclip"] = preclip
                    logs["train/grad_clip_active"] = float(
                        preclip > float(self.args.max_grad_norm)
                    )
                    logs["train/grad_clip_fraction"] = (
                        self.grad_clip_observations / self.grad_norm_observations
                    )
                if self.latest_metrics_step == self.state.global_step - 1:
                    logs.update({f"train/{key}": value for key, value in self.latest_metrics.items()})
                logs["train/global_supervised_tokens_update"] = self.last_global_supervised_tokens
                logs["train/update_seconds"] = self.last_update_seconds
                logs["train/update_supervised_tokens_per_second"] = (
                    self.last_global_supervised_tokens / max(self.last_update_seconds, 1e-9)
                )
                logs["train/total_supervised_tokens"] = self.total_supervised_tokens
                elapsed = max(time.monotonic() - self.started_at, 1e-9)
                logs["train/supervised_tokens_per_second"] = self.session_supervised_tokens / elapsed
                base = getattr(self.optimizer, "optimizer", self.optimizer)
                if isinstance(base, CompositeOptimizer):
                    logs["train/lr_dense_muon"] = base.dense.param_groups[0]["lr"]
                    logs["train/lr_monarch_muon"] = base.monarch.param_groups[0]["lr"]
                    logs["train/lr_adamw"] = base.auxiliary.param_groups[0]["lr"]
                    logs.update(base.latest_gradient_metrics)
                logs.update(self.profile_metrics)
            super().log(logs, *args, **kwargs)

        def evaluate(self, *args, **kwargs):
            device = self.accelerator.device
            self._eval_generator = torch.Generator(device=device).manual_seed(self.architecture.eval_seed)
            self._eval_metric_sums = torch.zeros(len(METRIC_NAMES), dtype=torch.float64)
            self._eval_metric_count = 0.0
            output = super().evaluate(*args, **kwargs)
            # Evaluation is deliberately excluded from the next optimizer
            # update's wall-clock measurement.
            self.last_commit_at = time.monotonic()
            self._eval_generator = None
            aggregate = {
                name: float((self._eval_metric_sums[index] / max(self._eval_metric_count, 1.0)).item())
                for index, name in enumerate(METRIC_NAMES)
            }
            aggregation_error: float | None = None
            if self.architecture.kl_backend == "chunked-global":
                native = float(output["eval_loss"])
                aggregation_error = abs(native - aggregate["full_kl"])
                output["eval_kl_aggregation_error"] = aggregation_error
                if aggregation_error > 5e-4:
                    raise RuntimeError(
                        "canonical KL aggregation mismatch: "
                        f"eval_loss={native}, full_kl={aggregate['full_kl']}"
                    )
            self._eval_metric_sums = None
            row = {
                "step": float(self.state.global_step),
                "supervised_tokens": float(self.total_supervised_tokens),
                **aggregate,
            }
            if aggregation_error is not None:
                row["kl_aggregation_error"] = aggregation_error
            self.eval_history.append(row)
            output.update(
                {
                    f"eval_{key}": value
                    for key, value in row.items()
                    if key not in {"step", "supervised_tokens"}
                }
            )
            self.log({f"eval/{key}": value for key, value in row.items()})
            if self.is_world_process_zero():
                write_json(Path(self.args.output_dir) / "eval_history.json", {"rows": self.eval_history})
            return output

    return DistillationTrainer


def load_tokenizer():
    from transformers import PreTrainedTokenizerFast
    from transformers.utils.hub import cached_file

    # Dream's pinned repository contains the exact Rust tokenizer graph, but
    # its remote AutoTokenizer declaration exposes only the slow Python class.
    # Load the maintained fast backend directly and retain the pinned special
    # token declarations verbatim.
    tokenizer_path = cached_file(
        CONFIG.teacher_model,
        "tokenizer.json",
        revision=CONFIG.teacher_revision,
    )
    tokenizer_config_path = cached_file(
        CONFIG.teacher_model,
        "tokenizer_config.json",
        revision=CONFIG.teacher_revision,
    )
    chat_template_path = cached_file(
        CONFIG.teacher_model,
        "chat_template.jinja",
        revision=CONFIG.teacher_revision,
    )
    tokenizer_config = json.loads(Path(tokenizer_config_path).read_text())
    chat_template = Path(chat_template_path).read_text()
    tokenizer = PreTrainedTokenizerFast(
        tokenizer_file=tokenizer_path,
        bos_token=tokenizer_config.get("bos_token"),
        eos_token=tokenizer_config.get("eos_token"),
        pad_token=tokenizer_config.get("pad_token"),
        mask_token=tokenizer_config.get("mask_token"),
        additional_special_tokens=tokenizer_config.get("additional_special_tokens", []),
        clean_up_tokenization_spaces=tokenizer_config.get("clean_up_tokenization_spaces", False),
        model_max_length=tokenizer_config.get("model_max_length", CONFIG.max_position_embeddings),
        padding_side="right",
        chat_template=chat_template,
    )
    # Dream's tokenizer exposes 151,670 assigned token IDs while its tied model
    # vocabulary intentionally retains the 151,936-row embedding table.
    expected_special_ids = {"pad": 151_643, "eos": 151_645, "mask": CONFIG.mask_token_id}
    observed_special_ids = {
        "pad": tokenizer.pad_token_id,
        "eos": tokenizer.eos_token_id,
        "mask": tokenizer.mask_token_id,
    }
    if (
        not tokenizer.is_fast
        or len(tokenizer) != 151_670
        or len(tokenizer) > CONFIG.vocab_size
        or observed_special_ids != expected_special_ids
        or hashlib.sha256(tokenizer.chat_template.encode()).hexdigest()
        != "639b8dbd0ae92bdf5267b69d54a7a79026cf395cebac7a0af04c9b463d8d5a65"
    ):
        raise RuntimeError(
            f"Dream tokenizer changed: fast={tokenizer.is_fast}, vocab={len(tokenizer)}, "
            f"special_ids={observed_special_ids}"
        )
    return tokenizer


def audit_fast_tokenizer() -> dict[str, Any]:
    from transformers import AutoTokenizer

    slow = AutoTokenizer.from_pretrained(
        CONFIG.teacher_model,
        revision=CONFIG.teacher_revision,
        trust_remote_code=True,
        padding_side="right",
    )
    fast = load_tokenizer()
    if slow.get_vocab() != fast.get_vocab():
        raise AssertionError("fast and reference Dream vocabularies differ")
    cases = (
        "Hello world",
        " Hello world",
        "caf\u00e9 cafe\u0301",
        "\u4e2d\u6587\u6ca1\u6709\u7a7a\u683c\u3002",
        "emoji \U0001f9e0\U0001f680 and\r\nnewlines\n",
        "<|MASK|><|im_start|>x<|im_end|>",
        "don't we'll I'm",
        "  trailing   \n\n",
    )
    for text in cases:
        reference = slow(text, add_special_tokens=True).input_ids
        candidate = fast(text, add_special_tokens=True).input_ids
        if reference != candidate:
            raise AssertionError(f"fast Dream token IDs differ for {text!r}")
    return {
        "cases": len(cases),
        "vocabulary_entries": len(fast),
        "is_fast": fast.is_fast,
        "chat_template_sha256": hashlib.sha256(fast.chat_template.encode()).hexdigest(),
    }


def row_source_allowed(source: str, config: Config = CONFIG) -> bool:
    if config.allow_all_row_sources:
        return bool(source.strip())
    lowered = source.lower()
    return any(fragment.lower() in lowered for fragment in config.allowed_row_sources)


def calculate_training_budget(
    *,
    dataset_contexts: int,
    microbatch: int,
    world_size: int,
    gradient_accumulation: int,
    sequence_length: int,
    maximum_epochs: float,
    max_steps: int | None,
    num_train_epochs: float | None,
) -> dict[str, int | float | None]:
    """Resolve a step/epoch request and hard-reject more than the epoch cap."""
    if dataset_contexts <= 0:
        raise ValueError("training dataset is empty")
    if min(microbatch, world_size, gradient_accumulation, sequence_length) <= 0:
        raise ValueError("batch, world size, accumulation, and sequence length must be positive")
    if (max_steps is None) == (num_train_epochs is None):
        raise ValueError("choose exactly one of --max-steps or --num-train-epochs")
    global_contexts = microbatch * world_size * gradient_accumulation
    if max_steps is not None:
        if max_steps <= 0:
            raise ValueError("--max-steps must be positive")
        planned_steps = max_steps
        planned_epochs = planned_steps * global_contexts / dataset_contexts
        requested_epochs = None
    else:
        assert num_train_epochs is not None
        if num_train_epochs <= 0:
            raise ValueError("--num-train-epochs must be positive")
        planned_epochs = float(num_train_epochs)
        requested_epochs = float(num_train_epochs)
        planned_steps = math.ceil(planned_epochs * dataset_contexts / global_contexts)
    if planned_epochs > maximum_epochs + 1e-12:
        raise ValueError(
            f"training budget is {planned_epochs:.6f} epochs; hard maximum is {maximum_epochs:g}"
        )
    return {
        "dataset_contexts": dataset_contexts,
        "global_contexts_update": global_contexts,
        "requested_max_steps": max_steps,
        "requested_epochs": requested_epochs,
        "planned_optimizer_steps": planned_steps,
        "planned_epochs": planned_epochs,
        "maximum_epochs": maximum_epochs,
        "processed_tokens_update": global_contexts * sequence_length,
        "planned_processed_tokens": planned_steps * global_contexts * sequence_length,
    }


def checkpoint_global_step(checkpoint: str | Path | None) -> int:
    if checkpoint is None:
        return 0
    state_path = Path(checkpoint) / "trainer_state.json"
    if not state_path.is_file():
        raise FileNotFoundError(f"resume checkpoint has no trainer state: {state_path}")
    step = int(json.loads(state_path.read_text())["global_step"])
    if step < 0:
        raise ValueError("checkpoint global step cannot be negative")
    return step


def stable_conversation_id(row: dict[str, Any]) -> str:
    identifier = row.get("id")
    if identifier is not None and str(identifier):
        return str(identifier)
    canonical = json.dumps(row.get("messages"), ensure_ascii=False, sort_keys=True)
    return hashlib.sha256(canonical.encode()).hexdigest()


def conversation_is_heldout(identifier: str, config: Config = CONFIG) -> bool:
    bucket = int.from_bytes(hashlib.sha256(identifier.encode()).digest()[:8], "little")
    return bucket % config.heldout_hash_modulus == config.heldout_hash_remainder


def conversation_tokens(row: dict[str, Any], tokenizer) -> tuple[list[int], list[bool]] | None:
    messages = row.get("messages")
    if not isinstance(messages, list):
        return None
    assistant = next(
        (
            index
            for index in range(len(messages) - 1, -1, -1)
            if isinstance(messages[index], dict)
            and messages[index].get("role") == "assistant"
            and isinstance(messages[index].get("content"), str)
        ),
        None,
    )
    if assistant is None:
        return None
    selected = [dict(message) for message in messages[: assistant + 1]]
    all_ids = tokenizer.apply_chat_template(
        selected, tokenize=True, add_generation_prompt=False
    )
    empty = copy.deepcopy(selected)
    empty[-1]["content"] = ""
    empty_ids = tokenizer.apply_chat_template(
        empty, tokenize=True, add_generation_prompt=False
    )
    all_ids, empty_ids = map(list, (all_ids, empty_ids))
    response_start = 0
    while (
        response_start < min(len(all_ids), len(empty_ids))
        and all_ids[response_start] == empty_ids[response_start]
    ):
        response_start += 1
    suffix = 0
    maximum_suffix = min(len(all_ids) - response_start, len(empty_ids) - response_start)
    while suffix < maximum_suffix and all_ids[-1 - suffix] == empty_ids[-1 - suffix]:
        suffix += 1
    response_end = len(all_ids) - suffix
    if response_end <= response_start:
        return None
    eligibility = [False] * len(all_ids)
    eligibility[response_start:response_end] = [True] * (response_end - response_start)
    return all_ids, eligibility


def iter_posttraining_contexts(
    files: list[str],
    split: str,
    architecture_name: str,
    maximum_contexts: int | None = None,
):
    import pyarrow.parquet as parquet

    if split not in {"train", "heldout"}:
        raise ValueError(f"unknown post-training split: {split}")
    config = ARCHITECTURES[architecture_name]
    tokenizer = load_tokenizer()
    eos = tokenizer.eos_token_id
    if eos is None:
        raise RuntimeError("Dream tokenizer has no EOS token")
    token_buffer: list[int] = []
    eligibility_buffer: list[bool] = []
    yielded = 0
    for filename in files:
        reader = parquet.ParquetFile(filename)
        for batch in reader.iter_batches(
            batch_size=256,
            columns=["id", "messages", config.dataset_source_field],
        ):
            for row in batch.to_pylist():
                if not row_source_allowed(str(row.get(config.dataset_source_field, "")), config):
                    continue
                heldout = conversation_is_heldout(stable_conversation_id(row), config)
                if heldout != (split == "heldout"):
                    continue
                converted = conversation_tokens(row, tokenizer)
                if converted is None:
                    continue
                tokens, eligibility = converted
                token_buffer.extend((*tokens, eos))
                eligibility_buffer.extend((*eligibility, False))
                while len(token_buffer) >= config.sequence_length:
                    ids = token_buffer[: config.sequence_length]
                    eligible = eligibility_buffer[: config.sequence_length]
                    del token_buffer[: config.sequence_length]
                    del eligibility_buffer[: config.sequence_length]
                    # A very long user prompt can occupy one or more complete
                    # packed contexts. Keep only contexts with real assistant
                    # targets so every BD3 microbatch has maskable positions.
                    if not any(eligible):
                        continue
                    yield {"input_ids": ids, "eligible_mask": eligible}
                    yielded += 1
                    if maximum_contexts is not None and yielded >= maximum_contexts:
                        return


def posttraining_source_audit(files: list[Path], config: Config) -> dict[str, Any]:
    import pyarrow.parquet as parquet

    source_counts: defaultdict[str, int] = defaultdict(int)
    train_ids: set[str] = set()
    heldout_ids: set[str] = set()
    file_rows: list[dict[str, Any]] = []
    for filename in files:
        digest = hashlib.sha256()
        with filename.open("rb") as source_file:
            for chunk in iter(lambda: source_file.read(8 << 20), b""):
                digest.update(chunk)
        metadata_path = filename.with_suffix(filename.suffix + ".download.json")
        if metadata_path.is_file():
            metadata = json.loads(metadata_path.read_text())
            if (
                filename.stat().st_size != metadata["bytes"]
                or digest.hexdigest() != metadata["sha256"]
                or config.dataset_revision not in metadata["url"]
            ):
                raise RuntimeError(f"staged dataset shard failed provenance audit: {filename}")
            source_url = metadata["url"]
        else:
            metadata = {
                "bytes": filename.stat().st_size,
                "sha256": digest.hexdigest(),
            }
            source_url = (
                f"https://huggingface.co/datasets/{config.dataset}/resolve/"
                f"{config.dataset_revision}/data/{filename.name}"
            )
        file_rows.append(
            {
                "name": filename.name,
                "bytes": metadata["bytes"],
                "sha256": metadata["sha256"],
                "url": source_url,
            }
        )
        reader = parquet.ParquetFile(filename)
        for batch in reader.iter_batches(
            batch_size=4096,
            # The split audit needs only stable IDs and source labels. Avoid
            # decoding the multi-gigabyte nested message column a second time.
            columns=["id", config.dataset_source_field],
        ):
            for row in batch.to_pylist():
                source = str(row.get(config.dataset_source_field, ""))
                if not row_source_allowed(source, config):
                    continue
                source_counts[source] += 1
                identifier = str(row.get("id") or "")
                if not identifier:
                    raise RuntimeError("pinned post-training row has no stable ID")
                (heldout_ids if conversation_is_heldout(identifier, config) else train_ids).add(identifier)
    if not train_ids or len(heldout_ids) < config.eval_contexts or train_ids & heldout_ids:
        raise RuntimeError("deterministic train/heldout ID split failed")
    return {
        "source_files": file_rows,
        "allowed_source_counts": dict(sorted(source_counts.items())),
        "train_conversation_ids": len(train_ids),
        "heldout_bucket_conversation_ids": len(heldout_ids),
        "train_conversation_ids_sha256": hashlib.sha256(
            "\n".join(sorted(train_ids)).encode()
        ).hexdigest(),
        "heldout_conversation_ids_sha256": hashlib.sha256(
            "\n".join(sorted(heldout_ids)).encode()
        ).hexdigest(),
    }


def materialized_dataset_audit(dataset, config: Config = CONFIG) -> dict[str, Any]:
    import numpy as np

    token_digest = hashlib.sha256()
    eligibility_digest = hashlib.sha256()
    eligible_positions = 0
    # Arrow's default Python formatter constructs billions of Python integer
    # objects for a large cache. NumPy formatting keeps the same exact bytes
    # and is over 50x faster on the measured 512-token dataset.
    formatted = dataset.with_format(
        "numpy", columns=["input_ids", "eligible_mask"]
    )
    for batch in formatted.iter(batch_size=1024):
        ids = np.asarray(batch["input_ids"], dtype="<i4")
        eligible = np.asarray(batch["eligible_mask"], dtype=np.bool_)
        if ids.ndim != 2 or ids.shape[1] != config.sequence_length or eligible.shape != ids.shape:
            raise RuntimeError("materialized post-training context has invalid shape")
        token_digest.update(ids.tobytes())
        eligibility_digest.update(np.packbits(eligible, axis=None).tobytes())
        eligible_positions += int(eligible.sum())
    return {
        "contexts": len(dataset),
        "raw_tokens": len(dataset) * config.sequence_length,
        "assistant_eligible_tokens": eligible_positions,
        "assistant_eligible_fraction": eligible_positions / max(len(dataset) * config.sequence_length, 1),
        "token_ids_sha256": token_digest.hexdigest(),
        "eligibility_sha256": eligibility_digest.hexdigest(),
    }


def posttraining_collator(features: list[dict[str, Any]]) -> dict[str, torch.Tensor]:
    input_ids = torch.tensor([feature["input_ids"] for feature in features], dtype=torch.long)
    eligible = torch.tensor([feature["eligible_mask"] for feature in features], dtype=torch.bool)
    return {"input_ids": input_ids, "labels": input_ids.clone(), "eligible_mask": eligible}


def load_datasets(tokenizer, config: Config):
    import accelerate
    from datasets import load_from_disk

    state = accelerate.PartialState()
    with state.local_main_process_first():
        manifest_path = config.preprocessed_dataset_path / "manifest.json"
        if not manifest_path.is_file():
            raise RuntimeError("paid training requires the prepared post-training-only cache")
        cached = load_from_disk(str(config.preprocessed_dataset_path))
        if len(cached["train"]) < config.preprocess_minimum_contexts:
            raise RuntimeError("preprocessed post-training cache is too small")
        training_manifest = json.loads(manifest_path.read_text())
        if (
            training_manifest.get("dataset") != config.dataset
            or training_manifest.get("dataset_revision") != config.dataset_revision
        ):
            raise RuntimeError("prepared training cache provenance changed")
        heldout_manifest_path = config.heldout_split_manifest_path
        if not heldout_manifest_path.is_file():
            raise RuntimeError("paid training requires the prepared fixed held-out cache")
        heldout = load_from_disk(str(config.preprocessed_eval_dataset_path))
        training = cached["train"].shuffle(seed=config.dataset_seed)
        evaluation = heldout["train"]
        if len(evaluation) != config.eval_contexts:
            raise RuntimeError("fixed held-out split has invalid shape")
        observed = materialized_dataset_audit(evaluation, config)
        heldout_manifest = json.loads(heldout_manifest_path.read_text())
        if (
            heldout_manifest.get("dataset") != config.dataset
            or heldout_manifest.get("dataset_revision") != config.dataset_revision
        ):
            raise RuntimeError("prepared held-out cache provenance changed")
        if observed["token_ids_sha256"] != heldout_manifest.get("token_ids_sha256"):
            raise RuntimeError("fixed held-out token digest changed")
        if observed["eligibility_sha256"] != heldout_manifest.get("eligibility_sha256"):
            raise RuntimeError("fixed held-out eligibility digest changed")
    return training, evaluation


def resolve_source_files(config: Config) -> list[Path]:
    staged = sorted(config.staged_dataset_root.glob("*.parquet"))
    if staged:
        if len(staged) != config.expected_source_shards:
            raise RuntimeError(
                f"expected {config.expected_source_shards} staged shards, found {len(staged)}"
            )
        return staged
    from huggingface_hub import HfApi, snapshot_download

    names = sorted(
        name
        for name in HfApi().list_repo_files(
            config.dataset,
            repo_type="dataset",
            revision=config.dataset_revision,
        )
        if name.startswith("data/") and name.endswith(".parquet")
    )
    if len(names) != config.expected_source_shards:
        raise RuntimeError(
            f"pinned dataset exposes {len(names)} Parquet shards, "
            f"expected {config.expected_source_shards}"
        )
    snapshot = Path(
        snapshot_download(
            config.dataset,
            repo_type="dataset",
            revision=config.dataset_revision,
            allow_patterns=["data/*.parquet"],
            max_workers=min(16, len(names)),
        )
    )
    downloaded = sorted(snapshot.glob("data/*.parquet"))
    if [path.relative_to(snapshot).as_posix() for path in downloaded] != names:
        raise RuntimeError("downloaded dataset snapshot does not match pinned shard list")
    return downloaded


def prepare_data_command(args: argparse.Namespace) -> None:
    from datasets import Dataset, DatasetDict, Features, Sequence, Value, load_from_disk

    config = ARCHITECTURES[args.architecture]
    files = resolve_source_files(config)
    source_audit = posttraining_source_audit(files, config)
    template_digest = hashlib.sha256(load_tokenizer().chat_template.encode()).hexdigest()
    features = Features(
        {
            "input_ids": Sequence(Value("int32"), length=config.sequence_length),
            "eligible_mask": Sequence(Value("bool"), length=config.sequence_length),
        }
    )
    manifests: dict[str, Any] = {}
    for split, destination, required in (
        ("train", config.preprocessed_dataset_path, config.preprocess_minimum_contexts),
        ("heldout", config.preprocessed_eval_dataset_path, config.eval_contexts),
    ):
        manifest_path = destination / "manifest.json"
        if not manifest_path.is_file():
            if destination.exists():
                raise RuntimeError(f"incomplete post-training cache requires audit: {destination}")
            worker_count = max(1, min(args.num_proc, len(files)))
            global_cap = (
                config.preprocess_maximum_contexts
                if split == "train"
                else config.eval_contexts
            )
            per_worker_cap = (
                (
                    # Held-out matches are sparse and uneven across source
                    # shards. Let every worker find up to the global target,
                    # then take the deterministic first 128 after shard-order
                    # concatenation. Equal quotas can underfill this split.
                    global_cap
                    if split == "heldout"
                    else math.ceil(global_cap / worker_count)
                )
                if global_cap is not None
                else None
            )
            dataset = Dataset.from_generator(
                iter_posttraining_contexts,
                gen_kwargs={
                    "files": [str(path) for path in files],
                    "split": split,
                    "architecture_name": args.architecture,
                    "maximum_contexts": per_worker_cap,
                },
                features=features,
                cache_dir=str(CONFIG.cache_root / "generator-cache"),
                num_proc=worker_count,
            )
            if global_cap is not None and len(dataset) > global_cap:
                dataset = dataset.select(range(global_cap))
            if len(dataset) < required or (split == "heldout" and len(dataset) != required):
                raise RuntimeError(
                    f"materialized {split} cache has {len(dataset)} contexts, need {required}"
                )
            audit = materialized_dataset_audit(dataset, config)
            DatasetDict({"train": dataset}).save_to_disk(str(destination))
            manifest = {
                "schema": "v3-dllm-monarch-posttraining-data-v1",
                "split": split,
                "dataset": config.dataset,
                "dataset_revision": config.dataset_revision,
                "allowed_row_sources": list(config.allowed_row_sources),
                "allow_all_row_sources": config.allow_all_row_sources,
                "split_rule": (
                    f"sha256(conversation_id) mod {config.heldout_hash_modulus} "
                    f"== {config.heldout_hash_remainder}"
                ),
                "sequence_length": config.sequence_length,
                "tokenizer_model": CONFIG.teacher_model,
                "tokenizer_revision": CONFIG.teacher_revision,
                "chat_template_sha256": template_digest,
                "supervision": "final assistant response content tokens only",
                "dllm_commit": DLLM_COMMIT,
                "path": str(destination),
                **source_audit,
                **audit,
            }
            write_json(manifest_path, manifest)
        cached = load_from_disk(str(destination))["train"]
        manifest = json.loads(manifest_path.read_text())
        observed = materialized_dataset_audit(cached, config)
        if any(observed[key] != manifest[key] for key in observed):
            raise RuntimeError(f"materialized {split} cache digest changed")
        manifests[split] = manifest
    print(json.dumps(manifests, indent=2, sort_keys=True))


def initialize_wandb(
    output: Path,
    run_name: str,
    run_id: str | None,
    architecture: Config,
) -> tuple[str, str]:
    import torch.distributed as dist
    import wandb
    from accelerate import PartialState

    state = PartialState()
    values = [run_id or "", ""]
    if state.is_main_process:
        if not os.environ.get("WANDB_API_KEY"):
            raise RuntimeError("WANDB_API_KEY is required")
        wandb.login(key=os.environ["WANDB_API_KEY"], relogin=True, verify=True)
        wandb_config = asdict(architecture)
        wandb_config = {
            key: str(value) if isinstance(value, Path) else value
            for key, value in wandb_config.items()
        }
        run = wandb.init(
            project=CONFIG.wandb_project,
            name=run_name,
            id=run_id,
            resume="must" if run_id else "never",
            config={**wandb_config, "dllm_commit": DLLM_COMMIT, "liger_version": LIGER_VERSION},
        )
        if not run.url:
            raise RuntimeError("W&B did not return a direct run URL")
        values = [run.id, run.url]
        write_json(output / "launch.json", {"status": "initialized", "wandb_id": run.id, "wandb_url": run.url})
    if dist.is_initialized():
        dist.broadcast_object_list(values, src=0)
    os.environ["WANDB_RUN_ID"] = values[0]
    return values[0], values[1]


def train_command(args: argparse.Namespace) -> None:
    import torch.distributed as dist
    import transformers
    from accelerate import PartialState
    from dllm.core.trainers.bd3lm import BD3LMConfig

    output = Path(args.output)
    output.mkdir(parents=True, exist_ok=True)
    architecture = ARCHITECTURES[args.architecture]
    muon_lr = architecture.muon_lr if args.muon_lr is None else args.muon_lr
    adamw_lr = architecture.adamw_lr if args.adamw_lr is None else args.adamw_lr
    if min(muon_lr, adamw_lr) <= 0:
        raise ValueError("all effective learning rates must be positive")
    architecture = replace(architecture, muon_lr=muon_lr, adamw_lr=adamw_lr)
    resume_step = checkpoint_global_step(args.resume)
    effective_max_steps = args.max_steps
    if args.additional_supervised_tokens is not None:
        conservative_steps = math.ceil(
            args.additional_supervised_tokens / architecture.minimum_global_supervised_tokens
        )
        effective_max_steps = resume_step + conservative_steps
    state = PartialState()
    if state.num_processes != 8:
        raise RuntimeError(f"paid V3 training requires eight processes, found {state.num_processes}")
    if architecture.eval_contexts % state.num_processes:
        raise RuntimeError("fixed holdout must divide evenly across all ranks")
    if not torch.cuda.is_available() or "H100" not in torch.cuda.get_device_name(state.local_process_index):
        raise RuntimeError("paid V3 training requires eight H100 GPUs")
    torch.cuda.set_device(state.local_process_index)
    torch.cuda.reset_peak_memory_stats()
    run_id, run_url = initialize_wandb(
        output,
        args.run_name,
        args.wandb_id,
        architecture,
    )
    tokenizer = load_tokenizer()
    training, evaluation = load_datasets(tokenizer, architecture)
    training_budget = calculate_training_budget(
        dataset_contexts=len(training),
        microbatch=args.microbatch,
        world_size=state.num_processes,
        gradient_accumulation=args.gradient_accumulation,
        sequence_length=architecture.sequence_length,
        maximum_epochs=architecture.maximum_epochs,
        max_steps=effective_max_steps,
        num_train_epochs=args.num_train_epochs,
    )
    training_budget.update(
        {
            "resume_global_step": resume_step,
            "requested_additional_supervised_tokens": args.additional_supervised_tokens,
            "effective_max_steps_cap": effective_max_steps,
        }
    )
    if state.is_main_process:
        import wandb

        wandb.config.update({"training_budget": training_budget}, allow_val_change=True)
    teacher = DreamTeacher(state.device)
    # Trainer seeds its own data path, but the model object is constructed
    # before Trainer.__init__. Seed here so every fresh V3 initialization is
    # reproducible and identical across DDP ranks.
    torch.manual_seed(CONFIG.dataset_seed)
    model = MonarchStudent(
        MonarchConfig(architecture, use_flex_attention=args.flex_attention)
    )
    if args.compile and args.compile_student_stack:
        raise ValueError("choose whole-model compile or fixed student-stack compile, not both")
    if args.compile_student_stack:
        model.enable_compiled_train_stack()
    if sum(p.numel() for p in model.parameters()) != architecture.expected_parameters:
        raise RuntimeError("student parameter count changed")

    training_args = BD3LMConfig(
        output_dir=str(output),
        overwrite_output_dir=False,
        run_name=args.run_name,
        report_to="wandb",
        seed=CONFIG.dataset_seed,
        data_seed=CONFIG.dataset_seed,
        per_device_train_batch_size=args.microbatch,
        # The fixed holdout is one exact global batch. Declaring 104 here made
        # Trainer repeat each rank scalar 104 times and truncate the gathered
        # 832 entries to 128, biasing eval_loss toward the first ranks even
        # though Accelerate dispatched 16 real contexts/rank.
        per_device_eval_batch_size=architecture.eval_contexts // state.num_processes,
        gradient_accumulation_steps=args.gradient_accumulation,
        max_steps=effective_max_steps if effective_max_steps is not None else -1,
        num_train_epochs=(
            args.num_train_epochs if args.num_train_epochs is not None else 1.0
        ),
        learning_rate=CONFIG.adamw_lr,
        lr_scheduler_type="constant",
        warmup_steps=0,
        max_grad_norm=1.0,
        bf16=True,
        torch_compile=args.compile,
        torch_compile_backend="inductor" if args.compile else None,
        torch_compile_mode="default" if args.compile else None,
        logging_steps=args.logging_steps,
        eval_strategy="steps" if args.eval_steps > 0 else "no",
        eval_steps=args.eval_steps if args.eval_steps > 0 else None,
        eval_on_start=args.eval_on_start,
        save_strategy="steps" if args.save_steps > 0 else "no",
        save_steps=args.save_steps if args.save_steps > 0 else 500,
        save_only_model=False,
        save_total_limit=3,
        remove_unused_columns=False,
        dataloader_num_workers=CONFIG.dataloader_workers,
        dataloader_pin_memory=True,
        dataloader_persistent_workers=CONFIG.dataloader_workers > 0,
        dataloader_prefetch_factor=(
            CONFIG.dataloader_prefetch_factor if CONFIG.dataloader_workers > 0 else None
        ),
        accelerator_config={
            "dispatch_batches": True,
            "split_batches": False,
            "non_blocking": True,
        },
        ddp_find_unused_parameters=False,
        block_size=architecture.block_size,
        loss_weight_type="uniform",
        loss_norm_type="token",
    )
    if bool(training_args.torch_compile) != bool(args.compile):
        raise RuntimeError(
            "Transformers changed the requested torch_compile state: "
            f"requested={args.compile}, actual={training_args.torch_compile}"
        )
    TrainerClass = make_trainer_class()
    trainer = TrainerClass(
        model=model,
        teacher=teacher,
        architecture=architecture,
        profile_update=args.profile_update,
        additional_supervised_tokens=args.additional_supervised_tokens,
        tokenizer=tokenizer,
        train_dataset=training,
        eval_dataset=evaluation,
        args=training_args,
        data_collator=posttraining_collator,
    )
    monitor = GpuMonitor(state.local_process_index)
    monitor.start()
    started = time.monotonic()
    status = "failed"
    error = ""
    try:
        trainer.train(resume_from_checkpoint=args.resume or None)
        if (
            args.additional_supervised_tokens is not None
            and trainer.session_supervised_tokens < args.additional_supervised_tokens
        ):
            raise RuntimeError(
                "training hit its conservative step cap before the supervised-token target: "
                f"observed={trainer.session_supervised_tokens}, "
                f"target={args.additional_supervised_tokens}"
            )
        status = "preflight_complete" if args.preflight else "train_leg_complete"
    except Exception as exc:
        error = f"{type(exc).__name__}: {exc}"
        raise
    finally:
        telemetry = monitor.stop()
        elapsed = time.monotonic() - started
        local = {
            "rank": state.process_index,
            **telemetry,
            "peak_allocated_gib": torch.cuda.max_memory_allocated() / 2**30,
            "peak_reserved_gib": torch.cuda.max_memory_reserved() / 2**30,
        }
        gathered: list[dict[str, Any] | None] = [None] * state.num_processes
        if dist.is_initialized():
            dist.all_gather_object(gathered, local)
        else:
            gathered = [local]
        if state.is_main_process:
            rows = [row for row in gathered if row is not None]
            history = trainer.eval_history
            result = {
                "schema": "v3-dllm-monarch-result-v1",
                "status": status,
                "error": error,
                "wandb_id": run_id,
                "wandb_url": run_url,
                "dllm_commit": DLLM_COMMIT,
                "teacher_model": CONFIG.teacher_model,
                "teacher_revision": CONFIG.teacher_revision,
                "architecture_id": architecture.architecture_id,
                "student_parameters": architecture.expected_parameters,
                "optimizer_routing": getattr(trainer, "optimizer_inventory", {}),
                "effective_learning_rates": trainer.effective_learning_rates,
                "max_grad_norm": trainer.args.max_grad_norm,
                "grad_norm_observations": trainer.grad_norm_observations,
                "grad_clip_observations": trainer.grad_clip_observations,
                "grad_clip_fraction": (
                    trainer.grad_clip_observations / max(trainer.grad_norm_observations, 1)
                ),
                "world_size": state.num_processes,
                "microbatch_per_gpu": args.microbatch,
                "gradient_accumulation": args.gradient_accumulation,
                "training_budget": training_budget,
                "torch_compile_requested": args.compile,
                "torch_compile": bool(trainer.args.torch_compile),
                "compile_student_stack": args.compile_student_stack,
                "flex_attention": args.flex_attention,
                "profile_update": args.profile_update,
                "update_profile": trainer.profile_metrics,
                "train_dataloader_workers": CONFIG.train_dataloader_workers,
                "eval_dataloader_workers": CONFIG.dataloader_workers,
                "dispatch_batches": True,
                "tokenizer_backend": type(tokenizer).__name__,
                "training_data": json.loads(
                    (architecture.preprocessed_dataset_path / "manifest.json").read_text()
                ),
                "heldout_data": json.loads(architecture.heldout_split_manifest_path.read_text()),
                "global_contexts_update": training_budget["global_contexts_update"],
                "last_global_supervised_tokens_update": trainer.last_global_supervised_tokens,
                "last_update_seconds": trainer.last_update_seconds,
                "last_update_supervised_tokens_per_second": (
                    trainer.last_global_supervised_tokens / max(trainer.last_update_seconds, 1e-9)
                ),
                "total_supervised_tokens": trainer.total_supervised_tokens,
                "session_supervised_tokens": trainer.session_supervised_tokens,
                "supervised_tokens_per_second": trainer.session_supervised_tokens / max(elapsed, 1e-9),
                "elapsed_seconds": elapsed,
                "gpu_rows": rows,
                "gpu_utilization_mean": statistics.fmean(row["mean_gpu_utilization_percent"] for row in rows),
                "gpu_utilization_min_rank_median": min(row["median_gpu_utilization_percent"] for row in rows),
                "peak_allocated_gib": max(row["peak_allocated_gib"] for row in rows),
                "peak_reserved_gib": max(row["peak_reserved_gib"] for row in rows),
                "eval_history": history,
                "checkpoint_paths": sorted(str(path) for path in output.glob("checkpoint-*")),
            }
            write_json(output / "result.json", result)
            write_json(output / "audit.json", result)


def sample_command(args: argparse.Namespace) -> None:
    import wandb
    from dllm.core.samplers import BD3LMSampler, BD3LMSamplerConfig

    if not torch.cuda.is_available() or "H100" not in torch.cuda.get_device_name(0):
        raise RuntimeError("checkpoint sampling requires an H100")
    checkpoint = Path(args.checkpoint).resolve()
    if not (checkpoint / "model.safetensors").is_file():
        raise FileNotFoundError(f"missing student checkpoint: {checkpoint}")
    if not os.environ.get("WANDB_API_KEY"):
        raise RuntimeError("WANDB_API_KEY is required for generation evidence")
    if args.max_new_tokens <= 0 or args.max_new_tokens % CONFIG.block_size:
        raise ValueError("max-new-tokens must be a positive whole number of blocks")
    if args.steps < args.max_new_tokens:
        raise ValueError("generation needs at least one diffusion step per output token")

    torch.manual_seed(args.seed)
    model = MonarchStudent.from_pretrained(
        checkpoint,
        dtype=torch.bfloat16,
        low_cpu_mem_usage=True,
    ).to("cuda:0").eval()
    tokenizer = load_tokenizer()
    prompts = args.prompt or [
        "The capital of France is",
        "Two plus two equals",
        "Once upon a time, there was",
        "A Python function that adds two numbers is",
    ]
    encoded = [tokenizer(prompt, add_special_tokens=True).input_ids for prompt in prompts]
    sampler = BD3LMSampler(model=model, tokenizer=tokenizer)
    config = BD3LMSamplerConfig(
        max_new_tokens=args.max_new_tokens,
        block_size=CONFIG.block_size,
        steps=args.steps,
        temperature=args.temperature,
        remasking="low_confidence",
        right_shift_logits=False,
        return_dict=True,
    )
    torch.cuda.reset_peak_memory_stats()
    started = time.monotonic()
    sampled = sampler.sample(encoded, config, return_dict=True)
    elapsed = time.monotonic() - started
    rows = []
    for prompt, prompt_ids, sequence in zip(prompts, encoded, sampled.sequences, strict=True):
        generated_ids = sequence[-args.max_new_tokens :].tolist()
        rows.append(
            {
                "prompt": prompt,
                "prompt_tokens": len(prompt_ids),
                "generated_ids": generated_ids,
                "generated_text": tokenizer.decode(generated_ids, skip_special_tokens=True),
                "remaining_mask_tokens": generated_ids.count(CONFIG.mask_token_id),
            }
        )
    remaining_masks = sum(row["remaining_mask_tokens"] for row in rows)
    if remaining_masks:
        raise RuntimeError(f"sampler left {remaining_masks} mask tokens in generated blocks")

    wandb.login(key=os.environ["WANDB_API_KEY"], relogin=True, verify=True)
    run = wandb.init(
        project=CONFIG.wandb_project,
        id=args.wandb_id,
        resume="must",
        name=args.run_name,
    )
    if not run.url:
        raise RuntimeError("W&B did not return a direct run URL for generation evidence")
    table = wandb.Table(columns=["prompt", "generated_text", "prompt_tokens"])
    for row in rows:
        table.add_data(row["prompt"], row["generated_text"], row["prompt_tokens"])
    run.log(
        {
            "generation/table": table,
            "generation/examples": len(rows),
            "generation/new_tokens": len(rows) * args.max_new_tokens,
            "generation/seconds": elapsed,
            "generation/tokens_per_second": len(rows) * args.max_new_tokens / max(elapsed, 1e-9),
            "generation/remaining_mask_tokens": remaining_masks,
        }
    )
    run_url = run.url
    run.finish()
    result = {
        "schema": "v3-dllm-monarch-generation-v1",
        "architecture_id": model.config.architecture_id,
        "inference_adapter": f"{model.config.architecture_id}-generation-v1.3",
        "checkpoint": str(checkpoint),
        "wandb_url": run_url,
        "seed": args.seed,
        "block_size": CONFIG.block_size,
        "steps": args.steps,
        "max_new_tokens": args.max_new_tokens,
        "temperature": args.temperature,
        "elapsed_seconds": elapsed,
        "tokens_per_second": len(rows) * args.max_new_tokens / max(elapsed, 1e-9),
        "peak_allocated_gib": torch.cuda.max_memory_allocated() / 2**30,
        "remaining_mask_tokens": remaining_masks,
        "rows": rows,
    }
    write_json(args.output, result)
    print(json.dumps(result, indent=2, sort_keys=True))


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description="KISS dLLM Monarch distillation")
    sub = result.add_subparsers(dest="command", required=True)
    sub.add_parser("self-test")
    prepare_data = sub.add_parser("prepare-data")
    prepare_data.add_argument("--num-proc", type=int, default=32)
    prepare_data.add_argument(
        "--architecture",
        choices=tuple(ARCHITECTURES),
        default="current",
    )
    train = sub.add_parser("train")
    train.add_argument("--output", required=True)
    train.add_argument("--run-name", required=True)
    train.add_argument(
        "--architecture",
        choices=tuple(ARCHITECTURES),
        default="current",
    )
    train.add_argument("--wandb-id")
    train.add_argument("--microbatch", type=int, required=True)
    train.add_argument("--gradient-accumulation", type=int, default=1)
    budget = train.add_mutually_exclusive_group(required=True)
    budget.add_argument("--max-steps", type=int)
    budget.add_argument("--num-train-epochs", type=float)
    budget.add_argument("--additional-supervised-tokens", type=int)
    train.add_argument("--muon-lr", type=float)
    train.add_argument("--adamw-lr", type=float)
    train.add_argument("--logging-steps", type=int, default=1)
    train.add_argument("--eval-steps", type=int, default=0)
    train.add_argument("--save-steps", type=int, default=0)
    train.add_argument("--eval-on-start", action="store_true")
    train.add_argument("--resume")
    train.add_argument("--preflight", action="store_true")
    train.add_argument("--compile", action="store_true")
    train.add_argument("--compile-student-stack", action="store_true")
    train.add_argument("--flex-attention", action="store_true")
    train.add_argument("--profile-update", action="store_true")
    sample = sub.add_parser("sample")
    sample.add_argument("--checkpoint", required=True)
    sample.add_argument("--output", required=True)
    sample.add_argument("--wandb-id", required=True)
    sample.add_argument("--run-name", default="quality-v1.2-b128")
    sample.add_argument("--prompt", action="append")
    sample.add_argument("--max-new-tokens", type=int, default=32)
    sample.add_argument("--steps", type=int, default=32)
    sample.add_argument("--temperature", type=float, default=0.0)
    sample.add_argument("--seed", type=int, default=194)
    return result


def main() -> None:
    args = parser().parse_args()
    if args.command == "self-test":
        print(json.dumps(run_self_tests(), indent=2, sort_keys=True))
    elif args.command == "prepare-data":
        prepare_data_command(args)
    elif args.command == "train":
        train_command(args)
    else:
        sample_command(args)


if __name__ == "__main__":
    main()
