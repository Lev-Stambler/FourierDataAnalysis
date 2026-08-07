from __future__ import annotations

import torch

from .model import SimplerBlockDiffusionForMaskedLM


def commit_high_confidence(
    token_ids: torch.Tensor,
    logits: torch.Tensor,
    *,
    mask_token_id: int,
    tokens_per_step: int = 4,
) -> tuple[torch.Tensor, torch.Tensor]:
    """Commit up to k masked positions per row, simultaneously.

    This is the only commit rule used by decoding and on-policy rollouts.  The
    returned mask identifies positions changed by this call.
    """
    if token_ids.ndim != 2 or logits.shape[:2] != token_ids.shape:
        raise ValueError("tokens/logits must have shapes [groups, width] and [groups, width, vocab]")
    if tokens_per_step <= 0:
        raise ValueError("tokens_per_step must be positive")
    eligible = token_ids.eq(mask_token_id)
    safe_logits = logits.float().clone()
    safe_logits[..., mask_token_id] = -torch.inf
    confidence, proposed = safe_logits.softmax(-1).max(-1)
    confidence.masked_fill_(~eligible, -torch.inf)
    count = min(tokens_per_step, token_ids.shape[1])
    positions = confidence.topk(count, dim=-1).indices
    chosen = eligible.gather(1, positions)
    committed = torch.zeros_like(eligible)
    committed.scatter_(1, positions, chosen)
    result = token_ids.clone()
    result[committed] = proposed[committed]
    if bool(result[committed].eq(mask_token_id).any()):
        raise RuntimeError("commit rule emitted the mask token")
    return result, committed


@torch.inference_mode()
def generate_blocks_batch(
    model: SimplerBlockDiffusionForMaskedLM,
    prompt_ids: torch.Tensor,
    *,
    max_new_tokens: int,
    eos_token_id: int | None = None,
    temperature: float = 0.0,
    top_p: float = 1.0,
    generator: torch.Generator | None = None,
) -> list[torch.Tensor]:
    """Greedy eight-step block decoding with four simultaneous commits/step."""
    if prompt_ids.ndim != 2 or prompt_ids.shape[0] == 0:
        raise ValueError("generate_blocks_batch requires a non-empty [batch, tokens] tensor")
    if max_new_tokens < 0 or temperature < 0 or not 0 < top_p <= 1:
        raise ValueError("invalid generation limit or top_p")
    if temperature != 0 or top_p != 1:
        raise ValueError("the shared production commit rule currently supports greedy decoding only")
    del generator
    config = model.config
    prompt_length = prompt_ids.shape[1]
    if prompt_length + max_new_tokens > config.max_position_embeddings:
        raise ValueError("prompt plus generation exceeds configured context")
    if max_new_tokens == 0:
        return [row.clone() for row in prompt_ids]

    device = prompt_ids.device
    dtype = model.embed_tokens.weight.dtype
    batch_size = prompt_ids.shape[0]
    block_size = config.block_size
    cache = model.empty_cache(batch_size, device=device, dtype=dtype)
    complete = (prompt_length // block_size) * block_size
    for start in range(0, complete, block_size):
        _, cache = model.forward_block(
            prompt_ids[:, start : start + block_size],
            torch.zeros(batch_size, device=device, dtype=dtype),
            cache,
            commit=True,
        )

    partial = prompt_ids[:, complete:].clone()
    completions: list[list[int]] = [[] for _ in range(batch_size)]
    done = torch.zeros(batch_size, dtype=torch.bool, device=device)
    while not bool(done.all()):
        block = torch.full(
            (batch_size, block_size), config.mask_token_id, device=device, dtype=torch.long
        )
        known = partial.shape[1]
        if known:
            block[:, :known] = partial
        for _ in range((block_size + 3) // 4):
            if not bool(block.eq(config.mask_token_id).any()):
                break
            rate = block.eq(config.mask_token_id).sum(1, dtype=torch.float32) / block_size
            hidden, _ = model.forward_block(block, rate, cache, commit=False)
            block, _ = commit_high_confidence(
                block,
                model.selected_logits(hidden),
                mask_token_id=config.mask_token_id,
                tokens_per_step=4,
            )
        if bool(block.eq(config.mask_token_id).any()):
            raise RuntimeError("eight-step decoder left mask tokens in a block")
        _, cache = model.forward_block(
            block,
            torch.zeros(batch_size, device=device, dtype=dtype),
            cache,
            commit=True,
        )
        for row in range(batch_size):
            if done[row]:
                continue
            remaining = max_new_tokens - len(completions[row])
            values = block[row, known : known + remaining].tolist()
            if eos_token_id is not None and eos_token_id in values:
                values = values[: values.index(eos_token_id) + 1]
                done[row] = True
            completions[row].extend(values)
            if len(completions[row]) >= max_new_tokens:
                done[row] = True
        partial = prompt_ids.new_empty((batch_size, 0))
    return [
        torch.cat((prompt_ids[row], torch.tensor(values, device=device, dtype=torch.long)))
        for row, values in enumerate(completions)
    ]


@torch.inference_mode()
def generate_blocks(
    model: SimplerBlockDiffusionForMaskedLM,
    prompt_ids: torch.Tensor,
    *,
    max_new_tokens: int,
    eos_token_id: int | None = None,
    temperature: float = 0.0,
    top_p: float = 1.0,
    generator: torch.Generator | None = None,
) -> torch.Tensor:
    if prompt_ids.ndim != 1:
        raise ValueError("generate_blocks accepts one unbatched prompt")
    return generate_blocks_batch(
        model,
        prompt_ids.unsqueeze(0),
        max_new_tokens=max_new_tokens,
        eos_token_id=eos_token_id,
        temperature=temperature,
        top_p=top_p,
        generator=generator,
    )[0]
