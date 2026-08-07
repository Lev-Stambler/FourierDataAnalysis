from __future__ import annotations

import torch

from .generation import commit_high_confidence
from .masking import CorruptionBatch
from .model import SimplerBlockDiffusionForMaskedLM


@torch.no_grad()
def rollout_corruption(
    student: SimplerBlockDiffusionForMaskedLM,
    clean_ids: torch.Tensor,
    *,
    eligible_mask: torch.Tensor,
    generator: torch.Generator | None = None,
    cutoffs: torch.Tensor | None = None,
    tokens_per_step: int = 4,
    projection_group_chunk: int = 32,
) -> CorruptionBatch:
    """Build within-block DAgger states using a random decoder prefix.

    Every context receives one cutoff in [0, 7].  All of its blocks are rolled
    to that cutoff together, keeping the implementation to at most seven
    student backbone calls.  Teacher querying remains a separate single call.
    """
    if clean_ids.ndim != 2 or eligible_mask.shape != clean_ids.shape:
        raise ValueError("clean IDs and eligible mask must be matching matrices")
    config = student.config
    batch, length = clean_ids.shape
    if length % config.block_size:
        raise ValueError("DAgger contexts must be block aligned")
    blocks = length // config.block_size
    if cutoffs is None:
        cutoffs = torch.randint(
            0, 8, (batch,), device=clean_ids.device, generator=generator
        )
    if cutoffs.shape != (batch,) or bool(((cutoffs < 0) | (cutoffs > 7)).any()):
        raise ValueError("one DAgger cutoff in [0, 7] is required per context")
    noisy = clean_ids.masked_fill(eligible_mask, config.mask_token_id)
    grouped_eligible = eligible_mask.reshape(batch * blocks, config.block_size)
    grouped_noisy = noisy.reshape(batch * blocks, config.block_size)
    group_rows = torch.arange(batch, device=clean_ids.device).repeat_interleave(blocks)
    was_training = student.training
    student.eval()
    try:
        for step in range(7):
            active_groups = cutoffs.index_select(0, group_rows) > step
            active_groups &= grouped_noisy.eq(config.mask_token_id).any(1)
            if not bool(active_groups.any()):
                continue
            rates = grouped_noisy.eq(config.mask_token_id).sum(1, dtype=torch.float32)
            rates = rates.reshape(batch, blocks) / config.block_size
            output = student(noisy, clean_ids, rates)
            grouped_hidden = output.noisy_hidden.reshape(
                batch * blocks, config.block_size, config.hidden_size
            )
            for start in range(0, batch * blocks, projection_group_chunk):
                stop = min(start + projection_group_chunk, batch * blocks)
                local_active = active_groups[start:stop]
                if not bool(local_active.any()):
                    continue
                candidate, _ = commit_high_confidence(
                    grouped_noisy[start:stop],
                    student.selected_logits(grouped_hidden[start:stop]),
                    mask_token_id=config.mask_token_id,
                    tokens_per_step=tokens_per_step,
                )
                grouped_noisy[start:stop] = torch.where(
                    local_active[:, None], candidate, grouped_noisy[start:stop]
                )
    finally:
        student.train(was_training)

    remaining = grouped_noisy.eq(config.mask_token_id) & grouped_eligible
    selected = torch.nonzero(remaining.reshape(-1), as_tuple=False).flatten()
    return CorruptionBatch(
        noisy,
        clean_ids,
        remaining.sum(1, dtype=torch.float32).reshape(batch, blocks) / config.block_size,
        selected,
        clean_ids.reshape(-1).index_select(0, selected),
        selected // config.block_size,
        eligible_mask,
    )
