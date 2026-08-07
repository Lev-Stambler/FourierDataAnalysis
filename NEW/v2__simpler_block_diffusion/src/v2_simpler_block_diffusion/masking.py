from __future__ import annotations

from dataclasses import dataclass

import torch


@dataclass
class CorruptionBatch:
    noisy_ids: torch.Tensor
    clean_ids: torch.Tensor
    block_noise: torch.Tensor
    selected_indices: torch.Tensor
    hard_labels: torch.Tensor
    selected_block_ids: torch.Tensor
    eligible_mask: torch.Tensor

    @property
    def target_tokens(self) -> int:
        return int(self.selected_indices.numel())


def subsample_corruption_targets(
    corruption: CorruptionBatch,
    *,
    max_targets_per_context: int,
    generator: torch.Generator | None = None,
) -> CorruptionBatch:
    """Cap supervised rows while preserving the complete noisy corruption."""
    if max_targets_per_context <= 0:
        raise ValueError("target cap must be positive")
    batch, length = corruption.clean_ids.shape
    selected_rows = corruption.selected_indices // length
    kept: list[torch.Tensor] = []
    for row in range(batch):
        candidates = torch.nonzero(selected_rows == row, as_tuple=False).flatten()
        if candidates.numel() <= max_targets_per_context:
            kept.append(candidates)
            continue
        # Round-robin over randomly ordered blocks so one heavily masked block
        # cannot consume the complete per-context supervision budget.
        queues: list[torch.Tensor] = []
        row_blocks = corruption.selected_block_ids.index_select(0, candidates)
        for block_id in torch.unique(row_blocks, sorted=True):
            local = candidates.index_select(
                0, torch.nonzero(row_blocks == block_id, as_tuple=False).flatten()
            )
            order = torch.randperm(local.numel(), device=local.device, generator=generator)
            queues.append(local.index_select(0, order))
        cursor = [0] * len(queues)
        chosen: list[torch.Tensor] = []
        while len(chosen) < max_targets_per_context:
            progressed = False
            for queue_index, queue in enumerate(queues):
                if cursor[queue_index] < queue.numel():
                    chosen.append(queue[cursor[queue_index]])
                    cursor[queue_index] += 1
                    progressed = True
                    if len(chosen) == max_targets_per_context:
                        break
            if not progressed:
                break
        kept.append(torch.stack(chosen))
    indices = torch.cat(kept) if kept else corruption.selected_indices.new_empty(0)
    return CorruptionBatch(
        corruption.noisy_ids,
        corruption.clean_ids,
        corruption.block_noise,
        corruption.selected_indices.index_select(0, indices),
        corruption.hard_labels.index_select(0, indices),
        corruption.selected_block_ids.index_select(0, indices),
        corruption.eligible_mask,
    )


def corrupt_blocks(
    clean_ids: torch.Tensor,
    *,
    mask_token_id: int,
    block_size: int,
    eligible_mask: torch.Tensor | None = None,
    generator: torch.Generator | None = None,
) -> CorruptionBatch:
    """Mask an exactly uniform nonzero count independently in every eligible block."""
    if clean_ids.ndim != 2 or clean_ids.shape[1] % block_size:
        raise ValueError("clean_ids must be [batch, block-aligned sequence]")
    if eligible_mask is None:
        eligible_mask = torch.ones_like(clean_ids, dtype=torch.bool)
    if eligible_mask.shape != clean_ids.shape:
        raise ValueError("eligible_mask must match clean_ids")
    eligible_mask = eligible_mask.bool()
    noisy = clean_ids.clone()
    batch, length = clean_ids.shape
    blocks = length // block_size
    rates = torch.zeros((batch, blocks), dtype=torch.float32, device=clean_ids.device)
    positions: list[torch.Tensor] = []
    labels: list[torch.Tensor] = []
    block_ids: list[torch.Tensor] = []

    for row in range(batch):
        for block in range(blocks):
            start = block * block_size
            local = torch.nonzero(eligible_mask[row, start : start + block_size], as_tuple=False).flatten()
            count = int(local.numel())
            if count == 0:
                continue
            masked_count = int(
                torch.randint(1, count + 1, (), generator=generator, device=clean_ids.device).item()
            )
            order = torch.randperm(count, generator=generator, device=clean_ids.device)[:masked_count]
            chosen = local.index_select(0, order) + start
            noisy[row, chosen] = mask_token_id
            rates[row, block] = masked_count / block_size
            flat = chosen + row * length
            positions.append(flat)
            labels.append(clean_ids[row, chosen])
            block_ids.append(
                torch.full((masked_count,), row * blocks + block, device=clean_ids.device, dtype=torch.long)
            )

    if not positions:
        empty = torch.empty(0, dtype=torch.long, device=clean_ids.device)
        return CorruptionBatch(noisy, clean_ids, rates, empty, empty, empty, eligible_mask)
    return CorruptionBatch(
        noisy,
        clean_ids,
        rates,
        torch.cat(positions).long(),
        torch.cat(labels).long(),
        torch.cat(block_ids).long(),
        eligible_mask,
    )


def dual_stream_attention_mask(
    *,
    length: int,
    block_size: int,
    batch_size: int = 1,
    token_mask: torch.Tensor | None = None,
    device: torch.device | str | None = None,
    additive: bool = False,
    dtype: torch.dtype = torch.float32,
) -> torch.Tensor:
    """Packed Dream/BD3 mask for `[noisy, clean]` streams.

    Noisy queries see their noisy same-block workspace and earlier clean blocks.
    Clean queries see their clean same-block workspace and earlier clean blocks.
    """
    if length <= 0 or length % block_size:
        raise ValueError("length must be a positive multiple of block_size")
    device = torch.device("cpu") if device is None else torch.device(device)
    blocks = torch.arange(length, device=device) // block_size
    total = 2 * length
    allowed = torch.zeros((total, total), dtype=torch.bool, device=device)
    for query_stream in range(2):
        q_start = query_stream * length
        q_blocks = blocks[:, None]
        if query_stream == 0:
            allowed[q_start : q_start + length, :length] = q_blocks == blocks[None, :]
        allowed[q_start : q_start + length, length:] = blocks[None, :] < q_blocks
        if query_stream == 1:
            allowed[q_start : q_start + length, length:] |= blocks[None, :] == q_blocks
    allowed = allowed.unsqueeze(0).expand(batch_size, -1, -1).clone()
    if token_mask is not None:
        if token_mask.shape == (batch_size, length):
            token_mask = torch.cat((token_mask, token_mask), dim=1)
        if token_mask.shape != (batch_size, total):
            raise ValueError("token_mask must cover one or both packed streams")
        valid = token_mask.to(device=device, dtype=torch.bool)
        allowed &= valid[:, :, None]
        allowed &= valid[:, None, :]
    if not additive:
        return allowed[:, None]
    result = torch.zeros(allowed.shape, device=device, dtype=dtype)
    result.masked_fill_(~allowed, torch.finfo(dtype).min)
    return result[:, None]
