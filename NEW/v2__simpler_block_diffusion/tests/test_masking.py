from __future__ import annotations

import torch

from v2_simpler_block_diffusion.masking import (
    corrupt_blocks,
    dual_stream_attention_mask,
    subsample_corruption_targets,
)


def test_corruption_masks_nonzero_uniform_block_count() -> None:
    ids = torch.arange(16).reshape(2, 8)
    eligible = torch.ones_like(ids, dtype=torch.bool)
    eligible[0, :4] = False
    batch = corrupt_blocks(
        ids,
        mask_token_id=99,
        block_size=4,
        eligible_mask=eligible,
        generator=torch.Generator().manual_seed(3),
    )
    assert batch.block_noise[0, 0] == 0
    assert bool((batch.block_noise[batch.block_noise > 0] >= 0.25).all())
    assert batch.target_tokens == int((batch.noisy_ids == 99).sum())
    assert torch.equal(ids.reshape(-1)[batch.selected_indices], batch.hard_labels)


def test_dual_stream_mask_semantics() -> None:
    mask = dual_stream_attention_mask(length=8, block_size=4)[0, 0]
    # First noisy block sees only its own noisy block.
    assert bool(mask[0, :4].all())
    assert not bool(mask[0, 4:].any())
    # Second noisy block sees its own noisy block and first clean block.
    assert bool(mask[4, 4:8].all())
    assert bool(mask[4, 8:12].all())
    assert not bool(mask[4, 12:].any())
    # Second clean block sees both clean blocks and no noisy positions.
    assert not bool(mask[12, :8].any())
    assert bool(mask[12, 8:].all())


def test_target_subsampling_caps_each_context_without_changing_noise() -> None:
    ids = torch.arange(64).reshape(2, 32)
    corruption = corrupt_blocks(
        ids,
        mask_token_id=99,
        block_size=4,
        generator=torch.Generator().manual_seed(7),
    )
    noisy = corruption.noisy_ids.clone()
    limited = subsample_corruption_targets(
        corruption,
        max_targets_per_context=5,
        generator=torch.Generator().manual_seed(11),
    )
    assert torch.equal(limited.noisy_ids, noisy)
    rows = limited.selected_indices // ids.shape[1]
    assert all(int((rows == row).sum()) <= 5 for row in range(ids.shape[0]))
    assert limited.target_tokens <= 10
    assert torch.equal(
        ids.reshape(-1).index_select(0, limited.selected_indices), limited.hard_labels
    )
