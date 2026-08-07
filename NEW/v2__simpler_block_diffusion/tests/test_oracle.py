from __future__ import annotations

import torch

from v2_simpler_block_diffusion.oracle import factorize_tied_head


def test_head_oracle_recovers_low_rank_matrix() -> None:
    torch.manual_seed(5)
    left = torch.randn(31, 4)
    right = torch.randn(13, 4)
    weight = left @ right.T
    embedding, projection = factorize_tied_head(
        weight, width=4, oversample=2, iterations=4
    )
    assert torch.allclose(embedding @ projection.T, weight, atol=2e-4, rtol=2e-4)
