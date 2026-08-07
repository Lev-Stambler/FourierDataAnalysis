from __future__ import annotations

import pytest

from v2_simpler_block_diffusion.config import SimplerBlockDiffusionConfig


@pytest.fixture
def tiny_config() -> SimplerBlockDiffusionConfig:
    return SimplerBlockDiffusionConfig(
        architecture_id="test-tiny",
        vocab_size=64,
        mask_token_id=63,
        hidden_size=8,
        num_hidden_layers=2,
        num_attention_heads=2,
        num_key_value_heads=1,
        head_dim=4,
        block_size=4,
        max_position_embeddings=8,
        monarch_blocks=4,
        local_expansion=2,
        noise_embedding_size=8,
        noise_mlp_size=16,
    )

