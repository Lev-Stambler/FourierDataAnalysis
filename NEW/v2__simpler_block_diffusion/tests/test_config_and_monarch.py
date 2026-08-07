from __future__ import annotations

import torch

from v2_simpler_block_diffusion.config import SimplerBlockDiffusionConfig
from v2_simpler_block_diffusion.model import SimplerBlockDiffusionForMaskedLM
from v2_simpler_block_diffusion.monarch import MonarchLinear


def test_frozen_parameter_count() -> None:
    assert SimplerBlockDiffusionConfig().expected_parameter_count == 97_832_320


def test_tiny_parameter_count_matches_model(tiny_config: SimplerBlockDiffusionConfig) -> None:
    model = SimplerBlockDiffusionForMaskedLM(tiny_config)
    assert model.num_parameters() == tiny_config.expected_parameter_count


def test_monarch_dense_equivalence() -> None:
    torch.manual_seed(1)
    layer = MonarchLinear(16, 24, nblocks=4, bias=False)
    x = torch.randn(3, 16)
    assert torch.allclose(layer(x), torch.nn.functional.linear(x, layer.dense_weight()), atol=1e-5)

