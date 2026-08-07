from __future__ import annotations

import torch

from v2_simpler_block_diffusion.config import SimplerBlockDiffusionConfig
from v2_simpler_block_diffusion.model import SimplerBlockDiffusionForMaskedLM


def test_zero_ada_stack_is_identity_before_final_norm(
    tiny_config: SimplerBlockDiffusionConfig,
) -> None:
    model = SimplerBlockDiffusionForMaskedLM(tiny_config)
    noisy = torch.randint(0, 60, (2, 8))
    clean = torch.randint(0, 60, (2, 8))
    rates = torch.rand(2, 2)
    output = model(noisy, clean, rates)
    embedded = model.embed_tokens(torch.stack((noisy, clean), dim=1))
    expected = model.final_norm(embedded)
    assert torch.allclose(output.noisy_hidden, expected[:, 0], atol=1e-6)
    assert torch.allclose(output.clean_hidden, expected[:, 1], atol=1e-6)


def test_selected_logits_only(tiny_config: SimplerBlockDiffusionConfig) -> None:
    model = SimplerBlockDiffusionForMaskedLM(tiny_config)
    ids = torch.randint(0, 60, (1, 8))
    selected = torch.tensor([1, 6])
    output = model(ids, ids, torch.zeros(1, 2), selected_indices=selected, return_logits=True)
    assert output.selected_hidden is not None and output.selected_hidden.shape == (2, 8)
    assert output.logits is not None and output.logits.shape == (2, 64)


def test_bfloat16_model_accepts_float_noise_rates(
    tiny_config: SimplerBlockDiffusionConfig,
) -> None:
    model = SimplerBlockDiffusionForMaskedLM(tiny_config).to(torch.bfloat16)
    ids = torch.randint(0, 60, (1, 8))
    output = model(ids, ids, torch.rand(1, 2, dtype=torch.float32))
    assert output.noisy_hidden.dtype == torch.bfloat16


def test_structural_attention_mask_broadcasts_across_batch(
    tiny_config: SimplerBlockDiffusionConfig,
) -> None:
    torch.manual_seed(7)
    model = SimplerBlockDiffusionForMaskedLM(tiny_config).eval()
    ids = torch.randint(0, 60, (1, 8))
    rates = torch.rand(1, 2)
    with torch.no_grad():
        single = model(ids, ids, rates).noisy_hidden
        batched = model(ids.expand(3, -1), ids.expand(3, -1), rates.expand(3, -1)).noisy_hidden
    torch.testing.assert_close(batched, single.expand_as(batched))


def test_no_future_block_gradient_through_cross_attention(
    tiny_config: SimplerBlockDiffusionConfig,
) -> None:
    model = SimplerBlockDiffusionForMaskedLM(tiny_config)
    x = torch.randn(1, 2, 2, 4, 8, requires_grad=True)
    output = model.layers[0].attention.forward_dual(x, None)
    output[:, 0, 1].sum().backward()
    assert x.grad is not None
    assert bool((x.grad[:, 1, 0].abs().sum() > 0))
    assert torch.equal(x.grad[:, 1, 1], torch.zeros_like(x.grad[:, 1, 1]))


def test_incremental_clean_blocks_match_dual_clean_stream(
    tiny_config: SimplerBlockDiffusionConfig,
) -> None:
    torch.manual_seed(8)
    model = SimplerBlockDiffusionForMaskedLM(tiny_config)
    for layer in model.layers:
        with torch.no_grad():
            layer.ada.bias[2 * tiny_config.hidden_size : 3 * tiny_config.hidden_size].fill_(0.2)
            layer.ada.bias[5 * tiny_config.hidden_size :].fill_(0.2)
    ids = torch.randint(0, 60, (1, 8))
    full = model(ids, ids, torch.zeros(1, 2)).clean_hidden
    cache = model.empty_cache(1, device=ids.device, dtype=model.embed_tokens.weight.dtype)
    blocks = []
    for start in (0, 4):
        hidden, cache = model.forward_block(ids[:, start : start + 4], torch.zeros(1), cache, commit=True)
        blocks.append(hidden)
    assert torch.allclose(torch.cat(blocks, dim=1), full, atol=2e-5, rtol=2e-5)
