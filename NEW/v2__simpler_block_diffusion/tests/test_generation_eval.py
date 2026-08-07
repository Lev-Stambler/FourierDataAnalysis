from __future__ import annotations

import torch

from v2_simpler_block_diffusion.generation import (
    commit_high_confidence,
    generate_blocks,
    generate_blocks_batch,
)
from v2_simpler_block_diffusion.generation_eval import (
    generation_health,
    longest_identical_run,
    repeated_ngram_fraction,
)
from v2_simpler_block_diffusion.model import SimplerBlockDiffusionForMaskedLM


def test_generation_health_accepts_varied_completion() -> None:
    tokens = list(range(16))
    health = generation_health(tokens, "a useful completion", mask_token_id=99)
    assert health["non_degenerate"] is True
    assert health["longest_identical_run"] == 1
    assert health["repeated_4gram_fraction"] == 0.0


def test_generation_health_rejects_repetition_and_mask() -> None:
    tokens = [7] * 12 + [99]
    health = generation_health(tokens, "repeated repeated", mask_token_id=99)
    assert health["non_degenerate"] is False
    assert health["contains_mask"] is True
    assert longest_identical_run(tokens) == 12
    assert repeated_ngram_fraction(tokens) > 0.5


def test_batched_greedy_generation_matches_serial(tiny_config) -> None:
    torch.manual_seed(7)
    model = SimplerBlockDiffusionForMaskedLM(tiny_config).eval()
    prompts = torch.tensor([[1, 2, 3, 4], [5, 6, 7, 8]])
    batched = generate_blocks_batch(model, prompts, max_new_tokens=4)
    serial = [generate_blocks(model, prompt, max_new_tokens=4) for prompt in prompts]
    assert len(batched) == 2
    assert all(torch.equal(batch_output, serial_output) for batch_output, serial_output in zip(batched, serial))


def test_commit_high_confidence_commits_four_and_never_mask() -> None:
    tokens = torch.full((2, 8), 10, dtype=torch.long)
    logits = torch.full((2, 8, 11), -10.0)
    for position in range(8):
        logits[:, position, position] = float(position)
    result, committed = commit_high_confidence(
        tokens, logits, mask_token_id=10, tokens_per_step=4
    )
    assert torch.equal(committed.sum(1), torch.tensor([4, 4]))
    assert not bool(result[committed].eq(10).any())


def test_full_block_generation_uses_eight_refinement_forwards(tiny_config) -> None:
    torch.manual_seed(11)
    model = SimplerBlockDiffusionForMaskedLM(tiny_config).eval()
    calls = 0
    original = model.forward_block

    def counted(*args, **kwargs):
        nonlocal calls
        if not kwargs.get("commit", False):
            calls += 1
        return original(*args, **kwargs)

    model.forward_block = counted  # type: ignore[method-assign]
    prompt = torch.tensor([1, 2, 3, 4])
    output = generate_blocks(model, prompt, max_new_tokens=tiny_config.block_size)
    assert calls == tiny_config.block_size // 4
    assert output.shape[0] == prompt.shape[0] + tiny_config.block_size
    assert not bool(output.eq(tiny_config.mask_token_id).any())
