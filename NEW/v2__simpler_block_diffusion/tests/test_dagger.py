from __future__ import annotations

import torch

from v2_simpler_block_diffusion.dagger import rollout_corruption
from v2_simpler_block_diffusion.model import SimplerBlockDiffusionForMaskedLM


def test_dagger_cutoffs_leave_expected_targets_and_no_grad(tiny_config) -> None:
    torch.manual_seed(3)
    model = SimplerBlockDiffusionForMaskedLM(tiny_config)
    clean = torch.arange(2 * tiny_config.block_size).reshape(2, -1) % (
        tiny_config.vocab_size - 1
    )
    eligible = torch.ones_like(clean, dtype=torch.bool)
    corruption = rollout_corruption(
        model,
        clean,
        eligible_mask=eligible,
        cutoffs=torch.tensor([0, 7]),
        projection_group_chunk=1,
    )
    # Tiny fixtures use four-token blocks, so one commit completes row 1.
    assert corruption.target_tokens == tiny_config.block_size
    assert corruption.selected_block_ids.unique().tolist() == [0]
    assert all(parameter.grad is None for parameter in model.parameters())
    assert model.training


def test_dagger_partial_eligibility_never_supervises_ineligible_tokens(tiny_config) -> None:
    model = SimplerBlockDiffusionForMaskedLM(tiny_config)
    clean = torch.arange(tiny_config.block_size).reshape(1, -1)
    eligible = torch.tensor([[True, False, True, False]])
    corruption = rollout_corruption(
        model, clean, eligible_mask=eligible, cutoffs=torch.tensor([0])
    )
    assert torch.equal(corruption.selected_indices, torch.tensor([0, 2]))
    assert torch.equal(corruption.hard_labels, clean[0, [0, 2]])
