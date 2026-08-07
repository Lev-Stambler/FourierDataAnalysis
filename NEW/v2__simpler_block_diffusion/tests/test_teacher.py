from types import SimpleNamespace

import torch
from torch import nn

from v2_simpler_block_diffusion.masking import CorruptionBatch
from v2_simpler_block_diffusion.teacher import OnlineDreamTeacher


class HiddenModel(nn.Module):
    def __init__(self) -> None:
        super().__init__()
        self.last_mask_shape = None

    def forward(self, *, input_ids, attention_mask, **_kwargs):
        self.last_mask_shape = tuple(attention_mask.shape)
        hidden = torch.nn.functional.one_hot(input_ids % 8, 8).float()
        return SimpleNamespace(last_hidden_state=hidden)


class FakeDream(nn.Module):
    def __init__(self) -> None:
        super().__init__()
        self.model = HiddenModel()
        self.lm_head = nn.Linear(8, 16, bias=False)


def test_teacher_structural_mask_keeps_singleton_batch() -> None:
    dream = FakeDream()
    teacher = OnlineDreamTeacher(dream)
    clean = torch.arange(3 * 64).reshape(3, 64) % 16
    corruption = CorruptionBatch(
        noisy_ids=clean,
        clean_ids=clean,
        block_noise=torch.ones(3, 2),
        selected_indices=torch.tensor([0, 64, 128]),
        hard_labels=torch.zeros(3, dtype=torch.long),
        selected_block_ids=torch.arange(3),
        eligible_mask=torch.ones_like(clean, dtype=torch.bool),
    )
    hidden = teacher.selected_hidden(corruption)
    assert hidden.shape == (3, 8)
    assert dream.model.last_mask_shape == (1, 1, 128, 128)
