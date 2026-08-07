from __future__ import annotations

from dataclasses import dataclass

import torch

from .loss import GroupedTeacherTargets, streaming_topk
from .masking import CorruptionBatch, dual_stream_attention_mask


TEACHER_MODEL_ID = "Dream-org/DreamReasoner-8B"
TEACHER_REVISION = "ed62b1d2c82ccd234b05ed2463b4c0ee640f2068"


@dataclass(frozen=True)
class TeacherSpec:
    model_id: str = TEACHER_MODEL_ID
    revision: str = TEACHER_REVISION
    topk: int = 16
    position_chunk_size: int = 512
    vocab_chunk_size: int = 8_192


class OnlineDreamTeacher:
    """Frozen DreamReasoner adapter that emits ephemeral grouped targets."""

    def __init__(self, model: torch.nn.Module, *, spec: TeacherSpec | None = None) -> None:
        self.model = model.eval()
        self.spec = spec or TeacherSpec()
        self.model.requires_grad_(False)
        if not hasattr(model, "model") or not hasattr(model, "lm_head"):
            raise TypeError("Dream teacher must expose .model and .lm_head")
        self._attention_masks: dict[tuple[int, int, torch.device], torch.Tensor] = {}

    @classmethod
    def from_pretrained(
        cls,
        *,
        device: torch.device | str = "cuda",
        spec: TeacherSpec | None = None,
        attn_implementation: str = "sdpa",
    ) -> "OnlineDreamTeacher":
        from transformers import AutoModelForCausalLM

        selected = spec or TeacherSpec()
        model = AutoModelForCausalLM.from_pretrained(
            selected.model_id,
            revision=selected.revision,
            trust_remote_code=True,
            dtype=torch.bfloat16,
            attn_implementation=attn_implementation,
        ).to(device)
        return cls(model, spec=selected)

    @property
    def output_weight(self) -> torch.Tensor:
        return self.model.lm_head.weight

    @torch.inference_mode()
    def selected_hidden(self, corruption: CorruptionBatch) -> torch.Tensor:
        noisy, clean = corruption.noisy_ids, corruption.clean_ids
        batch, length = noisy.shape
        packed = torch.cat((noisy, clean), dim=1)
        positions = torch.arange(length, device=packed.device).expand(batch, -1)
        positions = torch.cat((positions, positions), dim=1)
        block_size = length // corruption.block_noise.shape[1]
        mask_key = (length, block_size, packed.device)
        attention_mask = self._attention_masks.get(mask_key)
        if attention_mask is None:
            # Every training context is full length, so every example has the
            # same structural mask. A singleton batch dimension broadcasts in
            # SDPA and avoids a batch-sized ~4 GiB FP32 clone at batch 56.
            attention_mask = dual_stream_attention_mask(
                length=length,
                block_size=block_size,
                batch_size=1,
                token_mask=None,
                device=packed.device,
                additive=True,
                dtype=torch.float32,
            )
            self._attention_masks[mask_key] = attention_mask
        output = self.model.model(
            input_ids=packed,
            attention_mask=attention_mask,
            position_ids=positions,
            use_cache=False,
            return_dict=True,
        )
        hidden = output.last_hidden_state[:, :length].reshape(batch * length, -1)
        return hidden.index_select(0, corruption.selected_indices)

    def targets(self, corruption: CorruptionBatch, *, excluded_token_id: int) -> GroupedTeacherTargets:
        # selected_hidden is an inference tensor. Clone outside inference mode
        # so the resulting grouped targets may safely be saved by the custom
        # student autograd function as non-differentiable constants.
        hidden = self.selected_hidden(corruption).clone()
        chunks: list[GroupedTeacherTargets] = []
        for start in range(0, hidden.shape[0], self.spec.position_chunk_size):
            chunks.append(
                streaming_topk(
                    hidden[start : start + self.spec.position_chunk_size],
                    self.output_weight,
                    topk=self.spec.topk,
                    vocab_chunk_size=self.spec.vocab_chunk_size,
                    excluded_token_id=excluded_token_id,
                )
            )
        targets = GroupedTeacherTargets(
            torch.cat([chunk.top_ids for chunk in chunks]),
            torch.cat([chunk.top_log_probs for chunk in chunks]),
            torch.cat([chunk.tail_log_prob for chunk in chunks]),
        )
        return GroupedTeacherTargets(
            targets.top_ids.detach().clone(),
            targets.top_log_probs.detach().clone(),
            targets.tail_log_prob.detach().clone(),
        )

    def hidden_for_loss(self, corruption: CorruptionBatch) -> torch.Tensor:
        """Return frozen teacher features as an ordinary, autograd-saveable tensor."""
        return self.selected_hidden(corruption).clone()
