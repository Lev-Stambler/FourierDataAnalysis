from __future__ import annotations

import json
import math
from dataclasses import asdict, dataclass
from pathlib import Path

import torch

from .loss import DistillationLoss, DistillationLossOutput
from .masking import corrupt_blocks
from .model import SimplerBlockDiffusionForMaskedLM
from .teacher import OnlineDreamTeacher


@dataclass
class TokenCounters:
    target_tokens: int = 0
    clean_tokens: int = 0
    model_tokens: int = 0
    optimizer_steps: int = 0


@dataclass(frozen=True)
class OptimizerConfig:
    learning_rate: float = 3e-4
    weight_decay: float = 0.1
    beta1: float = 0.9
    beta2: float = 0.95
    eps: float = 1e-8
    warmup_target_tokens: int = 100_000_000
    total_target_tokens: int = 10_000_000_000
    schedule_origin_target_tokens: int = 0
    final_lr_fraction: float = 0.1
    gradient_clip: float = 1.0


def build_adamw(model: torch.nn.Module, config: OptimizerConfig) -> torch.optim.AdamW:
    decay: list[torch.nn.Parameter] = []
    no_decay: list[torch.nn.Parameter] = []
    for name, parameter in model.named_parameters():
        if not parameter.requires_grad:
            continue
        if parameter.ndim < 2 or "embed_tokens" in name or ".ada." in name:
            no_decay.append(parameter)
        else:
            decay.append(parameter)
    return torch.optim.AdamW(
        [
            {"params": decay, "weight_decay": config.weight_decay},
            {"params": no_decay, "weight_decay": 0.0},
        ],
        lr=config.learning_rate,
        betas=(config.beta1, config.beta2),
        eps=config.eps,
    )


def token_lr_scale(target_tokens: int, config: OptimizerConfig) -> float:
    schedule_tokens = max(target_tokens - config.schedule_origin_target_tokens, 0)
    if schedule_tokens < config.warmup_target_tokens:
        return max(schedule_tokens, 1) / config.warmup_target_tokens
    progress = min(
        1.0,
        (schedule_tokens - config.warmup_target_tokens)
        / max(config.total_target_tokens - config.warmup_target_tokens, 1),
    )
    cosine = 0.5 * (1 + math.cos(math.pi * progress))
    return config.final_lr_fraction + (1 - config.final_lr_fraction) * cosine


class OnlineDistillationTrainer:
    def __init__(
        self,
        student: SimplerBlockDiffusionForMaskedLM,
        teacher: OnlineDreamTeacher,
        optimizer: torch.optim.Optimizer,
        optimizer_config: OptimizerConfig,
    ) -> None:
        self.student = student
        self.teacher = teacher
        self.optimizer = optimizer
        self.optimizer_config = optimizer_config
        self.counters = TokenCounters()
        self.loss_fn = DistillationLoss(excluded_token_id=student.config.mask_token_id)

    def train_step(
        self,
        clean_ids: torch.Tensor,
        *,
        eligible_mask: torch.Tensor | None = None,
        generator: torch.Generator | None = None,
    ) -> DistillationLossOutput:
        corruption = corrupt_blocks(
            clean_ids,
            mask_token_id=self.student.config.mask_token_id,
            block_size=self.student.config.block_size,
            eligible_mask=eligible_mask,
            generator=generator,
        )
        if corruption.target_tokens == 0:
            raise ValueError("training batch contains no eligible target tokens")
        targets = self.teacher.targets(
            corruption, excluded_token_id=self.student.config.mask_token_id
        )
        output = self.student(
            corruption.noisy_ids,
            corruption.clean_ids,
            corruption.block_noise,
            selected_indices=corruption.selected_indices,
        )
        assert output.selected_hidden is not None
        loss = self.loss_fn(
            output.selected_hidden,
            self.student.embed_tokens.weight,
            targets,
            corruption.hard_labels,
            corruption.selected_block_ids,
        )
        self.optimizer.zero_grad(set_to_none=True)
        loss.loss.backward()
        torch.nn.utils.clip_grad_norm_(self.student.parameters(), self.optimizer_config.gradient_clip)
        for parameter in self.student.parameters():
            if parameter.grad is not None and not bool(torch.isfinite(parameter.grad).all()):
                raise FloatingPointError("non-finite student gradient")
        self.optimizer.step()
        self.counters.target_tokens += corruption.target_tokens
        self.counters.clean_tokens += clean_ids.numel()
        self.counters.model_tokens += 2 * clean_ids.numel()
        self.counters.optimizer_steps += 1
        scale = token_lr_scale(self.counters.target_tokens, self.optimizer_config)
        for group in self.optimizer.param_groups:
            group["lr"] = self.optimizer_config.learning_rate * scale
        return loss

    def save_checkpoint(self, directory: str | Path, *, metadata: dict) -> None:
        destination = Path(directory)
        destination.mkdir(parents=True, exist_ok=True)
        combined = {**metadata, "token_counters": asdict(self.counters)}
        self.student.save_pretrained(destination, combined)
        torch.save(self.optimizer.state_dict(), destination / "optimizer.pt")
        (destination / "trainer_state.json").write_text(
            json.dumps({"counters": asdict(self.counters)}, indent=2, sort_keys=True) + "\n"
        )
