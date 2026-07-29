from __future__ import annotations

import math
import os
from dataclasses import asdict, dataclass
from typing import Literal

MODEL_ID = "Qwen/Qwen3.5-0.8B-Base"
MODEL_REVISION = "5c8a1b97ddef11f79b47ab9d07bf82b9117413f6"
FINEWEB_EDU_ID = "HuggingFaceFW/fineweb-edu"
FINEWEB_EDU_CONFIG = "sample-350BT"
FINEWEB_EDU_REVISION = "87f09149ef4734204d70ed1d046ddc9ca3f2b8f9"

CONTEXT_LENGTH = 16
EMBEDDING_WIDTH = 1_024
VOCAB_SIZE = 248_320
TRAIN_EXAMPLES = 4_194_304
VALIDATION_EXAMPLES = 8_192
TEST_EXAMPLES = 8_192
AUDIT_EXAMPLES = 2_048

SCREEN_EXAMPLES = 262_144
FINAL_EXAMPLES = TRAIN_EXAMPLES
NORMUON_LR_GRID = (1e-3, 3e-3, 1e-2, 2e-2)
DEFAULT_NORMUON_LR = 3e-3
AUX_ADAMW_LR = 3e-4
WEIGHT_DECAY = 0.01
GRADIENT_CLIP_NORM = 1.0
NORMUON_BETA1 = 0.95
NORMUON_BETA2 = 0.95
ADAMW_BETAS = (0.9, 0.95)
OPTIMIZER_EPS = 1e-10
INITIALIZER_RANGE = 0.02

Stage = Literal["screen", "final"]


def default_optimizer_policy() -> dict:
    return {
        "factor_optimizer": "normuon",
        "factor_update_granularity":
            "independent_rank_slice_matrix",
        "shape_batched_newton_schulz": True,
        "factor_lr": DEFAULT_NORMUON_LR,
        "factor_betas": [NORMUON_BETA1, NORMUON_BETA2],
        "nesterov": True,
        "newton_schulz_steps": 5,
        "epsilon": OPTIMIZER_EPS,
        "weight_decay": WEIGHT_DECAY,
        "auxiliary_optimizer": "fused_adamw",
        "auxiliary_lr": AUX_ADAMW_LR,
        "auxiliary_betas": list(ADAMW_BETAS),
        "schedule": "wsd_shared_multiplier",
        "selection_provenance":
            "matched_step64_normuon_vs_adamw_2026-07-28",
    }


@dataclass(frozen=True)
class Architecture:
    context_length: int = CONTEXT_LENGTH
    embedding_width: int = EMBEDDING_WIDTH
    depth: int = 4
    expansion: int = 4
    repetitions: int = 4
    rank: int = 2_407
    rank_chunk: int = 32

    @property
    def full_width(self) -> int:
        return self.context_length * self.embedding_width

    @property
    def residual_multiplier(self) -> float:
        return 1.0 / math.sqrt(self.depth)

    @property
    def label(self) -> str:
        return (
            f"kron-c3-r{self.rank}-resffn-d{self.depth}"
            f"-x{self.expansion}-repeat{self.repetitions}"
        )

    def validate(self) -> None:
        if min(
            self.context_length,
            self.embedding_width,
            self.depth,
            self.expansion,
            self.repetitions,
            self.rank,
            self.rank_chunk,
        ) <= 0:
            raise ValueError("architecture dimensions must be positive")
        if self.expansion != 4:
            raise ValueError("the study is pinned to expansion four")
        if math.isqrt(self.embedding_width) ** 2 != self.embedding_width:
            raise ValueError("embedding width must be a perfect square")


@dataclass(frozen=True)
class Trial:
    stage: Stage
    normuon_lr: float
    effective_batch: int
    examples: int
    warmup_steps: int
    stable_steps: int
    cooldown_steps: int
    seed: int = 0
    architecture: Architecture = Architecture()
    aux_adamw_lr: float = AUX_ADAMW_LR
    gradient_clip_norm: float = GRADIENT_CLIP_NORM

    @property
    def steps(self) -> int:
        return self.warmup_steps + self.stable_steps + self.cooldown_steps

    @property
    def checkpoint_every_examples(self) -> int:
        return 65_536 if self.stage == "screen" else 262_144

    @property
    def audit_every(self) -> int:
        return self.checkpoint_every_examples // self.effective_batch

    @property
    def label(self) -> str:
        lr = f"{self.normuon_lr:g}".replace(".", "p")
        return (
            f"normuon_{self.stage}-{self.architecture.label}"
            f"-nlr{lr}-alr3e-4-b{self.effective_batch}"
            f"-wsd-w{self.warmup_steps}-c{self.cooldown_steps}"
            f"-clip1-ce-erand-s{self.seed}"
        )

    def validate(self) -> None:
        self.architecture.validate()
        if self.stage not in ("screen", "final"):
            raise ValueError(f"unsupported stage {self.stage}")
        if not 1e-3 <= self.normuon_lr <= 2e-2:
            raise ValueError("NorMuon LR must be in [1e-3, 2e-2]")
        if self.aux_adamw_lr != AUX_ADAMW_LR:
            raise ValueError("auxiliary AdamW LR is pinned to 3e-4")
        if self.effective_batch <= 0:
            raise ValueError("effective batch must be positive")
        if self.steps * self.effective_batch != self.examples:
            raise ValueError("schedule must cover the exact example budget")
        if min(
            self.warmup_steps,
            self.stable_steps,
            self.cooldown_steps,
        ) <= 0:
            raise ValueError("all WSD phases must be positive")
        if self.checkpoint_every_examples % self.effective_batch:
            raise ValueError("checkpoint cadence must divide by batch")

    def to_dict(self) -> dict:
        return asdict(self)


def screen_trials() -> list[Trial]:
    trials = [
        Trial(
            stage="screen",
            normuon_lr=lr,
            effective_batch=2_048,
            examples=SCREEN_EXAMPLES,
            warmup_steps=8,
            stable_steps=108,
            cooldown_steps=12,
        )
        for lr in NORMUON_LR_GRID
    ]
    for trial in trials:
        trial.validate()
    return trials


def final_trials(top_lrs: tuple[float, float]) -> list[Trial]:
    if (
        len(top_lrs) != 2
        or len(set(top_lrs)) != 2
        or any(lr not in NORMUON_LR_GRID for lr in top_lrs)
    ):
        raise ValueError("finalists must be two distinct screened LRs")
    phases = {
        2_048: (128, 1_728, 192),
        4_096: (64, 864, 96),
    }
    trials = []
    for effective_batch, (
        warmup_steps,
        stable_steps,
        cooldown_steps,
    ) in phases.items():
        for lr in top_lrs:
            trials.append(Trial(
                stage="final",
                normuon_lr=lr,
                effective_batch=effective_batch,
                examples=FINAL_EXAMPLES,
                warmup_steps=warmup_steps,
                stable_steps=stable_steps,
                cooldown_steps=cooldown_steps,
            ))
    for trial in trials:
        trial.validate()
    return trials


def wsd_multiplier(trial: Trial, step: int) -> float:
    trial.validate()
    if not 1 <= step <= trial.steps:
        raise ValueError(f"step must be in 1..{trial.steps}")
    if step <= trial.warmup_steps:
        return step / trial.warmup_steps
    stable_end = trial.warmup_steps + trial.stable_steps
    if step <= stable_end:
        return 1.0
    return (trial.steps - step) / trial.cooldown_steps


def microbatch_for(trial: Trial) -> tuple[int, int]:
    limit = int(os.environ.get("QWEN_NORMUON_MICROBATCH", "64"))
    if limit <= 0:
        raise ValueError("microbatch limit must be positive")
    microbatch = min(limit, trial.effective_batch)
    return microbatch, math.ceil(trial.effective_batch / microbatch)
