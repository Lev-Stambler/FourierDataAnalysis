from __future__ import annotations

from dataclasses import asdict, dataclass
from typing import Literal

MODEL_ID = "Qwen/Qwen3.5-0.8B-Base"
MODEL_REVISION = "5c8a1b97ddef11f79b47ab9d07bf82b9117413f6"
FINEWEB_ID = "HuggingFaceFW/fineweb"
FINEWEB_CONFIG = "CC-MAIN-2024-10"
FINEWEB_REVISION = "9bb295ddab0e05d785b879661af7260fed5140fc"

CONTEXT_LENGTH = 16
LSH_BITS = 18
LSH_SEED = 0
ACTIVE_WIDTH = CONTEXT_LENGTH * LSH_BITS
FULL_WIDTH = ACTIVE_WIDTH + 1
MONARCH_BLOCKS = 17

TRAIN_EXAMPLES = 4_194_304
VALIDATION_EXAMPLES = 8_192
TEST_EXAMPLES = 8_192
AUDIT_EXAMPLES = 2_048
EFFECTIVE_BATCH = 1_024

Form = Literal["sequential", "residual_one", "residual_ffn"]


@dataclass(frozen=True)
class ArchitectureConfig:
    form: Form
    depth: int
    expansion: int = 1
    context_length: int = CONTEXT_LENGTH
    code_bits: int = LSH_BITS
    full_width: int = FULL_WIDTH
    monarch_blocks: int = MONARCH_BLOCKS
    monarch_rank: int = 1

    @property
    def active_width(self) -> int:
        return self.context_length * self.code_bits

    @property
    def label(self) -> str:
        expansion = f"-x{self.expansion}" if self.form == "residual_ffn" else ""
        return (
            f"lsh{self.code_bits}-monarch-r{self.monarch_rank}-"
            f"{self.form}-d{self.depth}{expansion}"
        )

    def validate(self) -> None:
        if self.depth <= 0:
            raise ValueError("depth must be positive")
        if self.form not in ("sequential", "residual_one", "residual_ffn"):
            raise ValueError(f"unknown form {self.form}")
        if self.form != "residual_ffn" and self.expansion != 1:
            raise ValueError("expansion only applies to residual_ffn")
        if self.expansion not in (1, 4):
            raise ValueError("expansion must be 1 or 4")
        if self.monarch_rank <= 0:
            raise ValueError("Monarch rank must be positive")
        if self.active_width + 1 != self.full_width:
            raise ValueError("full width must be active LSH width plus one pad")
        if self.monarch_blocks * self.monarch_blocks != self.full_width:
            raise ValueError("full width must be the square of Monarch blocks")
        if self.full_width % self.monarch_blocks:
            raise ValueError("full width must divide evenly into Monarch blocks")
        if (self.full_width * self.expansion) % self.monarch_blocks:
            raise ValueError("expanded width must divide into Monarch blocks")

    def to_dict(self) -> dict:
        return asdict(self)


@dataclass(frozen=True)
class TrialConfig:
    architecture: ArchitectureConfig
    lr: float = 1e-3
    seed: int = 0
    steps: int = 1_000
    stage: str = "screen"
    audit_every: int = 100
    compile_model: bool = False
    smoke: bool = False

    @property
    def label(self) -> str:
        lr = f"{self.lr:g}".replace(".", "p")
        return f"{self.stage}-{self.architecture.label}-lr{lr}-s{self.seed}"

    def to_dict(self) -> dict:
        value = asdict(self)
        value["architecture"]["label"] = self.architecture.label
        return value


def microbatch_for(_: ArchitectureConfig) -> tuple[int, int]:
    microbatch = 256
    return microbatch, EFFECTIVE_BATCH // microbatch


def monarch_screen() -> list[ArchitectureConfig]:
    configs = [
        *[ArchitectureConfig("sequential", d) for d in (1, 2, 4, 8)],
        *[ArchitectureConfig("residual_one", d) for d in (1, 2, 4, 8)],
        *[ArchitectureConfig("residual_ffn", d, 1) for d in (1, 2, 4, 8)],
        *[ArchitectureConfig("residual_ffn", d, 4) for d in (1, 2, 4, 8)],
    ]
    for config in configs:
        config.validate()
    return configs
