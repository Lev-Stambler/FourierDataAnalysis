"""Locked configuration for the ExpV2-1 soft reset."""

from __future__ import annotations

from dataclasses import asdict, dataclass
from typing import Any


CONTEXT_LENGTH = 128
VOCAB_SIZE = 4_096
SYNTHETIC_VOCAB_SIZE = 128
VOCABULARY_WIDTH = 128
TINY_STORIES_ID = "roneneldan/TinyStories"
TINY_STORIES_REVISION = "f54c09fd23315a6f9c86f9dc80f725de7d8f9c64"
WANDB_PROJECT = "expv2-1-kronecker-basics"
SCHEMA = "expv2-1-config-v1"

KRONECKER_SHAPES = {
    "kron-r1-d66": (1, 66),
    "kron-r2-d33": (2, 33),
    "kron-r3-d22": (3, 22),
    "kron-r6-d11": (6, 11),
}
VARIANTS = (*KRONECKER_SHAPES, "dense", "transformer")


@dataclass(frozen=True)
class ModelConfig:
    variant: str
    context_length: int = CONTEXT_LENGTH
    vocab_size: int = VOCAB_SIZE
    vocabulary_width: int = VOCABULARY_WIDTH
    width: int = VOCABULARY_WIDTH
    depth: int = 0
    rank: int = 0
    dense_width: int = 14
    heads: int = 4
    ffn_width: int = 360

    def validate(self) -> None:
        if self.variant not in VARIANTS:
            raise ValueError(f"unknown ExpV2-1 variant: {self.variant}")
        if min(self.context_length, self.vocab_size, self.vocabulary_width) <= 0:
            raise ValueError("model dimensions must be positive")
        if self.variant in KRONECKER_SHAPES:
            expected = KRONECKER_SHAPES[self.variant]
            if (self.rank, self.depth) != expected:
                raise ValueError(
                    f"{self.variant} requires rank/depth {expected}, "
                    f"got {(self.rank, self.depth)}"
                )
            if self.width != self.vocabulary_width:
                raise ValueError("Kronecker hidden width must equal vocabulary width")
        elif self.variant == "dense":
            if self.depth != 1 or self.rank:
                raise ValueError("dense control requires depth one and rank zero")
            if self.dense_width <= 0:
                raise ValueError("dense internal width must be positive")
        else:
            if self.depth != 8 or self.rank:
                raise ValueError("Transformer control requires depth eight")
            if self.width != self.vocabulary_width:
                raise ValueError("Transformer width must equal vocabulary width")
            if self.width % self.heads:
                raise ValueError("Transformer width must divide evenly over heads")
            if (self.width // self.heads) % 2:
                raise ValueError("RoPE head width must be even")

    def as_dict(self) -> dict[str, Any]:
        return {"schema": SCHEMA, **asdict(self)}


def default_config(
    variant: str,
    *,
    vocab_size: int = VOCAB_SIZE,
    context_length: int = CONTEXT_LENGTH,
) -> ModelConfig:
    if variant in KRONECKER_SHAPES:
        rank, depth = KRONECKER_SHAPES[variant]
        value = ModelConfig(
            variant=variant,
            vocab_size=vocab_size,
            context_length=context_length,
            rank=rank,
            depth=depth,
        )
    elif variant == "dense":
        value = ModelConfig(
            variant=variant,
            vocab_size=vocab_size,
            context_length=context_length,
            depth=1,
            width=14,
        )
    elif variant == "transformer":
        value = ModelConfig(
            variant=variant,
            vocab_size=vocab_size,
            context_length=context_length,
            depth=8,
        )
    else:
        raise ValueError(f"unknown ExpV2-1 variant: {variant}")
    value.validate()
    return value
