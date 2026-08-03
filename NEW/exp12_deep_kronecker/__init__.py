"""Experiment 12: deep, shared-basis, content-routed Kronecker language models."""

from .model import (
    DeepKroneckerBlock,
    DeepLanguageModel,
    SharedCausalBasis,
    build_model,
    model_inventory,
)

__all__ = [
    "DeepKroneckerBlock",
    "DeepLanguageModel",
    "SharedCausalBasis",
    "build_model",
    "model_inventory",
]
