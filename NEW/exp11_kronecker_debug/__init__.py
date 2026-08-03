"""Debug-first order-three causal Kronecker language-model experiments."""

from .model import (
    BalancedOrder2Block,
    CanonicalOrder3Block,
    Exp10ReplicaBlock,
    LanguageModel,
    ModelConfig,
    TransformerBlock,
    build_model,
    model_inventory,
)

__all__ = [
    "BalancedOrder2Block",
    "CanonicalOrder3Block",
    "Exp10ReplicaBlock",
    "LanguageModel",
    "ModelConfig",
    "TransformerBlock",
    "build_model",
    "model_inventory",
]
