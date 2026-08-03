"""Experiment V2-1: a basic whole-state Kronecker architecture test."""

from .config import ModelConfig, default_config
from .model import LanguageModel, build_model, model_inventory

__all__ = [
    "LanguageModel",
    "ModelConfig",
    "build_model",
    "default_config",
    "model_inventory",
]
