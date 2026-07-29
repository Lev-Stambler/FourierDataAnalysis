"""Tensor-native small-width Kronecker distillation from pinned Qwen3.5."""

from .config import Architecture, Cell, depth_cells
from .kronecker import KroneckerSumLinear
from .model import TensorKroneckerStudent

__all__ = [
    "Architecture",
    "Cell",
    "KroneckerSumLinear",
    "TensorKroneckerStudent",
    "depth_cells",
]
