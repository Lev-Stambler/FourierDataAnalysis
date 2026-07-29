"""Full-width Qwen distillation with dense, Monarch, BTT, and Kronecker maps."""

from .btt import BTTLinear
from .config import ArchitectureConfig, TrialConfig
from .kronecker import KroneckerLinear
from .model import FullWidthStudent
from .monarch import MonarchLinear

__all__ = [
    "ArchitectureConfig",
    "BTTLinear",
    "FullWidthStudent",
    "KroneckerLinear",
    "MonarchLinear",
    "TrialConfig",
]
