"""Planted profiles (PLAN §4.1): exact level weights from construction."""

from __future__ import annotations

import numpy as np

from ..families.base import Family


def planted_profile(family: Family) -> np.ndarray:
    """Level weights W^k (length L+1) as derived from the family construction."""
    W = np.asarray(family.planted_profile(), dtype=np.float64)
    assert W.shape == (family.L + 1,)
    assert W.min() >= -1e-12, "planted weights must be nonnegative"
    return W
