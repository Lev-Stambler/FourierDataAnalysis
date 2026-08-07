"""F5 — control families (PLAN §3).

F5IID:      uniform iid tokens; degree-0 next-token law; max-difficulty control
            for learners, min-difficulty for degree.
F5MaxSum:   mod-q sum over ALL L positions (categorical degree L) — parity-like
            maximum degree; expected censored-hard control.
"""

from __future__ import annotations

import numpy as np

from .base import Family
from .f2_subset_sum import F2SubsetSum


class F5IID(Family):
    name = "F5_iid"

    def __init__(self, q: int, L: int, enum_cap: int | None = None):
        self.q = q
        self.L = L
        super().__init__(enum_cap=enum_cap if enum_cap is not None else (1 << 22))

    def params(self) -> dict:
        return {"q": self.q, "L": self.L}

    @property
    def _max_lag(self) -> int:
        return 1

    def probs_rows(self, t: int, rows: np.ndarray) -> np.ndarray:
        return np.full((np.asarray(rows).size, self.q), 1.0 / self.q)

    def next_token_dist(self, context: np.ndarray) -> np.ndarray:
        return np.full(self.q, 1.0 / self.q)

    def next_token_dist_batch(self, contexts: np.ndarray) -> np.ndarray:
        return np.full((np.asarray(contexts).shape[0], self.q), 1.0 / self.q)

    def support_positions(self) -> list[int]:
        return []

    def sample(self, n_tokens: int, rng: np.random.Generator) -> np.ndarray:
        return rng.integers(0, self.q, size=n_tokens)

    def entropy_rate(self) -> float:
        return float(np.log2(self.q))

    def planted_profile(self) -> np.ndarray:
        W = np.zeros(self.L + 1, dtype=np.float64)
        W[0] = 1.0 / self.q
        return W


class F5MaxSum(F2SubsetSum):
    name = "F5_max_sum"

    def __init__(self, q: int, L: int, eta: float, enum_cap: int | None = None):
        super().__init__(q=q, L=L, lags=tuple(range(1, L + 1)), eta=eta,
                         enum_cap=enum_cap)

    def params(self) -> dict:
        return {"q": self.q, "L": self.L, "eta": self.eta}
