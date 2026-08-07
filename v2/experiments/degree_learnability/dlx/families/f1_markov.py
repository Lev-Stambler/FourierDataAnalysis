"""F1 — Markov-k family (PLAN §3): x_t = sum of previous k tokens mod q, with noise.

Local-lag special case of the subset-sum rule (contiguous lags {1..k}); degree <= k,
position-local. The "easy cell" F1-k1 is the copy rule.
"""

from __future__ import annotations

from .f2_subset_sum import F2SubsetSum


class F1Markov(F2SubsetSum):
    name = "F1_markov"

    def __init__(self, q: int, L: int, k: int, eta: float, enum_cap: int | None = None):
        assert 1 <= k <= L
        self.k = k
        super().__init__(q=q, L=L, lags=tuple(range(1, k + 1)), eta=eta, enum_cap=enum_cap)

    def params(self) -> dict:
        return {"q": self.q, "L": self.L, "k": self.k, "eta": self.eta}
