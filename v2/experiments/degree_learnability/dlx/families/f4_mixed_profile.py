"""F4 — mixed degree profile family: level weights W^k ~ r^k (PLAN §3).

Same exact Fourier-polynomial construction as F3, but terms are stratified
across degrees k = 1..K with per-term amplitudes a_k chosen so the planted
level masses satisfy W^k ~ r^k under the positivity budget sum |w| <= 0.9/q.
Knob: decay r (soft degree).
"""

from __future__ import annotations

import numpy as np

from .f3_random_poly import F3RandomPoly


class F4MixedProfile(F3RandomPoly):
    name = "F4_mixed_profile"
    def __init__(self, q: int, L: int, K: int, M: int, r: float, beta: float,
                 eta: float, draw_seed: int, enum_cap: int | None = None):
        assert 1 <= K <= L and 0.0 < r < 1.0 and M >= 1
        self.K = int(K)
        self.r = float(r)
        self.M_per = int(M)
        # per-degree amplitudes: a_k = sqrt(c r^k / M_per); c chosen so the total
        # shape budget sum_k M_per a_k = 1.0 (beta scales overall signal strength)
        budget = 1.0
        denom = sum(np.sqrt(self.M_per * r**k) for k in range(1, K + 1))
        c = (budget / denom) ** 2
        self._a = {k: float(np.sqrt(c * r**k / self.M_per)) for k in range(1, K + 1)}
        super().__init__(q=q, L=L, d=1, M=M * K, amp=budget, beta=beta, eta=eta,
                         draw_seed=draw_seed, enum_cap=enum_cap)

    def params(self) -> dict:
        return {"q": self.q, "L": self.L, "K": self.K, "M": self.M_per,
                "r": self.r, "beta": self.beta, "eta": self.eta,
                "draw_seed": self.draw_seed}

    def _build_terms(self, rng: np.random.Generator) -> None:
        ys = np.arange(self.q)
        self._pos, self._freq, self._w, self._b = [], [], [], []
        for k in range(1, self.K + 1):
            for _ in range(self.M_per):
                pos = rng.choice(self.L, size=k, replace=False)
                freq = rng.integers(1, self.q, size=k)
                phi = rng.random() * self.q
                wmag = self._a[k]
                phase_w = rng.standard_normal() + 1j * rng.standard_normal()
                phase_w = phase_w / abs(phase_w) * wmag
                self._pos.append(np.sort(pos))
                self._freq.append(freq)
                self._w.append(complex(phase_w))
                self._b.append(np.cos(2.0 * np.pi * (ys + phi) / self.q))
        # exponential form guarantees positivity for any amplitudes; the shape
        # budget (sum_k M_per a_k = 1.0) is documented in __init__

    def planted_profile(self) -> np.ndarray:
        return super().planted_profile()  # enumeration-defined (v2 amendment)
