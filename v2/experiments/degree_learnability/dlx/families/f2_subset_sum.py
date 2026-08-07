"""F2 — mod-q subset-sum family (PLAN §3).

Rule (bulk):  x_t = (sum_{i in S} x_{t-i}) mod q, corrupted by noise eta:
    with probability eta the token is replaced by a uniform draw.
Next-token law:  (1-eta) * delta_{s(x)} + eta * Uniform(q).

- Non-local when lags are spread (e.g. {1, L//4, L//2}): separates degree from
  Markov span.
- Categorical degree of the deterministic part = s = |S| (one degree-s character
  line in the one-hot Fourier expansion).
- Stationary one-token marginals are uniform.

Planted level weights of the one-hot next-token function f(x) = P(x_t | context),
under the uniform context measure (stationary bulk):
    W^0 = 1/q                     (mean of f is the uniform vector)
    W^s = (q-1) (1-eta)^2 / q     (the q-1 nonzero characters on the sum line)
    all other W^k = 0.
Check: W^0 + W^s = E||f||^2 at eta=0; both sides continuous in eta.
"""

from __future__ import annotations

import numpy as np

from .base import Family


class F2SubsetSum(Family):
    name = "F2_subset_sum"

    def __init__(self, q: int, L: int, lags: tuple[int, ...], eta: float,
                 enum_cap: int | None = None):
        assert q >= 2 and L >= 2
        assert all(1 <= i <= L for i in lags), "lags must lie in 1..L"
        assert len(set(lags)) == len(lags)
        assert 0.0 <= eta < 1.0
        self.q = q
        self.L = L
        self.lags = tuple(sorted(lags))
        self.eta = float(eta)
        super().__init__(enum_cap=enum_cap if enum_cap is not None else (1 << 22))

    # ------------------------------------------------------------------ identity
    @property
    def _max_lag(self) -> int:
        return max(self.lags)

    def params(self) -> dict:
        return {"q": self.q, "L": self.L, "lags": list(self.lags), "eta": self.eta}

    # ------------------------------------------------------------------ the law
    def probs_rows(self, t: int, rows: np.ndarray) -> np.ndarray:
        rows = np.asarray(rows, dtype=np.int64)
        active = [i for i in self.lags if i <= t]
        if not active:
            # No lag available yet (boundary): draw uniform.
            return np.full((rows.size, self.q), 1.0 / self.q)
        s = np.zeros(rows.size, dtype=np.int64)
        for i in active:
            # position t-i has digit weight q^i in the row encoding
            s = (s + (rows // self.q**i) % self.q) % self.q
        P = np.full((rows.size, self.q), self.eta / self.q)
        P[np.arange(rows.size), s] += 1.0 - self.eta
        return P

    def next_token_dist(self, context: np.ndarray) -> np.ndarray:
        context = np.asarray(context, dtype=np.int64) % self.q
        assert context.shape == (self.L,), "F2 context must have length L"
        # context[k] is position t-L+k; lag i refers to position t-i = index L-i.
        s = int(sum(int(context[self.L - i]) for i in self.lags)) % self.q
        P = np.full(self.q, self.eta / self.q)
        P[s] += 1.0 - self.eta
        return P

    def support_positions(self) -> list[int]:
        """Context indices (0 = oldest) the next-token function depends on."""
        return [self.L - i for i in self.lags]

    def next_token_dist_batch(self, contexts: np.ndarray) -> np.ndarray:
        """Fast path: compute the lag sum directly (no row encoding, so this
        works at protocol sizes where q^max_lag overflows int64)."""
        contexts = np.asarray(contexts, dtype=np.int64) % self.q
        m = contexts.shape[0]
        s = np.zeros(m, dtype=np.int64)
        for i in self.lags:
            s = (s + contexts[:, self.L - i]) % self.q
        P = np.full((m, self.q), self.eta / self.q)
        P[np.arange(m), s] += 1.0 - self.eta
        return P

    def sample(self, n_tokens: int, rng: np.random.Generator) -> np.ndarray:
        """Direct sampler (row encoding would overflow for large lags at big q)."""
        out = np.empty(n_tokens, dtype=np.int64)
        for t in range(n_tokens):
            active = [i for i in self.lags if i <= t]
            if not active:
                out[t] = rng.integers(self.q)
                continue
            s = int(sum(int(out[t - i]) for i in active)) % self.q
            if rng.random() < self.eta:
                out[t] = rng.integers(self.q)
            else:
                out[t] = s
        return out

    def _causal_refill(self, mask: np.ndarray, values: np.ndarray,
                       rng: np.random.Generator) -> np.ndarray:
        x = values.copy()
        for t in range(self.L):
            if mask[t]:
                continue
            active = [i for i in self.lags if i <= t]
            if not active:
                x[t] = rng.integers(self.q)
                continue
            s = int(sum(int(x[t - i]) for i in active)) % self.q
            if rng.random() < self.eta:
                x[t] = rng.integers(self.q)
            else:
                x[t] = s
        return x

    # ------------------------------------------------------------------ profiles
    def entropy_rate(self) -> float:
        """Entropy in bits of (1-eta) delta + eta Uniform(q)."""
        q, eta = self.q, self.eta
        p0 = (1.0 - eta) + eta / q
        H = -p0 * np.log2(p0)
        if eta > 0.0:
            p1 = eta / q
            H -= (q - 1) * p1 * np.log2(p1)
        return float(H)

    def planted_profile(self) -> np.ndarray:
        W = np.zeros(self.L + 1, dtype=np.float64)
        W[0] = 1.0 / self.q
        s = len(self.lags)
        W[s] = (self.q - 1) * (1.0 - self.eta) ** 2 / self.q
        return W
