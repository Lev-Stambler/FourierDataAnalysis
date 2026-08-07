"""F3 — random Fourier-polynomial family, exponential form (PLAN §3; v2 amendment).

The next-token law is an exponential family over degree-d characters:
    score_y(x) = sum_a Re(w_a chi_a(x)) b_a(y),   b_a(y) = cos(2pi(y+phi_a)/q),
    P(y|x)     = softmax_y(beta * score_y(x)),
then eta-uniform noise.  Positivity holds for any beta (softmax), so signal
strength is tunable without the fragile additive-positivity bound.  Characters
a are drawn uniformly with categorical degree exactly d on random support.

Planted profile: defined by enumeration over the support subspace (exact ground
truth).  Construction additionally verifies spectral concentration: at moderate
beta the non-level-0 mass is dominated by degree d (checked in tests).
"""

from __future__ import annotations

import numpy as np

from .base import Family


class F3RandomPoly(Family):
    name = "F3_random_poly"

    def __init__(self, q: int, L: int, d: int, M: int, amp: float, beta: float,
                 eta: float, draw_seed: int, enum_cap: int | None = None):
        assert q >= 3 and L >= 2 and 1 <= d <= L and M >= 1
        assert 0.0 <= eta < 1.0 and amp > 0.0 and beta > 0.0
        self.q = q
        self.L = L
        self.d = d
        self.M = M
        self.amp = float(amp)
        self.beta = float(beta)
        self.eta = float(eta)
        self.draw_seed = int(draw_seed)
        super().__init__(enum_cap=enum_cap if enum_cap is not None else (1 << 22))

        rng = np.random.default_rng(self.draw_seed)
        self._pos: list[np.ndarray] = []
        self._freq: list[np.ndarray] = []
        self._w: list[complex] = []
        self._b: list[np.ndarray] = []
        self._build_terms(rng)

    def _build_terms(self, rng: np.random.Generator) -> None:
        """Draw self.M terms, all at degree self.d, with sum |w| = self.amp."""
        ys = np.arange(self.q)
        raw = rng.standard_normal(self.M) + 1j * rng.standard_normal(self.M)
        raw = raw / np.abs(raw).sum() * self.amp
        for i in range(self.M):
            pos = rng.choice(self.L, size=self.d, replace=False)
            freq = rng.integers(1, self.q, size=self.d)
            phi = rng.random() * self.q
            self._pos.append(np.sort(pos))
            self._freq.append(freq)
            self._w.append(complex(raw[i]))
            self._b.append(np.cos(2.0 * np.pi * (ys + phi) / self.q))

    # ------------------------------------------------------------------ identity
    def params(self) -> dict:
        return {"q": self.q, "L": self.L, "d": self.d, "M": self.M,
                "amp": self.amp, "beta": self.beta, "eta": self.eta,
                "draw_seed": self.draw_seed}

    @property
    def _max_lag(self) -> int:
        return self.L

    # ------------------------------------------------------------------ the law
    def probs_rows(self, t: int, rows: np.ndarray) -> np.ndarray:
        """Window law for exact conditioning: position t is predicted from the
        preceding window positions, zero-padded before index 0."""
        rows = np.asarray(rows, dtype=np.int64)
        N = rows.size
        ctxs = np.zeros((N, self.L), dtype=np.int64)
        for p in range(0, t):  # positions strictly before the predicted position
            k = p - t + self.L
            if 0 <= k < self.L:
                ctxs[:, k] = (rows // self.q ** (t - p)) % self.q
        return self.next_token_dist_batch(ctxs)

    def _scores(self, contexts: np.ndarray) -> np.ndarray:
        """score_y(x) for each context, shape (m, q)."""
        contexts = np.asarray(contexts, dtype=np.int64) % self.q
        m = contexts.shape[0]
        R = np.empty((m, self.M), dtype=np.float64)
        for i in range(self.M):
            phase = (contexts[:, self._pos[i]] @ self._freq[i]) % self.q
            R[:, i] = (self._w[i] * np.exp(2j * np.pi * phase / self.q)).real
        B = np.stack(self._b, axis=0)  # (M, q)
        return self.beta * (R @ B)

    def next_token_dist(self, context: np.ndarray) -> np.ndarray:
        return self.next_token_dist_batch(np.asarray(context)[None, :])[0]

    def next_token_dist_batch(self, contexts: np.ndarray) -> np.ndarray:
        logits = self._scores(contexts)
        logits = logits - logits.max(axis=1, keepdims=True)
        P = np.exp(logits)
        P = P / P.sum(axis=1, keepdims=True)
        P = (1.0 - self.eta) * P + self.eta / self.q
        return P

    def support_positions(self) -> list[int]:
        return sorted({int(p) for pos in self._pos for p in pos})

    # ------------------------------------------------------------------ sampling
    def sample(self, n_tokens: int, rng: np.random.Generator) -> np.ndarray:
        out = np.empty(n_tokens, dtype=np.int64)
        ctx = rng.integers(0, self.q, size=self.L)  # uniform burn-in context
        for t in range(n_tokens):
            P = self.next_token_dist(ctx)
            x = int(rng.choice(self.q, p=P))
            out[t] = x
            ctx = np.roll(ctx, -1)
            ctx[-1] = x
        return out

    def _causal_refill(self, mask: np.ndarray, values: np.ndarray,
                       rng: np.random.Generator) -> np.ndarray:
        x = values.copy()
        for t in range(self.L):
            if mask[t]:
                continue
            ctx = np.zeros(self.L, dtype=np.int64)
            start = t - self.L
            for j in range(self.L):
                src = start + j
                ctx[j] = x[src] if 0 <= src < self.L else 0
            P = self.next_token_dist(ctx)
            x[t] = int(rng.choice(self.q, p=P))
        return x

    # ------------------------------------------------------------------ profiles
    def entropy_rate(self) -> float:
        if not hasattr(self, "_h_cache"):
            rng = np.random.default_rng(self.draw_seed + 777)
            ctxs = rng.integers(0, self.q, size=(4096, self.L))
            P = self.next_token_dist_batch(ctxs)
            with np.errstate(divide="ignore"):
                H = -np.sum(P * np.log2(np.where(P > 0, P, 1.0)), axis=1)
            self._h_cache = float(H.mean())
        return self._h_cache

    def planted_profile(self) -> np.ndarray:
        """Exact level weights by enumeration over the support subspace."""
        if not hasattr(self, "_profile_cache"):
            from ..profiles.enumerate import enumerated_profile
            self._profile_cache = enumerated_profile(self)
        return self._profile_cache
