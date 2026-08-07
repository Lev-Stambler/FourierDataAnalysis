"""Family interface (PLAN §12.2).

A Family defines a distribution over token sequences in Z_q^L (one context window),
with exact conditionals where tractable. All methods are deterministic given an RNG.

Row-encoding convention used throughout dlx: a prefix x_0..x_t is encoded as
    row = x_t + q * x_{t-1} + q^2 * x_{t-2} + ...     (newest = lowest digit)
so `row % q` is the newest token.
"""

from __future__ import annotations

import abc
import hashlib
import inspect
import json

import numpy as np

#: Exact joint enumeration is used for conditional sampling while q^L <= ENUM_CAP.
ENUM_CAP_DEFAULT = 1 << 22


class Family(abc.ABC):
    name: str = "family"
    q: int
    L: int

    def __init__(self, enum_cap: int = ENUM_CAP_DEFAULT):
        self.enum_cap = enum_cap
        self._version = self._compute_version()
        self._joint_cache: np.ndarray | None = None

    # ------------------------------------------------------------------ identity
    @property
    def version(self) -> str:
        return self._version

    @abc.abstractmethod
    def params(self) -> dict:
        """Canonical JSON-able parameters (must fully determine the law)."""

    def _compute_version(self) -> str:
        h = hashlib.sha256()
        h.update(type(self).name.encode())
        h.update(inspect.getsource(type(self)).encode())
        h.update(json.dumps(self.params(), sort_keys=True, default=str).encode())
        return h.hexdigest()[:16]

    # ------------------------------------------------------------------ the law
    @abc.abstractmethod
    def probs_rows(self, t: int, rows: np.ndarray) -> np.ndarray:
        """Conditional token law at position t for prefixes encoded as `rows`.

        rows: (N,) int array encoding prefixes of length t+1 (position t included,
        newest = lowest digit). Returns (N, q) probability matrix.
        """

    @abc.abstractmethod
    def next_token_dist(self, context: np.ndarray) -> np.ndarray:
        """P(x_t | context) for a full length-L context (oldest first, positions
        t-L..t-1). Must agree with `probs_rows` in the stationary bulk."""

    def next_token_dist_batch(self, contexts: np.ndarray) -> np.ndarray:
        """Vectorized next_token_dist over (m, L) contexts. Default: loop."""
        return np.array([self.next_token_dist(c) for c in contexts])

    # ------------------------------------------------------------------ sampling
    @property
    def _max_lag(self) -> int:
        """Largest lag the rule reads; row state is kept modulo q^(_max_lag+1)."""
        return self.L

    def sample(self, n_tokens: int, rng: np.random.Generator) -> np.ndarray:
        """A token stream of length n_tokens drawn from the family law."""
        out = np.empty(n_tokens, dtype=np.int64)
        cap = self.q ** (self._max_lag + 1)
        row = 0
        for t in range(n_tokens):
            cands = np.array([v + self.q * row for v in range(self.q)], dtype=np.int64)
            P = self.probs_rows(t, cands)[0]
            x = int(rng.choice(self.q, p=P))
            out[t] = x
            row = (x + self.q * row) % cap
        return out

    def conditional_sample(self, mask: np.ndarray, values: np.ndarray,
                           rng: np.random.Generator) -> tuple[np.ndarray, bool]:
        """Draw from the family law given observed positions (mask-refill oracle).

        Returns (x, exact): exact=True when the draw is from the true conditional
        law (joint enumeration tractable), else a causal left-to-right refill
        approximation with exact=False (PLAN §11, refill-oracle caveat).
        """
        mask = np.asarray(mask, dtype=bool)
        values = np.asarray(values, dtype=np.int64) % self.q
        assert mask.shape == (self.L,) and values.shape == (self.L,)
        if self.q**self.L <= self.enum_cap:
            p = self.joint_table()
            return _exact_conditional_sample(p, self.q, self.L, mask, values, rng), True
        return self._causal_refill(mask, values, rng), False

    def _causal_refill(self, mask: np.ndarray, values: np.ndarray,
                       rng: np.random.Generator) -> np.ndarray:
        x = values.copy()
        cap = self.q ** (self._max_lag + 1)
        row = 0
        for t in range(self.L):
            cands = np.array([v + self.q * row for v in range(self.q)], dtype=np.int64)
            P = self.probs_rows(t, cands)[0]
            if mask[t]:
                xt = int(values[t])
            else:
                xt = int(rng.choice(self.q, p=P))
            x[t] = xt
            row = (xt + self.q * row) % cap
        return x

    # ------------------------------------------------------------------ joint law
    def joint_table(self) -> np.ndarray:
        """Full joint probability vector over all q^L sequences (row encoding)."""
        if self._joint_cache is not None:
            return self._joint_cache
        if self.q**self.L > self.enum_cap:
            raise ValueError(f"joint_table too large: q^L = {self.q**self.L} > cap {self.enum_cap}")
        p = np.ones(1, dtype=np.float64)
        for t in range(self.L):
            rows = np.arange(self.q ** (t + 1), dtype=np.int64)
            P = self.probs_rows(t, rows)
            p = p[rows // self.q] * P[np.arange(rows.size), rows % self.q]
        total = p.sum()
        assert abs(total - 1.0) < 1e-8, f"joint table not normalized: {total}"
        self._joint_cache = p
        return p

    # ------------------------------------------------------------------ profiles
    @abc.abstractmethod
    def entropy_rate(self) -> float:
        """Bayes floor in bits per token (closed form)."""

    @abc.abstractmethod
    def planted_profile(self) -> np.ndarray:
        """Level weights W^k (length L+1) of the next-token function, from construction.

        Describes the stationary bulk (boundary positions with missing lags may have
        lower effective degree).
        """


def _exact_conditional_sample(p: np.ndarray, q: int, L: int, mask: np.ndarray,
                              values: np.ndarray, rng: np.random.Generator) -> np.ndarray:
    """Sample exactly from p(x | x_mask = values) with row encoding oldest=highest digit.

    Row encoding: row = sum_j x_j q^{L-1-j}, so in the reshaped (q,)*L table
    axis a corresponds to position a.
    """
    table = p.reshape((q,) * L)
    remaining = list(range(L))  # positions (== original axes) still present
    for pos in sorted(np.where(mask)[0], reverse=True):
        ax = remaining.index(int(pos))
        table = np.take(table, int(values[pos]), axis=ax)
        remaining.pop(ax)
    flat = table.reshape(-1)
    total = flat.sum()
    if total <= 0.0:
        raise ValueError("conditioning event has zero probability under this family")
    idx = int(rng.choice(flat.size, p=flat / total))
    digits = np.array(np.unravel_index(idx, table.shape)) if table.shape else np.array([], dtype=int)
    x = values.copy()
    for k, pos in enumerate(remaining):
        if not mask[pos]:
            x[pos] = digits[k]
    return x
