"""Fresh token streams (PLAN §6: no epoch reuse; statistical regime).

Each batch draws brand-new tokens from the family sampler; windows are
non-overlapping so every token is used once.
"""

from __future__ import annotations

import numpy as np

from ..families.base import Family


class FamilyStream:
    def __init__(self, family: Family, rng: np.random.Generator, ctx_len: int,
                 chunk_tokens: int = 1 << 18):
        self.family = family
        self.rng = rng
        self.ctx_len = ctx_len
        self.chunk_tokens = max(chunk_tokens, 4 * ctx_len)
        self._buf = np.empty(0, dtype=np.int64)

    def _refill(self, need: int):
        while len(self._buf) < need:
            self._buf = np.concatenate([self._buf, self.family.sample(self.chunk_tokens, self.rng)])

    def next_batch(self, batch: int) -> tuple[np.ndarray, np.ndarray]:
        """(inputs, targets), each (batch, ctx_len); targets = inputs shifted by one."""
        need = batch * (self.ctx_len + 1)
        self._refill(need)
        take = self._buf[:need]
        self._buf = self._buf[need:]
        take = take.reshape(batch, self.ctx_len + 1)
        return take[:, :-1].copy(), take[:, 1:].copy()
