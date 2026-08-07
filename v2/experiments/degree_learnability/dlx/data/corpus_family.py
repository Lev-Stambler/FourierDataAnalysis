"""CorpusFamily: fixed-token-stream adapter exposing the Family interface so the
standard train_run harness trains on real corpora (R1 ladder).

Split discipline: the token array is divided into a train region (sequential
no-reuse reads) and a held-out val tail. Training streams read the train region;
validation reads the val tail via val_view().
"""

from __future__ import annotations

import numpy as np

from ..families.base import Family


class CorpusFamily(Family):
    name = "corpus"

    def __init__(self, tokens: np.ndarray, q: int, L: int, name: str,
                 floor_bits: float, val_fraction: float = 0.02,
                 split: str = "train", pointer: int = 0, cyclic: bool = False,
                 shuffle_seed: int = 0, data_version: str | None = None):
        assert 0.0 < val_fraction < 0.5
        self.tokens = np.asarray(tokens, dtype=np.int64)
        self.q = q
        self.L = L
        self.name = name
        self._floor = float(floor_bits)
        self._val_fraction = val_fraction
        self._split = split
        self._cyclic = cyclic
        self._data_version = data_version
        n = len(self.tokens)
        self._split_point = int(n * (1.0 - val_fraction))
        lo, hi = (0, self._split_point) if split == "train" else (self._split_point, n)
        self._lo, self._hi = lo, hi
        self._ptr = lo + pointer
        self._epochs = 0
        self._shuffle_rng = np.random.default_rng(shuffle_seed)
        if cyclic:
            # A corpus is a sequence, not an exchangeable table. Vary the
            # circular starting point across seeds while preserving adjacency.
            span = hi - lo
            self._ptr = lo + int(self._shuffle_rng.integers(span))
        super().__init__(enum_cap=1)

    # ------------------------------------------------------------- Family iface
    def params(self) -> dict:
        return {"name": self.name, "q": self.q, "L": self.L, "split": self._split,
                "n_tokens": len(self.tokens), "data_version": self._data_version}

    def sample(self, n_tokens: int, rng: np.random.Generator) -> np.ndarray:
        """Sequential no-reuse read from this split's region; in cyclic mode
        (fixed datasets, amendment v1.2 note) wraps without permuting tokens."""
        if self._cyclic:
            out = np.empty(n_tokens, dtype=np.int64)
            got = 0
            while got < n_tokens:
                if self._ptr >= self._hi:
                    self._ptr = self._lo
                    self._epochs += 1
                take = min(n_tokens - got, self._hi - self._ptr)
                out[got:got + take] = self.tokens[self._ptr:self._ptr + take]
                self._ptr += take
                got += take
            return out
        avail = self._hi - self._ptr
        if avail <= self.L + 1:
            raise RuntimeError(
                f"corpus {self.name} split={self._split} exhausted ({self._ptr-self._lo} tokens read)")
        take = min(n_tokens, avail)
        out = self.tokens[self._ptr : self._ptr + take]
        self._ptr += take
        if take < n_tokens:
            out = np.concatenate([out, np.full(n_tokens - take, out[-1])])
        return out

    def val_view(self) -> CorpusFamily:
        return CorpusFamily(self.tokens, self.q, self.L, self.name, self._floor,
                            self._val_fraction, split="val", cyclic=False,
                            data_version=self._data_version)

    def entropy_rate(self) -> float:
        return self._floor

    # next-token law of a raw corpus is not a closed form; the harness never
    # calls these for corpus training.
    def probs_rows(self, t, rows):
        raise NotImplementedError("corpus family: no closed-form conditional law")

    def next_token_dist(self, context):
        raise NotImplementedError("corpus family: no closed-form conditional law")

    def planted_profile(self) -> np.ndarray:
        raise NotImplementedError("corpus family: profile is measured, not planted")
