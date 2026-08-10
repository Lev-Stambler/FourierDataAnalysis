"""Deterministic block splits and random contiguous windows for fixed corpora."""

from __future__ import annotations

import numpy as np


class RandomWindowCorpus:
    """Sample intact next-token windows from disjoint, shuffled corpus blocks."""

    SPLITS = ("train", "profile", "validation")

    def __init__(
        self,
        tokens: np.ndarray,
        *,
        ctx_len: int,
        block_size: int,
        split_seed: int,
        split_fractions: tuple[float, float, float] = (0.75, 0.125, 0.125),
    ) -> None:
        self.tokens = np.asarray(tokens, dtype=np.int64)
        self.ctx_len = int(ctx_len)
        self.block_size = int(block_size)
        if self.tokens.ndim != 1 or len(self.tokens) < 4 * self.block_size:
            raise ValueError(
                "tokens must be a sufficiently long one-dimensional stream"
            )
        if self.ctx_len < 1 or self.block_size < 4 * (self.ctx_len + 1):
            raise ValueError("block_size must be at least four complete windows")
        fractions = np.asarray(split_fractions, dtype=float)
        if np.any(fractions <= 0.0) or not np.isclose(fractions.sum(), 1.0):
            raise ValueError("split fractions must be positive and sum to one")

        blocks = []
        for start in range(0, len(self.tokens), self.block_size):
            stop = min(start + self.block_size, len(self.tokens))
            if stop - start >= self.ctx_len + 1:
                blocks.append((start, stop))
        order = np.random.default_rng(split_seed).permutation(len(blocks))
        counts = np.floor(fractions * len(blocks)).astype(int)
        counts[0] += len(blocks) - int(counts.sum())
        if np.any(counts < 1):
            raise ValueError("corpus has too few blocks for the requested split")
        boundaries = np.cumsum(np.concatenate(([0], counts)))
        self.block_ids: dict[str, np.ndarray] = {}
        self.starts: dict[str, np.ndarray] = {}
        for index, name in enumerate(self.SPLITS):
            ids = np.sort(order[boundaries[index] : boundaries[index + 1]])
            self.block_ids[name] = ids
            self.starts[name] = np.concatenate(
                [
                    np.arange(
                        blocks[block][0],
                        blocks[block][1] - self.ctx_len,
                        dtype=np.int64,
                    )
                    for block in ids
                ]
            )

    def sample_starts(
        self,
        split: str,
        n: int,
        rng: np.random.Generator,
        *,
        replace: bool,
    ) -> np.ndarray:
        if split not in self.starts:
            raise ValueError(f"unknown split: {split}")
        available = self.starts[split]
        if n < 1 or (not replace and n > len(available)):
            raise ValueError("invalid number of requested windows")
        return rng.choice(available, size=n, replace=replace)

    def batch(self, starts: np.ndarray) -> tuple[np.ndarray, np.ndarray]:
        positions = np.asarray(starts, dtype=np.int64)[:, None]
        offsets = np.arange(self.ctx_len, dtype=np.int64)[None, :]
        x = self.tokens[positions + offsets]
        y = self.tokens[positions + offsets + 1]
        return np.asarray(x), np.asarray(y)

    def summary(self) -> dict:
        return {
            "block_size": self.block_size,
            "ctx_len": self.ctx_len,
            "blocks": {name: len(self.block_ids[name]) for name in self.SPLITS},
            "eligible_windows": {name: len(self.starts[name]) for name in self.SPLITS},
        }
