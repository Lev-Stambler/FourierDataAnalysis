"""Block-iid categorical tasks with analytically controlled Fourier spectra."""

from __future__ import annotations

import numpy as np


def planted_modular_blocks(
    *,
    n_blocks: int,
    ctx_len: int,
    q: int,
    support_lags: tuple[int, ...],
    signal_probability: float,
    seed: int,
) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """Return packed contexts and targets for a noisy modular categorical rule.

    Contexts are independent and uniform.  With the requested probability the
    target is the modular sum over ``support_lags``; otherwise it is uniform.
    Under the product-uniform input law this is pure degree ``len(support_lags)``
    in the categorical ANOVA filtration, apart from its constant component.
    """
    if n_blocks < 100 or ctx_len < 2 or q < 2:
        raise ValueError("n_blocks, ctx_len, and q are too small")
    support = tuple(sorted({int(lag) for lag in support_lags}))
    if not support or support[-1] > ctx_len or support[0] < 1:
        raise ValueError("support_lags must lie inside the context")
    if len(support) != len(support_lags):
        raise ValueError("support_lags must be distinct")
    if not 0.0 <= signal_probability <= 1.0:
        raise ValueError("signal_probability must be in [0, 1]")

    rng = np.random.default_rng(seed)
    contexts = rng.integers(0, q, size=(n_blocks, ctx_len), dtype=np.int64)
    targets = np.zeros(n_blocks, dtype=np.int64)
    for lag in support:
        targets += contexts[:, ctx_len - lag]
    targets %= q
    signal = rng.random(n_blocks) < signal_probability
    noise = rng.integers(0, q, size=n_blocks, dtype=np.int64)
    targets = np.where(signal, targets, noise)

    blocks = np.concatenate((contexts, targets[:, None]), axis=1)
    tokens = np.ascontiguousarray(blocks.reshape(-1))
    positions = np.arange(ctx_len, len(tokens), ctx_len + 1, dtype=np.int64)
    fold_ids = np.arange(n_blocks, dtype=np.int64) & 1
    return tokens, positions, fold_ids
