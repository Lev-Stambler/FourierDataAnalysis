from __future__ import annotations

import numpy as np

from dlx.data.random_windows import RandomWindowCorpus


def test_random_window_splits_are_reproducible_disjoint_and_contiguous() -> None:
    tokens = np.arange(10_000, dtype=np.int64) % 256
    first = RandomWindowCorpus(tokens, ctx_len=16, block_size=256, split_seed=28)
    second = RandomWindowCorpus(tokens, ctx_len=16, block_size=256, split_seed=28)
    for split in first.SPLITS:
        assert np.array_equal(first.starts[split], second.starts[split])
    block_sets = [set(first.block_ids[name]) for name in first.SPLITS]
    assert not (block_sets[0] & block_sets[1])
    assert not (block_sets[0] & block_sets[2])
    assert not (block_sets[1] & block_sets[2])
    assert all(stop - start <= 256 for start, stop in first.block_ranges["profile"])

    starts = first.sample_starts("train", 32, np.random.default_rng(9), replace=True)
    x, y = first.batch(starts)
    assert x.shape == y.shape == (32, 16)
    assert np.array_equal(x[:, 1:], y[:, :-1])
    assert np.all((x[:, 1:].astype(int) - x[:, :-1].astype(int)) % 256 == 1)


def test_random_window_profile_sampling_without_replacement() -> None:
    corpus = RandomWindowCorpus(
        np.arange(20_000, dtype=np.int64) % 251,
        ctx_len=64,
        block_size=512,
        split_seed=3,
    )
    starts = corpus.sample_starts(
        "profile", 500, np.random.default_rng(4), replace=False
    )
    assert len(np.unique(starts)) == 500
