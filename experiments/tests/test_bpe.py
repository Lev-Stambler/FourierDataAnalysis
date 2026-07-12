"""Tests for the self-contained byte-level BPE: power-of-2 vocab, ids within range, byte fallback
(no <unk>), encode/decode roundtrip, exact vocab on a rich corpus, and caching."""
import random

import numpy as np

from fda_exp import bpe

_CORPUS = [
    "Once upon a time there was a little cat. The cat liked to play.",
    "The dog and the cat were friends. They played in the park every day.",
    "A little girl found a small red ball and gave it to the happy dog.",
] * 40


def _rich_corpus():
    """A corpus with enough distinct byte-pairs to reach V-256 merges at V=512."""
    rng = random.Random(0)
    syll = ["ba", "ke", "lo", "mi", "tu", "ra", "ne", "si", "do", "fu",
            "pa", "le", "ru", "ga", "zo", "vi", "ta", "me", "ni", "ho"]
    words = ["".join(rng.choice(syll) for _ in range(rng.randint(2, 5))) for _ in range(800)]
    return [" ".join(rng.choice(words) for _ in range(14)) for _ in range(500)]


def test_ids_within_vocab_and_power_of_two():
    """The load-bearing invariant for hadamard_gl: V is a power of 2 and every id is in 0..V-1
    (<= V-256 merges; density holds only when the corpus is rich enough)."""
    for V in (256, 512, 1024):
        merges = bpe.train_bpe(_CORPUS, V)
        assert (V & (V - 1)) == 0
        assert len(merges) <= V - 256
        ids = bpe.encode(" ".join(_CORPUS), merges)
        assert ids and min(ids) >= 0 and max(ids) < V              # never out of range


def test_exact_vocab_on_rich_corpus():
    merges = bpe.train_bpe(_rich_corpus(), 512)
    assert len(merges) == 512 - 256                                # exact 2^k when pairs suffice
    assert bpe.vocab_size(merges) == 512


def test_v256_is_raw_bytes():
    merges = bpe.train_bpe(_CORPUS, 256)
    assert merges == []
    ids = bpe.encode("hello", merges)
    assert ids == list("hello".encode("utf-8"))                    # no merges => pure byte ids


def test_roundtrip_and_no_unk():
    """decode(encode(t)) == t.lower(); arbitrary/unseen chars still encode (byte fallback, no <unk>)."""
    merges = bpe.train_bpe(_CORPUS, 512)
    for text in ["The Cat Sat On The Mat!", "weird ~chars~ 123 \n newlines\t tabs", "café naïve"]:
        assert bpe.decode(bpe.encode(text, merges), merges) == text.lower()
        ids = bpe.encode(text, merges)
        assert all(0 <= i < 512 for i in ids)                      # never <unk> / out of range


def test_encode_iter_matches_encode():
    merges = bpe.train_bpe(_CORPUS, 512)
    per_text = list(bpe.encode_iter(_CORPUS, merges))
    for t, ids in zip(_CORPUS, per_text):
        assert ids == bpe.encode(t, merges)


def test_train_or_load_caches(tmp_path, monkeypatch):
    monkeypatch.setenv("FDA_DATA_DIR", str(tmp_path))
    m1 = bpe.train_or_load(512, n_stories=3, texts=_CORPUS)        # trains + caches
    assert (tmp_path / "tinystories_bpe_v512_n3.json").exists()
    m2 = bpe.train_or_load(512, n_stories=3, texts=_CORPUS)        # loads from cache
    assert m1 == m2


def test_tinystories_bpe_next_shapes(tmp_path, monkeypatch):
    monkeypatch.setenv("FDA_DATA_DIR", str(tmp_path))
    from fda_exp import hf_data
    corpus = _rich_corpus()
    monkeypatch.setattr(hf_data, "load_texts",
                        lambda n=None, split="valid", shards=None: corpus[:n] if n else corpus)
    C, y, V, w = hf_data.tinystories_bpe_next(window=4, vocab_size=512, n_stories=200,
                                              max_pairs=5000, bpe_train_stories=200)
    assert V == 512 and w == 4
    assert C.shape[1] == 4 and len(C) == len(y) and 0 < len(C) <= 5000
    assert C.min() >= 0 and C.max() < 512 and y.min() >= 0 and y.max() < 512    # no <unk>, in range
    import pytest
    with pytest.raises(ValueError):
        hf_data.tinystories_bpe_next(vocab_size=500)               # not a power of 2


def test_collapse_contexts():
    from fda_exp.hf_data import collapse_contexts
    C = np.array([[1, 2], [1, 2], [3, 4], [1, 2]], dtype=np.int64)
    y = np.array([0, 1, 0, 0], dtype=np.int64)
    Cd, counts, n, m = collapse_contexts(C, y, W=2)
    assert m == 4 and len(Cd) == 2                                 # 2 distinct contexts
    row = {tuple(c): (list(counts[i]), int(n[i])) for i, c in enumerate(Cd)}
    assert row[(1, 2)] == ([2, 1], 3)                              # y=[0,1,0] over the 3 occurrences
    assert row[(3, 4)] == ([1, 0], 1)
