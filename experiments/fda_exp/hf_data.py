"""Real next-token data from the TinyStories dataset (roneneldan/TinyStories).

We pull the small validation split (~10 MB parquet) directly, tokenize to a
top-``vocab_size`` vocabulary (rarer tokens -> ``<unk>``), and slide a
``window``-token context to predict the next token.  Each token is encoded in
``bpt = ceil(log2 vocab_size)`` bits, so a context is a point in ``{-1,1}^n`` with
``n = window * bpt`` --- exactly the harness's Boolean-cube setting.

    from fda_exp.hf_data import tinystories_next_token
    X, y, vocab, window, bpt = tinystories_next_token(window=3, vocab_size=64)
"""

from __future__ import annotations

import os
import re
import urllib.request
from collections import Counter

import numpy as np

_VALID_URL = (
    "https://huggingface.co/datasets/roneneldan/TinyStories/resolve/"
    "refs%2Fconvert%2Fparquet/default/validation/0000.parquet"
)
_CACHE = os.path.join(os.path.dirname(__file__), "..", "data", "tinystories_valid.parquet")

_TOKEN_RE = re.compile(r"[a-z]+|[.!?,;]")


def _download_validation() -> str:
    path = os.path.abspath(_CACHE)
    if not os.path.exists(path):
        os.makedirs(os.path.dirname(path), exist_ok=True)
        print(f"[hf_data] downloading TinyStories validation split -> {path}")
        urllib.request.urlretrieve(_VALID_URL, path)
    return path


def load_texts(n_stories: int | None = None) -> list[str]:
    import pyarrow.parquet as pq

    table = pq.read_table(_download_validation(), columns=["text"])
    texts = table.column("text").to_pylist()
    return texts if n_stories is None else texts[:n_stories]


def tokenize(text: str) -> list[str]:
    return _TOKEN_RE.findall(text.lower())


def build_vocab(streams, vocab_size: int):
    counts = Counter(t for s in streams for t in s)
    top = [w for w, _ in counts.most_common(vocab_size - 1)]
    vocab = ["<unk>"] + top                       # id 0 = <unk>
    return vocab, {w: i for i, w in enumerate(vocab)}


def _token_bits(t: int, bpt: int) -> np.ndarray:
    return np.array([1 - 2 * ((t >> j) & 1) for j in range(bpt)], dtype=np.int64)


def decode_context(x: np.ndarray, window: int, bpt: int, vocab) -> str:
    toks = []
    for w in range(window):
        bits = x[w * bpt:(w + 1) * bpt]
        tid = sum(((1 - int(v)) // 2) << j for j, v in enumerate(bits))
        toks.append(vocab[tid] if tid < len(vocab) else "?")
    return " ".join(toks)


def tinystories_next_token(window: int = 3, vocab_size: int = 64,
                           n_stories: int = 3000, max_pairs: int = 300_000,
                           seed: int = 0):
    """Return (X in {-1,1}^n contexts, y next-token ids, vocab, window, bpt)."""
    texts = load_texts(n_stories)
    streams = [tokenize(t) for t in texts]
    vocab, idx = build_vocab(streams, vocab_size)
    bpt = int(np.ceil(np.log2(vocab_size)))

    rows, nxt = [], []
    for s in streams:
        ids = [idx.get(t, 0) for t in s]
        for i in range(len(ids) - window):
            rows.append(np.concatenate([_token_bits(t, bpt) for t in ids[i:i + window]]))
            nxt.append(ids[i + window])
        if len(rows) >= max_pairs:
            break
    X = np.array(rows[:max_pairs], dtype=np.int64)
    y = np.array(nxt[:max_pairs], dtype=np.int64)
    print(f"[hf_data] TinyStories: {len(texts)} stories -> {len(X)} (context,next) pairs; "
          f"vocab={vocab_size} (bpt={bpt}), window={window}, n={window * bpt}")
    return X, y, vocab, window, bpt
