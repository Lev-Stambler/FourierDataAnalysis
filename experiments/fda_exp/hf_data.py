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

_BASE = ("https://huggingface.co/datasets/roneneldan/TinyStories/resolve/"
         "refs%2Fconvert%2Fparquet/default/")
_VALID_URL = _BASE + "validation/0000.parquet"
_DATA_DIR = os.environ.get("FDA_DATA_DIR") or os.path.join(os.path.dirname(__file__), "..", "data")
_CACHE = os.path.join(_DATA_DIR, "tinystories_valid.parquet")
_TRAIN_SHARDS = 4                          # train/0000.parquet .. train/0003.parquet (~991 MB, 2.12M stories)

_TOKEN_RE = re.compile(r"[a-z]+|[.!?,;]")


def _download_parquet(rel: str) -> str:
    """Download+cache a TinyStories parquet, e.g. 'validation/0000.parquet' or 'train/0002.parquet'."""
    fname = "tinystories_valid.parquet" if rel == "validation/0000.parquet" \
        else "tinystories_" + rel.replace("/", "_")                   # keep the legacy validation cache name
    path = os.path.abspath(os.path.join(_DATA_DIR, fname))
    if not os.path.exists(path):
        os.makedirs(os.path.dirname(path), exist_ok=True)
        print(f"[hf_data] downloading TinyStories {rel} -> {path}", flush=True)
        urllib.request.urlretrieve(_BASE + rel, path)
    return path


def _download_validation() -> str:
    return _download_parquet("validation/0000.parquet")


def load_texts(n_stories: int | None = None, split: str = "valid", shards: int | None = None) -> list[str]:
    """TinyStories story texts.  split: 'valid' (~22k), 'train' (2.12M across 4 shards, ~991 MB), or
    'all' (validation + train).  shards: how many train shards to read (1..4; None = all).  n_stories
    truncates the concatenated total (and short-circuits shard downloads once reached)."""
    import pyarrow.parquet as pq
    if split == "valid":
        rels = ["validation/0000.parquet"]
    elif split in ("train", "all"):
        ns = _TRAIN_SHARDS if shards is None else max(1, min(shards, _TRAIN_SHARDS))
        rels = (["validation/0000.parquet"] if split == "all" else []) \
            + [f"train/{i:04d}.parquet" for i in range(ns)]
    else:
        raise ValueError(f"split must be 'valid'|'train'|'all', got {split!r}")
    texts: list[str] = []
    for rel in rels:
        texts.extend(pq.read_table(_download_parquet(rel), columns=["text"]).column("text").to_pylist())
        if n_stories is not None and len(texts) >= n_stories:
            break
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


def tinystories_char_next(window: int = 8, n_stories: int = 60000, max_pairs: int = 1_500_000,
                          seed: int = 0, alphabet: str | None = None):
    """CHARACTER-level (context, next-char) pairs.  Lowercase the text; the vocabulary IS the
    alphabet (default a-z + space), so there is NO <unk> bucket and every symbol carries signal.
    Characters outside the alphabet are dropped.  Returns (C (m,window) char-ids, y next-char-ids,
    V=len(alphabet), window).  V^w is un-enumerable for w>~4 yet contexts repeat heavily."""
    if alphabet is None:
        alphabet = "abcdefghijklmnopqrstuvwxyz "
    cmap = {c: i for i, c in enumerate(alphabet)}
    V = len(alphabet)
    texts = load_texts(n_stories)
    rows, nxt = [], []
    for txt in texts:
        ids = [cmap[c] for c in txt.lower() if c in cmap]
        for i in range(len(ids) - window):
            rows.append(ids[i:i + window]); nxt.append(ids[i + window])
        if len(rows) >= max_pairs:
            break
    C = np.array(rows[:max_pairs], dtype=np.int64)
    y = np.array(nxt[:max_pairs], dtype=np.int64)
    print(f"[hf_data] TinyStories CHAR: {len(texts)} stories -> {len(C)} (context,next-char) pairs; "
          f"V={V} (alphabet '{alphabet}'), window={window}")
    return C, y, V, window


def tinystories_bpe_next(window=5, vocab_size=512, n_stories=60000, max_pairs=1_500_000,
                         seed=0, split="valid", shards=None, bpe_train_stories=20000):
    """(context, next-token) pairs over a REAL byte-level BPE tokenizer with an exact power-of-2 vocab.

    Mirrors `tinystories_char_next`: returns (C (m,window) token-ids 0..V-1, y next-token ids, V, window).
    Byte-fallback BPE => NO <unk>.  The tokenizer is trained once on a `bpe_train_stories` validation
    sample (stable merges) and reused across data scales; `split`/`shards` choose how much text to encode
    (use split='train' + shards to grow m and drive C_D = V^w/m -> O(1))."""
    from . import bpe
    k = int(vocab_size).bit_length() - 1
    if vocab_size != (1 << k):
        raise ValueError(f"vocab_size must be a power of 2, got {vocab_size}")
    n = window * k
    if n > 62:
        raise ValueError(f"window*log2(vocab_size) = {n} exceeds the int64 packing limit (62)")
    merges = bpe.train_or_load(vocab_size, bpe_train_stories)          # trained once on a valid sample
    texts = load_texts(n_stories, split=split, shards=shards)
    from numpy.lib.stride_tricks import sliding_window_view
    parts_C, parts_y, total = [], [], 0                               # numpy sliding windows (scales to 1e8)
    for ids in bpe.encode_iter(texts, merges):
        if len(ids) <= window:
            continue
        wins = sliding_window_view(np.asarray(ids, dtype=np.int64), window + 1)   # (L-w, w+1): ctx + next
        parts_C.append(wins[:, :window]); parts_y.append(wins[:, window])
        total += len(wins)
        if total >= max_pairs:
            break
    C = np.concatenate(parts_C)[:max_pairs].copy()
    y = np.concatenate(parts_y)[:max_pairs].copy()
    print(f"[hf_data] TinyStories BPE: {len(texts)} stories -> {len(C)} (context,next) pairs; "
          f"V={vocab_size} (merges={len(merges)}, bpt={k}), window={window}, n_bits={n}", flush=True)
    return C, y, vocab_size, window


def tinystories_bpe_next_split(window=3, vocab_size=512, n_stories=60000, max_pairs=1_500_000,
                               test_frac=0.2, seed=0, split="valid", shards=None, bpe_train_stories=20000):
    """STORY-DISJOINT (context, next-token) split over a real byte-level BPE.

    Each story goes wholly to train or to test *before* windowing, so no context (and no recurring
    n-gram) straddles the split -- held-out contexts are genuinely unseen, unlike a random-row shuffle
    of pooled windows.  Returns (Ctr, ytr, Cte, yte, V, w, merges); train is capped at `max_pairs`."""
    from numpy.lib.stride_tricks import sliding_window_view

    from . import bpe
    k = int(vocab_size).bit_length() - 1
    if vocab_size != (1 << k):
        raise ValueError(f"vocab_size must be a power of 2, got {vocab_size}")
    if window * k > 62:
        raise ValueError(f"window*log2(vocab_size)={window * k} exceeds int64 packing limit 62")
    merges = bpe.train_or_load(vocab_size, bpe_train_stories)
    texts = load_texts(n_stories, split=split, shards=shards)
    rng = np.random.default_rng(seed)
    trC, trY, teC, teY, tot = [], [], [], [], 0
    for ids in bpe.encode_iter(texts, merges):
        if len(ids) <= window:
            continue
        wins = sliding_window_view(np.asarray(ids, np.int64), window + 1)
        if rng.random() < test_frac:
            teC.append(wins[:, :window]); teY.append(wins[:, window])
        else:
            trC.append(wins[:, :window]); trY.append(wins[:, window]); tot += len(wins)
        if tot >= max_pairs:
            break
    Ctr = np.concatenate(trC)[:max_pairs].copy(); ytr = np.concatenate(trY)[:max_pairs].copy()
    Cte = np.concatenate(teC).copy(); yte = np.concatenate(teY).copy()
    print(f"[hf_data] TinyStories BPE story-disjoint: V={vocab_size} w={window} "
          f"train={len(Ctr)} test={len(Cte)} (merges={len(merges)})", flush=True)
    return Ctr, ytr, Cte, yte, vocab_size, window, merges


def collapse_contexts(C, y, W):
    """Collapse (m,w) contexts + (m,) next-token ids into DISTINCT contexts with next-token counts.

    Returns (Cd (D,w) distinct contexts, counts (D,W) per-next-token counts, n (D,) totals, m).  For
    Zipfian text D << m, the lever that makes the exact-W search tractable at large m while preserving
    the empirical distribution exactly: for the one-vs-rest target of class t, the per-context weighted
    sum is  sum_{x in c} f(x) = 2*counts[c,t] - n[c]  (f = 2*1[y=t]-1)."""
    C = np.ascontiguousarray(C)
    m, w = C.shape
    base = int(C.max()) + 1 if C.size else 1
    if base ** w < (1 << 62):                                     # fast 1D packed unique (>> np.unique axis=0)
        Vp = base ** np.arange(w)
        uk, inv = np.unique((C.astype(np.int64) * Vp).sum(1), return_inverse=True)
        Cd = ((uk[:, None] // Vp) % base).astype(np.int64)        # decode packed keys back to (D, w)
    else:
        Cd, inv = np.unique(C, axis=0, return_inverse=True)
    counts = np.zeros((len(Cd), W), dtype=np.int64)
    np.add.at(counts, (inv, np.asarray(y, np.int64)), 1)
    return Cd, counts, counts.sum(1), len(C)
