"""Self-contained byte-level BPE with an exact power-of-2 vocabulary.

256 base byte-ids (full byte fallback -> NO <unk>, any UTF-8 text encodes) + exactly `V-256` learned
merges  =>  dense vocabulary `0..V-1` with `V = 2^k`.  Dependency-free (stdlib only), matching the
repo's hand-rolled style; trained on lowercased TinyStories, merges cached as JSON in `$FDA_DATA_DIR`.

Why exact 2^k matters: the categorical Householder GL search (`hadamard_gl`) needs `V = 2^k` so its
Walsh basis is unit-modulus and the V-way tree collapses to a binary bit tree.  Reserving all 256
bytes and doing exactly `V-256` merges lands on `V` dense ids by construction (no vocab_size drift,
no <unk> bucket, total coverage).
"""

from __future__ import annotations

import json
import os
import re
from collections import Counter

# Pre-tokenization: split lowercased text into letter / digit / punct runs (a leading space attaches
# to its run, GPT-2 style), or whitespace runs.  Merges never cross these boundaries.  The alternatives
# partition every character, so "".join(_words(t)) == t.lower() (byte fallback handles any residue).
_PAT = re.compile(r" ?[a-z]+| ?[0-9]+| ?[^a-z0-9\s]+|\s+")


def _cache_dir() -> str:
    return os.environ.get("FDA_DATA_DIR", os.path.join(os.path.dirname(__file__), "..", "data"))


def _words(text: str) -> list[str]:
    return _PAT.findall(text.lower())


def vocab_size(merges) -> int:
    return 256 + len(merges)


def train_bpe(texts, vocab: int):
    """Learn exactly `vocab-256` merges on the corpus; return `[((a,b), new_id), ...]` in learned order."""
    if vocab < 256 or (vocab & (vocab - 1)) != 0:
        raise ValueError(f"vocab must be a power of 2 >= 256, got {vocab}")
    wc = Counter()
    for t in texts:
        wc.update(_words(t))
    words = [[list(w.encode("utf-8")), cnt] for w, cnt in wc.items()]         # [symbols, count]
    merges = []
    next_id = 256
    for _ in range(vocab - 256):
        pairs = Counter()
        for syms, cnt in words:
            for i in range(len(syms) - 1):
                pairs[(syms[i], syms[i + 1])] += cnt
        if not pairs:
            break                                                             # corpus fully merged
        (a, b), _cnt = pairs.most_common(1)[0]
        merges.append(((a, b), next_id))
        for entry in words:                                                   # apply merge in place
            syms = entry[0]
            if a not in syms:
                continue
            out, i = [], 0
            while i < len(syms):
                if i < len(syms) - 1 and syms[i] == a and syms[i + 1] == b:
                    out.append(next_id); i += 2
                else:
                    out.append(syms[i]); i += 1
            entry[0] = out
        next_id += 1
    return merges


def make_rank(merges) -> dict:
    """{(a,b): new_id} — merge priority (smaller new_id = learned earlier = applied first)."""
    return {pair: nid for pair, nid in merges}


def encode_word(word: str, rank: dict) -> list[int]:
    """Encode a single pre-token to ids, greedily applying the earliest-learned mergeable pair."""
    syms = list(word.encode("utf-8"))                                         # byte fallback => no <unk>
    while len(syms) >= 2:
        best_i, best_rank = None, None
        for i in range(len(syms) - 1):
            nid = rank.get((syms[i], syms[i + 1]))
            if nid is not None and (best_rank is None or nid < best_rank):
                best_i, best_rank = i, nid
        if best_i is None:
            break
        syms = syms[:best_i] + [best_rank] + syms[best_i + 2:]
    return syms


def encode(text: str, merges) -> list[int]:
    """Encode text -> list of token ids in 0..V-1 (never <unk>)."""
    rank = make_rank(merges)
    cache: dict[str, list[int]] = {}
    out: list[int] = []
    for w in _words(text):
        e = cache.get(w)
        if e is None:
            e = encode_word(w, rank); cache[w] = e
        out.extend(e)
    return out


def encode_iter(texts, merges):
    """Yield the id list of each text, caching per unique pre-token (fast over a whole corpus)."""
    rank = make_rank(merges)
    cache: dict[str, list[int]] = {}
    for t in texts:
        ids: list[int] = []
        for w in _words(t):
            e = cache.get(w)
            if e is None:
                e = encode_word(w, rank); cache[w] = e
            ids.extend(e)
        yield ids


def _id_to_bytes(merges) -> dict:
    table = {i: bytes([i]) for i in range(256)}
    for (a, b), nid in merges:
        table[nid] = table[a] + table[b]
    return table


def decode(ids, merges) -> str:
    table = _id_to_bytes(merges)
    return b"".join(table[int(i)] for i in ids).decode("utf-8", errors="replace")


def _merges_path(vocab: int, n_stories: int) -> str:
    d = _cache_dir(); os.makedirs(d, exist_ok=True)
    return os.path.join(d, f"tinystories_bpe_v{vocab}_n{n_stories}.json")


def save_merges(merges, path: str) -> None:
    with open(path, "w") as fh:
        json.dump([[list(pair), nid] for pair, nid in merges], fh)


def load_merges(path: str):
    with open(path) as fh:
        raw = json.load(fh)
    return [((int(p[0]), int(p[1])), int(nid)) for p, nid in raw]


def train_or_load(vocab: int, n_stories: int, texts=None):
    """Cached trainer: load merges for (vocab, n_stories) if present, else train on `texts`
    (default: `hf_data.load_texts(n_stories)`) and cache them.  Returns the merge list."""
    path = _merges_path(vocab, n_stories)
    if os.path.exists(path):
        return load_merges(path)
    if texts is None:
        from .hf_data import load_texts
        texts = load_texts(n_stories)
    merges = train_bpe(texts, vocab)
    save_merges(merges, path)
    return merges
