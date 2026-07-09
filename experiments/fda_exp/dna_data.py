"""Real DNA next-nucleotide data (GenomicBenchmarks human promoters).

Small 4-letter alphabet {A,C,G,T}, so we one-hot both input and output and keep
``n = 4 * window`` tiny --- reusing the binary GL unchanged, running locally with
exact ground truth at small ``window``.

Each context = ``window`` nucleotides, one-hot encoded (a 4-dim +/-1 block per
position: the nucleotide's slot is +1, the other three -1).  Target = the next
nucleotide (one-vs-rest over 4 classes).

    from fda_exp.dna_data import dna_next_nucleotide
    X, y, vocab, window, block = dna_next_nucleotide(window=6)
"""

from __future__ import annotations

import os
import urllib.request

import numpy as np

_TRAIN_URL = (
    "https://huggingface.co/datasets/katarinagresova/"
    "Genomic_Benchmarks_human_nontata_promoters/resolve/"
    "refs%2Fconvert%2Fparquet/default/train/0000.parquet"
)
_CACHE = os.path.join(os.path.dirname(__file__), "..", "data", "dna_promoters_train.parquet")

_VOCAB = ["A", "C", "G", "T"]
_ID = {c: i for i, c in enumerate(_VOCAB)}
BLOCK = 4  # one-hot block size per nucleotide


def load_sequences(n_seqs: int | None = None) -> list[str]:
    import pyarrow.parquet as pq

    path = os.path.abspath(_CACHE)
    if not os.path.exists(path):
        os.makedirs(os.path.dirname(path), exist_ok=True)
        print(f"[dna_data] downloading promoter sequences -> {path}")
        urllib.request.urlretrieve(_TRAIN_URL, path)
    seqs = pq.read_table(path, columns=["seq"]).column("seq").to_pylist()
    seqs = [s.upper() for s in seqs]
    return seqs if n_seqs is None else seqs[:n_seqs]


def _onehot(nt_id: int) -> np.ndarray:
    b = -np.ones(BLOCK, dtype=np.int64)
    b[nt_id] = 1
    return b


def dna_next_nucleotide(window: int = 6, n_seqs: int = 20000,
                        max_pairs: int = 400_000, **_ignore):
    """Return (X in {-1,1}^{4w} one-hot contexts, y next-nt ids, vocab, window, BLOCK)."""
    seqs = load_sequences(n_seqs)
    rows, nxt = [], []
    for s in seqs:
        ids = [_ID.get(c, -1) for c in s]
        for i in range(len(ids) - window):
            ctx = ids[i:i + window]
            tgt = ids[i + window]
            if tgt < 0 or min(ctx) < 0:      # skip any window touching an 'N'
                continue
            rows.append(np.concatenate([_onehot(t) for t in ctx]))
            nxt.append(tgt)
        if len(rows) >= max_pairs:
            break
    X = np.array(rows[:max_pairs], dtype=np.int64)
    y = np.array(nxt[:max_pairs], dtype=np.int64)
    print(f"[dna_data] promoters: {len(seqs)} seqs -> {len(X)} (context,next) pairs; "
          f"window={window}, n={window * BLOCK}, alphabet={_VOCAB}")
    return X, y, _VOCAB, window, BLOCK


def decode_context(x: np.ndarray, window: int) -> str:
    """One-hot-aware context decode: read each 4-slot block, +1 slot = nucleotide."""
    out = []
    for p in range(window):
        blk = x[p * BLOCK:(p + 1) * BLOCK]
        out.append(_VOCAB[int(np.argmax(blk))])
    return "".join(out)
