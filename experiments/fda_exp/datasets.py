"""Datasets ``D subset {-1,1}^n`` and labels ``f``.

Every builder returns ``(D, fD, f_full)`` where

- ``D``      : ``(m, n)`` array of ``+/-1`` (distinct rows),
- ``fD``     : ``(m,)`` labels of ``f`` on ``D`` (in ``[-1, 1]``),
- ``f_full`` : length-``2^n`` array of ``f`` on the *whole* cube, or ``None``
               (needed only for the global-degree side of E5).

Controls (`uniform_random`, `subcube`) pin down the two regimes of
``thm:context-gl``; `markov` is a self-contained *structured/real-like* corpus
whose contiguous contexts repeat (the phenomenon the headline theorem exploits).
"""

from __future__ import annotations

import numpy as np


def _dedup(D: np.ndarray) -> np.ndarray:
    """Keep distinct rows (datasets in the paper are sets of distinct points)."""
    _, keep = np.unique(D, axis=0, return_index=True)
    return D[np.sort(keep)]


def _label_full(kind: str, n: int, rng: np.random.Generator):
    """Return a length-``2^n`` array of ``f`` over the whole cube for a simple rule.

    ``kind`` is one of ``"const"``, ``"parity:i,j,..."`` (a planted parity on the
    listed coordinates), or ``"random_lowdeg:d"`` (a random degree-<=d function
    normalized to ``[-1,1]``).
    """
    from .spectrum import fwht, popcount_table

    size = 1 << n
    if kind == "const":
        return np.ones(size)
    if kind.startswith("parity:"):
        coords = [int(c) for c in kind.split(":", 1)[1].split(",") if c != ""]
        S = 0
        for c in coords:
            S |= (1 << c)
        # chi_S over the cube in index order: chi_S(x) = (-1)^{popcount(S & idx)}
        idxs = np.arange(size)
        pc = popcount_table(n)
        return (-1.0) ** pc[S & idxs]
    if kind.startswith("random_lowdeg:"):
        d = int(kind.split(":", 1)[1])
        deg = popcount_table(n)
        coeff = np.zeros(size)
        mask = deg <= d
        coeff[mask] = rng.standard_normal(int(mask.sum()))
        vals = fwht(coeff)  # synthesize a function with those Fourier coeffs
        vals /= np.max(np.abs(vals))  # normalize into [-1, 1]
        return vals
    raise ValueError(f"unknown label kind: {kind}")


def uniform_random(n: int, m: int, seed: int = 0, label: str = "parity:0,1"):
    """``m`` i.i.d. uniform points (the *blindness* regime)."""
    rng = np.random.default_rng(seed)
    D = _dedup(rng.choice([-1, 1], size=(m, n)).astype(np.int64))
    f_full = _label_full(label, n, rng)
    fD = f_full[_index(D)]
    return D, fD, f_full


def subcube(n: int, fixed: int = 3, seed: int = 0, label: str = "const"):
    """Full subcube fixing the first ``fixed`` coordinates to ``+1``.

    The paper's canonical structured example (``thm:context-gl`` regime 2):
    ``|D| = 2^{n-fixed}`` and, with ``label='const'``, ``R_k = 2^{|K cap [k]|}``.
    """
    rng = np.random.default_rng(seed)
    free = n - fixed
    m = 1 << free
    D = np.ones((m, n), dtype=np.int64)
    grid = ((np.arange(m)[:, None] >> np.arange(free)[None, :]) & 1)
    D[:, fixed:] = 1 - 2 * grid  # 0/1 -> +1/-1
    D = _dedup(D)
    f_full = _label_full(label, n, rng)
    fD = f_full[_index(D)]
    return D, fD, f_full


def markov(n: int, m: int, flip_prob: float = 0.15, seed: int = 0,
           label: str = "trained"):
    """A structured binary *sequence* corpus with repeating contiguous contexts.

    Each sequence is a run-length-ish walk: start random, then each coordinate
    equals the previous one except with probability ``flip_prob``.  Low
    ``flip_prob`` -> few distinct contiguous contexts -> small context profile.
    Realistic stand-in for n-gram repetition, fully self-contained.
    """
    rng = np.random.default_rng(seed)
    seqs = np.empty((m, n), dtype=np.int64)
    seqs[:, 0] = rng.choice([-1, 1], size=m)
    for j in range(1, n):
        flip = rng.random(m) < flip_prob
        seqs[:, j] = np.where(flip, -seqs[:, j - 1], seqs[:, j - 1])
    D = _dedup(seqs)
    f_full = _make_label(label, D, n, rng)
    fD = f_full[_index(D)] if f_full is not None else _fallback_label(D, rng)
    return D, fD, f_full


def mixture(n: int, m: int, templates: int = 8, flip_prob: float = 0.05,
            seed: int = 0, label: str = "trained"):
    """Mixture of ``templates`` base sequences, each copy locally corrupted with
    per-coordinate flip probability ``flip_prob``.

    Models how real corpora actually look: a few templates, many noisy copies, so
    long contiguous contexts repeat (large fibers at high levels) while the data
    stays sparse in the cube.  This is the regime where context conditioning has
    the best chance to keep GL's *search overhead* near 1.
    """
    rng = np.random.default_rng(seed)
    tpl = rng.choice([-1, 1], size=(templates, n))
    who = rng.integers(0, templates, size=m)
    flips = rng.random((m, n)) < flip_prob
    D = _dedup(tpl[who] * np.where(flips, -1, 1))
    f_full = _make_label(label, D, n, rng)
    fD = f_full[_index(D)] if f_full is not None else _fallback_label(D, rng)
    return D, fD, f_full


# --- tiny-stories next-token prediction (a real toy language task) ----------
# 16-token vocabulary (4 bits/token); a simple sentence grammar generates a
# token stream, we slide a length-``window`` context and predict a bit of the
# NEXT token.  Natural-language-style n-gram repetition is genuine here.
_STORY_TOKENS = ["<s>", "the", "a", "cat", "dog", "sun", "boy", "girl",
                 "sat", "ran", "saw", "ate", "big", "small", "and", "."]
_STORY_VERBS = {8, 9, 10, 11}   # sat ran saw ate
_STORY = dict(dets=[1, 2], nouns=[3, 4, 5, 6, 7], adjs=[12, 13],
              verbs=[8, 9, 10, 11], conj=[14], period=15)


def _gen_story_stream(n_sentences: int, rng: np.random.Generator) -> list[int]:
    s = _STORY
    stream = [0]  # <s>
    for _ in range(n_sentences):
        stream.append(int(rng.choice(s["dets"])))
        if rng.random() < 0.5:
            stream.append(int(rng.choice(s["adjs"])))
        stream.append(int(rng.choice(s["nouns"])))
        stream.append(int(rng.choice(s["verbs"])))
        stream.append(s["period"])
        if rng.random() < 0.6:
            stream.append(int(rng.choice(s["conj"])))
    return stream


def _token_bits(t: int, bpt: int) -> np.ndarray:
    return np.array([1 - 2 * ((t >> j) & 1) for j in range(bpt)], dtype=np.int64)


def _next_token_bit(tok: int, target: str) -> float:
    if target == "is_verb":
        return 1.0 if tok in _STORY_VERBS else -1.0
    if target == "high_bit":
        return 1.0 if tok >= 8 else -1.0
    raise ValueError(f"unknown target '{target}'")


def tiny_stories(window: int = 4, bits_per_token: int = 4, n_sentences: int = 5000,
                 seed: int = 0, target: str = "is_verb", **_ignore):
    """Next-token prediction on a tiny grammar-generated story corpus.

    Each datapoint is a ``window``-token context (``n = window*bits_per_token``
    bits); the label is a bit of the *next* token (default: is it a verb).  ``D``
    is the set of distinct contexts, ``f`` their majority next-token bit.  Returns
    ``f_full = None`` (``f`` is only the empirical next-token rule, defined on the
    corpus) --- fine for GL, which only searches on-dataset coefficients.
    """
    rng = np.random.default_rng(seed)
    stream = _gen_story_stream(n_sentences, rng)
    bpt = bits_per_token
    rows, labs = [], []
    for i in range(len(stream) - window):
        ctx = stream[i:i + window]
        rows.append(np.concatenate([_token_bits(t, bpt) for t in ctx]))
        labs.append(_next_token_bit(stream[i + window], target))
    X = np.array(rows, dtype=np.int64)
    y = np.array(labs, dtype=np.float64)
    uniq, inv = np.unique(X, axis=0, return_inverse=True)
    fD = np.array([1.0 if y[inv == g].mean() >= 0 else -1.0 for g in range(len(uniq))])
    return uniq, fD, None


def _make_label(kind: str, D: np.ndarray, n: int, rng: np.random.Generator):
    """Labels for real-like data.  ``trained`` fits a small tree so ``f`` is
    defined on the whole cube (needed for global degree in E5)."""
    if kind == "trained":
        from sklearn.tree import DecisionTreeClassifier

        # target: a structured but nonlinear rule on the sequence
        y = _target(D)
        clf = DecisionTreeClassifier(max_depth=6, random_state=int(rng.integers(1 << 30)))
        clf.fit(((1 - D) // 2), y)  # fit on 0/1 features
        cube = _all_points(n)
        pred = clf.predict(((1 - cube) // 2))
        return (2 * pred - 1).astype(np.float64)  # {0,1} -> {-1,1}
    return _label_full(kind, n, rng)


def _target(D: np.ndarray) -> np.ndarray:
    """A structured binary target for the sequence corpus: majority of a few
    contiguous windows (depends on local structure, not a single coordinate)."""
    n = D.shape[1]
    a = (D[:, : n // 3] == 1).sum(1)
    b = (D[:, n // 3 : 2 * n // 3] == 1).sum(1)
    return ((a >= b)).astype(np.int64)


def _fallback_label(D: np.ndarray, rng: np.random.Generator) -> np.ndarray:
    return (2 * _target(D) - 1).astype(np.float64)


def _all_points(n: int) -> np.ndarray:
    size = 1 << n
    idx = np.arange(size)
    bits = ((idx[:, None] >> np.arange(n)[None, :]) & 1)
    return (1 - 2 * bits).astype(np.int64)  # 0/1 bit -> +1/-1


def _index(D: np.ndarray) -> np.ndarray:
    from .spectrum import points_to_index

    return points_to_index(D)


BUILDERS = {
    "uniform_random": uniform_random,
    "subcube": subcube,
    "markov": markov,
    "mixture": mixture,
    "tiny_stories": tiny_stories,
}


def build(kind: str, **kwargs):
    if kind not in BUILDERS:
        raise ValueError(f"unknown dataset '{kind}'; choose from {list(BUILDERS)}")
    return BUILDERS[kind](**kwargs)
