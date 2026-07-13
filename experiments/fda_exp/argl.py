"""Tokenizer-native categorical Dataset GL primitives.

No token is packed into bits.  Frequencies are explicit arrays in Z_q and the
positive-exponent child transform is q*ifft under torch/numpy conventions.
"""

from __future__ import annotations

import math
from dataclasses import dataclass

import numpy as np


def tokenizer_alphabet(tokenizer, raw_logit_width: int) -> int:
    """Return q after checking that tokenizer ids are exactly 0,...,q-1."""
    ids = set(tokenizer.get_vocab().values())
    q = len(tokenizer)
    if ids != set(range(q)):
        raise ValueError("tokenizer ids are not contiguous 0..len(tokenizer)-1")
    if raw_logit_width < q:
        raise ValueError(f"raw logit width {raw_logit_width} is smaller than tokenizer {q}")
    return q


def tokenizer_log_probs(logits, q: int):
    """Log-softmax over valid tokenizer rows; padded neural rows are excluded."""
    import torch

    return torch.log_softmax(logits[..., :q].float(), dim=-1)


def tokenizer_probs(logits, q: int):
    return tokenizer_log_probs(logits, q).exp()


def phase_from_difference(alpha, y, yp, q: int):
    """conj(chi_alpha(y))*chi_alpha(yp), returned as complex128 numpy."""
    alpha = np.asarray(alpha, dtype=np.int64)
    y = np.asarray(y, dtype=np.int64)
    yp = np.asarray(yp, dtype=np.int64)
    if y.shape != yp.shape or y.shape[-1] != len(alpha):
        raise ValueError("suffix shapes do not match alpha")
    if len(alpha) == 0:
        return np.ones(y.shape[:-1], dtype=np.complex128)
    # Reduce each product before the sum.  int64 is safe for this tokenizer/q/n.
    dot = np.remainder(np.remainder(yp - y, q) * alpha, q).sum(axis=-1) % q
    return np.exp((2j * np.pi / q) * dot)


def weighted_histogram(difference, weights, q: int, symmetrize: bool = True):
    """M^-1 sum_i weight_i 1[D_i=d], optionally Hermitian symmetrized."""
    difference = np.asarray(difference, dtype=np.int64) % q
    weights = np.asarray(weights, dtype=np.complex128)
    if difference.shape != weights.shape:
        raise ValueError("difference and weights must have the same shape")
    m = len(difference)
    if m == 0:
        raise ValueError("at least one pair is required")
    h = np.zeros(q, dtype=np.complex128)
    if symmetrize:
        np.add.at(h, difference, 0.5 * weights / m)
        np.add.at(h, (-difference) % q, 0.5 * np.conjugate(weights) / m)
    else:
        np.add.at(h, difference, weights / m)
    return h


def child_scores_from_histogram(h):
    """Positive-exponent DFT: score[c] = sum_d h[d] exp(+2pi i c d/q)."""
    h = np.asarray(h, dtype=np.complex128)
    return len(h) * np.fft.ifft(h)


def direct_child_scores(difference, weights, q: int):
    difference = np.asarray(difference, dtype=np.int64) % q
    weights = np.asarray(weights, dtype=np.complex128)
    c = np.arange(q, dtype=np.int64)[:, None]
    return (np.exp((2j * np.pi / q) * (c * difference[None, :] % q))
            * weights[None, :]).mean(axis=1)


def vector_pair_weights(p, pp):
    """Coordinatewise Hermitian inner product for batches of output vectors."""
    p = np.asarray(p)
    pp = np.asarray(pp)
    if p.shape != pp.shape:
        raise ValueError("vector batches must have the same shape")
    return (p * np.conjugate(pp)).sum(axis=-1)


def simplex_kernel(x, xp, support, q: int):
    """Exact centered-simplex tensor kernel for one position support."""
    x = np.asarray(x)
    xp = np.asarray(xp)
    support = np.asarray(support, dtype=np.int64)
    if len(support) == 0:
        return np.ones(np.broadcast_shapes(x.shape[:-1], xp.shape[:-1]))
    eq = x[..., support] == xp[..., support]
    return np.prod((q * eq.astype(np.float64) - 1.0) / (q - 1.0), axis=-1)


def hoeffding_radius(m: int, comparisons: int, delta: float = 0.01) -> float:
    """Two-sided radius for real bounded samples in [-1,1], union bounded."""
    return math.sqrt(2.0 * math.log(2.0 * max(comparisons, 1) / delta) / max(m, 1))


@dataclass(frozen=True)
class FrontierDecision:
    heavy: np.ndarray
    unresolved: np.ndarray
    light: np.ndarray
    radius: float


def classify_scores(scores, threshold: float, m: int, delta: float = 0.01):
    """Conservative bounded classification of real bucket estimates."""
    scores = np.asarray(scores).real
    radius = hoeffding_radius(m, len(scores), delta)
    return FrontierDecision(
        heavy=np.flatnonzero(scores - radius >= threshold),
        unresolved=np.flatnonzero((scores - radius < threshold) & (scores + radius >= threshold)),
        light=np.flatnonzero(scores + radius < threshold),
        radius=radius,
    )


def categorical_degrees(alphas) -> np.ndarray:
    a = np.asarray(alphas)
    if a.ndim != 2:
        raise ValueError("alphas must be a two-dimensional explicit frequency array")
    return (a != 0).sum(axis=1)
