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


def empirical_bernstein_radius(variance, m: int, comparisons: int, delta: float = 0.01):
    """Two-sided Maurer--Pontil radius for samples in ``[-1, 1]``.

    ``variance`` is the unbiased sample variance.  A union bound over the
    requested finite comparison class is included.  Unlike a Gaussian fit,
    this remains a finite-sample distribution-free certificate.
    """
    variance = np.asarray(variance, dtype=np.float64)
    if m < 2:
        return np.full_like(variance, np.inf)
    log_term = math.log(4.0 * max(comparisons, 1) / delta)
    return (np.sqrt(2.0 * np.maximum(variance, 0.0) * log_term / m)
            + 14.0 * log_term / (3.0 * (m - 1)))


def exponential_top_energy_fraction(k: int, q: int) -> float:
    """Asymptotic top-``k`` energy share for iid complex-Gaussian coefficients.

    Squared magnitudes of equal-variance circular complex Gaussians are iid
    exponentials.  If ``rho = k / q``, integrating the exponential quantile
    above its top-``rho`` cutoff gives ``rho * (1 - log(rho))``.  This is a
    modeling diagnostic, not a distribution-free Dataset-GL guarantee.
    """
    if q <= 0 or not 0 <= k <= q:
        raise ValueError("expected 0 <= k <= q and q > 0")
    if k == 0:
        return 0.0
    rho = k / q
    return float(rho * (1.0 - math.log(rho)))


def clopper_pearson_lower(successes: int, trials: int, alpha: float = 0.01) -> float:
    """Exact one-sided ``1-alpha`` binomial lower confidence bound."""
    from scipy.stats import beta

    if not 0 <= successes <= trials or trials <= 0:
        raise ValueError("expected 0 <= successes <= trials and trials > 0")
    if not 0 < alpha < 1:
        raise ValueError("alpha must lie in (0,1)")
    if successes == 0:
        return 0.0
    return float(beta.ppf(alpha, successes, trials - successes + 1))


def argmax_kl_threshold(top_probability, runner_up_probability):
    """Smallest KL needed to move the teacher top two across their tie face.

    If ``KL(p || q)`` is strictly below this value, ``q`` must have the same
    argmax as ``p`` (assuming the supplied teacher top probability is unique).
    """
    p1 = np.asarray(top_probability, dtype=np.float64)
    p2 = np.asarray(runner_up_probability, dtype=np.float64)
    if np.any(p1 < p2) or np.any(p2 < 0) or np.any(p1 > 1):
        raise ValueError("expected 1 >= p1 >= p2 >= 0")
    midpoint_mass = p1 + p2
    first = np.where(p1 > 0, p1 * np.log(2.0 * p1 / midpoint_mass), 0.0)
    second = np.where(p2 > 0, p2 * np.log(2.0 * p2 / midpoint_mass), 0.0)
    return first + second


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
