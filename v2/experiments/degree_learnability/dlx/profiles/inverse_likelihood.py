"""Inverse-likelihood categorical functional ANOVA (Ferrere et al., 2026).

The basis generator is Definition 3.1 / equation (19) of arXiv:2603.02673.
For a coordinate subset A and non-reference category tuple z,

    phi_A^z(x) = prod_i (1{x_i=z_i} - 1{x_i=N_i-1}) / p_A(x_A).

The exact helper below is intentionally for small audit instances.  Text-scale
pair profiles use the equivalent nested projection implemented in
``text_anova.py`` without materializing all inverse-probability columns.
"""

from __future__ import annotations

from dataclasses import dataclass
from itertools import combinations, product

import numpy as np


@dataclass(frozen=True)
class InverseLikelihoodTerm:
    support: tuple[int, ...]
    categories: tuple[int, ...]

    @property
    def degree(self) -> int:
        return len(self.support)


def canonical_terms(cardinalities: tuple[int, ...], max_degree: int) -> list[InverseLikelihoodTerm]:
    """Paper-canonical low-order-first dictionary order."""
    terms = [InverseLikelihoodTerm((), ())]
    d = len(cardinalities)
    for degree in range(1, min(max_degree, d) + 1):
        for support in combinations(range(d), degree):
            ranges = [range(cardinalities[i] - 1) for i in support]
            terms.extend(InverseLikelihoodTerm(support, tuple(z))
                         for z in product(*ranges))
    return terms


def inverse_likelihood_column(X: np.ndarray, term: InverseLikelihoodTerm) -> np.ndarray:
    """Evaluate one Formula-(19) column under X's empirical distribution."""
    X = np.asarray(X, dtype=np.int64)
    if not term.support:
        return np.ones(len(X), dtype=np.float64)

    support = np.asarray(term.support, dtype=np.int64)
    XA = X[:, support]
    refs = X[:, support].max(axis=0)
    # Formula (19) assumes the encoded alphabet is 0..N_i-1.  Require the
    # reference category to occur so the empirical inverse likelihood is valid.
    numerator = np.ones(len(X), dtype=np.float64)
    for j, z in enumerate(term.categories):
        numerator *= ((XA[:, j] == z).astype(np.float64)
                      - (XA[:, j] == refs[j]).astype(np.float64))

    _, inverse, counts = np.unique(XA, axis=0, return_inverse=True, return_counts=True)
    probability = counts[inverse].astype(np.float64) / len(X)
    return numerator / probability


def fit_exact_inverse_likelihood(
    X: np.ndarray,
    targets: np.ndarray,
    cardinalities: tuple[int, ...],
    max_degree: int,
    max_rank: int | None = None,
    rank_tolerance: float = 1e-10,
) -> dict:
    """Rank-select Formula-(19) columns and solve Gamma c = mu.

    This mirrors Algorithm 1 on the empirical support. ``targets`` may be scalar
    or vector valued.  It is used by audits/tests, not by the q=256 text path.
    """
    X = np.asarray(X, dtype=np.int64)
    target = np.asarray(targets, dtype=np.float64)
    if target.ndim == 1:
        target = target[:, None]
    if len(X) != len(target):
        raise ValueError("X and targets must have the same row count")

    support_X, first, inverse, counts = np.unique(
        X, axis=0, return_index=True, return_inverse=True, return_counts=True)
    weights = counts.astype(np.float64) / len(X)
    support_target = np.zeros((len(support_X), target.shape[1]), dtype=np.float64)
    np.add.at(support_target, inverse, target)
    support_target /= counts[:, None]

    limit = len(support_X) if max_rank is None else min(max_rank, len(support_X))
    selected: list[InverseLikelihoodTerm] = []
    columns: list[np.ndarray] = []
    # Rank is unchanged by positive row weighting; weighted orthogonalization is
    # numerically better aligned with the L2(p) linear system.
    Q: list[np.ndarray] = []
    for term in canonical_terms(cardinalities, max_degree):
        col = inverse_likelihood_column(support_X, term)
        weighted = np.sqrt(weights) * col
        residual = weighted.copy()
        for q in Q:
            residual -= q * np.dot(q, residual)
        norm = float(np.linalg.norm(residual))
        if norm <= rank_tolerance * max(1.0, float(np.linalg.norm(weighted))):
            continue
        Q.append(residual / norm)
        selected.append(term)
        columns.append(col)
        if len(selected) >= limit:
            break

    B = np.column_stack(columns)
    gamma = B.T @ (weights[:, None] * B)
    mu = B.T @ (weights[:, None] * support_target)
    coefficients = np.linalg.lstsq(gamma, mu, rcond=rank_tolerance)[0]
    reconstruction = B @ coefficients
    mse = float(np.sum(weights[:, None] * (support_target - reconstruction) ** 2))
    return {
        "terms": selected,
        "coefficients": coefficients,
        "rank": len(selected),
        "support_size": len(support_X),
        "weighted_mse": mse,
        "reconstruction": reconstruction,
        "support_target": support_target,
        "first_indices": first,
    }

