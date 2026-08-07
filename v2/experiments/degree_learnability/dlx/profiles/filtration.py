"""Filtration (variance-telescoping) profiles (PLAN §4.3).

For any coordinate ordering pi, the conditional-expectation martingale gives an
exact decomposition under any law with finite support:
    Var(f) = sum_{k=1}^n Delta_k^pi,
    Delta_k^pi = E||E[f | X_{pi_1..pi_k}]||^2 - E||E[f | X_{pi_1..pi_{k-1}}]||^2
(with Delta measured against E||E[f]||^2 at k=0).  No orthogonality is needed:
this telescopes by construction.

Under the UNIFORM product law, the averaged increments over random orderings are
a known linear transform of the Fourier level weights:
    E_order[Delta_k] = sum_t M_{k,t} W^t,
    M_{k,t} = [C(n-t, k-t) / C(n, k)] * (t / k)   for k >= t, else 0.
M is lower triangular with positive diagonal (M_{t,t} = 1/C(n,t)), hence
invertible: order-averaged filtration profiles identify the level weights.
"""

from __future__ import annotations

import itertools
from math import comb

import numpy as np

__all__ = [
    "exact_filtration_uniform",
    "order_mixing_matrix",
    "levels_from_averaged_filtration",
    "averaged_filtration_uniform_n",
]


def _conditional_energy_uniform(f_table: np.ndarray, q: int, n: int, S: list[int]) -> float:
    """E||E[f | X_S]||^2 under the uniform law; f_table shape (q,)*n [+ (out,)]."""
    if not S:
        m = f_table.mean(axis=tuple(range(n)))
        return float(np.real(np.sum(m * np.conj(m))))
    axes_S = list(S)
    out_extra = f_table.shape[n:]
    perm = axes_S + [j for j in range(n) if j not in S]
    t = np.transpose(f_table, perm + list(range(n, f_table.ndim)))
    t2 = t.reshape((q ** len(S), -1) + out_extra)
    mean = t2.mean(axis=1)
    e = np.sum(mean * np.conj(mean), axis=-1) if mean.ndim > 1 else mean * np.conj(mean)
    return float(np.real(e.mean()))


def exact_filtration_uniform(f_table: np.ndarray, q: int, order: list[int]) -> np.ndarray:
    """Increments Delta_k along `order` under the uniform law. Length n."""
    n = len(order)
    prev = _conditional_energy_uniform(f_table, q, n, [])  # ||E f||^2 baseline
    out = np.zeros(n, dtype=np.float64)
    for k in range(n):
        S = order[: k + 1]
        cur = _conditional_energy_uniform(f_table, q, n, S)
        out[k] = cur - prev
        prev = cur
    return out


def order_mixing_matrix(n: int) -> np.ndarray:
    """M_{k,t} = P a fixed level-t character is counted at step k of a random order.

    Indices k (step) and t (level), both 1..n, stored 0-based.  Level 0 is
    absorbed by the ||E f||^2 baseline and does not appear.  Lower triangular,
    positive diagonal.
    """
    M = np.zeros((n, n), dtype=np.float64)
    for k in range(1, n + 1):
        for t in range(1, k + 1):
            M[k - 1, t - 1] = comb(n - t, k - t) / comb(n, k) * (t / k)
    return M


def levels_from_averaged_filtration(avg_increments: np.ndarray) -> np.ndarray:
    """Invert the order-mixing transform: averaged increments -> level weights."""
    M = order_mixing_matrix(len(avg_increments))
    return np.linalg.solve(M, np.asarray(avg_increments, dtype=np.float64))


def averaged_filtration_uniform_n(f_table: np.ndarray, q: int, n: int,
                                  max_orders: int | None = None,
                                  rng: np.random.Generator | None = None) -> np.ndarray:
    """Average exact increments over all n! orderings (or a random sample)."""
    orders = list(itertools.permutations(range(n)))
    if max_orders is not None and len(orders) > max_orders:
        assert rng is not None
        idx = rng.choice(len(orders), size=max_orders, replace=False)
        orders = [orders[i] for i in idx]
    acc = np.zeros(n, dtype=np.float64)
    for order in orders:
        acc += exact_filtration_uniform(f_table, q, list(order))
    return acc / len(orders)
