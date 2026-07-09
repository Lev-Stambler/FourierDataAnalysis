"""E2 --- the context profile of a dataset.

For a coordinate order and each level ``k`` (with ``J = J_k`` the first ``k``
coordinates in the order and ``Jbar`` the rest), we compute the quantities that
govern ``thm:context-gl``:

- ``c_k``      : number of distinct contexts ``{x_Jbar : x in D}``;
- ``R_k``      : the level mass ``sum_{S subseteq J} Psi(S|J)``
                 ``= (2^k / m) sum_z (1/|D_z|) sum_{x in D_z} f(x)^2``
                 (``= 2^k c_k / m`` when ``f == 1``);
- ``N_k``      : the *true search width* ``#{S subseteq J_k : Psi(S|J_k) >= tau^2/4}``
                 (computed exactly when ``2^k`` is small enough);
- ``N_bound``  : the theorem's bound ``min(4 R_k / tau^2, 2 N_bound_{k-1})``.

``Psi`` is accumulated fiber by fiber.  A *singleton* fiber (one datapoint with a
given context) contributes ``f(x)^2 / m`` to ``Psi(S|J)`` for **every** ``S`` --- a
flat term.  Only *multi-point* fibers (repeated contexts) create structure, via a
size-``2^k`` Walsh--Hadamard transform of the fiber's lift.  Fully-singleton
levels are therefore exactly the blind ones (``Psi`` flat, ``N_k = 2^k``): the
code mirrors ``lem:blindness``.
"""

from __future__ import annotations

import numpy as np

from .spectrum import fwht, points_to_index


def apply_order(D: np.ndarray, order) -> np.ndarray:
    """Return ``D`` with columns permuted so that ``order[j]`` becomes coordinate ``j``."""
    order = np.asarray(order)
    return D[:, order]


def make_order(D: np.ndarray, kind: str, seed: int = 0) -> np.ndarray:
    """A coordinate order.  ``contiguous`` = identity (natural for sequences);
    ``random`` = a random permutation; ``greedy`` = coordinates sorted by how
    *unbalanced* they are on ``D`` (a cheap repetition-maximizing heuristic:
    put the most predictable coordinates last so contexts collapse)."""
    n = D.shape[1]
    if kind == "contiguous":
        return np.arange(n)
    if kind == "random":
        return np.random.default_rng(seed).permutation(n)
    if kind == "greedy":
        bias = np.abs(D.mean(axis=0))            # 1 = constant coordinate
        return np.argsort(bias)                   # least biased first, most biased last
    raise ValueError(f"unknown order '{kind}'")


def context_profile(D: np.ndarray, fD: np.ndarray, tau: float,
                    exact_cap: int = 20) -> dict:
    """Compute the per-level context profile.  Assumes ``D`` is already permuted
    into the desired coordinate order (so ``J_k`` is the first ``k`` columns)."""
    D = np.asarray(D)
    m, n = D.shape
    fD = np.asarray(fD, dtype=np.float64)
    idx = points_to_index(D)

    levels = {}
    n_bound_prev = None
    for k in range(0, n + 1):
        ctx = idx >> k
        jpart = idx & ((1 << k) - 1)

        order = np.argsort(ctx, kind="stable")
        ctx_s, jpart_s, f_s = ctx[order], jpart[order], fD[order]
        uniq, starts = np.unique(ctx_s, return_index=True)
        c_k = int(len(uniq))
        bounds = np.append(starts, m)

        exact = (k <= exact_cap)
        Rk = 0.0
        flat = 0.0
        heavy = None
        for gi in range(c_k):
            lo, hi = int(bounds[gi]), int(bounds[gi + 1])
            g = f_s[lo:hi]
            gsize = hi - lo
            ssq = float(np.dot(g, g))
            Rk += ssq / gsize
            if exact:
                if gsize == 1:
                    flat += (g[0] * g[0]) / m
                else:
                    if heavy is None:
                        heavy = np.zeros(1 << k)
                    u = np.zeros(1 << k)
                    np.add.at(u, jpart_s[lo:hi], g)
                    H = fwht(u)
                    heavy += (H * H) / (m * gsize)
        Rk *= (1 << k) / m

        thresh = tau * tau / 4.0
        if exact:
            if heavy is None:  # all singletons -> Psi flat -> blind level
                Nk = (1 << k) if flat >= thresh else 0
                psi_flat = True
            else:
                Psi = heavy + flat
                Nk = int(np.count_nonzero(Psi >= thresh))
                psi_flat = False
        else:
            Nk = None
            psi_flat = None

        markov_bound = (4.0 * Rk) / (tau * tau)
        n_bound = markov_bound if n_bound_prev is None else min(markov_bound, 2 * n_bound_prev)
        n_bound_prev = n_bound

        levels[k] = dict(
            k=k,
            c_k=c_k,
            R_k=Rk,
            N_k=Nk,
            N_bound=n_bound,
            psi_flat=psi_flat,
            mean_fiber=float(m / c_k),
        )
    return levels


def profile_summary(levels: dict) -> dict:
    """Reduce a per-level profile to the GL-relevant headline scalars.

    For Goldreich--Levin the metrics are the *output* (number of heavy dataset
    coefficients = the list ``L``, i.e. ``N`` at the top level) and the *search
    overhead* ``N_max / output`` --- how far above the true answer the tree
    search wanders.  Overhead ~1 means context conditioning is output-sensitive;
    large overhead is blindness.  (Coefficient *degree* is irrelevant to GL.)
    """
    n = max(levels)
    exact_Nk = [v["N_k"] for v in levels.values() if v["N_k"] is not None]
    output = levels[n]["N_k"]
    N_max = max(exact_Nk) if exact_Nk else None
    overhead = (N_max / output) if (N_max is not None and output) else None
    return dict(
        output=output,
        N_max=N_max,
        search_overhead=overhead,
        N_bound_max=max(v["N_bound"] for v in levels.values()),
        R_max=max(v["R_k"] for v in levels.values()),
    )
