"""Measured degree profiles on raw token streams (R1 ladder).

Oracle-free reverse-time filtration profile of the empirical next-token law:
    R_k = E || E[Y | suffix of length k] ||^2,     Y = one-hot next token,
estimated by grouping corpus positions by their length-k suffix.  Increments
Delta_k = R_k - R_{k-1} are the energy newly explained by the k-th newest token —
the data-side analog of the filtration increments (PLAN §4), requiring no
conditional oracle and no refill model.

Budget identity: for one-hot Y, E||Y||^2 = 1, so R_k in [R_0, 1] and the profile
measures how much of the next-token mass the newest k tokens explain.
"""

from __future__ import annotations

import numpy as np

__all__ = ["suffix_filtration_profile", "suffix_filtration_profile_crossfit"]


def suffix_filtration_profile(tokens: np.ndarray, q: int, L: int, k_max: int = 3,
                              stride: int = 1) -> dict:
    """R_k estimates for k=0..k_max from a token stream.

    Returns dict with R (array), Delta (array), n_positions, groups per k, and
    collision coverage (fraction of positions in suffix groups seen >= 2 times).
    """
    tokens = np.asarray(tokens, dtype=np.int64)
    n = len(tokens)
    assert n > L + k_max + 1
    starts = np.arange(L, n - 1, stride)
    Y = tokens[starts + 1]                      # next tokens
    N = len(starts)

    R = np.zeros(k_max + 1, dtype=np.float64)
    groups = np.zeros(k_max + 1, dtype=np.int64)
    coverage = np.zeros(k_max + 1, dtype=np.float64)

    # k = 0: marginal next-token energy
    cnt0 = np.bincount(Y, minlength=q)
    p = cnt0 / N
    R[0] = float((p**2).sum())

    # newest-first suffix digits: k-th newest token before Y is tokens[starts-(k-1)]
    suffix_id = np.zeros(N, dtype=np.int64)
    for k in range(1, k_max + 1):
        suffix_id = suffix_id * q + tokens[starts - (k - 1)]
        key = suffix_id * q + Y
        uk, inv = np.unique(key, return_inverse=True)
        c = np.bincount(inv)                     # count per (suffix, y) pair
        pair_suffix = uk // q
        us, sinv = np.unique(pair_suffix, return_inverse=True)
        ns = np.bincount(sinv, weights=c.astype(np.float64))  # positions per suffix
        # R_k = sum_{(s,y)} c_{s,y}^2 / (n_s * N)
        R[k] = float(((c.astype(np.float64) ** 2) / ns[sinv]).sum() / N)
        groups[k] = len(us)
        coverage[k] = float(ns[ns >= 2].sum() / N)

    Delta = np.diff(R)
    return {"R": R.tolist(), "Delta": Delta.tolist(), "n_positions": int(N),
            "groups": groups.tolist(), "coverage": coverage.tolist(),
            "k_max": k_max, "q": q, "L": L}


def suffix_filtration_profile_crossfit(tokens: np.ndarray, q: int, L: int,
                                       k_max: int = 3, stride: int = 1) -> dict:
    """Cross-fitted suffix profile for large alphabets/sparse suffix spaces.

    Conditional frequencies are fit on alternating positions and scored on the
    held-out positions. This removes the severe in-sample collision bias that
    otherwise drives R_k toward one when most q^k suffixes are singletons.
    Unseen held-out suffixes fall back to the fitted marginal distribution. Raw
    estimates and their monotone projection are both recorded.
    """
    tokens = np.asarray(tokens, dtype=np.int64)
    n = len(tokens)
    assert n > L + k_max + 1
    starts = np.arange(L, n - 1, stride)
    Y = tokens[starts + 1]
    N = len(starts)
    fit = (np.arange(N) % 2) == 0
    score = ~fit

    cnt0 = np.bincount(Y[fit], minlength=q).astype(np.float64)
    p0 = cnt0 / cnt0.sum()
    raw = np.zeros(k_max + 1, dtype=np.float64)
    raw[0] = float(p0[Y[score]].mean())
    groups = np.zeros(k_max + 1, dtype=np.int64)
    coverage = np.zeros(k_max + 1, dtype=np.float64)
    suffix_id = np.zeros(N, dtype=np.int64)

    for k in range(1, k_max + 1):
        suffix_id = suffix_id * q + tokens[starts - (k - 1)]
        fit_suffix = suffix_id[fit]
        fit_y = Y[fit]
        unique_suffix, suffix_count = np.unique(fit_suffix, return_counts=True)
        fit_pair = fit_suffix * q + fit_y
        unique_pair, pair_count = np.unique(fit_pair, return_counts=True)

        score_suffix = suffix_id[score]
        score_y = Y[score]
        si = np.searchsorted(unique_suffix, score_suffix)
        suffix_seen = (si < len(unique_suffix))
        suffix_seen[suffix_seen] &= unique_suffix[si[suffix_seen]] == score_suffix[suffix_seen]
        denom = np.ones(len(score_suffix), dtype=np.float64)
        denom[suffix_seen] = suffix_count[si[suffix_seen]]

        score_pair = score_suffix * q + score_y
        pi = np.searchsorted(unique_pair, score_pair)
        pair_seen = (pi < len(unique_pair))
        pair_seen[pair_seen] &= unique_pair[pi[pair_seen]] == score_pair[pair_seen]
        prob = np.zeros(len(score_suffix), dtype=np.float64)
        both = suffix_seen & pair_seen
        prob[both] = pair_count[pi[both]] / denom[both]
        prob[~suffix_seen] = p0[score_y[~suffix_seen]]
        raw[k] = float(prob.mean())
        groups[k] = len(unique_suffix)
        coverage[k] = float(np.mean(suffix_seen & (denom >= 2)))

    projected = np.maximum.accumulate(raw)
    return {
        "R": projected.tolist(), "R_raw_crossfit": raw.tolist(),
        "Delta": np.diff(projected).tolist(), "n_positions": int(N),
        "n_fit": int(fit.sum()), "n_score": int(score.sum()),
        "groups": groups.tolist(), "coverage": coverage.tolist(),
        "k_max": k_max, "q": q, "L": L,
        "estimator": "alternating-position cross-fit; unseen suffix -> fit marginal",
        "monotone_projection": True,
    }
