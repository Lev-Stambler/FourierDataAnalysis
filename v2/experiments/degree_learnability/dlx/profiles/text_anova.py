"""Cross-fitted inverse-likelihood functional-ANOVA profiles for token streams.

For a pair of context positions, Ferrere et al.'s Definition-3.1 columns through
degree 1 span the constant-plus-additive categorical functions.  Adding the
degree-2 columns spans all functions on the observed pair support after
low-degree-first rank selection.  We compute the two nested weighted projections
directly, avoiding a q^2-column design matrix while retaining the same subspaces
and interaction order.  Equation (19) in that paper is only a toy example.
"""

from __future__ import annotations

from itertools import combinations

import numpy as np


def _brier_from_predictions(
    actual_probability: np.ndarray, squared_norm: np.ndarray
) -> float:
    return float(np.mean(1.0 - 2.0 * actual_probability + squared_norm))


def variance_concentration(
    baseline_brier: float, degree1_brier: float, degree2_brier: float
) -> dict:
    """Convert nested Brier risks into variance and degree concentration.

    In population, the baseline-minus-projection risk is the squared L2 norm of
    the projected conditional one-hot function. Cross-fit noise can make the
    nominal degree-2 estimate worse than degree 1, so the nested projection risk
    is monotonized before forming concentrations.
    """
    observed_variance = max(0.0, float(baseline_brier))
    nested_degree1_risk = min(float(baseline_brier), float(degree1_brier))
    nested_degree2_risk = min(nested_degree1_risk, float(degree2_brier))
    degree1_variance = max(0.0, observed_variance - nested_degree1_risk)
    pair_function_variance = max(0.0, observed_variance - nested_degree2_risk)
    degree2_variance = max(0.0, pair_function_variance - degree1_variance)
    if pair_function_variance > 0.0:
        concentration1 = degree1_variance / pair_function_variance
        concentration2 = 1.0
    else:
        concentration1 = None
        concentration2 = None
    return {
        "observed_output_variance": observed_variance,
        "pair_function_variance": pair_function_variance,
        "pair_variance_fraction_of_observed": (
            pair_function_variance / observed_variance
            if observed_variance > 0
            else None
        ),
        "degree1_variance": degree1_variance,
        "degree2_incremental_variance": degree2_variance,
        "concentration_leq_1": concentration1,
        "concentration_leq_2": concentration2,
        "tail_above_degree_1": 1.0 - concentration1
        if concentration1 is not None
        else None,
        "monotone_projection_applied": degree2_brier > nested_degree1_risk,
    }


def conditional_fourier_spectrum(
    baseline_brier: float, degree1_brier: float, degree2_brier: float, q: int
) -> dict:
    """Level weights of a two-variable conditional next-token function.

    Nested Brier projection identities give the squared coefficient mass in a
    Gram-orthonormalized categorical basis ordered by degree.  This reports the
    absolute level weights, including the constant, rather than renormalizing the
    degree-1/2 signal to one.
    """
    if q < 2:
        raise ValueError("q must be at least two")
    risk0 = float(baseline_brier)
    risk1 = min(risk0, float(degree1_brier))
    risk2 = min(risk1, float(degree2_brier))
    weights = [
        max(0.0, 1.0 - risk0),
        max(0.0, risk0 - risk1),
        max(0.0, risk1 - risk2),
    ]
    counts = [1, 2 * (q - 1), (q - 1) ** 2]
    total = float(sum(weights))
    nonconstant = float(weights[1] + weights[2])
    return {
        "level_weights": weights,
        "level_cardinalities": counts,
        "mean_squared_coefficient_by_level": [
            weight / count for weight, count in zip(weights, counts, strict=True)
        ],
        "total_square_energy": total,
        "nonconstant_energy": nonconstant,
        "mean_spectral_degree": (
            (weights[1] + 2.0 * weights[2]) / total if total > 0.0 else None
        ),
        "mean_nonconstant_spectral_degree": (
            (weights[1] + 2.0 * weights[2]) / nonconstant if nonconstant > 0.0 else None
        ),
        "cumulative_concentration": [
            weights[0] / total if total > 0.0 else None,
            (weights[0] + weights[1]) / total if total > 0.0 else None,
            1.0 if total > 0.0 else None,
        ],
        "irreducible_or_unresolved_brier": risk2,
        "identity": "W0=1-L0; W1=L0-L1; W2=L1-L2; sum Wk=1-L2",
        "basis": "Gram-orthonormalized Definition-3.1 categorical degree filtration",
    }


def _additive_fit(
    a: np.ndarray, b: np.ndarray, y: np.ndarray, q: int, max_iter: int, tolerance: float
) -> tuple[np.ndarray, np.ndarray, np.ndarray, int]:
    n = len(y)
    n_a = np.bincount(a, minlength=q).astype(np.float64)
    n_b = np.bincount(b, minlength=q).astype(np.float64)
    n_y = np.bincount(y, minlength=q).astype(np.float64)
    n_ab = np.bincount(a * q + b, minlength=q * q).reshape(q, q).astype(np.float64)
    n_ay = np.bincount(a * q + y, minlength=q * q).reshape(q, q).astype(np.float64)
    n_by = np.bincount(b * q + y, minlength=q * q).reshape(q, q).astype(np.float64)

    intercept = n_y / n
    g_a = np.zeros((q, q), dtype=np.float64)
    g_b = np.zeros((q, q), dtype=np.float64)
    active_a = n_a > 0
    active_b = n_b > 0

    for iteration in range(1, max_iter + 1):
        old_a = g_a.copy()
        old_b = g_b.copy()
        g_a[active_a] = (
            n_ay[active_a] - n_a[active_a, None] * intercept - (n_ab @ g_b)[active_a]
        ) / n_a[active_a, None]
        g_a[~active_a] = 0.0
        g_a -= (n_a @ g_a / n)[None, :]

        g_b[active_b] = (
            n_by[active_b] - n_b[active_b, None] * intercept - (n_ab.T @ g_a)[active_b]
        ) / n_b[active_b, None]
        g_b[~active_b] = 0.0
        g_b -= (n_b @ g_b / n)[None, :]
        change = max(
            float(np.max(np.abs(g_a - old_a))), float(np.max(np.abs(g_b - old_b)))
        )
        if change <= tolerance:
            break
    return intercept, g_a, g_b, iteration


def _score_additive(
    intercept: np.ndarray,
    g_a: np.ndarray,
    g_b: np.ndarray,
    a: np.ndarray,
    b: np.ndarray,
    y: np.ndarray,
) -> float:
    actual = intercept[y] + g_a[a, y] + g_b[b, y]
    c2 = float(intercept @ intercept)
    ga2 = np.sum(g_a * g_a, axis=1)
    gb2 = np.sum(g_b * g_b, axis=1)
    cga = g_a @ intercept
    cgb = g_b @ intercept
    norms = (
        c2
        + ga2[a]
        + gb2[b]
        + 2.0 * cga[a]
        + 2.0 * cgb[b]
        + 2.0 * np.sum(g_a[a] * g_b[b], axis=1)
    )
    return _brier_from_predictions(actual, norms)


def _score_pair_lookup(
    a_fit: np.ndarray,
    b_fit: np.ndarray,
    y_fit: np.ndarray,
    a_score: np.ndarray,
    b_score: np.ndarray,
    y_score: np.ndarray,
    q: int,
    smoothing: float,
    intercept: np.ndarray,
    g_a: np.ndarray,
    g_b: np.ndarray,
    prior_chunk_pairs: int = 512,
) -> float:
    """Score a smoothed pair lookup without materializing its q-by-q-by-q prior."""
    pair_fit = a_fit * q + b_fit
    pair_score = a_score * q + b_score
    n_pair = np.bincount(pair_fit, minlength=q * q).astype(np.float64)
    triple = pair_fit * q + y_fit
    unique, counts = np.unique(triple, return_counts=True)
    pair_u = unique // q
    y_u = unique % q
    sum_count_sq = np.bincount(
        pair_u, weights=counts.astype(float) ** 2, minlength=q * q
    )

    active_pairs = np.union1d(np.unique(pair_fit), np.unique(pair_score))
    fit_active = np.searchsorted(active_pairs, pair_u)
    score_active = np.searchsorted(active_pairs, pair_score)
    fit_order = np.argsort(fit_active, kind="stable")
    score_order = np.argsort(score_active, kind="stable")
    fit_prior = np.empty(len(pair_u), dtype=np.float64)
    score_prior = np.empty(len(pair_score), dtype=np.float64)
    prior_sq = np.zeros(q * q, dtype=np.float64)

    for start in range(0, len(active_pairs), prior_chunk_pairs):
        stop = min(start + prior_chunk_pairs, len(active_pairs))
        codes = active_pairs[start:stop]
        a_codes = codes // q
        b_codes = codes % q
        prior = intercept[None, :] + g_a[a_codes] + g_b[b_codes]
        prior = np.maximum(prior, 1e-9)
        prior /= prior.sum(axis=1, keepdims=True)
        prior_sq[codes] = np.sum(prior * prior, axis=1)

        fit_lo = np.searchsorted(fit_active[fit_order], start, side="left")
        fit_hi = np.searchsorted(fit_active[fit_order], stop, side="left")
        fit_indices = fit_order[fit_lo:fit_hi]
        fit_prior[fit_indices] = prior[
            fit_active[fit_indices] - start, y_u[fit_indices]
        ]

        score_lo = np.searchsorted(score_active[score_order], start, side="left")
        score_hi = np.searchsorted(score_active[score_order], stop, side="left")
        score_indices = score_order[score_lo:score_hi]
        score_prior[score_indices] = prior[
            score_active[score_indices] - start, y_score[score_indices]
        ]

    sum_count_prior = np.bincount(pair_u, weights=counts * fit_prior, minlength=q * q)
    denom = n_pair + smoothing
    norm_by_pair = (
        sum_count_sq + 2.0 * smoothing * sum_count_prior + smoothing**2 * prior_sq
    ) / (denom**2)

    score_triple = pair_score * q + y_score
    loc = np.searchsorted(unique, score_triple)
    seen = loc < len(unique)
    seen[seen] &= unique[loc[seen]] == score_triple[seen]
    observed = np.zeros(len(score_triple), dtype=np.float64)
    observed[seen] = counts[loc[seen]]
    actual = (observed + smoothing * score_prior) / denom[pair_score]
    return _brier_from_predictions(actual, norm_by_pair[pair_score])


def inverse_likelihood_pair_profile(
    tokens: np.ndarray,
    q: int,
    lag_a: int,
    lag_b: int,
    max_tokens: int = 1_000_000,
    smoothing: float = 8.0,
    max_iter: int = 100,
    tolerance: float = 1e-9,
    positions: np.ndarray | None = None,
    fold_ids: np.ndarray | None = None,
) -> dict:
    """Degree-0/1/2 predictive decomposition for two categorical text lags."""
    if lag_a == lag_b or min(lag_a, lag_b) < 1:
        raise ValueError("lags must be distinct positive integers")
    tok = np.asarray(tokens, dtype=np.int64)
    if np.any(tok < 0) or np.any(tok >= q):
        raise ValueError("tokens outside declared alphabet")
    lag_a, lag_b = sorted((int(lag_a), int(lag_b)))
    if positions is None:
        start = lag_b
        stop = min(len(tok), start + int(max_tokens))
        selected_positions = np.arange(start, stop)
    else:
        selected_positions = np.asarray(positions, dtype=np.int64)
        if selected_positions.ndim != 1:
            raise ValueError("positions must be one-dimensional")
        if (
            np.any(selected_positions < lag_b)
            or np.any(selected_positions >= len(tok))
            or np.any(np.diff(selected_positions) <= 0)
        ):
            raise ValueError("positions must be strictly increasing valid targets")
    if len(selected_positions) < 100:
        raise ValueError("too few positions for a cross-fitted profile")
    a = tok[selected_positions - lag_a]
    b = tok[selected_positions - lag_b]
    y = tok[selected_positions]
    if fold_ids is None:
        fold = np.arange(len(y)) & 1
    else:
        fold = np.asarray(fold_ids, dtype=np.int64)
        if fold.shape != (len(y),) or np.any((fold != 0) & (fold != 1)):
            raise ValueError("fold_ids must be aligned binary fold labels")

    records = []
    for fit_fold in (0, 1):
        fit = fold == fit_fold
        score = ~fit
        p0 = np.bincount(y[fit], minlength=q).astype(np.float64) / fit.sum()
        baseline_actual = p0[y[score]]
        baseline_norm = np.full(score.sum(), float(p0 @ p0))
        baseline = _brier_from_predictions(baseline_actual, baseline_norm)
        intercept, g_a, g_b, iterations = _additive_fit(
            a[fit], b[fit], y[fit], q, max_iter, tolerance
        )
        additive = _score_additive(intercept, g_a, g_b, a[score], b[score], y[score])
        pair = _score_pair_lookup(
            a[fit],
            b[fit],
            y[fit],
            a[score],
            b[score],
            y[score],
            q,
            smoothing,
            intercept,
            g_a,
            g_b,
        )
        records.append(
            {
                "fit_fold": fit_fold,
                "baseline_brier": baseline,
                "degree1_additive_brier": additive,
                "degree2_pair_brier": pair,
                "backfit_iterations": iterations,
            }
        )

    baseline = float(np.mean([r["baseline_brier"] for r in records]))
    additive = float(np.mean([r["degree1_additive_brier"] for r in records]))
    pair = float(np.mean([r["degree2_pair_brier"] for r in records]))
    additive_gain = baseline - additive
    interaction_gain = additive - pair
    total_gain = baseline - pair
    positive_degree1 = max(0.0, additive_gain)
    positive_degree2 = max(0.0, interaction_gain)
    positive_gain = positive_degree1 + positive_degree2
    concentration = variance_concentration(baseline, additive, pair)
    spectrum = conditional_fourier_spectrum(baseline, additive, pair, q)
    return {
        "lags": [lag_a, lag_b],
        "n_positions": len(y),
        "q": q,
        "baseline_brier": baseline,
        "degree1_additive_brier": additive,
        "degree2_pair_brier": pair,
        "degree1_gain": additive_gain,
        "degree2_incremental_gain": interaction_gain,
        "total_pair_gain": total_gain,
        "positive_hierarchical_gain": positive_gain,
        "effective_degree": (
            (positive_degree1 + 2.0 * positive_degree2) / positive_gain
            if positive_gain > 0
            else None
        ),
        "interaction_fraction": positive_degree2 / positive_gain
        if positive_gain > 0
        else None,
        "variance_concentration": concentration,
        "conditional_fourier_spectrum": spectrum,
        "folds": records,
        "smoothing": smoothing,
        "basis": "inverse-likelihood categorical functional ANOVA; arXiv:2603.02673 Def. 3.1",
        "solver": "cross-fitted nested L2 projection; Definition-3.1 column-span equivalent",
    }


def text_inverse_likelihood_profile(
    tokens: np.ndarray,
    q: int = 256,
    lags: tuple[int, ...] = (1, 2, 4, 8, 16),
    max_tokens: int = 1_000_000,
    positions: np.ndarray | None = None,
    fold_ids: np.ndarray | None = None,
) -> dict:
    pairs = [
        inverse_likelihood_pair_profile(
            tokens,
            q,
            a,
            b,
            max_tokens=max_tokens,
            positions=positions,
            fold_ids=fold_ids,
        )
        for a, b in combinations(lags, 2)
    ]
    useful = [
        pair
        for pair in pairs
        if pair["variance_concentration"]["pair_function_variance"] > 0
    ]
    best = (
        max(
            useful,
            key=lambda pair: pair["variance_concentration"]["pair_function_variance"],
        )
        if useful
        else None
    )
    return {
        "q": q,
        "lags": list(lags),
        "max_tokens": max_tokens,
        "pairs": pairs,
        "best_pair_by_function_variance": best["lags"] if best else None,
        "best_pair_by_total_gain": best["lags"] if best else None,
        "best_pair_total_gain": best["total_pair_gain"] if best else 0.0,
        "best_pair_positive_hierarchical_gain": (
            best["positive_hierarchical_gain"] if best else 0.0
        ),
        "effective_degree_best_pair": best["effective_degree"] if best else None,
        "concentration_leq_1_best_pair": (
            best["variance_concentration"]["concentration_leq_1"] if best else None
        ),
        "tail_above_degree_1_best_pair": (
            best["variance_concentration"]["tail_above_degree_1"] if best else None
        ),
        "pair_function_variance_best_pair": (
            best["variance_concentration"]["pair_function_variance"] if best else 0.0
        ),
        "primary_degree_metric": "cumulative variance concentration",
        "max_positive_degree2_gain": max(
            0.0, max((p["degree2_incremental_gain"] for p in pairs), default=0.0)
        ),
        "max_interaction_fraction": max(
            (
                p["interaction_fraction"]
                for p in useful
                if p["interaction_fraction"] is not None
            ),
            default=None,
        ),
        "paper": "https://arxiv.org/abs/2603.02673",
        "memory_strategy": (
            "pair priors evaluated in active-pair chunks; no q-by-q-by-q array"
        ),
    }
