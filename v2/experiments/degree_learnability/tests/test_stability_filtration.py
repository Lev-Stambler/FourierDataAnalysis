"""M3 tests (PLAN §12.4 tests 3-4): stability bounds, filtration telescoping,
order-averaging transform, and D-native mask-refill ground truth."""

import numpy as np
import pytest

from dlx.families import F2SubsetSum
from dlx.profiles import (
    averaged_filtration_uniform_n,
    exact_filtration_uniform,
    exact_stability_mask_refill,
    exact_stability_uniform_from_W,
    levels_from_averaged_filtration,
    order_mixing_matrix,
    sampled_stability_mask_refill,
    sampled_stability_uniform,
    tail_bounds,
)
from dlx.domains import fhat_enumerated, level_weights


# ---------------------------------------------------------------- tail bounds
def test_tail_bounds_contain_truth_for_exact_stability():
    # arbitrary level weights; exact Stab => bounds must contain the true tail
    W = np.array([0.3, 0.4, 0.2, 0.08, 0.02])
    B = W.sum()
    for rho in (0.25, 0.5, 0.75, 0.9):
        stab = exact_stability_uniform_from_W(W, rho)
        for d in range(len(W)):
            lo, up = tail_bounds(B, stab, rho, d)
            truth = W[d + 1 :].sum()
            assert lo - 1e-9 <= truth <= up + 1e-9, (rho, d, lo, truth, up)


def test_tail_bounds_with_sampled_noise_still_sound_mostly():
    """With MC noise the bounds are estimates; check algebra on perturbed stab."""
    W = np.array([0.5, 0.3, 0.15, 0.05])
    B = W.sum()
    rho = 0.5
    stab = exact_stability_uniform_from_W(W, rho)
    lo, up = tail_bounds(B, stab * 1.01, rho, 1)  # slightly high stab estimate
    assert lo >= 0.0 and up >= lo


# ---------------------------------------------------------------- uniform stab
def test_sampled_stability_uniform_matches_exact():
    rng = np.random.default_rng(0)
    q, n = 4, 4
    table = rng.standard_normal((q,) * n)
    W = level_weights(fhat_enumerated(table))

    def f_batch(xs):
        return table[tuple(xs[:, j] for j in range(n))]

    for rho in (0.25, 0.5, 0.75, 0.9):
        est = sampled_stability_uniform(f_batch, q, n, rho, m=40_000, rng=rng)
        exact = exact_stability_uniform_from_W(W, rho)
        assert abs(est.real - exact) < 5 * np.sqrt(1.0 / 40_000) * 4, (rho, est, exact)
        assert abs(est.imag) < 5 * np.sqrt(1.0 / 40_000) * 4


# ---------------------------------------------------------------- filtration
def test_filtration_telescopes_to_variance():
    rng = np.random.default_rng(1)
    q, n = 3, 4
    # complex vector-valued target
    table = rng.standard_normal((q,) * n + (2,)) + 1j * rng.standard_normal((q,) * n + (2,))
    Ef = table.mean(axis=tuple(range(n)))
    var = float(np.real((np.abs(table) ** 2).sum(axis=-1).mean() - np.sum(Ef * np.conj(Ef)).real))
    for order in ([0, 1, 2, 3], [2, 0, 3, 1], [3, 3 - 1, 1, 0]):
        inc = exact_filtration_uniform(table, q, order)
        assert abs(inc.sum() - var) < 1e-9, order


def test_order_mixing_matrix_and_inversion():
    n = 4
    M = order_mixing_matrix(n)
    # each level-t character is counted exactly once across steps: column sums = 1
    assert np.allclose(M.sum(axis=0), 1.0)
    # diagonal positive, lower triangular
    assert np.all(np.diag(M) > 0) and np.allclose(np.triu(M, 1), 0.0)

    # averaged increments == M @ W, and inversion recovers W
    rng = np.random.default_rng(2)
    q = 3
    table = rng.standard_normal((q,) * n)
    W = level_weights(fhat_enumerated(table))
    avg = averaged_filtration_uniform_n(table, q, n)  # all 24 orderings
    # level 0 is absorbed in the baseline; the transform acts on W[1:]
    assert np.allclose(avg, M @ W[1:], atol=1e-9)
    Wrec = levels_from_averaged_filtration(avg)
    assert np.allclose(Wrec, W[1:], atol=1e-8)


# ---------------------------------------------------------------- D-native
def test_mask_refill_sampled_vs_exact_ground_truth():
    fam = F2SubsetSum(q=4, L=4, lags=(1, 2), eta=0.2)
    rng = np.random.default_rng(3)
    rows = np.arange(fam.q**fam.L)
    ctxs = np.array([(rows // fam.q ** (fam.L - 1 - j)) % fam.q for j in range(fam.L)]).T
    F = fam.next_token_dist_batch(ctxs)  # (q^L, q)

    for rho in (0.4, 0.8):
        exact = exact_stability_mask_refill(fam, F, rho)
        est = sampled_stability_mask_refill(fam, fam.next_token_dist_batch, rho, m=1500, rng=rng)
        scale = abs(exact) + 1e-12
        assert abs(est - exact) / scale < 0.08, (rho, est, exact)
