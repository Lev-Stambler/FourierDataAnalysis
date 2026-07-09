"""The actual headline algorithm: Goldreich--Levin from context conditioning
(``thm:context-gl``).  In-domain only --- it touches nothing but datapoints.

This *runs the pruned tree search*: start from the empty bucket, at each level
split every live bucket on the next coordinate, estimate the conditional bucket
weight ``Psi`` of each child, keep children with ``Psi >= tau^2/4``, descend, and
output the surviving singletons (the heavy dataset coefficients, the list ``L``).

Two estimators for ``Psi``:
- ``psi_exact``    : computes ``Psi(S|J)`` exactly from the fibers (ground truth).
- ``psi_sampled``  : estimates it using ONLY ``SAMP`` (draw ``x ~ D``, read its
                     context ``z = x_Jbar``) and ``CSAMP`` (draw ``x' ~ D_z``),
                     via ``lem:psi-estimator`` --- i.e. the real access model, no
                     membership tests, no off-data evaluation.

    uv run python -m fda_exp.context_gl
"""

from __future__ import annotations

import numpy as np

from .datasets import tiny_stories
from .spectrum import coeffs_at, dataset_coeffs, points_to_index, popcount


# ---------------------------------------------------------------- Psi estimators

def psi_exact(idx, f, m, S, k):
    """Exact ``Psi(S | J_k)`` with ``J_k = [k]`` (coords ``0..k-1``)."""
    ctx = idx >> k
    jpart = idx & ((1 << k) - 1)
    chi = 1 - 2 * (popcount(S & jpart) & 1)     # chi_S(x_J) = (-1)^{|S cap jpart|}
    w = f * chi
    uniq, inv = np.unique(ctx, return_inverse=True)
    sums = np.zeros(len(uniq)); np.add.at(sums, inv, w)
    cnts = np.zeros(len(uniq)); np.add.at(cnts, inv, 1.0)
    vbar = sums / cnts
    return float(np.sum((cnts / m) * vbar ** 2))


def _level_groups(idx, k):
    """Precompute (once per level) the fiber structure for ``J_k = [k]`` so that
    a SAMP draw + a context-matched CSAMP draw are fully vectorizable."""
    ctx = idx >> k
    uniq, inv = np.unique(ctx, return_inverse=True)     # inv[r] = fiber id of row r
    order = np.argsort(inv, kind="stable")
    sorted_rows = np.arange(len(idx))[order]            # rows grouped by fiber
    gsizes = np.bincount(inv)
    gstart = np.concatenate([[0], np.cumsum(gsizes)[:-1]])
    return inv, sorted_rows, gstart, gsizes


def _a_of_S(idx, f, S, k):
    """The per-row statistic ``a_r = f(x_r) * chi_S(x_r|_J)`` for ``J = [k]``."""
    jpart = idx & ((1 << k) - 1)
    return f * (1 - 2 * (popcount(S & jpart) & 1))


def psi_sampled_vec(a, inv, sorted_rows, gstart, gsizes, n_exp, rng):
    """Vectorized SAMP+CSAMP estimate of ``Psi(S|J) = E_z E_{x,x'~D_z}[a_x a_{x'}]``.

    One experiment = one SAMP draw ``x`` (giving its context) + one CSAMP draw
    ``x' ~ D_z``.  No membership test, no off-data evaluation.
    """
    m = len(a)
    rr = rng.integers(0, m, size=n_exp)                 # SAMP: x
    gid = inv[rr]
    off = (rng.random(n_exp) * gsizes[gid]).astype(np.int64)
    rp = sorted_rows[gstart[gid] + off]                 # CSAMP: x' ~ same fiber
    return float(np.mean(a[rr] * a[rp]))


# ---------------------------------------------------------------- the tree search

def context_gl_search(D, fD, tau, mode="exact", n_exp=2000, seed=0, verbose=False):
    """Run ``thm:context-gl`` and return the sorted list ``L`` of heavy-coefficient
    subset masks it recovers.  ``mode='exact'`` uses ground-truth ``Psi``;
    ``mode='sampled'`` uses SAMP+CSAMP only (vectorized)."""
    m, n = D.shape
    idx = points_to_index(D)
    f = np.asarray(fD, dtype=np.float64)
    thresh = tau * tau / 4.0
    rng = np.random.default_rng(seed)

    live = [0]                                   # bucket S=empty at level 0
    experiments = 0
    for k in range(n):                           # split coordinate k, go to level k+1
        kk = k + 1
        if mode == "sampled":
            grp = _level_groups(idx, kk)         # precompute once per level
        nxt = []
        for S in live:
            for child in (S, S | (1 << k)):
                if mode == "exact":
                    psi = psi_exact(idx, f, m, child, kk)
                else:
                    a = _a_of_S(idx, f, child, kk)
                    psi = psi_sampled_vec(a, *grp, n_exp, rng)
                    experiments += n_exp
                if psi >= thresh:
                    nxt.append(child)
        live = nxt
        if verbose:
            print(f"  level {kk:2d}: {len(live):5d} live buckets")
    return sorted(set(live)), experiments


def brute_force_heavy(D, fD, tau):
    """Ground truth: every S with ``|fhat_D(S)| >= tau/2`` (what the search must return)."""
    n = D.shape[1]
    fhat = dataset_coeffs(D, fD, n)
    heavy = np.nonzero(np.abs(fhat) >= tau / 2)[0]
    return sorted(int(s) for s in heavy), fhat


# ---------------------------------------------------------------- demo / verification

def main():
    tau = 0.25
    D, fD, _ = tiny_stories(window=4, bits_per_token=4, n_sentences=8000, seed=0)
    m, n = D.shape
    print(f"tiny-stories (is-verb-next)  n={n}  |D|={m}  tau={tau}")

    heavy, fhat = brute_force_heavy(D, fD, tau)
    print(f"\nground truth: {len(heavy)} coefficients with |fhat_D(S)| >= tau/2")

    L_exact, _ = context_gl_search(D, fD, tau, mode="exact", verbose=True)
    print(f"\nGL (exact Psi):   recovered |L|={len(L_exact)}   "
          f"matches ground truth? {set(L_exact) == set(heavy)}")

    L_samp, exps = context_gl_search(D, fD, tau, mode="sampled", n_exp=4000, seed=1)
    tp = len(set(L_samp) & set(heavy))
    print(f"GL (SAMP+CSAMP):  recovered |L|={len(L_samp)}   "
          f"found {tp}/{len(heavy)} true heavy   "
          f"(spurious {len(set(L_samp) - set(heavy))})   ~{exps} experiments")
    print("\nNote: sampled GL uses ONLY draws from D and context-conditioned draws"
          " -- no membership test, no off-data evaluation of f.")


if __name__ == "__main__":
    main()
