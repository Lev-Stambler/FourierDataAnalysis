"""Noise-stability profiles and tail bounds (PLAN §4.3, §7).

Convention: rho is the KEEP probability.  Uniform-resample noise replaces each
masked coordinate by an independent uniform draw; characters are eigenfunctions
with eigenvalue rho^k, so under the uniform law
    Stab_rho(f) = sum_k rho^k W^k,      B = sum_k W^k = E||f||^2.
Combined with B, one stability scalar gives two-sided tail bounds:
    B - rho^{-d} Stab_rho  <=  W^{>d}  <=  (B - Stab_rho) / (1 - rho^{d+1}).

Two noise operators are implemented:
  * uniform-resample      — the operator for which the level-weight identity
                            holds exactly (used for calibration and bounds);
  * D-native mask-refill  — mask positions and refill from the family's exact
                            conditional law (the real-data oracle; its spectrum
                            is intrinsic to D, not the uniform level weights).
"""

from __future__ import annotations

import numpy as np

__all__ = [
    "tail_bounds",
    "sampled_stability_uniform",
    "exact_stability_uniform_from_W",
    "sampled_stability_mask_refill",
    "exact_stability_mask_refill",
]


def tail_bounds(B: float, stab: float, rho: float, d: int) -> tuple[float, float]:
    """(lower, upper) bounds on W^{>d} from budget B and Stab_rho at keep-prob rho."""
    assert 0.0 < rho < 1.0
    lower = B - stab / rho**d
    upper = (B - stab) / (1.0 - rho ** (d + 1))
    return float(max(lower, 0.0)), float(upper)


def exact_stability_uniform_from_W(W: np.ndarray, rho: float) -> float:
    """sum_k rho^k W^k."""
    k = np.arange(W.shape[0])
    return float(np.sum(W * rho**k))


def sampled_stability_uniform(f_batch, q: int, n: int, rho: float, m: int,
                              rng: np.random.Generator) -> complex:
    """Paired estimator of Stab_rho under uniform law with uniform-resample noise.

    f_batch: callable (m, n) int -> (m, out_dim) complex/real values.
    Estimates E <f(x), f(y)> with y = keep(x) where each coordinate is kept with
    probability rho and resampled uniformly otherwise.
    """
    xs = rng.integers(0, q, size=(m, n))
    keep = rng.random((m, n)) < rho
    ys = np.where(keep, xs, rng.integers(0, q, size=(m, n)))
    fx = f_batch(xs)
    fy = f_batch(ys)
    pairs = np.sum(fx * np.conj(fy), axis=-1) if fx.ndim > 1 else fx * np.conj(fy)
    return complex(pairs.mean())


def sampled_stability_mask_refill(family, f_batch, rho: float, m: int,
                                  rng: np.random.Generator) -> complex:
    """Paired estimator under the family law with D-native mask-refill noise.

    Draws x as a fresh length-L sequence from the family (the canonical joint
    law that `joint_table`/`conditional_sample` refer to), masks each position
    independently with probability 1-rho, and refills via the family's exact
    conditional sampler when the joint is enumerable (else causal refill).
    """
    L = family.L
    total = 0.0 + 0.0j
    for _ in range(m):
        x = family.sample(L, rng)
        keep = rng.random(L) < rho
        y, _exact = family.conditional_sample(keep, x, rng)
        fx = f_batch(x[None, :])[0]
        fy = f_batch(y[None, :])[0]
        total += complex(np.sum(fx * np.conj(fy)))
    return total / m


def exact_stability_mask_refill(family, f_values: np.ndarray, rho: float) -> complex:
    """Ground truth of the D-native mask-refill stability via the joint table.

    f_values: (q^L, out_dim) values on all sequences in row encoding.
    Stab = E_K sum_a p_K(a) ||E[f | x_K = a]||^2, K ~ keep-each(rho).
    Cost ~ (q+1)^L; enumerable tiers only.
    """
    p = family.joint_table()
    q, L = family.q, family.L
    assert f_values.shape[0] == q**L
    total = 0.0 + 0.0j
    for Kmask in range(1 << L):
        kept = [j for j in range(L) if (Kmask >> j) & 1]
        k = len(kept)
        prob = rho**k * (1.0 - rho) ** (L - k)
        if prob == 0.0:
            continue
        # marginalize the joint over free positions, keyed by kept digits
        table = p.reshape((q,) * L)  # axis j <-> position j
        # first accumulate mass and weighted f over the kept assignments
        axes_free = [j for j in range(L) if j not in kept]
        mass = table
        fview = f_values.reshape((q,) * L + f_values.shape[1:])
        fmass = fview * table[..., None]
        for ax in sorted(axes_free, reverse=True):
            mass = mass.sum(axis=ax)
            fmass = fmass.sum(axis=ax)
        # conditional mean where mass > 0
        with np.errstate(invalid="ignore", divide="ignore"):
            cond = np.where(mass[..., None] > 0, fmass / np.maximum(mass, 1e-300)[..., None], 0.0)
        total += prob * complex(np.sum(mass[..., None] * cond * np.conj(cond)))
    return total
