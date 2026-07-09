"""E5 --- on-dataset vs global degree.

The paper's thesis: a trained model can be *low-degree over the data* even when it
is not low-degree globally.  We compare two exact degree profiles:

- ``on-dataset``: normalized level weights
  ``Wbar^k_D[f] = sum_{#alpha=k} fbreve_D(alpha)^2`` with
  ``fbreve_D(alpha) = C_D^{-1/2} fhat_D(alpha)`` (``defn:normalized-coeffs``).
  By ``lem:dataset-parseval`` these sum to ``E_D[f^2]``.
- ``global``: ``W^k[f] = sum_{#alpha=k} fhat(alpha)^2`` over the uniform cube.
  These sum to ``E_unif[f^2]``.

Both are enumerated exactly over all ``2^n`` coefficients (small-``n`` track).
"""

from __future__ import annotations

import numpy as np

from .spectrum import (
    dataset_coeffs,
    density_constant,
    full_coeffs,
    popcount_table,
)


def dataset_level_weights(D: np.ndarray, fD: np.ndarray, n: int,
                          normalized: bool = True) -> np.ndarray:
    """On-dataset level-``k`` weights, ``k = 0..n``.

    With ``normalized=True`` these are ``Wbar^k_D`` and sum to ``E_D[f^2]``.
    """
    fhat = dataset_coeffs(D, fD, n)
    weights = fhat * fhat
    if normalized:
        weights = weights / density_constant(n, D.shape[0])
    deg = popcount_table(n)
    W = np.zeros(n + 1)
    np.add.at(W, deg, weights)
    return W


def global_level_weights(f_full: np.ndarray, n: int) -> np.ndarray:
    """Global (uniform-measure) level-``k`` weights; sum to ``E_unif[f^2]``."""
    fhat = full_coeffs(f_full, n)
    deg = popcount_table(n)
    W = np.zeros(n + 1)
    np.add.at(W, deg, fhat * fhat)
    return W


def degree_report(D: np.ndarray, fD: np.ndarray, f_full, n: int) -> dict:
    """On-dataset vs global degree profiles, plus their energy-normalized forms
    (fraction of spectral mass at each degree) and the mean degree of each."""
    on = dataset_level_weights(D, fD, n, normalized=True)
    out = dict(
        on_dataset=on.tolist(),
        on_dataset_energy=float(on.sum()),
        on_dataset_fraction=(on / on.sum()).tolist() if on.sum() > 0 else None,
        on_dataset_mean_degree=_mean_degree(on),
    )
    if f_full is not None:
        gl = global_level_weights(np.asarray(f_full, dtype=np.float64), n)
        out.update(
            global_=gl.tolist(),
            global_energy=float(gl.sum()),
            global_fraction=(gl / gl.sum()).tolist() if gl.sum() > 0 else None,
            global_mean_degree=_mean_degree(gl),
        )
    return out


def _mean_degree(W: np.ndarray) -> float | None:
    total = W.sum()
    if total <= 0:
        return None
    return float((np.arange(len(W)) * W).sum() / total)
