"""L0 — truncated Fourier reconstruction with the C_D^{-1} scaling.

This is the algorithm of the paper's low-degree learning theorem
(`thm:learning-low-degree`): estimate dataset coefficients
    fhat_D(alpha) = (1/m) sum_j f(x_j) conj(chi_alpha(x_j)),  |alpha| <= d,
and output the SCALED reconstruction
    h(x) = C_D^{-1} sum_{|alpha|<=d} fhat_D(alpha) chi_alpha(x),
where C_D = q^n / |D| is the density constant.  The scaling is essential: the
unscaled plug-in reconstruction double-counts aliased characters (paper example:
half-cube, f = x_2 -> unscaled returns 2 f).
"""

from __future__ import annotations

import itertools

import numpy as np


def low_degree_indices(q: int, n: int, d: int) -> np.ndarray:
    """All multi-indices alpha in Z_q^n with categorical degree <= d.

    Returns array of shape (N_d, n), N_d = sum_{k<=d} C(n,k) (q-1)^k.
    """
    assert 0 <= d <= n
    idx = np.array(list(itertools.product(range(q), repeat=n)), dtype=np.int64)
    deg = np.count_nonzero(idx, axis=1)
    return idx[deg <= d]


class SpectralLearner:
    def __init__(self, q: int, n: int, d: int):
        self.q = q
        self.n = n
        self.d = d
        self.indices = low_degree_indices(q, n, d)
        self.coeffs: np.ndarray | None = None
        self.C_D: float | None = None

    @property
    def N_d(self) -> int:
        return self.indices.shape[0]

    def _characters(self, xs: np.ndarray) -> np.ndarray:
        """chi_alpha(x) table, shape (N_d, m)."""
        xs = np.asarray(xs, dtype=np.int64) % self.q
        phase = (self.indices @ xs.T) % self.q
        return np.exp(2j * np.pi / self.q) ** phase

    def fit(self, xs: np.ndarray, ys: np.ndarray, dataset_size: int) -> "SpectralLearner":
        """Fit from m labeled samples (x_j, f(x_j)) drawn uniformly from D.

        ys: (m,) real/complex scalar targets, or (m, out_dim) vector targets.
        dataset_size: |D| (known in the theorem; gives C_D = q^n / |D|).
        """
        xs = np.asarray(xs, dtype=np.int64)
        ys = np.asarray(ys)
        assert xs.ndim == 2 and xs.shape[1] == self.n
        m = xs.shape[0]
        chi = self._characters(xs)  # (N_d, m)
        self.coeffs = (chi.conj() @ ys) / m
        self.C_D = self.q**self.n / float(dataset_size)
        return self

    def predict(self, xs: np.ndarray) -> np.ndarray:
        """Scaled truncated reconstruction at xs."""
        assert self.coeffs is not None, "fit() first"
        chi = self._characters(xs)  # (N_d, m)
        return (chi.T @ self.coeffs) / self.C_D
