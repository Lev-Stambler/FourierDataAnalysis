"""Categorical Fourier utilities on Z_q^n (dataset-native characters).

Convention (matches v2 paper):
    omega_q   = exp(2 pi i / q)
    chi_a(x)  = omega_q^{<a, x>}                    (characters, unit modulus)
    fhat(k)   = E_x[f(x) conj(chi_k(x))]            (uniform-law coefficient)
              = (1/q^n) sum_x f(x) omega_q^{-<k,x>}

With this sign convention the forward transform is `numpy.fft.fftn(values) / q^n`
and exact reconstruction is `values = q^n * numpy.fft.ifftn(fhat)`.
"""

from __future__ import annotations

import numpy as np

__all__ = [
    "omega",
    "character_matrix",
    "fhat_enumerated",
    "invert_enumerated",
    "multi_degree",
    "level_weights",
    "orthogonality_check",
]


def omega(q: int) -> complex:
    return np.exp(2j * np.pi / q)


def character_matrix(q: int, alphas: np.ndarray, xs: np.ndarray) -> np.ndarray:
    """chi_a(x) for alphas (A, n) and xs (B, n) -> matrix (A, B)."""
    alphas = np.asarray(alphas, dtype=np.int64) % q
    xs = np.asarray(xs, dtype=np.int64) % q
    phase = (alphas @ xs.T) % q
    return omega(q) ** phase


def fhat_enumerated(values: np.ndarray) -> np.ndarray:
    """Uniform Fourier coefficients of a fully enumerated table.

    values has shape (q, q, ..., q) (n axes), scalar-valued on Z_q^n.
    Returns fhat with the same shape, fhat[k] = E_x[f(x) conj(chi_k(x))].
    Vector-valued targets: apply along a trailing axis by looping/`moveaxis`.
    """
    n = values.ndim
    size = values.size
    return np.fft.fftn(values, axes=range(n)) / size


def invert_enumerated(fhat: np.ndarray) -> np.ndarray:
    """Exact reconstruction f(x) = sum_k fhat(k) chi_k(x)."""
    return np.fft.ifftn(fhat, axes=range(fhat.ndim)) * fhat.size


def multi_degree(alpha: np.ndarray, q: int | None = None) -> int:
    """Categorical degree: number of nonzero coordinates (one token = one variable)."""
    alpha = np.asarray(alpha)
    if q is not None:
        alpha = alpha % q
    return int(np.count_nonzero(alpha))


def level_weights(coeffs: np.ndarray) -> np.ndarray:
    """Level weights W^k = sum_{deg(alpha)=k} |fhat(alpha)|^2 for an enumerated table.

    coeffs has shape (q,)*n. Returns array of length n+1.
    Real-valued check: sum_k W^k == mean |f|^2 (Parseval, uniform law).
    """
    n = coeffs.ndim
    q = coeffs.shape[0]
    assert all(s == q for s in coeffs.shape), "level_weights expects a cubic (q,)*n table"
    idx = np.indices(coeffs.shape).reshape(n, -1)  # (n, q^n) multi-indices
    deg = np.count_nonzero(idx, axis=0)
    mass = np.abs(coeffs.reshape(-1)) ** 2
    W = np.zeros(n + 1, dtype=np.float64)
    np.add.at(W, deg, mass)
    return W


def orthogonality_check(q: int, n: int, rng: np.random.Generator, trials: int = 8,
                        atol: float = 1e-10) -> bool:
    """Brute-force check (1/q^n) sum_x chi_a(x) conj(chi_b(x)) == delta_{a,b}.

    Uses sampled pairs (a, b) and full enumeration over x; fine for small q^n.
    """
    total = q**n
    xs = np.array(np.unravel_index(np.arange(total), (q,) * n)).T  # (q^n, n)
    for _ in range(trials):
        a = rng.integers(0, q, size=n)
        b = rng.integers(0, q, size=n)
        vals = character_matrix(q, a[None], xs)[0] * np.conj(character_matrix(q, b[None], xs)[0])
        inner = vals.mean()
        target = 1.0 if np.array_equal(a % q, b % q) else 0.0
        if abs(inner - target) > atol:
            return False
    return True
