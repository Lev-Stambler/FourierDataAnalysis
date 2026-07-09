"""Core spectral primitives on the Boolean cube ``{-1,1}^n``.

Conventions
-----------
- A point ``x in {-1,1}^n`` is encoded as an integer index by bits
  ``b_i = (1 - x_i)/2`` (so ``x_i = +1 -> 0`` and ``x_i = -1 -> 1``) with
  ``idx = sum_i b_i 2^i``.  A subset ``S subseteq [n]`` is the bitmask with bit
  ``i`` set iff ``i in S``; then the parity character is
  ``chi_S(x) = prod_{i in S} x_i = (-1)^{sum_{i in S} b_i} = (-1)^{<S, b(x)>}``.
- The natural-ordering fast Walsh--Hadamard transform (``fwht``) of a length-``2^n``
  array ``v`` returns ``H[S] = sum_x (-1)^{<S,b(x)>} v[x] = sum_x chi_S(x) v[x]``.

With ``v`` the *lift* of ``(D, f)`` (``v[x] = f(x)`` on ``D``, ``0`` off ``D``),
``H[S] = |D| * fhat_D(S)`` where ``fhat_D(S) = E_{x~D}[f(x) chi_S(x)]``.
"""

from __future__ import annotations

import numpy as np


def fwht(a: np.ndarray) -> np.ndarray:
    """Natural-ordering fast Walsh--Hadamard transform (unnormalized).

    Returns ``H`` with ``H[S] = sum_x chi_S(x) a[x]``.
    """
    a = np.asarray(a, dtype=np.float64).ravel().copy()
    n = a.shape[0]
    if n == 0 or (n & (n - 1)) != 0:
        raise ValueError(f"length must be a power of two, got {n}")
    h = 1
    while h < n:
        a = a.reshape(-1, 2 * h)
        left = a[:, :h].copy()
        right = a[:, h:].copy()
        a[:, :h] = left + right
        a[:, h:] = left - right
        a = a.reshape(-1)
        h *= 2
    return a


def points_to_index(D: np.ndarray) -> np.ndarray:
    """Map an ``(m, n)`` array of ``+/-1`` points to integer cube indices."""
    D = np.asarray(D)
    b = ((1 - D) // 2).astype(np.int64)
    n = D.shape[1]
    weights = (1 << np.arange(n, dtype=np.int64))
    return b @ weights


def popcount_table(n: int) -> np.ndarray:
    """Hamming weight (degree ``#S``) of every subset index ``0..2^n - 1``."""
    size = 1 << n
    p = np.zeros(size, dtype=np.int64)
    for i in range(1, size):
        p[i] = p[i >> 1] + (i & 1)
    return p


def lift(D: np.ndarray, fD: np.ndarray, n: int) -> np.ndarray:
    """The lift ``D o f``: a length-``2^n`` vector, ``f`` on ``D`` and ``0`` elsewhere."""
    idx = points_to_index(D)
    v = np.zeros(1 << n, dtype=np.float64)
    np.add.at(v, idx, np.asarray(fD, dtype=np.float64))
    return v


def dataset_coeffs(D: np.ndarray, fD: np.ndarray, n: int) -> np.ndarray:
    """All dataset Fourier coefficients ``fhat_D(S) = E_{x~D}[f chi_S]`` (length ``2^n``)."""
    m = D.shape[0]
    return fwht(lift(D, fD, n)) / m


def full_coeffs(f_full: np.ndarray, n: int) -> np.ndarray:
    """All *global* (uniform-measure) coefficients ``fhat(S)`` of ``f`` on the whole cube.

    ``f_full`` is a length-``2^n`` array giving ``f(x)`` at every cube index.
    """
    f_full = np.asarray(f_full, dtype=np.float64)
    return fwht(f_full) / (1 << n)


def density_constant(n: int, m: int) -> float:
    """``C_D = |T|^n / |D| = 2^n / m`` for the binary alphabet."""
    return (1 << n) / m


# --- table-free vectorized popcount (so large n never allocates a 2^n table) ---
_POP16 = None


def popcount(a):
    """Vectorized Hamming weight for integer arrays up to 48 bits."""
    global _POP16
    if _POP16 is None:
        _POP16 = np.array([bin(i).count("1") for i in range(1 << 16)], dtype=np.int64)
    a = np.asarray(a, dtype=np.int64)
    return _POP16[a & 0xFFFF] + _POP16[(a >> 16) & 0xFFFF] + _POP16[(a >> 32) & 0xFFFF]


def coeffs_at(D, fD, S_masks):
    """Dataset coefficients ``fhat_D(S)`` for a *specific* list of subset masks,
    in ``O(m * |S_masks|)`` --- no ``2^n`` WHT.  ``chi_S(x) = (-1)^{|S cap b(x)|}``."""
    idx = points_to_index(D)
    f = np.asarray(fD, dtype=np.float64)
    m = len(D)
    out = np.empty(len(S_masks), dtype=np.float64)
    for i, S in enumerate(S_masks):
        chi = 1 - 2 * (popcount(int(S) & idx) & 1)
        out[i] = float(np.dot(f, chi) / m)
    return out
