"""These tests encode the paper's theorems.  Passing them is the correctness
gate for the harness (and an independent numerical re-check of the math)."""

from __future__ import annotations

import numpy as np
import pytest

from fda_exp import datasets
from fda_exp.context_profile import context_profile
from fda_exp.degree import dataset_level_weights, global_level_weights
from fda_exp.spectrum import (
    dataset_coeffs,
    density_constant,
    fwht,
    points_to_index,
    popcount_table,
)


def _random_dataset(n, m, seed=0):
    rng = np.random.default_rng(seed)
    D = rng.choice([-1, 1], size=(m, n)).astype(np.int64)
    D = np.unique(D, axis=0)
    fD = rng.uniform(-1, 1, size=D.shape[0])
    return D, fD


# ---- fast transform sanity -------------------------------------------------

def test_fwht_involution():
    rng = np.random.default_rng(1)
    a = rng.standard_normal(1 << 6)
    # fwht is self-inverse up to the factor 2^k
    assert np.allclose(fwht(fwht(a)) / (1 << 6), a)


def test_chi_matches_index_convention():
    # chi_S(x) computed directly must equal (-1)^{popcount(S & idx(x))}
    n = 6
    rng = np.random.default_rng(2)
    D = rng.choice([-1, 1], size=(20, n)).astype(np.int64)
    idx = points_to_index(D)
    pc = popcount_table(n)
    for S in [0b000001, 0b101010, 0b111111]:
        chi_direct = np.prod(D[:, [i for i in range(n) if (S >> i) & 1]], axis=1) if S else np.ones(len(D))
        chi_idx = (-1.0) ** pc[S & idx]
        assert np.allclose(chi_direct, chi_idx)


# ---- Mass Identity and normalized Parseval (prelims) -----------------------

def test_mass_identity():
    n, m = 10, 300
    D, fD = _random_dataset(n, m, seed=3)
    fhat = dataset_coeffs(D, fD, n)
    lhs = float((fhat ** 2).sum())
    rhs = density_constant(n, D.shape[0]) * float(np.mean(fD ** 2))
    assert lhs == pytest.approx(rhs, rel=1e-9)


def test_normalized_parseval_and_level_weights_sum():
    n, m = 12, 500
    D, fD = _random_dataset(n, m, seed=4)
    W = dataset_level_weights(D, fD, n, normalized=True)
    assert W.sum() == pytest.approx(float(np.mean(fD ** 2)), rel=1e-9)


def test_global_level_weights_sum():
    n = 8
    rng = np.random.default_rng(5)
    f_full = rng.uniform(-1, 1, size=1 << n)
    W = global_level_weights(f_full, n)
    assert W.sum() == pytest.approx(float(np.mean(f_full ** 2)), rel=1e-9)


# ---- Level-mass identity  R_k = 2^k c_k / m   for f == 1 -------------------

def test_level_mass_constant_f_equals_2k_ck_over_m():
    n, m = 12, 400
    D, _fD, _ = datasets.uniform_random(n, m, seed=6)
    fD = np.ones(D.shape[0])  # f == 1
    levels = context_profile(D, fD, tau=0.3, exact_cap=12)
    for k, v in levels.items():
        expected = (1 << k) * v["c_k"] / D.shape[0]
        assert v["R_k"] == pytest.approx(expected, rel=1e-9), f"level {k}"


def test_Rk_equals_sum_psi_via_termination():
    # At the top level, R_n = sum_S Psi(S|[n]) = sum_S fhat_D(S)^2 = C_D E_D[f^2].
    n, m = 10, 200
    D, fD = _random_dataset(n, m, seed=7)
    levels = context_profile(D, fD, tau=0.2, exact_cap=n)
    top = levels[n]["R_k"]
    assert top == pytest.approx(density_constant(n, D.shape[0]) * np.mean(fD ** 2), rel=1e-9)


# ---- thm:context-gl regime 2 : the subcube is output-sensitive -------------

def test_subcube_regime_Rk_and_Nk():
    n, fixed = 9, 3  # K = {0,1,2}
    D, _fD, _ = datasets.subcube(n, fixed=fixed, label="const")
    fD = np.ones(D.shape[0])
    levels = context_profile(D, fD, tau=0.5, exact_cap=n)
    for k, v in levels.items():
        k_in_K = min(k, fixed)          # |K cap [k]| since K = {0..fixed-1}
        expected = 1 << k_in_K
        assert v["R_k"] == pytest.approx(expected, rel=1e-9), f"R_{k}"
        assert v["N_k"] == expected, f"N_{k}"  # output size, not 2^k


# ---- lem:blindness : singleton fibers => flat Psi => N_k = 2^k --------------

def test_blindness_flat_buckets():
    n = 8
    # last 5 coordinates distinct across the 8 points -> singleton fibers for k<=3
    rng = np.random.default_rng(8)
    suffix_ids = rng.choice(1 << 5, size=8, replace=False)
    D = np.ones((8, n), dtype=np.int64)
    for r, sid in enumerate(suffix_ids):
        bits = [(sid >> b) & 1 for b in range(5)]
        D[r, 3:] = [1 - 2 * b for b in bits]
    D[:, :3] = rng.choice([-1, 1], size=(8, 3))
    D = np.unique(D, axis=0)
    fD = rng.choice([-1.0, 1.0], size=D.shape[0])  # E_D[f^2] = 1
    levels = context_profile(D, fD, tau=0.5, exact_cap=n)
    for k in range(0, 4):
        assert levels[k]["psi_flat"] is True, f"level {k} should be blind"
        assert levels[k]["N_k"] == (1 << k), f"blind level {k}: N_k should be 2^k"


def test_random_blows_up_subcube_stays_small():
    n, m = 12, 400
    Dr, fr, _ = datasets.uniform_random(n, m, seed=9, label="parity:0,1")
    Ds, fs, _ = datasets.subcube(n, fixed=3, label="const")
    fs = np.ones(Ds.shape[0])
    rand = context_profile(Dr, fr, tau=0.3, exact_cap=12)
    sub = context_profile(Ds, fs, tau=0.3, exact_cap=12)
    rand_Nmax = max(v["N_k"] for v in rand.values())
    sub_Nmax = max(v["N_k"] for v in sub.values())
    assert sub_Nmax <= 8            # 2^{|K|} = 2^3
    assert rand_Nmax >= 64          # random blows toward 2^k
    assert rand_Nmax > sub_Nmax
