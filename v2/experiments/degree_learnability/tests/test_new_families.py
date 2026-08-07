"""F3/F4/F5 families (v2 exponential construction): positivity, determinism,
spectral concentration, planted-vs-enumerated consistency, sampling."""

import numpy as np
import pytest

from dlx.families import F3RandomPoly, F4MixedProfile, F5IID, F5MaxSum
from dlx.profiles import enumerated_profile, planted_profile


@pytest.fixture()
def f3():
    return F3RandomPoly(q=4, L=6, d=2, M=3, amp=0.2, beta=2.0, eta=0.1, draw_seed=5)


@pytest.fixture()
def f4():
    return F4MixedProfile(q=4, L=6, K=3, M=2, r=0.5, beta=2.0, eta=0.1, draw_seed=9)


@pytest.mark.parametrize("fam_name", ["f3", "f4"])
def test_dist_positive_normalized(fam_name, request):
    fam = request.getfixturevalue(fam_name)
    rng = np.random.default_rng(0)
    for _ in range(50):
        ctx = rng.integers(0, fam.q, size=fam.L)
        P = fam.next_token_dist(ctx)
        assert np.all(P >= -1e-12)
        assert abs(P.sum() - 1.0) < 1e-9
    ctxs = rng.integers(0, fam.q, size=(7, fam.L))
    Pb = fam.next_token_dist_batch(ctxs)
    for j in range(7):
        assert np.allclose(Pb[j], fam.next_token_dist(ctxs[j]), atol=1e-10)


@pytest.mark.parametrize("fam_name", ["f3", "f4"])
def test_planted_equals_enumerated_by_construction(fam_name, request):
    fam = request.getfixturevalue(fam_name)
    assert np.allclose(planted_profile(fam), enumerated_profile(fam), atol=1e-12)


def test_f3_spectral_concentration():
    """At moderate beta the non-level-0 mass is dominated by the planted degree."""
    fam = F3RandomPoly(q=4, L=6, d=2, M=3, amp=0.2, beta=2.0, eta=0.1, draw_seed=5)
    W = enumerated_profile(fam)
    nonzero = W[1:].sum()
    assert nonzero > 1e-6
    assert W[fam.d] / nonzero >= 0.5, W
    # f depends only on support coordinates: profile mass above support size is 0
    assert W[len(fam.support_positions()) + 1:].sum() < 1e-9


def test_f4_geometric_decay_shape():
    fam = F4MixedProfile(q=4, L=6, K=3, M=2, r=0.5, beta=1.0, eta=0.0, draw_seed=11)
    W = planted_profile(fam)
    levels = W[1:4]
    target = 0.5 ** np.arange(1, 4)
    corr = np.corrcoef(levels, target)[0, 1]
    assert corr >= 0.9, (levels, corr)


def test_signal_strength_tunable():
    """Larger beta pushes the floor further below uniform (deeper signal)."""
    floors = []
    for beta in (1.0, 8.0):
        fam = F3RandomPoly(q=8, L=16, d=2, M=4, amp=1.0, beta=beta, eta=0.1,
                           draw_seed=5)
        floors.append(fam.entropy_rate())
    assert floors[1] < floors[0] - 0.2, floors


def test_f5_iid():
    fam = F5IID(q=8, L=16)
    x = fam.sample(1000, np.random.default_rng(0))
    assert x.min() >= 0 and x.max() < 8
    assert abs(fam.entropy_rate() - 3.0) < 1e-12
    assert np.allclose(planted_profile(fam), enumerated_profile(fam), atol=1e-9)
    assert abs(fam.next_token_dist(np.zeros(16, dtype=int)).sum() - 1) < 1e-12


def test_f5_maxsum_is_degree_L():
    fam = F5MaxSum(q=4, L=5, eta=0.1)
    W = planted_profile(fam)
    assert W[5] > 0 and abs(W[[0, 5]].sum() - W.sum()) < 1e-12
    assert np.allclose(W, enumerated_profile(fam), atol=1e-9)


def test_f3_determinism_and_conditional():
    kw = dict(q=4, L=6, d=2, M=3, amp=0.2, beta=2.0, eta=0.1, draw_seed=5)
    a = F3RandomPoly(**kw)
    b = F3RandomPoly(**kw)
    assert a.version == b.version
    ra, rb = np.random.default_rng(1), np.random.default_rng(1)
    assert np.array_equal(a.sample(200, ra), b.sample(200, rb))
    x = a.sample(6, np.random.default_rng(2))
    mask = np.zeros(6, dtype=bool); mask[[1, 3]] = True
    y, exact = a.conditional_sample(mask, x, np.random.default_rng(3))
    assert exact and np.array_equal(y[mask], x[mask])
