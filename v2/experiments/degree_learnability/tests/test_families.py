"""S0 tier tests: F2 family — interface, determinism, law consistency."""

import numpy as np
import pytest

from dlx.families import F2SubsetSum
from dlx.families.base import _exact_conditional_sample
from dlx.seeding import rng_for, seed_from


@pytest.fixture()
def fam():
    # small enumerable instance (q^L = 8^6 = 262144)
    return F2SubsetSum(q=8, L=6, lags=(1, 3), eta=0.1)


def test_interface_methods_exist(fam):
    rng = np.random.default_rng(0)
    x = fam.sample(64, rng)
    assert x.shape == (64,) and x.min() >= 0 and x.max() < fam.q

    d = fam.next_token_dist(np.zeros(fam.L, dtype=int))
    assert d.shape == (fam.q,) and abs(d.sum() - 1) < 1e-12

    mask = np.zeros(fam.L, dtype=bool)
    mask[[0, 2]] = True
    vals = x[: fam.L].copy()
    y, exact = fam.conditional_sample(mask, vals, np.random.default_rng(1))
    assert y.shape == (fam.L,) and exact
    assert np.array_equal(y[mask], vals[mask])

    assert 0.0 < fam.entropy_rate() <= np.log2(fam.q)
    W = fam.planted_profile()
    assert W.shape == (fam.L + 1,) and W.sum() <= 1.0 + 1e-12


def test_planted_profile_values(fam):
    W = fam.planted_profile()
    q, eta, s = fam.q, fam.eta, len(fam.lags)
    assert abs(W[0] - 1.0 / q) < 1e-12
    assert abs(W[s] - (q - 1) * (1 - eta) ** 2 / q) < 1e-12
    other = np.delete(W, [0, s])
    assert np.allclose(other, 0.0)


def test_next_token_dist_matches_rule(fam):
    rng = np.random.default_rng(2)
    ctx = rng.integers(0, fam.q, size=fam.L)
    d = fam.next_token_dist(ctx)
    s_true = int(ctx[fam.L - 1] + ctx[fam.L - 3]) % fam.q
    assert abs(d[s_true] - ((1 - fam.eta) + fam.eta / fam.q)) < 1e-12
    others = np.delete(d, s_true)
    assert np.allclose(others, fam.eta / fam.q)


def test_probs_rows_agrees_with_next_token_dist_in_bulk(fam):
    rng = np.random.default_rng(3)
    x = fam.sample(fam.L + 20, rng)
    t = fam.L + 10
    # row encoding: position j has digit weight q^(t-j) (oldest = highest digit)
    row = 0
    for j in range(t + 1):
        row = fam.q * row + int(x[j])
    P = fam.probs_rows(t, np.array([row]))[0]
    ctx = x[t - fam.L : t]
    assert np.allclose(P, fam.next_token_dist(ctx), atol=1e-12)


def test_determinism_same_seed_same_stream(fam):
    a = fam.sample(128, np.random.default_rng(seed_from("proto", fam.version, "cell", "train")))
    b = fam.sample(128, np.random.default_rng(seed_from("proto", fam.version, "cell", "train")))
    c = fam.sample(128, rng_for("proto", fam.version, "cell", "train"))
    assert np.array_equal(a, b)
    assert np.array_equal(a, c)
    d = fam.sample(128, rng_for("proto", fam.version, "cell", "val"))
    assert not np.array_equal(a, d)


def test_marginal_uniformity(fam):
    x = fam.sample(200_000, np.random.default_rng(4))
    counts = np.bincount(x[fam.L :], minlength=fam.q)
    p = counts / counts.sum()
    assert np.max(np.abs(p - 1.0 / fam.q)) < 0.01


def test_joint_table_and_exact_conditioning(fam):
    p = fam.joint_table()
    assert p.shape == (fam.q**fam.L,)
    assert abs(p.sum() - 1.0) < 1e-8

    # exact conditional reproduces the joint marginal on the observed event
    rng = np.random.default_rng(5)
    mask = np.zeros(fam.L, dtype=bool)
    mask[[1, 4]] = True
    obs = np.array([0, 2, 0, 0, 5, 0])
    # reference: rejection from the joint. Row encoding: position p has digit weight q^(L-1-p)
    rows = np.arange(fam.q**fam.L)
    pos_digit = np.array([(rows // fam.q ** (fam.L - 1 - j)) % fam.q for j in range(fam.L)]).T
    ref_rows = rows[(pos_digit[:, 1] == 2) & (pos_digit[:, 4] == 5)]
    ref_p = p[ref_rows]
    ref_p = ref_p / ref_p.sum()

    samples = np.array([
        _exact_conditional_sample(p, fam.q, fam.L, mask, obs, rng)
        for _ in range(3000)
    ])
    emp = np.bincount(samples[:, 0], minlength=fam.q) / len(samples)
    ref = np.array([ref_p[pos_digit[ref_rows, 0] == v].sum() for v in range(fam.q)])
    assert np.max(np.abs(emp - ref)) < 0.03


def test_conditional_exact_flag_and_zero_prob_event(fam):
    vals = np.zeros(fam.L, dtype=int)
    mask = np.ones(fam.L, dtype=bool)
    y, exact = fam.conditional_sample(mask, vals, np.random.default_rng(6))
    assert exact and np.array_equal(y, vals)

    tiny = F2SubsetSum(q=2, L=3, lags=(1,), eta=0.0)  # deterministic: x_t = x_{t-1}
    bad = np.array([0, 1, 0])  # impossible under the law (x1 must equal x0)
    with pytest.raises(ValueError):
        tiny.conditional_sample(np.ones(3, dtype=bool), bad, np.random.default_rng(7))


def test_version_stable_and_param_sensitive():
    a = F2SubsetSum(q=8, L=6, lags=(1, 3), eta=0.1)
    b = F2SubsetSum(q=8, L=6, lags=(1, 3), eta=0.1)
    c = F2SubsetSum(q=8, L=6, lags=(1, 3), eta=0.2)
    assert a.version == b.version
    assert a.version != c.version
    assert len(a.version) == 16
