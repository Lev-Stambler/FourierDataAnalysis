"""S1 tier tests (PLAN §12.4 test 2): planted vs enumerated exact agreement."""

import numpy as np
import pytest

from dlx.domains import character_matrix
from dlx.families import F2SubsetSum
from dlx.profiles import enumerated_profile, planted_profile


@pytest.mark.parametrize(
    "q,L,lags,eta",
    [
        (4, 6, (1, 2), 0.0),
        (4, 6, (1, 3), 0.25),
        (8, 6, (1, 3), 0.1),
        (8, 8, (1, 2, 5), 0.05),   # s=3, spread lags
        (16, 16, (1, 4, 8, 16), 0.3),  # S1 protocol-size domain, small support
        (5, 7, (2,), 0.0),          # prime q, single lag
    ],
)
def test_planted_equals_enumerated(q, L, lags, eta):
    fam = F2SubsetSum(q=q, L=L, lags=lags, eta=eta)
    Wp = planted_profile(fam)
    We = enumerated_profile(fam)
    assert np.allclose(Wp, We, atol=1e-9), f"max diff {np.max(np.abs(Wp-We))}"


def test_energy_consistency():
    """sum_k W^k == E||f||^2 under the uniform context measure (Parseval)."""
    fam = F2SubsetSum(q=8, L=6, lags=(1, 3), eta=0.1)
    We = enumerated_profile(fam)
    # direct: average ||f||^2 over the support subspace
    support = fam.support_positions()
    total = 0.0
    ctx = np.zeros(fam.L, dtype=int)
    for flat in range(fam.q ** len(support)):
        digits = np.unravel_index(flat, (fam.q,) * len(support))
        for k, pos in enumerate(support):
            ctx[pos] = digits[k]
        total += np.sum(fam.next_token_dist(ctx) ** 2)
    E = total / fam.q ** len(support)
    assert abs(We.sum() - E) < 1e-10


def test_enumerated_matches_direct_character_bruteforce():
    """Independent check: coefficients via character_matrix on a tiny instance."""
    fam = F2SubsetSum(q=3, L=4, lags=(1, 3), eta=0.2)
    support = fam.support_positions()
    s = len(support)
    # all contexts (full q^L, tiny)
    xs = np.array(np.unravel_index(np.arange(fam.q**fam.L), (fam.q,) * fam.L)).T
    F = np.array([fam.next_token_dist(x) for x in xs])  # (q^L, q)

    # direct coefficient for one multi-index alpha over the full L coordinates
    def direct(alpha):
        chi = character_matrix(fam.q, alpha[None], xs)[0]
        return (F * np.conj(chi)[:, None]).mean(axis=0)

    # nonzero coefficients must live on the support line alpha = a * 1_support
    W_direct = np.zeros(fam.L + 1)
    for a in range(fam.q):
        alpha = np.zeros(fam.L, dtype=int)
        for pos in support:
            alpha[pos] = a
        c = direct(alpha)
        W_direct[np.count_nonzero(alpha)] += np.sum(np.abs(c) ** 2)

    We = enumerated_profile(fam)
    assert np.allclose(W_direct, We, atol=1e-9)
