"""Tests for interaction screening (functional ANOVA / Sobol in the dataset-Householder basis)."""
from itertools import combinations

import numpy as np

from fda_exp.householder import householder_basis
from fda_exp.interaction_gl import (
    _codes_with_support,
    cond_energy,
    interaction_energy,
    recover_chars_on,
    screen_energies,
    screen_interactions,
)
from fda_exp.qary_gl import _encode_qary, qary_coeffs_at, qary_gl_search


def _char(C, alpha, Psi):
    chi = np.ones(len(C))
    for p, a in alpha.items():
        chi = chi * Psi[a, C[:, p]]
    return chi


def _code(alpha, V):
    return int(sum(a * V ** p for p, a in alpha.items()))


def _cube(V, w):
    g = np.array(np.meshgrid(*([np.arange(V)] * w), indexing="ij")).reshape(w, -1).T
    return g.astype(np.int64)


def test_pure_high_order_and_blindness():
    """A pure degree-3 character (zero lower-order): its triple is the top interaction, its pairs
    carry ~no interaction energy (blindness), and the exact character is recovered."""
    rng = np.random.default_rng(0)
    V, w, m = 4, 8, 40000
    C = rng.integers(0, V, size=(m, w)).astype(np.int64)
    Psi = householder_basis(V)
    supp = (2, 4, 6)
    alpha = {2: 1, 4: 2, 6: 3}
    f = _char(C, alpha, Psi)
    ranked, E = screen_interactions(C, f, V, w, max_degree=3)
    topS, topI = ranked[0]
    assert tuple(sorted(topS)) == supp, f"top interaction {topS} != {supp}"
    assert topI > 0.8, topI                                      # ~1 for a unit character
    assert interaction_energy(E, (2, 4)) < 0.05 * topI          # blind: no pairwise shadow
    assert interaction_energy(E, (2, 6)) < 0.05 * topI
    codes, co = recover_chars_on(C, f, supp, V, Psi, w, topk=4)
    assert int(codes[0]) == _code(alpha, V), (int(codes[0]), _code(alpha, V))
    assert abs(abs(co[0]) - 1.0) < 0.05


def test_matches_brute_anova():
    """On a full cube (exact), the plug-in Mobius interaction energy equals sum_{supp beta = S} f_hat^2."""
    V, w = 4, 6
    g = _cube(V, w)
    Psi = householder_basis(V)
    alphas = [{0: 1, 3: 2}, {1: 2, 2: 3, 5: 1}, {4: 1}]         # degree 2, 3, 1
    f = sum(_char(g, a, Psi) for a in alphas)

    def brute_I(S):                                             # sum of squared coeffs with support EXACTLY S
        codes = _codes_with_support(S, V)
        co = qary_coeffs_at(g, f, codes, V, Psi, w)
        return float((co ** 2).sum())

    E, _ = screen_energies(g, f, V, w, max_degree=3, plugin=True)
    for S in [(0, 3), (1, 2, 5), (4,), (0, 1), (2, 3)]:
        assert abs(interaction_energy(E, S) - brute_I(S)) < 1e-6, (S, interaction_energy(E, S), brute_I(S))


def test_reaches_where_csamp_cannot():
    """At w=50 the CSAMP suffix never repeats (GL recovers nothing), but conditioning on the 3
    interacting positions still recovers the planted degree-3 interaction."""
    rng = np.random.default_rng(2)
    V, w, m = 4, 50, 30000
    C = rng.integers(0, V, size=(m, w)).astype(np.int64)
    Psi = householder_basis(V)
    supp = (0, 25, 49)
    alpha = {0: 1, 25: 2, 49: 3}
    f = _char(C, alpha, Psi)

    cand = [supp] + [tuple(sorted(rng.choice(w, 3, replace=False))) for _ in range(150)]
    allsets = {T for S in cand for r in range(len(S) + 1) for T in combinations(S, r)}
    Ed = {frozenset(T): cond_energy(C, f, T, V) for T in allsets}
    I = {S: interaction_energy(Ed, S) for S in cand}
    best = max(I, key=I.get)
    assert tuple(sorted(best)) == supp, (best, I[best], I[supp])

    r = qary_gl_search(_encode_qary(C, V), f, w, V, Psi, tau=1.0, n_exp=20000,
                       device="cpu", mode="csamp", seed=0, max_width=200000)
    assert _code(alpha, V) not in set(r.get("L") or []), "CSAMP should not recover it at w=50"
