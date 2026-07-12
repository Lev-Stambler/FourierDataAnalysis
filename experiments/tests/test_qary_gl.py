"""Correctness tests for the categorical (Householder) GL search."""
import numpy as np

from fda_exp.householder import householder_basis, qary_spectrum
from fda_exp.qary_gl import (
    _digits,
    _encode_qary,
    qary_coeffs_at,
    qary_gl_search,
    qary_recon,
)


def _cube(V, w):
    g = np.array(np.meshgrid(*([np.arange(V)] * w), indexing="ij")).reshape(w, -1).T
    return g.astype(np.int64)


def _char(C, alpha, Psi):
    chi = np.ones(len(C))
    for p, a in enumerate(alpha):
        if a:
            chi = chi * Psi[a, C[:, p]]
    return chi


def _code(alpha, V):
    return int(sum(a * V ** p for p, a in enumerate(alpha)))


def _msd(code, V, w):                                        # LSD code -> C-order flat (qary_spectrum)
    return int(np.ravel_multi_index(_digits([code], V, w)[0], (V,) * w))


def test_recovers_single_character_and_convention():
    V, w = 4, 6
    C, Psi = _cube(V, w), householder_basis(V)
    alpha = [1, 0, 2, 0, 0, 0]                                # degree-2 Householder char at positions 0,2
    f = _char(C, alpha, Psi)
    r = qary_gl_search(_encode_qary(C, V), f, w, V, Psi, tau=1.0, n_exp=20000, device="cpu", mode="csamp")
    code = _code(alpha, V)
    assert code in set(r["L"]), f"expected code {code}, got {r['L'][:10]}"
    assert _digits([code], V, w)[0].tolist() == alpha        # convention: code decodes to alpha
    g = qary_recon(C, np.array([code]), qary_coeffs_at(C, f, [code], V, Psi, w), V, Psi, w)
    assert np.corrcoef(g, f)[0, 1] > 0.98


def test_recall_vs_bruteforce_and_coeffs_match():
    V, w = 4, 6
    C, Psi = _cube(V, w), householder_basis(V)
    alphas = [[2, 0, 0, 0, 0, 0], [1, 0, 3, 0, 0, 0], [1, 2, 0, 3, 0, 0]]   # deg 1,2,3 (each |f_hat|=1)
    f = sum(_char(C, a, Psi) for a in alphas)
    fhat = qary_spectrum(C, f, Psi)                          # exact spectrum (C-order)
    tau = 1.0                                                # leaf kept iff |f_hat| >= 0.5 -> the 3 planted
    brute = set(int(s) for s in np.where(fhat ** 2 >= (tau / 2) ** 2)[0] if s != 0)
    r = qary_gl_search(_encode_qary(C, V), f, w, V, Psi, tau=tau, n_exp=40000, device="cpu", mode="csamp")
    rec_msd = set(_msd(c, V, w) for c in r["L"] if c != 0)
    assert len(rec_msd & brute) / max(len(brute), 1) >= 0.8
    for c in [c for c in r["L"] if c != 0][:20]:             # qary_coeffs_at == qary_spectrum
        assert abs(qary_coeffs_at(C, f, [c], V, Psi, w)[0] - fhat[_msd(c, V, w)]) < 1e-6


def test_W_recovers_heavy_char_where_psi_would_prune():
    """The crux fix. On NON-uniform V=3 data, a planted heavy Householder character has a level-0
    ancestor bucket whose *conditional* weight Psi = E_z[vbar_S^2] falls BELOW its coefficient^2
    (Psi-Completeness FAILS for the real Householder basis, |chi_U| != 1), while the *Parseval*
    bucket weight W = sum_U f_hat(S u U)^2 stays >= it.  A Psi-threshold in the gap would prune the
    ancestor and drop the character; the W-based search (what qary_gl_search now computes) recovers it."""
    # V must have NO +-1 Hadamard (householder_basis is exactly +-1/Walsh when V is a power of 2,
    # i.e. unimodular -> completeness already holds).  V=3 is the smallest genuinely non-unimodular case.
    V, w, m = 3, 3, 6000
    Psi = householder_basis(V)
    a = 1                                                      # a contrast whose |chi| varies (V=3: not +-1)
    vstar = int((Psi[a] ** 2).argmax())                       # value where chi_a^2 is largest (> 1)
    rng = np.random.default_rng(0)
    p2 = np.full(V, 0.10 / (V - 1)); p2[vstar] = 0.90         # concentrate pos2 on vstar -> E[chi_a(x2)^2] > 1
    C = np.stack([rng.integers(0, V, m), rng.integers(0, V, m),
                  rng.choice(V, m, p=p2)], axis=1).astype(np.int64)
    alpha = [a, 0, a]                                          # degree-2: contrast a at pos0 and pos2
    f = _char(C, alpha, Psi)                                   # planted heavy character
    code = _code(alpha, V)

    # LEVEL-0 ancestor bucket S = {contrast a at position 0}; suffix = positions (1,2)
    g = f * Psi[a, C[:, 0]]                                    # f * chi_S(x_0)
    _, inv, cnt = np.unique(C[:, 1] * V + C[:, 2], return_inverse=True, return_counts=True)
    G = np.zeros(len(cnt)); np.add.at(G, inv, g)              # per-context group sums
    Psi_anc = float(((cnt / m) * (G / cnt) ** 2).sum())       # E_z[vbar_S^2]  (conditional weight)
    W_anc = float(V ** (w - 1) / m ** 2 * (G ** 2).sum())     # sum_U f_hat(S u U)^2  (Parseval weight)
    f2 = float((f * _char(C, alpha, Psi)).mean()) ** 2        # f_hat_D(alpha)^2  (the leaf)

    assert Psi_anc < f2, (Psi_anc, f2)                        # Psi-Completeness FAILS on Householder
    assert W_anc >= f2 - 1e-9, (W_anc, f2)                    # W-Completeness holds

    tau = float(np.sqrt(4.0 * np.sqrt(Psi_anc * f2)))         # tau^2/4 = geomean(Psi_anc, f2) -> in the gap
    assert Psi_anc < tau ** 2 / 4 <= f2 + 1e-9               # a Psi-search prunes the ancestor; leaf stays heavy
    r = qary_gl_search(_encode_qary(C, V), f, w, V, Psi, tau=tau, n_exp=20000, device="cpu", mode="csamp")
    assert r["status"] == "ok" and code in set(r["L"]), (r["status"], r.get("L"))


def test_blindness_csamp_vs_samp_full_degree():
    V, w = 4, 5
    C, Psi = _cube(V, w), householder_basis(V)
    alpha = [1, 2, 3, 1, 2]                                   # full-degree Householder character
    f = _char(C, alpha, Psi)
    idx = _encode_qary(C, V)
    code = _code(alpha, V)
    rc = qary_gl_search(idx, f, w, V, Psi, tau=1.0, n_exp=40000, device="cpu", mode="csamp")
    rs = qary_gl_search(idx, f, w, V, Psi, tau=1.0, n_exp=40000, device="cpu", mode="samp")
    assert code in set(rc["L"] or []), "CSAMP should recover the full-degree character"
    assert code not in set(rs["L"] or []), "SAMP should be BLIND to it"
