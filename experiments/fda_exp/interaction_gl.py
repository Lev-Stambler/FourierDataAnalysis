"""Interaction screening = functional ANOVA / Sobol in the dataset-Householder basis.

Complements the CSAMP search in `qary_gl.py`.  CSAMP conditions on the `w-d` suffix (needs
`V^{w-d} <~ |D|`) -> good for high-degree / short contexts.  This module conditions on the `d`
INTERACTING positions (needs only `V^d <~ |D|`, independent of `w`) -> reaches sparse high-order in
LONG contexts, but is degree-capped at `d <~ log_V|D|` (the SQ-optimal reach from random samples).

Math.  Householder characters `chi_beta(x)=prod_p Psi[beta_p, x_p]`, `f_hat_D(beta)=E_D[f chi_beta]`.
- conditional-mean energy on a set S:  `E_S = E_{x_S}[(E[f|x_S])^2]`, estimated exactly by group-by on
  `x_S` with the U-statistic (self-pair) correction  `Ê_S = (1/m) sum_g (S_g^2 - Q_g)/(n_g - 1)`
  (`S_g=sum f`, `Q_g=sum f^2` per group; singleton groups drop out).  `shrink>0` gives an
  empirical-Bayes soft version (thin cells shrink toward the global mean -> fuzzy degree cap).
- pure degree-|S| interaction energy (Hoeffding/Mobius):
  `I_S = sum_{beta: supp(beta)=S} f_hat_D(beta)^2 = sum_{T subseteq S} (-1)^{|S|-|T|} E_T`.
  Blind: a pure degree-d character gives `I_S >> 0` with all `I_{T subsetneq S} = 0`.

Convention: mixed-radix LSD, matching `qary_gl` (do NOT mix with `householder.qary_spectrum`'s
C-order flat indices without decoding to digits first).
"""

from __future__ import annotations

from itertools import combinations

import numpy as np

from .qary_gl import qary_coeffs_at


def _group_key(C, S, V):
    """Mixed-radix key of the S-coordinates: sum_j C[:,S[j]] * V^j  (S small -> keys fit in int64)."""
    S = np.asarray(list(S), dtype=np.int64)
    if len(S) == 0:
        return np.zeros(len(C), dtype=np.int64)
    return (C[:, S].astype(np.int64) * (V ** np.arange(len(S)))).sum(1)


def cond_energy(C, f, S, V, shrink=0.0, mu=None, plugin=False):
    """`Ê_S = E_{x_S}[(E[f|x_S])^2]` via group-by on `x_S`.

    Default (hard, shrink=0): U-statistic `(1/m) sum_g (S_g^2 - Q_g)/(n_g-1)` over groups with
    `n_g>=2` -> unbiased for the POPULATION conditional energy (removes the finite-sample diagonal).
    plugin=True: the plug-in `sum_g (n_g/m)*mean_g^2` -> exactly `sum_{supp(beta) subseteq S} f_hat^2`
    on a full cube (used to check the Mobius/ANOVA identity against brute force).
    Soft (shrink>0): shrink each group mean toward `mu` (default global mean) with strength `shrink`
    -> thin cells (proliferating as V^|S| grows past |D|) contribute ~mu^2 and wash out after the
    Mobius, so the degree cap fuzzes instead of walling.
    """
    f = np.asarray(f, dtype=np.float64)
    m = len(f)
    key = _group_key(C, S, V)
    order = np.argsort(key, kind="stable")
    ks, fs = key[order], f[order]
    uniq, start, counts = np.unique(ks, return_index=True, return_counts=True)
    ends = start + counts
    csum = np.concatenate([[0.0], np.cumsum(fs)])
    csq = np.concatenate([[0.0], np.cumsum(fs * fs)])
    Sg = csum[ends] - csum[start]
    Qg = csq[ends] - csq[start]
    ng = counts.astype(np.float64)
    if plugin:
        return float((Sg * Sg / ng).sum() / m)                   # frequency-weighted E[(E[f|x_S])^2]
    if shrink <= 0.0:
        good = ng >= 2
        contrib = np.zeros(len(uniq))
        contrib[good] = (Sg[good] ** 2 - Qg[good]) / (ng[good] - 1.0)
        return float(contrib.sum() / m)
    mu = float(f.mean()) if mu is None else float(mu)
    m_shrunk = (Sg + shrink * mu) / (ng + shrink)                 # empirical-Bayes group means
    return float((ng * m_shrunk ** 2).sum() / m)


def cond_collision(C, y, S, V, W):
    """Multiclass conditional energy `E_S = E_{x_S}[ sum_t (E[1{y=t}|x_S])^2 ]` (expected next-symbol
    collision / Gini predictability), one group-by for ALL classes at once.  U-statistic (distinct
    same-class pairs): `(1/m) sum_g [ sum_t n_{g,t}(n_{g,t}-1) ] / (n_g - 1)` over groups `n_g>=2`.
    Unbiased -> thin high-degree cells do NOT inflate it (no overfitting bloat).  W = #classes."""
    y = np.asarray(y, dtype=np.int64)
    m = len(y)
    key = _group_key(C, S, V)
    gk, ng = np.unique(key, return_counts=True)
    joint = key * W + y                                          # unique per (group, class)
    juniq, jc = np.unique(joint, return_counts=True)
    jgroup = juniq // W
    gi = np.searchsorted(gk, jgroup)
    same = np.zeros(len(gk))
    np.add.at(same, gi, jc * (jc - 1.0))                        # sum_t n_{g,t}(n_{g,t}-1) per group
    good = ng >= 2
    contrib = np.zeros(len(gk))
    contrib[good] = same[good] / (ng[good] - 1.0)
    return float(contrib.sum() / m)


def screen_collisions(C, y, V, w, W, max_degree, active=None):
    """Multiclass analogue of `screen_energies`: `E[frozenset(S)] = cond_collision(...)` for all sets
    of size <= max_degree (over `active` positions if given)."""
    positions = list(range(w)) if active is None else list(active)
    E = {frozenset(): cond_collision(C, y, (), V, W)}
    sets = [()]
    for d in range(1, max_degree + 1):
        for S in combinations(positions, d):
            E[frozenset(S)] = cond_collision(C, y, S, V, W)
            sets.append(S)
    return E, sets


def energy_by_degree(E, sets, w):
    """Sum of pure interaction energy `I_S` at each degree d = the predictability that jointly using
    exactly-d positions adds beyond all lower orders.  Returns array indexed by degree."""
    out = np.zeros(w + 1)
    for S in sets:
        out[len(S)] += interaction_energy(E, S)
    return out


def screen_energies(C, f, V, w, max_degree, active=None, shrink=0.0, plugin=False):
    """Return (E, sets): `E[frozenset(S)] = Ê_S` for every set of size <= max_degree over the
    candidate positions (`active` restricts them -> heredity pruning for long w), plus the empty set."""
    f = np.asarray(f, dtype=np.float64)
    mu = float(f.mean())
    positions = list(range(w)) if active is None else list(active)
    E = {frozenset(): cond_energy(C, f, (), V, shrink=shrink, mu=mu, plugin=plugin)}
    sets = [()]
    for d in range(1, max_degree + 1):
        for S in combinations(positions, d):
            E[frozenset(S)] = cond_energy(C, f, S, V, shrink=shrink, mu=mu, plugin=plugin)
            sets.append(S)
    return E, sets


def interaction_energy(E, S):
    """Pure interaction energy `I_S = sum_{T subseteq S} (-1)^{|S|-|T|} Ê_T` (Mobius inversion)."""
    S = tuple(sorted(S))
    d = len(S)
    total = 0.0
    for r in range(d + 1):
        for T in combinations(S, r):
            total += ((-1) ** (d - r)) * E[frozenset(T)]
    return float(total)


def screen_interactions(C, f, V, w, max_degree, active=None, shrink=0.0, min_degree=1):
    """Screen all sets (1<=|S|<=max_degree) and return (sorted [(S, I_S)] desc, E-dict)."""
    E, sets = screen_energies(C, f, V, w, max_degree, active=active, shrink=shrink)
    out = [(S, interaction_energy(E, S)) for S in sets if len(S) >= min_degree]
    out.sort(key=lambda t: -t[1])
    return out, E


def active_positions(C, f, V, w, degree=2, topfrac=0.5, shrink=0.0):
    """Heredity seed: positions whose main/low-order interaction energy is in the heavy fraction."""
    E, sets = screen_energies(C, f, V, w, degree, shrink=shrink)
    score = np.zeros(w)
    for S in sets:
        if S:
            iS = interaction_energy(E, S)
            for p in S:
                score[p] = max(score[p], iS)
    keep = max(1, int(np.ceil(topfrac * w)))
    return sorted(int(p) for p in np.argsort(-score)[:keep])


def _group_means(Ctr, ytr, T, V, W):
    """Train per-group class distribution `E[onehot(y)|x_T]`: (uniq_keys, table (n_uniq, W))."""
    key = _group_key(Ctr, T, V)
    uniq, ng = np.unique(key, return_counts=True)
    table = np.zeros((len(uniq), W))
    joint = key * W + np.asarray(ytr, np.int64)
    ju, jc = np.unique(joint, return_counts=True)
    gi = np.searchsorted(uniq, ju // W)
    table[gi, (ju % W).astype(np.int64)] = jc
    return uniq, table / ng[:, None]


def _lookup(uniq, table, key_te, fallback):
    idx = np.clip(np.searchsorted(uniq, key_te), 0, len(uniq) - 1)
    hit = uniq[idx] == key_te
    return np.where(hit[:, None], table[idx], fallback[None, :])


def anova_scores(Ctr, ytr, Cte, chosen_sets, V, W):
    """Held-out ANOVA-reconstruction of `P(y=t|x) ~= sum_{S in chosen} g_{S,t}(x_S)`, `g_{S,t}` the
    pure interaction component (Mobius over subsets of the train conditional means, looked up on
    test; unseen cells fall back to the base rate).  Include `()` for the base term.  Returns
    (m_te, W) scores -> argmax = prediction.  Reuses only group-bys; no character materialization."""
    ytr = np.asarray(ytr, np.int64)
    base = np.bincount(ytr, minlength=W).astype(np.float64) / len(ytr)
    Ts = {tuple(sorted(T)) for S in chosen_sets for r in range(len(S) + 1)
          for T in combinations(sorted(S), r)}
    mu = {}
    for T in Ts:
        if len(T) == 0:
            mu[T] = np.broadcast_to(base, (len(Cte), W))
        else:
            uniq, table = _group_means(Ctr, ytr, T, V, W)
            mu[T] = _lookup(uniq, table, _group_key(Cte, T, V), base)
    scores = np.zeros((len(Cte), W))
    for S in chosen_sets:
        S = tuple(sorted(S)); d = len(S)
        for r in range(d + 1):
            for T in combinations(S, r):
                scores += ((-1) ** (d - r)) * mu[T]
    return scores


def _codes_with_support(S, V):
    """All Householder character codes with support EXACTLY S (nonzero digit on each p in S)."""
    S = list(S)
    if not S:
        return np.array([0], dtype=np.int64)
    grids = np.meshgrid(*[np.arange(1, V) for _ in S], indexing="ij")
    digits = np.stack([g.ravel() for g in grids], axis=1)         # (num, |S|) contrasts in 1..V-1
    Vp = V ** np.asarray(S, dtype=np.int64)
    return (digits * Vp[None, :]).sum(1).astype(np.int64)


def recover_chars_on(C, f, S, V, Psi, w, topk=64):
    """The heavy Householder characters supported exactly on S, by direct `qary_coeffs_at`
    (no enumeration of V^w; only (V-1)^|S| codes).  Returns (codes, coeffs) top-`topk` by |f_hat_D|."""
    codes = _codes_with_support(S, V)
    co = qary_coeffs_at(C, f, codes, V, Psi, w)
    order = np.argsort(-np.abs(co))[:topk]
    return codes[order], co[order]
