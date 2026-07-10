"""Phase 3 centerpiece: the differentiator between GL and Lasso/enumerated bases.

To capture a degree-d interaction, any enumerated-basis method (Lasso, logistic on an
explicit feature set) needs O(n^d) features, so at scale it is capped at low order
(degree <=2 is already C(n,2) features).  GL finds the few heavy HIGH-order coefficients
adaptively via CSAMP without ever enumerating them.  This asks, on data whose predictive
terms are a-priori unknown and possibly high-order -- natural LANGUAGE (TinyStories
next-token), where a distant token can flip the prediction:

  - does GL's recovered heavy set actually contain degree->=3 coefficients? (degree histogram)
  - does the GL reconstruction beat degree-<=2 logistic (the feasible-at-scale enumerated baseline)?
  - a planted degree-k calibration: GL recovers a term degree-<=2 is provably blind to.
  - scalability: CSAMP search width stays bounded on real language, blows up on random sequences.

    uv run python -m fda_exp.gl_scale_predict
"""

from __future__ import annotations

import numpy as np

from .gl_torch import gl_search_torch
from .predict import majority_by_context, reconstruct
from .spectrum import coeffs_at, points_to_index, popcount


def _pairwise_design(D):
    """Degree-<=2 design over the n +/-1 bits: [1 | bits | all C(n,2) pair products]."""
    m, n = D.shape
    iu, ju = np.triu_indices(n, k=1)
    return np.concatenate([np.ones((m, 1), np.float32), D.astype(np.float32),
                           (D[:, iu] * D[:, ju]).astype(np.float32)], axis=1)


def gl_onevsrest(Dtr, ytr, Dte, V, tau, n_exp, device, seed, max_width=80000):
    """Deprecated shim -- use gl_recover + gl_score (validation-selected K)."""
    per_tok, n, _ = gl_recover(Dtr, ytr, V, tau, n_exp, device, seed, max_width)
    cd_inv = len(Dtr) / (1 << n)
    scores = gl_score(per_tok, Dte, n, cd_inv, K=10 ** 9)
    return scores, [mt[0] for mt in per_tok], [len(mt[0]) for mt in per_tok]


def gl_recover(Dtr, ytr, V, tau, n_exp, device, seed, max_width):
    """Per token: GL-recover the 'next==t' indicator's heavy coefficients.  Return, per
    token, the non-constant masks ranked by |f_hat_D| (descending), their coefficients, and
    the constant coefficient (the base rate).  n_blowup = #token searches that hit max_width."""
    n = Dtr.shape[1]
    idx = points_to_index(Dtr)
    per_tok, n_blowup = [], 0
    for t in range(V):
        if (ytr == t).sum() == 0:
            # token never seen in train: exclude from argmax (sentinel -inf), matching the
            # baselines' class set -- do NOT give it a 0 base rate that beats real tokens.
            per_tok.append((np.empty(0, np.int64), np.empty(0), -np.inf)); continue
        ft = (2 * (ytr == t) - 1).astype(np.float64)
        const = float(coeffs_at(Dtr, ft, [0])[0])
        r = gl_search_torch(idx, ft, n, tau, n_exp=n_exp, device=device, mode="csamp",
                            seed=seed + t, max_width=max_width)
        n_blowup += r["status"] != "ok"
        rec = np.array(r["L"], dtype=np.int64) if (r["status"] == "ok" and r["L"]) else np.empty(0, np.int64)
        nz = rec[rec != 0]
        if len(nz):
            c = np.asarray(coeffs_at(Dtr, ft, nz.tolist()))
            o = np.argsort(-np.abs(c))                     # rank by empirical magnitude
            per_tok.append((nz[o], c[o], const))
        else:
            per_tok.append((nz, np.empty(0), const))
    return per_tok, n, n_blowup


def gl_score(per_tok, D_eval, n, cd_inv, K):
    """Score each token by reconstructing with its base rate (constant) + top-K non-constant
    coefficients.  K is chosen once on a validation split (prunes the aliasing-noise tail)."""
    scores = np.full((len(D_eval), len(per_tok)), -1e9)
    for t, (masks, coeffs, const) in enumerate(per_tok):
        if not np.isfinite(const):                         # train-absent token -> stays -inf (excluded)
            continue
        m = np.concatenate([[0], masks[:K]]).astype(np.int64)
        c = np.concatenate([[const], coeffs[:K]])
        scores[:, t] = reconstruct(D_eval, n, m, c, cd_inv)
    return scores


def _top1_top3(scores, yte):
    pred = scores.argmax(1)
    top3 = np.argsort(-scores, 1)[:, :3]
    return (float((pred == yte).mean()),
            float(np.mean([yte[i] in top3[i] for i in range(len(yte))])), pred)


def _logistic(Dtr, ytr, Dte, yte, degree):
    from sklearn.linear_model import LogisticRegression
    if degree == 1:
        Xtr, Xte = (1 - Dtr) // 2, (1 - Dte) // 2         # bits 0/1
    else:
        Xtr, Xte = _pairwise_design(Dtr), _pairwise_design(Dte)
    lr = LogisticRegression(max_iter=400).fit(Xtr, ytr)
    P = lr.predict_proba(Xte)
    pred = lr.classes_[P.argmax(1)]
    t3 = lr.classes_[np.argsort(-P, 1)[:, :3]]
    return (float((pred == yte).mean()),
            float(np.mean([yte[i] in t3[i] for i in range(len(yte))])), pred, P.max(1))


def run_language(window=6, vocab_size=48, n_stories=12000, tau=0.35, n_exp=40000,
                 test_frac=0.25, seed=0, device="cpu", max_width=80000, exclude_top=0,
                 max_pairs=300_000):
    """exclude_top>0 drops contexts whose majority next-token is one of the `exclude_top`
    most-frequent (function-word) targets, de-degenerating the otherwise base-rate-dominated
    next-token task so the GL-vs-baselines comparison is meaningful."""
    from .hf_data import tinystories_next_token
    X, y, vocab, w, bpt = tinystories_next_token(window, vocab_size, n_stories, max_pairs, seed=seed)
    D, maj = majority_by_context(X, y)
    n = D.shape[1]
    V = len(vocab)
    if exclude_top:
        freq = np.bincount(maj, minlength=V)
        drop = set(np.argsort(-freq)[:exclude_top].tolist())
        keep = ~np.isin(maj, list(drop))
        D, maj = D[keep], maj[keep]
        toks = np.array(sorted(set(maj.tolist())))
        maj = np.searchsorted(toks, maj)                   # remap to 0..V'-1
        V = len(toks)
        print(f"[exclude_top={exclude_top}] kept {len(D)} non-degenerate contexts, V={V}")
    perm = np.random.default_rng(seed + 7).permutation(len(D))
    nte, nval = int(test_frac * len(D)), int(0.15 * len(D))
    te, val, tr = perm[:nte], perm[nte:nte + nval], perm[nte + nval:]
    Dtr, ytr, Dval, yval, Dte, yte = D[tr], maj[tr], D[val], maj[val], D[te], maj[te]
    cd_inv = len(Dtr) / (1 << n)
    base = float((yte == np.bincount(ytr).argmax()).mean())

    print(f"\n########## TinyStories next-token via GL (window={w} tokens, {bpt} bits/tok, "
          f"n={n}, V={V}) ##########")
    print(f"distinct contexts {len(D)} (train {len(Dtr)}, val {len(Dval)}, test {len(Dte)}); "
          f"2^n={1 << n:.0e} -> enumeration/full-FWHT INFEASIBLE, degree-<=2 = {1 + n + n*(n-1)//2} feats")

    per_tok, _, n_blowup = gl_recover(Dtr, ytr, V, tau, n_exp, device, seed, max_width)
    # validation-select K (#non-constant coeffs per token) -- prunes the aliasing-noise tail
    Ks = (0, 2, 4, 8, 16, 32, 64, 128, 256, 512)
    valacc = {K: float((gl_score(per_tok, Dval, n, cd_inv, K).argmax(1) == yval).mean()) for K in Ks}
    Kstar = max(Ks, key=lambda K: valacc[K])
    print("  GL val-accuracy vs K: " + "  ".join(f"K={K}:{valacc[K]:.3f}" for K in Ks))
    gl1, gl3, gl_pred = _top1_top3(gl_score(per_tok, Dte, n, cd_inv, Kstar), yte)
    flat = np.concatenate([mt[0][:Kstar] for mt in per_tok if len(mt[0])]) if Kstar else np.empty(0, np.int64)
    dh = np.bincount(popcount(flat), minlength=6) if len(flat) else np.zeros(6, int)

    l1_1, l1_3, l1_pred, _ = _logistic(Dtr, ytr, Dte, yte, 1)
    l2_1, l2_3, l2_pred, l2_conf = _logistic(Dtr, ytr, Dte, yte, 2)

    print(f"\n{'method':>22}  {'top-1':>6} {'top-3':>6}")
    print("-" * 40)
    print(f"{'majority baseline':>22}  {base:>6.3f} {'-':>6}")
    print(f"{'logistic degree-1':>22}  {l1_1:>6.3f} {l1_3:>6.3f}")
    print(f"{'logistic degree-<=2':>22}  {l2_1:>6.3f} {l2_3:>6.3f}   <- feasible enumerated baseline")
    print(f"{'GL reconstruction':>22}  {gl1:>6.3f} {gl3:>6.3f}   (val-selected K={Kstar}/token, "
          f"{n_blowup}/{V} blowups)")
    print(f"\nGL selected-coeff degree histogram (deg 0..5): {dh.tolist()}  "
          f"(deg>=3 coeffs: {int(dh[3:].sum())})")
    order = np.argsort(l2_conf)
    q = len(order) // 4
    hard = order[:q]                                       # bottom-quartile deg<=2 confidence = "sensitive"
    print(f"on degree-<=2's least-confident quartile ({q} contexts): "
          f"GL {float((gl_pred[hard]==yte[hard]).mean()):.3f}  vs  deg<=2 {float((l2_pred[hard]==yte[hard]).mean()):.3f}")
    return dict(n=n, V=V, Kstar=Kstar, gl_top1=gl1, l1_top1=l1_1, l2_top1=l2_1, deg_hist=dh.tolist())


def run_planted(window=6, vocab_size=24, n_stories=8000, k=3, tau=0.4, n_exp=40000,
                test_frac=0.25, seed=0, device="cpu"):
    """Calibration: plant a degree-k Walsh term chi_S on REAL language contexts. GL must
    recover S and reconstruct it (acc ~1); degree-<=2 logistic is PROVABLY blind for k>=3
    (a degree-k character is orthogonal to every degree-<=2 feature) -> acc ~0.5.
    The clean 'GL does what an enumerated low-order basis structurally cannot at scale'."""
    from sklearn.linear_model import LogisticRegression
    from .hf_data import tinystories_next_token
    X, _, vocab, w, bpt = tinystories_next_token(window, vocab_size, n_stories, seed=seed)
    D = np.unique(X, axis=0)
    n = D.shape[1]
    pos = np.unique(np.linspace(0, n - 1, k).astype(int))[:k]     # k spread bit positions
    S_mask = int(sum(1 << int(p) for p in pos))
    f = np.prod(D[:, pos], axis=1).astype(np.float64)             # chi_S in +/-1
    perm = np.random.default_rng(seed).permutation(len(D))
    nte = int(test_frac * len(D)); te, tr = perm[:nte], perm[nte:]
    Dtr, ftr, Dte, fte = D[tr], f[tr], D[te], f[te]
    idx = points_to_index(Dtr); cd_inv = len(Dtr) / (1 << n)
    r = gl_search_torch(idx, ftr, n, tau, n_exp=n_exp, device=device, mode="csamp", seed=seed)
    L = np.array([s for s in (r["L"] or []) if s != 0], dtype=np.int64) if r["status"] == "ok" else np.empty(0, np.int64)
    g = reconstruct(Dte, n, L, coeffs_at(Dtr, ftr, L), cd_inv) if len(L) else np.zeros(len(Dte))
    gl_acc = float(((g < 0) == (fte < 0)).mean())
    lr = LogisticRegression(max_iter=400).fit(_pairwise_design(Dtr), (ftr > 0).astype(int))
    l2_acc = float((lr.predict(_pairwise_design(Dte)) == (fte > 0)).mean())
    print(f"\n########## Planted degree-{k} term on real language contexts (n={n}, "
          f"positions {pos.tolist()} -> mask {S_mask}) ##########")
    print(f"  GL recovered the planted mask: {S_mask in set(L.tolist())}   (|L|={len(L)})")
    print(f"  sign accuracy:  GL {gl_acc:.3f}   vs   degree-<=2 logistic {l2_acc:.3f}  "
          f"(deg-<=2 is blind to a degree-{k} char)")
    return dict(k=k, n=n, recovered=S_mask in set(L.tolist()), gl_acc=gl_acc, l2_acc=l2_acc)


def scalability_control(window=8, vocab_size=24, n_stories=8000, tau=0.3, n_exp=30000,
                        seed=0, device="cpu"):
    """GL's feasibility needs context repetition (R_k): real language contexts share
    suffixes so CSAMP width stays bounded; random-bit contexts (no repetition) blow it up."""
    from .hf_data import tinystories_next_token
    X, y, vocab, w, bpt = tinystories_next_token(window, vocab_size, n_stories, seed=seed)
    D, maj = majority_by_context(X, y)
    n = D.shape[1]
    f = (2 * (maj == np.bincount(maj).argmax()) - 1).astype(np.float64)
    rng = np.random.default_rng(seed)
    Drand = np.unique(1 - 2 * rng.integers(0, 2, size=(len(D), n)), axis=0)
    frand = rng.choice([-1.0, 1.0], len(Drand))
    print(f"\n########## Scalability: CSAMP search width, real vs random contexts (n={n}) ##########")
    for name, Dd, ff in [("real language", D, f), ("random bits", Drand, frand)]:
        r = gl_search_torch(points_to_index(Dd), ff, n, tau, n_exp=n_exp, device=device,
                            mode="csamp", seed=seed, max_width=100_000)
        mw = max(r["widths"]) if r["widths"] else 0
        print(f"  {name:>14}: status={r['status']:>7}  max width={mw:>7}  "
              f"({'bounded -> GL feasible' if r['status'] == 'ok' else 'BLEW UP -> no context repetition'})")


def main():
    run_language()
    run_planted(k=3)
    run_planted(k=4)
    scalability_control()


if __name__ == "__main__":
    main()
