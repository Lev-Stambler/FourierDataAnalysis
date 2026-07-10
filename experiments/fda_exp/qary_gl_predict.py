"""In-distribution categorical (Householder) GL as a predictor.

Recover heavy Householder characters over the dataset via the V-ary CSAMP search
(`qary_gl.qary_gl_search`), reconstruct from them, evaluate held-out.  Three datasets:
  - Poelwijk GFP (V=2, w=13): the high-order predictive WIN -- sparse high-order epistasis, GL
    recovers a sparse mostly-degree>=3 character set and beats the additive+pairwise baseline.
  - GB1 (V=20, w=4): real protein epistasis, V^w=160k is enumerable so we can check GL vs
    brute-force `qary_spectrum`; here the true spectrum is degree-<=2 and GL confirms it (unbiased).
  - TinyStories next-token in the RIGHT (Householder, per-token-position) basis, where V^w is
    un-enumerable -- high order is present but undersampled (C_D>>1) and does not help.

    uv run python -m fda_exp.qary_gl_predict
"""

from __future__ import annotations

import numpy as np

from .householder import householder_basis, qary_reconstruct, qary_spectrum
from .qary_gl import (
    _char_columns,
    _digits,
    _encode_qary,
    degree_of_codes,
    qary_coeffs_at,
    qary_gl_search,
    qary_recon,
)


def _spearman(a, b):
    from scipy.stats import spearmanr
    return float(spearmanr(a, b).statistic)


def _valpick(Ks, score):
    best = (-2.0, Ks[0])
    for K in Ks:
        s = score(K)
        if s == s and s > best[0]:
            best = (s, K)
    return best[1]


def run_gb1(seeds=(0, 1, 2), tau_rank=250, n_exp=120000, n_test=20000, n_val=5000,
            Ks=(1, 2, 4, 8, 16, 32, 64, 128, 256), device="cpu"):
    from sklearn.linear_model import Ridge
    from .audit import design_matrix
    from .fitness_data import gb1_windows
    C, y, V, w = gb1_windows()
    Psi = householder_basis(V)
    print(f"\n########## GB1 categorical Householder GL (V={V}, w={w}, m={len(C)}, "
          f"V^w={V**w}) ##########")
    for seed in seeds:
        perm = np.random.default_rng(123 + seed).permutation(len(C))
        te, val, tr = perm[:n_test], perm[n_test:n_test + n_val], perm[n_test + n_val:]
        Ctr, ytr, Cval, yval, Cte, yte = C[tr], y[tr], C[val], y[val], C[te], y[te]
        mu = ytr.mean(); ftr = (ytr - mu).astype(np.float64)

        fhat = qary_spectrum(Ctr, ftr, Psi)                          # exact spectrum (enumerable here)
        order = np.argsort(-np.abs(fhat))
        tau = 2.0 * abs(fhat[order[tau_rank]])

        r = qary_gl_search(_encode_qary(Ctr, V), ftr, w, V, Psi, tau, n_exp=n_exp,
                           device=device, mode="csamp", seed=seed)
        codes = np.array([c for c in (r["L"] or []) if c != 0], dtype=np.int64) if r["status"] == "ok" else np.empty(0, np.int64)
        coeffs = qary_coeffs_at(Ctr, ftr, codes, V, Psi, w) if len(codes) else np.empty(0)
        gl_ord = codes[np.argsort(-np.abs(coeffs))] if len(codes) else codes
        gl_co = coeffs[np.argsort(-np.abs(coeffs))] if len(codes) else coeffs

        gl_K = _valpick([k for k in Ks if k <= len(codes)] or [0],
                        lambda K: _spearman(yval, qary_recon(Cval, gl_ord[:K], gl_co[:K], V, Psi, w) + mu))
        br_K = _valpick(Ks, lambda K: _spearman(yval, qary_reconstruct(Cval, order[:K], fhat[order[:K]], Psi) + mu))

        gl_te = _spearman(yte, qary_recon(Cte, gl_ord[:gl_K], gl_co[:gl_K], V, Psi, w) + mu) if gl_K else 0.0
        br_te = _spearman(yte, qary_reconstruct(Cte, order[:br_K], fhat[order[:br_K]], Psi) + mu)
        br_matched = _spearman(yte, qary_reconstruct(Cte, order[:gl_K], fhat[order[:gl_K]], Psi) + mu) if gl_K else 0.0
        gl_msd = set(int(np.ravel_multi_index(d, (V,) * w)) for d in _digits(gl_ord[:gl_K], V, w)) if gl_K else set()
        recall = len(gl_msd & set(int(order[i]) for i in range(gl_K))) / max(gl_K, 1) if gl_K else 0.0

        # GL as a FEATURE SELECTOR: fit Ridge on the recovered characters.  Raw-coefficient
        # reconstruction (gl_te) is only optimal under a UNIFORM design; GB1's variants are not
        # uniform, so the fair predictor fits weights on the GL-selected columns.
        glf_te, glf_K = 0.0, 0
        maxK = min(max(Ks), len(gl_ord))
        if maxK:
            Xtr = _char_columns(Ctr, gl_ord[:maxK], V, Psi, w)
            Xval = _char_columns(Cval, gl_ord[:maxK], V, Psi, w)
            Xte = _char_columns(Cte, gl_ord[:maxK], V, Psi, w)
            glf_K = _valpick([k for k in Ks if k <= maxK],
                             lambda K: _spearman(yval, Ridge(alpha=1.0).fit(Xtr[:, :K], ytr).predict(Xval[:, :K])))
            glf_te = _spearman(yte, Ridge(alpha=1.0).fit(Xtr[:, :glf_K], ytr).predict(Xte[:, :glf_K]))

        d2 = Ridge(alpha=1.0).fit(design_matrix(Ctr, Psi, 2), ytr)
        d2_te = _spearman(yte, d2.predict(design_matrix(Cte, Psi, 2)))
        degs = degree_of_codes(gl_ord[:gl_K], V, w) if gl_K else np.array([0])
        gl_dh = np.bincount(degs, minlength=w + 1).tolist()
        brute_digs = np.array(np.unravel_index(order[:gl_K], (V,) * w)) if gl_K else np.zeros((w, 1), int)
        brute_dh = np.bincount((brute_digs != 0).sum(0), minlength=w + 1).tolist()   # true top-K degrees
        print(f"  seed{seed}: GL-recon {gl_te:.3f} (K={gl_K}, recovered {len(codes)}, deg {gl_dh}) | "
              f"GL-select+fit {glf_te:.3f} (K={glf_K}) | brute@GL-K {br_matched:.3f} (recall {recall:.2f}, "
              f"true-top-K deg {brute_dh}) | brute-qary {br_te:.3f} (K={br_K}) | degree<=2 {d2_te:.3f}")


def run_poelwijk(target="combined", seeds=(0, 1, 2), n_exp=400000, tau_rank=512,
                 max_width=1_200_000, n_test=2000, n_val=1000, device="cpu",
                 Ks=(4, 8, 16, 32, 64, 128, 256, 512, 1024)):
    """The high-order predictive WIN on real data.  Poelwijk 2019 GFP landscape (V=2, w=13, all
    2^13 genotypes) has many high-order epistatic terms yet is sparse in the Walsh basis.  Exact-leaf
    GL recovers a sparse set of heavy characters (many degree>=3) and beats the additive+pairwise
    (degree-<=2) baseline that cannot see high order -- capturing most of the dense degree-<=4 gain
    with ~tens of coefficients."""
    from itertools import combinations

    from sklearn.linear_model import Ridge

    from .fitness_data import poelwijk_windows

    def walsh_design(C, degree):
        m, ww = C.shape
        s = 1 - 2 * C
        cols = [np.ones((m, 1))]
        for d in range(1, degree + 1):
            for S in combinations(range(ww), d):
                col = np.ones(m)
                for p in S:
                    col = col * s[:, p]
                cols.append(col[:, None])
        return np.concatenate(cols, 1)

    C, y, V, w = poelwijk_windows(target)
    Psi = householder_basis(V)
    print(f"\n########## Poelwijk-{target} high-order WIN (V={V}, w={w}, m={len(C)}, full 2^{w}) ##########")
    for seed in seeds:
        perm = np.random.default_rng(seed).permutation(len(C))
        te, val, tr = perm[:n_test], perm[n_test:n_test + n_val], perm[n_test + n_val:]
        Ctr, ytr, Cval, yval, Cte, yte = C[tr], y[tr], C[val], y[val], C[te], y[te]
        mu = ytr.mean(); ftr = (ytr - mu).astype(np.float64)
        fhat = qary_spectrum(Ctr, ftr, Psi); order = np.argsort(-np.abs(fhat))
        tau = 2.0 * abs(fhat[order[tau_rank]])
        heavy = set(int(s) for s in np.where(fhat ** 2 >= (tau / 2) ** 2)[0] if s != 0)
        r = qary_gl_search(_encode_qary(Ctr, V), ftr, w, V, Psi, tau, n_exp=n_exp,
                           device=device, mode="csamp", seed=seed, max_width=max_width)
        if r["status"] != "ok":
            print(f"  seed{seed}: GL BLEW UP at level {r['level']} (width {r['width']})"); continue
        codes = np.array([c for c in r["L"] if c != 0], dtype=np.int64)
        codes_msd = set(int(np.ravel_multi_index(d, (V,) * w)) for d in _digits(codes, V, w))  # LSD->MSD
        recall = len(codes_msd & heavy) / max(len(heavy), 1)
        co = qary_coeffs_at(Ctr, ftr, codes, V, Psi, w); o = np.argsort(-np.abs(co))
        gl_ord, gl_co = codes[o], co[o]
        avail = [k for k in Ks if k <= len(codes)] or [1]
        Kr = _valpick(avail, lambda K: _spearman(yval, qary_recon(Cval, gl_ord[:K], gl_co[:K], V, Psi, w) + mu))
        gl_recon = _spearman(yte, qary_recon(Cte, gl_ord[:Kr], gl_co[:Kr], V, Psi, w) + mu)

        def refit(K, Cev):
            return Ridge(alpha=1.0).fit(_char_columns(Ctr, gl_ord[:K], V, Psi, w), ytr).predict(
                _char_columns(Cev, gl_ord[:K], V, Psi, w))
        Kf = _valpick(avail, lambda K: _spearman(yval, refit(K, Cval)))
        gl_refit = _spearman(yte, refit(Kf, Cte))
        d2 = _spearman(yte, Ridge(alpha=1.0).fit(walsh_design(Ctr, 2), ytr).predict(walsh_design(Cte, 2)))
        d4 = _spearman(yte, Ridge(alpha=1.0).fit(walsh_design(Ctr, 4), ytr).predict(walsh_design(Cte, 4)))
        dh = np.bincount(degree_of_codes(gl_ord[:Kf], V, w), minlength=w + 1)
        print(f"  seed{seed}: GL-recon {gl_recon:.3f} | GL+refit {gl_refit:.3f} (K={Kf}, "
              f"rec {len(codes)}/{len(heavy)} recall {recall:.2f}) | degree<=2 {d2:.3f} | "
              f"dense degree<=4 {d4:.3f} | GL deg {dh[:6].tolist()} (>=3: {int(dh[3:].sum())})")


def _decode_tokens(X, window, bpt):
    """bit-encoded contexts X -> (m, window) token-id array (categorical)."""
    bits = (1 - X) // 2
    tok = np.zeros((len(X), window), np.int64)
    for p in range(window):
        for j in range(bpt):
            tok[:, p] += bits[:, p * bpt + j] << j
    return tok


def run_language(window=4, vocab_size=32, n_stories=20000, max_pairs=800000, tau=0.16,
                 n_exp=60000, test_frac=0.25, seed=0, device="mps", max_width=120000,
                 Ks=(0, 1, 2, 4, 8, 16, 32, 64, 128)):
    """Actual in-distribution categorical GL for next-token, in the Householder basis over
    TOKEN positions (degree = #token positions).  V^w is un-enumerable -> GL is the only route.

    Two GL predictors: raw-coefficient reconstruction (argmax of sum f_hat_D(a) chi_a), and the
    fair GL-select+logistic-FIT (recovered chars as features for a logistic, matching how the
    degree-<=2 baseline is a fitted logistic).  In-vocab next-tokens only (dropping the degenerate
    <unk> bucket that otherwise dominates the target)."""
    from sklearn.linear_model import LogisticRegression
    from .audit import design_matrix
    from .hf_data import tinystories_next_token
    from .predict import majority_by_context
    X, y, vocab, w, bpt = tinystories_next_token(window, vocab_size, n_stories, max_pairs, seed=seed)
    tok = _decode_tokens(X, window, bpt)
    keep = y != 0                                            # drop <unk> next-token (degenerate majority)
    tok, y = tok[keep], y[keep]
    D, maj = majority_by_context(tok, y)                     # distinct token-contexts + majority next-token
    V = vocab_size
    Psi = householder_basis(V)
    perm = np.random.default_rng(seed + 7).permutation(len(D))
    nte, nval = int(test_frac * len(D)), int(0.15 * len(D))
    te, val, tr = perm[:nte], perm[nte:nte + nval], perm[nte + nval:]
    Dtr, ytr, Dval, yval, Dte, yte = D[tr], maj[tr], D[val], maj[val], D[te], maj[te]
    cd_inv = len(Dtr) / (V ** w)
    base = float((yte == np.bincount(ytr).argmax()).mean())
    print(f"\n########## TinyStories next-token via categorical (Householder) GL "
          f"(V={V}, w={w}, distinct {len(D)}, in-vocab next only) ##########")
    print(f"V^w={V**w:.1e} -> enumeration infeasible; degree = #token positions")

    idx_tr = _encode_qary(Dtr, V)
    per_tok, n_blow = [], 0
    for t in range(V):
        if (ytr == t).sum() == 0:
            per_tok.append((np.empty(0, np.int64), np.empty(0), -np.inf)); continue
        ft = (2 * (ytr == t) - 1).astype(np.float64)
        const = float(qary_coeffs_at(Dtr, ft, [0], V, Psi, w)[0])
        r = qary_gl_search(idx_tr, ft, w, V, Psi, tau, n_exp=n_exp, device=device,
                           mode="csamp", seed=seed + t, max_width=max_width)
        n_blow += r["status"] != "ok"
        rec = np.array([c for c in (r["L"] or []) if c != 0], dtype=np.int64) if r["status"] == "ok" else np.empty(0, np.int64)
        co = qary_coeffs_at(Dtr, ft, rec, V, Psi, w) if len(rec) else np.empty(0)
        o = np.argsort(-np.abs(co)) if len(rec) else np.empty(0, int)
        per_tok.append((rec[o], co[o], const))

    def score(D_eval, K):                                    # GL raw-coefficient reconstruction
        s = np.full((len(D_eval), V), -1e9)
        for t, (codes, co, const) in enumerate(per_tok):
            if not np.isfinite(const):
                continue
            s[:, t] = qary_recon(D_eval, np.concatenate([[0], codes[:K]]).astype(np.int64),
                                 np.concatenate([[const], co[:K]]), V, Psi, w, cd_inv)
        return s

    Kstar = max(Ks, key=lambda K: float((score(Dval, K).argmax(1) == yval).mean()))
    gl1 = float((score(Dte, Kstar).argmax(1) == yte).mean())

    # GL-select + logistic FIT: union of recovered chars as features (fair vs degree-<=2 logistic)
    allcodes = np.array(sorted(set(int(c) for (rec, _, _) in per_tok for c in rec)), dtype=np.int64)
    dh = np.bincount(degree_of_codes(allcodes, V, w), minlength=w + 1) if len(allcodes) else np.zeros(w + 1, int)
    if len(allcodes):
        lrg = LogisticRegression(max_iter=400).fit(_char_columns(Dtr, allcodes, V, Psi, w), ytr)
        glf1 = float((lrg.classes_[lrg.predict_proba(_char_columns(Dte, allcodes, V, Psi, w)).argmax(1)] == yte).mean())
    else:
        glf1 = base

    d2tr, d2te = design_matrix(Dtr, Psi, 2), design_matrix(Dte, Psi, 2)
    lr = LogisticRegression(max_iter=400).fit(d2tr, ytr)
    d2_1 = float((lr.classes_[lr.predict_proba(d2te).argmax(1)] == yte).mean())

    # DECISIVE high-order test: add GL's recovered deg>=3 chars ON TOP of the full deg<=2 model.
    # If that improves top-1, high order adds predictive value; if not, the deg>=3 chars are noise.
    hi = allcodes[degree_of_codes(allcodes, V, w) >= 3] if len(allcodes) else np.empty(0, np.int64)
    if len(hi):
        Xtr_h = np.hstack([d2tr, _char_columns(Dtr, hi, V, Psi, w)])
        Xte_h = np.hstack([d2te, _char_columns(Dte, hi, V, Psi, w)])
        lrh = LogisticRegression(max_iter=400).fit(Xtr_h, ytr)
        d2hi_1 = float((lrh.classes_[lrh.predict_proba(Xte_h).argmax(1)] == yte).mean())
    else:
        d2hi_1 = d2_1
    C_D = (V ** w) / len(Dtr)
    print(f"  GL-recon (val K={Kstar}/tok, {n_blow}/{V} blowups)  top-1 {gl1:.3f}   (density C_D={C_D:.0f})")
    print(f"  GL-select+fit ({len(allcodes)} chars)  top-1 {glf1:.3f}   "
          f"token-degree hist {dh.tolist()} (deg>=3: {int(dh[3:].sum())})")
    print(f"  degree-<=2 logistic  top-1 {d2_1:.3f}   +GL's {len(hi)} deg>=3 chars  top-1 {d2hi_1:.3f}   "
          f"majority {base:.3f}")


def main():
    run_poelwijk()     # the high-order predictive WIN (sparse high-order epistasis)
    run_gb1()          # verifiable anchor: GL == brute-force where enumerable; here data is low-order
    run_language()     # un-enumerable regime: high-order present but undersampled/reducible


if __name__ == "__main__":
    main()
