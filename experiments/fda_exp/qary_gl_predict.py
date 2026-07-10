"""In-distribution categorical (Householder) GL as a predictor.

Recover heavy Householder characters over the dataset via the V-ary CSAMP search
(`qary_gl.qary_gl_search`), reconstruct from them, evaluate held-out.  Two datasets:
  - GB1 (V=20, w=4): real protein epistasis, and V^w=160k is enumerable so we can check
    GL vs brute-force `qary_spectrum` vs the categorical degree-<=2 baseline.
  - TinyStories next-token in the RIGHT (Householder, per-token-position) basis, where V^w
    is un-enumerable so GL is the only route -- the high-order question done properly.

    uv run python -m fda_exp.qary_gl_predict
"""

from __future__ import annotations

import numpy as np

from .householder import householder_basis, qary_reconstruct, qary_spectrum
from .qary_gl import (
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
        d2 = Ridge(alpha=1.0).fit(design_matrix(Ctr, Psi, 2), ytr)
        d2_te = _spearman(yte, d2.predict(design_matrix(Cte, Psi, 2)))
        degs = degree_of_codes(gl_ord[:gl_K], V, w) if gl_K else np.array([0])
        print(f"  seed{seed}: GL {gl_te:.3f} (K={gl_K}, recovered {len(codes)}, "
              f"deg-hist {np.bincount(degs, minlength=w + 1).tolist()}) | "
              f"brute-qary {br_te:.3f} (K={br_K}) | degree<=2 {d2_te:.3f}")


def _decode_tokens(X, window, bpt):
    """bit-encoded contexts X -> (m, window) token-id array (categorical)."""
    bits = (1 - X) // 2
    tok = np.zeros((len(X), window), np.int64)
    for p in range(window):
        for j in range(bpt):
            tok[:, p] += bits[:, p * bpt + j] << j
    return tok


def run_language(window=5, vocab_size=16, n_stories=15000, max_pairs=600000, tau=0.18,
                 n_exp=40000, test_frac=0.25, seed=0, device="cpu", max_width=120000,
                 Ks=(0, 1, 2, 4, 8, 16, 32, 64, 128)):
    """Actual in-distribution categorical GL for next-token, in the Householder basis over
    TOKEN positions (degree = #token positions).  V^w is un-enumerable -> GL is the only route.
    Per-token one-vs-rest: recover heavy Householder chars, reconstruct score, argmax."""
    from sklearn.linear_model import LogisticRegression
    from sklearn.metrics import log_loss
    from .audit import design_matrix
    from .hf_data import tinystories_next_token
    from .predict import majority_by_context
    X, y, vocab, w, bpt = tinystories_next_token(window, vocab_size, n_stories, max_pairs, seed=seed)
    tok = _decode_tokens(X, window, bpt)
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
          f"(V={V}, w={w}, distinct {len(D)}) ##########")
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

    def score(D_eval, K):
        s = np.full((len(D_eval), V), -1e9)
        for t, (codes, co, const) in enumerate(per_tok):
            if not np.isfinite(const):
                continue
            s[:, t] = qary_recon(D_eval, np.concatenate([[0], codes[:K]]).astype(np.int64),
                                 np.concatenate([[const], co[:K]]), V, Psi, w, cd_inv)
        return s

    Kstar = max(Ks, key=lambda K: float((score(Dval, K).argmax(1) == yval).mean()))
    sc = score(Dte, Kstar)
    gl1 = float((sc.argmax(1) == yte).mean())
    flat = np.concatenate([mt[0][:Kstar] for mt in per_tok if len(mt[0])]) if Kstar else np.empty(0, np.int64)
    dh = np.bincount(degree_of_codes(flat, V, w), minlength=w + 1) if len(flat) else np.zeros(w + 1, int)
    lr = LogisticRegression(max_iter=400).fit(design_matrix(Dtr, Psi, 2), ytr)
    P = lr.predict_proba(design_matrix(Dte, Psi, 2))
    d2_1 = float((lr.classes_[P.argmax(1)] == yte).mean())
    print(f"  GL (val K={Kstar}/tok, {n_blow}/{V} blowups)  top-1 {gl1:.3f}   "
          f"token-degree hist {dh.tolist()} (deg>=3: {int(dh[3:].sum())})")
    print(f"  degree-<=2 Householder logistic  top-1 {d2_1:.3f}   majority baseline {base:.3f}")


def main():
    run_gb1()
    run_language()


if __name__ == "__main__":
    main()
