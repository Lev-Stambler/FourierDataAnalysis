"""The ACTUAL Goldreich-Levin over a dataset -- not Lasso over an enumerated basis --
run on the real Poelwijk epistasis landscape, in-distribution.

Uses `gl_torch.gl_search_torch`: the recursive heavy-Fourier-bucket search whose only
data access is CSAMP (draw a datapoint sharing a context suffix, `idx >> level` =
subcube conditioning).  We check, on real biological data:
  A. correctness -- GL recovers the same heavy coefficients brute force finds;
  B. blindness  -- SAMP (context-blind partners) misses the high-order epistatic
     coefficients that CSAMP recovers;
  C. prediction -- refit the GL-recovered support and predict held-out fitness.

    uv run python -m fda_exp.gl_real
"""

from __future__ import annotations

import numpy as np

from .gl_torch import gl_search_torch


def _fwht(a):
    """Iterative Walsh-Hadamard transform (unnormalized)."""
    a = a.astype(np.float64).copy()
    h, n = 1, len(a)
    while h < n:
        for i in range(0, n, h * 2):
            j = np.arange(i, i + h)
            x, y = a[j].copy(), a[j + h].copy()
            a[j], a[j + h] = x + y, x - y
        h *= 2
    return a


def _popcount(v):
    return bin(int(v)).count("1")


def _encode(C):
    w = C.shape[1]
    return (C * (1 << np.arange(w))).sum(1).astype(np.int64)


def _brute_spectrum(idx, f, w):
    """Exact dataset coefficients f_hat(S) = E_D[f chi_S] for all S (full landscape)."""
    fvec = np.zeros(1 << w)
    fvec[idx] = f
    return _fwht(fvec) / (1 << w)                          # +/-1 convention, matches gl_torch's Psi


def run(target="combined", topK=20, n_exp=200000, seed=0, device="cpu"):
    from .fitness_data import poelwijk_windows
    C, y, V, w = poelwijk_windows(target)
    idx = _encode(C)
    f = (y - y.mean()).astype(np.float64)

    fhat = _brute_spectrum(idx, f, w)
    order = np.argsort(-np.abs(fhat))
    tau = 2.0 * abs(fhat[order[topK - 1]])                 # leaf kept iff |f_hat(S)| >= tau/2
    brute_heavy = set(int(s) for s in np.where(fhat ** 2 >= (tau / 2) ** 2)[0] if s != 0)

    print(f"### ACTUAL GL on real Poelwijk-{target} landscape (n={w}, full 2^{w}={1 << w}) ###")
    print(f"threshold tau={tau:.4f} -> brute-force heavy set = {len(brute_heavy)} coefficients\n")

    out = {}
    for mode in ("csamp", "samp"):
        r = gl_search_torch(idx, f, w, tau, n_exp=n_exp, device=device, mode=mode, seed=seed)
        if r["status"] != "ok":
            print(f"{mode:>6}: BLEW UP at level {r['level']} (width {r['width']}) -- blindness")
            out[mode] = set()
            continue
        rec = set(s for s in r["L"] if s != 0)
        recall = len(rec & brute_heavy) / max(len(brute_heavy), 1)
        prec = len(rec & brute_heavy) / max(len(rec), 1)
        degs = np.array([_popcount(s) for s in rec]) if rec else np.array([0])
        hi = sum(1 for s in rec if _popcount(s) >= 3)
        print(f"{mode:>6}: recovered {len(rec):>3} | recall {recall:.2f} precision {prec:.2f} "
              f"| degrees {np.bincount(degs).tolist()} | high-order(>=3): {hi}  "
              f"[{r['experiments']} oracle calls]")
        out[mode] = rec
    miss = out.get("csamp", set()) - out.get("samp", set())
    hi_miss = sum(1 for s in miss if _popcount(s) >= 2)
    print(f"\nBLINDNESS: CSAMP found {len(out.get('csamp', set()))}, SAMP found {len(out.get('samp', set()))}; "
          f"SAMP misses {len(miss)} that CSAMP got ({hi_miss} of them order>=2).")
    return out, brute_heavy, fhat, order


def _design_2p(C, masks, w):
    """Design columns chi_S(x) in the SAME bit convention as _fwht / gl_torch
    (bit p <-> position p, value 2^p).  (Note: sample_efficiency._support_design uses
    the reversed unravel_index/C-order convention -- do NOT mix it with GL/FWHT masks.)"""
    s = 1 - 2 * C
    cols = [np.ones(len(C))]
    for mask in masks:
        chi = np.ones(len(C))
        for p in range(w):
            if (int(mask) >> p) & 1:
                chi *= s[:, p]
        cols.append(chi)
    return np.column_stack(cols)


def _recon(C, masks, coeffs, mu, w):
    """Direct reconstruction g(x) = mu + sum_S coeffs[S] chi_S(x)  (Convention A)."""
    if len(masks) == 0:
        return np.full(len(C), mu)
    return _design_2p(C, masks, w)[:, 1:] @ coeffs[masks] + mu


def _refit(Ctr, ytr, Cte, masks, w):
    """OLS refit of a recovered support (Convention A design)."""
    from sklearn.linear_model import LinearRegression
    if len(masks) == 0:
        return np.full(len(Cte), ytr.mean())
    lr = LinearRegression().fit(_design_2p(Ctr, masks, w), ytr)
    return lr.predict(_design_2p(Cte, masks, w))


def _train_spectrum(Ctr, ytr, w):
    """Empirical dataset spectrum f_hat_D(S) over the train sample, + magnitude order."""
    mu = ytr.mean()
    idx_tr = _encode(Ctr)
    hist = np.zeros(1 << w)
    hist[idx_tr] = (ytr - mu)                              # each distinct variant appears once
    fhat = _fwht(hist) / len(idx_tr)
    return fhat, np.argsort(-np.abs(fhat)), mu


def _val_pick_K(Cval, yval, ranked, fhat, mu, w, Ks):
    """Choose K (prefix of `ranked`) maximizing validation Spearman of the reconstruction."""
    from .sample_efficiency import _metrics
    best = (-2.0, ranked[:1])
    for K in Ks:
        if K > len(ranked):
            break
        s = _metrics(yval, _recon(Cval, ranked[:K], fhat, mu, w))[1]
        if s > best[0]:
            best = (s, ranked[:K])
    return best[1]


def _one_seed(target, seed, n_exp, Kmax, Ks, n_test=2000, n_val=1000, device="cpu"):
    """One train/val/test split: GL (CSAMP) support -> val-selected K -> test Spearman,
    vs exact-FWHT top-K (val-selected) and Fourier-Lasso."""
    from .fitness_data import poelwijk_windows
    from .householder import householder_basis
    from .sample_efficiency import _metrics, fourier_lasso
    C, y, V, w = poelwijk_windows(target)
    perm = np.random.default_rng(seed).permutation(len(C))
    te, val, tr = perm[:n_test], perm[n_test:n_test + n_val], perm[n_test + n_val:]
    Ctr, ytr, Cval, yval, Cte, yte = C[tr], y[tr], C[val], y[val], C[te], y[te]
    fhat, order, mu = _train_spectrum(Ctr, ytr, w)
    tau = 2.0 * abs(fhat[order[Kmax - 1]])                 # low threshold -> recover a generous heavy set
    idx_tr = _encode(Ctr)
    r = gl_search_torch(idx_tr, (ytr - mu).astype(np.float64), w, tau,
                        n_exp=n_exp, device=device, mode="csamp", seed=seed)
    gl = np.array([s for s in r["L"] if s != 0]) if r["status"] == "ok" else np.empty(0, np.int64)
    gl_ranked = gl[np.argsort(-np.abs(fhat[gl]))] if len(gl) else gl
    gl_sel = _val_pick_K(Cval, yval, gl_ranked, fhat, mu, w, Ks) if len(gl) else gl
    bf_sel = _val_pick_K(Cval, yval, order, fhat, mu, w, Ks)
    lasso, li = fourier_lasso(Ctr, ytr, Cval, yval, Cte, householder_basis(2), 2, w)
    return dict(
        gl_direct=_metrics(yte, _recon(Cte, gl_sel, fhat, mu, w))[1],
        gl_refit=_metrics(yte, _refit(Ctr, ytr, Cte, gl_sel, w))[1],
        bf_direct=_metrics(yte, _recon(Cte, bf_sel, fhat, mu, w))[1],
        lasso=_metrics(yte, lasso)[1],
        n_rec=len(gl), K_gl=len(gl_sel), K_bf=len(bf_sel),
    )


def evaluate(target="combined", seeds=(0, 1, 2, 3, 4), n_exp=150000,
             Kmax=512, Ks=(4, 8, 16, 32, 64, 128, 256, 512), device="cpu"):
    """Multi-seed, validation-selected-K comparison (mean +/- std)."""
    rows = [_one_seed(target, s, n_exp, Kmax, Ks, device=device) for s in seeds]
    def ms(k):
        v = np.array([r[k] for r in rows]); return v.mean(), v.std()
    print(f"\n### Poelwijk {target}: val-selected-K test Spearman over {len(seeds)} seeds "
          f"(GL n_exp={n_exp}) ###")
    print(f"  GL reconstruction (CSAMP)  {ms('gl_direct')[0]:.3f} +/- {ms('gl_direct')[1]:.3f}   "
          f"(K~{int(np.mean([r['K_gl'] for r in rows]))}, recovered ~{int(np.mean([r['n_rec'] for r in rows]))})")
    print(f"  GL support + OLS refit     {ms('gl_refit')[0]:.3f} +/- {ms('gl_refit')[1]:.3f}")
    print(f"  exact-FWHT top-K           {ms('bf_direct')[0]:.3f} +/- {ms('bf_direct')[1]:.3f}   "
          f"(K~{int(np.mean([r['K_bf'] for r in rows]))})")
    print(f"  Fourier-Lasso (L1)         {ms('lasso')[0]:.3f} +/- {ms('lasso')[1]:.3f}")
    return rows


def nexp_convergence(target="combined", seed=0, n_exps=(20000, 50000, 100000, 200000, 500000),
                     Kmax=256, Ks=(4, 8, 16, 32, 64, 128, 256), device="cpu"):
    """Does GL-CSAMP reconstruction -> exact-FWHT reconstruction as n_exp grows?"""
    print(f"\n### GL-CSAMP -> exact convergence ({target}, seed {seed}): val-K test Spearman vs n_exp ###")
    for ne in n_exps:
        r = _one_seed(target, seed, ne, Kmax, Ks, device=device)
        print(f"  n_exp={ne:>8}: GL {r['gl_direct']:.3f}   (exact-FWHT {r['bf_direct']:.3f}, "
              f"recovered {r['n_rec']})")


def main():
    run("combined")
    run("red")
    for t in ("combined", "red", "blue"):
        evaluate(t)
    nexp_convergence("combined")


if __name__ == "__main__":
    main()
