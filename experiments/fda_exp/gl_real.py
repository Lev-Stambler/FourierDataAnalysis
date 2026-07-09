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


def predict(target="combined", topK=60, n_exp=200000, seed=0, n_test=2000, device="cpu"):
    """In-distribution prediction: run the ACTUAL GL on a TRAIN subsample; the predictor
    is the reconstruction from the recovered heavy-hitter Fourier functions.  Report both
    the GL-native direct reconstruction (sum of heavy chars * their empirical coefficients)
    and an OLS refit of the support, vs Fourier-Lasso (L1)."""
    from sklearn.linear_model import LinearRegression
    from .fitness_data import poelwijk_windows
    from .householder import householder_basis
    from .sample_efficiency import _metrics, fourier_lasso
    C, y, V, w = poelwijk_windows(target)
    Psi = householder_basis(2)
    perm = np.random.default_rng(seed).permutation(len(C))
    te, val, tr = perm[:n_test], perm[n_test:n_test + 1000], perm[n_test + 1000:]
    Ctr, ytr, Cte, yte = C[tr], y[tr], C[te], y[te]
    mu = ytr.mean()
    idx_tr = _encode(Ctr)
    ftr = (ytr - mu).astype(np.float64)

    hist = np.zeros(1 << w)
    hist[idx_tr] = ftr                                     # each variant once
    fhat_tr = _fwht(hist) / len(idx_tr)                   # empirical dataset spectrum f_hat_D(S)
    order = np.argsort(-np.abs(fhat_tr))
    tau = 2.0 * abs(fhat_tr[order[topK - 1]])

    r = gl_search_torch(idx_tr, ftr, w, tau, n_exp=n_exp, device=device, mode="csamp", seed=seed)
    gl_masks = np.array([s for s in r["L"] if s != 0]) if r["status"] == "ok" else np.empty(0, np.int64)

    def direct(masks):                                    # GL-native: reconstruct f = sum f_hat(S) chi_S
        if len(masks) == 0:
            return np.full(len(Cte), mu)
        return _design_2p(Cte, masks, w)[:, 1:] @ fhat_tr[masks] + mu

    def refit(masks):                                     # OLS refit of the recovered support
        if len(masks) == 0:
            return np.full(len(Cte), mu)
        lr = LinearRegression().fit(_design_2p(Ctr, masks, w), ytr)
        return lr.predict(_design_2p(Cte, masks, w))

    lasso_pred, li = fourier_lasso(Ctr, ytr, C[val], y[val], Cte, Psi, 2, w)
    print(f"\n### In-distribution prediction ({target}: GL on {len(tr)} train, test {n_test}) ###")
    print(f"GL recovered {len(gl_masks)} heavy coefficients via CSAMP ({r['status']}, {r['experiments']} calls)")
    print(f"  GL reconstruction (direct) Spearman {_metrics(yte, direct(gl_masks))[1]:.3f}")
    print(f"  GL support + OLS refit     Spearman {_metrics(yte, refit(gl_masks))[1]:.3f}")
    print(f"  brute empirical top-{len(gl_masks):<4}   Spearman {_metrics(yte, refit(order[:max(len(gl_masks), 1)]))[1]:.3f}")
    print(f"  Fourier-Lasso (L1)         Spearman {_metrics(yte, lasso_pred)[1]:.3f}  (nnz {li['nnz']})")


def main():
    run("combined")
    run("red")
    predict("combined")
    predict("red")


if __name__ == "__main__":
    main()
