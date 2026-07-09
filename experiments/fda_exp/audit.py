"""Critical audit: does dataset-GL recover HIGHER-ORDER sparse structure that a
low-order / linear baseline misses -- and is it competitive as ML?

Per target, in the Householder categorical basis:
  - top-K Fourier held-out correlation, + the DEGREE of the recovered coeffs
    (degree = #{positions p : alpha_p != 0}); low-order-dominated => trivial.
  - logistic regression on all degree-<=1 and degree-<=2 Householder features
    (the low-order baseline). Fourier > logistic-<=2 => higher-order structure.
  - performance: accuracy/AUC of the top-K Fourier predictor vs logistic/GBT/MLP
    on the raw one-hot features.

Calibration targets are genuine Householder characters (so a degree-k char is
exactly 1-sparse IN THIS BASIS): deg-1 (both models get it), deg-3 (Fourier gets
it at K=1, logistic-<=2 cannot), random (nobody beats baseline).

    uv run python -m fda_exp.audit
"""

from __future__ import annotations

import numpy as np

from .householder import householder_basis, qary_reconstruct, qary_spectrum


def _corr(g, f):
    gc, fc = g - g.mean(), f - f.mean()
    return float((gc * fc).sum() / (np.sqrt((gc ** 2).sum() * (fc ** 2).sum()) + 1e-12))


def design_matrix(Cd, Psi, degree):
    """Householder features up to `degree` (float32): constant, all deg-1, all deg-2.

    Dense: at V=20,w=5 the full deg-2 matrix is ~3706 cols, so we build in float32
    and (in logistic_lowdeg) fit on a subsample + evaluate in chunks to bound RAM."""
    m, w = Cd.shape
    B = [Psi[1:, Cd[:, p]].T.astype(np.float32) for p in range(w)]     # (m, V-1) per position
    cols = [np.ones((m, 1), np.float32)]
    if degree >= 1:
        cols += B
    if degree >= 2:
        for p in range(w):
            for q in range(p + 1, w):
                cols.append((B[p][:, :, None] * B[q][:, None, :]).reshape(m, -1))
    return np.concatenate(cols, axis=1)


def _auc(score, y01):
    from sklearn.metrics import roc_auc_score
    if y01.min() == y01.max():
        return float("nan")
    return float(roc_auc_score(y01, score))


def logistic_auc(Cd, f, tr, te, Psi, degree, n_fit=50_000, chunk=40_000):
    """Fourier's low-order rival: logistic on degree-<=`degree` Householder features.
    Convex fit needs few rows, so subsample train; eval test decision fn in chunks.
    Returns held-out AUC (imbalance-robust)."""
    from sklearn.linear_model import LogisticRegression
    y = (f > 0).astype(int)
    rng = np.random.default_rng(0)
    fit_idx = tr if len(tr) <= n_fit else rng.choice(tr, n_fit, replace=False)
    if y[fit_idx].min() == y[fit_idx].max():
        return float("nan")
    clf = LogisticRegression(max_iter=800, C=1.0).fit(design_matrix(Cd[fit_idx], Psi, degree), y[fit_idx])
    dec = np.empty(len(te))
    for s in range(0, len(te), chunk):
        sl = te[s:s + chunk]
        dec[s:s + len(sl)] = clf.decision_function(design_matrix(Cd[sl], Psi, degree))
    return _auc(dec, y[te])


def fourier_pick(Cd, f, tr, val, te, Psi, V, w, ks):
    """Dataset-GL predictor: dense empirical spectrum on train, choose K by held-out
    (validation) AUC, evaluate on test.  Constant term doesn't affect AUC (rank), so
    it is harmless.  Reports the degree profile of the selected coefficients."""
    fhat = qary_spectrum(Cd[tr], f[tr], Psi)
    order = np.argsort(-np.abs(fhat))
    yv = (f[val] > 0).astype(int)
    best_a, best_K = -1.0, ks[0]
    for K in ks:
        a = _auc(qary_reconstruct(Cd[val], order[:K], fhat[order[:K]], Psi), yv)
        if a == a and a > best_a:                          # a==a skips nan
            best_a, best_K = a, K
    sel = order[:best_K]
    degs = degree_of(sel, V, w)
    nz = degs[degs > 0]                                    # ignore the degree-0 constant in the profile
    return dict(K=best_K, auc=_auc(qary_reconstruct(Cd[te], sel, fhat[sel], Psi), (f[te] > 0).astype(int)),
                degmean=float(nz.mean()) if len(nz) else 0.0, degmax=int(degs.max()))


def onehot_baselines_auc(Cd, f, tr, te, V, n_fit=120_000):
    """Strong standard baselines on raw one-hot features: held-out AUC."""
    from sklearn.ensemble import HistGradientBoostingClassifier
    from sklearn.linear_model import LogisticRegression
    from sklearn.neural_network import MLPClassifier
    y = (f > 0).astype(int)
    rng = np.random.default_rng(0)
    fit_idx = tr if len(tr) <= n_fit else rng.choice(tr, n_fit, replace=False)
    m, w = Cd.shape

    def onehot(rows):
        X = np.zeros((len(rows), V * w), dtype=np.float32)
        X[np.repeat(np.arange(len(rows)), w), (np.arange(w)[None, :] * V + Cd[rows]).ravel()] = 1
        return X
    Xf, Xt = onehot(fit_idx), onehot(te)
    out = {}
    for name, clf in [("logit-1hot", LogisticRegression(max_iter=800)),
                      ("GBT", HistGradientBoostingClassifier(max_iter=200, random_state=0)),
                      ("MLP", MLPClassifier((64,), max_iter=300, early_stopping=True, random_state=0))]:
        if y[fit_idx].min() == y[fit_idx].max():
            out[name] = float("nan"); continue
        clf.fit(Xf, y[fit_idx])
        out[name] = _auc(clf.predict_proba(Xt)[:, 1], y[te])
    return out


def degree_of(order, V, w):
    mult = np.array(np.unravel_index(np.asarray(order), (V,) * w)).T   # (K, w)
    return (mult != 0).sum(1)


def topk_fourier(Cd, f, tr, te, Psi, ks):
    fhat = qary_spectrum(Cd[tr], f[tr], Psi)
    order = np.argsort(-np.abs(fhat))
    corrs = [_corr(qary_reconstruct(Cd[te], order[:K], fhat[order[:K]], Psi), f[te]) for K in ks]
    return corrs, order, fhat


def cal_targets(Cd, Psi, w, seed=0):
    """Binary calibration targets (so all comparisons are apples-to-apples AUC):
    sign of a deg-1 char (low-order), sign of a deg-3 char (genuine higher-order),
    and random.  sign(prod of contrasts) is a real degree-k Boolean function."""
    rng = np.random.default_rng(seed)
    def char(pos, cs):
        return np.prod([Psi[cs[i], Cd[:, pos[i]]] for i in range(len(pos))], axis=0)
    P3 = sorted({0, w // 2, w - 1})[:3]                # 3 distinct positions for any w >= 3
    return {
        "deg-1 (calib)": np.where(char([0], [1]) > 0, 1.0, -1.0),
        "deg-3 (calib)": np.where(char(P3, [1, 1, 1]) > 0, 1.0, -1.0),
        "random (calib)": rng.choice([-1.0, 1.0], size=len(Cd)),
    }


def auc_report(Cd, targets, Psi, V, w, split, ks, full_for=()):
    """Held-out AUC of the dataset-GL predictor vs low-order & standard baselines.

    The higher-order test done right: does an interaction-capable model (Fourier
    top-K, GBT, MLP) beat degree-<=2 logistic?  If logistic-<=2 already hits the
    ceiling, the structure is order-<=2 and dataset-GL buys nothing here."""
    tr, val, te = split
    print(f"{'target':>26} | {'FourierGL':>18} {'logit<=1':>8} {'logit<=2':>8}"
          f" {'l-1hot':>7} {'GBT':>7} {'MLP':>7} | verdict")
    print("-" * 108)
    rows = {}
    for name, f in targets.items():
        f = f.astype(float)
        fp = fourier_pick(Cd, f, tr, val, te, Psi, V, w, ks)
        l1 = logistic_auc(Cd, f, tr, te, Psi, 1)
        l2 = logistic_auc(Cd, f, tr, te, Psi, 2)
        if name in full_for:
            ob = onehot_baselines_auc(Cd, f, tr, te, V)
        else:
            ob = {"logit-1hot": float("nan"), "GBT": float("nan"), "MLP": float("nan")}
        # low-order ceiling = best LINEAR / degree-<=2 model available
        low = max(v for v in (l1, l2, ob["logit-1hot"]) if v == v)
        interaction = max((v for v in (ob["GBT"], ob["MLP"]) if v == v), default=float("nan"))
        best = max(v for v in (fp["auc"], low, interaction) if v == v)
        if best < 0.55:
            verdict = "no signal"
        elif fp["auc"] > low + 0.02:
            verdict = "GL WINS (higher-order)"          # Fourier beats every low-order model
        elif interaction == interaction and interaction > low + 0.02:
            verdict = "interaction exists, GL misses"   # trees/MLP win but Fourier doesn't
        else:
            verdict = "order<=low (GL redundant)"
        gb = f"{ob['GBT']:>7.3f}" if ob["GBT"] == ob["GBT"] else f"{'-':>7}"
        ml = f"{ob['MLP']:>7.3f}" if ob["MLP"] == ob["MLP"] else f"{'-':>7}"
        oh = f"{ob['logit-1hot']:>7.3f}" if ob["logit-1hot"] == ob["logit-1hot"] else f"{'-':>7}"
        print(f"{name:>26} | {fp['auc']:>6.3f} (K={fp['K']:>3},deg{fp['degmean']:.1f}/{fp['degmax']})"
              f" {l1:>8.3f} {l2:>8.3f} {oh} {gb} {ml} | {verdict}")
        rows[name] = dict(fp=fp, l1=l1, l2=l2, **ob, verdict=verdict)
    return rows


def _split3(n, seed, test_frac=0.25, val_frac=0.15):
    perm = np.random.default_rng(seed).permutation(n)
    nte, nval = int(test_frac * n), int(val_frac * n)
    return perm[nte + nval:], perm[nte:nte + nval], perm[:nte]      # tr, val, te


def run_dna(w=6, n_seqs=8000, max_pairs=200_000, seed=0,
            ks=(1, 2, 4, 8, 16, 32, 64, 128, 256)):
    from .sparsity import build_windows
    C, nxt, lab = build_windows(w, n_seqs, max_pairs)
    Cd, inv = np.unique(C, axis=0, return_inverse=True)
    lab_mean = np.array([lab[inv == g].mean() for g in range(len(Cd))])
    maj = np.array([np.bincount(nxt[inv == g], minlength=4).argmax() for g in range(len(Cd))])
    V, w = 4, Cd.shape[1]
    Psi = householder_basis(V)
    targets = cal_targets(Cd, Psi, w, seed)
    targets["REAL promoter label"] = np.where(lab_mean >= 0, 1.0, -1.0)
    targets["next nt == A"] = 2 * (maj == 0).astype(float) - 1
    split = _split3(len(Cd), seed + 1)
    title = f"DNA promoter (V=4, w={w})"
    print(f"\n########## {title}, {len(Cd)} contexts, C_D={V ** w / len(split[0]):.2f} ##########")
    rows = auc_report(Cd, targets, Psi, V, w, split, ks,
                      full_for=["REAL promoter label", "next nt == A"])
    return title, rows


def run_protein(w=4, n_seqs=15000, max_windows=600_000, test_frac=0.25, seed=0,
                ks=(1, 2, 4, 8, 16, 32, 64, 128, 256, 512)):
    """V=20 decisive test: real proteins + the degree-3 N-glyc sequon (real biology).

    w=4 keeps V^w=160k close to the #distinct windows (density constant C_D~=1.6),
    so the dense empirical spectrum does NOT alias -- a clean test of whether the
    degree-3 sequon coefficient is recovered.  (w>=5 makes V^w >> m and the naive
    dense spectrum aliases; that larger regime is exactly what GL/CSAMP is for.)
    The sequon label is a deterministic function of the window, so the window-level
    split has no label leakage.  Householder is the ONLY feasible basis (2^(20*w))."""
    from .protein_data import build_windows
    C, y, csite = build_windows(w, n_seqs, max_windows, seed)
    V = 20
    Psi = householder_basis(V)
    # de-duplicate windows; label is deterministic so each distinct window -> one label
    Cd, first = np.unique(C, axis=0, return_index=True)
    yd = y[first].astype(float)
    targets = cal_targets(Cd, Psi, w, seed)
    seq_name = f"REAL N-glyc sequon @{csite},{csite+1},{csite+2}"
    targets[seq_name] = yd
    split = _split3(len(Cd), seed + 1)
    title = f"Proteins (V=20, w={w})"
    print(f"\n########## {title}, {len(Cd)} distinct windows, "
          f"C_D={V ** w / len(split[0]):.2f}, sequon rate {(yd > 0).mean():.4f} ##########")
    print(f"sequon positions {csite},{csite+1},{csite+2} (N-X-[S/T]); others are context distractors")
    rows = auc_report(Cd, targets, Psi, V, w, split, ks, full_for=[seq_name])
    return title, rows


def _figure(datasets):
    """Grouped-bar AUC per method, for the deg-3 calibration (where GL should win)
    and every REAL target (where we ask whether it does)."""
    import os

    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    methods = ["FourierGL", "logit<=1", "logit<=2", "GBT", "MLP"]
    keys = ["auc", "l1", "l2", "GBT", "MLP"]
    panels = []
    for title, rows in datasets:
        for name, r in rows.items():
            if "calib" in name and "deg-3" not in name:
                continue                                   # show only the deg-3 calibration + real targets
            panels.append((f"{title.split('(')[0].strip()}\n{name}", r))
    fig, ax = plt.subplots(figsize=(max(8, 1.7 * len(panels)), 4.6))
    x = np.arange(len(panels))
    width = 0.16
    for j, (m, k) in enumerate(zip(methods, keys)):
        vals = [p[1]["fp"]["auc"] if k == "auc" else p[1].get(k, float("nan")) for p in panels]
        ax.bar(x + (j - 2) * width, [v if v == v else 0 for v in vals], width, label=m)
    ax.axhline(0.5, ls=":", c="gray", lw=1)
    ax.set_xticks(x)
    ax.set_xticklabels([p[0] for p in panels], fontsize=7)
    ax.set_ylabel("held-out AUC")
    ax.set_ylim(0.4, 1.03)
    ax.set_title("Dataset-GL vs low-order & standard baselines  (GL wins only on the synthetic degree-3 target)")
    ax.legend(fontsize=8, ncol=5, loc="lower center")
    fig.tight_layout()
    os.makedirs("outputs", exist_ok=True)
    fig.savefig("outputs/fig_audit.png", dpi=150, bbox_inches="tight")
    plt.close(fig)
    print("\nwrote outputs/fig_audit.png")


def main():
    d1 = run_dna()
    d2 = run_protein()
    _figure([d1, d2])


if __name__ == "__main__":
    main()
