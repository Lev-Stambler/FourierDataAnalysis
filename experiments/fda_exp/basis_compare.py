"""Was the basis wrong?  Compare the label's Fourier sparsity across three bases.

For DNA promoter windows (V=4), measure held-out top-K correlation vs K for the
true label (+ calibration/controls) in:
  - householder   : categorical, Householder-orthonormalized  (the principled basis)
  - walsh(2bit)   : categorical, Z_2^2 Walsh                   (V=4 de-aliased)
  - onehot-binary : {-1,1}^{4w} Boolean                        ("wrong"/aliased)

    uv run python -m fda_exp.basis_compare
"""

from __future__ import annotations

import numpy as np

from .householder import analyze_qary, householder_basis, walsh4
from .predict import reconstruct as bool_reconstruct
from .sparsity import build_windows, make_targets
from .spectrum import dataset_coeffs


def encode_onehot(Cd, V=4):
    m, w = Cd.shape
    X = -np.ones((m, V * w), dtype=np.int64)
    cols = np.arange(w)[None, :] * V + Cd
    X[np.arange(m)[:, None], cols] = 1
    return X


def analyze_boolean(Xtr, ftr, Xte, fte, n, ks):
    fhat = dataset_coeffs(Xtr, ftr, n)
    order = np.argsort(-np.abs(fhat))
    cd_inv = len(Xtr) / (1 << n)
    fc = fte - fte.mean()
    corrs = []
    for K in ks:
        sel = order[:K]
        g = bool_reconstruct(Xte, n, sel, fhat[sel], cd_inv)
        gc = g - g.mean()
        den = np.sqrt((gc ** 2).sum() * (fc ** 2).sum()) + 1e-12
        corrs.append(float((gc * fc).sum() / den))
    ceil = max(corrs)
    kstar = (next((ks[i] for i, c in enumerate(corrs) if c >= 0.95 * ceil), ks[-1])
             if ceil > 0.05 else ks[-1])
    return dict(corrs=corrs, ceil=ceil, kstar=kstar)


def run(w=6, n_seqs=8000, max_pairs=200_000, test_frac=0.25, seed=0,
        ks=(1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 4096)):
    C, nxt, lab = build_windows(w, n_seqs, max_pairs)
    Cd, inv = np.unique(C, axis=0, return_inverse=True)
    maj = np.array([np.bincount(nxt[inv == g], minlength=4).argmax() for g in range(len(Cd))])
    lab_mean = np.array([lab[inv == g].mean() for g in range(len(Cd))])
    targets = make_targets(Cd, maj, seed)
    targets["REAL promoter label"] = np.sign(lab_mean + 1e-9)

    rng = np.random.default_rng(seed + 1)
    perm = rng.permutation(len(Cd))
    nte = int(test_frac * len(Cd))
    te, tr = perm[:nte], perm[nte:]

    n_bool = 4 * w
    print(f"DNA promoter windows: w={w}, V=4, distinct contexts={len(Cd)} "
          f"(train {len(tr)}, test {len(te)}); one-hot-binary n={n_bool}\n")

    bases = {
        "householder": ("qary", householder_basis(4)),
        "walsh(2bit)": ("qary", walsh4()),
        "onehot-binary": ("bool", None),
    }
    Xoh_tr = Xoh_te = None
    if n_bool <= 24:
        Xoh = encode_onehot(Cd)
        Xoh_tr, Xoh_te = Xoh[tr], Xoh[te]

    print(f"{'target':>22} {'basis':>14} | {'ceil':>5} {'K*':>6} |  corr @ K=1,4,16,64,256,1024")
    print("-" * 100)
    results = {}
    for tname, f in targets.items():
        results[tname] = {}
        for bname, (kind, Psi) in bases.items():
            if kind == "qary":
                r = analyze_qary(Cd[tr], f[tr].astype(float), Cd[te], f[te].astype(float), Psi, ks)
            else:
                if Xoh_tr is None:
                    continue
                r = analyze_boolean(Xoh_tr, f[tr].astype(float), Xoh_te, f[te].astype(float), n_bool, ks)
            results[tname][bname] = r
            pick = [r["corrs"][ks.index(k)] for k in (1, 4, 16, 64, 256, 1024)]
            print(f"{tname:>22} {bname:>14} | {r['ceil']:>5.2f} {r['kstar']:>6} | "
                  + " ".join(f"{a:>5.2f}" for a in pick))
        print()
    _fig(results.get("REAL promoter label", {}), ks, w)
    return results


def _fig(label_res, ks, w):
    if not label_res:
        return
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    import os
    os.makedirs("outputs", exist_ok=True)
    fig, ax = plt.subplots(figsize=(6.6, 4.4))
    for bname, r in label_res.items():
        ax.semilogx(ks, r["corrs"], "o-", ms=3, label=f"{bname} (K*={r['kstar']})")
    ax.set_xlabel("K = top coefficients")
    ax.set_ylabel("held-out corr(reconstruction, label)")
    ax.set_title(f"REAL promoter label: sparsity by basis (DNA, w={w})")
    ax.legend(fontsize=8)
    fig.tight_layout()
    fig.savefig("outputs/fig_basis_compare.png", dpi=150, bbox_inches="tight")
    plt.close(fig)


def main():
    run()


if __name__ == "__main__":
    main()
