"""Is the target Fourier-sparse over the dataset? (done right this time)

Prior mistake: I compared coefficient *counts above an absolute threshold*
across encodings, which just measured C_D = 2^n/m (Mass Identity), not structure.
Here we use basis-fair measures in a DE-ALIASED (2-bit) basis:
  - held-out top-K prediction accuracy vs K   (the thing that actually matters)
  - normalized spectral norm ||f_breve||_1     (KM/GL efficient iff small)
  - effective # coefficients (participation ratio)

Targets are the functions classification LABELS actually are (motif detectors:
monomials / DNF = bounded ||f_hat||_1), with next-nucleotide as the diffuse
control, plus planted junta/random to CALIBRATE the instrument on real DNA.

    uv run python -m fda_exp.sparsity
"""

from __future__ import annotations

import os

import numpy as np

from .predict import reconstruct
from .spectrum import dataset_coeffs, density_constant

_ID = {"A": 0, "C": 1, "G": 2, "T": 3}
_CACHE = os.path.join(os.path.dirname(__file__), "..", "data", "dna_promoters_train.parquet")


def load_promoters(n_seqs=8000, seed=0):
    import pyarrow.parquet as pq
    t = pq.read_table(os.path.abspath(_CACHE), columns=["seq", "label"])
    seqs = [s.upper() for s in t.column("seq").to_pylist()]
    labs = list(t.column("label").to_pylist())
    idx = np.random.default_rng(seed).permutation(len(seqs))[:n_seqs]   # shuffle -> both classes
    return [seqs[i] for i in idx], [labs[i] for i in idx]


def build_windows(w, n_seqs=8000, max_pairs=200_000):
    seqs, labs = load_promoters(n_seqs)
    ctx, nxt, lab = [], [], []
    for s, y in zip(seqs, labs):
        ids = [_ID.get(c, -1) for c in s]
        sl = 1 if y == 1 else -1                        # source-sequence label (promoter vs not)
        for i in range(len(ids) - w):
            c = ids[i:i + w]
            t = ids[i + w]
            if t < 0 or min(c) < 0:
                continue
            ctx.append(c)
            nxt.append(t)
            lab.append(sl)
        if len(ctx) >= max_pairs:
            break
    return (np.array(ctx[:max_pairs], dtype=np.int64),
            np.array(nxt[:max_pairs], dtype=np.int64),
            np.array(lab[:max_pairs], dtype=np.int64))


def encode_twobit(C):
    """(m,w) nucleotide ids -> (m,2w) +/-1 (bijective, no one-hot aliasing)."""
    m, w = C.shape
    X = np.empty((m, 2 * w), dtype=np.int64)
    X[:, 0::2] = 1 - 2 * (C & 1)
    X[:, 1::2] = 1 - 2 * ((C >> 1) & 1)
    return X


def _has_motif(Cd, motif_ids):
    """+/-1 : does the window contain `motif_ids` at any offset?"""
    w = Cd.shape[1]
    L = len(motif_ids)
    hit = np.zeros(len(Cd), dtype=bool)
    for off in range(w - L + 1):
        m = np.ones(len(Cd), dtype=bool)
        for j, b in enumerate(motif_ids):
            m &= (Cd[:, off + j] == b)
        hit |= m
    return 2 * hit.astype(np.int64) - 1


def make_targets(Cd, maj, seed=0):
    rng = np.random.default_rng(seed)
    w = Cd.shape[1]
    jp = [p for p in (0, 2, 4) if p < w][:3]                # w-safe junta positions
    # 1-sparse, balanced: purine/pyrimidine at a fixed position (single 2-bit char)
    onesparse = (1 - 2 * (Cd[:, jp[0]] & 1)).astype(np.float64)
    # junta: parity of |jp| bits -> 1 heavy coeff, balanced
    junta = np.prod(1 - 2 * (Cd[:, jp] & 1), axis=1).astype(np.float64)
    # balanced DNF: OR of many 2-mers, tuned to ~50% occurrence (motif-detector = what labels ARE)
    orm = np.zeros(len(Cd), bool)
    for off in range(0, w - 1, 2):
        orm |= (Cd[:, off] == 0) & (Cd[:, off + 1] == 0)          # "AA" at even offsets
    dnf = 2 * orm.astype(np.float64) - 1
    return {
        "1-sparse (pos2 purine)": onesparse,
        "junta-3 (parity)": junta,
        "motif-DNF (~50%, bal.)": dnf,
        "next nt == A": 2 * (maj == 0).astype(np.float64) - 1,
        "random labels": rng.choice([-1.0, 1.0], size=len(Cd)),
    }


def analyze(Dtr, ftr, Dte, fte, n, ks):
    """Spectral sparsity + held-out top-K accuracy (2-bit, de-aliased)."""
    fhat = dataset_coeffs(Dtr, ftr, n)
    cd = density_constant(n, len(Dtr))
    fbreve2 = (fhat ** 2) / cd
    total = fbreve2.sum()
    l1 = float(np.abs(fhat).sum() / np.sqrt(cd))
    eff = float(total ** 2 / ((fbreve2 ** 2).sum()))            # participation ratio
    order = np.argsort(-np.abs(fhat))
    cd_inv = len(Dtr) / (1 << n)
    fc = fte - fte.mean()
    corrs = []
    for K in ks:
        sel = order[:K]
        g = reconstruct(Dte, n, sel, fhat[sel], cd_inv)
        gc = g - g.mean()
        d = np.sqrt((gc ** 2).sum() * (fc ** 2).sum()) + 1e-12
        corrs.append(float((gc * fc).sum() / d))            # imbalance-robust
    ceil = max(corrs)
    kstar = next((ks[i] for i, c in enumerate(corrs) if c >= 0.95 * ceil), ks[-1]) if ceil > 0.05 else ks[-1]
    return dict(l1=l1, eff=eff, rate=float((fte == 1).mean()), corrs=corrs, ceil=ceil, kstar=kstar)


def run(w=10, n_seqs=8000, max_pairs=200_000, test_frac=0.25, seed=0,
        ks=(1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 4096)):
    C, nxt, lab = build_windows(w, n_seqs, max_pairs)
    Cd, inv = np.unique(C, axis=0, return_inverse=True)
    maj = np.array([np.bincount(nxt[inv == g], minlength=4).argmax() for g in range(len(Cd))])
    # real label: is this k-mer more often from a promoter than a negative sequence?
    lab_mean = np.array([lab[inv == g].mean() for g in range(len(Cd))])
    X = encode_twobit(Cd)
    n = X.shape[1]
    targets = make_targets(Cd, maj, seed)
    targets["REAL: promoter-assoc k-mer"] = np.sign(lab_mean + 1e-9)

    rng = np.random.default_rng(seed + 1)
    perm = rng.permutation(len(Cd))
    nte = int(test_frac * len(Cd))
    te, tr = perm[:nte], perm[nte:]
    Dtr, Dte = X[tr], X[te]

    print(f"promoter windows: w={w}, 2-bit n={n}, distinct contexts={len(Cd)} "
          f"(train {len(Dtr)}, test {len(Dte)}), C_D={ (1<<n)/len(Dtr):.1f}\n")
    print(f"{'target':>24} | {'rate':>5} {'ceilCorr':>8} {'K*(95%)':>8} |   test corr(g,f) @ K=1,4,16,64,256,1024")
    print("-" * 104)
    results = {}
    for name, f in targets.items():
        r = analyze(Dtr, f[tr].astype(float), Dte, f[te].astype(float), n, ks)
        results[name] = r
        pick = [r["corrs"][ks.index(k)] for k in (1, 4, 16, 64, 256, 1024)]
        print(f"{name:>24} | {r['rate']:>5.2f} {r['ceil']:>8.2f} {r['kstar']:>8} | "
              + " ".join(f"{a:>5.2f}" for a in pick))
    _fig(results, ks, w, n)
    return results


def _fig(results, ks, w, n):
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    os.makedirs("outputs", exist_ok=True)
    fig, ax = plt.subplots(figsize=(7, 4.6))
    for name, r in results.items():
        ax.semilogx(ks, r["corrs"], "o-", ms=3, label=name)
    ax.set_xlabel("K = number of top de-aliased (2-bit) coefficients")
    ax.set_ylabel("held-out correlation corr(reconstruction, target)")
    ax.set_title(f"How many Fourier coefficients does each target need?  (promoters, w={w}, n={n})")
    ax.legend(fontsize=7, loc="lower right")
    fig.tight_layout()
    fig.savefig("outputs/fig_sparsity.png", dpi=150, bbox_inches="tight")
    plt.close(fig)


def main():
    run()


if __name__ == "__main__":
    main()
