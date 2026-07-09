"""How many top Fourier coefficients does DNA next-nucleotide prediction need?

Compute the full dataset spectrum of each nucleotide indicator (exact, n small),
keep only the top-K by magnitude, reconstruct, argmax, and measure held-out
accuracy as a function of K.  Answers "what eps / how many top coefficients".

    uv run python -m fda_exp.dna_topk
"""

from __future__ import annotations

import numpy as np

from .dna_data import dna_next_nucleotide
from .predict import majority_by_context, reconstruct
from .spectrum import dataset_coeffs, popcount


def sweep(window=6, n_seqs=20000, test_frac=0.25, seed=0,
          ks=(1, 3, 10, 30, 100, 300, 1000, 3000, 10000, 30000)):
    X, y, vocab, w, block = dna_next_nucleotide(window=window, n_seqs=n_seqs)
    D, maj = majority_by_context(X, y)
    n = D.shape[1]
    V = len(vocab)

    rng = np.random.default_rng(seed + 7)
    perm = rng.permutation(len(D))
    n_te = int(test_frac * len(D))
    te, tr = perm[:n_te], perm[n_te:]
    Dtr, ytr, Dte, yte = D[tr], maj[tr], D[te], maj[te]
    cd_inv = len(Dtr) / (1 << n)
    base = float((yte == np.bincount(ytr).argmax()).mean())
    maxK = max(ks)

    # top-maxK coefficients of each nucleotide indicator (exact full spectrum)
    top_idx, top_val, deg1 = [], [], []
    for t in range(V):
        ft = (2 * (ytr == t) - 1).astype(float)
        fhat = dataset_coeffs(Dtr, ft, n)                    # full 2^n spectrum
        order = np.argsort(-np.abs(fhat))[:maxK]
        top_idx.append(order)
        top_val.append(fhat[order])
        # how much mass is in degree<=1 coeffs (position-nucleotide biases)?
        deg = popcount(order)
        deg1.append(int((deg <= 1).sum()))

    print(f"\nDNA next-nt, n={n}, V={V}, train contexts={len(Dtr)}, test={len(Dte)}")
    print(f"baseline={base:.3f}   (of top-{maxK} coeffs/nt, ~{np.mean(deg1):.0f} are degree<=1)")
    print(f"{'K':>7} | {'top-1':>7}")
    accs = []
    for K in ks:
        scores = np.full((len(Dte), V), -1e9)
        for t in range(V):
            scores[:, t] = reconstruct(Dte, n, top_idx[t][:K], top_val[t][:K], cd_inv)
        acc = float((scores.argmax(1) == yte).mean())
        accs.append(acc)
        print(f"{K:>7} | {acc:>7.3f}")

    _fig(ks, accs, base, path="outputs/fig_dna_topk.png")
    return dict(ks=list(ks), accs=accs, baseline=base)


def _fig(ks, accs, base, path):
    import os

    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    os.makedirs(os.path.dirname(path), exist_ok=True)
    fig, ax = plt.subplots(figsize=(6.4, 4.2))
    ax.semilogx(ks, accs, "o-", label="top-K Fourier (next-nt argmax)")
    ax.axhline(base, ls="--", color="grey", label=f"baseline ({base:.2f})")
    ax.set_xlabel("K = number of top Fourier coefficients per nucleotide")
    ax.set_ylabel("held-out top-1 accuracy")
    ax.set_title("How many top Fourier coefficients does DNA need?")
    ax.legend(fontsize=9)
    fig.tight_layout()
    fig.savefig(path, dpi=150, bbox_inches="tight")
    plt.close(fig)


def main():
    sweep()


if __name__ == "__main__":
    main()
