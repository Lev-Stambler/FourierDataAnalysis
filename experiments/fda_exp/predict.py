"""Vibe check: predict the next token from only the top-K heavy dataset
coefficients (Kushilevitz--Mansour over the dataset).

Task: tiny-stories next-token prediction.  We predict the *full* next token by
predicting each of its ``bits_per_token`` bits with a sparse reconstruction

    g_j(x) = C_D^{-1} * sum_{S in top-K} fhat_D^{(j)}(S) * chi_S(x)         (prop:inversion, truncated)

built from the K largest-magnitude dataset coefficients of bit ``j``.  A
train/test split over *distinct contexts* makes this a genuine generalization
test: coefficients are fit on train contexts, accuracy is measured on held-out
contexts (so ``g`` must extrapolate off the training data).

    uv run python -m fda_exp.predict
"""

from __future__ import annotations

import numpy as np

from .datasets import _STORY_TOKENS, _gen_story_stream, _token_bits
from .spectrum import dataset_coeffs


def build_next_token(window=4, bpt=4, n_sentences=8000, seed=0):
    """Return (contexts X in {-1,1}^n, next-token ids y)."""
    rng = np.random.default_rng(seed)
    stream = _gen_story_stream(n_sentences, rng)
    rows, nxt = [], []
    for i in range(len(stream) - window):
        rows.append(np.concatenate([_token_bits(t, bpt) for t in stream[i:i + window]]))
        nxt.append(stream[i + window])
    return np.array(rows, dtype=np.int64), np.array(nxt, dtype=np.int64)


def majority_by_context(X, y):
    uniq, inv = np.unique(X, axis=0, return_inverse=True)
    maj = np.array([np.bincount(y[inv == g]).argmax() for g in range(len(uniq))])
    return uniq, maj


def _bit_pm(tokens, j):
    """+/-1 encoding of bit j of each token id (matches _token_bits)."""
    return 1 - 2 * ((tokens >> j) & 1)


def top_k_coeffs(D, fD, n, k):
    fhat = dataset_coeffs(D, fD, n)
    sel = np.argsort(-np.abs(fhat))[:k]
    return sel, fhat[sel]


def reconstruct(Xq, n, S_masks, coeffs, cd_inv):
    """g(x) = cd_inv * sum_S coeff_S chi_S(x) over query points Xq (q, n)."""
    q = Xq.shape[0]
    g = np.zeros(q)
    for S, c in zip(S_masks, coeffs):
        cols = [i for i in range(n) if (S >> i) & 1]
        chi = np.prod(Xq[:, cols], axis=1) if cols else np.ones(q)
        g += c * chi
    return cd_inv * g


def decode_context(x, window, bpt):
    toks = []
    for w in range(window):
        bits = x[w * bpt:(w + 1) * bpt]
        tid = sum(((1 - int(v)) // 2) << j for j, v in enumerate(bits))
        toks.append(_STORY_TOKENS[tid] if tid < len(_STORY_TOKENS) else f"?{tid}")
    return " ".join(toks)


def predict_tokens(Dtr, ytr, Dte, n, bpt, k):
    """Predict next-token ids on Dte from the top-``k`` coeffs (per bit) fit on Dtr."""
    cd_inv = Dtr.shape[0] / (1 << n)
    pred = np.zeros(Dte.shape[0], dtype=np.int64)
    for j in range(bpt):
        fj = _bit_pm(ytr, j).astype(float)
        S_masks, coeffs = top_k_coeffs(Dtr, fj, n, k)
        g = reconstruct(Dte, n, S_masks, coeffs, cd_inv)
        bit_j = (g < 0).astype(np.int64)  # sign -> bit: +1->0, -1->1
        pred |= (bit_j << j)
    return pred


def run(window=4, bpt=4, n_sentences=8000, seed=0,
        ks=(10, 50, 100, 300, 1000), test_frac=0.25):
    X, y = build_next_token(window, bpt, n_sentences, seed)
    D, maj = majority_by_context(X, y)
    n = D.shape[1]

    rng = np.random.default_rng(seed + 7)
    perm = rng.permutation(len(D))
    n_te = int(test_frac * len(D))
    te, tr = perm[:n_te], perm[n_te:]
    Dtr, ytr, Dte, yte = D[tr], maj[tr], D[te], maj[te]

    global_majority = np.bincount(ytr).argmax()
    base = float((yte == global_majority).mean())
    # is-verb accuracy uses the semantic bits set {8,9,10,11} (high bit == token>=8)
    verbs = {8, 9, 10, 11}
    yte_verb = np.array([t in verbs for t in yte])

    print(f"tiny-stories next-token  |  n={n}  distinct contexts={len(D)} "
          f"(train {len(Dtr)}, test {len(Dte)})  C_D={ (1<<n)/len(Dtr):.1f}")
    print(f"baseline (always predict '{_STORY_TOKENS[global_majority]}'): "
          f"token acc {base:.3f}")
    print(f"{'top-K':>7} | {'token acc':>9} | {'is-verb acc':>11} | full-coeff? ")
    print("-" * 46)
    results = {}
    for k in list(ks) + ["all"]:
        kk = (1 << n) if k == "all" else k
        pred = predict_tokens(Dtr, ytr, Dte, n, bpt, kk)
        tok_acc = float((pred == yte).mean())
        pred_verb = np.array([t in verbs for t in pred])
        verb_acc = float((pred_verb == yte_verb).mean())
        results[str(k)] = dict(token_acc=tok_acc, verb_acc=verb_acc)
        print(f"{str(k):>7} | {tok_acc:9.3f} | {verb_acc:11.3f} | "
              f"{'(exact on train)' if k == 'all' else ''}")

    _save_figure(results, base, ks)

    # a few readable examples at K=100
    print("\nExamples (top-100 predictor, held-out contexts):")
    pred100 = predict_tokens(Dtr, ytr, Dte, n, bpt, 100)
    rng2 = np.random.default_rng(1)
    for i in rng2.choice(len(Dte), size=min(12, len(Dte)), replace=False):
        ctx = decode_context(Dte[i], window, bpt)
        pt = _STORY_TOKENS[pred100[i]] if pred100[i] < len(_STORY_TOKENS) else "?"
        at = _STORY_TOKENS[yte[i]]
        mark = "OK " if pred100[i] == yte[i] else "x  "
        print(f"  {mark}[{ctx:>28}]  ->  pred '{pt:5}'  actual '{at}'")
    return results


def _save_figure(results, base, ks, path="outputs/fig_predict_topk.png"):
    import os

    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    os.makedirs(os.path.dirname(path), exist_ok=True)
    kk = list(ks)
    tok = [results[str(k)]["token_acc"] for k in kk]
    verb = [results[str(k)]["verb_acc"] for k in kk]
    fig, ax = plt.subplots(figsize=(6.4, 4.2))
    ax.semilogx(kk, tok, "o-", label="next-token accuracy")
    ax.semilogx(kk, verb, "s-", label="is-verb-next accuracy")
    ax.axhline(base, ls="--", color="grey", label=f"majority baseline ({base:.2f})")
    ax.axhline(results["all"]["token_acc"], ls=":", color="red",
               label="all coeffs = the lift (memorizes; 0 off-data)")
    ax.set_xlabel("K = number of heavy dataset coefficients used")
    ax.set_ylabel("held-out accuracy")
    ax.set_ylim(-0.03, 1.03)
    ax.set_title("Next token from top-K on-dataset Fourier coefficients")
    ax.legend(fontsize=8)
    fig.tight_layout()
    fig.savefig(path, dpi=150, bbox_inches="tight")
    plt.close(fig)


def main():
    run()


if __name__ == "__main__":
    main()
