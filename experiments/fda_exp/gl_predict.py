"""End-to-end: run the in-domain GL (thm:context-gl) to *recover* the heavy
coefficients, then predict the next token from ONLY the recovered set --- and
print held-out examples.  Works on the synthetic grammar or on real TinyStories.

Pipeline (per bit ``j`` of the next token):
  1. ``context_gl_search`` on the training contexts -> recovered heavy set ``L_j``
     (samples-only SAMP+CSAMP, or exact),
  2. score those coefficients (``fhat_D(S)`` for ``S in L_j``),
  3. reconstruct ``g_j = C_D^{-1} sum_{S in L_j} fhat_D(S) chi_S`` and read bit ``j``
     on held-out contexts.

    uv run python -m fda_exp.gl_predict --source tinystories --mode sampled
    uv run python -m fda_exp.gl_predict --source synthetic --mode exact
"""

from __future__ import annotations

import argparse

import numpy as np

from .context_gl import brute_force_heavy, context_gl_search
from .hf_data import decode_context
from .predict import _bit_pm, majority_by_context, reconstruct
from .spectrum import coeffs_at, dataset_coeffs


def run(X, y, vocab, window, bpt, seed=0, tau=0.5, mode="sampled",
        n_exp=20000, test_frac=0.25, n_examples=16):
    D, maj = majority_by_context(X, y)
    n = D.shape[1]

    rng = np.random.default_rng(seed + 7)
    perm = rng.permutation(len(D))
    n_te = int(test_frac * len(D))
    te, tr = perm[:n_te], perm[n_te:]
    Dtr, ytr, Dte, yte = D[tr], maj[tr], D[te], maj[te]
    cd_inv = Dtr.shape[0] / (1 << n)

    print(f"\nnext-token via in-domain GL  n={n}  distinct contexts={len(D)} "
          f"(train {len(Dtr)}, test {len(Dte)})  C_D={ (1 << n) / len(Dtr):.1f}")
    print(f"recover heavy coeffs  [mode={mode}, tau={tau}"
          + (f", n_exp={n_exp}" if mode == "sampled" else "") + "]\n")

    pred = np.zeros(len(Dte), dtype=np.int64)
    per_bit_acc = []
    total_exps = 0
    for j in range(bpt):
        fj = _bit_pm(ytr, j).astype(float)
        L, exps = context_gl_search(Dtr, fj, tau, mode=mode, n_exp=n_exp, seed=seed + j)
        total_exps += exps
        heavy_true, _ = brute_force_heavy(Dtr, fj, tau)
        recall = len(set(L) & set(heavy_true)) / max(len(heavy_true), 1)
        g = reconstruct(Dte, n, np.array(L), coeffs_at(Dtr, fj, L), cd_inv)
        bit_pred = (g < 0).astype(np.int64)
        pred |= (bit_pred << j)
        per_bit_acc.append(float((bit_pred == ((yte >> j) & 1)).mean()))
        print(f"  bit {j}: |L_{j}|={len(L):4d}  recall(vs exact)={recall:.2f}  "
              f"bit-acc={per_bit_acc[-1]:.3f}")

    tok_acc = float((pred == yte).mean())
    base = float((yte == np.bincount(ytr).argmax()).mean())
    print(f"\nnext-token accuracy from GL-recovered coeffs: {tok_acc:.3f}   "
          f"(mean per-bit {np.mean(per_bit_acc):.3f}; majority baseline {base:.3f})")
    if mode == "sampled":
        print(f"SAMP+CSAMP experiments used: {total_exps / 1e6:.0f}M")

    print(f"\nHeld-out examples (predicted next token from the GL-recovered function):")
    ex = np.random.default_rng(3).choice(len(Dte), size=min(n_examples, len(Dte)), replace=False)
    for i in ex:
        ctx = decode_context(Dte[i], window, bpt, vocab)
        pt = vocab[pred[i]] if pred[i] < len(vocab) else f"?{pred[i]}"
        at = vocab[yte[i]] if yte[i] < len(vocab) else "?"
        mark = "OK " if pred[i] == yte[i] else "x  "
        print(f"  {mark}[{ctx:>34}]  ->  '{pt}'   (actual '{at}')")
    return dict(token_acc=tok_acc, per_bit=float(np.mean(per_bit_acc)), baseline=base)


def run_onevsrest(X, y, vocab, window, bpt, seed=0, tau=0.15, mode="exact",
                  n_exp=20000, test_frac=0.25, n_examples=16, compare_sklearn=False,
                  decode_fn=None):
    """Next-token prediction the RIGHT way for real vocabularies: one Boolean
    function per token ("is the next token = t"), GL-recover its heavy
    coefficients, reconstruct a score, and argmax over tokens.  Unlike predicting
    the (meaningless) bits of a token id, each indicator is a natural function."""
    D, maj = majority_by_context(X, y)
    n = D.shape[1]
    V = len(vocab)
    rng = np.random.default_rng(seed + 7)
    perm = rng.permutation(len(D))
    n_te = int(test_frac * len(D))
    te, tr = perm[:n_te], perm[n_te:]
    Dtr, ytr, Dte, yte = D[tr], maj[tr], D[te], maj[te]
    cd_inv = len(Dtr) / (1 << n)

    scores = np.full((len(Dte), V), -1e9)
    sizes = []
    for t in range(V):
        if (ytr == t).sum() == 0:
            continue
        ft = (2 * (ytr == t) - 1).astype(float)
        L, _ = context_gl_search(Dtr, ft, tau, mode=mode, n_exp=n_exp, seed=seed + t)
        sizes.append(len(L))
        if L:
            scores[:, t] = reconstruct(Dte, n, np.array(L), coeffs_at(Dtr, ft, L), cd_inv)

    pred = scores.argmax(1)
    top3 = np.argsort(-scores, 1)[:, :3]
    top1 = float((pred == yte).mean())
    top3_acc = float(np.mean([yte[i] in top3[i] for i in range(len(yte))]))
    base = float((yte == np.bincount(ytr).argmax()).mean())

    out = dict(n=n, V=V, m_train=int(len(Dtr)), m_test=int(len(Dte)),
               top1=top1, top3=top3_acc, baseline=base, avg_L=float(np.mean(sizes)),
               tau=tau, mode=mode, unk_frac=float((yte == 0).mean()))

    if compare_sklearn:
        from sklearn.linear_model import LogisticRegression
        Xtr, Xte = (1 - Dtr) // 2, (1 - Dte) // 2
        lr = LogisticRegression(max_iter=300).fit(Xtr, ytr)
        P = lr.predict_proba(Xte); cls = lr.classes_
        out["lr_top1"] = float((cls[P.argmax(1)] == yte).mean())
        t3 = cls[np.argsort(-P, 1)[:, :3]]
        out["lr_top3"] = float(np.mean([yte[i] in t3[i] for i in range(len(yte))]))

    examples = []
    for i in np.random.default_rng(1).choice(len(Dte), size=min(n_examples, len(Dte)), replace=False):
        ctx = decode_fn(Dte[i], window) if decode_fn else decode_context(Dte[i], window, bpt, vocab)
        examples.append(f"{'OK' if pred[i]==yte[i] else 'x '} [{ctx}] -> {vocab[pred[i]]} (actual {vocab[yte[i]]})")
    out["examples"] = examples

    print(f"\none-vs-rest next-token (in-domain GL, mode={mode}, tau={tau})  n={n}, V={V}")
    print(f"  GL   top-1={top1:.3f}  top-3={top3_acc:.3f}  baseline={base:.3f}  avg|L|={np.mean(sizes):.0f}")
    if compare_sklearn:
        print(f"  LogReg top-1={out['lr_top1']:.3f}  top-3={out['lr_top3']:.3f}")
    for e in examples:
        print("   ", e)
    return out


def run_tinystories_onevsrest(window=3, vocab_size=48, n_stories=2500, **kw):
    from .hf_data import tinystories_next_token

    X, y, vocab, w, bpt = tinystories_next_token(window, vocab_size, n_stories, seed=kw.pop("seed", 0))
    return run_onevsrest(X, y, vocab, w, bpt, **kw)


def run_dna_onevsrest(window=6, n_seqs=20000, **kw):
    from . import dna_data

    X, y, vocab, w, block = dna_data.dna_next_nucleotide(window=window, n_seqs=n_seqs)
    return run_onevsrest(X, y, vocab, w, block, decode_fn=dna_data.decode_context, **kw)


def run_synthetic(window=4, bpt=4, n_sentences=8000, **kw):
    from .datasets import _STORY_TOKENS
    from .predict import build_next_token

    X, y = build_next_token(window, bpt, n_sentences, kw.get("seed", 0))
    return run(X, y, _STORY_TOKENS, window, bpt, **kw)


def run_tinystories(window=3, vocab_size=64, n_stories=3000, max_pairs=300_000, **kw):
    from .hf_data import tinystories_next_token

    X, y, vocab, w, bpt = tinystories_next_token(window, vocab_size, n_stories, max_pairs,
                                                 seed=kw.get("seed", 0))
    return run(X, y, vocab, w, bpt, **kw)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--source", default="dna", choices=["dna", "tinystories", "synthetic"])
    ap.add_argument("--mode", default="exact", choices=["sampled", "exact"])
    ap.add_argument("--tau", type=float, default=None)
    ap.add_argument("--n_exp", type=int, default=30000)
    ap.add_argument("--window", type=int, default=None)
    ap.add_argument("--vocab_size", type=int, default=64)
    ap.add_argument("--n_stories", type=int, default=3000)
    ap.add_argument("--n_seqs", type=int, default=20000)
    args = ap.parse_args()
    if args.source == "dna":
        run_dna_onevsrest(window=args.window or 6, n_seqs=args.n_seqs, mode=args.mode,
                          tau=args.tau or 0.1, n_exp=args.n_exp, compare_sklearn=True, n_examples=14)
    elif args.source == "tinystories":
        run_tinystories_onevsrest(window=args.window or 3, vocab_size=args.vocab_size,
                                  n_stories=args.n_stories, mode=args.mode, tau=args.tau or 0.15,
                                  n_exp=args.n_exp, compare_sklearn=True)
    else:
        run_synthetic(window=4, bpt=4, mode=args.mode, tau=args.tau or 0.5, n_exp=args.n_exp)


if __name__ == "__main__":
    main()
