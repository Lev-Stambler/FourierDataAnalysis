"""Phase-diagram demonstration: interaction screening (= functional ANOVA) vs CSAMP.

Does long-context high-order — degree-3 interactions found by conditioning on the 3 interacting
positions (not the CSAMP suffix) — add next-symbol predictability that degree-<=2 (Fourier-Lasso's
forced fallback at scale) cannot?  Reports (a) the model-free collision-energy decomposition by
degree on train AND held-out test, and (b) a held-out ANOVA-reconstruction top-1 for degree-<=2 vs
+heavy-degree-3.  Char (V=27) then word (V=64).  The CSAMP corner (high-degree/short/repeating) is
the existing Poelwijk result (`qary_gl_predict.run_poelwijk`, 0.82 vs deg-<=2 0.74).

    uv run python -m fda_exp.interaction_predict
"""

from __future__ import annotations

from itertools import combinations
from math import comb

import numpy as np

from .interaction_gl import (
    anova_scores,
    energy_by_degree,
    interaction_energy,
    screen_collisions,
)


def _active_positions_multi(C, y, V, w, W, topk):
    """Heredity seed: positions with the heaviest degree-<=2 collision interaction energy."""
    E, sets = screen_collisions(C, y, V, w, W, max_degree=2)
    score = np.zeros(w)
    for S in sets:
        if S:
            iS = interaction_energy(E, S)
            for p in S:
                score[p] = max(score[p], iS)
    return sorted(int(p) for p in np.argsort(-score)[:topk])


def run_interactions(C, y, V, w, max_degree=3, active=None, n_heavy=64, test_frac=0.2, seed=0, label=""):
    rng = np.random.default_rng(seed + 7)
    perm = rng.permutation(len(C))
    nte, nval = int(test_frac * len(C)), int(0.1 * len(C))
    te, val, tr = perm[:nte], perm[nte:nte + nval], perm[nte + nval:]
    Ctr, ytr, Cval, yval, Cte, yte = C[tr], y[tr], C[val], y[val], C[te], y[te]
    W = int(y.max()) + 1
    positions = list(range(w)) if active is None else list(active)
    base = float((yte == np.bincount(ytr, minlength=W).argmax()).mean())
    print(f"\n########## {label} interaction screening (V={V}, w={w}, tr={len(Ctr)}, classes={W}, "
          f"positions screened={len(positions)}) ##########", flush=True)

    E, sets = screen_collisions(Ctr, ytr, V, w, W, max_degree=max_degree, active=active)
    Ete, _ = screen_collisions(Cte, yte, V, w, W, max_degree=max_degree, active=active)
    deg_tr = energy_by_degree(E, sets, w)
    deg_te = energy_by_degree(Ete, sets, w)                       # held-out: does degree-3 generalize?
    IS = [(S, interaction_energy(E, S)) for S in sets if len(S) >= 2]
    n_neg = sum(1 for _, v in IS if v < -1e-9)                    # non-product diagnostic (impossible if product)
    hi = sorted([(S, v) for S, v in IS if len(S) == 3], key=lambda t: -t[1])[:n_heavy]
    hi_sets = [S for S, _ in hi]
    print(f"  collision energy by degree — train {np.round(deg_tr[:max_degree + 1], 4).tolist()}", flush=True)
    print(f"  collision energy by degree — TEST  {np.round(deg_te[:max_degree + 1], 4).tolist()}", flush=True)
    print(f"  non-product diagnostic: {n_neg}/{len(IS)} interaction energies I_S < 0 "
          f"(0 iff product-D exactness holds)", flush=True)

    # STRICT nested held-out test: tune the SAME empirical-Bayes shrinkage per model on validation,
    # then evaluate on test -- so neither degree class is handicapped by a weak/over-fit baseline.
    base_sets = [()] + [(p,) for p in positions] + list(combinations(positions, 2))
    lams = [0.0, 2.0, 8.0, 32.0, 128.0]
    vsub = rng.choice(len(Cval), min(20000, len(Cval)), replace=False)
    tsub = rng.choice(len(Cte), min(30000, len(Cte)), replace=False)

    def tuned_test_acc(chosen):
        best = (-1.0, 0.0)
        for lam in lams:
            a = float((anova_scores(Ctr, ytr, Cval[vsub], chosen, V, W, shrink=lam).argmax(1) == yval[vsub]).mean())
            if a > best[0]:
                best = (a, lam)
        lam = best[1]
        return float((anova_scores(Ctr, ytr, Cte[tsub], chosen, V, W, shrink=lam).argmax(1) == yte[tsub]).mean()), lam

    acc2, lam2 = tuned_test_acc(base_sets)
    acc3, lam3 = tuned_test_acc(base_sets + hi_sets)
    n3 = comb(len(positions), 3)
    print(f"  STRICT held-out top-1 — fitted degree<=2 {acc2:.4f} (lam={lam2:g})   "
          f"+ {len(hi_sets)} heavy degree-3 {acc3:.4f} (lam={lam3:g})   majority {base:.4f}", flush=True)
    print(f"  screened {n3} triples; a Lasso degree-3 enumeration would need {n3 * (V - 1) ** 3:.1e} columns",
          flush=True)
    return dict(deg_tr=deg_tr.tolist(), deg_te=deg_te.tolist(), acc2=acc2, acc3=acc3, base=base,
                n_neg=n_neg, n_heavy=len(hi_sets), n_triples=n3)


def run_char(window=24, n_stories=120000, max_pairs=1_500_000, max_degree=3, heredity_topk=None, seed=0):
    """Char-level next-char (V=27, a-z+space).  heredity_topk=None -> blind full screen (moderate w);
    heredity_topk=k -> screen only among the k low-order-active positions (long w)."""
    from .hf_data import tinystories_char_next
    C, y, V, w = tinystories_char_next(window, n_stories, max_pairs, seed=seed)
    active = None if heredity_topk is None else _active_positions_multi(C, y, V, w, int(y.max()) + 1, heredity_topk)
    tag = "blind-full" if active is None else f"heredity-top{heredity_topk}"
    return run_interactions(C, y, V, w, max_degree=max_degree, active=active, seed=seed,
                            label=f"CHAR next-char w={window} ({tag})")


def run_word(window=12, vocab_size=64, n_stories=120000, max_pairs=1_500_000, max_degree=3,
             heredity_topk=None, seed=0):
    """Word-level next-token (V=vocab_size), in-vocab targets only."""
    from .hf_data import tinystories_next_token
    from .qary_gl_predict import _decode_tokens
    X, y, vocab, w, bpt = tinystories_next_token(window, vocab_size, n_stories, max_pairs, seed=seed)
    tok = _decode_tokens(X, window, bpt)
    keep = y != 0
    tok, y = tok[keep], y[keep]
    V = vocab_size
    active = None if heredity_topk is None else _active_positions_multi(tok, y, V, window, V, heredity_topk)
    tag = "blind-full" if active is None else f"heredity-top{heredity_topk}"
    return run_interactions(tok, y, V, window, max_degree=max_degree, active=active, seed=seed,
                            label=f"WORD next-token w={window} V={V} ({tag})")


def phase_diagram(device="cpu"):
    """ANOVA corner (char + word, long context) here; CSAMP corner = qary_gl_predict.run_poelwijk."""
    print("\n===== ANOVA / interaction-screening corner (long context, low degree) =====")
    run_char(window=24, heredity_topk=None)                      # blind-full moderate
    run_char(window=96, heredity_topk=16)                        # heredity-long
    run_word(window=12, vocab_size=64, heredity_topk=None)
    print("\n===== CSAMP corner (high degree, short/repeating): Poelwijk =====")
    from .qary_gl_predict import run_poelwijk
    run_poelwijk(seeds=(0,), device=device)


if __name__ == "__main__":
    phase_diagram()
