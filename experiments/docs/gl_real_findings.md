# The ACTUAL Goldreich-Levin algorithm, run on real data (Poelwijk epistasis)

Everything before this ran **Lasso over the enumerated Fourier basis** and loosely called it
"GL".  This runs the **actual algorithm** — `gl_torch.gl_search_torch`, the recursive heavy-Fourier-
bucket search whose only data access is **CSAMP** (draw a datapoint sharing a context suffix
`idx >> level` = subcube conditioning) — on the real Poelwijk mTagBFP2↔mKate2 landscape (n=13, all
2^13 measured).  `fda_exp/gl_real.py`.

## A. Correctness — GL recovers the true heavy coefficients (full landscape)

At threshold recovering the true top-20 heavy coefficients (200k oracle calls):

| phenotype | CSAMP recovered | recall | precision | degrees found |
|---|---|---|---|---|
| combined | 11 | 0.55 | **1.00** | deg 1,2,3 |
| red | 12 | 0.60 | **1.00** | deg 1,2,3,4 |

Precision 1.00 — everything GL returns is genuinely heavy, including real **order-2/3 epistatic**
terms. (Recall <1 because tree pruning under sampling noise drops some near-threshold coefficients;
more samples raise it — 40k→200k took top-15 recall 0.22→0.67.)

## B. Blindness — CSAMP is ESSENTIAL, on real biology (the paper's core claim)

Same run, `mode='samp'` (partners drawn context-**blind**) vs `mode='csamp'`:

| phenotype | CSAMP heavy found | SAMP heavy found | SAMP misses (order≥2) |
|---|---|---|---|
| combined | 11 | **0** | 8 |
| red | 12 | **0** | 8 |

**Context-blind sampling recovers *nothing*** on the real landscape; the high-order epistatic
coefficients are invisible without subcube conditioning.  This is the paper's central theoretical
prediction, confirmed on real biological data with the actual algorithm — the strongest result here.

## C. In-distribution prediction — GL is NOT a better predictor (honest negative)

Run GL on a 5192-variant TRAIN subsample, refit the recovered heavy support, predict 2000 held-out:

| method | held-out Spearman |
|---|---|
| GL-refit (CSAMP heavy support) | ~0.04 |
| brute-force top-K (same size) | ~0.26–0.38 |
| **true top-91 heavy** (from full data), refit | 0.36 |
| **Fourier-Lasso** (L1 over full 2^13 basis) | **0.866** |

The real reason is **not** that the signal is spread (an earlier draft claimed that — wrong).  The
L1 sparsity/accuracy tradeoff shows sparse prediction is easy:

| full-Walsh Lasso nnz | 4 | 9 | 50 | 231 | 1301 |
|---|---|---|---|---|---|
| test Spearman | 0.710 | 0.743 | 0.801 | **0.866** | 0.874 |

Just **4 coefficients** give 0.71.  So the failure is **selection paradigm**, not sparsity:
- **L1 joint selection** (Lasso): 0.87 — picks the *predictive* coefficients.
- **magnitude ranking** (brute top-K by |f_hat(S)|, which is fundamentally what GL does): **0.38** —
  even *perfect* magnitude ranking loses badly, because |f_hat(S)| ranks by marginal variance, not
  joint predictive value, and hard-threshold + OLS overfits vs L1 soft-shrinkage.
- **GL** (magnitude search + Ψ sampling noise on a subsample): **0.04** — worse still.

So GL is a **recovery / characterization** tool (find the heavy coefficients), *not* a predictor.
For prediction, jointly-regularized selection (L1) over the basis is the right estimator; GL's
paradigm of ranking by coefficient magnitude is the wrong one.  (An earlier draft also degree-capped
the GL search to try to salvage prediction — removed: it is not part of GL, would suppress the real
high-order coefficients that make section B work, and did not help anyway.)

## Honest bottom line

1. **The algorithm is correct on real data** (precision-1.0 recovery of real epistatic coefficients).
2. **Blindness holds dramatically on real biology** — CSAMP is necessary; SAMP recovers nothing.
   This validates the paper's *central* claim on real data and is the keeper result.
3. **GL is not a better ML predictor in-distribution.** Its value is **scalable support-finding**
   (heavy coefficients without enumerating 2^n) — which only pays off at large n where Lasso-over-
   the-full-basis is infeasible, and where the data has **context repetition** (n-grams, not
   complete combinatorial landscapes).  Poelwijk (small n, no repetition) cannot show that; the
   DNA/language n-gram setting can, and is the honest place to demonstrate a GL-specific advantage.
