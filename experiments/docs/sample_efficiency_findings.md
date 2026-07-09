# Sample efficiency: when IS dataset-Fourier / GL a better ML method?

The audit (`audit_findings.md`) showed dataset-GL does not beat standard baselines *as a
classifier on low-order targets*.  This asks the sharper question on the axis where sparsity
should pay — **sample efficiency** (accuracy vs #training labels) — on real epistatic fitness
landscapes, the textbook case of sparse *higher-order* structure. (`fda_exp/sample_efficiency.py`)

**Method.** Held-out **Spearman** (ranking is what matters for a fitness landscape) vs training-set
size N, for the sparse dataset-Fourier predictor (**Fourier-Lasso**: L1 over the Householder/Walsh
spectrum — the full 2^13 spectrum when small, degree-≤2 otherwise; α by validation) against a naive
GL (**rawrank**: top-K empirical coefficients + refit), an additive linear model (**Ridge** on
one-hot), and black boxes (**GBT**, **MLP**).  Landscapes: **Poelwijk** mTagBFP2↔mKate2 (V=2, w=13,
all 2^13 measured — 59% of the signal is higher-order) and **GB1** (V=20, w=4 — mostly additive).

## Results — held-out Spearman

**Poelwijk GFP, combined phenotype** (additive ceiling ≈ 0.34):

| N | Fourier-Lasso | GL-rawrank | Ridge (additive) | GBT | MLP |
|---|---|---|---|---|---|
| 50 | 0.467 | **0.603** | 0.169 | 0.171 | 0.157 |
| 100 | 0.665 | 0.625 | 0.206 | 0.675 | 0.214 |
| 200 | 0.719 | 0.659 | 0.263 | 0.716 | 0.162 |
| 500 | 0.756 | 0.722 | 0.329 | 0.783 | 0.756 |

**Poelwijk GFP, red phenotype** (additive ceiling ≈ 0.50):

| N | Fourier-Lasso | Ridge | GBT | MLP |
|---|---|---|---|---|
| 200 | **0.501** | 0.481 | 0.480 | 0.235 |
| 500 | **0.581** | 0.491 | 0.540 | 0.464 |
| 1000 | 0.583 | 0.497 | 0.593 | 0.561 |

**Samples to reach Spearman 0.5:**

| landscape | Fourier-Lasso | GBT | MLP | Ridge (additive) |
|---|---|---|---|---|
| Poelwijk combined | 100 | 100 | 500 | **never** |
| Poelwijk red | **200** | 500 | 1000 | **never** |
| GB1 (additive) | 1000 | 500 | 500 | **200** |

## CORRECTION — the sample-efficiency "win" mostly evaporates against TUNED baselines

The table above compares Fourier-Lasso to a **badly configured** MLP (128,64 with early-stopping
on N=50 → a 10% val split of 5 points; it scores 0.084) and an under-regularized GBT.  With honest
tuning (Poelwijk combined, 5 seeds, held-out Spearman):

| N | Fourier-Lasso | MLP(128,64)+ES *(orig)* | MLP(32),α=1,5k it | GBT | SVR-rbf |
|---|---|---|---|---|---|
| 50  | 0.492 | 0.084 | 0.467 | 0.128 | 0.450 |
| 100 | 0.663 | 0.346 | 0.653 | **0.694** | 0.572 |
| 200 | 0.711 | 0.485 | 0.663 | **0.725** | 0.662 |

A **regularized MLP(32) nearly ties Fourier-Lasso at N=50** (0.467 vs 0.492, within seed noise) and
**GBT beats it at N≥100**.  So the earlier "~3× win at N=50" was largely a broken-baseline artifact.

## Honest conclusions (revised)

1. **The one robust result is negative-for-additive:** additive Ridge plateaus at 0.34 (combined) /
   0.50 (red) and **never reaches 0.5** → the higher-order structure is real and essential; you need
   *some* interaction-capable model.

2. **Fourier-Lasso is competitive, not superior.** Against *tuned* black boxes it ties a regularized
   MLP at the lowest N and loses to GBT by N≈100.  In-distribution, on accuracy or sample efficiency,
   it does **not** beat well-configured standard methods.  Its honest edge is being a **simple,
   interpretable sparse-linear model in a meaningful basis** — parity, not superiority.

3. **Honest contrast (GB1).** On an additive-dominated landscape additive Ridge is most
   sample-efficient and Fourier-Lasso is worst at low N — when additive suffices its machinery hurts.

4. **Predictive parity ≠ recovery.** From N=200, Lasso recovers only ~10/30 of the landscape's true
   heavy terms and selects spurious high-degree ones.

5. **This is not the GL *algorithm*.** At 2^13 we enumerate all characters and Lasso over them; GL is
   the sublinear heavy-coefficient search (subcube/conditional sampling) that avoids enumeration at
   large w — untested here.  And the split is **in-distribution** (i.i.d. random variants), where a
   black box interpolates fine — which is why there's no win.

## Where a real win might actually be (not yet shown)

- **Out-of-distribution extrapolation**: train on low-order (1–2 mutation) variants, predict
  high-order combinations.  A recovered sparse mechanistic model should extrapolate where a
  correlational black box cannot — this is the regime structure beats correlation, and the natural
  next experiment.
- **Scale**: large w where enumerate+Lasso is infeasible and GL/CSAMP's structured search is the only
  way to build the support — the paper's actual algorithmic contribution.
- **Interpretability at parity**: exact named interaction terms with black-box-level accuracy.

Do **not** claim an in-distribution predictive win; the tuned-baseline check does not support it.
