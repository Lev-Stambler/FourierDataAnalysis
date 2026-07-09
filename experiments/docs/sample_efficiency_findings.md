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

## Honest conclusions

1. **The win is real, and scoped.** On real high-epistasis landscapes, in the **scarce-label
   regime**, sparse dataset-Fourier is the most (or tied-most) sample-efficient predictor:
   2.5–5× fewer labels than MLP, and it beats GBT on `red` (200 vs 500) and dominates at the
   very lowest budget (N=50 combined: 0.47–0.60 vs ~0.17 for every black box, a ~3× gap).
   This is exactly the regime that matters for expensive assays (directed evolution: every
   label is a wet-lab measurement).

2. **Additive genuinely fails here** — Ridge plateaus at 0.34 (combined) / 0.50 (red) and
   **never reaches 0.5**. So this is not the low-order story from the audit: the higher-order
   structure is real, essential, and what dataset-Fourier exploits.

3. **GBT is a strong competitor** — it catches up by N≈500 and slightly overtakes both
   landscapes past N≈1000. The advantage is specifically **low N**, not all N.

4. **Honest contrast (GB1).** On an additive-dominated landscape, additive Ridge is the most
   sample-efficient and Fourier-Lasso is *worst* at low N (diluting 80 additive features among
   thousands). When additive suffices, dataset-Fourier's machinery hurts.

5. **Predictive win ≠ clean recovery (yet).** From N=200, Lasso recovers only ~10/30 of the
   landscape's true heavy terms and selects many spurious high-degree ones — prediction works
   before exact support recovery converges. Robust support recovery is where GL/CSAMP (heavy-
   coefficient search with conditional sampling) should help, and is the natural follow-on.

## Synthesis (with the audit)

Dataset-Fourier / GL is a better ML method **precisely when both hold: (a) the target has sparse
*higher-order* structure so additive/linear underfits, AND (b) labels are scarce so black-box
interaction models can't yet fit.** When either fails — low-order target (promoters, GB1) or
abundant labels — standard methods match or beat it. That intersection (sparse epistasis + few
expensive labels) is real and important (protein/enzyme engineering), which is the honest, defensible
niche for the paper — not a blanket "better predictor" claim.
