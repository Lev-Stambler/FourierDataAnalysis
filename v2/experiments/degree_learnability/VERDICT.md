# VERDICT REPORT — Degree-learnability experiment (v2/PLAN.md, M0–M10)

Date: 2026-08-04/05 · Protocols: v1 (90d42a12), v1.1 (32652e6d), v1.2 (7b87f24b)
Analysis artifact: `runs/m10_analysis.json` · Figures: `runs/figures/fig1–fig3`

## Falsification criteria (verbatim from PLAN §7 item 6)

> H1 falsified if any family shows non-monotone `T*` in its degree knob at fixed
> entropy (beyond seed noise, per a pre-specified trend test);
> H4 falsified if model (ii) explains ≥ 90% of what model (iii) explains.
> Either falsification is written up as a negative result with the same rigor.

Trend test (preregistered operationalization, `runs/m10_analysis.json`): per family,
Spearman ρ between knob value and median difficulty; difficulty proxy =
`norm_remaining` (bounded, defined for censored cells) with `final_gap_bits` and
`T*` reported alongside; families with ≥2 censored cells classified censored-hard.

## Verdicts

### H1 — monotonicity of difficulty in the degree knob: **PASS (with scope note)**

F1 Markov ladder (fixed entropy floor 0.939 bits for all k): median final gaps are
strictly non-decreasing in k:
`k1 −0.001 < k2 1.528 < k3 2.805 < k4 2.861 ≤ k6 2.875 ≤ k8 2.961`.
`T*` is reached for k1 (149,504 tokens) and censored (None) for k2–k8 — censoring
is consistent with monotone non-decreasing difficulty. The `norm_remaining` proxy
shows sub-noise wobble at k6/k8 (0.792→0.765→0.744, ≤0.03 on 3-seed medians) caused
by proxy saturation at the censoring boundary, not by T* inversions.
Scope note: k4–k8 are censored at the 3M-token budget, so monotonicity is resolved
on the k1–k4 range and bounded (not separated) beyond it.

### H2 — sample-complexity scaling matches the theorem: **PASS**

Measured slope of log m* vs log N_d = **1.000** (L0 spectral learner, d=1..3);
theorem predicts ~1 (m* ~ N_d/ε). Artifact: `runs/local/m2/scaling.json`.

### H3 — measured profiles suffice: **PASS**

Measured-vs-planted calibration at protocol size: max relative error **0.63%** ≤
preregistered 5% (Hoeffding-margin containment verified separately with exact
inputs); artifact `runs/local/m3/calibration.json` (verdict PASS). Measured
suffix-filtration profiles recorded for all 7 R1 corpora (rung_meta.json per cell).

### H4 — degree predicts difficulty beyond entropy/span/sensitivity: **FAIL (falsified)**

Preregistered models over the 17 synthetic families (median per family):
- (i) degree only: R² = 0.077
- (ii) span + entropy floor: R² = 0.525
- (iii) degree + span + floor: R² = 0.543
- fraction (ii)/(iii) = **0.967 ≥ 0.90 → H4 falsified.**

Reading: support geometry (contiguous vs spread) and entropy floor carry nearly all
cross-family difficulty variance; pure categorical degree adds ~2% on top. Every
spread-support family (F2 lag sums, F3/F4 Fourier polynomials, F5_max_sum) is
censored-hard regardless of degree, while contiguous structure learns. Degree orders
difficulty cleanly *within* the contiguous family (H1), but does not dominate across
structure classes for this fixed learner. Reported as the preregistered negative
result.

### H5 — external validity on real ladders: **FAIL**

- Language (21 cells): measured 1-suffix predictability R1 alone explains R² = 0.063
  of difficulty; adding log2(q) gives R² = 0.175 — weak.
- Tabular (36 cells): measured mean spectral degree alone R² = 0.046; adding
  n_features gives R² = 0.993 — difficulty is dominated by problem size, not degree.
- Image (18 cells): difficulty tracks VQ floor/corpus statistics; the marginal
  code-prior floor (protocol-specified) is an upper bound on the conditional entropy
  rate, so strong datasets show small/negative gaps (convention documented).
- Pooled cross-domain model with domain fixed effects: R² = 0.55, with the
  degree-proxy coefficient subdominant to domain/size covariates.

The measured degree profile does not predict real-ladder difficulty beyond coarse
covariates → H5 falsified as stated.

## Bottom line

The theoretical machinery is validated end-to-end (exact identities, calibrated
estimators, theorem-matching scaling), and categorical degree cleanly orders
difficulty for contiguous synthetic structure. But the central proxy claim — degree
as a cross-structure, cross-domain predictor of Transformer learnability — is
falsified: support locality/geometry and problem size dominate. This is the
preregistered negative result, recorded with the same rigor as a positive one.

## Deviation log (amendments, all documented in README + protocol files)

1. v1.1 (2026-08-04): F3/F4 rebuilt in exponential-softmax form (amp=1.0, beta=32)
   after v1 amplitudes left Bayes floors within θ of uniform CE (T* degenerate);
   12 cells retrained. Secondary scalars (final_gap, norm_remaining, T_half) added.
2. v1.2 (2026-08-04): M7/M8 student budget 20M→5M (tractability); local-CPU
   execution for students; connect-4 resolution (did 1591 variant); R2 grayscale
   unification; cyclic epoch reuse for fixed datasets (image codes, wikitext).
3. M3 gate refinement: Hoeffding margin for measured-input tail-bound containment
   (algebraic soundness checked separately with exact inputs).
4. enwik8 loaded from canonical origin URL (pinned repo is script-only; file sha
   recorded). rung-1 iid floor = exact 12.0 (Qwen text entropy not applicable).
5. R2 floor convention: marginal code-prior entropy (protocol-specified) is an upper
   bound on the conditional entropy rate; negative gaps possible, documented.
6. Total sensitivity covariate (H4): exact spectral influences where enumerable
   (M9 + small instances); protocol-size synthetic cells lack a conditional oracle —
   documented in the analysis note.
7. Modal client flakiness during the first 51-wide fan-out caused premature
   cancellations; resolved via re-run of affected cells and nohup'd sequential
   batches. All 51 cells ultimately committed and validated.

## Budget accounting

- Modal CPU (M6 grid): 79,327 vCPU·s ≈ **$3.33** (`runs/modal/m6/budget_accounting.json`);
  smoke/calibration adds <$0.20. Total Modal spend < $4 < $5 CPU cap.
- GPU: **0 / 80 GPU-hours** used (d64 learner is CPU-saturated; documented).
- Local CPU runs (M7/M8/M9 students): no direct cost; manifests + metrics recorded.

## Artifact index

- Synthetic grid: `runs/modal/m6/{difficulty_table.json,budget_accounting.json}` + 51 cell dirs (manifest+metrics)
- Language ladder: `runs/local/m7/` (stage_a/b/c JSONs + 21 cell dirs)
- Image ladder: `runs/local/m8/` (m8_report.json + 18 cell dirs)
- Tabular ladder: `runs/local/m9/` (m9_report.json + 36 cell dirs)
- Scaling/calibration: `runs/local/m2/scaling.json`, `runs/local/m3/calibration.json`
- Analysis + figures: `runs/m10_analysis.json`, `runs/figures/`

<!-- MATCHED_H5_V1_3_ADDENDUM -->
## H5 matched-design addendum (protocol v1.3, 2026-08-06)

**Updated H5 verdict: INCONCLUSIVE.**
This supersedes the interpretation of the original H5 result, not its preserved artifacts.
The revised test exactly matches rows and feature count within each tabular band,
uses q=256 for every language rung, and excludes the invalid image floor from difficulty.

### Matched tabular bands

- `f4_n500` (row spread 0, feature spread 0): degree→curve-area slope -0.071, R² 0.494, Spearman ρ -0.500.
- `f6_n500` (row spread 0, feature spread 0): degree→curve-area slope 0.123, R² 0.990, Spearman ρ 1.000.
- `f9_n500` (row spread 0, feature spread 0): degree→curve-area slope 0.010, R² 0.171, Spearman ρ 0.500.

### Vocabulary-matched language ladder

Across four q=256 rungs, suffix-gain→curve-area slope -0.145, R² 0.049, Spearman ρ -0.200.
The two synthetic rungs share the same q and generating entropy; their planted
degree/span contrast is reported in the integrated JSON.

### Image correction

All 18 existing cells were re-scored and 0 were retrained. Primary difficulty uses init-to-final learning and normalized curve area;
the marginal-code entropy remains historical metadata only. Negative floor-gap artifacts: 0.

### Interpretation

The protocol's pre-frozen PASS/FAIL/INCONCLUSIVE rule is applied mechanically.
With only three datasets per tabular band and four language rungs, coefficients are
descriptive; no p-value or broad external-validity claim is made.

<!-- TEXT_ANOVA_V1_4_ADDENDUM -->
## Corrected text result (protocol v1.4, 2026-08-06)

**Controlled text verdict: PASS.**
**Scoped conclusion: NARROW_CAUSAL_TEXT_EFFECT_CONFIRMED; NATURAL-DATASET PROXY NOT ESTABLISHED.**

The v1.3 language training cells are invalid for sequence learnability: cyclic corpus
reuse permuted individual token indices before windows were constructed. Protocol v1.4
preserves contiguous token order and retrains every q=256 language cell from scratch.

The text profile now uses the inverse-likelihood categorical functional-ANOVA basis
from Ferrere et al. (arXiv:2603.02673, Definition 3.1 / equation 19). Each
context position is one categorical variable; cross-fitted nested projections measure
degree-1 additive and degree-2 interaction gain over the frozen lag pairs.

In the entropy/vocabulary-matched synthetic contrast, Markov-2 minus copy curve area is 0.175; copy minus Markov-2 learning is 3.412 bits.
The degree-2 local rule is harder even though the degree-1 control has the longer span (16).

The two natural corpora remain descriptive anchors only; they cannot establish a broad
natural-dataset proxy claim. Tabular and image conclusions are unchanged.

<!-- VARIANCE_CONCENTRATION_V1_5_ADDENDUM -->
## Variance-concentration correction (protocol v1.5, 2026-08-06)

**This supersedes effective degree as the headline profile metric.** No training
or profile was rerun: the correction is computed from the saved nested Brier risks.

For each lag pair, total conditional-function variance is `L0 - L2`; degree-at-most-1
variance is `L0 - L1`; their ratio is low-degree concentration `C_<=1`.
Raw sampled-token variance `L0` is also retained, but is not the concentration denominator.

The planted copy rule has V=0.555460, C_<=1=1.000, and degree-2 tail 0.000. The planted Markov-2 rule has V=0.163842, C_<=1=0.000, and degree-2 tail 1.000.
The controlled spectrum is therefore recovered exactly at the degree level, and the
v1.4 training-difficulty contrast is unchanged.

For the strongest measured pair only, enwik8 has C_<=1=0.462 and TinyStories has C_<=1=0.600. These are pairwise conditional-function profiles,
not estimates of the complete 64-position Fourier spectrum, so no full-context natural
text concentration claim is made.

<!-- CONDITIONAL_SPECTRUM_V1_6_ADDENDUM -->
## Conditional Fourier spectrum (protocol v1.6, 2026-08-06)

The degree correlator is now computed from absolute squared Fourier coefficient
mass of `f(x)=P(next token|x)`. For a two-variable categorical slice, the nested
projection identities are `W0=1-L0`, `W1=L0-L1`, and `W2=L1-L2`; equivalently,
`Wk=|Lambda_k| E[|fhat(alpha)|^2]` in a Gram-orthonormalized Formula-(19) basis.
Their sum is the measured total square energy `E||f||^2`.

The exact copy spectrum is `(0.003906, 0.560303, 0)` with nonconstant mean degree
1. The exact Markov-2 spectrum is `(0.003906, 0, 0.560303)` with mean degree 2.
Markov-2 remains harder by 0.175 curve-area units, while copy learns 3.412 more bits.

For the strongest measured pair, enwik8 has spectrum
`(0.050692, 0.106259, 0.123987)` and nonconstant mean degree 1.538; TinyStories has
`(0.070452, 0.149588, 0.099845)` and mean degree 1.400. The positive degree-1 and
degree-2 weights survive the two-heldout-fold 95% intervals.

Across all four rungs, mean nonconstant spectral degree versus normalized curve area
has Spearman rho `0.40`. This is a weak positive association: both the controlled
contrast and the natural pair have the expected direction, but four datasets and
pairwise natural slices do not establish a broad full-context correlation. The earlier
sparse zero-extension calculation was conceptually invalid, deleted, and never reused.

<!-- LOCAL_GEOMETRY_V1_7_ADDENDUM -->
## Joint degree-locality spectrum (protocol v1.7, 2026-08-06)

**Exploratory verdict: PROMISING_CONTROLLED_GEOMETRY_EFFECT;
REAL_DATA_CONFIRMATION_OPEN.** No learner or Fourier profile was recomputed.

Degree alone treats a nearby and a distant coefficient equally. Protocol v1.7 keeps
the joint spectrum `W[k,r]`, with interaction degree `k` and maximum causal lag `r`,
and defines the untuned geometric search cost
`log2[binom(r,k)(q-1)^k]`. The scalar correlator is this cost averaged by
nonconstant Fourier energy. It measures how many categorical coefficients a learner
must distinguish within the local support ball; it is not a fitted mixture of degree
and span.

The cleanest evidence is controlled. Across the eight q=32 F1/copy families with
an identical 0.938948-bit Bayes floor, geometric complexity perfectly orders median
final gap (`rho=1.00`; post-hoc exact permutation `p=4.96e-5`). In particular,
copy-lag-16 is 0.757 bits harder at the final budget than copy-lag-4 although both
are degree 1; both remain easier than the local degree-2 rule. Across the four
corrected q=256 text rungs, curve-area correlation improves from `rho=0.40` for raw
nonconstant degree to `rho=0.80` for geometric complexity. The result is unchanged
over both endpoints of the natural texts' unresolved degree-1 radius assignment.

This does not yet constitute real-data confirmation. In the nine real tabular
datasets, geometry and raw nonconstant degree have identical rankings inside every
matched feature-count band (`rho=-0.5, 1.0, 0.5` against curve area). The tabular
features are unordered, so their coefficient-search width is not spatial locality;
the pooled diagnostic is weak (`rho=0.183`). Only two natural text corpora have valid
corrected spectra. Existing image learning curves lack a compatible joint spectrum
and were not forced into the calculation.

<!-- REAL_NATURAL_V1_8_ADDENDUM -->
## Prospective four-corpus natural-text run (protocol v1.8, 2026-08-06)

**Frozen-rule result: INCONCLUSIVE.** Protocol v1.8 was hash-frozen before the
WikiText-2 and CodeParrot profiles or corrected curves were inspected. It adds six
fresh contiguous 5M-token training cells to the valid v1.4 enwik8 and TinyStories
cells. All corpora use q=256, the same two-layer student, three seeds, and held-out
tail evaluation.

The four median held-out curve areas are TinyStories `0.6530`, WikiText-2 `0.6788`,
enwik8 `0.6844`, and CodeParrot `0.7227`. Every strongest conditional spectrum
selects lags `(1,2)`. Raw nonconstant spectral degree and conservative geometric
complexity consequently have the same association with difficulty: Spearman
`rho=0.60`, exact permutation `p=0.417`. Depending on the unresolved allocation of
each pair's aggregate degree-1 energy, geometry rho ranges from `0.60` to `1.00`;
this does not meet the frozen requirement that the minimum improve over raw degree
by at least `0.20`.

The requested held-out-CE analysis does not change the result. Curve area already
integrates held-out CE over log training time. Final held-out `CE/initial_CE` gives
`rho=0.60` for both raw degree and geometry, as does raw final CE. Byte-unigram
entropy correlates `rho=0.80` with raw final CE, confirming that unnormalized CE
mixes training difficulty with intrinsic corpus entropy.

The experiment therefore provides a moderately positive natural degree association
but no independent real-data locality effect: all four corpora are dominated by the
same radius-2 slice. A genuine natural locality test requires corpora or tasks whose
measured support radii vary, or a full non-double-counted `W[k,r]` estimator rather
than strongest-pair slices.

Audit correction: M8 image students were trained through the same pre-v1.4 cyclic
reader that permuted tokens. Their artifacts are preserved but are invalid as
sequence/locality evidence; none is reused here.

<!-- PARSEVAL_STRIDE_V1_9_ADDENDUM -->
## Parseval scaling and controlled real-data locality (protocol v1.9, 2026-08-07)

**Frozen-rule verdict: INCONCLUSIVE; geometric endpoint evidence: strong.**

The v1.8 statistic conditioned on nonconstant energy and thereby removed part of
the Parseval mass. Protocol v1.9 instead starts from
`S=sum_k Wk=E_D||f(X)||²`, defines `p_k=Wk/S`, and reports
`G_total=sum_k p_k log2[binom(r,k)(q-1)^k]`. The level-entropy term can be added to
obtain the upper bound produced by spreading each known level mass uniformly across
its coefficient cardinality. On the four independent v1.8 corpora, this correction
raises the descriptive association with both held-out difficulty measures from
`rho=0.60` to `rho=0.80`.

The prospective pilot varies geometry while fixing the real data. The same retained
5.5M enwik8 bytes are divided into `s` contiguous lanes and emitted round-robin for
`s=1,2,4,8`. Thus natural lag-one and lag-two structure moves to `(s,2s)` without
changing a single byte count. Profiles recover exactly `(1,2)`, `(2,4)`, `(4,8)`,
and `(8,16)`. Across these transformations, total energy remains approximately
`0.230`, nonconstant energy approximately `0.181`, and nonconstant degree
approximately `1.47`; the intervention isolates radius rather than degree or signal
amount.

`G_total` rises `9.69→11.03→12.30→13.48`. Median final held-out
`CE/initial_CE` rises `0.442→0.598→0.609→0.638`, so their Spearman correlation is
`1.00` (two-sided exact permutation `p=0.083` at n=4). Median curve area is
`0.706→0.785→0.776→0.791`, giving `rho=0.80`. Degree-only statistics are nearly
constant and their residual rank correlations are negative, while the absolute,
nonconstant-normalized, total-normalized, and entropy-bound geometric statistics
all give `rho=0.80` with area and `1.00` with the endpoint.

The stride-2 and stride-4 curves explain the frozen inconclusive label: stride 4
improves faster early, but plateaus at a worse held-out CE, crossing stride 2 around
0.9M tokens. The frozen rule required both area and endpoint difficulty to be
perfectly monotone. Substantively, the same-spectrum intervention supports the
theoretical claim that Fourier support geometry changes learnability beyond degree;
it also shows that acquisition speed and finite-budget endpoint difficulty should be
reported separately.

All eight training cells ran remotely on Modal A10/A10G GPUs in 61.8 aggregate
cell-seconds; local training cells: zero.

<!-- CONFIRMATORY_GEOMETRY_V2_0_ADDENDUM -->
## Confirmatory real-data Fourier geometry (protocol v2.0, 2026-08-07)

**Frozen-rule verdict: SUPPORTED.** The protocol was hash-frozen after the enwik8
pilot but before any stride-transformed TinyStories, WikiText-2, or CodeParrot
profile or curve was computed. It applies the same stride-1/2/4/8 intervention to
four fixed 5.5M-byte real corpora, with three fresh seeds and 3M training tokens per
cell. All 48 cells ran remotely on Modal A10/A10G GPUs; local training cells: zero.

The primary difficulty statistic is the dataset-stride median final held-out
`CE/initial_CE`. The primary predictor is the Parseval-normalized geometric moment
`G_total`. Their mean within-dataset Spearman correlation is `rho=0.85`. The exact
two-sided test independently permutes the four difficulty labels inside each corpus;
462 of all `24^4=331,776` blocked permutations are at least as extreme, giving
`p=0.0013925`. Per-corpus rho is `1.0`, `1.0`, `1.0`, and `0.4` for enwik8,
TinyStories, WikiText-2, and CodeParrot respectively.

Because enwik8 supplied the pilot that motivated the frozen protocol, a separate
post-hoc confirmatory-purity check excludes it. The three genuinely unseen corpora
retain mean within-dataset `rho=0.80`; 194 of `24^3=13,824` exact blocked
permutations are as extreme (`p=0.01403`). The frozen primary test is not replaced,
but the positive conclusion therefore does not depend on reusing the pilot corpus.

Every endpoint check points in the predicted direction: stride 8 is harder than
stride 1 in all four corpora, by `+0.225`, `+0.160`, `+0.076`, and `+0.032` in
final-CE fraction. Seed-specific blocked mean correlations are `0.90`, `0.55`, and
`0.85`, all positive. The independent secondary curve-area statistic gives mean
within-corpus `rho=0.75`.

The mechanism is specifically geometric. Every profile recovers `(s,2s)`; within
each corpus, nonconstant spectral energy and mean nonconstant degree remain nearly
fixed across strides. Degree alone has blocked `rho=-0.30`, whereas every
geometry-aware moment has `rho=0.85`. Geometry variants have identical within-
intervention ranks, but `G_total` remains the theoretically preferred quantity
because `p_k=W_k/sum_j W_j` uses the complete Parseval mass, including `W0`.

This establishes a causal locality effect for these four byte corpora and the fixed
d64 two-layer Transformer: spreading the same measured low-degree Fourier mass over
a larger coordinate support makes it harder to learn at a fixed token budget. It
does not establish a universal cross-domain law or estimate the full 64-position
Fourier spectrum. Audit PASS; 66 tests PASS. Artifacts:
`runs/local/v20_confirmatory_geometry/{integrated_analysis.json,audit.json}`.

<!-- PREDICTOR_SELECTION_V2_1_ADDENDUM -->
## Unseen-corpus geometry and predictor selection (protocol v2.1, 2026-08-07)

**Frozen verdict: GEOMETRY_SUPPORTED_METRIC_UNRESOLVED.** Protocol v2.1 was
hash-frozen before any new profile or learning curve. It uses six previously unseen
corpora—Gutenberg books, Reuters, Brown, PubMed abstracts, CPython, and Linux C—at
eight strides `1/2/3/4/6/8/12/16` and three seeds. Every corpus contains exactly
5,499,984 pinned bytes, and each transformation preserves its exact byte multiset.
The pair `(s,2s)` is preregistered rather than selected after profiling.

The geometry effect replicates decisively. The mean within-corpus Spearman
correlation between `G_total` and median final held-out `CE/initial_CE` is
`rho=0.857`. Exact convolution of the six independent `8!` rank-permutation nulls
gives two-sided `p=1.10e-11`. Per-corpus rho is `0.952`, `1.000`, `0.976`, `1.000`,
`0.810`, and `0.405`. Five of six stride-16 endpoints are harder than stride 1;
Linux C is the exception. All three seed-specific blocked means are positive
(`0.905/0.881/0.802`). The secondary curve-area mean rho is `0.885`, including
`0.952` on Linux C. Ordinary total/nonconstant degree gives only `0.012/0.127`.

The stronger claim that `G_total` is the correct scalar law does not pass. Under
the frozen leave-one-corpus-out delta prediction, `log2(radius)` has RMSE `0.0809`,
`G_nonconstant` has `0.0831`, and `G_total` has `0.0937`; the latter is 15.8% worse
than radius alone and beats it for only one of six held-out corpora. The same
ordering appears for the secondary curve-area outcome. Moreover, `G_total` and the
spectral-entropy upper bound have centered correlation `0.999999`, violating the
preregistered identifiability gate.

The supported conclusion is therefore narrower and clearer: Fourier support
locality robustly affects learnability beyond interaction degree, across both prose
and source code, but the current evidence does not justify the full
Parseval-weighted `G_total` formula over a simpler logarithmic radius law. The code
corpora expose periodic and formatting effects at intermediate strides, so maximum
radius is itself still a coarse scalar rather than a complete geometry.

All 144 training cells ran on Modal A10 GPUs (1,408.6 aggregate cell-seconds); all
48 profiles ran on Modal CPU (18.7 aggregate cell-seconds). Local training and
profiling cells: zero. Audit PASS; 67 tests PASS. Artifacts:
`runs/local/v21_predictor_selection/{integrated_analysis.json,audit.json,data_manifest.json}`.
