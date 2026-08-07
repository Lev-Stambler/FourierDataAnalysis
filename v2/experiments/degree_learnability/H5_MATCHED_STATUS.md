# Revised H5 matched-design status

## v2.2 hard-domain single-H100 stress test

- State: complete; frozen verdict **SUPPORTED**; audit **PASS**. Thirty-six
  d256 four-layer students ran sequentially with a one-H100 cap, all 12 profiles
  ran on remote CPU, no local training or profiling occurred, and 67 tests PASS.
- Data: three untouched, hash-pinned 8M-byte corpora—mathlib Lean formal
  mathematics, Rust source, and RFC 9000–9113 technical prose. The protocol was
  frozen before any profile or curve.
- Endpoint result: mean within-domain Spearman rho is `0.867`, with domain values
  `1.0/0.8/0.8`; exact blocked `p=0.00579`. Stride 16 minus stride 1 final-CE
  fraction is `+0.205/+0.110/+0.124`. All three seed-specific blocked means are
  positive (`0.733/0.867/0.867`).
- Statistic naming: this is locality rank rho. The frozen implementation used
  `G_total`, but `G_total` and `log2(radius)` have identical ranks in every domain,
  so the rank result does not select the `G_total` functional form.
- Dynamics: curve-area rho is `1.0` for mathlib, `1.0` for Rust, and `0.0` for
  RFCs, for a mean of `0.667`. Thus the frozen comparison-only “cleaner than
  v2.1” diagnostic fails despite the slightly higher endpoint rho (`0.867` versus
  `0.857`).
- Interpretation: harder data and the larger H100 learner preserve and slightly
  sharpen final-budget locality separation, but do not make acquisition dynamics
  universally monotone. This reinforces the decision to report endpoint CE and
  curve area separately.
- Artifacts: `runs/local/v22_hard_h100/{integrated_analysis.json,audit.json,data_manifest.json}`.

## v2.1 unseen-corpus predictor selection

- State: complete; frozen verdict **GEOMETRY_SUPPORTED_METRIC_UNRESOLVED**;
  audit **PASS**. All 144 students ran on remote A10 GPUs and all 48 Definition-3.1
  profiles on remote CPUs; zero local training or profiling; 67 tests PASS.
- Data: six previously unused, equal-length, hash-pinned byte corpora spanning
  books, news, balanced English, biomedical abstracts, Python, and C. Each uses
  the exact-byte-multiset stride intervention at `1/2/3/4/6/8/12/16`, with the
  lag pair `(s,2s)` preregistered rather than selected from the data.
- Geometry replication: mean within-corpus locality rank versus final held-out
  `CE/initial_CE` is `rho=0.857`; exact convolution of the six `8!` permutation
  distributions gives `p=1.10e-11`. The seed-specific means are
  `0.905/0.881/0.802`, and five of six stride-16 endpoints are harder than stride
  1. Curve-area rho is `0.885`. Degree-only rho is `0.012/0.127`.
- Metric selection: leave-one-corpus-out RMSE is `0.0809` for `log2(radius)`,
  `0.0831` for `G_nonconstant`, and `0.0937` for `G_total`. Thus `G_total` is
  15.8% worse than the radius-only predictor and wins in only one held-out corpus.
  It is also almost identical to the spectral-entropy upper bound (`r=0.999999`),
  so the frozen identifiability gate fails.
- Interpretation: locality robustly changes learnability beyond degree, but these
  data favor a simple radius description and do not identify the full
  Parseval-weighted `G_total` formula as the unique quantitative law.
- Artifacts: `runs/local/v21_predictor_selection/{integrated_analysis.json,audit.json,data_manifest.json}`.

## v2.0 confirmatory Fourier-geometry result

- State: complete; frozen verdict **SUPPORTED**; audit **PASS**. Forty-eight remote
  A10/A10G cells, zero local training cells, and 66 tests PASS.
- Design: for each of enwik8, TinyStories, WikiText-2, and CodeParrot Python, the
  same 5.5M-byte stream is interleaved at strides 1/2/4/8. This preserves the exact
  byte multiset while moving the recovered strongest pair to `(s,2s)`. Three seeds
  and a 3M-token budget are used in every dataset-stride condition.
- Primary result: mean within-dataset Spearman correlation of `G_total` with median
  final held-out `CE/initial_CE` is `rho=0.85`; the exact two-sided blocked test over
  all `24^4=331,776` permutations gives `p=0.0013925` (462 extreme permutations).
  Per-corpus rho is `1.0/1.0/1.0/0.4`.
- Confirmatory-purity sensitivity: excluding enwik8, whose pilot motivated v2.0,
  the three genuinely unseen corpora retain mean within-dataset `rho=0.80` and an
  exact two-sided `p=0.0140`. This post-hoc check is not substituted for the frozen
  primary test; it shows that the result does not depend on recycling the pilot.
- Robustness: stride 8 is harder than stride 1 in all four corpora; seed-specific
  blocked mean rho values are `0.90/0.55/0.85`, all positive. The preregistered
  secondary curve-area statistic gives mean within-corpus `rho=0.75`.
- Mechanism: within each corpus, nonconstant energy and mean degree stay nearly
  invariant, while `G_total` grows with the support radius. Mean spectral degree
  has blocked `rho=-0.30`; geometry-aware moments have `rho=0.85`. Thus the signal
  is locality/search geometry, not a hidden increase in interaction degree.
- Scope: this confirms a causal locality effect for four real byte corpora and the
  fixed d64 two-layer Transformer. It is not a universal cross-domain law, nor a
  complete 64-position Fourier spectrum estimate.
- Artifacts: `runs/local/v20_confirmatory_geometry/{integrated_analysis.json,audit.json}`.

## v1.9 Parseval scaling and real-data locality intervention

- State: complete; audit **PASS**. Eight remote A10/A10G cells, zero local
  training cells, and 65 tests PASS.
- Scaling correction: normalize spectral mass by the Parseval total
  `S=W0+W1+W2=E||f(X)||²`, not by nonconstant energy alone. Define
  `G_total=sum_k Wk log2[binom(r,k)(q-1)^k]/S`. On the four existing natural
  corpora, raw nonconstant degree and `G_nonconstant` give `rho=0.60`, whereas
  total-normalized degree, `G_total`, and the spectral-entropy upper bound give
  `rho=0.80` against both held-out area and final `CE/initial_CE`.
- Prospective intervention: the same 5.5M enwik8 bytes were split into 1/2/4/8
  contiguous lanes and emitted round-robin. Byte counts are identical in every
  cell. Recovered strongest pairs move exactly as `(1,2)`, `(2,4)`, `(4,8)`,
  `(8,16)`, while nonconstant energy stays `0.181±0.001` and nonconstant degree
  stays near `1.47`.
- Result: `G_total=9.69,11.03,12.30,13.48`; final held-out `CE/initial_CE` is
  `0.442,0.598,0.609,0.638`, giving Spearman `rho=1.00` (exact `p=0.083`, the
  smallest possible two-sided value at n=4). Curve areas are
  `0.706,0.785,0.776,0.791`, giving `rho=0.80`.
- Dynamics: stride 4 initially learns faster than stride 2 but plateaus worse;
  their normalized held-out curves cross around 0.9M tokens. Integrated area and
  final held-out CE therefore measure different aspects of difficulty.
- Frozen decision: **INCONCLUSIVE**, solely because the preregistered support rule
  required perfectly nondecreasing curve area as well as endpoint difficulty.
  Ordinary degree is held fixed and has negative rank correlation from estimator
  noise, whereas every geometry-aware moment gives `rho=0.80/1.00`.
- Artifacts: `runs/local/v19_parseval_stride/{integrated_analysis.json,audit.json}`.

## v1.8 prospective natural-data run

- State: complete; audit **PASS**. Six fresh 5M-token students and two fresh
  Definition-3.1 profiles were run locally with two CPU threads. The protocol was
  hash-frozen before inspecting either new profile or corrected learning curve.
- Data: four q=256 natural byte corpora with identical learner and budget:
  enwik8, TinyStories, WikiText-2, and CodeParrot Python. Only the first two valid
  contiguous v1.4 runs are reused; all older shuffled language runs are excluded.
- Result: every corpus selects the strongest lag pair `(1,2)`. Raw nonconstant
  spectral degree and conservative geometric complexity both correlate `rho=0.60`
  with held-out normalized curve area (exact permutation `p=0.417`). Across all
  unresolved degree-1 radius endpoint assignments, geometry rho ranges from `0.60`
  to `1.00`, so the frozen rule returns **INCONCLUSIVE**, not support or refutation.
- Held-out CE check: curve area already integrates held-out validation CE. Using
  final `CE/initial_CE` instead leaves both degree correlations at `rho=0.60`.
  Raw final CE also gives `0.60`, while unigram entropy gives `0.80`; raw CE is
  therefore not a cleaner standalone training-difficulty target.
- Correction: M8 image students used the same pre-fix `CorpusFamily` cyclic reader
  that permuted individual tokens. Their re-scored curves remain historical
  artifacts but are invalid as sequence/locality evidence and are not used in v1.8.
- Artifacts: `runs/local/v18_real_natural/{integrated_analysis.json,audit.json}`.

## v1.7 joint degree-locality spectrum

- State: complete exploratory analysis; audit **PASS**; zero training or profile
  recomputation.
- Definition: retain the joint mass `W[k,r]`, where `k` is interaction degree and
  `r` is maximum causal lag. Its scalar search complexity is
  `G = sum W[k,r] log2[binom(r,k)(q-1)^k] / sum W[k,r]` over nonconstant levels.
- Matched synthetic locality check: eight q=32 families share exactly the same
  Bayes floor. `G` perfectly orders their median final gaps (`rho=1.00`, exploratory
  exact permutation `p=4.96e-5`); copy-lag-16 is `0.757` bits harder than
  copy-lag-4 despite both having degree 1.
- Corrected text: raw spectral degree versus curve area is `rho=0.40`; geometric
  complexity is `rho=0.80`. This remains `0.80` at both endpoints of the unresolved
  natural degree-1 radius allocation.
- Real-data check: 9 matched tabular datasets plus 2 natural texts were examined.
  Within all three tabular bands, geometry preserves the raw-degree ranking, so
  there is no independent improvement on real data. Two natural texts are too few
  for inference. Verdict: **PROMISING_CONTROLLED_GEOMETRY_EFFECT;
  REAL_DATA_CONFIRMATION_OPEN**.
- Images are excluded because the existing curves have no compatible joint
  `W[k,r]` spectrum; curve values were not misrepresented as Fourier coefficients.
- Artifacts: `runs/local/v17_local_geometry/{integrated_analysis.json,audit.json}`.

## v1.6 conditional Fourier spectrum

- State: complete; audit **PASS**; no learner or profile recomputation.
- Correct target: `f(x)=P(next token | x)`. The level weights are absolute
  squared-coefficient masses: `W0=1-L0`, `W1=L0-L1`, `W2=L1-L2`, so
  `W0+W1+W2=E||f||^2`. The discarded sparse zero-extension calculation is not
  used anywhere.
- Exact controls: copy has `(W0,W1,W2)=(0.003906,0.560303,0)` and nonconstant
  spectral degree `1`; Markov-2 has `(0.003906,0,0.560303)` and degree `2`.
- Natural strongest-pair spectra: enwik8 `(0.050692,0.106259,0.123987)`, mean
  nonconstant degree `1.538`; TinyStories `(0.070452,0.149588,0.099845)`, mean
  degree `1.400`. Both positive natural levels survive the two-fold 95% check.
- Spectrum degree versus curve area across all four rungs has Spearman `rho=0.40`:
  weakly positive. Both the controlled contrast and natural pair point in the
  expected direction, but the broad correlation is not established at `n=4`.
- Artifacts: `runs/local/v16_conditional_spectrum/{integrated_analysis.json,audit.json}`.

## v1.5 variance-concentration correction

- State: complete auxiliary ANOVA diagnostic; analysis-only, with zero training
  or profile recomputation. It is not the requested full spectral-degree correlator.
- Primary profile: first estimate total conditional-function variance
  `V = L0 - L2`, then cumulative low-degree concentration
  `C_<=1 = (L0 - L1) / V`. Raw token variance `L0` is recorded separately.
- Controlled spectra: copy-lag-16 has `V=0.555460`, `C_<=1=1.000`, and
  degree-2 tail `0.000`; Markov-2 has `V=0.163842`, `C_<=1=0.000`, and
  degree-2 tail `1.000`.
- The v1.4 training result is unchanged: the pure degree-2 rule is harder than
  the degree-1 rule despite its shorter dependency span.
- Natural-text values are profiles of the strongest measured lag pair only:
  enwik8 `C_<=1=0.462`; TinyStories `C_<=1=0.600`. They do not constitute a
  full 64-position low-degree-concentration estimate.
- Artifacts: `runs/local/v15_variance_concentration/{integrated_analysis.json,audit.json}`.

## v1.4 text correction

- State: complete; audit **PASS**; 12/12 fresh q=256 cells.
- Protocol: v1.4 `86781a91…`; local CPU, two Torch threads, zero GPU/Modal spend.
- v1.3 language evidence is invalidated because cyclic corpus reuse permuted
  individual tokens before training windows were formed.
- v1.4 preserves contiguous order and uses the inverse-likelihood categorical
  functional-ANOVA basis from arXiv:2603.02673 Definition 3.1. Formula (19) is
  only the paper's concrete binary example.
- Controlled text verdict: **PASS**. Markov-2 minus copy curve area is `0.175`;
  copy minus Markov-2 learning is `3.412` bits. The degree-2 local rule is harder
  even though the degree-1 copy rule has the longer span (`16`).
- Scoped conclusion: categorical interaction degree has a confirmed causal effect
  in the matched text control; a broad natural-dataset difficulty proxy remains
  unestablished because only two natural corpora are present.
- Artifacts: `runs/local/v14_text_anova/{integrated_analysis.json,audit.json}`.

## Preserved v1.3 matched-design record

- State: complete; final audit PASS
- Protocol: v1.3 `f982f37d…`, generated and hash-checked before new cells.
- Part A: complete — 3 exact bands, 9 datasets, 27/27 new manifests.
- Part B: complete — 4 q=256 rungs, 3 enwik8 cells reused, 9/9 new manifests.
- Part C: complete — 18/18 cells re-scored, zero retrained.
- Part D: complete — updated H5 verdict **INCONCLUSIVE** under the frozen rule.
- Compute rule: local CPU only, at most two Torch threads, no Modal/GPU spend.

Analysis correction discovered after Part B training: the legacy in-sample suffix
estimator is collision-biased for sparse q=256 suffixes. v1.3 profiles use an
alternating-position cross-fit estimator with held-out scoring. This changes no
training cell or frozen data/model/budget choice; both raw cross-fit values and a
monotone projection are recorded.

Headline numbers: tabular band Spearman ρ values are `-0.5`, `1.0`, and
`0.5`; language suffix-gain vs normalized curve-area ρ is `-0.2`. The corrected
image ranking and all per-cell scalars are in
`runs/local/h5_matched/integrated_analysis.json`.
