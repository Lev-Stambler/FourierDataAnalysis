# dlx — degree-learnability experiments

Implements `v2/PLAN.md` (milestones M0–M10): categorical degree profiles as a
learnability proxy for fixed Transformers, on synthetic families and real dataset
ladders.

## Current conclusion

The reproducible headline is **dataset spectrum is informative, but hardness is
learner-conditional and the present Fourier summaries are not uniquely
predictive**. The v2.0-v2.2 byte-preserving interventions produced strong locality
rank correlations (`rho=0.850/0.857/0.867`), but v2.3 showed that positional
geometry moderates the effect: learned absolute, ALiBi, and the larger learned
model passed, while sinusoidal did not. In the locked v2.4 test on 12 new corpora,
compact Fourier features predicted held-out endpoint difficulty (`R^2=0.260`) but
simple entropy/bigram/compression controls predicted it better (`R^2=0.572`), and
adding Fourier features worsened RMSE by 8.8% (95% corpus-bootstrap interval
`[-20.5%, +1.36%]`). The intervention direction remained useful (88.9% sign
accuracy), but its magnitude prediction had negative `R^2`.

The post-confirmatory v2.5 diagnosis found two concrete limitations rather than a
positive confirmation: v2.4 selected ridge `alpha=100` under a natural/stride
covariate mismatch, and its degree-two profile omitted substantial degree-three
conditional energy. Plain OLS plus degree-three summaries produced promising
held-out numbers, but only small, target-dependent gains under pooled
leave-one-corpus-out analysis. Those choices were post hoc and do not change the
v2.4 verdict.

The prospective v2.6.2 test resolves the immediate follow-up negatively for
locality. On 32 new corpora, controls predict endpoint difficulty with
`R^2=0.134`. Adding sampled degree-three energy and mean degree gives
`R^2=0.189` and a 3.2% RMSE improvement, but its 95% paired-corpus bootstrap
interval `[-10.8%, +18.1%]` is unresolved. Adding the frozen geometric locality
summary reduces `R^2` to `0.110` and worsens RMSE by 4.76%; its improvement
interval is entirely negative (`[-10.10%, -0.17%]`). Raw locality has only
`rho=0.051` with endpoint difficulty (`p=0.780`). Thus the primary sampled
locality hypothesis is not supported, while an incremental energy/degree signal
remains suggestive rather than confirmed.

The v2.8 random-window repair resolves a later sampling confound in v2.7. The
old profile read a corpus prefix while training traversed an ordered cyclic
stream and validation used its tail. With disjoint randomized 16 KiB blocks and
random intact context windows for all three roles, the development-frozen
marginal-locality OLS reduces held-out RMSE by **26.0%** (stratified 95% interval
`[18.1%, 35.0%]`), reaches `R^2=0.453`, and has pooled Spearman `rho=0.806` on
the same 24-corpus panel. The blocked within-domain rank gate still fails (mean
`rho=0.067`, `p=0.410`), so the mechanical verdict remains
**PREDICTIVE_ONLY**. This is a paired sampling repair, not a second
source-disjoint confirmation.

The exact controlled factorial clarifies the mechanism: at equal nonconstant
Fourier energy, degree 2 was harder than degree 1 by `0.130` normalized endpoint
CE, while radius had a strong effect for ALiBi but not a universal effect across
the three positional encodings. Thus the supported object is a spectrum of
degree, energy, and locality interpreted jointly with learner geometry—not a
model-independent scalar dataset-hardness law and not the specific `G_total`
formula.

Protocol v3.1 tested the learner-conditional idea directly. The response surface
is realized held-out CE learning-curve area on exact Fourier characters, and the
dataset feature weights it by measured Fourier support energy. A frozen
degree-three saturation gate reduced the response experiment to 675 measured
cells, with all degree-one/two supports exact and 18 unmeasured degree-three
supports pooled. On the 24-corpus pilot, adding the overlap to ordinary
entropy/compression controls improved grouped LOCO RMSE by 2.04%, but adding it
to the preregistered strong spectrum baseline worsened RMSE by 0.62%. The pilot
gate failed, so confirmation was not run. The result supports aggregate Fourier
predictivity, not the proposed incremental exact-support architecture match. See
[`docs/V30_ARCHITECTURE_SPECTRUM.md`](docs/V30_ARCHITECTURE_SPECTRUM.md).

Protocol v3.2 then froze the narrower, pilot-derived ordinary-baseline claim
before observing outcomes on the 48 unused sources. Across 240 held-out
corpus–architecture rows, adding Fourier-CE overlap improved ordinary-baseline
RMSE by 1.10% (`0.024867` to `0.024593`), but the preregistered stratified
corpus-bootstrap interval was `[-1.57%, 3.54%]`; the prospective replication
gate therefore failed. Against the stronger degree/energy/locality baseline,
overlap worsened RMSE by 0.31%, with interval `[-0.512%, -0.106%]`. The larger
panel supports a modest aggregate Fourier signal but not a conclusive unique
exact-support matching effect. Expansion audit PASS.

A post-outcome v3.3 diagnostic then removed corpus-wide difficulty by predicting
paired architecture differences. For RoPE minus NoPE, the overlap contrast was
associated with expansion final-CE-fraction contrast at `r=-0.454`
(`p_Holm=0.00238`) and improved development-to-expansion RMSE by 7.63%, although
the paired-bootstrap interval narrowly crossed zero (`[-0.24%,12.49%]`). The
sign is opposite the hardness hypothesis. Curve-area transfer improved only
2.51% with a wide interval, normalized learning time worsened 5.46%, and the
initial-CE negative control was null. Pairing reveals endpoint information, but
the current Walsh-character overlap is a directionally inverted bridge rather
than a valid architecture-hardness match.

Protocol v3.4.1 made the architecture intervention causal and narrower: the
same two-layer RoPE transformer was trained with hard attention windows 8, 16,
32, and 64. Uniform degree-one/two Walsh-character responses were frozen before
any limited-window natural outcome, the paired model was fitted on 24 corpora,
and it transferred unchanged to 24 balanced held-out corpora. Adding the
uniform Fourier fingerprint to window fixed effects improved held-out
curve-area RMSE by **19.97%**, interval `[16.10%,28.21%]`, and raised `R²` from
`0.286` to `0.543`. It also improved RMSE by **17.75%** beyond inaccessible
far-energy, interval `[12.96%,26.68%]`; within-window Pearson correlations were
`0.680/0.767/0.718`. The gate **PASSES**. Smaller windows actually learned
faster on average at this budget, so the supported claim is conditional: the
uniform Fourier fingerprint predicts which datasets gain less (or are penalized
more) under the architecture intervention after its mean regularization benefit
is removed. Endpoint CE prediction remained weak. See
[`docs/V34_LOCAL_WINDOW.md`](docs/V34_LOCAL_WINDOW.md).

The exact basis and statistic are defined in [`LOCALITY_MATH.md`](LOCALITY_MATH.md).
The research narrative—including the failed endpoint test and the post-hoc
marginal-locality correction—is in [`BLOG.md`](BLOG.md).
The post-v2.4 plain-OLS and sampled higher-degree diagnostic is summarized in
[`docs/history/V25_KISS_STATUS.md`](docs/history/V25_KISS_STATUS.md).
The critical prior-art and novelty assessment is in
[`NOVELTY_ASSESSMENT.md`](NOVELTY_ASSESSMENT.md). In short: the broad theory is
not new; the potentially new contribution is the controlled, data-conditioned
empirical construction and result.

Attribution correction: the general inverse-likelihood categorical basis is
Definition 3.1 of Ferrere et al. Formula (19) is only their concrete
two-Bernoulli-variable example. Frozen protocol JSON and historical result
artifacts keep their original wording to preserve hashes and provenance.

## Layout (PLAN §12.1)

- `dlx/domains.py` — categorical Fourier utilities on Z_q^n (characters, FFT
  conventions, level weights).
- `dlx/families/` — synthetic sequence families with planted degree profiles
  (`base.py` interface per PLAN §12.2; `f2_subset_sum.py`).
- `dlx/seeding.py` — deterministic seed tree (PLAN §12.5).
- `tests/` — preregistration-relevant tests (PLAN §12.4).
- `dlx/profiles/` and `dlx/analysis/` — data-derived spectra and frozen predictors.
- `dlx/protocol/` — protocol hashing, lock verification, and pure cell enumeration.
- `runs/local/v25_kiss_diagnostic/` — compact v2.5 summaries and lossless audits.

## Conventions

- Row encoding for prefixes/sequences: oldest token = highest base-q digit, i.e.
  `row = sum_j x_j * q^(L-1-j)`. In a reshaped `(q,)*L` joint table, axis `a`
  corresponds to position `a`.
- Fourier convention matches the v2 paper:
  `fhat(k) = E_x[f(x) conj(chi_k(x))]`, `chi_k(x) = omega_q^{<k,x>}` — the forward
  transform is `np.fft.fftn(values) / q^n`.
- Tiers (PLAN §8): S0 q=8,L=8 · S1 q=16,L=16 · S2 q=32,L=64 · G1/G2 GPU.
- Deviation log (PLAN §11 style): torch/modal/scipy/pandas/matplotlib are optional
  extras (`dlx[gpu]`, `dlx[modal]`, `dlx[analysis]`) so CPU tiers stay lightweight;
  PLAN §12.1 listed them as core deps.
- M3 gate refinement (2026-08-04): the measured-input tail-bound containment check
  uses a preregistered Hoeffding margin (delta=1e-6, recorded in the artifact)
  because plug-in bounds are tight at exact inputs and MC noise alone would violate
  them; algebraic soundness is checked separately with exact inputs. Row-encoding
  int64 overflow at q^max_lag > 2^62: F2 overrides sample/refill/batch with direct
  lag-sum implementations (enumeration paths unaffected; enumerable tiers always fit).
- M4 pre-freeze tuning (2026-08-04): harness defaults set to lr=1e-3 and
  tokens_per_step=1024 after probes showed lr=3e-4 with 32k-token steps stalled on
  the easy cell; with these defaults the G1 gate passes within the preregistered 1e5
  token budget (T*=49,152). The M5 pilot must still validate lr in {3e-4, 1e-3} on
  the mid-difficulty family before freeze. G1 smoke ran on CPU (no GPU on this host);
  the Modal GPU path ships with the M6 grid runner.
- F2 spread-lag unlearnable finding (2026-08-04): spread-lag mod-sum families sit at a
  chance-level plateau for the fixed harness within all probed budgets (flat at 1M
  tokens even for d256/6L; q=32 and q=16), while contiguous Markov-k families learn
  with difficulty increasing in k and pure lagged copy at any span stays easy
  (lag-4 copy gap 0.25). This is grokking-hard non-local modular arithmetic, not a
  harness defect (long-range copy works). Consequence: the learnable degree gradient
  lives in the contiguous F1 ladder; F2 spread-lag cells remain in the grid as
  censored hard controls (T*=None recorded). Pilot mid family changed from the
  original F2-s3 to F1Markov k=3 accordingly (recorded in m5_pilot.py).
- M5 pilot outcome (2026-08-04): pilot on F1Markov k=3, 2M tokens, ladder
  small/mid/large gave final val CE 3.7597 / 3.7558 / 4.4978. The large config
  UNDERFIT at the pilot budget (worse than small). The literal preregistered rule
  (smallest config within 10% of the largest config's final loss) therefore selects
  'small' (d_model=64, n_layers=2, n_heads=4). The large-config underfit is recorded
  as a finding; the rule was applied verbatim without post-hoc change. Selected config
  frozen into configs/protocol_v1.json.
- M6 grid diagnosis + protocol amendment v1.1 (2026-08-04): the full 51-cell grid ran
  on Modal CPU (all 51 manifests complete; one premature volume sync initially looked
  like 12 failures — the cells had not yet committed). Diagnosis of the difficulty
  table found two issues: (1) v1 F3/F4 amplitudes (additive, amp=0.9/q) left Bayes
  floors at 4.97-4.98 bits, within theta=0.05 of the uniform CE, so T* was degenerate
  (reached at the first checkpoint); (2) theta=0.05 saturates for every non-trivial
  task (only F1_k1 solves to within 0.05 bits in 3M tokens), although the learning
  curves themselves carry a clean difficulty gradient. Amendments (configs/
  protocol_v1.1.json, hash 32652e6d...): F3/F4 rebuilt in exponential-softmax form
  (positivity guaranteed, signal tunable; amp=1.0, beta=32.0 gives floors 3.37-3.72
  bits, mid-difficulty); 12 F3/F4 cells retrained under v1.1; secondary difficulty
  scalars added uniformly (final_gap_bits, norm_remaining, T_half), extractable from
  the saved curves of all runs; primary T*(theta=0.05) retained verbatim. Secondary-
  metric view of the v1 grid shows the intended gradient: F1 ladder monotone in k
  (k1 gap ~0 < k2 ~1.5 < k3 ~2.8 <= k4..k8 ~2.9-3.5); copy_lag16 (degree 1, span 16)
  easier than F1_k2 (degree 2, span 2) — degree dominates span; F2 spread-lag sums
  and F5_max_sum censored-hard.
- Matched-H5 protocol v1.3 (2026-08-06): the original H5 FAIL was re-tested after
  removing its largest design confounds. Tabular data now use three exact
  `(n_rows, n_features)` bands (500 rows; 4, 6, or 9 features), the language ladder
  uses q=256 for all four rungs, and all 18 image curves use true token-zero CE and
  floor-independent curve metrics. Result: **INCONCLUSIVE** under the frozen rule
  (tabular degree-vs-difficulty Spearman rho `-0.5/1.0/0.5`; language suffix-gain
  rho `-0.2`). Nine new language cells ran locally with two CPU threads; enwik8 was
  reused; images were not retrained. See `docs/history/H5_MATCHED_STATUS.md`, protocol v1.3,
  `runs/local/h5_matched/integrated_analysis.json`, and the `VERDICT.md` addendum.
- Corrected text protocol v1.4 (2026-08-06): the v1.3 language cells were invalidated
  after finding that cyclic corpus reuse shuffled individual tokens and erased
  sequence structure. Contiguous cyclic reads now preserve adjacency; all 12 q=256
  language cells were retrained locally. Text profiles use the inverse-likelihood
  categorical functional-ANOVA basis of Ferrere et al. (arXiv:2603.02673, Definition
  3.1). The entropy/vocabulary-matched controlled contrast **PASSES**:
  the degree-2 Markov rule has 0.175 higher curve area than degree-1 copy, while copy
  learns 3.412 more bits despite its longer span. This confirms a narrow causal text
  effect; with only two natural corpora, a broad natural-dataset proxy is not
  established. Audit PASS, 57 tests PASS; see `runs/local/v14_text_anova/`.
- Variance-concentration protocol v1.5 (2026-08-06): analysis-only correction of
  the text degree statistic. It estimates total pair-conditional function variance
  before reporting cumulative low-degree concentration; the former effective-degree
  average is diagnostic only. Copy-lag-16 is entirely degree <=1 (`C=1.000`) and
  Markov-2 entirely degree 2 (`C_<=1=0.000`), while the existing difficulty contrast
  remains unchanged. Natural values are strongest-pair profiles, not full-context
  spectra. Audit PASS; see `runs/local/v15_variance_concentration/`.
- Conditional-spectrum protocol v1.6 (2026-08-06): replaces ratios with absolute
  Fourier level weights of `f(x)=P(next token|x)`. For each two-position slice,
  `W0=1-L0`, `W1=L0-L1`, and `W2=L1-L2`; these equal the squared coefficient
  sums in a Gram-orthonormalized Definition-3.1 degree filtration. The controls
  recover exact nonconstant spectral degrees 1 and 2. Across four rungs, spectral
  degree versus curve area has Spearman `0.40`: the controlled and natural-pair
  directions agree, but the overall association is weak and the natural estimates
  remain strongest-pair slices. Audit PASS; see `runs/local/v16_conditional_spectrum/`.
- Local-geometry protocol v1.7 (2026-08-06): augments categorical interaction degree
  with support radius through the untuned search cost
  `log2[binom(r,k)(q-1)^k]`, averaged by nonconstant Fourier energy. On eight
  q=32 synthetic families with the same Bayes floor, this metric exactly orders
  median final training gap (`rho=1.00`; post-hoc exact permutation
  `p=4.96e-5`), including the two degree-1 copy radii. On the corrected four-rung
  text set, curve-area rho rises from `0.40` to `0.80`. This is controlled evidence,
  not real-data confirmation: the nine real tabular datasets retain exactly the
  same within-band ranks as raw degree, and only two natural text spectra are
  available. Audit PASS; zero training/profile recomputation; see
  `runs/local/v17_local_geometry/`.
- Prospective natural-text protocol v1.8 (2026-08-06): adds fresh contiguous
  WikiText-2 and CodeParrot byte runs (six new 5M-token cells) to the valid v1.4
  enwik8/TinyStories cells. All four strongest pair spectra select lags `(1,2)`.
  Raw nonconstant degree and geometric complexity therefore have the same
  held-out curve-area correlation (`rho=0.60`, exact permutation `p=0.417`) and
  the frozen geometry-improvement rule is **INCONCLUSIVE**. Final held-out
  `CE/initial_CE` gives the same `rho=0.60`; raw final CE is more aligned with
  byte-unigram entropy (`rho=0.80`), as expected from entropy confounding. The
  same pre-v1.4 cyclic-reader bug also affects M8 image sequence curves, so those
  curves are now explicitly excluded from locality evidence. Audit PASS; 64 tests
  PASS; see `runs/local/v18_real_natural/`.
- Parseval/stride protocol v1.9 (2026-08-07): corrects the geometric normalization
  to use total square energy `S=sum_k Wk=E||f(X)||²`. On the four v1.8 natural
  corpora, total-normalized degree, geometric moment, and the uniform-within-level
  spectral-entropy upper bound each improve descriptive rho from `0.60` to `0.80`.
  A prospective controlled test then interleaves exactly the same enwik8 bytes at
  strides 1/2/4/8, shifting the recovered pair from `(1,2)` through `(8,16)` while
  leaving spectral energy and degree nearly fixed. Eight 1.5M-token cells ran on
  Modal A10/A10G GPUs (61.8 aggregate cell-seconds; zero local training).
  `G_total` rises `9.69→11.03→12.30→13.48` and correlates `rho=1.00` with final
  held-out `CE/initial_CE`, but `rho=0.80` with curve area because stride 4 learns
  faster early than stride 2 before plateauing worse. The conservative frozen rule
  is **INCONCLUSIVE** because area is not perfectly monotone; the geometry signal
  itself is strong and isolated. Audit PASS; 65 tests PASS; see
  `runs/local/v19_parseval_stride/`.
- Confirmatory geometry protocol v2.0 (2026-08-07): freezes a four-corpus,
  four-stride, three-seed replication before computing any transformed
  TinyStories, WikiText-2, or CodeParrot spectrum or learning curve. Across the 16
  dataset-stride medians, the mean within-corpus Spearman correlation between
  `G_total` and final held-out `CE/initial_CE` is `rho=0.85` (exact blocked
  two-sided `p=0.00139`; all `24^4=331,776` permutations). Stride 8 is harder than
  stride 1 in all four corpora, and the blocked correlation is positive for each
  seed separately. Excluding enwik8, which supplied the pilot, the three genuinely
  unseen corpora still give `rho=0.80`, exact `p=0.0140` (post-hoc purity check).
  Curve area independently gives mean within-corpus `rho=0.75`.
  Ordinary spectral degree has `rho=-0.30` because energy and degree remain nearly
  fixed while the recovered support moves from `(1,2)` to `(s,2s)`. The frozen
  verdict is **SUPPORTED**, scoped to a causal locality effect on these four fixed
  byte corpora and this learner. All 48 training cells ran on Modal A10/A10G GPUs
  (486.6 aggregate cell-seconds; zero local training). Audit PASS; 66 tests PASS;
  see `runs/local/v20_confirmatory_geometry/`.
- Unseen-corpus predictor-selection protocol v2.1 (2026-08-07): adds six
  hash-pinned corpora never used in v2.0 (Gutenberg, Reuters, Brown, PubMed,
  CPython, and Linux C), eight preregistered strides `1/2/3/4/6/8/12/16`, and
  three seeds. The causal geometry effect replicates: mean within-corpus
  locality rank versus final held-out `CE/initial_CE` is `rho=0.857`, exact blocked
  `p=1.10e-11`; five of six endpoint directions and all three seed-specific means
  are positive. Curve area gives `rho=0.885`, while ordinary degree gives only
  `rho=0.012/0.127`. However, the frozen out-of-corpus predictor-selection rule
  does **not** select `G_total`: `log2(radius)` has lower LOCO RMSE
  (`0.0809` versus `0.0937`), and `G_total` beats radius in only one of six held-out
  corpora. Its correlation with the spectral-entropy upper bound is `0.999999`,
  failing the identifiability gate. Verdict:
  **GEOMETRY_SUPPORTED_METRIC_UNRESOLVED**. All 144 training cells ran on Modal
  A10 GPUs and all 48 profiles on Modal CPU; zero local training/profiling. Audit
  PASS; 67 tests PASS; see `runs/local/v21_predictor_selection/`.
- Hard-domain single-H100 protocol v2.2 (2026-08-07): prospectively tests 8M
  pinned bytes each of mathlib Lean, Rust source, and RFC technical prose using a
  larger d256 four-layer learner, 8M training tokens, strides `1/4/8/16`, and
  three seeds. All 36 training cells ran sequentially under a one-H100 container
  cap; 12 profiles ran remotely on CPU. The frozen endpoint verdict is
  **SUPPORTED**: mean within-domain locality rho is `0.867` (per-domain
  `1.0/0.8/0.8`), exact blocked `p=0.00579`, all three stride-16 endpoints are
  harder, and seed-specific means are `0.733/0.867/0.867`. It is not uniformly
  cleaner than v2.1: curve-area rho is `1.0/1.0/0.0` (mean `0.667`) because RFC
  stride 8 improves faster early than stride 4. The H100 strengthens endpoint
  separation but does not remove domain-specific learning dynamics. Audit PASS;
  67 tests PASS; see `runs/local/v22_hard_h100/`.
- Positional-geometry robustness protocol v2.3 (2026-08-07): crosses the six
  v2.1 corpora and four strides with learned absolute, sinusoidal, and ALiBi
  d64/l2 Transformers plus a learned d128/l4 model. Three of four configurations
  satisfy the frozen endpoint gate: learned d64/l2 `rho=0.800` (`p=0.000232`),
  ALiBi `rho=0.933` (`p=1.67e-6`), and learned d128/l4 `rho=0.733`
  (`p=0.00109`). Sinusoidal gives only `rho=0.433` (`p=0.0765`). Overall verdict:
  **MIXED**. The data intervention matters, but its effect is conditional on
  positional geometry. Audit PASS; see `runs/local/v23_transformer_robustness/`.
- Spectrum-predictor protocol v2.4 (2026-08-07): estimates a resolved dyadic
  degree/locality lower-bound surface, fits grouped low-data predictors on 13
  development corpora, then locks predictions for 12 hash-pinned unseen corpora
  before 270 natural-text H100 cells. Development gains did not confirm:
  Fourier-only endpoint `R^2=0.260` versus `0.572` for four ordinary controls;
  combined RMSE is 8.8% worse, so the verdict is
  **PREDICTIVE_NOT_UNIQUELY_FOURIER**. A 108-cell exact q-ary factorial finds a
  strong degree effect but architecture-specific locality, and an exploratory
  corrected 18-cell image panel finds DCT high-frequency tail associated with
  curve-area difficulty (`rho=0.943`, nominal `p=0.0048`, only six datasets).
  All 396 training cells ran sequentially with at most one H100; zero local
  training. Audit PASS; see `runs/local/v24_spectrum_predictor/`.

## Quick start

```bash
uv sync                      # CPU tiers
uv sync --extra gpu --extra analysis   # GPU/analysis milestones
uv run pytest -q             # S0/S1 tests
```

## Completed v2.6 confirmation

Protocol v2.6.2 uses 32 new pinned corpora, one
learned-absolute d64/l2 Transformer, two seeds, and exactly 64 sequential H100
cells. Final held-out `CE/initial_CE` is primary. The primary comparison asks
whether the sampled geometric feature improves frozen OLS predictions beyond
ordinary controls plus sampled energy and degree.

All profiles ran on Modal CPU and all training cells ran sequentially with H100
concurrency one. The mechanical protocol label is
`SPECTRUM_SUPPORTED_LOCALITY_UNRESOLVED` because energy/degree has a positive
point improvement. The more informative primary conclusion is
`FROZEN_LOCALITY_INCREMENT_HARMFUL`; the spectrum-only improvement is not
statistically resolved. Audit PASS. Artifacts are under
`runs/local/v26_sampled_locality/`.

The enforced run order is:

```bash
uv run python scripts/v26_prepare_data.py --validate-only
uv run python scripts/v26_prepare_data.py
modal run scripts/v26_modal.py --stage profile
uv run python scripts/v26_freeze_predictions.py
modal run scripts/v26_modal.py --stage train
uv run python scripts/v26_analyze.py
uv run python scripts/v26_audit.py
```

`scripts/v26_dry_run.py` validates the 64-cell grid without downloading data or
launching remote work. Training refuses to start unless the protocol, data,
profile, model, and prediction hashes all match.

## Completed v2.7 marginal-locality confirmation

Protocol v2.7 prospectively tests the corrected marginal locality centroid on
24 source-disjoint corpora in six strata. A one-feature OLS was frozen from 54
compatible development corpora before any of the 48 confirmation training cells
ran. The target is normalized learning time
`(curve_area - final_fraction) / (1 - final_fraction)` for the same
learned-absolute d64/l2 Transformer.

The frozen result is **PREDICTIVE_ONLY**. Relative RMSE improvement over the
historical intercept is 6.14%, with a stratified paired-bootstrap 95% interval
of `[1.70%, 27.96%]`; prospective R² improves from -0.041 to 0.083. The blocked
rank gate does not pass: mean within-stratum Spearman rho is 0.30, one-sided
permutation `p=0.119`. Pooled rho is 0.757, indicating substantial between-domain
signal that should not be mistaken for a uniform within-domain ordering. All 48
cells ran sequentially on one H100 at a time, and the mechanical audit passes.
Artifacts are under `runs/local/v27_marginal_locality/`.

```bash
uv run python scripts/v27_dry_run.py
uv run python scripts/v27_analyze.py
uv run python scripts/v27_audit.py
```

## Completed v2.8 random-window repair

Protocol v2.8 replaces the v2.7 ordered-prefix/cyclic-stream sampling with one
data law. Each byte stream is partitioned into deterministic randomized 16 KiB
blocks: 75% train, 12.5% Fourier profile, and 12.5% validation. Training samples
intact 65-byte context/target windows uniformly with replacement; profiling
samples positions without replacement; validation uses 2,048 fixed random
windows shared across learner seeds. Individual bytes are never shuffled.

The corrected 54-corpus development grid and 78 Fourier profiles were completed
before the 24 confirmation predictions were hash-locked. On 48 sequential H100
confirmation cells, marginal locality reduces RMSE from `0.02544` to `0.01881`,
a **26.04%** improvement with stratified paired-bootstrap interval
`[18.12%, 34.99%]`. Prospective `R^2` is `0.453`; pooled Spearman is `0.806`
(`p=1.99e-6`). Mean within-stratum Spearman is only `0.067`, however, with
blocked permutation `p=0.410`. The frozen verdict is therefore
**PREDICTIVE_ONLY**, and the audit passes. Because this repair reuses the v2.7
source panel, it strengthens the predictive evidence but is not a new
source-disjoint replication. Artifacts are under
`runs/local/v28_random_windows/`.

```bash
uv run python scripts/v28_analyze.py
uv run python scripts/v28_audit.py
```
