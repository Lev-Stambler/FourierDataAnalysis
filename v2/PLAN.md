# PLAN: Degree profiles as a proxy for learnability (by Transformers)

Status: **draft for preregistration.** No runs before Section 9 is frozen.
Prior-art review: Section 0 (2026-08-03, web survey; re-check before submission).

## 0. Prior art (checked 2026-08-03) and our deltas

**Closest theory — the staircase line (Abbe et al.).**
- "The staircase property" (NeurIPS 2021) and "The merged-staircase property" (COLT 2022):
  characterize which sparse Boolean functions SGD on two-layer (mean-field) nets can learn,
  in terms of Fourier-coefficient chains.
- "Generalization on the Unseen, Logic Reasoning and Degree Curriculum" (ICML 2023): nets are
  biased toward min-degree solutions; degree curricula help.
- "Provable Advantage of Curriculum Learning on Parity Targets" (NeurIPS 2023).
- *Delta:* they characterize *learnability/structure* of sparse Boolean functions on the
  uniform hypercube for mean-field two-layer nets. We measure a *difficulty proxy*
  (tokens-to-threshold) for **sequence distributions** parameterized by their full
  categorical degree **profile**, learned by a **standard fixed Transformer**, under
  non-uniform dataset laws (density-constant bookkeeping is ours). We should log the
  staircase/merged-staircase status of every F3 instance as a covariate so their
  learnability predictions are separable from our difficulty claims.

**Closest in spirit — "Why are Sensitive Functions Hard for Transformers?" (ACL 2024).**
Proves Transformer loss landscapes are constrained by input-space sensitivity; notes an
empirical bias toward low-degree functions.
- *Delta:* their quantity is total sensitivity/influence, as an expressivity/landscape
  statement, not a measured sample-complexity proxy on sequence distributions.
- *Action:* **total influence joins the predictor ablation (H4)** — degree must beat
  sensitivity, entropy, and Markov span, not just entropy. Our prelims already define
  dataset sensitivity, so it costs nothing to measure.

**Spectral bias / frequency principle (Rahaman et al. ICML 2019; Xu et al. 2019;
follow-ups incl. "A Fourier perspective on learning dynamics", 2025/26; "Spectral Bias in
Practice").** Nets fit low frequencies first, on continuous/image domains.
- *Delta:* training-dynamics statement about frequency on $\mathbb R^d$; not categorical
  degree, not data-side difficulty prediction. We cite it and use it as an *in-run
  diagnostic*: do our Transformers also acquire level weight low-degree-first?

**Grokking / Fourier circuits on modular arithmetic** (Power et al. 2022; Nanda et al.
2023; Varma et al. 2023; "Why Do You Grok?" 2024; Fourier-circuits case studies).
Mod-$q$ tasks are solved by Fourier circuits; difficulty manifests as delayed
generalization.
- *Delta:* their tasks are 2–3-input operations, not length-$L$ next-token prediction,
  and they explain the mechanism rather than predict difficulty from a data-side profile.
- *Action:* F2 (mod-$q$ subset sums) may grok; add **time-to-generalization-jump** as a
  secondary difficulty metric and log whether it occurs per cell.

**Transformer expressivity limits** (Hahn 2020; Merrill et al.; AC0/sensitivity results).
Constant-depth Transformers cannot compute parity, etc. These are possibility/impossibility
results, not difficulty measurements; our F2/F5 families connect to them but our question
(sample complexity at fixed depth) is different.

**Synthetic-sequence learning-dynamics work** (induction-head formation, Markov-chain ICL,
compositional synthetic tasks). Studies data distribution → learned algorithm, but none
uses a Fourier degree profile as the difficulty axis.

**Bottom line:** no existing work measures the categorical degree profile of a sequence
distribution's next-token function and tests it as a quantitative difficulty proxy for a
fixed Transformer against competing predictors. The spectral-bias, staircase, sensitivity,
and grokking literatures all predict our effect should *exist*; the open question we add
is whether the *profile* is the right quantitative axis (vs. entropy, span, sensitivity),
on sequence laws, at measurable budgets.


## 1. Claim under test

> For next-token prediction over a sequence distribution, the categorical Fourier
> degree profile of the next-token function is a good predictor of how hard the
> distribution is for a fixed, standard Transformer to learn — beyond what entropy
> and Markov span alone predict. The claim is tested causally on synthetic families
> with planted profiles, and correlationally on real language, image (VQ-tokenized),
> and tabular dataset ladders.

Decomposed into hypotheses:

- **H1 (monotonicity).** Holding entropy rate and alphabet fixed, difficulty rises
  monotonically with the degree profile (cutoff degree, or decay rate of level weights).
- **H2 (scaling).** The tokens-to-threshold budget grows roughly polynomially in
  sequence length with exponent proportional to degree: `log T* ~ c·d·log L` — the
  Transformer analog of the theorem's `N_d = Σ_{k≤d} C(n,k)(q−1)^k` scaling.
- **H3 (measurement suffices).** Difficulty is predicted as well by the *measured*
  degree profile (Part II tools, samples only) as by the planted one.
- **H4 (specificity).** Degree predicts difficulty after controlling for entropy rate,
  Markov span, **and total sensitivity/influence** (the ACL-2024 predictor); and none of
  those explains the degree effect away.
- **H5 (external validity).** On real dataset ladders (language, image, tabular —
  Section 3R), measured profile features predict difficulty after controlling for
  domain-standard covariates (entropy floor, dataset size, class imbalance, alphabet
  size); a pooled cross-domain regression with domain fixed effects remains significant.

H4 is the one that can fail, and its failure is a publishable negative result.

## 2. Theory anchor (what is certified vs. what is empirical)

- Certified (`thm:learning-low-degree`): the *spectral* learner needs
  `m = O(B^{2d} N_d/ε · log(N_d/δ))` samples when tail weight above degree d is ≤ ε/4,
  with the `C_D^{-1}` reconstruction scaling; density constant governs aliasing.
- Empirical (this experiment): whether a fixed Transformer's difficulty tracks the same
  axis, with what exponent, and whether the axis is specific once entropy is controlled.
- The paper claims the first as theorem and the second as measurement. Never mix.

## 3. Dataset families (all synthetic, all planted)

Alphabet `A = Z_q` with small q (default q = 32), sequences length L (default 64),
task = next-token prediction. Each family defines a distribution over sequences; the
next-token function's categorical degree profile is controlled by construction.

| ID | Family | Degree knob | Notes |
|---|---|---|---|
| F1 | Markov-k: `x_t = g(x_{t-k..t-1})` + noise | k ∈ {1..8}, local lags | degree ≤ k, position-local |
| F2 | Subset-sum: `x_t = Σ_{i∈S} x_{t-i} mod q` (+ noise) | s = \|S\| ∈ {1..8}, lags spread (e.g. {1, L/4, L/2}) | single degree-s character; **non-local**; separates degree from Markov span |
| F3 | Random polynomial: M random degree-d characters, quantized output | d, and M (sparse vs dense within level) | tests structure-vs-profile; log staircase/merged-staircase status as covariate |
| F4 | Mixed profile: level weights `W^k ∝ r^k` | decay r ∈ (0,1) | "soft degree" |
| F5 | Controls: (a) iid matched entropy; (b) Markov-1 entropy-matched to F2; (c) parity-like max degree | — | isolate entropy vs degree |

Rules for all families: uniform stationary token frequencies (by construction or
rejection); entropy rate computed in closed form or by enumeration; noise level as a
secondary knob (Bayes floor raised above 0 so threshold metrics are well-defined).

**Key design point:** F1 vs F2 at matched entropy separates *degree* from *Markov
span/locality*. F3-sparse vs F3-dense at identical profile separates *profile* from
*within-level structure*. If Transformers care about locality rather than degree,
F1-vs-F2 exposes it; if they care about structure rather than profile, F3 exposes it.

## 3R. Real dataset ladders (external validity; H5)

The synthetic families give causal evidence (knobs we control); the real ladders give
external validity. Same tools, same metrics, same fixed learners — only the data
changes. Every dataset is pinned by source + revision/commit hash in the protocol.

### R1 — Language ladder (next-token LM)

Student = the same fixed L1 Transformer, trained from scratch on each corpus
(checkpointed learning curve; 3 seeds). Teacher for degree mapping and Bayes floor =
one frozen small reference LM (Qwen2.5-0.5B) providing `f(x)=P(next|x)` on corpus
windows; θ is relative to that floor.

| # | Dataset | Tokenization | Why it's in the ladder |
|---|---|---|---|
| 1 | iid random tokens | vocab-matched | max-difficulty control |
| 2 | synthetic F2-s2 corpus | native Z_q | bridge control |
| 3 | TinyStories | char/small-BPE | simple grammar; expect low degree |
| 4 | enwik8 | character | natural text, char level |
| 5 | WikiText-2 | ~10k BPE | natural text, word level |
| 6 | CodeParrot-clean Python subset (capped) | BPE | long-range syntactic structure |
| 7 | sampled arithmetic corpus (`a+b mod p`, chained ops) | native | grokking bridge |

Per-dataset token cap (e.g. 20M train tokens) and one validation split; q varies per
dataset and is logged as a covariate (profile features used are relative tail weights).
Mask-refill conditional for stability estimation: a small masked filler trained per
corpus (cheap), recorded in the manifest; suffix-filtration profile is the fallback
that needs no refill model.

### R2 — Image ladder (VQ-tokenized patches → sequences)

Images become discrete sequences so the *same categorical machinery* applies: one tiny
VQ-VAE per dataset (fixed architecture, 8×8 patches, codebook K=512, tokenizer hash in
manifest); row = raster-order patch-code sequence; student = the L1 Transformer with
vocab K predicting the next code; floor = code-prior entropy.

| # | Dataset | Why |
|---|---|---|
| 1 | Gaussian-noise images, matched code histogram | control |
| 2 | MNIST | simplest structure |
| 3 | FashionMNIST | same scale, more intra-class variation |
| 4 | SVHN | natural images, small |
| 5 | CIFAR-10 | standard |
| 6 | STL-10 (downsampled) | harder textures |

Degree mapping as in R1 (per-dataset small masked filler over patch codes, or suffix
filtration fallback). This ladder meets the spectral-bias literature on its home turf
(images) but with a discrete, dataset-law-native measurement.

### R3 — Tabular ladder (the native dataset setting)

Here `D` *is* the table and `f` is the label column — exactly the object of the
theory. Numerics → 16 quantile bins; per-coordinate alphabet sizes logged.

| # | Dataset (OpenML/sklearn) | Notes |
|---|---|---|
| 1 | iris, balance-scale, tic-tac-toe, car | tiny; fully enumerable → exact `W^k` ground truth |
| 2 | mushroom, kr-vs-kp, credit-g, churn | small; enumerable or nearly so |
| 3 | adult (subset), bank-marketing-8k | medium; measured profiles only |
| 4 | connect-4 (subsampled), mini-boone | larger controls |

Learner = one fixed MLP (2×256, identical across datasets, early stopping); difficulty
= samples-to-target log-loss on a fixed budget grid. Additionally measured:
coordinate-masking total influence (the ACL-2024 predictor and our own `Sens`), so the
predictor ablation runs on real data too. Enumerable instances double as ground-truth
checks of the measurement tools on *real* data, not just synthetic.

## 4. Degree mapping protocol (the "map the degree of the dataset" step)

For each family instance:

1. **Planted profile** — exact, from construction (level weights `W^k`, max degree).
2. **Enumerated profile** (small instances only, L ≤ 20): brute-force Fourier
   transform over the full enumerable distribution; ground truth for tool validation.
3. **Measured profile** — from m samples only, using Part II tools:
   - noise-stability envelope at ρ-grid (paired suffix/resampling estimator);
   - filtration increments under sampled coordinate orderings;
   - output: estimated tail bounds `B − ρ^{-d} Stab_ρ ≤ W^{>d} ≤ (B − Stab_ρ)/(1−ρ^{d+1})`.
4. Deliverable: measured-vs-planted calibration plot; the measured profile is what
   enters H3.

## 5. Learners

- **L0 — Spectral learner (calibration instrument).** Truncated Fourier
  reconstruction with the `C_D^{-1}` scaling; closed form. Its sample complexity is
  the theorem's prediction; it anchors the x-axis of "predicted difficulty".
- **L1 — Fixed Transformer (primary subject).** ONE configuration for all runs:
  chosen in a preregistered pilot on the mid-difficulty family only (pilot selection
  criterion written down first: smallest config whose final loss is within 10% of the
  largest pilot config on the mid family). Defaults: d_model 128, 4 layers, 4 heads,
  context L, rotary positions, AdamW, cosine schedule, weight decay 0.1.
- **L2 — Fixed MLP/TCN control (optional, Phase 3):** checks the effect is not
  Transformer-specific but "deep-learning-generic".

## 6. Training protocol (fixed across all cells)

- Data: fresh streams from the family sampler; no epoch reuse before the budget grid
  is exhausted (statistical, not memorization, regime).
- Budget grid: total training tokens `T ∈ {1e5, 3e5, 1e6, 3e6}`; checkpoint the
  validation curve at ~20 log-spaced points.
- Validation: freshly sampled sequences, cross-entropy in bits/token.
- Difficulty metrics (all preregistered):
  - `T*(θ)` = first T where val CE ≤ Bayes floor + θ (θ fixed globally, e.g. 0.05 bits);
  - final gap = val CE − Bayes floor at the largest budget;
  - sample-complexity proxy from L0 at matched error;
  - secondary (F2 cells): time-to-generalization-jump (grokking), logged if present;
  - diagnostic: per-level Fourier weight of the *trained* model's predictions vs training
    time (low-degree-first acquisition check, connecting to spectral bias).
- Seeds: ≥ 3 per (family, knob value) cell; report median + spread.

## 7. Analysis plan (fixed before P2)

1. Difficulty-vs-knob curves per family: `T*` vs k (F1), vs s (F2), vs d/M (F3), vs r (F4).
2. Scaling fit: `log T* = a + b·d·log L` across a small L-grid (L ∈ {32, 64, 128}) for
   F2; report exponent b vs the theorem's `N_d` exponent.
3. Predictor ablation for H4: fit `log T*` with (i) degree features only, (ii)
   entropy + Markov span + **total sensitivity** only, (iii) both; compare adjusted
   R² / predictability on held-out family instances.
4. H3: replace planted with measured profiles in (1)–(3); report degradation.
5. Real-ladder regressions for H5: per ladder (R1–R3), fit difficulty ~ measured profile
   + domain covariates (entropy floor, size, imbalance, q); then the pooled
   cross-domain model with domain fixed effects; report per-ladder and pooled R², and
   the profile-vs-entropy-only difficulty ranking agreement per ladder.
6. Falsification criteria (preregistered):
   - H1 falsified if any family shows non-monotone `T*` in its degree knob at fixed
     entropy (beyond seed noise, per a pre-specified trend test);
   - H4 falsified if model (ii) explains ≥ 90% of what model (iii) explains.
   Either falsification is written up as a negative result with the same rigor.

## 8. Milestones, tiers, and compute budget (single source of truth)

Tiers are size levels over one frozen table, not separate code paths: a cell spec is
`(dataset/family, knob, seed, tier)` and the tier selects `q, L, budgets, n_pairs`.
Each tier unlocks only when the previous tier's gate is green; `run_tier` refuses to
launch past a failed gate.

### Tier definitions

| Tier | Scale | Compute | Wall target | Est. cost |
|---|---|---|---|---|
| **S0 smoke** | q=8, L=8 (tabular toy) | Modal CPU or laptop | < 30 s | ~$0 |
| **S1 enumerable** | q=16, L=16; enumerable tabular set | Modal CPU | < 5 min | <$1 |
| **S2 protocol size** | q=32, L=64; full tabular ladder | Modal CPU, parallel | < 30 min | <$2 |
| **G1 GPU smoke** | q=32, L=64 | 1× cheap GPU (A10G/T4) | < 10 min | <$1 |
| **G2 grid** | all cells incl. real ladders | 1 GPU per cell, fanned out | ≤ GPU cap | ≈$60–90 |

### Milestones

| M | Deliverable | Tier | Phase | Gate |
|---|---|---|---|---|
| M0 | repo scaffold, `domains.py`, `base.py`, F2-s2 | S0 | P0 | unit/identity tests green |
| M1 | `enumerate.py` + `planted.py` agreement | S1 | P0 | planted == enumerated exactly |
| M2 | L0 spectral learner; theorem-scaling curves | S1→S2 | P0 | exponent within tolerance of `N_d` |
| M3 | `stability.py` + `filtration.py`; measured-vs-planted calibration | S2 | P1 | tail bounds sound; calibration passes |
| M4 | Transformer + stream + metrics; F1-k1 smoke | G1 | P2-pre | val CE reaches floor + θ on easy cell |
| M5 | pilot config selection; **protocol freeze** | G1 | P2-pre | preregistered pilot rule satisfied |
| M6 | synthetic grid F1–F5 × knobs × seeds | G2 | P2 | all cells have complete manifests |
| M7 | R1 language ladder (7 corpora × 3 seeds) + profiles | G2 | P2 | manifests + measured profiles recorded |
| M8 | R2 image ladder (VQ tokenizers + 6 datasets × 3 seeds) | G2 | P2 | same |
| M9 | R3 tabular ladder (fixed MLP; enumerable ground truths) | S2/G1 | P2 | same |
| M10 | analysis, ablations, hypothesis verdicts (incl. H5) | CPU local | P3/P4 | falsification criteria evaluated verbatim |

Phase mapping: P0 = M0–M2, P1 = M3, P2 = M4–M9, P3/P4 = M10 (and the optional L2
control runs fold into M6/M9 if budget allows).

M0–M3 are CPU-only and must pass before any Transformer code runs on GPU: every
failure mode is designed to surface at the cheapest tier that can exhibit it.

**Budget.** CPU spend < $5 total. GPU hard cap **80 GPU-hours** (≈ $60–90 at A10G-class
rates), enforced both in-runner (Section 12.6) and by an account spend limit of 1.5×
the cap. No result exists until a run's config hash, seed, and metric artifact are
recorded (same artifact discipline as the old Part 2 protocol). Cost figures are
order-of-magnitude at current Modal rates; re-verify at freeze.

## 9. Preregistration checklist (freeze before P2)

- [ ] prior-art re-check (Section 0) rerun, diff noted;
- [ ] family definitions as executable samplers with version hashes;
- [ ] q, L grid, θ, budget grid, seed list;
- [ ] Transformer config (after pilot, pilot rule documented);
- [ ] metric definitions and trend-test specification;
- [ ] falsification criteria (Section 7.5) copied verbatim;
- [ ] real dataset ladders (Section 3R): dataset lists pinned with source + revision
      hashes, token caps, teacher model + revision, VQ-tokenizer convention;
- [ ] Modal tier table and milestone gates (Section 8) frozen verbatim; app/volume
      names and image hashes recorded; account spend limit set to 1.5× the GPU-hour cap.

## 10. Paper integration

- Part II sections (degree profiles, stability envelope, filtration decomposition)
  supply Sections 4's tools; this plan is their experimental section.
- Write-up scope guardrail: the Transformer result is *a measurement about a proxy*,
  never a compression or SOTA claim. The spectral learner carries the theorem; the
  Transformer carries the motivation.

## 11. Risks

- **Locality bias:** attention + positional encoding may reward F1 over F2 regardless
  of degree → H4's controls catch it; if it dominates, the honest statement is
  "degree predicts difficulty within locality class".
- **Optimization vs statistics:** fixed schedule may confound optimization difficulty
  with sample complexity → the data-rich final-budget metric separates them; L0 anchor
  isolates the statistical part.
- **Small q:** results at q=32 may not extrapolate to large vocab → state as a scope
  limit; one q-sweep cell (q ∈ {16, 32, 64}) in P3 if budget allows.
- **Threshold choice:** θ relative to Bayes floor keeps metrics comparable across
  families with different floors.
- **Real-ladder confounds:** q and tokenization differ across R1 corpora (logged as
  covariates; use relative tail-weight features); VQ tokenization quality affects R2
  floors (fixed tokenizer protocol, hash recorded); class imbalance affects R3
  (logged; balanced log-loss metric).
- **Refill oracles on real data:** masked fillers for stability estimation are
  themselves trained models; the suffix-filtration fallback needs no refill model and
  both variants are recorded so conclusions can be restricted to the oracle-free one.

## 12. Concrete coding plan

Self-contained package at `v2/experiments/degree_learnability/` (uv-managed; nothing
imports from `NEW/`, `experiments/`, or `experiments--bin-lsh/`). Package name: `dlx`.

### 12.1 Layout

```
v2/experiments/degree_learnability/
├── pyproject.toml              # uv package; deps: torch, numpy, pytest (+dev: ruff)
├── README.md                   # how to run P0..P4 from the command line
├── dlx/
│   ├── domains.py              # Z_q^n types; characters χ_α; FFT over enumerated tables
│   ├── families/
│   │   ├── base.py             # Family interface (below)
│   │   ├── f1_markov.py        # Markov-k, noise η
│   │   ├── f2_subset_sum.py    # mod-q subset sums, spread lags, noise η
│   │   ├── f3_random_poly.py   # random degree-d characters, sparse/dense; logs staircase status
│   │   ├── f4_mixed_profile.py # level weights W^k ∝ r^k
│   │   └── f5_controls.py      # iid + entropy-matched Markov-1 + max-degree
│   ├── profiles/
│   │   ├── planted.py          # exact profile from construction
│   │   ├── enumerate.py        # brute-force Fourier on the *support subspace* of f
│   │   ├── stability.py        # Stab_ρ grid via paired mask-refill; tail bounds
│   │   └── filtration.py       # filtration increments over sampled orderings
│   ├── learners/
│   │   ├── spectral.py         # L0: truncated Fourier reconstruction with C_D^{-1} scaling
│   │   ├── transformer.py      # L1: minimal causal Transformer (torch, from scratch)
│   │   └── mlp.py              # L2: fixed MLP/TCN control (P3 only)
│   ├── training/
│   │   ├── stream.py           # fresh on-the-fly token stream; bounded shuffle buffer (size in protocol)
│   │   ├── run.py              # one training run: config → checkpoints + val curve
│   │   └── metrics.py          # T*(θ), final gap, grokking-jump detector, level-acquisition diagnostic
│   ├── analysis/
│   │   ├── difficulty.py       # artifacts → difficulty scalars table
│   │   ├── ablation.py         # predictor fits (i)/(ii)/(iii), R², trend tests
│   │   └── plots.py            # the fixed figure set from Section 7
│   └── protocol/
│       ├── schema.py           # frozen Protocol dataclass ⇄ JSON
│       └── registry.py         # run manifests (see 12.6)
├── tests/                      # see 12.4
├── configs/                    # frozen protocol JSONs (created at Section 9 freeze)
└── runs/                       # artifacts + manifests (gitignored except manifests)
```

### 12.2 Core interfaces (write these first, everything programs against them)

```python
class Family:                       # dlx/families/base.py
    name: str; version: str         # version = hash of sampler code + params
    q: int; L: int
    def sample(self, n_tokens, rng) -> np.ndarray[int]: ...
    def conditional_sample(self, mask, values, rng) -> np.ndarray[int]:
        """exact draw from the family law given observed positions (mask-refill oracle)."""
    def next_token_dist(self, context) -> np.ndarray[float]:
        """exact P(x_{t+1} | context); closed form by construction."""
    def entropy_rate(self) -> float: ...        # Bayes floor, closed form
    def planted_profile(self) -> LevelWeights: ...
```

- Enumeration never touches `q^L`: it Fourier-transforms `next_token_dist` restricted to
  its planted support coordinates (k positions for F1, s lags for F2, coefficient supports
  for F3), plus a random-coordinate check that the support is complete.
- `conditional_sample` exists per family from day one: it is the toy version of the
  masked-model oracle and is what `stability.py`/`filtration.py` call. (F1/F5: trivial;
  F2 with noise η: small exact computation; F3/F4: ancestral re-sampling of the rule.)

### 12.3 Milestones and dependencies

Milestones M0–M10, their tiers, phases, and gates live in **Section 8** (single source
of truth; the Modal tier mechanics of Section 13 execute them). M0–M3 are CPU-only and
must pass before any Transformer code is written — the tools get validated on ground
truth first, exactly as the paper's verification contract demands.

### 12.4 Tests (all preregistration-relevant, not just hygiene)

1. Character orthogonality on `Z_q^n` for composite q (`domains`).
2. Planted profile == enumerated profile on every family at small size (`profiles`).
3. Tail-bound soundness: `B − ρ^{-d}Stab_ρ ≤ W^{>d} ≤ (B − Stab_ρ)/(1−ρ^{d+1})` holds on
   enumerable instances for the whole ρ-grid (`stability`).
4. Filtration telescoping: `Σ_k Δ_k = Var(f)` to floating tolerance, several orderings.
5. L0 exactness: noiseless degree-d target, `|D| ≥ N_d` → reconstruction error 0.
6. Family sanity: uniform stationary marginals (χ² test), entropy estimate matches
   `entropy_rate()` within Monte-Carlo error.
7. Determinism: same seed → identical token stream and identical training curve
   (torch seeded, `torch.use_deterministic_algorithms(True)` on CPU path).

### 12.5 Determinism and seeding

- Every object takes an explicit `np.random.Generator`; global seed tree:
  `seed(family, cell, split) = hash(protocol_hash, family.version, cell_id, split)`.
- Torch seeded per run; CUDA nondeterminism tolerated in P2 but the manifest records
  device + torch version; CPU smoke runs are bit-reproducible.

### 12.6 Artifact discipline

A run writes `runs/<cell_id>/manifest.json`:
`{protocol_hash, family.version, cell_id, seed, budget_tokens, device,
torch_version, wallclock, config_hash, metrics_path, status}`.
`difficulty.py` refuses to read any directory without a complete manifest. The frozen
protocol JSON in `configs/` is the only place hyperparameters live; code reads it, never
hardcodes knobs. GPU-hour accounting: `run.py` accumulates token-throughput × time and
aborts the grid if the protocol cap (80 GPU-h) would be exceeded.

### 12.7 What we deliberately do NOT build

- No wandb/mlflow: manifests + JSON metrics are enough at this scale.
- No distributed training: single-device runs only; budgets are chosen to fit.
- No tokenizer/data-loader generality: families emit raw ids in `Z_q`.
- No hyperparameter search beyond the one preregistered pilot (Section 5).

### 12.8 First three concrete steps (in order)

1. `uv init` the package; `domains.py` with characters + a 4-point FFT test.
2. `families/base.py` + `f2_subset_sum.py` (q=32, L=64, s=2, η>0) with all four
   interface methods; tests 1, 2, 6.
3. `profiles/enumerate.py` + `planted.py`; confirm exact agreement; then M2's L0 on it.

## 13. Running on Modal with size escalation

All P0–P4 compute runs through Modal (repo convention: `modal.App`, image + volume,
timeouts, retries), but every entry point also runs locally with the identical code
path — the executor is a parameter, so debugging is `dlx local …` and scale-out is
`modal run …` against the same cell spec. One Modal app: `dlx-degree-learnability`,
one Volume `dlx-runs` mounted at `/runs` for artifacts and manifests (the manifest
rules of Section 12.6 apply unchanged; the image hash is recorded in every manifest).

### 13.1 Escalation tiers

Tier definitions, sizes, wall targets, costs, and the milestone gates that unlock them
are consolidated in **Section 8** (single source of truth, frozen with the protocol).
This section specifies only the Modal mechanics that execute them. Account-level spend
limit: 1.5× the GPU cap, independent of the in-runner accounting of Section 12.6.

### 13.2 Modal app shape (`dlx/modal_app.py`)

```python
app = modal.App("dlx-degree-learnability")
image = ...  # python 3.12, torch (cpu or cuda wheel per function), numpy, pytest, dlx installed from mounted source
runs_vol = modal.Volume.from_name("dlx-runs", create_if_missing=True)

@app.function(image=cpu_image, volumes={"/runs": runs_vol}, timeout=1800, retries=2)
def cpu_cell(cell: CellSpec) -> str: ...        # returns manifest path

@app.function(image=gpu_image, gpu="A10G", volumes={"/runs": runs_vol}, timeout=10800, retries=1)
def gpu_cell(cell: CellSpec) -> str: ...

@app.function(image=cpu_image, volumes={"/runs": runs_vol}, timeout=3600)
def run_tier(tier: str) -> dict:
    """fan out one tier: gate-check previous tier, run pytest tier suite first
    (fail fast), then cells via .map(), then return gate verdict."""
```

- Fan-out via `cpu_cell.map(cells)` / `gpu_cell.map(cells)`; cell list is generated
  from the frozen protocol JSON, never hand-typed.
- Each cell function runs the tier's pytest smoke subset *first* and aborts the cell
  if it fails (fail fast, cheap tier catches breakage before GPU spend).
- Artifacts land at `/runs/<cell_id>/` on the volume; a `dlx sync` CLI pulls manifests
  and metrics down for local analysis (analysis never runs on Modal).

### 13.3 Debug-first workflow

1. **Local loop:** `dlx local --tier s0 --cell F2/s2` — seconds, laptop, debugger
   attached. Same seeds, same code, Modal not involved.
2. **Modal parity check:** `modal run dlx/modal_app.py::cpu_cell --cell F2/s2` —
   confirms image/mount correctness at S0 cost before any batch.
3. **Tier run:** `modal run …::run_tier --tier s1` — gate check, smoke tests, fan-out,
   verdict JSON written to the volume.
4. **Interactive:** `modal shell` into the image with the volume mounted for post-
   mortem on failed cells; manifests record everything needed to reproduce locally.
5. **Escalation discipline:** no tier is launched manually past its gate; `run_tier`
   refuses and prints which gate failed. This is the mechanism that keeps the experiment
   fast and cheap to debug: every failure mode is designed to surface at the cheapest
   tier that can exhibit it.

### 13.4 Checklist additions (Section 9)

Covered by the Section 9 items: milestone/tier table (Section 8) frozen verbatim;
Modal app name, volume name, and image hashes recorded; account spend limit set to
1.5× the GPU-hour cap.

## 14. Amendment & deviation log (execution record, M0–M10)

Frozen protocols: `configs/protocol_v1.json` (90d42a12), `protocol_v1.1.json`
(32652e6d), `protocol_v1.2.json` (7b87f24b). Full verdict: `experiments/
degree_learnability/VERDICT.md`.

- **v1.1 (2026-08-04):** F3/F4 rebuilt in exponential-softmax form (amp=1.0,
  beta=32.0) after the M6 grid showed v1 additive amplitudes left Bayes floors
  within theta of uniform CE (T* degenerate). 12 F3/F4 cells retrained under v1.1.
  Secondary difficulty scalars added (final_gap_bits, norm_remaining, T_half).
- **v1.2 (2026-08-04):** R1/R2 student training budget reduced 20M -> 5M
  tokens/codes (CPU-budget tractability); M7/M8 students run on local CPU
  (goal local-fallback allowance); connect-4 resolved to OpenML did 1591 variant;
  R2 images converted to 32x32 grayscale for a uniform fixed VQ architecture;
  cyclic epoch reuse for fixed datasets (image codes; stream-exhausted wikitext).
- **M3 gate refinement:** measured-input tail-bound containment uses a
  preregistered Hoeffding margin (delta=1e-6); algebraic soundness checked
  separately with exact inputs.
- **enwik8:** pinned HF repo is script-only (unsupported by datasets>=5), so the
  canonical enwik8 file is fetched from its origin URL; file sha recorded in meta.
- **rung-1 iid floor:** set to exact 12.0 = log2(4096) (generating-law floor);
  Qwen text entropy not applicable to random token ids.
- **R2 floor convention:** protocol-specified marginal code-prior entropy is an
  upper bound on the conditional entropy rate; strong datasets show small/negative
  gaps; documented in VERDICT.md.
- **Total sensitivity covariate (H4):** exact spectral influences where enumerable
  (M9 + small instances); protocol-size synthetic cells lack a conditional oracle.
- **Modal client flakiness:** a premature cancellation during the first 51-wide
  fan-out was resolved by re-running affected cells; all 51 cells committed and
  validated (census in VERDICT.md).

## Verdict summary

H1 PASS (scope note) · H2 PASS · H3 PASS · H4 FAIL (falsified) · H5 FAIL.
The degree-learnability proxy is validated for contiguous synthetic structure but
falsified as a cross-structure / cross-domain predictor: support locality/geometry
and problem size dominate. See VERDICT.md.
