# Canonical q-ary Dataset GL over sign-LSH token codes

One isolated module (`qary_lsh_dataset_gl.py`) implementing the paper's dataset-GL fiber
recipe end to end, with brute-force-anchored tests (`test_qary_lsh_dataset_gl.py`, 12 tests).
No imports from `fda_exp`; artifacts live under `/cache/canonical/qary_lsh_gl` on the Modal
volume `fda-cache` with a self-enforced cost ledger (`cost.json`).

## The experiment

- **Fibers from the dataset**: M anchors in real FineWeb text; the real prefix (last k=3
  tokens = the deduplicated fixed string) is the conditioning. The teacher (Qwen3.5-0.8B)
  fills the remaining `fill_len` window tokens R=8 times per fiber — the model is the
  natural-language sampler; two fills of one fiber are a CSAMP pair.
- **Encoding q = 2^B**: every token enters the GL domain as its B-bit sign-LSH code
  (mean-centered teacher embeddings, nested Gaussian projections, tie-break bits for
  inseparable duplicate rows; B=133 at cap 128). Controls: capacity-matched random codes,
  raw token-id bits.
- **Search**: exact-Parseval csamp group-by tree over the filled span's bits, τ the only
  statistical parameter (`W ≥ τ²/4`, exact leaf test), ordered bounded frontier
  (`max_width`, compute-only) **deduplicated by on-data equivalence**, **centered targets**,
  incremental suffix-group refinement, fp32 buckets. Frontiers/norms persisted: changing τ
  is a re-filter, not a re-descent.

## Visualize

`vis/` is a self-contained explainer website (what the Fourier transform is applied to, the
KL objective, sign-LSH vs controls, the GL tree / paired-ψ estimators as an interactive toy,
and the recovered sparse model running live in-browser, self-checked against `demo.py`):
`cd vis && python -m http.server 8000`. Regenerate its data with
`uv run python vis/export_data.py`; test with `node vis/test_node.js`. See `vis/README.md`.

## Run

```bash
uv sync && uv run pytest                    # 12 tests, incl. brute-force anchor
# launch clients via setsid so they outlive tool timeouts; functions self-commit
uv run modal run --detach qary_lsh_dataset_gl.py --stage data --fill-len 61 --m-fibers 8000
uv run modal run --detach qary_lsh_dataset_gl.py --stage search --tau 0.02 --fill-len 61 \
    --m-fibers 8000 --max-width 64 --encoding parallel
uv run modal run --detach qary_lsh_dataset_gl.py --stage fit-deg1 --fill-len 61 --m-fibers 8000
uv run modal run --detach qary_lsh_dataset_gl.py --stage sensitivity --m-fibers 1000 --g 16
```

## Degree bounds via sensitivity — TOP-1 target (2026-07-14; sections/experiments.typ `<sec:sensitivity>`)

The fit-free probe of `thm:learning-low-degree`, on the **full-vocab top-1 target** (the arc's
agreement metric; the 512-slot projection is deliberately absent from this stage — do not
reintroduce it): fork the teacher's cache at back-offset `b`, resample that one token g=16 times
from the teacher's own conditional, teacher-force the real suffix, read the full-vocab argmax
(`_resample_and_label` + `_sens_from_top1`, brute-force-anchored in tests; every forked
evaluation self-checked for argmax equality against a fresh forward). Sens_b = P(resampling the
token b back flips the top-1); Var = P(two random contexts disagree on top-1) = 0.985.

M=1000 FineWeb spans: flip probability 0.60 at the last token, 0.18 three back, then a **plateau
at 2–3% per position from b≈23 all the way to b=124** (se ~0.003 — the tail is signal; identical
resampled tokens give identical rows, so flips only come from real substitutions). Measured
positions alone: S̄ = 2.14 (**d_eff ≥ 2.17, interpolation-free floor**); densified: S̄ = 5.33 →
**d_eff = S̄/Var ≈ 5.4**. Theorem bound d ≥ 4S̄/ε: 43 / 87 / 216 at ε_rel = 0.5 / 0.25 / 0.1;
Markov caps tails only weakly at this mean (≤ (d_eff−1)/(K−1) above degree K, still 49% at K=10).

**The top-1 function is NOT low-degree** — mirror image of the old distributional target. Argmax
flips are decided at near-ties resolved by long-range context, spreading spectral mass to degree
≈5+. This is the fit-free explanation of why held-out top-1 agreement barely moved for every
window student: a short-window low-degree model cannot represent a degree-≈5 function whose flip
mass plateaus across all 128 positions. For the top-1 program the lever is context coverage, not
window degree. Artifacts: `sensitivity_top1_conditional_fineweb_M1000.{json,npz}`; W&B run
`sens-top1-conditional-fineweb-M1000`.

## Exact deg-2 spectroscopy — deg-1+2 = 0.233 top-1 / 3.49 KL (2026-07-15, stages `plain-data`/`grad-sense`/`deg2-exact`/`deg2-fit`)

PLAIN streamed data (no fibers/fills — those are Dataset-GL requirements): 1M FineWeb-Edu
spans, one teacher forward each, UNIT-NORMALIZED centered pre-head target (Parseval: mass =
0.727 centered + 0.273 mean; ψ absolute), full-vocab top-1 + KL, sanity 0.980.

**An earlier version of this section declared the deg-≥2 spectrum "diffuse — character
scaling dead". That was a METHOD ARTIFACT, caught by Lev's keep-going loop.** Three stacked
failures produced it: (1) gradient soft-mixture sensing silently missed every heavy pair
(best find ψ≈3e-5 vs true max 3.2e-4 — never bound a spectrum by search); (2) the accept
gate sat at 7× the noise floor; (3) plain-coefficient reconstruction over correlated pairs
double-counts shared mass (first ladder DEGRADED 0.148→0.017, KL 123). The replacement is
EXACT: `deg2-exact` enumerates every pair character's coefficient via one n×n GEMM per
projected target dim (`M_d = Xᵀ(g_d wX)/Σw`, all 9M win-32 pairs in ~60 s, per-pair floor
2.5e-7), and `deg2-fit` fits the top pairs by block-OMP projection (correlated dictionaries
need projections, not independent averages; `sequential_deflate(block=512)`).

| model (win 32, 1M contexts, lsh) | TEST top-1 | TEST KL |
|---|---|---|
| mean vector only | — | 5.82 |
| deg-1 exact (4,256 chars, closed form) | 0.148 | 4.68 |
| + 6,000 exact deg-2 pairs | 0.200 | 4.02 |
| + 48,000 | 0.228 | 3.63 |
| **+ 160,000 (deg-2 test-optimal)** | **0.233** | **3.49** |
| + 400,000 | 0.232 | 3.48 |
| deg-1+2 **+ 100,000 exact deg-3 triples** (`deg3-fit`) | 0.251 | 3.19 |
| deg-1+2 + 200,000 triples | 0.254 | 3.13 |
| deg-1+2 **+ 1M STAGED triples (809k local-first + far)** | **0.2642** | **3.09** |
| + 2M triples / + quads on the overfit base | 0.254–0.256 | — |

- **+58% relative top-1 over deg-1**; deg-2 captures 0.233 of the 0.609 post-deg-1 residual;
  overfit onset only below ~4× floor. Beats every prior model in the program incl. the old
  "MLP ceiling" 0.167 (fiber data).
- **The structure is interpretable**: ~75% of significant pair mass is SAME-TOKEN-BLOCK
  quadratics of the newest ~5 tokens (richer token-identity features than linear code bits);
  cross-block pairs are adjacent-token bigrams, (0,1) ≫ (0,2) > (1,2). Heavy pairs live in
  the last ~32 tokens (win 61/128 enumerations match win 32). Deg-1 itself: 0.148 at every
  window 32/61/128; grew 0.132→0.148 from 50k→1M contexts.
- ψ values are NOT additive across pairs (on-data correlation): naive Σψ "exceeds" the
  residual mass; only projection-based captured mass is meaningful.
- Fail-loud lessons baked into code + tests: batch deflation amplifies duplicate clusters
  (1−m)²; residual must never grow; ψ ≤ total mass; floor-relative gates (ψ noise
  concentrates within √(2/dY)≈4% of floor).
- **Sparsity audit** (`_sparsity_stats`): deg-2 IS sparse (N_eff 18.4k; half the pair energy
  in 8,982 chars); deg-3 is a diffuse sea (N_eff 855k; k50 = 496k of 2.5M) — the ladder's
  ~1M turnover is dust-coefficient noise, so MORE DATA extends it (4M contexts generating).
  Far-anchored triples (5.07M above 4× floor — long-range copy-gating shape) help only when
  fitted AFTER locals (staged order); global ψ-ranking displaced better locals (0.2466).
  Deg-4 quads: 3.46M above floor from 2k pair-anchors, but flat when fitted on the OVERFIT
  2.5M-triple base — next rerun caps each layer at its test-optimal count first.
  DIAG (failure modes): copy cases 0.38 vs non-copy 0.19; teacher token in our top-5 48%;
  RARE targets (<100 train occurrences, 26% of cases) fail at ~3% regardless of copy —
  and only 17% of them even appear in the 32-token window (context-coverage, not gating).
- **Encoding control (exact regime)**: random codes — deg-1 0.124, deg-1+2 peak 0.218/KL
  3.86 (more raw deg-2 mass, 0.249: identity stored quadratically instead of linearly) vs
  LSH 0.148 / 0.233 / 3.49 — **LSH keeps +19% (deg-1) and +7% (deg-1+2) relative**; the
  encoding matters at every level, less as degree grows.
- **Learned decomposition (`fourier-learn`, Track B) — a supported null**: inclusion-gate
  characters (any degree, log-space product, STE to the closest character, bounded
  coefficients, leave-one-out −log ψ sensing) recover planted chars to deg-5 from warm
  prefixes in tests, but on the real residual four design iterations (cold start; full-warm;
  coefficient-anchored; dust-explorer on the fully-deflated residual) all fail to grow
  degree or discover beyond the enumeration frontier (final: 4,096 long-span dust chars,
  captured 4e-4). Each iteration's failure mode is test-pinned (sign-blind STE gradients;
  coefficient–mask deadlock; down-degree migration toward unfitted heavy chars). Gradient
  exploration adds nothing over exact enumeration on this representation.
- **Degree 3 works the same way** (`deg3-fit`): a triple's coefficient is the PAIR map of
  the sign-flipped target (`psi3(a,b,c) = deg2map(G·χ_c)[a,b]`; one global projection shared
  across anchors — the rowwise flip commutes). 378k triples above 6× floor in the newest 3
  blocks (40 s enumeration); block-OMP on the deg-1+2 residual adds +0.021 top-1 and −0.36
  KL at the 200k cap, still climbing.
- Artifacts: `plain_ctx128_N1000000_s30000_plain_tr.npz` (reusable supervision),
  `deg2exact_lsh_win{32,61}_r64.npz` (top-1M pairs + deg-1 fits), `summary_deg{2,3}fit_lsh_
  win32.json`; W&B `deg2exact-*`/`deg2fit-*`/`deg3fit-*`. Next: uncapped triples + 4 blocks,
  deg-4 via pair anchors, more contexts to push floors down.

## Learned Fourier features — TOP-1 target (2026-07-14, stage `dl-fourier`)

LEARN the parities instead of GL-searching them: an explicit degree profile (512 deg-1 +
512 deg-2 + 256 deg-3) of straight-through-selected bit products, linear head on the
standardized hidden target, activation-decorrelation diversity term (`learn_fourier_masks`;
STE forward is exactly the hard parity of the argmax picks, test-pinned). Evaluated 4 ways
per encoding on held-out full-vocab top-1 (M16k×R8 = 128k contexts, fill61, sanity 0.977):

| encoding | e2e | **ridge-refit** | random same-profile | MLP ceiling |
|---|---|---|---|---|
| lsh  | 0.152 | **0.160** | 0.096 | 0.167 |
| ctrl | 0.136 | **0.142** | 0.075 | 0.135 |

- **NO conclusion vs Dataset GL from these numbers**: the tree's 0.141 masks were
  DISCOVERED on the forked m1500×g16 data (~24k fork rows); these masks were learned
  directly on the 128k flat contexts — the comparison confounds algorithm with
  discovery-data budget (and the 0.160 ≈ the 512 deg-1 anchors' 0.158 on the same data).
  What IS supported: learning buys 1.7× over random masks of the SAME degree profile
  on the SAME data (0.160 vs 0.096).
- **The learned parities nearly saturate the bits**: an unconstrained 2×2048 GELU MLP on
  the identical inputs reaches only 0.167 — the sparse interpretable model is within 4%
  relative of ANYTHING representable from these 4,087 bits at this data size. ≥0.9 top-1
  is therefore a context-coverage/data problem (cf. sensitivity: d_eff ≈ 5.4, flip mass
  across all 128 positions), not a feature-learning problem.
- **The encoding gap survives the strongest learner**: lsh > ctrl for the learned parities
  (+13% rel) AND for the fully-trained MLP (0.167 vs 0.135, +24%) — on ctrl codes the MLP
  overfits (val_mse 0.96 vs lsh 0.86) and even loses to the ctrl parity model.
- Degree discipline held: 1 of 1,280 features collapsed degree (duplicate pick); dedupe
  removed ~5. Artifacts: `dlfourier_masks_{lsh,ctrl}_M16000_K512_512_256.npz`,
  `summary_dlfourier_M16000_K512_512_256.json`; W&B `dlfourier-{lsh,ctrl}-M16000-K512+512+256`.
- Launch: `setsid nohup uv run modal run --detach qary_lsh_dataset_gl.py --stage dl-fourier
  --fill-len 61 --m-fibers 16000 --r 8 --k1 512 --k2 512 --k3 256 --lam 0.1 --steps 3000 &`

## Results (2026-07-13; see sections/experiments.typ `<sec:whlsh-canonical>`)

0. **SPARSE RECOVERY WORKS, TWO DEGREES DEEP — clean-test verified** (`spectrum-deg1`,
   `spectrum-deg2`, `fit-sparse2` with a strict 3-way fiber split; test untouched):
   ~220 exactly-certified deg-1 characters → TEST KL 1.146; + top 1,000 certified pairs →
   **TEST KL 1.1034 (top-1 65.7%), the best model of the arc** vs ctrl 1.2003 and unigram
   1.6304 — encoding gap 0.097 nats on untouched data. Deg-2 spectrum: LSH has 485k pairs
   ≥ 0.01 (27%) vs ctrl 19.8k (1.4%) — a 25× low-degree-concentration gap; encodings
   converge as degree grows, LSH's edge is needing LESS degree. Recovery method: exact
   per-degree anchored GEMMs (never the tree below deg ~3 — the tie region buries
   candidates; see finding 1). The recovered model is 2MB / 625k params
   (`model_sparse2_lsh.npz`); try it: `uv run python demo.py --prompt "..."`.
0b. **The ladder terminates at degree 2 — by redundancy, not absence** (`fit-sparse3`):
   1,000 certified triples exist (top 0.0604 > any pair) but even incremental fitting on
   the frozen deg-1+2 residual keeps their weights at exactly zero (val-gated, both
   encodings): they are products of already-captured characters, informationally spanned.
   (The flat joint refit degrades test 1.103 → 1.192 — heavy-but-redundant features +
   flat refit = overfit.) The shipped deg-1+2 model is complete: every degree below it
   certified and used, every degree above certified and provably spanned.
1. **CORRECTED — the exact degree-1 spectrum has hundreds of heavy characters** (`spectrum-deg1`,
   one GEMM, no search): LSH top norm 0.048, 217 chars ≥ 0.01, mass 0.174 vs random codes
   0.044 / 171 / 0.124 — all on the last token, certifiable at τ ≤ 0.09. The tree's earlier
   "noise ceiling 0.006, 1/√m" was FRONTIER BURIAL: in the singleton-suffix region all
   bucket weights are provably equal, the width-bounded frontier tie-breaks arbitrarily,
   and each single-bit leaf gets exactly one chance to be carried. Evaluate the complete
   deg-1 basis exactly; use the tree only for degree ≥ 2 (the campaign's add_deg1 lesson).
1b. **The burial is the ESTIMATOR's, not dataset GL's** (`dataset_gl_csamp`, stage `csamp`):
   the group-by-SQUARE keeps the diagonal Σ‖f‖² — an S-independent mass that ties every
   bucket. The paper's paired estimator (lem:qary-kv-estimator) shares the real context
   AND the un-split continuation L_k between the two draws and excludes the diagonal; on a
   flat table a valid pair is two fills of one fiber agreeing on every un-split coordinate
   (value-matched = cache-forked, by the AR factorization). Fiber-only pairing (independent
   L_k) is the "cancellation-prone pooled quantity" the theory excludes — test-pinned
   counterexample in the suite.
1c. **But the offline flat-file bit-tree STILL floors at unigram** (empirical, fill3 tau=0.1,
   M4000): it is NOT collision-starved (min 7782 pairs/level) — it TIE-SATURATES. The deep
   399-bit descent fills the width-512 frontier with high-degree masks whose paired ψ are all
   ~0.0235 (point-mass inflation: peaked fibers make χ_S(i)χ_S(j)=1 for all S, a per-fiber
   S-independent mass), burying the true deg-1 signal; the fit floors EXACTLY at unigram
   (val_kl = unigram_kl = 1.8287, "found nothing"). Lesson (Lev): floor-at-unigram is a bug
   signal, not a finding — the deep bit-tree is the wrong tool below deg ~3 (3rd confirmation).
1d. **The collision-FREE fix — the ONLINE ORACLE, VALIDATED POSITIVE** (`oracle_deg1_psi`,
   stages `oracle-data`/`oracle`/`oracle-sweep`): fork the rollout by fixing the real prefix +
   older filled tokens as a shared STUB drawn once per fiber, then resample the split token G
   times. Every fiber then has G(G−1) guaranteed pairs (no waiting for flat-file collisions),
   and per-token deg-1 avoids the deep-tree tie-saturation entirely. Prints a KL LADDER (top-K
   leaves → held-out KL) so progress is visible live. **fill3, newest token, M1500 G16,
   fiber-disjoint held-out**: LSH sparse KL **1.4859 (−0.368 nats vs unigram 1.8544)** vs ctrl
   1.6675 (−0.187) vs idbits 1.6857 (−0.169) — LSH−ctrl gap **0.182 nats, identical to the exact
   enumeration frame**. The paper's native estimator reproduces both the recovery and the LSH
   advantage where the flat tree floored. **p_back sweep** (deg-1 at every filled position): gain
   decays with distance from the prediction — LSH −0.368/−0.059/≈0 at newest/middle/oldest, LSH
   ~2× the random-code gain at every position with signal, ≈0 for all encodings at the oldest
   (the oracle reports null where there is none). **Clean estimator** (`clean=True`) subtracts the
   b-independent same-token collision floor (a peaked fiber makes χ_S(i)χ_S(j)=1 for all single
   bits) → LSH certifies all 133 bits, random codes only ~67: LSH's deg-1 content is real,
   random's is mostly floor.
1e. **Efficient cache fork**: `_fork_and_label` forwards each prefix ONCE and branches G via
   `Cache.reorder_cache(beam_idx)` — NOT `batch_repeat_interleave` (KV-only); reorder_cache
   dispatches to Qwen3.5's `LinearAttentionCacheLayerMixin` (conv+recurrent states) too. A
   fail-loud self-check asserts a forked branch matches a fresh forward (passed, max slot err
   ~6e-3). Ops fixed this run: batch 32→128 (A10G starvation), `logits_to_keep=1` (the 8.07GB =
   batch×seq×248k-vocab full-logits OOM).
2. **The degree-1 aggregate is decisively encoding-dependent** (M=8000, held-out slot KL
   vs unigram 1.6303): LSH **1.4915 (−0.139 nats)**, id-bits 1.5975 (−0.033), random codes
   1.6303 (exactly zero — never improves on init).
3. **Frontier characters add nothing beyond degree-1** (all encodings) — consistent with 1.
4. **Wider codes are not a free win**: 517-bit codes extract nothing where 133-bit codes
   extract −0.139 (replicated in the older bin-lsh frame too). B≈133 is the operating point.
5. **Full-context deg-1 floors at unigram for all encodings**: prefix bits identify fibers,
   so full-context linear students memorize and fiber-disjoint validation rejects them (the
   campaign's w64-additive failure, reproduced). Filled-span is the honest linear domain;
   full context needs staged/backoff fitting.
6. **The staged backoff student is the best of the arc** (`fit-staged`): LSH **1.1232
   (−0.507 nats, 3 blocks)** vs random 1.2849 (−0.345) vs flat 1.4915. Per-block fitting
   extracts 3.7× the flat fit; the encoding gap is 0.162 nats at equal capacity; the val
   gate localizes honest signal to the last 3–4 tokens.
7c. **Two-regime law (transformer attribution, 40k, controlled)**: transformer + deg-1
   masks — LSH 21.2% top-1, random-codes 20.9%, baseline 19.3%: the feature lift is
   generic capacity; encoding geometry matters only in BOTTLENECKED models (0.18–0.21
   nats there), never in models that see raw tokens through an embedding table.
7b. **Interactions are dead at these scales** (3rd confirmation, strongest form):
   hereditary deg-2 pairs among individually-proven bits (val-gated, 40k real contexts)
   rejected for both encodings. Degree-1 saturates the parity class; beyond it, use a
   transformer.
7a. **Data-scaling law on the student** (20k → 40k contexts, clean test): LSH 1.4001 →
   1.3583, random 1.5819 → 1.5428 — ≈ −0.045 nats per doubling, both encodings; the
   encoding gap (~0.18 nats) is scale-stable; accepted depth stays 3 blocks (deeper
   context needs interaction features or a transformer, not more data).
7. **On REAL 256-token contexts** (`fit-staged-real`, cached qwen35_argl data; CLEAN
   test_n5000 numbers): LSH **test KL 1.4001 (−0.470 nats, top-1 61.9%)** vs random
   1.5819 (−0.288) vs unigram 1.8702 — encoding gap **0.182 nats**, gate-uncontaminated
   (gated-val ≈ identical, so no optimism). Localization robust (patience 8 accepts the
   same 3 blocks). This is the student-input-layer recipe: 133-bit LSH codes + staged
   val-gated degree-1 blocks.

## Gotchas (each cost a run today; all are fixed and test-pinned)

- Pure-τ over n ≫ log₂(m) bits provably keeps all 2^kk prefixes while suffix groups are
  singletons — the ordered bounded frontier is required (fast_gl's design).
- Characters on dataset-fixed bits are per-fiber signs that square away in W — only the
  model-FILLED span is searchable; "search more context" = generate longer fills.
- Uncentered targets admit bias-artifact characters (large ‖f̂‖, zero conditional content).
- LSH bits constant on-data create equivalence classes that flood the frontier — dedup by
  hashing χ columns.
- fp64 `index_add` on A10G is ~1/32 throughput — fp32 buckets.
- Peaked soft labels + many features diverge at fixed lr and the best-val floor masks it as
  "no signal" — fits report `improved`; lr scales with 1/√features.
- Modal: launch via `setsid nohup` (detach only preserves the last-triggered function on
  client death); generation is sharded (preemption costs one shard); check
  `modal container list`, not `app list`, before declaring a run dead.
