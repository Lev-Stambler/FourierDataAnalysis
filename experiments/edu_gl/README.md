# edu-GL: dataset-GL compression of a scalar classifier

Compress `HuggingFaceFW/fineweb-edu-classifier` (109.5M params, scalar 0–5
educational score) into a sparse degree-≤2 Fourier model over encoded token
bits. Scalar target = the pivot away from the vector-valued targets
(next-token distributions, hidden states) that dogged earlier dataset-GL arcs.
All student coefficients are CALCULATED (closed-form deg-1 LS, exact deg-2
pair enumeration, matching-pursuit deflation) — never Adam/ridge.

v1 uses plain streamed FineWeb windows (no fibers/CSAMP; exact deg-1/2 needs
no conditional sampling). v2 (later): CSAMP completion pairs for deg-3+.

```bash
uv sync && uv run pytest                       # planted exact-recovery tests
# smoke
uv run modal run edu_gl.py --stage label --n-train 20000 --n-val 5000 --n-test 5000
uv run modal run edu_gl.py --stage fit --encoding lsh --n-train 20000 --n-val 5000 --n-test 5000
# full
uv run modal run --detach edu_gl.py --stage label
uv run modal run --detach edu_gl.py --stage fit --encoding all
uv run modal run edu_gl.py --stage ceilings
uv run modal run edu_gl.py --stage show
```

Encodings: `lsh` (sign-LSH on the classifier's own word embeddings), `ctrl`
(capacity-matched random bits), `tokid` (15-bit token ids). Ceilings (fitted,
allowed as references only): ridge + tiny MLP on mean-pooled embeddings.
Artifacts land on the `fda-cache` volume under `/cache/edu_gl`; W&B project
`fda-edu-gl`.

## Results (2026-07-15, w=64, 1M train / 25k val / 25k test)

Test R² vs the classifier score (val-selected rung = 50k pairs):

| model | deg-1 | deg-1 + 50k pairs | Spearman | size |
|---|---|---|---|---|
| lsh | 0.265 | **0.406** | 0.649 | 681 KB (643×) |
| ctrl | 0.125 | 0.307 | 0.583 | 681 KB |
| tokid | 0.066 | 0.111 (best @10k) | 0.349 | 141 KB |
| ridge ceiling (fitted) | — | 0.629 | 0.799 | 3 KB + emb |
| MLP ceiling (fitted) | — | 0.756 | 0.858 | 1 MB + emb |

- Deg-2 needs data: at 20k rows the pair noise ceiling sits above the real
  spectrum (every rung hurt); at 1M rows 50k calculated pairs take LSH from
  0.265 to 0.406. All encodings peak at ~50k then decline into the 2×floor
  noise tail (val selects the rung).
- LSH > random codes on a scalar semantic target: 2.1× the deg-1 R², +32%
  relative at the selected rung. Token-id bits are far behind throughout.
- Deg-3 (pair-anchored exact enumeration, 4096 anchors on the 50k residual):
  +0.002 test R² — floor-limited at 1M rows, not evidence of absent structure.
- Data scaling (2M rows, lsh): deg-1 flat at 0.267 (saturated); deg-2 test R²
  0.406 → **0.428** with the val-optimal rung moving 50k → 200k pairs (the
  halved floor unlocks more real pairs). ~+0.02 per doubling — real but
  diminishing; the 0.43 → 0.76 gap is beyond the deg-≤2-over-bits class.

## The basis was the bottleneck: token-table deg-1 (2026-07-16)

The ridge ceiling (0.629) is itself a TOKEN-degree-1 function; bit-deg-1
(0.267) was its 69-dim shadow. Computing degree-1 in the right alphabet —
one closed-form ridge over the full 30,522-entry per-token value table,
wd val-swept — changes everything:

| model | test R² | Spearman | size |
|---|---|---|---|
| bit-deg-1 (B=64) | 0.267 | 0.518 | 17 KB |
| B=128 bits + 200k pairs | 0.525 | — | 2.4 MB |
| **token table (q-ary deg-1)** | **0.708** | 0.839 | **61 KB (7,178×)** |
| token + 1k pairs (best) | 0.7105 | 0.840 | 70 KB |
| MLP ceiling (fitted) | 0.756 | 0.858 | ~800 KB + emb |

94% of the MLP's R² from a calculated 61 KB student. Pairs on the token
residual add +0.002 then decline at both B=64 and B=128 — degree-1 in token
space is essentially the whole low-order story; the remaining 0.05 to the
MLP is position effects + nonlinear pooling (2-bucket positional tables
being tested).

## v2: CSAMP dataset-GL pilot (edu_gl_csamp.py, 2026-07-15)

Qwen3.5-0.8B fiber forks (1000 fibers × 12 forks × 6 levels, pure sampling,
disjoint FineWeb dump), classifier-labeled; scalar pair-ψ + hereditary tree.
Pilot verdict: **no win over the exact stack at this target/scale.**
- Uncentered f (DC −0.78) reproduces the recorded pure-GL density inversion:
  every child passes the gate, top-ψ chars add +0.0000. The scalar-specific
  mechanism is the fiber-mean × density term — fixed by PER-FIBER centering
  inside pair_psi (the estimator now measures within-fiber structure).
- Centered ψ diagnostics: level-0 max 2.3e-5 ≈ the global spectrum scale →
  conditioning does not concentrate this target's spectrum; ψ noise (±1e-5 at
  132k pairs) leaves the pilot underpowered below that.
- With a noise-calibrated gate (τ=0.0063) the tree keeps 34k chars (deg ≤6),
  but deflating them on the 8k-row flat table adds nothing over deg-1
  (0.0205 → 0.0188). Fork windows are NOT extra refit data — they share
  spines within fibers (adding 72k of them dropped the base to −0.03).
- The edu score is dominated by low-degree global (bag-of-token) structure;
  plain-data exact enumeration with millions of rows is the stronger tool.
- Paper-faithful rerun (uncentered ψ sieve, deflate-all 50k on a 200k-row
  model-law table): lsh chars add exactly +0.0000 over their deg-1 base —
  the suffix-local conditional spectrum is intrinsically tiny here.

## Fitted arms: AdamW + STE vs calculated (2026-07-16, 2M rows)

Controlled challenge to the "coefficients CALCULATED, never Adam" rule.
Standard hygiene throughout: AdamW, linear warmup 500, grad clip, constant
lr, val early-stop with the init scored at step 0.

| arm | train R² | test R² | note |
|---|---|---|---|
| calculated (deg-1 + 200k pairs) | ~0.45 | **0.428** | the bar |
| AdamW-warm, same features | 0.553 | 0.428 = init | best point on the whole trajectory IS the calculated init |
| AdamW-cold | 0.490 | 0.378 | memorization gap |
| STE learned masks (16k, deg 1–10) | 0.334 | 0.310 | masks genuinely learn once gates start saturated (±8) |
| STE masks + CALCULATED coefs | — | **0.336** | calculated weights beat fitted weights on the same learned features |

STE lessons (5 rounds): θ-gradient ∝ c (warm-init the coefficients);
the log-space surrogate DIES at n=4416 unless gates start saturated at ±8
(exp(−Σ log|1−2m|) over ~2.2k set bits — width-realistic liveness test in
test_edu_gl.py); exclude θ from global grad-clip; two-timescale churn caps
fitted val at ~0.31.

## Planted certificate: dataset-GL provably works (edu_gl_csamp.py --stage planted)

Relabel the cached fork/flat tables (real Qwen windows, real fibers, real
density) with a KNOWN 5-char spectrum (deg 1–4, coef² 0.0225–0.09) and run
the identical tree + deflation. Result: every recovered char has ψ ≈ coef²
(density-inflated ≤20%), and recall scales with frontier width exactly as
the theorem's N-dependence demands:

| max_width | recovered | refit test R² |
|---|---|---|
| 512 | 3/5 | 0.756 |
| 4096 | 3/5 | 0.749 |
| 32768 | 4/5 (deg-4 char at prefix rank 45k) | 0.837 |
| τ=0.28 calibrated | 4/5 (393k chars still ≥ gate) | 0.837 |

The unrecovered char (weakest, oldest blocks) needs width ≳ 2.6e5: under a
skewed LM law a uniform-sparse plant spreads into a ~7M-char live spectrum
(the density term), so dataset-GL is provably CORRECT but its width/sample
budget is set by the law-basis spectrum, not the uniform-basis sparsity.
