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

## Run

```bash
uv sync && uv run pytest                    # 12 tests, incl. brute-force anchor
# launch clients via setsid so they outlive tool timeouts; functions self-commit
uv run modal run --detach qary_lsh_dataset_gl.py --stage data --fill-len 61 --m-fibers 8000
uv run modal run --detach qary_lsh_dataset_gl.py --stage search --tau 0.02 --fill-len 61 \
    --m-fibers 8000 --max-width 64 --encoding parallel
uv run modal run --detach qary_lsh_dataset_gl.py --stage fit-deg1 --fill-len 61 --m-fibers 8000
```

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
   AND the un-split continuation between the two draws and excludes the diagonal; on a
   flat table a valid pair is two fills of one fiber agreeing on every un-split coordinate
   (value-matched = cache-forked, by the AR factorization). Singleton cells cancel exactly,
   so there is no noise mass — but the flat file's evidence is its per-level collision
   profile (returned as `pair_profile`; near zero at early levels for fill61, real for
   fill3). Pairing on the fiber alone (independent L_k) is the "cancellation-prone pooled
   quantity" the theory excludes — test-pinned counterexample in the suite. The
   no-collisions-needed version is the ONLINE oracle (fork the KV cache at the split
   boundary), not yet implemented.
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
