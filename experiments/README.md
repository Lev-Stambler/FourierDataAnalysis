# Experiments — Fourier Analysis on a Dataset

## Learned Walsh noun distillation (no Dataset-GL)

`modal_fourier_noun.py` asks pinned `Qwen/Qwen3.5-0.8B` for contextual
noun/not-noun labels, puts the target and nearest neighbors in independent
fixed-width fields, converts every Qwen token to a distinct collision-repaired
32-bit embedding-LSH code, and learns `M` exact Walsh parities and scalar
coefficients with AdamW. Teacher labels are cached, so repeated H100 training
runs do not reload Qwen. A hard top-k mask uses the multilinear product STE;
its high-LR warmup+cosine schedule reaches zero at mask freeze, after which the
remaining full cosine schedule fits coefficients and bias.

Here, a `mask` is not an attention or padding mask. For Walsh term `j`, it is
the sparse binary selector `m_j` choosing 1--8 input bits from the 4,096-bit
example encoding; the resulting Fourier character is
`(-1)^<m_j,x>`. The STE changes those selected coordinates during the first
15% of training. A fixed-random-mask ablation (`--mask-discovery-fraction 0`)
reached only 88.73% test agreement and 49.13% teacher-positive recall, versus
roughly 90.5--90.9% agreement for individual learned-mask models. Consequently,
mask discovery remains enabled briefly and is then frozen.

Continuous mask search is available with
`--mask-discovery-fraction 1 --mask-schedule global_cosine`; character and
coefficient learning rates are independent (`--mask-lr` and
`--coefficient-lr`). On the 192-slot/6,144-bit layout, 1,500-step no-freeze
runs reached 90.19% test agreement at mask LR 1.0 and 90.12% at mask LR 5.0.
The LR-1.0 run improved teacher-positive recall to 60.88%, but neither beat the
best briefly-discovered/frozen member. Both sustained 100% median active H100
utilization and healthy gradient norms. Thus no-freeze remains an ablation,
while brief discovery remains the agreement-oriented default.
Variable-degree top-k hardening sorts the top-eight candidates before taking
each row's requested degree, so degree-1 through degree-7 characters always
use their actual highest-scoring coordinates.

### Exact character uniqueness and function-preserving repair

Token-code uniqueness and Fourier-character uniqueness are separate
invariants. Vocabulary encoding first makes every token's 32-bit LSH code
distinct. During training, however, continuously moving top-k masks can make
two character slots select the same sorted support. Equal supports compute the
same Walsh function and waste capacity even when their coefficient values are
different.

At initialization and after each validation interval, optional duplicate-mask
repair groups characters by their complete sorted, padded support. For a group
with support `S` and coefficients `c_1, ..., c_k`, it applies the exact
identity

```text
c_1 chi_S(x) + ... + c_k chi_S(x) = (c_1 + ... + c_k) chi_S(x).
```

For example, if three slots independently converge to support `{17, 91}` with
coefficients `+0.8`, `-0.3`, and `+0.1`, they are only one effective feature:
their combined contribution is `+0.6 chi_{17,91}(x)`. Repair retains `+0.6`
on one slot, zeros the other two slots, and only then gives those two empty
slots new supports. No prediction changes during that operation.

The member with the largest absolute coefficient is the keeper. Its
coefficient becomes the group sum; every other member's coefficient becomes
zero. Only after this merge does repair relocate the zero-valued rows. It
tries score-ranked runner-up coordinates first, replacing one currently
selected coordinate, and accepts a proposal only when its full support is
globally unused. If the local candidates are exhausted, a deterministic scan
of all input coordinates supplies a replacement. Each accepted support is
reserved immediately, preventing repaired rows from colliding with each other.

Finally, repair clears row-shaped AdamW moments for the keeper and relocated
rows: the old moments refer to pre-merge coefficients or pre-swap mask scores.
It then asserts that the number of unique supports equals the number of
character slots. Because every relocated row has coefficient zero and its old
contribution was transferred to the keeper, repair leaves model logits
unchanged except for floating-point summation noise. A Modal H100 smoke test
measured a maximum logit delta of `1.49e-8` while repairing eight duplicates
and ending with 256/256 unique characters.

This operation guarantees exact uniqueness, not broad diversity: supports
that differ by one coordinate are distinct and will not be repaired. Overlap
penalties or other diversity objectives are a separate experiment. Export
also performs a final defensive global group-and-sum before serialization, so
the deployed sparse artifact cannot double-count an identical character.

The repair telemetry is intentionally explicit. `unique_characters_before`
is the effective character-bank capacity immediately before one repair;
`duplicates_repaired` is the number of redundant rows relocated in that one
event, not a cumulative counter; and `capacity_recovered_fraction` divides
that count by `M`. The corresponding `*_after_repair` count must equal `M` or
training aborts. These metrics say nothing about pairwise support overlap:
100% exact uniqueness can still contain many near-duplicate characters.

Teacher sharpening is a log-odds multiplier (`--teacher-sharpness`), not the
usual softening temperature: 2.0 is equivalent to temperature 0.5. The
original cached teacher remains the canonical evaluation distribution. Each
run exports a constrained-agreement champion and an uncalibrated minimum-KL
champion, and reports KL, MAE/RMSE, p50/p90/p95/p99 absolute error,
confidence-bucketed MAE, probability/logit centered cosine, probability R²,
and hard mutual information normalized by teacher-label entropy.

```bash
uv run pytest tests/test_fourier_noun.py -q
uv run modal run modal_fourier_noun.py --stage pilot
uv run modal run modal_fourier_noun.py --stage train --m 131072
uv run modal run modal_fourier_noun.py --stage benchmark --m 131072
uv run modal run modal_fourier_noun.py --stage web-pilot \
  --train-n 1000000 --val-n 8192 --test-n 8192 \
  --m 262144 --steps 4000 --batch-size 16384
uv run modal run modal_fourier_noun.py --stage v3-pilot \
  --train-n 1000000 --val-n 8192 --test-n 8192 --student-length 128 \
  --m 131072 --batch-size 16384 --extra-train-repeat 10
uv run modal run modal_fourier_noun.py --stage v4-pilot \
  --train-n 1000000 --val-n 8192 --test-n 8192 --student-length 192 \
  --m 131072 --steps 1500 --batch-size 16384 --extra-train-repeat 10 \
  --mask-lr 1.0 --coefficient-lr 0.03 \
  --mask-discovery-fraction 1 --mask-schedule global_cosine
uv run modal run modal_fourier_noun.py --stage sharpness-screen \
  --train-n 1000000 --student-length 128 --lsh-bits 32 \
  --batch-size 16384 --extra-train-repeat 10
uv run modal run modal_fourier_noun.py --stage sharpness-full \
  --train-n 1000000 --student-length 128 --lsh-bits 32 \
  --batch-size 16384 --extra-train-repeat 10
uv run modal run modal_fourier_noun.py --stage kl-ensemble \
  --train-n 1000000 --student-length 128 --lsh-bits 32 \
  --model-paths /cache/fourier_noun/models/member-a.npz,/cache/fourier_noun/models/member-b.npz \
  --max-terms 450000
```

Artifacts use `fda-cache` under `/cache/fourier_noun`; W&B project:
`fda-fourier-noun`. Pilot defaults are 90,000/8,192/8,192 balanced EWT
train/dev/test targets, 64 token slots × 32 collision-repaired unique LSH bits,
131,072 fixed-context Fourier terms, batch 16,384, and 1,000 warmup+cosine
steps. The fast schedule uses 20 coefficient-warmup steps, 15 mask-warmup
steps, mask freeze at step 150, validation every 25 steps, and patience 12; its
measured run stopped at step 750 in 235 seconds with healthy gradient norms.
The measured large-data operating point combines one million unique
FineWeb-Edu targets with 90,000 balanced EWT targets (EWT gets 10× sampling
weight), and adds fixed lowercase/prefix/suffix/casing fields. A validation-
weighted three-support hybrid Fourier sum with a 70% validation positive-recall
constraint reaches 91.27% held-out teacher agreement, 66.50% positive recall,
and 95.56% negative recall. It exports to a standalone 7,632,448-byte NPZ:
209.63× smaller than 1.6 GB of teacher weights. The `web-pilot` path streams
unique `(document, token-index)` targets while retaining balanced EWT
validation and test splits. For distribution fidelity, `kl-ensemble` fits a
nonnegative validation-BCE logit stack, deduplicates identical characters
globally, prunes by weighted coefficient magnitude, and refits its final affine
calibration. Serialization stores degree-8 character lengths as `uint8`, packs
the 4,096-bit input indices into 12 bits, and block-scales FP16 coefficients;
the loader remains backward compatible with the original CSR NPZ format. The
selected 450,000-character, sharpness-diverse ensemble exports to 6,153,776
bytes (260.00× compression) and reaches test KL 0.01309, probability MAE
0.05379, RMSE 0.07343, and R² 0.7994. Artifact creation fails if the exact
serialized size exceeds the 16 MB budget, and reported metrics are recomputed
from the deployed quantized representation.

`v2-pilot` reuses cached schema-6 teacher probabilities without
rerunning Qwen, and `v3-pilot` appends 64 prompt-context token slots while
reusing the same cached teacher probabilities. Exact sparse inference is
vectorized over the degree-8 CSR masks. Every vocabulary token retains a
distinct collision-repaired LSH code.

Tests the paper's two make-or-break empirical claims (small-`n` exact track, `n ≲ 18`, so there is genuine ground truth):

- **E2 (context profile) — the GL metric.** For Goldreich–Levin the quantities that matter are the **output** (# heavy dataset coefficients = the list `L`) and the **search overhead** `N_max / output` (how far above the true answer the tree search wanders). Overhead ≈1 = output-sensitive (context repetition helps); large overhead = blindness. Coefficient *degree is irrelevant to GL*. `fda_exp/context_profile.py`.
- **E5 (degree) — a *separate* question.** Whether a trained model is low-degree *over the data* tests the low-degree **learning** theorem (`thm:learning-low-degree`), **not** the GL headline. Reported for completeness; a "high on-dataset degree" does not bear on GL. `fda_exp/degree.py`.

Controls make E2/E5 a *test*, not a demo: `uniform_random` (blindness → `N_k → 2^k`), `subcube` (output-sensitive → `N_k = 2^{|K∩[k]|}`). `markov` is a self-contained structured sequence corpus (repeating contiguous contexts).

## Run

```bash
cd experiments
uv sync
uv run pytest              # correctness gate: tests encode the paper's theorems
uv run python -m fda_exp.cli
```

Outputs land in `experiments/outputs/`: `results.json`, `fig_e2_context_profile.png`, `fig_e5_degree.png`. Override via Hydra, e.g. `uv run python -m fda_exp.cli tau=0.2 orders='[contiguous,random,greedy]'`.

### Next-token vibe check (KM over the dataset)

```bash
uv run python -m fda_exp.predict
```

Predicts the next token on the tiny-stories task from only the **top-K heavy dataset coefficients** (the frame reconstruction `g = C_D^{-1} Σ_{top-K} f̂_D(S) χ_S`), with a train/test split over distinct contexts. Writes `outputs/fig_predict_topk.png`. Shows the key point: the **sparse** (top-K) reconstruction generalizes to held-out contexts, while using **all** coefficients is exactly the lift (`f` on `D`, `0` off it) and generalizes to nothing.

## What the tests check (= the theorems)

- Mass Identity `Σ_S f̂_D(S)² = C_D·E_D[f²]`; normalized Parseval sums to `E_D[f²]`.
- Level-mass `R_k = 2^k·c_k/|D|` for `f≡1`; termination `R_n = C_D·E_D[f²]`.
- Subcube regime: `R_k = N_k = 2^{|K∩[k]|}` (output-sensitive).
- Blindness: singleton fibers ⇒ `Ψ` flat ⇒ `N_k = 2^k`.

## Scope / honesty

Exact enumeration caps this at `n ≲ 18–20`. Larger corpora (real n-gram indices, images) need the sampling estimators for `Ψ` (SAMP+CSAMP) and would move to a remote run — not implemented here. `markov` is a *controlled* structured corpus, not a real dataset; swap in UCI (`pip install ucimlrepo`) or a real binarized corpus for the camera-ready measurement.
