# Experiments — Fourier Analysis on a Dataset

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
