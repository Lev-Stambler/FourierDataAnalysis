# Interaction screening = functional ANOVA: the long-context / low-degree primitive

## Why (the access-model phase diagram)

CSAMP / context-conditioning GL conditions on the **suffix** (the `w-d` un-split coordinates) and needs it
to repeat — the *Blindness lemma*'s birthday horizon `k ≈ n - 2 log₂ m`. So it only reaches a degree-`d`
character when `n - d ≲ log_V |D|` (**short context, high degree**). Long contexts have unique suffixes →
GL is information-free → no recovery. The literature says this is fundamental, not a tuning bug: recovering a
pure degree-`k` structure on unknown support from **random samples** is SQ-lower-bounded at `n^Ω(k)` and
LSPN-hard to beat (best known `n^{~0.7k}`, Mossel–O'Donnell–Servedio). The extra power that beats it is
**subcube conditioning** (learns a `k`-junta in `2^O(k)`, Chen et al. 2021) — which is exactly what dataset
CSAMP *is*, available only where `D` contains the conditioned subcube.

**The complementary primitive** conditions on the `d` **interacting positions** instead of the suffix. For a
set `S`, the conditional-mean energy `ν_S = E_{x_S}[(E[f|x_S])²] = Σ_{U⊆S} f̂_D(U)²` (product `D`), and the
pure interaction `f̂_D(S)² = Σ_{U⊆S}(−1)^{|S|−|U|} ν_U` (Möbius). This needs only the `2^{|S|}`-cell
`S`-subcube to repeat (`|S| ≲ log_V|D|`) — **independent of `n`**: it reaches sparse high-order in *long*
contexts, capped at low degree. It **is functional ANOVA / Hoeffding–Sobol** in the dataset-Fourier basis,
and it is provably the SQ-optimal reach from samples (not a breakthrough — that's the honest framing).

The two primitives tile the (degree `d`, dimension `n`) plane by `min(d, n−d)`; the `min(d,n−d) ≳ log_V|D|`
corner (high degree **and** long context) is SQ-hard, no method. Paper: `sections/gl.typ`,
"Interaction Screening: Conditioning on the Interacting Set."

## Method (`fda_exp/interaction_gl.py`, `interaction_predict.py`)

- `cond_energy` / `cond_collision` — `ν_S` by group-by on `x_S` with the **U-statistic** self-pair
  correction (drops singleton cells = the diagonal that swamps the suffix version); `cond_collision` does all
  next-symbol classes in one group-by (`E_{x_S}[Σ_t p_t(x_S)²]`, the Gini/collision predictability).
- `interaction_energy` — Möbius over subsets → pure `I_S`. `energy_by_degree` — Σ`I_S` per degree.
- `anova_scores` — held-out predictor: reconstruct `P(y=t|x) ≈ Σ_{S∈chosen} g_{S,t}(x_S)` (train conditional
  means, Möbius, looked up on test) → argmax. Degree-≤2 vs +heavy-degree-3, no character materialization.
- `recover_chars_on` — the heavy Householder characters on a set via `qary_gl.qary_coeffs_at`.
- Soft: `cond_energy(shrink=λ)` (empirical-Bayes) fuzzes the `V^d ≲ |D|` cap; kernel/embedding is the
  discussion-level bridge to representation learning.

Tests (`tests/test_interaction_gl.py`, all green): pure degree-3 + blindness (its triple is the top
interaction, pairs ≈0); matches brute ANOVA on a full cube; **recovers a degree-3 at w=50 where CSAMP blows
up / recovers nothing** — the crux that this primitive reaches where CSAMP can't.

## Results — the phase diagram, empirically

**The `V^d ≲ |D|` boundary is visible in held-out degree-3 collision energy** (positive = generalizes):

| data | V | w | held-out deg-3 collision | held-out top-1: deg≤2 → +deg-3 |
|------|---|---|--------------------------|--------------------------------|
| char next-char (dense) | 27 | 10 | **+1.86** (generalizes) | **0.526 → 0.560** (helps) |
| word next-token (sparser) | 32 | 6 | **−0.27** (overfits) | 0.381 → 0.298 (hurts) |

(Small local configs; the Modal H100 sweep — char w=24 blind-full, char w=96 heredity-pruned, word V=64 w=12,
plus the Poelwijk CSAMP-corner line — is in `/cache/interactions_results.txt` via `modal_gl.py::interactions`
→ `show`.) So **interaction-screening finds usable degree-3 high-order over contexts CSAMP cannot reach**,
exactly where the interacting set is densely sampled (char); where it isn't (word), degree-3 overfits — the
ANOVA/SQ ceiling. The genuinely *high-degree* win stays in the CSAMP corner (Poelwijk V=2 w=13, 0.82 vs
degree-≤2 0.74; `qary_gl_predict.run_poelwijk`).

## Bottom line

Dataset-GL is best understood as an **access model** — subcube conditioning over the empirical support —
sitting strictly above random samples (which are SQ-capped) and below membership queries. Its two conditioning
primitives are complementary: **context-conditioning** (CSAMP) for high-degree/short/repeating contexts, and
**interaction screening** (= ANOVA) for low-degree/long contexts. High-degree-and-long-context is provably
out of reach from samples. This is the honest resolution of the "does language have a high-order win" hunt:
yes, low-order-degree (degree-3, character-level) high-order is real, generalizes, and helps — and the
primitive that reaches it is set-conditioning, not the suffix-conditioning CSAMP tree.
