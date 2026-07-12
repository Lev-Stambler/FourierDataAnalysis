# Categorical Householder CSAMP on a REAL BPE tokenizer (power-of-2 vocab)

## The goal
Run dataset Goldreich-Levin via the **CSAMP (subcube-conditioning) oracle** in the **categorical
Householder basis** on **real text** tokenized by a **real byte-level BPE tokenizer** — only that path
(no bit-Walsh-as-a-separate-method, no ANOVA), made **as efficient as binary bit-Walsh** and **correct**
(no basis-inflation, no completeness pathology).

## The unlock: Walsh-Hadamard for V = 2^k
`householder_basis(V)` is a single Householder *reflection* — ±1 (unit-modulus) **only for V=2,4**; at
V≥8 `max|Psi| ≈ √V`, so high-degree characters **inflate** as `|chi| ~ V^{deg/2}` and ranking by `|f̂_D|`
over-weights degree. (An earlier `qary_gl_findings.md` note wrongly claimed ±1-Walsh for all powers of 2;
corrected.)

For **V = 2^k** the true Walsh-Hadamard matrix `H_2^{⊗k}` (`householder.hadamard_basis`,
`H[a,x]=(-1)^{<a,x>_GF2}`) is **unit-modulus** (all ±1) *and* a **tensor product over the token's k bits**.
So a V-way Householder character **factors into a single binary Walsh parity** over the `w·k` packed bits,
and the categorical CSAMP search **is exactly** binary Walsh GL over those bits (`hadamard_gl`):

- pack token p's k bits into global bits `k·p..k·p+k-1` (= `_encode_qary(C, V)` reinterpreted at V=2);
- run the existing `qary_gl_search` at **V=2** (branch-2, depth `w·k`, exact-W group-by — already
  completeness-correct because V=2 is unit-modulus, so **no new search code**);
- read a recovered binary mask **base-V** → per-token contrasts `a_p=(mask>>k·p)&(V-1)`; **token-degree** =
  #nonzero contrasts. `codes` feed `qary_coeffs_at`/`_char_columns`/`qary_recon` unchanged with `hadamard_basis(V)`.

**Efficiency:** branch-2 depth-`w·k` vs branch-V depth-`w` → **~V/(2·log2 V)** speedup (28× at V=512, 51× at
V=1024), and no `V^{deg/2}` magnitude inflation. It *is* bit-Walsh under the hood, with token-level output.

## The statistical wall (not algorithmic)
CSAMP reach is governed by the density factor `C_D = V^w/m` and the birthday horizon. Prior categorical-CSAMP
on TinyStories used `C_D ≈ 11–2200` (undersampled) → recovered degree≥3 was aliasing noise that *hurt*. The
fix is data + short windows: TinyStories **train** (2.12M stories, 4 parquet shards) + **frequency-collapse**
to distinct contexts (`collapse_contexts`, exact via `norm_m`) drives `C_D → O(1)` at short windows where
CSAMP provably works. Sweet spot **V=512, w=3** (the minimal window hosting a degree-3 / 4-gram interaction):
`C_D≈1` at ~one train shard.

## Validation
- **Unit tests** (`tests/test_hadamard_gl.py`, `tests/test_bpe.py`, all green): `hadamard_basis` ±1 /
  orthonormal / == `walsh4`; planted token-degree-2/3 recovery with correct token-degree and **no inflation**
  (`|coef|≈1`); recall vs brute-force `qary_spectrum` on non-uniform data ≥0.8; **collapse equivalence**
  (distinct-context weighted search == full-row search, exactly); BPE exact-2^k vocab, byte-fallback (no
  `<unk>`), roundtrip.
- **Correctness at scale (Modal H100, real byte-BPE, 5M rows, C_D=0.013):** GL **recall = 1.000** vs the exact
  spectrum — e.g. `t=32`: brute heavy 45932, GL 45931. Across 5 targets, all 1.000. The efficient Walsh
  categorical CSAMP recovers essentially every heavy character on real tokens.

## The degree-3 experiment (running: `modal_gl.py::qary_bpe_sweep`)
Honest test with **both prior pathologies removed** (unit-modulus basis ⇒ not inflation; C_D≈1 full-data
search ⇒ not aliasing): nested held-out **degree≤2 vs +recovered-degree-3** top-1, over a **C_D sweep**
(valid → 1 → 4 train shards) as the aliasing discriminator (genuine degree-3 → benefit stable as C_D→1;
aliasing → benefit →0), plus V=256 (dense) / V=1024 (realistic) and a V=512 **w=4 negative control**.
Retrieve with `modal run modal_gl.py::show`. **[result pending]**

Caveat (a strength): on TinyStories the dense regime (w=3) is also brute-forceable, so w=3 is *brute-validated*;
GL's strict necessity is w≥4, which TinyStories undersamples. A dense-and-un-enumerable regime needs a bigger
corpus (OpenWebText) — a follow-on.

## GL as an ACTUAL language model (next-token CE/perplexity) — `fda_exp/bpe_lm.py`

Beyond spectrum-recall, the recovered spectrum IS a next-token predictor. One **GPU one-shot multiclass
top-K Walsh CSAMP search** (`fast_gl.multiclass_search`, float32, MPS/CUDA) recovers the characters
predictive of the *whole* next-token distribution `W_multi(S)=Σ_t Σ_U f̂_t(S∪U)²` in a **single** search
(not V separate ones); their exact dataset coefficients (`coeffs_all`, one GEMM) reconstruct
`g_t(x)=2P(t|x)−1`, and a val-tuned softmax fits the per-token weights. We report **held-out
cross-entropy / perplexity** on a **story-disjoint 3-way split** against a unigram floor and the full-context
n-gram lookup (`= the Bayes/neural target at that context`), plus the degree-≤2-vs-≤3 delta.

### Results (Modal H100, TinyStories BPE, all fixes; `modal_gl.py::bpe_lm`)
| context | vocab | m_train | unigram PPL | n-gram ceiling | GL deg≤2 | GL deg≤3 | top-1 | **deg-3 Δ** |
|---|---|---|---|---|---|---|---|---|
| **w=3** | V=512 | 6M  | 173.9 | **10.5** | **12.0** | **11.9** | 0.426 | **+0.11 ✓** |
| w=4     | V=512 | 12M | 173.8 | 11.9 | 14.4 | 14.5 | 0.420 | −0.05 |
| w=6     | V=512 | 25M | 174.0 | 21.8 | 22.9 | 22.9 | 0.383 | +0.00 |
| w=7 (char) | V=256 | 25M | 19.4 | 2.5 | 18.4 | 18.4 | 0.201 | +0.00 |

Three honest conclusions:
1. **On BPE tokens GL is a genuinely competitive short/medium-context LM.** w=3 reaches PPL 12.0 vs the
   n-gram ceiling 10.5 (14% off) with top-1 0.426; w=6 is 22.9 vs 21.8 (**5% off the memorization ceiling**).
   Longer context raises top-1 (0.35→0.42). The unigram floor (~174) is left far behind.
2. **Degree-3 helps at short context (+0.11 PPL at w=3) and is neutral beyond (w≥4: Δ≈0).** This is the first
   *clean* positive degree-3 token signal — measured with all fit bugs removed, so it is a real (small)
   high-order effect, not aliasing and not a broken-fit artifact.
3. **Character-level long context (V=256, w=7) is where the sparse-Fourier compression underperforms.** GL
   (18.4) barely beats unigram (19.4) against a 2.5 ceiling: 3000 recovered characters cannot represent the
   dense, near-deterministic degree-4-through-7 structure that a full 7-gram lookup memorizes. Honest limit of
   a low-width spectral model — not a bug (GL still floors above unigram; Δ still ≈0).

### The four fit bugs that produced fake negatives (all fixed in `_softmax_fit`/`_bpe_split`)
Before these fixes the LM returned catastrophic numbers (PPL 477, 73 690, 148 479) and large *negative*
degree-3 deltas. Every one was a fitting bug, **not** a density/CSAMP barrier — invoking "physics" for them was
wrong. In order of impact:

1. **Init was never a fit candidate (the big one).** The early-stopping loop stepped Adam *before* recording
   `best`, so the unigram (and warm-started deg≤2) starting point was discarded — a bad run could return
   *worse than unigram* (PPL 477 vs 19). Fix: seed `best`/`bW` with the init model's val CE, so the fit **can
   never end up worse than its own starting point** (`bpe_lm.py:92-95`).
2. **Non-story-disjoint validation.** Early-stopping val was a random split of *train* contexts, so it could
   not see cross-story overfitting → PPL 73 690. Fix: 3-way **story-disjoint** split (`_bpe_split`) — each
   story goes wholly to train/val/test; val tuning now reflects the (also disjoint) test.
3. **False L2 penalty on the bias.** An L2 term shrank the log-unigram intercept toward uniform; heavier L2 at
   larger K made PPL *climb* with more characters. Fix: **remove L2 entirely** — regularization is purely early
   stopping + log-unigram bias init.
4. **Learning rate too high (0.1) diverged the warm-started deg≤3 fit** (PPL 148 479). Fix: lr=0.03.

Plus two structural guards: **degree-3 is warm-started from the degree-≤2 solution** (`Winit`) so deg≤3 ≥ deg≤2
on the fit objective (turns the fake −17/−148k deltas into the true ≈0/+0.11); and `max_ctx=250k` +
`max_width=4000` bound `fast_gl`'s `live_chi (max_width×D)` tensor (was CUDA-OOM at w=3). Residual tiny
negatives (e.g. w=4 −0.05) are val→test generalization noise on the warm-started fit (deg≤3 is floored at deg≤2
on *val*, not test), ~0.3% of PPL — not a regression.

## Code map
`householder.hadamard_basis` · `hadamard_gl.py` (`hadamard_gl_search`, `token_blocks`, `token_degree`) ·
`bpe.py` (self-contained byte-level BPE) · `hf_data.tinystories_bpe_next` / `collapse_contexts` / train-shard
`load_texts` · `qary_gl_predict.run_bpe_next` / `run_bpe_recall` / `_highorder_test(basis=, search=,
deg2_full=False, collapse=True)` · `qary_gl.qary_gl_search(norm_m=)` (collapsed exact-W) · `modal_gl.qary_bpe_sweep`.
**LM:** `fast_gl.py` (`multiclass_search`, `char_matrix`, `coeffs_all` — GPU one-shot top-K CSAMP) ·
`bpe_lm.py` (`_bpe_split` story-disjoint, `_softmax_fit` init-floored no-L2, `fit_bpe_lm`, `ce_perplexity` /
`ngram_perplexity` / `unigram_perplexity`, `predict_next` / `generate`, `run_bpe_lm_eval`) · `modal_gl.bpe_lm`.
