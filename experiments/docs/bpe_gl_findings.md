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

## Code map
`householder.hadamard_basis` · `hadamard_gl.py` (`hadamard_gl_search`, `token_blocks`, `token_degree`) ·
`bpe.py` (self-contained byte-level BPE) · `hf_data.tinystories_bpe_next` / `collapse_contexts` / train-shard
`load_texts` · `qary_gl_predict.run_bpe_next` / `run_bpe_recall` / `_highorder_test(basis=, search=,
deg2_full=False, collapse=True)` · `qary_gl.qary_gl_search(norm_m=)` (collapsed exact-W) · `modal_gl.qary_bpe_sweep`.
