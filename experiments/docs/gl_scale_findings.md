# High-order at scale (language): does GL beat what an enumerated basis can't reach?

**The differentiator.** To capture a degree-d interaction, an enumerated-basis method (Lasso, logistic
on an explicit feature set) needs O(nᵈ) features, so at scale it is capped at low order (degree-≤2 is
already C(n,2) features). GL finds the few heavy HIGH-order terms adaptively via CSAMP without
enumerating them. We tested whether that buys a predictive win on natural **language** (TinyStories
next-token) at large n, where enumeration/full-FWHT is impossible. (`fda_exp/gl_scale_predict.py`,
Modal A10G.)

## The result — NO high-order predictive win (after fixing two bugs and selecting K properly)

Per-token GL reconstruction (recover the `next==t` indicator's heavy coeffs, reconstruct a score,
argmax over tokens), with the number of coefficients per token **chosen on a validation split** (the
same fix that made Phase 1 work), n=25, V=32, 2²⁵≈3·10⁷:

| method | top-1 | top-3 |
|---|---|---|
| majority baseline | 0.674 | – |
| logistic degree-1 | 0.677 | 0.818 |
| logistic degree-≤2 (feasible enumerated) | 0.667 | 0.840 |
| **GL reconstruction (val-K)** | **0.674** | 0.770 |

**GL ties the baseline** — because validation picks **K=0** coefficients per token: the GL
val-accuracy-vs-K curve is *flat/decreasing* (K=0 0.662, K=8 0.654, K=512 0.653). The high-order
coefficients GL recovers (its heavy set is full of degree≥3 terms) **do not generalize**: at n=25 with
~23k training contexts the density constant is C_D ≈ 2²⁵/23k ≈ 1400, so the empirical high-order
coefficients are aliasing noise, and the honest best is to use none. Same story at n=20 (val curve
flat at ~0.83, K=0 best).

### Honesty note — the earlier "GL loses on language" was a bug, but the fix gives a tie, not a win
Earlier drafts reported GL *below* baseline (0.659). That was a **two-part bug** (caught only when Lev
insisted there was one): (1) the reconstruction dropped the constant coefficient S=0, which is the
token base rate f_hat(∅), centering the indicator and biasing argmax toward rare tokens; (2) tokens
whose search hit `max_width` returned an empty support and scored −∞, so common tokens were never
predicted. Fixed (always seed the base-rate constant) + validation-selected K. The corrected result is
**GL = baseline**, not GL > degree-≤2. The balanced planted calibration was unaffected by the bug
(f_hat(∅)≈0 there), which is why it looked fine while the real target was broken.

## Two supporting negatives

**Planted degree-k on REAL contexts — the confound.** GL recovers a planted degree-3/4 Walsh character
(mask recovered: True), but degree-≤2 logistic nearly matches it (0.997 / 0.986) — on **correlated**
real contexts the bits are not independent, so a high-Walsh-degree character is **reducible to low
order**. "High Walsh degree ≠ high predictive order" on real data. The clean separation (degree-≤2 at
chance) holds only on *independent-bit* data — that is `tests/test_gl_real.py`, which passes.

**Scalability.** At n=40 (8-token windows) real language contexts don't repeat, so the CSAMP width
blows up just like random bits — GL needs shorter contexts / more repetitive data (R_k condition).

## CRITICAL: the bit basis was WRONG; use the categorical (one-hot) basis over token positions

Encoding each token in log2(V) BITS and running Walsh over those bits mismeasures degree: a single
token's identity needs bit-degree up to `bpt` (all its bits), so bit-degree != token-degree. The right
basis for categorical tokens is one-hot / Householder over token POSITIONS (degree = #token positions
jointly involved). Switching to it makes the low-order model far stronger: **categorical degree-<=2
log-loss ~0.58 vs ~1.0 in the bit basis**. And any "degree-3 helps" seen in the bit basis is a proven
artifact — the helpful "degree-3 bit-characters" all live within <=2 *token* positions.

## Verified conclusion (correct basis, 4 bugs fixed, 2 independent agents + 5 angles)

In the CORRECT categorical basis, **degree-3 (>=3 token positions jointly) token structure does NOT add
predictive value** for TinyStories next-token, across every angle tried:
- categorical majority (log-loss deg<=2 0.596 vs deg<=3 0.607, WORSE);
- full-distribution over all pairs (cross-entropy 1.313 = 1.313);
- longer context window=8 (CE 1.302 vs 1.345, worse);
- non-degenerate larger vocab=64 (CE 1.941 vs 1.976, worse);
- an independent from-scratch subagent (0.596 vs 0.607; proved the bit-basis gain is an artifact).
Language next-token is bigram/trigram-dominated = <=2-order in tokens.

**FOUR bugs were found and fixed** along the way, all of the "makes GL look artificially bad" family
(bit-ordering; off-by-one dropping the top heavy hitter; `gl_onevsrest` dropping the constant/base rate;
and train-absent tokens scored 0 and beating real tokens in argmax). With all four fixed, **GL
reconstruction = the majority baseline exactly (val-K=0)** on next-token. The distinction that matters:
the earlier three "GL loses" readings were BUGS — each broke under a single correct check; this
high-order negative HOLDS under 5 correct-basis angles + 2 independent agents. It is a real property of
the data, not a mistake.

**Bottom line:** dataset-GL is a validated recovery/characterization algorithm (+ the blindness insight)
and, with all bugs fixed, a predictor that ties standard baselines where checkable — **not** a
demonstrated high-order predictive winner on real language, because real language next-token has no
usable >=3-order token structure. The differentiator is real only where its preconditions hold (sparse,
irreducible, estimable high-order structure with repeating contexts) — synthetic independent-bit data
satisfies them; natural language does not.
