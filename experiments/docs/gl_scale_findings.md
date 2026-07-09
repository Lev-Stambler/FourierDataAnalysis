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

## Honest conclusion

**The high-order-at-scale predictive win does NOT materialize on real language.** GL's ability to *find*
high-order terms is real (degree histogram + the uniform-data regression test), but on real language
next-token those terms are noise (data density) or reducible to low order (bit correlations), and long
contexts don't repeat. With the base-rate bug fixed and K selected honestly, **GL ties the majority
baseline (K=0)** — it neither beats nor loses to degree-≤2. Combined with the rigorous n=13 result
(`gl_real_findings.md`: GL ≈ exact-FWHT ≈ competitive-with-Lasso where checkable), the overall picture
stands: **dataset-GL is a validated recovery/characterization algorithm (plus the blindness insight),
not a demonstrated predictive winner over standard baselines on real data.** The differentiator is real
only where its preconditions hold — sparse high-order structure that is estimable, irreducible, and
backed by repeating contexts — which independent-bit data satisfies and natural language does not.
