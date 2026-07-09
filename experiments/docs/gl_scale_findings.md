# High-order at scale (language): does GL beat what an enumerated basis can't reach?

**The differentiator we set out to show.** To capture a degree-d interaction, any enumerated-basis
method (Lasso, logistic on an explicit feature set) needs O(nᵈ) features, so at scale it is capped at
low order (degree-≤2 is already C(n,2) features).  GL finds the few heavy HIGH-order terms adaptively
via CSAMP without enumerating them.  We tested whether this buys a predictive win on data whose terms
are a-priori unknown and possibly high-order — natural **language** (TinyStories next-token) at large n.
(`fda_exp/gl_scale_predict.py`, run on Modal A10G.)

## Results

### 1. Natural next-token (n=25, V=32, 2²⁵≈3·10⁷ — full FWHT/Lasso infeasible) — GL LOSES

| method | top-1 | top-3 |
|---|---|---|
| majority baseline | 0.674 | – |
| logistic degree-1 | 0.677 | 0.817 |
| logistic degree-≤2 (feasible enumerated) | 0.675 | **0.849** |
| **GL reconstruction** | **0.659** | 0.771 |

GL is the **worst** method — below even the majority baseline. Its heavy set is full of degree≥3
coefficients (11,503; degree histogram peaks at degree 3–4), but at n=25 with only ~23k training
contexts (C_D ≈ 2²⁵/23k ≈ 1400) those high-order coefficients are **aliasing noise** that *hurts*
prediction. GL does edge degree-≤2 on its least-confident ("sensitive") quartile — 0.447 vs 0.428 —
a real but tiny signal.

### 2. Planted high-order term on REAL contexts — the confound: high Walsh-degree ≠ high predictive order

Plant a pure degree-k Walsh character on real language contexts; GL recovers it, but so does degree-≤2:

| planted | GL recovered? | GL sign-acc | degree-≤2 sign-acc |
|---|---|---|---|
| degree-3 (positions 0,14,29) | **True** | 0.999 | 0.997 |
| degree-4 (positions 0,9,19,29) | **True** | 0.999 | 0.986 |

GL recovers the planted mask (the **mechanism works**), but degree-≤2 nearly matches it — because on
**real, correlated** contexts the bits are not independent, so a nominally-degree-k Walsh character is
**reducible to low order** (it correlates with pairwise/single-bit features). So on real data,
"high Walsh degree" does not mean "high predictive order" — which is exactly *why* degree-≤2 does fine
on language. (The clean separation — degree-≤2 provably at chance — holds only on *independent-bit*
data; that is `tests/test_gl_real.py`, which passes.)

### 3. Scalability — long language contexts don't repeat, so GL isn't even feasible at large n

| n=40 (8-token windows) | status | max search width |
|---|---|---|
| real language | **blowup** | 131072 |
| random bits | blowup | 131072 |

GL needs context repetition (the R_k condition) for CSAMP to condition on. Real 8-token windows recur
too rarely, so the search width explodes just like random bits. GL is feasible only for shorter
contexts / more repetitive data (e.g. the DNA-repeat sweep in `scale_experiment`).

## Honest conclusion

**The high-order-at-scale predictive win does NOT materialize on real language**, for three compounding
reasons: (a) the genuinely high-order structure GL recovers at large n is dominated by aliasing noise
(data density: you cannot estimate degree-5 coefficients from ~20k contexts over 2²⁵); (b) on real
*correlated* data, the high-Walsh-degree structure that *is* predictive is reducible to low order, so
degree-≤2 already captures it; (c) long contexts do not repeat enough for CSAMP to run.

So GL's ability to find high-order terms (real — the degree histogram and the uniform-data test both
confirm it) is genuine but does **not** translate into a predictive advantage over degree-≤2 on real
language. Combined with the rigorous n=13 result (`gl_real_findings.md`: GL ≈ exact-FWHT ≈ competitive
with Lasso where checkable), the honest overall picture stands: **dataset-GL is a validated
recovery/characterization algorithm (plus the blindness insight), not a demonstrated predictive winner
over standard baselines on real data.** The differentiator is real in principle (independent-bit data)
but the conditions it needs — sparse high-order structure that is estimable, irreducible, and backed by
repeating contexts — are not met by natural language at scale.
