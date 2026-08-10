# KISS higher-degree diagnostic

Historical post-confirmatory status note. The canonical current conclusion is in
`../../README.md`; artifacts now live under
`runs/local/v25_kiss_diagnostic/`.

Date: 2026-08-07

## Status

This is a post-v2.4 diagnostic, not a new frozen confirmation. It answers two
questions raised by the negative v2.4 predictor comparison:

1. Did regularization and a train/test mismatch hurt the simple linear model?
2. Did stopping at degree two discard substantial natural-text signal?

The answer to both is yes. The evidence is promising but not yet a clean new
claim because the analysis choices were made after the v2.4 outcomes were known.

## Plain linear regression

The v2.4 predictor was already linear, but grouped selection chose ridge
`alpha=100`, the strongest shrinkage considered. It was trained on 124 rows,
including many artificial stride transformations, and scored for absolute
hardness on 36 natural stride-1 rows. In those natural rows, the pair-envelope
locality feature was exactly constant (`log2(3)=1.58496`) and radial entropy was
zero, so neither could explain cross-corpus variation.

The KISS diagnostic uses unpenalized OLS. Training only on the 31 natural
development rows and applying the fitted model unchanged to the 36 v2.4 natural
rows gives:

| Features | Endpoint R2 | Curve-area R2 |
|---|---:|---:|
| configuration only | 0.489 | 0.178 |
| four ordinary controls | 0.624 | 0.647 |
| controls + pair energy/degree | 0.699 | 0.662 |
| sampled degree-through-3 + configuration | 0.546 | 0.515 |
| controls + sampled degree-through-3 | 0.602 | 0.746 |

These are honest train-on-old/test-on-new calculations, but the choice to refit
on natural rows and the higher-degree features themselves are post-confirmatory.
They cannot replace the frozen v2.4 verdict.

Pooling all 25 corpora and recomputing leave-one-corpus-out OLS is more
conservative. Endpoint R2 is 0.697 for controls and 0.708 after adding the pair
spectrum; curve-area R2 is 0.681 for controls and 0.693 after adding sampled
degree-through-3. The incremental advantage is therefore small and metric
dependent at the current sample size.

## Sampled higher-degree estimator

For a support A, define the cumulative conditional collision energy

\[
M(A)=E\left[\left\|P(Y=\cdot\mid X_A)\right\|_2^2\right].
\]

The implementation samples random nested supports from lags 1--16. It estimates
M(A) by two-fold cross-fitting: an evaluation example receives the opposite
fold's empirical probability for its observed `(context,target)` cell. Every
score lies in `[0,1]`, giving a direct Hoeffding-style evaluation interval.
Unseen opposite-fold contexts score zero, and their exact mass is reported as a
coverage-identification interval instead of being silently treated as absence of
high-degree energy.

All 25 corpora show substantial cumulative gains through degree three. On the 12
v2.4 corpora, opposite-fold context coverage at degree three remains 80--90%.
Degree-three increments are commonly comparable to or larger than degree-two
increments; examples include LLVM (`+0.110/+0.127`) and Coq
(`+0.111/+0.124`). At degree four coverage often falls to 50--75%; degrees five
and six are generally unresolved, and negative empirical increments reflect
missing-context bias rather than negative true energy.

Under an orthogonal product input measure, the average over uniform size-k
supports obeys

\[
M_k=\sum_{j=0}^k\frac{\binom{k}{j}}{\binom{d}{j}}W_j,
\]

so the Fourier level weights can be recovered by binomial inversion. Natural
text coordinates are strongly dependent, and applying that inversion produces
large signed levels; it is retained only as a product-reference diagnostic. The
cross-fitted cumulative curve is the defensible dependent-data object.

## Next confirmatory design

Use one fixed Transformer architecture and token budget, one row per independent
corpus, and plain OLS. Freeze only:

- configuration/intercept baseline;
- four ordinary controls;
- pair energy and mean degree;
- sampled energy and mean degree through degree three;
- controls plus the spectral features.

Thirty to forty new corpora are preferable. Extra training seeds reduce outcome
noise but do not increase the effective sample size for estimating dataset-level
regression coefficients. Degrees above three require more corpus bytes, a
collision sketch, or a coarser categorical representation before they can be
used responsibly.
