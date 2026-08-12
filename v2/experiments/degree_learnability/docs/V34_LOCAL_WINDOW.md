# v3.4.1: uniform Fourier fingerprints transfer across attention windows

## Result

The preregistered held-out gate passed. A transformer's measured response to
uniform binary Fourier characters predicts how changing its attention radius
changes its natural-text learning curve.

The intervention holds a two-layer, width-64 RoPE transformer and its training
protocol fixed while changing the per-layer hard attention window to 8, 16, 32,
or 64 tokens. Radius 64 reuses the v3.2 cells. The 168 new uniform cells cover
all 28 degree-one and degree-two supports on lags
`{1,2,4,8,16,32,64}`, two seeds, and the three smaller windows. The 288 new
natural cells cover 48 corpora, three smaller windows, and two seeds.

The 48 corpora were split before any limited-window natural outcome: four of
eight corpus IDs per source stratum formed development, and four formed
confirmation. Existing radius-64 outcomes were known, so this is prospective
for the new architecture intervention and paired contrast, not a wholly new
corpus collection.

## Statistic

For exact binary Walsh support \(A\) and attention window \(w\), define

\[
h_w(A)=\operatorname{median}_{s\in\{0,1\}}
  \operatorname{AUC}_{\log n}
  \frac{\mathrm{CE}_{w,A,s}(n)}{\mathrm{CE}_{w,A,s}(0)}.
\]

For natural corpus \(D\), let \(e_D(A)\) be its positive nested-projection
increment attributed to support \(A\), aggregated over categorical coefficient
directions. Within degree one and two,

\[
p_D(A)=\frac{e_D(A)}{\sum_{1\le |B|\le2}e_D(B)},\qquad
\Omega(D,w)=\sum_A p_D(A)h_w(A).
\]

The frozen predictor and paired target are

\[
\Delta\Omega(D,w)=\Omega(D,w)-\Omega(D,64),\qquad
\Delta A(D,w)=A(D,w)-A(D,64).
\]

Positive \(\Delta A\) would mean the smaller window learned more slowly. The
OLS is deliberately small: window fixed effects, with or without the single
\(\Delta\Omega\) feature. It is fitted on 24 development corpora and evaluated
unchanged on 24 confirmation corpora.

## Confirmation results

| predictor | held-out RMSE | held-out R² |
|---|---:|---:|
| window only | 0.003432 | 0.286 |
| window + uniform Fourier fingerprint | 0.002746 | 0.543 |
| window + unreachable Fourier energy | 0.003097 | 0.419 |
| window + unreachable energy + fingerprint | 0.002548 | 0.607 |

Adding the uniform fingerprint improves RMSE by **19.97%**, with the frozen
100,000-draw stratified corpus-bootstrap interval **[16.10%, 28.21%]**. Its
development coefficient is positive. The verdict is therefore
`LOCAL_WINDOW_UNIFORM_FINGERPRINT_TRANSFERS`.

The fingerprint also improves RMSE by **17.75%** beyond the unreachable-energy
model, interval **[12.96%, 26.68%]**. Within each confirmation window, the
fingerprint/curve-area Pearson correlations are `0.680`, `0.767`, and `0.718`
for radii 8, 16, and 32. The transfer is therefore not created merely by the
three window means.

On this dyadic lag grid, direct far-energy and algebraically unreachable energy
are identical. A two-layer window \(w\) has graph reach
\(1+2(w-1)\), but no sampled lag lies strictly between `w` and that reach.

## What the architecture probe says

Uniform Fourier response is highly non-isotropic. For radius 8, degree alone
explains only `8.0%` of support-hardness variance; adding log radius raises that
to `73.7%`. Within-degree hardness standard deviations are `0.288` at degree one
and `0.248` at degree two. Degree-one characters at lags 1–8 have area about
`0.400`, while lags 16–64 are near the unlearned ceiling (`0.981–0.983`). This
matches the two-layer reach boundary exactly.

Reachability is not the whole response. Some reachable degree-two characters
learn immediately, while others learn late or saturate. That support-specific
optimization geometry is what lets the empirical fingerprint improve over a
binary far-energy statistic.

## The important sign and scope caveats

Every corpus had a *lower* curve area at radii 8, 16, and 32 than at radius 64:
the mean confirmation contrasts were `-0.00979`, `-0.00765`, and `-0.00444`.
Thus a smaller window is not an absolute hardness penalty here. It behaves like
an optimization or regularization benefit at this fixed 3M-token budget.

After the window mean is removed, however, a larger uniform Fourier hardness
contrast predicts a larger natural curve-area contrast: the dataset benefits
less, or would be penalized more, when the corresponding uniform characters are
harder for that architecture. This is the learner-conditional claim supported
by v3.4.1.

The endpoint result is weak and differently oriented. For final CE fraction,
the fingerprint improves the window-only RMSE by only `2.40%`, reaches held-out
`R²=0.052`, and adds nothing beyond far-energy. The strong result is therefore
about finite-budget learning-curve shape, not final entropy or an intrinsic
model-independent scalar hardness.

Finally, the bridge still aggregates a 256-category dependent-data projection
to support energy and then weights it by one binary Walsh parity response per
support. It does not align every categorical frequency direction. The result
supports architecture-conditioned Fourier geometry, while leaving a more exact
data-conditioned characteristic basis as the next refinement.

Audit PASS. Protocol `06ae1e6c…`; uniform response `b3533c5c…`; frozen features
`e1ae6b0b…`; analysis `7cac8d99…`; audit `4f179c42…`.
