# Fourier spectrum × empirical CE learnability

## Question

Does a dataset become harder for a particular architecture when its Fourier
spectrum places more energy on exact characters that the architecture learns
slowly in held-out cross-entropy?

This is a direct Fourier-to-learning test. The dataset side is measured without
training the student. The architecture side is measured from realized held-out
CE learning curves—no initialization or gradient proxy is used.

## Fourier characters

For binary inputs and lag support \(A\), the real Fourier character is

\[
\chi_A(x)=(-1)^{\sum_{j\in A}x_{-j}}.
\]

The support bank contains all 63 nonempty subsets through degree three of
\(\{1,2,4,8,16,32,64\}\). Each of five positional geometries—learned absolute,
RoPE, NoPE, ALiBi, and reverse-ALiBi—is trained from scratch to predict every
character, with three seeds per cell.

For architecture \(m\), support \(A\), seed \(s\), let

\[
H_{m,A,s}
=
\operatorname{Area}_{\log n}
\frac{\operatorname{CE}_{m,A,s}(n)}
     {\operatorname{CE}_{m,A,s}(0)}.
\]

The architecture's empirical Fourier-character hardness is simply

\[
h_m(A)=\operatorname{median}_s H_{m,A,s}.
\]

High \(h_m(A)\) means the architecture spends more of its finite training budget
at high held-out CE on that exact Fourier character.

## Dataset Fourier support energy

For each corpus, 200,000 target positions are sampled uniformly from held-out
randomized intact blocks. For random nested support chains, the profiler
cross-fits conditional collision energy

\[
C_D(A)=\mathbb E\sum_y \widehat P_1(y\mid X_A)
                         \widehat P_2(y\mid X_A).
\]

When coordinate \(j\) is added to support \(A\), its positive projection
increment is

\[
\Delta_D(A\cup\{j\})=[C_D(A\cup\{j\})-C_D(A)]_+.
\]

Averaging these increments by exact unordered lag support gives \(e_D(A)\).
This is an order-averaged attribution in the dependent-data categorical
projection basis. It is the scalable dataset Fourier-spectrum estimator used by
this experiment.

## Architecture-conditioned Fourier overlap

The proposed difficulty feature is

\[
\Omega_{\mathrm{CE}}(D,m)
=
\frac{\sum_A e_D(A)h_m(A)}{\sum_A e_D(A)}.
\]

Every quantity has a direct interpretation:

- \(e_D(A)\): how much dataset projection energy is attributed to Fourier
  support \(A\);
- \(h_m(A)\): how difficult that exact Fourier character was to learn in
  held-out CE with architecture \(m\);
- \(\Omega_{\mathrm{CE}}\): expected empirical character difficulty under the
  dataset's measured Fourier energy distribution.

No monotonic locality assumption is required. If an architecture finds lags 1
and 64 easy but intermediate lags hard, that non-monotone shape is retained in
\(h_m\) and used as measured.

## Frozen predictive test

Both OLS models include architecture and source-stratum fixed effects. The strong
baseline contains unigram entropy, held-out bigram CE, lag-one mutual
information, compression rate, low-degree energy, mean degree, and marginal
radius. The matched model adds only \(\Omega_{\mathrm{CE}}\).

The 24-corpus pilot gate is grouped leave-one-corpus-out RMSE. If the Fourier-CE
model improves point RMSE, both models are fit on the complete pilot and their
predictions for 48 new source-disjoint corpora are hash-locked before natural
training begins. The confirmation unit is the corpus, retaining all five
architecture outcomes within each bootstrap block.

## Status

Protocol v3.1 corrected the experiment before any corpus profile, pilot outcome,
or confirmation outcome existed. The full empirical response surface requires
945 Fourier-character CE cells. Of those, 270 already-completed CE training
outcomes were reusable because the removed side analysis never entered their
optimization or validation computation. A bounded single-H100 continuation has
now saved 36 additional cells, leaving 639. The runner is cell-level resumable;
no partial cell is treated as an observation.

All 72 label-free corpus profiles are now complete and audit-locked. Within the
measured nonconstant energy through degree three, the mean shares are 19.8% at
degree one, 36.5% at degree two, and 43.6% at degree three. Degree three therefore
cannot simply be discarded. Because the five already-measured degree-three
supports are nevertheless ceiling-saturated for every architecture, a 12-support
sentinel extension was frozen before observing those new outcomes. It combines
the eight highest-energy unmeasured triples with four geometric/boundary stress
triples. Pooling is allowed only if every sampled architecture/support median is
at least 0.94 and each architecture's range over all 17 sampled triples is at
most 0.04; otherwise all 35 triples are measured exactly.

The 180-cell sentinel bank completed and the frozen pooling gate passed for all
five architectures. The smallest architecture/support median was 0.96496, and
the within-architecture ranges were 0.00715--0.01159. Degree-three response is
therefore pooled by architecture under the predeclared rule; the pooled medians
are 0.96905 (learned absolute), 0.97266 (RoPE), 0.97257 (NoPE), 0.97168
(ALiBi), and 0.97247 (reverse-ALiBi). Exact support resolution remains necessary
at degrees one and two, where the observed response variation is large.

All 315 degree-two cells are now complete. Together with degree one and the
17-support degree-three sample, the response artifact contains 675 measured
cells, 45 exact supports, and 18 pooled degree-three supports. The exact table
does retain information that degree and radius alone erase. For example, RoPE's
degree-two pairs containing lag 1 have hardness near 0.401, while almost every
pair excluding lag 1 is near 0.97. ALiBi learns `(2,4)` more readily (median
0.887), but reverse-ALiBi does not (median 0.975); `(4,8)` is hard for both.

This specificity comes with a hard predictive test. Across the 72 profiles and
five architectures, the strong baseline features plus architecture/stratum
effects explain 98.48% of the overlap feature's variance. The remaining overlap
residual has standard deviation 0.00991, and the dataset-by-architecture
interaction standard deviation is 0.00961. Thus the pilot is testing whether a
small exact-support matching component predicts held-out CE beyond highly
correlated degree, locality, entropy, and compression controls.

Protocol: `configs/protocol_v3.1.json`. Artifacts:
`runs/local/v30_architecture_spectrum/`.
