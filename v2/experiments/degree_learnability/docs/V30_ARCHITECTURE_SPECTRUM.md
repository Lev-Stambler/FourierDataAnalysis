# v3.0 architecture–spectrum matching

## Question

The dataset spectrum is model-independent, but finite-budget learning difficulty
need not be. This experiment tests a narrower mechanism:

> A Transformer learns a corpus more easily when the corpus's categorical
> projection spectrum puts more mass on supports to which that architecture is
> responsive at initialization.

The five positional geometries are learned absolute positions, RoPE, NoPE,
ALiBi, and reverse-ALiBi. All use the same two-layer, width-64 causal
Transformer and the same 64-token effective context.

## Character-response kernel

For a binary Walsh character on lag support \(A\),

\[
\chi_A(x)=(-1)^{\sum_{j\in A}x_{-j}},
\]

the initialization response of architecture \(m\) is the empirical NTK
Rayleigh quotient

\[
R_m(A)=\frac{1}{n}\chi_A^\top K_m\chi_A
=\left\|\nabla_\theta\frac{1}{\sqrt n}
\sum_i\chi_A(x_i)[z_{i,1}-z_{i,0}]\right\|_2^2.
\]

It is evaluated for all 63 nonempty subsets through degree three of the lag bank
\(\{1,2,4,8,16,32,64\}\), then geometrically averaged over eight initialization
seeds. The architecture-specific hardness kernel is

\[
h_m(A)=-\frac{\log R_m(A)-\mathbb E_B[\log R_m(B)]}
{\operatorname{sd}_B[\log R_m(B)]}.
\]

The sign convention makes positive values denote supports to which the
architecture has relatively weak gradient response.

## Dataset support spectrum

For each corpus, 200,000 target positions are sampled uniformly from a held-out
set of randomized intact 16 KiB blocks. For 512 random nested support chains,
the code cross-fits the conditional collision energy

\[
C_D(A)=\mathbb E\sum_y \widehat P_1(y\mid X_A)
                         \widehat P_2(y\mid X_A).
\]

When coordinate \(j\) is added to the current support \(A\), its positive
marginal increment is

\[
\Delta_D(A\cup\{j\})=[C_D(A\cup\{j\})-C_D(A)]_+.
\]

Averaging those increments by exact unordered support gives \(e_D(A)\). This is
an order-averaged dependent-data projection attribution in the
inverse-likelihood categorical basis. It is deliberately not described as an
exact product-measure Fourier coefficient.

The architecture-matched scalar is

\[
\Omega(D,m)=
\frac{\sum_A e_D(A)h_m(A)}{\sum_A e_D(A)}.
\]

This preserves a model-independent dataset measurement \(e_D\), while making
the final difficulty prediction explicitly learner-conditional through \(h_m\).

## Frozen tests

The mechanism stage trains the five architectures on 18 exact character tasks
with three seeds each. Its regression is

\[
H_{m,A}=\alpha_m+\gamma_{|A|}+\beta\log R_m(A)+\epsilon_{m,A},
\]

where \(H\) is normalized held-out CE curve area. Complete support blocks are
resampled 100,000 times. The mechanism gate requires the upper 95% bound for
\(\beta\) to be below zero and opposite degree-one radius directions for ALiBi
and reverse-ALiBi.

If that gate passes, a strong seven-feature OLS baseline is compared with the
same model plus \(\Omega(D,m)\). Architecture and source-stratum fixed effects
are included in both. The continuation decision uses grouped
leave-one-corpus-out RMSE on 24 pilot corpora. Only a positive point improvement
permits fitting the full pilot models and hash-locking 240 predictions for 48
new, source-disjoint corpora before their 480 training cells begin.

The final transfer gate requires a strictly positive paired stratified
corpus-bootstrap interval and positive point improvement for at least three of
the five architectures.

## Result

The strict mechanism gate **did not pass**, so the natural-corpus pilot and
confirmation stages were not run.

One half of the mechanism test was strongly positive. Across the 90 reduced
architecture/support cells, controlling for architecture and degree, the
coefficient of mean log NTK response was

\[
\widehat\beta=-0.05329,
\]

with a frozen 100,000-draw support-cluster bootstrap interval of
`[-0.12938,-0.03639]`. Larger initialization response therefore corresponded to
smaller held-out learning-curve area in this controlled grid. The standardized
coefficient was `-0.364`. Adding NTK response raised descriptive in-sample
\(R^2\) from `0.601` to `0.694` (partial \(R^2=0.233\)).

The engineered directional control failed. Degree-one hardness versus log radius
had Spearman `rho=+0.464` for both ALiBi and reverse-ALiBi; the frozen gate
required ALiBi positive and reverse-ALiBi negative. Reverse-ALiBi did make lag 64
easy, but it did not progressively favor lags 2, 4, 8, 16, and 32. Instead, the
learner had two easy anchors:

- lag 1, available through the current-token residual stream;
- lag 64, the unique left boundary, which reverse-ALiBi attends strongly.

Intermediate lags remained hard. Ordinary ALiBi showed the expected increasing
difficulty from lag 1 through lag 32, but the same boundary shortcut made lag 64
easy there too. Thus changing the sign of an ALiBi bias does not create the
smooth opposite geometric kernel assumed by the control.

The degree/architecture separation itself was large. Median character hardness
by degree was:

| architecture | degree 1 | degree 2 | degree 3 |
|---|---:|---:|---:|
| learned absolute | 0.401 | 0.401 | 0.971 |
| RoPE | 0.401 | 0.401 | 0.973 |
| NoPE | 0.950 | 0.973 | 0.973 |
| ALiBi | 0.831 | 0.966 | 0.972 |
| reverse-ALiBi | 0.959 | 0.974 | 0.973 |

These medians hide the easy lag-1/lag-64 endpoints for NoPE and reverse-ALiBi,
but they make the architecture dependence unambiguous. The character schedule
also had limited time resolution: only 7.4% of cells reached half of their best
learning after the first checkpoint. Easy cells were already solved by 65,536
examples, while most hard cells stayed near chance.

A clearly labeled post-gate diagnostic found only a `0.57%` RMSE improvement when
NTK response was tested by grouped leave-one-support-out prediction. Therefore
the negative coefficient is credible as an association on the frozen support
grid, but it is not yet a strong claim that the scalar response generalizes to a
new support shape.

The correct conclusion is **partial mechanism evidence, strict gate failure**:
the character NTK contains real architecture-specific learnability information,
but the proposed reverse-ALiBi monotonicity control was misspecified. Running 720
natural-corpus training cells after that failure would violate the predeclared
decision rule and spend compute without a validated bridge.

Protocol: `configs/protocol_v3.0.json`. Artifacts:
`runs/local/v30_architecture_spectrum/`.
