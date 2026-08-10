# Locality spectrum: canonical definition

This is the canonical mathematical description of the text experiment. It
supersedes the loose phrase “Formula-(19) basis” in frozen historical protocols
and artifacts.

## 1. Categorical basis

Let the categorical context be

\[
X=(X_1,\ldots,X_d),\qquad X_i\in\{0,\ldots,N_i-1\},
\]

with possibly dependent coordinates. For a support $A\subseteq[d]$, a tuple
$z\in\prod_{i\in A}\{0,\ldots,N_i-2\}$, and marginal mass function $p_A$,
[Ferrere et al., Definition 3.1](https://arxiv.org/abs/2603.02673) defines

\[
\phi_A^{(z)}(x)
=
\frac{\prod_{i\in A}
\left(\mathbf 1\{x_i=z_i\}-\mathbf 1\{x_i=N_i-1\}\right)}
{p_A(x_A)},
\qquad
\phi_\varnothing(x)=1.
\]

This is a reference-category one-hot contrast, multiplied by an inverse joint
likelihood. It is not the usual complex character
$\exp(2\pi i\langle k,x\rangle/q)$. Low-degree-first rank selection handles
linear dependence caused by sparse or non-rectangular data support.

Important attribution correction: Formula (19) of that paper only lists four
vectors for a particular two-Bernoulli-variable support. The displayed formula
above is the general basis definition. Frozen JSON protocols and historical run
artifacts retain the old wording so that their hashes and provenance do not
change.

## 2. What the text profiler computes

For the preregistered lag pair $a<b$, define

\[
X=(X_{t-a},X_{t-b}),\qquad
Y=X_t,\qquad
f(X)=P(Y=\cdot\mid X)\in\mathbb R^q.
\]

The q=256 implementation does not materialize the inverse-likelihood columns.
It fits the equivalent nested subspaces under the empirical context
distribution:

\[
\begin{aligned}
V_0 &= \{\text{constant functions}\},\\
V_1 &= \{c+g_a(X_{t-a})+g_b(X_{t-b})\},\\
V_2 &= \{h(X_{t-a},X_{t-b})\}.
\end{aligned}
\]

Let $P_j f$ be the $L^2(P_X)$ projection onto $V_j$. An orthonormal basis
adapted to $V_0\subseteq V_1\subseteq V_2$ gives level masses

\[
W_0=\lVert P_0f\rVert_2^2,\quad
W_1=\lVert P_1f-P_0f\rVert_2^2,\quad
W_2=\lVert P_2f-P_1f\rVert_2^2.
\]

The profiler estimates these with two-fold cross-fitted Brier risks
$L_0,L_1,L_2$:

\[
W_0=1-L_0,\qquad W_1=L_0-L_1,\qquad W_2=L_1-L_2.
\]

Therefore

\[
S=W_0+W_1+W_2=1-L_2
=E\left[\left\|P(Y=\cdot\mid X_{t-a},X_{t-b})\right\|_2^2\right].
\]

This is the resolved square energy of the selected pair-conditional function,
not the full 64-position next-token spectrum.

## 3. Degree and locality

The degree of a basis term is its number of participating context variables:

\[
k=|A|.
\]

For the pair profile, the total-normalized mean degree and low-degree
concentration are

\[
\bar k=\frac{W_1+2W_2}{S},
\qquad
C_{\leq 1}=\frac{W_0+W_1}{S}.
\]

For a causal support $A$, the current locality radius is the farthest lag:

\[
r(A)=\max A.
\]

In the stride intervention the preregistered pair is $(s,2s)$, so its envelope
radius is $r=2s$. Its interaction degree remains two while its location moves
away from the prediction point.

The number of categorical degree-k columns available inside a radius-r causal
ball is

\[
C_k(r,q)=\binom{r}{k}(q-1)^k.
\]

The exploratory Parseval-weighted search-volume score was

\[
G_{\mathrm{total}}
=\frac{1}{S}\sum_{k=0}^2 W_k\log_2 C_k(r,q),
\qquad C_0=1.
\]

This score is coarse: it attaches the pair's outer radius to every degree level.
It is not an estimate of the ideal joint spectrum $W_{k,r}$, and it is not a
quantity defined by Ferrere et al.

## 4. Proposed full Fourier locality surface

The clean model-agnostic object is a cumulative two-parameter projection surface,
not $G_{\mathrm{total}}$. Let the full causal context contain lags
$1,\ldots,L$, and define

\[
\mathcal V_{k,r}
=\operatorname{span}\left\{
\phi_A^{(z)}:
|A|\leq k,\ \max A\leq r
\right\}.
\]

For the dataset conditional $f_P(x)=P(Y=\cdot\mid X=x)$, define

\[
\mathcal E_P(k,r)
=\left\|\Pi_{\mathcal V_{k,r}}f_P\right\|_{L^2(P_X)}^2,
\qquad
\mathcal C_P(k,r)
=\frac{\mathcal E_P(k,r)}{\mathcal E_P(K,L)}.
\]

Here $K,L$ are the maximum resolved degree and radius. The surface
$\mathcal C_P(k,r)$ is:

- computed from the data distribution, without reference to a trained learner;
- invariant to the choice of orthonormal coordinates inside
  $\mathcal V_{k,r}$;
- nondecreasing in both $k$ and $r$;
- equal, for independent uniform coordinates, to cumulative ordinary Fourier
  square mass over terms satisfying $|A|\leq k$ and $\max A\leq r$.

It is also estimable by nested held-out Brier risks. If $L_{k,r}$ is the risk of
the projection onto $\mathcal V_{k,r}$, then

\[
\mathcal E_P(k,r)=1-L_{k,r}.
\]

For a fixed degree cap, the radial increments

\[
\Delta_r\mathcal E_P(k,r)
=\mathcal E_P(k,r)-\mathcal E_P(k,r-1)\geq0
\]

form a genuine nonnegative locality spectrum. Useful scalar summaries include a
spectral locality quantile

\[
R_{k,\eta}
=\min\left\{
r:\mathcal E_P(k,r)-\mathcal E_P(k,0)\geq
\eta\left[\mathcal E_P(k,L)-\mathcal E_P(k,0)\right]
\right\},
\]

and an energy-weighted log radius

\[
\Lambda_k
=
\frac{
\sum_{r=1}^{L}
\Delta_r\mathcal E_P(k,r)\log_2(1+r)
}{
\mathcal E_P(k,L)-\mathcal E_P(k,0)
}.
\]

This avoids a subtle problem with an exact joint mass $W_{k,r}$: under dependent
inputs, degree and radius projection increments need not commute, so a
two-dimensional inclusion-exclusion difference need not be nonnegative. The
cumulative surface remains canonical.

This surface is the plausible new formal framing. Protocols through v2.3 do not
estimate it: they estimate one pair at $(s,2s)$ and use its outer radius $2s$.
Protocol v2.4 takes a finite-resolution step toward it, described in Section 7.

## 5. “Locality rho”

For corpus $c$ and stride condition $s$, the primary difficulty is

\[
D_{c,s}=\operatorname{median}_{\text{seed}}
\frac{\mathrm{CE}^{\mathrm{heldout}}_{\mathrm{final}}}
{\mathrm{CE}^{\mathrm{heldout}}_{\mathrm{initial}}}.
\]

The within-corpus locality statistic is Spearman's rank correlation

\[
\rho_c=\operatorname{Spearman}
\left(\log_2 r_{c,s},D_{c,s}\right),
\qquad
\rho_{\mathrm{local}}=\frac1C\sum_c\rho_c.
\]

The frozen v2.0-v2.2 scripts used $G_{\mathrm{total}}$ for the rank test. In
every corpus in those experiments, $G_{\mathrm{total}}$ is strictly increasing
with $r$. Consequently its ranks, every reported rho, and every exact blocked
permutation p-value are identical to those from $\log_2 r$. “Locality rho” is
therefore the honest headline. The rank test does not identify a particular
functional form.

The exact null independently permutes the difficulty ranks within each corpus
and recomputes the mean blocked rho.

| Protocol | Domains | Locality rho | Exact two-sided p | Endpoint direction |
|---|---:|---:|---:|---:|
| v2.0 | 4 natural byte corpora | 0.850 | 0.00139 | 4/4 |
| v2.1 | 6 unseen corpora | 0.857 | 1.10e-11 | 5/6 |
| v2.2 | 3 harder math/code/technical domains | 0.867 | 0.00579 | 3/3 |

The v2.2 value 0.867 is a strong rank association: its three domain-level rhos
are 1.0, 0.8, and 0.8. It is not an “86.7% effect size,” explained variance, or
change in CE. With four stride conditions, individual Spearman values are coarse;
the blocked exact test and the independent v2.1 replication carry more evidential
weight than the decimal precision of 0.867.

Degree-only rho in v2.1 was 0.012 or 0.127, depending on normalization. In the
quantitative leave-one-corpus-out comparison, $\log_2 r$ had lower RMSE than
$G_{\mathrm{total}}$ (0.0809 versus 0.0937). Thus the supported result is a
locality ordering, not validation of the proposed $G_{\mathrm{total}}$ law.

## 6. Scope

The experiment shows that this byte-preserving stride transformation makes
finite-budget learning harder as the measured low-degree dependency is moved
farther away. It does not yet show that radius alone causes the change: the
interleaving also changes unmeasured higher-order and full-context structure. It
also does not establish a universal architecture-independent sample-complexity
law.

## 7. Resolved v2.4 lower-bound surface

Let the resolved lag grid be

\[
\mathcal R=\{1,2,4,8,16,32,64\}.
\]

Protocol v2.4 profiles every pair $(a,b)$ with $a,b\in\mathcal R$ and $a<b$,
using the exact cross-fitted degree decomposition from Section 2 inside each
support. For a radius threshold $r$, it forms the monotone lower-envelope
estimates

\[
\widetilde{\mathcal E}(1,r)
=\max_{a<b\leq r}(W_0^{a,b}+W_1^{a,b}),\qquad
\widetilde{\mathcal E}(2,r)
=\max_{a<b\leq r}(W_0^{a,b}+W_1^{a,b}+W_2^{a,b}).
\]

These quantities are nondecreasing in $r$ and are conservative lower bounds on
the corresponding full-context projection energies: each pair subspace is
contained in the full radius-$r$ subspace. Their increments define a resolved
radial mass

\[
\widetilde\Delta_r
=\widetilde{\mathcal E}(2,r)-
  \widetilde{\mathcal E}(2,r^-),
\]

from which v2.4 computes an energy-weighted log radius, radial entropy, and 50%
and 90% radius quantiles. Degree summaries use the best resolved pair and the
maximum incremental degree-2 energy. This construction avoids double-counting
overlapping pair projections, but it can miss energy that is distributed across
several pairs or lives at degree greater than two. It is therefore explicitly a
resolved lower-bound surface, not an estimate of every coefficient in the
64-position conditional.

The locked natural-data result shows the current limitation quantitatively.
These Fourier summaries have positive out-of-sample predictive power for endpoint
difficulty ($R^2=0.260$), but held-out bigram CE, unigram entropy, lag-1 mutual
information, and compression rate do better jointly ($R^2=0.572$). Thus the
surface is mathematically interpretable without yet being a sufficient statistic
for natural-text learnability.

## 8. Image bases

For real-valued images, v2.4 uses a separate basis rather than treating pixels as
q-ary text symbols. For a channel image $I\in\mathbb R^{H\times W}$, the
orthonormal two-dimensional DCT-II coefficients are

\[
C_{uv}=\alpha_u\alpha_v
\sum_{x=0}^{H-1}\sum_{y=0}^{W-1}
I_{xy}
\cos\!\left[\frac{\pi(2x+1)u}{2H}\right]
\cos\!\left[\frac{\pi(2y+1)v}{2W}\right],
\]

with the usual orthonormal factors $\alpha$. Parseval gives
$\sum_{uv}C_{uv}^2=\sum_{xy}I_{xy}^2$. After removing the DC coefficient, the
implementation reports low-frequency concentration, high-frequency tail,
energy-weighted frequency, and spectral entropy.

To describe nonlinear spatial dependence, each pixel coordinate is also
rank-Gaussianized to $Z=\Phi^{-1}(F(X))$. The first Hermite coordinate is
$H_1(Z)=Z$, so a degree-2 product coefficient at two patch positions is

\[
\widehat f_{ij}=E[H_1(Z_i)H_1(Z_j)]=E[Z_iZ_j].
\]

Squared coefficients, grouped by spatial separation, give a degree-2 Hermite
energy and locality profile. These are natural continuous-product analogues of
degree and radius, but they are not numerically commensurate with the dependent
categorical text basis and are never pooled with it.

## 9. Sampled higher-order projection curve

Enumerating q-ary coefficients at degree k costs
$\binom{d}{k}(q-1)^k$ and is infeasible for byte text. The KISS diagnostic instead
samples nested supports $A_0\subset A_1\subset\cdots\subset A_K$ and estimates

\[
M(A_k)=E\left[\left\|P(Y=\cdot\mid X_{A_k})\right\|_2^2\right].
\]

For each two-fold split, the opposite fold supplies an empirical conditional
probability $\widehat p_{-f}(Y_i\mid X_{i,A})$. The score

\[
Z_i=\widehat p_{-f}(Y_i\mid X_{i,A})\in[0,1]
\]

is averaged over held-out positions. Conditional on the fitted opposite-fold
table, Hoeffding gives

\[
P(|\bar Z-EZ|>\epsilon)\leq2e^{-2n\epsilon^2}.
\]

If an evaluation context was never observed in the opposite fold, it contributes
zero. The profiler therefore reports context coverage $c_A$ and the conservative
identification interval $[\widehat M(A),\widehat M(A)+1-c_A]$ separately from
sampling uncertainty.

Under a product input measure with orthogonal coordinate subspaces, the mean
$M_k=E_{|A|=k}M(A)$ is a binomial transform of the Fourier levels:

\[
M_k=\sum_{j=0}^k\frac{\binom{k}{j}}{\binom{d}{j}}W_j.
\]

This can be inverted recursively without enumerating individual coefficients.
For dependent natural text, however, coordinate subspaces overlap and this
identity does not define the Ferrere spectrum. Large signed inverted levels in
the real profiles demonstrate the failure of that product assumption. The
reported cumulative conditional-energy curve remains basis invariant and useful;
its increments must not be relabeled as exact dependent-input Fourier weights.

## 10. Frozen v2.6 sampled geometric summary

For v2.6, each random chain uses the pinned lag bank
$\{1,2,4,8,16,32,64\}$ and stops at degree three. Empirical nested increments
are clipped only for the scalar predictor:

\[
\delta_{c,k}=\max\{\widehat M(A_{c,k})-\widehat M(A_{c,k-1}),0\}.
\]

Raw signed increments and coverage intervals remain in the audit artifact. The
three frozen dataset features are

\[
E_{\leq3}=\frac1C\sum_c
\max\{\widehat M(A_{c,3})-\widehat M(A_{c,0}),0\},
\]

\[
D_{\leq3}=\frac{\sum_{c,k}k\delta_{c,k}}{\sum_{c,k}\delta_{c,k}},
\]

and

\[
G_{\leq3}=\frac{
\sum_{c,k}\delta_{c,k}\log_2\!\left[
\binom{\max A_{c,k}}{k}(q-1)^k\right]
}{\sum_c\widehat M(A_{c,3})}.
\]

The denominator of $G_{\leq3}$ is the resolved total square energy, including
the constant component, matching the Parseval normalization motivation. This is
a fixed randomized projection summary, not an exact recovery of dependent-input
Fourier levels. Its value is computed before training and is model-independent;
whether it predicts finite-budget hardness is learner-conditional and is the
prospective v2.6 question.
