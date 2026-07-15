#import "../levs-commands/main_commands.typ": *
#import "../custom_commands.typ": *
#show: thmrules
#show: eqrules
#set math.equation(numbering: none)

= Position-Shared Fourier Correlation Memory <sec:fourier-correlation-memory>

The standalone student of @sec:qwen-scaling stores a list of characters whose supports name particular
input positions.  This is transparent, but position sharing is absent: an interaction at lag $3$ and the
same interaction at lag $30$ are two different characters with two different fitted weights.  At fixed
token degree $d$, the complete categorical basis contains
$
sum_(k=0)^d binom(n,k)(q-1)^k
$
characters, so even degree one grows linearly with the context length.

This section records a different structural hypothesis.  Instead of learning a free coefficient for every
position tuple, a recurrent memory learns what each token writes, how the current token queries those
writes, and how quickly the resulting correlations decay.  Stacking such memories makes higher-order
interactions implicit.  The construction is closely related to recurrent linear attention and fast-weight
programmers @katharopoulos2020transformers @schlag2021linear @yang2024gated.  The point here is not a new
architecture claim.  It is to state precisely which Fourier coefficients a shared correlation recurrence
ties together, and how its categorical degree grows with depth.

== A one-line Boolean example

Let $x_t in {-1,1}$, choose a retention parameter $gamma in [0,1)$, and set
$
M_0=0,
quad M_t=gamma M_(t-1)+x_t,
quad r_t=x_t M_t.
$
Because $x_t^2=1$,
$
r_t
=1+sum_(s<t) gamma^(t-s) x_s x_t.
$
A flat degree-two Fourier model would store a separate coefficient for every pair ${s,t}$.  This recurrence
generates all past--current pair coefficients from one number: the coefficient at lag $a=t-s$ is
$gamma^a$.  With $H$ memories and a linear readout, the lag kernel becomes
$
K(a)=sum_(h=1)^H w_h gamma_h^a.
$
Thus a fixed number of learned decay modes supplies a position-shared family of degree-two coefficients
at every context length.  It does not represent an arbitrary lag kernel: that restriction is exactly where
the parameter saving comes from.

== Gated correlation memories

Let $G=ZZ_q$ be the token group.  Fix one shared token map
$
phi:G -> RR^m.
$
Its coordinates may be real sine/cosine coordinates of categorical characters, or Walsh characters of the
LSH code in @sec:whlsh.  The same map is applied at every position; there is no position-specific token
table in this construction.

Set $h_t^0=phi(x_t)$.  At layer $ell in {1,dots,L}$ and memory head $a in {1,dots,H}$, define
$
q_t^(ell,a)=Q_(ell,a) h_t^(ell-1),
quad
k_t^(ell,a)=K_(ell,a) h_t^(ell-1),
quad
v_t^(ell,a)=V_(ell,a) h_t^(ell-1).
$
For a learned constant retention $gamma_(ell,a) in [0,1)$, the memory and its read are
$
M_0^(ell,a)=0,
quad
M_t^(ell,a)
=gamma_(ell,a) M_(t-1)^(ell,a)
 +v_t^(ell,a) (k_t^(ell,a))^T,
quad
r_t^(ell,a)=M_t^(ell,a)q_t^(ell,a).
$
The analyzed vertical update is linear:
$
h_t^ell
=A_ell h_t^(ell-1)
 +sum_(a=1)^H B_(ell,a) r_t^(ell,a).
$
Nonlinear pointwise blocks can be placed after the read in an implementation, but they are not part of the
degree theorem below.  Likewise, a data-dependent retention gate can make memory selective, but the
constant learned $gamma_(ell,a)$ is what gives the exact lag formula in this first model.

#lemma[Unfolding a gated correlation memory][
  For every layer, head, and time,
  $
  M_t^(ell,a)
  =sum_(s=1)^t gamma_(ell,a)^(t-s)
    v_s^(ell,a)(k_s^(ell,a))^T,
  $
  and hence
  $
  r_t^(ell,a)
  =sum_(s=1)^t gamma_(ell,a)^(t-s)
    v_s^(ell,a)
    chevron.l k_s^(ell,a),q_t^(ell,a) chevron.r.
  $
]<lem:correlation-memory-unfold>
#proof[
  The memory identity follows by induction on $t$.  The base case is the single outer product at $t=1$.
  Multiplying the inductive sum by $gamma_(ell,a)$ increments every lag, and the new outer product supplies
  the lag-zero term.  Right-multiplication by $q_t^(ell,a)$ gives the read identity.
]

The identity separates the three learned roles.  $V_(ell,a)$ determines what is written, the inner product
between $K_(ell,a)$ and $Q_(ell,a)$ determines which past writes correlate with the current query, and
$gamma_(ell,a)$ determines retention.  None is indexed by $t$ or by the maximum context length.

For common hidden width $d$, key width $r$, and value width $r_v$, the displayed linear model has
$
L [d^2+H(2 r d+2 d r_v+1)]
$
learned scalars, apart from biases and the token/output maps.  Its recurrent memory contains
$L H r r_v$ scalars.  Both counts are independent of $n$; evaluation still performs one update per token.

== The induced Fourier interactions

Degree here means the categorical token-support degree of @lem:qary-interaction-order under the uniform
reference transform.  For a vector function it is the maximum degree of a scalar coordinate.  This is a
structural statement about the represented function, not a claim that its coefficients under a nonuniform
dataset law equal its uniform Fourier reconstruction weights.

#proposition[One correlation layer has token degree at most two][
  Suppose $h_t^0=phi(x_t)$ and every coordinate of $phi$ is a single-token function.  Then every coordinate
  of $h_t^1$ has categorical token degree at most two.  More precisely, the term in
  @lem:correlation-memory-unfold indexed by $s<t$ depends only on $(x_s,x_t)$, while the term $s=t$
  depends only on $x_t$.  For $H$ heads, every lag dependence lies in the span of
  ${a mapsto gamma_h^a : h in [H]}$, and the token-frequency weights factor through the shared matrices
  $Q_h,K_h,V_h,B_h$.
]<prop:correlation-memory-degree-two>
#proof[
  At layer one, $q_t$ is a function only of $x_t$, while $k_s$ and $v_s$ are functions only of $x_s$.
  Each scalar coordinate of
  $v_s chevron.l k_s,q_t chevron.r$ is therefore a product of single-coordinate functions supported on
  ${s,t}$, or on ${t}$ when $s=t$.  The constant-versus-contrast tensor decomposition of
  @lem:qary-interaction-order places such a function in degrees at most two.  The unfolded identity supplies
  the factor $gamma_h^(t-s)$, and the remaining coefficient is a contraction of the four shared linear
  maps.  Summing heads and applying the linear residual map cannot increase degree or leave the stated span.
]

When $phi$ consists of group characters, products at one token stay within the character algebra:
$psi_a(x)psi_b(x)=psi_(a+b)(x)$.  For Walsh/LSH characters the corresponding operation is XOR of masks.
Thus the product of a key feature and a value feature at the same token may create a new token frequency,
but it still activates only one token position.  The query introduces the second position.

#proposition[Degree growth in a linear memory stack][
  Let $D_ell$ bound the categorical degree of every coordinate of $h_t^ell$, uniformly over $t$.
  For the displayed linear stack,
  $
  D_0=1,
  quad D_1 <= 2,
  quad D_ell <= 3 D_(ell-1) \ "for" \ ell>=2.
  $
  Consequently $D_ell <= 2 dot 3^(ell-1)$ for $ell>=1$.
]<prop:correlation-memory-degree-growth>
#proof[
  The first-layer improvement is @prop:correlation-memory-degree-two.  At a later layer, every scalar
  summand in an unfolded read is a product of one coordinate from $v_s$, one from $k_s$, and one from
  $q_t$.  Each has degree at most $D_(ell-1)$, so their product has degree at most
  $3D_(ell-1)$.  Sums, linear maps, and residual addition do not increase the maximum degree.  Iterating the
  recurrence from $D_1<=2$ proves the final bound.
]

The upper bound is deliberately coarse: repeated positions and character cancellations can lower the true
degree substantially.  Its role is conceptual.  A shallow stack can represent a dense collection of
higher-order Fourier interactions without storing those characters as a list.  Its complexity is the size
of the shared correlation program, not the number of nonzero coefficients in its fully expanded spectrum.

== What this does and does not resolve

This construction shares the transformer's parameter-count advantage, but not its episodic memory.
A causal transformer retains one key and value per earlier position, so its runtime cache grows with $n$.
The correlation recurrence replaces that cache by fixed-size matrices.  Writes with nonorthogonal keys can
interfere, and no theorem above promises exact recall of an arbitrary old token.  More heads, larger key
width, retention gates, or delta-rule updates can change that engineering tradeoff, but they do not make
finite memory equivalent to an unbounded cache.

The relation to the rest of the paper is therefore complementary:

+ Dataset GL searches for individually heavy character correlations under the rollout law.  It does not
  learn $Q,K,V$, or the memory matrices of this section.
+ The correlation model is fitted pointwise by supervised loss.  Its uniform Fourier spectrum is an
  implicit structural expansion; under a nonuniform dataset law, density aliasing still applies exactly as
  in @sec:qwen-experiment.
+ A successful fit would replace the sparse-character complexity hypothesis by a shared-memory hypothesis.
  Nothing here proves that the Qwen target has low correlation-memory width or that such a student matches
  transformer language-modeling quality.

The immediate mathematical question is now concrete: whether the teacher's measured degree profile is
better explained by a small number of shared correlation heads and layers than by a growing list of
position-specific characters.  That comparison requires a separate experiment; it is not assumed by the
representation theorem.
