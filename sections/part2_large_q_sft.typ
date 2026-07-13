#import "../levs-commands/main_commands.typ": *
#import "../custom_commands.typ": *
#show: thmrules
#show: eqrules
#set math.equation(numbering: none)

#let part2-protocol = json("../experiments/part2_protocol.json")
#let part2-date = part2-protocol.at("date_display")
#let part2-q = part2-protocol.at("q")
#let part2-n = part2-protocol.at("n")
#let part2-T = part2-protocol.at("energy_threshold")
#let part2-rms-cutoff = part2-protocol.at("rms_cutoff")
#let part2-toy = part2-protocol.at("toy")
#let part2-audit = part2-protocol.at("qwen_audit")
#let part2-scenario = part2-protocol.at("complexity_scenario")

= Part 2: Affine Sparse-Fourier Audit for Large-$q$ Dataset GL <sec:part2-large-q>

#align(center)[_#(part2-date)_]

This dated Part 2 is additive.  The preceding theory, experiments, and historical controls remain in the
paper.  The active object here is still the tokenizer-native group
$ZZ_(#(part2-q))^(#(part2-n))$: one token is one categorical variable, and a recovered frequency is an
explicit vector in that group.  Logarithmic-time arithmetic on a group element does not encode a token
into Boolean input variables.  Part 2 introduces no simplex basis, binary token representation, model
layer deletion, or neural student hidden behind the Fourier representation.

The question is whether finite-group sparse Fourier algorithms can bypass the broad reverse-time tree.
Akavia--Goldwasser--Safra and its finite-abelian exposition find scalar significant coefficients with
polylogarithmic group-size dependence, but they assume chosen evaluations of the analyzed function and
also have polynomial dependence on relative significance, requested accuracy, and
$norm(f)_infinity/norm(f)_2$ @akavia2003proving @galbraith2016finding.  For the density-weighted table
$h_D=q^n D F$, that norm ratio can be as large as $sqrt(q^n)$, so the reduction need not be tractable.
The $q$-SFT and GFast algorithms use designed affine grids and
singleton peeling, together with exact sparsity and random-support assumptions
@erginbas2023efficiently @tsui2025efficient.  None receives only natural samples and ordinary
prefix-conditional continuations.  We therefore derive the exact oracle reduction before treating any of
these algorithms as executable Dataset GL.

== What is sampled, and what is only read

For a real FineWeb context $Z=z$, strict Dataset GL samples a new continuation one token at a time,
$
X=(X_1,dots,X_n) ~ calD_z,
quad
calD_z(x)=product_(t=1)^n Q_theta(x_t|z,x_(<t)).
$
At a reverse-time node, the realized left prefix is cached and its KV state is forked to sample two
independent new suffixes.  Those are genuine conditional samples from $calD_z$.

After a complete $n=128$ token string $x$ has been obtained, a separate forward receives $x$ alone and
returns
$
F(x)=P_theta^"tok"(dot|x).
$
This is the distribution of token $129$ after the context lens; token $129$ is not sampled for the vector
label.  Let $g(x)=arg max_t F_t(x)$.  The hard target is $Y(x)=e_(g(x))$, while $F(x)$ itself is the
secondary soft target.

An affine SFT query is different.  It chooses all of
$x=d+M ell in ZZ_q^n$ algebraically, then asks for the function value at that point.  Qwen can in principle
teacher-force the chosen string to score $D_z(x)$, its autoregressive mass for the fixed context $z$, and
can separately return $F(x)$, but the
chosen $x$ is not a natural rollout.  We call that stronger operation a *density-and-label point query* and
never silently identify it with prefix-$sans("CSAMP")$.

== Density-weighted chosen-point reduction

Fix one real context $z$, abbreviate $D=calD_z$, and let $F:G^n -> CC^m$ be bounded.  With
$N=q^n$ and $chi_k(x)=omega_q^(chevron.l k,x chevron.r)$, the dataset coefficient is
$
a_D(k)
= EE_(X~D)[F(X) overline(chi_k(X))].
$
Define the density-weighted table
$
h_D(x)=N D(x)F(x).
$
Under the normalized uniform transform,
$
hat(h_D)(k)
=frac(1,N)sum_(x in G^n)h_D(x)overline(chi_k(x))
=a_D(k).
$
Thus a standard uniform-law SFT can target the fixed-context dataset coefficients only when its chosen
oracle returns both the absolute probability $D(x)$ and the label $F(x)$ for every requested $x$.
Autoregressive teacher forcing supplies a computational density query for a fixed $z$; it does not turn
that query into an in-distribution sample.  For the marginal mixture
$D_X(x)=EE_Z[D_Z(x)]$, an exact chosen query would additionally have to integrate over the corpus-context
law.  Its transform is $EE_Z[a_(D_Z)(k)]$, whose squared norm can differ arbitrarily from the Part 1 target
$EE_Z[norm(a_(D_Z)(k))_2^2)$.  Consequently no SFT of the marginal table solves the random-context RMS
problem; the shared-$Z$ pair identity below is the relevant one.

#proposition[Affine alias identity for dataset coefficients][
  Suppose $q$ is prime, $M in ZZ_q^(n times b)$ has full column rank, $d in ZZ_q^n$, and
  $j in ZZ_q^b$.  Define
  $
  U_(M,d)(j)
  =q^(-b)sum_(ell in ZZ_q^b)
    h_D(d+M ell) omega_q^(-chevron.l j,ell chevron.r).
  $
  Then
  $
  U_(M,d)(j)
  =sum_(k:M^T k=j)a_D(k)omega_q^(chevron.l k,d chevron.r).
  $
]<prop:part2-affine-alias>
#proof[
  Fourier inversion gives $h_D(x)=sum_k a_D(k)chi_k(x)$.  Substitute $x=d+M ell$ and use
  $chevron.l k,M ell chevron.r=chevron.l M^T k,ell chevron.r$.  The normalized character sum over
  $ell$ is one exactly when $M^T k=j$ and zero otherwise.  The displayed positive offset phase follows
  from the paper's negative-exponent forward-transform convention.
]

For vector coefficients, $U_(M,d)(j)$ is a vector alias sum.  Its norm generally contains cross terms:
$
norm(U_(M,d)(j))_2^2
!=sum_(k:M^T k=j)norm(a_D(k))_2^2.
$
If $d$ is uniform on all of $G^n$, independently of everything else, character orthogonality removes those
cross terms,
$
EE_d[norm(U_(M,d)(j))_2^2]
=sum_(k:M^T k=j)norm(a_D(k))_2^2,
$
but published scalar singleton tests do not by themselves constitute a vector-valued Dataset-GL theorem.

== Exact affine energy hash for the vector target

The paired one-hot kernel avoids explicitly materializing a $q$-dimensional target:
$chevron.l Y(X),Y(X') chevron.r=ind[g(X)=g(X')]$.  More generally, let
$
B_j
=sum_(alpha:M^T alpha=j)norm(a_D(alpha))_2^2.
$

#proposition[Affine pair identity][
  Let $X,X'$ be independent draws from $D$, let $Delta=X'-X$, and let $alpha_0$ satisfy
  $M^T alpha_0=j$.  Take the Hermitian inner product to be linear in its first argument.  Then
  $
  B_j
  =q^(n-b) EE[
    chevron.l F(X),F(X') chevron.r
    ind[Delta in "im" M]
    omega_q^(chevron.l j,t_M(Delta) chevron.r)
  ],
  $
  where full column rank makes $t_M(Delta)$ the unique $t$ such that $Delta=M t$ whenever the indicator
  is one.
]<prop:part2-affine-pair>
#proof[
  Expand each squared vector coefficient as
  $
  norm(a_D(alpha))_2^2
  =EE[chevron.l F(X),F(X') chevron.r chi_alpha(X'-X)].
  $
  The fiber ${alpha:M^T alpha=j}$ is $alpha_0+"ker"(M^T)$.  Character orthogonality gives
  $
  sum_(beta in "ker"(M^T))chi_beta(Delta)
  =q^(n-b) ind[Delta in ("ker"(M^T))^perp]
  =q^(n-b) ind[Delta in "im" M].
  $
  When $Delta=M t$, the remaining phase is
  $chi_(alpha_0)(M t)=omega_q^(chevron.l M^T alpha_0,t chevron.r)$.
]

For a random-context target define
$
B_j^"rc"
=sum_(alpha:M^T alpha=j) EE_Z[norm(a_(D_Z)(alpha))_2^2].
$
The same identity holds after also averaging its right-hand side over $Z~mu$, with $X,X'$ conditionally
independent from $D_Z$ given their shared $Z$.  If the label itself depends on context, the paired kernel
becomes $chevron.l F(Z,X),F(Z,X') chevron.r$.

== The affine-oracle barrier

The event $X'-X in "im" M$ is a dense system of $n-b$ linear constraints for a generic $M$.
Prefix-$sans("CSAMP")$ can impose only the coordinate-aligned case in which $"im" M$ is a suffix
subspace: two continuations share an ordinary left prefix and independently regenerate what follows.
It cannot condition future autoregressive choices on a random dense linear system.  Even in the
coordinate case, it forces only the support event; its pair law differs from the independent-pair law
conditioned on that event under nonuniform $D$, as quantified below.
GFast often uses partial-identity coordinate subspaces rather than generic dense $M$.  Except for a group
aligned exactly with the final suffix, those grids still fix and vary noncausal coordinate subsets, and the
published algorithm evaluates every grid point by chosen queries.  Their coordinate structure therefore
does not make prefix-$sans("CSAMP")$ sufficient.

Under the uniform law, a $b$-dimensional subspace accepts an independent pair with probability
$q^(b-n)$, so rejection needs $q^(n-b)$ pairs on average.  More generally, for a uniformly random
$b$-dimensional subspace and any nonzero fixed difference, the exact membership probability is
$
frac(q^b-1,q^n-1).
$
For arbitrary nonuniform $D$ there is no corresponding universal lower bound on the event probability; it
can be zero because of the support of $D$.  For the concrete integer-grid choice $q^b >= S$, $1<S<q$
forces $b=1$: the affine grid itself contains $q=248077$ points, while simulating its pair condition by
natural rejection would impose $127$ dense constraints.

Even a coordinate-aligned energy hash is not identical to the conditional Dataset-GL bucket under a
nonuniform law.  Let $rho(ell)=Pr_D[L=ell]$ be the marginal prefix probability and let $C_j(ell)$ be the
conditional paired suffix score.  Then
$
B_j=q^(n-b)sum_ell rho(ell)^2 C_j(ell),
quad
Psi_j=sum_ell rho(ell) C_j(ell).
$
Prefix-$sans("CSAMP")$ estimates the latter because it samples the prefix once.  With the displayed
normalization $B_j=Psi_j$ exactly under the uniform law; they differ for a general language rollout.

== Consequences at the registered threshold

Part 2 fixes the energy threshold
$
T=tau^2=#(part2-T),
quad "RMS cutoff"=sqrt(T)=#(part2-rms-cutoff).
$
This $T$ is an absolute random-context Dataset-GL bucket energy, not the AGS relative significance
parameter.  AGS compares $|hat(h)(alpha)|^2$ with $tau_"AGS" norm(h)_2^2$; matching our absolute cutoff
would require the distribution-dependent value $tau_"AGS"=T/norm(h_D)_2^2$.  The existing centered root
run is not promoted into Part 2 until a compact checked artifact records its source hash and simultaneous
calibration allowance.  Once inserted, any claim that all $q$ root children survive will carry that artifact
provenance.  Enumerating $q$ requested root items necessarily costs $Omega(q)$ output time.

A published q-SFT can instead bypass the coordinate tree only after changing to its stronger chosen-point
oracle and verifying its scalar exact-sparsity and singleton assumptions.  An affine-difference conditional
pair oracle together with the event probability could support a new vector energy-hashing algorithm, but
that would require a new theorem; it is not an oracle accepted by published AGS, q-SFT, or GFast.

At the registered hypothetical sparsity $S=#(part2-scenario.at("sparsity"))$, storing the $128$
tokenizer-native frequency vectors explicitly in dense coordinate form takes $S n log_2(q)$ bits.  The
noiseless $q$-SFT headline $O(S n)$ query count is about $12.8$ million chosen points.  Substitution into
$S n^2 log_2 q$ gives $29.4$ billion before hidden constants; it is a scaling proxy, not a count of machine
instructions.  Because $S<q$, the registered $b=1$ affine transform has $q$ points.  Its $n$ nonzero
offsets use $n q=31753856$ chosen values, and the fixed zero delay makes the per-group count
$(n+1)q=32001933$; the theorem uses an unspecified $C=O(1)$ groups.  The asymptotic $O(S n)$ notation hides
the present $q/S approx 2.48$ grid rounding.

The noiseless theorem remains scalar and assumes an exactly $S$-sparse transform with iid-uniform random
support.  The robust $q$-SFT analysis takes $n -> infinity$ at fixed $q$, and its phase separation degrades
with large $q$.  GFast's robust result assumes each alphabet size is $O(1)$ and identifies alphabets of
several thousand as future work.  None of these papers provides vector singleton peeling; running scalar
recovery coordinatewise would require $q$ output runs absent a new sketching theorem.

== Part 2 experiment preregistration <sec:part2-experiments>

The machine-readable protocol is `part2-large-q-affine-sft-v1`, dated #(part2-date).  Typst and Python read
the same JSON values.  No fresh lockbox document is used in either audit.

*Exact toy oracle control.*  On $ZZ_(#(part2-toy.at("q")))^(#(part2-toy.at("n")))$, with affine dimension
$b=#(part2-toy.at("b"))$, planted scalar sparsity #(part2-toy.at("sparsity")), and an
#(part2-toy.at("output_dimension"))-dimensional vector check, enumerate the full group and verify
@prop:part2-affine-alias and @prop:part2-affine-pair against brute force.  Run the noiseless affine hashing
and peeling logic of q-SFT only on the planted scalar sparse transform.  Each of
#(part2-toy.at("trials")) registered trials uses #(part2-toy.at("groups")) iid-uniform full-column-rank
matrices, a fixed zero delay plus the standard-basis delays, a uniform size-$S$ support without replacement,
and equal-magnitude complex $q$-th-root coefficients.  Success requires exact support, no false positives,
and coefficient error below $10^(-6)$ in every trial.  This establishes that the implementation matches the
chosen-point mathematics; it makes no claim about the Qwen oracle or vector peeling.

*Qwen affine-oracle audit.*  On #(part2-audit.at("contexts")) development FineWeb contexts, draw two
independent natural length-$128$ continuations sharing each real context.  On
#(part2-audit.at("affine_contexts")) of those contexts, benchmark
#(part2-audit.at("points_per_context")) points on
random and naturally anchored affine lines.  Report autoregressive log mass, normalized importance-weight
effective sample size, affine membership and exact-sequence collisions, chosen-point throughput, and the
projected cost of the full affine grids.  Natural rollouts and chosen affine points are stored in separate
artifact fields.

Finite-field products, phases, affine membership, histograms, and transforms use native compiled Torch
kernels on Modal.  The audit may spend at most #(part2-audit.at("gpu_budget_usd")) additional
GPU-equivalent dollars.  It does not launch
tens of millions of teacher evaluations.  Its artifact status is `oracle_mismatch` and its frequencies, if
any, are rejected by the certified Fourier-student loader.

#let part2-results-ready = true
#if part2-results-ready [
  #include "../experiments/results/qwen35_argl/part2_results.typ"
] else [
  #block(stroke: 0.6pt + gray, inset: 8pt)[
    *Part 2 preregistered; results not yet inserted.*  This box is replaced only from the checked Modal
    artifact whose protocol hash matches `part2-large-q-affine-sft-v1`.
  ]
]
