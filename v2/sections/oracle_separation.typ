#import "../../levs-commands/main_commands.typ": *
#import "../../custom_commands.typ": *
#show: thmrules
#show: eqrules
#set math.equation(numbering: none)

= An Oracle Separation for Large-$q$ Dataset GL <sec:oracle-separation>

The reverse-time tree of @thm:ar-qary-gl spends $Omega(q)$ conditional-sampling experiments per live
parent, because it branches over all $q$ frequencies of each coordinate.
A natural question is whether finite-group sparse Fourier transform (SFT) algorithms can bypass the tree.
Akavia--Goldwasser--Safra and its finite-abelian exposition find scalar significant coefficients with
polylogarithmic group-size dependence, but they assume *chosen evaluations* of the analyzed function and
have polynomial dependence on relative significance, requested accuracy, and
$norm(f)_infinity/norm(f)_2$ @akavia2003proving @galbraith2016finding.
For the density-weighted table $h_D=q^n D F$ below, that norm ratio can be as large as $sqrt(q^n)$, so
the reduction need not be tractable.
The $q$-SFT and GFast algorithms use designed affine grids and singleton peeling, together with exact
sparsity and random-support assumptions @erginbas2023efficiently @tsui2025efficient.
None receives only natural samples and ordinary prefix-conditional continuations.
This section derives the exact oracle reduction, and shows that the generative oracle of
@sec:generative-oracle is strictly weaker than the oracle those algorithms require.

== What is sampled, and what is only read

Fix one real context $z$, and let the autoregressive law $calD_z$ generate continuations one token at a
time.  At a reverse-time node, the realized left prefix is cached and its KV state is forked to sample two
independent new suffixes.  Those are genuine conditional samples from $calD_z$.

After a complete $n$-token string $x$ has been obtained, a separate forward receives $x$ alone and returns
$
F(x)=P_theta^"tok"(dot|x).
$
This is the distribution of token $n+1$ after the context lens; token $n+1$ is not sampled for the vector
label.  Let $g(x)=arg max_t F_t(x)$.  The hard target is $Y(x)=e_(g(x))$, while $F(x)$ itself is the
secondary soft target.

An affine SFT query is different.  It chooses all of
$x=d+M ell in ZZ_q^n$ algebraically, then asks for the function value at that point.  The teacher can in
principle teacher-force the chosen string to score $D_z(x)$, its autoregressive mass for the fixed context
$z$, and can separately return $F(x)$, but the chosen $x$ is not a natural rollout.  We call that stronger
operation a *density-and-label point query* and never silently identify it with prefix-conditional
sampling.

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
law.  Its transform is $EE_Z[a_(D_Z)(k)]$, whose squared norm can differ arbitrarily from the
random-context RMS target $EE_Z[norm(a_(D_Z)(k))_2^2]$ of @thm:random-context-vector-gl.  Consequently no
SFT of the marginal table solves the random-context RMS problem; the shared-$Z$ pair identity below is the
relevant one.

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
]<prop:affine-alias>
#proof[
  Fourier inversion gives $h_D(x)=sum_k a_D(k)chi_k(x)$.  Substitute $x=d+M ell$ and use
  $chevron.l k,M ell chevron.r=chevron.l M^T k,ell chevron.r$.  The normalized character sum over
  $ell$ is one exactly when $M^T k=j$ and zero otherwise.  The displayed positive offset phase follows
  from the negative-exponent forward-transform convention.
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
]<prop:affine-pair>
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
Prefix-conditional sampling can impose only the coordinate-aligned case in which $"im" M$ is a suffix
subspace: two continuations share an ordinary left prefix and independently regenerate what follows.
It cannot condition future autoregressive choices on a random dense linear system.  Even in the
coordinate case, it forces only the support event; its pair law differs from the independent-pair law
conditioned on that event under nonuniform $D$.

GFast often uses partial-identity coordinate subspaces rather than generic dense $M$ @tsui2025efficient.
Except for a group aligned exactly with the final suffix, those grids still fix and vary noncausal
coordinate subsets, and the published algorithm evaluates every grid point by chosen queries.  Their
coordinate structure therefore does not make prefix-conditional sampling sufficient.

Under the uniform law, a $b$-dimensional subspace accepts an independent pair with probability
$q^(b-n)$, so rejection needs $q^(n-b)$ pairs on average.  More generally, for a uniformly random
$b$-dimensional subspace and any nonzero fixed difference, the exact membership probability is
$
frac(q^b-1,q^n-1).
$
For arbitrary nonuniform $D$ there is no corresponding universal lower bound on the event probability; it
can be zero because of the support of $D$.

Even a coordinate-aligned energy hash is not identical to the conditional Dataset-GL bucket under a
nonuniform law.  Let $rho(ell)=Pr_D[L=ell]$ be the marginal prefix probability and let $C_j(ell)$ be the
conditional paired suffix score.  Then
$
B_j=q^(n-b)sum_ell rho(ell)^2 C_j(ell),
quad
Psi_j=sum_ell rho(ell) C_j(ell).
$
Prefix-conditional sampling estimates the latter because it samples the prefix once.  With the displayed
normalization $B_j=Psi_j$ exactly under the uniform law; they differ for a general language rollout.

== Consequences

+ *Output cost is unavoidable.*  Enumerating $q$ requested frequencies at one coordinate costs
  $Omega(q)$ output time regardless of the algorithm; no SFT bypass removes it.
+ *AGS significance is relative.*  Akavia--Goldwasser--Safra compares $|hat(h)(alpha)|^2$ with
  $tau_"AGS" norm(h)_2^2$; matching an absolute bucket-energy cutoff $T$ would require the
  distribution-dependent value $tau_"AGS"=T/norm(h_D)_2^2$, which is not known before the search.
+ *Scalar versus vector.*  The noiseless $q$-SFT theorem is scalar and assumes an exactly $S$-sparse
  transform with iid-uniform random support @erginbas2023efficiently; its robust analysis takes
  $n -> infinity$ at fixed $q$, with phase separation degrading at large $q$.  GFast's robust result
  assumes each alphabet size is $O(1)$ and identifies alphabets of several thousand as future work
  @tsui2025efficient.  None provides vector singleton peeling; running scalar recovery coordinatewise
  costs $q$ output runs absent a new sketching theorem.
+ *The open problem.*  An affine-difference conditional pair oracle, together with event-probability
  bounds, could support a new vector energy-hashing algorithm.  That is a new oracle and a new theorem;
  it is not an oracle accepted by published AGS, $q$-SFT, or GFast.
