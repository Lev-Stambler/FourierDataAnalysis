#import "../levs-commands/main_commands.typ": *
#import "../custom_commands.typ": *
#show: thmrules
#show: eqrules
#set math.equation(numbering: none)

== Autoregressive Categorical Dataset GL from a Fixed Prefix

We now give the construction used by the long-context experiment.
It differs from the indexed-corpus instantiation below in two essential ways.
First, a datapoint is a length-$n$ continuation sampled autoregressively from one fixed real prefix; it is
not a window retrieved from a flat corpus.
Second, every token position is one $q$-ary variable.
There is no binary encoding of token ids, no bit-degree, and no Walsh parity across pieces of a token.

The key observation is an ordering observation.
The context-conditioning tree of @thm:context-gl need not split coordinates oldest-first.
If it splits them *newest-first*, then the coordinates left fixed by a bucket are always an ordinary left
prefix.
Conditional sampling is therefore exactly autoregressive continuation: cache the fixed prefix and draw a
fresh suffix.

The classical finite-group literature already gives query algorithms for significant Fourier coefficients
over arbitrary finite abelian groups @akavia2003proving, and recent $q$-ary sparse Fourier algorithms handle
high-dimensional, high-degree spectra directly over $ZZ_q^n$ @erginbas2023efficiently.
Those algorithms use chosen function evaluations under the uniform group law.
Our point here is different: the measure is the non-uniform autoregressive rollout law, every evaluated
continuation is sampled from that law, and the extra resource is prefix-conditional sampling.

=== The categorical Fourier basis

Fix any integer $q >= 2$ and write $G = ZZ_q$ additively.
Primal token values and Fourier indices both lie in $G$, but they play different roles.
Let
$
omega_q = exp(2 pi i / q),
quad
psi_a (v) = omega_q^(a v),
quad a,v in G.
$
For $alpha = (alpha_1, dots, alpha_n) in G^n$ and $x = (x_1, dots, x_n) in G^n$, define
$
chi_alpha (x)
= product_(j = 1)^n psi_(alpha_j) (x_j)
= omega_q^(sum_(j = 1)^n alpha_j x_j).
$
The *categorical degree* is
$
deg(alpha) = |{j in [n] : alpha_j != 0}|.
$
Thus one token position contributes either zero or one to the degree, even when $q$ is large.
A degree-$n$ coefficient is allowed; none of the algorithm or theorem truncates degree.

#lemma[Categorical character identities][
  For all $alpha, beta, x, x' in G^n$:
  + $|chi_alpha (x)| = 1$;
  + $q^(-n) sum_(x in G^n) chi_alpha (x) overline(chi_beta (x)) = ind[alpha = beta]$;
  + $sum_(alpha in G^n) chi_alpha (x) overline(chi_alpha (x')) = q^n ind[x = x']$.
]<lem:qary-identities>
#proof[
  In one coordinate, $sum_(v in G) omega_q^((a-b)v)$ equals $q$ when $a=b$ and zero otherwise.
  Tensoring this identity over the $n$ token positions proves orthogonality.
  Applying the same geometric sum in the dual variable $a$ proves the reproducing-kernel identity.
  Unit modulus is immediate from the definition.
]

This is a genuinely categorical basis.
For example, with $q=512$, $alpha_j$ is one of $512$ categorical frequencies at token position $j$;
it is not a collection of nine Boolean frequencies.
The group labeling of a vocabulary is a modeling choice, so individual coefficients should be checked under
fixed random permutations of the token ids.
The support $"supp"(alpha) = {j : alpha_j != 0}$ and its token-degree never refer to an encoding bit.
For real-valued targets, $hat(f)(-alpha) = overline(hat(f)(alpha))$; an implementation may store the
conjugate pair or its equivalent real sine/cosine pair without changing the categorical variables.

=== The fixed-prefix rollout distribution

Fix a real token prefix $pi$ once and for all, a continuation length $n$ (in the experiment, $n=128$),
and a randomized autoregressive sampling kernel
$
Q_theta (dot | pi, x_1, dots, x_(j-1)) in Delta(G).
$
$Q_theta$ may use a temperature or a fixed vocabulary restriction.
It need not equal the raw probability vector that we wish to label.
The induced rollout law on $G^n$ is
$
calD_(pi,theta)^n (x)
= product_(j = 1)^n Q_theta (x_j | pi, x_1, dots, x_(j-1)).
$
This law is generally far from uniform and far from a product law.

Let the bounded terminal label be any deterministic function of the fixed prefix and continuation,
$
f : G^n -> CC,
quad norm(f)_infinity <= 1.
$
For next-token distillation the vector target is the raw teacher distribution after the full continuation,
$
F(x) = P_theta (dot | pi, x) in Delta(G).
$
Its scalar coordinates, or bounded transforms such as $2 P_theta(t | pi,x)-1$, fit the theorem below.
The sampling law $Q_theta$ and labeling law $P_theta$ may differ: Dataset GL only requires samples from
$calD_(pi,theta)^n$ and bounded labels on those samples.

#proposition[Autoregressive continuation is prefix-CSAMP][
  Let $r in {0, dots, n}$ and let $z = (z_1, dots, z_r)$ be a prefix with positive
  $calD_(pi,theta)^n$ probability.
  Then a conditional sample
  $
  X ~ calD_(pi,theta)^n | X_(1:r) = z
  $
  is obtained exactly by fixing $X_(1:r)=z$ and sampling, for $j=r+1,dots,n$,
  $
  X_j ~ Q_theta (dot | pi,z,X_(r+1:j-1)).
  $
  The prefix state may be computed once in a KV cache and forked to produce independent conditional suffixes.
]<prop:ar-csamp>
#proof[
  In the autoregressive factorization of $calD_(pi,theta)^n$, the factors for coordinates $1,dots,r$
  are constant after conditioning on $X_(1:r)=z$ and cancel with the normalizing probability of $z$.
  The remaining conditional density is exactly
  $
  product_(j = r+1)^n Q_theta (x_j | pi,z,x_(r+1:j-1)),
  $
  which is the stated continuation procedure.
  A KV cache is only an exact computational reuse of the deterministic prefix activations, so it does not
  alter this distribution.
]

One ordinary rollout supplies a sample from $calD_(pi,theta)^n$.
During that rollout the model also emits the labels
$P_theta(dot|pi)$, $P_theta(dot|pi,X_1)$, and so on before each sampled token.
These are valid labels for the *separate marginal problems* of lengths $0,1,dots,n-1$.
They must not be pooled as if they were independent length-$n$ datapoints.
For the context lens $n=128$, the terminal query $P_theta(dot|pi,X_(1:128))$ is the label of the
length-$128$ datapoint; the shorter-prefix logits are free auxiliary datasets at shorter lenses.

=== Dataset coefficients under an arbitrary rollout law

For a scalar target define the categorical dataset coefficient
$
hat(f)_calD (alpha)
= EE_(X ~ calD_(pi,theta)^n) [f(X) overline(chi_alpha (X))].
$
We split positions in reverse temporal order.
At level $k$, let
$
J_k = {n-k+1, dots, n},
quad
overline(J)_k = {1, dots, n-k}.
$
A node is a categorical frequency assignment $a in G^(J_k)$ on the newest $k$ positions.
The unsplit context $Z=X_(overline(J)_k)$ is the ordinary prefix of length $n-k$, while
$Y=X_(J_k)$ is its random continuation.
Define
$
v_a (z)
= EE[f(X) overline(chi_a (Y)) | Z=z],
quad
Psi_k(a) = EE_Z [|v_a(Z)|^2].
$

#lemma[Categorical conditional-bucket identities for weighted data][
  For every level $k$ and node $a in G^(J_k)$:
  + *(Tower and completeness.)* For every $b in G^(overline(J)_k)$,
    $
    hat(f)_calD (b,a)
    = EE_Z [overline(chi_b(Z)) v_a(Z)],
    quad
    |hat(f)_calD (b,a)|^2 <= Psi_k(a).
    $
  + *(Termination.)* $Psi_n(alpha) = |hat(f)_calD(alpha)|^2$.
  + *(Monotonicity.)* Every one of the $q$ children $(c,a)$ at level $k+1$ satisfies
    $Psi_(k+1)(c,a) <= Psi_k(a)$.
  + *(Weighted level mass.)* Writing
    $p_z(y) = Pr[Y=y | Z=z]$,
    $
    sum_(a in G^(J_k)) Psi_k(a)
    = q^k EE_Z [sum_(y in G^k) p_Z(y)^2 |f(Z,y)|^2]
    =: R_k.
    $
    In particular,
    $
    R_k <= q^k norm(f)_infinity^2 EE_Z [norm(p_Z)_2^2].
    $
]<lem:qary-weighted-buckets>
#proof[
  For the tower identity, condition the coefficient on $Z$ and pull out the factor
  $overline(chi_b(Z))$.
  Cauchy--Schwarz then gives
  $
  |hat(f)_calD(b,a)|^2
  <= EE_Z[|chi_b(Z)|^2] EE_Z[|v_a(Z)|^2]
  = Psi_k(a),
  $
  where the last equality uses the pointwise unit-modulus property from @lem:qary-identities.

  At $k=n$ the context is empty, so $v_alpha=hat(f)_calD(alpha)$, proving termination.

  For monotonicity, moving coordinate $n-k$ from $Z$ into $Y$ coarsens the conditioning sigma-field.
  For child frequency $c$, the finer conditional mean is the parent's conditional mean multiplied by the
  fixed unit-modulus value $overline(psi_c(X_(n-k)))$.
  Conditional Jensen after coarsening shows that its mean square cannot increase.

  Finally fix $z$.
  The values $v_a(z)$ are the uniform-group Fourier transform of the function
  $h_z(y)=p_z(y) f(z,y)$.
  Parseval on $G^k$ gives
  $
  sum_a |v_a(z)|^2 = q^k sum_y p_z(y)^2 |f(z,y)|^2.
  $
  Averaging over $Z$ proves the identity and the displayed bound.
]

The weighted level mass is the correct extension of the empirical-set formula in @lem:psi-properties.
If a conditional fiber contains $m_z$ equally likely rows, then $p_z(y)=1/m_z$ on that fiber and its
collision term is $sum_y p_z(y)^2=1/m_z$.
For an autoregressive law it is instead the conditional collision probability of two independent suffixes.

#lemma[Unbiased KV estimator][
  For a node $a in G^(J_k)$:
  $
  Psi_k(a)
  = EE_Z EE_(X,X' "i.i.d." ~ calD_(pi,theta)^n | Z)
    [f(X) overline(f(X')) overline(chi_a(Y)) chi_a(Y')].
  $
  One experiment consists of a rollout $X$, followed by an independent resampling of its suffix $Y'$ from
  the cached prefix $Z$ as in @prop:ar-csamp.
  The estimator has modulus at most one, so its real and imaginary parts are estimated to additive
  $plus.minus eps$ with $O(log(1/delta) / eps^2)$ experiments.
]<lem:qary-kv-estimator>
#proof[
  Conditional on $Z=z$, independence makes the expectation factor as
  $v_a(z) overline(v_a(z))=|v_a(z)|^2$.
  Average over $Z$.
  The modulus bound follows from $norm(f)_infinity <= 1$ and $|chi_a|=1$.
  Hoeffding applies separately to the real and imaginary parts.
]

#lemma[One categorical DFT estimates all $q$ children][
  Fix a level-$k$ node $a$ and write a pair of suffix draws from the level-$(k+1)$ context as
  $X=(Z,A,Y)$ and $X'=(Z,A',Y')$, where $A,A' in ZZ_q$ are coordinate $n-k$ and
  $Y,Y' in G^k$ are the newest $k$ tokens.
  Put
  $
  B = f(X) overline(f(X')) overline(chi_a(Y)) chi_a(Y'),
  quad
  D = A'-A mod q,
  $
  and define the $q$ empirical bins
  $
  h(d) = EE[B ind[D=d]],
  quad d in ZZ_q.
  $
  Then every child's bucket is one entry of the categorical Fourier transform
  $
  Psi_(k+1)(c,a) = sum_(d in ZZ_q) h(d) omega_q^(c d),
  quad c in ZZ_q.
  $
  Consequently $E=O(log(q/delta)/eps^2)$ shared suffix-pair experiments estimate all $q$ children
  to additive $plus.minus eps$ simultaneously; the postprocessing costs $O(E+q log q)$ with a native
  length-$q$ DFT and introduces no Boolean variables.
]<lem:qary-all-children>
#proof[
  The paired estimator for child $(c,a)$ is
  $
  B overline(psi_c(A)) psi_c(A')
  = B omega_q^(c(A'-A)).
  $
  Partition its expectation by $D=A'-A$ to obtain the displayed transform.
  Each transformed sample has modulus at most one, so Hoeffding and a union bound over the $q$ children
  give the simultaneous bound.
]

=== High-degree categorical Dataset GL

For threshold $tau$, define the live width
$
N_k = |{a in G^(J_k) : Psi_k(a) >= tau^2 / 4}|,
quad
N = max_(0 <= k <= n) N_k.
$
By @lem:qary-weighted-buckets,
$N_k <= 4 R_k / tau^2$.

#theorem[Fixed-prefix autoregressive categorical Dataset GL][
  Fix $q >= 2$, a continuation length $n$, a real prefix $pi$, and autoregressive sampling access to
  $calD_(pi,theta)^n$ through $Q_theta$.
  Given bounded labels $f:G^n -> CC$ with $norm(f)_infinity <= 1$, threshold $tau in (0,1]$, and
  failure probability $delta$, the reverse-time $q$-ary tree search outputs a list $L subset.eq G^n$ such
  that, with probability at least $1-delta$:
  + $|hat(f)_calD(alpha)| >= tau ==> alpha in L$;
  + $alpha in L ==> |hat(f)_calD(alpha)| >= tau/2$.

  It performs at most
  $
  O(n N dot log(n q N / delta) / tau^4)
  $
  KV conditional-sampling experiments and runs in time polynomial in
  $n,q,N,1/tau,log(1/delta)$ plus the cost of the autoregressive rollouts.
  The guarantee permits arbitrary categorical degree up to $n$ and requires no binary representation of
  $G$.
]<thm:ar-qary-gl>
#proof[
  Start from the empty assignment at $k=0$.
  At level $k$, expand every live node to its $q$ possible frequencies for coordinate $n-k$.
  Use the shared histogram and categorical DFT of @lem:qary-all-children to estimate all $q$ child buckets
  to accuracy $tau^2/8$, and retain a child when its estimate is at least $3 tau^2/8$.
  Give the $r$-th parent expansion total failure probability $delta/(2r^2)$ (including its union over $q$
  children), so a union bound covers the adaptive and a-priori unbounded search.

  If $|hat(f)_calD(alpha)| >= tau$, tower completeness says every reverse-time ancestor has
  $Psi >= tau^2$; on the good estimation event none is pruned.
  A surviving leaf has true $Psi_n >= tau^2/4$, and termination gives
  $|hat(f)_calD(alpha)| >= tau/2$.
  On the same event every surviving node belongs to the corresponding $N_k$ set.
  There are at most $n N$ live-parent expansions, and simultaneous accuracy $tau^2/8$ for their $q$
  children costs $O(log(n q N/delta)/tau^4)$ shared experiments per expansion.
]

The dependence is on the live spectrum $N$, not on $deg(alpha)$.
This is the sense in which the procedure is a high-degree categorical GL: a coefficient supported on all
$128$ token positions is recovered whenever it is heavy and its ancestors fit within the live-width profile.
Large $q$ increases the branching factor but does not create hidden Boolean variables.

#corollary[One-shot next-token distribution search][
  Let $F:G^n -> CC^q$ satisfy $norm(F(x))_2 <= 1$ for every $x$; in particular this holds for a probability
  vector $F(x)=P_theta(dot|pi,x)$.
  Define
  $
  hat(F)_calD(alpha) = EE[F(X) overline(chi_alpha(X))]
  $
  and replace $Psi_k(a)$ by
  $
  Psi_k^F(a)
  = EE_Z [norm(EE[F(X) overline(chi_a(Y)) | Z])_2^2].
  $
  Then the same search lists every $alpha$ with
  $norm(hat(F)_calD(alpha))_2 >= tau$, with soundness threshold $tau/2$ and the same asymptotic
  complexity.
]<cor:ar-qary-vector>
#proof[
  Apply the scalar identities coordinatewise and sum them.
  Cauchy--Schwarz holds in $CC^q$, termination is exact, and the paired estimator replaces scalar
  multiplication by the Hermitian inner product.
  Its magnitude is at most $norm(F(X))_2 norm(F(X'))_2 <= 1$.
]

=== What is and is not proved for the implementation

+ *Proved:* full rollouts sampled with the KV loop are draws from $calD_(pi,theta)^n$; resampling a suffix
  from the cached left prefix is exact $sans("CSAMP")$; the reverse-time $q$-ary search recovers every heavy
  categorical coefficient subject to the stated live-width complexity.
+ *Proved:* the raw next-token probability vector may be used as the label even if generation uses a
  temperature or restricted sampling kernel, because it is simply a bounded function on the sampled
  continuation.
+ *Not proved:* treating the logits harvested at $128$ successive time steps as $128$ i.i.d. rows of one
  length-$128$ Dataset-GL problem.  They are labels for nested prefix distributions and are dependent.
+ *Not proved:* truncating a standard model's KV cache to a sliding window unless the model's attention rule
  itself makes the next-token label a function only of that window.
+ *Not required:* exact repeated $128$-grams in a pre-existing corpus.  The generative conditional oracle
  creates independent suffixes from a realized prefix on demand; its difficulty is measured by the weighted
  collision profile $R_k$ and live width $N$, not by accidental duplicates in a flat file.
