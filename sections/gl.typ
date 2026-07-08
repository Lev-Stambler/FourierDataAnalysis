#import "../levs-commands/main_commands.typ": *
#show: thmrules
#show: eqrules
#set math.equation(numbering: none)

// ============ Reusable Symbols ============
#let inD = $calD$
#let normC = $C_calD$
#let normCInv = $C_calD^(-1)$
#let Jbar = $overline(J)$
#let ind = $bb(1)$
#let heavyB = $B_theta$
#let dcoeff(S) = $hat(f)_calD (#S)$
#let ucoeff(S) = $hat(f) (#S)$

= Goldreich-Levin over Datasets

We now adapt the Goldreich-Levin (GL) algorithm (@o2021analysis, Section 3.5) to the dataset setting.
Throughout this section we work over the Boolean cube $Omega^n = {-1, 1}^n$ with the parity characters $chi_S (x) = product_(i in S) x_i$ for clarity of exposition; we remark on general finite alphabets at the end.
#footnote[
  All identities below that only use orthonormality and the "reproducing kernel" property $sum_S chi_S (x) chi_S (x') = 2^n dot ind [x = x']$ extend verbatim to any finite abelian group with its character basis (with $chi_S overline(chi_T)$ in place of $chi_S chi_T$), and more generally to product orthonormal bases over finite sets.
]

Recall the dataset Fourier coefficient
$
dcoeff(S) = EE_(x ~ calD) [f(x) chi_S (x)] = frac(1, |calD|) sum_(x in calD) f(x) chi_S (x),
$
and the normalization constant $normC = frac(2^n, |calD|)$.
The algorithmic goal, mirroring the classical GL guarantee, is:

#definition[Dataset Heavy-Coefficient Search][
  Given $0 < tau <= 1$ and access to $f$ and $calD$ (in a model to be specified), output a list $L$ of subsets of $[n]$ such that with high probability:
  + (Completeness) $|dcoeff(S)| >= tau ==> S in L$;
  + (Soundness) $S in L ==> |dcoeff(S)| >= tau \/ 2$.
]<defn:dataset-gl-goal>

== Access Models

The choice of access model turns out to be the crux of the problem.
We distinguish three oracles:
+ *Samples* ($sans("SAMP")$): i.i.d. draws $x ~ calD$ together with the label $f(x)$.
+ *Membership* ($sans("MEM")$): the in-distribution tester $x |-> calD(x) in {0, 1}$, evaluable on _arbitrary_ $x in {-1,1}^n$.
+ *Model queries* ($sans("QUERY")$): $f$ evaluable at arbitrary $x in {-1, 1}^n$, including out-of-distribution points.
  This is the natural model when $f$ is a trained model or some other efficiently computable function.

Classical GL uses only $sans("QUERY")$, and its guarantee concerns the _global_ (uniform-measure) coefficients $ucoeff(S)$.
We will see that for the _dataset_ coefficients $dcoeff(S)$, samples alone are provably insufficient (@lem:blindness), and that the positive results route the dataset structure through a single object: the dataset's _bias spectrum_ (@lem:convolution).

== Recap: the Classical Goldreich-Levin Algorithm

#theorem[Goldreich-Levin; @o2021analysis Section 3.5][
  Given query access to $f : {-1,1}^n -> {-1,1}$ and $0 < tau <= 1$, there is a $"poly"(n, 1\/tau)$-time algorithm that with high probability outputs a list $L = {U_1, dots, U_ell}$ of subsets of $[n]$ such that:
  + $|ucoeff(U)| >= tau ==> U in L$;
  + $U in L ==> |ucoeff(U)| >= tau \/ 2$.
  By Parseval's theorem, the second guarantee implies $|L| <= 4 \/ tau^2$.
]<thm:classical-gl>

The engine of the algorithm is a divide-and-conquer search over the tree of coordinate prefixes.
For $S subset.eq J subset.eq [n]$, define the *bucket weight* (@o2021analysis, Definition 3.39)
$
W^(S | J) [f] = sum_(T subset.eq Jbar) ucoeff(S union T)^2,
$
the Fourier weight of $f$ on sets whose restriction to $J$ equals $S$.
The algorithm maintains a collection of buckets of weight $>= tau^2 \/ 2$ (at most $4 \/ tau^2$ of them, by Parseval), splitting one coordinate at a time until each surviving bucket is a singleton ${S}$ with $ucoeff(S)^2 >= tau^2 \/ 2$.
Its feasibility rests on the restriction identity (@o2021analysis, equation (3.5) and Proposition 3.40)
$
W^(S | J) [f] = EE_(z ~ {-1,1}^(Jbar)) [hat(f)_(J | z) (S)^2] = EE_(z ~ {-1,1}^(Jbar)) space EE_(y, y' ~ {-1,1}^J) [f(y, z) chi_S (y) dot f(y', z) chi_S (y')],
$
which lets a $sans("QUERY")$ algorithm estimate any bucket weight to accuracy $plus.minus eps$ with $O(log(1 \/ delta) \/ eps^2)$ queries.

Two remarks that we will use later.
First, the proof only uses $norm(f)_infinity <= 1$ (for the Chernoff estimates) and $norm(f)_2 <= 1$ (for the Parseval pruning bound), so @thm:classical-gl holds verbatim for $f : {-1,1}^n -> [-1, 1]$, with list size $4 norm(f)_2^2 \/ tau^2$.
Second, the running time is polynomial in $1 \/ tau$; this will be the quantity that blows up when we naively lift a sparse dataset.

== Obstructions

Over a dataset, the characters ${chi_S}$ are _not_ orthonormal with respect to the measure $"Unif"(calD)$, and the familiar Fourier toolkit breaks in quantifiable ways.
The first casualty is Parseval.

#lemma[Mass Identity][
  For any $calD subset.eq {-1,1}^n$ (distinct points) and any $f : calD -> RR$,
  $
  sum_(S subset.eq [n]) dcoeff(S)^2 = normC dot EE_(x ~ calD) [f(x)^2].
  $
]<lem:mass>
#proof[
  Expanding the square and swapping sums,
  $
  sum_S dcoeff(S)^2 = frac(1, |calD|^2) sum_(x, x' in calD) f(x) f(x') sum_(S subset.eq [n]) chi_S (x) chi_S (x').
  $
  Since $chi_S (x) chi_S (x') = chi_S (x compose x')$ where $(x compose x')_i = x_i x'_i$, the inner sum is $2^n dot ind [x = x']$ (the reproducing kernel of the character basis).
  Only the diagonal survives:
  $
  sum_S dcoeff(S)^2 = frac(2^n, |calD|^2) sum_(x in calD) f(x)^2 = normC dot EE_(x ~ calD) [f(x)^2].
  $
]

Since $normC$ is exponentially large for small datasets, the total dataset Fourier mass is _not_ bounded by $EE_calD [f^2]$: Parseval fails over $calD$ by exactly the factor $normC$.
#TODO[
  @lem:mass contradicts the "Parseval for Dataset Closeness" lemma of the previous section, which asserts $EE_(x ~ calD) [(f - g)^2] = sum_alpha (hat(f)_calD (alpha) - hat(g)_calD (alpha))^2$; the right-hand side is in fact $normC$ times the left.
  The basis ${phi_alpha}$ is orthonormal in $L^2 (Omega^n, mu)$, not in $L^2 (calD)$.
  The "Learning Low-Degree Fourier Functions" theorem built on that lemma needs to be revisited as well.
]

Two immediate consequences.
First, the number of $tau$-heavy coefficients can be as large as $normC dot EE_calD [f^2] \/ tau^2$, i.e., exponential; so no analogue of the list-size bound $|L| <= 4 \/ tau^2$ can hold unconditionally.
The subcube example shows this is not a pathology of weird functions --- the constant function already exhibits it.

#example[
  Let $K subset.eq [n]$ and let $calD = {x : x_i = 1 "for all" i in K}$ be the subcube fixing $K$, with $f equiv 1$.
  Then for every $S subset.eq K$ we have $chi_S equiv 1$ on $calD$, hence
  $
  dcoeff(S) = cases(1 &"if" S subset.eq K, 0 &"otherwise,")
  $
  and there are $2^(|K|)$ maximal coefficients.
  On a dataset, distinct characters can be _identical_ as functions on $calD$; "the" heavy set is only defined up to this aliasing.
]<ex:subcube>

Second --- and more damning for the GL strategy --- the bucket weights themselves carry _no information_ over a generic dataset.
The following exact identity is the dataset analogue of the restriction identity above.

#lemma[Pair Form of Dataset Bucket Weights][
  Fix $J subset.eq [n]$ with $|J| = k$ and $S subset.eq J$. Then
  $
  sum_(U subset.eq Jbar) dcoeff(S union U)^2 = 2^(n - k) dot EE_(x, x' ~ calD) [f(x) f(x') chi_S (x_J) chi_S (x'_J) dot ind [x_(Jbar) = x'_(Jbar)]].
  $
]<lem:pair-form>
#proof[
  Expand each $dcoeff(S union U) = EE_(x ~ calD) [f(x) chi_S (x_J) chi_U (x_(Jbar))]$, square, and swap sums:
  $
  sum_(U subset.eq Jbar) dcoeff(S union U)^2 = EE_(x, x' ~ calD) [f(x) f(x') chi_S (x_J) chi_S (x'_J) sum_(U subset.eq Jbar) chi_U (x_(Jbar)) chi_U (x'_(Jbar))],
  $
  and the inner sum is the reproducing kernel $2^(n-k) dot ind [x_(Jbar) = x'_(Jbar)]$ on ${-1,1}^(Jbar)$.
]

#lemma[Blindness of Dataset Bucket Weights][
  In the setting of @lem:pair-form, suppose the projection $x |-> x_(Jbar)$ is injective on $calD$ (no two dataset points collide on $Jbar$).
  Then for _every_ $S subset.eq J$,
  $
  sum_(U subset.eq Jbar) dcoeff(S union U)^2 = frac(2^(n-k), |calD|) dot EE_(x ~ calD) [f(x)^2],
  $
  independent of $S$.
]<lem:blindness>
#proof[
  Under injectivity, $ind [x_(Jbar) = x'_(Jbar)] = ind [x = x']$ on $calD times calD$, so only the diagonal of @lem:pair-form survives, and $chi_S (x_J)^2 = 1$.
]

In words: as long as the dataset's projection onto the un-split coordinates is collision-free, _every_ bucket at level $k$ has exactly the same weight, no matter what $f$ is and no matter where its heavy dataset coefficients lie.
For a dataset of $m$ points in general position (e.g., $m$ uniformly random points), a union bound gives collisions on $Jbar$ with probability at most $binom(m, 2) \/ 2^(n - k)$, so the search tree is information-free for all levels
$
k lt.tilde n - 2 log_2 m :
$
the "birthday horizon."
A GL-style search would have to descend blindly through $2^(n - 2 log_2 m)$ buckets before bucket weights begin to distinguish anything.
Together with the subcube example (which shows the target list itself can be exponential), this rules out a sample-only dataset GL: some additional access to $f$ or structural knowledge of $calD$ is necessary.
#footnote[
  A sanity check for @lem:blindness: take $n = 2$, $calD = {(1,1), (-1,-1)}$, $f(x) = x_1$, $J = {1}$.
  Then $dcoeff({1}) = 1$, $dcoeff({1,2}) = 0$, and the bucket weight is $1 = frac(2^(2-1), 2) dot 1$, matching the lemma; the "signal" coefficient and the aliasing noise conspire to a flat bucket profile.
  All identities in this section have additionally been verified numerically on random instances.
]

== The Convolution Identity

The positive results all flow from one structural fact: the dataset spectrum of $f$ is the global spectrum of $f$ _convolved with the spectrum of the dataset itself_.

#definition[Bias Spectrum][
  The *bias spectrum* of $calD$ is the family
  $
  b_V = EE_(x ~ calD) [chi_V (x)], quad V subset.eq [n],
  $
  i.e., the dataset Fourier coefficients of the constant function $1$.
  Note $b_emptyset = 1$ and $|b_V| <= 1$.
  For $theta > 0$ we write $heavyB = {V : |b_V| >= theta}$ for the *$theta$-heavy bias set*.
]<defn:bias-spectrum>

#lemma[Convolution Identity][
  For any $f : {-1,1}^n -> RR$ and any $S subset.eq [n]$,
  $
  dcoeff(S) = sum_(V subset.eq [n]) b_V dot ucoeff(S Delta V),
  $
  where $Delta$ denotes symmetric difference.
]<lem:convolution>
#proof[
  Expand $f$ in its (exact, global) Fourier expansion $f = sum_T ucoeff(T) chi_T$ inside the dataset expectation:
  $
  dcoeff(S) = EE_(x ~ calD) [f(x) chi_S (x)] = sum_T ucoeff(T) dot EE_(x ~ calD) [chi_T (x) chi_S (x)] = sum_T ucoeff(T) dot b_(T Delta S),
  $
  using $chi_T chi_S = chi_(T Delta S)$. Substituting $V = T Delta S$ gives the claim.
]

The bias spectrum is exactly the aliasing structure of the subcube example: there, $b_V = ind [V subset.eq K]$, and the convolution smears the single global coefficient $ucoeff(emptyset) = 1$ onto all $2^(|K|)$ sets $S subset.eq K$.
The blindness of @lem:blindness is the statement that, for a generic dataset, the bias spectrum is an exponentially wide field of $tilde plus.minus 1 \/ sqrt(|calD|)$ noise; the convolution then buries any bucket-level signal.
Conversely, whenever the bias spectrum is _dominated by few heavy terms_, the dataset spectrum of $f$ is a controlled deformation of its global spectrum --- and that is precisely what the main theorem exploits.

== Main Theorem: Dataset GL from Model Queries

We now give the main positive result.
The standing assumptions: $f$ is a queryable model with bounded _spectral norm_ $norm(hat(f))_1 = sum_T |ucoeff(T)|$, and we know the heavy part of the dataset's bias spectrum.
The latter is a structural input about $calD$ alone (not about $f$); we discuss how to obtain it below.

#theorem[Dataset Goldreich-Levin, model-query access][
  Let $f : {-1,1}^n -> [-1,1]$ with $norm(hat(f))_1 <= A$ be given by $sans("QUERY")$ access, and let $calD$ be given by $sans("SAMP")$ access.
  Let $0 < tau <= 1$, $delta in (0,1)$, fix any $theta <= tau \/ (4A)$, and suppose the $theta$-heavy bias set $heavyB$ of $calD$ is given explicitly.
  Then there is an algorithm running in time $"poly"(n, |heavyB|, 1\/tau, log(1\/delta))$, using $"poly"(n, |heavyB|, 1\/tau) dot log(1\/delta)$ queries to $f$ and $O(log(|heavyB| \/ (tau delta)) \/ tau^2)$ samples from $calD$, that with probability $>= 1 - delta$ outputs a list $L$ satisfying:
  + (Completeness) $|dcoeff(S)| >= tau ==> S in L$;
  + (Soundness) $S in L ==> |dcoeff(S)| >= tau \/ 2$;
  + $|L| <= frac(64 |heavyB|^3, 9 tau^2)$.
]<thm:dataset-gl>

The algorithm:
+ Run classical GL (@thm:classical-gl, in its $[-1,1]$-valued form) on $f$ with threshold $tau' = frac(3 tau, 4 |heavyB|)$ and confidence $1 - delta\/2$, obtaining a list $F$ of global heavy sets, $|F| <= 4 \/ tau'^2$.
+ Form the candidate list $C = {T Delta V : T in F, V in heavyB}$, so $|C| <= |F| dot |heavyB|$.
+ Draw $m = O(log(|C| \/ delta) \/ tau^2)$ samples from $calD$ and, for each $S in C$, compute the empirical coefficient $tilde(f)_calD (S) = frac(1, m) sum_(j = 1)^m f(x^((j))) chi_S (x^((j)))$.
  Output $L = {S in C : |tilde(f)_calD (S)| >= 3 tau \/ 4}$.

#proof[
  *Decomposition.* By @lem:convolution, for every $S$,
  $
  dcoeff(S) = sum_(V in heavyB) b_V ucoeff(S Delta V) + R_S, quad "where" |R_S| <= sum_(V in.not heavyB) |b_V| |ucoeff(S Delta V)| <= theta dot norm(hat(f))_1 <= theta A <= tau / 4,
  $
  using $|b_V| < theta$ off the heavy set and re-indexing the tail sum over $T = S Delta V$.

  *Completeness.* Suppose $|dcoeff(S)| >= tau$. Then
  $
  sum_(V in heavyB) |b_V| |ucoeff(S Delta V)| >= |dcoeff(S)| - |R_S| >= tau - tau/4 = frac(3 tau, 4),
  $
  and since $|b_V| <= 1$, some $V^* in heavyB$ has $|ucoeff(S Delta V^*)| >= frac(3 tau, 4 |heavyB|) = tau'$.
  By the completeness of classical GL (except with probability $delta \/ 2$), $S Delta V^* in F$, hence $S = (S Delta V^*) Delta V^* in C$.
  By Hoeffding's inequality and a union bound over $C$ (except with probability $delta \/ 2$), every empirical coefficient satisfies $|tilde(f)_calD (S) - dcoeff(S)| <= tau \/ 8$; in particular $|tilde(f)_calD (S)| >= tau - tau\/8 > 3 tau \/ 4$, so $S in L$.

  *Soundness.* If $S in L$ then $|dcoeff(S)| >= 3 tau \/ 4 - tau \/ 8 = 5 tau \/ 8 >= tau \/ 2$.

  *List size and complexity.* $|L| <= |C| <= |F| |heavyB| <= frac(4, tau'^2) |heavyB| = frac(64 |heavyB|^3, 9 tau^2)$.
  Classical GL at threshold $tau'$ costs $"poly"(n, 1\/tau') = "poly"(n, |heavyB|, 1\/tau)$ queries; step 3 costs $m$ samples and $m dot |C|$ arithmetic operations.
]

Note that the algorithm uses only the _set_ $heavyB$, not the values $b_V$; and the theorem is meaningful precisely in the distribution-shift regime where $dcoeff(S)$ and $ucoeff(S)$ differ --- the shift is routed through the dataset's heavy biases.
The same completeness argument, run purely combinatorially (Parseval bounds the number of $tau'$-heavy global coefficients by $1 \/ tau'^2$), yields an unconditional structural bound:

#corollary[List-Size Bound under Sparse Bias Spectrum][
  If $norm(hat(f))_1 <= A$ and $theta <= tau \/ (4A)$, then
  $
  |{S : |dcoeff(S)| >= tau}| <= frac(16 |heavyB|^3, 9 tau^2).
  $
]<cor:list-size>

== Companion Results

#h3([Dense Datasets: the Membership-Oracle Route])

When the dataset is _dense_ --- $normC = "poly"(n)$ --- the naive lifting strategy already works, and moreover supplies the heavy bias set $heavyB$ needed above.
Recall the lifted function $g = calD compose f$, i.e., $g(x) = calD(x) f(x)$, which is evaluable anywhere given $sans("MEM")$ together with the ability to evaluate $f$ on points of $calD$; its global coefficients satisfy $hat(g)(S) = normCInv dcoeff(S)$.

#proposition[Dataset GL for Dense Datasets][
  Given query access to $g = calD compose f$ with $norm(f)_infinity <= 1$, and $0 < tau <= 1$, running classical GL on $g$ with threshold $normCInv tau$ solves the search problem of @defn:dataset-gl-goal in time $"poly"(n, normC, 1\/tau)$, with list size $|L| <= 4 normC EE_calD [f^2] \/ tau^2$.
]<prop:dense>
#proof[
  $g$ takes values in $[-1,1]$, so the $[-1,1]$-valued form of @thm:classical-gl applies at threshold $tau'' = normCInv tau$: completeness and soundness transfer through $hat(g)(S) = normCInv dcoeff(S)$, the running time is $"poly"(n, 1\/tau'') = "poly"(n, normC, 1\/tau)$, and Parseval gives $|L| <= 4 norm(g)_2^2 \/ tau''^2 = 4 normCInv EE_calD [f^2] dot normC^2 \/ tau^2$.
]

The list-size bound matches the mass identity (@lem:mass) exactly, so @prop:dense is tight in general; and by taking $f equiv 1$, the same run recovers the heavy bias set: $b_V = normC dot hat(ind)_calD (V)$, so $heavyB$ is computable in time $"poly"(n, normC, 1\/theta)$ given $sans("MEM")$.
For sparse datasets ($normC$ superpolynomial) this route is unavailable --- consistent with @lem:blindness --- and $heavyB$ must come from prior structural knowledge of the dataset (e.g., known feature constraints, class balance, or template structure).

#h3([Representative Datasets])

Finally, if the dataset's spectrum uniformly tracks the global one --- the "no distribution shift" regime --- dataset GL degenerates gracefully to classical GL plus re-scoring.

#definition[$eps$-Representative Dataset][
  $calD$ is *$eps$-representative* for $f$ if $|dcoeff(S) - ucoeff(S)| <= eps$ for all $S subset.eq [n]$.
]<defn:representative>

#lemma[
  Let $f : {-1,1}^n -> [-1,1]$ and let $calD$ consist of $m$ i.i.d. uniform samples.
  If $m >= frac(2, eps^2) (n ln 2 + ln frac(2, delta))$, then with probability $>= 1 - delta$, $calD$ is $eps$-representative for $f$.
]<lem:representative>
#proof[
  For fixed $S$, $dcoeff(S)$ is an average of $m$ i.i.d. terms $f(x) chi_S (x) in [-1,1]$ with mean $ucoeff(S)$; Hoeffding gives $Pr[|dcoeff(S) - ucoeff(S)| > eps] <= 2 e^(- m eps^2 \/ 2)$.
  Union bound over all $2^n$ sets $S$.
]

#proposition[Dataset GL for Representative Datasets][
  Suppose $calD$ is $(tau\/8)$-representative for $f : {-1,1}^n -> [-1,1]$, and we have $sans("QUERY")$ access to $f$ and $sans("SAMP")$ access to $calD$.
  Then the search problem of @defn:dataset-gl-goal is solvable in time $"poly"(n, 1\/tau, log(1\/delta))$ with $O(log(1 \/ (tau delta)) \/ tau^2)$ samples: run classical GL at threshold $7 tau \/ 8$ to get $F$, estimate $dcoeff(S)$ for $S in F$ to accuracy $tau \/ 8$ from samples, and keep $|tilde(f)_calD (S)| >= 3 tau \/ 4$.
]<prop:representative>
#proof[
  If $|dcoeff(S)| >= tau$ then representativeness gives $|ucoeff(S)| >= tau - tau\/8 = 7 tau \/ 8$, so $S in F$ by GL completeness; its estimate satisfies $|tilde(f)_calD (S)| >= |dcoeff(S)| - tau\/8 >= 7 tau \/ 8 > 3 tau \/ 4$, so $S$ is kept.
  (The representativeness slack is needed only for the GL step; the estimation step compares directly against $dcoeff(S)$.)
  If $S$ is kept then $|dcoeff(S)| >= 3 tau \/ 4 - tau \/ 8 = 5 tau \/ 8 >= tau \/ 2$.
  GL's Parseval bound gives $|F| <= 4 \/ (7 tau \/ 8)^2 <= 7 \/ tau^2$, so a union bound over $F$ needs $O(log(|F| \/ delta) \/ tau^2)$ samples.
]

In this regime the heavy dataset coefficients essentially _are_ the heavy global coefficients; the substantive new content of @thm:dataset-gl lies exactly where representativeness fails.

== Application: Kushilevitz-Mansour over Datasets

Classical GL powers the Kushilevitz-Mansour learning algorithm (@o2021analysis, Theorems 3.37-3.38): any $f$ with $norm(hat(f))_1 <= s$ --- in particular any decision tree of size $s$ --- is learnable from queries in time $"poly"(n, s, 1\/eps)$.
The spectral-norm hypothesis is exactly the $A$ of @thm:dataset-gl, so we get the on-distribution analogue of the coefficient-collection step for free:

#corollary[Heavy On-Dataset Coefficients of Decision Trees][
  Let $f : {-1,1}^n -> {-1,1}$ be computable by a decision tree of size $s$ (so $norm(hat(f))_1 <= s$), given by $sans("QUERY")$ access, and let $heavyB$ be the $theta$-heavy bias set of $calD$ for some $theta <= tau \/ (4s)$.
  Then all $S$ with $|dcoeff(S)| >= tau$ can be listed in time $"poly"(n, s, |heavyB|, 1\/tau, log(1\/delta))$.
]<cor:km-datasets>

We emphasize what @cor:km-datasets does _not_ yet give: an $eps_calD$-close sparse approximation to $f$.
Classically, coefficient collection plus Parseval yields the learning guarantee; over a dataset, Parseval fails (@lem:mass), so bounding $EE_(x ~ calD) [(f - g)^2]$ for a sparse $g$ assembled from the recovered coefficients requires genuinely new arguments.
#TODO[
  On-dataset approximation from recovered coefficients.
  One route: $EE_calD [(f-g)^2] = sum_V b_V dot hat((f-g)^2) (V)$ by @lem:convolution applied to $(f-g)^2$ at $S = emptyset$, splitting the sum over $heavyB$ and its complement; the tail is controlled by $theta dot norm(hat((f-g)^2))_1 <= theta (norm(hat(f))_1 + norm(hat(g))_1)^2$, but the heavy-bias terms need per-$V$ control.
  Relatedly, fix the "Parseval for Dataset Closeness" lemma of the previous section.
]

== Discussion and Open Directions

#TODO[
  - Non-uniform (weighted) datasets: all identities above should extend with $p(x)$ in place of $1\/|calD|$; the reproducing-kernel steps are unaffected, the mass identity picks up $norm(p)_2^2$-type factors.
  - General product alphabets and orthonormal bases (Hermite, tokens): the pair-form and blindness lemmas need only the reproducing kernel; the convolution identity needs the group structure ($chi_S overline(chi_T) = chi_(S - T)$), so state it over $ZZ_q^n$ or general finite abelian groups.
  - How to _certify_ or _learn_ the heavy bias set $heavyB$ for structured real datasets (images, token corpora)?
    For dense datasets @prop:dense computes it; for sparse ones, what natural structure (subcube constraints, sparsity templates, group symmetries) makes $heavyB$ small and known?
  - Relation to the "Active Fourier Auditor" of #link("https://arxiv.org/pdf/2410.08111")[arXiv:2410.08111], which estimates distributional properties of ML models from queries and samples.
  - Lower bound: turn @lem:blindness into a formal sample-complexity lower bound for @defn:dataset-gl-goal in the $sans("SAMP")$-only model (two datasets, same visible statistics, different heavy sets).
]
