#import "../levs-commands/main_commands.typ": *
#import "../custom_commands.typ": *
#show: thmrules
#show: eqrules
#set math.equation(numbering: none)

// ============ Reusable Symbols ============
#let normC = $C_calD$
#let normCInv = $C_calD^(-1)$
#let Jbar = $overline(J)$
#let ind = $bb(1)$
#let heavyB = $B_theta$
#let dcoeff(S) = $hat(f)_calD (#S)$
#let ucoeff(S) = $hat(f) (#S)$
#let vbar = $overline(v)$

= Highlight: Goldreich-Levin over a Dataset

We now come to the highlight: adapting the Goldreich-Levin algorithm @goldreich1989hardcore (@o2021analysis, Section 3.5) to the dataset setting.
The classical algorithm, given query access to $f$, finds _all_ of the characters with which $f$ is noticeably correlated — the engine behind the Kushilevitz-Mansour learning algorithm @kushilevitz1993learning.
Our use case is the on-distribution version of the same question: $f$ is a trained model (or labeling rule), $calD$ is the data, and we want every character that is noticeably correlated with $f$ _over the dataset_, i.e., every heavy dataset coefficient $dcoeff(S)$.
Moreover, we insist that the headline algorithm _never leaves the data_: no membership tester for $calD$, and no evaluation of $f$ on synthetic, out-of-distribution inputs.
The resource that replaces them is one that real corpora actually offer: _conditional sampling_ — retrieving datapoints that share a fixed substring (a context), the way an $n$-gram index retrieves them from a text corpus.

For clarity of exposition we work over the Boolean cube $Omega^n = {-1,1}^n$ with the parity characters $chi_S$, writing $dcoeff(S) = EE_(x ~ calD) [f(x) chi_S (x)]$ as before.
#footnote[
  Every identity below that uses only orthonormality and the reproducing kernel $sum_S chi_S (x) chi_S (x') = 2^n dot ind [x = x']$ extends verbatim to any finite abelian group with its character basis (with $chi_S overline(chi_T)$ in place of $chi_S chi_T$), and to general product orthonormal bases over finite sets.
  The convolution identity additionally uses the group structure $chi_S chi_T = chi_(S Delta T)$.
]
The goal, mirroring the classical guarantee:

#definition[Dataset Heavy-Coefficient Search][
  Given $0 < tau <= 1$ and access to $f$ and $calD$ (in a model to be specified), output a list $L$ of subsets of $[n]$ such that with high probability:
  + (Completeness) $|dcoeff(S)| >= tau ==> S in L$;
  + (Soundness) $S in L ==> |dcoeff(S)| >= tau \/ 2$.
]<defn:dataset-gl-goal>

== Access Models

The choice of access model turns out to be the crux of the problem.
The underlying object is a corpus $calD subset {-1,1}^n$ together with $f$ on $calD$; the algorithm reaches it through:
+ *Samples* ($sans("SAMP")$): i.i.d. draws $x ~ calD$ together with the label $f(x)$.
+ *Conditional samples* ($sans("CSAMP")$): given coordinates $Jbar subset.eq [n]$ and a context $z$ realized in the data, a draw $x ~ calD$ conditioned on $x_(Jbar) = z$, together with $f(x)$.
  This is the "retrieve strings containing this substring" primitive; it is implemented exactly by an index over the corpus (an $n$-gram or suffix-array structure), or by holding an explicit copy — or large subsample — of $calD$.
  It is the _subcube-conditioning_ oracle studied in distribution testing and learning @canonne2015testing @chakraborty2013power @chen2021junta, specialized here to the empirical support of $calD$ — and put to a different use: recovering heavy Fourier coefficients of a function $f$, rather than testing a distribution.
That is the entire model.
Note what is absent: any membership tester for $calD$, any evaluation of $f$ outside the data, indeed any synthetic input whatsoever — every string the algorithm ever touches is a datapoint.
(A companion result, @thm:dataset-gl, considers the strictly stronger setting where $f$ is additionally a cheaply queryable model; we keep that assumption out of the headline deliberately, to highlight that it is not needed.)

Classical GL is an algorithm of the stronger type — it queries $f$ at arbitrary, algorithm-chosen points — and its guarantee concerns the _global_ coefficients $ucoeff(S)$.
For the _dataset_ coefficients, we will see that plain samples are provably insufficient (@lem:blindness); the headline theorem (@thm:context-gl) restores power through $sans("CSAMP")$ alone, with complexity governed by how much the dataset's contexts _repeat_, while the companion routes the dataset's structure through its _bias spectrum_ (@lem:convolution) at the price of model queries.

== Recap: the Classical Goldreich-Levin Algorithm

#theorem[Goldreich-Levin @goldreich1989hardcore; @o2021analysis Section 3.5][
  Given query access to $f : {-1,1}^n -> {-1,1}$ and $0 < tau <= 1$, there is a $"poly"(n, 1\/tau)$-time algorithm that with high probability outputs a list $L = {U_1, dots, U_ell}$ of subsets of $[n]$ such that:
  + $|ucoeff(U)| >= tau ==> U in L$;
  + $U in L ==> |ucoeff(U)| >= tau \/ 2$.
  By Parseval, the second guarantee implies $|L| <= 4 \/ tau^2$.
]<thm:classical-gl>

The engine is a divide-and-conquer search over the tree of coordinate prefixes.
For $S subset.eq J subset.eq [n]$, define the *bucket weight* (@o2021analysis, Definition 3.39)
$
W^(S | J) [f] = sum_(T subset.eq Jbar) ucoeff(S union T)^2,
$
the Fourier weight of $f$ on sets whose restriction to $J$ equals $S$.
The algorithm maintains the buckets of weight at least $tau^2 \/ 2$ — at most $2 \/ tau^2$ of them by Parseval ($4 \/ tau^2$ once estimation slack is accounted) — splitting one coordinate at a time until each survivor is a singleton ${S}$ with $ucoeff(S)^2 >= tau^2 \/ 2$.
What makes this feasible is that bucket weights are _estimable_: by the restriction identity (@o2021analysis, equation (3.5) and Proposition 3.40),
$
W^(S | J) [f] = EE_(z ~ {-1,1}^(Jbar)) [hat(f)_(J | z) (S)^2] = EE_(z ~ {-1,1}^(Jbar)) space EE_(y, y' ~ {-1,1}^J) [f(y, z) chi_S (y) dot f(y', z) chi_S (y')],
$
so an algorithm with query access to $f$ gets accuracy $plus.minus eps$ with $O(log(1\/delta) \/ eps^2)$ queries.

Two remarks for later use.
First, the proof only needs $norm(f)_infinity <= 1$ (for the Chernoff estimates) and $norm(f)_2 <= 1$ (for the Parseval pruning bound), so @thm:classical-gl holds verbatim for $f : {-1,1}^n -> [-1, 1]$, with list size $4 norm(f)_2^2 \/ tau^2$.
Second, the restriction identity averages the _uniform_ restriction $z$; keep an eye on it — the headline theorem below will replace this uniform average by the dataset's own conditional distribution.

== Obstructions

Why can't we just run the same tree search on the dataset coefficients?
Recall from the Mass Identity (@lem:mass) that $sum_S dcoeff(S)^2 = normC dot EE_calD [f^2]$: Parseval fails over $calD$ by exactly the density factor.
This already kills the pruning bound — the number of $tau$-heavy dataset coefficients can be as large as $normC EE_calD [f^2] \/ tau^2$, i.e., exponential — and the failure is not confined to exotic functions:

#example[
  Let $K subset.eq [n]$ and let $calD = {x : x_i = 1 "for all" i in K}$ be the subcube fixing $K$, with $f equiv 1$.
  Every $chi_S$ with $S subset.eq K$ is identically $1$ on $calD$, so
  $
  dcoeff(S) = cases(1 &"if" S subset.eq K, 0 &"otherwise,")
  $
  and there are $2^(|K|)$ maximal coefficients: distinct characters are _identical as functions on $calD$_, and "the" heavy set is only defined up to this aliasing.
]<ex:subcube>

More damning still: the bucket weights themselves carry _no information_ over a generic dataset.
The following exact identity is the dataset analogue of the restriction identity.

#lemma[Pair Form of Dataset Bucket Weights][
  Fix $J subset.eq [n]$ with $|J| = k$ and $S subset.eq J$. Then
  $
  sum_(U subset.eq Jbar) dcoeff(S union U)^2 = 2^(n - k) dot EE_(x, x' ~ calD) [f(x) f(x') chi_S (x_J) chi_S (x'_J) dot ind [x_(Jbar) = x'_(Jbar)]].
  $
]<lem:pair-form>
#proof[
  Expand each $dcoeff(S union U) = EE_(x ~ calD) [f(x) chi_S (x_J) chi_U (x_(Jbar))]$, square, and swap sums; the inner sum $sum_(U subset.eq Jbar) chi_U (x_(Jbar)) chi_U (x'_(Jbar))$ is the reproducing kernel $2^(n-k) dot ind [x_(Jbar) = x'_(Jbar)]$ on ${-1,1}^(Jbar)$.
]

#lemma[Blindness of Dataset Bucket Weights][
  In the setting of @lem:pair-form, suppose no two dataset points collide on $Jbar$ (the projection $x |-> x_(Jbar)$ is injective on $calD$).
  Then for _every_ $S subset.eq J$,
  $
  sum_(U subset.eq Jbar) dcoeff(S union U)^2 = frac(2^(n-k), |calD|) dot EE_(x ~ calD) [f(x)^2],
  $
  independent of $S$.
]<lem:blindness>
#proof[
  Under injectivity only the diagonal of @lem:pair-form survives, and $chi_S (x_J)^2 = 1$.
]

In words: as long as the dataset's projection onto the un-split coordinates is collision-free, every bucket at level $k$ has exactly the same weight — no matter what $f$ is, and no matter where its heavy dataset coefficients lie.
For $m$ points in general position, a union bound gives collisions on $Jbar$ with probability at most $binom(m, 2) \/ 2^(n-k)$, so the search tree is information-free for all levels $k lt.tilde n - 2 log_2 m$ — the _birthday horizon_.
A GL-style search would descend blindly through $2^(n - 2 log_2 m)$ buckets before the weights begin to distinguish anything.
Together with the subcube example, this rules out a sample-only dataset GL: additional access, or structural repetition in $calD$, is necessary.
This is a dataset-specific incarnation of the statistical-query / noisy-parity barrier @kearns1998sq @blum1994weakly @blum2003noise --- correlational (sample-only) access cannot isolate a heavy parity --- and of the sample-only insufficiency that interactive proofs for learning circumvent with a query-capable prover @goldwasser2021interactive @gur2024power; here the missing power will be supplied instead by context repetition (@thm:context-gl) or by the dataset's bias spectrum (@thm:dataset-gl).
#footnote[
  A sanity check for @lem:blindness: $n = 2$, $calD = {(1,1), (-1,-1)}$, $f(x) = x_1$, $J = {1}$.
  Then $dcoeff({1}) = 1$, $dcoeff({1,2}) = 0$, and the bucket weight is $1 = frac(2^(2-1), 2) dot 1$: the signal coefficient and the aliasing noise conspire to a flat bucket profile.
  All identities and theorem constants in this section have been verified numerically on random and structured instances.
]

== Main Theorem: Goldreich-Levin from Context Conditioning

The blindness lemma tells us exactly what to look for: its collision-free hypothesis fails precisely when dataset points _share contexts_.
Real corpora share contexts constantly — that is what $n$-gram statistics are — and $sans("CSAMP")$ is the primitive that exposes the sharing.
The headline algorithm replaces the classical bucket weight, whose restriction $z$ is averaged over the _uniform_ measure, by a conditional statistic whose contexts are drawn from _the dataset itself_.

#definition[Contexts, Fibers, and the Context Profile][
  Fix $J = J_k = [k]$ and $Jbar = [n] without J$.
  The _contexts_ at level $k$ are the realized projections $Z_k = {x_(Jbar) : x in calD}$; the _fiber_ of $z in Z_k$ is $calD_z = {x in calD : x_(Jbar) = z}$.
  We write $z ~ calD_(Jbar)$ for the marginal (i.e., $z = x_(Jbar)$ with $x ~ calD$), and define the _context profile_ $c_k = |Z_k|$.
]<defn:contexts>

#definition[Conditional Bucket Weight][
  For $S subset.eq J$ and $z in Z_k$, let $vbar_S (z) = EE_(x ~ calD_z) [f(x) chi_S (x_J)]$ — the "average with the string $z$ fixed."
  The _conditional bucket weight_ is
  $
  Psi(S | J) = EE_(z ~ calD_(Jbar)) [vbar_S (z)^2].
  $
]<defn:psi>

#lemma[Properties of the Conditional Bucket Weight][
  For all $S subset.eq J = J_k$:
  + (Tower) $dcoeff(S union U) = EE_(z ~ calD_(Jbar)) [chi_U (z) vbar_S (z)]$ for every $U subset.eq Jbar$;
  + (Completeness) $Psi(S | J) >= dcoeff(S union U)^2$ for every $U subset.eq Jbar$;
  + (Termination) $Psi(S | [n]) = dcoeff(S)^2$;
  + (Monotonicity) $Psi(S' | J_(k+1)) <= Psi(S | J_k)$ for both children $S' in {S, S union {k + 1}}$;
  + (Level mass) $sum_(S subset.eq J) Psi(S | J) = 2^k dot EE_(z ~ calD_(Jbar)) [frac(EE_(x ~ calD_z) [f(x)^2], |calD_z|)] =: R_k <= 2^k dot norm(f)_infinity^2 dot frac(c_k, |calD|)$.
]<lem:psi-properties>
#proof[
  (1) is the tower property of conditional expectation.
  (2) is Cauchy-Schwarz applied to (1), using $chi_U (z)^2 = 1$.
  (3): at $J = [n]$ there is a single trivial context and $vbar_S = dcoeff(S)$.
  (4): for $S' = S$, coarsening the context from level $k$ to level $k+1$ replaces $vbar_S (z)$ by its conditional average, and Jensen gives $EE[vbar^2]$ non-increasing.
  For $S' = S union {k+1}$, write $w = f dot chi_(S') (x_(J_(k+1)))$; the level-$k$ context fixes $x_(k+1)$, so $EE[w | z] = plus.minus vbar_S (z)$ and $EE_z [EE[w|z]^2] = Psi(S | J_k)$; coarsening to level $k+1$ is again Jensen.
  (5): points of a fiber share $z$, hence have _distinct_ $J$-parts, so ${x_J : x in calD_z}$ is a dataset in ${-1,1}^J$ and the Mass Identity (@lem:mass) on it gives $sum_(S subset.eq J) vbar_S (z)^2 = frac(2^k, |calD_z|) EE_(x ~ calD_z) [f^2]$; average over $z$.
  The final bound uses $EE_(z ~ calD_(Jbar)) [1 \/ |calD_z|] = c_k \/ |calD|$.
]

Everything a GL search needs is here: heavy descendants keep their ancestors heavy (2), the search terminates at exactly the dataset coefficient (3), pruned buckets stay pruned (4), and the number of live buckets is controlled by the _repetition structure_ of the data (5) — $R_k$ is small exactly when contexts repeat ($c_k << |calD|$ at levels where $2^k$ is large).
Estimation is the dataset analogue of the classical restriction identity: draw a context _from the data_ and two strings sharing it.

#lemma[Estimating $Psi$][
  $
  Psi(S | J) = EE_(z ~ calD_(Jbar)) space EE_(x, x' "i.i.d." ~ calD_z) [f(x) chi_S (x_J) dot f(x') chi_S (x'_J)].
  $
  Each experiment costs one $sans("SAMP")$ draw (yielding $x$ and its context $z = x_(Jbar)$) and one $sans("CSAMP")$ draw ($x' ~ calD_z$); the product lies in $[-1,1]$ when $norm(f)_infinity <= 1$, so $O(log(1\/delta) \/ eps^2)$ experiments estimate $Psi(S|J)$ to accuracy $plus.minus eps$ with confidence $1 - delta$.
]<lem:psi-estimator>
#proof[
  For fixed $z$, independence of $x, x'$ gives $EE[f(x) chi_S (x_J) f(x') chi_S (x'_J)] = vbar_S (z)^2$; average over $z$.
  A $sans("SAMP")$ draw $x$ has $x_(Jbar) ~ calD_(Jbar)$ and, conditioned on its context, $x ~ calD_z$; Hoeffding does the rest.
]

#theorem[Dataset Goldreich-Levin from Context Conditioning][
  Given $sans("SAMP")$ and $sans("CSAMP")$ access to $(calD, f)$ with $norm(f)_infinity <= 1$, and $0 < tau <= 1$, $delta in (0,1)$, there is an algorithm that with probability at least $1 - delta$ outputs a list $L$ satisfying:
  + (Completeness) $|dcoeff(S)| >= tau ==> S in L$;
  + (Soundness) $S in L ==> |dcoeff(S)| >= tau \/ 2$;
  + (Complexity) on the same $1 - delta$ event, the algorithm performs at most $O(n N dot log(n N \/ delta) \/ tau^4)$ sampling experiments and runs in time $"poly"(n, N, 1\/tau, log(1\/delta))$, where
  $
  N = max_(0 <= k <= n) N_k, quad N_k = |{S subset.eq J_k : Psi(S | J_k) >= tau^2 \/ 4}|, quad "and" quad N_k <= min(frac(4 R_k, tau^2), 2 N_(k-1)) " (the latter for " k >= 1 ")".
  $
  The algorithm needs no advance knowledge of $N$, never evaluates $f$ outside $calD$, and never tests membership: every string it touches is a datapoint.
]<thm:context-gl>

The algorithm is the classical tree search with $Psi$ in place of $W$:
+ Maintain a set of live buckets $(S, J_k)$, starting from $(emptyset, J_0)$.
+ At level $k$, split each live bucket on coordinate $k+1$ and estimate $Psi(S' | J_(k+1))$ for both children to accuracy $tau^2 \/ 8$, giving the $t$-th estimate performed (in any fixed processing order) confidence $1 - delta \/ (2 t^2)$ via @lem:psi-estimator; keep the children with estimate at least $3 tau^2 \/ 8$.
  Note the confidence schedule references only the running index $t$, so the algorithm is well-defined with no advance knowledge of $N$.
+ Output the surviving singletons at level $n$.

#proof[
  Condition on every performed estimate being $tau^2\/8$-accurate.
  The $t$-th estimate uses fresh samples, so it fails with probability at most $delta \/ (2 t^2)$ regardless of which buckets earlier estimates caused to be explored; summing over the (adaptively determined, possibly unbounded) sequence of estimates, the total failure probability is at most $sum_(t >= 1) delta \/ (2 t^2) <= delta$.

  *Completeness.* If $|dcoeff(S)| >= tau$, then by completeness (2) of @lem:psi-properties every ancestor bucket of $S$ has $Psi >= tau^2$, so its estimate is at least $7 tau^2 \/ 8 >= 3 tau^2 \/ 8$ and the ancestor survives at every level; hence $S in L$.

  *Soundness.* A surviving leaf has estimate at least $3 tau^2 \/ 8$, hence $Psi(S | [n]) >= tau^2 \/ 4$; by termination (3), $dcoeff(S)^2 >= tau^2 \/ 4$.

  *Complexity.* On the good event, a surviving bucket at level $k$ has true $Psi >= tau^2 \/ 4$, so there are at most $N_k$ of them; the level-mass identity (5) gives $N_k <= 4 R_k \/ tau^2$ (Markov), and monotonicity (4) gives $N_k <= 2 N_(k-1)$ (children of dead buckets are dead).
  Hence at most $2 n N$ estimates are performed in total, and the $t$-th costs $O(log(t \/ delta) \/ tau^4) = O(log(n N \/ delta) \/ tau^4)$ experiments.
]

(If a deterministic time budget is preferred over a probabilistic one, run with a cap $overline(N)$ on live buckets per level, aborting when it is exceeded, and guess-and-double $overline(N) = 1, 2, 4, dots$ with per-run confidence budget $delta \/ 2^(j+1)$; on the good event with $overline(N) >= N$ no abort occurs, at the cost of $O(log N)$ restarts.)

Three instances of the parameter $N$, spanning the possible regimes (all verified numerically):
- *Dense datasets.* Always $c_k <= min(|calD|, 2^(n-k))$, so $R_k <= min(2^k, normC)$ and $N <= 4 normC \/ tau^2$: for $normC = "poly"(n)$ the search is polynomial outright — using strictly weaker access than a membership tester.
- *Subcube-structured data.* For the subcube example above (fixing $K$, $f equiv 1$): $R_k = 2^(|K inter J_k|) <= 2^(|K|)$, matching the true output size $2^(|K|)$ — the search is output-sensitive, even though the density constant $normC = 2^(|K|)$ is itself exponential in $|K|$.
- *Generic sparse data.* Singleton fibers give $vbar_S (z) = f(x) chi_S (x_J)$, so $Psi(S|J) = EE_calD [f^2]$ for _every_ $S$ — flat — and $c_k = |calD|$, $R_k = 2^k dot EE_calD [f^2]$: the parameter degrades to the trivial $2^k$ exactly when @lem:blindness bites, and $sans("CSAMP")$ degenerates to $sans("SAMP")$.
  The theorem's power is context repetition, and the blindness lemma says this is not an artifact.

Two further remarks.
First, nothing forces the split order $1, 2, dots, n$: any coordinate order works, and the profile $R_k$ — hence the runtime — depends on it.
For sequence data the natural choice is to keep _contiguous_ contexts (so fibers are "strings sharing an $(n-k)$-gram"), and choosing the order to maximize repetition is a purely combinatorial preprocessing question on $calD$.
Second, applying @thm:context-gl to the constant function $f equiv 1$ finds all heavy _biases_ $b_V = EE_calD [chi_V]$ at threshold $theta$ in time $"poly"(n, N(ind), 1\/theta)$ — recovering, on-distribution, exactly the side information that the companion result below consumes.

== Companion: Dataset GL from Model Queries

This subsection — and only this subsection, together with its corollaries below — assumes one additional oracle: *model queries* ($sans("QUERY")$), i.e., $f$ evaluable at arbitrary $x in {-1,1}^n$, including out-of-distribution points.
This is realistic when $f$ is a cheaply queryable model, and it enables a different route: never condition, but instead read the dataset's effect on the spectrum through one structural object.

#definition[Bias Spectrum][
  The *bias spectrum* of $calD$ is the family
  $
  b_V = EE_(x ~ calD) [chi_V (x)], quad V subset.eq [n]
  $
  — the dataset Fourier coefficients of the constant function $1$.
  Note $b_emptyset = 1$ and $|b_V| <= 1$.
  For $theta > 0$, write $heavyB = {V : |b_V| >= theta}$ for the *$theta$-heavy bias set*.
]<defn:bias-spectrum>

#lemma[Convolution Identity][
  For any $f : {-1,1}^n -> RR$ and any $S subset.eq [n]$,
  $
  dcoeff(S) = sum_(V subset.eq [n]) b_V dot ucoeff(S Delta V),
  $
  where $Delta$ denotes symmetric difference.
]<lem:convolution>
#proof[
  Expand $f$ in its global Fourier expansion inside the dataset expectation:
  $
  dcoeff(S) = sum_T ucoeff(T) dot EE_(x ~ calD) [chi_T (x) chi_S (x)] = sum_T ucoeff(T) dot b_(T Delta S),
  $
  using $chi_T chi_S = chi_(T Delta S)$; substitute $V = T Delta S$.
]

The bias spectrum is exactly the aliasing structure of the subcube example: there $b_V = ind [V subset.eq K]$, and the convolution smears the single global coefficient $ucoeff(emptyset) = 1$ onto all $2^(|K|)$ sets $S subset.eq K$.
This coefficient aliasing is the same mechanism that subsampling induces on the Walsh-Hadamard spectrum, exploited constructively --- with a _designed_, invertible subsampling pattern --- in sparse-transform algorithms @scheibler2015fast @li2015spright; the difference here is that the dataset, and hence its aliasing pattern, is given rather than chosen.
The blindness of @lem:blindness is the statement that for a generic dataset the bias spectrum is an exponentially wide field of $tilde plus.minus 1\/sqrt(|calD|)$ noise, which buries any bucket-level signal.
Conversely, whenever the bias spectrum is _dominated by few heavy terms_, the dataset spectrum of $f$ is a controlled deformation of its global spectrum — which the following theorem exploits.

#theorem[Dataset Goldreich-Levin, model-query access][
  Let $f : {-1,1}^n -> [-1,1]$ with $norm(hat(f))_1 <= A$ be given by $sans("QUERY")$ access, and let $calD$ be given by $sans("SAMP")$ access.
  Let $0 < tau <= 1$, $delta in (0,1)$, fix any $theta <= min(1, tau \/ (4A))$ (so that $emptyset in heavyB$ and $|heavyB| >= 1$), and suppose the $theta$-heavy bias set $heavyB$ of $calD$ is given explicitly (e.g., via @thm:context-gl applied to $f equiv 1$, or from prior structural knowledge).
  Then there is an algorithm running in time $"poly"(n, |heavyB|, 1\/tau, log(1\/delta))$, using $"poly"(n, |heavyB|, 1\/tau) dot log(1\/delta)$ queries to $f$ and $O(log(|heavyB| \/ (tau delta)) \/ tau^2)$ samples from $calD$, that with probability at least $1 - delta$ outputs a list $L$ satisfying:
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
  By Hoeffding and a union bound over $C$ (except with probability $delta \/ 2$), every empirical coefficient satisfies $|tilde(f)_calD (S) - dcoeff(S)| <= tau \/ 8$; in particular $|tilde(f)_calD (S)| >= tau - tau\/8 > 3 tau \/ 4$, so $S in L$.

  *Soundness.* If $S in L$ then $|dcoeff(S)| >= 3 tau \/ 4 - tau \/ 8 = 5 tau \/ 8 >= tau \/ 2$.

  *List size and complexity.* $|L| <= |C| <= |F| |heavyB| <= frac(4, tau'^2) |heavyB| = frac(64 |heavyB|^3, 9 tau^2)$.
  Classical GL at threshold $tau'$ costs $"poly"(n, 1\/tau') = "poly"(n, |heavyB|, 1\/tau)$ queries; step 3 costs $m$ samples and $m dot |C|$ arithmetic operations.
]

Note that the algorithm uses only the _set_ $heavyB$, never the values $b_V$; and the theorem is meaningful precisely in the distribution-shift regime where $dcoeff(S)$ and $ucoeff(S)$ differ — the shift is routed through the dataset's heavy biases.
Running the completeness argument purely combinatorially (Parseval bounds the number of $tau'$-heavy global coefficients by $1 \/ tau'^2$) also yields an unconditional structural bound:

#corollary[List-Size Bound under Sparse Bias Spectrum][
  If $f : {-1,1}^n -> [-1,1]$ with $norm(hat(f))_1 <= A$, and $theta <= min(1, tau \/ (4A))$, then
  $
  |{S : |dcoeff(S)| >= tau}| <= frac(16 |heavyB|^3, 9 tau^2).
  $
]<cor:list-size>

#h3([Representative Datasets])

At the opposite extreme, if the dataset's spectrum uniformly tracks the global one — the "no distribution shift" regime — dataset GL degenerates gracefully to classical GL plus re-scoring.

#definition[$eps$-Representative Dataset][
  $calD$ is *$eps$-representative* for $f$ if $|dcoeff(S) - ucoeff(S)| <= eps$ for all $S subset.eq [n]$.
]<defn:representative>

#lemma[
  Let $f : {-1,1}^n -> [-1,1]$ and let $calD$ consist of $m$ i.i.d. uniform samples.
  If $m >= frac(2, eps^2) (n ln 2 + ln frac(2, delta))$, then with probability at least $1 - delta$, $calD$ is $eps$-representative for $f$.
]<lem:representative>
#proof[
  For fixed $S$, $dcoeff(S)$ averages $m$ i.i.d. terms $f(x) chi_S (x) in [-1,1]$ with mean $ucoeff(S)$; Hoeffding gives $Pr[|dcoeff(S) - ucoeff(S)| > eps] <= 2 e^(- m eps^2 \/ 2)$.
  Union bound over all $2^n$ sets.
  (Here $calD$ is the sampled _multiset_ and $dcoeff(S)$ its multiset average; distinctness of points plays no role in this lemma or the proposition below.)
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

In this regime the heavy dataset coefficients essentially _are_ the heavy global coefficients; the substantive content of @thm:context-gl and @thm:dataset-gl lies exactly where representativeness fails.

== Application: Kushilevitz-Mansour over Datasets

Classical GL powers the Kushilevitz-Mansour learning algorithm @kushilevitz1993learning (@o2021analysis, Theorems 3.37-3.38): any $f$ with $norm(hat(f))_1 <= s$ — in particular any decision tree of size $s$ — is learnable from queries in time $"poly"(n, s, 1\/eps)$.
The spectral-norm hypothesis is exactly the $A$ of @thm:dataset-gl --- the same $L^1$-geometry exploited by agnostic decision-tree learning @gopalan2008agnostically --- so the on-distribution analogue of the coefficient-collection step comes for free:

#corollary[Heavy On-Dataset Coefficients of Decision Trees][
  Let $f : {-1,1}^n -> {-1,1}$ be computable by a decision tree of size $s$ (so $norm(hat(f))_1 <= s$), given by $sans("QUERY")$ access, and let $heavyB$ be the $theta$-heavy bias set of $calD$ for some $theta <= min(1, tau \/ (4s))$.
  Then all $S$ with $|dcoeff(S)| >= tau$ can be listed in time $"poly"(n, s, |heavyB|, 1\/tau, log(1\/delta))$.
]<cor:km-datasets>

We emphasize what @cor:km-datasets does _not_ yet give: an $eps_calD$-close sparse approximation to $f$.
Classically, coefficient collection plus Parseval yields the learning guarantee; over a dataset, the closeness identity is constant-free only in _normalized_ coefficients (@lem:dataset-parseval), and a sparse $g$ assembled from recovered raw coefficients has dataset coefficients that differ from its design coefficients (aliasing again), so bounding $EE_(x ~ calD) [(f - g)^2]$ requires genuinely new arguments.
#TODO[
  On-dataset approximation from recovered coefficients.
  One route: $EE_calD [(f-g)^2] = sum_V b_V dot hat((f-g)^2) (V)$ by @lem:convolution applied to $(f-g)^2$ at $S = emptyset$, splitting the sum over $heavyB$ and its complement; the tail is controlled by $theta dot norm(hat((f-g)^2))_1 <= theta (norm(hat(f))_1 + norm(hat(g))_1)^2$, but the heavy-bias terms need per-$V$ control.
  Compare with the frame-truncation route of @thm:learning-low-degree.
]

== Discussion and Open Directions

The most closely related work is the Active Fourier Auditor @ajarra2024active, which likewise estimates spectral properties of a model over a data distribution; it orthonormalizes the parity basis to that distribution (following @heidari2021finding) and reports a few scalar functionals (robustness, individual/group fairness), whereas our dataset Goldreich-Levin keeps the fixed characters over the empirical measure and recovers the heavy coefficients themselves.
See @sec:related for the fuller landscape (non-uniform Fourier, the samples-versus-queries barrier, subsampling aliasing, and Fourier auditing of trained models).

#TODO[
  - Non-uniform (weighted) datasets: the identities above should extend with $p(x)$ in place of $1\/|calD|$; the reproducing-kernel steps are unaffected, the mass identity picks up $norm(p)_2^2$-type factors.
  - General product alphabets and orthonormal bases (Hermite, tokens): the pair-form and blindness lemmas need only the reproducing kernel; the convolution identity needs the group structure ($chi_S overline(chi_T) = chi_(S - T)$), so state it over $ZZ_q^n$ or general finite abelian groups.
    For token alphabets, $sans("CSAMP")$ is literally $n$-gram retrieval, and @thm:context-gl applies as stated.
  - Choosing the split order: the profile $R_k$ depends on the coordinate order; find the order minimizing $max_k R_k$ (a combinatorial problem on the dataset's context statistics — related to building a good trie).
  - $sans("CSAMP")$ from a flat subsample only: estimating $Psi$ without a corpus index requires context collisions _within_ the subsample — a birthday/U-statistic analysis; quantify the subsample size needed as a function of the fiber profile.
  - Characterize which real datasets (images, token corpora) have small context profiles $c_k$ along some order, and how $N$ behaves empirically; relatedly, when is the heavy-bias set $heavyB$ small and low-degree?
  - Lower bound: turn @lem:blindness into a formal sample-complexity lower bound for @defn:dataset-gl-goal in the $sans("SAMP")$-only model (two datasets, same visible statistics, different heavy sets), and extend it to $sans("CSAMP")$ with all fibers singletons.
]
