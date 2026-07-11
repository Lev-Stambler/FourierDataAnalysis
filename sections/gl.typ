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

= Goldreich-Levin over a Dataset

We adapt the Goldreich-Levin algorithm @goldreich1989hardcore (@o2021analysis, Section 3.5) to the dataset setting.
Given query access to $f$, the classical algorithm finds _all_ characters with which $f$ is noticeably correlated; it is what powers the Kushilevitz-Mansour learning algorithm @kushilevitz1993learning.
We want the on-distribution version: $f$ is a trained model (or labeling rule), $calD$ is the data, and we want every character noticeably correlated with $f$ _over the dataset_, i.e. every heavy dataset coefficient $dcoeff(S)$.
We also require that the algorithm _never leaves the data_: no membership tester for $calD$, and no evaluation of $f$ on synthetic, out-of-distribution inputs.
The replacement resource is one real corpora offer: _conditional sampling_ — retrieving datapoints that share a fixed substring (a context), the way an $n$-gram index retrieves them from a text corpus.

We work over the Boolean cube $Omega^n = {-1,1}^n$ with the parity characters $chi_S$, writing $dcoeff(S) = EE_(x ~ calD) [f(x) chi_S (x)]$ as before.
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

The choice of access model is decisive.
The underlying object is a corpus $calD subset {-1,1}^n$ with $f$ on $calD$; the algorithm reaches it through:
+ *Samples* ($sans("SAMP")$): i.i.d. draws $x ~ calD$ together with the label $f(x)$.
+ *Conditional samples* ($sans("CSAMP")$): given coordinates $Jbar subset.eq [n]$ and a context $z$ realized in the data, a draw $x ~ calD$ conditioned on $x_(Jbar) = z$, together with $f(x)$.
  This is the "retrieve strings containing this substring" primitive; it is implemented exactly by an index over the corpus (an $n$-gram or suffix-array structure), or by holding an explicit copy — or large subsample — of $calD$.
  It is the _subcube-conditioning_ oracle studied in distribution testing and learning @canonne2015testing @chakraborty2013power @chen2021junta, specialized here to the empirical support of $calD$ — and put to a different use: recovering heavy Fourier coefficients of a function $f$, rather than testing a distribution.
That is the entire model.
Absent: any membership tester for $calD$, any evaluation of $f$ outside the data, any synthetic input at all — every string the algorithm touches is a datapoint.
(The companion result @thm:dataset-gl uses the strictly stronger setting where $f$ is also a cheaply queryable model; we keep that out of the main theorem to show it is not needed.)

Classical GL is of the stronger type — it queries $f$ at arbitrary, algorithm-chosen points — and its guarantee concerns the _global_ coefficients $ucoeff(S)$.
For the _dataset_ coefficients, plain samples are insufficient (@lem:blindness); the main theorem (@thm:context-gl) restores power through $sans("CSAMP")$ alone, with complexity governed by how much the dataset's contexts _repeat_, while the companion routes the dataset's structure through its _bias spectrum_ (@lem:convolution) at the cost of model queries.

== Recap: the Classical Goldreich-Levin Algorithm

#theorem[Goldreich-Levin @goldreich1989hardcore; @o2021analysis Section 3.5][
  Given query access to $f : {-1,1}^n -> {-1,1}$ and $0 < tau <= 1$, there is a $"poly"(n, 1\/tau)$-time algorithm that with high probability outputs a list $L = {U_1, dots, U_ell}$ of subsets of $[n]$ such that:
  + $|ucoeff(U)| >= tau ==> U in L$;
  + $U in L ==> |ucoeff(U)| >= tau \/ 2$.
  By Parseval, the second guarantee implies $|L| <= 4 \/ tau^2$.
]<thm:classical-gl>

It is a divide-and-conquer search over the tree of coordinate prefixes.
For $S subset.eq J subset.eq [n]$, define the *bucket weight* (@o2021analysis, Definition 3.39)
$
W^(S | J) [f] = sum_(T subset.eq Jbar) ucoeff(S union T)^2,
$
the Fourier weight of $f$ on sets whose restriction to $J$ equals $S$.
The algorithm keeps the buckets of weight at least $tau^2 \/ 2$ — at most $2 \/ tau^2$ of them by Parseval ($4 \/ tau^2$ once estimation slack is included) — splitting one coordinate at a time until each survivor is a singleton ${S}$ with $ucoeff(S)^2 >= tau^2 \/ 2$.
This is feasible because bucket weights are _estimable_: by the restriction identity (@o2021analysis, equation (3.5) and Proposition 3.40),
$
W^(S | J) [f] = EE_(z ~ {-1,1}^(Jbar)) [hat(f)_(J | z) (S)^2] = EE_(z ~ {-1,1}^(Jbar)) space EE_(y, y' ~ {-1,1}^J) [f(y, z) chi_S (y) dot f(y', z) chi_S (y')],
$
so an algorithm with query access to $f$ gets accuracy $plus.minus eps$ with $O(log(1\/delta) \/ eps^2)$ queries.

Two facts for later.
The proof only needs $norm(f)_infinity <= 1$ (for the Chernoff estimates) and $norm(f)_2 <= 1$ (for the Parseval pruning bound), so @thm:classical-gl holds verbatim for $f : {-1,1}^n -> [-1, 1]$, with list size $4 norm(f)_2^2 \/ tau^2$.
And the restriction identity averages the _uniform_ restriction $z$; the main theorem below replaces this uniform average by the dataset's own conditional distribution.

== Obstructions

Why can't we just run the tree search on the dataset coefficients?
The Mass Identity (@lem:mass) already warns us: $sum_S dcoeff(S)^2 = normC dot EE_calD [f^2]$, so Parseval fails over $calD$ by the density factor $normC$.
This kills the pruning bound — there can be up to $normC EE_calD [f^2] \/ tau^2$ heavy coefficients, i.e. exponentially many — and not only for exotic $f$:

#example[
  Let $K subset.eq [n]$ and let $calD = {x : x_i = 1 "for all" i in K}$ be the subcube fixing $K$, with $f equiv 1$.
  Every $chi_S$ with $S subset.eq K$ is identically $1$ on $calD$, so
  $
  dcoeff(S) = cases(1 &"if" S subset.eq K, 0 &"otherwise,")
  $
  and there are $2^(|K|)$ maximal coefficients: distinct characters are _identical as functions on $calD$_, and "the" heavy set is only defined up to this aliasing.
]<ex:subcube>

Worse: the bucket weights themselves carry _no information_ over a generic dataset.
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

In words: whenever the dataset is collision-free on the un-split coordinates, every level-$k$ bucket has _identical_ weight — regardless of $f$ or where its heavy coefficients lie.
As $m$ random points collide on $Jbar$ only with probability $binom(m, 2) \/ 2^(n-k)$, the tree is information-free below the _birthday horizon_ $k approx n - 2 log_2 m$, so a GL search descends blindly through $2^(n - 2 log_2 m)$ buckets before any weight tells buckets apart.
With the subcube example, this rules out a sample-only dataset GL — extra access or structural repetition in $calD$ is necessary.
It is the dataset-specific form of the statistical-query / noisy-parity barrier @kearns1998sq @blum1994weakly @blum2003noise, and of the sample-only insufficiency that query-capable provers remove for learning @goldwasser2021interactive @gur2024power; the missing power is restored below by context repetition (@thm:context-gl) or the bias spectrum (@thm:dataset-gl).
#footnote[
  Sanity check for @lem:blindness: take $n = 2$, $calD = {(1,1), (-1,-1)}$, $f(x) = x_1$, $J = {1}$.
  Then $dcoeff({1}) = 1$, $dcoeff({1,2}) = 0$, yet the bucket weight is $1 = frac(2^(2-1), 2) dot 1$ — signal and aliasing noise conspiring to a flat profile.
]

== Main Theorem: Goldreich-Levin from Context Conditioning

The blindness lemma tells us exactly what to look for: its collision-free hypothesis fails precisely when dataset points _share contexts_.
Real corpora share contexts constantly — that is what $n$-gram statistics are — and $sans("CSAMP")$ is the primitive that exposes the sharing.
The headline algorithm replaces the classical bucket weight, whose restriction $z$ is averaged over the _uniform_ measure, by a conditional statistic whose contexts are drawn from _the dataset itself_.

#definition[Contexts and Context Groups][
  Fix $J = J_k = [k]$ and $Jbar = [n] without J$.
  The _contexts_ at level $k$ are the distinct strings $Z_k = {x_(Jbar) : x in calD}$ appearing on the un-split coordinates; the _context group_ of $z in Z_k$ is $calD_z = {x in calD : x_(Jbar) = z}$ — the datapoints sharing that context.
  We write $z ~ calD_(Jbar)$ for a context drawn with the data ($z = x_(Jbar)$, $x ~ calD$), and $c_k = |Z_k|$ for the number of distinct contexts (the _context profile_).
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
  Throughout, $calD_z$ is the context group of $z$ (@defn:contexts) and $vbar_S (z) = EE_(x ~ calD_z) [f(x) chi_S (x_J)]$ is the average of $f chi_S$ over the datapoints sharing context $z$.

  *(1) Tower.* Group the average defining $dcoeff(S union U)$ by context. Once the context $z = x_(Jbar)$ is fixed, the factor $chi_U (x_(Jbar)) = chi_U (z)$ is constant across the group and pulls out of the inner average:
  $
  dcoeff(S union U) = EE_(x ~ calD) [f(x) chi_S (x_J) chi_U (x_(Jbar))] = EE_(z ~ calD_(Jbar)) [chi_U (z) underbrace(EE_(x ~ calD_z) [f(x) chi_S (x_J)], vbar_S (z))].
  $
  This is the law of total expectation, splitting the data by context.

  *(2) Completeness.* Read (1) as the inner product of $vbar_S$ with the unit vector $chi_U$ (unit because $chi_U (z)^2 = 1$): one inner product cannot exceed the norm of $vbar_S$. Concretely, Cauchy-Schwarz on (1) gives
  $
  dcoeff(S union U)^2 <= EE_z [chi_U (z)^2] dot EE_z [vbar_S (z)^2] = Psi(S | J).
  $
  So a descendant coefficient is dominated by its ancestor bucket's weight — a heavy leaf forces every ancestor heavy, which is exactly what lets the search prune without discarding a heavy coefficient.

  *(3) Termination.* At $J = [n]$ the un-split part $Jbar$ is empty, so there is one (empty) context whose group is all of $calD$; hence $vbar_S = EE_calD [f chi_S] = dcoeff(S)$ and $Psi(S | [n]) = dcoeff(S)^2$. At the bottom of the tree the bucket weight is exactly the squared coefficient we are hunting for.

  *(4) Monotonicity.* Descending one level moves coordinate $k+1$ out of the context and into $J$, so the level-$(k+1)$ context fixes _one fewer_ coordinate: it is a coarsening of the level-$k$ context, and each level-$(k+1)$ group is a union of level-$k$ groups. Averaging over the coarser groups before squaring can only shrink the mean square (Jensen).
  For the child $S' = S$: the level-$(k+1)$ value is the average of the level-$k$ values over the groups merged into $z'$, i.e. $vbar_S (z') = EE[vbar_S (z) | z']$, so
  $
  Psi(S | J_(k+1)) = EE_(z') [EE[vbar_S (z) | z']^2] <= EE_(z') EE[vbar_S (z)^2 | z'] = Psi(S | J_k).
  $
  For the child $S' = S union {k+1}$: put $w = f dot chi_(S') (x_(J_(k+1))) = f dot chi_S (x_J) dot x_(k+1)$. The level-$k$ context already fixes $x_(k+1) = plus.minus 1$, so on each level-$k$ group $EE[w | z] = plus.minus vbar_S (z)$, giving $EE_z [EE[w | z]^2] = EE_z [vbar_S (z)^2] = Psi(S | J_k)$; coarsening to level $k+1$ is the same Jensen step. Either way a child is never heavier than its parent, so once a bucket is pruned its descendants stay pruned.

  *(5) Level mass.* Fix a context $z$. The points of its group $calD_z$ agree on $Jbar$, hence _differ_ on $J$: the map $x |-> x_J$ is injective on $calD_z$, so ${x_J : x in calD_z}$ is itself a dataset inside ${-1,1}^J$, on which $vbar_S (z)$ is precisely the dataset coefficient at $S$. The Mass Identity (@lem:mass) then applies verbatim to this small dataset — its density constant is $2^k \/ |calD_z|$ — and yields
  $
  sum_(S subset.eq J) vbar_S (z)^2 = frac(2^k, |calD_z|) EE_(x ~ calD_z) [f(x)^2].
  $
  Taking $EE_(z ~ calD_(Jbar))$ of both sides gives the stated identity. For the bound, $EE_(x ~ calD_z) [f^2] <= norm(f)_infinity^2$ and
  $
  EE_(z ~ calD_(Jbar)) [1 \/ |calD_z|] = sum_(z in Z_k) frac(|calD_z|, |calD|) dot frac(1, |calD_z|) = frac(c_k, |calD|).
  $
  So the live weight at level $k$ is governed by the number of distinct contexts $c_k$: when contexts repeat ($c_k << |calD|$) there is little mass to spread, and few buckets can be heavy.
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

#definition[Live-Bucket Profile][
  For a threshold $tau$, the _live-bucket profile_ of $(calD, f)$ is
  $
  N_k = |{S subset.eq J_k : Psi(S | J_k) >= tau^2 \/ 4}|, quad quad N = max_(0 <= k <= n) N_k.
  $
]<defn:live-buckets>

By Markov on the level mass (property 5) and monotonicity (property 4), $N_k <= min(4 R_k \/ tau^2, space 2 N_(k-1))$, the latter for $k >= 1$; the instances after the theorem show how the repetition structure of $calD$ controls $N$.

#theorem[Dataset Goldreich-Levin from Context Conditioning][
  Given $sans("SAMP")$ and $sans("CSAMP")$ access to $(calD, f)$ with $norm(f)_infinity <= 1$, and $0 < tau <= 1$, $delta in (0,1)$, there is an algorithm that with probability at least $1 - delta$ outputs a list $L$ satisfying:
  + (Completeness) $|dcoeff(S)| >= tau ==> S in L$;
  + (Soundness) $S in L ==> |dcoeff(S)| >= tau \/ 2$;
  + (Complexity) on the same event, at most $O(n N dot log(n N \/ delta) \/ tau^4)$ sampling experiments and $"poly"(n, N, 1\/tau, log(1\/delta))$ time.
  Moreover, given a target $eps in (0, 1]$ and a bound $M$ on the _dataset spectral norm_ $norm(hat(f)_calD)_1 := sum_(S subset.eq [n]) |dcoeff(S)| <= M$, running at threshold $tau = min(1, eps normC \/ (2 M))$ and estimating each recovered coefficient to accuracy $tau sqrt(eps \/ 8)$, the algorithm also returns the sparse reconstruction
  $
  g = normCInv sum_(S in L) tilde(f)_calD (S) chi_S quad "with" quad EE_(x ~ calD) [(f - g)^2] <= eps,
  $
  at the cost of $O(log(|L| \/ delta) \/ (eps tau^2))$ further $sans("SAMP")$ draws.
  The algorithm needs no advance knowledge of $N$, never evaluates $f$ outside $calD$, and never tests membership: every string it touches is a datapoint.
]<thm:context-gl>

The algorithm is the classical tree search with $Psi$ in place of $W$:
+ Maintain a set of live buckets $(S, J_k)$, starting from $(emptyset, J_0)$.
+ At level $k$, split each live bucket on coordinate $k+1$ and estimate $Psi(S' | J_(k+1))$ for both children to accuracy $tau^2 \/ 8$, giving the $t$-th estimate performed (in any fixed processing order) confidence $1 - delta \/ (2 t^2)$ via @lem:psi-estimator; keep the children with estimate at least $3 tau^2 \/ 8$.
  Note the confidence schedule references only the running index $t$, so the algorithm is well-defined with no advance knowledge of $N$.
+ Output the surviving singletons at level $n$ as the list $L$.
+ _(Reconstruction.)_ For each $S in L$, estimate $dcoeff(S)$ from $sans("SAMP")$ draws to accuracy $tau sqrt(eps \/ 8)$, and return $g = normCInv sum_(S in L) tilde(f)_calD (S) chi_S$.

#proof[
  Condition on every performed estimate being $tau^2\/8$-accurate.
  The $t$-th estimate uses fresh samples, so it fails with probability at most $delta \/ (2 t^2)$ regardless of which buckets earlier estimates caused to be explored; summing over the (adaptively determined, possibly unbounded) sequence of estimates, the total failure probability is at most $sum_(t >= 1) delta \/ (2 t^2) <= delta$.

  *Completeness.* If $|dcoeff(S)| >= tau$, then by completeness (2) of @lem:psi-properties every ancestor bucket of $S$ has $Psi >= tau^2$, so its estimate is at least $7 tau^2 \/ 8 >= 3 tau^2 \/ 8$ and the ancestor survives at every level; hence $S in L$.

  *Soundness.* A surviving leaf has estimate at least $3 tau^2 \/ 8$, hence $Psi(S | [n]) >= tau^2 \/ 4$; by termination (3), $dcoeff(S)^2 >= tau^2 \/ 4$.

  *Complexity.* On the good event, a surviving bucket at level $k$ has true $Psi >= tau^2 \/ 4$, so there are at most $N_k$ of them; the level-mass identity (5) gives $N_k <= 4 R_k \/ tau^2$ (Markov), and monotonicity (4) gives $N_k <= 2 N_(k-1)$ (children of dead buckets are dead).
  Hence at most $2 n N$ estimates are performed in total, and the $t$-th costs $O(log(t \/ delta) \/ tau^4) = O(log(n N \/ delta) \/ tau^4)$ experiments.

  *Reconstruction.* Condition on the good event, so completeness holds. Let $g^arrow.t = calD compose f$ be the lift, with $hat(g^arrow.t)(S) = normCInv dcoeff(S)$ (@lem:lift) and $f = g^arrow.t$ on $calD$. Exactly as in @thm:learning-low-degree — drop the indicator and apply Parseval in $L^2 (mu)$:
  $
  EE_calD [(f - g)^2] = normC dot EE_mu [calD dot (g^arrow.t - g)^2] &<= normC dot EE_mu [(g^arrow.t - g)^2] = normC sum_S (hat(g^arrow.t)(S) - hat(g)(S))^2 .
  $
  Since $hat(g)(S) = normCInv tilde(f)_calD (S)$ for $S in L$ and $0$ otherwise, the two factors of $normCInv$ cancel the $normC$:
  $
  EE_calD [(f - g)^2] <= underbrace(normCInv sum_(S in.not L) dcoeff(S)^2, "tail") + underbrace(normCInv sum_(S in L) (dcoeff(S) - tilde(f)_calD (S))^2, "estimation") .
  $
  For the _tail_, completeness gives $|dcoeff(S)| < tau$ for every $S in.not L$, so $sum_(S in.not L) dcoeff(S)^2 <= tau sum_(S in.not L) |dcoeff(S)| <= tau norm(hat(f)_calD)_1 <= tau M$; with $tau <= eps normC \/ (2M)$ this is $normCInv tau M <= eps \/ 2$.
  For the _estimation_ term, soundness and the Mass Identity (@lem:mass) give $|L| (tau \/ 2)^2 <= sum_S dcoeff(S)^2 = normC dot EE_calD [f^2] <= normC$, so $|L| <= 4 normC \/ tau^2$; estimating each $dcoeff(S)$, $S in L$, to accuracy $tau sqrt(eps \/ 8)$ (the product $f chi_S in [-1,1]$, so Hoeffding and a union bound over $L$ cost $O(log(|L| \/ delta) \/ (eps tau^2))$ samples) makes it at most $normCInv dot |L| dot tau^2 eps \/ 8 <= eps \/ 2$.
  Summing, $EE_calD [(f - g)^2] <= eps$.
]

(For a deterministic time budget: cap the live buckets per level at $overline(N)$, abort on overflow, and guess-and-double $overline(N)$ with confidence budget halved per run; the run with $overline(N) >= N$ never aborts on the good event, at the cost of $O(log N)$ restarts.)

Three instances of the parameter $N$, spanning the possible regimes (all verified numerically):
- *Dense datasets.* Always $c_k <= min(|calD|, 2^(n-k))$, so $R_k <= min(2^k, normC)$ and $N <= 4 normC \/ tau^2$: for $normC = "poly"(n)$ the search is polynomial outright — using strictly weaker access than a membership tester.
- *Subcube-structured data.* For the subcube example above (fixing $K$, $f equiv 1$): $R_k = 2^(|K inter J_k|) <= 2^(|K|)$, matching the true output size $2^(|K|)$ — the search is output-sensitive, even though the density constant $normC = 2^(|K|)$ is itself exponential in $|K|$.
- *Generic sparse data.* Singleton context groups (one datapoint per context) give $vbar_S (z) = f(x) chi_S (x_J)$, so $Psi(S|J) = EE_calD [f^2]$ for _every_ $S$ — flat — and $c_k = |calD|$, $R_k = 2^k dot EE_calD [f^2]$: the parameter degrades to the trivial $2^k$ exactly when @lem:blindness bites, and $sans("CSAMP")$ degenerates to $sans("SAMP")$.
  The theorem's power is context repetition, and the blindness lemma says this is not an artifact.

Two further remarks.
First, nothing forces the split order $1, 2, dots, n$: any coordinate order works, and the profile $R_k$ — hence the runtime — depends on it.
For sequence data the natural choice is to keep _contiguous_ contexts (so each context group is a set of strings sharing an $(n-k)$-gram), and choosing the order to maximize repetition is a purely combinatorial preprocessing question on $calD$.
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
  For $theta in (0, 1]$ — we never take $theta$ larger — write $heavyB = {V : |b_V| >= theta}$ for the *$theta$-heavy bias set*; since $b_emptyset = 1$, always $emptyset in heavyB$, so $|heavyB| >= 1$.
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
  Let $0 < tau <= 1$, $delta in (0,1)$, fix any $theta <= tau \/ (4A)$, and suppose the $theta$-heavy bias set $heavyB$ of $calD$ is given explicitly (e.g., via @thm:context-gl applied to $f equiv 1$, or from prior structural knowledge).
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
  If $f : {-1,1}^n -> [-1,1]$ with $norm(hat(f))_1 <= A$, and $theta <= tau \/ (4A)$, then
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

== Interaction Screening: Conditioning on the Interacting Set

@lem:blindness pinpoints why a sample-only dataset GL stalls: the context-conditioning estimator splits off $J$ and conditions on the _complement_ $Jbar$ of $n - k$ coordinates, whose subcube must repeat in $calD$.
This fails above the birthday horizon $k approx n - 2 log_2 m$: to reach a degree-$d$ character the search needs $|J| >= d$, i.e. $|Jbar| <= n - d$, and the complement repeats only when $n - d lt.tilde 2 log_2 m$ — a _short-context, high-degree_ regime.

The complementary move conditions on the _interacting set itself_.
For $S subset.eq [n]$ write $nu_S := EE_(z)[ (EE_(x ~ calD)[f(x) | x_S = z])^2 ]$, the energy of the best predictor using only the coordinates $x_S$.
When $calD$ is a product distribution this is a bucket sum on the $S$-subcube,
$
nu_S = sum_(U subset.eq S) dcoeff(U)^2,
$ <eq:cond-energy>
so the _pure_ degree-$|S|$ interaction is recovered by Möbius inversion over subsets,
$
dcoeff(S)^2 = sum_(U subset.eq S) (-1)^(|S| - |U|) nu_U.
$ <eq:mobius>
Independent of any product assumption, $nu_S$ is estimated from $sans("SAMP")$ alone by the $U$-statistic over dataset points sharing $x_S$,
$
hat(nu)_S = frac(1, m) sum_(g : n_g >= 2) frac(S_g^2 - Q_g, n_g - 1), quad S_g = sum_(x in g) f(x), quad Q_g = sum_(x in g) f(x)^2,
$ <eq:nu-hat>
over the fibres $g = {x in calD : x_S = z}$ (this is the dual of @lem:pair-form — splitting on $S$ and keeping the diagonal on $S$ rather than on $Jbar$); singleton groups drop out, removing exactly the self-pair diagonal that @lem:blindness showed swamps the suffix version.

#lemma[Unbiased Conditional Energy][
  For _any_ $calD$ and any $S subset.eq [n]$, condition on the realized fibre values ${x_S^i}_(i=1)^m$. Within each fibre $g$ the labels $f(x)$ are i.i.d. with mean $mu_S (z_g) := EE_calD [f | x_S = z_g]$, and @eq:nu-hat obeys
  $
  EE[hat(nu)_S | {x_S^i}] = frac(1, m) sum_(g : n_g >= 2) n_g mu_S (z_g)^2,
  $
  the empirical mean of $mu_S (x_S)^2$ over non-singleton fibres. Hence $hat(nu)_S$ is unbiased for $nu_S$ up to the omitted singletons, and consistent for $nu_S$ whenever $2^(|S|) = o(m)$. It uses _no_ repetition on the complement $Jbar$, so the birthday-horizon obstruction of @lem:blindness never arises.
]<lem:unbiased-energy>
#proof[
  Fix a fibre $g$ with $n_g >= 2$. Then $S_g^2 - Q_g = sum_(i != j in g) f_i f_j$, so $frac(S_g^2 - Q_g, n_g (n_g - 1))$ is the $U$-statistic for the product of two i.i.d. draws, with expectation $mu_S (z_g)^2$; multiplying by $n_g$ and summing gives the display. Each singleton fibre contributes $0$ to $hat(nu)_S$ but $mu_S (z)^2 \/ m$ to the empirical proxy of $nu_S$; that omitted mass vanishes once each of the $2^(|S|)$ fibres is hit $Omega(1)$ times in expectation, i.e. $2^(|S|) = o(m)$.
]

So $hat(nu)_S$ needs only the $S$-subcube — $2^(|S|)$ cells — to repeat, i.e. $|S| lt.tilde log_2 m$, _independent of $n$_: it reaches a sparse interaction inside an arbitrarily long context, at the price of a degree cap. Under a product law this recovers the coefficients exactly.

#proposition[Interaction Screening (product $calD$)][
  Suppose $calD$ is a product distribution — so ${chi_U}$ are orthonormal under $calD$ — and fix $d$ with $2^d = o(m)$ and $tau > 0$. Then @eq:cond-energy holds, and @eq:mobius gives $dcoeff(S)^2 = sum_(U subset.eq S) (-1)^(|S| - |U|) nu_U >= 0$ _exactly_. Consequently, from $sans("SAMP")$ access alone, screening the sets of size $<= d$ with $hat(nu)_S$ (@lem:unbiased-energy) and @eq:mobius recovers every $S$ with $|S| <= d$ and $dcoeff(S)^2 >= tau^2$, in time $O(binom(n, d) dot m dot 2^d)$ — with no repetition on any complement $Jbar$, hence no birthday-horizon obstruction. It is _not_ blind in reverse: a pure degree-$d$ character ($dcoeff(S) != 0$ but $dcoeff(U) = 0$ for all $U subset.neq S$) has $nu_S = dcoeff(S)^2$ and $nu_U = 0$ for $U subset.neq S$, so it is detected.
]<prop:interaction-screening>
#proof[
  For product $calD$, $EE[f | x_S = z] = sum_(U subset.eq S) dcoeff(U) chi_U (z)$ — conditioning annihilates every character with a coordinate outside $S$ — so orthonormality gives $nu_S = sum_(U subset.eq S) dcoeff(U)^2$ (@eq:cond-energy), whose Möbius inverse is @eq:mobius. By @lem:unbiased-energy each $nu_U$, $U subset.eq S$, is estimated to accuracy $eps$ from $O(log(1\/delta)\/eps^2)$ bounded-label samples once $2^d = o(m)$; propagating $eps = Theta(tau^2)$ through the $2^(|S|)$-term sum @eq:mobius separates $dcoeff(S)^2 >= tau^2$ from $0$ with the stated resources.
]

*Non-product $calD$ (open).* When $calD$ is not a product law the characters are no longer $calD$-orthonormal (the density factor $normC$ of the Mass Identity @lem:mass), so @eq:cond-energy _fails_: $hat(nu)_S$ still consistently estimates the variance $nu_S$ explained by $x_S$ (@lem:unbiased-energy), but $nu_S != sum_(U subset.eq S) dcoeff(U)^2$, and the Möbius combination @eq:mobius is then only the _independent-input Sobol_ interaction — which can be negative. Exact recovery of $dcoeff(S)$ there requires a product $calD$, the generalized functional ANOVA for dependent inputs, or a fitted nested-degree comparison; a negative screened $I_S$ is the diagnostic signature of this regime.

@prop:interaction-screening is functional ANOVA — the Hoeffding–Sobol decomposition — read in the dataset-Fourier basis, and it is precisely what sample-only access _can_ do: screening $binom(n, d)$ sets matches the statistical-query lower bound $n^(Omega(d))$ for recovering a size-$d$ junta from random examples @kearns1998sq @blum2003noise, and no sample-only method does essentially better without breaking the noisy-parity assumption.
The extra power the dataset model buys is the stronger _subcube-conditioning_ access @chen2021junta @canonne2015testing — which learns a $d$-junta in $2^(O(d))$ rather than $n^(Omega(d))$ — realized here as "retrieve rows sharing this partial context," available exactly where $calD$ contains the conditioned subcube.
The two primitives are complementary, tiling the (degree $d$, dimension $n$) plane by the single quantity $min(d, n - d)$:

#table(
  columns: 3, align: (left, left, left),
  [*primitive*], [*conditions on*], [*available when*],
  [context-conditioning (@thm:context-gl)], [complement $Jbar$, size $n - d$], [$n - d lt.tilde log_2 m$ (high degree, short context)],
  [interaction screening (@prop:interaction-screening)], [set $S$, size $d$], [$d lt.tilde log_2 m$ (low degree, long context)],
)

The uncovered corner $min(d, n - d) gt.tilde log_2 m$ — high degree _and_ long context — is exactly the sample-only, subcube-empty regime where @lem:blindness and the statistical-query / noisy-parity barrier @kearns1998sq @blum2003noise leave no efficient method.
The boundary is soft, not sharp: shrinking the conditional means of @eq:cond-energy toward their lower-order parents (empirical Bayes) degrades $hat(nu)_S$ gracefully as the $2^(|S|)$ cells thin, giving a data-adaptive effective degree; and a kernel/embedding relaxation of the fibre ("rows with $x_S approx z$") trades bias for reach — the exact-to-soft spectrum that connects this combinatorial recovery to representation learning, which passes the worst-case barrier only by assuming the smoothness a parity lacks.
#CLAUDE[
  _Empirical illustration only — not a theorem_ (real language is the non-product regime above, so this is a held-out predictive comparison, not exact coefficient recovery). Categorical Householder version, `interaction_predict.py`: on TinyStories next-_character_ (dense $V = 27$, window $24$) the recovered degree-$3$ interactions carry held-out predictability (out-of-sample $hat(nu)$-energy positive) and lift a fitted degree-$<= 3$ model's top-$1$ over the fitted degree-$<= 2$ model; on next-_word_ (sparser) the same screen overfits (held-out degree-$3$ energy $< 0$, and negative $I_S$ appear — the non-product signature) — the $2^(|S|) lt.tilde m$ boundary of @lem:unbiased-energy made visible. The genuinely high-degree win stays in the context-conditioning corner (Poelwijk, @thm:context-gl).
]

== Application: Kushilevitz-Mansour over Datasets

Classical GL powers the Kushilevitz-Mansour learning algorithm @kushilevitz1993learning (@o2021analysis, Theorems 3.37-3.38): any $f$ with $norm(hat(f))_1 <= s$ — in particular any decision tree of size $s$ — is learnable from queries in time $"poly"(n, s, 1\/eps)$.
The spectral-norm hypothesis is exactly the $A$ of @thm:dataset-gl --- the same $L^1$-geometry exploited by agnostic decision-tree learning @gopalan2008agnostically --- so the on-distribution analogue of the coefficient-collection step comes for free:

#corollary[Heavy On-Dataset Coefficients of Decision Trees][
  Let $f : {-1,1}^n -> {-1,1}$ be computable by a decision tree of size $s$ (so $norm(hat(f))_1 <= s$), given by $sans("QUERY")$ access, and let $heavyB$ be the $theta$-heavy bias set of $calD$ for some $theta <= tau \/ (4s)$.
  Then all $S$ with $|dcoeff(S)| >= tau$ can be listed in time $"poly"(n, s, |heavyB|, 1\/tau, log(1\/delta))$.
]<cor:km-datasets>

From listing to learning: @cor:km-datasets recovers the heavy coefficients, and the reconstruction clause of @thm:context-gl turns them into an $eps_calD$-close approximation.
That clause needs nothing but the recovered heavy list and a bound on the dataset spectral norm $norm(hat(f)_calD)_1$, so — although stated there for the context-conditioning search — the same scaled reconstruction $g = normCInv sum_(S in L) tilde(f)_calD (S) chi_S$ applies verbatim to the model-query list $L$ of @cor:km-datasets.
This is the honest dataset analogue of "coefficient collection plus Parseval": the aliasing that made a naive raw-coefficient $g$ fail is absorbed by the single factor $normCInv$ (@lem:dataset-parseval), the same normalization that fixes the low-degree learner (@thm:learning-low-degree).
#TODO[
  Bound the dataset spectral norm from the global one: $norm(hat(f)_calD)_1 <= norm(b)_1 dot norm(hat(f))_1$ by @lem:convolution, so a low-$L^1$ bias spectrum together with small $norm(hat(f))_1 <= s$ makes the reconstruction of @thm:context-gl efficient in the model-query setting — genuine on-dataset _learning_ of size-$s$ decision trees, not just coefficient listing.
]

== Discussion and Open Directions

The most closely related work is the Active Fourier Auditor @ajarra2024active, which likewise estimates spectral properties of a model over a data distribution; it orthonormalizes the parity basis to that distribution (following @heidari2021finding) and reports a few scalar functionals (robustness, individual/group fairness), whereas our dataset Goldreich-Levin keeps the fixed characters over the empirical measure and recovers the heavy coefficients themselves.
See @sec:related for the fuller landscape (non-uniform Fourier, the samples-versus-queries barrier, subsampling aliasing, and Fourier auditing of trained models).

#TODO[
  - Non-uniform (weighted) datasets: the identities above should extend with $p(x)$ in place of $1\/|calD|$; the reproducing-kernel steps are unaffected, the mass identity picks up $norm(p)_2^2$-type factors.
  - General product alphabets and orthonormal bases (Hermite, tokens): the pair-form and blindness lemmas need only the reproducing kernel; the convolution identity needs the group structure ($chi_S overline(chi_T) = chi_(S - T)$), so state it over $ZZ_q^n$ or general finite abelian groups.
    For token alphabets, $sans("CSAMP")$ is literally $n$-gram retrieval, and @thm:context-gl applies as stated.
  - Choosing the split order: the profile $R_k$ depends on the coordinate order; find the order minimizing $max_k R_k$ (a combinatorial problem on the dataset's context statistics — related to building a good trie).
  - $sans("CSAMP")$ from a flat subsample only: estimating $Psi$ without a corpus index requires context collisions _within_ the subsample — a birthday/U-statistic analysis; quantify the subsample size needed as a function of the context-group sizes.
  - Characterize which real datasets (images, token corpora) have small context profiles $c_k$ along some order, and how $N$ behaves empirically; relatedly, when is the heavy-bias set $heavyB$ small and low-degree?
  - Lower bound: turn @lem:blindness into a formal sample-complexity lower bound for @defn:dataset-gl-goal in the $sans("SAMP")$-only model (two datasets, same visible statistics, different heavy sets), and extend it to $sans("CSAMP")$ with all context groups singletons.
]
