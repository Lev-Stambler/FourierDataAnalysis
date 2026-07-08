#import "../levs-commands/main_commands.typ": *
#import "../custom_commands.typ": *
#show: thmrules
#show: eqrules
#set math.equation(numbering: none)

// ============ Reusable Symbols ============
#let basis = $phi$
#let normC = $C_calD$
#let normCInv = $C_calD^(-1)$
#let Sens = "Sens"
#let bIS = $bold(S)$
#let gam = $gamma$
#let gamn = $gamma_n$
#let hermite = $h$
#let Hermite = $H$
#let dcoeff(a) = $hat(f)_calD (#a)$
#let ncoeff(a) = $breve(f)_calD (#a)$
#let nbasis = $breve(basis)$

= Fourier Analysis on a Dataset

The classical theory of Boolean functions — and, more generally, Fourier analysis over product probability spaces — is one of the most versatile toolkits in theoretical computer science.
Its basic premise, however, is that expectations are taken over a _product measure_: every coordinate is resampled independently.
Real data is not like this.
A corpus of token sequences, a folder of images, a table of feature vectors — each is a finite set $calD subset calT^n$ inside an ambient product space, and the functions we care about (a trained model, a labeling rule) are only meaningfully evaluated _on the data_.

This paper develops the elementary theory that results from one stubborn design decision: *keep the characters of the ambient product space, but take all expectations over the dataset.*
The resulting "dataset Fourier coefficients" are easy to estimate from samples and retain much of the classical dictionary — but not all of it, and the failures are precise, quantifiable, and (as we will see in the Goldreich-Levin section) algorithmically meaningful.
Throughout, think of $f$ as either a trained model, an ideal labeling function on the dataset, or some other efficiently computable function.

== Orthonormal Bases and Parseval

We first fix the ambient framework, which is standard @o2021analysis.

#definition[Probability Space with Orthonormal Basis][
  Let $Omega$ be a measurable space with probability measure $mu$, and let $I$ be an index set.
  An _orthonormal basis_ for $L^2 (Omega, mu)$ is a collection ${basis_alpha}_(alpha in I)$ of functions $basis_alpha : Omega -> RR$ satisfying:
  + *Orthonormality:* $iprod(basis_alpha, basis_beta)_mu = EE_(x ~ mu) [basis_alpha (x) basis_beta (x)] = delta_(alpha beta)$;
  + *Completeness:* every $f in L^2 (Omega, mu)$ can be written as $f = sum_(alpha in I) hat(f)(alpha) basis_alpha$.
  We always take $basis_0 = 1$.
  For product spaces $Omega^n$ with product measure $mu^(times.o n)$, the products $basis_alpha = product_i basis_(alpha_i)$ (over multi-indices $alpha$) form an orthonormal basis for $L^2 (Omega^n, mu^(times.o n))$.
]

#definition[Fourier Coefficients][
  For $f in L^2 (Omega^n, mu)$, the _Fourier coefficient_ at $alpha$ is
  $
  hat(f)(alpha) = iprod(f, basis_alpha)_mu = EE_(x ~ mu) [f(x) basis_alpha (x)],
  $
  so that $f = sum_alpha hat(f)(alpha) basis_alpha$ (in $L^2$).
]

#proposition[Parseval][
  For any $f, g in L^2 (Omega^n, mu)$,
  $
  iprod(f, g)_mu = sum_alpha hat(f)(alpha) hat(g)(alpha), quad "in particular" quad EE_(x ~ mu) [f(x)^2] = sum_alpha hat(f)(alpha)^2 .
  $
]<prop:parseval>
#proof[
  Expand both functions in the basis and use orthonormality; the interchange of sum and integral is justified by $L^2$ convergence.
]

The two running examples to keep in mind:

#example[
  _(Haar / parity basis.)_
  $Omega = {-1, 1}$ with the uniform measure.
  For $S subset.eq [n]$ the _parity functions_ $chi_S (x) = product_(i in S) x_i$ form an orthonormal basis of $L^2 ({-1,1}^n)$: for $S != T$ pick $i in S Delta T$ and note $EE[chi_(S Delta T)] = EE[x_i] dot EE[chi_(S Delta T without {i})] = 0$.
]

#example[
  _(Hermite basis.)_
  $Omega = RR$ with the standard Gaussian measure $gam$.
  The orthonormalized _probabilist's Hermite polynomials_ $hermite_0 = 1$, $hermite_1 (x) = x$, $hermite_2 (x) = (x^2 - 1)\/sqrt(2), dots$ form an orthonormal basis; their products $Hermite_alpha (x) = product_i hermite_(alpha_i) (x_i)$, $alpha in NN^n$, handle Gaussian space $L^2 (RR^n, gamn)$, with $deg Hermite_alpha = |alpha| = sum_i alpha_i$.
  (The Ornstein-Uhlenbeck noise operator and the invariance principle port over as well; we will not need them here.)
]

== Coordinate Averaging and Sensitivity

For product spaces there is a standard way to measure how much $f$ depends on a coordinate.

#definition[Coordinate Averaging and Sensitivity][
  For $i in [n]$, the _averaging operator_ is $E^i [f](x) = EE_(a ~ mu_i) [f(x^(i arrow.l a))]$, where $x^(i arrow.l a)$ replaces the $i$-th coordinate of $x$ by $a$.
  The _sensitivity of coordinate $i$_ (often called _influence_) and the _total sensitivity_ are
  $
  Sens_i [f] = EE_(x ~ mu) [(f(x) - E^i [f](x))^2], quad quad bIS [f] = sum_(i = 1)^n Sens_i [f].
  $
]

#proposition[
  $E^i$ kills exactly the basis directions that use coordinate $i$: $E^i [basis_alpha] = basis_alpha$ if $alpha_i = 0$ and $E^i [basis_alpha] = 0$ otherwise.
  Consequently $Sens_i [f] = sum_(alpha : alpha_i != 0) hat(f)(alpha)^2$ and $bIS [f] = sum_alpha \#alpha dot hat(f)(alpha)^2$, where $\#alpha = |{i : alpha_i != 0}|$.
]<prop:global-sensitivity>
#proof[
  The first claim is orthonormality in the $i$-th factor ($EE_(a ~ mu_i) [basis_(alpha_i) (a)] = iprod(basis_(alpha_i), basis_0) = delta_(alpha_i, 0)$); the rest is Parseval applied to $f - E^i f = sum_(alpha : alpha_i != 0) hat(f)(alpha) basis_alpha$.
]

This classical quantity is the spectral form of the total-effect (Sobol) index of variance-based global sensitivity analysis @sobol2001global @hooker2007generalized; estimated over data, it underlies variance-based feature attribution @datta2016algorithmic — a connection the dataset version below makes literal.

== Datasets: the Basic Dictionary

Now for the object of study.
Fix a finite alphabet $calT$ and — for this subsection through the end of the paper — let $mu$ be the *uniform* measure on $calT$, with ${basis_alpha}$ any orthonormal basis of $L^2 (calT^n)$ containing $basis_0 = 1$.
(We remark below on what survives for non-uniform $mu$.)

#definition[Dataset; Dataset Fourier Coefficient][
  A _dataset_ is a finite set $calD subset calT^n$; we write $x ~ calD$ for a uniformly random element of $calD$ and
  $
  dcoeff(alpha) = EE_(x ~ calD) [f(x) basis_alpha (x)] = frac(1, |calD|) sum_(x in calD) f(x) basis_alpha (x)
  $
  for the _dataset Fourier coefficient_ of $f : calD -> RR$ at $alpha$.
  The _density constant_ is $normC = |calT|^n \/ |calD|$.
]

Dataset coefficients are exactly the quantities estimable from labeled samples $(x, f(x))$, $x ~ calD$ — this is the point of the definition.
The price: ${basis_alpha}$ is orthonormal in $L^2 (calT^n, mu)$, _not_ in $L^2 (calD)$.
Two characters may even coincide as functions on $calD$ (we call this _aliasing_; the subcube example in the next section makes it vivid).
So none of the Parseval-based dictionary may be borrowed blindly.
The correct dictionary is routed through one object:

#definition[The Lift][
  For $f : calD -> RR$, the _lift_ $calD compose f : calT^n -> RR$ is
  $
  (calD compose f)(x) = cases(f(x) &"if" x in calD, 0 &"otherwise.")
  $
]

#lemma[Lifting Lemma][
  $hat((calD compose f)) (alpha) = normCInv dcoeff(alpha)$ for every $alpha$.
]<lem:lift>
#proof[
  $EE_(x ~ mu) [(calD compose f)(x) basis_alpha (x)] = frac(1, |calT|^n) sum_(x in calD) f(x) basis_alpha (x) = frac(|calD|, |calT|^n) dot dcoeff(alpha)$.
]

Everything true about datasets in this paper is the Lifting Lemma composed with an honest fact about $L^2 (mu)$.
Three instances follow.
First, inversion — the dataset coefficients determine $f$ on $calD$:

#proposition[Inversion][
  For every $x in calD$: $quad f(x) = normCInv sum_alpha dcoeff(alpha) basis_alpha (x)$.
]<prop:inversion>
#proof[
  Expand the lift in $L^2 (mu)$ and evaluate at $x in calD$, where $(calD compose f)(x) = f(x)$; apply @lem:lift.
]

Second, the replacement for Parseval.
There is no Parseval identity over $calD$; what is true is off by exactly the density constant.

#theorem[Mass Identity — "Parseval over a dataset"][
  For any dataset $calD$ (distinct points) and any $f : calD -> RR$,
  $
  sum_alpha dcoeff(alpha)^2 = normC dot EE_(x ~ calD) [f(x)^2].
  $
]<lem:mass>
#proof[
  Since ${basis_alpha}$ is a complete orthonormal system for the uniform measure on the finite set $calT^n$, it satisfies the _reproducing kernel identity_
  $
  sum_alpha basis_alpha (x) basis_alpha (x') = |calT|^n dot bb(1)[x = x']
  $
  (the matrix $M_(alpha, x) = basis_alpha (x) \/ sqrt(|calT|^n)$ has orthonormal rows, hence orthonormal columns).
  Now expand and swap sums:
  $
  sum_alpha dcoeff(alpha)^2 = frac(1, |calD|^2) sum_(x, x' in calD) f(x) f(x') sum_alpha basis_alpha (x) basis_alpha (x') = frac(|calT|^n, |calD|^2) sum_(x in calD) f(x)^2 = normC dot EE_calD [f^2].
  $
]

For a non-uniform finite base measure the same proof gives the weighted form $sum_alpha dcoeff(alpha)^2 = frac(1, |calD|^2) sum_(x in calD) f(x)^2 \/ mu(x)$; for continuous $Omega$ (e.g., Gaussian space) the left side diverges — the empirical measure of a finite dataset has no $L^2 (mu)$ density.
This is why we fix a finite alphabet.

Since $normC$ is enormous for small datasets, the total dataset Fourier mass is _not_ $EE_calD [f^2]$; a function of unit norm on the dataset carries $normC$ worth of squared coefficients, smeared across aliases.
The constant, however, is not a defect to be endured — it is a normalization to be absorbed.
In the language of frame theory, @lem:mass says precisely that the restricted characters ${basis_alpha|_calD}$ form a _tight frame_ for $L^2 (calD)$ with frame bound $normC$; dividing each frame vector by $sqrt(normC)$ yields a _Parseval frame_, for which every identity we care about holds over the dataset with no constants at all.

#definition[Normalized Dataset Coefficients][
  The _normalized characters_ and _normalized dataset coefficients_ are
  $
  nbasis_alpha = normC^(-1\/2) basis_alpha, quad quad ncoeff(alpha) = iprod(f, nbasis_alpha)_calD = normC^(-1\/2) dcoeff(alpha).
  $
]<defn:normalized-coeffs>

#theorem[Parseval and Closeness over the Dataset][
  For all $f, g : calD -> RR$:
  + (Parseval) $sum_alpha ncoeff(alpha)^2 = EE_(x ~ calD) [f(x)^2]$;
  + (Closeness) $EE_(x ~ calD) [(f(x) - g(x))^2] = sum_alpha (ncoeff(alpha) - breve(g)_calD (alpha))^2$;
  + (Reconstruction) $f(x) = sum_alpha ncoeff(alpha) nbasis_alpha (x)$ for every $x in calD$.
]<lem:dataset-parseval>
#proof[
  (1) is @lem:mass divided by $normC$; (2) is (1) applied to $f - g$, using linearity of $EE_calD$; (3) is @prop:inversion, with the two factors of $normC^(-1\/2)$ recombining into $normCInv$.
]

Everything on both sides of @lem:dataset-parseval is over the dataset — no expectation over $calT^n$ appears — and there are no stray constants.
The normalization is doing exactly one thing: bookkeeping the overcompleteness.
($L^2 (calD)$ has dimension $|calD|$, yet the frame has $|calT|^n$ vectors, each of norm $normC^(-1\/2)$ over $calD$; aliasing is this overcompleteness made visible.)
The raw coefficient $dcoeff(alpha) = EE_calD [f basis_alpha]$ remains the _estimable correlation_ — the quantity a sampling algorithm sees, and the natural unit for heavy-coefficient search in the Goldreich-Levin section — while $ncoeff(alpha)$ is the natural unit for energy and closeness; translating between them costs exactly one factor of $sqrt(normC)$.

#definition[$eps_calD$-closeness; Normalized Weights][
  We say $g$ is _$eps_calD$-close_ to $f$ if $EE_(x ~ calD) [(f(x) - g(x))^2] <= eps$.
  The _normalized level weights_ are
  $
  overline(W)^k_calD [f] = sum_(\#alpha = k) ncoeff(alpha)^2, quad "so that" quad sum_(k >= 0) overline(W)^k_calD [f] = EE_calD [f^2]
  $
  by @lem:dataset-parseval.
]<defn:eps-closeness>

== Averaging and Sensitivity on a Dataset

The third instance of the lift-then-use-$L^2 (mu)$ recipe concerns coordinate averaging.
On a dataset, resampling a coordinate may leave the data; the natural operator zeros out such settings — and this choice makes it agree exactly with the global operator applied to the lift.

#definition[Dataset Coordinate Averaging; Dataset Sensitivity][
  For $x in calD$,
  $
  E^i_calD [f](x) = EE_(a ~ mu_i) [calD(x^(i arrow.l a)) dot f(x^(i arrow.l a))], quad quad
  Sens^i_calD [f] = EE_(x ~ calD) [(f(x) - E^i_calD [f](x))^2],
  $
  where $calD(dot)$ denotes the indicator of the dataset.
  Note $E^i_calD [f] = E^i [calD compose f]$ restricted to $calD$, directly from the definitions.
]

#proposition[Dataset Sensitivity Bound][
  $
  Sens^i_calD [f] <= normC dot EE_(x ~ mu) [((calD compose f) - E^i [calD compose f])^2] = sum_(alpha : alpha_i != 0) ncoeff(alpha)^2,
  $
  and the inequality is an equality when $(calD compose f) - E^i [calD compose f]$ is supported on $calD$.
  Consequently $bIS_calD [f] := sum_i Sens^i_calD [f] <= sum_(k >= 1) k dot overline(W)^k_calD [f]$.
]<prop:dataset-sensitivity>
#proof[
  Write $q = (calD compose f) - E^i [calD compose f]$, so that $Sens^i_calD [f] = EE_calD [q^2] = normC dot EE_mu [calD dot q^2] <= normC dot EE_mu [q^2]$.
  Then apply @prop:global-sensitivity to the lift and @lem:lift; summing over $i$ counts each $alpha$ once per nonzero coordinate.
]

The inequality can be strict, and the naive analogue of @prop:global-sensitivity with dataset coefficients (no $normCInv$) is simply false.
A useful example to remember: $calD = {x : x_1 = 1} subset {-1,1}^n$ and $f = x_2$.
Then $Sens^2_calD [f] = 1$, while $chi_({2})$ and $chi_({1,2})$ alias on $calD$, so $sum_(alpha : alpha_2 != 0) dcoeff(alpha)^2 = 1 + 1 = 2$; in normalized units the bound of @prop:dataset-sensitivity reads $sum_(alpha : alpha_2 != 0) ncoeff(alpha)^2 = 2 \/ normC = 1$ (here $normC = 2$) — tight.

== Learning Low-Degree Functions from Dataset Samples

As a first payoff, the classical Low-Degree Algorithm @o2021analysis survives on datasets, _provided_ the reconstruction is the truncated _frame_ expansion $sum_(\#alpha <= d) ncoeff(alpha) nbasis_alpha$ — equivalently, the raw plug-in reconstruction scaled by $normCInv$.
The scaling matters: in the half-cube example above, the unscaled plug-in reconstruction of $f = x_2$ at degree $2$ returns $chi_2 + chi_(12) = 2 x_2$ on $calD$ — error $1$ with exact coefficients and zero tail — because each character is counted once per alias.
The frame reconstruction returns $(x_2 + x_1 x_2)\/2 = x_2$ on $calD$, exactly, as guaranteed by part (3) of @lem:dataset-parseval.

#theorem[Learning Low-Degree Functions over a Dataset][
  Let $f : calD -> [-1, 1]$ and suppose the normalized high-degree weight satisfies
  $
  sum_(k > d) overline(W)^k_calD [f] <= eps^2 / 4
  $
  (e.g., $d >= 4 overline(S) \/ eps^2$ suffices when $sum_k k dot overline(W)^k_calD [f] <= overline(S)$).
  Given $m = O(n^d / eps^2 dot log (n^d \/ delta))$ samples from $calD$, one can output $h$ with $EE_(x ~ calD) [(f(x) - h(x))^2] <= eps^2$ with probability at least $1 - delta$.
]<thm:learning-low-degree>
#proof[
  *Algorithm.* For each $\#alpha <= d$, estimate $dcoeff(alpha)$ by $tilde(f)(alpha) = frac(1, m) sum_(j = 1)^m f(x^((j))) basis_alpha (x^((j)))$; output the _scaled_ reconstruction $h = normCInv sum_(\#alpha <= d) tilde(f)(alpha) basis_alpha$.

  *Analysis.* Write $g^arrow.t = calD compose f$, so $hat(g^arrow.t)(alpha) = normCInv dcoeff(alpha)$ (@lem:lift) and $f = g^arrow.t$ on $calD$.
  Dropping the indicator and applying Parseval in $L^2 (mu)$:
  $
  EE_calD [(f - h)^2] = normC dot EE_mu [calD dot (g^arrow.t - h)^2] &<= normC dot EE_mu [(g^arrow.t - h)^2] \
  &= normC sum_alpha (hat(g^arrow.t)(alpha) - hat(h)(alpha))^2 .
  $
  Since $hat(h)(alpha) = normCInv tilde(f)(alpha)$ for $\#alpha <= d$ and $0$ otherwise, the $normC$ cancels against the two factors of $normCInv$:
  $
  EE_calD [(f - h)^2] <= underbrace(sum_(k > d) overline(W)^k_calD [f], <= eps^2 \/ 4 "(hypothesis)") + underbrace(normCInv sum_(\#alpha <= d) (dcoeff(alpha) - tilde(f)(alpha))^2, <= eps^2 \/ 4 "w.h.p.") .
  $
  For the second term: each $tilde(f)(alpha)$ averages $m$ i.i.d. terms in $[-1, 1]$ with mean $dcoeff(alpha)$; by Hoeffding and a union bound over the $O(n^d)$ low-degree indices, the stated $m$ makes every deviation at most $eps \/ (2 n^(d \/ 2))$, so the sum is at most $eps^2 \/ 4$ even before the (generous) factor $normCInv <= 1$.
]

Two remarks.
First, the hypothesis is stated in normalized weights, the correct analogue of "$eps^2$ of Fourier weight above degree $d$"; in raw dataset units it reads $sum_(k > d) sum_(\#alpha = k) dcoeff(alpha)^2 <= normC eps^2 \/ 4$.
Second, for a _generic_ sparse dataset the hypothesis is genuinely demanding — it asks the lift $calD compose f$ to be low-degree concentrated in $L^2 (mu)$ — and the next section shows this is not an artifact of our proof but a structural fact about datasets.
Structured datasets (dense ones, or subcube-like ones as in the example above) satisfy it naturally.
