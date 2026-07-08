#import "../levs-commands/main_commands.typ": *
#import "../custom_commands.typ": *
#show: thmrules
#show: eqrules

// ============ Reusable Symbols ============
#let gam = $gamma$
#let gamn = $gamma_n$
#let hermite = $h$
#let Hermite = $H$
#let Urho = $U_rho$
#let bIS = $bold(S)$
#let normC = $C_calD$
#let normCInv = $C_calD^(-1)$
#let Sens = "Sens"
#let inD = $calD$
#let origCoeff(alpha) = $hat(f)_"orig" (#alpha)$
#let basis = $phi$

= Orthonormal Basis Analysis

We develop a Fourier-analytic framework over general probability spaces equipped with an orthonormal basis. This generalizes both Boolean Fourier analysis (Haar basis) and Gaussian space analysis (Hermite polynomials).

== General Framework

#definition[Probability Space with Orthonormal Basis][
  Let $Omega$ be a measurable space with probability measure $mu$, and let $I$ be an index set.
  An _orthonormal basis_ for $L^2 (Omega, mu)$ is a collection ${basis_alpha}_(alpha in I)$ of functions $basis_alpha : Omega -> RR$ satisfying:
  1. *Orthonormality:* $iprod(basis_alpha, basis_beta)_mu = EE_(x ~ mu) [basis_alpha (x) basis_beta (x)] = delta_(alpha beta)$.
  2. *Completeness:* Every $f in L^2 (Omega, mu)$ can be written as $f = sum_(alpha in I) hat(f)(alpha) basis_alpha$.
]

#definition[Fourier Coefficients][
  For $f in L^2 (Omega, mu)$, the _Fourier coefficient_ at index $alpha$ is
  $
  hat(f)(alpha) = iprod(f, basis_alpha)_mu = EE_(x ~ mu) [f(x) basis_alpha (x)].
  $
]

#theorem[Fourier Expansion][
  Every $f in L^2 (Omega, mu)$ has a unique expansion
  $
  f = sum_(alpha in I) hat(f)(alpha) basis_alpha
  $
  with convergence in $L^2$.
]

#lemma[Parseval's Identity][
  For any $f in L^2 (Omega, mu)$,
  $
  norm(f)_2^2 = EE_(x ~ mu) [f(x)^2] = sum_(alpha in I) hat(f)(alpha)^2.
  $
]
#proof[
  Expanding $f$ in the orthonormal basis and using bilinearity of the inner product:
  $
  EE_(x ~ mu) [f(x)^2] &= iprod(f, f)_mu \
                       &= iprod(sum_alpha hat(f)(alpha) basis_alpha, sum_beta hat(f)(beta) basis_beta)_mu \
                       &= sum_alpha sum_beta hat(f)(alpha) hat(f)(beta) iprod(basis_alpha, basis_beta)_mu \
                       &= sum_alpha sum_beta hat(f)(alpha) hat(f)(beta) delta_(alpha beta) \
                       &= sum_alpha hat(f)(alpha)^2.
  $
  The interchange of sum and integral is justified by $L^2$ convergence.
]

#corollary[Parseval for Differences][
  For $f, g in L^2 (Omega, mu)$,
  $
  EE_(x ~ mu) [(f(x) - g(x))^2] = sum_(alpha in I) (hat(f)(alpha) - hat(g)(alpha))^2.
  $
]
#proof[
  Define $h = f - g$.
  By linearity of the Fourier transform, $hat(h)(alpha) = hat(f)(alpha) - hat(g)(alpha)$.
  Applying Parseval's identity to $h$:
  $
  EE_(x ~ mu) [(f(x) - g(x))^2] = norm(h)_2^2 = sum_alpha hat(h)(alpha)^2 = sum_alpha (hat(f)(alpha) - hat(g)(alpha))^2.
  $
]

== Sensitivity

For product spaces $Omega = Omega_1 times dots.c times Omega_n$ with product measure $mu = mu_1 times.o dots.c times mu_n$, we can define coordinate-wise sensitivity.

#definition[Coordinate Averaging][
  For $x in Omega^n$ and coordinate $i in [n]$, define
  $
  E^i [f] (x) = EE_(a ~ mu_i) [f(x^(i arrow.l a))]
  $
  where $x^(i arrow.l a)$ denotes $x$ with the $i$-th coordinate replaced by $a$.
]

#definition[Coordinate Sensitivity][
  The _sensitivity of coordinate $i$_ on $f in L^2 (Omega^n, mu)$ is
  $
  Sens_i [f] = EE_(x ~ mu) [(f(x) - E^i [f] (x))^2].
  $
]

#definition[Total Sensitivity][
  The _total sensitivity_ of $f$ is
  $
  bIS [f] = sum_(i = 1)^n Sens_i [f].
  $
]

In the literature, sensitivity is often called _influence_.
We use "sensitivity" to emphasize its interpretation as measuring how much $f$ depends on each coordinate.

== Examples of Orthonormal Bases

=== Haar Basis (Boolean Cube)

#definition[Boolean Cube][
  The Boolean cube is $Omega = {-1, 1}^n$ with the uniform measure $mu(x) = 2^(-n)$ for all $x$.
]

#definition[Haar Basis / Parity Functions][
  For $S subset.eq [n]$, define the _parity function_
  $
  chi_S (x) = product_(i in S) x_i.
  $
  The collection ${chi_S}_(S subset.eq [n])$ forms an orthonormal basis for $L^2 ({-1, 1}^n, mu)$.
]

#proposition[Haar Orthonormality][
  $iprod(chi_S, chi_T) = delta_(S T)$.
]
#proof[
  We have $chi_S (x) chi_T (x) = chi_(S Delta T) (x)$ where $S Delta T$ is the symmetric difference.
  For $S != T$, there exists $i in S Delta T$, so $EE [chi_(S Delta T)] = EE [x_i] dot EE [chi_(S Delta T without {i})] = 0$.
  For $S = T$, $chi_(S Delta T) = chi_emptyset = 1$.
]

#proposition[Boolean Sensitivity][
  For $f : {-1, 1}^n -> RR$,
  $
  Sens_i [f] = sum_(S : i in S) hat(f)(S)^2.
  $
]

=== Probabilist's Hermite (Gaussian Space)

#definition[Standard Gaussian Measure][
  The _standard Gaussian measure_ gam on $RR$ has density
  $
  d gam (x) = 1 / sqrt(2 pi) exp(-x^2 / 2) d x.
  $
  The $n$-dimensional Gaussian measure $gamn$ on $RR^n$ is the product measure $gam^(times.o n)$.
]

#definition[Univariate Hermite Polynomials][
  The _probabilist's Hermite polynomials_ $hermite_j : RR -> RR$ are the orthonormalized polynomials with respect to gam.
  The first few are:
  $
  hermite_0 (x) = 1, quad hermite_1 (x) = x, quad hermite_2 (x) = (x^2 - 1) / sqrt(2), quad hermite_3 (x) = (x^3 - 3x) / sqrt(6).
  $
]

#proposition[Hermite Orthonormality][
  $iprod(hermite_j, hermite_k)_gam = delta_(j k)$.
]

#definition[Multivariate Hermite Polynomials][
  For a multi-index $alpha in NN^n$, define
  $
  Hermite_alpha (x) = product_(i = 1)^n hermite_(alpha_i) (x_i).
  $
  The _degree_ of $Hermite_alpha$ is $|alpha| = sum_(i = 1)^n alpha_i$.
]

#theorem[Hermite Expansion][
  Every $f in L^2 (RR^n, gamn)$ has a unique expansion
  $
  f = sum_(alpha in NN^n) hat(f)(alpha) Hermite_alpha
  $
  where $hat(f)(alpha) = iprod(f, Hermite_alpha)_gamn = EE_(x ~ gamn) [f(x) Hermite_alpha (x)]$.
]

#proposition[Gaussian Sensitivity via Derivatives][
  For $f in L^2 (RR^n, gamn)$ with weak derivative $partial_i f$,
  $
  Sens_i [f] = EE_(x ~ gamn) [(partial_i f(x))^2] = sum_(alpha : alpha_i >= 1) alpha_i dot hat(f)(alpha)^2.
  $
]
#proof[
  We have $partial_i Hermite_alpha = sqrt(alpha_i) Hermite_(alpha - e_i)$ where $e_i$ is the $i$-th standard basis vector (the term vanishes if $alpha_i = 0$).
  By Parseval,
  $
  Sens_i [f] = EE [(partial_i f)^2] = sum_(alpha : alpha_i >= 1) alpha_i hat(f)(alpha)^2.
  $
]

==== Noise Operator

#definition[$rho$-Correlated Gaussians][
  For $rho in [-1, 1]$, we say $(x, y)$ are _$rho$-correlated Gaussians_ if $x ~ gamn$ and
  $
  y = rho x + sqrt(1 - rho^2) z
  $
  where $z ~ gamn$ is independent of $x$.
  We write $y ~ N_rho (x)$ for the conditional distribution of $y$ given $x$.
]

#definition[Ornstein-Uhlenbeck Operator][
  The _noise operator_ $Urho$ is defined by
  $
  Urho f(x) = EE_(y ~ N_rho (x)) [f(y)].
  $
]

#proposition[Hermite Eigenfunction Property][
  The Hermite polynomials are eigenfunctions of $Urho$:
  $
  Urho Hermite_alpha = rho^(|alpha|) Hermite_alpha.
  $
]
#proof[
  By independence of coordinates and linearity, it suffices to check the univariate case.
  For $y = rho x + sqrt(1 - rho^2) z$ with $z ~ gam$ independent, we have $EE_z [hermite_j (rho x + sqrt(1 - rho^2) z)] = rho^j hermite_j (x)$ by the generating function of Hermite polynomials.
]

==== Invariance Principle

#theorem[Gaussian Invariance Principle (Informal)][
  Let $f : {-1, 1}^n -> RR$ be a multilinear polynomial with small sensitivities.
  Let $g : RR^n -> RR$ be its _Gaussian version_: the function with the same multilinear coefficients, viewed as Hermite coefficients.
  Then $f(x)$ for $x in {-1, 1}^n$ uniform and $g(z)$ for $z ~ gamn$ have approximately the same distribution.
]

This principle allows us to transfer results between Boolean and Gaussian settings.

== Dataset-Specific Analysis

We now adapt the above to the setting where we have a finite dataset $calD subset Omega^n$.

#definition[Dataset Distribution][
  Let $calD subset Omega^n$ be a finite dataset.
  The uniform distribution over $calD$ is
  $
  p(x) = cases(1 / (|calD|) &"if" x in calD, 0 &"otherwise").
  $
]

#definition[Dataset Fourier Coefficient][
  The _dataset Fourier coefficient_ is
  $
  hat(f)_calD (alpha) = EE_(x ~ calD) [f(x) basis_alpha (x)] = 1 / (|calD|) sum_(x in calD) f(x) basis_alpha (x).
  $
]

#definition[Dataset Coordinate Averaging][
  For $x in calD$, define
  $
  E_calD^i [f] (x) = EE_(a ~ mu_i) [calD (x^(i arrow.l a)) dot f(x^(i arrow.l a))]
  $
  where $calD (y) = 1$ if $y in calD$ and $0$ otherwise.
  This operator _zeros out_ any coordinate setting that leaves the dataset.
]

#definition[Dataset Sensitivity][
  $
  Sens_calD^i [f] = EE_(x ~ calD) [(f(x) - E_calD^i [f] (x))^2].
  $
]

#if false [
// INCORRECT (kept for comparison — see the CLAUDE note below):
#proposition[Sensitivity via Fourier Coefficients][
  For orthonormal bases with product structure indexed by $(alpha_1, dots, alpha_n)$,
  $
  Sens_calD^i [f] = sum_(alpha : alpha_i != 0) hat(f)_calD (alpha)^2.
  $
]
#proof[
  We first show that $EE_calD [f dot E_calD^i f] = EE_calD [(E_calD^i f)^2]$.
  This follows because $E_calD^i$ is idempotent: applying it twice gives the same result as applying it once, since averaging over $x_i$ on a function already independent of $x_i$ has no effect.

  Now expand the definition:
  $
  Sens_calD^i [f] &= EE_calD [(f - E_calD^i f)^2] \
                   &= EE_calD [f^2] - 2 EE_calD [f dot E_calD^i f] + EE_calD [(E_calD^i f)^2] \
                   &= EE_calD [f^2] - EE_calD [(E_calD^i f)^2].
  $

  The Fourier expansion of $E_calD^i f$ only includes terms with $alpha_i = 0$.
  By Parseval: $EE_calD [f^2] = sum_alpha hat(f)_calD (alpha)^2$ and $EE_calD [(E_calD^i f)^2] = sum_(alpha : alpha_i = 0) hat(f)_calD (alpha)^2$.

  Therefore:
  $
  Sens_calD^i [f] = sum_alpha hat(f)_calD (alpha)^2 - sum_(alpha : alpha_i = 0) hat(f)_calD (alpha)^2 = sum_(alpha : alpha_i != 0) hat(f)_calD (alpha)^2.
  $
]
]
#CLAUDE[
  The proposition formerly here (deactivated above, kept for comparison) claimed $Sens_calD^i [f] = sum_(alpha : alpha_i != 0) hat(f)_calD (alpha)^2$.
  This is *false as an equality*: its proof invokes "Parseval over the dataset distribution," but ${basis_alpha}$ is orthonormal in $L^2 (Omega^n, mu)$, _not_ in $L^2 (calD)$ (see the Mass Identity, @lem:mass, in the Goldreich-Levin section: $sum_alpha hat(f)_calD (alpha)^2 = normC dot EE_calD [f^2]$, not $EE_calD [f^2]$).
  Counterexample (verified numerically): $calD = {x : x_1 = 1} subset {-1,1}^n$, $f = x_2$, $i = 2$.
  Then $Sens_calD^2 [f] = 1$, while $sum_(alpha : alpha_2 != 0) hat(f)_calD (alpha)^2 = 2$ (the coefficients at ${2}$ and ${1,2}$ alias on $calD$ and both equal $1$).

  What _is_ true (writing $calD compose f$ for the lift that is $f$ on $calD$ and $0$ elsewhere, so that $hat(calD compose f) (alpha) = normCInv hat(f)_calD (alpha)$; finite alphabet, uniform base measure): $E_calD^i [f] = E^i [calD compose f]$ exactly, and
  $
  Sens_calD^i [f] <= normC dot EE_(x ~ mu) [(calD compose f - E^i [calD compose f])^2] = normCInv sum_(alpha : alpha_i != 0) hat(f)_calD (alpha)^2,
  $
  with the inequality from dropping the indicator ($EE_calD [q^2] = normC EE_mu [calD dot q^2] <= normC EE_mu [q^2]$) and the equality from honest Parseval in $L^2 (mu)$ applied to the lift.
  On the counterexample: $1 <= 2 \/ normC = 1$ (here $normC = 2$), tight.
  A full corrected treatment of the sensitivity machinery is deferred — it is not needed for the Goldreich-Levin section, which is self-contained.
]

#definition[Total Dataset Sensitivity][
  $
  bIS_calD [f] = sum_(i = 1)^n Sens_calD^i [f].
  $
]

#definition[Fourier Weight at Level $k$][
  $
  W^k_calD [f] = sum_(|alpha| = k) hat(f)_calD (alpha)^2.
  $
]

#if false [
// INCORRECT (kept for comparison — see the CLAUDE note below):
#proposition[Total Sensitivity via Fourier Spectrum][
  $
  bIS_calD [f] = sum_(alpha) |alpha| dot hat(f)_calD (alpha)^2 = sum_(k >= 1) k dot W^k_calD [f].
  $
]
#proof[
  Summing the sensitivity formula over all $i in [n]$:
  $
  bIS_calD [f] = sum_(i = 1)^n Sens_calD^i [f] = sum_(i = 1)^n sum_(alpha : alpha_i != 0) hat(f)_calD (alpha)^2.
  $
  Each $alpha$ with $|alpha| = k$ appears in exactly $k$ of the inner sums (once for each $i$ where $alpha_i != 0$).
  Thus $bIS_calD [f] = sum_alpha |alpha| dot hat(f)_calD (alpha)^2 = sum_(k >= 1) k dot W^k_calD [f]$.
]
]
#CLAUDE[
  Deactivated: this summed the incorrect per-coordinate equality above.
  The summation argument itself is fine, so what survives is the corresponding inequality with the normalized weights $overline(W)^k_calD [f] := normCInv W^k_calD [f]$ (which satisfy $sum_k overline(W)^k_calD [f] = EE_calD [f^2]$ by @lem:mass):
  $
  bIS_calD [f] <= sum_(k >= 1) k dot overline(W)^k_calD [f],
  $
  with equality when each $Sens_calD^i$ is replaced by its lifted version.
]

#definition[Dataset Closeness][
  We say $g$ is _$eps$-close_ to $f$ over $calD$ if
  $
  EE_(x ~ calD) [(f(x) - g(x))^2] <= eps.
  $
]

#if false [
// INCORRECT (kept for comparison — see the CLAUDE note below):
#lemma[Parseval for Dataset Closeness][
  $
  EE_(x ~ calD) [(f(x) - g(x))^2] = sum_(alpha) (hat(f)_calD (alpha) - hat(g)_calD (alpha))^2.
  $
]
#proof[
  Define $h = f - g$.
  By linearity, $hat(h)_calD (alpha) = hat(f)_calD (alpha) - hat(g)_calD (alpha)$.
  Applying Parseval's identity over the dataset distribution:
  $
  EE_(x ~ calD) [h(x)^2] &= sum_alpha hat(h)_calD (alpha)^2 = sum_alpha (hat(f)_calD (alpha) - hat(g)_calD (alpha))^2.
  $
]

This lemma reduces proving $eps$-closeness to bounding the sum of squared coefficient differences.
]
#CLAUDE[
  The deactivated lemma above is off by exactly the factor $normC$: there is no Parseval identity "over the dataset distribution," and numerically the ratio of its right-hand side to its left-hand side equals $normC$ on random instances.
  The corrected statement is simply the Mass Identity (@lem:mass) applied to $f - g$:
]

#lemma[Parseval for Dataset Closeness, corrected][
  For a finite alphabet with uniform base measure,
  $
  EE_(x ~ calD) [(f(x) - g(x))^2] = normCInv sum_(alpha) (hat(f)_calD (alpha) - hat(g)_calD (alpha))^2.
  $
]<lem:dataset-closeness-corrected>
#proof[
  By linearity $hat((f - g))_calD (alpha) = hat(f)_calD (alpha) - hat(g)_calD (alpha)$; apply @lem:mass to $h = f - g$.
]

Note the $eps$-closeness on the left is still measured _over the dataset_ — only the coefficient side carries the normalization.
(For a non-uniform finite base measure the identity holds in the weighted form $sum_alpha hat(h)_calD (alpha)^2 = frac(1, |calD|^2) sum_(x in calD) h(x)^2 \/ mu(x)$; for continuous $Omega$, e.g. Gaussian space, the coefficient sum diverges.)

== Learning Low-Degree Functions

#if false [
// INCORRECT (kept for comparison — see the CLAUDE note below):
#theorem[Learning Low-Degree Fourier Functions][
  Let $f : Omega^n -> RR$ be bounded with total sensitivity $bIS_calD [f] <= S$.
  Given $d >= 4 S / eps^2$ and $m = O(n^d / eps^2 dot log n)$ samples from $calD$, one can output $g$ that is $eps$-close to $f$ with probability $>= 2 / 3$.
]
#proof[
  *High-degree weight bound:* By the total sensitivity formula,
  $
  bIS_calD [f] = sum_(k >= 1) k dot W^k_calD [f] >= sum_(k > d) k dot W^k_calD [f] >= d dot sum_(k > d) W^k_calD [f].
  $
  Thus $sum_(k > d) W^k_calD [f] <= bIS_calD [f] / d <= S / d <= eps^2 / 4$ when $d >= 4 S / eps^2$.

  *Algorithm:* For each $|alpha| <= d$, estimate $hat(f)_calD (alpha)$ by
  $tilde(f)(alpha) = 1 / m sum_(j = 1)^m f(x^((j))) basis_alpha (x^((j)))$.
  Output $g = sum_(|alpha| <= d) tilde(f)(alpha) basis_alpha$.

  *Analysis:* By Parseval for Dataset Closeness:
  $
  EE_calD [(f - g)^2] = underbrace(sum_(k > d) W^k_calD [f], <= eps^2 / 4) + underbrace(sum_(|alpha| <= d) (hat(f)_calD (alpha) - tilde(f)(alpha))^2, <= eps^2 / 4 "w.h.p.").
  $
  Total: $EE_calD [(f - g)^2] <= eps^2 / 2 < eps$.
]
]
#CLAUDE[
  The deactivated theorem above fails in two ways, but — as suspected — the constants _do_ cancel once the reconstruction is normalized correctly, and a corrected theorem survives with the same shape.

  _Failure 1 (aliasing, not scaling)._
  The plug-in output $g = sum_(|alpha| <= d) tilde(f)(alpha) basis_alpha$ counts each character once per alias class.
  Concrete counterexample with *exact* coefficients and a *satisfied* tail hypothesis (verified numerically): $calD = {x : x_1 = 1} subset {-1,1}^n$, $f = x_2$, $d = 2$.
  Here $hat(f)_calD ({2}) = hat(f)_calD ({1,2}) = 1$ (aliases on $calD$), the tail $sum_(|S| > 2) hat(f)_calD (S)^2 = 0$, yet $g = chi_2 + chi_(12) = 2 x_2$ on $calD$, so $EE_calD [(f - g)^2] = 1$.
  _Failure 2._ The analysis uses the incorrect closeness lemma, and additionally conflates the output's design coefficients $tilde(f)(alpha)$ with its dataset coefficients $hat(g)_calD (alpha)$.

  _The fix: scale the reconstruction by $normCInv$._
  On the counterexample, $normCInv (chi_2 + chi_(12)) = (x_2 + x_1 x_2) \/ 2 = x_2$ on $calD$ — exact recovery.
  In general the $normC$ from the closeness relation and the $normCInv$ in the reconstruction cancel through the inversion identity $f(x) = normCInv sum_alpha hat(f)_calD (alpha) basis_alpha (x)$ for $x in calD$ (equivalently: the scaled empirical coefficients estimate the _lifted_ coefficients $hat(calD compose f) (alpha) = normCInv hat(f)_calD (alpha)$, and honest Parseval applies to the lift in $L^2 (mu)$).
  The corrected theorem and proof follow; the closeness guarantee is still over the dataset.
]

#theorem[Learning Low-Degree Fourier Functions, corrected][
  Finite alphabet, uniform base measure.
  Let $f : calD -> [-1, 1]$ and suppose the normalized high-degree weight satisfies
  $
  normCInv sum_(k > d) W^k_calD [f] <= eps^2 / 4
  $
  (e.g., $d >= 4 overline(S) \/ eps^2$ when $sum_k k dot overline(W)^k_calD [f] <= overline(S)$).
  Given $m = O(n^d / eps^2 dot log (n^d \/ delta))$ samples from $calD$, one can output $h$ with $EE_(x ~ calD) [(f(x) - h(x))^2] <= eps^2$ with probability $>= 1 - delta$.
]<thm:learning-low-degree-corrected>
#proof[
  *Algorithm:* For each $|alpha| <= d$, estimate $hat(f)_calD (alpha)$ by $tilde(f)(alpha) = frac(1, m) sum_(j = 1)^m f(x^((j))) basis_alpha (x^((j)))$.
  Output the _scaled_ reconstruction $h = normCInv sum_(|alpha| <= d) tilde(f)(alpha) basis_alpha$.

  *Analysis:* Write $g^arrow.t = calD compose f$ for the lift, with $hat(g^arrow.t) (alpha) = normCInv hat(f)_calD (alpha)$, and note $f = g^arrow.t$ on $calD$.
  Dropping the indicator and applying Parseval in $L^2 (mu)$:
  $
  EE_calD [(f - h)^2] = normC dot EE_mu [calD dot (g^arrow.t - h)^2] &<= normC dot EE_mu [(g^arrow.t - h)^2] \
  &= normC sum_alpha (hat(g^arrow.t) (alpha) - hat(h) (alpha))^2.
  $
  Since $hat(h) (alpha) = normCInv tilde(f)(alpha)$ for $|alpha| <= d$ and $0$ otherwise, the $normC$ cancels against the two factors of $normCInv$:
  $
  EE_calD [(f - h)^2] <= underbrace(normCInv sum_(k > d) W^k_calD [f], <= eps^2 / 4 "(hypothesis)") + underbrace(normCInv sum_(|alpha| <= d) (hat(f)_calD (alpha) - tilde(f)(alpha))^2, <= eps^2 / 4 "w.h.p.").
  $
  For the second term, each $tilde(f)(alpha)$ is an average of $m$ i.i.d. terms in $[-1,1]$ with mean $hat(f)_calD (alpha)$; by Hoeffding and a union bound over the $O(n^d)$ low-degree indices, $m = O(n^d \/ eps^2 dot log(n^d \/ delta))$ samples make every deviation at most $eps \/ (2 n^(d\/2))$, so the sum is at most $eps^2 / 4$ even before the (generous) $normCInv <= 1$ factor.
  Total: $EE_calD [(f - h)^2] <= eps^2 / 2 <= eps^2$.
]

Two remarks.
First, the hypothesis is stated in _normalized_ weights: $sum_k overline(W)^k_calD [f] = EE_calD [f^2]$ by @lem:mass, so "$eps^2$-of-normalized-tail" is the correct analogue of the classical "$eps^2$-of-Fourier-weight" condition; in raw dataset units the same hypothesis reads $sum_(k > d) W^k_calD [f] <= normC eps^2 \/ 4$.
Second, for a generic sparse dataset the hypothesis is genuinely strong — the lift $calD compose f$ must be low-degree concentrated in $L^2 (mu)$ — consistent with the obstructions in the Goldreich-Levin section; structured (e.g., dense or subcube-like) datasets satisfy it naturally.
