#import "../levs-commands/main_commands.typ": *
#show: thmrules
#show: eqrules

// ============ Reusable Symbols ============
#let gam = $gamma$
#let gamn = $gamma_n$
#let hermite = $h$
#let Hermite = $H$
#let Urho = $U_rho$
#let bIG = $bold(I)$
#let normC = $C_calD$
#let normCInv = $C_calD^(-1)$
#let Inf = "Inf"
#let inD = $calD$
#let origCoeff(alpha) = $hat(f)_"orig" (#alpha)$

= Gaussian Space Analysis

We reformulate the Fourier-analytic framework over Gaussian space, following O'Donnell @o2021analysis Chapter 11.
The key insight is that Hermite polynomials serve as the orthonormal basis for $L^2 (RR^n, gamn)$, analogous to characters $chi_S$ on the Boolean cube.

== Gaussian Space Foundations

#definition[Standard Gaussian Measure][
  The _standard Gaussian measure_ gam on $RR$ has density
  $
  d gam (x) = 1 / sqrt(2 pi) exp(-x^2 / 2) d x.
  $
  The $n$-dimensional Gaussian measure $gamn$ on $RR^n$ is the product measure $gam^(times.o n)$.
]

#definition[Gaussian Inner Product][
  For $f, g in L^2 (RR^n, gamn)$, the inner product is
  $
  iprod(f, g)_gamn = EE_(x ~ gamn) [f(x) g(x)].
  $
]

#definition[Dataset in Gaussian Space][
  Let $calD subset RR^n$ be a finite dataset.
  We define the uniform distribution over $calD$ by
  $
  p(x) = cases(1 / (|calD|) &"if" x in calD, 0 &"otherwise").
  $
  We abuse notation and write $calD : RR^n -> {0, 1}$ for the indicator function of $calD$.
]

== Hermite Polynomials

The Hermite polynomials form a complete orthonormal basis for $L^2 (RR, gam)$.

#definition[Univariate Hermite Polynomials][
  The _probabilist's Hermite polynomials_ $hermite_j : RR -> RR$ are the orthonormalized polynomials with respect to gam.
  The first few are:
  $
  hermite_0 (x) = 1, quad hermite_1 (x) = x, quad hermite_2 (x) = (x^2 - 1) / sqrt(2), quad hermite_3 (x) = (x^3 - 3x) / sqrt(6).
  $
]

#proposition[
  The Hermite polynomials satisfy $iprod(hermite_j, hermite_k)_gam = delta_(j k)$.
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
  where the _Hermite coefficients_ are $hat(f)(alpha) = iprod(f, Hermite_alpha)_gamn = EE_(x ~ gamn) [f(x) Hermite_alpha (x)]$.
]

== Noise Operator

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

== Influence in Gaussian Space

#definition[Gaussian Influence][
  The _influence of coordinate $i$_ on $f in L^2 (RR^n, gamn)$ is
  $
  Inf_i [f] = EE_(x ~ gamn) [(partial_i f(x))^2]
  $
  where $partial_i f = (partial f) / (partial x_i)$ is the partial derivative.
]

#proposition[Hermite Representation of Influence][
  $
  Inf_i [f] = sum_(alpha : alpha_i >= 1) alpha_i dot hat(f)(alpha)^2.
  $
]
#proof[
  We have $partial_i Hermite_alpha = sqrt(alpha_i) Hermite_(alpha - e_i)$ where $e_i$ is the $i$-th standard basis vector (and the term vanishes if $alpha_i = 0$).
  By Parseval,
  $
  Inf_i [f] = EE [(partial_i f)^2] = sum_(alpha : alpha_i >= 1) alpha_i hat(f)(alpha)^2.
  $
]

#definition[Total Influence][
  The _total influence_ of $f$ is
  $
  bIG [f] = sum_(i = 1)^n Inf_i [f] = sum_(alpha in NN^n) |alpha| dot hat(f)(alpha)^2.
  $
]

== Dataset-Specific Analysis

We now adapt the above to the setting where we have a finite dataset $calD subset RR^n$.
The key insight is relating dataset coefficients to "original" Gaussian coefficients via the indicator function.

#definition[Original Hermite Coefficient][
  For $f : RR^n -> RR$, the _original Hermite coefficient_ is
  $
  origCoeff(alpha) = EE_(x ~ gamn) [f(x) Hermite_alpha (x)].
  $
]

#definition[Dataset Hermite Coefficient][
  The _dataset Hermite coefficient_ is
  $
  hat(f)_calD (alpha) = EE_(x ~ calD) [f(x) Hermite_alpha (x)] = 1 / (|calD|) sum_(x in calD) f(x) Hermite_alpha (x).
  $
]

#definition[Normalizing Constant][
  Let $gamn (calD) = EE_(x ~ gamn) [calD (x)]$ be the Gaussian measure of the dataset.
  The normalizing constant is
  $
  normC = 1 / (gamn (calD) dot |calD|).
  $
  Importantly, we have the relation
  $
  hat(f)_calD (alpha) = normC dot hat(calD circ f)_"orig" (alpha)
  $
  where $(calD circ f)(x) = calD (x) dot f(x)$.
]

#lemma[Reconstruction][
  For all $x in calD$,
  $
  f(x) = normCInv sum_(alpha in NN^n) hat(f)_calD (alpha) Hermite_alpha (x).
  $
]
#proof[
  For $x in calD$, we have $f(x) = calD (x) dot f(x)$.
  Applying the standard Hermite expansion to $calD circ f$ over $(RR^n, gamn)$:
  $
  calD (x) dot f(x) = sum_alpha hat(calD circ f)_"orig" (alpha) Hermite_alpha (x) = normCInv sum_alpha hat(f)_calD (alpha) Hermite_alpha (x).
  $
]

#definition[Dataset Coordinate Averaging][
  For $x in calD$, define
  $
  E_calD^i [f] (x) = EE_(a ~ gam) [calD (x^(i mapsto a)) dot f(x^(i mapsto a))]
  $
  where $x^(i mapsto a)$ denotes $x$ with the $i$-th coordinate replaced by $a$.
  This operator _zeros out_ any coordinate setting that leaves the dataset.
]

#proposition[Hermite Expansion of Coordinate Average][
  $
  E_calD^i [f] (x) = normCInv sum_(alpha : alpha_i = 0) hat(f)_calD (alpha) Hermite_alpha (x).
  $
]
#proof[
  By the reconstruction lemma, $f(y) = normCInv sum_alpha hat(f)_calD (alpha) Hermite_alpha (y)$ for $y in calD$.
  The multivariate Hermite polynomial factors as $Hermite_alpha (x^(i mapsto a)) = Hermite_(alpha_(-i)) (x_(-i)) dot hermite_(alpha_i) (a)$.
  Applying $E_calD^i$:
  $
  E_calD^i [f] (x) &= EE_(a ~ gam) [calD (x^(i mapsto a)) dot f(x^(i mapsto a))] \
                   &= normCInv sum_alpha hat(f)_calD (alpha) Hermite_(alpha_(-i)) (x_(-i)) dot EE_(a ~ gam) [calD (x^(i mapsto a)) hermite_(alpha_i) (a)].
  $
  When $alpha_i >= 1$, the integral $EE_(a ~ gam) [hermite_(alpha_i) (a) dot (dots.c)]$ vanishes since $EE_(a ~ gam) [hermite_k (a)] = 0$ for $k >= 1$.
  Thus only $alpha_i = 0$ terms survive.
]

#definition[Dataset Influence][
  $
  Inf_calD^i [f] = EE_(x ~ calD) [(f(x) - E_calD^i [f] (x))^2].
  $
]

#proposition[Influence via Hermite Coefficients][
  $
  Inf_calD^i [f] = sum_(alpha : alpha_i >= 1) hat(f)_calD (alpha)^2.
  $
]
#proof[
  We first show that $EE_calD [f dot E_calD^i f] = EE_calD [(E_calD^i f)^2]$.
  This follows because $E_calD^i$ is idempotent: applying it twice gives the same result as applying it once, since averaging over $x_i ~ gam$ on a function already independent of $x_i$ has no effect.

  Now expand the definition:
  $
  Inf_calD^i [f] &= EE_calD [(f - E_calD^i f)^2] \
                 &= EE_calD [f^2] - 2 EE_calD [f dot E_calD^i f] + EE_calD [(E_calD^i f)^2] \
                 &= EE_calD [f^2] - EE_calD [(E_calD^i f)^2].
  $

  By Parseval's identity over $calD$, we have $EE_calD [f^2] = sum_alpha hat(f)_calD (alpha)^2$.
  By the Hermite expansion of $E_calD^i f$ (which only includes terms with $alpha_i = 0$), we get $EE_calD [(E_calD^i f)^2] = sum_(alpha : alpha_i = 0) hat(f)_calD (alpha)^2$.

  Therefore:
  $
  Inf_calD^i [f] = sum_alpha hat(f)_calD (alpha)^2 - sum_(alpha : alpha_i = 0) hat(f)_calD (alpha)^2 = sum_(alpha : alpha_i >= 1) hat(f)_calD (alpha)^2.
  $
]

#definition[Total Dataset Influence][
  $
  bIG_calD [f] = sum_(i = 1)^n Inf_calD^i [f].
  $
]

#definition[Hermite Weight at Level $k$][
  $
  W^k_calD [f] = sum_(|alpha| = k) hat(f)_calD (alpha)^2.
  $
]

#proposition[Total Influence via Hermite Spectrum][
  $
  bIG_calD [f] = sum_(alpha in NN^n) |alpha| dot hat(f)_calD (alpha)^2 = sum_(k >= 1) k dot W^k_calD [f].
  $
]
#proof[
  Summing Proposition 5.8 (Influence via Hermite Coefficients) over all $i in [n]$:
  $
  bIG_calD [f] = sum_(i = 1)^n Inf_calD^i [f] = sum_(i = 1)^n sum_(alpha : alpha_i >= 1) hat(f)_calD (alpha)^2.
  $
  Each $alpha$ with $|alpha| = k$ appears in exactly $k$ of the inner sums (once for each $i$ where $alpha_i >= 1$).
  Thus $bIG_calD [f] = sum_alpha |alpha| dot hat(f)_calD (alpha)^2 = sum_(k >= 1) k dot W^k_calD [f]$.
]

#definition[Dataset Closeness][
  We say $g$ is _$eps$-close_ to $f$ over $calD$ if
  $
  EE_(x ~ calD) [(f(x) - g(x))^2] <= eps.
  $
]

#lemma[Parseval for Dataset Closeness][
  $
  EE_(x ~ calD) [(f(x) - g(x))^2] = sum_(alpha in NN^n) (hat(f)_calD (alpha) - hat(g)_calD (alpha))^2.
  $
]
#proof[
  Apply Parseval to $f - g$: by Definition 5.2, $hat((f - g))_calD (alpha) = hat(f)_calD (alpha) - hat(g)_calD (alpha)$.
]

This lemma reduces proving $eps$-closeness to bounding the sum of squared coefficient differences.

== Hypercontractivity

#theorem[(2,q)-Hypercontractivity][
  For $q >= 2$ and $rho <= 1 / sqrt(q - 1)$,
  $
  norm(Urho f)_q <= norm(f)_2.
  $
]

#corollary[Level-$k$ Inequality][
  If $f$ has Hermite expansion supported on degree at most $k$, then for $q >= 2$:
  $
  norm(f)_q <= (q - 1)^(k / 2) norm(f)_2.
  $
]
#proof[
  Apply hypercontractivity with $rho = 1 / sqrt(q - 1)$; since $f$ is degree-$k$, we have $Urho f = rho^(<=k) f$ where the lowest eigenvalue is $rho^k$.
]

== Invariance Principle

The Invariance Principle connects Boolean analysis to Gaussian analysis.

#theorem[Gaussian Invariance Principle (Informal)][
  Let $f : {-1, 1}^n -> RR$ be a multilinear polynomial with small influences.
  Let $g : RR^n -> RR$ be its _Gaussian version_: the function with the same multilinear coefficients, viewed as Hermite coefficients.
  Then $f(x)$ for $x in {-1, 1}^n$ uniform and $g(z)$ for $z ~ gamn$ have approximately the same distribution.
]

This principle allows us to transfer results between Boolean and Gaussian settings, and is crucial for applications like the Majority Is Stablest theorem.

== Learning Low-Degree Functions

#theorem[Learning Low-Degree Hermite Functions][
  Let $f : RR^n -> RR$ be bounded with total influence $bIG_calD [f] <= I$.
  Given $d >= 4 I / eps^2$ and $m = O(n^d / eps^2 dot log n)$ samples from $calD$, one can output $g$ that is $eps$-close to $f$ with probability $>= 2 / 3$.
]
#proof[
  *High-degree weight bound:* By Proposition (Total Influence via Hermite Spectrum),
  $
  bIG_calD [f] = sum_(k >= 1) k dot W^k_calD [f] >= sum_(k > d) k dot W^k_calD [f] >= d dot sum_(k > d) W^k_calD [f].
  $
  Thus $sum_(k > d) W^k_calD [f] <= bIG_calD [f] / d <= I / d <= eps^2 / 4$ when $d >= 4 I / eps^2$.

  *Algorithm:* For each $|alpha| <= d$, estimate $hat(f)_calD (alpha)$ (Definition 5.2) by
  $tilde(f)(alpha) = 1 / m sum_(j = 1)^m f(x^((j))) Hermite_alpha (x^((j)))$.
  Output $g = sum_(|alpha| <= d) tilde(f)(alpha) Hermite_alpha$.

  *Analysis:* By Parseval for Dataset Closeness (Lemma 5.11):
  $
  EE_calD [(f - g)^2] = underbrace(sum_(k > d) W^k_calD [f], <= eps^2 / 4) + underbrace(sum_(|alpha| <= d) (hat(f)_calD (alpha) - tilde(f)(alpha))^2, <= eps^2 / 4 "w.h.p.").
  $
  Total: $EE_calD [(f - g)^2] <= eps^2 / 2 < eps$.
]

