#import "../levs-commands/main_commands.typ": *
#show: thmrules
#show: eqrules
#set math.equation(numbering: none)

#let TotalSpace = $calT^(otimes n)$
#let Size = $|calT|^n$
#let pScale = $p_"scale"$
#let Inf = "Inf"
#let normC = $C_calD$
#let normCInv = $C_calD^(-1)$
#let orig = "OG"
#let origCoeff(S) = $hat(f)_orig (#S)$
#let inD = $calD$
#let bI = $bold(I)$

= Core Idea: Datasets and Basic In-Distribution Testing
Ideally, we'd like to take the useful tools of Fourier Analysis (which assumes a product space) and generalize them to any distribution where the underlying super-space is a product space #footnote[
  This includes, but is not limited too, language datasets over tokens, images over pixels, and other similar data modalities.
].
Though, we do not have a formal method of reasoning about this, we do not think that it is quite possible.

Rather, we will attempt to think about analysis as over a _dataset:_ i.e. we will think of our distribution as being defined by a finite-sized dataset, $calD$, and the probability vector will be defined as $
p(x) = cases(frac(1, |calD|) " if " x in calD, 0 "otherwise") " " .
$

More formally, we will be over a space $calT^n$ (think of $calT$ as either $RR$ or the space of tokens etc.) and $calD subset calT^n$.
Then, we will be focusing on functions $f in calT^n -> RR$. $f$ can either be a trained model, ideal labeling function on the dataset, or some other efficiently computable function.

Then, we want to reason about Fourier coefficients as $
hat(f) (S) = EE_(x ~ calD) [f(x) chi^(-1)_S (x)].
$
Further, we will abuse notation to denote $calD : calT^n -> {0, 1}$ as the indicator function for the inclusion within the dataset (i.e. an in-distribution tester).
We will write $hat(f)_orig (S)$ to denote the normal ("original") Fourier coefficient:
$
origCoeff(S) = EE_(x ~ calT^n) [f(x) chi^(-1)_S (x)].
$

Importantly, notice that $
hat(f) (S) = frac(1, |calD|) sum_(x in calD) f(x) chi_S (x) = frac(|calT^n|, |calD|) dot origCoeff(calD circ f) 
$
where $circ$ is the element wise composition.
#TODO[I think we need to use $chi^(-1)$?]

$frac(calT^n, |calD|)$ is a normalizing constant which we will frequently arise.
As such, we will denote $frac(|calT|^n, |calD|)$ as $C_calD$ and its inverse as $C_calD^(-1)$ for ease of notation.

We conveniently have our first lemma.

#lemma[
  For all $x in calD$,
  $
  f(x) = normCInv sum_S hat(f) (S) chi_S (x)
  $
]
#proof[
  First, note that $f(x) = calD(x) dot f(x)$ for $x in calD$ and then $hat(calD circ f)_orig (S) = normCInv dot hat(f) (S)$ and as such, using the standard Fourier identity
  $
  calD(x) dot f(x) = sum_S hat(calD circ f) (S) chi_S (x) = normCInv sum_S hat(f) (S).
  $
]



== In Distribution Testing and Related Notions
Unfortunately, we were not able to find a good way to define property testing, _soley_ over the distribution.
Rather, we will need to test both in-distribution and out-of-distribution samples.
Still, we will only need to test out-of-distribution samples in a very limited way: samples which are only hamming distance $1$ away from in-distribution samples for most of our properties.

To denote the need for in-distribution testing, we will append $calD$ as a subscript to various operators.

#definition[Distribution-Testing Coordinate Averaging][
  Let $E_calD^i$ for $i in n$ be the $i$-th in distribution operator for $x in calD$:
  $
  E_calD^i [ f ] (x) = EE_(a in calT) [calD(x_i mapsto a) circ f (x^(x_i mapsto a))].
  $
]<defn:indistr-coord-avg>
Note that @defn:indistr-coord-avg _zeros out_ any coordinate setting which does not remain within the dataset.
Intuitively, we can think about $E_calD^i$ as the generic coordinate averaging operator for a function which will test whether an input is in the dataset and output $0$ if it is not.

We can now define influence:
#definition[$i$-th Coordinate Distribution-Testing Influence Operator][
  $
  Inf_calD^i f = EE_(x in calD) [(f(x) - E_calD^i f (x))^2].
  $
]

#proposition[
  We now prove that basic identities still hold as in O'Donnell's Analysis of Boolean Functions @o2021analysis:
  $
  Inf_calD^i f &= normC iprod(calD circ f, calD circ (f - E_D^i)), \
  E_D^i_calD^i f &= normC sum_(S, S_i = 0) hat(f) (S) chi_S (x)  \
  Inf_calD^i f &= normC sum_(S, S_i != 0) hat(f) (S)^2.
  $
]
#proof[
  We start with the second equality. #TODO[check constants]
  Note that, $
  E_inD^i_inD^i (x) &= EE_(a in calT) [calD(x_i mapsto a) circ f (x^(x_i mapsto a))] \
                   &= sum_(S, S_i = 0) origCoeff(f) (S) chi_S (x) \
                   &= normC sum_(S, S_i = 0) hat(f) (S) chi_S (x).
  $

  The first equality holds as we note that, $
  Inf_inD^i f &= normC^(-1) iprod(f(x) - E_inD^i (x), f(x) - E_inD^i (x)) \
              &= normC^(-1) (EE [f(x)^2] - 2 E_x [f(x) E_inD^i (x)] + EE_calD [E_inD^i (x)^2]).
  $
  #TODO[norm constant]
  Then, note that by the second equality and Parseval's theorem, $E_(x ~ calT^n) [f(x) E_inD^i (x)] = sum_(S, S_i = 0) origCoeff(S)^2$ and $EE_calD [E_inD^i (x)^2] = sum_(S, S_i = 0) hat(f) (S)^2$ as desired.

  Finally, we note that as $Inf_inD^i f = EE_calD [f(x)^2] - EE_calD [E_inD^i (x)^2]$, we can see that $
  EE_calD [f(x)^2] - E_x [f(x) E_inD^i (x)] = sum_S hat(f) (S)^2 - sum_(S, S_i = 0) hat(f) (S)^2 = sum_(S, S_i != 0) hat(f) (S)^2.
  $

]

Just as in #TODO[cite], we can define total influence and get some convenient corollaries:
#definition[
  Total Influence, $bI_inD$
][
  $
  bI_inD [f] = sum_(i in [n]) Inf_inD^i [f].
  $
]
We immediately get:
#proposition[
  $
  bI_inD [f] = sum_S \# S " " hat(f) (S)^2
  $
  which, for finite groups, we get
  $
  bI_inD [f] = sum_S k dot W^k [f].
  $
]
as in page 213 of #TODO[a] where $\# S = |"supp" (S)| = "supp" (S) = {i : S_i != 0}$.

Finally, we introduce one more definition which will capture the closeness of two functions, $f, g$ over $calD$.

#definition[In-Distribution Closeness, $eps_calD$-closeness][
  We say that a function $g$ is $eps_calD$ close to $f$ if:
  $
  Expec_(x in calD) [(f(x) - g(x))^2] &<= eps \
  $
  or equivalently,
  $
  normCInv Expec_(x in TotalSpace) [(calD circ f(x) - calD circ g(x))^2] <= eps.
  $
]

== Immediate Consequences
#h3([
  Learning from Random Examples
])
We can already adopt learning low-degree functions from random examples to our setting.
Specifically, if the in-distribution Fourier mass is concentrated on low-degree terms, we can learn the function from random examples drawn from $calD$.

#TODO[this is auto-gened, check over and make right! CLAUDE CHECK]
#theorem[
  Let $f : calT^n -> RR$ be a function such that $sum_(S : |S| > d) hat(f) (S)^2 <= eps^2 / 4$.
  Then, there exists an algorithm which, given $O(frac(n^d, eps^2) log(n))$ random examples from $calD$, outputs a function $g$ which is $eps_calD$ close to $f$ with probability at least $2/3$.
]

#h3([
  One-Way Property Testing for Computable via Decision Tree
])

#if false [
We continue onwards to prove that 

Let $calD$ be some distribution over alphabet (tokens) $calT$ with $n$ elements.
I.e. $calD subset.eq calT^(otimes n)$.
Importantly, we do not require that non-zero elements of $calD$ factorize into a product distribution, nor do we require that $calD$ is uniform over its elements.
Then, we define $p : calT^(otimes n) -> RR$ to be the probability function for $calD$.


Then, note that for a function defined over $f : calT^(otimes n) -> SS^m$ (with $SS^m$ being the $m$-dimensional sphere), $sum_x p(x) dot f(x) = EE_(x ~ calD) [f(x)]$ and $Size dot EE_(x ~ TotalSpace)[p(x) dot f(x)] = EE_(x ~ calD) [f(x)]$.
For ease of notation, let $pScale = Size dot p$

So then, we have, for Fourier transform coefficient $calF$,
$
calF(pScale dot f)(S) &= EE_(x ~ TotalSpace) [pScale dot f(x) dot chi^(-1)_S (x)].
                      &= EE_(x ~ calD) [f(x) dot chi^(-1)_S (x)] \
$
We will write $hat(f)(S)$ to denote $EE_(x ~ TotalSpace) [p dot f(x) dot chi^(-1)_S (x)]$ which we note to be the Fourier coefficient on $S$ for function $p dot f(x)$.
]

== In Distribution Testing Definitions
Surprisingly, if want to characterize the complexity (or sensitivity or one of many Fourier properties) of our function $f$ over distribution $calD$ and in-distribution testing, we can more or less use standard analysis.

In more detail, we will model our modified function $g : Size -> RR^+^m union {0}$ as $
g(x) = cases(f(x) " if " x in calD, 0 " otherwise")
$
And, taking the standard inner-product (TODO: we are over sphere/ real numbers, first take the inner-product then the other thing!)

Note that $g$ is required to differentiate between in and out-of distribution.
#h3([Influence and Related Operators])
We would like coordinate wise influence to be defined in the standard way, but adopted to our setting:
$
Inf^i_calD [g] = Expec_(x in calD) [(g - sfE^i_calD g)^2]
$
where $sfE^i_calD$ is the $i$-th coordinate expectation operator:
$
[E^i_calD g] (x) = Expec_(x_i in calT) [g(x^(i mapsto x_i))].
$
In words, the $i$-th expectation operator "averages out" the $i$-th coordinate over the dataset while the Influence measures the difference.

#TODO[define distribution coeffs!]
#TODO[This prob dist thing doesn't work??? Just have a subset thingy for now!!!]
Then, we have the following proposition:
#proposition[
$
E^i_calD g = sum_(s : s_i != 0) hat(pScale dot f) (s) hat(f)_calD 
$
]

#lemma[
  We can re-write the influence as an inner-product
  $
  Inf^i_calD [f] = iprod(p dot (g - E^i_calD g), g) = iprod(p dot (g - E^i_calD g), g - E^i_calD g)
  $
]
#proof[
  The second equality follows directly from the definition.
  The first is because $
   iprod(p dot (g - E^i_calD g), g - E^i_calD g) &= EE_x [g^2] - 2 EE_x [g dot E^i_calD f] + EE_x [(E^i_calD f) ^2]
  $
]


#TODO[What happens if you take $f = calD$?]
#TODO[
  Is there a monotonic question of degree over dataset vs degree over full space?
  (you can then know if a model class can learn it or not)
  (!!!! This is a good idea. People would care a lot more !!!!)
  Look into: Statistical Learning theory: _distribution agnostic learning theory_
]

// Active Fourier Auditor for Estimating
// Distributional Properties of ML Models
// https://arxiv.org/pdf/2410.08111
