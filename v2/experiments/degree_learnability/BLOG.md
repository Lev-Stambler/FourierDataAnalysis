# Can We Measure How Hard a Dataset Is Before Training?

## A Fourier view of data, locality, and learnability

Suppose I hand you two text datasets but do not let you train a model on either
one. Can you predict which dataset a Transformer will learn faster?

Dataset statistics such as entropy, compression ratio, and n-gram perplexity
give partial answers. But they do not directly describe the *structure* of the
prediction rule: how much signal depends on one context variable versus several,
or whether the useful variables are nearby or far away.

Fourier analysis gives us a language for exactly those questions. The hope is
to compute a spectrum from the dataset alone and use it to predict learning
difficulty before spending compute on training.

We ran a sequence of experiments pursuing that idea. The first proposed
locality scalar failed on a 32-corpus prospective test. That failure was useful:
it exposed a mismatch between the mathematical object we wanted and the
quantity we had actually measured. After correcting the attribution and the
definition of hardness, a simpler locality statistic showed a small but
transferable signal.

This is not yet a confirmed universal law of dataset hardness. It is a sharper
hypothesis, a reproducible measurement procedure, and a clear next experiment.

## The short version

For next-token prediction, define the dataset function

\[
f(x)=P(Y=\cdot\mid X=x),
\]

where \(X\) is the preceding context and \(Y\) is the next token. A categorical
Fourier or functional-ANOVA decomposition asks how much of the squared energy of
\(f\) can be explained by different subsets of context coordinates.

Our original statistic combined three things:

- how much nonconstant predictive energy was resolved;
- the interaction degree of that energy;
- how far away its support extended.

It did not predict final held-out cross-entropy on 32 unseen corpora. Raw rank
correlation was only \(\rho=0.051\), and adding it to a frozen linear predictor
made RMSE 4.76% worse.

The diagnosis uncovered three problems:

1. Historical training profiles and new test profiles used different lag grids.
2. A marginal energy increment was assigned to the farthest coordinate already
   present in its chain, rather than to the coordinate that produced the
   increment.
3. Final cross-entropy mixes irreducible dataset entropy with learning speed.
   It is not a clean measure of the difficulty of learning the available signal.

After re-profiling the historical panel on the identical lag grid, assigning
energy to the newly added coordinate, and measuring normalized convergence
difficulty, a one-feature linear model reduced transferred RMSE by 9.48% versus
the historical intercept predictor. Its paired bootstrap interval was
\([4.28\%,14.34\%]\). That corrected result was post-hoc, so we froze the same
feature and learner and tested them on 24 additional source-disjoint corpora.

The prospective result is informative but mixed. The frozen model reduced RMSE
by 6.14%, with a stratified paired-bootstrap interval of
\([1.70\%,27.96\%]\), but its mean within-domain rank correlation was only
\(\rho=0.30\), one-sided permutation \(p=0.119\). The preregistered label is
**PREDICTIVE_ONLY**: the feature transferred as a numerical predictor, but did
not establish a uniform within-domain ordering.

## What “the Fourier spectrum of a dataset” means

For ordinary Boolean Fourier analysis, a function is expanded in orthogonal
characters and Parseval's identity says

\[
E[f(X)^2]=\sum_A \widehat f(A)^2.
\]

The right-hand side divides total squared variation among subsets \(A\) of the
input variables. The Fourier degree of a term is \(|A|\): the number of variables
participating in it.

Text is neither Boolean nor uniformly distributed. Tokens are categorical, and
natural contexts are highly dependent. We therefore use a data-adapted
categorical basis: a constant direction, categorical one-hot contrasts, and
higher-order products, interpreted through nested projections under the actual
context distribution. This is the projection form of the inverse-likelihood
categorical construction in [Ferrere et al., Definition
3.1](https://arxiv.org/abs/2603.02673).

For a subset \(S\) of context coordinates, define its conditional collision
energy

\[
M(S)
=E\!\left[\left\|E[e_Y\mid X_S]\right\|_2^2\right],
\]

where \(e_Y\) is the one-hot vector for the next token. This quantity increases
as the projection learns more about \(Y\). It can be estimated directly from the
dataset using cross-fitted conditional frequencies.

If the input coordinates were independent and the basis exactly orthogonal,
increments of \(M(S)\) would recover sums of squared Fourier coefficients. On
dependent natural text, we should be more careful: the cumulative projection
energy is canonical, while a unique joint degree-radius decomposition generally
is not.

That distinction matters later.

## Degree is not enough

Degree measures how many variables interact, but not where those variables are.
A degree-one dependency at lag 1 and a degree-one dependency at lag 64 have the
same degree. A Transformer may not find them equally easy.

For a Fourier support \(A\), the natural causal radius is

\[
r(A)=\max A.
\]

A simple geometric search-volume argument says that the number of categorical
degree-\(k\) directions available inside radius \(r\) is

\[
C_k(r,q)=\binom{r}{k}(q-1)^k.
\]

This motivated an energy-weighted complexity statistic of the form

\[
G
=
\frac{
\sum_{\pi,k}\delta_{\pi,k}
\log_2\!\left[\binom{R_{\pi,k}}{k}(q-1)^k\right]
}{
\sum_\pi \widehat M(S_{\pi,3})
},
\]

where \(S_{\pi,k}\) is the first \(k\) coordinates in a random support chain,

\[
\delta_{\pi,k}
=
\left[
\widehat M(S_{\pi,k})-
\widehat M(S_{\pi,k-1})
\right]_+,
\]

and the old radius assignment was

\[
R_{\pi,k}=\max_{j\leq k}r_{\pi_j}.
\]

On controlled synthetic tasks, related degree-radius quantities produced strong
orderings. Moving otherwise matched predictive structure farther away made the
fixed learner struggle more. That encouraged us to ask whether \(G\) would also
predict absolute difficulty across unrelated natural corpora.

## The prospective test

We froze a simple linear protocol:

- 32 previously unseen corpora;
- four balanced strata: documentation, programming-language implementations,
  additional source code, and formal mathematics;
- two seeds of the same learned-position d64/l2 Transformer;
- 64 sequential H100 training cells;
- predictions frozen before any new training outcome;
- final held-out cross-entropy divided by initial cross-entropy as the primary
  target.

The ordinary controls were unigram entropy, held-out bigram cross-entropy,
lag-one mutual information, and compression rate. Fourier features were then
added to the same unpenalized linear regression.

The endpoint result was:

| Predictor | RMSE | \(R^2\) |
|---|---:|---:|
| Intercept only | 0.06670 | -0.121 |
| Ordinary controls | 0.05861 | 0.134 |
| Controls + energy/degree | 0.05673 | 0.189 |
| Controls + energy/degree/\(G\) | 0.05943 | 0.110 |

Energy and degree improved point RMSE by 3.2%, but the paired bootstrap interval
crossed zero: \([-10.75\%,18.14\%]\). Adding \(G\) worsened RMSE by 4.76%, with
an improvement interval entirely below zero: \([-10.10\%,-0.17\%]\). Raw \(G\)
versus the endpoint had Spearman \(\rho=0.051\), \(p=0.780\).

Taken literally, the frozen scalar failed.

## What was wrong

### 1. The profiles were not defined identically

The historical OLS panel had been sampled on lags \(1,2,\ldots,16\). The new
panel used the geometric bank

\[
\{1,2,4,8,16,32,64\}.
\]

It also used a different chain seed and sampling budget. The model was therefore
trained and tested on quantities bearing the same name but generated by
different support distributions.

We re-profiled all 22 historical corpora with the exact new settings. This did
not rescue the original endpoint hypothesis; in fact, corrected-grid \(G\) was
still worse. The mismatch invalidated the original comparison procedure, but it
was not the whole scientific explanation.

### 2. We attached marginal energy to the wrong location

The chain increment \(\delta_{\pi,k}\) is the additional predictive energy seen
when coordinate \(\pi_k\) is added. But the original statistic assigned that
increment to the maximum radius of the entire accumulated support.

Suppose lag 64 enters at step one. If lag 1 enters at step two and produces a
large increment, the old rule still labels the increment as radius 64. Once a
far coordinate enters a chain, every later increment remains “far.” This causes
the radial distribution to saturate: in our new panel, the old 90% radius became
constant and therefore contained no ranking information.

The maximum-support radius is appropriate for an exact Fourier coefficient with
a known support. A random-chain projection increment on dependent data is not
such a coefficient. It is a marginal contribution. Its directly observed
location is the location of the coordinate that was added.

### 3. We predicted loss, not learning difficulty

A high final cross-entropy can mean at least two different things:

- the corpus has high irreducible uncertainty;
- the corpus contains learnable structure that the model failed to capture in
  the available compute budget.

Only the second is the kind of hardness that degree and locality should predict.

Final loss confounds the amount of learnable signal with the rate at which it is
learned. This helps explain why held-out bigram cross-entropy was a good endpoint
predictor while geometric locality was not.

## Marginal Fourier locality

The corrected construction treats the random chains as a truncated,
Shapley-style attribution of projection energy.

Let \(r_{\pi_k}\) be the lag of the coordinate added at step \(k\). Define radial
mass

\[
w(r)
=
\sum_{\pi,k}
\delta_{\pi,k}\,\mathbf 1\{r_{\pi_k}=r\}.
\]

Then define the marginal locality centroid

\[
\Lambda_{\mathrm{marg}}
=
\frac{
\sum_r w(r)\log_2(1+r)
}{
\sum_r w(r)
}.
\]

This has a simple interpretation: among the low-degree predictive energy we can
resolve, how far away is the coordinate that contributes the typical marginal
unit of energy?

This is deliberately simpler than \(G\). It removes the large
\((q-1)^k\) degree term, does not normalize locality by the constant/unigram
energy, and does not pretend that a dependent-input chain increment is an exact
support coefficient.

## A better target: normalized learning time

Let

- \(F\) be final held-out CE divided by initial CE;
- \(A\) be the normalized area under that learning curve.

We define

\[
H_{\mathrm{learn}}
=
\frac{A-F}{1-F}.
\]

The numerator is the average loss remaining above the achieved endpoint. The
denominator is the total loss reduction the run eventually achieves. Roughly:

- \(H_{\mathrm{learn}}\approx0\) means the model captured its eventual gain very
  early;
- larger \(H_{\mathrm{learn}}\) means the model spent more of training above its
  eventual loss.

This is still learner- and budget-specific. That is a feature, not an accident.
The dataset spectrum is model-independent; whether a model can exploit that
spectrum is not.

## What happens after the correction

We trained a one-feature OLS model on the 22 historical corpora after
re-profiling them on the exact same lag grid. We then evaluated it on the 32 new
corpora, using \(H_{\mathrm{learn}}\) rather than terminal loss.

The marginal locality centroid reduced RMSE by **9.48%** relative to the
historical intercept-only predictor. A paired corpus bootstrap gave a 95%
interval of **[4.28%, 14.34%]**.

Within the new panel alone:

- centroid versus normalized learning difficulty had Spearman
  \(\rho=0.312\), \(p=0.082\);
- Pearson correlation was \(r=0.287\), \(p=0.111\);
- an exploratory marginal median radius had Spearman \(\rho=0.527\),
  \(p=0.00196\).

The median-radius result is tempting, but it did not transfer with a stable
direction from the historical panel. We should not promote it to the primary
claim. The smoother centroid is the defensible candidate.

The transferred model's absolute \(R^2\) remained negative because the mean of
the learning-time target shifted between the historical and new panels. The
result is therefore evidence for an incremental locality signal, not evidence
that we already possess a well-calibrated universal hardness predictor.

There is another useful decomposition. On the new panel, much of the apparent
signal in the original \(G\) came from resolved nonconstant energy normalized by
total energy, not from its spatial centroid. Once those pieces are separated,
we can ask clean questions:

- How much predictable low-degree signal exists?
- What degree does it occupy?
- At what marginal radii does it appear?
- How well does a particular learner convert each kind of signal into loss
  reduction over time?

Those are more informative than forcing everything into one search-volume
number.

## What we can and cannot claim

The strongest justified statement today is:

> A data-derived categorical projection spectrum contains information about
> finite-budget Transformer learnability. The original absolute endpoint scalar
> failed, but marginally attributed locality shows a transferable post-hoc signal
> for normalized learning speed.

We cannot yet claim:

- a universal, model-independent scalar law of dataset hardness;
- a prospective confirmation of \(\Lambda_{\mathrm{marg}}\);
- that locality will have the same coefficient for every architecture or
  positional encoding;
- that the exploratory median-radius correlation will replicate.

The controlled experiments still matter. They show causally that moving matched
predictive structure farther away can make it harder for several Transformer
configurations. The natural-corpus analysis asks a harder question: whether one
summary survives all the confounding differences among real datasets.

## The prospective test

We made the next experiment smaller conceptually, even though it used more data.

Freeze exactly one primary feature:

\[
\Lambda_{\mathrm{marg}}
=
\frac{\sum_r w(r)\log_2(1+r)}{\sum_r w(r)}.
\]

Freeze exactly one primary target:

\[
H_{\mathrm{learn}}=\frac{A-F}{1-F}.
\]

The frozen v2.7 protocol did the following:

1. Use the identical lag bank, chain distribution, seed policy, sample budget,
   and estimator for every training and test corpus.
2. Fit only an intercept and a one-feature linear model. Avoid feature selection
   on the confirmation panel.
3. Lock all predictions before training on a new, source-disjoint corpus panel.
4. Test a stratified paired RMSE improvement and a blocked rank association.
5. Treat architecture changes as a later robustness test, not as extra degrees
   of freedom in the primary analysis.

The development panel contained 54 compatible corpora. The confirmation panel
contained 24 new corpora, four each from prose, science, documentation,
programming-language implementations, application code, and formal code. Two
seeds of the fixed learned-absolute d64/l2 Transformer produced 48 sequential
H100 cells.

| Frozen predictor | RMSE | \(R^2\) | pooled Spearman \(\rho\) |
|---|---:|---:|---:|
| Historical intercept | 0.05363 | -0.041 | — |
| Marginal locality OLS | 0.05034 | 0.083 | 0.757 |

The 6.14% RMSE improvement passed its frozen interval gate. The pooled rank
correlation is large, but it should not be read as 24 exchangeable confirmations.
Within the six source strata, the Spearman correlations were
\(-0.4,0.8,-0.4,1.0,0.4,0.4\), whose mean is 0.30. The frozen blocked
permutation test gave \(p=0.119\), so the second gate did not pass.

This distinction clarifies what transferred. Marginal locality captured useful
between-domain structure—for example, prose and science received lower frozen
predictions than code-heavy strata—but the four-corpus rankings inside every
domain were not consistently correct. One application-code corpus, Blender,
was also a conspicuous high-difficulty residual. A model-independent dataset
feature can therefore carry real predictive information without being a
sufficient scalar ordering of all datasets.

The original ambition survives in a narrower, evidence-backed form: a
dataset-only spectral measurement, frozen before training, predicted some of how
slowly this specified learner captured available signal. The next experiment
should increase the number of corpora *within* each domain and test whether the
blocked rank signal resolves, rather than adding more hand-selected features.

---

The implementation, frozen protocols, compact audit chains, and complete result
artifacts are available in this repository. The exact categorical basis and
projection definitions are documented in [`LOCALITY_MATH.md`](LOCALITY_MATH.md),
and the full experimental history is summarized in [`VERDICT.md`](VERDICT.md).

## Correction: sample corpus positions, not an ordered stream

The v2.7 result contained a sampling confound that the hash audit could not
detect. Its Fourier measurement described a prefix, the learner advanced through
one cyclic ordering, and validation described the tail. These are reproducible
procedures, but reproducibility is not distributional alignment. Repository
concatenations often change genre across position; in Blender, a large generated
numeric file near the tail made the issue obvious.

The v2.8 repair first permuted 16 KiB blocks into disjoint train, profile, and
validation sets. It then sampled whole context/next-byte windows uniformly from
the appropriate blocks. The contexts remain contiguous—there is no token or byte
shuffling—so the local conditional structure that the Fourier statistic is meant
to measure is preserved. The only discarded information is accidental global
file order.

After rebuilding the development measurements and freezing a new OLS, the same
24 held-out sources gave a 26.0% RMSE reduction, `R^2=0.453`, and pooled
`rho=0.806`. The stratified bootstrap interval, `[18.1%,35.0%]`, is comfortably
positive. Yet the mean rank correlation inside the six domains is `0.067`
(`p=0.410`). That combination is not paradoxical: the scalar predicts broad
domain-level differences well but leaves much of the within-domain difficulty
unexplained.

So the strongest defensible statement is now sharper: a dataset-only marginal
Fourier-locality statistic predicts a substantial fraction of learning-time
variation for this fixed Transformer under a correctly randomized window law.
It is not yet a universal scalar theory of dataset hardness. The next decisive
test is a newly sourced panel with more datasets per domain, frozen after this
sampling correction—not more feature search on these 24 sources.

## Architecture matching through realized Fourier-character CE

The architecture-conditioned experiment asks the most direct version of the
question. Train each architecture to learn every exact Fourier character through
degree three. For each architecture/support pair, retain median held-out CE curve
area. Then weight those empirical hardness values by the dataset's measured
Fourier support energy.

This construction makes no monotonic locality claim. An architecture may have an
irregular response across lags and support shapes; the empirical character CE
surface preserves that irregularity. The only predictive question is whether a
natural dataset is harder when its Fourier energy lies on characters that the
same architecture actually learned slowly.

The resulting scalar is

\[
\Omega_{\mathrm{CE}}(D,m)
=\frac{\sum_A e_D(A)h_m(A)}{\sum_A e_D(A)},
\]

where \(e_D(A)\) is dataset Fourier support energy and \(h_m(A)\) is realized
held-out CE character hardness. It will be compared against the strong ordinary
controls by grouped leave-one-corpus-out prediction before any confirmation
training begins.
