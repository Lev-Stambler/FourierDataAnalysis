#import "../levs-commands/main_commands.typ": *
#show: thmrules
#show: eqrules
#set math.equation(numbering: none)

#let Sens = "Sens"

= Related Work <sec:related>

Our starting point --- the Fourier analysis of functions on ${-1,1}^n$ --- is classical @o2021analysis.
What changes is the _measure_: we replace the uniform distribution on the cube by the uniform distribution on a finite dataset $calD$, _keep_ the parity characters $chi_S$, and ask what of the Fourier toolkit survives.
This one choice --- fixed basis, empirical measure --- forces the non-orthogonality defect $C_calD = 2^n \/ |calD|$, the bias-spectrum convolution, and the samples-only "blindness" barrier.
We organize prior work around it.

*Fourier analysis off the uniform measure.*
The best-understood non-uniform setting is the $p$-biased / product-measure analysis (@o2021analysis, Chapter 8; for learning, @furst1991improved), where the basis is rebuilt to remain orthonormal, so Parseval still holds.
A recent line pushes this to genuinely non-product distributions by _orthonormalizing the basis to the distribution_: the discrete Fourier expansion of @heidari2021finding for correlated variables, its use for learning DNF under a fixed distribution @heidari2025learning, and, in uncertainty quantification, the arbitrary polynomial chaos expansion built from the empirical moments of a raw dataset @oladyshkin2012data.
All of these _re-orthonormalize_ and thereby avoid any $C_calD$-type defect; we instead retain the fixed characters and confront the non-orthogonality directly, which is what makes the dataset spectrum a _convolution_ of the global spectrum rather than a change of basis.

*Heavy-coefficient recovery and access models.*
We adapt the Goldreich--Levin algorithm @goldreich1989hardcore and its use in the Kushilevitz--Mansour learner @kushilevitz1993learning, extended to general finite abelian groups by @akavia2003proving; the agnostic-learning refinements of @gopalan2008agnostically use exactly the bounded spectral-norm ($norm(hat(f))_1 <= A$) geometry we assume.
Modern $q$-SFT likewise searches sparse, potentially high-degree spectra directly on $ZZ_q^n$ without first encoding the alphabet as Boolean variables @erginbas2023efficiently.
These methods operate under the _uniform_ group measure with chosen function evaluations; our fixed-prefix result instead uses the non-uniform autoregressive rollout law and prefix-conditional samples, while the finite-dataset results use the empirical measure.

*Why samples alone are insufficient.*
That sample-only access cannot isolate a heavy parity is the statistical-query / noisy-parity barrier @kearns1998sq @blum1994weakly @blum2003noise.
The same phenomenon reappears in verification: PAC-verification and the "interactive Goldreich--Levin" of @goldwasser2021interactive and @gur2024power let a sample-only verifier recover heavy coefficients _only_ with the help of a query-capable prover.
Our blindness lemma is a dataset-specific version of this barrier (a birthday-horizon on collision-free projections).
Where those works restore power through an interactive prover, we restore it either through _conditional (subcube) sampling_ over the empirical dataset --- the oracle studied for distribution testing and learning @canonne2015testing @chakraborty2013power @chen2021junta, here exposing the context repetition that real corpora exhibit --- or, when the model is queryable, through the dataset's bias spectrum.

*Aliasing as subsampling.*
The convolution identity $hat(f)_calD (S) = sum_V b_V hat(f)(S Delta V)$ is, mechanically, the aliasing that subsampling induces on the Walsh--Hadamard spectrum, exploited constructively in sparse-transform algorithms @scheibler2015fast @li2015spright and in compressed-sensing recovery of sparse set functions @stobbe2012learning @amrollahi2019efficiently, including in explicitly non-orthogonal bases @wendler2021learning.
The distinction is that those methods _design_ an invertible subsampling pattern with full query access, whereas our dataset is _given_: its induced aliasing (the bias spectrum) is not ours to choose, which is precisely why samples alone are blind and why the bias spectrum must be supplied.

*Auditing and interpreting trained models via Fourier.*
Closest in motivation is the Active Fourier Auditor @ajarra2024active, which estimates robustness and (individual/group) fairness of a model from queries and samples, building on the fairness-verification line of @ghosh2021justicia @ghosh2022fvgm @ghosh2023fairness and the query-based auditing of @yan2022active.
That work also leaves the uniform measure --- but by Gram--Schmidt-orthonormalizing the parity basis to the data distribution (the expansion of @heidari2021finding) and estimating a handful of scalar functionals of the spectrum; we keep the fixed characters over the empirical measure and target the spectrum itself.
A parallel, independent thread recovers the _sparse Walsh--Hadamard spectrum of a trained model_ for explanation @gorji2025amortized @kang2025spex, and --- nearest to our machinery --- @reing2025activation searches an _in-distribution_ activation support for non-redundant heavy coefficients via an explicit "Goldreich--Levin with problem-specific constraints"; dataset Goldreich--Levin is the missing recovery primitive underneath these applications.

*Sensitivity, learnability, and testing.*
Our per-coordinate sensitivity $Sens_calD^i$ is the Fourier form of the total-effect Sobol index of variance-based global sensitivity analysis @sobol2001global, whose subtlety under _dependent_ (non-product) inputs @hooker2007generalized is the classical shadow of the $C_calD$ corrections; the same variance-based influence, estimated over a dataset, underlies feature-attribution methods such as @datta2016algorithmic.
The question of whether a model class can fit a given dataset is distribution-specific learnability: learnability under a _fixed_ measure is governed by its metric entropy @benedek1991learnability, and, spectrally, by how the target's weight aligns with the data-dependent spectrum @canatar2021spectral @abbe2023sgd --- an information-theoretic cousin being dataset difficulty relative to a model family @ethayarajh2022understanding.
Finally, our in-distribution testing (which zeroes points outside $calD$) is close in spirit to distribution-free property testing @halevy2007distribution, measuring distance over the data distribution rather than the uniform one.
