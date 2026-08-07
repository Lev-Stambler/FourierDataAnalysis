#import "../../levs-commands/main_commands.typ": *
#show: thmrules
#show: eqrules
#set math.equation(numbering: none)

= Discussion: What the Theory Does and Does Not Certify <sec:discussion>

The theorems of this paper are statements about *access* and *identity*, not about any concrete model's
spectrum.  We separate what is proved from what an empirical program would still have to establish.

*What is proved.*
Given autoregressive sampling and prefix-conditional resampling from $calD_(pi,theta)^n$, the reverse-time
search lists every categorical coefficient whose magnitude exceeds $tau$, with soundness threshold $tau/2$
and conditional-sample complexity $O(n N log(n q N/delta)/tau^4)$, for arbitrary categorical degree up to
$n$ (@thm:ar-qary-gl).  The random-context vector version recovers every coefficient whose RMS energy over
real contexts exceeds the same threshold (@thm:random-context-vector-gl).  The identities are exact for
non-uniform, non-product rollout laws, and the estimator is unbiased with explicit concentration.

*What is instance-dependent.*
The live spectral width $N$ is a property of the pair (rollout law, target function).  Generation solves
conditional access, not spectral density: if exponentially many buckets are genuinely heavy, every correct
all-heavy-coefficient algorithm pays for them.  Measuring the live-width profile of a real teacher is
itself an experiment, and the theory gives no bound on it.

*What a compression claim would additionally require.*
Recovered characters are correlations under a non-uniform law, not orthogonal reconstruction weights: the
point-mass example shows that rollout density alone can create high-support moments.  Turning a character
list into a student requires supervised refitting of vector weights against the teacher's raw next-token
function $f(X)=P_theta(dot|X)$ under a probabilistic loss, and neither low KL nor heavy spectral energy
alone implies top-token agreement without a teacher-margin condition.  Compression, generalization to
unseen generated $X$, and any fixed top-token agreement level are therefore empirical claims that this
paper does not make.

*What an architecture claim would require.*
The correlation-memory representation results of @sec:fourier-correlation-memory state which coefficients a
shared-memory stack computes, not that any teacher's spectrum is narrow in that basis.  The comparison
between a position-specific character list and a small number of shared decay modes is a concrete open
experiment; nothing in the paper assumes its outcome.  Early architecture experiments exploring related
structured-mixer candidates proved inconclusive and are not relied on here.

*Preregistration discipline.*
When these experiments are run, the dated protocol should be fixed before data collection: the teacher and
its revision, the corpus and context sampling rule, the energy threshold, and the promotion gates.
The theory of @sec:oracle-separation shows in particular that any claimed shortcut past the reverse-time
tree must first exhibit its oracle reduction; a benchmark of chosen affine points is evidence about a
different oracle, not about prefix-conditional sampling.
