#import "../levs-commands/main_commands.typ": *
#import "../custom_commands.typ": *
#show: thmrules
#show: eqrules
#set math.equation(numbering: none)

= Qwen3.5 Compression Experiment <sec:qwen-experiment>

This section pre-registers the experiment that tests whether the correlations recovered by
@thm:random-context-vector-gl are useful for compression.  The theorem guarantees correlation discovery
under its conditional oracle; every claim about prediction, KL, generalization, or compression below is an
empirical claim.

== Tokenizer-native domain

The teacher is `Qwen/Qwen3.5-0.8B-Base`.  Its tokenizer has $q=248077$ valid, contiguous ids
$0,dots,248076$: $248044$ base tokens and $33$ added tokens, including eight special tokens.  Its neural
output is padded to $248320$ logits.  The categorical input alphabet and learned output distribution both
use exactly the $248077$ tokenizer ids; the $243$ padded non-token rows are sliced away before softmax.
All valid special tokens remain in the alphabet.

For raw logits $ell(c) in RR^248320$, the tokenizer-level distribution is
$
P_theta^"tok"(t|c)
= frac(exp(ell_t(c)),sum_(u=0)^(248076) exp(ell_u(c))),
quad 0 <= t < 248077.
$
There is no top-$k$ slot vocabulary, `OTHER` class, binary token encoding, or padded output class.

== Real contexts, conditional rollouts, and labels

We stream the `CC-MAIN-2024-10` configuration of FineWeb and split complete documents by a stable content
hash.  From every retained document we choose one deterministic contiguous $128$-token span
$Z ~ mu_"FW"$.  Conditioned on $Z$, the teacher samples exactly $128$ additional tokens
$X ~ calD_Z^128$ from $P_theta^"tok"$ at temperature one, with no top-$k$ or nucleus truncation.
EOS is treated as an ordinary token so that every categorical datapoint has length $128$.

Write
$
g(Z,X)=arg max_t P_theta^"tok"(t|Z,X),
quad g_0(Z)=arg max_t P_theta^"tok"(t|Z).
$
The primary vector target in the scaling experiment is the teacher's hard decision
$Y(Z,X)=e_(g(Z,X))$.  A separately labeled diagnostic is its prefix residual
$
F_"res"^"hard"(Z,X)=frac(e_(g(Z,X))-e_(g_0(Z)),sqrt(2)).
$
Both have pointwise Euclidean norm at most one.  More importantly, the paired Dataset-GL label operation
does not materialize a $248077$-vector.  For paired continuations $X,X'$ sharing the real prefix $Z$, write
$g=g(Z,X)$, $g'=g(Z,X')$, and $a=g_0(Z)$.  Then
$
chevron.l Y,Y' chevron.r=ind[g=g'],
$
$
chevron.l F_"res"^"hard"(Z,X),F_"res"^"hard"(Z,X') chevron.r
=frac(ind[g=g']-ind[g=a]-ind[a=g']+1,2).
$
Thus the vector theorem searches all output coordinates jointly with constant-time integer comparisons per pair.
The full probability target $P_theta^"tok"(dot|Z,X)$, its probability residual, and terminal KL remain
secondary diagnostics and distillation objectives; they are not needed to define the primary GL gate.
A constant unit-vector target still measures correlations caused only by the rollout density.
At a GL level, the cache shared by a pair contains the real $Z$ followed by the generated left prefix
$L_k$.  The executed code repeats that identical token context and recomputes its deterministic activations
for the two draws; this is distributionally equivalent to an implementation-safe KV-cache fork, but is not
literal cache-object cloning.

== Search and feasibility gate

Each parent uses probability-weighted, Hermitian-symmetrized difference bins followed by
$248077 dot "ifft"$.  The implementation is native PyTorch: it constructs Hermitian histograms with
`scatter_add_`, evaluates parent blocks with batched `torch.fft.ifft`, and keeps only a block's top
children rather than materializing the full parent-by-alphabet frontier.  The active escalation also
computes all child variances with one additional length-$q$ inverse DFT, as specified below.  Each depth resamples new
continuations from the same fixed outer contexts; the executed run has no separate confirmation set.
The $0.10$ bucket-energy diagnostic uses $4096$ context pairs and reports a simultaneous interval over the
$248077$ children of each parent.  The executable path is a capped top-$1024$ beam and is never labeled as
the theorem-backed heavy-plus-unresolved frontier.  It retains candidates separately at suffix depths one,
two, and three, hence at most $3072$ characters before constant and conjugate deduplication.  These depths
permit categorical degrees at most three and do not constitute a degree-$128$ search.

At level one the completed run measured raw, residual, and constant targets and benchmarked the
prime-length GPU inverse FFT; deeper heuristic levels used the residual target.  The active method remains
strictly in the tokenizer-native $ZZ_q^128$ character basis.

== Student and loss

The terminal student consumes all $256$ tokens in the concatenation $(Z,X)$.  It uses a shared
$248077 times 64$ factorized input/output vocabulary matrix, width $384$, six heads, feed-forward width
$1536$, and initially eight encoder layers.  Real and imaginary parts of at most $4096$ selected characters
are gated into the terminal representation.  A predeclared twelve-layer variant is trained only if the
eight-layer validation agreement is below $90%$; every artifact must contain at most $50$ million parameters.
Training and evaluation use BF16 matrix kernels with float32 KL reductions, batch size $64$, and a
static-shape `torch.compile` graph in `max-autotune-no-cudagraphs` mode.  The latter retains Inductor/Triton
autotuning while avoiding CUDA-graph capture, which is incompatible with the flash-attention path on the
tested PyTorch 2.13/A10 stack.

The completed first-run student minimizes the full-tokenizer teacher-to-student KL.  Cached BF16 teacher
logits are converted to float32 and normalized there during training and evaluation; there is no fresh
float32 teacher forward.  KL is a supervised loss, not a Fourier target or theorem consequence.  The active
scaling experiment instead makes the hard teacher decision the primary spectral and agreement target while
retaining soft KL as a secondary training signal.

== Budget and predeclared evaluation

Modal spending is capped at $25$: $23$ for aggregate GPU seconds and $2$ reserved for CPU, memory, and
storage.  The allocations are $1$ for compatibility, $4$ for the spectral gate, at most $6$ for the
remaining search, $6$ for labels, and $6$ for fitting and evaluation.  Every stage reads a persistent cost
ledger, reuses shape-validated shards, projects its cost from measured throughput, and refuses a launch
that would exceed the cap.  The executed artifacts did not pin or record the exact Hugging Face revision,
so shape validation is not a revision-integrity guarantee.

The executed split sizes are $4096$ search, $20000$ distillation, $2000$ validation, and $5000$ test contexts.
If the label allocation cannot support $20000$ training examples, the pipeline uses the largest multiple of
$256$ that fits, with a minimum of $4096$, and records the reduction.

The primary metric is strict agreement between teacher and student argmax over all $248077$ tokenizer ids
after the generated token $128$.  Success is a point estimate of at least $90%$ on unseen FineWeb documents;
the completed artifact reports a $95%$ Wilson interval, full-tokenizer KL, top-five agreement, the mean
teacher top-two margin, parameter compression, the measured three-depth widths, density diagnostics, and
categorical degrees.  Margin strata, latency, peak memory, and token-id permutations remain unreported
rather than being silently treated as successful controls.

== Results

#let results-ready = true
#include "../experiments/results/qwen35_argl/paper_macros.typ"
#include "../experiments/results/qwen35_argl/paper_tables.typ"

All three data shards passed a memory-mapped integrity and shape check: $20000$ train, $2000$ validation,
and $5000$ test examples, each with a $256$-token input and all $248077$ terminal teacher logits.  The
$4096$-pair search retained $3072$ heuristic candidates and $1437$ distinct nonconstant characters after
conjugate deduplication: $689$ of degree one, $566$ of degree two, and $182$ of degree three.

At level one the residual maximum was $0.19121$ and the simultaneous Hoeffding radius over all $248077$
children was $0.09302$.  Its $0.09819$ lower endpoint misses the $0.10$ energy gate and clears $0.05$, but
the maximizing child is the zero-frequency/DC bucket $Psi_1(0)$, which the student later drops.  It is *not*
a certificate for a nonconstant first-order feature.  All residual children were unresolved at $0.10$, and
the saved summary does not identify a certified nonconstant child at $0.05$.  Every reported best value at
depths one through three is likewise on the all-zero path and is excluded before fitting; the remaining
degree-one through degree-three bank is entirely heuristic.  For the constant target the DC value $1$ is
tautological, but $239496$ non-DC children also had lower endpoints at least $0.10$, directly demonstrating
a broad rollout-density spectrum.  These 99% simultaneous intervals cover conditional-rollout randomness
for the fixed $4096$ search contexts, not population FineWeb sampling by themselves.
Consequently the actual theorem frontier at energy $0.10$ has all $248077$ children in its
heavy-plus-unresolved set, exceeds the width-$64$ cap at level one, and terminates as `spectral_blowup`.
Only the separately labeled heuristic beam descends to depths two and three.

Table @tab:qwen-final reports the untouched test split.  Fourier attained $18.22%$ strict top-token
agreement with Wilson $95%$ interval $[17.17%,19.31%]$, top-five agreement $39.14%$, and full-tokenizer KL
$3.4659$.  Against the exactly parameter-matched no-feature adapter, this is a $1.36$ percentage-point
top-one gain and a $0.0897$ KL reduction.

For historical context only, the old supervised additive- and tensor-simplex controls reached respectively
$16.38%$ and $18.70%$ top-one, while the tensor control reached $37.84%$ top-five and KL $3.5546$.  Their
intervals overlapped the Fourier result.  These finite controls never ran Dataset GL and are neither an
alternative basis nor a forward step in the active experiment.

Each student has $31472320$ parameters versus all $852985920$ parameters loaded from the multimodal teacher
checkpoint, a $27.10 times$ same-precision full-checkpoint parameter reduction.  This denominator includes
the vision tower unused by the text-only task and is not an active-text-parameter ratio.  The twelve-layer escalation did not beat the eight-layer Fourier
checkpoint on validation KL.  The primary $90%$ target is therefore decisively *not met*; Dataset GL's
correlation-recovery theorem is not a compression or argmax-agreement theorem.  Reconciled A10 time,
including conservative full-duration charges for interrupted jobs and the tensor-control rerun, cost at
most $4.81$, well below the $23$ GPU cap.  CPU, storage, and memory charges were not independently
reconciled in the committed artifacts, so the $25$ overall cap is not claimed as an audited total.

== Active scaling escalation <sec:qwen-scaling>

#block(stroke: 0.6pt + gray, inset: 8pt)[
  *Status.*  Everything in this subsection is an active, predeclared diagnostic or escalation.  It is not
  a result.  Modal measurements replace these statements only through checked-in artifacts.  The method is
  exclusively the tokenizer-native $ZZ_q^128$ Fourier search; there is no binary compression and no simplex
  Dataset-GL variant.
]

=== Two-DFT empirical-Bernstein gate

For one parent expansion, let pair $i$ produce the complex bounded parent weight $B_i$, token difference
$D_i in ZZ_q$, and child observation
$
U_(i,c)=Re(B_i omega_q^(c D_i)) in [-1,1].
$
The first probability-weighted histogram
$
h_1(d)=frac(1,M) sum_(i=1)^M B_i ind[D_i=d]
$
and one length-$q$ inverse DFT give all means
$hat(mu)_c=M^(-1)sum_i U_(i,c)$, exactly as in @lem:qary-all-children.  A second histogram
$
h_2(d)=frac(1,M) sum_(i=1)^M B_i^2 ind[D_i=d]
$
gives every second moment in one more inverse DFT, because
$
U_(i,c)^2
=frac(|B_i|^2+Re(B_i^2 omega_q^(2 c D_i)),2).
$
Thus
$
overline(U_c^2)
=frac(M^(-1)sum_i |B_i|^2+Re(sum_d h_2(d) omega_q^(2 c d)),2),
quad
s_c^2=frac(M,M-1)(overline(U_c^2)-hat(mu)_c^2).
$
For $Q$ simultaneous comparisons and failure budget $delta$, the implemented two-sided radius is
$
r_c
=sqrt(frac(2 s_c^2 log(4 Q/delta),M))
+frac(14 log(4 Q/delta),3(M-1)).
$
It is the Maurer--Pontil empirical-Bernstein inequality @maurer2009empirical, rescaled from $[0,1]$ to
$[-1,1]$ and union-bounded over the declared comparisons.  It makes no Gaussian assumption.  Low-variance
children can therefore be resolved much sooner than under the common worst-case Hoeffding radius, while
the theorem's failure accounting remains valid.  Independence counts outer-context pairs; several suffix
pairs from one cached context remain one dependent cluster.

Both histograms use `scatter_add_`; the two prime-length transforms are batched `torch.fft.ifft` calls.
The scaling implementation is designed to process parents and up to roughly $110000$ retained characters in fixed-size
chunks with compiled PyTorch kernels, so neither a parent-by-$q$ matrix nor a dense
character-by-sequence tensor is allocated.  The all-$q$ gate, ranking, chunked feature evaluation, and
student forward are specified as GPU operations; CPU work is limited to artifact orchestration and
integrity checks.

=== Why $100000$ terms is plausible, and why it is not yet proved

The proposed energy model is testable.  If scalar coefficients were i.i.d. circular complex Gaussians
$A_c ~ calN_CC(0,sigma^2)$, then their energies satisfy
$|A_c|^2/sigma^2 ~ "Exp"(1)$.  As $q$ grows, the largest fraction $rho=K/q$ would contain the fraction
$
G(rho)=rho(1-log rho)
$
of total scalar energy: the cutoff is $-sigma^2 log rho$ and
$EE[E ind(E >= -sigma^2 log rho)]/EE[E]=rho(1-log rho)$ for $E ~ "Exp"(sigma^2)$.
For $q=248077$, this predicts
$
G(100000/q) approx 0.769,
quad
G(109000/q) approx 0.801.
$
This calculation supports the $100000$--$110000$ scale; it does *not* say that $100000$ terms capture all
energy, and it is not a Dataset-GL certificate.  Moreover, if a coefficient has many independent Gaussian
output coordinates, its squared vector norm is gamma rather than exponential and concentrates more tightly,
making vector energies less sparse.  The teacher's argmax one-hot coefficients are also correlated across
characters, so the empirical profile, rather than a Gaussian fit, decides the expansion.

The energy diagnostic therefore uses a fixed, hash-recorded split of fresh GL pairs.  The discovery half
ranks all $q$ children.  Without reranking, the confirmation half measures their energies, rank stability,
and cumulative confirmed share on a logarithmic $K$ grid through $100000$ and $110000$.  It reports both
the primary one-hot vector profile and, only as a secondary diagnostic, the full-probability vector profile,
along with empirical-Bernstein intervals.
This split prevents selection on the same noise used to claim a top-$K$ curve.  A Gaussian quantile plot and
$G(K/q)$ are descriptive overlays only; the simultaneous bounds and the Dataset-GL transcript carry the
statistical claim.

=== The old gate was not the theorem search

It is important not to read the completed $4096$-pair screen as a measurement of true spectral density.
Writing $T=tau^2$ for the theorem's target energy, @thm:ar-qary-gl requires every expanded child's estimate
to have additive error at most
$T/8$, and retains at the theorem cutoff $3T/8$.  The old screen used $T=0.10$ but had common Hoeffding
radius $0.09302$, rather than the required $0.0125$.  Its statement that all $248077$ residual children
were unresolved says that this sample size could not classify the frontier.  The subsequent top-$1024$ cap
then says only that the heuristic bank was narrow; neither observation proves a dense residual spectrum.

By contrast, the constant-target observation is informative within its stated scope: $239496$ non-DC
children had simultaneous lower endpoints at least $0.10$ on the fixed search contexts.  That is real
breadth of the rollout-density spectrum, although it is not by itself a population-FineWeb statement.

The escalation records two different outputs.  The *Dataset-GL transcript* expands every theorem-live child
under a scheduled failure budget and never applies a top-$K$ cap; if its certified live width exceeds the
resource ceiling, it terminates with that width as an honest infeasibility result.  Separately, the
*engineering bank* may retain the best $K$ discovery-ranked, confirmation-measured characters for fitting.
Only the first output receives the completeness guarantee; the second is always labeled a capped heuristic.

=== The compressed model is the Fourier representation

Qwen is kept fixed and is used only as the teacher, label oracle, and conditional sampler.  No teacher
block is deleted, copied into the student, or used as a residual backbone.  For a recovered character bank
$S subset.eq ZZ_q^128$ of size $K$, the student logits are
$
ell_S(x)
=b+sum_(alpha in S)
  (c_alpha cos(2 pi chevron.l alpha,x chevron.r/q)
   +d_alpha sin(2 pi chevron.l alpha,x chevron.r/q)),
quad
Q_S(dot|x)="softmax"(ell_S(x)).
$
Here $c_alpha,d_alpha in RR^q$ are genuinely vector-valued Fourier output coefficients.  A dense
$2K times q$ coefficient table would not be a compression, so the implementation learns a joint rank-$r$
factorization
$
c_alpha=A_(alpha,:) U,
quad d_alpha=B_(alpha,:) U,
quad
A,B in RR^(K times r),
quad U in RR^(r times q).
$
This restricts the recovered coefficient vectors to a learned output subspace but does not change the input
characters or encode tokens into bits.  Its parameter count, including the output bias, is
$2 K r+r q+q$.  At $K=109000$, $r=64$, and $q=248077$, this is $30077005$ parameters.  The rank is selected on
validation agreement; the full-$q$ softmax and argmax are always evaluated.

The packed GPU evaluator stores only each character's nonzero token positions and $ZZ_q$ frequencies.
For fixed chunks of $8192$ terms it gathers the required tokenizer ids, performs one integer dot product
modulo $q$, evaluates sine and cosine, and accumulates directly into the rank-$r$ state.  The compiled
PyTorch path therefore never allocates a batch-by-$K$-by-$128$ tensor or a $2K times q$ table.

Training uses terminal hard teacher argmax as the primary label and terminal full-distribution KL as a
secondary objective.  Each teacher rollout also supplies up to $128$ ordinary next-token samples: the
last $128$ tokens before a sampled token form an additional tokenizer-native input window, and the sampled
token is an unbiased hard-label draw from the teacher distribution at that window.  These extra causal
rows increase fitting data but do not count as independent GL pairs or alter the terminal evaluation law.
The first-level $109000$ run characterizes the energy scale only; a final high-order bank must retain and
confirm complete $128$-coordinate character indices, and its capped engineering status remains distinct
from the uncapped Dataset-GL transcript above.

=== What would constitute a provable $80%$ agreement result

There are two useful sufficient loss bounds for the hard target.  If $Q(dot|C)$ is the student's probability
vector and $g$ the teacher argmax, then a top-token disagreement forces $Q_g <= 1/2$ and hence
$
ind[arg max Q != g]
<= frac(-log Q_g,log 2).
$
It also forces the multiclass Brier loss to be at least $1/2$, so
$
ind[arg max Q != g] <= 2 norm(Q-e_g)_2^2.
$
Thus population hard-label cross-entropy below $0.2 log 2$ or population Brier loss below $0.1$ is
sufficient for at least $80%$ agreement.  These are conservative sufficient conditions, not claims about
the current model, and empirical training loss alone does not prove either population inequality.

For the secondary soft target there is also an exact per-example margin certificate.  Let $p_1 >= p_2$ be
the teacher's largest two probabilities and set
$
kappa(P)
=p_1 log frac(2p_1,p_1+p_2)
+p_2 log frac(2p_2,p_1+p_2).
$
If $"KL"(P || Q)<kappa(P)$, the student and teacher argmax must agree: $kappa(P)$ is the minimum forward KL
needed to move the runner-up to a tie with the teacher winner.  The artifact records $(p_1,p_2,kappa)$, KL,
and direct agreement for every evaluation row.

The final claim uses a fresh, document-hash-disjoint lockbox that is never inspected during search,
ranking, capacity selection, training, or early stopping.  For $s$ direct agreements among $n$ independent
lockbox documents, the one-sided exact binomial lower confidence bound is
$
L_"CP"="Beta"^(-1)(delta_"eval";s,n-s+1).
$
Success requires $L_"CP" >= 0.80$, not merely an $80%$ point estimate.  For the predeclared
$n=5000$ and $delta_"eval"=0.025$, this requires at least $4056$ agreements ($81.12%$ observed).  The final
failure statement is union-bounded with the separate Dataset-GL transcript budget, for example
$delta_"GL"+delta_"eval"=0.05$.  Dataset-GL completeness certifies recovery of heavy correlations; the
untouched exact-binomial lockbox certifies agreement of the resulting frozen predictor.  Neither is
silently substituted for the other.
