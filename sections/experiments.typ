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

The terminal vector label is
$F_"raw"(Z,X)=P_theta^"tok"(dot|Z,X)$.
The primary spectral target removes the prefix-only distribution,
$
F_"res"(Z,X)
= frac(P_theta^"tok"(dot|Z,X)-P_theta^"tok"(dot|Z),sqrt(2)),
$
and a constant unit-vector target measures correlations caused only by the rollout density.
At a GL level, the cache shared by a pair contains the real $Z$ followed by the generated left prefix
$L_k$.  The executed code repeats that identical token context and recomputes its deterministic activations
for the two draws; this is distributionally equivalent to an implementation-safe KV-cache fork, but is not
literal cache-object cloning.

== Search and feasibility gate

Each parent uses probability-weighted, Hermitian-symmetrized difference bins followed by
$248077 dot "ifft"$.  The implementation is native PyTorch: it constructs Hermitian histograms with
`scatter_add_`, evaluates parent blocks with batched `torch.fft.ifft`, and keeps only a block's top
children rather than materializing the full parent-by-alphabet frontier.  Each depth resamples new
continuations from the same fixed outer contexts; the executed run has no separate confirmation set.
The $0.10$ bucket-energy diagnostic uses $4096$ context pairs and reports a simultaneous interval over the
$248077$ children of each parent.  The executable path is a capped top-$1024$ beam and is never labeled as
the theorem-backed heavy-plus-unresolved frontier.  It retains candidates separately at suffix depths one,
two, and three, hence at most $3072$ characters before constant and conjugate deduplication.  These depths
permit categorical degrees at most three and do not constitute a degree-$128$ search.

At level one we measure raw, residual, and constant targets and benchmark the prime-length GPU inverse FFT;
deeper heuristic levels use the residual target.  Fixed random token-id permutations and the complete exact
simplex support-kernel stability check were predeclared but not executed, and no robustness claim based on
those controls is made below.

== Student, baselines, and loss

The terminal student consumes all $256$ tokens in the concatenation $(Z,X)$.  It uses a shared
$248077 times 64$ factorized input/output vocabulary matrix, width $384$, six heads, feed-forward width
$1536$, and initially eight encoder layers.  Real and imaginary parts of at most $4096$ selected characters
are gated into the terminal representation.  A predeclared twelve-layer variant is trained only if the
eight-layer validation agreement is below $90%$; every artifact must contain at most $50$ million parameters.
Training and evaluation use BF16 matrix kernels with float32 KL reductions, batch size $64$, and a
static-shape `torch.compile` graph in `max-autotune-no-cudagraphs` mode.  The latter retains Inductor/Triton
autotuning while avoiding CUDA-graph capture, which is incompatible with the flash-attention path on the
tested PyTorch 2.13/A10 stack.

The completed fit compares a parameter-matched model without Fourier features, an additive simplex
landmark ablation, and a support-matched tensor-simplex Nyström control.  For every retained complex
character, the additive ablation receives two real one-position
coordinates
$phi_(j,a)(x)=(q 1{x_j=a}-1)/sqrt(q(q-1))$, chosen from frequent tokens evenly across the searched support.
Thus the Fourier and simplex projections have exactly the same width; the no-feature control spends those
same parameters on a terminal-state adapter.  The finite landmark set is explicitly an approximation.  If
all $q$ landmarks at one position were retained, their inner product would be exactly
$(q 1{x_j=x'_j}-1)/(q-1)$, which is invariant under every permutation of token ids.
Because these features are additive across positions, this first run is not the support-matched tensor
simplex control of @lem:character-simplex-support and cannot by itself isolate cyclic geometry from
interaction-order capacity.

The tensor control instead attaches two categorical landmark tuples $a_S$ to every recovered support $S$
and evaluates exact kernel columns
$K_S(x,a_S)=product_(j in S)(q 1{x_j=a_j}-1)/(q-1)$.
It therefore matches the Fourier support multiset and projection width while replacing cyclic phases with
permutation-symmetric equality contrasts.  Its $2874$ columns are still a finite Nyström approximation, not
the exponentially large complete frame.

The student minimizes the full-tokenizer teacher-to-student KL.  Cached BF16 teacher logits are converted to
float32 and normalized there during training and evaluation; there is no fresh float32 teacher forward.  KL
is the supervised loss, not a Fourier target or theorem consequence.

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
categorical degrees.  Margin strata, latency, peak memory, token-id permutations, and exact simplex-support
stability remain unreported rather than being silently treated as successful controls.

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
top-one gain and a $0.0897$ KL reduction.  The additive simplex control reached $16.38%$ and KL $3.5445$;
this is the additive landmark ablation above, not a tensor-support basis match.  The cyclic features helped
relative to the additive and no-feature controls.  The subsequently run support-matched tensor-simplex
control reached $18.70%$ top-one agreement (Wilson $95%$ interval $[17.64%,19.80%]$), top-five $37.84%$,
and KL $3.5546$.  It slightly exceeds Fourier on top-one while Fourier is better by $0.0887$ KL and $1.30$
top-five percentage points.  The intervals overlap; in particular, this control removes the apparent
top-one advantage for cyclic token-id geometry, and no unregistered significance claim is made.

Each student has $31472320$ parameters versus all $852985920$ parameters loaded from the multimodal teacher
checkpoint, a $27.10 times$ same-precision full-checkpoint parameter reduction.  This denominator includes
the vision tower unused by the text-only task and is not an active-text-parameter ratio.  The twelve-layer escalation did not beat the eight-layer Fourier
checkpoint on validation KL.  The primary $90%$ target is therefore decisively *not met*; Dataset GL's
correlation-recovery theorem is not a compression or argmax-agreement theorem.  Reconciled A10 time,
including conservative full-duration charges for interrupted jobs and the tensor-control rerun, cost at
most $4.81$, well below the $23$ GPU cap.  CPU, storage, and memory charges were not independently
reconciled in the committed artifacts, so the $25$ overall cap is not claimed as an audited total.
