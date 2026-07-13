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
$L_k$; it is forked before drawing either child token.

== Search and feasibility gate

Each parent uses probability-weighted, Hermitian-symmetrized difference bins followed by
$248077 dot "ifft"$.  The implementation is native PyTorch: it constructs Hermitian histograms with
`scatter_add_`, evaluates parent blocks with batched `torch.fft.ifft`, and keeps only a block's top
children rather than materializing the full parent-by-alphabet frontier.  Screening and confirmation use
fresh independent outer contexts.  We test bucket
energies $0.10,0.05,0.02$, beginning with $256$ context pairs and doubling to at most $4096$.
The theorem-backed frontier retains both confidently heavy and statistically unresolved children.
It descends only while its width is at most $64$.  Exceeding that cap is reported as
`spectral_blowup` or `statistically_unresolved`; a separate top-$64$ beam may continue only as a labeled
heuristic feature search.  The empirical bank retains the top $1024$ candidates separately at each of
degrees one, two, and three, hence at most $3072$ characters before constant and conjugate deduplication.

Before a full search we measure levels one through three for raw, residual, and constant targets, benchmark
the prime-length GPU inverse FFT, and project the cost of all remaining suffix tokens and transforms.
Two fixed random token-id permutations and the exact simplex support kernel from
@lem:character-simplex-support measure sensitivity to the cyclic id geometry.

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

We compare a parameter-matched model without Fourier features, a low-degree-only adapter, raw versus
residual feature searches, and a Nyström simplex predictive control.  For every retained complex character,
the simplex model receives two real landmark coordinates
$phi_(j,a)(x)=(q 1{x_j=a}-1)/sqrt(q(q-1))$, chosen from frequent tokens evenly across the searched support.
Thus the Fourier and simplex projections have exactly the same width; the no-feature control spends those
same parameters on a terminal-state adapter.  The finite landmark set is explicitly an approximation.  If
all $q$ landmarks at one position were retained, their inner product would be exactly
$(q 1{x_j=x'_j}-1)/(q-1)$, which is invariant under every permutation of token ids.

The student minimizes the full-tokenizer teacher-to-student KL.  Cached BF16 teacher logits use float32
normalizers during training, while the final teacher distributions are recomputed in float32.  KL is the
supervised loss, not a Fourier target or theorem consequence.

== Budget and predeclared evaluation

Modal spending is capped at $25$: $23$ for aggregate GPU seconds and $2$ reserved for CPU, memory, and
storage.  The allocations are $1$ for compatibility, $4$ for the spectral gate, at most $6$ for the
remaining search, $6$ for labels, and $6$ for fitting and evaluation.  Every stage reads a persistent cost
ledger, reuses revision-matched shards, projects its cost from measured throughput, and refuses a launch
that would exceed the cap.

The target split sizes are $2000$ search, $20000$ distillation, $2000$ validation, and $5000$ test contexts.
If the label allocation cannot support $20000$ training examples, the pipeline uses the largest multiple of
$256$ that fits, with a minimum of $4096$, and records the reduction.

The primary metric is strict agreement between teacher and student argmax over all $248077$ tokenizer ids
after the generated token $128$.  Success is a point estimate of at least $90%$ on unseen FineWeb documents;
we also report a $95%$ Wilson interval, full-tokenizer KL, top-five agreement, the teacher top-two margin,
agreement stratified by margin, parameter and byte compression, latency, memory, live widths, density
controls, categorical degrees, supports, token-id permutations, and simplex support stability.

== Results

#let results-ready = true
#include "../experiments/results/qwen35_argl/paper_macros.typ"
#include "../experiments/results/qwen35_argl/paper_tables.typ"

All three data shards passed a memory-mapped integrity and shape check: $20000$ train, $2000$ validation,
and $5000$ test examples, each with a $256$-token input and all $248077$ terminal teacher logits.  The
$4096$-pair search retained $3072$ heuristic candidates and $1437$ distinct nonconstant characters after
conjugate deduplication: $689$ of degree one, $566$ of degree two, and $182$ of degree three.

At level one the residual maximum was $0.19121$ and the simultaneous Hoeffding radius over all $248077$
children was $0.09302$.  Hence the lower confidence endpoint, $0.09819$, narrowly misses the strict $0.10$
energy gate but clears the predeclared $0.05$ gate.  This certifies the top first-order bucket at the latter
threshold; it does *not* certify the complete adaptive frontier.  In particular, the degree-two and
degree-three bank remains a labeled heuristic.  The constant control reached essentially $1$, confirming
that rollout-density correlations are substantial, while residualization removes the prefix-only output
component rather than the rollout density itself.

Table @tab:qwen-final reports the untouched test split.  Fourier attained $18.22%$ strict top-token
agreement with Wilson $95%$ interval $[17.17%,19.31%]$, top-five agreement $39.14%$, and full-tokenizer KL
$3.4659$.  Against the exactly parameter-matched no-feature adapter, this is a $1.36$ percentage-point
top-one gain and a $0.0897$ KL reduction.  The centered-simplex control reached $16.38%$ and KL $3.5445$;
therefore the cyclic characters helped in this run, but the comparison does not establish that tokenizer-id
geometry is intrinsically meaningful.  The intervals overlap and no unregistered significance claim is
made.

Each student has $31472320$ parameters versus the teacher's measured $852985920$, a $27.10 times$
same-precision parameter reduction.  The twelve-layer escalation did not beat the eight-layer Fourier
checkpoint on validation KL.  The primary $90%$ target is therefore decisively *not met*; Dataset GL's
correlation-recovery theorem is not a compression or argmax-agreement theorem.  Reconciled A10 time,
including conservative full-duration charges for interrupted jobs, cost at most $4.59$, well below the
$23$ GPU and $25$ overall caps.
