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

The teacher is `Qwen/Qwen3.5-0.8B-Base` pinned at revision
`5c8a1b97ddef11f79b47ab9d07bf82b9117413f6`.  Its tokenizer has $q=248077$ valid, contiguous ids
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

We stream the `CC-MAIN-2024-10` configuration of FineWeb at revision
`9bb295ddab0e05d785b879661af7260fed5140fc` and split complete documents by a stable content
hash.  From every retained document we choose one deterministic contiguous $128$-token span
$Z ~ mu_"FW"$.  Conditioned on $Z$, the teacher samples exactly $128$ additional tokens
$X ~ calD_Z^128$ from $P_theta^"tok"$ at temperature one, with no top-$k$ or nucleus truncation.
EOS is treated as an ordinary token so that every categorical datapoint has length $128$.

The real span $Z$ is used only inside this conditional generator.  After generation, its token ids and KV
state are removed.  Frozen Qwen is evaluated afresh on the generated string $X$ *alone*, with positions
$1,dots,128$ and a fresh cache, to define the single function
$
f_theta(X)=P_theta^"tok"(dot|X) in Delta(ZZ_q),
quad
g(X)=arg max_t f_theta(X)_t,
quad
Y(X)=e_(g(X)).
$
Thus the learned input is exactly $X in ZZ_q^128$, the context lens is exactly $128$, and neither the
teacher label nor the student receives $Z$.  The primary hard vector target has unit norm and its paired
Dataset-GL operation is the constant-time equality test
$
chevron.l Y(X),Y(X') chevron.r=ind[g(X)=g(X')].
$
The full vector $f_theta(X)$ and $"KL"(f_theta(X) || Q(X))$ remain secondary diagnostics and distillation
objectives; no prefix residual is defined.

At GL level $k$, the conditional sampler may retain the latent generation state consisting of $Z$ and the
generated left prefix $L_k$.  It forks that state only to draw two conditional generated suffixes and form
$X=(L_k,R)$ and $X'=(L_k,R')$.  Each label is nevertheless a new Qwen evaluation on the resulting
$128$-token $X$ or $X'$ alone.  In @thm:random-context-vector-gl this is the specialization
$F(z,x)=f_theta(x)$: $Z$ indexes the in-distribution conditional sampler but is not an argument of the
learned function, a Fourier feature, or the student.

The induced evaluation law is the marginal mixture
$calD_X(x)=EE_(Z ~ mu_"FW")[calD_Z^128(x)]$.  Its ordinary single-function coefficient is
$
hat(f_theta)_calD_X(alpha)
=EE_Z EE_(X ~ calD_Z^128)[f_theta(X) overline(chi_alpha(X))].
$
The random-context theorem estimates the stronger conditional-RMS quantity
$
calE_f(alpha)
=EE_Z[norm(EE_(X ~ calD_Z^128)[f_theta(X) overline(chi_alpha(X))])_2^2].
$
Jensen gives $norm(hat(f_theta)_calD_X(alpha))_2^2 <= calE_f(alpha)$, so its complete list contains every
heavy coefficient of the marginal single-function law, possibly with additional context-varying
coefficients.  Replacing $f_theta$ by $Y$ gives the identical statement for the primary hard target.  All
retained Fourier weights are then refit using $X$ alone under $calD_X$.

== Search and feasibility gate

Each parent uses probability-weighted, Hermitian-symmetrized difference bins followed by
$248077 dot "ifft"$.  The strict implementation is native PyTorch: it constructs the two histograms below
with `scatter_add_` and evaluates all $q$ children with batched `torch.fft.ifft`.  Parent batching and fixed
tensor chunks limit GPU memory only; chunk boundaries never rank, cap, or discard a child.  A child survives
exactly when it passes the threshold rule in @thm:ar-qary-gl under the scheduled simultaneous confidence
event.  Every surviving child is expanded at the next newest coordinate, through all $128$ coordinates.
If the complete theorem-live list exceeds the declared memory, time, or parameter ceiling, the run stops
and reports that certified width as an infeasibility result.  It does not substitute a beam, a top-$K$
list, or any other block-pruned search.

The corrected root diagnostics measure the hard argmax-one-hot and full-probability targets of $f_theta(X)$
and benchmark the tokenizer-length GPU transform.  Fresh rollout pairs are split before ranking and
confirmation.  These diagnostics may plot top-$K$ energy curves, but the curves never determine which
children the strict Dataset-GL search expands.  The input basis throughout is exactly the tokenizer-native
$ZZ_q^128$ character basis.

== Standalone Fourier student and loss

The student receives only $X in ZZ_q^128$ and is the pure tokenizer-native Fourier representation specified
in @sec:qwen-scaling.  Hard teacher decisions $g(X)$ are its primary labels.  Soft distillation, when used,
comes only from a fresh float32 normalization of Qwen logits produced by the separate forward on $X$ alone.
Training and evaluation use BF16 matrix kernels with float32 loss reductions and a static-shape
`torch.compile` graph.  Every counted artifact contains at most $50$ million parameters.

== Budget and predeclared evaluation

Modal spending is capped at $25$: $23$ for aggregate GPU seconds and $2$ reserved for CPU, memory, and
storage.  The allocations are $1$ for compatibility, $4$ for the spectral gate, at most $6$ for the
remaining search, $6$ for labels, and $6$ for fitting and evaluation.  Every stage reads a persistent cost
ledger, projects its cost from measured throughput, and refuses a launch that would exceed the cap.  A
cached shard is reusable only if its manifest records the exact frozen-model revision, the $X$-only input
law, length $128$, and matching tensor shapes; every old label shard fails this check.

The corrected split sizes are $4096$ search, $20000$ distillation, $2000$ validation, and $5000$ fresh
lockbox FineWeb documents, each inducing one generated $X$.  The document hashes are disjoint.  Old
search, training, and validation document identities may seed newly generated pinned-revision $X$-only rows,
but their old generated tokens and logits are not reused because their model revision was not recorded.
Previously viewed test documents become development data and cannot enter the fresh lockbox.  All old label,
feature, and student artifacts are invalidated.
If the label allocation cannot support $20000$ training examples, the pipeline uses the largest multiple of
$256$ that fits, with a minimum of $4096$, and records the reduction.

The primary metric is strict agreement between teacher and student argmax over all $248077$ tokenizer ids
after Qwen processes token $128$ of $X$ alone.  The completed artifact reports the one-sided exact-binomial
lockbox bound specified below, full-tokenizer KL, top-five agreement, teacher top-two margins, parameter
count, the complete $128$-level Dataset-GL transcript, and categorical degrees.

== Invalidated wrong-target artifacts

Every numerical spectral, energy-profile, and student result produced before this correction is discarded.
Those artifacts evaluated Qwen on $(Z,X)$, cached $256$-token inputs, or trained from labels belonging to
that different function.  They estimate neither $f_theta(X)=P_theta^"tok"(dot|X)$ nor agreement after the
$128$-token input $X$ alone.  No coefficient, checkpoint, accuracy, KL, confidence interval, or cost-derived
model-selection decision from those artifacts is an active result in this section.  Corrected Modal runs
start from regenerated $X$-only labels and a fresh lockbox.

== Active scaling escalation <sec:qwen-scaling>

*Status.*  Everything explicitly described as planned below is an active, predeclared diagnostic or
escalation rather than a result.  Modal measurements replace planned statements only through checked-in
artifacts.  The method is exclusively the tokenizer-native $ZZ_q^128$ Fourier search: it uses no binary
encoding, alternate categorical basis, beam, or block-pruning rule.

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
The scaling implementation processes the complete certified list in fixed-size chunks with compiled
PyTorch kernels, so neither a parent-by-$q$ matrix nor a dense
character-by-sequence tensor is allocated.  The all-$q$ gate, ranking, chunked feature evaluation, and
student forward are specified as GPU operations; CPU work is limited to artifact orchestration and
integrity checks.

=== Gaussian top-$K$ hypothesis

The following scalar *leaf-coefficient* model is a descriptive reference, not a model implied by Dataset
GL.  If scalar leaf coefficients were i.i.d. circular complex Gaussians
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
This calculation motivates diagnostic grid points near $100000$--$110000$; it does *not* say that those
terms capture the corresponding share for the experiment and is not a Dataset-GL certificate.  In
particular, a root child score is the vector conditional-bucket energy
$
Psi_1^Y(c)
=EE_(Z,L_1)[norm(EE[Y(X) overline(psi_c(X_128))|Z,L_1])_2^2],
$
not one scalar leaf energy $|A_c|^2$.  It aggregates output coordinates and unresolved earlier-coordinate
structure; only at depth $128$ does @thm:random-context-vector-gl identify a bucket with a vector
coefficient-section energy.  Even for independent Gaussian output coordinates, a squared vector norm is
gamma rather than exponential and concentrates more tightly, making vector energies less sparse.  The
teacher's argmax one-hot coefficients are also correlated across characters, so the empirical profile,
rather than a Gaussian fit, decides the diagnostic; the strict expansion remains threshold-only.

The energy diagnostic therefore uses a fixed, hash-recorded split of fresh GL pairs.  The discovery half
ranks all $q$ children.  Without reranking, the confirmation half measures their energies, rank stability,
and cumulative confirmed share on a logarithmic $K$ grid through $100000$ and $110000$.  It reports both
the primary one-hot vector profile and, only as a secondary diagnostic, the full-probability vector profile,
along with empirical-Bernstein intervals.
This split prevents selection on the same noise used to claim a top-$K$ curve.  A Gaussian quantile plot and
$G(K/q)$ are descriptive overlays only; the simultaneous bounds and the Dataset-GL transcript carry the
statistical claim.

=== Corrected pinned $X$-only root result

The first valid artifact uses the pinned revisions above, target law $X mapsto Y(X)$, $q=248077$, and
$4096$ independent outer pairs.  Its hard-target child-score quantiles
$("min",25%,50%,75%,"max")$ are
$
(0.19534692, 0.20576279, 0.21590899, 0.23120807, 0.34545898),
$
and the corresponding empirical-Bernstein-radius quantiles are
$
(0.06524073, 0.06620854, 0.06684654, 0.06770138, 0.07223774).
$
Every one of the $248077$ root children has a simultaneous lower confidence bound above $T=0.10$.
Consequently all $q$ children are certified heavy, the strict root frontier has width $248077$, and the
uncapped Dataset-GL traversal is infeasible at this threshold.  The saved diagnostic bank has $54501$ real
sine/cosine pairs, spanning $109002$ conjugate complex characters, but it is not the strict frontier and is
not promoted to the final fit.

The disjoint $2048$-pair discovery and $2048$-pair confirmation halves give confirmed total root energy
$52328.73556$.  Discovery-ranked $K=100000$ has confirmed sum $21233.27152$, numerator standard error
$900.96993$, and descriptive share $0.40576695$, versus scalar-i.i.d.-Gaussian prediction $0.76934540$.
For $K=109000$ the corresponding values are $23128.67590$, $982.10458$, $0.44198805$, and $0.80072176$.
At $K=131072$ the descriptive confirmed share is $0.53071741$.  These are cross-fit point ratios, not
simultaneous confidence bounds or Dataset-GL certificates; in particular, they do not show that $109000$
characters contain $80%$ of the vector energy.

=== Planned independently centered hard target

The next strict diagnostic calibrates once on a separate, hash-disjoint sample $calC$ and then freezes
$
tilde(m)=frac(1,|calC|) sum_(X in calC) Y(X) in Delta(ZZ_q),
quad
R_(tilde(m))(X)=frac(Y(X)-tilde(m),sqrt(2)).
$
It is a mathematically valid bounded vector target: for $g=g(X)$,
$
norm(R_(tilde(m))(X))_2^2
=frac(1+norm(tilde(m))_2^2-2 tilde(m)_g,2) <= 1.
$
For a GL pair with $g'=g(X')$, its inner product remains cheap and exact,
$
chevron.l R_(tilde(m))(X),R_(tilde(m))(X') chevron.r
=frac(ind[g=g']-tilde(m)_g-tilde(m)_(g')+norm(tilde(m))_2^2,2).
$
Conditional on the independent calibration sample, $tilde(m)$ and hence the target are fixed, so the same
simultaneous empirical-Bernstein and Dataset-GL proof applies.  The output decomposition restores the
calibrated global mean exactly as $Y(X)=tilde(m)+sqrt(2) R_(tilde(m))(X)$.  In a vector reconstruction,
$tilde(m)$ is an explicit fixed output-space bias.  The softmax model instead fits its constant logit bias
separately; no equality between a probability mean and a logit bias is assumed.

This centering is a strict diagnostic, not a sparsity theorem.  Its surviving density term is clearest at
the random-context coefficient-section level.  Let
$
m=EE_Z[p_Z],
quad p_z=EE[Y(X)|Z=z],
quad d_z(alpha)=EE[overline(chi_alpha(X))|Z=z],
$
and
$
c_z(alpha)=EE[(Y(X)-p_z) overline(chi_alpha(X))|Z=z].
$
Then the calibrated target satisfies the exact identity
$
hat(R_(tilde(m)))_z(alpha)
=frac(c_z(alpha)+(p_z-tilde(m))d_z(alpha),sqrt(2)).
$
Thus global centering removes the density contribution of $tilde(m)$ but leaves within-context covariance
$c_z(alpha)$ and the between-context drift $(p_z-tilde(m))d_z(alpha)$.  Even with the population value
$tilde(m)=m$, its DC random-context energy is generally nonzero:
$
calE_(R_m)(0)=frac(1,2) EE_Z[norm(p_Z-m)_2^2].
$
Conditional on the independent calibration sample, Dataset GL certifies the fixed target
$R_(tilde(m))$ exactly.  It does not thereby certify the population-centered target $R_m$.  If calibration
establishes $norm(tilde(m)-m)_2 <= eps_m$, then at every character, and likewise at every conditional
bucket, the two RMS magnitudes differ by at most $eps_m/sqrt(2)$; any claim about $R_m$ must include this
threshold allowance.  The search therefore evaluates all $q$ children and uses the same uncapped threshold
semantics.  If its complete live frontier remains too wide, it reports infeasibility; it never substitutes
a beam or the $54501$-pair diagnostic bank.

=== Strict transcript semantics

Writing $T=tau^2$ for the theorem's target energy, @thm:ar-qary-gl requires every expanded child's estimate
to have additive error at most $T/8$ and retains at the theorem cutoff $3T/8$.  Both the target evaluation
and every confirmation sample use fresh $X$-only teacher forwards.  Any feature list lacking this target,
accuracy, or all-$128$-level provenance is rejected before fitting.

The strict escalation records one search output.  Its *Dataset-GL transcript* expands every theorem-live child
under a scheduled failure budget and never applies a top-$K$ cap; if its certified live width exceeds the
resource ceiling, it terminates with that width as an honest infeasibility result.  Descriptive top-$K$
energy curves are diagnostics only and can never be promoted into the search output or final fit.

=== The standalone compressed model is the Fourier representation

Frozen Qwen has exactly two offline roles: it samples $X$ from the real-context conditional law, and a
separate $X$-only forward supplies labels.  Qwen is absent at student inference.  The compressed artifact
contains only the certified character list, learned Fourier factors and output bias displayed below, plus
tokenizer metadata; it contains no Qwen parameters, embeddings, hidden states, activations, or KV cache.
For the complete certified character list
$S subset.eq ZZ_q^128$ of size $K$, the student logits are
$
ell_S(x)
=b+sum_(alpha in S)
  (c_alpha cos(2 pi chevron.l alpha,x chevron.r/q)
   +d_alpha sin(2 pi chevron.l alpha,x chevron.r/q)),
quad
Q_S(dot|x)="softmax"(ell_S(x)).
$
Here $c_alpha,d_alpha in RR^q$ are genuinely vector-valued *logit weights* in a Fourier representation.
Because they are refit through a softmax objective under a nonuniform law, they are not identified with the
dataset Fourier coefficient vectors $hat(F)_calD(alpha)$ from @cor:ar-qary-vector.  A dense
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
For a real target, one stored sine/cosine pair spans the conjugate characters $alpha$ and $-alpha$.
Thus $109000$ selected complex characters normally become about $54500$ stored real pairs; at rank $128$
this uses $45953933$ parameters and remains below the same $50$ million ceiling.

The packed GPU evaluator stores only each character's nonzero token positions and $ZZ_q$ frequencies.
For fixed chunks of $8192$ terms it gathers the required tokenizer ids, performs one integer dot product
modulo $q$, evaluates sine and cosine, and accumulates directly into the rank-$r$ state.  The compiled
PyTorch path therefore never allocates a batch-by-$K$-by-$128$ tensor or a $2K times q$ table.  Chunking is
only an algebraically exact evaluation schedule for the displayed sum; the prediction contains no neural
backbone, attention stack, or path back into Qwen.

Training uses $g(X)$ from the fresh teacher forward on the complete $128$-token $X$ as the primary label and
the corresponding full-distribution KL as a secondary objective.  There are no auxiliary sampled-token
labels and no shorter or shifted windows: every supervised row has exactly the same $X mapsto f_theta(X)$
semantics as evaluation.  The final fit accepts only the complete certified character list after all $128$
coordinates.  If that list does not fit the declared resource and parameter ceilings, the strict experiment
reports infeasibility rather than pruning it.

=== What would constitute a provable $90%$ agreement result

There are two useful sufficient loss bounds for the hard target.  If $Q(dot|X)$ is the student's probability
vector and $g=g(X)$ the teacher argmax, then a top-token disagreement forces $Q_g <= 1/2$ and hence
$
ind[arg max Q != g]
<= frac(-log Q_g,log 2).
$
It also forces the multiclass Brier loss to be at least $1/2$, so
$
ind[arg max Q != g] <= 2 norm(Q-e_g)_2^2.
$
Thus population hard-label cross-entropy below $0.1 log 2$ or population Brier loss below $0.05$ is
sufficient for at least $90%$ agreement.  These are conservative sufficient conditions, not claims about
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
Success requires $L_"CP" >= 0.90$, not merely a $90%$ point estimate.  For the predeclared
$n=5000$ and $delta_"eval"=0.025$, this requires at least $4542$ agreements ($90.84%$ observed).  The final
failure statement is union-bounded with the separate Dataset-GL transcript budget, for example
$delta_"GL"+delta_"eval"=0.05$.  Dataset-GL completeness certifies recovery of heavy correlations; the
untouched exact-binomial lockbox certifies agreement of the resulting frozen predictor.  Neither is
silently substituted for the other.

== Walsh--Hadamard characters over LSH token codes <sec:whlsh>

The cyclic $ZZ_q$ basis assigns the vocabulary an arbitrary group law: characters couple token ids that are
numerically close for tokenizer-internal reasons only, and @sec:qwen-experiment therefore treats
id-permutation sensitivity as an artifact detector.  This variant replaces the id geometry with an
embedding-derived binary one.  Every token $t$ receives a code $c(t) in {0,1}^B$ by sign-LSH: rows of the
teacher's input-embedding matrix are mean-centered and projected onto $B$ fixed Gaussian directions, so
that tokens with nearby embeddings share most bits.  $B$ is doubled (nested projections, fixed seed) while
additional bits still separate distinct embedding rows, capped where the measured collision curve flattens;
the residual colliding rows -- a near-duplicate tail including exactly duplicated embedding rows, which no
projection separates -- receive deterministic tie-break bits, so all $q=248077$ codes are distinct.  The
realized $B$, the collision-versus-$B$ curve, tie-break count, and duplicate-group sizes are reported.  A sequence is encoded as the concatenation of its tokens'
codes and characters are Walsh--Hadamard parities of those bits -- the $q=2$ specialization of the
categorical machinery, so the phase, histogram, and confidence-interval primitives are reused unchanged.

Sampling remains token-granular: the gate reuses the conditional rollout law and pair collection of
@sec:qwen-experiment verbatim, and one shared rollout per level scores every encoding, so comparisons are
exactly matched.  Within the newly resampled token, masks of up to three bits are grown greedily by
single-bit additions re-scored on the same pairs; this within-token search is a declared heuristic with the
same epistemic status as the categorical beam, and conservative simultaneous intervals are still reported
per parent.  The control is a code table of i.i.d. uniform bits at the same width $B$: unique codes with no
locality.  Any gap between the LSH and control encodings on identical rollouts, identical training data,
and an identical student isolates the embedding geometry as the explanatory factor, in the same spirit as
the token-id permutation control.

The student, data splits, loss, and evaluation battery are those of @sec:qwen-experiment; the only change
is the gated feature map, which becomes the $plus.minus 1$ parity of each selected bit mask (characters are
real at $q=2$, so there is no sine block).  Spending is tracked on a separate ledger capped at $12$ so the
variant cannot consume the pre-registered categorical budget; the label and prefix artifacts are shared
byte-for-byte.

#let whlsh-results-ready = false
#if whlsh-results-ready [
  #include "../experiments/results/qwen35_argl/whlsh_macros.typ"
  #include "../experiments/results/qwen35_argl/whlsh_tables.typ"
] else [
  #block(stroke: 0.6pt + gray, inset: 8pt)[
    *Not yet run.*  This box is replaced only by result fragments generated from the Modal
    `summary_whlsh.json` artifact.
  ]
]

== Canonical dataset GL over LSH codes: fibers, exact buckets, and the two-scale result <sec:whlsh-canonical>

An isolated single-module implementation (`experiments/canonical/qary_lsh_dataset_gl.py`) runs the
dataset-GL fiber recipe directly: $M$ anchors in real FineWeb text supply the fixed conditioning
(a real prefix whose last $k=3$ tokens are the deduplicated fixed string), the teacher itself fills the
remaining $61$ window tokens $R=8$ times per fiber as the natural-language sampler, and the terminal
next-token distribution (512 slots) labels every row.  The search is the exact-Parseval csamp group-by
tree over the filled span's $61 times 133$ LSH bits with $tau$ as the statistical parameter, an ordered
bounded frontier deduplicated by on-data equivalence, and centered targets (an uncentered target admits
bias-artifact characters: biased bits give coefficients aligned with the unigram with zero conditional
content -- token-id bits produced 511 such characters whose fit is exactly the unigram).  Per-level
widths, pre-filter leaf norms, and frontier masks are persisted, so changing $tau$ is a re-filter rather
than a re-descent.  Every bucket computation is verified against brute-force enumeration at small $n$ in
the module's tests.

Two results, measured on identical fibers, targets, and fits:

+ *The exact degree-one spectrum contains hundreds of heavy characters -- and the bounded-frontier
  tree buries them.*  Computing every degree-one coefficient exactly (one GEMM, no search) shows
  top centered norms of $0.048$ for LSH ($217$ characters $>= 0.01$, all on the last token) against
  $0.044$ for random codes ($171$), with LSH carrying $40%$ more total degree-one mass ($0.174$ vs
  $0.124$).  The ordered tree, by contrast, reported only noise-order statistics ($approx 0.006$,
  scaling as $1\/sqrt(m)$): in the singleton-suffix region every bucket weight is provably equal
  ($chi^2 = 1$ makes each bucket the total mass), so the width-bounded frontier tie-breaks
  arbitrarily and a heavy single-bit leaf -- created exactly once, at its own level -- survives to
  the exact leaf test only by luck.  This is the earlier campaign's documented degree-one
  under-allocation failure; the complete degree-one basis must be evaluated exactly (it is one
  GEMM) rather than entrusted to the frontier, which is needed only for degree $>= 2$.

  To be precise about what failed: the failure is a property of the *estimator this implementation
  used*, not of the dataset-GL tree of the theorems.  The implementation grouped rows by the VALUE
  of the un-split coordinates and squared the group sums, which retains the diagonal terms
  $sum_x f(x)^2 chi_S(x)^2 = sum_x f(x)^2$ -- an $S$-independent mass that dominates every bucket
  while un-split values rarely coincide, producing the tie region above.  The paper's CSAMP
  estimator is different in exactly the right way: it conditions on the FIBER (the generating
  context), takes PAIRS of independent fills within it, and its off-diagonal pair sum
  $sum_z sum_(x != x') f(x) chi_S (x) dot f(x') chi_S (x')$ is unbiased for the conditional weight
  $EE_z |EE[f chi_S | z]|^2$ with no diagonal and no extension-noise aggregation: un-split
  coordinates are averaged over the conditional law rather than matched, so the bucket
  discriminates at every level and the threshold prunes on conditional mass, as the theorem
  requires.  Equivalently, the pair form is the fiber group-by square minus the computable
  diagonal.  The exact per-degree enumeration used above is a valid (and for low degree, cheap)
  alternative; the native pair-CSAMP tree is the general method.  Done this
  way, the recovery-then-build program works end to end, verified on a strictly untouched test
  split: a model of the $approx 220$ exactly certified LSH characters plus the $1000$ heaviest
  exactly certified pairs reaches test KL $1.103$ against the unigram floor $1.630$ -- surpassing
  every fitted student of this section -- while the random-code certified set reaches $1.200$.  The
  degree-two spectrum itself is the sharpest encoding measurement of all: LSH concentrates $485"k"$
  anchored pairs above $0.01$ ($27%$) where random codes hold $19.8"k"$ ($1.4%$), a $25times$
  low-degree-concentration gap; encodings converge as degree grows, so the LSH advantage is
  precisely that it needs less degree.  The recovered model is $625"k"$ parameters and runs as a
  standalone demo.  The ladder terminates at degree two by *redundancy rather than absence*: a
  thousand exactly certified triples exist (top norm $0.060$, above every pair), yet fitted
  incrementally on the frozen degree-one-plus-two residual they receive weight exactly zero under
  the validation gate, for both encodings -- products of already-captured characters, spanned by
  the lower degrees.  The window's recoverable information is exhausted, and the recovery knows it.

+ *The aggregate is decisively encoding-dependent.*  Fitting the complete degree-one basis of the
  filled span (held-out slot KL against the unigram floor $1.6303$, $M=8000$):

  #table(columns: 4,
    [Encoding], [Features], [Held-out KL], [Gain (nats)],
    [sign-LSH of teacher embeddings], [8113], [1.4915], [*0.139*],
    [token-id bits], [1098], [1.5975], [0.033],
    [random codes (capacity-matched)], [8113], [1.6303], [0.000])

  The random-code fit never improves on its unigram initialization by a single accepted step; the
  identical fit on LSH bits extracts $0.139$ nats.  The ordering is monotone in the semantic content of
  the code table.  (Optimization footnote: on peaked soft labels the degree-one fit diverges at the
  default learning rate and the best-validation floor masks the failure as "no signal"; the reported
  fits use learning rate $0.004$ over $4000$ steps.)

Together: the LSH encoding advantage over arbitrary orderings is real and reproducible, and it is
carried by the mass of many individually sub-threshold coefficients rather than by any certifiable
single character at practical fiber counts.  Consistently, appending each encoding's searched
frontier (the 64 heaviest deduplicated multi-bit masks) to the degree-one basis changes no held-out
KL (LSH $1.4915 -> 1.4977$, random codes $1.6303 -> 1.6303$): at these fiber counts the searched
interaction characters are order statistics of noise, and the degree-one aggregate already saturates
the recoverable signal.

Finally, the STAGED backoff student (one token block of degree-one features at a time, nearest the
prediction first, each block accepted only if fiber-disjoint validation improves) extracts far more
than the flat joint fit and preserves the encoding gap at higher absolute quality:

#table(columns: 5,
  [Student], [Accepted blocks], [Held-out KL], [Gain (nats)], [Top-1],
  [staged, LSH codes], [3], [1.1232], [*0.507*], [0.659],
  [staged, random codes], [4], [1.2849], [0.345], [0.655],
  [staged, token-id bits], [7], [1.3965], [0.234], [0.652],
  [flat degree-one, LSH], [--], [1.4915], [0.139], [--],
  [unigram], [--], [1.6303], [0.000], [0.653])

The per-block efficiency ordering is the sharpest signature of the encoding: LSH extracts the most
information from the fewest tokens (three blocks), random codes act as token-lookup keys (four
blocks, two-thirds the gain), and token-id bits crawl to position eight for half of it.  Honest
signal localizes near the prediction, echoing the earlier campaign's localization finding.  Top-1
agreement barely moves for any window student -- the teacher's argmax matches the majority slot on
$65%$ of held-out rows, so the distributional (KL) view is where window-scale structure lives.  A
full-context flat degree-one fit, by contrast, is rejected wholesale by the validation floor:
prefix bits identify fibers, and the flat student memorizes them.

On real cached contexts (story-disjoint splits, untouched test set) the staged student reproduces
and extends every finding: LSH $1.4001$ held-out KL against unigram $1.8702$ ($-0.470$ nats) versus
random codes $1.5819$; doubling the training contexts ($20"k" -> 40"k"$) improves both encodings by
$approx 0.045$ nats -- the data-scaling law transfers to the student -- while the encoding gap
($approx 0.18$ nats) is scale-stable and the accepted depth stays at three tokens.  Hereditary
degree-two pairs among the individually proven bits are rejected by the validation gate for every
encoding, the third independent confirmation that degree one saturates the parity class.

Finally, the transformer-level attribution.  A 30M-parameter student with the proven degree-one
masks as gated features (40k contexts, full-tokenizer evaluation) reaches $21.2%$ top-1 against a
$19.3%$ baseline -- but the identical architecture with random-code masks reaches $20.9%$: the
feature lift is generic capacity, not geometry.  The complete picture is a *two-regime law*: the
LSH encoding is worth $0.18$--$0.21$ nats precisely in bottlenecked models, where the code is the
model's only access to the tokens (the Walsh-polynomial compression regime), and nothing in models
that already see the tokens through an embedding table.
