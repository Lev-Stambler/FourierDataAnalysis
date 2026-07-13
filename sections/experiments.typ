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

== Corrected student and loss

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

No numerical curve currently in the repository was generated under this $X$-only target law.  The first
corrected split-rank/confirmation run will therefore establish the energy profile from scratch; the scalar
Gaussian overlay supplies no substitute values.

=== Strict transcript semantics

Writing $T=tau^2$ for the theorem's target energy, @thm:ar-qary-gl requires every expanded child's estimate
to have additive error at most $T/8$ and retains at the theorem cutoff $3T/8$.  Both the target evaluation
and every confirmation sample use fresh $X$-only teacher forwards.  Any feature list lacking this target,
accuracy, or all-$128$-level provenance is rejected before fitting.

The strict escalation records one search output.  Its *Dataset-GL transcript* expands every theorem-live child
under a scheduled failure budget and never applies a top-$K$ cap; if its certified live width exceeds the
resource ceiling, it terminates with that width as an honest infeasibility result.  Descriptive top-$K$
energy curves are diagnostics only and can never be promoted into the search output or final fit.

=== The compressed model is the Fourier representation

Qwen is kept fixed and is used only as the teacher, label oracle, and conditional sampler.  No teacher
layer is deleted, copied into the student, or used as a residual backbone.  For the complete certified character list
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
For a real target, one stored sine/cosine pair spans the conjugate characters $alpha$ and $-alpha$.
Thus $109000$ selected complex characters normally become about $54500$ stored real pairs; at rank $128$
this uses $45953933$ parameters and remains below the same $50$ million ceiling.

The packed GPU evaluator stores only each character's nonzero token positions and $ZZ_q$ frequencies.
For fixed chunks of $8192$ terms it gathers the required tokenizer ids, performs one integer dot product
modulo $q$, evaluates sine and cosine, and accumulates directly into the rank-$r$ state.  The compiled
PyTorch path therefore never allocates a batch-by-$K$-by-$128$ tensor or a $2K times q$ table.

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
