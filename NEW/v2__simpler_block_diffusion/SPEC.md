# V2-SBD-97M-DreamR32-v1 frozen specification

Status: implemented, locally tested, eight-H100 systems preflight passed, and
the paired learning-rate screen plus 100M-target-token confirmation are
complete. The frozen recipe failed top-1 and greedy-generation promotion, so
the 10B campaign is not authorized. Its CE corrective continuation also failed
the greedy gate. `V2-SBD-FKL-Muon-r1` below is a fresh proposed successor, not
a resume or silent mutation of this frozen candidate.

## Objective

Distill frozen `Dream-org/DreamReasoner-8B` revision
`ed62b1d2c82ccd234b05ed2463b4c0ee640f2068` online into a
97,832,320-parameter block-diffusion model. The primary budget is 10 billion
supervised masked target tokens at context 2,048. Teacher distributions are
ephemeral exact top-16-plus-tail targets; a full vocabulary-logit tensor is
never materialized or stored.

This experiment has no paid dense-student control. It can measure teacher
retention and throughput, but cannot establish that Monarch is superior to a
dense Transformer.

## Frozen student

| Field | Value |
|---|---:|
| Vocabulary | 151,936 |
| Mask token | 151,669 |
| Width / layers | 256 / 11 |
| Query / KV heads | 4 / 2 |
| Head width | 64 |
| Context / block | 2,048 / 32 |
| Monarch blocks / rank | 128 / 1 |
| Local expansion | 2 |
| RoPE theta | 1,000,000 |

For current block `b`, GQA keys and values come only from clean completed
blocks `< b`. No attention edge exists within the current block. Instead, each
layer flattens the current `32 × 256` workspace to width 8,192 and applies

```text
M_down(SiLU(M_gate(x)) * M_up(x))
```

where gate/up are rank-1 Monarch maps `8192 → 16384` and down is
`16384 → 8192`. Every map uses 128 blocks and no bias. There is no separate
token FFN.

Every residual branch uses non-affine RMSNorm plus AdaLN-Zero-style scale,
shift, and zero-initialized gate. A shared sinusoidal/MLP embedding encodes the
per-block mask fraction `masked_count / 32`; clean blocks use zero.

Exact parameter accounting:

| Component | Parameters |
|---|---:|
| Tied embedding/output | 38,895,616 |
| 11 layer bodies | 58,410,880 |
| Mask-rate MLP | 525,568 |
| Final RMSNorm | 256 |
| **Total** | **97,832,320** |

## Online objective

Each example contains noisy and clean streams. Noisy block `b` sees its own
noisy workspace plus earlier clean blocks. Clean block `b` sees its own clean
workspace plus earlier clean blocks. The Dream teacher receives the equivalent
packed 4D mask; the student implements the two mechanisms separately.

For every eligible block, sample a mask count uniformly from one through the
number of eligible positions, then select exactly that many positions. Loss is
averaged inside each block before averaging blocks.

The teacher output projection is streamed over vocabulary chunks in FP32. For
teacher probabilities `p`, exact student probabilities `q`, and clean target
`x0`, the loss is

```text
0.8 * KL(p_top16+tail || q_top16+tail) + 0.2 * CE(x0, q).
```

The custom autograd implementation recomputes vocabulary chunks in backward,
producing exact hidden and tied-output gradients without retaining full logits.
The mask token is excluded from both distributions.

## Data and schedule

`data_manifest.lock.json` pins all dataset commits and licenses. Stage 0
(0–8B target tokens) weights are Dolma 3 55%, FineWeb-Edu 20%, FineMath
`finemath-4plus` 10%, OpenWebMath 10%, and permitted Tulu 3 subsets 5%. Stage 1
(8–10B) uses 10%, 10%, 10%, 10%, 35%, and permitted Dolci-Instruct 25%.

The ungated ODC-BY FineMath 4+ split replaces the originally proposed gated
Stack v1 source. A preflight also rejected SmolLM Python-Edu because its
parquet rows contain repository indexes but not source text. Non-commercial
and unapproved third-party subsets remain excluded.

AdamW uses betas `(0.9, 0.95)`, epsilon `1e-8`, matrix weight decay `0.1`,
gradient clip `1.0`, a 100M-target-token warmup, and cosine decay to 10% of the
peak LR. Peak LR is selected from `1e-4, 3e-4, 6e-4, 1e-3` using four 25M-token
online screens.

The screen protocol (not an architecture change) used a 2M-token
linear warmup and cosine decay over each 25M-token run, with fixed held-out
evaluation every 5M tokens. This is necessary to expose the actual candidate
peak inside a short screen. All four runs use the same audited, duplicate-free
32,768-context cache and compiled batch 64 on all eight GPUs. All candidates
were finite. Peak LR `1e-3` won every paired milestone and finished with fixed
grouped KL `1.04975`, a 66.75% reduction from `3.15725`, versus `1.34115`,
`2.04618`, and `2.70216` for `6e-4`, `3e-4`, and `1e-4`. Its normalized KL AUC
was also best at `1.53986`. Confirm the selected LR for 100M target tokens with
a 10M-token warmup. A promoted 10B run retains the frozen 100M-token warmup.

## Accelerator and evidence gates

Paid execution requires exactly eight H100-80GB GPUs and eight-way DDP. Each
GPU holds a frozen BF16 teacher replica and one student replica. W&B must be
authenticated before launch and its direct run URL must be present in launch,
checkpoint, result, and audit artifacts.

The batch sweep searches downward from 64 physical contexts/GPU. It records
target/clean/model tokens per second, global target batch, allocated/reserved
VRAM, utilization, and scaling. The selected recipe must reach at least
100,000 global target tokens per optimizer step. Gradient accumulation is used
only when the largest stable physical batch cannot do so directly.

Measured on eight H100-80GB GPUs, eager batch 80 and 72 OOMed while eager batch
64 was stable at about 541,648 target tokens/step and 41.27k target tokens/s.
The corrected compiled batch-64 run reached about 543k target tokens/step,
41.62k target tokens/s, 94.84% mean utilization, and 56.22/58.59 GiB peak
allocated/reserved VRAM. The real-corpus compiled hot-load proof at batch 56
reached about 462.6k target tokens/step and 39.24k target tokens/s. The teacher
consumes about 90% of steady step time. These are systems measurements, not a
long-run model-quality claim.

Promotion beyond 100M target tokens requires finite state, at least 20% KL
improvement from 10M, at least 35% overall teacher top-1 agreement, at least
20% fully masked agreement, and at least 95% non-degenerate fixed generations.
The online/cache decision is measured after 100M tokens exactly as recorded in
the experiment plan; online remains the default.

The fixed generation gate uses all 128 immutable held-out contexts. Their first
64 clean tokens are prompts and the student greedily generates up to 64 tokens
by committing one 32-token block at a time. A completion is non-degenerate when
it emits at least eight tokens before EOS, contains no mask token, decodes to at
least eight non-whitespace characters, has at least 10% unique token IDs, no
identical-token run longer than eight, and repeated 4-grams occupy at most 50%
of its 4-grams. The eight-H100 evaluator assigns 16 prompts to every rank,
persists all samples and per-sample diagnostics, and requires its own direct
W&B URL. This is an evaluation-protocol definition, not a model change.

## Staged corrective recipe: V2-SBD-v1-CE80-r1

The immutable 100M result reduced grouped KL from `3.15725` to `0.95336`, but
ended at `18.51%` teacher top-1 and 0/128 non-degenerate greedy completions.
Its sampled diagnostic passed the mechanical health test for 127/128 outputs
but remained incoherent. Before implementation, the proposed mathematical
delta is therefore to change the objective weights from `0.8 KL + 0.2 clean
CE` to `0.2 KL + 0.8 clean CE`, starting from the immutable 100M checkpoint.
The teacher remains online and frozen. There is no architecture, parameter,
data, or forward-compute change, and the exact streamed loss kernel is reused.

The first stage ends at 150M total target tokens (50M new). It is falsified if
state becomes non-finite, fully-masked hard NLL fails to improve by 5% from
`7.67888`, or greedy non-degenerate rate remains 0%. Passing this diagnostic
does not retroactively promote the frozen recipe; continuation requires a new
recorded checkpoint and generation result.

### Superseding staged recipe: V2-SBD-v1-CE100-pretrain-r1

The CE80 implementation passed one finite online-teacher optimizer step, but
its planned 50M continuation is superseded by the user's benchmark-quality
goal and permission to use pretraining/SFT. The proposed training loss is 100%
group-balanced clean-token cross-entropy under the unchanged block corruption.
Dream is used only for fixed held-out evaluation and unloaded before training.
This removes the measured 91.6% steady teacher bottleneck and does not change
the model, parameter count, or inference graph.

First perform a new no-accumulation batch/compile sweep, searching downward
from approximately one million global target tokens per step. Then run a
25M-new-token continuation from the immutable 100M checkpoint. It must be
finite, improve fully-masked hard NLL, improve greedy generation beyond 0/128,
and retain directly logged held-out Dream metrics. Only passing evidence can
authorize larger corpus pretraining and a separately identified chat-SFT
stage. Any comparison with a roughly 0.5--1B dense model must report individual
benchmark tasks and define block pseudo-likelihood scoring explicitly.

For that resumed diagnostic, LR progress is based on new CE tokens rather than
the historical absolute counter: schedule origin is the immutable checkpoint's
target-token count, warmup is 2M new target tokens, and the cosine horizon is
25M new target tokens. Optimizer moments are retained, while all parameter-group
LRs are set from this continuation schedule before the first CE update. Both
absolute counters and the explicit schedule origin/horizon are checkpointed and
audited.

The throughput implementation audits finite model, gradient, and optimizer
state on update 1, every W&B logging update, and the terminal preflight update.
Device predicates are aggregated before a single host synchronization. Loss
metric all-reduces and CUDA-event timing are likewise deferred to logging
updates. This does not alter the objective or update values; the 21-step
log-every-10 preflight must demonstrate both finite state and improved measured
throughput before the quality launch.

## Fresh successor implementation: V2-SBD-FKL-Muon-r1

This candidate starts from new student and optimizer state and retains only
the pinned Dream teacher, tokenizer, data locks, and reusable systems assets.
An output-rank oracle screens width/layer pairs `256/11`, `288/9`, `320/7`,
and `384/5`, whose exact parameter counts are respectively `97,832,320`,
`104,656,608`, `107,085,696`, and `119,011,456`. A randomized-SVD teacher-head
factorization initializes a tied student vocabulary and temporary projected
teacher hidden. The smallest oracle at or below `0.10` exact held-out KL on
both aggregate and fully masked slices is selected; if none qualifies, only a
lowest oracle no worse than `0.20` may continue.

The training and evaluation metric is temperature-one, full-vocabulary
`KL(P_Dream || P_student)`, excluding only the mask token. It is streamed over
vocabulary and position chunks and never collapses a tail bucket. Training
uses up to 256 stratified targets per context; evaluation uses all targets.

Muon acts independently on every final-two-dimensional matrix represented by
each Monarch rank/block slice. Dense body matrices also use Muon. The tied
vocabulary, norm vectors, and biases use AdamW. Optimizer routing must be
complete, disjoint, checkpointable, and audited by parameter name and count.

The first 10M targets use exact KL plus a `0.1` projected-hidden cosine
auxiliary with the oracle vocabulary frozen. Thereafter training is pure exact
KL with the vocabulary unfrozen. Once fixed KL is at most `1.0`, live DAgger
states enter at 50%; at KL at most `0.5`, they enter at 80%. A DAgger state is
produced by a random prefix of the production eight-step block decoder, after
which Dream supplies one exact full-distribution label query for all remaining
masks. The fixed distribution remains in every mixture and in every gate.

The CE-entry gate is exact full KL at most `0.20` on aggregate fixed,
fully-masked, and on-policy evaluations for three frozen seeds. Hard NLL is
logged but non-blocking. The adaptive cap is 4B supervised targets. A passing
checkpoint begins expected `0.9 CE + 0.1 full KL`: CE runs every step and a
live full-KL anchor is added deterministically every tenth step.

The exact loss, optimizer routing, oracle artifact, projected-hidden warm start,
target cap, and shared DAgger/decoder commit rule are implemented. The initial
systems evidence is batch 88 OOM and batch 80 stable; compiled batch 80 reached
163,636 supervised targets/update, 10,628 targets/s, 93.33% mean utilization,
and 54.03/58.33 GiB peak allocated/reserved memory over three finite updates.
This validates the execution path only. The randomized-head KL near `5.56`
does not satisfy a quality gate and is not an initialization claim.

Checkpoints are scheduled at 10M, 50M, 100M, 250M, 500M, 1B, 2B, 4B, 6B, 8B,
and 10B target tokens. A failure is recorded as a completed negative result;
the architecture identity is never silently changed.
