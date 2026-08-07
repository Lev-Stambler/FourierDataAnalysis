# Exp18: memorize before generalizing

Exp18 is a Karpathy-style debugging ladder for the matched 5.4M-parameter
WikiText models. It asks a simpler question than Exp17: can the implementation,
architecture, and optimizer drive the teacher-forced loss of one fixed block,
then two fixed blocks, essentially to zero?

## Why this experiment comes before another corpus run

Exp17 did not produce a clear architectural win. Its coarse best validation
NLL was 7.3701 for the Transformer, 7.3843 for the previous rank-8 KronMix,
and 7.4367 for the best rank-1 group-density candidate. More depth helped the
group model; increasing matched-budget group rank from 1 to 2 or 4 did not.
Those results mix together expressivity, optimization, and generalization.
Exp18 separates them.

## Ladder and gates

Eight serious parameter-matched tracks are retained: the Transformer, current
rank-8 baseline, router-free token model, rank-1/2/4 group models, hybrid, and
deep/narrow group model. The already-poor literal dense-group control is omitted
so that the LR screen maps exactly onto eight GPUs.

1. Repeat one deterministic 256-token WikiText block and screen four AdamW plus
   four Muon recipes. Weight decay is disabled and rates span conservative to
   deliberately aggressive boundaries.
2. Promote the best stable AdamW and Muon recipe independently for every model.
3. Repeat two deterministic blocks and confirm both promoted recipes at two
   initialization seeds.
4. A model memorizes only if exact teacher-forced token accuracy is 100% and NLL
   is at most 0.01. NLL at most 0.001 is logged as the stricter result.

The report retains complete loss/accuracy curves and first-hit optimizer steps
and wall times for NLL 1, 0.1, 0.01, 0.001, and perfect token accuracy. This is
an interpolation diagnostic, not evidence of generalization. Per-target-group
NLL and accuracy show whether a failure is localized to a particular causal
distance or the final continuation group rather than hiding it in one mean.

## Compute accounting

Training is cloud-only on the paid 8xH100-80GB service. The one or two unique
examples are repeated into the highest-throughput stable physical batch found
by an ambitious per-model sweep. Results report both the real physical tokens
processed and the 256 or 512 unique tokens per optimizer step; duplicate tokens
are never represented as new data. W&B authentication, a direct run URL, exact
BF16 fused-loss agreement, finite forward/backward/optimizer state, all eight
GPUs, VRAM, utilization, and throughput are launch gates.

If a group-Kronecker model fails while the Transformer passes, the next action
is architecture/gradient debugging. If it passes, the same selected recipe
advances through 8, 32, and 128 examples before returning to the full corpus.

## Completed evidence: a tied interpolation win

The 64-cell one-sample screen and 32-cell two-sample confirmation are complete.
Every architecture/optimizer pair memorizes both blocks at both fresh seeds.
With independently selected optimizers, group rank 1, group rank 2, and the
deep group model reach exact accuracy and NLL at most `0.01` in a mean of six
steps. Current rank 8 needs eight, the router-free token model needs twelve, and
the matched Transformer needs thirteen. Rank 1 therefore has a real optimizer-
step interpolation advantage, but it is tied rather than a unique winner.

Rank 1 remains the next group default because it combines the tied fastest tier
with the strongest replicated full-data group result (`7.380991` versus
`7.381523` rank 4 and `7.382181` rank 2), the broadest observed aggressive-LR
success, the fewest paths, and higher throughput than ranks 2/4. The completed
result does not establish generalization: the tuned Transformer still wins
Exp17 at `7.155114` and is 26.7 times faster in the present kernels.

The complete optimizer-by-architecture table, unique-versus-physical compute
accounting, caveats, W&B link, and durable raw artifacts are recorded in
[`RUN_RESULTS.md`](RUN_RESULTS.md). The next gate is an 8/32/128-unique-example
ladder with held-out evaluation, not another immediate full-corpus run.
