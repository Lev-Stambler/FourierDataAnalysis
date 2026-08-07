# Exp19: repair the residual stream before judging group density

Exp19 is a cloud-only normalization and conditioning audit for the 5.4M
block-causal models. Exp17 showed that the rank-1 group branch is useful but
does not beat the Transformer; Exp18 showed that it can memorize one and two
blocks quickly. The saved telemetry then exposed a confound: the 32-layer
group model applies 64 post-residual, affine-free RMS normalizations, while the
depth-3 Transformer applies only three. The resulting residual stream has no
exact identity path.

The strongest rank-1 cloud replicas also collapse group-hidden variance by
roughly three to four orders of magnitude from the first to last layer while
some nonlinear updates exceed the state RMS. Exp19 repairs and isolates that
conditioning problem before changing Kronecker rank or adding another router.

## Eight cumulative tracks

| Track | Intended delta |
|---|---|
| `legacy-exp17-postnorm-r1` | frozen Exp17 rank-1 reference |
| `prenorm-affine-free-redundant-r1` | only replace post-norm topology with an identity skip; audit-only because the retained gauges fail numerical parity |
| `prenorm-learned-mixed-redundant-r1` | add learned RMS gains while retaining tokenwise mixer and joint-group FFN statistics; audit-only |
| `prenorm-clean-joint-r1` | remove redundant gauges, calibrate embedding residual RMS, and retain joint-group FFN normalization |
| `prenorm-clean-token-r1` | change only the clean FFN normalization domain from joint-group to tokenwise; proposed primary |
| `prenorm-no-router-token-ffn` | replace group density with a parameter-matched token-local SwiGLU |
| `prenorm-transformer-d32-w128` | same width, depth, vocabulary rank, and norm topology |
| `prenorm-transformer-d3-w256` | practical wide/shallow total-parameter control |

The proposed primary is `prenorm-clean-token-r1`, the scale-clean tokenwise
learned-RMS rank-1 model. The six training tracks are the frozen legacy,
clean-joint, clean-token, token-FFN, deep Transformer, and wide Transformer.
The two redundant-gauge tracks still occupy their own GPUs during the
correctness audit, but their measured compiler/optimizer discrepancies make
them non-promotable; they do not contaminate the compiled learning curves.
Every changed identity is new; no historical checkpoint or result is silently
relabelled.

All primary and control tracks are within `0.1%` of the 5,400,896-parameter
target. The learned-RMS cumulative diagnostic is deliberately 8,320 parameters
larger (`0.155%`): those parameters are the change being measured, and its FFN
was not silently narrowed to disguise them. Exact inventories and the exception
are hard-recorded by preflight.

## Corrected residual form

The corrected layers use

$$
x' = x + \alpha\,\mathrm{Mixer}(\mathrm{RMSNorm}_1(x)),
\qquad
x'' = x' + \beta\,\mathrm{GroupFFN}(\mathrm{RMSNorm}_2(x')),
$$

followed by one final learned RMSNorm. The primary fixes
$\alpha=\beta=1/\sqrt{2L}$ and removes the redundant product of group path,
input scale, output scale, and learned residual gain. The frozen and cumulative
ablation tracks identify which change matters.

## Launch gates

No corpus training may start unless all applicable tracks pass:

- exact factored/materialized contraction and block-causal isolation;
- branch-zero identity for pre-norm tracks;
- eager versus checkpointed FP32 gradient and optimizer-step parity;
- eager versus compiled BF16 gradient parity;
- fused BF16 versus materialized FP32 loss agreement;
- complete, finite optimizer routing;
- finite initialized branch/state ratios, no individual ratio above `0.5`, and
  aggregate branch-energy RSS in `[0.05, 0.75]` for corrected tracks;
- all eight H100s, at least 100,000 physical tokens per step, and an ambitious
  downward batch sweep selecting the measured highest-throughput stable batch.

The old per-branch `[0.02, 0.20]` fraction remains logged as a diagnostic, not
a depth-independent lower-bound gate. With fixed `1/sqrt(2L)` residual scales,
the recovered depth-3 and depth-32 Transformers have nearly identical
aggregate branch energy (`0.08213` and `0.08238`) even though each deep branch
is smaller.

The preflight records utilization, power, throughput, allocated/reserved VRAM,
physical batch, global tokens, gradient accumulation, and full-node behavior.
It also uploads the complete preflight JSON as a W&B artifact and a per-layer
initialization table to the direct W&B run, including failed audit tracks.

## Evidence ladder

1. Screen three AdamW rates and three joint Muon body/auxiliary rates on one
   fixed block, with weight decay disabled.
2. Confirm the best recipe from each optimizer family on two blocks and three
   fresh seeds.
3. Advance through nested 8, 32, and 128 unique-block sets with a fixed held-out
   set. Physical repetition fills the GPU but is never counted as unique data.
4. Run a corpus screen only if a corrected group track beats the legacy model,
   corrected token-FFN control, and same-shape Transformer while satisfying the
   conditioning gates.

Large physical batches no longer imply tiny optimization horizons. Every
result reports both tokens and optimizer updates; a corpus verdict requires at
least 128 updates, followed by fresh-seed confirmation at 256 or more updates.

## Telemetry

At initialization and evaluation boundaries Exp19 records, per layer:

- state, raw branch, scaled update, and update/state RMS;
- residual input/output cosine;
- normalization gains and every remaining branch/rank scale;
- group-hidden variance, participation ratio, zero-variance fraction, and
  activation saturation;
- factor singular extrema, stable rank, condition, and row/column energy;
- parameter-gradient and update/weight RMS by layer and parameter family;
- radial versus tangent updates for normalized factors and effective
  function-space change after normalization;
- vocabulary, hidden, and logit scales;
- train and held-out NLL by causal group and workspace position;
- clipping, LR, unique exposure, wall time, throughput, VRAM, and utilization.

The hard conditioning gate rejects branch/state ratios above `0.5`, more than
a 100-fold first-to-last hidden-variance or gradient imbalance, and normalized
factor updates retaining less than 25% of their raw optimizer displacement.

## Status

The corrected eight-H100 preflight passed at a 1,048,576-token physical batch
on all tracks with 99.5–100% median utilization. The one-example screen showed
an 8-step clean-Kron versus 24-step best-Transformer memorization advantage,
but also a 9.9–25x Transformer throughput advantage and only 6–8% effective
factor displacement after normalization. Northflank replaced the instance
during replicated confirmation, so that W&B run is correctly marked crashed
and no later result is claimed. Because `/cache` proved instance-ephemeral, the
launcher now detects stale heartbeats and restores source, corpus, runtime, and
a local SHA-256-verified scientific-state bundle after bounded replacement.
The bundle restores the passed preflight and complete W&B-recovered screen; no
partial confirmation is claimed. Every completed stage now emits a portable
aggregate snapshot. Live evidence and W&B URLs are recorded in
[`RUN_RESULTS.md`](RUN_RESULTS.md).

The verified-bundle resume completed all two-example confirmations. Clean joint
Kron memorized in `8/8/8` evaluated steps and clean token in `12/8/8`, versus
`16/16/16` for the token-FFN control and `24/24/24` for both winning Transformer
recipes. This establishes a replicated optimizer-step advantage, not a compute
or held-out win. Clean residual conditioning remains healthy; the remaining
hard failure is normalized-factor update efficiency (`6.7–8.0%` effective
versus the `25%` gate). The 8/32/128 held-out ladder is active on the direct W&B
run linked in [`RUN_RESULTS.md`](RUN_RESULTS.md).
