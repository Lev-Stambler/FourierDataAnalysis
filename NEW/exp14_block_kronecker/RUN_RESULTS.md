# Experiment 14 run results

Status: **optimizer audit complete; the candidate's small AdamW win does not
survive independent Muon tuning, and tuned Muon Transformer wins decisively**

## Corrective optimizer audit — 2026-08-04

- W&B: <https://wandb.ai/lev-tear-tear-labs/exp14-block-kronecker/runs/f8ihuo7e>
- Full protocol and tables: [`OPTIMIZER_AUDIT.md`](OPTIMIZER_AUDIT.md)
- Grid: five AdamW rates plus 24 Muon body/auxiliary-rate combinations per
  model at 3M tokens.
- Selection: the best two recipes from **each** optimizer family advanced to
  constant/warmup-cosine testing on two seeds at 10M tokens.
- Final: best AdamW and best Muon recipes for each model, four fresh seeds at
  40M tokens.

The 10M stage independently locked Muon for both models: candidate body/aux
LR `0.06/0.003`, Transformer `0.03/0.003`, both constant. At 40M, the paired
candidate-minus-Transformer deltas were `+0.425489, +0.419244, +0.429490,
+0.427601`; mean `+0.425456`. The Transformer won all four seeds.

AdamW alone still gave the candidate four tiny wins, mean `-0.007284`, but
Muon improved the Transformer from mean NLL `7.383753` to `6.958072` while the
candidate's Muon mean was `7.383528`. The original Exp14 result must therefore
be interpreted as AdamW-specific rather than as a tuned architecture-level
win.

All eight candidate final cells ran concurrently at 100% utilization,
approximately 110k tokens/s, and 73.46/76.43 GiB allocated/reserved. The
Transformer ran approximately 1.84M tokens/s. Total actual training across the
corrective audit was `1,155,891,200` tokens. Northflank paused successfully.

## Locked architecture

- Candidate: `block-kron-r8`
- State: `16 causal groups × 4 × 4 workspace × 8 × 16 channels`
- Rank paths: 8
- Parameters: 5,400,896 total / 3,303,744 body
- Token-factor parameters: 43,008
- Objective: 16-token block-autoregressive WikiText NLL

## Local evidence

- Factorized/materialized order-five forward and backward equivalence: pass.
- Future-group isolation: pass for candidate, ablation, and all Transformer
  controls.
- Within-group bidirectional influence: pass.
- Reconstructed 272-token stream and 16-token label shift: pass.
- Parameter match across four Transformer aspect ratios: within 0.1%.
- Finite forward/backward with every candidate parameter receiving a gradient:
  pass.

No local training was run.

Repository-wide CPU suite after the fused-layout regression fix: `318 passed`.
After adding the compiled probes and replay: `321 passed`.

## Paid evidence — tuned pilot

- W&B: <https://wandb.ai/lev-tear-tear-labs/exp14-block-kronecker/runs/p6os7f4i>
- Hardware: 8 × NVIDIA H100 80GB HBM3.
- Uploaded source digest: `6fc375c6df8f...`.
- Exact BF16 fused-loss relative error across all six models: `1.49e-5` to
  `3.99e-5`, below the locked `0.02` limit.
- Candidate physical batch for AdamW and Muon: 512 contexts = 131,072 tokens
  per optimizer step, no gradient accumulation.
- Candidate utilization: 100% median; 74.0 GiB peak allocated / 75.5 GiB peak
  reserved.
- Eight simultaneous candidate workers: 225,493 aggregate tokens/s and 99.67%
  scaling efficiency.
- Selected candidate recipe: AdamW, constant LR `0.003`, weight decay `0.01`.
- Strongest Transformer locked by the robust stage: `block-transformer-d3-w256`,
  AdamW constant LR `0.003`.

### Final paired result at 10M target tokens

| Seed | block-kron-r8 | locked Transformer | candidate − control |
|---:|---:|---:|---:|
| 3 | 7.391438 | 7.411661 | -0.020223 |
| 4 | 7.390634 | 7.414110 | -0.023476 |
| 5 | 7.388037 | 7.411388 | -0.023350 |
| 6 | 7.388819 | 7.414455 | -0.025636 |

The candidate won all four fresh paired seeds. Mean delta is `-0.023171` block
NLL. This is a consistent matched-parameter/matched-token win, but it is smaller
than the predeclared `-0.05` strong-win gate, so the locked verdict remains
`block_kronecker_not_yet_better` rather than moving the goalposts after seeing
the data.

The workspace-permutation ablation was tied: permuted-minus-unpermuted deltas
were `+0.000165, +0.000441, -0.000121, +0.000022`. The current permutation
schedule has no demonstrated benefit.

### Performance caveat

The eager candidate ran at roughly 28.2k tokens/s. Transformer controls ran
roughly 0.59M–1.86M tokens/s at their selected batches. Thus the current
implementation was 20–66× slower despite full GPU utilization. Compilation
improves the candidate materially but does not close the control gap; details
follow.

## Compiled execution evidence

### Mode probe

- W&B: <https://wandb.ai/lev-tear-tear-labs/exp14-block-kronecker/runs/qk3ftxx7>
- Default Inductor: 108,937 tokens/s median, 3.85× eager, 100% steady-state
  median utilization, 57.9 GiB maximum allocated.
- Reduce-overhead: 91,552 tokens/s median, 3.24× eager.
- Eager repeat: 28,285 tokens/s median, 74.0 GiB maximum allocated.
- All compiled exact-loss relative errors were `4.1e-6` to `4.14e-5`; every
  forward/backward/optimizer benchmark was finite.
- Default Inductor was selected. Compile warmup was about 130 seconds.

The first mode probe at
<https://wandb.ai/lev-tear-tear-labs/exp14-block-kronecker/runs/nsoo13k5>
was intentionally interrupted after max-autotune spent more than six minutes
compiling the monolithic graph while six faster workers were idle. It produced
no accepted throughput result. The balanced replacement used two eager, three
default, and three reduce-overhead workers.

### Compiled batch sweep

- W&B: <https://wandb.ai/lev-tear-tear-labs/exp14-block-kronecker/runs/20t5mto4>
- Batches 768, 896, and 1,024 OOMed in both repeats.
- Batch 640 passed both repeats: 163,840 tokens/step, 110,374 tokens/s median,
  100% steady-state utilization, 73.46/76.43 GiB allocated/reserved.
- Batch 640 was only about 1.3% faster than compiled batch 512. Compilation,
  not extra batch growth, supplied almost all of the speedup.

### Compiled 10M-token replay

- W&B: <https://wandb.ai/lev-tear-tear-labs/exp14-block-kronecker/runs/xq61bnjk>
- Configuration: default Inductor, batch 640, 163,840 tokens/step, AdamW
  constant LR `0.003`, 62 steps / 10,158,080 actual tokens per cell.

| Seed | compiled candidate | frozen tuned Transformer | candidate − control |
|---:|---:|---:|---:|
| 3 | 7.389621 | 7.411661 | -0.022040 |
| 4 | 7.389448 | 7.414110 | -0.024662 |
| 5 | 7.386462 | 7.411388 | -0.024926 |
| 6 | 7.387279 | 7.414455 | -0.027176 |

The compiled candidate again won all four seeds, with mean delta `-0.024701`.
It was `0.00119`–`0.00182` NLL better than its eager replay, consistent with
the slightly larger actual token budget. Compiled permuted-minus-unpermuted
deltas remained negligible (`-1.11e-4` to `+1.35e-4`).

Steady-state compiled training was about 110k tokens/s and 100% utilized, but
the 128-second one-time compile exceeded the roughly 92-second 10M-token train
phase. Consequently the whole training-context sampler reported 0% median
utilization: more than half its samples were compilation rather than training.
For short debugging cells, compiling the monolithic 32-layer graph is wasteful;
for longer confirmations it amortizes and is materially faster. A shared
layer/kernel compile is the next implementation-performance target.

### Launch audit — 2026-08-03

- Stored W&B credentials: verified successfully before allocation.
- Northflank allocation: not started.
- GPU-hours consumed by this attempt: zero.
- Reason: the six pre-existing CLI contexts were expired (`401`) or lacked
  access to `fda-race-us-central` (`404`).
- Remediation: a fresh browser login was started under the non-destructive
  context name `exp14-live`; the launch remains gated on its project scope.

### Failed preflight audit — 2026-08-03

- W&B: <https://wandb.ai/lev-tear-tear-labs/exp14-block-kronecker/runs/krsw9c35>
- The frozen corpus and all checksums were verified, but FLA rejected the
  non-contiguous block-shifted target view before training.
- No optimizer step ran. The service paused cleanly.
- Fix: make hidden states and shifted targets contiguous at the FLA boundary;
  a regression test now exercises the `.view()`-only contract.

### Infrastructure audit

The historical service had 4 TB ephemeral storage but no volume, so a pause
discarded the rebuilt environment and corpus. Northflank rejected its default
single-writer NVMe class for GPU workloads and this account does not expose the
required shared storage class. The launcher now downloads a git-ignored tar of
the checksum-verified corpus before pausing and restores it on later launches.
