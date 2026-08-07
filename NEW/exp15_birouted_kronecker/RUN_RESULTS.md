# Exp15 run results

Status: **complete — mechanistic screen stopped before tuning**.

Primary W&B: https://wandb.ai/lev-tear-tear-labs/exp15-birouted-kronecker/runs/x9s7y8hf

## Result

The eight-way 10M-token screen found no material successor to the Exp14
destination-routed operator. `bi-decoupled-r8` ranked first, but improved over
`current-r8` by only `0.000464` block NLL, far short of the predeclared `0.02`
screen threshold. It beat `ffn-only` by `0.013580`, also short of that threshold.
The evidence gate therefore stopped the campaign before LR/optimizer tuning or
the four-seed 40M-token confirmation. These are deliberately **untuned screen
results**, not a tuned architecture verdict.

| Variant | Actual tokens | Batch | Tokens/step | Validation block NLL | Successor − current |
|---|---:|---:|---:|---:|---:|
| `bi-decoupled-r8` | 10,092,544 | 512 | 131,072 | **7.391032** | **−0.000464** |
| `dense-workspace-r8` | 10,158,080 | 640 | 163,840 | 7.391469 | −0.000027 |
| `current-r8` | 10,158,080 | 640 | 163,840 | 7.391496 | 0 |
| `decoupled-r8` | 10,092,544 | 512 | 131,072 | 7.391685 | +0.000189 |
| `bi-r8` | 10,092,544 | 512 | 131,072 | 7.391894 | +0.000398 |
| `source-r8` | 10,092,544 | 512 | 131,072 | 7.392108 | +0.000612 |
| `bi-r12` | 10,035,200 | 400 | 102,400 | 7.392476 | +0.000980 |
| `ffn-only` | 10,485,760 | 2,048 | 524,288 | 7.404612 | +0.013116 |

The key mechanistic reading is narrow but useful:

- Sender-side routing, sender+receiver routing, extra rank, rank decoupling,
  and a dense local workspace all failed to move loss materially at this
  budget under the locked AdamW recipe.
- Removing structured mixing was worse by `0.013116`, so the mixer has a small
  positive effect; the result is not evidence that token mixing is useless.
- The dense workspace is operationally attractive—about `2.1x` the baseline
  mixer's throughput with near-identical loss—but it is not a quality win.
- No optimizer-independent claim is justified because the stop rule correctly
  prevented the AdamW/Muon/hybrid grid after the architecture signal was too
  small.

## Accepted preflight

The accepted run used compiled BF16 hidden graphs and exact fused linear cross
entropy. Loss agreement passed all eight variants. Every selected benchmark
measured 99–100% GPU utilization, used no gradient accumulation, and had at
least 102,400 physical tokens/step. Eight-worker scaling efficiency was
`0.9832` at 875,010 aggregate tokens/s.

| Variant | Batch | Tokens/step | Tokens/s | GPU util | Peak allocated |
|---|---:|---:|---:|---:|---:|
| `current-r8` | 640 | 163,840 | 111,246 | 100% | 73.92 GiB |
| `ffn-only` | 2,048 | 524,288 | 2,620,058 | 99% | 47.24 GiB |
| `source-r8` | 512 | 131,072 | 109,149 | 100% | 66.36 GiB |
| `bi-r8` | 512 | 131,072 | 107,349 | 100% | 66.42 GiB |
| `decoupled-r8` | 512 | 131,072 | 100,869 | 100% | 66.36 GiB |
| `bi-decoupled-r8` | 512 | 131,072 | 99,863 | 100% | 67.42 GiB |
| `dense-workspace-r8` | 640 | 163,840 | 237,336 | 100% | 63.92 GiB |
| `bi-r12` | 400 | 102,400 | 88,923 | 100% | 71.15 GiB |

The retrieved machine-readable artifact is
[`cloud_state/result.json`](cloud_state/result.json); the service lifecycle and
all stopped-preflight audits live beside it. Northflank was paused after the
result and a validated 241 MiB corpus cache snapshot was recovered.

An initial preflight run was intentionally stopped after it exposed a harness
bug: `ffn-only` was stable at batch 768 and 2.21M tokens/s but only 61% GPU
utilization, and the upward batch search wrongly required the final 85%
utilization threshold before advancing. That inverted the search direction for
an underfilled model. No training cell ran. The corrected search advances any
finite stable row upward and applies the utilization threshold only when
selecting the final batch.

- Stopped preflight W&B: https://wandb.ai/lev-tear-tear-labs/exp15-birouted-kronecker/runs/n8tmsk8d
- Eight H100s, exact-loss checks passed for all eight models.
- At batch 768, `dense-workspace-r8` was stable at 238,314 tokens/s and 100%
  utilization; `ffn-only` was stable but underfilled; the other six variants
  OOMed and correctly require downward search.

A second preflight was stopped before training when `bi-r16` remained OOM at
batch 400, the campaign's 102,400-token physical-batch floor. The other routed
variants were stable at batch 512 with 100% utilization. Rather than lower the
token batch or add avoidable accumulation, the capacity diagnostic was changed
to `bi-r12`. This retains a 50% rank increase over the baseline while respecting
the paid-run batch contract.

- Stopped capacity preflight W&B: https://wandb.ai/lev-tear-tear-labs/exp15-birouted-kronecker/runs/yt0bp1cr
- `current-r8`, batch 640: 110,903 tokens/s, 100% utilization, 73.46 GiB.
- `dense-workspace-r8`, batch 640: 239,559 tokens/s, 100% utilization,
  63.46 GiB.
- `bi-r8`, batch 512: 107,965 tokens/s, 100% utilization, 65.96 GiB.
- `source-r8`, batch 512: 109,540 tokens/s, 100% utilization, 65.89 GiB.
- `decoupled-r8`, batch 512: 100,132 tokens/s, 100% utilization, 65.89 GiB.
- `bi-decoupled-r8`, batch 512: 100,310 tokens/s, 100% utilization,
  66.96 GiB.

A third preflight completed the rank-12 and upward batch curves, then exposed
an input-pipeline bottleneck in the extremely fast FFN-only control. It reached
2.61M tokens/s at batch 2,048, but short compute bursts separated by NumPy
memmap gather and host-to-device copies produced only 48% sampled utilization.
Batch 3,072 regressed to 0.71M tokens/s and batch 4,096 OOMed, proving that a
larger batch was not the remedy. No training cell ran. The corrected runner
caches the immutable int32 corpus on each GPU and performs indexed gathers
there; it revalidates only selected batches and cites the complete prior sweep.

- Stopped input-pipeline preflight W&B: https://wandb.ai/lev-tear-tear-labs/exp15-birouted-kronecker/runs/21a4s6tm
- `bi-r12`, batch 400: 89,091 tokens/s, 100% utilization, 70.69 GiB.
- `ffn-only`, batch 2,048: 2,608,294 tokens/s, 48% sampled utilization,
  46.78 GiB.

The first GPU-resident-input launch stopped during bootstrap, before W&B run
creation, because an interrupted prior cache download had replaced the local
corpus snapshot with a truncated tar. The 27 MiB partial snapshot was moved to
`/tmp/exp10-corrupt-20260804.tar`; the runner now downloads cache snapshots to
a candidate path, validates the full tar, and only then atomically replaces the
known-good local cache. Corpus regeneration remains checksum-gated on cloud.
