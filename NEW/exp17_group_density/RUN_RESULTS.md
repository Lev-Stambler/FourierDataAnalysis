# Exp17 run results

Status: complete. Verdict: `stop_group_density_no_mechanistic_win`.

- W&B: https://wandb.ai/lev-tear-tear-labs/exp17-group-density/runs/zdmkehl2
- cloud: Northflank `fda-race-us-central/gpu-h200-8`, actually allocated as
  eight NVIDIA H100 80GB HBM3 GPUs;
- execution source: `ac699e1`, digest `fd1d9effcdff`;
- objective: 256-token, 16-token-shifted block WikiText NLL;
- matching: total parameters within 0.1% of 5,400,896;
- training: compiled BF16 exact fused loss, one physical batch, no gradient
  accumulation, independently tuned AdamW and Muon recipes for every track.

The raw result, preflight, lifecycle, and complete remote log are retained in
[`cloud_state/retry1`](cloud_state/retry1).

## Accelerator preflight

All nine tracks passed finite forward/backward/optimizer checks and BF16 fused
versus materialized FP32 loss agreement. Relative loss errors ranged from
`0.00000616` to `0.00005666`. The selected batches were:

| Track | Contexts/step | Tokens/step | Throughput | GPU util. | Peak allocated |
|---|---:|---:|---:|---:|---:|
| current rank 8 | 640 | 163,840 | 109,905 tok/s | 100% | 73.9 GiB |
| no-router token | 4,096 | 1,048,576 | 99,771 tok/s | 100% | 31.9 GiB |
| dense group | 4,096 | 1,048,576 | 100,683 tok/s | 100% | 31.9 GiB |
| group rank 1 | 4,096 | 1,048,576 | 90,536 tok/s | 100% | 41.1 GiB |
| group rank 2 | 4,096 | 1,048,576 | 79,298 tok/s | 100% | 43.1 GiB |
| group rank 4 | 4,096 | 1,048,576 | 76,069 tok/s | 100% | 47.3 GiB |
| group hybrid | 4,096 | 1,048,576 | 96,142 tok/s | 100% | 36.1 GiB |
| group deep | 4,096 | 1,048,576 | 58,734 tok/s | 100% | 39.8 GiB |
| Transformer | 4,096 | 1,048,576 | 2,400,957 tok/s | 99.5% | 36.4 GiB |

Measured eight-worker cell scaling efficiency was `0.9981`. The experiment is
parameter matched, not compute matched; the Transformer is also roughly 25–40
times faster than the group models in this implementation.

## Coarse independent optimizer screen

These are single-seed minima at two training tokens per parameter. Every row
has its own AdamW and joint Muon body/auxiliary LR search.

| Track | Best NLL ↓ | Optimizer | Body LR | Auxiliary LR |
|---|---:|---|---:|---:|
| Transformer | **7.370125** | Muon | 0.03 | 0.006 |
| current rank 8 | 7.384280 | AdamW | 0.00075 | 0.00075 |
| group deep | 7.422289 | AdamW | 0.012 | 0.012 |
| no-router token | 7.433064 | AdamW | 0.006 | 0.006 |
| group rank 1 | 7.436661 | Muon | 0.06 | 0.012 |
| group hybrid | 7.437781 | Muon | 0.12 | 0.012 |
| group rank 2 | 7.442674 | AdamW | 0.012 | 0.012 |
| group rank 4 | 7.443547 | Muon | 0.06 | 0.012 |
| dense group | 7.536244 | AdamW | 0.012 | 0.012 |

At a fixed parameter budget, increasing group rank did not help. Higher rank
split the nonlinear budget into narrower paths: rank 1 was `64×250×1`, rank 2
was `64×121×2`, and rank 4 was `68×56×4`.

## Replicated schedule/seed result

The two best recipes per optimizer family crossed constant and warmup-cosine
schedules on seeds 35 and 36 at five training tokens per parameter. The table
reports the winning two-seed recipe mean for each architecture.

| Track | Robust NLL ↓ | Delta vs current | Delta vs Transformer |
|---|---:|---:|---:|
| Transformer | **7.155114** | -0.221845 | — |
| current rank 8 | 7.376960 | — | +0.221845 |
| group rank 1 | 7.380991 | +0.004031 | +0.225876 |
| group hybrid | 7.381334 | +0.004374 | +0.226220 |
| group rank 4 | 7.381523 | +0.004563 | +0.226408 |
| group rank 2 | 7.382181 | +0.005221 | +0.227067 |
| no-router token | 7.382747 | +0.005787 | +0.227633 |
| group deep | 7.383408 | +0.006448 | +0.228294 |
| dense group | 7.408296 | +0.031336 | +0.253182 |

The selected recipes were Muon `0.03/0.012`, constant schedule for the
Transformer; AdamW `0.00075`, warmup-cosine for current rank 8; and Muon
`0.06/0.012`, warmup-cosine for group rank 1.

## Gate interpretation

Rank 1 was the best group candidate, but it was `+0.004031` NLL worse than the
strongest KronMix baseline. Advancement required at least a `-0.02` win. Its
group nonlinear branch was real rather than dead: disabling it worsened NLL by
`0.542544`, comfortably passing the `0.01` causal-ablation threshold. Thus the
branch contributes strongly to the model it learned, but that model is not a
better solution.

The mechanistic gate failed, so no four-seed 20-token-per-parameter final or
length/parameter scaling run was launched. More activation sites, depth, or
rank did not create a win under the current factorization. Exp18 therefore
moves to one/two-example memorization to separate basic interpolation and
optimization from generalization before another corpus-scale architecture
claim.

## Earlier failed preflight

The first W&B run, https://wandb.ai/lev-tear-tear-labs/exp17-group-density/runs/yafo20sa,
stopped before training because a static leading-batch compile guard exhausted
TorchDynamo's recompile limit. It is an infrastructure audit, not a model
result. Symbolic leading-batch compilation corrected it for the completed run.
