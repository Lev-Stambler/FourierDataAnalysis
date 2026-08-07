# Exp18 run results

Status: complete. All 64 one-sample LR cells and all 32 two-sample
confirmation cells completed successfully on 8xH100. The campaign verdict is
`all_matched_architectures_memorize_two_blocks`.

- W&B: https://wandb.ai/lev-tear-tear-labs/exp18-memorization/runs/gn3ycswg
- cloud: eight NVIDIA H100 80GB HBM3 GPUs on Northflank;
- remote source digest: `3475122e3ada`;
- fixed WikiText block indices: `17` and `997`;
- models: eight approximately 5.4M-total-parameter tracks;
- success: exact teacher-forced token accuracy `1.0` and mean NLL at most
  `0.01`;
- screen: one fixed 256-token block, seed 1801, four AdamW and four joint Muon
  body/auxiliary LR recipes per model, no weight decay;
- confirmation: independently promoted best stable AdamW and Muon recipes for
  every model, two fixed blocks, fresh initialization seeds 1802 and 1803.

## Bottom line

There is a real interpolation-step advantage, but no unique rank-1 win. With
the best independently screened optimizer for each architecture:

- group rank 1, group rank 2, and deep group-Kronecker all memorize both blocks
  at both seeds in a mean of **6 optimizer steps**;
- current rank 8 and hybrid need 8 steps, router-free token needs 12, and the
  matched Transformer needs 13;
- rank 2 has the lowest recorded stopped-run NLL, `0.001935`, narrowly ahead of
  rank 4 at `0.001940`, while rank 1 records `0.003430`;
- all 16 promoted architecture/optimizer pairs pass at both seeds, so no model
  has a basic two-example expressivity or gradient-path failure;
- none reaches the separately logged strict `0.001` gate before early stopping.

The NLL values above are **not fixed-budget endpoints**. Successful cells stop
after confirming the `0.01` gate, so threshold steps—not final NLL—are the
primary tiny-data comparison. Likewise, this is interpolation on two repeated
examples, not validation loss or evidence of generalization.

## Paid preflight and compute audit

All eight tracks passed matched-parameter, exact compiled-BF16 versus
materialized-FP32 loss, finite forward/backward/optimizer, and full-node gates.
Every selected batch measured 100% median GPU utilization. Gradient
accumulation was 1. Seven tracks used 4,096 contexts and 1,048,576 physical
tokens per step; current rank 8 used the largest stable batch of 640 contexts
and 163,840 physical tokens per step.

| Track | Parameters | Physical contexts/step | Physical tokens/step | Throughput |
|---|---:|---:|---:|---:|
| current rank 8 | 5,400,896 | 640 | 163,840 | 109,839 tok/s |
| no-router token | 5,404,992 | 4,096 | 1,048,576 | 99,155 tok/s |
| group rank 1 | 5,400,928 | 4,096 | 1,048,576 | 90,139 tok/s |
| group rank 2 | 5,400,960 | 4,096 | 1,048,576 | 79,544 tok/s |
| group rank 4 | 5,401,024 | 4,096 | 1,048,576 | 76,026 tok/s |
| group hybrid | 5,400,928 | 4,096 | 1,048,576 | 95,777 tok/s |
| group deep | 5,399,232 | 4,096 | 1,048,576 | 58,731 tok/s |
| Transformer | 5,397,760 | 4,096 | 1,048,576 | 2,407,498 tok/s |

The maximum total-parameter mismatch from current rank 8 is 0.076%. The
Transformer is 26.7 times faster than rank 1 in the stable preflight. The full
eight-worker aggregate was 3,007,500 physical tok/s.

For the two-example confirmation, each optimizer step contains only two unique
contexts and 512 unique tokens. The physical batches repeat those two examples
2,048 times (320 times for current rank 8) solely to fill each GPU. Physical
throughput is a compute audit; it is not additional information or sample
efficiency.

## Complete one-sample LR screen

“Successes” counts recipes satisfying exact accuracy and NLL at most `0.01`.
“Hit step” is the first evaluation satisfying both gates. The selected recipe
shown is the fastest successful cell, with stopped-run NLL as the tie-breaker.

| Track | Successes | Fastest recipe | Hit step ↓ | Recorded final NLL |
|---|---:|---|---:|---:|
| group rank 1 | 7/8 | Muon `0.24/0.048` | **6** | **0.001281** |
| group rank 2 | 7/8 | AdamW `0.048` | **6** | 0.002452 |
| group deep | 7/8 | AdamW `0.048` | **6** | 0.001674 |
| current rank 8 | 5/8 | AdamW `0.012` | 8 | 0.003794 |
| group rank 4 | 7/8 | AdamW `0.048` | 8 | 0.001770 |
| group hybrid | 5/8 | AdamW `0.012` | 8 | 0.002496 |
| no-router token | 6/8 | Muon `0.06/0.012` | 11 | 0.003341 |
| Transformer | 4/8 | Muon `0.06/0.012` | 12 | 0.003354 |

The deliberately aggressive Muon `0.96/0.096` cell succeeds for rank 1 at
step 11 but fails for current rank 8. Rank 1 therefore has evidence of a wider
stable one-example LR region, not merely one lucky recipe.

## Two-sample, two-seed confirmation

Both promoted optimizer families were retained rather than choosing a shared
optimizer. Every row below represents two fresh seeds, and every row passes at
both seeds. “Steps” gives the two first-hit steps followed by their mean. Mean
final NLL is the average stopped-run NLL.

| Track | Optimizer and LR | Hit steps | Mean ↓ | Mean final NLL |
|---|---|---:|---:|---:|
| current rank 8 | **AdamW `0.012`** | 8 / 8 | **8** | 0.006740 |
| current rank 8 | Muon `0.06/0.012` | 12 / 12 | 12 | 0.004364 |
| no-router token | AdamW `0.012` | 28 / 26 | 27 | 0.005750 |
| no-router token | **Muon `0.06/0.012`** | 12 / 12 | **12** | 0.004446 |
| group rank 1 | **AdamW `0.048`** | 6 / 6 | **6** | 0.003430 |
| group rank 1 | Muon `0.24/0.048` | 8 / 6 | 7 | 0.002182 |
| group rank 2 | **AdamW `0.048`** | 6 / 6 | **6** | **0.001935** |
| group rank 2 | Muon `0.06/0.012` | 8 / 8 | 8 | 0.006694 |
| group rank 4 | **AdamW `0.048`** | 6 / 8 | **7** | 0.001940 |
| group rank 4 | Muon `0.06/0.012` | 8 / 8 | 8 | 0.005797 |
| group hybrid | **AdamW `0.012`** | 8 / 8 | **8** | 0.003888 |
| group hybrid | Muon `0.06/0.012` | 10 / 10 | 10 | 0.004859 |
| group deep | **AdamW `0.048`** | 6 / 6 | **6** | 0.002908 |
| group deep | Muon `0.24/0.048` | 6 / 8 | 7 | 0.005273 |
| Transformer | AdamW `0.003` | 22 / 22 | 22 | 0.005499 |
| Transformer | **Muon `0.06/0.012`** | 14 / 12 | **13** | 0.003870 |

The bold optimizer is the per-architecture winner under the predeclared
ordering: both-seed success, then threshold steps, then stopped-run NLL. AdamW
wins six architectures; Muon wins the router-free token model and Transformer.
This is independent optimizer selection, but it is selection on the one-block
interpolation screen. It is not an exhaustive two-block retune and cannot be
substituted for corpus-level tuning.

The raw result retains every curve, both per-example metrics, and all 16 causal
target-group NLL/accuracy traces. At the end of every successful confirmation
cell, both examples and every target group have token accuracy 1.0. There is no
hidden final-group or long-causal-distance accuracy failure.

## Is rank 1 best?

Rank 1 remains the preferred **next group default**, but the completed result
narrows the claim:

1. Rank 1 is in the fastest two-example tier at 6 steps, but rank 2 and the deep
   model tie it exactly.
2. Rank 2, not rank 1, has the lowest recorded two-example NLL. Because cells
   stop at the success gate, that is suggestive rather than a fixed-budget win.
3. In Exp17's replicated full-data tuning, rank 1 remains the best group model:
   `7.380991` versus rank 4 `7.381523` and rank 2 `7.382181`.
4. Rank 1 has the broad aggressive-LR success noted above and is simpler and
   faster than ranks 2/4: 90,139 tok/s versus 79,544 and 76,026 tok/s.

At matched total parameters, higher rank divides approximately the same
group-FFN budget into narrower paths: rank 1 uses one `64×250` path, rank 2 uses
two `64×121` paths, and rank 4 uses four `68×56` paths. All three memorize,
so higher rank is not needed for tiny-data expressivity. This result does not
show that rank 1 is universally superior; it says rank 1 is the simplest member
of a tied fastest tier and has the best existing full-data group evidence.

## Scientific interpretation and next gate

Exp18 rules out a basic inability of the group operator to fit the objective.
It also shows that the group nonlinearity reaches a two-example interpolation
gate in fewer optimizer steps than the matched Transformer. It does **not**
overturn Exp17: the tuned Transformer still wins replicated WikiText block NLL
decisively (`7.155114` versus `7.380991` for rank 1) and its present kernels are
26.7 times faster.

The productive hypothesis is now precise: group density has strong tiny-data
interpolation, but the current architecture/training does not convert that into
data-efficient generalization. The next experiment should keep rank 1, rank 2,
deep, and Transformer as controls and advance the same ladder through 8, 32,
and 128 **unique** examples. It must evaluate held-out examples at each rung,
use fixed unique-example exposure budgets, and keep architecture-specific
optimizer tuning. That is where memorization speed can be separated from useful
inductive bias before another full-corpus spend.

## Durable artifacts

- `cloud_state/result.json`: complete machine-readable campaign result and all
  curves;
- `cloud_state/preflight.json`: batch sweep, exact-loss, parameter-match,
  utilization, VRAM, power, and throughput audit;
- `cloud_state/remote.log`: immutable remote execution log;
- `cloud_state/lifecycle.json`: clean completion and service pause record.

The Northflank service was returned to zero instances after the campaign
completed.
