# ExpV2-1 run results

## Verdict

`stop_at_synthetic_gate` with failure class `invalid_control`.

This is not evidence to abandon the Kronecker idea, and it is not evidence to
advance it to TinyStories. The matched Transformer also missed every absolute
algorithmic threshold by a large margin, so this particular gate cannot
separate an architecture failure from a task/training-budget failure.
TinyStories validation and sealed test data were never opened by the campaign.

- Final W&B run: <https://wandb.ai/lev-tear-tear-labs/expv2-1-kronecker-basics/runs/udnvaca1>
- Campaign result: `/home/lev/.cache/research-control/expv2-1-kronecker-basics-h100-v1/stages/h100-pilot/result.json`
- Paid preflight: `/home/lev/.cache/research-control/expv2-1-kronecker-basics-h100-v1/stages/h100-pilot/artifacts/paid-preflight.json`
- Synthetic gate: `/home/lev/.cache/research-control/expv2-1-kronecker-basics-h100-v1/stages/h100-pilot/artifacts/synthetic-gate.json`
- Controller audit: `/home/lev/.cache/research-control/expv2-1-kronecker-basics-h100-v1/audit.json`

## Matched setup and tuning

The four Kronecker shapes have 2,150,528 parameters, versus 2,146,048 for the
dense whole-state control and 2,154,496 for the Transformer. Maximum total
parameter mismatch is 0.394%. Every model used pure batched Muon; there was no
AdamW group and no gradient accumulation.

Every model/task pair was tuned independently over learning rates 0.003, 0.01,
0.03, and 0.1, with up to two geometric boundary extensions. Coarse cells saw
5,000,064 tokens. The selected recipe then saw 20,000,000 tokens on each of
five seeds. The tuning result was boundary-locked for every model/task pair.

| Model | Delay LR | Assoc. LR | Two-hop LR |
|---|---:|---:|---:|
| Kronecker r1/d66 | 0.10 | 0.10 | 0.03 |
| Kronecker r2/d33 | 0.30 | 0.10 | 0.10 |
| Kronecker r3/d22 | 0.30 | 0.10 | 0.10 |
| Kronecker r6/d11 | 0.30 | 0.10 | 0.10 |
| Dense whole-state | 0.30 | 0.10 | 0.10 |
| Transformer | 0.10 | 0.03 | 0.10 |

## Five-seed synthetic accuracy

Values are mean percentages. The required ID/OOD thresholds were 99/95 for
delay copy, 95/90 for associative recall, and 90/80 for two-hop recall.

| Model | Delay ID | Delay OOD | Assoc. ID | Assoc. OOD | Two-hop ID | Two-hop OOD |
|---|---:|---:|---:|---:|---:|---:|
| Kronecker r1/d66 | 5.65 | **3.67** | 24.89 | 6.39 | 25.05 | 8.04 |
| Kronecker r2/d33 | 6.59 | 2.70 | 24.91 | 6.84 | **25.14** | 6.59 |
| Kronecker r3/d22 | 7.35 | 2.65 | 24.96 | 6.86 | 25.12 | 6.52 |
| Kronecker r6/d11 | **8.72** | 2.18 | 24.90 | 6.99 | 24.93 | 6.52 |
| Dense whole-state | 1.80 | 1.09 | 20.76 | 6.53 | 22.90 | 7.97 |
| Transformer | 7.35 | 1.55 | **25.10** | **12.54** | 25.02 | **12.54** |

Kronecker r1/d66 and r2/d33 beat the dense control on mean OOD accuracy in all
five paired seeds; r3/d22 did so in four, and r6/d11 in three. That narrow
signal is real. It is not enough to pass: no Kronecker model met the absolute
thresholds or the predeclared closeness-to-Transformer rule. The Transformer
itself was also invalid as a positive control.

The pattern is informative: higher Kronecker rank/depth tradeoff improved
in-distribution delay copy, while the Transformer retained a large advantage
on OOD associative and two-hop recall. All models remained close to chance on
the hard generalization splits.

## H100 preflight and throughput

The paid preflight passed on exactly one NVIDIA H100 80GB HBM3. It swept down
from batch 8,192, selected the highest measured-throughput stable physical
batch, required at least 100,000 global tokens/step and 85% utilization, and
checked finite BF16 forward/backward/optimizer state plus FP32 agreement.

| Model | Contexts/step | Tokens/step | Tokens/s | Median util. | Peak allocated | vs Transformer |
|---|---:|---:|---:|---:|---:|---:|
| Kronecker r1/d66 | 2,048 | 262,144 | 1.49M | 100% | 39.38 GiB | 0.60x |
| Kronecker r2/d33 | 4,096 | 524,288 | 2.35M | 100% | 57.94 GiB | 0.94x |
| Kronecker r3/d22 | 4,096 | 524,288 | 2.84M | 99% | 51.03 GiB | 1.14x |
| Kronecker r6/d11 | 4,096 | 524,288 | 3.61M | 97% | 44.12 GiB | 1.45x |
| Dense whole-state | 8,192 | 1,048,576 | 6.22M | 99% | 61.10 GiB | 2.49x |
| Transformer | 4,096 | 524,288 | 2.50M | 99% | 54.32 GiB | 1.00x |

The ambitious batches delivered 3.11x--5.21x the batch-128 throughput. A 10x
gain was not available because even the underfilled baseline was already fast;
the selected batches were measured at 97--100% utilization. `torch.compile`
was not used because the bounded probe failed while building Triton's helper
against the container's Python headers; eager execution already saturated the
GPU.

## Correctness, data, and compute audit

- 24 ExpV2-1 tests plus 12 controller tests pass (`36 passed`).
- Maximum factored/materialized Kronecker forward/backward errors are
  `4.44e-16` and `7.11e-15`; causality errors are zero for all six models.
- Prepared TinyStories contains 150,000,000 training tokens, 1,780,608
  validation tokens, and 1,750,272 sealed test tokens with disjoint document
  IDs and pinned checksums. It was prepared but not opened for this failed gate.
- Controller audit is complete. Reconciled usage, including setup failures and
  service lifecycle gaps, is 3,001.23 H100-seconds (0.834 GPU-hours) and about
  $2.28, below the one-GPU-hour cap.
- The disposable `fda-test/expv2-1-h100` service has no running containers and
  was deleted after artifact and W&B flush; the shared cache volume is kept.

## What this run says to do next

Debug the gate before spending on language modeling. First make the matched
Transformer reliably solve progressively easier versions of these exact tasks,
then increase difficulty one axis at a time. Keep the tuned Kronecker r1/d66
and r6/d11 as diagnostic endpoints: r1 is the strongest mean OOD Kronecker,
while r6 is the fastest and best ID delay-copy model. Only reopen TinyStories
after the positive control passes and the comparison can falsify the candidate.
