# Experiment 8: scale-safe Kronecker student

## Motivation

Exp7 proved that FP32 master weights fixed the immediate flatline, but its
inherited Exp6 checkpoint exposed a deeper scale pathology. The dense tied
vocabulary had RMS `2.876`; block factor RMS values reached `12.6` for `A` and
`5.2` for `B`; and the residual stream grew from RMS `3.1` at the embedding to
`1,465` after block 32. Final per-token RMS normalization hid this internal
growth while producing logits with a reasonable standard deviation near
`3.0`.

The cause is structural. Each Kronecker term is bilinear, so multiplying `A`
by a constant and dividing `B` by the same constant leaves the represented
operator unchanged. Pre-normalization also bounds each branch's input without
bounding the accumulated residual stream.

## Canonical correction

Exp8 is a fresh-initialized fork and intentionally refuses Exp7 checkpoints.
It keeps the conventional unconstrained FP32 tied dense vocabulary because
its norm has a real output-logit-temperature role. For the scale-free body:

- every rank matrix is fan-in RMS-normalized during the forward pass;
- raw factors are projected back to the same canonical gauge after every
  optimizer step;
- projected factors receive no radial weight decay;
- every residual result is independently RMS-normalized per token;
- exact FP32 teacher probabilities, exact per-token KL, BF16 autocast, and
  full compilation remain unchanged.

## Implementation and correctness gate

The experiment remains one executable training file, `exp8.py`. The student
has 17,006,592 parameters: context length 16, width 64, depth 32, Kronecker
rank 8, and one standard tied `248320 x 64` embedding/unembedding matrix.
AdamW8bit uses betas `(0.9, 0.95)`; weight decay `0.1` applies only to the
functional vocabulary scale, while projected body factors use zero decay.

The local and paid gates both passed:

- 16 unit tests and the standalone self-test pass;
- all requested eight GPUs participate;
- forward, backward, AdamW8bit state, gradients, and updates are finite;
- exact teacher row-sum error is about `3.5e-7`;
- factor RMS remains `0.25` for `A` and `0.125` for `B`;
- deepest pre-normalization residual RMS stays near `1.06`, and post-residual
  per-token RMS error stays below `1.1e-4`;
- more than `99.99%` of both vocabulary and body weights change.

Paid preflight W&B:
[pxwe437n](https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/pxwe437n).

## Paid H100 batch study

Run on the sole surviving Northflank service, an 8x H100 80 GB node in US
Central. The sweep searched downward from ambitious configurations; it did
not fall back to the historical small batch.

| Physical contexts/GPU | Optimizer contexts/GPU | Global tokens/update | Result | Maximum measured interval |
| ---: | ---: | ---: | --- | ---: |
| 65,536 | 16,384 | 2,097,152 | OOM | — |
| 49,152 | 49,152 | 6,291,456 | OOM | — |
| 49,152 | 24,576 | 3,145,728 | stable | 3.218M tok/s |
| 49,152 | 16,384 | 2,097,152 | stable, selected | 4.103M tok/s |

The selected shape has 49,152 physical contexts/GPU, 393,216 global physical
contexts (`6,291,456` input tokens) per frozen-teacher pass, and three student
optimizer updates from that same teacher target buffer. Peak allocation was
`66.12 GiB` and peak reservation `66.31 GiB` on every GPU. Runtime allocation
was `69,812 / 81,559 MiB` per GPU, or 85.6% of device memory. Steady samples
showed most GPUs at 95–100% compute; phase-boundary samples could catch one
rank lower.

Batch W&B:
[65,536 OOM](https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/0t21buvn),
[selected](https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/gzpjck5s),
[whole-pass OOM](https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/k659jal2),
and [24,576 stable](https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/o1vyvcvs).

## Learning-rate study

Each arm used the identical fresh initialization, exact held-out set, and
`268,435,456` input tokens. The held-out metric—not a transient streamed
training value—selected the winner.

| Peak LR | Held-out KL | Accuracy | W&B |
| ---: | ---: | ---: | --- |
| `1e-4` | 7.728577 | 0.012% | [14atbxoj](https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/14atbxoj) |
| `3e-4` | 5.910610 | 0.024% | [nzroxu73](https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/nzroxu73) |
| `1e-3` | 4.086589 | 5.884% | [958gfd9i](https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/958gfd9i) |
| `3e-3` | **3.762220** | **7.227%** | [dmy4s7qa](https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/dmy4s7qa) |

The `3e-3` checkpoint then passed the required monotonic confirmation:
held-out KL `3.762220 -> 2.819427`. A resume-counter bug in the running study
made that already-launched confirmation execute one additional interval,
ending at KL `2.510833`, 15.405% accuracy, and 805,306,368 total screen input
tokens. The committed code restores `run_contexts` from the checkpoint, with
an explanatory comment, so future two-interval confirmations run exactly one
additional interval.

## 1T-token continuation

The exact 1,000,000,000,000-token continuation is live from the best `3e-3`
checkpoint with preserved AdamW8bit state. Its LR follows a token-based cosine
from `0.003` to `0.00003`; it retains the selected 2,097,152-token optimizer
batch and exact per-token KL.

Direct W&B:
[22995f91](https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/22995f91).

The launch reproduced held-out KL `2.5108328400` exactly. Its first durable
checkpoint at `268,435,456` long-run tokens improved held-out KL to `2.298864`
and accuracy to 16.467%. The corresponding interval had streamed KL `2.3239`,
finite grad norm `0.2071` with no clipping, and a prefetch queue depth of 3;
nearby intervals reached `1.82M tok/s`. The one-time FineWeb-Edu shuffle and
tokenization startup caused the initial idle period; accumulated data wait
then remained constant while the asynchronous queue stayed populated. Both
`checkpoint.pt` and the improved `best.pt` were written before training
continued.
