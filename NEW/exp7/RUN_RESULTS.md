# Experiment 7: dense-free tied vocabulary

Exp V7 replaces Exp V6's multiplicative `485 × 512` vocabulary factors with
one unconstrained tied BF16 matrix of shape `248320 × 64`. Every token owns a
different trainable row. The width-64, depth-32, rank-8 residual body and
per-token RMS normalization are unchanged.

The model has exactly 17,006,592 trainable parameters. It is initialized by
materializing the best Exp V6 vocabulary row `i*512+j` as `A[i] * B[j]` and
copying the body, so its pre-update function should match Exp V6 while all
vocabulary rows become independent after the first optimizer step.

Run URLs, batch sweep measurements, and training results will be appended after
the paid preflight and launch.

## Preflight

- Exp V6 source: `/cache/expv6-kiss-long/best.pt`
- Source SHA-256:
  `97b3bd8960fa823949cfd87fa621b205c08bf81e997363d9d649561bb0e414ab`
- Converted validation KL: `2.76665156`
- Recorded Exp V6 validation KL: `2.76682720`
- Absolute conversion difference: `0.00017564`
- Conversion W&B:
  <https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/4sqept3p>

The first paid backward preflight identified that an `inference_mode` teacher
target cannot be saved by compiled student autograd. No optimizer update
occurred. The target generation scope was changed to `no_grad`, leaving the
teacher frozen while producing an ordinary tensor for student backward.

The corrected 8,192-context/GPU optimizer-batch benchmark completed 30 finite
updates:

| Metric | Result |
|---|---:|
| Global contexts/update | 65,536 |
| Global input tokens/update | 1,048,576 |
| Physical contexts/GPU | 245,760 |
| Peak allocated VRAM/GPU | 124.315 GiB |
| Peak reserved VRAM/GPU | 125.039 GiB |
| Steady compiled student throughput | about 25M tok/s |
| End-to-end throughput including teacher targets and evaluation | 238,889 tok/s |
| Final validation KL | 2.72240360 |
| Pre-clip gradient norm | finite; about 0.016–0.020 |

Corrected benchmark W&B:
<https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/ux1jghg3>

## Active study

The detached batch/LR study was launched on the existing eight-H200 Central
service. It runs the remaining 12,288 and 16,384 context/GPU batch benchmarks,
selects the highest end-to-end-throughput stable batch, screens all four
learning rates, and continues the best checkpoint for two hours. The service
is deliberately left running after completion.

- Coordinator log: `/cache/expv7-dense-free/logs/coordinator.log`
- First active sweep run:
  <https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/4gvvm5x4>
- Final machine-readable study artifact:
  `/cache/expv7-dense-free/study.json`
