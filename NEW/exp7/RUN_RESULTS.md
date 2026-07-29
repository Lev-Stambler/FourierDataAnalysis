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

## H100 recovery study

The failed Central deployment was replaced by the reusable capacity racer,
which selected the first healthy eight-GPU Hopper node across all Northflank
regions and paused every losing candidate. The winner is eight H100 80 GB GPUs
in Asia Northeast. The exact dataset and source checkpoint were copied to its
persistent `/cache`; the source SHA-256 was rechecked before launch. W&B
authentication, all eight requested GPUs, seven unit tests, and the Exp V7
self-test passed before the paid run started.

- W&B:
  <https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/ol3rar01>
- Coordinator log:
  `/cache/expv7-dense-free/logs/coordinator-recovery.log`
- Physical contexts/GPU: `131,072`
- Optimizer contexts/GPU: `8,192`
- Global contexts/update: `65,536`
- Global input tokens/update: `1,048,576`
- Gradient accumulation: none

## AdamW8bit continuation

The NorMuon continuation was stopped by request after its checkpoint at
`1,073,741,824` long-run input tokens. Its best held-out KL was `2.17026095`.
The model and exact FineWeb-Edu stream state were retained, while the NorMuon
optimizer state was intentionally discarded.

Training restarted with bitsandbytes `AdamW8bit` using conventional
transformer AdamW settings: peak LR `3e-4`, betas `(0.9, 0.95)`, epsilon
`1e-8`, weight decay `0.1`, and token-based cosine decay to `3e-6`. A direct
H100 preflight verified finite BF16 parameter updates and confirmed that both
moment buffers are stored as `uint8`.

- Fresh AdamW8bit W&B:
  <https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/15974db0>
- Supervisor log:
  `/cache/expv7-dense-free/logs/long-1t-adamw8bit-supervisor.log`
- Preserved NorMuon checkpoint:
  `/cache/expv7-dense-free/long-1t/normuon-final-checkpoint.pt`
- First finite update: train KL `2.59271455`, gradient norm `0.01977539`
- At 5,242,880 contexts: train KL `2.51389980`, gradient norm `0.01446533`
- Peak allocated VRAM/GPU: `71.254 GiB`
- Peak reserved VRAM/GPU: `71.992 GiB`
- Observed device memory: `75,634 / 81,559 MiB` (`92.7%`) on every GPU
- Observed GPU utilization: `99–100%` on all eight GPUs
- Compiled training throughput: `1,617,775` input tokens/s
- Cumulative end-to-end throughput after 5,242,880 contexts:
  `330,571` input tokens/s, still amortizing the one-time compile

The recovery study first screens learning rates `0.003`, `0.006`, `0.0125`,
and `0.025` for 16,777,216 contexts each. It then resumes the lowest-validation
KL checkpoint for up to two hours or until the target KL of `1.0` is reached.

## FineWeb-Edu 1T-token run

The four H100 learning-rate screens completed as follows:

| Peak LR | Validation KL | End-to-end tok/s | W&B |
|---:|---:|---:|---|
| 0.003 | 2.58197210 | 728,968 | [ol3rar01](https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/ol3rar01) |
| 0.006 | 2.52063298 | 938,009 | [8n2ht5r5](https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/8n2ht5r5) |
| 0.0125 | 2.43912730 | 940,459 | [ymu0bghp](https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/ymu0bghp) |
| 0.025 | 2.34095590 | 947,670 | [xag2wast](https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/xag2wast) |

Training now streams the pinned FineWeb-Edu revision, keeps only score-3+
examples supplied by that dataset configuration, excludes the static
train/validation/test documents on the first epoch, and only repeats after
stream exhaustion. Each document contributes all non-overlapping 17-token
windows, with 16 input tokens per exact per-token KL target. Stream state and
pending contexts are saved in every checkpoint.

The paid streaming preflight processed 268,435,456 input tokens and ended at
validation KL `2.19282032`. Steady intervals reached `1.37–1.40M` input tok/s,
the producer queue stayed at depth 3, and all eight H100s sustained `99–100%`
utilization with `75,652 / 81,559 MiB` device memory. Peak allocated and
reserved memory were `71.254` and `72.012 GiB` per GPU. The first-batch
compile/data warm-up is included in its cumulative `668,617` tok/s result.

The production job resumes that validated stream/model state and targets
exactly `1,000,000,000,000` input tokens, including the preflight tokens. It
uses a user-selected peak LR of `0.01`, cosine-decayed by input-token progress
to `0.0001`, with no KL early stop and no wall-clock limit.

- Fresh production W&B:
  <https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/4290cc67>
- Supervisor log:
  `/cache/expv7-dense-free/logs/long-1t-supervisor.log`
- Checkpoint/result directory:
  `/cache/expv7-dense-free/long-1t`
- Global contexts/update: `65,536`
- Global input tokens/update: `1,048,576`
- Physical contexts/GPU: `131,072`
- Gradient accumulation: none
