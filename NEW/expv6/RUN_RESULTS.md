# Exp V6 long-run result

## Outcome

The `expv6-kiss-long-production` continuation was stopped manually on
2026-07-29 after exact validation KL plateaued at the minimum configured
learning rate. The target KL of 1.0 was not reached. The Central Northflank
8xH200 service was deliberately left running and idle for follow-up work.

- W&B: <https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/pm2ychas>
- Initial exact validation KL: `3.2954538`
- Best exact validation KL: `2.7668272` at update `7,851`
- Latest exact validation KL: `2.7710873` at update `9,387`
- Final logged update: `9,499`
- Final logged training KL: `2.6186851`
- Total contexts seen: `678,559,744`
- Total input tokens seen: `10,856,955,904`
- Approximate wall time: 1 hour 37 minutes
- Stop reason: five-plus validation checks without a new best at minimum LR

The final logged update occurred after the most recent checkpoint. Update
`9,387` is the latest durable state; update `7,851` is the best durable state.

## Model and objective

The student has 1,177,920 BF16 parameters:

- context length `16`, width `64`, depth `32`, Kronecker rank `8`;
- tied multiplicative vocabulary factors `[485, 64]` and `[512, 64]`,
  covering all 248,320 teacher vocabulary entries;
- 32 pre-normalized residual blocks, each containing eight
  `[16, 16]` position matrices and eight `[64, 64]` channel matrices;
- parameter-free RMS normalization independently over each token's channels;
- SiLU before each Kronecker transform and residual scaling by `1/sqrt(32)`;
- final per-token RMS normalization before the last-token vocabulary head.

The frozen teacher was pinned `Qwen/Qwen3.5-0.8B-Base`. Training minimized
full-vocabulary soft cross-entropy and reported literal per-next-token
`KL(P_teacher || P_student)` by subtracting teacher entropy. Exact validation
used 8,192 fixed examples and FP32 full-vocabulary probabilities.

## Training configuration

- Optimizer: NorMuon, weight decay `0.01`, global gradient clipping at `1.0`
- LR progression: `0.2 -> 0.1 -> 0.05 -> 0.025`
- Minimum LR: `0.025`
- Optimizer batch: 65,536 global contexts / 1,048,576 global input tokens
- Physical teacher buffer: 245,760 contexts per GPU
- Physical global buffer: 1,966,080 contexts / 31,457,280 input tokens
- Teacher microbatch: 2,048 contexts per GPU
- Eight-way data parallelism with no gradient accumulation
- BF16 teacher/student fast paths and `torch.compile`

This launch predates the `grad_norm` metric added to Exp V6, so gradient norms
are not available retrospectively. Future launches log the pre-clipping global
L2 norm.

## Validation trajectory

| LR | First exact KL | Best exact KL | Last exact KL |
|---:|---:|---:|---:|
| `0.2` | `3.0675378` | `2.8522169` | `2.8522169` |
| `0.1` | `2.7946428` | `2.7826765` | `2.7890915` |
| `0.05` | `2.7735938` | `2.7717112` | `2.7734724` |
| `0.025` | `2.7683422` | `2.7668272` | `2.7710873` |

Each LR reduction produced an immediate improvement, followed by progressively
smaller gains. At LR 0.025, the best occurred at update 7,851; the subsequent
exact KL values were `2.7713051`, `2.7706355`, `2.7717504`, `2.7711055`,
`2.7753693`, and `2.7710873`. Continuing the same configuration was therefore
not cost-effective.

## Hardware and throughput

- Node: Northflank US Central, 8x NVIDIA H200 141 GB
- Sustained GPU utilization while training: 99-100% on every GPU
- Peak allocated VRAM: `130.034 GiB` per GPU
- Peak reserved VRAM: `130.227 GiB` per GPU
- Observed resident VRAM while training: approximately 94% per GPU
- Final measured throughput: `1,686,145` global input tokens/second

The large physical teacher buffer kept allocation stable and amortized one
frozen-teacher pass across 30 student optimizer updates.

## Durable artifacts

- Latest: `/cache/expv6-kiss-long/checkpoint.pt`
  - update `9,387`
  - 4,819,259 bytes
  - SHA256 `4bfaa722fe8cdb8334e0a3623c8107379d483cde1dad4c5a7ac8f516c16a2a5c`
- Best: `/cache/expv6-kiss-long/best.pt`
  - update `7,851`
  - exact validation KL `2.7668272`
  - 4,817,459 bytes
  - SHA256 `97b3bd8960fa823949cfd87fa621b205c08bf81e997363d9d649561bb0e414ab`
- Console log: `/cache/expv6-kiss-long/train.log`
- Source checkpoint artifact:
  `lev-tear-tear-labs/qwen-causal-kron-distill/expv6-kiss-production-kt69ytky:v0`

## Conclusions

The hardware/data path is no longer the bottleneck: utilization exceeded the
85% requirement and throughput substantially exceeded the 400k token/second
target. The remaining limitation is optimization and/or model capacity.

- Repeating the same width-64, depth-32, rank-8 run is unlikely to reach KL 1.
- A checkpoint-fork LR screen below 0.025 can test whether NorMuon is still
  stepping too aggressively.
- Parameter-matched `depth=64, rank=8` and `depth=32, rank=16` runs can isolate
  nonlinear depth from Kronecker operator rank.
- Increasing width is the only one of these knobs that also expands the tied
  vocabulary head, but its body compute scales approximately quadratically.
