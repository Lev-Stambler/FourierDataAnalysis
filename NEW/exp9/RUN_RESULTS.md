# Experiment 9: standard Muon continuation

Exp9 keeps Exp8's 17,006,592-parameter student, frozen teacher, exact
full-vocabulary per-token KL, scale-safe residuals, and tied dense
`248320 × 64` vocabulary. It changes only the optimizer split:

- canonical Muon independently updates every rank-local `16 × 16` and
  `64 × 64` Kronecker matrix;
- AdamW8bit continues to update the tied embedding/unembedding matrix;
- Exp8 vocabulary moments are preserved, while body Adam state is discarded
  and Muon momentum starts fresh.

The body uses tuned Muon LR `0.002`, momentum `0.95`, Nesterov momentum, five
Newton–Schulz iterations, and no factor weight decay. The vocabulary uses LR
`3e-4`, betas `(0.9, 0.95)`, epsilon `1e-8`, and weight decay `0.1`.
Muon warms over 268,435,456 input tokens, then both learning rates follow
token-based cosine schedules over the remaining cumulative 1T-token budget.

## Preflight and production

Launched 2026-07-30 on the sole surviving Northflank service
`fda-race-asia-northeast/gpu-h100-8`, with eight H100 80GB GPUs. The source is
the Exp8 checkpoint at 65,500,348,416 long-run input tokens:

- source SHA-256:
  `7870748ba0bce9a5d7d6226b6e2dcbd0c364126522162367f683977935ab80fa`;
- initial held-out KL: `1.3437708285` (source best: `1.3432278435`);
- physical local batch: `49,152` contexts/GPU;
- optimizer local batch: `24,576` contexts/GPU, with two updates per teacher
  pass and no cross-pass accumulation;
- global optimizer batch: `196,608` contexts = `3,145,728` input tokens;
- peak PyTorch allocation/reservation: `75.49/75.80 GiB` on every GPU
  (`94.8%/95.2%` of the 79.65-GiB device capacity);
- live `nvidia-smi` sample: `99–100%` utilization and
  `79,534/81,559 MiB` used on all eight GPUs;
- stable measured intervals: approximately `1.15–1.67M` input tokens/second
  in the production stream, with a `2.66M` peak in the fixed-data preflight;
- preflight W&B:
  <https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/8kf7c8b9>;
- production W&B:
  <https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/a411e888>.

The direct `0.02` preflight produced a `1.77%` body-relative update and a sharp
KL regression. Reducing Muon tenfold produced a `0.177%` body-relative update;
production additionally warms from `2.34375e-5` over 268,435,456 tokens. At
session update 21, optimizer state was finite, gradients were not clipped,
the producer queue was full at depth three, train KL remained near `1.35`,
and Muon LR had reached `4.921875e-4`.

Two resume-only accounting defects found by paid preflight were fixed and
covered before launch: the three-batch benchmark now adds work relative to the
restored context counter, and throughput intervals start from the restored
session counter instead of zero.

## 2026-07-30 random-projection diagnostic

The production run was gracefully checkpointed and stopped at
`83,726,696,448` long-run input tokens and optimizer update `37,411`. Its best
validation KL was `1.333878764`; final validation KL was `1.334100747`. The
immutable diagnostic source is
`/cache/exp9-random-projection-debug-v1/source/production-82b.pt`, SHA-256
`d74ebc008d3e4f92f8a34d7e623d99a31219028204f21626b701077cc84acdf6`.
The sole eight-H100 Northflank node was retained, but all training processes
were stopped after the diagnostic gate failed.
The compiled-BF16 numerical preflight passed with absolute loss error
`0.012223`, gradient cosine `0.997994`, and finite optimizer state:
[W&B](https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/mf2bkuva).

The fresh initializer used a seeded Rademacher JL map
`R ∈ {-1,+1}^{1024×64}/sqrt(64)`, copied Qwen's tied vocabulary as
`E_student = E_Qwen R`, and matched the student's final hidden state to
`RMSNorm(h_Qwen R)` before exact-KL training. The random-projected teacher
itself was a poor 248k-way softmax approximation:

| Projection seed | Oracle validation KL |
|---:|---:|
| 0 | 7.832379 |
| 1 | **7.801428** |
| 2 | 8.157643 |
| 3 | 8.397881 |

Seed 1 was used for the hidden warm-start. All four cells saw 25,165,824
fixed token presentations with the vocabulary frozen:

| Optimizer contexts | Muon LR | Held-out hidden MSE | Hidden cosine |
|---:|---:|---:|---:|
| 24,576 | 0.002 | 0.804685 | 0.597658 |
| 8,192 | 0.002 | 0.660965 | 0.669517 |
| 24,576 | 0.004 | 0.726872 | 0.636564 |
| 8,192 | 0.004 | **0.613843** | **0.693079** |

The winning hidden state was cloned into four fresh exact-KL cells. Each cell
then saw 100,663,296 fixed-buffer token presentations; optimizers were reset,
the tied vocabulary was unfrozen, and the control cells reloaded the immutable
production checkpoint.

| Initialization | Optimizer contexts | Muon LR | Vocabulary LR | Best train KL | Final validation KL |
|---|---:|---:|---:|---:|---:|
| projected fresh | 24,576 | 0.002 | 3e-4 | 3.125394 | 3.549179 |
| projected fresh | 8,192 | 0.002 | 3e-4 | 1.711739 | 3.536515 |
| projected fresh | 24,576 | 0.004 | 3e-4 | 2.966154 | 3.517097 |
| projected fresh | 24,576 | 0.002 | 1e-3 | **1.656380** | **3.469671** |
| trained control | 24,576 | 0.002 | 3e-4 | 0.596699 | **1.747373** |
| trained control | 8,192 | 0.002 | 3e-4 | **0.457643** | 1.970253 |
| trained control | 24,576 | 0.004 | 3e-4 | 0.583184 | 1.816883 |
| trained control | 24,576 | 0.002 | 1e-3 | 0.474604 | 1.907639 |

The control proves that the exact-KL model/optimizer path can drive the fixed
buffer well below one quickly. The projected initialization cannot: its best
cell stopped at `1.656380`, so the predeclared `<1` gate prevented the
536,870,912-token fresh-data stage. Random projection preserves inner products
only in expectation; at width 64, its logit variance is too damaging for a
248,320-way softmax to serve as a useful direct output initialization.

High-batch exact-KL cells peaked at `75.48/75.82 GiB`
allocated/reserved (about 95% of H100 memory) and approximately `2.15M`
cached-target token presentations/second. The 8,192-context diagnostic cells
peaked near `56.9 GiB` reserved and approximately `1.59M` token
presentations/second. W&B system sampling includes compilation, validation,
and deep CPU-side diagnostics; GPU peaks reached `99%`, but the short
diagnostic did not establish sustained `>=85%` utilization and must not be
used as a production-utilization claim.

Persistent W&B arms:
[P0](https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/22dc3517),
[P1](https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/df2eb9ff),
[P2](https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/f3996d89),
[P3](https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/824c6272),
[C0](https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/e05c5985),
[C1](https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/c3250af1),
[C2](https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/e99a6d3a),
and
[C3](https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/b33b0f58).

## 2026-07-31 fresh validation sweep

Eight one-GPU arms resumed the immutable `83.7B`-token checkpoint on distinct
post-checkpoint FineWeb-Edu stream shards. Each arm consumed `176,160,768`
fresh input tokens in 448 updates with `24,576` contexts =
`393,216` input tokens per optimizer update. Initial held-out KL was
`1.334100747`.

The first launch exposed and canonically fixed a fresh-stream lifetime bug:
Python retained the previous `49,152 × 248,320` FP32 teacher-probability
tensor while allocating the next one. Explicitly releasing each completed
teacher batch before refill prevents the transient two-target OOM. No
optimizer state from that failed launch was reused.

| Arm | Muon LR | Vocabulary LR | Final validation KL |
|---|---:|---:|---:|
| fresh-0 | 1e-4 | 3e-5 | **1.323781** |
| fresh-1 | 2e-4 | 3e-5 | 1.324475 |
| fresh-2 | 4e-4 | 3e-5 | 1.328334 |
| fresh-3 | 8e-4 | 3e-5 | 1.336523 |
| fresh-4 | 2e-4 | 1e-4 | 1.325658 |
| fresh-5 | 4e-4 | 1e-4 | 1.329143 |
| fresh-6 (body only) | 2e-4 | 0 | 1.326274 |
| fresh-7 (vocabulary only) | 0 | 3e-4 | 1.330887 |

The winning joint low-rate update improved held-out KL by `0.010320`.
Body-only and vocabulary-only controls both underperformed it, while the
largest Muon rate regressed. LR tuning therefore recovers some progress but
does not explain the full `~1.33` plateau.

Every arm peaked at `75.42/75.82 GiB` allocated/reserved, approximately
95% of each H100. Stable compiled intervals reached approximately
`2.5–2.83M` input tokens/second per arm; one-second utilization samples were
usually `99–100%` during compute and dropped asynchronously during teacher
refill and diagnostics.

W&B:
[fresh-0](https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/37101c13),
[fresh-1](https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/d8ca665d),
[fresh-2](https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/aefac8ba),
[fresh-3](https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/ec565f8d),
[fresh-4](https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/8142b621),
[fresh-5](https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/97c2a661),
[fresh-6](https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/b79c5ea8),
and
[fresh-7](https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/8bf3e57d).

### Promoted eight-GPU continuation

The winning rates were promoted from the immutable production checkpoint into
a full eight-way DDP continuation:

- W&B:
  <https://wandb.ai/lev-tear-tear-labs/qwen-causal-kron-distill/runs/a30921cf>
- output: `/cache/exp9-standard-muon/long-1t-lr-1e-4-v2`
- Muon LR: `1e-4`, cosine floor `1e-5`
- AdamW8bit vocabulary LR: `3e-5`, cosine floor `3e-6`
- fresh optimizer on initial promotion; optimizer and all eight stream states
  preserved on supervised restarts
- local physical/optimizer batch: `49,152/24,576` contexts
- global optimizer batch: `196,608` contexts =
  `3,145,728` input tokens
- per-GPU peak allocated/reserved: `75.49/75.78 GiB`
- stable measured throughput: approximately `1.1–1.72M` global input
  tokens/second

Early held-out validation is monotonic:
`1.334100747 → 1.327680901 → 1.324427568`. At the second validation,
the durable checkpoint contained `84,022,394,880` long-run input tokens,
all eight stream states, optimizer moments, and W&B ID `a30921cf`.

### 2026-07-31 final state and teardown

The continuation was gracefully stopped after reaching `91,191,508,992`
long-run input tokens and optimizer update `39,784`. Its best held-out
validation KL was `1.318127608`; progress had slowed to a plateau near `1.319`,
well above the `<1` target. The final restart had no reported training error.
The durable checkpoint is
`/cache/exp9-standard-muon/long-1t-lr-1e-4-v2/checkpoint.pt`, SHA-256
`9367efa39b51c238f381b0aa38107921ac5f9069a82cdae2800e817ec6fcf7e7`,
with all eight stream states, optimizer state, and W&B ID `a30921cf`.

After the checkpoint completed, both live Northflank services were paused:
the eight-H100 service
`fda-race-asia-northeast/gpu-h100-8` and temporary CPU cache-export service
`fda-test/fda-cache-export`. An account-wide post-stop audit checked all 15
projects, 14 services, six jobs, and all add-ons. It found zero live service
containers, zero services with nonzero desired instances, zero active job
runs, no add-ons, and no query errors. Service definitions and storage were
retained for a future restart.
