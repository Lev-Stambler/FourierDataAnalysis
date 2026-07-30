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
