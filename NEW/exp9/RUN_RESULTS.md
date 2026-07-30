# Experiment 9: standard Muon continuation

Exp9 keeps Exp8's 17,006,592-parameter student, frozen teacher, exact
full-vocabulary per-token KL, scale-safe residuals, and tied dense
`248320 × 64` vocabulary. It changes only the optimizer split:

- canonical Muon independently updates every rank-local `16 × 16` and
  `64 × 64` Kronecker matrix;
- AdamW8bit continues to update the tied embedding/unembedding matrix;
- Exp8 vocabulary moments are preserved, while body Adam state is discarded
  and Muon momentum starts fresh.

The body uses Muon LR `0.02`, momentum `0.95`, Nesterov momentum, five
Newton–Schulz iterations, and no factor weight decay. The vocabulary uses LR
`3e-4`, betas `(0.9, 0.95)`, epsilon `1e-8`, and weight decay `0.1`.
Muon warms over 268,435,456 input tokens, then both learning rates follow
token-based cosine schedules over the remaining cumulative 1T-token budget.

## Preflight and production

Pending the replacement eight-H100 container. Record the winning physical and
optimizer batches, utilization, throughput, checkpoint SHA-256, direct W&B
URL, and first held-out comparison here before declaring the launch healthy.
