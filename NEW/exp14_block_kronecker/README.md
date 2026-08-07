# Experiment 14: block-causal fully factorized Kronecker

Exp14 tests the architecture described in [`../ARCHITECTURE.md`](../ARCHITECTURE.md).
It replaces the dense 256 × 256 learned token bank with an order-five rank
path over `16 × 4 × 4 × 8 × 16` modes.

## Scientific question

Can eight rank paths with one causal group factor and four dense workspace /
channel factors beat independently tuned, total-parameter-matched
block-causal Transformers on WikiText block NLL?

This is a block-autoregressive objective. Every 16-token source group predicts
the next 16-token target group. Inner workspace tokens communicate
bidirectionally, so conventional one-token-shift NLL would leak targets and is
not used.

## Locked pilot

- Cloud only: one 8 × H100 80GB Northflank node.
- Candidate: `block-kron-r8`, 5,400,896 parameters.
- Controls: four block-causal Transformer depth/width shapes, all within 0.1%
  total parameters.
- Optimizers independently screened for every primary model: AdamW and Muon.
- Coarse grid: eight recipes at 1M predicted tokens.
- Robust selection: the best two coarse recipes, constant and warmup-cosine,
  on two new seeds at 3M tokens.
- Final: selected recipes on four new seeds at 10M tokens.
- Mechanistic ablation: the candidate with all workspace permutations set to
  identity, using the selected candidate recipe.
- Exact FLA fused-linear cross entropy after numerical agreement with
  materialized FP32 cross entropy.
- No gradient accumulation. Physical batch is selected by measured throughput
  after an ambitious OOM-downward sweep.

The primary pilot gate requires the candidate to beat the Transformer aspect
ratio locked by the robust stage on all four final seeds and by at least 0.05
mean block NLL.

## Local checks

Local execution is correctness-only; training is forbidden locally.

```bash
CUDA_VISIBLE_DEVICES='' uv run --no-sync pytest -q exp14_block_kronecker
```

## Cloud launch

```bash
uv run --no-sync python -m exp14_block_kronecker.remote_runner \
  --repo-root . \
  --local-state exp14_block_kronecker/cloud_state
```

The launcher refuses to allocate without a verified W&B credential, an actual
8 × H100 deployment configuration, an idle GPU inventory, and the cached
WikiText corpus. The durable service retains its historical `gpu-h200-8` id
after being converted in place to H100; the actual allocation is checked rather
than inferred from that id. The launcher pauses it after success or failure.

This Northflank account exposes only single-writer NVMe volumes, while GPU
workloads require multi-writer volumes. The launcher therefore snapshots the
checksum-verified frozen corpus to a git-ignored local tar before pausing and
uploads it on later launches. Runtime and Inductor files remain ephemeral; the
large deterministic data-preparation step is not repeated.

After the tuned eager campaign, benchmark compilation on all eight H100s:

```bash
uv run --no-sync python -m exp14_block_kronecker.remote_runner \
  --repo-root . \
  --local-state exp14_block_kronecker/compile_state \
  --program compile_probe
```

The balanced probe runs two eager, three default-Inductor, and three
reduce-overhead workers simultaneously. Max-autotune was removed after a live
probe spent more than six minutes compiling the unrolled graph while six H100s
sat idle. A compiled mode is accepted only after exact-loss and finite
optimizer checks and at least 5% measured median throughput improvement over
eager.

The observed selected mode and batch can be reproduced with:

```bash
uv run --no-sync python -m exp14_block_kronecker.remote_runner \
  --repo-root . --local-state exp14_block_kronecker/compiled_batch_state \
  --program compiled_batch_probe

uv run --no-sync python -m exp14_block_kronecker.remote_runner \
  --repo-root . --local-state exp14_block_kronecker/compiled_replay_state \
  --program compiled_replay
```

Observed: default Inductor at batch 640 (163,840 tokens/step) reached about
110k tokens/s and repeated the candidate's four-seed NLL win. See
[`RUN_RESULTS.md`](RUN_RESULTS.md) for the full audit and compile-amortization
caveat.

Exp14's original selection policy eliminated Muon after a one-seed 1M-token
screen with a fixed auxiliary LR. The locked corrective campaign is documented
in [`OPTIMIZER_AUDIT.md`](OPTIMIZER_AUDIT.md); it jointly tunes Muon body and
auxiliary rates for both the unchanged candidate and locked Transformer before
making any longer-horizon scaling claim.

Observed: independently tuned Muon reverses the AdamW-only result. At 40M
tokens the Transformer reached mean block NLL `6.958072`, versus `7.383528`
for the candidate, and won all four paired seeds. The candidate retained only
a small AdamW-specific `-0.007284` mean advantage.
