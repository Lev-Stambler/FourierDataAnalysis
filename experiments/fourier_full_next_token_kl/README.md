# 16-token input/output Walsh KL

This experiment learns one sparse full-vocabulary score:

```text
score(x, y) = token_bias(y) + sum_j coefficient_j * chi_j(x, y)
```

`x` is a fresh 16-token FineWeb context represented by 512 fixed input bits.
`y` is one of Qwen3.5-0.8B's 248,077 tokens in a balanced, injective 18-bit
output tree. The student has 3,950,000 learned input/output Walsh characters:
87,552 input-degree-1 terms and 3,862,448 input-degree-2 terms, all with total
degree at most 4.

The loop is deliberately small:

1. Stream contexts forward only; never cache or replay training samples.
2. Run the pinned compiled teacher once for full next-token logits.
3. Run the compiled Walsh student for all vocabulary logits.
4. Minimize exact `KL(P_teacher || P_student)`.
5. Backpropagate the raw exact KL, with no loss multiplier.
6. Step two standard fused AdamW optimizers and audit exact duplicate
   characters without changing them.
7. After the final optimizer step, merge exact duplicates once for export.

There is no CE objective, vocabulary truncation, cached dataset, scheduler,
dynamic loss scaler, repair, recycle, resume, or checkpoint selection.

Duplicate characters remain live throughout optimization. Although merging
duplicates preserves logits at that instant, it would destroy their distinct
AdamW histories and prevent their STE supports from splitting later. The one
final quotient sums duplicate coefficients, deactivates the redundant rows,
verifies that logits and KL are unchanged, and omits redundant rows from the
compact artifact.

The codebook is pinned at:

```text
/cache/fourier_full_next_token_kl/token_codes_b32_outtree18_s0_qwen5c8a1b9.npz
```

The runner validates model revision, vocabulary and raw-logit widths, bit
counts, balanced-tree scheme, input/output hashes, and injectivity before
training. It never reconstructs output vertices from truncated LSH codes.

The projected artifact is 31,643,052 bytes, or 50.56x relative to the 1.6 GB
reference. Both the projected and exported artifact must stay at or below
32,000,000 bytes.

## Run

From `experiments/`:

```bash
uv run modal run fourier_full_next_token_kl/modal_train.py --stage tests

uv run modal run fourier_full_next_token_kl/modal_train.py::prepare_token_codes

uv run modal run --detach \
  fourier_full_next_token_kl/modal_train.py::train_corrected_capacity_gate \
  --run-label kiss16-capacity-gate-v1
```

The 250-step gate passes only if:

- loss remains finite and the job does not OOM;
- balanced-codebook and degree invariants hold;
- final exact duplicate-row fraction is at most 0.5%;
- fresh 2,048-context KL improves at least 0.5 over the measured mean-prior
  baseline.

After a passing gate:

```bash
uv run modal run --detach \
  fourier_full_next_token_kl/modal_train.py::train_corrected_capacity \
  --run-label kiss16-capacity-x3950000-joint-cartesian
```

The full run uses batch 512, teacher microbatch 256, character chunk 8,192,
up to 8,000 steps and 25,200 training seconds. Training is logged to W&B every
step; exact merge plus fresh 2,048-context evaluation runs every 50 steps. The
target is fresh exact KL at most 1.0.

## Corrected full run: effective batch 4,096

The high-batch variant averages exact KL gradients from eight consecutive,
never-replayed 512-context microbatches at fixed parameters before each AdamW
step. This is mathematically identical to a physical batch of 4,096 but keeps
the 3.95M-term `[batch, terms]` matrix within H100 memory. Merge/evaluation
frequency is rescaled to preserve approximately the same number of fresh
examples between read-only audits. The two standard AdamWs use LR `0.03` for
the STE tensors and LR `0.003` for coefficients/token bias, with betas
`(0.9, 0.999)`, epsilon `1e-8`, and weight decay `0.01`.

```bash
uv run modal run --detach \
  fourier_full_next_token_kl/modal_train.py::train_high_batch_gate \
  --run-label kiss16-high-batch4096-gate

uv run modal run --detach \
  fourier_full_next_token_kl/modal_train.py::train_high_batch \
  --run-label kiss16-high-batch4096-rawkl-finalmerge-full
```
