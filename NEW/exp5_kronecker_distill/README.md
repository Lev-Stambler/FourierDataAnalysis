# Tensor-native Kronecker distillation

Experiment 5 distills pinned `Qwen/Qwen3.5-0.8B-Base` on exact 16-token
FineWeb-Edu contexts. Every body map contracts the `[B, 16, width]` state in
native tensor form; it never materializes a generic `16 × width` dense map.
The terminal Kronecker layer computes only the final sequence slot.

The current Northflank target is the Central `fda-node8h2` service with eight
H200s. Each optimizer step contains 65,536 contexts, exactly 1,048,576 input
tokens, with no gradient accumulation. Teacher forward work is shared between
simultaneous screen cells.

## Vocabulary variants

- `v3-khatri-rao` through `v5-normuon-lr` use a tied multiplicative vocabulary
  `E[i,j,d] = A[i,d] * B[j,d]`, with exact modes `485 × 512`.
- `v6-dense-tied` keeps the Kronecker body fixed at width 64, depth 32, and
  operator rank 8. It isolates tied dense vocabulary widths 64/128/256 at the
  same NorMuon LR 0.2. Learned input-compression and output-expansion matrices
  bridge wider vocabulary channels to the fixed width-64 body.

The v6 trainable parameter counts are 17,070,016 / 32,978,880 / 64,780,224.
Thus the screen changes vocabulary expressivity without paying the quadratic
body-width cost seen in v4.

## Recorded results

All KL values are exact full-vocabulary `KL(P_teacher || P_student)` on the
fixed 8,192-context validation split.

| Study / milestone | Configuration | Contexts | Validation KL |
|---|---|---:|---:|
| v3 final | Khatri-Rao width 64, NorMuon factors 0.03 + AdamW auxiliaries 0.003 | 67,108,864 | 2.891722 |
| v4 isolated screen | width 64, LR 0.1 | 2,097,152 | 4.071430 |
| v4 isolated screen | width 128, LR 0.1 | 2,097,152 | 3.982570 |
| v4 isolated screen | width 256, LR 0.1 | 2,097,152 | 3.899940 |
| v4 isolated screen | width 384, LR 0.1 | 2,097,152 | 3.862620 |
| v5 screen | all-NorMuon LR 0.1 | 2,097,152 | 4.109303 |
| v5 screen | all-NorMuon LR 0.2 | 2,097,152 | **4.027509** |
| v5 screen | all-NorMuon LR 0.3 | 2,097,152 | 4.542660 |
| v5 screen | all-NorMuon LR 0.5 | 2,097,152 | 6.583729 |
| v5 mid | all-NorMuon LR 0.2 | 16,777,216 | 3.376868 |

The v5 run was checkpointed again at 24,117,248 contexts and stopped because
its mid result was already dominated by the matched v3 mid KL of 3.174985.
The H200 service was deliberately left running for the other active job.

## Commands

```bash
uv run --no-sync pytest -q exp5_kronecker_distill/tests
QWEN_KRON_STUDY=v6-dense-tied uv run --no-sync python -m qwen_kron_distill plan

NF_PROJECT=fda-test NF_SERVICE=fda-node8h2 ./northflank/nf.sh exp5-dense-preflight
NF_PROJECT=fda-test NF_SERVICE=fda-node8h2 EXP5_WANDB_MODE=online \
  ./northflank/nf.sh exp5-dense-launch
```
