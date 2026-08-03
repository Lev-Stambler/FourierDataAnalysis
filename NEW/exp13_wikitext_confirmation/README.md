# Experiment 13: adversarial WikiText confirmation

Exp13 is a clean confirmation of the Exp12 signal, not another architecture
iteration. It asks whether the frozen `deep-kron-r8` model really beats a
properly implemented and tuned, total-parameter-matched standard Transformer.
The paid result does not exist until the locked campaign completes.

## Frozen claim and controls

The candidate is an exact numerical copy of the Exp12 32-layer, width-128,
rank-8 content-routed Kronecker model. Its Muon recipe is frozen from Exp12 so
the new holdout cannot influence it. A replay of the old Exp12 Transformer and
four standard pre-norm causal RoPE Transformer aspect ratios make provenance
errors and an accidentally weak narrow control visible.

| Configuration | Depth | Width | SwiGLU width | Total parameters |
|---|---:|---:|---:|---:|
| deep Kronecker candidate | 32 | 128 | 256 | 6,410,560 |
| exact Exp12 control replay | 32 | 128 | 192 | 6,553,664 |
| standard Transformer | 32 | 128 | 176 | 6,356,992 |
| standard Transformer | 10 | 160 | 576 | 6,410,240 |
| standard Transformer | 6 | 192 | 688 | 6,408,192 |
| standard Transformer | 3 | 256 | 624 | 6,418,432 |

Every standard control uses the Exp10 block semantics: pre-attention RMSNorm,
causal RoPE scaled-dot-product attention, a residual update, pre-FFN RMSNorm,
SwiGLU, and a second residual update. The four shapes prevent a conclusion
from depending on one depth/width allocation.

## Selection and sealed evaluation

Only the official WikiText-103 training and validation windows are available
to tuning. Before any Exp13 evaluation, the previously untouched 1,163-window
official test array is split contiguously and checksum-sealed:

- confirmation: source indices 0–580 (148,736 prediction tokens);
- final: source indices 581–1,162 (148,992 prediction tokens);
- overlap: zero windows.

The campaign first tunes all four standard controls independently over AdamW
and canonical Muon. It searches four learning rates per family with an
automatic one-step 3× boundary extension, then tests constant versus token
warmup/cosine schedules across seeds 0 and 1. Candidate and replay recipes
remain frozen. The top two standard configurations, candidate, and replay are
trained on fresh seeds 3–6 to at least 10M tokens.

Validation chooses the strongest standard configuration. Only it and the
candidate open the confirmation split. Promotion is deliberately strict:

- the Kronecker model must win all four paired seeds; and
- mean candidate-minus-Transformer NLL must be at most `-0.10`.

Failure stops immediately and the final split is never opened. Success extends
candidate, selected control, and the runner-up control to at least 40M tokens.
The final report contains both a token-matched comparison and a paired
equal-optimization-time comparison, with the latter valid only when measured
times match within 5% and the one-corpus-pass cap was not reached.

## Compute policy

The paid stage requires exactly eight H100 80GB GPUs and a direct W&B URL. Its
preflight starts at 4,096 contexts (1,048,576 global tokens per optimizer step)
and searches downward after OOMs. V2 rejects compilation for this short-cell
campaign because v1 measured repeated fresh-process compilation consuming the
entire budget before training. It instead measures ten eager steady-state
steps, BF16-versus-FP32 loss agreement, allocated/reserved VRAM, in-window
utilization and power, a batch-128 underfilled baseline, and 1/2/4/8-GPU
cell-parallel scaling. Stable rows require at least 85% median utilization and
eight-way scaling requires at least 80% efficiency. Training uses BF16 eager
execution, physical batches, and no gradient accumulation. Short six-model
benchmarks are padded with independent replicas so all eight paid GPUs are
active; the smaller scaling measurements are the explicit exception.

The research controller adds a locked confirmation tier: eight GPUs, at most
four aggregate GPU-hours, at most `$15`, and 1,800 wall-clock seconds. It always
pauses the exact Northflank target after success, failure, timeout, or stale
heartbeat.

## CPU-only audit commands

```bash
uv run --no-sync python -m exp13_wikitext_confirmation inventory
uv run --no-sync python -m exp13_wikitext_confirmation local-audit \
  --output=/tmp/exp13-local-audit.json
uv run --no-sync python -m exp13_wikitext_confirmation prepare-holdout \
  --data-root=/tmp/exp10/data/wikitext \
  --output-root=/tmp/exp13-holdout \
  --result=/tmp/exp13-holdout-gate.json
uv run --no-sync pytest -q exp13_wikitext_confirmation/test_exp13.py \
  research_control/test_controller.py
```

No training is permitted locally. After source hashes are locked, the durable
launcher resumes the exact Northflank 8×H100 node, verifies it is idle, uploads
an immutable source snapshot and W&B credential, executes all three controller
stages in the cloud, retrieves result/audit/ledger/preflight artifacts, and
pauses the service in `finally`:

```bash
uv run --no-sync python -m exp13_wikitext_confirmation.remote_runner_v2 \
  --repo-root=. \
  --local-state=/home/lev/.cache/research-control/exp13-wikitext-confirmation-8xh100-v2b \
  --timeout-seconds=2400
```
