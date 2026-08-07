# Exp14 optimizer audit

Status: **complete; independently tuned Muon reverses the AdamW-only result**

Exp14 did not fully tune Muon. It screened body learning rates `0.01`, `0.03`,
`0.1`, and `0.3` for one seed at 1M tokens while fixing the auxiliary AdamW
rate at `0.001`. The two best recipes overall were AdamW, so Muon did not reach
the schedule, multi-seed, 3M, or 10M stages. Consequently, Exp14 supports the
reported AdamW comparison but not an optimizer-independent claim about the
architectures' learning curves.

This audit freezes both architectures and repairs only the tuning policy:

- candidate: `block-kron-r8`;
- control: the previously locked `block-transformer-d3-w256`;
- AdamW LRs: `0.0015, 0.003, 0.006, 0.01, 0.02`;
- Muon body LRs: `0.01, 0.03, 0.06, 0.1, 0.2, 0.3`;
- Muon auxiliary AdamW LRs: `0.001, 0.003, 0.01, 0.03`;
- coarse screen: seed 7, 3M tokens;
- advance the best two recipes **from each optimizer family**;
- robust screen: constant and warmup/cosine, seeds 8 and 9, 10M tokens;
- final: the best AdamW and best Muon recipe for each model, fresh seeds
  10–13, 40M tokens.

The 40M primary recipes are locked independently for each model by the 10M
robust stage. Family-specific AdamW and Muon comparisons are also reported; a
post-hoc optimizer choice at 40M is not silently substituted into the primary
comparison.

## Systems policy

- Exactly eight H100 80GB GPUs, cloud only.
- Eight persistent task workers and candidate-first queues keep all GPUs on
  the expensive cells in parallel.
- Candidate: default Inductor, physical batch 640, 163,840 tokens/step.
- Control: eager execution, physical batch 2,048, 524,288 tokens/step. Its
  cells are too short for compilation to amortize.
- No gradient accumulation.
- The candidate batch inherits the completed ambitious compiled OOM-downward
  sweep (W&B `20t5mto4`) and is re-qualified for both optimizers before
  training.
- Exact BF16 loss agreement, finite optimizer state, utilization, throughput,
  allocated/reserved VRAM, actual token batches, and a direct W&B URL are paid
  launch gates.
- Fast Transformer preflight measurements use 40 optimizer steps. The initial
  10-step window lasted under three seconds and produced too few utilization
  samples for a stable repeat-level median.

## Launch

```bash
uv run --no-sync python -m exp14_block_kronecker.remote_runner \
  --repo-root . \
  --local-state exp14_block_kronecker/optimizer_audit_state \
  --program optimizer_audit
```

## Result — 2026-08-04

- W&B: <https://wandb.ai/lev-tear-tear-labs/exp14-block-kronecker/runs/f8ihuo7e>
- Source digest: `e688b9dbedba...`
- Actual trained tokens across all cells: `1,155,891,200`.
- Candidate execution: default Inductor, batch 640, 163,840 tokens/step,
  approximately 110k tokens/s, 100% steady utilization.
- Control execution: eager, batch 2,048, 524,288 tokens/step, approximately
  1.84M tokens/s, 96.5–100% final-cell utilization.

At the 10M robust-selection horizon, the independently selected primary
recipes were:

| Model | Optimizer | Body LR | Auxiliary LR | Schedule | Mean NLL |
|---|---|---:|---:|---|---:|
| block-kron-r8 | Muon | 0.06 | 0.003 | constant | 7.390986 |
| block-transformer-d3-w256 | Muon | 0.03 | 0.003 | constant | 7.339090 |

Muon therefore already reversed the earlier AdamW comparison at 10M: the
candidate-minus-control delta was `+0.051896` NLL.

### Fresh four-seed 40M confirmation

| Seed | Kronecker Muon | Transformer Muon | candidate − control |
|---:|---:|---:|---:|
| 10 | 7.381856 | 6.956367 | +0.425489 |
| 11 | 7.384008 | 6.964764 | +0.419244 |
| 12 | 7.383466 | 6.953976 | +0.429490 |
| 13 | 7.384780 | 6.957179 | +0.427601 |

The primary mean delta is `+0.425456`: the tuned Transformer wins all four
fresh seeds decisively.

The optimizer-family control remains informative. With the best independently
tuned AdamW recipes, the Kronecker NLL was `7.376468` and Transformer NLL was
`7.383753`, a small four-for-four candidate win of `-0.007284`. With Muon, the
corresponding means were `7.383528` and `6.958072`, respectively. The original
small win was therefore an AdamW-specific result, not an optimizer-independent
architecture win.

The first paid attempt at
<https://wandb.ai/lev-tear-tear-labs/exp14-block-kronecker/runs/8ug7yoig>
ran no tuning cells. A three-second Transformer/Muon preflight repeat produced
only six utilization samples and a noisy 74% median. The service paused; the
accepted run used a 40-step window and a repeat-aggregated utilization gate.
