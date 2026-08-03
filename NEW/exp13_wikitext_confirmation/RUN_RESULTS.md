# Experiment 13 run log

## v1 — operational failure, no architecture verdict

- Date: 2026-08-02 UTC
- Hardware: exactly 8× NVIDIA H100 80GB HBM3 on Northflank
- W&B: https://wandb.ai/lev-tear-tear-labs/exp13-wikitext-confirmation/runs/ah88kkkd
- Locked stage budget: 1,800 wall seconds, 4 aggregate GPU-hours,
  `$11.30` projected campaign maximum
- Controller outcome: `failed: wall-clock or dollar budget exceeded`
- Training cells completed: zero
- Confirmation holdout opened: no
- Final holdout opened: no
- Architecture verdict: none

All local and remote correctness gates passed before launch. The official
WikiText test array was checksum-verified and sealed into disjoint 581-window
confirmation and 582-window final halves. W&B authentication and a direct run
URL succeeded before GPU work. All eight H100s were allocated and the physical
batch sweep started from 4,096 contexts, or 1,048,576 global tokens/step.

The failure was in the runner. V1 launched a fresh process for every compiled
benchmark cell and measured only steady-state step time, excluding compilation.
Torch Dynamo/AOT/Inductor repeatedly spent minutes compiling deep graphs during
the model/family/common-batch and 1/2/4/8 scaling sweeps. Preflight never
published `preflight.json` before the hard controller timeout, so no tuning,
10M-token comparison, or holdout evaluation happened. This is not positive or
negative evidence about the architecture.

The controller terminated the campaign at its ceiling and the exact service
was paused. One orphaned compile worker retained 53 GiB on GPU 5 after the
parent exited; it was terminated by exact PID, after which all eight GPUs
reported zero utilization and zero allocated memory.

## v2 correction

`campaign_v2.py` preserves the frozen models, recipes, tuning policy, seeds,
holdout split, and verdict thresholds. It changes only execution policy:

- rejects compilation for this short-cell campaign based on the measured v1
  end-to-end failure;
- benchmarks and trains eagerly, while retaining BF16 and physical batches;
- keeps the ambitious OOM-down batch search and underfilled baseline;
- persists preflight progress and reuses a completed preflight artifact;
- compares the actual BF16 autocast loss against FP32 for every architecture;
- measures ten steady-state steps with in-window utilization, power, allocated
  and reserved VRAM, and rejects rows below 85% median utilization;
- still measures 1/2/4/8-worker scaling without recompiling each worker and
  requires at least 80% eight-way cell-parallel efficiency;
- forces every training task to record `compiled: false` truthfully.

V2 has not yet been launched. The active architecture-verdict request now
authorizes it as the next paid run; launch awaits renewal of the expired
Northflank user session so the prior orphan H100 can first be torn down.
