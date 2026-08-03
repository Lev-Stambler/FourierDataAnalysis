# ExpV2-2: fix the synthetic gate before judging OOD

ExpV2-2 preserves the completed ExpV2-1 code and results. It changes only the
diagnostic task and training protocol while importing the same locked model and
optimizer implementations from ExpV2-1.

## What was broken

ExpV2-1 recall asked every stored key exactly once. Predicting one arbitrary
visible value therefore scored exactly `1/N`: 25% with four records and 12.5%
with eight. Training and evaluation landed at those plateaus. The old OOD split
also introduced new support slots and new answer positions at the same time it
doubled cardinality.

Old delay OOD was worse: delay IDs 17--32 never appeared during training, so
their tied embeddings were untrained. A fixed-lag predictor scored 7.3% ID,
which matched the Transformer. Finally, the 20M-token robust cells had only
20--77 optimizer updates because the physical batches were enormous.

## Corrected task contract

- Recall uses one randomly selected query, random support slots, and explicit
  visible-value shortcut baselines.
- Training varies between two, three, and four records.
- `ood-cardinality` uses eight records but only slots and query positions seen
  during training.
- `ood-position` keeps an ID cardinality and changes only the query-position
  domain.
- Delay is encoded with five binary positions. Both bit symbols occur at every
  bit position in training; `ood-composition` holds out bit combinations, not
  token embeddings.
- Every result reports raw accuracy, shortcut accuracy, and normalized progress
  above the shortcut.
- OOD is not interpreted until a positive control exceeds 95% ID accuracy.

The generator audit proves deterministic data, perfect causal oracle recovery,
disjoint positional axes, complete delay-bit coverage, and empirical agreement
with the analytical shortcut baselines.

```bash
uv run --no-sync python -m expv2.exp2 audit \
  --output=/tmp/expv2-2-task-audit.json
uv run --no-sync python -m expv2.exp2 budget --batch-contexts=4096
uv run --no-sync pytest -q expv2/exp2/test_exp2.py
```

## Calibration order

1. Run the Transformer with AdamW on the sanity distributions. This checks the
   data, loss, and architecture with a conventional positive-control recipe.
2. Require at least 95% Transformer ID accuracy on the full training
   distributions. If it fails, tune the control; do not inspect OOD.
3. Repeat the Transformer control with Muon. This separates optimizer failure
   from task failure.
4. Only then run matched Kronecker cells and interpret cardinality and position
   OOD separately.

Every calibration cell requires at least 1,000 optimizer updates. At a physical
batch of 4,096 contexts this is 4,096,000 contexts or 524,288,000 tokens. Paid
launch code must still benchmark stable batches, utilization, VRAM, power, and
W&B connectivity. Hyperparameter replicas should be packed concurrently when
possible so the H100 remains full without reducing each model to a few dozen
updates.

The initial sanity control exposed a two-hop optimization-path failure. A
joint-edge curriculum fixed it, and the subsequent full variable-distribution
Transformer control passed the ID gate with 97.064% delay, 100% associative,
and 100% two-hop accuracy. OOD is now interpretable; the exact results and the
infrastructure recovery note are in `RUN_RESULTS.md`.

The optional rank-one diagnostic is a cloud-only, eight-GPU matched Muon
comparison. It tunes
four learning rates independently for the Transformer and rank-1/depth-66
Kronecker candidate, selects from ID only, then confirms both recipes on four
fresh paired seeds. Confirmation phases are matched by contexts/tokens rather
than optimizer-step count; the tuning sweep is wall-time balanced across all
eight workers and never uses OOD for LR selection:

```bash
uv run --no-sync python -m expv2.exp2 launch-matched-cloud \
  --repo-root=. \
  --local-output=/home/lev/.cache/research-control/expv2-2-matched-muon-8gpu-v1/artifacts \
  --result=/home/lev/.cache/research-control/expv2-2-matched-muon-8gpu-v1/result.json
```

The launcher refuses local or partial-node execution, verifies eight H100/H200
GPUs, benchmarks ambitious physical batches and 1/2/4/8-worker scaling, creates
direct W&B runs before training, uses a durable remote job, retrieves artifacts,
and pauses the guarded `fda-test/fda-node8h2` service in `finally`.

It is not the current paid priority: the rank-one model predates the stronger
Exp12 content-routed candidate. The next architecture-verdict run is the sealed
Exp13 v2 four-seed WikiText confirmation of that current candidate.
