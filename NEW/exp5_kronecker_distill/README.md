# Tensor-native Kronecker distillation

Experiment 5 distills pinned Qwen3.5-0.8B on exact 16-token FineWeb-Edu
contexts into a learned width-64 tied-vocabulary student.

The student never treats its `16 × 64` state as a generic dense vector.
Order-two maps apply context and channel factors directly to `[B, 16, 64]`;
order-three maps use `[B, 16, 8, 8]`. The terminal layer is a rectangular
Kronecker map that contracts `(16, …) → (1, …)`, so it computes only the
final context position. The exact full-vocabulary head is the transpose of
the learned-from-scratch `248,320 × 64` input embedding.

Rank-one depth is screened first for depths 1–32. Ranks 2, 4, and 8 are then
tested only at the selected depth for each factor order. Kronecker factor
matrices use NorMuon at `3e-3`; the tied vocabulary table and other auxiliary
parameters use fused AdamW at `3e-4`.

The staged run uses four-way DDP on the Central `fda-node4` H100 service:
global batch 512, local batch 128, with automatic fallback to microbatch 64
and two gradient-accumulation steps. Stage A screens rank one at depths
1/2/4/8/16/32 for both factor orders. Stage B freshly screens ranks 2/4/8 at
each selected depth, then continues the two order winners to 1,048,576
contexts and the validation winner to 4,194,304.

```bash
uv run python -m qwen_kron_distill plan
uv run pytest -q exp5_kronecker_distill/tests
```

Northflank preflight and launch are intentionally separate. Both mutating
commands pin the target to four H100s and refuse to sync or start while a GPU
job or coordinator is present:

```bash
cd northflank
./nf.sh exp5-plan
NF_PROJECT=fda-test-e1 NF_SERVICE=fda-node4 ./nf.sh exp5-preflight
NF_PROJECT=fda-test-e1 NF_SERVICE=fda-node4 ./nf.sh exp5-launch
```

The H200 Spot service is not an Exp 5 launch target.
