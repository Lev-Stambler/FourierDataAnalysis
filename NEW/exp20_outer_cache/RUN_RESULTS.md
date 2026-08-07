# Exp20 run results

## 2026-08-06 eight-H100 preflight: failed and stopped

- W&B: <https://wandb.ai/lev-tear-tear-labs/exp20-outer-cache/runs/qtfn1634>
- Frozen source commit: `679350e`
- Uploaded source digest: `ff70c047a34b`
- Allocation: Northflank `fda-race-asia-northeast/gpu-h100-8`, exactly eight
  NVIDIA H100 80GB HBM3 GPUs
- Objective: 256-token contexts arranged as 16 causal groups of 16 tokens
- Gradient accumulation: 1
- Registered tracks: two reference execution modes, packed dense ranks
  8/4/2/1, packed order-three semiseparable rank 8, and the matched depth-32
  Transformer

The hardware and inventory gates passed. All models were within the registered
0.1% parameter-matching tolerance; the worst mismatch was 0.083% for packed
rank 2. Seven correctness cells returned without failure. The
`semiseparable3-r8-packed` cell failed with `RuntimeError: non-finite gradient`,
so the campaign stopped at `correctness/parity` as designed.

No batch sweep, utilization-qualified throughput result, cache-speed result,
optimizer tuning, memorization result, or architecture comparison was produced.
This run therefore says nothing about whether KronMix beats the Transformer.

The failure was traced to the recurrent outer row norm. It used
`sqrt(c^T G c)`; although $G$ is positive semidefinite mathematically, finite
precision cancellation can make the computed quadratic form negative. The
working tree now computes the norm from explicit row coefficients in FP32,
which is non-negative by construction. That change has not had a paid cloud
rerun and is not claimed as validated.

Artifacts committed with this record:

- `cloud_state/preflight.json`: exact GPU and parameter inventory
- `cloud_state/remote.log`: complete compiler/autotuner output and terminal
  traceback
- `cloud_state/lifecycle.json`: launcher cleanup record
- `cloud_state/failure-summary.json`: compact machine-readable verdict

After launcher cleanup paused the service, it was explicitly resumed at the
user's request. The 8×H100 service is up, with no experiment process and all
eight GPUs idle.
