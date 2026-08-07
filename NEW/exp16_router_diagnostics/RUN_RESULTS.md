# Exp16 run results

Status: **complete — stop; the routers move but are not causally useful**.

- W&B: https://wandb.ai/lev-tear-tear-labs/exp16-router-diagnostics/runs/e7vf53oi
- Verdict: `stop_router_not_causally_useful`
- Hardware: 8 × NVIDIA H100 80GB HBM3
- Training: cloud only, compiled BF16, exact fused loss, no gradient
  accumulation

## Result

Increasing the router learning rate produced large, frequently saturated gate
changes, but those changes did not improve the model or materially affect its
predictions. The best `bi-decoupled-r8` track used the `10×` router LR and beat
`current-r8` by only `0.000087` validation block NLL. Forcing both of its
routers back to neutral changed NLL by only `+0.000014`.

| Track | Router LR | Validation NLL | Difference vs current | Neutral-ablation effect |
|---|---:|---:|---:|---:|
| `dense-router-1x` | 1× | **7.387660** | −0.000401 | +0.000002 |
| `bi-router-10x` | 10× | 7.387779 | −0.000282 | −0.000000 |
| `bidec-router-10x` | 10× | 7.387973 | **−0.000087** | **+0.000014** |
| `current-router-1x` | 1× | 7.388061 | 0 | +0.000018 |
| `bidec-router-1x` | 1× | 7.388189 | +0.000128 | +0.000003 |
| `bidec-router-30x` | 30× | 7.388193 | +0.000132 | +0.000014 |
| `bidec-router-0x` | 0× | 7.388213 | +0.000152 | 0 |
| `bidec-router-3x` | 3× | 7.388515 | +0.000454 | +0.000004 |

The predeclared screen required a `−0.005` loss win, a `+0.002` neutral
ablation effect, and gate standard deviation of at least `0.02`. The best
track passed only the gate-variation condition (`0.121806` source-gate mean
standard deviation). It missed the loss and causal-effect thresholds by about
57× and 147× respectively, so no tuning or 40M-token confirmation ran.

The ordinary destination router is also unsupported: `current-r8` had mean
destination-gate standard deviation `0.157490` with 54.3% saturation, yet
neutralizing it changed NLL by just `0.000018`. Future candidates should keep
the frozen routed model as a control but reallocate router parameters to the
nonlinear workspace rather than tune the router again.

## Accelerator and correctness audit

All eight tracks ran concurrently. Every accepted benchmark measured 100%
median GPU utilization, finite forward/backward/optimizer state, and exact
loss agreement. Physical batches were 512 or 640 contexts, giving 131,072 or
163,840 tokens per optimizer step without accumulation. Routed variants ran
at roughly 99–109k tokens/s; the dense-workspace control ran at 235k tokens/s.

The retrieved machine-readable artifact is
[`cloud_state/result.json`](cloud_state/result.json), the complete remote log
is beside it, and `cloud_state/lifecycle.json` confirms the Northflank service
was paused after retrieval.

## Bootstrap audit

Before the accepted run, three attempts stopped before W&B or training while
the minimal image and Northflank upload semantics were corrected: apt/curl was
removed, a portable `uv` executable was uploaded, and the uploader's
directory-plus-basename behavior was handled explicitly. These attempts made
no architecture measurement. The accepted bootstrap reused the validated
241 MiB corpus snapshot and completed normally.
