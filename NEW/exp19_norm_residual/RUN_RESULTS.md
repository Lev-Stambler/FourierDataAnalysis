# Exp19 run results

Status: first paid 8xH100 preflight recovered and diagnosed; corrected rerun
pending. No training cell ran in the failed preflight.

Recovered W&B run:
https://wandb.ai/lev-tear-tear-labs/exp19-norm-residual/runs/c813xaby

All eight allocated devices were NVIDIA H100 80GB HBM3. The clean joint and
tokenwise Kron models passed architecture, eager/checkpoint FP32, eager/compiled
BF16, optimizer-step, exact-loss, finite-gradient, and initialization gates.
Their initialized branch/state ratios were `0.0631–0.0753`, with aggregate RSS
`0.5526–0.5527`; the residual collapse seen in Exp17 is absent at
initialization.

The preflight also made two distinct problems visible:

- The old 95%-of-branches lower-bound gate incorrectly rejected healthy deep
  controls. The depth-32 Transformer had aggregate branch RSS `0.08238`, almost
  exactly the depth-3 Transformer's `0.08213`; its individual `0.0055–0.0138`
  updates are the intended consequence of `1/sqrt(2L)` scaling. The token-FFN
  control likewise had healthy aggregate RSS `0.40692` despite its dense FFN
  branch being smaller than `0.02`.
- The redundant-scale pre-norm diagnostics are genuinely ill-conditioned:
  aggregate RSS `2.05065`, maximum branch/state `0.83088`, compiled BF16
  gradient relative error `0.0256`, and first-Adam-step relative error
  `0.00023–0.00024`. They remain in the audit artifact but are excluded from
  compiled training and promotion. The parity thresholds were not relaxed.

The fixed preflight gates finite/nonempty ratios, maximum branch/state `<=0.5`,
and aggregate RSS `[0.05,0.75]` for corrected models. The frozen legacy remains
a deliberately pathological reference and is not required to pass the new
conditioning gate. Short six-model benchmark stages add two duplicate worker
cells so all eight paid GPUs stay occupied; the learning stages contain 36+
independent tasks in a dynamic eight-GPU queue.

The local non-training suite now passes 80 Exp14/Exp19 tests, including explicit
Kronecker forward/backward equivalence, block causality, zero-branch identity,
parameter inventories, aggregate conditioning gates, telemetry contracts,
full-node task filling, and cloud-runner integration.

The run is not valid without a direct W&B URL, all eight H100s, the gradient
parity gates, complete physical-versus-unique accounting, and the conditioning
telemetry specified in [`README.md`](README.md).

## Corrected paid run and infrastructure interruption

Corrected W&B run:
https://wandb.ai/lev-tear-tear-labs/exp19-norm-residual/runs/8h20h44k

The corrected eight-H100 preflight passed. Every model selected a physical
batch of 4,096 contexts, exactly 1,048,576 tokens per optimizer step, with no
gradient accumulation. All eight simultaneous workers measured 99.5–100%
median utilization and 3.56M aggregate tokens/s. Selected per-GPU throughput:

| Model | tokens/s | allocated/reserved GiB |
|---|---:|---:|
| legacy post-norm | 90,578 | 40.7 / 53.0 |
| clean joint Kron | 86,318 | 41.9 / 54.2 |
| clean token Kron | 86,456 | 43.1 / 65.7 |
| token-FFN control | 94,260 | 42.4 / 51.9 |
| depth-32 Transformer | 857,473 | 29.2 / 34.2 |
| depth-3 Transformer | 2,164,381 | 20.8 / 23.4 |

The one-example screen completed all 36 model/optimizer/LR cells. The primary
clean-token and clean-joint models reached exact memorization by the step-8
evaluation with AdamW `0.048`. The best depth-32 and depth-3 Transformer hits
were both step 24 with Muon `0.06`; their best AdamW hits were steps 28 and 36.
Thus the residual repair produced a replicated-worthy optimizer-step advantage,
but not a wall-clock win: the deep and shallow Transformers remain about 9.9x
and 25x faster per physical step.

The new telemetry also exposed the next mechanism defect. Only about 6–8% of
the raw optimizer displacement survives row normalization of useful clean-Kron
factor updates, below the preregistered 25% gate. Clean branch/state ratios
remain healthy (`~0.085–0.134`), so this is distinct from the old residual
collapse. A tangent-space/projected factor optimizer is the next justified
architectural-optimization intervention.

During three-seed two-example confirmation, Northflank replaced the running
instance (`gpu-h200-8-...-2dt4p` became `...-gfbdt`). The ephemeral `/root` job
wrapper/log/sentinel disappeared and W&B correctly marked the run crashed. No
model exception was observed, and no confirmation or ladder result is claimed.
The replacement node was paused.

An initial resume attempt established that `/cache` is also instance-ephemeral;
it began recomputing correctness and was deliberately stopped before duplicating
the paid preflight. That stopped run is retained at
https://wandb.ai/lev-tear-tear-labs/exp19-norm-residual/runs/ttb564mu.

The launcher now stores attempt control under `/cache` for same-instance
durability, monitors the campaign heartbeat, and on replacement recreates the
source/runtime/credentials and uploads an external verified resume bundle. The
bundle contains the 956 KB immutable passed preflight and a 36-row screen stage
snapshot reconstructed from 356 W&B history points. Its manifest binds both
files to state `e0d29801f69b` with SHA-256. Promotion from the recovered rows
exactly reproduces all twelve per-model AdamW/Muon choices. No partial
confirmation cell is claimed or reused; the next paid work begins at complete
three-seed confirmation. Every newly completed stage now writes an aggregate
snapshot for subsequent external mirroring.

The final confirmation and evidence-ladder verdict will be appended from the
downloaded durable result artifact; no final architecture result is claimed
before that artifact exists.

## Verified-bundle resumed run

Active W&B run:
https://wandb.ai/lev-tear-tear-labs/exp19-norm-residual/runs/uedf4491

The run remotely verified both resume-bundle SHA-256 hashes, republished the
passed preflight and complete 36-row screen as immutable artifacts, and began
at two-example confirmation without recomputing either stage.

All 36 confirmation cells completed on three fresh seeds. Mean evaluated steps
to exact two-example memorization for each independently selected optimizer
family were:

| Model | AdamW mean steps | Muon mean steps | ladder winner |
|---|---:|---:|---|
| legacy post-norm | 8.00 | 9.33 | AdamW `0.012` |
| clean joint Kron | 8.00 | 12.00 | AdamW `0.048` |
| clean token Kron | 9.33 | 12.00 | AdamW `0.048` |
| token-FFN control | 16.00 | 48.00 | AdamW `0.012` |
| depth-32 Transformer | 25.33 | 24.00 | Muon `0.06/0.012` |
| depth-3 Transformer | 52.00 | 24.00 | Muon `0.06/0.012` |

Every architecture/optimizer pair memorized both examples on all three seeds.
The clean joint model hit at steps `8/8/8`; clean token at `12/8/8`; the
token-FFN control at `16/16/16`; and both winning Transformers at `24/24/24`.
This is a real, replicated optimizer-step advantage: group density is about 2x
more step-efficient than the token-local FFN and 2.6–3x more than the matched
Transformers on two examples. It is not a wall-clock win because Transformer
throughput remains much higher.

The mechanism telemetry is equally consistent. Clean-Kron maximum branch/state
ratios stay healthy (`0.079–0.153`), but only `0.067–0.080` of raw normalized-
factor displacement survives in function space; all clean seeds therefore fail
only the preregistered 25% factor-efficiency gate. The depth-32 Transformer
passes every conditioning gate. The depth-3 Transformer memorizes but develops
branch/state ratios `1.24–1.31`, failing branch dominance. The token-FFN's
aggressive Muon recipe is decisively unstable (`8.0–35.3` branch/state) and
loses to stable AdamW.

The complete 68.5 MB confirmation snapshot is stored as W&B artifact
`exp19-two-example-confirmation-uedf4491:v0`. The run is now executing the
fixed 8/32/128 unique-example held-out ladder. No generalization win is claimed
until that three-seed artifact and paired decision exist.
