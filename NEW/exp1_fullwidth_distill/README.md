# Full-width Qwen3.5 distillation

This experiment distills pinned `Qwen/Qwen3.5-0.8B-Base` on exact
16-token FineWeb contexts into attention-free students. Qwen's frozen tied
embedding/unembedding is reused at both ends. The learned state is never
projected to 1,024 dimensions:

```text
token ids
  -> frozen embeddings [B, 16, 1024]
  -> flatten [B, 16384]
  -> dense, Monarch, BTT, or Kronecker full-width stack [B, 16384]
  -> reshape [B, 16, 1024]
  -> take final slot [B, 1024]
  -> frozen tied unembedding [B, tokenizer_vocab]
```

Because `16 × 1024 = 16384 = 128²`, every square Monarch map is exactly two
banks of 128 dense 128×128 blocks with the fixed Monarch permutation and no
padding. A square Monarch map has 4,194,304 trainable weights; a dense map has
268,435,456.

## Local checks

```bash
uv run --extra test python -m pytest -q
```

## Northflank SSH node (testing)

An 8×H100 SSH node on Northflank (`fda-test/fda-node`, us-central, tearedcoder
team) for fast edit → sync → pytest iteration. Modal (below) stays for long
detached runs. Everything is driven by `northflank/nf.sh`:

```bash
cd northflank
./nf.sh up          # create/refresh project + 8xH100 service + 500GB /cache volume
./nf.sh bootstrap   # one-time: apt basics, uv, python 3.11 env, modal-image extras
./nf.sh test        # rsync NEW/ to the node and run pytest there
./nf.sh run 'uv run --no-sync python -c "..."'   # ad-hoc command in NEW/ on the node
./nf.sh ssh         # interactive shell
./nf.sh proxy       # local SSH endpoint for scp/rsync/VS Code Remote (root@127.0.0.1)
./nf.sh status      # deployment state + nvidia-smi
./nf.sh pause       # ⚠ stop billing (node is ~$22.6/hr!) — /cache persists
./nf.sh resume
```

**⚠ Cost:** the node bills ~$22.6/hr while running (8× h100-80 @ $2.74 +
plan). `./nf.sh pause` the moment you stop iterating.

Notes:
- One-time per machine: `up` generates `~/.ssh/id_northflank` and registers it
  as a Northflank SSH identity automatically.
- `HF_TOKEN`/`WANDB_API_KEY` are copied from your local environment onto the
  node during `bootstrap` (re-run it after changing them).
- The remote venv mirrors the Modal image (torch 2.10 cu12, causal_conv1d
  wheel, flash-linear-attention); `UV_NO_SYNC=1` is set on the node so
  `uv run` never strips those extras. Re-run `bootstrap` after changing
  `pyproject.toml`.
- Full SSH/scp/rsync/VS Code access guide: `northflank/SSH.md`.

### Fresh next-token pretraining

The pure-CE study uses the live four-GPU node named in `northflank/SSH.md`,
not the capacity-queued default eight-GPU service. It trains four cells at a
time in two waves and writes resumable artifacts to the shared `/cache`
volume:

```bash
cd northflank
export NF_PROJECT=fda-test-e1 NF_SERVICE=fda-node4
./nf.sh status
./nf.sh bootstrap                  # requires WANDB_API_KEY in the local env
./nf.sh test
./nf.sh pretrain-plan
./nf.sh pretrain-prepare
./nf.sh pretrain-preflight
./nf.sh pretrain-launch
./nf.sh pretrain-status
./nf.sh pretrain-logs
```

The eight cells are the Cartesian product of effective batches
`{2048, 4096}` and base learning rates
`{5e-6, 1e-5, 1.5e-5, 2e-5}`. Every cell sees exactly 4,194,304
FineWeb-Edu contexts and trains the randomly initialized, tied vocabulary
matrix together with the d4/r2407 Kronecker body. No teacher or teacher cache
is loaded. The coordinator ranks by full-validation next-token cross-entropy,
evaluates only the winner on test, and uploads the winner artifact to W&B.

### μP-free NorMuon restart

The active replacement study lives in the self-contained
`qwen_normuon_pretrain` package and does not import the historical training
stack. Every one of the 57,768 rank-slice factor matrices receives an
independent NorMuon update. The tied vocabulary matrix, normalization
weights, gains, mixing vectors, and biases use auxiliary fused AdamW.

The selected default is NorMuon LR `3e-3` for every factor-matrix slice and
auxiliary AdamW LR `3e-4`. This default is recorded in
[`normuon_default.json`](normuon_default.json) and was selected by the
matched step-64 comparison in
[`NORMUON_ADAMW_RESULTS.md`](NORMUON_ADAMW_RESULTS.md).

For each factor tensor, every leading-index slice
\(W_j\in\mathbb{R}^{m\times n}\) has independent states \(M_j\) and
\(V_j\):

```text
M_j <- beta1 M_j + (1 - beta1) G_j
D_j <- beta1 M_j + (1 - beta1) G_j              # Nesterov direction
X_j <- bf16(D_j) / (||D_j||_F + 1e-7)
transpose X_j when m > n
repeat 5 times:
    A_j <- X_j X_j^T
    X_j <- 3.4445 X_j + (-4.7750 A_j + 2.0315 A_j^2) X_j
transpose X_j back
V_j <- beta2 V_j + (1 - beta2) mean_columns(X_j^2)
U_j <- X_j / (sqrt(V_j) + 1e-10)
U_j <- U_j * ||X_j||_F / (||U_j||_F + 1e-10)
U_j <- U_j * sqrt(max(1, m / n))
W_j <- (1 - lr * 0.01) W_j - lr U_j
```

Here `beta1 = beta2 = 0.95`. Equal-shaped slices are placed in one batched
Newton–Schulz kernel, but no momentum or row-moment state is shared between
matrices. WSD multiplies both the direct NorMuon LR and the auxiliary AdamW
LR; it introduces no width-dependent LR or initialization scaling.

The first stage screens direct NorMuon learning rates
`{1e-3, 3e-3, 1e-2, 2e-2}` for 262,144 contexts at batch 2,048. The best two
learning rates restart from seed zero and run at batches `{2048, 4096}` for
4,194,304 contexts per cell. All four cells in either stage run concurrently
on `fda-test-e1/fda-node4`.

```bash
cd northflank
export NF_PROJECT=fda-test-e1 NF_SERVICE=fda-node4
./nf.sh normuon-plan
./nf.sh normuon-prepare
./nf.sh normuon-preflight
./nf.sh normuon-launch
./nf.sh normuon-status
./nf.sh normuon-logs
```

Artifacts are written to `/cache/qwen_normuon_pretrain/v1`; online runs use
the W&B project `qwen-normuon-next-token-pretrain`. The abandoned study is
retained at
`/cache/qwen_fullwidth_distill/archive/next-token-pretrain-v1-mup-aborted-20260727`.

### Winner-matched AdamW control

The detached AdamW control matches the completed 128-step NorMuon screen
winner's architecture, initialization hashes, seed-zero data order, batch
2,048, 262,144-context budget, complete 8/108/12 WSD schedule, clipping, and
full-validation protocol. The auxiliary path remains AdamW at `3e-4`; only
the 57,768 factor matrices change from NorMuon to fused AdamW at
`3e-4`, with betas `(0.9, 0.95)`, epsilon `1e-10`, and weight decay `0.01`.

Before training, the queue executes an exact accumulated H100 step and
requires nonzero factor and embedding gradients plus peak memory at or below
92%. Checkpoints contain the model and both optimizer states. The final
`adamw-control/comparison.json` records matched metadata, paired validation
metrics, wall time, W&B links, and signed AdamW-minus-NorMuon deltas. W&B
also receives a two-row comparison table and the AdamW weights.

```bash
cd northflank
export NF_PROJECT=fda-test-e1 NF_SERVICE=fda-node4
./nf.sh adamw-queue
./nf.sh adamw-status
./nf.sh adamw-logs
```

## Modal stages

Every teacher/student run uses an H100. The single remote training function is
capped at eight containers, so parallel waves respect the account-wide
eight-GPU limit.

```bash
uv run modal run modal_app.py --stage tests
uv run modal run modal_app.py --stage prepare
uv run modal run modal_app.py --stage smoke
uv run modal run modal_app.py --stage tensor-smoke
uv run modal run modal_app.py --stage tensor-invariants
uv run modal run modal_app.py --stage kron-rank-smoke
uv run modal run --detach modal_app.py --stage kron-rank
uv run modal run --detach modal_app.py --stage kron-rank-control
uv run modal run --detach modal_app.py --stage kron-rank-boundary
uv run modal run --detach modal_app.py --stage kron-rank-batch-boundary
uv run modal run --detach modal_app.py --stage kron-rank-batch-lr-boundary
uv run modal run --detach modal_app.py --stage kron-rank-batch-lr-continue
uv run modal run modal_app.py --stage kron-rank-chunk-benchmark
uv run modal run modal_app.py --stage kron-rank-compile-benchmark
uv run modal run modal_app.py --stage kron-rank-fullstep-benchmark
uv run modal run -d modal_app.py --stage kron-rank-resume-after-spend
uv run modal run -d modal_app.py --stage kron-rank-next-eight
uv run modal run --detach modal_app.py --stage kron-rank-continue
uv run modal run --detach modal_app.py --stage kron-rank-monarch-decay
uv run modal run --detach modal_app.py --stage kron-rank-monarch-capacity
uv run modal run --detach modal_app.py --stage kron-rank-monarch-batch
uv run modal run --detach modal_app.py --stage kron-rank-monarch-batch-b128
uv run modal run --detach modal_app.py --stage kron-rank-monarch-batch-b64
uv run modal run --detach modal_app.py --stage kron-rank-monarch-batch-b32
uv run modal run --detach modal_app.py --stage kron-rank-monarch-batch-transition
uv run modal run --detach modal_app.py --stage kron-rank-monarch-capacity-continue
uv run modal run --detach modal_app.py --stage kron-rank-monarch-capacity-checkpoint
uv run modal run --detach modal_app.py --stage kron-rank-monarch-capacity-depth-scaled
uv run modal run --detach modal_app.py --stage kron-rank-monarch-decay-continue-5e4
uv run modal run --detach modal_app.py --stage kron-rank-monarch-decay-continue-5e4-to-2p5e4
uv run modal run modal_app.py --stage screen
uv run modal run --detach modal_app.py --stage study
uv run modal run modal_app.py --stage audit
uv run modal run --detach modal_app.py --stage depth
uv run modal run modal_app.py --stage depth-audit
uv run modal run --detach modal_app.py --stage tensor
uv run modal run modal_app.py --stage tensor-audit
uv run modal run modal_app.py --stage prepare-edu
uv run modal run --detach modal_app.py --stage prepare-teacher-cache
uv run modal run --detach modal_app.py --stage kron-edu-optimizer
uv run modal run --detach modal_app.py --stage kron-edu-objective
uv run modal run --detach modal_app.py --stage kron-edu-architecture
uv run modal run --detach modal_app.py --stage kron-edu-scale-262k
uv run modal run --detach modal_app.py --stage kron-edu-scale-1m
uv run modal run --detach modal_app.py --stage kron-edu-scale-4m
uv run modal run --detach modal_app.py --stage kron-edu-push
```

`study` performs the fixed-LR screen, waits for it, selects common and
unrestricted Monarch topologies using validation KL, runs the LR refinements,
waits again, and then runs three-seed finals. Training artifacts live under
`/cache/qwen_fullwidth_distill` on the `fda-cache` Modal volume. Metrics are
logged to the `qwen-fullwidth-monarch-distill` W&B project.

Completed trials are reused only when their exact label, architecture, LR,
seed, step budget, finite validation result, and embedding hash match. Final
cache hits additionally require test metrics and an atomically completed
checkpoint. This makes a detached coordinator safe to relaunch without
repeating committed work. The coordinator reads those committed results
directly from its volume mount; it does not allocate H100 containers for cache
hits, and dispatches only genuinely missing trials.

Final trials also commit an atomic model progress checkpoint every 1,000
steps. A retry or coordinator relaunch restores the exact student weights,
AdamW moments, optimizer step, and deterministic data cursor from that point.
It also preserves the true step-0 validation metrics and accumulated training
wall time. The result and W&B config record `resumed_from_step` and whether
optimizer state was restored. The progress file is removed only when the
final result and `student.pt` are complete. Training inputs allow three
automatic retries, all sharing this committed progress, while the global
training-function cap remains eight H100 containers.

The coordinator persists `study-plan.json` before finals and
`study-summary.json` after all finals. The `audit` stage verifies the complete
23-screen + 9-tuning + 12-final grid directly on the Modal volume, including
validation-only selection, test-split isolation, the frozen embedding hash,
checkpoint metadata, and checkpoint parameter counts. It also checks that
every trial has a matching finished W&B run with optimizer, performance,
gradient-diagnostic, validation, and (for finals) test summaries.

FineWeb documents are hash-assigned wholly to train, validation, or test.
Each document contributes up to 32 deterministic non-overlapping 17-token
windows, so splits remain document-disjoint while examples are approximately
token-weighted and corpus preparation stays practical.

The exact screen is:

- Dense: sequential 1/2, one-map residual 1/2, unexpanded two-map residual
  1/2, and one expanded two-map residual block.
- Monarch: all four topology families at depths/blocks 1/2/4/8.

The objective is temperature-1, full-tokenizer
`KL(P_teacher || P_student)`. AdamW uses LR `1e-3`, betas `(0.9, 0.999)`,
epsilon `1e-8`, weight decay `0.01`, and no scheduler or clipping. LR
refinement tests `3e-4`, `1e-3`, and `3e-3`.

Screen and tuning trials run for 1,000 optimizer steps at effective batch
1,024. The four validation-selected finalists run for 4,000 steps for each of
seeds 0, 1, and 2, producing twelve `student.pt` checkpoints and held-out test
metrics.

## Monarch depth and tied-cycle follow-up

The `depth` stage is a separate resumable study and does not alter the
baseline grid above. It compares rank-1, expansion-4 residual Monarch stacks
at effective depths 4/8/16/32. Untied stacks physically contain that many
layers. Tied stacks physically contain the winning four-layer stack and
repeat those exact modules 2/4/8 times, so all tied variants retain exactly
84,279,296 trainable parameters at effective depths 8/16/32.

The primary tied variants multiply every residual update by the inverse loop
count. One effective-depth-16 diagnostic repeats the full residual update
without damping. The screen tests constant learning rates `3e-4` and `1e-3`
for 1,000 steps and reuses three exact committed baseline cells, leaving
twelve new H100 runs.

The best structural untied and damped-loop configurations then enter an
eight-cell fixed-data optimization screen. Each architecture tests effective
batch 512 with LR `5e-4`/`1e-3` for 2,048 steps and batch 256 with LR
`3e-4`/`6e-4` for 4,096 steps. Every cell therefore sees exactly 1,048,576
examples while smaller batches receive more AdamW updates. The best
architecture/LR/batch combination in each family receives a three-seed final
with exactly 4,096,000 examples: 4,000 steps at batch 1,024, 8,000 at batch
512, or 16,000 at batch 256. `depth-study-plan.json` and
`depth-study-summary.json` make the complete 3-reference + 12-depth-screen +
8-optimization + 6-final grid independently auditable. The frozen
embedding/head, context 16, full width 16,384, full-vocabulary forward KL,
AdamW betas/epsilon/weight decay, and lack of an input bottleneck remain
unchanged.

## BTT and context-aligned Kronecker depth follow-up

The `tensor` stage starts only after the depth study passes its artifact audit.
It replaces each learned full-width map with the Block Tensor-Train (BTT)
operator from [Compute Better Spent](https://arxiv.org/abs/2406.06248), using
the authors' [reference contraction and structure-aware optimizer
scaling](https://github.com/shikaiqiu/compute-better-spent). It does not add an
input stem or bottleneck: activations remain width 16,384 throughout.

Before allocating the full grid, the coordinator runs a mandatory H100
invariant check proving that Qwen's input and output weights are tied, the
student lookup and head use a bitwise-identical copy of those weights, head
logits agree within BF16 GEMM tolerance, the single copied buffer is frozen
and hash-stable after backward, and it is excluded from the trainable
checkpoint. A controlled 262,144-example BTT gate then compares
three LR cells against a Monarch run with identical data order, seed, batch,
and budget. At least one BTT cell must halve its initial KL, finish within
`1.0` KL of the control, keep activation RMS growth below `10×`, and preserve
the embedding hash. Failure stops the study before the expensive frontier.

The padding-free, context-preserving factorizations are:

- Three cores: `(32, 16, 32)` at width 16,384 and `(64, 16, 64)` at
  expansion width 65,536.
- Four cores: `(16, 8, 8, 16)` at width 16,384 and
  `(16, 16, 16, 16)` at expansion width 65,536.

Every core uses the paper's maximum-RMS weight normalization with a learned
scalar gain. Residual updates are scaled by `1 / sqrt(depth)`. The LR probe
tests structure-aware AdamW base LRs `1e-5`, `3e-5`, `1e-4`, `3e-4`, `1e-3`,
`3e-3`, `6e-3`, and `1e-2`, plus uniform-LR controls at `3e-3` and `6e-3`;
if the winning stable LR lies on an edge, the coordinator extends that edge by
up to two additional probes. Expected probe divergence is recorded rather
than canceling the wave.

The parameter-matched frontier holds learned parameters near the roughly 84M
best Monarch reference while trading per-layer parameters for untied depth:

- Three-core rank-1 expansion-4 residual FFN, depth 11: 81,821,762 parameters.
- Four-core rank-1 expansion-4 residual FFN, depth 20: 85,852,320 parameters.
- Four-core rank-2 expansion-4 residual FFN, depth 7: 81,428,536 parameters.
- Four-core rank-1 gated residual, depth 35: 84,869,540 parameters.

The same study includes sums of order-three Kronecker products,
`Σ_r m_r (A_r ⊗ B_r ⊗ C_r)`. They use `(16, 32, 32)` modes at width 16,384
and `(16, 64, 64)` at expansion width 65,536. Thus every `A_r` is an explicit
learned context mixer and the other factors act on channel axes, while the
flatten/reshape ordering stays exact. The sum is initialized with
`m_r = 1 / sqrt(rank)` and never materialized as a dense matrix. The CUDA
path contracts one factor at a time with reusable cuTENSOR plans, performs
shrinking modes first, and fuses rank mixing into the final factor so the
last full-width activation is not rank-expanded. Custom autograd executes the
matching factorwise backward contractions. Modal training uses rank chunks of
1,024 through effective batch 128 and 512 above it, plus a measured
physical-microbatch ceiling of 352; for any fixed scientific effective batch
it automatically chooses the largest divisor below that ceiling. This layout
generalizes to a future context length `L` as `(L, 32, 32)`.

On an H100, the exact depth-4/rank-2407 84,272,352-parameter stack sustains
43.60 examples/s at physical batch 352 while allocating 64.56 GiB. Batch 384
still fits (69.80 GiB) but makes cuTENSOR select a slow contraction path and
drops to 16.55 examples/s; batches 432 and 448 OOM. The runtime therefore
targets the highest measured fast point rather than allocation alone. Runs
with the quality-selected effective batch 32 necessarily execute at physical
batch 32 unless that optimization hyperparameter is deliberately changed.

The parameter-matched Kronecker ablation deliberately trades rank per layer
for untied depth:

- Depth 32, rank 291: 84,271,872 trainable parameters.
- Depth 64, rank 140: 84,350,976 trainable parameters (the primary balance).
- Depth 128, rank 64: 83,951,616 trainable parameters.

Live measurements and W&B run links are summarized in
[`KRONECKER_RANK_RESULTS.md`](KRONECKER_RANK_RESULTS.md).

The separate `kron-rank` early grid runs all three allocations at
structure-aware AdamW base LRs `1e-6` and `3e-6` for 65,536 examples. A
full-architecture smoke test rejected `1e-5` after its first update. The
formal probe on the primary depth-64/rank-140 allocation covers `1e-6`,
`3e-6`, `1e-5`, `3e-5`, and `1e-4`, with uniform controls at `3e-4`, `1e-3`,
and `3e-3`. Exact model and AdamW progress is saved every 16,384 examples in
the probes and every 65,536 examples in subsequent screens and finals.

A separate H100-time frontier benchmarks complete teacher + full-vocabulary
KL + student + AdamW steps, then binary-searches the deepest feasible version
of each family within 105% of the selected Monarch reference step time and
below 75 GiB peak allocation. Both frontiers use the two best stable LR
settings for exactly 1,048,576 examples. The best parameter-matched BTT, best
parameter-matched Kronecker, and best time-matched cell each receive a
three-seed, 4,096,000-example final, with duplicate cells reused.

The better parameter-primary finalist is then warm-started with its exact
AdamW state for a resumable long run. It uses deterministic additional passes
over the document-disjoint FineWeb training split, stops only when the complete
8,192-example validation split confirms KL at or below `1.0`, and otherwise
continues to the 32,768,000-example ceiling. Artifacts record whether the
target was reached and the remaining KL gap.

## FineWeb-Edu optimization and higher-order screen

The current `kron-edu-push` coordinator starts only from a completed
8,192-row full-validation winner. It first materializes the pinned teacher's
final 1,024-wide hidden state for all 16,793,600 FineWeb-Edu examples as
revision-, embedding-, dataset-, and checksum-bound BF16 shards. Eight H100
workers build 65,536-row shards in parallel; interrupted builds resume from
identity-bound sidecars. Student logits are reconstructed with the same
frozen copied tied embedding/unembedding, so no learned 1,024-wide stem is
introduced.

The next barriers are:

- Eight role-aware AdamW continuations for 262,144 examples, including
  factor/gain/mixing learning-rate and weight-decay policies.
- Eight 524,288-example objective continuations spanning normalized
  teacher-hidden MSE and temperature-2 KL mixtures.
- Eight fresh, exactly parameter-matched architecture screens between
  84.264M and 84.280M parameters: order-three controls, a learned gated
  second pass, gated residuals, order-four Kronecker FFNs at depths 8/16/32,
  an order-four gated residual, and a Monarch-boundary/Kronecker-interior
  hybrid.
- A validation-only successive-halving frontier: the best four architectures
  continue to 262,144 examples, the best two to 1,048,576, and the winner to
  4,194,304. Each boundary has a distinct resumable artifact identity.

Order four uses the padding-free modes `(16, 8, 8, 16)` at width 16,384 and
`(16, 16, 16, 16)` at width 65,536. The contraction orders modes by expansion
ratio and fuses learned rank mixing into the last factor; it never expands a
dense matrix and halves the largest rank-indexed intermediate for these FFN
maps. Every continuation retains exact AdamW moments, applies an
8,192-example warmup/hold ramp, and selects solely by completed
full-validation KL. One-step H100 gates precede the order-four screen.

All tensor runs log activation RMS by layer, BTT core or Kronecker factor RMS
and gain statistics, Kronecker mixing concentration/effective rank, effective
structured LRs, gradient/parameter norms, throughput, and H100 memory to W&B.
`tensor-invariants.json`,
`tensor-gate.json`, `tensor-study-plan.json`, `tensor-benchmark.json`, and
`tensor-study-summary.json` record the adaptive choices. `tensor-audit`
reconstructs the grid and checks the gates, selection, exact parameter counts,
benchmark constraints, frozen embedding hashes, optimizer-bearing final
checkpoints, long-run target accounting, test isolation, and matching
finished W&B runs.
