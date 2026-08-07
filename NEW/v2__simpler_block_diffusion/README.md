# V2 simpler block diffusion

The reusable H100-node preparation entry point is:

```bash
./scripts/prepare_h100.sh
```

It reuses the persistent Python/Inductor caches, stages immutable source shards
to local disk, builds disjoint held-out and training token caches, audits source
quotas and cross-rank duplicates, and runs the full test suite. Set
`V2_CACHE_CONTEXTS_PER_RANK=4096` for short LR screens or leave the production
default of 16,384 contexts/rank. `V2_CACHE_SKIP_CONTEXTS_PER_RANK` can advance
each rank past prior immutable caches; the 100M confirmation used `4112` to skip
16 held-out plus 4,096 screen contexts per rank.

This folder implements both the frozen grouped-tail experiment and its fresh
exact-full-KL successor `V2-SBD-FKL-Muon-r1`. See [SPEC.md](SPEC.md).

## Local verification

Use the repository environment, which already contains CUDA PyTorch:

```bash
PYTHONPATH=src ../.venv/bin/python -m pytest -q
PYTHONPATH=src ../.venv/bin/python -m v2_simpler_block_diffusion.cli parameter-count
PYTHONPATH=src ../.venv/bin/python -m v2_simpler_block_diffusion.cli \
  validate-manifest data_manifest.lock.json
```

The architecture notebook is served from the repository root:

```bash
uv run --no-sync python tools/serve_architecture.py
# http://127.0.0.1:8765/
```

## Eight-H100 execution

The runner refuses partial-node execution, a non-H100 node, active GPU compute
processes, or a missing W&B credential.

On the Northflank node, build or refresh the reusable lock-keyed environment:

```bash
cd /root/fda/NEW/v2__simpler_block_diffusion
./scripts/bootstrap_cached_env.sh
```

The environment lives at `/cache/v2_sbd/env`; unchanged invocations only
reinstall the tiny editable package. `Dockerfile` provides the equivalent
CUDA 12.8 base image for a baked deployment.

Resolve repository contents once, then build per-rank token caches while GPUs
are idle. The production default creates 16,384 contexts/rank, enough for the
initial 100M-target-token experiment without recycling:

```bash
./scripts/resolve_data_shards.sh
./scripts/build_token_cache.sh
```

Run the downward physical-batch sweep from an ambitious size. Eager batch 80
and 72 OOMed; corrected eager and compiled batch 64 are stable and batch 64 is
the selected 512-context full-node configuration:

```bash
WANDB_API_KEY=... ./scripts/batch_sweep.sh
```

Run an explicit eight-way preflight:

```bash
V2_MODE=preflight \
V2_MICROBATCH=56 \
V2_GRADIENT_ACCUMULATION=1 \
V2_RUN_NAME=v2-sbd-preflight-b56 \
./scripts/run_h100.sh --preflight-steps 10
```

`--synthetic-data` is permitted only for numerical correctness and isolated
compute/batch measurement. It is logged in W&B and is never treated as corpus
evidence.

Direct JSONL/Parquet readers bypass Hub repository discovery entirely. Paid
real-data launches require the revision-checked local token cache so network
and tokenization work cannot create rank skew in the training critical path.

After the sweep identifies the physical batch and the audit reaches at least
100,000 global target tokens per optimizer step, launch a selected LR screen
or training continuation by setting `V2_MODE=train`, `V2_TARGET_TOKENS`, and
`V2_LEARNING_RATE` explicitly.

For the fresh successor, first run the eight-way tied-head oracle, then use its
selected artifact in the exact-KL launch:

```bash
source /root/.fda_env
./scripts/run_head_oracle.sh

V2_MODE=preflight \
V2_RUN_NAME=fkl-muon-oracle-preflight \
V2_OUTPUT=/cache/v2_sbd/fkl-muon-oracle-preflight \
./scripts/run_fkl.sh \
  --oracle /cache/v2_sbd/head-oracle-r1/oracle-width256-rank0.pt \
  --preflight-steps 10
```

The selected 100M confirmation recipe is:

```bash
source /root/.fda_env
V2_MODE=train \
V2_MICROBATCH=64 \
V2_TARGET_TOKENS=100000000 \
V2_WARMUP_TARGET_TOKENS=10000000 \
V2_LEARNING_RATE=0.001 \
V2_KD_WEIGHT=0.8 \
V2_HARD_WEIGHT=0.2 \
V2_RESOLVED_SHARDS=/cache/v2_sbd/resolved-shards-stage0-local.json \
V2_TOKEN_CACHE=/cache/v2_sbd/token-cache-stage0-confirm-v1 \
V2_EVAL_TOKEN_CACHE=/cache/v2_sbd/token-cache-stage0-eval \
V2_RUN_NAME=confirm-100m-lr1e-3-b64 \
V2_OUTPUT=/cache/v2_sbd/confirm-100m-lr1e-3-b64 \
V2_CHECKPOINT_TARGETS=25000000,50000000,75000000 \
./scripts/run_h100.sh --compile --eval-every-target-tokens 10000000
```

Artifacts live below `/cache/v2_sbd/`. Each successful run writes `launch.json`,
`checkpoint/metadata.json`, `result.json`, and `audit.json`, all containing the
direct W&B URL and actual token counters.

## Generation

`generate_blocks` accepts one prompt tensor, commits complete prompt blocks,
fills each current block with four simultaneous highest-confidence commits per
step, and performs an exact clean commit before moving to the next block.
Primary evaluation is deterministic and uses eight steps per full block; the
same commit primitive generates DAgger states.

After a checkpoint is complete and the node is idle, evaluate all 128 fixed
held-out prompts across all eight GPUs and record a separate W&B audit run:

```bash
source /root/.fda_env
V2_CHECKPOINT=/cache/v2_sbd/confirm-100m-lr1e-3-b64/checkpoint \
V2_RUN_NAME=confirm-100m-generation-eval \
./scripts/run_generation_eval.sh
```

The evaluator stores every decoded completion and its exact degeneracy
diagnostics in `generation-eval.json`; the numerical criteria are frozen in
[`SPEC.md`](SPEC.md).
