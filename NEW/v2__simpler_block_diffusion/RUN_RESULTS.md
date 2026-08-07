# V2-SBD run results

Status: implementation and paid eight-H100 systems preflight complete; long
learning has not started.

## Local evidence

- Frozen parameter formula: 97,832,320.
- Reference/unit suite: 19 passed locally and on the H100 node.
- Data manifest: six revision-locked, redistributable sources.
- Handoff artifacts: `artifacts/batch-sweep-v2/selection.json` and
  `artifacts/real-cache-preflight-b56/`.

## Paid attempt 1: tokenizer guard stop

- Run: [v2-sbd-correctness-b1](https://wandb.ai/lev-tear-tear-labs/v2-simpler-block-diffusion/runs/y0hf5fx0)
- Hardware: eight H100-80GB GPUs; all idle before launch.
- Outcome: stopped before model loading or an optimizer step.
- Cause: the preflight compared the tokenizer's 151,643-token base vocabulary
  property with the model head's 151,936 rows. The pinned tokenizer actually
  has 151,670 entries after added tokens and mask ID 151,669; the remaining
  head rows are not tokenizer entries. The corrected guard checks the base,
  full tokenizer, mask ID, and 151,936-row model head separately.

No architecture-quality or throughput claim is supported by this stopped
attempt.

## Paid attempt 2: BF16 conditioner stop

- Run: [v2-sbd-correctness-b1-r2](https://wandb.ai/lev-tear-tear-labs/v2-simpler-block-diffusion/runs/j5xor91x)
- Hardware: eight H100-80GB GPUs with one frozen teacher replica per GPU.
- Progress: the pinned teacher loaded on all ranks, accepted the packed
  dual-stream mask, and produced online selected-position targets.
- Outcome: stopped before backward because FP32 sampled mask rates produced an
  FP32 sinusoidal embedding for a BF16 conditioner linear.
- Correction: cast the deterministic sinusoidal features to the conditioner's
  learned weight dtype and cover BF16 model plus FP32 rates in the test suite.

This attempt supports teacher-path compatibility only; it contains no finite
optimizer step or throughput evidence.

## Paid attempt 3: inference-tensor boundary stop

- Run: [v2-sbd-correctness-b1-r3](https://wandb.ai/lev-tear-tear-labs/v2-simpler-block-diffusion/runs/icub2gn1)
- Progress: completed packed teacher and BF16 student forwards and entered the
  exact grouped loss.
- Outcome: stopped before backward because custom autograd cannot save target
  tensors created inside PyTorch inference mode.
- Correction: inference mode now ends at the frozen teacher hidden-state
  boundary; selected hidden states and grouped targets are cloned as ordinary
  detached tensors before entering student autograd.

No optimizer or throughput claim is made from this attempt.

## Paid attempt 4: Hub enumeration stop

- Run: [v2-sbd-correctness-b1-r4](https://wandb.ai/lev-tear-tear-labs/v2-simpler-block-diffusion/runs/e01zjvnx)
- Progress: passed the previous inference-tensor boundary and began live data
  acquisition on all ranks.
- Outcome: stopped manually before an optimizer step when eight unauthenticated
  workers independently enumerated Dolma 3's large repository and hit Hub 429
  retries. GPUs were idle during the retry window.
- Correction: correctness and compute-only batch probes now use an explicitly
  labeled deterministic synthetic stream. Corpus runs require a rank-zero
  resolved shard manifest so workers open assigned parquet shards directly and
  never enumerate a dataset repository concurrently.

Synthetic probes are not corpus or model-quality evidence.

## Paid attempt 5: end-to-end correctness pass

- Run: [v2-sbd-correctness-b1-r5](https://wandb.ai/lev-tear-tear-labs/v2-simpler-block-diffusion/runs/ykd7whyk)
- Hardware: eight H100-80GB GPUs, eight-way DDP, one frozen BF16 teacher
  replica per GPU.
- Completed one finite online teacher/student optimizer step, exact grouped
  loss backward, DDP synchronization, checkpoint, result, and audit writes.
- Global batch: 8 contexts, 8,499 supervised target tokens, 16,384 clean
  tokens, and 32,768 dual-stream model tokens.
- Grouped KL `0.191726`, hard NLL `11.972620`, peak allocated/reserved
  `16.704/16.943 GiB` per GPU.
- The one-step wall-clock rate was 2,378 target tok/s, including synchronized
  startup. It is not a steady-state throughput measurement.

This passes the small correctness gate but fails the required 100,000-target-
token global batch floor. The physical-batch sweep is therefore mandatory
before any learning run.

## Physical-batch sweep

The eight-way DDP sweep tested physical batches 64, 56, 48, 40, 32, 24, 16,
12, and 1 with no gradient accumulation. Batch 64 OOMed in the exact streamed
loss ([W&B](https://wandb.ai/lev-tear-tear-labs/v2-simpler-block-diffusion/runs/64mw6brc)).
Batch 56 was the largest stable configuration and completed five steps:

- [W&B run](https://wandb.ai/lev-tear-tear-labs/v2-simpler-block-diffusion/runs/3mst27s9)
- 448 global contexts and 474,513 target tokens on the final step
- 40,587 target tokens/s, 95.47% mean utilization, 100% minimum-rank median
- 75.27/77.08 GiB peak allocated/reserved per GPU
- teacher time fraction 89.87%

Steady batch 1 reached 23,144 target tokens/s, so the measured batching gain
was only 1.75x, not the desired 10x. The teacher is already the throughput
ceiling; this unmet aspirational target is retained as negative evidence.

## Real-data input attempts

- [Attempt r1](https://wandb.ai/lev-tear-tear-labs/v2-simpler-block-diffusion/runs/ihsphiqf)
  rejected Hugging Face's loader because explicit URLs were still converted to
  Hub filesystem discovery and rate-limited. Direct JSONL/Parquet readers now
  bypass the API.
- [Attempt r2](https://wandb.ai/lev-tear-tear-labs/v2-simpler-block-diffusion/runs/varrdcf1)
  was stopped before an optimizer step when live tokenization produced lasting
  rank skew. Paid real-data runs now require persistent per-rank token caches.
- The data-only gate produced one 2,048-token context from every locked source.
  SmolLM Python-Edu was rejected because it contained repository indexes but
  no source text, and its unchanged 10% weight was replaced by pinned ODC-BY
  FineMath 4+.

## Paid real-cache preflight pass

- Run: [real-cache-preflight-b56](https://wandb.ai/lev-tear-tear-labs/v2-simpler-block-diffusion/runs/wlbekvnu)
- Completed five finite online-teacher/student steps with eight-way DDP,
  optimizer updates, checkpoint, result, audit, and direct W&B URL.
- Final global batch: 448 contexts, 471,883 target tokens, 917,504 clean tokens,
  and 1,835,008 dual-stream model tokens per step.
- Final throughput/utilization: 40,384 target tokens/s, 95.38% mean GPU
  utilization, and 100% minimum-rank median utilization.
- Peak allocated/reserved: 75.26/76.75 GiB per GPU; teacher time fraction
  89.86%; grouped KL 2.9204 and hard NLL 11.8142 on the fifth step.

The expedited smoke cache reused rank 0's real examples for five ranks whose
distinct cache build was slow. It is valid systems-path evidence but not
learning-quality evidence. A full run requires the default distinct 16,384
contexts/rank cache.

## Fresh exact-full-KL/Muon systems preflight

- Batch 96 OOM: [W&B](https://wandb.ai/lev-tear-tear-labs/v2-simpler-block-diffusion/runs/mjjqloy2).
- Batch 88 OOM: [W&B](https://wandb.ai/lev-tear-tear-labs/v2-simpler-block-diffusion/runs/xby2yiga).
- Eager batch 80 pass: [W&B](https://wandb.ai/lev-tear-tear-labs/v2-simpler-block-diffusion/runs/m1wyns3u), 163,180 supervised targets/update and 73.93/74.28 GiB peak allocated/reserved.
- Compiled batch 80 pass: [W&B](https://wandb.ai/lev-tear-tear-labs/v2-simpler-block-diffusion/runs/q8g4v4ef), three finite updates, 163,636 supervised targets/update, 10,628 targets/s, 93.33% mean utilization, and 54.03/58.33 GiB peak allocated/reserved.

These runs use the real cached corpus and all eight H100s with no accumulation.
They validate exact-KL backward, factorized Muon state, compilation, checkpoint,
and direct W&B audit. Their random-head KL near `5.56` is not quality evidence.
