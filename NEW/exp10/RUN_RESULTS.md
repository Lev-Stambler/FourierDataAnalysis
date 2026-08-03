# Experiment 10 results

Status: complete. The eight-H100 campaign finished on 2026-07-31 with no
failed cells. Final audit verdict: **stop pursuing this architecture as a
general causal language model**.

## Reproducibility and utilization

- Shared tokenizer SHA256:
  `f724890961457d3ab4072010a64244ec857aab2e9f98fd38970c386d6951b38d`.
- TinyStories train budget: 250,000,128 prediction tokens. WikiText-103 train
  budget: 124,359,168 prediction tokens.
- Every comparison used eight NVIDIA H100 80GB GPUs, 512 windows/GPU, context
  256, and a 1,048,576-token global optimizer batch without gradient
  accumulation.
- Parameter match: small Kronecker 4,369,408 vs Transformer 4,360,192
  (0.211%); large Kronecker 17,773,568 vs Transformer 17,809,408 (0.202%).
- Both architectures were independently tuned. Each large-scale family
  screened six learning rates, promoted the two best candidates to the full
  token budget, and selected by validation NLL before three-seed final tests.
- Large-scale selected LRs: Kronecker 0.0512; Transformer 0.0016. Small-scale
  selected LRs: Kronecker 0.1024; Transformer 0.0064.

The batch preflight passed finite forward/backward/optimizer-state checks,
full 8-GPU participation, exact-loss checks, and held-out evaluation. At large
scale the Kronecker model sustained 2.84--2.85M train tokens/s and allocated
31.38 GiB/GPU; the Transformer sustained 4.30--4.33M tokens/s and allocated
26.02 GiB/GPU.

Preflight W&B runs:

- [Kronecker batch 512](https://wandb.ai/lev-tear-tear-labs/exp10-causal-architecture-verdict/runs/9ulktthf)
- [Transformer batch 512](https://wandb.ai/lev-tear-tear-labs/exp10-causal-architecture-verdict/runs/5ey7l21n)

## Matched held-out results

The NLL delta below is Transformer minus Kronecker, so a negative value favors
the Transformer. Confidence intervals are paired seed bootstrap intervals.

| Corpus / scale | Kronecker test NLL | Transformer test NLL | Mean delta (95% CI) | K train speed / T | K decode speed / T |
|---|---:|---:|---:|---:|---:|
| TinyStories / 4.36M | 5.9743, 4.7072, 5.7020 | 5.9638, 5.9631, 5.9635 | +0.5023 (-0.0105, +1.2559) | 0.813x | 2.111x |
| WikiText / 4.36M | 7.4110, 7.4063, 7.4089 | 7.4006, 7.3969, 7.3942 | -0.01151 (-0.01469, -0.00939) | 0.814x | 2.150x |
| WikiText / 17.8M | 7.4170, 7.4022, 7.4014 | 7.3966, 7.3917, 7.3922 | -0.01334 (-0.02037, -0.00916) | 0.660x | 0.931x |

TinyStories passed the preregistered continuation gate on its mean, but its
Kronecker result was highly seed-variable and its interval includes no win.
WikiText is the decision-bearing test: the Transformer wins all six paired
small/large seeds. At 17.8M parameters it is also 1.51x faster in training,
uses 5.36 GiB less allocated VRAM per GPU, and is 1.07x faster in the measured
incremental decode benchmark. The small model's Kronecker decode advantage did
not survive scale-up.

## W&B final runs

Project: [exp10-causal-architecture-verdict](https://wandb.ai/lev-tear-tear-labs/exp10-causal-architecture-verdict)

- TinyStories Kronecker: [seed 0](https://wandb.ai/lev-tear-tear-labs/exp10-causal-architecture-verdict/runs/cnmeb29x), [seed 1](https://wandb.ai/lev-tear-tear-labs/exp10-causal-architecture-verdict/runs/rcmsif7r), [seed 2](https://wandb.ai/lev-tear-tear-labs/exp10-causal-architecture-verdict/runs/46nr3ge2)
- TinyStories Transformer: [seed 0](https://wandb.ai/lev-tear-tear-labs/exp10-causal-architecture-verdict/runs/pheoxsxp), [seed 1](https://wandb.ai/lev-tear-tear-labs/exp10-causal-architecture-verdict/runs/2qqaj12n), [seed 2](https://wandb.ai/lev-tear-tear-labs/exp10-causal-architecture-verdict/runs/rxizaqft)
- WikiText small Kronecker: [seed 0](https://wandb.ai/lev-tear-tear-labs/exp10-causal-architecture-verdict/runs/tq0igvqs), [seed 1](https://wandb.ai/lev-tear-tear-labs/exp10-causal-architecture-verdict/runs/66bbra35), [seed 2](https://wandb.ai/lev-tear-tear-labs/exp10-causal-architecture-verdict/runs/uj78jetf)
- WikiText small Transformer: [seed 0](https://wandb.ai/lev-tear-tear-labs/exp10-causal-architecture-verdict/runs/rr5jm7os), [seed 1](https://wandb.ai/lev-tear-tear-labs/exp10-causal-architecture-verdict/runs/vl371j1k), [seed 2](https://wandb.ai/lev-tear-tear-labs/exp10-causal-architecture-verdict/runs/6ur072a1)
- WikiText large Kronecker: [seed 0](https://wandb.ai/lev-tear-tear-labs/exp10-causal-architecture-verdict/runs/v4uwiwtw), [seed 1](https://wandb.ai/lev-tear-tear-labs/exp10-causal-architecture-verdict/runs/599bralc), [seed 2](https://wandb.ai/lev-tear-tear-labs/exp10-causal-architecture-verdict/runs/r4u8b1ob)
- WikiText large Transformer: [seed 0](https://wandb.ai/lev-tear-tear-labs/exp10-causal-architecture-verdict/runs/fp673inb), [seed 1](https://wandb.ai/lev-tear-tear-labs/exp10-causal-architecture-verdict/runs/412eztsd), [seed 2](https://wandb.ai/lev-tear-tear-labs/exp10-causal-architecture-verdict/runs/214io74k)

The machine-readable final audit is `/cache/exp10/runs/comparison.json` on the
run volume. It reports `status: complete`, `tinystories_gate: pass`,
`verdict: stop_pursuing`, and an empty failure list.

## 255M Transformer reference follow-up

After rejecting Kronecker, the standard Transformer was scaled to 255,000,576
parameters (width 1024, 20 layers, 16 heads, SwiGLU width 2512) and run on the
same WikiText-103 corpus. The full 8-H100 preflight selected 512 windows/GPU:
1,048,576 global tokens/step, no gradient accumulation, 793.7--795.2K sustained
tokens/s, and 38.65 GiB peak allocated VRAM/GPU. All GPUs reached 100%
utilization during measured training.

Five LRs from 1e-4 through 1.6e-3 were screened independently. LRs 4e-4 and
8e-4 were promoted to 30% of the corpus; their validation NLLs were 7.39310 and
7.39841, selecting 4e-4 before the three locked full-budget seeds.

| Seed | Validation NLL | Test NLL | W&B |
|---:|---:|---:|---|
| 0 | 7.38819 | 7.39918 | [o8ptqkky](https://wandb.ai/lev-tear-tear-labs/exp10-causal-architecture-verdict/runs/o8ptqkky) |
| 1 | 7.38040 | 7.39289 | [n02ajf9t](https://wandb.ai/lev-tear-tear-labs/exp10-causal-architecture-verdict/runs/n02ajf9t) |
| 2 | 7.38178 | 7.39360 | [na757tsc](https://wandb.ai/lev-tear-tear-labs/exp10-causal-architecture-verdict/runs/na757tsc) |

Mean test NLL is 7.39522. This is statistically indistinguishable in practical
terms from the tuned 17.8M Transformer mean of 7.39350 and is 0.00172 worse.
At a one-pass 124M-token budget, the 255M model is severely data/optimization
limited; simply adding parameters does not improve the WikiText result. The
machine-readable follow-up audit is
`/cache/exp10/runs/wikitext/transformer-256m/audit.json`.
