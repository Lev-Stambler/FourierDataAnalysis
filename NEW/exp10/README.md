# Experiment 10: causal architecture verdict

**Decision: stop pursuing the Kronecker architecture as a general causal
language model. The standard Transformer is now the executable default and the
locked reference.** The Kronecker code remains available only to reproduce the
completed matched comparison. See [RUN_RESULTS.md](RUN_RESULTS.md) and
[`transformer_reference.json`](transformer_reference.json).

Experiment 10 answers whether the complete Kronecker language model is worth
pursuing beyond the specialized 16-token Qwen-distillation setting. It uses a
strictly causal 256-token operator, a shared 16,384-token BPE, a TinyStories
gate, and two parameter-matched WikiText-103 scales.

## Models

The Kronecker position factor stores only the 32,896 entries on and below the
causal diagonal. There are no masked but counted future-position parameters.
Its multiplicative `128 × 128` vocabulary is part of the whole-model design.
The control is a conventional RoPE/SwiGLU Transformer with a tied dense
embedding/head. A deterministic solver chooses the Transformer FFN width.

| Scale | Kronecker | Transformer | Difference |
|---|---:|---:|---:|
| small | 4,369,408 | 4,360,192 | 0.211% |
| large | 17,773,568 | 17,809,408 | 0.202% |

Both models predict every next token in each 256-token sequence. Tests enforce
prefix invariance and compare cached Transformer decoding with a full causal
forward pass.

## Data preparation

Run data preparation on a CPU service before allocating the paid GPU node:

```bash
EXP10_ROOT=/cache/exp10 exp10/prepare_cpu.sh
```

The tokenizer sees training text only from TinyStories and WikiText-103. The
TinyStories gate uses 250M prediction tokens plus disjoint validation/test
halves. WikiText preserves the official train/validation/test splits and uses
one complete training-corpus pass. Checksums cover only the tokenizer and the
three split files; they detect accidental cache changes without per-example
hashing.

## Paid campaign

Bootstrap an eight-H100 Northflank service, sync the repository and prepared
data, then run:

```bash
exp10/run_h100.sh
```

The preflight checks all eight H100s, W&B authentication, finite optimizer
state, and searches down from an ambitious physical batch. It benchmarks the
first stable batch and two smaller candidates on both architectures, recording
throughput, VRAM, power and utilization before choosing the best common batch.
Evaluation defaults to 512 windows per GPU. Completed preflight and campaign
cells are reused, so an interrupted campaign resumes without repeating paid
work.

The preflight also measures compilation amortization. At this one-pass corpus
budget, large-model graph compilation takes minutes per independent tuning cell
but saves only seconds of training, so campaign cells use the measured eager
path. This is faster end-to-end; compiled throughput remains recorded from the
TinyStories runs for reference.

TinyStories first brackets each architecture's distinct learning-rate scale.
The coordinator then screens six values around each bracketed optimum over 10%
of a corpus pass, promotes the best two to 30%, then trains the
locked recipe for seeds 0, 1 and 2 over one full token budget. The test split
is evaluated only after all six locked finals exist.

## Verdict

`/cache/exp10/runs/comparison.json` contains paired test-NLL differences,
bootstrap intervals, scaling behavior, training throughput, VRAM, 32/64/128/256
prefix-position NLL, prefill latency and natural decoding speed. Transformer
decoding uses a KV cache; the fixed-window Kronecker model must recompute its
rolling 256-token window.

The audit emits one of:

- `pursue_general_causal_lm`
- `pursue_fixed_window_mixer_only`
- `stop_pursuing`
- `ambiguous_requires_web_confirmation`

FineWeb-Edu is deliberately deferred unless the middle-ground TinyStories and
WikiText evidence remains promising.
