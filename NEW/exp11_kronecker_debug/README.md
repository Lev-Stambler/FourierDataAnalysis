# Experiment 11: debug-first order-three Kronecker mixer

Experiment 11 reopens the architectural question from Experiment 10 because a
specific implementation defect was found in that comparison. Exp10 normalized
one packed causal triangle as a whole even though causal row fan-in grows from
1 to 256. Its initialized last-position branch RMS is about 18 times its first
position branch RMS. The corrected mixer normalizes each causal row separately.

This is a falsification campaign, not an authorization for a large run. The
locked hypothesis is that the corrected order-three operator mixes and uses its
representation well enough to match Transformer WikiText quality with at most
half as many trainable body parameters.

## Current outcome

The original v5 pilot was under-tuned and is superseded by independently tuned
AdamW v6 and canonical-Muon v8 campaigns. Both optimizer families used
three-seed 10M-token validation selection before matched 40M-token held-out
finals. Muon improves Transformer by `0.19916` mean test NLL but worsens the
order-3 model by `0.11058`.

Giving each architecture its selected better optimizer chooses AdamW for
order-3 rank-8 and Muon for Transformer. Order-3 is then worse by `+0.39208`
mean paired test NLL, with a paired bootstrap 95% interval of
`[+0.22431, +0.68247]`. It is also only `64.99%` as fast despite having
`99.05%` of Transformer's body parameters. This exact variant therefore does
not qualify for scale-up. See [`RUN_RESULTS.md`](RUN_RESULTS.md) and
[`paid_muon_v8_summary.json`](paid_muon_v8_summary.json) for the complete result
and audit.

## Canonical operator

For `H` shaped `T × d1 × d2`, each block is exactly:

```text
Z = SiLU(RMS(H))
H = RMS(H + alpha * sum_r g_r * Z ×1 A_r ×2 B_r ×3 C_r)
```

`A_r` is strictly causal including the diagonal and has unit L2 energy in every
row. `B_r` and `C_r` mix the two hidden modes. Radial scale gauges are removed
from every factor; `g_r` is the only learned rank amplitude. The implementation
has a separately materialized dense reference used only by correctness tests.

## Pilot models

All models use the same 256-token context, 16,384-token tied vocabulary,
128-wide hidden state, eight blocks, tokenizer, data splits, prediction-token
budgets, and validation/test procedure.

| Variant | Purpose | Body parameters | Total parameters |
|---|---|---:|---:|
| Exp10 replica, rank 8 | reproduce the defect | 3,153,920 | 5,251,072 |
| balanced order 2, rank 5 | isolate row balancing | 1,971,240 | 4,068,392 |
| canonical order 3, rank 4 | primary candidate | 1,062,944 | 3,160,096 |
| canonical order 3, rank 8 | rank-capacity ablation | 2,125,888 | 4,223,040 |
| RoPE/SwiGLU Transformer | standard control | 2,146,304 | 4,243,456 |

The primary comparison is quality at equal prediction tokens versus trainable
body parameters. Vocabulary parameters are also reported, never hidden. Rank-4
uses 49.53% of the Transformer's body parameters and 74.47% of its total
parameters.

## Hard gates and tuning

The manifest [`experiment.json`](experiment.json) is executed by the shared
research controller. Its immutable plan enforces a cumulative `$25` and one
GPU-hour pilot ceiling, exactly one visible GPU, mandatory W&B authentication
and direct URL capture, source/data checksums, a heartbeat, atomic result and
checkpoint publication, and exact-target Northflank pause on every exit path.

The paid run is one continuous stage so ephemeral services are paused only
after every result is copied. After charging 1,800 GPU-seconds for provisioning,
the first synthetic run, and infrastructure recovery, the final stage has 1,800
seconds left. Completed synthetic evidence is reused by checksum and W&B URL.
The sequence is:

1. Synthetic delayed-copy and associative-recall probes for all five variants.
2. An ambitious descending physical-batch sweep for every variant. Each stable
   batch records measured tokens/s, allocated/reserved VRAM, GPU utilization,
   power, token batch, and finite forward/backward/optimizer state. The
   highest-throughput stable batch wins; compiled and eager execution are
   measured separately, and gradient accumulation is disabled.
3. Three LRs per architecture to 2M tokens. Kronecker variants use
   `{0.003, 0.01, 0.03}` and Transformer uses
   `{0.0003, 0.001, 0.003}`.
4. Each architecture's validation winner resumes to 10M tokens. Scaling stops
   unless rank-4 improves at least `0.05` NLL over the Exp10 replica.
5. Rank-4 and Transformer alone run paired seeds 0/1/2 to 40M tokens. The test
   split is touched only after LR selection is locked.

Thus both primary architectures are tuned independently and compared at equal
tokens. Promotion requires the 95% paired bootstrap upper bound on
`NLL(order3) - NLL(transformer)` to be at most `0.02`, while the order-3 body is
at most half the size. If measured preflight throughput cannot fit the locked
290M-token campaign plus overhead in the remaining wall budget, the runner
records that fact and launches no tuning cells.

## Commands

Local correctness is free:

```bash
uv run --no-sync python -m research_control plan \
  exp11_kronecker_debug/experiment.json
uv run --no-sync python -m research_control run \
  exp11_kronecker_debug/experiment.json --stage local-correctness
```

The superseding tuned manifest and direct runner are:

```bash
uv run --no-sync python -m research_control plan \
  exp11_kronecker_debug/experiment_tuning.json
uv run --no-sync python -m exp11_kronecker_debug wikitext-tune \
  --output=/cache/research-control/exp11-optimizer-tuning-gpu1-v6/stages/optimizer-tuning/result.json \
  --data-root=/cache/exp10/data/wikitext
```

The completed canonical-Muon follow-up is reproducible through its locked
manifest:

```bash
uv run --no-sync python -m research_control plan \
  exp11_kronecker_debug/experiment_muon.json
uv run --no-sync python -m research_control run \
  exp11_kronecker_debug/experiment_muon.json --stage local-correctness
uv run --no-sync python -m research_control run \
  exp11_kronecker_debug/experiment_muon.json --stage muon-correctness
uv run --no-sync python -m research_control run \
  exp11_kronecker_debug/experiment_muon.json --stage muon-tuning
```

On the exact one-GPU Northflank service, set `WANDB_API_KEY`, `RC_NF_PROJECT`,
and `RC_NF_SERVICE`, sync the verified Exp10 WikiText artifacts to
`/cache/exp10/data/wikitext`, then execute the synthetic and WikiText stages in
order. `doctor` can be run before either paid stage. Result JSONs contain the
direct W&B URL; the controller audit lives under
`/cache/research-control/exp11-kronecker-debug-gpu1-v5/`.

No 255M-parameter run is authorized. The v6 parameter-matched gate failed, so
the next justified step is a small diagnostic or operator ablation. Any future
scale proposal must first produce a smaller frontier win and then train at
least 20 tokens per parameter.
