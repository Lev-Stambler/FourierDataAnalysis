# Experiment 12 run results

## Paid GPU v1 — 2026-08-02

Outcome: **large three-seed validation win, no final promotion**.

The 32-layer content-routed deep Kronecker candidate was substantially better
than the depth- and parameter-matched Transformer at the locked 10M-token
selection horizon. It passed every quality and parameter gate. The run stopped
because its measured throughput ratio was `0.3951748845`, narrowly below the
predeclared `0.4` systems gate. Per protocol, the controller did not spend the
additional 180M-token final budget and never loaded the test split.

- W&B: https://wandb.ai/lev-tear-tear-labs/exp12-deep-kronecker/runs/i7di0mjd
- Experiment ID: `exp12-deep-kronecker-gpu1-v1`
- Immutable launch-manifest SHA-256: `9e286c41603db94318af4cf638cdcf85404e845c18c7d166823e13e44ec5cad2`
- Accelerator: one full NVIDIA H100 80 GiB node, Northflank Netherlands
- Training data: locked 256-token WikiText arrays with a 16,384-token vocabulary
- Selection data: validation only
- Test opened: **no**
- Paid service after retrieval: paused and verified at `deployment.instances = 0`

## Matched comparison

Both architectures were tuned independently across AdamW and canonical Muon,
learning rates, constant versus token-based warmup/cosine schedules, auxiliary
AdamW rates for Muon, and multiple seeds. The 10M-token selection stage ran the
top two recipes for each architecture on seeds 0, 1, and 2.

| Metric | deep Kronecker rank 8 | matched Transformer |
|---|---:|---:|
| Depth / nonlinear updates | `32 / 64` | `32 / 64` |
| Body parameters | `4,313,408` | `4,456,512` |
| Total parameters | `6,410,560` | `6,553,664` |
| Body-parameter ratio | `0.9678888` | `1.0` |
| Winning optimizer | Muon | Muon |
| Winning body LR | `0.03` | `0.1` |
| Winning auxiliary AdamW LR | `0.003` | `0.002` |
| Winning schedule | constant | constant |
| Target tokens per seed | `10,000,000` | `10,000,000` |
| Actual tokens per seed | `10,027,008` | `10,158,080` |
| Validation NLL seed 0 | **`5.850284`** | `7.393556` |
| Validation NLL seed 1 | **`5.843149`** | `7.397866` |
| Validation NLL seed 2 | **`5.809621`** | `7.386935` |
| Mean validation NLL | **`5.834352`** | `7.392786` |
| Validation NLL standard deviation | `0.017728` | `0.004496` |

The paired Deep-Kronecker-minus-Transformer deltas were:

```text
seed 0: -1.5432717631
seed 1: -1.5547166960
seed 2: -1.5773134006
mean:   -1.5584339532
```

The Transformer received about `1.31%` more actual tokens because its larger
physical batch overshot the common 10M target by more. This makes the observed
validation advantage conservative with respect to token count; it does not
replace the sealed-test comparison.

The selected Transformer Muon recipe narrowly beat its AdamW finalist. Its
Muon mean was `7.392786`; AdamW at LR `0.01`, constant schedule, produced
`7.403653 / 7.401184 / 7.402512`. The selected Kronecker Muon recipe also beat
its warmup/cosine Muon finalist, whose validation NLLs were
`5.953563 / 5.939330 / 5.916735`.

## Promotion decision

| Locked condition | Limit | Observed | Result |
|---|---:|---:|---:|
| Mean paired validation NLL delta | `<= 0.05` | `-1.558434` | pass |
| Worst single-seed validation NLL delta | `<= 0.15` | `-1.543272` | pass |
| Kronecker/Transformer body-parameter ratio | `<= 1.0` | `0.967889` | pass |
| Kronecker/Transformer throughput ratio | `>= 0.4` | `0.395175` | **fail** |

Verdict recorded by the immutable controller:
`deep_kronecker_did_not_reach_final_promotion`.

This does **not** say that the model lost on quality. It says that this exact
implementation did not satisfy the combined quality-and-systems contract. The
miss was 0.004825 in ratio, or 0.48 percentage points. Changing the threshold
after seeing the result would be unscientific, so the v1 verdict is retained.

## Accelerator preflight

The batch search began at 2,048 examples and searched down after real OOMs. It
used no gradient accumulation. Finite forward, backward, and optimizer state
was checked for AdamW and Muon on both architectures.

| Model / optimizer | Selected batch | Global tokens/step | Mode | tok/s | Peak allocated / reserved | Sampled utilization | Power |
|---|---:|---:|---|---:|---:|---:|---:|
| Kronecker / AdamW | `384` | `98,304` | compiled | `390,205` | `48.27 / 48.32 GiB` | `76%` | `404.61 W` |
| Kronecker / Muon | `384` | `98,304` | compiled | `390,655` | `48.26 / 48.31 GiB` | `100%` | `396.68 W` |
| Transformer / AdamW | `640` | `163,840` | compiled | `978,429` | `53.47 / 58.48 GiB` | `97%` | `479.81 W` |
| Transformer / Muon | `640` | `163,840` | compiled | `988,563` | `53.45 / 58.46 GiB` | `82%` | `357.31 W` |

Compilation raised the stable Kronecker AdamW batch throughput from `153,526`
to `390,205 tok/s` (`2.54x`) and Transformer AdamW throughput from `524,923`
to `978,429 tok/s` (`1.86x`). Training traces settled near `391k tok/s` for
Kronecker and `990k tok/s` for Transformer.

The `98,304`-token Kronecker batch is below the repository's 100k target for the
separate 16-token distillation workload; this run is a 256-token WikiText
language-model workload. It was the highest-throughput stable physical batch
found after 512 and larger OOMed.

## Compute and artifacts

- Unique trained-token total across all successive-halving cells:
  `233,046,016` prediction tokens.
- Campaign stage: `756.615` H100-seconds, `$0.575868` at the manifest rate.
- Allocation/bootstrap/data/preflight charge: `579.529` H100-seconds,
  `$0.441086`.
- Audited controlled total: `1,336.144` H100-seconds, `$1.016954`.
- Campaign wall time was about 12.6 minutes after stage start; the compiler and
  throughput preflight were included in the controlled accounting.

Retrieved raw artifacts, excluding bulky recoverable checkpoints, are at:

```text
/tmp/exp12-deep-kronecker-gpu1-v1/
```

The directory contains the immutable plan, ledger, audit, local correctness
gate, full preflight curve, campaign result, and every per-cell result. Key
SHA-256 values:

```text
audit.json       1cd004279aec1347dfddc5a2f462676735365c14e8285e3690b14be48d4e3daf
ledger.jsonl     14febcde54af42182bc0b39941950b9c8714997fb2c7283452fe5ba763aabfaf
plan.json        07dd72acca7a1447d6a120b85518b4066d60961dace9883c6fdb3fba61afbd09
result.json      c037154fb836e340a844c80cb476e503530274076fec47a8b20a1aa652b4722c
preflight.json   7c32c8428f5c85566981a5e8209a8085fd57af190860e11e1cfee44e650b50cb
```

## Scientific interpretation

This is the first result in the series that strongly supports pursuing the
deeper Kronecker direction on optimization quality: the advantage is large,
consistent across three seeds, survives independent tuning, and occurs with a
slightly smaller body. It is still a pilot at one scale and one corpus, and its
test set was intentionally sealed.

The immediate bottleneck is implementation efficiency, not evidence that the
architecture cannot learn. The next compute-smart step is a profiler-driven
kernel pass on the content-routed factored mixer, followed by a locked
throughput-only qualification. A new campaign should be authorized only after
that implementation clears the systems threshold without changing the model's
numerics. Then the 40M-token, sealed-test comparison can be run under a new
manifest.
