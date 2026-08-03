# Experiment 11 run results

## Local correctness gate

Run on 2026-08-02 before any paid compute.

| Check | Observed | Gate |
|---|---:|---:|
| Factored vs materialized forward max error | `5.55e-16` | `≤ 1e-8` |
| Factored vs materialized backward max error | `2.22e-15` | `≤ 1e-8` |
| Future-to-past Jacobian max | `0` | `≤ 1e-12` |
| Exp10 replica position-balance ratio | `18.2951×` | `≥ 4×` reproduction |
| Canonical order-3 position-balance ratio | `1.24187×` | `≤ 2×` |
| Rank effective-use fraction | `0.992626` | `≥ 0.5` |
| Tiny identity overfit NLL | `0.000453551` | `< 0.001` |
| Finite, active gradients | yes | required |

Verdict: **pass**. This establishes a real Exp10 implementation bug and verifies
the corrected operator; it does not yet establish a language-model win.

## Tuned parameter-matched follow-up (v6)

Completed on 2026-08-02 on one NVIDIA H100 80GB HBM3. This follow-up
supersedes the optimizer conclusions from v5. The Northflank service
`fda-exp11-tune-us-central/gpu-h100-1` was paused after artifact retrieval and
verified at zero instances.

- Tuning and matched finals: [W&B run `br9xucjl`](https://wandb.ai/lev-tear-tear-labs/exp11-kronecker-debug/runs/br9xucjl)
- Controller experiment: `exp11-optimizer-tuning-gpu1-v6`
- Selection: lowest three-seed validation NLL after exactly `10,027,008`
  prediction tokens; the test split was not opened until both recipes were
  locked
- Final verdict: **`do_not_scale_yet`**

### Full optimizer tuning

Both architectures received the same tuning budget and selection rule. Each
LR/schedule recipe was run on seeds 0, 1, and 2. Order-3 rank-8 swept peak LRs
`{0.01, 0.03, 0.06, 0.12}` and Transformer swept
`{0.0005, 0.0015, 0.003, 0.006}`. Every LR was tested with constant and
2M-token-warmup cosine schedules. After finding each provisional winner, the
run also tested matrix weight decay `{0, 0.01}` and AdamW beta2 `{0.95, 0.99}`.
The winning recipes were interior to the LR grids, so no further boundary
expansion was required.

| Variant | Locked recipe | 10M validation NLL by seed | Mean +/- std. |
|---|---|---|---:|
| order 3, rank 8 | LR `0.06`, constant, WD `0.01`, beta2 `0.95`, clip `1.0` | `6.48711`, `6.51053`, `6.50197` | `6.49987 +/- 0.00967` |
| Transformer | LR `0.0015`, constant, WD `0.01`, beta2 `0.95`, clip `1.0` | `6.58918`, `6.59911`, `6.55925` | `6.58251 +/- 0.01694` |

The rank-8 model led validation by `0.08264` NLL at the selection horizon. This
did not persist to the longer held-out comparison. The old Transformer choice,
LR `0.003`, was unstable across seeds (`6.71499`, `6.73901`, `7.18718`), which
confirms that v5's single-seed boundary winner was not a legitimate tuned
baseline.

### Paid GPU preflight

The preflight verified finite forward, backward, and optimizer state in BF16,
used all requested hardware, used no gradient accumulation, and swept physical
batches downward from OOM at `2,048`, `1,536`, and `1,280`. Both models selected
compiled physical batch `768`, or `196,608` global tokens per optimizer step.

| Variant | Mode | Physical batch | Global tokens/step | Throughput | GPU util. sample | Peak alloc./reserved |
|---|---|---:|---:|---:|---:|---:|
| order 3, rank 8 | compiled | 768 | 196,608 | 1.372M tok/s | 96% | 46.37/46.79 GiB |
| Transformer | compiled | 768 | 196,608 | 2.129M tok/s | 96% | 45.49/45.58 GiB |

Adding the missing `setuptools` dependency repaired v5's order-3 compilation
failure. The final rank-8 runs averaged `1.379M` tok/s, versus `2.138M` tok/s
for Transformer: `64.50%` as fast at the same batch and token count.

### Matched 40M-token finals

The comparison matched trainable body parameters to `99.049%`: rank-8 had
`2,125,888` body parameters (`4,223,040` total), and Transformer had
`2,146,304` (`4,243,456` total). Every final used exactly `40,108,032`
prediction tokens with the recipe locked above. Lower NLL is better.

| Model | Seed | Test NLL | Validation NLL | Final train NLL | Throughput |
|---|---:|---:|---:|---:|---:|
| order 3, rank 8 | 0 | 5.69646 | 5.70900 | 5.73110 | 1.379M tok/s |
| order 3, rank 8 | 1 | 5.24367 | 5.24903 | 5.31871 | 1.380M tok/s |
| order 3, rank 8 | 2 | 5.25766 | 5.28142 | 5.31151 | 1.379M tok/s |
| Transformer | 0 | 5.20354 | 5.20321 | 5.22098 | 2.141M tok/s |
| Transformer | 1 | 5.22056 | 5.22152 | 5.29538 | 2.136M tok/s |
| Transformer | 2 | 5.19490 | 5.19504 | 5.22879 | 2.138M tok/s |

The paired test deltas `NLL(order3-r8) - NLL(transformer)` were
`[+0.49292, +0.02311, +0.06276]`; Transformer won every seed. Mean test NLL was
`5.39926` for rank-8 versus `5.20634` for Transformer, a mean paired gap of
`+0.19293`. The preregistered paired bootstrap 95% interval was
`[+0.02311, +0.49292]`, while promotion required its upper endpoint to be at
most `+0.02`. The quality gate therefore failed cleanly, without the collapsed
Transformer seed that confounded v5.

This is evidence against scaling this exact rank-8 order-3 architecture and
optimizer recipe. It does not distinguish whether the remaining problem is the
operator, conditioning, representation collapse, or optimization geometry; the
next justified work is a small diagnostic/ablation campaign, not a 255M-model
run.

### v6 audit and retained artifacts

The tuning stage took `608.585` GPU-seconds and cost `$0.46320`. Including the
conservative external charge covering prior work, current allocation,
bootstrap, and data construction, the audit records `3,331.922` GPU-seconds
and `$2.53596`. All controller stages completed with no failures. The immutable
controller manifest hash is
`8da542184a1a8607b616f75cc0e6df1943a323875f61851d98334e2de073e011`.

The retrieved artifacts live at
`/tmp/exp11-optimizer-tuning-gpu1-v6/`. Their SHA-256 hashes are:

| Artifact | SHA-256 |
|---|---|
| `stages/optimizer-tuning/result.json` | `544df5fe98d79047b6f13d709ef80c7998b06eb4b87015348a942d95dda6cf81` |
| `stages/optimizer-tuning/tuning-cells/preflight.json` | `a1e64bc7a0089198a5b3fb711d342757849494821c0c06c94e158de4e8b788ae` |
| `audit.json` | `e37bd64b523c62d1767f891e894c744f18a27e86832d64613c4ce170dd9f29f0` |
| `ledger.jsonl` | `1288bad035d845db8b6ba8f7ebd521593055dde6f0e4e6ec1c5e59d38276de7c` |

## Canonical Muon optimizer follow-up (v8)

Completed on 2026-08-02 on one NVIDIA H100 80GB HBM3. The campaign tested
whether the remaining order-3 deficit was an AdamW/optimization-geometry
artifact. The Northflank service
`fda-exp11-tune-europe-west-netherlands/gpu-h100-1` was paused after artifact
retrieval and verified at zero instances.

- Base tuning and finals: [W&B run `h0cbydty`](https://wandb.ai/lev-tear-tear-labs/exp11-kronecker-debug/runs/h0cbydty)
- Preregistered LR-boundary completion: [W&B run `163rz17g`](https://wandb.ai/lev-tear-tear-labs/exp11-kronecker-debug/runs/163rz17g)
- Controller experiment: `exp11-muon-tuning-gpu1-v8`
- Final verdict: **`muon_does_not_rescue_order3`**

The v8 four-round boundary-expansion plan was locked at 16:46:22 UTC, before
the base result was written at 16:50:33 UTC. The local preregistration and paid
plan are byte-identical (SHA-256
`a3f2cf3790f11c5ba2a7906a6bec9afd2f563059a4c3e1cf52a0c4acbfae7932`).
Thus the extra LR `2.7` check was specified before any base test result was
available, rather than selected after seeing the held-out comparison.

### Muon correctness and routing

The implementation applies canonical Muon independently to every hidden/body
matrix slice and AdamW to the tied vocabulary and vector rank amplitudes. Its
one-step update differed from PyTorch's Muon by at most `7.519e-4`, below the
locked `0.003` tolerance. Forward, backward, optimizer state, and the exact
parameter routing were all finite and passed before paid training.

### Independent optimizer tuning

Both architectures received the same three-seed, validation-only Muon search:
initial Muon LRs `{0.003, 0.01, 0.03, 0.1}`, constant versus 2M-token-warmup
cosine schedules, automatic 3x LR-boundary expansion, and auxiliary-AdamW LR,
weight-decay, and Muon-momentum ablations. Every screen used exactly
`10,027,008` prediction tokens. Optimizer family was then selected independently
for each architecture against the equally tuned AdamW v6 reference.

For order-3, the baseline-auxiliary boundary sequence improved through LR
`0.9` (`6.53370` mean validation NLL) but worsened at LR `2.7` (`6.56850`),
closing the boundary search without needing LR `8.1`. The auxiliary-LR
ablation then improved the final Muon selection to `6.51117`.

| Variant | Locked Muon recipe | Muon 10M validation NLL by seed | Mean | AdamW mean | Selected family |
|---|---|---|---:|---:|---|
| order 3, rank 8 | Muon LR `0.9`, auxiliary LR `0.03`, constant, WD `0.01`, momentum `0.95`, NS5 | `6.52644`, `6.49685`, `6.51022` | `6.51117` | `6.49987` | AdamW |
| Transformer | Muon LR `0.03`, auxiliary LR `0.003`, constant, WD `0.01`, momentum `0.95`, NS5 | `6.07246`, `6.06022`, `6.05594` | `6.06287` | `6.58251` | Muon |

### Paid GPU preflight

The paid preflight used the actual Muon-plus-AdamW optimizer, swept ambitious
physical batches downward from OOM, and selected compiled batch `768` for both
architectures: `196,608` global prediction tokens per optimizer step with no
gradient accumulation.

| Variant | Mode | Physical batch | Global tokens/step | Throughput | GPU util. sample | Peak alloc./reserved |
|---|---|---:|---:|---:|---:|---:|
| order 3, rank 8 | compiled | 768 | 196,608 | 1.342M tok/s | 92% | 46.36/46.79 GiB |
| Transformer | compiled | 768 | 196,608 | 2.085M tok/s | 94% | 45.48/45.57 GiB |

Across the 40M-token final cells, order-3 averaged `1.358M` tok/s and
Transformer `2.089M` tok/s. Order-3 therefore achieved `64.99%` of Transformer
throughput at the same physical and token batch.

### Matched 40M-token finals

The models remained matched to `99.049%` by trainable body parameters:
`2,125,888` for order-3 rank-8 and `2,146,304` for Transformer. Every final used
exactly `40,108,032` prediction tokens. Lower NLL is better.

| Optimizer | Model | Seed 0 test NLL | Seed 1 | Seed 2 | Mean |
|---|---|---:|---:|---:|---:|
| Muon | order 3, rank 8 | `5.54831` | `5.48910` | `5.49211` | `5.50984` |
| Muon | Transformer | `5.01399` | `5.01936` | `4.98820` | `5.00718` |
| AdamW v6 | order 3, rank 8 | `5.69646` | `5.24367` | `5.25766` | `5.39926` |
| AdamW v6 | Transformer | `5.20354` | `5.22056` | `5.19490` | `5.20634` |

Muon's effect is architecture-dependent:

- On order-3, `NLL(Muon) - NLL(AdamW) = +0.11058`; Muon is worse. The paired
  bootstrap interval is `[-0.14815, +0.24543]`.
- On Transformer, `NLL(Muon) - NLL(AdamW) = -0.19916`; Muon wins every seed.
  The paired interval is `[-0.20670, -0.18956]`.
- With Muon on both sides, order-3 is worse by `+0.50266` mean test NLL, with
  paired interval `[+0.46975, +0.53433]`.
- Giving each architecture its independently selected best optimizer chooses
  AdamW for order-3 and Muon for Transformer. The paired order-3-minus-
  Transformer deltas are `[+0.68247, +0.22431, +0.26946]`: a mean gap of
  `+0.39208` with interval `[+0.22431, +0.68247]`. The promotion rule required
  the upper endpoint to be at most `+0.02`, so it fails decisively.

Canonical Muon therefore does not expose a hidden win for this Kronecker
variant. It makes the matched control substantially stronger while leaving the
order-3 model statistically no better than its tuned AdamW reference. This is
evidence against scaling this exact operator, not evidence against every
architecture motivated by richer representation mixing.

### v8 audit and retained artifacts

The base campaign took `731.738` GPU-seconds and the preregistered boundary
completion took `58.164` GPU-seconds. Including allocation, bootstrap, data,
and all inter-stage paid time, the cumulative audit records `1,970.040`
GPU-seconds (`0.5472` GPU-hours) and `$1.49942`. All controller stages completed
with no failures. The manifest hash is
`f2f7db9d1bd33c7fe51bb77d933e9ea86cfc13b77e8853c460e7438b5d141279`.

Retrieved artifacts live at `/tmp/exp11-muon-tuning-gpu1-v7/` and
`/tmp/exp11-muon-tuning-gpu1-v8/`. Final artifact hashes are:

| Artifact | SHA-256 |
|---|---|
| `v8/plan.json` | `a3f2cf3790f11c5ba2a7906a6bec9afd2f563059a4c3e1cf52a0c4acbfae7932` |
| `v8/stages/muon-tuning/result.json` | `ac523ac9a72f00e2c2001a31dbeb0254531168c426740a51e3e1afcdf01e78cb` |
| `v8/stages/muon-tuning/muon-cells/preflight.json` | `70758ff23bdfc6b80fae72e4e96833bf74b2f4cd04fe86ed9bb61c34c98b1764` |
| `v8/audit.json` | `0a06222751a8f93e8035ca743abd85f87f12a0ca2914344d4ac3c884fd76f7e6` |
| `v8/ledger.jsonl` | `00423a7e9b8c5be593377d2315fd1c0db65010642573a9e58f3cf23ed639ff11` |

## Under-tuned paid pilot (v5; superseded)

Completed on 2026-08-02 on one NVIDIA H100 80GB HBM3. The Northflank service
`fda-exp11-us-central/gpu-h100-1` was verified paused after artifact retrieval
(`instances: 0`).

**Tuning qualification:** this v5 result is an under-tuned pilot, not the final
architecture verdict. Its 2M-token screen was only 16 optimizer updates, both
primary winners were at the upper LR-grid boundary, and it used constant LR
without warmup or decay. The locked v6 follow-up in `experiment_tuning.json`
replaces it with three-seed 10M-token screens, automatic LR-boundary expansion,
constant versus token-based warmup/cosine, weight-decay and beta2 ablations, and
a fresh 40M-token comparison after validation-only selection.

- Synthetic probes: [W&B run `jk32yzre`](https://wandb.ai/lev-tear-tear-labs/exp11-kronecker-debug/runs/jk32yzre)
- WikiText tuning and matched finals: [W&B run `sl8bloez`](https://wandb.ai/lev-tear-tear-labs/exp11-kronecker-debug/runs/sl8bloez)
- Controller experiment: `exp11-kronecker-debug-gpu1-v5`
- Final verdict: **`do_not_scale_yet`**

### Synthetic mechanism screen

All variants reached 100% delayed-copy accuracy. On associative recall,
order-3 rank-4 reached `0.28125` accuracy versus Transformer's `0.296875`, a
gap of `0.015625` within the locked `0.02` limit. The synthetic gate therefore
passed and allowed the WikiText pilot to proceed.

### Paid GPU preflight

The preflight verified finite forward, backward, and optimizer state in BF16,
used no gradient accumulation, swept down from an ambitious physical batch,
and selected the highest-throughput stable configuration for each model. The
BF16-versus-FP32 NLL absolute difference was `2.985e-4`.

| Variant | Mode | Physical batch | Global tokens/step | Throughput | GPU util. sample | Peak alloc./reserved |
|---|---|---:|---:|---:|---:|---:|
| Exp10 replica | compiled | 384 | 98,304 | 2.592M tok/s | 97% | 20.26/23.47 GiB |
| balanced order 2 | eager | 512 | 131,072 | 1.457M tok/s | 57% | 28.48/32.84 GiB |
| order 3, rank 4 | eager | 512 | 131,072 | 1.202M tok/s | 95% | 28.97/33.09 GiB |
| order 3, rank 8 | eager | 512 | 131,072 | 0.949M tok/s | 100% | 31.99/36.18 GiB |
| Transformer | compiled | 512 | 131,072 | 2.043M tok/s | 74% | 30.30/34.38 GiB |

The order-3 compiled benchmark did not run because the remote environment was
missing `setuptools`; eager was the stable measured path. Consequently, the
quality comparison is valid at equal tokens, but the measured throughput gap
must not be interpreted as a clean architecture-level compiler comparison.

### Independent tuning

Both primary architectures were tuned with the same procedure, but not rescued
per final seed: LR selection used seed 0 validation at about 2M tokens and the
winning LR was then locked for all paired final seeds. Kronecker variants swept
`{0.003, 0.01, 0.03}`; Transformer swept `{0.0003, 0.001, 0.003}`.

| Variant | Winning LR | 2M validation NLL | 10M validation NLL | Body parameters | Total parameters |
|---|---:|---:|---:|---:|---:|
| Exp10 replica | 0.01 | 7.48149 | 6.65269 | 3,153,920 | 5,251,072 |
| balanced order 2 | 0.03 | 7.48747 | 6.34016 | 1,971,240 | 4,068,392 |
| order 3, rank 4 | 0.03 | 7.21869 | 6.47391 | 1,062,944 | 3,160,096 |
| order 3, rank 8 | 0.03 | 7.19874 | 6.55911 | 2,125,888 | 4,223,040 |
| Transformer | 0.003 | 7.27351 | 6.16353 | 2,146,304 | 4,243,456 |

At 10M tokens, corrected order-3 rank-4 improved validation NLL by `0.17878`
over the Exp10 replica, exceeding the required `0.05` mechanism gate. This is
evidence that the implementation correction mattered. It is not a Transformer
win: the similarly body-parameter-matched rank-8 model was `0.39559` NLL worse
than Transformer at the same token budget.

### Matched 40M-token finals

Every final trained on 40,108,032 prediction tokens with the locked LR. Lower
NLL is better.

| Model | Seed | Test NLL | Validation NLL | Final train NLL | Throughput |
|---|---:|---:|---:|---:|---:|
| order 3, rank 4 | 0 | 5.54314 | 5.54340 | 5.54191 | 1.203M tok/s |
| order 3, rank 4 | 1 | 5.71086 | 5.72269 | 5.79338 | 1.204M tok/s |
| order 3, rank 4 | 2 | 5.78156 | 5.79554 | 5.81317 | 1.203M tok/s |
| Transformer | 0 | 4.80384 | 4.79728 | 4.80178 | 2.050M tok/s |
| Transformer | 1 | 4.82083 | 4.82896 | 4.91427 | 2.051M tok/s |
| Transformer | 2 | 7.19643 | 7.18810 | 7.19703 | 2.051M tok/s |

Order-3 used `49.524%` of the Transformer's trainable body parameters. The
paired test deltas `NLL(order3) - NLL(transformer)` were
`[+0.73931, +0.89003, -1.41487]`. Their preregistered mean was `+0.07149`, with
a paired bootstrap 95% interval of `[-1.41487, +0.89003]`. The promotion gate
required the upper bound to be at most `+0.02`, so it **failed**.

Transformer seed 2 clearly collapsed and should trigger an optimizer/stability
investigation. It does not reverse the scientific conclusion: Transformer won
the two non-collapsed paired seeds by `0.739` and `0.890` test NLL, while the
collapsed seed only makes the preregistered aggregate look artificially close.
No 255M-parameter scale-up is justified by this result.

### Audit and retained artifacts

The final paid stage took `363.337` GPU-seconds and cost `$0.27654`. Including
the deliberately conservative `1,800` GPU-second charge for all earlier
provisioning, synthetic training, and infrastructure recovery, the controller
audit records `2,163.337` GPU-seconds and `$1.64654` total. All controller
stages completed with no recorded failures.

The retrieved immutable artifacts live at
`/tmp/exp11-kronecker-debug-gpu1-v5/`. Their SHA-256 hashes are:

| Artifact | SHA-256 |
|---|---|
| `stages/full-pilot/result.json` | `e1ab227623f1d9270b90c0f2641b057fd42ca8d6141803db78f0ba082c53b978` |
| `stages/full-pilot/wikitext.json` | `eab18403ebd7022ddb7377ae2a2d1d9258f50a5388f4d17ccc451abff1da0b76` |
| `stages/full-pilot/synthetic.json` | `1d5563eb3ea9d82db00606bd4462b57b4bfac31ed1304c8663f519fca9dda933` |
| `stages/full-pilot/wikitext-cells/preflight.json` | `db92e0f03cde837572c18c79828e5622582ab5cf45a8dbd152ada60ade2543bb` |
| `audit.json` | `9fb3699c0423f6c403bc8d290eb46b19130b57a2da7f6f6cc17d254d16da53df` |
| `ledger.jsonl` | `d8a63848b3a2c21a0b62d6f8d1f13d3ba1608cad0ea54d7eb9a57264fae20abb` |
