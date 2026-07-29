# NorMuon versus AdamW

Last updated: 2026-07-28.

## Selected default

Future structured-factor training defaults to the winning hybrid:

- Independent NorMuon update for every rank-slice matrix.
- Equal-shaped matrices batched only as an implementation optimization.
- NorMuon LR `3e-3`, betas `(0.95, 0.95)`, Nesterov, five NS steps.
- Fused AdamW LR `3e-4` for the tied embedding/head, RMSNorm weights,
  gains, mixing vectors, and biases.
- Shared WSD multiplier, weight decay `0.01`, epsilon `1e-10`, and
  gradient clipping at `1.0`.

The machine-readable policy is [`normuon_default.json`](normuon_default.json).

## Matched early-stop result

The user-requested endpoint is step 64: 131,072 training contexts and the
identical 2,048-example validation audit for both optimizers.

| Factor optimizer | Factor LR | Train CE | Validation CE | Perplexity | Accuracy |
|---|---:|---:|---:|---:|---:|
| NorMuon + AdamW auxiliary | 0.003 | **7.33678** | **7.26126** | **1424.05** | **9.4238%** |
| AdamW on all parameters | 0.0003 | 7.45397 | 7.35160 | 1558.69 | 9.3262% |

At this matched endpoint, AdamW is worse by `0.0903403` validation CE,
`134.6395` perplexity, and `0.09766` accuracy percentage points. NorMuon
wins. The AdamW process had reached step 84 when stopped, but those unmatched
steps are excluded. Its complete step-64 model and both optimizer states are
preserved in `progress.pt`.

This is an early-stop comparison on the shared 2,048-example audit, not the
full 8,192-example validation split.

## Completed NorMuon endpoint

All rows use the same architecture, seed-zero initialization and data order,
batch 2,048, 262,144 training contexts, gradient clipping at 1, and the
complete 128-step WSD schedule (8 warmup, 108 stable, 12 cooldown). Selection
uses all 8,192 validation contexts.

| Factor optimizer | Base LR | Validation CE | Perplexity | Accuracy |
|---|---:|---:|---:|---:|
| NorMuon | 0.003 | **6.8693399** | **962.3132** | **13.1592%** |
| NorMuon | 0.010 | 6.9217209 | 1014.0636 | — |
| NorMuon | 0.001 | 6.9679824 | 1062.0777 | — |
| NorMuon | 0.020 | 6.9921886 | 1088.1002 | — |

The selected NorMuon reference is LR `0.003`. Its validation-CE advantage
over LR `0.01` is `0.05238098`.

## Matched AdamW control

Status: stopped at the matched step-64 checkpoint.

Every parameter used fused AdamW at base LR `3e-4`, betas `(0.9, 0.95)`,
epsilon `1e-10`, and weight decay `0.01`.

W&B:
[matched AdamW run](https://wandb.ai/lev-tear-tear-labs/qwen-normuon-next-token-pretrain/runs/229ab46380384406).

## NorMuon update granularity

Each of the 24 factor parameters has shape `[rank, rows, columns]`. NorMuon
acts independently on every `[rows, columns]` rank slice: 57,768 matrix
updates total. Equal-shaped matrices are batched into three Newton–Schulz
calls for throughput, but the batched dimension is independent and no matrix
is mixed with another. LR and weight decay are shared; momentum and
row-moment values are independent for every slice.
