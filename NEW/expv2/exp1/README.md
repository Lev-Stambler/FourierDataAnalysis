# ExpV2-1: Kronecker basics

ExpV2-1 is a clean restart of the architecture investigation. It tests one
simple claim: a dense linear map over the complete token-by-channel state can
be replaced by a small sum of Kronecker products.

```text
X <- X + 1/sqrt(depth * rank)
         sum_r A_r SiLU(RMSNorm(X)) B_r^T
```

`A_r` is learned and lower triangular, and `B_r` is a learned channel map.
This candidate contains no attention, query/key routing, FFN, content router,
channel permutation, or shared position-bank mechanism. Nothing in the package
imports model, optimizer, data, or trainer code from Exp5--13.

## Locked matched models

All models use context 128 and the same tied `4096 × 128` vocabulary.

| Model | Shape | Body parameters | Total parameters |
|---|---|---:|---:|
| Kronecker | rank/depth `1/66`, `2/33`, `3/22`, or `6/11` | 1,626,240 | 2,150,528 |
| Dense whole-state | internal width 14, depth 1 | 1,621,760 | 2,146,048 |
| Transformer | width 128, depth 8, 4 heads, SwiGLU 360 | 1,630,208 | 2,154,496 |

The maximum body mismatch is `0.521%`; the maximum total mismatch is
`0.394%`. Every trainable tensor, including the tied vocabulary, uses batched
Muon. There is no auxiliary AdamW group.

## Scientific gates

The paid campaign cannot reach TinyStories until a Kronecker variant passes
delay-controlled copy, associative recall, and two-hop recall against both
controls over five seeds. TinyStories uses a train-only 4K BPE, fixed
document-contained windows, document-disjoint validation and sealed test data,
independent LR/schedule tuning, and fresh final seeds 3--5.

A TinyStories win requires all three paired seed wins, mean test-NLL advantage
of at least `0.02` over Transformer, a document-level paired-bootstrap upper
95% bound below zero, a win over dense, and at least half Transformer training
throughput. A failure is scoped to this exact model and scale.

## Local commands

```bash
uv run --no-sync python -m expv2.exp1 inventory
uv run --no-sync pytest -q expv2/exp1/test_exp1.py
uv run --no-sync python -m expv2.exp1 local-audit \
  --output=/tmp/expv2-1-local-audit.json
uv run --no-sync python -m expv2.exp1 prepare-tinystories \
  --output-root=/home/lev/.cache/expv2/exp1/data/tinystories
```

The local audit passes all numerical, causality, gradient, optimizer, and
parameter-matching checks. The completed paid result is recorded in
`RUN_RESULTS.md`.

## Bounded H100 launch

The disposable target is exactly one H100 80GB at `$2.74/GPU-hour`; the
preserved `fda-cache` volume is attached, but the new service is deleted after
artifacts and W&B have flushed.

```bash
uv run --no-sync python -m expv2.exp1 provision-h100 \
  --output=/home/lev/.cache/expv2/exp1/h100-target.json

export RC_NF_PROJECT=fda-test
export RC_NF_SERVICE=expv2-1-h100
uv run --no-sync python -m research_control plan \
  expv2/exp1/experiment.json \
  --state-root=/home/lev/.cache/research-control
uv run --no-sync python -m research_control run \
  expv2/exp1/experiment.json --stage=local-audit \
  --state-root=/home/lev/.cache/research-control
uv run --no-sync python -m research_control run \
  expv2/exp1/experiment.json --stage=prepare-data \
  --state-root=/home/lev/.cache/research-control
uv run --no-sync python -m research_control run \
  expv2/exp1/experiment.json --stage=h100-pilot \
  --state-root=/home/lev/.cache/research-control
```

The paid preflight starts at 8,192 contexts (1,048,576 global tokens), searches
down only after OOM or lower measured throughput, forbids accumulation, requires
85% median utilization, records the measured improvement over batch 128 plus
VRAM/power/utilization, and gives compilation only a bounded 60-second probe.
Ten-fold throughput is an aspiration when measured headroom exists, not a
failure condition after the selected batch demonstrably saturates the GPU. W&B
authentication and a direct URL are mandatory before the first training step.
