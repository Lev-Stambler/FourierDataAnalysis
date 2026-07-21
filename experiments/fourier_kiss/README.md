# Fourier KISS

One model, one bank of exactly `X` jointly trained hard Walsh characters. The
forward pass is exact XOR; mask gradients use a log-space STE. A sampled
soft-Jaccard auxiliary loss discourages nearly identical masks, and periodic
function-preserving repair guarantees the exported masks are unique.

The fidelity objective is pure soft BCE against the pinned teacher
probabilities. There is no ensemble, hard-label mixture, mask freeze, or
post-hoc character-count sweep.

```bash
cd experiments
uv run modal run fourier_kiss/modal_train.py --stage smoke
uv run modal run fourier_kiss/modal_train.py --stage train --x 524288
uv run modal run fourier_kiss/modal_train.py --stage scale --x 524288
uv run modal run fourier_kiss/modal_rescue.py --stage upper --steps 500
uv run modal run fourier_kiss/modal_rescue.py --stage train --x 131072 \
  --steps 1000 --batch-size 8192 --coefficient-lr 0.001
```

`modal_rescue.py` is the screened, higher-order rescue ladder. It audits
injective token codes, removes constant/equivalent input columns, screens
degree-1/2 plus scored degree-3--8 characters on the H100, fits with
resumable AdamW, and exports an independently calibrated probability scale
and agreement threshold. All diagnostics (MAE/RMSE percentiles, confidence
buckets, cosine/R², normalized hard MI, recall, uniqueness, and compression)
are logged to W&B. Runtime tests and data work are intended to run remotely on
Modal; the local machine is not a training/evaluation target.

`scale` concurrently runs matched 131,072- and 524,288-character models and
publishes a W&B comparison. Production defaults use fused AdamW, batch 16,384,
1,000 warmup-plus-cosine steps, global gradient clipping at 1.0, and continuous
mask learning on H100. Both parameter groups search at high LR through step 50.
Masks decay to a live 1% tail by step 75; coefficients decay to a 33% tail by
step 150. Neither group is frozen.

## Data-aware residual pursuit

`modal_pursuit.py` is the exact discrete alternative to mask STE. It keeps the
deployed model a flat Walsh sum, but discovers supports as a staircase:

1. Canonicalize nonconstant input columns up to equality/complement.
2. Screen every singleton and all degree-two characters on 1M teacher examples.
3. Reject exact support collisions plus empirical character signatures equal up
   to global sign.
4. Score one-bit parent extensions against the current soft-BCE residual.
5. Add each block at zero coefficient, freeze the established function, and fit
   only the new block with fresh fused AdamW.
6. Warm-start the complete dictionary from the best nested prefix, zero all
   optional coefficients, then jointly refit with warmup-stable-decay.

The model uses the same fixed `1/sqrt(max_terms)` coefficient parameterization
as the direct baseline. This is essential: omitting it makes nominal AdamW
updates hundreds of times too large. Compact nodes are serialized as
`(parent:int32, appended_bit:uint16, block-scaled coefficient:fp16)`. The token
LSH codebook is stored with the model and checked to be injective.

```bash
PYTHONPATH=experiments modal run experiments/fourier_kiss/modal_pursuit.py \
  --stage tests
PYTHONPATH=experiments modal run experiments/fourier_kiss/modal_pursuit.py \
  --stage diagnose --initial-terms 32768
PYTHONPATH=experiments modal run experiments/fourier_kiss/modal_pursuit.py \
  --stage train --x 262144 --add-per-round 32768 --parent-beam 4096 \
  --batch-size 16384 --fit-steps 50 --final-steps 1000
```

Continuation accepts a compact parent artifact via `--parent-model`. It verifies
the exact token codebook and rescales raw parameters to preserve logits under a
larger fixed character budget. Checkpoints include optimizer/RNG state and are
resumable on Modal.

The July 2026 360,448-character run is recorded at
https://wandb.ai/umd-leans-well/fda-fourier-noun/runs/8g8hmkur. Its serialized
artifact is 3,882,566 bytes (412.1x versus 1.6 GB), with test KL 0.01309 and
teacher agreement 88.73%. This is the best validated pursuit artifact, not a
claim that the 95% agreement / 0.01 KL target has been met.
