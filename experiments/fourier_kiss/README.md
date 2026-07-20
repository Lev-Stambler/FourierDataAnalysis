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
uv run pytest fourier_kiss/test_model.py -q
uv run modal run fourier_kiss/modal_train.py --stage smoke
uv run modal run fourier_kiss/modal_train.py --stage train --x 524288
uv run modal run fourier_kiss/modal_train.py --stage scale --x 524288
```

`scale` concurrently runs matched 131,072- and 524,288-character models and
publishes a W&B comparison. Production defaults use fused AdamW, batch 16,384,
1,000 warmup-plus-cosine steps, global gradient clipping at 1.0, and continuous
mask learning on H100. Both parameter groups search at high LR through step 50.
Masks decay to a live 1% tail by step 75; coefficients decay to a 33% tail by
step 150. Neither group is frozen.
