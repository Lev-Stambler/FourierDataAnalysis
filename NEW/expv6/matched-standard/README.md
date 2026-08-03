# Experiment Six matched control

Experiment Six compares the original width-64, depth-32, rank-8 Kronecker
student with an exactly parameter-matched causal Transformer. Both students
have 1,177,920 trainable parameters and share the tied multiplicative
`485 × 512` vocabulary, teacher, data order, loss, and NorMuon optimizer.

The Transformer uses four attention heads, RoPE, and a width-96 SwiGLU. Its
16,384 attention weights plus 18,432 MLP weights exactly match each
Kronecker block's 34,816 weights.

## Local verification

```bash
uv run --no-sync pytest -q expv6/matched-standard/test_matched_standard.py
uv run --no-sync python expv6/matched-standard/matched_standard.py --self_test=true
```

## Paid-run sequence

All paid commands require eight CUDA ranks and an authenticated online W&B
run. Before training, benchmark both architectures at progressively larger
physical batches and retain the highest-throughput common global batch. An
OOM search starts at the largest candidate and moves downward.

Batch probes use `--steps=1 --evaluate_after_train=false` so they test a full
forward/backward/optimizer step without repeatedly paying for validation.

The symmetric-v2 learning-rate screen applies the same multi-fidelity grid to
both architectures:

```bash
for architecture in kronecker transformer; do
  for lr in 0.025 0.05 0.1 0.2 0.4 0.8; do
    torchrun --standalone --nproc-per-node=8 \
      expv6/matched-standard/matched_standard.py \
      --architecture="$architecture" --lr="$lr" --steps=16 \
      --legacy_root_layout=false --symmetric_campaign=true \
      --output_dir=/cache/expv6-kiss/matched-standard/symmetric-v2
  done
done
```

Promote the two lowest-KL candidates per architecture to 48 updates. Select
the lowest validation KL at 48 updates, breaking differences of at most
`0.01` in favor of higher measured throughput. Resume both winners to 144
updates, then run their locked recipes for seeds 1 and 2. At the selected
global batch of 466,944 contexts, 144 updates are exactly 67,239,936 contexts.

The original test aggregate has already been viewed and remains exploratory.
Create a new document-disjoint confirmation set and evaluate it only after
both recipes and all three seeds are locked:

```bash
torchrun --standalone --nproc-per-node=8 \
  expv6/matched-standard/matched_standard.py \
    --mode=evaluate --architecture=transformer --lr=SELECTED \
  --steps=144 --evaluation_split=confirmation \
  --legacy_root_layout=false --symmetric_campaign=true \
  --output_dir=/cache/expv6-kiss/matched-standard/symmetric-v2 \
  --confirmation_root=/cache/expv6-kiss/matched-standard/symmetric-v2/confirmation-v1
```

Finally, create the audit artifact on the training node. The symmetric audit
requires both full LR grids, exactly two promoted candidates per architecture,
three final seeds per locked recipe, six confirmation evaluations, and a
document-disjoint confirmation manifest:

```bash
uv run --no-sync python expv6/matched-standard/matched_standard.py \
  --mode=audit --symmetric_campaign=true --legacy_root_layout=false \
  --output_dir=/cache/expv6-kiss/matched-standard/symmetric-v2 \
  --confirmation_root=/cache/expv6-kiss/matched-standard/symmetric-v2/confirmation-v1
```
