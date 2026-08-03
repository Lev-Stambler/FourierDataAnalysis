#!/usr/bin/env bash
set -euo pipefail

cd /root/fda/NEW

root=/cache/expv6-kiss/matched-standard/symmetric-v2-h100
lrs=(0.025 0.05 0.1 0.2 0.4 0.8)

for architecture in kronecker transformer; do
  for lr in "${lrs[@]}"; do
    slug=${lr/./p}
    PYTHONUNBUFFERED=1 uv run --no-sync torchrun --standalone --nproc-per-node=8 \
      expv6/matched-standard/matched_standard.py \
      --architecture="$architecture" \
      --lr="$lr" \
      --steps=16 \
      --checkpoint_every=16 \
      --local_batch=32832 \
      --teacher_microbatch=2048 \
      --evaluation_batch=256 \
      --evaluate_after_train=true \
      --resume=false \
      --legacy_root_layout=false \
      --symmetric_campaign=true \
      --output_dir="$root" \
      --run_name="expv6-symmetric-v2-h100-${architecture}-screen-lr${slug}"
  done
done
