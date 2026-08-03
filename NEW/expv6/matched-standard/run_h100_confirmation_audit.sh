#!/usr/bin/env bash
set -euo pipefail

cd /root/fda/NEW

root=/cache/expv6-kiss/matched-standard/symmetric-v2-h100
confirmation="$root/confirmation-v1"

for architecture in kronecker transformer; do
  for seed in 0 1 2; do
    uv run --no-sync torchrun --standalone --nproc-per-node=8 \
      expv6/matched-standard/matched_standard.py \
      --mode=evaluate \
      --evaluation_split=confirmation \
      --architecture="$architecture" \
      --seed="$seed" \
      --lr=0.4 \
      --steps=256 \
      --local_batch=32832 \
      --teacher_microbatch=2048 \
      --evaluation_batch=256 \
      --legacy_root_layout=false \
      --symmetric_campaign=true \
      --output_dir="$root" \
      --confirmation_root="$confirmation"
  done
done

uv run --no-sync expv6/matched-standard/matched_standard.py \
  --mode=audit \
  --steps=256 \
  --local_batch=32832 \
  --legacy_root_layout=false \
  --symmetric_campaign=true \
  --output_dir="$root" \
  --confirmation_root="$confirmation"
