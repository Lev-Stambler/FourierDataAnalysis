#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

root=${EXP10_ROOT:-/cache/exp10}

uv run --no-sync python exp10/prepare_data.py tokenizer \
  --output="$root/tokenizer" \
  --documents-per-dataset=100000

uv run --no-sync python exp10/prepare_data.py corpus \
  --dataset=tinystories \
  --tokenizer-root="$root/tokenizer" \
  --output="$root/data/tinystories" \
  --train-tokens=250000000 \
  --eval-tokens=5000000

uv run --no-sync python exp10/prepare_data.py corpus \
  --dataset=wikitext \
  --tokenizer-root="$root/tokenizer" \
  --output="$root/data/wikitext" \
  --train-tokens=0 \
  --eval-tokens=0
