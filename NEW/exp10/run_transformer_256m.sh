#!/usr/bin/env bash
set -euo pipefail

cd /root/fda/NEW
: "${WANDB_API_KEY:?WANDB_API_KEY must be set}"

data_root=${EXP10_DATA_ROOT:-/cache/exp10/data/wikitext}
output_root=${EXP10_256M_OUTPUT_ROOT:-/cache/exp10/runs/wikitext/transformer-256m}
preflight_root=${EXP10_256M_PREFLIGHT_ROOT:-/cache/exp10/preflight-transformer-256m}

test -f "$data_root/manifest.json"
test "$(nvidia-smi --query-gpu=name --format=csv,noheader | wc -l)" -eq 8
nvidia-smi --query-gpu=name --format=csv,noheader | grep -qv H100 && exit 2

if [[ ! -f "$preflight_root/preflight.json" ]]; then
  uv run --no-sync python exp10/preflight.py \
    --data-root="$data_root" \
    --output-root="$preflight_root" \
    --scale=xlarge \
    --architectures transformer
fi

local_batch=$(uv run --no-sync python -c \
  'import json,sys; print(json.load(open(sys.argv[1]))["selected_local_batch"])' \
  "$preflight_root/preflight.json")

uv run --no-sync python exp10/transformer_256m.py \
  --data-root="$data_root" \
  --output-root="$output_root" \
  --local-batch="$local_batch" \
  --eval-batch="$local_batch"
