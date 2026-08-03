#!/usr/bin/env bash
set -euo pipefail

cd /root/fda/NEW

: "${WANDB_API_KEY:?WANDB_API_KEY must be set}"

data_root=${EXP10_DATA_ROOT:-/cache/exp10/data}
output_root=${EXP10_OUTPUT_ROOT:-/cache/exp10/runs}
preflight_root=${EXP10_PREFLIGHT_ROOT:-/cache/exp10/preflight}
eval_batch=${EXP10_EVAL_BATCH:-512}

for dataset in tinystories wikitext; do
  test -f "$data_root/$dataset/manifest.json"
done

gpu_count=$(nvidia-smi --query-gpu=name --format=csv,noheader | wc -l)
test "$gpu_count" -eq 8
nvidia-smi --query-gpu=name --format=csv,noheader | grep -qv H100 && {
  echo "Exp10 requires eight H100 GPUs" >&2
  exit 1
}

uv run --no-sync python exp10/architecture_verdict.py --mode=inventory
if [[ ! -f "$preflight_root/preflight.json" ]]; then
  uv run --no-sync python exp10/preflight.py \
    --data-root="$data_root/tinystories" \
    --output-root="$preflight_root"
else
  echo "Reusing completed preflight: $preflight_root/preflight.json"
fi

local_batch=$(uv run --no-sync python -c \
  'import json,sys; print(json.load(open(sys.argv[1]))["selected_local_batch"])' \
  "$preflight_root/preflight.json")

uv run --no-sync python exp10/coordinator.py \
  --mode=run \
  --stage=tinystories \
  --data-root="$data_root" \
  --output-root="$output_root" \
  --local-batch="$local_batch" \
  --eval-batch="$eval_batch"

uv run --no-sync python exp10/coordinator.py \
  --mode=run \
  --stage=wikitext \
  --data-root="$data_root" \
  --output-root="$output_root" \
  --local-batch="$local_batch" \
  --eval-batch="$eval_batch"

uv run --no-sync python exp10/coordinator.py \
  --mode=audit \
  --stage=all \
  --data-root="$data_root" \
  --output-root="$output_root" \
  --local-batch="$local_batch" \
  --eval-batch="$eval_batch"
