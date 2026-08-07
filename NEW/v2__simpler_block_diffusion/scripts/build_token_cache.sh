#!/usr/bin/env bash
set -euo pipefail

experiment_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
python_bin="${V2_PYTHON:-/cache/v2_sbd/env/bin/python}"
stage="${V2_CACHE_STAGE:-0}"
contexts="${V2_CACHE_CONTEXTS_PER_RANK:-16384}"
skip_contexts="${V2_CACHE_SKIP_CONTEXTS_PER_RANK:-0}"
output="${V2_TOKEN_CACHE:-/cache/v2_sbd/token-cache-stage${stage}}"
resolved="${V2_RESOLVED_SHARDS:-/cache/v2_sbd/resolved-shards-v1.json}"

mkdir -p "$output"
pids=()
for rank in 0 1 2 3 4 5 6 7; do
  "$python_bin" -m v2_simpler_block_diffusion.token_cache \
    --manifest "$experiment_dir/data_manifest.lock.json" \
    --resolved-shards "$resolved" \
    --output "$output" \
    --rank "$rank" \
    --world-size 8 \
    --contexts "$contexts" \
    --skip-contexts "$skip_contexts" \
    --stage "$stage" &
  pids+=("$!")
done
status=0
for pid in "${pids[@]}"; do
  wait "$pid" || status=1
done
if [[ "$status" -ne 0 ]]; then
  exit "$status"
fi
"$python_bin" -m v2_simpler_block_diffusion.cache_audit \
  --root "$output" \
  --manifest "$experiment_dir/data_manifest.lock.json" \
  --stage "$stage" \
  --world-size 8
