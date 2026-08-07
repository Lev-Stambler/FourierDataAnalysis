#!/usr/bin/env bash
set -euo pipefail

experiment_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
python_bin="${V2_PYTHON:-/cache/v2_sbd/env/bin/python}"
stage="${V2_CACHE_STAGE:-0}"
source_manifest="${V2_SOURCE_RESOLVED_SHARDS:-/cache/v2_sbd/resolved-shards-v1.json}"
staged_manifest="${V2_RESOLVED_SHARDS:-/cache/v2_sbd/resolved-shards-stage${stage}-local.json}"
staging_root="${V2_STAGED_DATA_ROOT:-/cache/v2_sbd/staged-data}"
workers="${V2_STAGE_WORKERS:-8}"

"$python_bin" -m v2_simpler_block_diffusion.stage_data \
  --manifest "$experiment_dir/data_manifest.lock.json" \
  --resolved-shards "$source_manifest" \
  --output-root "$staging_root" \
  --output-manifest "$staged_manifest" \
  --world-size 8 \
  --stage "$stage" \
  --workers "$workers"
