#!/usr/bin/env bash
set -euo pipefail

experiment_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
python_bin="${V2_PYTHON:-/cache/v2_sbd/env/bin/python}"
output="${V2_RESOLVED_SHARDS:-/cache/v2_sbd/resolved-shards-v1.json}"

[[ -x "$python_bin" ]] || {
  echo "cached environment missing; run scripts/bootstrap_cached_env.sh" >&2
  exit 45
}

exec "$python_bin" -m v2_simpler_block_diffusion.shards \
  --manifest "$experiment_dir/data_manifest.lock.json" \
  --output "$output" \
  --max-document-files "${V2_MAX_DOCUMENT_FILES:-1024}"
