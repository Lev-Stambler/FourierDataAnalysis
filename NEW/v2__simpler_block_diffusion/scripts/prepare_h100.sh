#!/usr/bin/env bash
set -euo pipefail

experiment_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cache_root="${V2_CACHE_ROOT:-/cache/v2_sbd}"
python_bin="${V2_PYTHON:-$cache_root/env/bin/python}"
source_resolved="${V2_SOURCE_RESOLVED_SHARDS:-$cache_root/resolved-shards-v1.json}"
local_resolved="${V2_RESOLVED_SHARDS:-$cache_root/resolved-shards-stage0-local.json}"
eval_cache="${V2_EVAL_TOKEN_CACHE:-$cache_root/token-cache-stage0-eval}"
train_cache="${V2_TOKEN_CACHE:-$cache_root/token-cache-stage0-train}"
eval_contexts="${V2_EVAL_CONTEXTS_PER_RANK:-16}"
train_contexts="${V2_CACHE_CONTEXTS_PER_RANK:-16384}"
train_skip_contexts="${V2_CACHE_SKIP_CONTEXTS_PER_RANK:-$eval_contexts}"

"$experiment_dir/scripts/bootstrap_cached_env.sh"

if [[ ! -f "$source_resolved" ]]; then
  V2_RESOLVED_SHARDS="$source_resolved" "$experiment_dir/scripts/resolve_data_shards.sh"
fi

V2_CACHE_STAGE=0 \
V2_SOURCE_RESOLVED_SHARDS="$source_resolved" \
V2_RESOLVED_SHARDS="$local_resolved" \
  "$experiment_dir/scripts/stage_data.sh"

if [[ ! -f "$eval_cache/audit.json" ]]; then
  V2_CACHE_STAGE=0 \
  V2_CACHE_CONTEXTS_PER_RANK="$eval_contexts" \
  V2_CACHE_SKIP_CONTEXTS_PER_RANK=0 \
  V2_TOKEN_CACHE="$eval_cache" \
  V2_RESOLVED_SHARDS="$local_resolved" \
    "$experiment_dir/scripts/build_token_cache.sh"
fi

if [[ ! -f "$train_cache/audit.json" ]]; then
  V2_CACHE_STAGE=0 \
  V2_CACHE_CONTEXTS_PER_RANK="$train_contexts" \
  V2_CACHE_SKIP_CONTEXTS_PER_RANK="$train_skip_contexts" \
  V2_TOKEN_CACHE="$train_cache" \
  V2_RESOLVED_SHARDS="$local_resolved" \
    "$experiment_dir/scripts/build_token_cache.sh"
fi

cd "$experiment_dir"
"$python_bin" -m pytest -q
"$python_bin" - <<PY
import json
from pathlib import Path

summary = {
    "schema": "v2-sbd-h100-setup-v1",
    "python": "$python_bin",
    "resolved_shards": "$local_resolved",
    "eval_cache": json.loads((Path("$eval_cache") / "audit.json").read_text()),
    "train_cache": json.loads((Path("$train_cache") / "audit.json").read_text()),
}
destination = Path("$cache_root") / "setup-audit.json"
destination.write_text(json.dumps(summary, indent=2, sort_keys=True) + "\n")
print(json.dumps({
    "status": "ready",
    "setup_audit": str(destination),
    "eval_contexts": summary["eval_cache"]["total_contexts"],
    "train_contexts": summary["train_cache"]["total_contexts"],
}, sort_keys=True))
PY
