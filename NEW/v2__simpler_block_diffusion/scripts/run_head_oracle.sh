#!/usr/bin/env bash
set -euo pipefail

experiment_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
python_bin="${V2_PYTHON:-/cache/v2_sbd/env/bin/python}"
output="${V2_ORACLE_OUTPUT:-/cache/v2_sbd/head-oracle-r1}"
run_name="${V2_RUN_NAME:-v2-sbd-head-oracle-r1}"
eval_cache="${V2_EVAL_TOKEN_CACHE:-/cache/v2_sbd/token-cache-stage0-eval}"

mapfile -t gpu_names < <(nvidia-smi --query-gpu=name --format=csv,noheader)
[[ "${#gpu_names[@]}" -eq 8 ]] || { echo "requires exactly eight GPUs" >&2; exit 41; }
for name in "${gpu_names[@]}"; do
  [[ "$name" == *H100* ]] || { echo "requires H100, found $name" >&2; exit 42; }
done
[[ -n "${WANDB_API_KEY:-}" ]] || { echo "WANDB_API_KEY is missing" >&2; exit 43; }

cd "$experiment_dir"
exec "$python_bin" -m torch.distributed.run --standalone --nproc_per_node=8 \
  -m v2_simpler_block_diffusion.oracle \
  --allow-paid \
  --manifest "$experiment_dir/data_manifest.lock.json" \
  --eval-token-cache "$eval_cache" \
  --output "$output" \
  --run-name "$run_name" \
  "$@"
