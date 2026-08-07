#!/usr/bin/env bash
set -euo pipefail

experiment_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
mode="${V2_MODE:-preflight}"
microbatch="${V2_MICROBATCH:-12}"
accumulation="${V2_GRADIENT_ACCUMULATION:-1}"
target_tokens="${V2_TARGET_TOKENS:-100000000}"
learning_rate="${V2_LEARNING_RATE:-0.0003}"
kd_weight="${V2_KD_WEIGHT:-0.8}"
hard_weight="${V2_HARD_WEIGHT:-0.2}"
teacher_mode="${V2_TEACHER_MODE:-online}"
warmup_target_tokens="${V2_WARMUP_TARGET_TOKENS:-100000000}"
lr_schedule_origin_target_tokens="${V2_LR_SCHEDULE_ORIGIN_TARGET_TOKENS:-0}"
lr_schedule_target_tokens="${V2_LR_SCHEDULE_TARGET_TOKENS:-}"
run_name="${V2_RUN_NAME:-v2-sbd-${mode}-b${microbatch}-a${accumulation}}"
output="${V2_OUTPUT:-/cache/v2_sbd/${run_name}}"
python_bin="${V2_PYTHON:-/cache/v2_sbd/env/bin/python}"
resolved_shards="${V2_RESOLVED_SHARDS:-/cache/v2_sbd/resolved-shards-v1.json}"
token_cache="${V2_TOKEN_CACHE:-/cache/v2_sbd/token-cache-stage0}"
stage1_token_cache="${V2_STAGE1_TOKEN_CACHE:-/cache/v2_sbd/token-cache-stage1}"
eval_token_cache="${V2_EVAL_TOKEN_CACHE:-}"
resume="${V2_RESUME:-}"
checkpoint_targets="${V2_CHECKPOINT_TARGETS:-10000000,50000000,100000000,250000000,500000000,1000000000,2000000000,4000000000,6000000000,8000000000,10000000000}"
compile_artifact="${V2_COMPILE_ARTIFACT:-/cache/torchinductor/v2_sbd/artifacts/v3-b${microbatch}.pt2cache}"
compile="${V2_COMPILE:-1}"

# Persist fixed-shape Inductor graphs across replacement containers. Eight
# ranks each get a bounded compiler pool so graph construction cannot spawn
# 256 competing CPU workers on the same node.
export TORCHINDUCTOR_CACHE_DIR="${TORCHINDUCTOR_CACHE_DIR:-/cache/torchinductor/v2_sbd}"
export TORCHINDUCTOR_COMPILE_THREADS="${TORCHINDUCTOR_COMPILE_THREADS:-8}"
export TORCHINDUCTOR_FX_GRAPH_CACHE="${TORCHINDUCTOR_FX_GRAPH_CACHE:-1}"

mapfile -t gpu_names < <(nvidia-smi --query-gpu=name --format=csv,noheader)
[[ "${#gpu_names[@]}" -eq 8 ]] || { echo "requires exactly eight GPUs" >&2; exit 41; }
for name in "${gpu_names[@]}"; do
  [[ "$name" == *H100* ]] || { echo "requires H100, found $name" >&2; exit 42; }
done
[[ -n "${WANDB_API_KEY:-}" ]] || { echo "WANDB_API_KEY is missing" >&2; exit 43; }
[[ -x "$python_bin" ]] || {
  echo "cached environment missing; run scripts/bootstrap_cached_env.sh" >&2
  exit 45
}

mapfile -t compute_pids < <(
  nvidia-smi --query-compute-apps=pid --format=csv,noheader,nounits \
    | sed '/^[[:space:]]*$/d' | sort -u
)
if [[ "${#compute_pids[@]}" -ne 0 ]]; then
  echo "GPU node is not idle; compute PIDs: ${compute_pids[*]}" >&2
  exit 44
fi

cd "$experiment_dir"
extra_args=()
if [[ -n "$eval_token_cache" ]]; then
  extra_args+=(--eval-token-cache "$eval_token_cache")
fi
if [[ -n "$resume" ]]; then
  extra_args+=(--resume "$resume")
fi
if [[ -n "$lr_schedule_target_tokens" ]]; then
  extra_args+=(--lr-schedule-target-tokens "$lr_schedule_target_tokens")
fi
if [[ "$compile" == "1" ]]; then
  extra_args+=(--compile)
fi
exec "$python_bin" -m torch.distributed.run --standalone --nproc_per_node=8 \
  -m v2_simpler_block_diffusion.ddp_train \
  --allow-paid \
  --mode "$mode" \
  --manifest "$experiment_dir/data_manifest.lock.json" \
  --resolved-shards "$resolved_shards" \
  --token-cache "$token_cache" \
  --stage1-token-cache "$stage1_token_cache" \
  --output "$output" \
  --run-name "$run_name" \
  --microbatch "$microbatch" \
  --gradient-accumulation "$accumulation" \
  --target-tokens "$target_tokens" \
  --warmup-target-tokens "$warmup_target_tokens" \
  --lr-schedule-origin-target-tokens "$lr_schedule_origin_target_tokens" \
  --learning-rate "$learning_rate" \
  --kd-weight "$kd_weight" \
  --hard-weight "$hard_weight" \
  --teacher-mode "$teacher_mode" \
  --checkpoint-targets "$checkpoint_targets" \
  --compile-artifact "$compile_artifact" \
  "${extra_args[@]}" \
  "$@"
