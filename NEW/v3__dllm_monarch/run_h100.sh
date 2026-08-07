#!/usr/bin/env bash
set -euo pipefail

experiment_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cache_root="${V3_CACHE_ROOT:-/cache/v3_dllm_monarch}"
environment="$cache_root/env"
upstream="$cache_root/upstream/dllm"
dllm_commit="ca176752fbceec49c6b4777a2c18ae88e4eb10ed"
export HF_HOME="${HF_HOME:-/cache/hf}"
export HF_DATASETS_CACHE="${HF_DATASETS_CACHE:-/cache/hf/datasets}"
export UV_CACHE_DIR="${UV_CACHE_DIR:-/cache/uv}"
export PYTORCH_ALLOC_CONF="${PYTORCH_ALLOC_CONF:-expandable_segments:True}"
export TORCHINDUCTOR_CACHE_DIR="${TORCHINDUCTOR_CACHE_DIR:-/cache/torchinductor/v3_dllm_monarch}"
export TORCHINDUCTOR_COMPILE_THREADS="${TORCHINDUCTOR_COMPILE_THREADS:-8}"
export PATH="/root/.local/bin:$PATH"
if [[ -f /root/.v3_env ]]; then
  set -a
  source /root/.v3_env
  set +a
fi

bootstrap() {
  command -v git >/dev/null
  command -v uv >/dev/null || curl -LsSf https://astral.sh/uv/install.sh | sh
  mkdir -p "$cache_root/upstream" "$cache_root/outputs" "$UV_CACHE_DIR" "$TORCHINDUCTOR_CACHE_DIR"
  if [[ ! -d "$upstream/.git" ]]; then
    git clone https://github.com/ZHZisZZ/dllm.git "$upstream"
  fi
  git -C "$upstream" fetch origin "$dllm_commit" --depth 1
  git -C "$upstream" switch --detach "$dllm_commit"
  [[ "$(git -C "$upstream" rev-parse HEAD)" == "$dllm_commit" ]]
  # The pinned dLLM Dream copy predates attention_bias in Dream's checkpoint
  # config and hard-codes three bias tensors per layer. Honor the checkpoint
  # field so from_pretrained can enforce an exact state-dict load.
  dream_model="$upstream/dllm/pipelines/dream/models/modeling_dream.py"
  sed -i \
    -e 's/self.num_heads \* self.head_dim, bias=True)/self.num_heads * self.head_dim, bias=config.attention_bias)/' \
    -e 's/self.num_key_value_heads \* self.head_dim, bias=True)/self.num_key_value_heads * self.head_dim, bias=config.attention_bias)/g' \
    "$dream_model"
  if ! grep -q 'self.q_norm = DreamRMSNorm(self.head_dim' "$dream_model"; then
    sed -i \
      '/self.rotary_emb = DreamRotaryEmbedding(config=self.config)/a\        self.q_norm = DreamRMSNorm(self.head_dim, eps=config.rms_norm_eps)\n        self.k_norm = DreamRMSNorm(self.head_dim, eps=config.rms_norm_eps)' \
      "$dream_model"
  fi
  sed -i \
    -e 's/query_states = query_states.view(bsz, q_len, self.num_heads, self.head_dim).transpose(1, 2)/query_states = self.q_norm(query_states.view(bsz, q_len, self.num_heads, self.head_dim)).transpose(1, 2)/g' \
    -e 's/key_states = key_states.view(bsz, q_len, self.num_key_value_heads, self.head_dim).transpose(1, 2)/key_states = self.k_norm(key_states.view(bsz, q_len, self.num_key_value_heads, self.head_dim)).transpose(1, 2)/g' \
    "$dream_model"
  [[ "$(grep -c 'bias=config.attention_bias' "$dream_model")" -eq 3 ]]
  [[ "$(grep -c 'self.q_norm(query_states.view' "$dream_model")" -eq 2 ]]
  [[ "$(grep -c 'self.k_norm(key_states.view' "$dream_model")" -eq 2 ]]
  git -C "$upstream" submodule update --init --depth 1 lm-evaluation-harness
  [[ -x "$environment/bin/python" ]] || uv venv --python 3.11 "$environment"
  export VIRTUAL_ENV="$environment"
  export PATH="$environment/bin:/root/.local/bin:$PATH"
  uv sync --frozen --inexact --active --no-install-project --project "$experiment_dir"
  uv pip show --python "$environment/bin/python" dllm >/dev/null || \
    uv pip install --python "$environment/bin/python" --no-deps -e "$upstream"
  uv pip show --python "$environment/bin/python" lm-eval >/dev/null || \
    uv pip install --python "$environment/bin/python" --no-deps -e "$upstream/lm-evaluation-harness"
}

require_idle_h100s() {
  mapfile -t names < <(nvidia-smi --query-gpu=name --format=csv,noheader)
  [[ "${#names[@]}" -eq 8 ]]
  for name in "${names[@]}"; do [[ "$name" == *H100* ]]; done
  mapfile -t pids < <(nvidia-smi --query-compute-apps=pid --format=csv,noheader,nounits | sed '/^[[:space:]]*$/d')
  [[ "${#pids[@]}" -eq 0 ]] || { echo "GPU node busy: ${pids[*]}" >&2; exit 44; }
  [[ -n "${WANDB_API_KEY:-}" ]] || { echo "WANDB_API_KEY missing" >&2; exit 45; }
}

command="${1:-}"
shift || true
case "$command" in
  bootstrap)
    bootstrap
    ;;
  self-test)
    bootstrap
    "$environment/bin/python" "$experiment_dir/train.py" self-test
    ;;
  prepare-data)
    bootstrap
    "$environment/bin/python" "$experiment_dir/train.py" prepare-data "$@"
    ;;
  train)
    require_idle_h100s
    bootstrap
    exec "$environment/bin/accelerate" launch \
      --config_file "$upstream/scripts/accelerate_configs/ddp.yaml" \
      --num_processes 8 --mixed_precision bf16 \
      "$experiment_dir/train.py" train "$@"
    ;;
  sample)
    require_idle_h100s
    bootstrap
    exec env CUDA_VISIBLE_DEVICES=0 "$environment/bin/python" \
      "$experiment_dir/train.py" sample "$@"
    ;;
  *)
    echo "usage: $0 {bootstrap|self-test|prepare-data|train|sample ...}" >&2
    exit 2
    ;;
esac
