#!/usr/bin/env bash
# Northflank multi-GPU SSH node for fast edit -> sync -> train iteration.
# Replaces Modal for testing; Modal stays for long detached runs.
#
# Usage: ./nf.sh {up|bootstrap|sync|test|run <cmd>|ssh|proxy|status|pause|resume|down|pretrain-*|normuon-*|adamw-*|exp5-*}
# Typical first run: ./nf.sh up && ./nf.sh bootstrap && ./nf.sh test
# COST: 8x h100-80 + nf-compute-2000-40 ~= $22.6/hr while running. Pause when idle!
set -euo pipefail

# --- config ---------------------------------------------------------------
TEAM_ID="${NF_TEAM:-tearedcoder}"
PROJECT_ID="${NF_PROJECT:-fda-test}"
REGION="${NF_REGION:-us-central}"
SERVICE_ID="${NF_SERVICE:-fda-node}"
PLAN="nf-gpu-h100-80-8g"           # GPU plans are separate from nf-compute-*
GPU_TYPE="h100-80"                 # from `northflank list regions`
GPU_COUNT=8                        # countOptions: 1,2,4,8
IMAGE="nvidia/cuda:12.8.1-devel-ubuntu22.04"
EPHEMERAL_MB=4096000                # API-enforced minimum for h100-80 nodes (4 TB NVMe)
VOLUME_NAME="fda-cache"
VOLUME_SIZE_MB=$((500 * 1024))     # persistent /cache (HF models, inductor cache)
REMOTE_DIR="/root/fda"
SSH_KEY="$HOME/.ssh/id_northflank"
EXP5_STUDY_VARIANT="${EXP5_STUDY_VARIANT:-v3-khatri-rao}"
EXP5_ROOT="${EXP5_OUTPUT_ROOT:-/cache/exp5_kronecker_distill/$EXP5_STUDY_VARIANT}"
EXP5_PREFLIGHT_BATCHES="${EXP5_PREFLIGHT_MICROBATCHES:-8192,4096,2048,1024}"
EXP5_WANDB_MODE="${EXP5_WANDB_MODE:-}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
NEW_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

# Local credentials stay out of rsync/git, but paid launches need them copied
# into the node environment during bootstrap.
if [[ -f "$NEW_DIR/.env" ]]; then
  set -a
  # shellcheck disable=SC1091
  source "$NEW_DIR/.env"
  set +a
fi

NF=(northflank)
NF_SCOPE=(--teamId "$TEAM_ID")
NF_SVC=(--projectId "$PROJECT_ID" --serviceId "$SERVICE_ID" "${NF_SCOPE[@]}")

die() { echo "nf.sh: $*" >&2; exit 1; }

# --- ssh proxy ------------------------------------------------------------
PROXY_PID=""
PROXY_ADDR=""
PROXY_PORT=""
SSH_OPTS=(-i "$SSH_KEY" -o IdentitiesOnly=yes -o IdentityAgent=none -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -o LogLevel=ERROR)

running_instance() {
  "${NF[@]}" get service containers "${NF_SVC[@]}" -o json | python3 -c '
import sys, json
cs = [c for c in json.load(sys.stdin).get("containers", []) if c["status"] == "TASK_RUNNING"]
print(cs[0]["name"] if cs else "")'
}

start_proxy() {
  local log inst
  inst="$(running_instance)"
  [[ -n "$inst" ]] || die "no TASK_RUNNING container (try: ./nf.sh status)"
  log="$(mktemp)"
  "${NF[@]}" ssh service "${NF_SVC[@]}" --instanceName "$inst" --proxyOnly >"$log" 2>&1 &
  PROXY_PID=$!
  trap stop_proxy EXIT
  for _ in $(seq 1 120); do
    local ep
    ep="$(grep -oE '127\.[0-9.]+:[0-9]+' "$log" | head -1 || true)"
    PROXY_ADDR="${ep%%:*}"
    PROXY_PORT="${ep##*:}"
    [[ -n "$ep" ]] && return 0
    kill -0 "$PROXY_PID" 2>/dev/null || { cat "$log" >&2; die "SSH proxy exited (is the service running? try: ./nf.sh status)"; }
    sleep 0.5
  done
  cat "$log" >&2
  die "timed out waiting for SSH proxy endpoint"
}

stop_proxy() {
  [[ -n "$PROXY_PID" ]] && kill "$PROXY_PID" 2>/dev/null || true
}

remote() { # remote "<shell command>" -- runs via login shell (sources ~/.profile)
  ssh "${SSH_OPTS[@]}" -p "$PROXY_PORT" "root@$PROXY_ADDR" "bash -lc $(printf '%q' "$1")"
}

assert_exp5_target() {
  [[ "$PROJECT_ID" == "fda-test" && "$SERVICE_ID" == "fda-node8h2" ]] \
    || die "exp5 v3 is pinned to Central 8xH200: set NF_PROJECT=fda-test NF_SERVICE=fda-node8h2"
}

select_exp5_wide() {
  EXP5_STUDY_VARIANT="v4-wide"
  EXP5_ROOT="/cache/exp5_kronecker_distill/v4-wide"
  EXP5_WANDB_MODE="${EXP5_WANDB_MODE:-offline}"
}

select_exp5_isolated() {
  EXP5_STUDY_VARIANT="v4-isolated"
  EXP5_ROOT="/cache/exp5_kronecker_distill/v4-isolated"
  EXP5_WANDB_MODE="${EXP5_WANDB_MODE:-offline}"
}

select_exp5_normuon_lr() {
  EXP5_STUDY_VARIANT="v5-normuon-lr"
  EXP5_ROOT="/cache/exp5_kronecker_distill/v5-normuon-lr"
  EXP5_WANDB_MODE="${EXP5_WANDB_MODE:-offline}"
}

select_exp5_dense_tied() {
  EXP5_STUDY_VARIANT="v6-dense-tied"
  EXP5_ROOT="/cache/exp5_kronecker_distill/v6-dense-tied"
  EXP5_WANDB_MODE="${EXP5_WANDB_MODE:-offline}"
}

require_remote_idle() {
  echo "==> refusing to proceed unless all GPUs and study coordinators are idle"
  remote '
set -euo pipefail
gpu_pids="$(nvidia-smi --query-compute-apps=pid --format=csv,noheader,nounits | sed "/^[[:space:]]*$/d" | sort -u)"
busy_processes="$(ps -eo pid=,args= | grep -E "[q]wen_|[n]orthflank_coordinator\\.py|[t]orch\\.distributed\\.run" || true)"
if [[ -n "$gpu_pids" || -n "$busy_processes" ]]; then
  echo "Northflank node is busy; no code was synced and exp5 was not started." >&2
  [[ -z "$gpu_pids" ]] || { echo "GPU compute PIDs:" >&2; echo "$gpu_pids" >&2; }
  [[ -z "$busy_processes" ]] || { echo "Study processes:" >&2; echo "$busy_processes" >&2; }
  exit 42
fi
gpu_names="$(nvidia-smi --query-gpu=name --format=csv,noheader)"
gpu_count="$(printf "%s\n" "$gpu_names" | sed "/^[[:space:]]*$/d" | wc -l)"
  if [[ "$gpu_count" -ne 8 ]] || printf "%s\n" "$gpu_names" | grep -qv "H200"; then
    echo "exp5 v3 requires exactly eight H200 GPUs; found:" >&2
  printf "%s\n" "$gpu_names" >&2
  exit 43
fi
'
}

# --- provisioning ---------------------------------------------------------
ensure_ssh_identity() {
  if [[ ! -f "$SSH_KEY" ]]; then
    echo "==> generating SSH key $SSH_KEY"
    ssh-keygen -t ed25519 -N "" -C "northflank-fda" -f "$SSH_KEY"
  fi
  local n
  n="$("${NF[@]}" list ssh-identities "${NF_SCOPE[@]}" -o json | python3 -c 'import sys,json; print(len(json.load(sys.stdin).get("identities",[])))')"
  if [[ "$n" == "0" ]]; then
    echo "==> registering SSH identity"
    "${NF[@]}" add ssh-identities "${NF_SCOPE[@]}" -i "$(python3 - "$SSH_KEY.pub" <<'EOF'
import json, sys
print(json.dumps({
    "name": "fda-node-key",
    "sshPublicKeys": [{"key": open(sys.argv[1]).read().strip()}],
    "restrictions": {"projects": {"enabled": False}, "tags": {"enabled": False, "matchCondition": "or"}},
}))
EOF
)"
  fi
}

render_service_json() {
  sed -e "s|__SERVICE_ID__|$SERVICE_ID|" \
      -e "s|__PLAN__|$PLAN|" \
      -e "s|__IMAGE__|$IMAGE|" \
      -e "s|__GPU_TYPE__|$GPU_TYPE|" \
      -e "s|__GPU_COUNT__|$GPU_COUNT|" \
      -e "s|__EPHEMERAL_MB__|$EPHEMERAL_MB|" \
      "$SCRIPT_DIR/service.json"
}

service_status() {
  "${NF[@]}" get service "${NF_SVC[@]}" -o json 2>/dev/null \
    | python3 -c 'import sys,json; d=json.load(sys.stdin); print(json.dumps(d.get("status",{}).get("deployment",{}).get("status") or d.get("status") or "unknown"))' \
    || echo '"absent"'
}

cmd_up() {
  ensure_ssh_identity

  if ! "${NF[@]}" get project --projectId "$PROJECT_ID" "${NF_SCOPE[@]}" -o json >/dev/null 2>&1; then
    echo "==> creating project $PROJECT_ID in $REGION"
    "${NF[@]}" create project "${NF_SCOPE[@]}" -i \
      "{\"name\": \"$PROJECT_ID\", \"region\": \"$REGION\", \"description\": \"FDA GPU test nodes\"}"
  fi

  if ! "${NF[@]}" get service "${NF_SVC[@]}" -o json >/dev/null 2>&1; then
    echo "==> creating service $SERVICE_ID (${GPU_COUNT}x $GPU_TYPE, $PLAN) -- ~\$22.6/hr"
    local svc_json
    svc_json="$(mktemp --suffix=.json)"
    render_service_json > "$svc_json"
    "${NF[@]}" create service deployment --projectId "$PROJECT_ID" "${NF_SCOPE[@]}" -f "$svc_json"
    rm -f "$svc_json"
  fi

  local vols
  vols="$("${NF[@]}" list volumes --projectId "$PROJECT_ID" "${NF_SCOPE[@]}" -o json | python3 -c '
import sys, json
d = json.load(sys.stdin)
# CLI wraps the list oddly: may be [..], {"volumes": [..]}, or {"0": [..]}
items = d if isinstance(d, list) else next((v for v in d.values() if isinstance(v, list)), [])
print(" ".join(v["name"] for v in items))')"
  if [[ " $vols " != *" $VOLUME_NAME "* ]]; then
    echo "==> creating ${VOLUME_SIZE_MB}MB volume $VOLUME_NAME mounted at /cache"
    "${NF[@]}" create volume --projectId "$PROJECT_ID" "${NF_SCOPE[@]}" -i "$(python3 - "$VOLUME_NAME" "$VOLUME_SIZE_MB" "$SERVICE_ID" <<'EOF'
import json, sys
print(json.dumps({
    "name": sys.argv[1],
    "mounts": [{"containerMountPath": "/cache"}],
    "spec": {"storageSize": int(sys.argv[2]), "accessMode": "ReadWriteMany", "storageClassName": "nf-multi-rw"},
    "attachedObjects": [{"id": sys.argv[3], "type": "service"}],
}))
EOF
)"
  fi

  echo "==> waiting for service to run"
  for _ in $(seq 1 120); do
    local st; st="$(service_status)"
    echo "    status: $st"
    [[ "$st" == *COMPLETED* || "$st" == *RUNNING* || "$st" == *running* ]] && break
    sleep 5
  done
  echo "==> up. Next: ./nf.sh bootstrap   (then ./nf.sh test)"
  echo "==> REMEMBER: ./nf.sh pause when idle -- this node bills ~\$22.6/hr."
}

# --- code sync + env ------------------------------------------------------
do_sync() {
  remote "mkdir -p '$REMOTE_DIR/NEW'"
  rsync -az --delete \
    -e "ssh ${SSH_OPTS[*]} -p $PROXY_PORT" \
    --exclude '.venv' --exclude '.git' --exclude '__pycache__' --exclude '*.pyc' \
    --exclude '.pytest_cache' --exclude '.env' --exclude 'wandb' \
    "$NEW_DIR/" "root@$PROXY_ADDR:$REMOTE_DIR/NEW/"
}

cmd_bootstrap() {
  start_proxy
  echo "==> installing base packages (rsync, git, uv)"
  ssh "${SSH_OPTS[@]}" -p "$PROXY_PORT" "root@$PROXY_ADDR" bash -s <<'EOS'
set -euo pipefail
export DEBIAN_FRONTEND=noninteractive
apt-get update -qq
apt-get install -y -qq --no-install-recommends rsync git curl ca-certificates >/dev/null
command -v uv >/dev/null 2>&1 || curl -LsSf https://astral.sh/uv/install.sh | sh >/dev/null
grep -q '.local/bin' ~/.profile 2>/dev/null || echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.profile
grep -q '.fda_env' ~/.profile 2>/dev/null || echo '[ -f "$HOME/.fda_env" ] && . "$HOME/.fda_env"' >> ~/.profile
EOS

  echo "==> writing remote env (~/.fda_env)"
  {
    echo 'export HF_HOME=/cache/hf'
    echo 'export HF_XET_HIGH_PERFORMANCE=1'
    echo 'export PYTORCH_ALLOC_CONF=expandable_segments:True'
    echo 'export TORCHINDUCTOR_CACHE_DIR=/cache/torchinductor/qwen_fullwidth16'
    # keep `uv run` from re-syncing the venv and uninstalling the
    # modal-image extras that live outside pyproject.toml
    echo 'export UV_NO_SYNC=1'
    if [[ -n "${HF_TOKEN:-}" ]]; then echo "export HF_TOKEN=$HF_TOKEN"; fi
    if [[ -n "${WANDB_API_KEY:-}" ]]; then echo "export WANDB_API_KEY=$WANDB_API_KEY"; fi
  } | ssh "${SSH_OPTS[@]}" -p "$PROXY_PORT" "root@$PROXY_ADDR" "cat > /root/.fda_env"

  echo "==> syncing code"
  do_sync

  echo "==> building python 3.11 env (uv sync + modal-image extras)"
  # Mirrors the Modal image in exp1_fullwidth_distill/modal_app.py.
  # plus accelerate/safetensors/sentencepiece/nvmath and the prebuilt
  # causal_conv1d cp311 wheel. Do not install flash-linear-attention here:
  # its optional Qwen3.5 chunk kernel failed at the large physical batches.
  remote "cd '$REMOTE_DIR/NEW' \
    && uv python install 3.11 \
    && uv sync --python 3.11 --all-packages \
    && uv pip install 'accelerate>=1.14' safetensors sentencepiece 'nvmath-python[cu12]>=0.9' \
    && uv pip install --no-build-isolation \
         'https://github.com/Dao-AILab/causal-conv1d/releases/download/v1.6.2.post1/causal_conv1d-1.6.2.post1%2Bcu12torch2.10cxx11abiTRUE-cp311-cp311-linux_x86_64.whl'"

  echo "==> smoke test: torch sees the GPUs"
  remote "cd '$REMOTE_DIR/NEW' && uv run --no-sync python -c 'import torch; print(torch.__version__, \"gpus:\", torch.cuda.device_count())'"
  echo "==> bootstrap done. Try: ./nf.sh test"
}

cmd_test() {
  start_proxy
  do_sync
  remote "cd '$REMOTE_DIR/NEW' && uv run --no-sync python -m pytest -q"
}

cmd_run() {
  [[ $# -ge 1 ]] || die "usage: ./nf.sh run '<command>'"
  start_proxy
  do_sync
  remote "cd '$REMOTE_DIR/NEW' && $*"
}

cmd_pretrain_plan() {
  start_proxy
  do_sync
  remote "cd '$REMOTE_DIR/NEW' && uv run --no-sync python -m qwen_fullwidth_distill.pretrain plan"
}

cmd_pretrain_prepare() {
  start_proxy
  do_sync
  remote "cd '$REMOTE_DIR/NEW' && uv run --no-sync python -m qwen_fullwidth_distill.pretrain prepare"
}

cmd_pretrain_preflight() {
  start_proxy
  do_sync
  remote "cd '$REMOTE_DIR/NEW' && uv run --no-sync python exp1_fullwidth_distill/northflank_coordinator.py preflight"
}

cmd_pretrain_launch() {
  start_proxy
  do_sync
  local command
  command="cd '$REMOTE_DIR/NEW' && mkdir -p /cache/qwen_fullwidth_distill/next-token-pretrain-v1/logs && nohup uv run --no-sync python exp1_fullwidth_distill/northflank_coordinator.py launch > /cache/qwen_fullwidth_distill/next-token-pretrain-v1/logs/coordinator.log 2>&1 < /dev/null & echo \$!"
  echo "==> launching detached two-wave next-token study"
  remote "$command"
}

cmd_pretrain_status() {
  start_proxy
  remote "cd '$REMOTE_DIR/NEW' && uv run --no-sync python exp1_fullwidth_distill/northflank_coordinator.py status && nvidia-smi --query-gpu=index,utilization.gpu,memory.used,memory.total --format=csv,noheader"
}

cmd_pretrain_logs() {
  start_proxy
  remote "tail -n 120 /cache/qwen_fullwidth_distill/next-token-pretrain-v1/logs/coordinator.log 2>/dev/null || true"
}

cmd_normuon_plan() {
  start_proxy
  do_sync
  remote "cd '$REMOTE_DIR/NEW' && uv run --no-sync python -m qwen_normuon_pretrain plan"
}

cmd_normuon_prepare() {
  start_proxy
  do_sync
  remote "cd '$REMOTE_DIR/NEW' && uv run --no-sync python -m qwen_normuon_pretrain prepare"
}

cmd_normuon_preflight() {
  start_proxy
  do_sync
  remote "cd '$REMOTE_DIR/NEW' && uv run --no-sync python exp3_normuon_pretrain/northflank_coordinator.py preflight"
}

cmd_normuon_launch() {
  start_proxy
  do_sync
  local command
  command="cd '$REMOTE_DIR/NEW' && mkdir -p /cache/qwen_normuon_pretrain/v1/logs && nohup uv run --no-sync python exp3_normuon_pretrain/northflank_coordinator.py launch > /cache/qwen_normuon_pretrain/v1/logs/coordinator.log 2>&1 < /dev/null & echo \$!"
  echo "==> launching detached staged NorMuon study"
  remote "$command"
}

cmd_normuon_status() {
  start_proxy
  remote "cd '$REMOTE_DIR/NEW' && uv run --no-sync python exp3_normuon_pretrain/northflank_coordinator.py status && nvidia-smi --query-gpu=index,utilization.gpu,memory.used,memory.total --format=csv,noheader"
}

cmd_normuon_logs() {
  start_proxy
  remote "tail -n 120 /cache/qwen_normuon_pretrain/v1/logs/coordinator.log 2>/dev/null || true"
}

cmd_adamw_queue() {
  start_proxy
  do_sync
  local command
  command="cd '$REMOTE_DIR/NEW' && mkdir -p /cache/qwen_normuon_pretrain/v1/adamw-control && setsid -f uv run --no-sync python exp4_adamw_control/northflank_coordinator.py queue > /cache/qwen_normuon_pretrain/v1/adamw-control/coordinator.log 2>&1 < /dev/null && sleep 1 && pgrep -f 'exp4_adamw_control/northflank_coordinator.py queue' | tail -1"
  echo "==> queueing winner-matched AdamW control"
  remote "$command"
}

cmd_adamw_status() {
  start_proxy
  remote "cd '$REMOTE_DIR/NEW' && uv run --no-sync python exp4_adamw_control/northflank_coordinator.py status"
}

cmd_adamw_logs() {
  start_proxy
  remote "tail -n 120 /cache/qwen_normuon_pretrain/v1/adamw-control/coordinator.log 2>/dev/null || true; tail -n 120 /cache/qwen_normuon_pretrain/v1/adamw-control/control.log 2>/dev/null || true"
}

cmd_exp5_plan() {
  (
    cd "$NEW_DIR"
    QWEN_KRON_STUDY="$EXP5_STUDY_VARIANT" \
      uv run --no-sync python -m qwen_kron_distill plan
  )
}

cmd_exp5_preflight() {
  assert_exp5_target
  start_proxy
  require_remote_idle
  do_sync
  remote "cd '$REMOTE_DIR/NEW' && uv sync --python 3.11 --all-packages && QWEN_KRON_STUDY='$EXP5_STUDY_VARIANT' QWEN_KRON_PREFLIGHT_MICROBATCHES='$EXP5_PREFLIGHT_BATCHES' uv run --no-sync python -m qwen_kron_distill preflight --output-root '$EXP5_ROOT'"
}

cmd_exp5_preflight_report() {
  assert_exp5_target
  start_proxy
  remote "cd '$REMOTE_DIR/NEW' && uv run --no-sync python -m json.tool '$EXP5_ROOT/preflight.json'"
}

cmd_exp5_preflight_logs() {
  assert_exp5_target
  start_proxy
  remote "for path in '$EXP5_ROOT'/preflight/*.log; do echo \"=== \$path\"; tail -n 100 \"\$path\"; done"
}

cmd_exp5_launch() {
  assert_exp5_target
  start_proxy
  require_remote_idle
  do_sync
  local command
  command="cd '$REMOTE_DIR/NEW' && uv sync --python 3.11 --all-packages && mkdir -p '$EXP5_ROOT/logs' '$EXP5_ROOT/wandb' && QWEN_KRON_STUDY='$EXP5_STUDY_VARIANT' WANDB_MODE='$EXP5_WANDB_MODE' WANDB_DIR='$EXP5_ROOT/wandb' setsid -f uv run --no-sync python -m qwen_kron_distill launch --output-root '$EXP5_ROOT' > '$EXP5_ROOT/logs/coordinator.log' 2>&1 < /dev/null && sleep 2 && pid=\$(pgrep -f '[q]wen_kron_distill launch' | tail -1); [[ -n \"\$pid\" ]] || { tail -n 80 '$EXP5_ROOT/logs/coordinator.log' >&2; exit 44; }; echo \"\$pid\""
  echo "==> launching detached tensor-native Kronecker distillation study"
  remote "$command"
}

cmd_exp5_status() {
  assert_exp5_target
  start_proxy
  remote "cd '$REMOTE_DIR/NEW' && QWEN_KRON_STUDY='$EXP5_STUDY_VARIANT' uv run --no-sync python -m qwen_kron_distill status --output-root '$EXP5_ROOT' && nvidia-smi --query-gpu=index,name,utilization.gpu,memory.used,memory.total --format=csv,noheader"
}

cmd_exp5_logs() {
  assert_exp5_target
  start_proxy
  remote "tail -n 160 '$EXP5_ROOT/logs/coordinator.log' 2>/dev/null || true; find '$EXP5_ROOT/logs' -maxdepth 1 -type f ! -name coordinator.log -printf '%T@ %p\n' 2>/dev/null | sort -nr | head -1 | cut -d' ' -f2- | xargs -r tail -n 160"
}

cmd_exp5_checkpoints() {
  assert_exp5_target
  start_proxy
  remote "find '$EXP5_ROOT' -path '*/checkpoint.pt' -printf '%TY-%Tm-%TdT%TH:%TM:%TSZ %s %p\n' 2>/dev/null | sort"
}

cmd_exp5_progress() {
  assert_exp5_target
  start_proxy
  remote "cd '$REMOTE_DIR/NEW' && CUDA_VISIBLE_DEVICES='' uv run --no-sync python -c 'import glob,json,torch; paths=glob.glob(\"$EXP5_ROOT/*/*/checkpoint.pt\"); values=[]; [(lambda value: values.append({\"path\": path, \"stage\": value[\"cell\"][\"stage\"], \"label\": value[\"cell\"][\"label\"], \"examples_seen\": int(value[\"examples_seen\"])}))(torch.load(path,map_location=\"cpu\",mmap=True,weights_only=True)) for path in paths]; print(json.dumps(sorted(values,key=lambda value:(value[\"stage\"],value[\"label\"])),indent=2))'"
}

cmd_exp5_audit() {
  assert_exp5_target
  start_proxy
  remote "cd '$REMOTE_DIR/NEW' && QWEN_KRON_STUDY='$EXP5_STUDY_VARIANT' uv run --no-sync python -m qwen_kron_distill audit --output-root '$EXP5_ROOT'"
}

cmd_exp5_summary() {
  assert_exp5_target
  start_proxy
  remote "cd '$REMOTE_DIR/NEW' && uv run --no-sync python -m json.tool '$EXP5_ROOT/study-summary.json'"
}

cmd_exp5_stop() {
  assert_exp5_target
  start_proxy
  remote "
set -euo pipefail
echo '==> stopping exp5 processes for $EXP5_ROOT after durable checkpoint'
pkill -TERM -f '[q]wen_kron_distill' 2>/dev/null || true
for _ in \$(seq 1 30); do
  pgrep -af '[q]wen_kron_distill' >/dev/null || break
  sleep 1
done
if pgrep -af '[q]wen_kron_distill'; then
  echo 'exp5 processes did not exit after SIGTERM' >&2
  exit 45
fi
nvidia-smi --query-gpu=index,utilization.gpu,memory.used --format=csv,noheader
"
}

cmd_exp5_data_status() {
  start_proxy
  remote "if pgrep -af '[q]wen_normuon_pretrain.pretrain prepare'; then echo 'status: running'; else echo 'status: not-running'; fi; test -f /cache/qwen_fullwidth_distill/context16-fineweb-edu-next-token-4m-v1/manifest.json && echo 'dataset: complete' || echo 'dataset: incomplete'; tail -n 40 /cache/exp5_kronecker_distill/v3-khatri-rao/logs/data-prepare.log 2>/dev/null || true"
}

cmd_wandb_status() {
  start_proxy
  remote "if test -f /root/.netrc && grep -q 'api.wandb.ai' /root/.netrc; then echo 'wandb-auth: present'; else echo 'wandb-auth: absent'; fi"
}

cmd_status() {
  "${NF[@]}" get service "${NF_SVC[@]}" -o json | python3 -c '
import sys, json
d = json.load(sys.stdin)
print("id:      ", d.get("id"))
print("status:  ", json.dumps(d.get("status")))
dep = d.get("deployment") or {}
print("gpu:     ", json.dumps(dep.get("gpu")))
print("instances:", dep.get("instances"))
print("plan:    ", (d.get("billing") or {}).get("deploymentPlan"))
'
  local st; st="$(service_status)"
  if [[ "$st" == *COMPLETED* || "$st" == *RUNNING* || "$st" == *running* ]]; then
    start_proxy
    remote "nvidia-smi --query-gpu=index,name,memory.total,utilization.gpu --format=csv,noheader" || true
  fi
}

# --- lifecycle ------------------------------------------------------------
cmd_pause()  { "${NF[@]}" pause service "${NF_SVC[@]}"; echo "==> paused (billing stopped; /cache volume persists)"; }
cmd_resume() {
  "${NF[@]}" resume service "${NF_SVC[@]}" -i '{"instances":1}'
  echo "==> resuming; ./nf.sh status to watch"
}

cmd_down() {
  read -r -p "Delete service $SERVICE_ID? (volume $VOLUME_NAME is kept) [y/N] " ans
  [[ "$ans" == "y" || "$ans" == "Y" ]] || die "aborted"
  "${NF[@]}" delete service "${NF_SVC[@]}"
}

# --- dispatch -------------------------------------------------------------
cmd="${1:-}"; shift || true
case "$cmd" in
  up)        cmd_up ;;
  bootstrap) cmd_bootstrap ;;
  sync)      start_proxy; do_sync ;;
  test)      cmd_test ;;
  run)       cmd_run "$@" ;;
  ssh)       exec "${NF[@]}" ssh service "${NF_SVC[@]}" ;;
  proxy)     "${NF[@]}" ssh service "${NF_SVC[@]}" --proxyOnly ;;
  status)    cmd_status ;;
  pause)     cmd_pause ;;
  resume)    cmd_resume ;;
  down)      cmd_down ;;
  pretrain-plan)      cmd_pretrain_plan ;;
  pretrain-prepare)   cmd_pretrain_prepare ;;
  pretrain-preflight) cmd_pretrain_preflight ;;
  pretrain-launch)    cmd_pretrain_launch ;;
  pretrain-status)    cmd_pretrain_status ;;
  pretrain-logs)      cmd_pretrain_logs ;;
  normuon-plan)       cmd_normuon_plan ;;
  normuon-prepare)    cmd_normuon_prepare ;;
  normuon-preflight)  cmd_normuon_preflight ;;
  normuon-launch)     cmd_normuon_launch ;;
  normuon-status)     cmd_normuon_status ;;
  normuon-logs)       cmd_normuon_logs ;;
  adamw-queue)        cmd_adamw_queue ;;
  adamw-status)       cmd_adamw_status ;;
  adamw-logs)         cmd_adamw_logs ;;
  exp5-plan)          cmd_exp5_plan ;;
  exp5-preflight)     cmd_exp5_preflight ;;
  exp5-preflight-report) cmd_exp5_preflight_report ;;
  exp5-preflight-logs) cmd_exp5_preflight_logs ;;
  exp5-launch)        cmd_exp5_launch ;;
  exp5-status)        cmd_exp5_status ;;
  exp5-logs)          cmd_exp5_logs ;;
  exp5-checkpoints)   cmd_exp5_checkpoints ;;
  exp5-progress)      cmd_exp5_progress ;;
  exp5-audit)         cmd_exp5_audit ;;
  exp5-summary)       cmd_exp5_summary ;;
  exp5-data-status)   cmd_exp5_data_status ;;
  exp5-wide-plan)          select_exp5_wide; cmd_exp5_plan ;;
  exp5-wide-preflight)     select_exp5_wide; cmd_exp5_preflight ;;
  exp5-wide-preflight-report) select_exp5_wide; cmd_exp5_preflight_report ;;
  exp5-wide-preflight-logs) select_exp5_wide; cmd_exp5_preflight_logs ;;
  exp5-wide-launch)        select_exp5_wide; cmd_exp5_launch ;;
  exp5-wide-status)        select_exp5_wide; cmd_exp5_status ;;
  exp5-wide-logs)          select_exp5_wide; cmd_exp5_logs ;;
  exp5-wide-checkpoints)   select_exp5_wide; cmd_exp5_checkpoints ;;
  exp5-wide-progress)      select_exp5_wide; cmd_exp5_progress ;;
  exp5-wide-audit)         select_exp5_wide; cmd_exp5_audit ;;
  exp5-wide-summary)       select_exp5_wide; cmd_exp5_summary ;;
  exp5-wide-stop)          select_exp5_wide; cmd_exp5_stop ;;
  exp5-isolated-plan)          select_exp5_isolated; cmd_exp5_plan ;;
  exp5-isolated-preflight)     select_exp5_isolated; cmd_exp5_preflight ;;
  exp5-isolated-preflight-report) select_exp5_isolated; cmd_exp5_preflight_report ;;
  exp5-isolated-preflight-logs) select_exp5_isolated; cmd_exp5_preflight_logs ;;
  exp5-isolated-launch)        select_exp5_isolated; cmd_exp5_launch ;;
  exp5-isolated-status)        select_exp5_isolated; cmd_exp5_status ;;
  exp5-isolated-logs)          select_exp5_isolated; cmd_exp5_logs ;;
  exp5-isolated-checkpoints)   select_exp5_isolated; cmd_exp5_checkpoints ;;
  exp5-isolated-progress)      select_exp5_isolated; cmd_exp5_progress ;;
  exp5-isolated-audit)         select_exp5_isolated; cmd_exp5_audit ;;
  exp5-isolated-summary)       select_exp5_isolated; cmd_exp5_summary ;;
  exp5-isolated-stop)          select_exp5_isolated; cmd_exp5_stop ;;
  exp5-normuon-plan)          select_exp5_normuon_lr; cmd_exp5_plan ;;
  exp5-normuon-preflight)     select_exp5_normuon_lr; cmd_exp5_preflight ;;
  exp5-normuon-preflight-report) select_exp5_normuon_lr; cmd_exp5_preflight_report ;;
  exp5-normuon-preflight-logs) select_exp5_normuon_lr; cmd_exp5_preflight_logs ;;
  exp5-normuon-launch)        select_exp5_normuon_lr; cmd_exp5_launch ;;
  exp5-normuon-status)        select_exp5_normuon_lr; cmd_exp5_status ;;
  exp5-normuon-logs)          select_exp5_normuon_lr; cmd_exp5_logs ;;
  exp5-normuon-checkpoints)   select_exp5_normuon_lr; cmd_exp5_checkpoints ;;
  exp5-normuon-progress)      select_exp5_normuon_lr; cmd_exp5_progress ;;
  exp5-normuon-audit)         select_exp5_normuon_lr; cmd_exp5_audit ;;
  exp5-normuon-summary)       select_exp5_normuon_lr; cmd_exp5_summary ;;
  exp5-normuon-stop)          select_exp5_normuon_lr; cmd_exp5_stop ;;
  exp5-dense-plan)          select_exp5_dense_tied; cmd_exp5_plan ;;
  exp5-dense-preflight)     select_exp5_dense_tied; cmd_exp5_preflight ;;
  exp5-dense-preflight-report) select_exp5_dense_tied; cmd_exp5_preflight_report ;;
  exp5-dense-preflight-logs) select_exp5_dense_tied; cmd_exp5_preflight_logs ;;
  exp5-dense-launch)        select_exp5_dense_tied; cmd_exp5_launch ;;
  exp5-dense-status)        select_exp5_dense_tied; cmd_exp5_status ;;
  exp5-dense-logs)          select_exp5_dense_tied; cmd_exp5_logs ;;
  exp5-dense-checkpoints)   select_exp5_dense_tied; cmd_exp5_checkpoints ;;
  exp5-dense-progress)      select_exp5_dense_tied; cmd_exp5_progress ;;
  exp5-dense-audit)         select_exp5_dense_tied; cmd_exp5_audit ;;
  exp5-dense-summary)       select_exp5_dense_tied; cmd_exp5_summary ;;
  exp5-dense-stop)          select_exp5_dense_tied; cmd_exp5_stop ;;
  wandb-status)       cmd_wandb_status ;;
  *) die "usage: ./nf.sh {up|bootstrap|sync|test|run <cmd>|ssh|proxy|status|pause|resume|down|wandb-status|pretrain-plan|pretrain-prepare|pretrain-preflight|pretrain-launch|pretrain-status|pretrain-logs|normuon-plan|normuon-prepare|normuon-preflight|normuon-launch|normuon-status|normuon-logs|adamw-queue|adamw-status|adamw-logs|exp5-plan|exp5-preflight|exp5-preflight-report|exp5-preflight-logs|exp5-launch|exp5-status|exp5-logs|exp5-checkpoints|exp5-progress|exp5-audit|exp5-summary|exp5-data-status|exp5-wide-plan|exp5-wide-preflight|exp5-wide-preflight-report|exp5-wide-preflight-logs|exp5-wide-launch|exp5-wide-status|exp5-wide-logs|exp5-wide-checkpoints|exp5-wide-progress|exp5-wide-audit|exp5-wide-summary|exp5-wide-stop}" ;;
esac
