#!/usr/bin/env bash
set -euo pipefail

experiment_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cache_root="${V2_CACHE_ROOT:-/cache/v2_sbd}"
environment="$cache_root/env"
export VIRTUAL_ENV="$environment"
export UV_CACHE_DIR="${UV_CACHE_DIR:-/cache/uv}"
export UV_PYTHON_INSTALL_DIR="${UV_PYTHON_INSTALL_DIR:-$cache_root/uv-python}"
export PATH="/root/.local/bin:$PATH"

if ! command -v curl >/dev/null || ! command -v git >/dev/null; then
  export DEBIAN_FRONTEND=noninteractive
  apt-get update -qq
  apt-get install -y -qq --no-install-recommends curl ca-certificates git >/dev/null
fi
command -v uv >/dev/null || curl -LsSf https://astral.sh/uv/install.sh | sh >/dev/null

mkdir -p "$cache_root" "$UV_CACHE_DIR" "$UV_PYTHON_INSTALL_DIR"
dependency_hash="$({ sha256sum "$experiment_dir/pyproject.toml"; test ! -f "$experiment_dir/uv.lock" || sha256sum "$experiment_dir/uv.lock"; } | sha256sum | cut -d' ' -f1)"
installed_hash="$(cat "$environment/.dependency-sha256" 2>/dev/null || true)"

if [[ ! -x "$environment/bin/python" || "$dependency_hash" != "$installed_hash" ]]; then
  uv python install 3.11
  rm -rf "$environment"
  uv venv --python 3.11 "$environment"
  if [[ -f "$experiment_dir/uv.lock" ]]; then
    uv sync --frozen --all-extras --no-install-project --active \
      --project "$experiment_dir"
  else
    uv pip install --python "$environment/bin/python" -e "$experiment_dir[data,dev]"
  fi
fi

# Refresh only the editable source link after every sync; dependencies remain
# resident on the persistent volume across replacement containers.
uv pip install --python "$environment/bin/python" --no-deps -e "$experiment_dir" >/dev/null
"$environment/bin/python" - <<'PY'
import torch
import transformers
assert torch.cuda.device_count() == 8, torch.cuda.device_count()
print({"torch": torch.__version__, "transformers": transformers.__version__, "gpus": 8})
PY
printf '%s\n' "$dependency_hash" > "$environment/.dependency-sha256"
