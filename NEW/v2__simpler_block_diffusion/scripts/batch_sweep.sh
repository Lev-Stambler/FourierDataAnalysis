#!/usr/bin/env bash
set -euo pipefail

experiment_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
state_root="${V2_SWEEP_ROOT:-/cache/v2_sbd/batch-sweep}"
mkdir -p "$state_root"

# Search downward from an ambitious physical batch and retain a deliberately
# underfilled baseline. Each candidate is a fresh eight-way DDP process so a
# CUDA OOM cannot poison the next measurement. V2_SWEEP_BATCHES permits a
# focused rerun after an implementation-only optimization.
sweep_batches="${V2_SWEEP_BATCHES:-80 72 64 56 48 40 32 24 16 12 1}"
for batch in $sweep_batches; do
  name="batch-b${batch}"
  if [[ -f "$state_root/$name/result.json" || -f "$state_root/$name/failed.json" ]]; then
    continue
  fi
  if V2_MODE=preflight \
     V2_MICROBATCH="$batch" \
     V2_GRADIENT_ACCUMULATION=1 \
     V2_TARGET_TOKENS=100000000 \
     V2_RUN_NAME="$name" \
     V2_OUTPUT="$state_root/$name" \
     "$experiment_dir/scripts/run_h100.sh" --preflight-steps 5 --synthetic-data; then
    :
  else
    exit_code="$?"
    "${V2_PYTHON:-/cache/v2_sbd/env/bin/python}" - "$state_root/$name" "$batch" "$exit_code" <<'PY'
import json
import pathlib
import sys

root = pathlib.Path(sys.argv[1])
launch_path = root / "launch.json"
launch = json.loads(launch_path.read_text()) if launch_path.exists() else {}
(root / "failed.json").write_text(json.dumps({
    "schema": "v2-sbd-batch-candidate-failure-v1",
    "status": "failed",
    "batch": int(sys.argv[2]),
    "exit_code": int(sys.argv[3]),
    "wandb_url": launch.get("wandb_url", ""),
}, indent=2, sort_keys=True) + "\n")
PY
  fi
done

"${V2_PYTHON:-/cache/v2_sbd/env/bin/python}" - "$state_root" <<'PY'
import json
import pathlib
import sys

root = pathlib.Path(sys.argv[1])
rows = []
for path in root.glob("batch-b*/result.json"):
    result = json.loads(path.read_text())
    metrics = result.get("last_metrics", {})
    rows.append({
        "batch": int(path.parent.name.removeprefix("batch-b")),
        "status": result.get("status"),
        "global_target_tokens": metrics.get("train/global_target_tokens_per_step", 0),
        "target_tokens_per_second": metrics.get("train/target_tokens_per_second", 0),
        "gpu_utilization_mean": metrics.get("system/gpu_utilization_mean", 0),
        "gpu_utilization_min_rank_median": metrics.get("system/gpu_utilization_min_rank_median", 0),
        "peak_reserved_gib": result.get("peak_reserved_gib", 0),
        "wandb_url": result.get("wandb_url", ""),
    })
eligible = [
    row for row in rows
    if row["status"] == "preflight_complete"
    and row["global_target_tokens"] >= 100_000
    and row["gpu_utilization_mean"] >= 85
    and row["gpu_utilization_min_rank_median"] >= 85
]
baseline = next((row for row in rows if row["batch"] == 1), None)
if not eligible or baseline is None:
    raise SystemExit("sweep lacks an eligible batch or batch-1 baseline")
selected = max(eligible, key=lambda row: row["target_tokens_per_second"])
ratio = selected["target_tokens_per_second"] / baseline["target_tokens_per_second"]
failures = [json.loads(path.read_text()) for path in root.glob("batch-b*/failed.json")]
artifact = {
    "schema": "v2-sbd-batch-sweep-v1",
    "rows": sorted(rows, key=lambda row: -row["batch"]),
    "failures": sorted(failures, key=lambda row: -row["batch"]),
    "selected": selected,
    "speedup_over_batch1": ratio,
    "speedup_target_met": ratio >= 10,
}
(root / "selection.json").write_text(json.dumps(artifact, indent=2, sort_keys=True) + "\n")
print(json.dumps(artifact, indent=2, sort_keys=True))
if ratio < 10:
    print(f"WARNING: selected batch improves only {ratio:.2f}x over batch 1", file=sys.stderr)
PY
