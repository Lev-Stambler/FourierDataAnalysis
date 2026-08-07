"""Run manifests: a run without a complete manifest does not exist (PLAN §12.6)."""

from __future__ import annotations

import json
from pathlib import Path

REQUIRED_FIELDS = (
    "protocol_hash",
    "family_version",
    "cell_id",
    "seed",
    "budget_tokens",
    "device",
    "torch_version",
    "wallclock_seconds",
    "config_hash",
    "metrics_path",
    "status",
)


def write_manifest(run_dir: Path, fields: dict) -> Path:
    run_dir = Path(run_dir)
    run_dir.mkdir(parents=True, exist_ok=True)
    missing = [k for k in REQUIRED_FIELDS if k not in fields or fields[k] in (None, "")]
    if missing:
        raise ValueError(f"incomplete manifest, missing fields: {missing}")
    path = run_dir / "manifest.json"
    path.write_text(json.dumps(fields, indent=2, sort_keys=True))
    return path


def load_manifest(run_dir: Path) -> dict:
    """Load and validate a manifest; refuse incomplete ones (analysis discipline)."""
    path = Path(run_dir) / "manifest.json"
    if not path.exists():
        raise FileNotFoundError(f"no manifest at {path} — run does not exist")
    data = json.loads(path.read_text())
    missing = [k for k in REQUIRED_FIELDS if k not in data or data[k] in (None, "")]
    if missing:
        raise ValueError(f"incomplete manifest at {path}: {missing}")
    return data
