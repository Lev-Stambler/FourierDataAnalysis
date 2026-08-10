"""Losslessly separate historical v2.5 artifacts from the frozen v2.4 run."""

from __future__ import annotations

import gzip
import hashlib
import json
import sys
from pathlib import Path

ROOT = Path(__file__).parent.parent
sys.path.insert(0, str(ROOT))

from dlx.profiles.sampled_degree import geometric_sampled_features

OLD = ROOT / "runs/local/v24_spectrum_predictor"
NEW = ROOT / "runs/local/v25_kiss_diagnostic"


def _sha256(payload: bytes) -> str:
    return hashlib.sha256(payload).hexdigest()


def migrate() -> dict:
    source_profiles = OLD / "higher_degree_profiles"
    existing_manifest = NEW / "manifest.json"
    if not source_profiles.exists():
        if existing_manifest.exists():
            return json.loads(existing_manifest.read_text())
        raise FileNotFoundError(f"missing historical profiles: {source_profiles}")
    profiles = NEW / "profiles"
    audits = NEW / "audit_chains"
    profiles.mkdir(parents=True, exist_ok=True)
    audits.mkdir(parents=True, exist_ok=True)
    records = []
    for source in sorted(source_profiles.glob("*.json")):
        source_bytes = source.read_bytes()
        profile = json.loads(source_bytes)
        chains = profile.pop("chains")
        profile["geometric_sampled_features"] = geometric_sampled_features(
            chains,
            tuple(range(1, int(profile["n_coordinates"]) + 1)),
            feature_degree=3,
            q=256,
        )
        summary_bytes = (json.dumps(profile, indent=2) + "\n").encode()
        summary_path = profiles / source.name
        summary_path.write_bytes(summary_bytes)
        audit = {
            "dataset": profile["dataset"],
            "data_sha256": profile["data_sha256"],
            "source_profile_sha256": _sha256(source_bytes),
            "summary_sha256": _sha256(summary_bytes),
            "chains": chains,
        }
        audit_bytes = json.dumps(audit, separators=(",", ":")).encode()
        audit_path = audits / f"{source.stem}.json.gz"
        audit_path.write_bytes(gzip.compress(audit_bytes, compresslevel=9, mtime=0))
        historical_summary = dict(profile)
        historical_summary.pop("geometric_sampled_features")
        restored = {
            **historical_summary,
            "chains": json.loads(gzip.decompress(audit_path.read_bytes()))["chains"],
        }
        if restored != json.loads(source_bytes):
            raise ValueError(f"lossless migration check failed: {source.name}")
        records.append(
            {
                "dataset": profile["dataset"],
                "source": str(source.relative_to(ROOT)),
                "source_profile_sha256": _sha256(source_bytes),
                "summary": str(summary_path.relative_to(ROOT)),
                "summary_sha256": _sha256(summary_bytes),
                "audit": str(audit_path.relative_to(ROOT)),
                "audit_sha256": _sha256(audit_path.read_bytes()),
            }
        )

    old_analysis = OLD / "v25_kiss_ols_analysis.json"
    analysis = NEW / "analysis.json"
    analysis.write_bytes(old_analysis.read_bytes())
    manifest = {
        "status": "post-v2.4 exploratory diagnostic; not a frozen confirmation",
        "migration": "lossless chain split; frozen v1-v2.4 artifacts unchanged",
        "source_analysis": str(old_analysis.relative_to(ROOT)),
        "source_analysis_sha256": _sha256(old_analysis.read_bytes()),
        "analysis": str(analysis.relative_to(ROOT)),
        "analysis_sha256": _sha256(analysis.read_bytes()),
        "profiles": records,
    }
    manifest_path = NEW / "manifest.json"
    manifest_path.write_text(json.dumps(manifest, indent=2) + "\n")

    for source in source_profiles.glob("*.json"):
        source.unlink()
    source_profiles.rmdir()
    old_analysis.unlink()
    return manifest


if __name__ == "__main__":
    result = migrate()
    print(f"migrated {len(result['profiles'])} profiles to {NEW.relative_to(ROOT)}")
