from __future__ import annotations

import argparse
import json
from pathlib import Path

from .controller import audit, doctor, load_manifest, lock_plan, run_stage, stage_by_id


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("command", choices=("plan", "doctor", "run", "audit"))
    parser.add_argument("manifest")
    parser.add_argument("--stage")
    parser.add_argument("--state-root", default="/cache/research-control")
    args = parser.parse_args()
    manifest_path = Path(args.manifest).resolve()
    manifest = load_manifest(manifest_path)
    manifest_dir = (
        manifest_path.parent / manifest.get("working_directory", ".")
    ).resolve()
    state_root = Path(args.state_root) / manifest["experiment_id"]
    if args.command == "plan":
        locked_path = lock_plan(manifest, state_root)
        value = {
            "manifest": manifest,
            "locked_plan": str(locked_path),
            "stages": [
                {
                    "id": stage["id"],
                    "tier": stage["tier"],
                    "projected_dollars": (
                        0.0
                        if stage["tier"] == "local"
                        else stage["gpu_count"]
                        * stage["price_per_gpu_hour"]
                        * stage["max_wall_seconds"]
                        / 3600
                    ),
                }
                for stage in manifest["stages"]
            ],
        }
    elif args.command == "audit":
        value = audit(manifest, state_root=state_root)
    else:
        if not args.stage:
            parser.error("--stage is required for doctor/run")
        stage_by_id(manifest, args.stage)
        if args.command == "doctor":
            value = doctor(
                manifest,
                args.stage,
                state_root=state_root,
                manifest_dir=manifest_dir,
            )
        else:
            value = run_stage(
                manifest,
                args.stage,
                state_root=state_root,
                manifest_dir=manifest_dir,
            )
    print(json.dumps(value, indent=2, sort_keys=True))


if __name__ == "__main__":
    main()
