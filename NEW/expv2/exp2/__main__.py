from __future__ import annotations

import argparse
import json
from pathlib import Path

from .audit import local_audit
from .budget import CalibrationBudget
from .campaign import run_positive_control
from .full_control import run_full_control
from .infra import provision
from .lifecycle import delete_now, schedule_delete
from .matched_comparison import run_matched_comparison
from .matched_remote import run_remote as run_matched_remote
from .two_hop_debug import local_debug_audit, run_joint_two_hop, run_two_hop_debug


def main() -> None:
    parser = argparse.ArgumentParser(prog="python -m expv2.exp2")
    commands = parser.add_subparsers(dest="command", required=True)
    audit = commands.add_parser("audit")
    audit.add_argument("--output", type=Path)
    budget = commands.add_parser("budget")
    budget.add_argument("--batch-contexts", type=int, required=True)
    budget.add_argument("--optimizer-updates", type=int, default=1_000)
    budget.add_argument("--minimum-contexts", type=int, default=100_000)
    control = commands.add_parser("positive-control")
    control.add_argument("--output-root", type=Path, required=True)
    control.add_argument("--result", type=Path, required=True)
    control.add_argument("--preflight", type=Path)
    debug_audit = commands.add_parser("two-hop-debug-audit")
    debug_audit.add_argument("--output", type=Path)
    debug = commands.add_parser("two-hop-debug")
    debug.add_argument("--output-root", type=Path, required=True)
    debug.add_argument("--result", type=Path, required=True)
    debug.add_argument("--preflight", type=Path)
    joint = commands.add_parser("joint-two-hop")
    joint.add_argument("--output-root", type=Path, required=True)
    joint.add_argument("--result", type=Path, required=True)
    joint.add_argument("--preflight", type=Path)
    full = commands.add_parser("full-control")
    full.add_argument("--output-root", type=Path, required=True)
    full.add_argument("--result", type=Path, required=True)
    full.add_argument("--preflight", type=Path)
    matched = commands.add_parser("matched-muon")
    matched.add_argument("--output-root", type=Path, required=True)
    matched.add_argument("--result", type=Path, required=True)
    matched.add_argument("--heartbeat", type=Path)
    matched_cloud = commands.add_parser("launch-matched-cloud")
    matched_cloud.add_argument("--repo-root", type=Path, default=Path("."))
    matched_cloud.add_argument("--local-output", type=Path, required=True)
    matched_cloud.add_argument("--result", type=Path, required=True)
    matched_cloud.add_argument("--timeout-seconds", type=int, default=10_800)
    provision_value = commands.add_parser("provision-h100")
    provision_value.add_argument("--output", type=Path, required=True)
    provision_value.add_argument("--timeout-seconds", type=int, default=900)
    commands.add_parser("delete-service")
    commands.add_parser("delete-service-now")
    args = parser.parse_args()
    if args.command == "audit":
        result = local_audit(args.output)
    elif args.command == "budget":
        result = CalibrationBudget(
            batch_contexts=args.batch_contexts,
            optimizer_updates=args.optimizer_updates,
            minimum_contexts=args.minimum_contexts,
        ).as_dict()
    elif args.command == "positive-control":
        result = run_positive_control(
            output_root=args.output_root,
            result_path=args.result,
            preflight_path=args.preflight,
        )
    elif args.command == "two-hop-debug-audit":
        result = local_debug_audit(args.output)
    elif args.command == "two-hop-debug":
        result = run_two_hop_debug(
            output_root=args.output_root,
            result_path=args.result,
            preflight_path=args.preflight,
        )
    elif args.command == "joint-two-hop":
        result = run_joint_two_hop(
            output_root=args.output_root,
            result_path=args.result,
            preflight_path=args.preflight,
        )
    elif args.command == "full-control":
        result = run_full_control(
            output_root=args.output_root,
            result_path=args.result,
            preflight_path=args.preflight,
        )
    elif args.command == "matched-muon":
        result = run_matched_comparison(
            output_root=args.output_root,
            result_path=args.result,
            heartbeat_path=args.heartbeat,
        )
    elif args.command == "launch-matched-cloud":
        result = run_matched_remote(
            repo_root=args.repo_root,
            local_output=args.local_output,
            result_path=args.result,
            timeout_seconds=args.timeout_seconds,
        )
    elif args.command == "provision-h100":
        result = provision(args.output, timeout_seconds=args.timeout_seconds)
    elif args.command == "delete-service":
        schedule_delete()
        result = {"status": "complete", "action": "delete-scheduled"}
    else:
        delete_now()
        result = {"status": "complete", "action": "service-deleted"}
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)


if __name__ == "__main__":
    main()
