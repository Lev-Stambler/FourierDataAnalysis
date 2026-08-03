from __future__ import annotations

import argparse
import json
from pathlib import Path

from .campaign import run_campaign, run_synthetic_only
from .data import DEFAULT_DATA_ROOT, prepare_tinystories, validate_manifest
from .diagnostics import local_audit
from .lifecycle import delete_now, schedule_delete
from .model import inventory
from .preflight import compilation_probe, paid_preflight
from .infra import provision
from .utils import atomic_json


def parser() -> argparse.ArgumentParser:
    value = argparse.ArgumentParser(prog="python -m expv2.exp1")
    commands = value.add_subparsers(dest="command", required=True)
    commands.add_parser("inventory")

    audit = commands.add_parser("local-audit")
    audit.add_argument("--output", type=Path)

    data = commands.add_parser("prepare-tinystories")
    data.add_argument("--output-root", type=Path, default=DEFAULT_DATA_ROOT)
    data.add_argument("--tokenizer-documents", type=int, default=100_000)
    data.add_argument("--train-tokens", type=int, default=150_000_000)
    data.add_argument("--eval-tokens", type=int, default=5_000_000)
    data.add_argument("--result", type=Path)

    preflight = commands.add_parser("paid-preflight")
    preflight.add_argument("--output", type=Path, required=True)
    preflight.add_argument("--wandb-url", required=True)

    compile_value = commands.add_parser("compile-probe")
    compile_value.add_argument("--variant", required=True)
    compile_value.add_argument("--batch-contexts", type=int, required=True)

    campaign = commands.add_parser("run-campaign")
    campaign.add_argument("--data-root", type=Path, required=True)
    campaign.add_argument("--output-root", type=Path, required=True)
    campaign.add_argument("--result", type=Path, required=True)
    campaign.add_argument("--preflight", type=Path)

    synthetic = commands.add_parser("synthetic-gate")
    synthetic.add_argument("--output-root", type=Path, required=True)
    synthetic.add_argument("--result", type=Path, required=True)

    inspect = commands.add_parser("audit")
    inspect.add_argument("--data-root", type=Path)
    inspect.add_argument("--output-root", type=Path, required=True)
    inspect.add_argument("--result", type=Path)

    commands.add_parser("delete-service")
    commands.add_parser("delete-service-now")
    provision_value = commands.add_parser("provision-h100")
    provision_value.add_argument("--output", type=Path, required=True)
    provision_value.add_argument("--timeout-seconds", type=int, default=900)
    return value


def main() -> None:
    args = parser().parse_args()
    if args.command == "inventory":
        result = inventory()
    elif args.command == "local-audit":
        result = local_audit(args.output)
    elif args.command == "prepare-tinystories":
        manifest = prepare_tinystories(
            args.output_root,
            tokenizer_documents=args.tokenizer_documents,
            train_tokens=args.train_tokens,
            eval_tokens=args.eval_tokens,
        )
        result = {
            "schema": "expv2-1-data-stage-v1",
            "status": "complete",
            "data_root": str(args.output_root),
            "manifest": manifest,
        }
        if args.result:
            atomic_json(args.result, result)
    elif args.command == "paid-preflight":
        result = paid_preflight(args.output, wandb_url=args.wandb_url)
    elif args.command == "compile-probe":
        result = compilation_probe(args.variant, args.batch_contexts)
    elif args.command == "run-campaign":
        result = run_campaign(
            data_root=args.data_root,
            output_root=args.output_root,
            result_path=args.result,
            preflight_path=args.preflight,
        )
    elif args.command == "synthetic-gate":
        result = run_synthetic_only(
            output_root=args.output_root, result_path=args.result
        )
    elif args.command == "audit":
        rows = {}
        for name in ("paid-preflight", "synthetic-gate", "tinystories-gate"):
            path = args.output_root / f"{name}.json"
            if path.is_file():
                rows[name] = json.loads(path.read_text())
        result_path = args.result or args.output_root / "campaign-result.json"
        if result_path.is_file():
            rows["campaign"] = json.loads(result_path.read_text())
        data = validate_manifest(args.data_root) if args.data_root else None
        result = {
            "schema": "expv2-1-audit-v1",
            "status": (
                "complete"
                if rows.get("campaign", {}).get("status") == "complete"
                else "incomplete"
            ),
            "data": data,
            "artifacts": rows,
        }
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
