from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import Any

from exp11_kronecker_debug.lm import write_json

from .campaign import GPU_COUNT, SCHEMA, run_campaign
from .gates import local_audit
from .holdout import prepare_holdout, validate_holdout
from .model import MODEL_NAMES, build_model, model_inventory
from .study import CONFIRMATION_SEEDS


VALID_CAMPAIGN_SCHEMAS = {
    "exp13-wikitext-confirmation-v1",
    "exp13-wikitext-confirmation-v2",
}


def campaign_audit(path: str | Path) -> dict[str, Any]:
    source = Path(path)
    value = json.loads(source.read_text())
    failures: list[str] = []
    if (
        value.get("schema") not in VALID_CAMPAIGN_SCHEMAS
        or value.get("status") != "complete"
    ):
        failures.append("campaign result is not complete Exp13 output")
    if not value.get("wandb_url"):
        failures.append("campaign lacks a direct W&B URL")
    preflight = value.get("preflight", {})
    if preflight.get("status") != "pass" or preflight.get("gpu_count") != GPU_COUNT:
        failures.append("eight-GPU preflight did not pass")
    if preflight.get("gradient_accumulation") != 1:
        failures.append("preflight used gradient accumulation")
    try:
        if int(preflight["common_batch"]) <= 0:
            failures.append("global context batch is not positive")
    except (KeyError, TypeError, ValueError):
        failures.append("global token batch is missing")
    if value.get("schema") == "exp13-wikitext-confirmation-v2":
        agreements = preflight.get("loss_agreement", {})
        if set(agreements) != set(MODEL_NAMES) or any(
            row.get("status") != "complete"
            or float(row.get("relative_error", float("inf")))
            > float(row.get("maximum_relative_error", 0.0))
            for row in agreements.values()
        ):
            failures.append("v2 BF16/FP32 loss agreement is incomplete or failed")
        if preflight.get("execution_mode") != "eager":
            failures.append("v2 did not truthfully record eager execution")
        if float(preflight.get("minimum_gpu_utilization_percent", 0.0)) < 85.0:
            failures.append("v2 utilization gate is below 85%")
        if float(preflight.get("eight_way_scaling_efficiency", 0.0)) < float(
            preflight.get("minimum_eight_way_scaling_efficiency", 1.0)
        ):
            failures.append("v2 eight-way scaling gate failed")
    decision = value.get("confirmation_decision", {})
    if decision.get("status") == "pass":
        final_rows = value.get("final_evaluations", [])
        if len(final_rows) != 12 or not value.get("final_holdout_opened"):
            failures.append("promoted campaign lacks the twelve sealed final evaluations")
    elif value.get("final_holdout_opened"):
        failures.append("failed confirmation opened the final holdout")
    seeds = set(CONFIRMATION_SEEDS)
    evaluated = value.get("confirmation_evaluations", [])
    by_model: dict[str, set[int]] = {}
    for row in evaluated:
        by_model.setdefault(str(row.get("model")), set()).add(int(row.get("seed", -1)))
    if len(by_model) != 2 or any(items != seeds for items in by_model.values()):
        failures.append("confirmation is not a complete paired four-seed comparison")
    return {
        "schema": "exp13-campaign-audit-v1",
        "status": "pass" if not failures else "fail",
        "campaign_result": str(source),
        "wandb_url": value.get("wandb_url"),
        "verdict": value.get("verdict"),
        "failures": failures,
    }


def main() -> None:
    parser = argparse.ArgumentParser(description="Experiment 13 confirmation study")
    commands = parser.add_subparsers(dest="command", required=True)

    inventory = commands.add_parser("inventory")
    inventory.add_argument("--output")

    local = commands.add_parser("local-audit")
    local.add_argument("--output", required=True)

    holdout = commands.add_parser("prepare-holdout")
    holdout.add_argument("--data-root", required=True)
    holdout.add_argument("--output-root", required=True)
    holdout.add_argument("--result", required=True)

    campaign = commands.add_parser("run-campaign")
    campaign.add_argument("--data-root", required=True)
    campaign.add_argument("--holdout-root", required=True)
    campaign.add_argument("--output", required=True)
    campaign.add_argument("--heartbeat")

    audit = commands.add_parser("audit")
    audit.add_argument("--campaign-result", required=True)
    audit.add_argument("--output")

    args = parser.parse_args()
    if args.command == "inventory":
        result: dict[str, Any] = {
            "schema": "exp13-model-inventory-v1",
            "status": "pass",
            "models": {
                name: model_inventory(build_model(name)) for name in MODEL_NAMES
            },
        }
        if args.output:
            write_json(Path(args.output), result)
    elif args.command == "local-audit":
        result = local_audit(args.output)
    elif args.command == "prepare-holdout":
        manifest = prepare_holdout(args.data_root, args.output_root)
        validate_holdout(Path(args.output_root))
        result = {
            "schema": "exp13-holdout-gate-v1",
            "status": "pass",
            "holdout_manifest": manifest,
            "final_holdout_opened": False,
        }
        write_json(Path(args.result), result)
    elif args.command == "run-campaign":
        result = run_campaign(
            args.output,
            data_root=args.data_root,
            holdout_root=args.holdout_root,
            heartbeat=args.heartbeat,
        )
    else:
        result = campaign_audit(args.campaign_result)
        if args.output:
            write_json(Path(args.output), result)
        if result["status"] != "pass":
            raise RuntimeError(f"Exp13 campaign audit failed: {result['failures']}")
    print(json.dumps(result, indent=2, sort_keys=True))


if __name__ == "__main__":
    main()
