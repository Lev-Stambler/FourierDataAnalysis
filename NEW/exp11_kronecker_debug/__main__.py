from __future__ import annotations

import argparse
import json
import time
from pathlib import Path

from .gates import local_correctness
from .lm import run_campaign, write_json
from .model import VARIANTS, build_model, model_inventory
from .muon_tuning import muon_correctness, run_muon_campaign
from .probes import run_suite
from .tuning import run_tuning_campaign


def main() -> None:
    parser = argparse.ArgumentParser(description="Experiment 11 debug campaign")
    subparsers = parser.add_subparsers(dest="command", required=True)

    local = subparsers.add_parser("local-correctness")
    local.add_argument("--output", required=True)

    probes = subparsers.add_parser("synthetic-probes")
    probes.add_argument("--output", required=True)
    probes.add_argument("--steps", type=int, default=600)

    campaign = subparsers.add_parser("wikitext-pilot")
    campaign.add_argument("--output", required=True)
    campaign.add_argument("--data-root", default="/cache/exp10/data/wikitext")
    campaign.add_argument("--heartbeat")
    campaign.add_argument("--wall-limit-seconds", type=float, default=2700.0)

    full = subparsers.add_parser("full-pilot")
    full.add_argument("--output", required=True)
    full.add_argument("--synthetic-output", required=True)
    full.add_argument("--wikitext-output", required=True)
    full.add_argument("--data-root", default="/cache/exp10/data/wikitext")
    full.add_argument("--heartbeat")
    full.add_argument("--wall-limit-seconds", type=float, default=2800.0)

    tuning = subparsers.add_parser("wikitext-tune")
    tuning.add_argument("--output", required=True)
    tuning.add_argument("--data-root", default="/cache/exp10/data/wikitext")
    tuning.add_argument("--heartbeat")
    tuning.add_argument("--wall-limit-seconds", type=float, default=1300.0)

    muon_gate = subparsers.add_parser("muon-correctness")
    muon_gate.add_argument("--output", required=True)

    muon = subparsers.add_parser("wikitext-muon")
    muon.add_argument("--output", required=True)
    muon.add_argument("--data-root", default="/cache/exp10/data/wikitext")
    muon.add_argument("--heartbeat")
    muon.add_argument("--wall-limit-seconds", type=float, default=2400.0)

    subparsers.add_parser("inventory")
    args = parser.parse_args()
    if args.command == "local-correctness":
        result = local_correctness(args.output)
    elif args.command == "synthetic-probes":
        result = run_suite(args.output, steps=args.steps)
    elif args.command == "wikitext-pilot":
        result = run_campaign(
            args.output,
            data_root=args.data_root,
            heartbeat=args.heartbeat,
            wall_limit_seconds=args.wall_limit_seconds,
        )
    elif args.command == "full-pilot":
        started = time.monotonic()
        synthetic_path = Path(args.synthetic_output)
        if synthetic_path.is_file():
            synthetic = json.loads(synthetic_path.read_text())
            if synthetic.get("status") != "complete" or not synthetic.get("wandb_url"):
                synthetic = run_suite(synthetic_path)
        else:
            synthetic = run_suite(synthetic_path)
        running = {
            "schema": "exp11-full-pilot-v1",
            "status": "running",
            "synthetic": synthetic,
            "wandb_urls": [synthetic["wandb_url"]],
        }
        write_json(Path(args.output), running)
        if synthetic.get("verdict") != "promote":
            result = {
                **running,
                "status": "complete",
                "verdict": "debug_synthetic_mechanism",
            }
        else:
            remaining = max(
                60.0, args.wall_limit_seconds - (time.monotonic() - started) - 30.0
            )
            wiki = run_campaign(
                args.wikitext_output,
                data_root=args.data_root,
                heartbeat=args.heartbeat,
                wall_limit_seconds=remaining,
            )
            result = {
                **running,
                "status": "complete",
                "verdict": wiki["verdict"],
                "wikitext": wiki,
                "wandb_urls": [synthetic["wandb_url"], wiki["wandb_url"]],
            }
        write_json(Path(args.output), result)
    elif args.command == "wikitext-tune":
        result = run_tuning_campaign(
            args.output,
            data_root=args.data_root,
            heartbeat=args.heartbeat,
            wall_limit_seconds=args.wall_limit_seconds,
        )
    elif args.command == "muon-correctness":
        result = muon_correctness(args.output)
    elif args.command == "wikitext-muon":
        result = run_muon_campaign(
            args.output,
            data_root=args.data_root,
            heartbeat=args.heartbeat,
            wall_limit_seconds=args.wall_limit_seconds,
        )
    else:
        result = {
            variant: model_inventory(build_model(variant)) for variant in VARIANTS
        }
    print(json.dumps(result, indent=2, sort_keys=True))


if __name__ == "__main__":
    main()
