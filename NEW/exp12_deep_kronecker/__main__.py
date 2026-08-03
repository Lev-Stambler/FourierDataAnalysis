from __future__ import annotations

import argparse
import json

from .gates import local_correctness
from .model import VARIANTS, build_model, model_inventory


def main() -> None:
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(dest="command", required=True)
    correctness = subparsers.add_parser("local-correctness")
    correctness.add_argument("--output")
    subparsers.add_parser("inventory")
    campaign = subparsers.add_parser("wikitext-campaign")
    campaign.add_argument("--output", required=True)
    campaign.add_argument("--data-root", required=True)
    campaign.add_argument("--heartbeat")
    campaign.add_argument("--wall-limit-seconds", type=float, default=3000.0)
    args = parser.parse_args()
    if args.command == "local-correctness":
        result = local_correctness(args.output)
    elif args.command == "wikitext-campaign":
        from .campaign import run_campaign

        result = run_campaign(
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
