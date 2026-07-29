from __future__ import annotations

import argparse
import json

from .config import DEFAULT_DATA_ROOT, DEFAULT_OUTPUT_ROOT, study_plan
from .coordinator import audit, launch_study, run_preflight, status
from .diagnostics import measure_product_floor
from .train import run_preflight_worker, run_stage


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "command",
        choices=(
            "plan",
            "preflight",
            "preflight-worker",
            "run-stage",
            "launch",
            "status",
            "audit",
            "product-floor",
        ),
    )
    parser.add_argument("--data-root", default=DEFAULT_DATA_ROOT)
    parser.add_argument("--output-root", default=DEFAULT_OUTPUT_ROOT)
    parser.add_argument("--spec")
    parser.add_argument("--result-file")
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    if args.command == "plan":
        print(json.dumps(study_plan(), indent=2, sort_keys=True))
    elif args.command == "preflight":
        result = run_preflight(
            data_root=args.data_root,
            output_root=args.output_root,
        )
        print(json.dumps(result, indent=2, sort_keys=True))
    elif args.command == "preflight-worker":
        if not args.result_file:
            raise ValueError("preflight-worker requires --result-file")
        run_preflight_worker(
            args.result_file,
            data_root=args.data_root,
        )
    elif args.command == "run-stage":
        if not args.spec:
            raise ValueError("run-stage requires --spec")
        run_stage(
            args.spec,
            data_root=args.data_root,
            output_root=args.output_root,
        )
    elif args.command == "launch":
        result = launch_study(
            data_root=args.data_root,
            output_root=args.output_root,
        )
        print(json.dumps(result, indent=2, sort_keys=True))
    elif args.command == "status":
        status(args.output_root)
    elif args.command == "product-floor":
        measure_product_floor(data_root=args.data_root)
    else:
        audit(args.output_root)


main()
