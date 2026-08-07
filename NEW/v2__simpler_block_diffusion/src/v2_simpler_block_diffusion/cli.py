from __future__ import annotations

import argparse
import json

from .config import SimplerBlockDiffusionConfig
from .data import load_and_validate_manifest, manifest_sha256


def main() -> None:
    parser = argparse.ArgumentParser(description="V2 simpler block-diffusion utilities")
    subparsers = parser.add_subparsers(dest="command", required=True)
    subparsers.add_parser("parameter-count")
    manifest = subparsers.add_parser("validate-manifest")
    manifest.add_argument("path")
    args = parser.parse_args()
    if args.command == "parameter-count":
        config = SimplerBlockDiffusionConfig()
        print(json.dumps({"architecture_id": config.architecture_id, "parameters": config.expected_parameter_count}))
    else:
        value = load_and_validate_manifest(args.path)
        print(
            json.dumps(
                {
                    "schema": value["schema"],
                    "sources": len(value["sources"]),
                    "sha256": manifest_sha256(args.path),
                }
            )
        )


if __name__ == "__main__":
    main()

