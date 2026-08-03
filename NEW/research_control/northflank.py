"""Exact-target Northflank lifecycle hook used by the controller."""

from __future__ import annotations

import argparse
import os
import subprocess
import sys
import time


def pause_now() -> None:
    project = os.environ.get("RC_NF_PROJECT")
    service = os.environ.get("RC_NF_SERVICE")
    if not project or not service:
        raise RuntimeError("RC_NF_PROJECT and RC_NF_SERVICE are required")
    subprocess.run(
        [
            "northflank",
            "pause",
            "service",
            "--projectId",
            project,
            "--serviceId",
            service,
        ],
        check=True,
    )


def schedule_pause() -> None:
    """Return promptly, then pause after result/ledger files are committed."""
    subprocess.Popen(
        [sys.executable, "-m", "research_control.northflank", "pause-now"],
        stdin=subprocess.DEVNULL,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
        start_new_session=True,
    )


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("command", choices=("pause", "pause-now"))
    args = parser.parse_args()
    if args.command == "pause":
        schedule_pause()
    else:
        time.sleep(120)
        pause_now()


if __name__ == "__main__":
    main()
