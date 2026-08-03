"""Exact-target delayed deletion for disposable Northflank pilot services."""

from __future__ import annotations

import os
import subprocess
import sys
import time


def _target() -> tuple[str, str]:
    project = os.environ.get("RC_NF_PROJECT")
    service = os.environ.get("RC_NF_SERVICE")
    if not project or not service:
        raise RuntimeError("RC_NF_PROJECT and RC_NF_SERVICE are required")
    if project != "fda-test" or service != "expv2-1-h100":
        raise RuntimeError(
            f"refusing to delete non-ExpV2-1 target: {project}/{service}"
        )
    return project, service


def schedule_delete() -> None:
    _target()
    subprocess.Popen(
        [sys.executable, "-m", "expv2.exp1", "delete-service-now"],
        stdin=subprocess.DEVNULL,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
        start_new_session=True,
        env=dict(os.environ),
    )


def delete_now(delay_seconds: int = 5) -> None:
    project, service = _target()
    time.sleep(delay_seconds)
    subprocess.run(
        [
            "northflank",
            "delete",
            "service",
            "--projectId",
            project,
            "--serviceId",
            service,
            "--force",
        ],
        check=True,
    )
