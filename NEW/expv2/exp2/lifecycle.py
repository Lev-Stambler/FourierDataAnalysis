"""Exact-target teardown for the ExpV2-2 disposable service."""

from __future__ import annotations

import os
import subprocess
import sys
import time

from .infra import PROJECT_ID, SERVICE_ID, delete_service


def _target() -> tuple[str, str]:
    project = os.environ.get("RC_NF_PROJECT")
    service = os.environ.get("RC_NF_SERVICE")
    if project != PROJECT_ID or service != SERVICE_ID:
        raise RuntimeError(f"refusing non-ExpV2-2 target: {project}/{service}")
    return project, service


def schedule_delete() -> None:
    _target()
    subprocess.Popen(
        [sys.executable, "-m", "expv2.exp2", "delete-service-now"],
        stdin=subprocess.DEVNULL,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
        start_new_session=True,
        env=dict(os.environ),
    )


def delete_now(delay_seconds: int = 0) -> None:
    _target()
    if delay_seconds:
        time.sleep(delay_seconds)
    delete_service()
