from __future__ import annotations

import json
import sys
from pathlib import Path

import pytest

from research_control.controller import (
    ControllerError,
    audit,
    doctor,
    load_manifest,
    lock_plan,
    run_stage,
    validate_manifest,
)


def base_manifest(stages: list[dict] | None = None) -> dict:
    return {
        "schema": "research-control-v1",
        "experiment_id": "test-experiment",
        "hypothesis": "A falsifiable hypothesis.",
        "policy": {
            "pilot_max_dollars": 25.0,
            "pilot_max_gpu_hours": 1.0,
            "scale_tokens_per_parameter": 20.0,
        },
        "stages": stages
        or [{"id": "local", "tier": "local", "requires": [], "command": ["true"]}],
    }


def test_real_experiment_manifest_is_valid() -> None:
    manifest = load_manifest(Path(__file__).parents[1] / "exp11_kronecker_debug" / "experiment.json")
    assert manifest["experiment_id"] == "exp11-kronecker-debug-gpu1-v5"


@pytest.mark.parametrize(
    ("field", "value"),
    (("pilot_max_dollars", 25.01), ("pilot_max_gpu_hours", 1.01), ("scale_tokens_per_parameter", 19.9)),
)
def test_hard_policy_cannot_be_relaxed(field: str, value: float) -> None:
    manifest = base_manifest()
    manifest["policy"][field] = value
    with pytest.raises(ControllerError):
        validate_manifest(manifest)


def test_pilot_requires_one_gpu_and_budget() -> None:
    paid = {
        "id": "pilot",
        "tier": "pilot",
        "requires": [],
        "command": ["true"],
        "gpu_count": 2,
        "price_per_gpu_hour": 3.0,
        "max_wall_seconds": 3600,
        "result_path": "result.json",
        "pause_command": ["true"],
    }
    with pytest.raises(ControllerError, match="exactly one GPU"):
        validate_manifest(base_manifest([paid]))


def test_confirmation_requires_full_node_and_locked_budget() -> None:
    stage = {
        "id": "confirmation",
        "tier": "confirmation",
        "requires": [],
        "command": ["true"],
        "gpu_count": 8,
        "price_per_gpu_hour": 2.74,
        "max_wall_seconds": 1800,
        "result_path": "result.json",
        "pause_command": ["true"],
    }
    manifest = base_manifest([stage])
    manifest["policy"].update(
        confirmation_max_dollars=15.0,
        confirmation_max_gpu_hours=4.0,
    )
    validate_manifest(manifest)
    manifest["stages"][0]["gpu_count"] = 4
    with pytest.raises(ControllerError, match="exactly eight GPUs"):
        validate_manifest(manifest)


def test_confirmation_policy_cannot_be_relaxed() -> None:
    stage = {
        "id": "confirmation",
        "tier": "confirmation",
        "requires": [],
        "command": ["true"],
        "gpu_count": 8,
        "price_per_gpu_hour": 2.74,
        "max_wall_seconds": 1800,
        "result_path": "result.json",
        "pause_command": ["true"],
    }
    manifest = base_manifest([stage])
    manifest["policy"].update(
        confirmation_max_dollars=15.01,
        confirmation_max_gpu_hours=4.0,
    )
    with pytest.raises(ControllerError, match=r"\$15 ceiling"):
        validate_manifest(manifest)


def test_scale_requires_twenty_tokens_per_parameter() -> None:
    scale = {
        "id": "scale",
        "tier": "scale",
        "requires": [],
        "command": ["true"],
        "gpu_count": 8,
        "price_per_gpu_hour": 3.0,
        "max_wall_seconds": 3600,
        "result_path": "result.json",
        "pause_command": ["true"],
        "parameters": 1_000_000,
        "training_tokens": 19_999_999,
        "promotion_checks": [
            {"path": "pilot.json", "field": "verdict", "equals": "promote"}
        ],
    }
    with pytest.raises(ControllerError, match="20 tokens/parameter"):
        validate_manifest(base_manifest([scale]))


def test_doctor_requires_gates_artifacts_wandb_and_exact_target(tmp_path) -> None:
    artifact = tmp_path / "data.bin"
    artifact.write_bytes(b"good")
    stage = {
        "id": "pilot",
        "tier": "pilot",
        "requires": [],
        "local_checks": ["math"],
        "data_artifacts": [{"path": str(artifact), "sha256": "bad"}],
        "command": ["true"],
        "gpu_count": 1,
        "price_per_gpu_hour": 1.0,
        "max_wall_seconds": 10,
        "result_path": "result.json",
        "pause_command": ["true"],
    }
    report = doctor(base_manifest([stage]), "pilot", state_root=tmp_path, environment={})
    failures = " ".join(report["failures"])
    assert report["status"] == "fail"
    assert "local check math" in failures
    assert "checksum" in failures
    assert "WANDB_API_KEY" in failures
    assert "Northflank target" in failures


def _write_script(path: Path, body: str) -> None:
    path.write_text(body)


def test_paid_run_writes_ledger_and_always_pauses(tmp_path) -> None:
    worker = tmp_path / "worker.py"
    pause = tmp_path / "pause.py"
    _write_script(
        worker,
        "import json,sys\nfrom pathlib import Path\nPath(sys.argv[1]).parent.mkdir(parents=True,exist_ok=True)\nPath(sys.argv[1]).write_text(json.dumps({'status':'complete','wandb_url':'https://wandb.ai/e/p/r'}))\n",
    )
    _write_script(pause, "import sys\nfrom pathlib import Path\nPath(sys.argv[1]).write_text('paused')\n")
    stage = {
        "id": "pilot",
        "tier": "pilot",
        "requires": [],
        "command": [sys.executable, str(worker), "{state_root}/result.json"],
        "gpu_count": 1,
        "price_per_gpu_hour": 1.0,
        "max_wall_seconds": 10,
        "result_path": "{state_root}/result.json",
        "pause_command": [sys.executable, str(pause), "{state_root}/paused"],
    }
    environment = {"WANDB_API_KEY": "x", "RC_NF_PROJECT": "p", "RC_NF_SERVICE": "s"}
    result = run_stage(
        base_manifest([stage]),
        "pilot",
        state_root=tmp_path / "state",
        manifest_dir=tmp_path,
        environment=environment,
        poll_seconds=0.01,
    )
    assert result["status"] == "complete"
    assert (tmp_path / "state" / "paused").read_text() == "paused"
    ledger = [json.loads(row) for row in (tmp_path / "state" / "ledger.jsonl").read_text().splitlines()]
    assert [row["event"] for row in ledger] == ["stage_started", "stage_finished"]


def test_paid_timeout_still_pauses_exact_target(tmp_path) -> None:
    worker = tmp_path / "slow.py"
    pause = tmp_path / "pause.py"
    _write_script(worker, "import time\ntime.sleep(5)\n")
    _write_script(pause, "import sys\nfrom pathlib import Path\nPath(sys.argv[1]).write_text('paused')\n")
    stage = {
        "id": "pilot",
        "tier": "pilot",
        "requires": [],
        "command": [sys.executable, str(worker)],
        "gpu_count": 1,
        "price_per_gpu_hour": 1.0,
        "max_wall_seconds": 0.05,
        "result_path": "{state_root}/result.json",
        "pause_command": [sys.executable, str(pause), "{state_root}/paused"],
    }
    environment = {"WANDB_API_KEY": "x", "RC_NF_PROJECT": "p", "RC_NF_SERVICE": "s"}
    with pytest.raises(ControllerError, match="budget exceeded"):
        run_stage(
            base_manifest([stage]),
            "pilot",
            state_root=tmp_path / "state",
            manifest_dir=tmp_path,
            environment=environment,
            poll_seconds=0.01,
        )
    assert (tmp_path / "state" / "paused").read_text() == "paused"
    ledger = [json.loads(row) for row in (tmp_path / "state" / "ledger.jsonl").read_text().splitlines()]
    assert ledger[-1]["status"] == "failed"


def test_plan_lock_is_immutable_and_empty_audit_is_incomplete(tmp_path) -> None:
    manifest = base_manifest()
    lock_plan(manifest, tmp_path)
    changed = base_manifest()
    changed["hypothesis"] = "Changed after lock."
    with pytest.raises(ControllerError, match="differs"):
        lock_plan(changed, tmp_path)
    report = audit(manifest, state_root=tmp_path)
    assert report["status"] == "incomplete"
    assert report["pending_stages"] == ["local"]
    assert report["wandb_urls"] == {}
