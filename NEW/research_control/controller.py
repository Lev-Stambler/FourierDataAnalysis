"""Budget, evidence, logging, and lifecycle gates for research workloads."""

from __future__ import annotations

import hashlib
import json
import os
import subprocess
import time
from pathlib import Path
from typing import Any, Mapping, Sequence


SCHEMA = "research-control-v1"
TERMINATION_GRACE_SECONDS = 10.0


class ControllerError(RuntimeError):
    pass


def canonical_json(value: Any) -> str:
    return json.dumps(value, sort_keys=True, separators=(",", ":"))


def write_json_atomic(path: Path, value: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(path.name + ".tmp")
    temporary.write_text(json.dumps(value, indent=2, sort_keys=True))
    temporary.replace(path)


def manifest_digest(manifest: Mapping[str, Any]) -> str:
    return hashlib.sha256(canonical_json(manifest).encode()).hexdigest()


def load_manifest(path: str | Path) -> dict[str, Any]:
    value = json.loads(Path(path).read_text())
    validate_manifest(value)
    return value


def _require(value: Mapping[str, Any], key: str, expected: type) -> Any:
    result = value.get(key)
    if not isinstance(result, expected):
        raise ControllerError(f"{key} must be {expected.__name__}")
    return result


def _validate_artifacts(value: Any, label: str) -> None:
    if not isinstance(value, list) or any(
        not isinstance(item, dict)
        or not isinstance(item.get("path"), str)
        or ("sha256" in item and not isinstance(item["sha256"], str))
        for item in value
    ):
        raise ControllerError(f"{label} artifacts are invalid")


def estimate_stage_cost(stage: Mapping[str, Any]) -> float:
    if stage["tier"] == "local":
        return 0.0
    return (
        float(stage["gpu_count"])
        * float(stage["price_per_gpu_hour"])
        * float(stage["max_wall_seconds"])
        / 3600.0
    )


def validate_manifest(manifest: Mapping[str, Any]) -> None:
    if manifest.get("schema") != SCHEMA:
        raise ControllerError(f"manifest schema must be {SCHEMA}")
    _require(manifest, "experiment_id", str)
    _require(manifest, "hypothesis", str)
    if not isinstance(manifest.get("working_directory", "."), str):
        raise ControllerError("working_directory must be a string")
    policy = _require(manifest, "policy", dict)
    if float(policy.get("pilot_max_dollars", 0)) != 25.0:
        raise ControllerError("hard policy requires a $25 pilot ceiling")
    if float(policy.get("pilot_max_gpu_hours", 0)) != 1.0:
        raise ControllerError("hard policy requires a one-GPU-hour pilot ceiling")
    if float(policy.get("scale_tokens_per_parameter", 0)) < 20.0:
        raise ControllerError("scale policy requires at least 20 tokens/parameter")
    _validate_artifacts(manifest.get("data_artifacts", []), "manifest data")
    _validate_artifacts(manifest.get("source_artifacts", []), "manifest source")
    stages = _require(manifest, "stages", list)
    if not stages:
        raise ControllerError("manifest must define stages")
    identifiers: list[str] = []
    for stage in stages:
        if not isinstance(stage, dict):
            raise ControllerError("every stage must be an object")
        identifier = _require(stage, "id", str)
        if identifier in identifiers:
            raise ControllerError(f"duplicate stage id: {identifier}")
        identifiers.append(identifier)
        tier = stage.get("tier")
        if tier not in ("local", "pilot", "confirmation", "scale"):
            raise ControllerError(f"invalid tier for {identifier}: {tier}")
        command = _require(stage, "command", list)
        if not command or not all(isinstance(item, str) and item for item in command):
            raise ControllerError(f"{identifier} command must be nonempty string argv")
        requirements = stage.get("requires", [])
        if not isinstance(requirements, list) or any(
            item not in identifiers for item in requirements
        ):
            raise ControllerError(
                f"{identifier} requirements must reference earlier stages"
            )
        checks = stage.get("local_checks", [])
        if not isinstance(checks, list) or not all(isinstance(item, str) for item in checks):
            raise ControllerError(f"{identifier} local_checks must be strings")
        artifacts = stage.get("data_artifacts", [])
        _validate_artifacts(artifacts, f"{identifier} data")
        if tier == "local":
            continue
        for key in (
            "gpu_count",
            "price_per_gpu_hour",
            "max_wall_seconds",
            "result_path",
            "pause_command",
        ):
            if key not in stage:
                raise ControllerError(f"paid stage {identifier} is missing {key}")
        if int(stage["gpu_count"]) <= 0 or float(stage["price_per_gpu_hour"]) <= 0:
            raise ControllerError(f"paid stage {identifier} has invalid GPU price/count")
        if float(stage["max_wall_seconds"]) <= 0:
            raise ControllerError(f"paid stage {identifier} has invalid wall limit")
        if (
            not isinstance(stage["pause_command"], list)
            or not stage["pause_command"]
            or not all(isinstance(item, str) and item for item in stage["pause_command"])
        ):
            raise ControllerError(f"paid stage {identifier} needs a pause command")
        if tier == "pilot":
            if int(stage["gpu_count"]) != 1:
                raise ControllerError("pilot stages require exactly one GPU")
            if estimate_stage_cost(stage) > float(policy["pilot_max_dollars"]):
                raise ControllerError(f"pilot stage {identifier} exceeds $25")
            gpu_hours = int(stage["gpu_count"]) * float(stage["max_wall_seconds"]) / 3600
            if gpu_hours > float(policy["pilot_max_gpu_hours"]):
                raise ControllerError(f"pilot stage {identifier} exceeds one GPU-hour")
        if tier == "confirmation":
            if float(policy.get("confirmation_max_dollars", 0)) != 15.0:
                raise ControllerError(
                    "hard confirmation policy requires a $15 ceiling"
                )
            if float(policy.get("confirmation_max_gpu_hours", 0)) != 4.0:
                raise ControllerError(
                    "hard confirmation policy requires a four-GPU-hour ceiling"
                )
            if int(stage["gpu_count"]) != 8:
                raise ControllerError("confirmation stages require exactly eight GPUs")
            if estimate_stage_cost(stage) > float(
                policy["confirmation_max_dollars"]
            ):
                raise ControllerError(
                    f"confirmation stage {identifier} exceeds $15"
                )
            gpu_hours = (
                int(stage["gpu_count"])
                * float(stage["max_wall_seconds"])
                / 3600
            )
            if gpu_hours > float(policy["confirmation_max_gpu_hours"]):
                raise ControllerError(
                    f"confirmation stage {identifier} exceeds four GPU-hours"
                )
        if tier == "scale":
            parameters = int(stage.get("parameters", 0))
            tokens = int(stage.get("training_tokens", 0))
            if parameters <= 0 or tokens / parameters < float(
                policy["scale_tokens_per_parameter"]
            ):
                raise ControllerError(
                    f"scale stage {identifier} has fewer than 20 tokens/parameter"
                )
            promotions = stage.get("promotion_checks", [])
            if not isinstance(promotions, list) or not promotions or any(
                not isinstance(item, dict)
                or not isinstance(item.get("path"), str)
                or not isinstance(item.get("field"), str)
                or "equals" not in item
                for item in promotions
            ):
                raise ControllerError(
                    f"scale stage {identifier} needs a locked smaller-run promotion check"
                )


def stage_by_id(manifest: Mapping[str, Any], identifier: str) -> dict[str, Any]:
    for stage in manifest["stages"]:
        if stage["id"] == identifier:
            return stage
    raise ControllerError(f"unknown stage: {identifier}")


def resolve_path(value: str, *, state_root: Path, manifest_dir: Path) -> Path:
    expanded = value.format(state_root=state_root, manifest_dir=manifest_dir)
    path = Path(expanded)
    return path if path.is_absolute() else manifest_dir / path


def file_sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(8 << 20), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _result_complete(path: Path, *, require_wandb: bool) -> tuple[bool, str]:
    if not path.is_file():
        return False, f"missing result: {path}"
    try:
        result = json.loads(path.read_text())
    except (OSError, json.JSONDecodeError) as error:
        return False, f"invalid result {path}: {error}"
    if result.get("status") not in ("complete", "pass"):
        return False, f"result is not complete: {path}"
    if require_wandb:
        direct = result.get("wandb_url")
        collection = result.get("wandb_urls")
        if not direct and not (isinstance(collection, list) and collection):
            return False, f"paid result lacks direct W&B URL: {path}"
    return True, "pass"


def lock_plan(manifest: Mapping[str, Any], state_root: Path) -> Path:
    state_root.mkdir(parents=True, exist_ok=True)
    path = state_root / "plan.json"
    value = {
        "schema": "research-control-locked-plan-v1",
        "manifest_sha256": manifest_digest(manifest),
        "manifest": manifest,
    }
    if path.is_file():
        existing = json.loads(path.read_text())
        if existing != value:
            raise ControllerError("locked experiment plan differs from manifest")
    else:
        write_json_atomic(path, value)
    return path


def doctor(
    manifest: Mapping[str, Any],
    identifier: str,
    *,
    state_root: str | Path,
    manifest_dir: str | Path = ".",
    environment: Mapping[str, str] | None = None,
) -> dict[str, Any]:
    validate_manifest(manifest)
    stage = stage_by_id(manifest, identifier)
    root, source = Path(state_root), Path(manifest_dir)
    env = os.environ if environment is None else environment
    failures: list[str] = []
    for requirement in stage.get("requires", []):
        required = stage_by_id(manifest, requirement)
        result_value = required.get(
            "result_path", f"{{state_root}}/stages/{requirement}/result.json"
        )
        complete, reason = _result_complete(
            resolve_path(result_value, state_root=root, manifest_dir=source),
            require_wandb=required["tier"] != "local",
        )
        if not complete:
            failures.append(f"requirement {requirement}: {reason}")
    for check in stage.get("local_checks", []):
        complete, reason = _result_complete(
            root / "gates" / f"{check}.json", require_wandb=False
        )
        if not complete:
            failures.append(f"local check {check}: {reason}")
    for artifact in [
        *manifest.get("data_artifacts", []),
        *manifest.get("source_artifacts", []),
        *stage.get("data_artifacts", []),
    ]:
        path = resolve_path(
            artifact["path"], state_root=root, manifest_dir=source
        )
        if not path.is_file():
            failures.append(f"missing data artifact: {path}")
        elif artifact.get("sha256") and file_sha256(path) != artifact["sha256"]:
            failures.append(f"data checksum mismatch: {path}")
    for check in stage.get("promotion_checks", []):
        path = resolve_path(check["path"], state_root=root, manifest_dir=source)
        if not path.is_file():
            failures.append(f"missing promotion result: {path}")
            continue
        try:
            value: Any = json.loads(path.read_text())
            for part in check["field"].split("."):
                value = value[part]
        except (OSError, json.JSONDecodeError, KeyError, TypeError) as error:
            failures.append(f"invalid promotion result {path}: {error}")
            continue
        if value != check["equals"]:
            failures.append(
                f"promotion check failed: {check['field']} is {value!r}, "
                f"expected {check['equals']!r}"
            )
    if stage["tier"] != "local":
        if not env.get("WANDB_API_KEY"):
            failures.append("WANDB_API_KEY is missing")
        if not env.get("RC_NF_PROJECT") or not env.get("RC_NF_SERVICE"):
            failures.append("controller-owned Northflank target is missing")
    report = {
        "schema": "research-control-doctor-v1",
        "experiment_id": manifest["experiment_id"],
        "stage": identifier,
        "status": "pass" if not failures else "fail",
        "projected_dollars": estimate_stage_cost(stage),
        "failures": failures,
    }
    return report


def append_ledger(state_root: Path, value: Mapping[str, Any]) -> None:
    state_root.mkdir(parents=True, exist_ok=True)
    with (state_root / "ledger.jsonl").open("a") as handle:
        handle.write(json.dumps(dict(value), sort_keys=True) + "\n")


def spent_dollars(state_root: Path) -> float:
    path = state_root / "ledger.jsonl"
    if not path.is_file():
        return 0.0
    total = 0.0
    for line in path.read_text().splitlines():
        value = json.loads(line)
        if value.get("event") in ("stage_finished", "external_charge"):
            total += float(value.get("actual_dollars", 0.0))
    return total


def spent_gpu_seconds(state_root: Path) -> float:
    path = state_root / "ledger.jsonl"
    if not path.is_file():
        return 0.0
    return sum(
        float(value.get("gpu_seconds", 0.0))
        for value in map(json.loads, path.read_text().splitlines())
        if value.get("event") in ("stage_finished", "external_charge")
    )


def _format_argv(
    values: Sequence[str], *, state_root: Path, manifest_dir: Path
) -> list[str]:
    return [
        item.format(state_root=state_root, manifest_dir=manifest_dir)
        for item in values
    ]


def _stop_process(process: subprocess.Popen) -> None:
    process.terminate()
    try:
        process.wait(timeout=TERMINATION_GRACE_SECONDS)
    except subprocess.TimeoutExpired:
        process.kill()
        process.wait()


def run_stage(
    manifest: Mapping[str, Any],
    identifier: str,
    *,
    state_root: str | Path,
    manifest_dir: str | Path = ".",
    environment: Mapping[str, str] | None = None,
    popen: type[subprocess.Popen] = subprocess.Popen,
    run_command: Any = subprocess.run,
    poll_seconds: float = 1.0,
) -> dict[str, Any]:
    root, source = Path(state_root), Path(manifest_dir)
    lock_plan(manifest, root)
    report = doctor(
        manifest,
        identifier,
        state_root=root,
        manifest_dir=source,
        environment=environment,
    )
    if report["status"] != "pass":
        raise ControllerError("doctor failed: " + "; ".join(report["failures"]))
    stage = stage_by_id(manifest, identifier)
    env = dict(os.environ if environment is None else environment)
    paid = stage["tier"] != "local"
    maximum_seconds = float(stage.get("max_wall_seconds", 2**31))
    if paid and stage["tier"] in ("pilot", "confirmation"):
        prefix = stage["tier"]
        remaining = float(
            manifest["policy"][f"{prefix}_max_dollars"]
        ) - spent_dollars(root)
        if remaining <= 0:
            raise ControllerError(f"{prefix} dollar budget is exhausted")
        affordable = (
            remaining
            / (int(stage["gpu_count"]) * float(stage["price_per_gpu_hour"]))
            * 3600
        )
        remaining_gpu_seconds = (
            float(manifest["policy"][f"{prefix}_max_gpu_hours"]) * 3600
            - spent_gpu_seconds(root)
        )
        if remaining_gpu_seconds <= 0:
            raise ControllerError(f"{prefix} GPU-hour budget is exhausted")
        maximum_seconds = min(
            maximum_seconds,
            affordable,
            remaining_gpu_seconds / int(stage["gpu_count"]),
        )
    command = _format_argv(stage["command"], state_root=root, manifest_dir=source)
    pause = _format_argv(
        stage.get("pause_command", []), state_root=root, manifest_dir=source
    )
    started_wall, started_monotonic = time.time(), time.monotonic()
    append_ledger(
        root,
        {
            "event": "stage_started",
            "stage": identifier,
            "time": started_wall,
            "projected_dollars": estimate_stage_cost(stage),
            "command": command,
        },
    )
    process = None
    failure = None
    try:
        process = popen(command, cwd=source, env=env)
        heartbeat_value = stage.get("heartbeat_path")
        heartbeat_timeout = float(stage.get("heartbeat_timeout_seconds", 0))
        while process.poll() is None:
            elapsed = time.monotonic() - started_monotonic
            if elapsed > maximum_seconds:
                failure = "wall-clock or dollar budget exceeded"
                _stop_process(process)
                break
            if heartbeat_value and heartbeat_timeout:
                heartbeat = resolve_path(
                    heartbeat_value, state_root=root, manifest_dir=source
                )
                if (
                    elapsed > heartbeat_timeout
                    and (
                        not heartbeat.exists()
                        or time.time() - heartbeat.stat().st_mtime > heartbeat_timeout
                    )
                ):
                    failure = "heartbeat missing or stale"
                    _stop_process(process)
                    break
            time.sleep(poll_seconds)
        returncode = process.wait()
        if returncode and not failure:
            failure = f"command exited {returncode}"
    except Exception as error:
        failure = f"launch or monitor error: {error}"
        if process is not None and process.poll() is None:
            _stop_process(process)
    finally:
        if paid and pause:
            try:
                paused = run_command(pause, cwd=source, env=env, check=False)
                if getattr(paused, "returncode", 0):
                    reason = f"pause command exited {paused.returncode}"
                    failure = f"{failure}; {reason}" if failure else reason
            except Exception as error:
                reason = f"pause command failed: {error}"
                failure = f"{failure}; {reason}" if failure else reason
    elapsed = time.monotonic() - started_monotonic
    dollars = (
        elapsed
        * int(stage.get("gpu_count", 0))
        * float(stage.get("price_per_gpu_hour", 0))
        / 3600
    )
    result_path = resolve_path(
        stage.get("result_path", f"{{state_root}}/stages/{identifier}/result.json"),
        state_root=root,
        manifest_dir=source,
    )
    complete, reason = _result_complete(result_path, require_wandb=paid)
    if not complete and not failure:
        failure = reason
    outcome = {
        "event": "stage_finished",
        "stage": identifier,
        "time": time.time(),
        "elapsed_seconds": elapsed,
        "gpu_seconds": elapsed * int(stage.get("gpu_count", 0)),
        "actual_dollars": dollars,
        "status": "complete" if not failure else "failed",
        "failure": failure,
        "result_path": str(result_path),
    }
    append_ledger(root, outcome)
    if failure:
        raise ControllerError(f"stage {identifier} failed: {failure}")
    return outcome


def audit(manifest: Mapping[str, Any], *, state_root: str | Path) -> dict[str, Any]:
    validate_manifest(manifest)
    root = Path(state_root)
    rows = []
    if (root / "ledger.jsonl").is_file():
        rows = [json.loads(line) for line in (root / "ledger.jsonl").read_text().splitlines()]
    finished = {
        row["stage"]: row for row in rows if row.get("event") == "stage_finished"
    }
    failures = [
        stage["id"]
        for stage in manifest["stages"]
        if stage["id"] in finished and finished[stage["id"]]["status"] != "complete"
    ]
    completed = [
        stage["id"]
        for stage in manifest["stages"]
        if finished.get(stage["id"], {}).get("status") == "complete"
    ]
    pending = [
        stage["id"]
        for stage in manifest["stages"]
        if stage["id"] not in completed and stage["id"] not in failures
    ]
    wandb_urls: dict[str, list[str]] = {}
    for stage_id in finished:
        result_path = Path(finished[stage_id]["result_path"])
        if not result_path.is_file():
            continue
        try:
            stage_result = json.loads(result_path.read_text())
        except (OSError, json.JSONDecodeError):
            continue
        urls = []
        if stage_result.get("wandb_url"):
            urls.append(str(stage_result["wandb_url"]))
        urls.extend(str(item) for item in stage_result.get("wandb_urls", []))
        if urls:
            wandb_urls[stage_id] = urls
    result = {
        "schema": "research-control-audit-v1",
        "experiment_id": manifest["experiment_id"],
        "status": "failed" if failures else ("incomplete" if pending else "complete"),
        "completed_stages": completed,
        "failed_stages": failures,
        "pending_stages": pending,
        "wandb_urls": wandb_urls,
        "actual_dollars": sum(
            float(row.get("actual_dollars", 0))
            for row in rows
            if row.get("event") in ("stage_finished", "external_charge")
        ),
        "gpu_seconds": sum(
            float(row.get("gpu_seconds", 0))
            for row in rows
            if row.get("event") in ("stage_finished", "external_charge")
        ),
        "manifest_sha256": manifest_digest(manifest),
    }
    root.mkdir(parents=True, exist_ok=True)
    write_json_atomic(root / "audit.json", result)
    return result
