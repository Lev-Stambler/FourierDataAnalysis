from __future__ import annotations

import json
import math
import statistics
import time
from pathlib import Path

import torch

from .config import MODEL_ID, MODEL_REVISION, TrialConfig
from .study import (
    btt_parameter_architectures,
    depth_reference_trials,
    depth_screen_trials,
    evaluate_tensor_gate,
    final_trials,
    optimization_trials,
    optimized_depth_final_trials,
    kronecker_depth_trials,
    kronecker_lr_probe_trials,
    kronecker_parameter_architecture,
    kronecker_parameter_trials,
    next_kronecker_boundary_probe,
    next_tensor_boundary_probe,
    screen_trials,
    select_common_topology,
    select_depth_winners,
    select_optimized_winners,
    select_tensor_finalists,
    select_kronecker_lr_settings,
    select_tensor_lr_settings,
    select_tensor_reference,
    select_unrestricted_monarch,
    tensor_family_key,
    tensor_final_trials,
    tensor_gate_btt_trials,
    tensor_gate_control_trial,
    tensor_long_trial,
    tensor_lr_probe_trials,
    tensor_parameter_trials,
    tensor_time_trials,
    tuning_trials,
)


def _read_results(root: Path, stage: str) -> dict[str, dict]:
    results: dict[str, dict] = {}
    stage_root = root / stage
    if not stage_root.exists():
        return results
    for path in sorted(stage_root.glob("*/result.json")):
        value = json.loads(path.read_text())
        label = value.get("label")
        if not isinstance(label, str) or label in results:
            raise RuntimeError(f"invalid or duplicate result label at {path}")
        results[label] = value
    return results


def _finite_metrics(value: object, location: str) -> None:
    if isinstance(value, dict):
        for key, child in value.items():
            _finite_metrics(child, f"{location}.{key}")
    elif isinstance(value, (int, float)) and not math.isfinite(float(value)):
        raise RuntimeError(f"nonfinite metric at {location}")


def _validate_result(result: dict, trial: TrialConfig) -> None:
    expected = {
        "label": trial.label,
        "architecture": trial.architecture.to_dict(),
        "lr": trial.lr,
        "seed": trial.seed,
        "steps": trial.steps,
    }
    for key, value in expected.items():
        if result.get(key) != value:
            raise RuntimeError(f"{trial.label}: mismatched {key}")
    if int(result.get("effective_batch", 1_024)) != trial.effective_batch:
        raise RuntimeError(f"{trial.label}: mismatched effective_batch")
    if result.get("lr_parameterization", "uniform") != trial.lr_parameterization:
        raise RuntimeError(f"{trial.label}: mismatched lr_parameterization")
    status = result.get("status", "complete")
    if status not in ("complete", "diverged"):
        raise RuntimeError(f"{trial.label}: invalid status {status}")
    if status == "diverged" and not trial.allow_divergence:
        raise RuntimeError(f"{trial.label}: unexpected divergence")
    completed_steps = int(result.get("steps_completed", trial.steps))
    target_reached = bool(result.get("target_reached", False))
    if (
        status == "complete"
        and completed_steps != trial.steps
        and not (
            target_reached
            and trial.target_validation_kl is not None
            and float(result["validation"]["kl"])
            <= trial.target_validation_kl
        )
    ):
        raise RuntimeError(f"{trial.label}: incomplete successful result")
    if status == "diverged" and not 0 <= completed_steps < trial.steps:
        raise RuntimeError(f"{trial.label}: invalid divergence step")
    for key in ("initial_validation", "validation"):
        metrics = result.get(key)
        if not isinstance(metrics, dict) or "kl" not in metrics:
            raise RuntimeError(f"{trial.label}: missing {key} metrics")
        _finite_metrics(metrics, f"{trial.label}.{key}")
    test = result.get("test")
    if status == "complete" and trial.is_final:
        if not isinstance(test, dict) or "kl" not in test:
            raise RuntimeError(f"{trial.label}: missing final test metrics")
        _finite_metrics(test, f"{trial.label}.test")
    elif test != {}:
        raise RuntimeError(f"{trial.label}: non-final trial used the test split")
    if int(result.get("trainable_parameters", 0)) <= 0:
        raise RuntimeError(f"{trial.label}: invalid trainable parameter count")
    if int(result.get("frozen_parameters", 0)) <= 0:
        raise RuntimeError(f"{trial.label}: invalid frozen parameter count")
    embedding_hash = result.get("embedding_sha256")
    if not isinstance(embedding_hash, str) or len(embedding_hash) != 64:
        raise RuntimeError(f"{trial.label}: invalid embedding hash")


def _validate_stage(
    actual: dict[str, dict],
    expected_trials: list[TrialConfig],
) -> list[dict]:
    expected = {trial.label: trial for trial in expected_trials}
    if set(actual) != set(expected):
        missing = sorted(set(expected) - set(actual))
        extra = sorted(set(actual) - set(expected))
        raise RuntimeError(f"trial grid mismatch: missing={missing}, extra={extra}")
    for label, trial in expected.items():
        _validate_result(actual[label], trial)
    return [actual[trial.label] for trial in expected_trials]


def _validate_checkpoint(root: Path, trial: TrialConfig, result: dict) -> int:
    path = root / trial.stage / trial.label / "student.pt"
    if not path.is_file() or path.stat().st_size <= 0:
        raise RuntimeError(f"{trial.label}: missing final checkpoint")
    checkpoint = torch.load(
        path,
        map_location="cpu",
        mmap=True,
        weights_only=True,
    )
    expected_metadata = {
        "schema": "qwen-fullwidth-student-v1",
        "trial": trial.to_dict(),
        "model_id": MODEL_ID,
        "model_revision": MODEL_REVISION,
        "embedding_sha256": result["embedding_sha256"],
        "metrics": result,
    }
    for key, value in expected_metadata.items():
        if checkpoint.get(key) != value:
            raise RuntimeError(f"{trial.label}: checkpoint mismatched {key}")
    if trial.stage in ("tensor_final", "tensor_long"):
        if (
            checkpoint.get("optimizer_state_included") is not True
            or not isinstance(checkpoint.get("optimizer_state_dict"), dict)
            or not checkpoint["optimizer_state_dict"].get("state")
        ):
            raise RuntimeError(
                f"{trial.label}: checkpoint lacks continuation optimizer state"
            )
    state = checkpoint.get("state_dict")
    if not isinstance(state, dict) or not state:
        raise RuntimeError(f"{trial.label}: empty checkpoint state")
    parameters = sum(
        int(tensor.numel())
        for tensor in state.values()
        if isinstance(tensor, torch.Tensor)
    )
    if parameters != int(result["trainable_parameters"]):
        raise RuntimeError(
            f"{trial.label}: checkpoint has {parameters} parameters, "
            f"expected {result['trainable_parameters']}"
        )
    return path.stat().st_size


def audit_study(output_root: str | Path) -> dict:
    root = Path(output_root)
    screen_map = _read_results(root, "screen")
    screen = _validate_stage(screen_map, screen_trials())

    common = select_common_topology(screen)
    deep = select_unrestricted_monarch(screen)
    tune_grid = tuning_trials(common, deep)
    tune_map = _read_results(root, "tune")
    tune = _validate_stage(tune_map, tune_grid)

    final_grid = final_trials(screen, tune)
    final_map = _read_results(root, "final")
    final = _validate_stage(final_map, final_grid)

    hashes = {
        result["embedding_sha256"]
        for result in screen + tune + final
    }
    if len(hashes) != 1:
        raise RuntimeError(f"embedding hash changed across trials: {sorted(hashes)}")

    plan = json.loads((root / "study-plan.json").read_text())
    expected_plan = {
        "selection_basis": "validation_only",
        "common_topology": common.to_dict(),
        "unrestricted_monarch": deep.to_dict(),
        "final_trials": [trial.to_dict() for trial in final_grid],
    }
    if plan != expected_plan:
        raise RuntimeError("persisted study plan does not match validation selection")

    summary = json.loads((root / "study-summary.json").read_text())
    expected_summary = {"screen": screen_map, "tune": tune_map, "final": final_map}
    for stage, result_map in expected_summary.items():
        rows = summary.get(stage)
        if not isinstance(rows, list):
            raise RuntimeError(f"study summary missing {stage}")
        summary_map = {row.get("label"): row for row in rows}
        if summary_map != result_map:
            raise RuntimeError(f"study summary disagrees with {stage} artifacts")

    checkpoint_bytes = 0
    for trial in final_grid:
        checkpoint_bytes += _validate_checkpoint(
            root, trial, final_map[trial.label]
        )
    leftover_progress = sorted(root.glob("final/*/progress.pt"))
    if leftover_progress:
        raise RuntimeError(
            f"completed study has leftover progress checkpoints: "
            f"{[str(path) for path in leftover_progress]}"
        )

    grouped: dict[str, list[float]] = {}
    for result in final:
        label = TrialConfig(
            architecture=trial_architecture(result),
            lr=float(result["lr"]),
            seed=int(result["seed"]),
            steps=int(result["steps"]),
            stage="final",
        ).architecture.label
        grouped.setdefault(label, []).append(float(result["test"]["kl"]))
    test_summary = {
        label: {
            "mean_kl": statistics.fmean(values),
            "std_kl": statistics.pstdev(values),
            "seeds": len(values),
        }
        for label, values in sorted(grouped.items())
    }
    return {
        "status": "complete",
        "screen_trials": len(screen),
        "tune_trials": len(tune),
        "final_trials": len(final),
        "embedding_sha256": next(iter(hashes)),
        "checkpoint_bytes": checkpoint_bytes,
        "test": test_summary,
    }


def _committed_reference_results(
    root: Path, trials: list[TrialConfig]
) -> list[dict]:
    stage_maps: dict[str, dict[str, dict]] = {}
    results = []
    for trial in trials:
        actual = stage_maps.setdefault(
            trial.stage, _read_results(root, trial.stage)
        )
        if trial.label not in actual:
            raise RuntimeError(f"missing depth reference {trial.label}")
        _validate_result(actual[trial.label], trial)
        results.append(actual[trial.label])
    return results


def _test_summary(final: list[dict]) -> dict[str, dict]:
    grouped: dict[str, list[float]] = {}
    for result in final:
        label = trial_architecture(result).label
        grouped.setdefault(label, []).append(float(result["test"]["kl"]))
    return {
        label: {
            "mean_kl": statistics.fmean(values),
            "std_kl": statistics.pstdev(values),
            "seeds": len(values),
        }
        for label, values in sorted(grouped.items())
    }


def audit_depth_study(output_root: str | Path) -> dict:
    root = Path(output_root)
    reference_grid = depth_reference_trials()
    references = _committed_reference_results(root, reference_grid)

    screen_grid = depth_screen_trials()
    screen_map = _read_results(root, "depth_screen")
    screens = _validate_stage(screen_map, screen_grid)
    selection_rows = references + screens
    untied, untied_lr, looped, looped_lr = select_depth_winners(selection_rows)

    optimization_grid = optimization_trials(selection_rows)
    optimization_map = _read_results(root, "depth_opt")
    optimization = _validate_stage(optimization_map, optimization_grid)
    (
        optimized_untied,
        optimized_untied_lr,
        optimized_untied_batch,
        optimized_looped,
        optimized_looped_lr,
        optimized_looped_batch,
    ) = select_optimized_winners(selection_rows, optimization)

    final_grid = optimized_depth_final_trials(selection_rows, optimization)
    final_map = _read_results(root, "depth_final")
    finals = _validate_stage(final_map, final_grid)

    hashes = {
        result["embedding_sha256"]
        for result in references + screens + optimization + finals
    }
    if len(hashes) != 1:
        raise RuntimeError(
            f"embedding hash changed across depth trials: {sorted(hashes)}"
        )

    loop_counts = {
        int(result["trainable_parameters"])
        for result in screens
        if trial_architecture(result).repetitions > 1
    }
    if loop_counts != {84_279_296}:
        raise RuntimeError(
            f"loop parameter count is not fixed at 84,279,296: {loop_counts}"
        )

    plan = json.loads((root / "depth-study-plan.json").read_text())
    expected_plan = {
        "selection_basis": "endpoint_validation_only",
        "reference_trials": [trial.to_dict() for trial in reference_grid],
        "screen_trials": [trial.to_dict() for trial in screen_grid],
        "structural_untied_winner": {
            "architecture": untied.to_dict(),
            "lr": untied_lr,
        },
        "structural_looped_winner": {
            "architecture": looped.to_dict(),
            "lr": looped_lr,
        },
        "optimization_trials": [
            trial.to_dict() for trial in optimization_grid
        ],
        "optimized_untied_winner": {
            "architecture": optimized_untied.to_dict(),
            "lr": optimized_untied_lr,
            "effective_batch": optimized_untied_batch,
        },
        "optimized_looped_winner": {
            "architecture": optimized_looped.to_dict(),
            "lr": optimized_looped_lr,
            "effective_batch": optimized_looped_batch,
        },
        "final_trials": [trial.to_dict() for trial in final_grid],
    }
    if plan != expected_plan:
        raise RuntimeError(
            "persisted depth plan does not match validation selection"
        )

    summary = json.loads((root / "depth-study-summary.json").read_text())
    expected_maps = {
        "reference": {
            result["label"]: result for result in references
        },
        "depth_screen": screen_map,
        "depth_opt": optimization_map,
        "depth_final": final_map,
    }
    for stage, result_map in expected_maps.items():
        rows = summary.get(stage)
        if not isinstance(rows, list):
            raise RuntimeError(f"depth summary missing {stage}")
        summary_map = {row.get("label"): row for row in rows}
        if summary_map != result_map:
            raise RuntimeError(
                f"depth summary disagrees with {stage} artifacts"
            )

    checkpoint_bytes = sum(
        _validate_checkpoint(root, trial, final_map[trial.label])
        for trial in final_grid
    )
    leftover_progress = sorted(root.glob("depth_final/*/progress.pt"))
    if leftover_progress:
        raise RuntimeError(
            "completed depth study has leftover progress checkpoints: "
            f"{[str(path) for path in leftover_progress]}"
        )
    return {
        "status": "complete",
        "reference_trials": len(references),
        "depth_screen_trials": len(screens),
        "optimization_trials": len(optimization),
        "depth_final_trials": len(finals),
        "untied_winner": {
            "architecture": optimized_untied.to_dict(),
            "lr": optimized_untied_lr,
            "effective_batch": optimized_untied_batch,
        },
        "looped_winner": {
            "architecture": optimized_looped.to_dict(),
            "lr": optimized_looped_lr,
            "effective_batch": optimized_looped_batch,
        },
        "embedding_sha256": next(iter(hashes)),
        "checkpoint_bytes": checkpoint_bytes,
        "test": _test_summary(finals),
    }


def _trial_from_dict(value: dict) -> TrialConfig:
    trial_value = dict(value)
    architecture_value = dict(trial_value.pop("architecture"))
    architecture_value.pop("label", None)
    trial_value.pop("label", None)
    from .config import ArchitectureConfig

    return TrialConfig(
        architecture=ArchitectureConfig(**architecture_value),
        **trial_value,
    )


def _expected_trainable_parameters(trial: TrialConfig) -> int:
    from .model import FullWidthStack

    with torch.device("meta"):
        model = FullWidthStack(trial.architecture)
    return sum(parameter.numel() for parameter in model.parameters())


def audit_tensor_study(output_root: str | Path) -> dict:
    root = Path(output_root)
    prerequisite = audit_depth_study(root)

    invariants = json.loads((root / "tensor-invariants.json").read_text())
    required_invariants = (
        "teacher_weights_tied",
        "embedding_lookup_exact",
        "unembedding_weights_exact",
        "unembedding_logits_close",
        "gradient_is_none",
        "excluded_from_state_dict",
    )
    if (
        invariants.get("status") != "complete"
        or not all(invariants.get(key) is True for key in required_invariants)
    ):
        raise RuntimeError("embedding/head invariant artifact is incomplete")

    references = _committed_reference_results(root, depth_reference_trials())
    screens = _validate_stage(
        _read_results(root, "depth_screen"),
        depth_screen_trials(),
    )
    selection_rows = references + screens
    optimization_grid = optimization_trials(selection_rows)
    optimization = _validate_stage(
        _read_results(root, "depth_opt"),
        optimization_grid,
    )
    reference = select_tensor_reference(optimization)

    control_trial = tensor_gate_control_trial(reference)
    control_map = _read_results(root, "tensor_gate_control")
    control_results = _validate_stage(control_map, [control_trial])
    control = control_results[0]

    probe_map = _read_results(root, "tensor_lr_probe")
    expected_probes = tensor_lr_probe_trials(reference)
    probe_rows = []
    for trial in expected_probes:
        if trial.label not in probe_map:
            raise RuntimeError(f"missing tensor LR probe {trial.label}")
        _validate_result(probe_map[trial.label], trial)
        probe_rows.append(probe_map[trial.label])
    for _ in range(2):
        boundary = next_tensor_boundary_probe(reference, probe_rows)
        if boundary is None:
            break
        expected_probes.append(boundary)
        if boundary.label not in probe_map:
            raise RuntimeError(f"missing tensor boundary probe {boundary.label}")
        _validate_result(probe_map[boundary.label], boundary)
        probe_rows.append(probe_map[boundary.label])
    if set(probe_map) != {trial.label for trial in expected_probes}:
        raise RuntimeError("tensor LR probe artifacts do not match adaptive grid")
    settings = select_tensor_lr_settings(probe_rows)

    gate = json.loads((root / "tensor-gate.json").read_text())
    gate_rows = [
        probe_map[trial.label]
        for trial in tensor_gate_btt_trials(reference)
    ]
    expected_gate = evaluate_tensor_gate(control, gate_rows)
    if gate != expected_gate or gate.get("status") != "passed":
        raise RuntimeError("persisted controlled BTT gate did not pass")

    kronecker_probe_map = _read_results(root, "tensor_kron_probe")
    expected_kronecker_probes = kronecker_lr_probe_trials(reference)
    kronecker_probe_rows = []
    for trial in expected_kronecker_probes:
        if trial.label not in kronecker_probe_map:
            raise RuntimeError(
                f"missing Kronecker LR probe {trial.label}"
            )
        _validate_result(kronecker_probe_map[trial.label], trial)
        kronecker_probe_rows.append(
            kronecker_probe_map[trial.label]
        )
    for _ in range(2):
        boundary = next_kronecker_boundary_probe(
            reference,
            kronecker_probe_rows,
        )
        if boundary is None:
            break
        expected_kronecker_probes.append(boundary)
        if boundary.label not in kronecker_probe_map:
            raise RuntimeError(
                f"missing Kronecker boundary probe {boundary.label}"
            )
        _validate_result(kronecker_probe_map[boundary.label], boundary)
        kronecker_probe_rows.append(
            kronecker_probe_map[boundary.label]
        )
    if set(kronecker_probe_map) != {
        trial.label for trial in expected_kronecker_probes
    }:
        raise RuntimeError(
            "Kronecker LR probe artifacts do not match adaptive grid"
        )
    kronecker_settings = select_kronecker_lr_settings(
        kronecker_probe_rows
    )
    kronecker_depth_grid = kronecker_depth_trials(
        reference,
        kronecker_probe_rows,
    )
    kronecker_depth_map = _read_results(root, "tensor_kron_depth")
    kronecker_depth_results = _validate_stage(
        kronecker_depth_map,
        kronecker_depth_grid,
    )

    parameter_grid = [
        *tensor_parameter_trials(reference, probe_rows),
        *kronecker_parameter_trials(reference, kronecker_probe_rows),
    ]
    parameter_map = _read_results(root, "tensor_param")
    parameter_results = _validate_stage(parameter_map, parameter_grid)

    plan = json.loads((root / "tensor-study-plan.json").read_text())
    benchmarks = json.loads((root / "tensor-benchmark.json").read_text())
    if plan.get("time_benchmarks") != benchmarks:
        raise RuntimeError("tensor plan and benchmark artifact disagree")
    if benchmarks.get("reference", {}).get("status") != "complete":
        raise RuntimeError("tensor time reference benchmark is incomplete")
    if not math.isclose(
        float(benchmarks["target_step_seconds"]),
        float(benchmarks["reference"]["median_step_seconds"]),
        rel_tol=1e-12,
        abs_tol=1e-12,
    ):
        raise RuntimeError("tensor benchmark target disagrees with reference")
    matched_depths = {
        str(key): int(value)
        for key, value in benchmarks.get("matched_depths", {}).items()
    }
    known_families = {
        tensor_family_key(config) for config in btt_parameter_architectures()
    }
    known_families.add(
        tensor_family_key(kronecker_parameter_architecture())
    )
    if not set(matched_depths).issubset(known_families):
        raise RuntimeError("tensor benchmark contains an unknown family")
    for family, depth in matched_depths.items():
        maximum = int(benchmarks["max_depths"][family])
        if not 1 <= depth <= maximum:
            raise RuntimeError(f"{family}: invalid time-matched depth {depth}")
        detail = benchmarks["families"].get(family)
        if detail is None or detail.get("status") != "matched":
            raise RuntimeError(f"{family}: missing matched benchmark detail")
        if int(detail.get("matched_depth", -1)) != depth:
            raise RuntimeError(f"{family}: benchmark depth disagreement")
        if float(detail.get("ratio", math.inf)) > 1.05:
            raise RuntimeError(f"{family}: time match exceeds tolerance")
        selected = [
            row
            for row in detail.get("measurements", [])
            if int(row.get("depth", -1)) == depth
        ]
        if len(selected) != 1:
            raise RuntimeError(f"{family}: missing unique selected benchmark")
        if (
            selected[0].get("status") != "complete"
            or float(selected[0].get("peak_allocated_gib", math.inf)) >= 75
        ):
            raise RuntimeError(f"{family}: selected benchmark is infeasible")

    time_grid = tensor_time_trials(
        reference,
        probe_rows,
        matched_depths,
        kronecker_probe_rows,
    )
    time_map = _read_results(root, "tensor_time")
    time_results = _validate_stage(time_map, time_grid)
    btt_winner, kronecker_winner, time_winner = select_tensor_finalists(
        parameter_results, time_results
    )
    final_grid = tensor_final_trials(parameter_results, time_results)
    final_map = _read_results(root, "tensor_final")
    finals = _validate_stage(final_map, final_grid)
    long_grid = [
        tensor_long_trial(parameter_results, time_results, finals)
    ]
    long_map = _read_results(root, "tensor_long")
    long_results = _validate_stage(long_map, long_grid)

    for trial, result in [
        *zip(
            kronecker_depth_grid,
            kronecker_depth_results,
            strict=True,
        ),
        *zip(parameter_grid, parameter_results, strict=True),
        *zip(time_grid, time_results, strict=True),
        *zip(final_grid, finals, strict=True),
        *zip(long_grid, long_results, strict=True),
    ]:
        expected = _expected_trainable_parameters(trial)
        if int(result["trainable_parameters"]) != expected:
            raise RuntimeError(
                f"{trial.label}: expected {expected} trainable parameters, "
                f"got {result['trainable_parameters']}"
            )

    hashes = {
        result["embedding_sha256"]
        for result in [
            reference,
            control,
            *probe_rows,
            *kronecker_probe_rows,
            *kronecker_depth_results,
            *parameter_results,
            *time_results,
            *finals,
            *long_results,
        ]
    }
    hashes.add(invariants["embedding_sha256"])
    if len(hashes) != 1:
        raise RuntimeError(
            f"embedding hash changed across tensor study: {sorted(hashes)}"
        )

    expected_plan = {
        "selection_basis": "endpoint_validation_only",
        "prerequisite_audit": prerequisite,
        "embedding_head_invariants": invariants,
        "monarch_reference": reference,
        "gate_control_trial": control_trial.to_dict(),
        "controlled_gate": gate,
        "lr_probe_trials": [trial.to_dict() for trial in expected_probes],
        "selected_lr_settings": [
            {"lr": lr, "lr_parameterization": parameterization}
            for lr, parameterization in settings
        ],
        "kronecker_lr_probe_trials": [
            trial.to_dict() for trial in expected_kronecker_probes
        ],
        "selected_kronecker_lr_settings": [
            {"lr": lr, "lr_parameterization": parameterization}
            for lr, parameterization in kronecker_settings
        ],
        "kronecker_depth_trials": [
            trial.to_dict() for trial in kronecker_depth_grid
        ],
        "parameter_frontier_trials": [
            trial.to_dict() for trial in parameter_grid
        ],
        "time_benchmarks": benchmarks,
        "time_frontier_trials": [trial.to_dict() for trial in time_grid],
        "btt_parameter_winner": btt_winner,
        "kronecker_parameter_winner": kronecker_winner,
        "time_winner": time_winner,
        "final_trials": [trial.to_dict() for trial in final_grid],
        "long_trials": [trial.to_dict() for trial in long_grid],
    }
    if plan != expected_plan:
        raise RuntimeError(
            "persisted tensor plan does not match audited selection"
        )

    summary = json.loads((root / "tensor-study-summary.json").read_text())
    expected_maps = {
        "tensor_gate_control": control_map,
        "tensor_lr_probe": probe_map,
        "tensor_kron_probe": kronecker_probe_map,
        "tensor_kron_depth": kronecker_depth_map,
        "tensor_param": parameter_map,
        "tensor_time": time_map,
        "tensor_final": final_map,
        "tensor_long": long_map,
    }
    for stage, result_map in expected_maps.items():
        rows = summary.get(stage)
        if not isinstance(rows, list):
            raise RuntimeError(f"tensor summary missing {stage}")
        if {row.get("label"): row for row in rows} != result_map:
            raise RuntimeError(f"tensor summary disagrees with {stage}")

    checkpoint_bytes = sum(
        _validate_checkpoint(root, trial, final_map[trial.label])
        for trial in final_grid
    )
    checkpoint_bytes += sum(
        _validate_checkpoint(root, trial, long_map[trial.label])
        for trial in long_grid
    )
    leftover_progress = sorted(root.glob("tensor_final/*/progress.pt"))
    leftover_progress.extend(
        sorted(root.glob("tensor_long/*/progress.pt"))
    )
    if leftover_progress:
        raise RuntimeError(
            "completed tensor study has leftover progress checkpoints: "
            f"{[str(path) for path in leftover_progress]}"
        )
    return {
        "status": "complete",
        "lr_probe_trials": len(probe_rows),
        "stable_lr_probes": sum(
            result.get("status", "complete") == "complete"
            for result in probe_rows
        ),
        "kronecker_lr_probe_trials": len(kronecker_probe_rows),
        "kronecker_depth_trials": len(kronecker_depth_results),
        "parameter_frontier_trials": len(parameter_results),
        "time_frontier_trials": len(time_results),
        "tensor_final_trials": len(finals),
        "matched_depths": matched_depths,
        "btt_parameter_winner": btt_winner["label"],
        "kronecker_parameter_winner": kronecker_winner["label"],
        "time_winner": time_winner["label"],
        "long_trial": long_results[0]["label"],
        "target_validation_kl": long_results[0][
            "target_validation_kl"
        ],
        "target_reached": long_results[0]["target_reached"],
        "validation_kl_gap": long_results[0]["validation_kl_gap"],
        "embedding_sha256": next(iter(hashes)),
        "checkpoint_bytes": checkpoint_bytes,
        "test": _test_summary([*finals, *long_results]),
    }


def trial_architecture(result: dict):
    from .config import ArchitectureConfig

    return ArchitectureConfig(**result["architecture"])


def _wandb_config_matches(config: dict, result: dict, stage: str) -> bool:
    try:
        architecture = dict(config.get("architecture", {}))
        architecture.pop("label", None)
        return (
            config.get("stage") == stage
            and architecture == result["architecture"]
            and float(config.get("lr", math.nan)) == float(result["lr"])
            and int(config.get("seed", -1)) == int(result["seed"])
            and int(config.get("steps", -1)) == int(result["steps"])
            and config.get("objective") == "full_vocab_forward_kl"
            and int(config.get("effective_batch", -1))
            == int(result.get("effective_batch", 1_024))
            and str(config.get("lr_parameterization", "uniform"))
            == str(result.get("lr_parameterization", "uniform"))
        )
    except (TypeError, ValueError):
        return False


def _audit_wandb_once(
    output_root: str | Path,
    project_path: str,
    stages: tuple[str, ...],
) -> dict:
    import wandb

    root = Path(output_root)
    expected: dict[str, tuple[str, dict]] = {}
    for stage in stages:
        for label, result in _read_results(root, stage).items():
            expected[label] = (stage, result)

    api = wandb.Api(timeout=60)
    runs = list(api.runs(project_path))
    by_name: dict[str, list] = {}
    for run in runs:
        by_name.setdefault(run.name, []).append(run)

    matched: dict[str, str] = {}
    required_history = (
        "train/kl",
        "validation/kl",
        "performance/step_seconds",
        "performance/peak_allocated_gib",
        "optimizer/lr",
        "diagnostic/gradient_norm",
        "diagnostic/nonfinite_gradients",
        "final_validation/kl",
    )
    for label, (stage, result) in expected.items():
        accepted = None
        for run in by_name.get(label, []):
            if run.state != "finished":
                continue
            config = dict(run.config)
            if not _wandb_config_matches(config, result, stage):
                continue
            summary = dict(run.summary)
            diverged = result.get("status", "complete") == "diverged"
            required = (
                ("validation/kl", "final_validation/kl", "status/diverged")
                if diverged
                else required_history
            )
            if any(key not in summary for key in required):
                continue
            if summary.get("embedding_sha256") != result["embedding_sha256"]:
                continue
            if not math.isclose(
                float(summary["final_validation/kl"]),
                float(result["validation"]["kl"]),
                rel_tol=1e-9,
                abs_tol=1e-9,
            ):
                continue
            if (
                not diverged
                and float(summary["diagnostic/nonfinite_gradients"]) != 0
            ):
                continue
            if diverged and float(summary["status/diverged"]) != 1:
                continue
            if stage in (
                "final",
                "depth_final",
                "tensor_final",
                "tensor_long",
            ):
                if "test/kl" not in summary or not math.isclose(
                    float(summary["test/kl"]),
                    float(result["test"]["kl"]),
                    rel_tol=1e-9,
                    abs_tol=1e-9,
                ):
                    continue
            accepted = run
            break
        if accepted is None:
            states = [run.state for run in by_name.get(label, [])]
            raise RuntimeError(
                f"{label}: no matching finished W&B run; observed states={states}"
            )
        matched[label] = accepted.id

    return {
        "project": project_path,
        "project_url": f"https://wandb.ai/{project_path}",
        "matched_trials": len(matched),
        "run_ids": matched,
    }


def audit_wandb(
    output_root: str | Path,
    project_path: str = "umd-leans-well/qwen-fullwidth-monarch-distill",
    *,
    attempts: int = 20,
    retry_seconds: float = 15.0,
    stages: tuple[str, ...] = ("screen", "tune", "final"),
) -> dict:
    if attempts <= 0 or retry_seconds < 0:
        raise ValueError("invalid W&B audit retry policy")
    last_error = None
    for attempt in range(1, attempts + 1):
        try:
            return _audit_wandb_once(output_root, project_path, stages)
        except RuntimeError as error:
            last_error = error
            if attempt == attempts:
                break
            print(
                f"[wandb-audit] attempt {attempt}/{attempts} waiting for "
                f"ingestion: {error}",
                flush=True,
            )
            time.sleep(retry_seconds)
    raise RuntimeError(
        f"W&B audit did not converge after {attempts} attempts: {last_error}"
    ) from last_error
