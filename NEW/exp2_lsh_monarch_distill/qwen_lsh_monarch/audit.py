from __future__ import annotations

import json
import math
import statistics
from pathlib import Path

import torch

from .codebook import load_codebook_artifact
from .config import LSH_BITS, MODEL_ID, MODEL_REVISION, TrialConfig
from .study import (
    final_trials,
    screen_trials,
    select_lr,
    select_topology,
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
    for key in ("initial_validation", "validation"):
        metrics = result.get(key)
        if not isinstance(metrics, dict) or "kl" not in metrics:
            raise RuntimeError(f"{trial.label}: missing {key} metrics")
        _finite_metrics(metrics, f"{trial.label}.{key}")
    test = result.get("test")
    if trial.stage == "final":
        if not isinstance(test, dict) or "kl" not in test:
            raise RuntimeError(f"{trial.label}: missing final test metrics")
        _finite_metrics(test, f"{trial.label}.test")
    elif test != {}:
        raise RuntimeError(f"{trial.label}: non-final trial used the test split")
    if int(result.get("trainable_parameters", 0)) <= 0:
        raise RuntimeError(f"{trial.label}: invalid trainable parameter count")
    if int(result.get("frozen_parameters", 0)) <= 0:
        raise RuntimeError(f"{trial.label}: invalid frozen parameter count")
    for key in ("embedding_sha256", "codebook_sha256"):
        value = result.get(key)
        if not isinstance(value, str) or len(value) != 64:
            raise RuntimeError(f"{trial.label}: invalid {key}")
    report = result.get("codebook_report")
    if (
        not isinstance(report, dict)
        or report.get("bits") != LSH_BITS
        or report.get("codebook_sha256") != result["codebook_sha256"]
    ):
        raise RuntimeError(f"{trial.label}: invalid codebook report")


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
    path = root / "final" / trial.label / "student.pt"
    if not path.is_file() or path.stat().st_size <= 0:
        raise RuntimeError(f"{trial.label}: missing final checkpoint")
    checkpoint = torch.load(
        path, map_location="cpu", mmap=True, weights_only=True
    )
    expected = {
        "schema": "qwen-lsh18-monarch-student-v1",
        "trial": trial.to_dict(),
        "model_id": MODEL_ID,
        "model_revision": MODEL_REVISION,
        "embedding_sha256": result["embedding_sha256"],
        "codebook_sha256": result["codebook_sha256"],
        "metrics": result,
    }
    for key, value in expected.items():
        if checkpoint.get(key) != value:
            raise RuntimeError(f"{trial.label}: checkpoint mismatched {key}")
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


def audit_study(
    output_root: str | Path,
    codebook_path: str | Path | None = None,
) -> dict:
    root = Path(output_root)
    screen_map = _read_results(root, "screen")
    screen = _validate_stage(screen_map, screen_trials())
    selected = select_topology(screen)
    tune_grid = tuning_trials(selected)
    tune_map = _read_results(root, "tune")
    tune = _validate_stage(tune_map, tune_grid)
    final_grid = final_trials(screen, tune)
    final_map = _read_results(root, "final")
    final = _validate_stage(final_map, final_grid)

    embedding_hashes = {
        result["embedding_sha256"] for result in screen + tune + final
    }
    codebook_hashes = {
        result["codebook_sha256"] for result in screen + tune + final
    }
    if len(embedding_hashes) != 1 or len(codebook_hashes) != 1:
        raise RuntimeError("teacher or codebook hash changed across trials")
    embedding_hash = next(iter(embedding_hashes))
    codebook_hash = next(iter(codebook_hashes))
    if codebook_path is not None:
        _, metadata = load_codebook_artifact(
            codebook_path, embedding_sha256=embedding_hash
        )
        if metadata["codebook_sha256"] != codebook_hash:
            raise RuntimeError("study results disagree with codebook artifact")

    selected_lr = select_lr(selected, screen + tune)
    plan = json.loads((root / "study-plan.json").read_text())
    expected_plan = {
        "selection_basis": "validation_only",
        "selected_topology": selected.to_dict(),
        "selected_lr": selected_lr,
        "final_trials": [trial.to_dict() for trial in final_grid],
    }
    if plan != expected_plan:
        raise RuntimeError("persisted study plan does not match validation selection")

    summary = json.loads((root / "study-summary.json").read_text())
    for stage, result_map in (
        ("screen", screen_map),
        ("tune", tune_map),
        ("final", final_map),
    ):
        rows = summary.get(stage)
        if not isinstance(rows, list):
            raise RuntimeError(f"study summary missing {stage}")  # noqa: TRY004
        if {row.get("label"): row for row in rows} != result_map:
            raise RuntimeError(f"study summary disagrees with {stage} artifacts")

    checkpoint_bytes = sum(
        _validate_checkpoint(root, trial, final_map[trial.label])
        for trial in final_grid
    )
    leftover_progress = sorted(root.glob("final/*/progress.pt"))
    if leftover_progress:
        raise RuntimeError(
            f"completed study has leftover progress checkpoints: "
            f"{[str(path) for path in leftover_progress]}"
        )
    values = [float(result["test"]["kl"]) for result in final]
    return {
        "status": "complete",
        "screen_trials": len(screen),
        "tune_trials": len(tune),
        "final_trials": len(final),
        "embedding_sha256": embedding_hash,
        "codebook_sha256": codebook_hash,
        "checkpoint_bytes": checkpoint_bytes,
        "test": {
            selected.label: {
                "mean_kl": statistics.fmean(values),
                "std_kl": statistics.pstdev(values),
                "seeds": len(values),
            }
        },
    }


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
            and config.get("representation") == "signed_repaired_lsh18"
            and int(config.get("effective_batch", -1)) == 1_024
            and config.get("embedding_sha256") == result["embedding_sha256"]
            and config.get("codebook_sha256") == result["codebook_sha256"]
        )
    except (TypeError, ValueError):
        return False


def audit_wandb(
    output_root: str | Path,
    project_path: str = "umd-leans-well/qwen-lsh18-monarch-distill",
) -> dict:
    import wandb

    root = Path(output_root)
    expected: dict[str, tuple[str, dict]] = {}
    for stage in ("screen", "tune", "final"):
        for label, result in _read_results(root, stage).items():
            expected[label] = (stage, result)
    runs = list(wandb.Api(timeout=60).runs(project_path))
    by_name: dict[str, list] = {}
    for run in runs:
        by_name.setdefault(run.name, []).append(run)
    required = (
        "train/kl",
        "validation/kl",
        "performance/step_seconds",
        "performance/peak_allocated_gib",
        "optimizer/lr",
        "diagnostic/gradient_norm",
        "diagnostic/nonfinite_gradients",
        "final_validation/kl",
    )
    matched: dict[str, str] = {}
    for label, (stage, result) in expected.items():
        accepted = None
        for run in by_name.get(label, []):
            if run.state != "finished":
                continue
            if not _wandb_config_matches(dict(run.config), result, stage):
                continue
            summary = dict(run.summary)
            if any(key not in summary for key in required):
                continue
            if not math.isclose(
                float(summary["final_validation/kl"]),
                float(result["validation"]["kl"]),
                rel_tol=1e-9,
                abs_tol=1e-9,
            ):
                continue
            if float(summary["diagnostic/nonfinite_gradients"]) != 0:
                continue
            if stage == "final" and (
                "test/kl" not in summary
                or not math.isclose(
                    float(summary["test/kl"]),
                    float(result["test"]["kl"]),
                    rel_tol=1e-9,
                    abs_tol=1e-9,
                )
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
