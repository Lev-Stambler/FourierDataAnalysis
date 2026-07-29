from __future__ import annotations

import json
import os
import subprocess
import sys
from pathlib import Path

from .config import (
    DEFAULT_DATA_ROOT,
    DEFAULT_OUTPUT_ROOT,
    FINAL_EXAMPLES,
    GLOBAL_BATCH,
    GLOBAL_TOKEN_BATCH,
    LOCAL_BATCH,
    MID_EXAMPLES,
    MINIMUM_ACCEPTED_THROUGHPUT_MULTIPLIER,
    PLAN_SCHEMA,
    PREFLIGHT_MICROBATCHES,
    STUDY_VARIANT,
    SUMMARY_SCHEMA,
    TARGET_THROUGHPUT_MULTIPLIER,
    TARGET_VALIDATION_KL,
    UNDERFILLED_BASELINE_CONTEXTS_PER_SECOND,
    WORLD_SIZE,
    Cell,
    canonical_hash,
    push_lr_cells,
    push_normuon_lr_cells,
    push_wide_cells,
    study_plan,
)
from .study import (
    continuation_cell,
    continuation_improved,
)
from .train import (
    atomic_json,
    cell_from_dict,
    checkpoint_path,
    completed_result,
)

COORDINATOR_SCHEMA = "qwen-kron-distill-coordinator-v1"
PREFLIGHT_SCHEMA = "qwen-kron-distill-preflight-v1"
STAGE_SPEC_SCHEMA = "qwen-kron-stage-spec-v1"


def _status_path(output_root: str) -> Path:
    return Path(output_root) / "coordinator-status.json"


def _write_status(output_root: str, value: dict) -> None:
    atomic_json(
        _status_path(output_root),
        {"schema": COORDINATOR_SCHEMA, **value},
    )


def _load_results(output_root: str, cells: list[Cell]) -> list[dict]:
    results = []
    for cell in cells:
        result = completed_result(output_root, cell)
        if result is None:
            raise RuntimeError(f"missing completed cell {cell.label}")
        results.append(result)
    return results


def _stage_entry(
    cell: Cell,
    *,
    source: dict | None,
    output_root: str,
) -> dict:
    entry = {"cell": cell.to_dict()}
    if source is not None:
        source_cell = Cell(
            stage=source["cell"]["stage"],
            architecture=cell.architecture,
            target_examples=int(source["cell"]["target_examples"]),
            factor_lr=float(source["cell"]["factor_lr"]),
            auxiliary_lr=float(source["cell"]["auxiliary_lr"]),
            seed=int(source["cell"]["seed"]),
        )
        source_cell.validate()
        entry.update(
            {
                "source_label": source["label"],
                "source_checkpoint": str(checkpoint_path(output_root, source_cell)),
                "initial_validation": source["validation"],
            }
        )
    return entry


def _run_stage(
    stage: str,
    cells: list[Cell],
    *,
    sources: dict[str, dict] | None,
    evaluate_test: bool,
    data_root: str,
    output_root: str,
) -> list[dict]:
    if all(completed_result(output_root, cell) for cell in cells):
        return _load_results(output_root, cells)
    entries = []
    for cell in cells:
        if completed_result(output_root, cell):
            continue
        source = None if sources is None else sources.get(cell.label)
        entries.append(
            _stage_entry(
                cell,
                source=source,
                output_root=output_root,
            )
        )
    spec = {
        "schema": STAGE_SPEC_SCHEMA,
        "stage": stage,
        "entries": entries,
        "evaluate_test": evaluate_test,
        "study_plan_sha256": study_plan()["plan_sha256"],
    }
    spec_path = Path(output_root) / "specs" / f"{stage}.json"
    atomic_json(spec_path, spec)
    log_path = Path(output_root) / "logs" / f"{stage}.log"
    log_path.parent.mkdir(parents=True, exist_ok=True)
    command = [
        sys.executable,
        "-m",
        "torch.distributed.run",
        "--standalone",
        f"--nproc-per-node={WORLD_SIZE}",
        "-m",
        "qwen_kron_distill",
        "run-stage",
        "--spec",
        str(spec_path),
        "--data-root",
        data_root,
        "--output-root",
        output_root,
    ]
    env = {
        **os.environ,
        "PYTHONUNBUFFERED": "1",
        "QWEN_KRON_MICROBATCH": os.environ.get(
            "QWEN_KRON_MICROBATCH",
            str(LOCAL_BATCH),
        ),
    }
    for attempt in range(1, 4):
        _write_status(
            output_root,
            {
                "status": "running",
                "stage": stage,
                "attempt": attempt,
                "cells": [cell.label for cell in cells],
                "log": str(log_path),
            },
        )
        with log_path.open("ab") as log:
            completed = subprocess.run(
                command,
                stdout=log,
                stderr=subprocess.STDOUT,
                env=env,
                check=False,
            )
        if completed.returncode == 0 and all(
            completed_result(output_root, cell) for cell in cells
        ):
            return _load_results(output_root, cells)
    raise RuntimeError(f"stage {stage} exhausted three retries")


def run_preflight(
    *,
    data_root: str = DEFAULT_DATA_ROOT,
    output_root: str = DEFAULT_OUTPUT_ROOT,
) -> dict:
    root = Path(output_root) / "preflight"
    root.mkdir(parents=True, exist_ok=True)
    base_command = [
        sys.executable,
        "-m",
        "qwen_kron_distill",
        "preflight-worker",
        "--data-root",
        data_root,
    ]
    requested = os.environ.get("QWEN_KRON_PREFLIGHT_MICROBATCHES")
    candidates = (
        [int(value) for value in requested.split(",")]
        if requested
        else list(PREFLIGHT_MICROBATCHES)
    )
    candidates = list(dict.fromkeys(candidates))
    if any(value <= 0 or LOCAL_BATCH % value for value in candidates):
        raise ValueError(
            "every preflight microbatch must be a positive local-batch divisor"
        )
    attempts = []
    viable = []

    for microbatch in candidates:
        baseline_path = root / f"world1-micro{microbatch}.json"
        distributed_path = root / f"world8-micro{microbatch}.json"
        baseline_log = root / f"world1-micro{microbatch}.log"
        distributed_log = root / f"world8-micro{microbatch}.log"
        baseline_path.unlink(missing_ok=True)
        distributed_path.unlink(missing_ok=True)
        env = {
            **os.environ,
            "PYTHONUNBUFFERED": "1",
            "QWEN_KRON_MICROBATCH": str(microbatch),
        }
        baseline_env = {**env, "CUDA_VISIBLE_DEVICES": "0"}
        with baseline_log.open("ab") as log:
            baseline = subprocess.run(
                [*base_command, "--result-file", str(baseline_path)],
                stdout=log,
                stderr=subprocess.STDOUT,
                env=baseline_env,
                check=False,
            )
        if baseline.returncode != 0 or not baseline_path.is_file():
            attempts.append(
                {
                    "microbatch": microbatch,
                    "status": "one_h200_failed",
                    "log": str(baseline_log),
                }
            )
            continue
        with distributed_log.open("ab") as log:
            distributed = subprocess.run(
                [
                    sys.executable,
                    "-m",
                    "torch.distributed.run",
                    "--standalone",
                    f"--nproc-per-node={WORLD_SIZE}",
                    "-m",
                    "qwen_kron_distill",
                    "preflight-worker",
                    "--data-root",
                    data_root,
                    "--result-file",
                    str(distributed_path),
                ],
                stdout=log,
                stderr=subprocess.STDOUT,
                env=env,
                check=False,
            )
        if distributed.returncode != 0 or not distributed_path.is_file():
            attempts.append(
                {
                    "microbatch": microbatch,
                    "status": "eight_h200_failed",
                    "log": str(distributed_log),
                }
            )
            continue

        one = json.loads(baseline_path.read_text())
        eight = json.loads(distributed_path.read_text())
        scaling = float(eight["contexts_per_second"]) / float(
            one["contexts_per_second"]
        )
        speedup = (
            float(eight["contexts_per_second"])
            / UNDERFILLED_BASELINE_CONTEXTS_PER_SECOND
        )
        failures = []
        for value, label, world_size in (
            (one, "one-H200", 1),
            (eight, "eight-H200", WORLD_SIZE),
        ):
            if (
                value.get("schema") != "qwen-kron-preflight-worker-v1"
                or int(value.get("world_size", -1)) != world_size
                or int(value.get("microbatch", -1)) != microbatch
            ):
                failures.append(f"{label} result identity is invalid")
            if float(value["memory_ratio"]) > 0.90:
                failures.append(f"{label} memory ratio exceeds 90%")
            if not value.get("factor_state_initialized") or not value.get(
                "embedding_state_initialized"
            ):
                failures.append(f"{label} optimizer state was not initialized")
            if (
                STUDY_VARIANT == "v5-normuon-lr"
                and not value.get("auxiliary_state_initialized")
            ):
                failures.append(
                    f"{label} auxiliary NorMuon state was not initialized"
                )
            if (
                STUDY_VARIANT == "v5-normuon-lr"
                and (
                    not value.get("full_base_lr_exercised")
                    or not value.get("optimizer_states_finite")
                )
            ):
                failures.append(
                    f"{label} did not pass the full-base-LR state check"
                )
            if not (
                float(value["loss_min"]) <= float(value["loss_max"]) < float("inf")
            ):
                failures.append(f"{label} loss is not finite")
            if (
                float(value.get("dense_optimized_loss_error", float("inf")))
                > 5e-4
            ):
                failures.append(f"{label} optimized KL disagrees with dense KL")
        if scaling < 6.4:
            failures.append("eight-H200 throughput scaling is below 6.4x")
        if int(eight.get("global_batch", -1)) != GLOBAL_BATCH:
            failures.append("eight-H200 global context batch is incorrect")
        if int(eight.get("global_token_batch", -1)) != GLOBAL_TOKEN_BATCH:
            failures.append("eight-H200 global token batch is incorrect")
        if speedup < MINIMUM_ACCEPTED_THROUGHPUT_MULTIPLIER:
            failures.append(
                "eight-H200 throughput is below the measured 5x acceptance floor"
            )
        attempts.append(
            {
                "microbatch": microbatch,
                "status": "viable" if not failures else "rejected",
                "failures": failures,
                "one_h200": one,
                "eight_h200": eight,
                "throughput_scaling": scaling,
                "underfilled_baseline_speedup": speedup,
                "one_h200_log": str(baseline_log),
                "eight_h200_log": str(distributed_log),
            }
        )
        if not failures:
            viable.append(
                {
                    "microbatch": microbatch,
                    "one_h200": one,
                    "eight_h200": eight,
                    "throughput_scaling": scaling,
                    "underfilled_baseline_speedup": speedup,
                }
            )

    if viable:
        selected = max(
            viable,
            key=lambda value: float(
                value["eight_h200"]["contexts_per_second"]
            ),
        )
        result = {
            "schema": PREFLIGHT_SCHEMA,
            "status": "complete",
            "accepted": True,
            "plan_sha256": study_plan()["plan_sha256"],
            "selected_microbatch": selected["microbatch"],
            "selected_global_batch": GLOBAL_BATCH,
            "selected_global_token_batch": GLOBAL_TOKEN_BATCH,
            "one_h200": selected["one_h200"],
            "eight_h200": selected["eight_h200"],
            "throughput_scaling": selected["throughput_scaling"],
            "underfilled_baseline_speedup": selected[
                "underfilled_baseline_speedup"
            ],
            "stretch_throughput_target_met": (
                selected["underfilled_baseline_speedup"]
                >= TARGET_THROUGHPUT_MULTIPLIER
            ),
            "requirements": {
                "maximum_memory_ratio": 0.90,
                "minimum_throughput_scaling": 6.4,
                "underfilled_baseline_contexts_per_second": (
                    UNDERFILLED_BASELINE_CONTEXTS_PER_SECOND
                ),
                "minimum_accepted_underfilled_baseline_speedup": (
                    MINIMUM_ACCEPTED_THROUGHPUT_MULTIPLIER
                ),
                "stretch_underfilled_baseline_speedup": (
                    TARGET_THROUGHPUT_MULTIPLIER
                ),
                "global_token_batch": GLOBAL_TOKEN_BATCH,
            },
            "failures": [],
            "attempts": attempts,
        }
    else:
        result = {
            "schema": PREFLIGHT_SCHEMA,
            "status": "complete",
            "accepted": False,
            "plan_sha256": study_plan()["plan_sha256"],
            "failures": [
                "no batch met memory, numerical, scaling, and throughput requirements"
            ],
            "attempts": attempts,
        }
    result["preflight_sha256"] = canonical_hash(result)
    atomic_json(Path(output_root) / "preflight.json", result)
    if not result["accepted"]:
        raise RuntimeError(f"preflight rejected: {result['failures']}")
    return result


def launch_study(
    *,
    data_root: str = DEFAULT_DATA_ROOT,
    output_root: str = DEFAULT_OUTPUT_ROOT,
) -> dict:
    root = Path(output_root)
    root.mkdir(parents=True, exist_ok=True)
    if (
        STUDY_VARIANT in (
            "v4-wide",
            "v4-isolated",
            "v5-normuon-lr",
        )
        and not os.environ.get("WANDB_API_KEY")
        and os.environ.get("WANDB_MODE") != "offline"
    ):
        raise RuntimeError(
            "v4 requires online W&B auth or explicit offline continuation mode"
        )
    plan = study_plan()
    atomic_json(root / "study-plan.json", plan)
    preflight_path = root / "preflight.json"
    if not preflight_path.is_file():
        raise RuntimeError("run exp5 preflight before launch")
    preflight = json.loads(preflight_path.read_text())
    preflight_digest = preflight.get("preflight_sha256")
    unsigned_preflight = dict(preflight)
    unsigned_preflight.pop("preflight_sha256", None)
    if (
        preflight.get("schema") != PREFLIGHT_SCHEMA
        or preflight.get("accepted") is not True
        or preflight.get("plan_sha256") != plan["plan_sha256"]
        or preflight_digest != canonical_hash(unsigned_preflight)
    ):
        raise RuntimeError("exp5 preflight is absent or rejected")
    os.environ["QWEN_KRON_MICROBATCH"] = str(int(preflight["selected_microbatch"]))

    wide = STUDY_VARIANT in ("v4-wide", "v4-isolated")
    normuon_only = STUDY_VARIANT == "v5-normuon-lr"
    screen_cells = (
        push_normuon_lr_cells()
        if normuon_only
        else push_wide_cells()
        if wide
        else push_lr_cells()
    )
    screen_results = _run_stage(
        "normuon_lr" if normuon_only else "width_lr" if wide else "lr",
        screen_cells,
        sources=None,
        evaluate_test=False,
        data_root=data_root,
        output_root=output_root,
    )
    screen_winner = min(
        screen_results,
        key=lambda value: (
            float(value["validation"]["kl"]),
            int(value["inventory"]["trainable_parameters"]),
            float(value["cell"]["factor_lr"]),
        ),
    )
    isolated = STUDY_VARIANT == "v4-isolated"
    mid_sources = (
        [
            value
            for value in screen_results
            if float(value["cell"]["factor_lr"])
            == max(cell.factor_lr for cell in screen_cells)
        ]
        if isolated
        else [screen_winner]
    )
    mid_cells = [
        continuation_cell(
            source,
            stage="mid",
            target_examples=MID_EXAMPLES,
        )
        for source in mid_sources
    ]
    mid_results = _run_stage(
        "mid",
        mid_cells,
        sources={
            cell.label: source
            for cell, source in zip(mid_cells, mid_sources, strict=True)
        },
        evaluate_test=False,
        data_root=data_root,
        output_root=output_root,
    )
    best_mid_kl = min(
        float(value["validation"]["kl"]) for value in mid_results
    )
    mid_winner = min(
        (
            value
            for value in mid_results
            if (
                not isolated
                or float(value["validation"]["kl"])
                <= best_mid_kl + 0.1
            )
        ),
        key=lambda value: (
            int(value["inventory"]["trainable_parameters"]),
            float(value["validation"]["kl"]),
        ),
    )
    final_cell = continuation_cell(
        mid_winner,
        stage="final",
        target_examples=FINAL_EXAMPLES,
    )
    final_results = _run_stage(
        "final",
        [final_cell],
        sources={final_cell.label: mid_winner},
        evaluate_test=True,
        data_root=data_root,
        output_root=output_root,
    )
    final_result = final_results[0]
    summary = {
        "schema": SUMMARY_SCHEMA,
        "status": "complete",
        "plan_sha256": plan["plan_sha256"],
        "product_family_irreducible_validation_kl": 1.728261911646456,
        "target_validation_kl": TARGET_VALIDATION_KL,
        "study_variant": STUDY_VARIANT,
        "screen_results": screen_results,
        "screen_winner": screen_winner,
        "mid_results": mid_results,
        "mid_winner": mid_winner,
        "final": final_result,
        "final_improved": continuation_improved(
            mid_winner,
            final_result,
        ),
        "winner": final_result,
        "target_met": (
            float(final_result["validation"]["kl"])
            <= TARGET_VALIDATION_KL
        ),
    }
    if not wide:
        summary["lr_results"] = screen_results
        summary["lr_winner"] = screen_winner
    summary["summary_sha256"] = canonical_hash(summary)
    atomic_json(root / "study-summary.json", summary)
    _write_status(
        output_root,
        {
            "status": "complete",
            "stage": "complete",
            "summary": str(root / "study-summary.json"),
            "summary_sha256": summary["summary_sha256"],
            "winner": summary["winner"]["label"],
        },
    )
    return summary


def status(output_root: str = DEFAULT_OUTPUT_ROOT) -> dict:
    root = Path(output_root)
    try:
        coordinator = json.loads(_status_path(output_root).read_text())
    except (FileNotFoundError, json.JSONDecodeError, OSError):
        coordinator = {"status": "not_started"}
    cells = []
    for stage in ("depth", "rank", "width", "lr", "mid", "final"):
        stage_root = root / stage
        if not stage_root.is_dir():
            continue
        for path in sorted(stage_root.glob("*/result.json")):
            try:
                value = json.loads(path.read_text())
            except (json.JSONDecodeError, OSError):
                continue
            cells.append(
                {
                    "stage": stage,
                    "label": value.get("label"),
                    "status": value.get("status"),
                    "examples_seen": value.get("examples_seen"),
                    "validation_kl": (value.get("validation") or {}).get("kl"),
                }
            )
    result = {"coordinator": coordinator, "cells": cells}
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)
    return result


def audit(output_root: str = DEFAULT_OUTPUT_ROOT) -> dict:
    root = Path(output_root)
    plan = json.loads((root / "study-plan.json").read_text())
    if (
        plan.get("schema") != PLAN_SCHEMA
        or plan.get("plan_sha256") != study_plan()["plan_sha256"]
    ):
        raise RuntimeError("study plan identity mismatch")
    summary = json.loads((root / "study-summary.json").read_text())
    digest = summary.get("summary_sha256")
    unsigned = dict(summary)
    unsigned.pop("summary_sha256", None)
    if digest != canonical_hash(unsigned):
        raise RuntimeError("study summary checksum mismatch")
    if summary.get("status") not in ("complete", "complete_early"):
        raise RuntimeError("study summary is not terminal")
    preflight = json.loads((root / "preflight.json").read_text())
    preflight_digest = preflight.get("preflight_sha256")
    unsigned_preflight = dict(preflight)
    unsigned_preflight.pop("preflight_sha256", None)
    if (
        preflight.get("schema") != PREFLIGHT_SCHEMA
        or preflight.get("accepted") is not True
        or preflight.get("plan_sha256") != plan["plan_sha256"]
        or preflight_digest != canonical_hash(unsigned_preflight)
    ):
        raise RuntimeError("preflight identity mismatch")

    embedded_results = []
    embedded_results.extend(summary.get("depth_winners", {}).values())
    embedded_results.extend(summary.get("rank_winners", {}).values())
    embedded_results.extend(summary.get("lr_results", []))
    embedded_results.extend(summary.get("screen_results", []))
    embedded_results.extend(summary.get("mid_results", []))
    for name in (
        "lr_winner",
        "screen_winner",
        "mid_winner",
        "final",
        "winner",
    ):
        if summary.get(name):
            embedded_results.append(summary[name])
    audited_labels = set()
    for embedded in embedded_results:
        label = embedded.get("label")
        if label in audited_labels:
            continue
        audited_labels.add(label)
        cell = cell_from_dict(embedded["cell"])
        stored = completed_result(output_root, cell)
        if (
            stored is None
            or stored.get("result_sha256") != embedded.get("result_sha256")
            or stored.get("plan_sha256") != plan["plan_sha256"]
        ):
            raise RuntimeError(f"result identity mismatch for {label}")
    result = {
        "status": "passed",
        "summary_sha256": digest,
        "winner": (summary.get("winner") or {}).get("label"),
        "audited_results": len(audited_labels),
    }
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)
    return result
