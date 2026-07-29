from __future__ import annotations

import argparse
import hashlib
import json
import math
import os
import time
from pathlib import Path

import numpy as np
import torch

from qwen_normuon_pretrain.config import (
    ADAMW_BETAS,
    AUDIT_EXAMPLES,
    AUX_ADAMW_LR,
    OPTIMIZER_EPS,
    WEIGHT_DECAY,
    Trial,
    microbatch_for,
    wsd_multiplier,
)
from qwen_normuon_pretrain.data import load_manifest, load_split
from qwen_normuon_pretrain.pretrain import (
    DEFAULT_DATA_ROOT,
    DEFAULT_OUTPUT_ROOT,
    SCREEN_SUMMARY_SCHEMA,
    atomic_json,
    canonical_hash,
    evaluate,
    factor_sha256,
    make_student,
    microbatch_chunks,
    next_token_cross_entropy_rows,
    tensor_sha256,
    training_order,
    trial_from_dict,
)

ADAMW_CONTROL_LR = AUX_ADAMW_LR
CONTROL_SCHEMA = "qwen-adamw-winner-matched-plan-v1"
CHECKPOINT_SCHEMA = "qwen-adamw-winner-matched-checkpoint-v1"
RESULT_SCHEMA = "qwen-adamw-winner-matched-result-v1"
COMPARISON_SCHEMA = "qwen-normuon-vs-adamw-comparison-v1"
PREFLIGHT_SCHEMA = "qwen-adamw-winner-matched-preflight-v1"
CONTROL_GROUP = "fwedu-normuon-vs-adamw-matched-d4r2407-v1"
WANDB_PROJECT = "qwen-normuon-next-token-pretrain"


def control_root(output_root: str) -> Path:
    return Path(output_root) / "adamw-control"


def load_reference(
    output_root: str,
) -> tuple[dict, dict, Trial]:
    root = Path(output_root)
    try:
        summary = json.loads((root / "screen-summary.json").read_text())
    except (FileNotFoundError, json.JSONDecodeError, OSError) as error:
        raise RuntimeError("NorMuon screen summary is unavailable") from error
    if (
        summary.get("schema") != SCREEN_SUMMARY_SCHEMA
        or summary.get("status") != "complete"
        or not summary.get("summary_sha256")
    ):
        raise RuntimeError("NorMuon screen summary is incomplete")
    ranking = summary.get("ranking", [])
    label = ranking[0].get("label") if ranking else None
    if not label:
        raise RuntimeError("NorMuon screen winner label is unavailable")
    try:
        result = json.loads(
            (root / "screen" / label / "result.json").read_text()
        )
    except (FileNotFoundError, json.JSONDecodeError, OSError) as error:
        raise RuntimeError(
            "NorMuon screen winner result is unavailable"
        ) from error
    if (
        result.get("status") != "complete"
        or result.get("label") != label
    ):
        raise RuntimeError("NorMuon screen winner result is incomplete")
    trial = trial_from_dict(result["trial"])
    if (
        trial.stage != "screen"
        or trial.steps != 128
        or trial.effective_batch != 2_048
        or trial.examples != 262_144
    ):
        raise RuntimeError("AdamW control requires the 128-step screen")
    return summary, result, trial


def build_control_plan(
    summary: dict,
    reference_result: dict,
    trial: Trial,
) -> dict:
    label = (
        f"adamw_control-{trial.architecture.label}"
        f"-flr3e-4-alr3e-4-b{trial.effective_batch}"
        f"-wsd-w{trial.warmup_steps}-c{trial.cooldown_steps}"
        f"-clip1-ce-erand-s{trial.seed}"
    )
    value = {
        "schema": CONTROL_SCHEMA,
        "status": "planned",
        "label": label,
        "objective": "next_token_cross_entropy_full_vocabulary",
        "optimizer": {
            "factor_optimizer": "fused_adamw",
            "factor_lr": ADAMW_CONTROL_LR,
            "factor_betas": list(ADAMW_BETAS),
            "auxiliary_optimizer": "fused_adamw",
            "auxiliary_lr": AUX_ADAMW_LR,
            "auxiliary_betas": list(ADAMW_BETAS),
            "epsilon": OPTIMIZER_EPS,
            "weight_decay": WEIGHT_DECAY,
            "schedule": "wsd_shared_multiplier",
        },
        "reference": {
            "optimizer": "normuon",
            "summary_sha256": summary["summary_sha256"],
            "label": reference_result["label"],
            "normuon_lr": trial.normuon_lr,
            "validation": reference_result["validation"],
            "test": {},
            "initial_hashes": reference_result["initial_hashes"],
        },
        "matched": {
            "architecture": trial.to_dict()["architecture"],
            "seed": trial.seed,
            "effective_batch": trial.effective_batch,
            "examples": trial.examples,
            "warmup_steps": trial.warmup_steps,
            "stable_steps": trial.stable_steps,
            "cooldown_steps": trial.cooldown_steps,
            "gradient_clip_norm": trial.gradient_clip_norm,
            "aux_adamw_lr": trial.aux_adamw_lr,
            "training_order": "identical_seeded_permutation",
            "dataset_manifest": reference_result["dataset_manifest"],
            "initializer_hashes": reference_result["initial_hashes"],
            "validation_protocol": "identical_full_validation",
            "test_protocol": "not_used_for_128_step_comparison",
        },
        "teacher_used": False,
        "teacher_cache_used": False,
    }
    value["plan_sha256"] = canonical_hash(value)
    return value


def load_control_plan(output_root: str) -> tuple[dict, dict, Trial, dict]:
    summary, result, trial = load_reference(output_root)
    return build_control_plan(summary, result, trial), summary, trial, result


def make_adamw_optimizers(student, *, device: str):
    factors = student.factor_parameters()
    auxiliary = student.auxiliary_parameters()
    factor_ids = {id(parameter) for parameter in factors}
    auxiliary_ids = {id(parameter) for parameter in auxiliary}
    if factor_ids & auxiliary_ids:
        raise RuntimeError("AdamW optimizer routes overlap")
    if factor_ids | auxiliary_ids != {
        id(parameter) for parameter in student.parameters()
    }:
        raise RuntimeError("AdamW optimizer routes do not cover the model")
    kwargs = {
        "lr": ADAMW_CONTROL_LR,
        "betas": ADAMW_BETAS,
        "eps": OPTIMIZER_EPS,
        "weight_decay": WEIGHT_DECAY,
        "fused": device.startswith("cuda"),
    }
    factor_optimizer = torch.optim.AdamW(factors, **kwargs)
    aux_optimizer = torch.optim.AdamW(auxiliary, **kwargs)
    metadata = {
        "factor_optimizer": "fused_adamw",
        "factor_tensors": len(factors),
        "factor_matrices": sum(
            math.prod(parameter.shape[:-2])
            for parameter in factors
        ),
        "factor_parameters": sum(
            parameter.numel() for parameter in factors
        ),
        "auxiliary_optimizer": "fused_adamw",
        "auxiliary_tensors": len(auxiliary),
        "auxiliary_parameters": sum(
            parameter.numel() for parameter in auxiliary
        ),
    }
    return factor_optimizer, aux_optimizer, metadata


def set_learning_rates(
    factor_optimizer: torch.optim.Optimizer,
    aux_optimizer: torch.optim.Optimizer,
    trial: Trial,
    step: int,
) -> tuple[float, float]:
    multiplier = wsd_multiplier(trial, step)
    lr = ADAMW_CONTROL_LR * multiplier
    for optimizer in (factor_optimizer, aux_optimizer):
        for group in optimizer.param_groups:
            group["lr"] = lr
    return multiplier, lr


def checkpoint_value(
    student,
    factor_optimizer,
    aux_optimizer,
    trial: Trial,
    plan: dict,
    *,
    step: int,
    examples_seen: int,
    initial_hashes: dict,
    initial_validation: dict,
    elapsed_wall_seconds: float,
) -> dict:
    value = {
        "schema": CHECKPOINT_SCHEMA,
        "plan_sha256": plan["plan_sha256"],
        "trial": trial.to_dict(),
        "state_dict": student.state_dict(),
        "factor_optimizer_state_dict": factor_optimizer.state_dict(),
        "aux_optimizer_state_dict": aux_optimizer.state_dict(),
        "optimizer_states_included": True,
        "step": step,
        "examples_seen": examples_seen,
        "initial_hashes": initial_hashes,
        "initial_validation": initial_validation,
        "elapsed_wall_seconds": elapsed_wall_seconds,
        "torch_rng_state": torch.get_rng_state(),
        "numpy_seed": trial.seed,
    }
    if torch.cuda.is_available():
        value["cuda_rng_state"] = torch.cuda.get_rng_state()
    return value


def save_checkpoint(path: Path, *args, **kwargs) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    torch.save(checkpoint_value(*args, **kwargs), temporary)
    os.replace(temporary, path)


def load_checkpoint(
    path: Path,
    student,
    factor_optimizer,
    aux_optimizer,
    trial: Trial,
    plan: dict,
    initial_hashes: dict,
) -> dict | None:
    if not path.is_file():
        return None
    checkpoint = torch.load(
        path,
        map_location="cpu",
        mmap=True,
        weights_only=True,
    )
    expected = {
        "schema": CHECKPOINT_SCHEMA,
        "plan_sha256": plan["plan_sha256"],
        "trial": trial.to_dict(),
        "initial_hashes": initial_hashes,
        "numpy_seed": trial.seed,
    }
    for key, value in expected.items():
        if checkpoint.get(key) != value:
            raise RuntimeError(f"AdamW checkpoint mismatched {key}")
    step = int(checkpoint.get("step", -1))
    examples_seen = int(checkpoint.get("examples_seen", -1))
    if not 0 < step < trial.steps:
        raise RuntimeError("AdamW checkpoint step is not resumable")
    if examples_seen != step * trial.effective_batch:
        raise RuntimeError("AdamW checkpoint data cursor is invalid")
    student.load_state_dict(checkpoint["state_dict"], strict=True)
    factor_optimizer.load_state_dict(
        checkpoint["factor_optimizer_state_dict"]
    )
    aux_optimizer.load_state_dict(
        checkpoint["aux_optimizer_state_dict"]
    )
    torch.set_rng_state(checkpoint["torch_rng_state"])
    if torch.cuda.is_available() and "cuda_rng_state" in checkpoint:
        torch.cuda.set_rng_state(checkpoint["cuda_rng_state"])
    return {
        "step": step,
        "examples_seen": examples_seen,
        "initial_validation": checkpoint["initial_validation"],
        "elapsed_wall_seconds":
            float(checkpoint["elapsed_wall_seconds"]),
    }


def comparison_value(
    plan: dict,
    reference_result: dict,
    adamw_result: dict,
) -> dict:
    normuon_validation = reference_result["validation"]
    adamw_validation = adamw_result["validation"]
    value = {
        "schema": COMPARISON_SCHEMA,
        "status": adamw_result["status"],
        "plan_sha256": plan["plan_sha256"],
        "matched_fields": plan["matched"],
        "rows": [
            {
                "optimizer": "normuon_factors_plus_adamw_aux",
                "factor_lr":
                    reference_result["trial"]["normuon_lr"],
                "effective_batch":
                    reference_result["trial"]["effective_batch"],
                "validation_cross_entropy":
                    normuon_validation["cross_entropy"],
                "validation_perplexity":
                    normuon_validation["perplexity"],
                "test_cross_entropy": None,
                "test_perplexity": None,
                "wall_seconds": reference_result["wall_seconds"],
                "wandb_url": reference_result["wandb_url"],
            },
            {
                "optimizer": "adamw_all_parameters",
                "factor_lr": ADAMW_CONTROL_LR,
                "effective_batch":
                    adamw_result["trial"]["effective_batch"],
                "validation_cross_entropy":
                    adamw_validation["cross_entropy"],
                "validation_perplexity":
                    adamw_validation["perplexity"],
                "test_cross_entropy": None,
                "test_perplexity": None,
                "wall_seconds": adamw_result["wall_seconds"],
                "wandb_url": adamw_result["wandb_url"],
            },
        ],
        "adamw_minus_normuon": {
            "validation_cross_entropy":
                adamw_validation["cross_entropy"]
                - normuon_validation["cross_entropy"],
            "validation_perplexity":
                adamw_validation["perplexity"]
                - normuon_validation["perplexity"],
            "test_cross_entropy": None,
            "test_perplexity": None,
            "wall_seconds":
                adamw_result["wall_seconds"]
                - reference_result["wall_seconds"],
        },
    }
    value["comparison_sha256"] = canonical_hash(value)
    return value


def init_wandb(
    plan: dict,
    trial: Trial,
    output: Path,
    manifest: dict,
    optimizer_metadata: dict,
    resume_step: int,
):
    import wandb

    if not os.environ.get("WANDB_API_KEY") and os.environ.get(
        "WANDB_MODE",
        "",
    ).lower() not in ("offline", "disabled"):
        raise RuntimeError("WANDB_API_KEY is required for online control")
    run_id = hashlib.sha256(
        f"{CONTROL_GROUP}:{plan['plan_sha256']}".encode()
    ).hexdigest()[:16]
    return wandb.init(
        project=WANDB_PROJECT,
        group=CONTROL_GROUP,
        name=plan["label"],
        job_type="next-token-adamw-control",
        id=run_id,
        resume="allow",
        dir=str(output / "wandb"),
        config={
            "schema": CONTROL_SCHEMA,
            "plan": plan,
            "trial": trial.to_dict(),
            "optimizer_routing": optimizer_metadata,
            "dataset_manifest": manifest,
            "resumed_from_step": resume_step,
        },
    )


def completed_result(output: Path, plan: dict, trial: Trial) -> dict | None:
    try:
        result = json.loads((output / "result.json").read_text())
    except (FileNotFoundError, json.JSONDecodeError, OSError):
        return None
    if (
        result.get("schema") != RESULT_SCHEMA
        or result.get("status") != "complete"
        or result.get("plan_sha256") != plan["plan_sha256"]
        or result.get("trial") != trial.to_dict()
        or int(result.get("steps_completed", -1)) != trial.steps
        or not (output / "student.pt").is_file()
    ):
        return None
    return result


def run_control(
    *,
    data_root: str = DEFAULT_DATA_ROOT,
    output_root: str = DEFAULT_OUTPUT_ROOT,
    device: str = "cuda",
) -> dict:
    import wandb

    plan, _, trial, reference_result = load_control_plan(output_root)
    output = control_root(output_root) / plan["label"]
    cached = completed_result(output, plan, trial)
    if cached is not None:
        print(json.dumps(cached, indent=2, sort_keys=True), flush=True)
        return cached
    output.mkdir(parents=True, exist_ok=True)
    (output / "wandb").mkdir(parents=True, exist_ok=True)
    atomic_json(control_root(output_root) / "plan.json", plan)

    torch.set_float32_matmul_precision("high")
    if device.startswith("cuda"):
        torch.backends.cuda.matmul.allow_tf32 = True
        torch.backends.cudnn.allow_tf32 = True
        torch.cuda.reset_peak_memory_stats()
    manifest = load_manifest(data_root)
    if manifest != plan["matched"]["dataset_manifest"]:
        raise RuntimeError("AdamW dataset manifest differs from reference")
    train_contexts, train_targets, _ = load_split(data_root, "train")
    val_contexts, val_targets, _ = load_split(data_root, "validation")
    student, initial_hashes = make_student(trial, device=device)
    if initial_hashes != plan["matched"]["initializer_hashes"]:
        raise RuntimeError("AdamW initializer differs from NorMuon winner")
    factor_optimizer, aux_optimizer, optimizer_metadata = (
        make_adamw_optimizers(student, device=device)
    )
    progress_path = output / "progress.pt"
    resume = load_checkpoint(
        progress_path,
        student,
        factor_optimizer,
        aux_optimizer,
        trial,
        plan,
        initial_hashes,
    )
    resume_step = int(resume["step"]) if resume else 0
    examples_seen = int(resume["examples_seen"]) if resume else 0
    prior_wall = (
        float(resume["elapsed_wall_seconds"]) if resume else 0.0
    )
    order = training_order(
        len(train_contexts),
        trial.examples,
        trial.seed,
    )
    microbatch, accumulation = microbatch_for(trial)
    run = init_wandb(
        plan,
        trial,
        output,
        manifest,
        optimizer_metadata,
        resume_step,
    )
    print(f"[wandb] {run.url}", flush=True)
    print(
        f"[start] {plan['label']} step={resume_step}/{trial.steps} "
        f"effective_batch={trial.effective_batch} "
        f"microbatch={microbatch} accumulation={accumulation}",
        flush=True,
    )
    current_validation = evaluate(
        student,
        val_contexts,
        val_targets,
        examples=AUDIT_EXAMPLES,
        batch_size=min(64, microbatch),
        device=device,
    )
    run.log({
        f"validation/{key}": value
        for key, value in current_validation.items()
    }, step=resume_step)
    initial_validation = (
        resume["initial_validation"] if resume else current_validation
    )
    cursor = examples_seen
    wall_start = time.perf_counter()
    step_times: list[float] = []
    completed_step = resume_step
    divergence_reason = None
    stable_end = trial.warmup_steps + trial.stable_steps

    try:
        for step in range(resume_step + 1, trial.steps + 1):
            step_start = time.perf_counter()
            schedule, adamw_lr = set_learning_rates(
                factor_optimizer,
                aux_optimizer,
                trial,
                step,
            )
            factor_optimizer.zero_grad(set_to_none=True)
            aux_optimizer.zero_grad(set_to_none=True)
            total_ce = 0.0
            total_correct = 0.0
            diagnostic = (
                step == resume_step + 1
                or step % trial.audit_every == 0
                or step in (stable_end, trial.steps)
            )
            for index, chunk_size in enumerate(microbatch_chunks(
                trial.effective_batch,
                microbatch,
            )):
                weight = chunk_size / trial.effective_batch
                student.collect_activation_diagnostics(
                    diagnostic and index == 0
                )
                batch_indices = order[cursor:cursor + chunk_size]
                cursor += chunk_size
                ids = torch.from_numpy(
                    np.array(
                        train_contexts[batch_indices],
                        copy=True,
                    )
                ).to(device=device, dtype=torch.long)
                targets = torch.from_numpy(
                    np.array(
                        train_targets[batch_indices],
                        copy=True,
                    )
                ).to(device=device, dtype=torch.long)
                with torch.autocast(
                    device_type="cuda",
                    dtype=torch.bfloat16,
                    enabled=device.startswith("cuda"),
                ):
                    logits = student(ids)
                student.collect_activation_diagnostics(False)
                rows = next_token_cross_entropy_rows(logits, targets)
                (rows.mean() * weight).backward()
                total_ce += float(rows.detach().mean()) * weight
                total_correct += (
                    float(
                        (logits.detach().argmax(-1) == targets)
                        .float()
                        .mean()
                    )
                    * weight
                )
                del ids, targets, logits, rows
            preclip = torch.nn.utils.clip_grad_norm_(
                student.parameters(),
                trial.gradient_clip_norm,
                error_if_nonfinite=False,
            )
            if not math.isfinite(total_ce):
                divergence_reason = f"nonfinite CE at step {step}"
            if not torch.isfinite(preclip):
                divergence_reason = (
                    f"nonfinite gradient norm at step {step}"
                )
            if divergence_reason is not None:
                run.log({
                    "status/diverged": 1,
                    "diagnostic/gradient_norm_preclip":
                        float(preclip),
                }, step=step)
                break
            factor_optimizer.step()
            aux_optimizer.step()
            if device.startswith("cuda"):
                torch.cuda.synchronize()
            elapsed = time.perf_counter() - step_start
            step_times.append(elapsed)
            completed_step = step
            examples_seen = cursor
            diagnostics = {
                "diagnostic/gradient_norm_preclip": float(preclip),
            }
            if diagnostic:
                diagnostics.update(student.activation_metrics())
                diagnostics.update(student.factor_metrics())
            log = {
                "train/cross_entropy": total_ce,
                "train/perplexity":
                    math.exp(min(total_ce, 30.0)),
                "train/target_accuracy": total_correct,
                "progress/examples_seen": examples_seen,
                "progress/input_tokens_seen":
                    examples_seen * trial.architecture.context_length,
                "progress/target_tokens_seen": examples_seen,
                "progress/optimizer_steps": step,
                "performance/step_seconds": elapsed,
                "performance/examples_per_second":
                    trial.effective_batch / elapsed,
                "optimizer/schedule_multiplier": schedule,
                "optimizer/factor_adamw_lr": adamw_lr,
                "optimizer/aux_adamw_lr": adamw_lr,
                "comparison/reference_normuon_lr":
                    trial.normuon_lr,
                **diagnostics,
            }
            if device.startswith("cuda"):
                log.update({
                    "performance/peak_allocated_gib":
                        torch.cuda.max_memory_allocated() / 2**30,
                    "performance/peak_reserved_gib":
                        torch.cuda.max_memory_reserved() / 2**30,
                })
            run.log(log, step=step)
            validate = (
                step % trial.audit_every == 0
                or step in (stable_end, trial.steps)
            )
            if validate:
                validation_examples = (
                    len(val_contexts)
                    if step in (stable_end, trial.steps)
                    else AUDIT_EXAMPLES
                )
                current_validation = evaluate(
                    student,
                    val_contexts,
                    val_targets,
                    examples=validation_examples,
                    batch_size=min(64, microbatch),
                    device=device,
                )
                run.log({
                    f"validation/{key}": value
                    for key, value in current_validation.items()
                }, step=step)
                print(
                    f"[{plan['label']}] step={step}/{trial.steps} "
                    f"train_ce={total_ce:.6f} "
                    f"val_ce={current_validation['cross_entropy']:.6f} "
                    f"examples_s={trial.effective_batch / elapsed:.2f}",
                    flush=True,
                )
            if (
                step < trial.steps
                and examples_seen % trial.checkpoint_every_examples == 0
            ):
                save_checkpoint(
                    progress_path,
                    student,
                    factor_optimizer,
                    aux_optimizer,
                    trial,
                    plan,
                    step=step,
                    examples_seen=examples_seen,
                    initial_hashes=initial_hashes,
                    initial_validation=initial_validation,
                    elapsed_wall_seconds=(
                        prior_wall
                        + time.perf_counter()
                        - wall_start
                    ),
                )

        status = "diverged" if divergence_reason else "complete"
        final_validation = evaluate(
            student,
            val_contexts,
            val_targets,
            examples=len(val_contexts),
            batch_size=min(64, microbatch),
            device=device,
        )
        test = {}
        final_hashes = {
            "embedding": tensor_sha256(student.tied_embedding),
            "factors": factor_sha256(student),
        }
        result = {
            "schema": RESULT_SCHEMA,
            "status": status,
            "divergence_reason": divergence_reason,
            "label": plan["label"],
            "plan_sha256": plan["plan_sha256"],
            "trial": trial.to_dict(),
            "reference_normuon_label": reference_result["label"],
            "steps_completed": completed_step,
            "examples_seen": examples_seen,
            "objective": "next_token_cross_entropy_full_vocabulary",
            "teacher_used": False,
            "teacher_cache_used": False,
            "initial_hashes": initial_hashes,
            "final_hashes": final_hashes,
            "embedding_changed":
                final_hashes["embedding"]
                != initial_hashes["embedding"],
            "factors_changed":
                final_hashes["factors"]
                != initial_hashes["factors"],
            "optimizer_routing": optimizer_metadata,
            "validation": final_validation,
            "test": test,
            "microbatch": microbatch,
            "gradient_accumulation": accumulation,
            "wall_seconds": (
                prior_wall + time.perf_counter() - wall_start
            ),
            "median_step_seconds": (
                float(np.median(step_times))
                if step_times
                else math.nan
            ),
            "wandb_run_id": run.id,
            "wandb_url": run.url,
            "dataset_manifest": manifest,
        }
        atomic_json(output / "result.json", result)
        if status == "complete":
            save_checkpoint(
                output / "student.pt",
                student,
                factor_optimizer,
                aux_optimizer,
                trial,
                plan,
                step=completed_step,
                examples_seen=examples_seen,
                initial_hashes=initial_hashes,
                initial_validation=initial_validation,
                elapsed_wall_seconds=result["wall_seconds"],
            )
            progress_path.unlink(missing_ok=True)
            weights_path = output / "weights.pt"
            temporary = weights_path.with_suffix(".pt.tmp")
            torch.save({
                "schema": "qwen-adamw-winner-matched-weights-v1",
                "state_dict": student.state_dict(),
                "plan_sha256": plan["plan_sha256"],
                "initial_hashes": initial_hashes,
                "final_hashes": final_hashes,
                "validation": final_validation,
                "test": test,
            }, temporary)
            os.replace(temporary, weights_path)
            comparison = comparison_value(
                plan,
                reference_result,
                result,
            )
            comparison_path = control_root(output_root) / "comparison.json"
            atomic_json(comparison_path, comparison)
            table = wandb.Table(
                columns=[
                    "optimizer",
                    "factor_lr",
                    "effective_batch",
                    "validation_cross_entropy",
                    "test_cross_entropy",
                    "wall_seconds",
                ],
                data=[
                    [
                        row["optimizer"],
                        row["factor_lr"],
                        row["effective_batch"],
                        row["validation_cross_entropy"],
                        row["test_cross_entropy"],
                        row["wall_seconds"],
                    ]
                    for row in comparison["rows"]
                ],
            )
            run.log({
                "comparison/table": table,
                "comparison/validation_ce_adamw_minus_normuon":
                    comparison["adamw_minus_normuon"][
                        "validation_cross_entropy"
                    ],
            }, step=completed_step)
            artifact = wandb.Artifact(
                name="qwen-adamw-winner-matched-control",
                type="model",
                metadata={
                    "plan_sha256": plan["plan_sha256"],
                    "comparison_sha256":
                        comparison["comparison_sha256"],
                },
            )
            artifact.add_file(str(weights_path))
            artifact.add_file(str(comparison_path))
            run.log_artifact(artifact)
            result["comparison"] = comparison
            atomic_json(output / "result.json", result)
        run.summary.update({
            "status": status,
            "validation_cross_entropy":
                final_validation["cross_entropy"],
            "test_cross_entropy":
                test.get("cross_entropy"),
            "reference_normuon_label": reference_result["label"],
            "embedding_changed": result["embedding_changed"],
            "factors_changed": result["factors_changed"],
        })
        run.finish(exit_code=0 if status == "complete" else 1)
        return result
    except BaseException:
        if wandb.run is not None:
            wandb.finish(exit_code=1)
        raise


def preflight(
    *,
    data_root: str = DEFAULT_DATA_ROOT,
    output_root: str = DEFAULT_OUTPUT_ROOT,
    device: str = "cuda",
) -> dict:
    plan, _, trial, _ = load_control_plan(output_root)
    manifest = load_manifest(data_root)
    contexts, targets, _ = load_split(data_root, "train")
    microbatch, _ = microbatch_for(trial)
    torch.set_float32_matmul_precision("high")
    torch.backends.cuda.matmul.allow_tf32 = True
    torch.cuda.reset_peak_memory_stats()
    student, initial_hashes = make_student(trial, device=device)
    if initial_hashes != plan["matched"]["initializer_hashes"]:
        raise RuntimeError("AdamW preflight initializer mismatch")
    factor_optimizer, aux_optimizer, metadata = make_adamw_optimizers(
        student,
        device=device,
    )
    chunks = microbatch_chunks(trial.effective_batch, microbatch)
    cursor = 0
    loss_value = 0.0
    torch.cuda.synchronize()
    started = time.perf_counter()
    for chunk_size in chunks:
        ids = torch.from_numpy(
            np.array(contexts[cursor:cursor + chunk_size], copy=True)
        ).to(device=device, dtype=torch.long)
        target = torch.from_numpy(
            np.array(targets[cursor:cursor + chunk_size], copy=True)
        ).to(device=device, dtype=torch.long)
        cursor += chunk_size
        with torch.autocast(
            device_type="cuda",
            dtype=torch.bfloat16,
        ):
            logits = student(ids)
        rows = next_token_cross_entropy_rows(logits, target)
        weight = chunk_size / trial.effective_batch
        (rows.mean() * weight).backward()
        loss_value += float(rows.detach().mean()) * weight
        del ids, target, logits, rows
    factor_gradient_nonzero = any(
        parameter.grad is not None
        and bool(torch.count_nonzero(parameter.grad))
        for parameter in student.factor_parameters()
    )
    embedding_gradient_nonzero = (
        student.tied_embedding.grad is not None
        and bool(torch.count_nonzero(student.tied_embedding.grad))
    )
    preclip = torch.nn.utils.clip_grad_norm_(
        student.parameters(),
        trial.gradient_clip_norm,
    )
    set_learning_rates(factor_optimizer, aux_optimizer, trial, 1)
    factor_optimizer.step()
    aux_optimizer.step()
    torch.cuda.synchronize()
    properties = torch.cuda.get_device_properties(
        torch.cuda.current_device()
    )
    allocated = torch.cuda.max_memory_allocated()
    reserved = torch.cuda.max_memory_reserved()
    result = {
        "schema": PREFLIGHT_SCHEMA,
        "status": "complete",
        "plan_sha256": plan["plan_sha256"],
        "microbatch": microbatch,
        "accumulation": len(chunks),
        "loss": loss_value,
        "gradient_norm_preclip": float(preclip),
        "factor_gradient_nonzero": factor_gradient_nonzero,
        "embedding_gradient_nonzero": embedding_gradient_nonzero,
        "step_seconds": time.perf_counter() - started,
        "peak_allocated_gib": allocated / 2**30,
        "peak_reserved_gib": reserved / 2**30,
        "memory_ratio":
            max(allocated, reserved) / properties.total_memory,
        "device": properties.name,
        "optimizer_routing": metadata,
        "dataset_manifest": manifest,
        "teacher_used": False,
    }
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)
    return result


def status(output_root: str = DEFAULT_OUTPUT_ROOT) -> dict:
    root = control_root(output_root)
    try:
        coordinator = json.loads(
            (root / "coordinator-status.json").read_text()
        )
    except (FileNotFoundError, json.JSONDecodeError, OSError):
        coordinator = {"status": "not_queued"}
    try:
        plan = json.loads((root / "plan.json").read_text())
    except (FileNotFoundError, json.JSONDecodeError, OSError):
        plan = None
    try:
        comparison = json.loads((root / "comparison.json").read_text())
    except (FileNotFoundError, json.JSONDecodeError, OSError):
        comparison = None
    value = {
        "coordinator": coordinator,
        "plan": plan,
        "comparison": comparison,
    }
    print(json.dumps(value, indent=2, sort_keys=True), flush=True)
    return value


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "command",
        choices=("plan", "run", "preflight", "status"),
    )
    parser.add_argument("--data-root", default=DEFAULT_DATA_ROOT)
    parser.add_argument("--output-root", default=DEFAULT_OUTPUT_ROOT)
    parser.add_argument("--result-path", default="")
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    if args.command == "status":
        status(args.output_root)
        return
    if args.command == "plan":
        plan, _, _, _ = load_control_plan(args.output_root)
        print(json.dumps(plan, indent=2, sort_keys=True))
        return
    if args.command == "preflight":
        result = preflight(
            data_root=args.data_root,
            output_root=args.output_root,
        )
        if args.result_path:
            atomic_json(Path(args.result_path), result)
        return
    run_control(
        data_root=args.data_root,
        output_root=args.output_root,
    )


if __name__ == "__main__":
    main()
