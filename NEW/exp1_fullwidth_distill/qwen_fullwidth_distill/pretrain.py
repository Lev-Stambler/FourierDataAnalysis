from __future__ import annotations

import argparse
import hashlib
import json
import math
import os
import time
from dataclasses import replace
from pathlib import Path
from typing import Callable

import numpy as np
import torch
import torch.nn.functional as F

from .config import (
    AUDIT_EXAMPLES,
    EMBEDDING_WIDTH,
    FINEWEB_EDU_CONFIG,
    FINEWEB_EDU_ID,
    FINEWEB_EDU_REVISION,
    MODEL_ID,
    MODEL_REVISION,
    TEST_EXAMPLES,
    TRAIN_EXAMPLES,
    ArchitectureConfig,
    TrialConfig,
    microbatch_for,
)
from .data import load_manifest, load_split, prepare_dataset
from .model import (
    FullWidthStudent,
    optimizer_adam_role_diagnostics,
    optimizer_parameter_groups,
    optimizer_role_diagnostics,
)
from .train import (
    ADAMW,
    _microbatch_chunks,
    _training_order,
    lr_schedule_multiplier,
    stable_checkpoint_step,
    tensor_sha256,
)


SCHEMA = "qwen-fullwidth-next-token-study-v1"
CHECKPOINT_SCHEMA = "qwen-fullwidth-next-token-checkpoint-v1"
RESULT_SCHEMA = "qwen-fullwidth-next-token-result-v1"
SUMMARY_SCHEMA = "qwen-fullwidth-next-token-summary-v1"
WANDB_PROJECT = "qwen-fullwidth-next-token-pretrain"
WANDB_GROUP = "fwedu-random-tied-d4r2407-4m-v1"
DEFAULT_DATA_ROOT = (
    "/cache/qwen_fullwidth_distill/"
    "context16-fineweb-edu-next-token-4m-v1"
)
DEFAULT_OUTPUT_ROOT = (
    "/cache/qwen_fullwidth_distill/next-token-pretrain-v1"
)
INITIALIZER_RANGE = 0.02
VOCAB_SIZE = 248_320
CHECKPOINT_EVERY_EXAMPLES = 524_288
LR_GRID = (5e-6, 1e-5, 1.5e-5, 2e-5)
BATCH_VARIANTS = {
    2_048: (128, 1_728, 192),
    4_096: (64, 864, 96),
}
EXPECTED_TRAINABLE_PARAMETERS = 338_552_032


def pretrain_architecture() -> ArchitectureConfig:
    return ArchitectureConfig(
        "kronecker",
        "residual_ffn",
        4,
        4,
        kronecker_rank=2_407,
        kronecker_rank_chunk=32,
    )


def pretrain_trials() -> list[TrialConfig]:
    architecture = pretrain_architecture()
    trials = []
    for effective_batch, phases in BATCH_VARIANTS.items():
        warmup_steps, stable_steps, cooldown_steps = phases
        steps = warmup_steps + stable_steps + cooldown_steps
        if steps * effective_batch != TRAIN_EXAMPLES:
            raise RuntimeError("pretraining phases do not cover the exact budget")
        for lr in LR_GRID:
            trials.append(TrialConfig(
                architecture,
                lr=lr,
                seed=0,
                steps=steps,
                stage="next_token_pretrain_4m",
                audit_every=CHECKPOINT_EVERY_EXAMPLES // effective_batch,
                effective_batch=effective_batch,
                lr_parameterization="mup",
                allow_divergence=True,
                max_activation_rms_growth=10.0,
                checkpoint_every_examples=CHECKPOINT_EVERY_EXAMPLES,
                allow_data_reuse=False,
                lr_schedule="wsd",
                warmup_steps=warmup_steps,
                cooldown_steps=cooldown_steps,
                min_lr_ratio=0.0,
                gradient_clip_norm=1.0,
                dataset_tag="fwedu350bt-nexttoken-4m-v1",
                objective="next_token_ce",
                embedding_initialization="trainable_random",
            ))
    if len(trials) != 8 or len({trial.label for trial in trials}) != 8:
        raise RuntimeError("next-token study must contain eight unique cells")
    return trials


def study_plan() -> dict:
    trials = pretrain_trials()
    value = {
        "schema": SCHEMA,
        "status": "planned",
        "model_id": MODEL_ID,
        "model_revision": MODEL_REVISION,
        "tokenizer_only": True,
        "teacher_used": False,
        "teacher_cache_used": False,
        "objective": "next_token_ce",
        "embedding_initialization": {
            "kind": "trainable_random",
            "distribution": "normal",
            "mean": 0.0,
            "std": INITIALIZER_RANGE,
            "tied_input_output": True,
        },
        "train_examples_per_cell": TRAIN_EXAMPLES,
        "input_tokens_per_cell":
            TRAIN_EXAMPLES * pretrain_architecture().context_length,
        "target_tokens_per_cell": TRAIN_EXAMPLES,
        "cells": [trial.to_dict() | {"label": trial.label} for trial in trials],
    }
    value["plan_sha256"] = _canonical_hash(value)
    return value


def _canonical_hash(value: dict) -> str:
    payload = json.dumps(
        value,
        sort_keys=True,
        separators=(",", ":"),
        allow_nan=False,
    ).encode("utf-8")
    return hashlib.sha256(payload).hexdigest()


def _atomic_json(path: Path, value: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_text(json.dumps(value, indent=2, sort_keys=True))
    os.replace(temporary, path)


def load_tokenizer():
    from transformers import AutoTokenizer

    token = (
        os.environ.get("HF_TOKEN")
        or os.environ.get("HF_HUB_TOKEN")
        or os.environ.get("HUGGING_FACE_HUB_TOKEN")
    )
    return AutoTokenizer.from_pretrained(
        MODEL_ID,
        revision=MODEL_REVISION,
        token=token,
    )


def prepare_pretrain_data(data_root: str = DEFAULT_DATA_ROOT) -> dict:
    tokenizer = load_tokenizer()
    return prepare_dataset(
        data_root,
        tokenizer,
        dataset_id=FINEWEB_EDU_ID,
        dataset_config=FINEWEB_EDU_CONFIG,
        dataset_revision=FINEWEB_EDU_REVISION,
    )


def random_trainable_student(
    trial: TrialConfig,
    *,
    vocab_size: int,
    device: str,
) -> tuple[FullWidthStudent, str]:
    if trial.objective != "next_token_ce":
        raise ValueError("pretraining requires objective='next_token_ce'")
    if trial.embedding_initialization != "trainable_random":
        raise ValueError("pretraining requires trainable_random embeddings")
    torch.manual_seed(trial.seed)
    np.random.seed(trial.seed)
    embedding = torch.empty(
        vocab_size,
        trial.architecture.embedding_width,
        dtype=torch.bfloat16,
        device="cpu",
    )
    embedding.normal_(mean=0.0, std=INITIALIZER_RANGE)
    initial_hash = tensor_sha256(embedding)
    student = FullWidthStudent(
        trial.architecture,
        embedding,
        vocab_size=vocab_size,
        trainable_embedding=True,
    ).to(device)
    if student.tied_embedding.data_ptr() != next(
        parameter.data_ptr()
        for name, parameter in student.named_parameters()
        if name == "tied_embedding"
    ):
        raise RuntimeError("input/output tied matrix is not the registered parameter")
    if student.trainable_parameter_count() != EXPECTED_TRAINABLE_PARAMETERS:
        raise RuntimeError(
            "unexpected trainable parameter count: "
            f"{student.trainable_parameter_count()} "
            f"!= {EXPECTED_TRAINABLE_PARAMETERS}"
        )
    return student, initial_hash


def next_token_cross_entropy_rows(
    logits: torch.Tensor,
    target: torch.Tensor,
) -> torch.Tensor:
    if logits.ndim != 2 or target.ndim != 1:
        raise ValueError("next-token logits/targets must be rank two/one")
    if len(logits) != len(target):
        raise ValueError("next-token logits/targets batch mismatch")
    return F.cross_entropy(logits.float(), target, reduction="none")


@torch.inference_mode()
def evaluate_next_token(
    student: FullWidthStudent,
    contexts: np.ndarray,
    targets: np.ndarray,
    *,
    examples: int,
    batch_size: int,
    device: str,
) -> dict[str, float]:
    student.eval()
    total_loss = 0.0
    total_correct = 0
    total = min(int(examples), len(contexts))
    for lo in range(0, total, batch_size):
        hi = min(lo + batch_size, total)
        ids = torch.from_numpy(np.array(contexts[lo:hi], copy=True)).to(
            device=device,
            dtype=torch.long,
        )
        target = torch.from_numpy(np.array(targets[lo:hi], copy=True)).to(
            device=device,
            dtype=torch.long,
        )
        with torch.autocast(device_type="cuda", dtype=torch.bfloat16):
            logits = student(ids)
        rows = next_token_cross_entropy_rows(logits, target)
        total_loss += float(rows.sum())
        total_correct += int((logits.argmax(-1) == target).sum())
    student.train()
    cross_entropy = total_loss / total
    return {
        "cross_entropy": cross_entropy,
        "perplexity": math.exp(min(cross_entropy, 30.0)),
        "target_accuracy": total_correct / total,
        "examples": total,
    }


def _checkpoint_value(
    student: FullWidthStudent,
    optimizer: torch.optim.Optimizer,
    trial: TrialConfig,
    *,
    step: int,
    examples_seen: int,
    initial_embedding_sha256: str,
    initial_validation: dict,
    elapsed_wall_seconds: float,
    stable_validation: dict | None,
) -> dict:
    return {
        "schema": CHECKPOINT_SCHEMA,
        "state_dict": student.state_dict(),
        "optimizer_state_dict": optimizer.state_dict(),
        "optimizer_state_included": True,
        "trial": trial.to_dict(),
        "model_id": MODEL_ID,
        "model_revision": MODEL_REVISION,
        "vocab_size": student.vocab_size,
        "initializer_range": INITIALIZER_RANGE,
        "initial_embedding_sha256": initial_embedding_sha256,
        "step": step,
        "examples_seen": examples_seen,
        "input_tokens_seen":
            examples_seen * trial.architecture.context_length,
        "target_tokens_seen": examples_seen,
        "optimizer_steps": step,
        "initial_validation": initial_validation,
        "stable_validation": stable_validation,
        "elapsed_wall_seconds": elapsed_wall_seconds,
    }


def save_pretrain_checkpoint(
    path: Path,
    student: FullWidthStudent,
    optimizer: torch.optim.Optimizer,
    trial: TrialConfig,
    **metadata,
) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    torch.save(
        _checkpoint_value(student, optimizer, trial, **metadata),
        temporary,
    )
    os.replace(temporary, path)


def load_pretrain_checkpoint(
    path: Path,
    student: FullWidthStudent,
    optimizer: torch.optim.Optimizer,
    trial: TrialConfig,
    initial_embedding_sha256: str,
) -> dict | None:
    if not path.is_file():
        return None
    checkpoint = torch.load(
        path,
        map_location="cpu",
        mmap=True,
        weights_only=True,
    )
    checkpoint_trial = dict(checkpoint.get("trial", {}))
    expected_trial = trial.to_dict()
    checkpoint_trial.pop("checkpoint_every_examples", None)
    expected_trial.pop("checkpoint_every_examples", None)
    if checkpoint_trial != expected_trial:
        raise RuntimeError(f"{trial.label}: progress checkpoint mismatched trial")
    expected = {
        "schema": CHECKPOINT_SCHEMA,
        "model_id": MODEL_ID,
        "model_revision": MODEL_REVISION,
        "vocab_size": student.vocab_size,
        "initial_embedding_sha256": initial_embedding_sha256,
    }
    for key, value in expected.items():
        if checkpoint.get(key) != value:
            raise RuntimeError(f"{trial.label}: checkpoint mismatched {key}")
    step = int(checkpoint.get("step", -1))
    if not 0 < step < trial.steps:
        raise RuntimeError(f"{trial.label}: invalid checkpoint step {step}")
    examples_seen = int(checkpoint.get("examples_seen", -1))
    if examples_seen != step * trial.effective_batch:
        raise RuntimeError(f"{trial.label}: invalid checkpoint data cursor")
    student.load_state_dict(checkpoint["state_dict"], strict=True)
    optimizer.load_state_dict(checkpoint["optimizer_state_dict"])
    return {
        "step": step,
        "examples_seen": examples_seen,
        "initial_validation": checkpoint["initial_validation"],
        "stable_validation": checkpoint.get("stable_validation"),
        "elapsed_wall_seconds": float(checkpoint["elapsed_wall_seconds"]),
    }


def _completed_result(output: Path, trial: TrialConfig) -> dict | None:
    try:
        result = json.loads((output / "result.json").read_text())
    except (FileNotFoundError, json.JSONDecodeError, OSError):
        return None
    if (
        result.get("schema") != RESULT_SCHEMA
        or result.get("status") != "complete"
        or result.get("label") != trial.label
        or result.get("trial") != trial.to_dict()
        or int(result.get("steps_completed", -1)) != trial.steps
        or not math.isfinite(float(
            result.get("validation", {}).get("cross_entropy", math.nan)
        ))
    ):
        return None
    checkpoint = output / "student.pt"
    if not checkpoint.is_file() or checkpoint.stat().st_size <= 0:
        return None
    return result


def _wandb_run_id(trial: TrialConfig) -> str:
    return hashlib.sha256(trial.label.encode()).hexdigest()[:16]


def _wandb_init(
    trial: TrialConfig,
    *,
    dataset_manifest: dict,
    initial_embedding_sha256: str,
    optimizer_groups: list[dict],
    output: Path,
    resumed_from_step: int,
):
    import wandb

    if not os.environ.get("WANDB_API_KEY") and os.environ.get(
        "WANDB_MODE", ""
    ).lower() not in ("offline", "disabled"):
        raise RuntimeError("WANDB_API_KEY is required for online pretraining")
    return wandb.init(
        project=WANDB_PROJECT,
        group=WANDB_GROUP,
        name=trial.label,
        job_type="next-token-pretrain",
        id=_wandb_run_id(trial),
        resume="allow",
        dir=str(output / "wandb"),
        config={
            "schema": SCHEMA,
            "trial": trial.to_dict(),
            "model_id": MODEL_ID,
            "model_revision": MODEL_REVISION,
            "objective": "next_token_cross_entropy_full_vocabulary",
            "teacher_used": False,
            "teacher_cache_used": False,
            "embedding_initialization": "trainable_random",
            "embedding_initializer_std": INITIALIZER_RANGE,
            "initial_embedding_sha256": initial_embedding_sha256,
            "trainable_parameters": EXPECTED_TRAINABLE_PARAMETERS,
            "dataset_manifest": dataset_manifest,
            "optimizer_groups": optimizer_groups,
            "resumed_from_step": resumed_from_step,
            "wandb_group": WANDB_GROUP,
        },
    )


def run_pretrain_trial(
    trial: TrialConfig,
    *,
    data_root: str = DEFAULT_DATA_ROOT,
    output_root: str = DEFAULT_OUTPUT_ROOT,
    device: str = "cuda",
    checkpoint_callback: Callable[[], None] | None = None,
) -> dict:
    import wandb

    if trial not in pretrain_trials():
        raise ValueError("run_pretrain_trial accepts only an exact study cell")
    trial.architecture.validate()
    lr_schedule_multiplier(trial, trial.steps)
    output = Path(output_root) / trial.label
    cached = _completed_result(output, trial)
    if cached is not None:
        print(f"[cache] {trial.label}", flush=True)
        return cached
    output.mkdir(parents=True, exist_ok=True)
    (output / "wandb").mkdir(parents=True, exist_ok=True)

    torch.set_float32_matmul_precision("high")
    torch.backends.cuda.matmul.allow_tf32 = True
    torch.backends.cudnn.allow_tf32 = True
    torch.cuda.reset_peak_memory_stats()

    manifest = load_manifest(data_root)
    train_contexts, train_targets, _ = load_split(data_root, "train")
    val_contexts, val_targets, _ = load_split(data_root, "validation")
    if len(train_contexts) != TRAIN_EXAMPLES:
        raise RuntimeError("pretraining dataset has the wrong train size")
    tokenizer = load_tokenizer()
    if len(tokenizer) > VOCAB_SIZE:
        raise RuntimeError(
            f"tokenizer vocabulary {len(tokenizer)} exceeds model "
            f"vocabulary {VOCAB_SIZE}"
        )
    student, initial_embedding_sha256 = random_trainable_student(
        trial,
        vocab_size=VOCAB_SIZE,
        device=device,
    )
    parameter_groups, group_metadata = optimizer_parameter_groups(
        student,
        trial.lr,
        trial.lr_parameterization,
        role_lr_multipliers=dict(trial.optimizer_role_lr_multipliers),
        role_weight_decay_overrides=dict(
            trial.optimizer_role_weight_decays
        ),
        default_weight_decay=ADAMW["weight_decay"],
    )
    optimizer = torch.optim.AdamW(parameter_groups, **ADAMW)
    initial_group_lrs = [float(group["lr"]) for group in optimizer.param_groups]
    progress_path = output / "progress.pt"
    resume = load_pretrain_checkpoint(
        progress_path,
        student,
        optimizer,
        trial,
        initial_embedding_sha256,
    )
    resume_step = int(resume["step"]) if resume else 0
    examples_seen = int(resume["examples_seen"]) if resume else 0
    prior_wall = float(resume["elapsed_wall_seconds"]) if resume else 0.0
    stable_validation = resume.get("stable_validation") if resume else None
    required = examples_seen + (
        trial.steps - resume_step
    ) * trial.effective_batch
    order = _training_order(
        len(train_contexts),
        required,
        trial.seed,
        trial.allow_data_reuse,
    )
    microbatch, accumulation = microbatch_for(
        trial.architecture,
        trial.effective_batch,
    )
    run = _wandb_init(
        trial,
        dataset_manifest=manifest,
        initial_embedding_sha256=initial_embedding_sha256,
        optimizer_groups=group_metadata,
        output=output,
        resumed_from_step=resume_step,
    )
    print(f"[wandb] {run.url}", flush=True)
    print(
        f"[start] {trial.label} step={resume_step}/{trial.steps} "
        f"effective_batch={trial.effective_batch} "
        f"microbatch={microbatch} accumulation={accumulation}",
        flush=True,
    )
    current_validation = evaluate_next_token(
        student,
        val_contexts,
        val_targets,
        examples=AUDIT_EXAMPLES,
        batch_size=min(64, microbatch),
        device=device,
    )
    run.log(
        {f"validation/{key}": value for key, value in current_validation.items()},
        step=resume_step,
    )
    initial_validation = (
        resume["initial_validation"] if resume else current_validation
    )
    cursor = examples_seen
    stable_step = stable_checkpoint_step(trial)
    wall_start = time.perf_counter()
    step_times: list[float] = []
    completed_step = resume_step
    divergence_reason = None
    try:
        for step in range(resume_step + 1, trial.steps + 1):
            step_start = time.perf_counter()
            lr_multiplier = lr_schedule_multiplier(trial, step)
            actual_lrs = []
            for group, base_lr in zip(
                optimizer.param_groups,
                initial_group_lrs,
                strict=True,
            ):
                group["lr"] = base_lr * lr_multiplier
                actual_lrs.append(float(group["lr"]))
            optimizer.zero_grad(set_to_none=True)
            total_ce = 0.0
            total_correct = 0.0
            diagnostic_step = (
                step == resume_step + 1
                or step % trial.audit_every == 0
                or step in (stable_step, trial.steps)
            )
            student.collect_activation_diagnostics(False)
            chunks = _microbatch_chunks(
                trial.effective_batch,
                microbatch,
            )
            for microbatch_index, chunk_size in enumerate(chunks):
                weight = chunk_size / trial.effective_batch
                student.collect_activation_diagnostics(
                    diagnostic_step and microbatch_index == 0
                )
                indices = order[cursor:cursor + chunk_size]
                cursor += chunk_size
                ids = torch.from_numpy(
                    np.array(train_contexts[indices], copy=True)
                ).to(device=device, dtype=torch.long)
                target = torch.from_numpy(
                    np.array(train_targets[indices], copy=True)
                ).to(device=device, dtype=torch.long)
                with torch.autocast(device_type="cuda", dtype=torch.bfloat16):
                    logits = student(ids)
                student.collect_activation_diagnostics(False)
                rows = next_token_cross_entropy_rows(logits, target)
                (rows.mean() * weight).backward()
                total_ce += float(rows.detach().mean()) * weight
                total_correct += (
                    float((logits.detach().argmax(-1) == target).float().mean())
                    * weight
                )
                del ids, target, logits, rows
            diagnostics = (
                optimizer_role_diagnostics(
                    optimizer.param_groups,
                    group_metadata,
                )
                | student.activation_metrics()
                | student.kronecker_metrics()
                if diagnostic_step
                else {}
            )
            activation_rms = diagnostics.get(
                "diagnostic/activation_rms_max"
            )
            if activation_rms is not None and not math.isfinite(
                float(activation_rms)
            ):
                divergence_reason = f"nonfinite activation RMS at step {step}"
            preclip = torch.nn.utils.clip_grad_norm_(
                student.parameters(),
                max_norm=trial.gradient_clip_norm,
                error_if_nonfinite=False,
            )
            diagnostics["diagnostic/gradient_norm_preclip"] = float(preclip)
            if not torch.isfinite(preclip):
                divergence_reason = f"nonfinite gradient norm at step {step}"
            if divergence_reason is not None:
                run.log(
                    {"status/diverged": 1, **diagnostics},
                    step=step,
                )
                break
            optimizer.step()
            if diagnostic_step:
                diagnostics.update(optimizer_adam_role_diagnostics(
                    optimizer,
                    group_metadata,
                ))
            torch.cuda.synchronize()
            elapsed = time.perf_counter() - step_start
            step_times.append(elapsed)
            completed_step = step
            examples_seen = cursor
            log = {
                "train/cross_entropy": total_ce,
                "train/perplexity": math.exp(min(total_ce, 30.0)),
                "train/target_accuracy": total_correct,
                "progress/examples_seen": examples_seen,
                "progress/input_tokens_seen":
                    examples_seen * trial.architecture.context_length,
                "progress/target_tokens_seen": examples_seen,
                "progress/optimizer_steps": step,
                "performance/step_seconds": elapsed,
                "performance/examples_per_second":
                    trial.effective_batch / elapsed,
                "performance/peak_allocated_gib":
                    torch.cuda.max_memory_allocated() / 2**30,
                "performance/peak_reserved_gib":
                    torch.cuda.max_memory_reserved() / 2**30,
                "optimizer/base_lr": trial.lr,
                "optimizer/lr": max(actual_lrs),
                "optimizer/lr_min_group": min(actual_lrs),
                "optimizer/lr_schedule_multiplier": lr_multiplier,
                **diagnostics,
            }
            run.log(log, step=step)
            validate = (
                step % trial.audit_every == 0
                or step in (stable_step, trial.steps)
            )
            if validate:
                validation_examples = (
                    len(val_contexts)
                    if step in (stable_step, trial.steps)
                    else AUDIT_EXAMPLES
                )
                current_validation = evaluate_next_token(
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
                    f"[{trial.label}] step={step}/{trial.steps} "
                    f"train_ce={total_ce:.6f} "
                    f"val_ce={current_validation['cross_entropy']:.6f} "
                    f"examples_s={trial.effective_batch / elapsed:.2f}",
                    flush=True,
                )
            checkpoint_metadata = {
                "step": step,
                "examples_seen": examples_seen,
                "initial_embedding_sha256": initial_embedding_sha256,
                "initial_validation": initial_validation,
                "elapsed_wall_seconds":
                    prior_wall + time.perf_counter() - wall_start,
                "stable_validation": stable_validation,
            }
            if step == stable_step:
                stable_validation = dict(current_validation)
                checkpoint_metadata["stable_validation"] = stable_validation
                save_pretrain_checkpoint(
                    output / "stable.pt",
                    student,
                    optimizer,
                    trial,
                    **checkpoint_metadata,
                )
                if checkpoint_callback:
                    checkpoint_callback()
            if (
                step < trial.steps
                and examples_seen % trial.checkpoint_every_examples == 0
            ):
                save_pretrain_checkpoint(
                    progress_path,
                    student,
                    optimizer,
                    trial,
                    **checkpoint_metadata,
                )
                if checkpoint_callback:
                    checkpoint_callback()

        status = "diverged" if divergence_reason else "complete"
        final_validation = (
            current_validation
            if divergence_reason
            else evaluate_next_token(
                student,
                val_contexts,
                val_targets,
                examples=len(val_contexts),
                batch_size=min(64, microbatch),
                device=device,
            )
        )
        final_embedding_sha256 = tensor_sha256(student.tied_embedding)
        result = {
            "schema": RESULT_SCHEMA,
            "status": status,
            "divergence_reason": divergence_reason,
            "label": trial.label,
            "trial": trial.to_dict(),
            "steps_completed": completed_step,
            "examples_seen": examples_seen,
            "input_tokens_seen":
                examples_seen * trial.architecture.context_length,
            "target_tokens_seen": examples_seen,
            "objective": "next_token_ce",
            "teacher_used": False,
            "teacher_cache_used": False,
            "embedding_trainable": True,
            "embedding_initialization": "trainable_random",
            "initial_embedding_sha256": initial_embedding_sha256,
            "final_embedding_sha256": final_embedding_sha256,
            "embedding_changed":
                final_embedding_sha256 != initial_embedding_sha256,
            "trainable_parameters": student.trainable_parameter_count(),
            "validation": final_validation,
            "test": {},
            "stable_validation": stable_validation,
            "microbatch": microbatch,
            "gradient_accumulation": accumulation,
            "wall_seconds":
                prior_wall + time.perf_counter() - wall_start,
            "median_step_seconds": (
                float(np.median(step_times)) if step_times else math.nan
            ),
            "wandb_run_id": run.id,
            "wandb_url": run.url,
            "dataset_manifest": manifest,
        }
        if status == "complete":
            save_pretrain_checkpoint(
                output / "student.pt",
                student,
                optimizer,
                trial,
                step=completed_step,
                examples_seen=examples_seen,
                initial_embedding_sha256=initial_embedding_sha256,
                initial_validation=initial_validation,
                elapsed_wall_seconds=result["wall_seconds"],
                stable_validation=stable_validation,
            )
            progress_path.unlink(missing_ok=True)
        _atomic_json(output / "result.json", result)
        run.log({
            "final/cross_entropy": final_validation["cross_entropy"],
            "final/perplexity": final_validation["perplexity"],
            "final/target_accuracy": final_validation["target_accuracy"],
            "final/embedding_changed": float(result["embedding_changed"]),
            "final/status_complete": float(status == "complete"),
        }, step=completed_step)
        run.summary.update({
            "status": status,
            "validation_cross_entropy":
                final_validation["cross_entropy"],
            "validation_perplexity": final_validation["perplexity"],
            "validation_target_accuracy":
                final_validation["target_accuracy"],
            "final_embedding_sha256": final_embedding_sha256,
        })
        run.finish(exit_code=0 if status == "complete" else 1)
        return result
    except BaseException:
        if wandb.run is not None:
            wandb.finish(exit_code=1)
        raise


def preflight_candidate(
    *,
    data_root: str,
    microbatch: int,
    device: str = "cuda",
) -> dict:
    if microbatch <= 0:
        raise ValueError("microbatch must be positive")
    trial = pretrain_trials()[0]
    manifest = load_manifest(data_root)
    contexts, targets, _ = load_split(data_root, "train")
    tokenizer = load_tokenizer()
    torch.set_float32_matmul_precision("high")
    torch.backends.cuda.matmul.allow_tf32 = True
    torch.cuda.reset_peak_memory_stats()
    student, initial_hash = random_trainable_student(
        trial,
        vocab_size=VOCAB_SIZE,
        device=device,
    )
    groups, metadata = optimizer_parameter_groups(
        student,
        trial.lr,
        trial.lr_parameterization,
        default_weight_decay=ADAMW["weight_decay"],
    )
    optimizer = torch.optim.AdamW(groups, **ADAMW)
    ids = torch.from_numpy(
        np.array(contexts[:microbatch], copy=True)
    ).to(device=device, dtype=torch.long)
    target = torch.from_numpy(
        np.array(targets[:microbatch], copy=True)
    ).to(device=device, dtype=torch.long)
    started = time.perf_counter()
    optimizer.zero_grad(set_to_none=True)
    student.collect_activation_diagnostics(True)
    with torch.autocast(device_type="cuda", dtype=torch.bfloat16):
        logits = student(ids)
    loss = next_token_cross_entropy_rows(logits, target).mean()
    loss.backward()
    student.collect_activation_diagnostics(False)
    preclip = torch.nn.utils.clip_grad_norm_(
        student.parameters(),
        trial.gradient_clip_norm,
    )
    optimizer.step()
    torch.cuda.synchronize()
    properties = torch.cuda.get_device_properties(torch.cuda.current_device())
    peak_allocated = torch.cuda.max_memory_allocated()
    peak_reserved = torch.cuda.max_memory_reserved()
    result = {
        "schema": "qwen-fullwidth-next-token-preflight-v1",
        "status": "complete",
        "microbatch": microbatch,
        "loss": float(loss.detach()),
        "gradient_norm_preclip": float(preclip),
        "step_seconds": time.perf_counter() - started,
        "peak_allocated_gib": peak_allocated / 2**30,
        "peak_reserved_gib": peak_reserved / 2**30,
        "memory_ratio": max(peak_allocated, peak_reserved)
            / properties.total_memory,
        "device": properties.name,
        "trainable_parameters": student.trainable_parameter_count(),
        "initial_embedding_sha256": initial_hash,
        "embedding_gradient_nonzero":
            float(student.tied_embedding.grad.float().norm()) > 0,
        "optimizer_roles": sorted({row["role"] for row in metadata}),
        "dataset_manifest": manifest,
        "teacher_used": False,
    }
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)
    return result


def _load_student_checkpoint(path: Path, device: str):
    checkpoint = torch.load(
        path,
        map_location="cpu",
        mmap=True,
        weights_only=True,
    )
    if checkpoint.get("schema") != CHECKPOINT_SCHEMA:
        raise RuntimeError("unsupported pretraining checkpoint")
    trial_value = dict(checkpoint["trial"])
    architecture_value = dict(trial_value.pop("architecture"))
    architecture_value.pop("label", None)
    trial = TrialConfig(
        architecture=ArchitectureConfig(**architecture_value),
        **trial_value,
    )
    embedding = torch.empty(
        int(checkpoint["vocab_size"]),
        trial.architecture.embedding_width,
        dtype=torch.bfloat16,
    )
    student = FullWidthStudent(
        trial.architecture,
        embedding,
        vocab_size=int(checkpoint["vocab_size"]),
        trainable_embedding=True,
    )
    student.load_state_dict(checkpoint["state_dict"], strict=True)
    return student.to(device), trial, checkpoint


def collect_study(
    *,
    data_root: str = DEFAULT_DATA_ROOT,
    output_root: str = DEFAULT_OUTPUT_ROOT,
    device: str = "cuda",
) -> dict:
    import wandb

    trials = pretrain_trials()
    results = []
    for trial in trials:
        result = _completed_result(Path(output_root) / trial.label, trial)
        if result is None:
            raise RuntimeError(f"missing completed cell {trial.label}")
        results.append(result)
    ranking = sorted(
        results,
        key=lambda result: (
            float(result["validation"]["cross_entropy"]),
            result["label"],
        ),
    )
    winner = ranking[0]
    winner_path = Path(output_root) / winner["label"] / "student.pt"
    student, winner_trial, checkpoint = _load_student_checkpoint(
        winner_path,
        device,
    )
    _, _, _ = load_split(data_root, "train")
    test_contexts, test_targets, _ = load_split(data_root, "test")
    test = evaluate_next_token(
        student,
        test_contexts,
        test_targets,
        examples=min(TEST_EXAMPLES, len(test_contexts)),
        batch_size=64,
        device=device,
    )
    winner["test"] = test
    _atomic_json(
        Path(output_root) / winner["label"] / "result.json",
        winner,
    )
    ranked_rows = []
    for rank, result in enumerate(ranking, start=1):
        ranked_rows.append({
            "rank": rank,
            "label": result["label"],
            "effective_batch":
                int(result["trial"].get("effective_batch", 1_024)),
            "lr": float(result["trial"]["lr"]),
            "validation_cross_entropy":
                float(result["validation"]["cross_entropy"]),
            "validation_perplexity":
                float(result["validation"]["perplexity"]),
            "validation_target_accuracy":
                float(result["validation"]["target_accuracy"]),
            "wandb_url": result["wandb_url"],
        })
    summary = {
        "schema": SUMMARY_SCHEMA,
        "status": "complete",
        "plan": study_plan(),
        "ranking": ranked_rows,
        "winner": ranked_rows[0] | {"test": test},
        "teacher_used": False,
        "teacher_cache_used": False,
    }
    summary["artifact_sha256"] = _canonical_hash(summary)
    summary_path = Path(output_root) / "study-summary.json"
    _atomic_json(summary_path, summary)
    weights_path = Path(output_root) / "winner-weights.pt"
    temporary = weights_path.with_suffix(".pt.tmp")
    torch.save({
        "schema": "qwen-fullwidth-next-token-weights-v1",
        "state_dict": student.state_dict(),
        "trial": winner_trial.to_dict(),
        "model_id": MODEL_ID,
        "model_revision": MODEL_REVISION,
        "vocab_size": checkpoint["vocab_size"],
        "initial_embedding_sha256":
            checkpoint["initial_embedding_sha256"],
        "final_embedding_sha256":
            winner["final_embedding_sha256"],
        "validation": winner["validation"],
        "test": test,
    }, temporary)
    os.replace(temporary, weights_path)

    summary_run = wandb.init(
        project=WANDB_PROJECT,
        group=WANDB_GROUP,
        name=f"{WANDB_GROUP}-summary",
        job_type="study-summary",
        id=hashlib.sha256(WANDB_GROUP.encode()).hexdigest()[:16],
        resume="allow",
        config={
            "schema": SUMMARY_SCHEMA,
            "plan_sha256": summary["plan"]["plan_sha256"],
            "winner_label": winner["label"],
        },
    )
    summary_run.log({
        "winner/validation_cross_entropy":
            winner["validation"]["cross_entropy"],
        "winner/validation_perplexity":
            winner["validation"]["perplexity"],
        "winner/test_cross_entropy": test["cross_entropy"],
        "winner/test_perplexity": test["perplexity"],
        "winner/test_target_accuracy": test["target_accuracy"],
    })
    artifact = wandb.Artifact(
        f"qwen-fullwidth-next-token-d4r2407-{summary_run.id}",
        type="model",
        metadata={
            "summary_sha256": summary["artifact_sha256"],
            "winner_label": winner["label"],
        },
    )
    artifact.add_file(str(weights_path))
    artifact.add_file(str(summary_path))
    summary_run.log_artifact(artifact)
    summary_run.finish()
    print(json.dumps(summary, indent=2, sort_keys=True), flush=True)
    return summary


def _parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "command",
        choices=("plan", "prepare", "run-cell", "preflight", "collect"),
    )
    parser.add_argument("--cell-index", type=int, default=-1)
    parser.add_argument("--microbatch", type=int, default=64)
    parser.add_argument("--data-root", default=DEFAULT_DATA_ROOT)
    parser.add_argument("--output-root", default=DEFAULT_OUTPUT_ROOT)
    parser.add_argument("--device", default="cuda")
    parser.add_argument("--result-path", default="")
    return parser.parse_args()


def main() -> None:
    args = _parse_args()
    if args.command == "plan":
        print(json.dumps(study_plan(), indent=2, sort_keys=True))
        return
    if args.command == "prepare":
        print(json.dumps(
            prepare_pretrain_data(args.data_root),
            indent=2,
            sort_keys=True,
        ))
        return
    if args.command == "preflight":
        result = preflight_candidate(
            data_root=args.data_root,
            microbatch=args.microbatch,
            device=args.device,
        )
        if args.result_path:
            _atomic_json(Path(args.result_path), result)
        return
    if args.command == "run-cell":
        trials = pretrain_trials()
        if not 0 <= args.cell_index < len(trials):
            raise ValueError(
                f"cell index must be between 0 and {len(trials) - 1}"
            )
        result = run_pretrain_trial(
            trials[args.cell_index],
            data_root=args.data_root,
            output_root=args.output_root,
            device=args.device,
        )
        print(json.dumps(result, indent=2, sort_keys=True))
        return
    collect_study(
        data_root=args.data_root,
        output_root=args.output_root,
        device=args.device,
    )


if __name__ == "__main__":
    main()
