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
import torch.nn.functional as F

from .config import (
    ADAMW_BETAS,
    AUDIT_EXAMPLES,
    AUX_ADAMW_LR,
    DEFAULT_NORMUON_LR,
    FINAL_EXAMPLES,
    INITIALIZER_RANGE,
    MODEL_ID,
    MODEL_REVISION,
    NORMUON_BETA1,
    NORMUON_BETA2,
    NORMUON_LR_GRID,
    OPTIMIZER_EPS,
    SCREEN_EXAMPLES,
    TEST_EXAMPLES,
    VOCAB_SIZE,
    WEIGHT_DECAY,
    Architecture,
    Trial,
    default_optimizer_policy,
    final_trials,
    microbatch_for,
    screen_trials,
    wsd_multiplier,
)
from .data import load_manifest, load_split, prepare_dataset
from .model import (
    EXPECTED_TRAINABLE_PARAMETERS,
    NextTokenStudent,
)
from .normuon import SOURCE_COMMIT, SingleDeviceNorMuon

PLAN_SCHEMA = "qwen-normuon-next-token-plan-v1"
CHECKPOINT_SCHEMA = "qwen-normuon-next-token-checkpoint-v1"
RESULT_SCHEMA = "qwen-normuon-next-token-result-v1"
SCREEN_SUMMARY_SCHEMA = "qwen-normuon-screen-summary-v1"
FINAL_SUMMARY_SCHEMA = "qwen-normuon-final-summary-v1"
PREFLIGHT_SCHEMA = "qwen-normuon-preflight-v1"

WANDB_PROJECT = "qwen-normuon-next-token-pretrain"
WANDB_GROUP = "fwedu-normuon-random-tied-d4r2407-v1"
DEFAULT_DATA_ROOT = (
    "/cache/qwen_fullwidth_distill/"
    "context16-fineweb-edu-next-token-4m-v1"
)
DEFAULT_OUTPUT_ROOT = "/cache/qwen_normuon_pretrain/v1"


def canonical_hash(value: dict) -> str:
    payload = json.dumps(
        value,
        sort_keys=True,
        separators=(",", ":"),
        allow_nan=False,
    ).encode()
    return hashlib.sha256(payload).hexdigest()


def atomic_json(path: Path, value: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_text(json.dumps(value, indent=2, sort_keys=True))
    os.replace(temporary, path)


def tensor_sha256(tensor: torch.Tensor) -> str:
    value = tensor.detach().contiguous().cpu()
    return hashlib.sha256(value.view(torch.uint8).numpy()).hexdigest()


def factor_sha256(student: NextTokenStudent) -> str:
    digest = hashlib.sha256()
    for parameter in student.factor_parameters():
        value = parameter.detach().contiguous().cpu()
        digest.update(value.view(torch.uint8).numpy())
    return digest.hexdigest()


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


def prepare_data(data_root: str = DEFAULT_DATA_ROOT) -> dict:
    return prepare_dataset(data_root, load_tokenizer())


def next_token_cross_entropy_rows(
    logits: torch.Tensor,
    targets: torch.Tensor,
) -> torch.Tensor:
    if logits.ndim != 2 or targets.ndim != 1:
        raise ValueError("logits/targets must be rank two/one")
    if len(logits) != len(targets):
        raise ValueError("logits/targets batch mismatch")
    return F.cross_entropy(
        logits.float(),
        targets,
        reduction="none",
    )


def study_plan() -> dict:
    value = {
        "schema": PLAN_SCHEMA,
        "status": "planned",
        "objective": "next_token_cross_entropy_full_vocabulary",
        "teacher_used": False,
        "teacher_cache_used": False,
        "model_id": MODEL_ID,
        "model_revision": MODEL_REVISION,
        "vocab_size": VOCAB_SIZE,
        "initializer": {
            "distribution": "normal",
            "mean": 0.0,
            "std": INITIALIZER_RANGE,
            "tied_input_output": True,
        },
        "optimizer": {
            "factor_optimizer": "normuon",
            "selected_default": default_optimizer_policy(),
            "source_commit": SOURCE_COMMIT,
            "shape_batched_newton_schulz": True,
            "beta1": NORMUON_BETA1,
            "beta2": NORMUON_BETA2,
            "nesterov": True,
            "newton_schulz_steps": 5,
            "epsilon": OPTIMIZER_EPS,
            "weight_decay": WEIGHT_DECAY,
            "direct_lr_grid": list(NORMUON_LR_GRID),
            "aux_optimizer": "fused_adamw",
            "aux_lr": AUX_ADAMW_LR,
            "aux_betas": list(ADAMW_BETAS),
            "aux_epsilon": OPTIMIZER_EPS,
            "schedule": "wsd_shared_multiplier",
        },
        "screen": {
            "examples_per_cell": SCREEN_EXAMPLES,
            "cells": [
                trial.to_dict() | {"label": trial.label}
                for trial in screen_trials()
            ],
            "selection": {
                "metric": "validation_cross_entropy",
                "finite_required": True,
                "finalists": 2,
                "tie_breaker": "lower_lr",
            },
        },
        "final": {
            "examples_per_cell": FINAL_EXAMPLES,
            "policy": "top_two_screen_lrs_x_batches_2048_4096",
            "fresh_seed": 0,
            "test_only_winner": True,
        },
    }
    value["plan_sha256"] = canonical_hash(value)
    return value


def trial_from_dict(value: dict) -> Trial:
    trial_value = dict(value)
    architecture = Architecture(**trial_value.pop("architecture"))
    trial = Trial(architecture=architecture, **trial_value)
    trial.validate()
    return trial


def make_student(
    trial: Trial,
    *,
    device: str,
) -> tuple[NextTokenStudent, dict[str, str]]:
    torch.manual_seed(trial.seed)
    np.random.seed(trial.seed)
    embedding = torch.empty(
        VOCAB_SIZE,
        trial.architecture.embedding_width,
        dtype=torch.bfloat16,
        device="cpu",
    )
    embedding.normal_(mean=0.0, std=INITIALIZER_RANGE)
    initial_embedding_hash = tensor_sha256(embedding)
    student = NextTokenStudent(
        trial.architecture,
        embedding,
        vocab_size=VOCAB_SIZE,
    ).to(device)
    inventory = student.validate_study_inventory()
    if inventory["trainable_parameters"] != EXPECTED_TRAINABLE_PARAMETERS:
        raise RuntimeError("trainable parameter identity changed")
    return student, {
        "embedding": initial_embedding_hash,
        "factors": factor_sha256(student),
    }


def make_optimizers(
    student: NextTokenStudent,
    trial: Trial,
    *,
    device: str,
):
    factors = student.factor_parameters()
    auxiliary = student.auxiliary_parameters()
    if {id(value) for value in factors} & {
        id(value) for value in auxiliary
    }:
        raise RuntimeError("optimizer routes overlap")
    if {
        id(value) for value in factors + auxiliary
    } != {id(value) for value in student.parameters()}:
        raise RuntimeError("optimizer routes do not cover the model")
    factor_optimizer = SingleDeviceNorMuon(
        factors,
        lr=trial.normuon_lr,
        weight_decay=WEIGHT_DECAY,
        beta1=NORMUON_BETA1,
        beta2=NORMUON_BETA2,
        ns_steps=5,
        nesterov=True,
        eps=OPTIMIZER_EPS,
    )
    aux_optimizer = torch.optim.AdamW(
        auxiliary,
        lr=trial.aux_adamw_lr,
        betas=ADAMW_BETAS,
        eps=OPTIMIZER_EPS,
        weight_decay=WEIGHT_DECAY,
        fused=device.startswith("cuda"),
    )
    metadata = {
        "default_policy": default_optimizer_policy(),
        "uses_selected_default":
            trial.normuon_lr == DEFAULT_NORMUON_LR,
        "factor_tensors": len(factors),
        "factor_shape_buckets": len({
            tuple(parameter.shape[-2:])
            for parameter in factors
        }),
        "shape_batched_newton_schulz": True,
        "factor_matrices": sum(
            math.prod(parameter.shape[:-2])
            for parameter in factors
        ),
        "factor_parameters": sum(
            parameter.numel() for parameter in factors
        ),
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
) -> tuple[float, float, float]:
    multiplier = wsd_multiplier(trial, step)
    factor_lr = trial.normuon_lr * multiplier
    aux_lr = trial.aux_adamw_lr * multiplier
    for group in factor_optimizer.param_groups:
        group["lr"] = factor_lr
    for group in aux_optimizer.param_groups:
        group["lr"] = aux_lr
    return multiplier, factor_lr, aux_lr


def microbatch_chunks(total: int, microbatch: int) -> tuple[int, ...]:
    if min(total, microbatch) <= 0:
        raise ValueError("batch sizes must be positive")
    chunks = [microbatch] * (total // microbatch)
    if total % microbatch:
        chunks.append(total % microbatch)
    return tuple(chunks)


def training_order(size: int, examples: int, seed: int) -> np.ndarray:
    if examples > size:
        raise ValueError("the study forbids data reuse")
    return np.random.default_rng(seed).permutation(size)[:examples]


@torch.inference_mode()
def evaluate(
    student: NextTokenStudent,
    contexts: np.ndarray,
    targets: np.ndarray,
    *,
    examples: int,
    batch_size: int,
    device: str,
) -> dict[str, float]:
    student.eval()
    total = min(examples, len(contexts))
    loss_sum = 0.0
    correct = 0
    for lo in range(0, total, batch_size):
        hi = min(lo + batch_size, total)
        ids = torch.from_numpy(
            np.array(contexts[lo:hi], copy=True)
        ).to(device=device, dtype=torch.long)
        target = torch.from_numpy(
            np.array(targets[lo:hi], copy=True)
        ).to(device=device, dtype=torch.long)
        with torch.autocast(
            device_type="cuda",
            dtype=torch.bfloat16,
            enabled=device.startswith("cuda"),
        ):
            logits = student(ids)
        rows = next_token_cross_entropy_rows(logits, target)
        loss_sum += float(rows.sum())
        correct += int((logits.argmax(-1) == target).sum())
    student.train()
    cross_entropy = loss_sum / total
    return {
        "cross_entropy": cross_entropy,
        "perplexity": math.exp(min(cross_entropy, 30.0)),
        "target_accuracy": correct / total,
        "examples": total,
    }


def checkpoint_value(
    student: NextTokenStudent,
    factor_optimizer: torch.optim.Optimizer,
    aux_optimizer: torch.optim.Optimizer,
    trial: Trial,
    *,
    step: int,
    examples_seen: int,
    initial_hashes: dict[str, str],
    initial_validation: dict,
    elapsed_wall_seconds: float,
) -> dict:
    value = {
        "schema": CHECKPOINT_SCHEMA,
        "trial": trial.to_dict(),
        "state_dict": student.state_dict(),
        "factor_optimizer_state_dict": factor_optimizer.state_dict(),
        "aux_optimizer_state_dict": aux_optimizer.state_dict(),
        "optimizer_states_included": True,
        "step": step,
        "examples_seen": examples_seen,
        "input_tokens_seen":
            examples_seen * trial.architecture.context_length,
        "target_tokens_seen": examples_seen,
        "initial_hashes": initial_hashes,
        "initial_validation": initial_validation,
        "elapsed_wall_seconds": elapsed_wall_seconds,
        "torch_rng_state": torch.get_rng_state(),
        "numpy_seed": trial.seed,
    }
    if torch.cuda.is_available():
        value["cuda_rng_state"] = torch.cuda.get_rng_state()
    return value


def save_checkpoint(
    path: Path,
    student: NextTokenStudent,
    factor_optimizer: torch.optim.Optimizer,
    aux_optimizer: torch.optim.Optimizer,
    trial: Trial,
    **metadata,
) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    torch.save(
        checkpoint_value(
            student,
            factor_optimizer,
            aux_optimizer,
            trial,
            **metadata,
        ),
        temporary,
    )
    os.replace(temporary, path)


def load_checkpoint(
    path: Path,
    student: NextTokenStudent,
    factor_optimizer: torch.optim.Optimizer,
    aux_optimizer: torch.optim.Optimizer,
    trial: Trial,
    initial_hashes: dict[str, str],
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
        "trial": trial.to_dict(),
        "initial_hashes": initial_hashes,
        "numpy_seed": trial.seed,
    }
    for key, value in expected.items():
        if checkpoint.get(key) != value:
            raise RuntimeError(f"checkpoint mismatched {key}")
    step = int(checkpoint.get("step", -1))
    examples_seen = int(checkpoint.get("examples_seen", -1))
    if not 0 < step < trial.steps:
        raise RuntimeError("checkpoint step is outside resumable range")
    if examples_seen != step * trial.effective_batch:
        raise RuntimeError("checkpoint data cursor is invalid")
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


def completed_result(
    output: Path,
    trial: Trial,
) -> dict | None:
    try:
        result = json.loads((output / "result.json").read_text())
    except (FileNotFoundError, json.JSONDecodeError, OSError):
        return None
    if (
        result.get("schema") != RESULT_SCHEMA
        or result.get("status") != "complete"
        or result.get("trial") != trial.to_dict()
        or result.get("label") != trial.label
        or int(result.get("steps_completed", -1)) != trial.steps
        or not math.isfinite(float(
            result.get("validation", {}).get(
                "cross_entropy",
                math.nan,
            )
        ))
    ):
        return None
    if trial.stage == "final":
        checkpoint = output / "student.pt"
        if not checkpoint.is_file() or checkpoint.stat().st_size <= 0:
            return None
    return result


def wandb_run_id(trial: Trial) -> str:
    return hashlib.sha256(
        f"{WANDB_GROUP}:{trial.label}".encode()
    ).hexdigest()[:16]


def init_wandb(
    trial: Trial,
    *,
    output: Path,
    manifest: dict,
    initial_hashes: dict[str, str],
    optimizer_metadata: dict,
    resume_step: int,
):
    import wandb

    if not os.environ.get("WANDB_API_KEY") and os.environ.get(
        "WANDB_MODE",
        "",
    ).lower() not in ("offline", "disabled"):
        raise RuntimeError("WANDB_API_KEY is required for online training")
    return wandb.init(
        project=WANDB_PROJECT,
        group=WANDB_GROUP,
        name=trial.label,
        job_type=f"next-token-{trial.stage}",
        id=wandb_run_id(trial),
        resume="allow",
        dir=str(output / "wandb"),
        config={
            "schema": PLAN_SCHEMA,
            "trial": trial.to_dict(),
            "model_id": MODEL_ID,
            "model_revision": MODEL_REVISION,
            "objective": "next_token_cross_entropy_full_vocabulary",
            "teacher_used": False,
            "teacher_cache_used": False,
            "embedding_initialization": "trainable_random",
            "initial_hashes": initial_hashes,
            "optimizer": {
                "factor": "normuon",
                "selected_default": default_optimizer_policy(),
                "uses_selected_default":
                    trial.normuon_lr == DEFAULT_NORMUON_LR,
                "source_commit": SOURCE_COMMIT,
                "beta1": NORMUON_BETA1,
                "beta2": NORMUON_BETA2,
                "newton_schulz_steps": 5,
                "nesterov": True,
                "weight_decay": WEIGHT_DECAY,
                "auxiliary": "fused_adamw",
                "auxiliary_betas": ADAMW_BETAS,
                "epsilon": OPTIMIZER_EPS,
            },
            "optimizer_routing": optimizer_metadata,
            "dataset_manifest": manifest,
            "resumed_from_step": resume_step,
        },
    )


def run_trial(
    trial: Trial,
    *,
    data_root: str = DEFAULT_DATA_ROOT,
    output_root: str = DEFAULT_OUTPUT_ROOT,
    device: str = "cuda",
) -> dict:
    import wandb

    trial.validate()
    allowed = (
        screen_trials()
        if trial.stage == "screen"
        else final_trials(load_screen_finalists(output_root))
    )
    if trial not in allowed:
        raise ValueError("runner accepts only an exact planned cell")
    output = Path(output_root) / trial.stage / trial.label
    cached = completed_result(output, trial)
    if cached is not None:
        print(f"[cache] {trial.label}", flush=True)
        return cached
    output.mkdir(parents=True, exist_ok=True)
    (output / "wandb").mkdir(parents=True, exist_ok=True)

    torch.set_float32_matmul_precision("high")
    if device.startswith("cuda"):
        torch.backends.cuda.matmul.allow_tf32 = True
        torch.backends.cudnn.allow_tf32 = True
        torch.cuda.reset_peak_memory_stats()

    manifest = load_manifest(data_root)
    train_contexts, train_targets, _ = load_split(data_root, "train")
    val_contexts, val_targets, _ = load_split(data_root, "validation")
    student, initial_hashes = make_student(trial, device=device)
    factor_optimizer, aux_optimizer, optimizer_metadata = make_optimizers(
        student,
        trial,
        device=device,
    )
    progress_path = output / "progress.pt"
    resume = load_checkpoint(
        progress_path,
        student,
        factor_optimizer,
        aux_optimizer,
        trial,
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
        trial,
        output=output,
        manifest=manifest,
        initial_hashes=initial_hashes,
        optimizer_metadata=optimizer_metadata,
        resume_step=resume_step,
    )
    print(f"[wandb] {run.url}", flush=True)
    print(
        f"[start] {trial.label} step={resume_step}/{trial.steps} "
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
            schedule, factor_lr, aux_lr = set_learning_rates(
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
            chunks = microbatch_chunks(
                trial.effective_batch,
                microbatch,
            )
            for index, chunk_size in enumerate(chunks):
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
                "optimizer/normuon_lr": factor_lr,
                "optimizer/aux_adamw_lr": aux_lr,
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
                    f"[{trial.label}] step={step}/{trial.steps} "
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
        final_validation = (
            current_validation
            if divergence_reason
            else evaluate(
                student,
                val_contexts,
                val_targets,
                examples=len(val_contexts),
                batch_size=min(64, microbatch),
                device=device,
            )
        )
        final_hashes = {
            "embedding": tensor_sha256(student.tied_embedding),
            "factors": factor_sha256(student),
        }
        result = {
            "schema": RESULT_SCHEMA,
            "status": status,
            "divergence_reason": divergence_reason,
            "label": trial.label,
            "trial": trial.to_dict(),
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
            "test": {},
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
        if status == "complete":
            if trial.stage == "final":
                save_checkpoint(
                    output / "student.pt",
                    student,
                    factor_optimizer,
                    aux_optimizer,
                    trial,
                    step=completed_step,
                    examples_seen=examples_seen,
                    initial_hashes=initial_hashes,
                    initial_validation=initial_validation,
                    elapsed_wall_seconds=result["wall_seconds"],
                )
            progress_path.unlink(missing_ok=True)
        atomic_json(output / "result.json", result)
        run.log({
            "final/cross_entropy":
                final_validation["cross_entropy"],
            "final/perplexity":
                final_validation["perplexity"],
            "final/target_accuracy":
                final_validation["target_accuracy"],
            "final/status_complete": float(status == "complete"),
        }, step=completed_step)
        run.summary.update({
            "status": status,
            "validation_cross_entropy":
                final_validation["cross_entropy"],
            "validation_perplexity":
                final_validation["perplexity"],
            "embedding_changed": result["embedding_changed"],
            "factors_changed": result["factors_changed"],
        })
        run.finish(exit_code=0 if status == "complete" else 1)
        return result
    except BaseException:
        if wandb.run is not None:
            wandb.finish(exit_code=1)
        raise


def collect_screen(
    *,
    output_root: str = DEFAULT_OUTPUT_ROOT,
) -> dict:
    rows = []
    for trial in screen_trials():
        result = completed_result(
            Path(output_root) / "screen" / trial.label,
            trial,
        )
        if result is None:
            raise RuntimeError(f"missing screen cell {trial.label}")
        rows.append(result)
    ranking = sorted(
        rows,
        key=lambda result: (
            float(result["validation"]["cross_entropy"]),
            float(result["trial"]["normuon_lr"]),
        ),
    )
    top_lrs = tuple(
        float(result["trial"]["normuon_lr"])
        for result in ranking[:2]
    )
    summary = {
        "schema": SCREEN_SUMMARY_SCHEMA,
        "status": "complete",
        "ranking": [
            {
                "rank": rank,
                "label": result["label"],
                "normuon_lr":
                    float(result["trial"]["normuon_lr"]),
                "validation_cross_entropy":
                    float(result["validation"]["cross_entropy"]),
                "validation_perplexity":
                    float(result["validation"]["perplexity"]),
                "wandb_url": result["wandb_url"],
            }
            for rank, result in enumerate(ranking, start=1)
        ],
        "top_lrs": list(top_lrs),
        "final_cells": [
            trial.to_dict() | {"label": trial.label}
            for trial in final_trials(top_lrs)
        ],
    }
    summary["summary_sha256"] = canonical_hash(summary)
    atomic_json(
        Path(output_root) / "screen-summary.json",
        summary,
    )
    print(json.dumps(summary, indent=2, sort_keys=True), flush=True)
    return summary


def load_screen_finalists(
    output_root: str = DEFAULT_OUTPUT_ROOT,
) -> tuple[float, float]:
    path = Path(output_root) / "screen-summary.json"
    try:
        summary = json.loads(path.read_text())
    except (FileNotFoundError, json.JSONDecodeError, OSError) as error:
        raise RuntimeError("screen summary is unavailable") from error
    if (
        summary.get("schema") != SCREEN_SUMMARY_SCHEMA
        or summary.get("status") != "complete"
    ):
        raise RuntimeError("screen summary is invalid")
    top_lrs = tuple(float(value) for value in summary["top_lrs"])
    final_trials(top_lrs)
    return top_lrs


def load_final_student(
    path: Path,
    *,
    device: str,
):
    checkpoint = torch.load(
        path,
        map_location="cpu",
        mmap=True,
        weights_only=True,
    )
    if checkpoint.get("schema") != CHECKPOINT_SCHEMA:
        raise RuntimeError("unsupported final checkpoint")
    trial = trial_from_dict(checkpoint["trial"])
    student, initial_hashes = make_student(trial, device="cpu")
    if checkpoint["initial_hashes"] != initial_hashes:
        raise RuntimeError("final checkpoint initializer mismatch")
    student.load_state_dict(checkpoint["state_dict"], strict=True)
    return student.to(device), trial, checkpoint


def collect_final(
    *,
    data_root: str = DEFAULT_DATA_ROOT,
    output_root: str = DEFAULT_OUTPUT_ROOT,
    device: str = "cuda",
) -> dict:
    import wandb

    trials = final_trials(load_screen_finalists(output_root))
    results = []
    for trial in trials:
        result = completed_result(
            Path(output_root) / "final" / trial.label,
            trial,
        )
        if result is None:
            raise RuntimeError(f"missing final cell {trial.label}")
        results.append(result)
    ranking = sorted(
        results,
        key=lambda result: (
            float(result["validation"]["cross_entropy"]),
            float(result["trial"]["normuon_lr"]),
            int(result["trial"]["effective_batch"]),
        ),
    )
    winner = ranking[0]
    winner_checkpoint = (
        Path(output_root)
        / "final"
        / winner["label"]
        / "student.pt"
    )
    student, winner_trial, checkpoint = load_final_student(
        winner_checkpoint,
        device=device,
    )
    test_contexts, test_targets, _ = load_split(data_root, "test")
    test = evaluate(
        student,
        test_contexts,
        test_targets,
        examples=min(TEST_EXAMPLES, len(test_contexts)),
        batch_size=64,
        device=device,
    )
    winner["test"] = test
    atomic_json(
        Path(output_root)
        / "final"
        / winner["label"]
        / "result.json",
        winner,
    )
    ranked_rows = [
        {
            "rank": rank,
            "label": result["label"],
            "normuon_lr":
                float(result["trial"]["normuon_lr"]),
            "effective_batch":
                int(result["trial"]["effective_batch"]),
            "validation_cross_entropy":
                float(result["validation"]["cross_entropy"]),
            "validation_perplexity":
                float(result["validation"]["perplexity"]),
            "wandb_url": result["wandb_url"],
        }
        for rank, result in enumerate(ranking, start=1)
    ]
    summary = {
        "schema": FINAL_SUMMARY_SCHEMA,
        "status": "complete",
        "plan": study_plan(),
        "screen": json.loads(
            (Path(output_root) / "screen-summary.json").read_text()
        ),
        "ranking": ranked_rows,
        "winner": ranked_rows[0] | {"test": test},
        "teacher_used": False,
        "teacher_cache_used": False,
    }
    summary["summary_sha256"] = canonical_hash(summary)
    summary_path = Path(output_root) / "study-summary.json"
    atomic_json(summary_path, summary)

    weights_path = Path(output_root) / "winner-weights.pt"
    temporary = weights_path.with_suffix(".pt.tmp")
    torch.save({
        "schema": "qwen-normuon-next-token-weights-v1",
        "state_dict": student.state_dict(),
        "trial": winner_trial.to_dict(),
        "initial_hashes": checkpoint["initial_hashes"],
        "final_hashes": winner["final_hashes"],
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
            "schema": FINAL_SUMMARY_SCHEMA,
            "plan_sha256": summary["plan"]["plan_sha256"],
            "winner_label": winner["label"],
        },
    )
    summary_run.log({
        "winner/validation_cross_entropy":
            winner["validation"]["cross_entropy"],
        "winner/test_cross_entropy": test["cross_entropy"],
        "winner/test_perplexity": test["perplexity"],
    })
    artifact = wandb.Artifact(
        name="qwen-normuon-next-token-winner",
        type="model",
        metadata={
            "summary_sha256": summary["summary_sha256"],
            "winner_label": winner["label"],
        },
    )
    artifact.add_file(str(weights_path))
    artifact.add_file(str(summary_path))
    summary_run.log_artifact(artifact)
    summary_run.summary.update(summary["winner"])
    summary["wandb_summary_url"] = summary_run.url
    summary_run.finish()
    atomic_json(summary_path, summary)
    print(json.dumps(summary, indent=2, sort_keys=True), flush=True)
    return summary


def preflight_candidate(
    *,
    data_root: str,
    microbatch: int,
    device: str = "cuda",
) -> dict:
    if microbatch <= 0:
        raise ValueError("microbatch must be positive")
    trial = screen_trials()[0]
    os.environ["QWEN_NORMUON_MICROBATCH"] = str(microbatch)
    manifest = load_manifest(data_root)
    contexts, targets, _ = load_split(data_root, "train")
    torch.set_float32_matmul_precision("high")
    torch.backends.cuda.matmul.allow_tf32 = True
    torch.cuda.reset_peak_memory_stats()
    student, initial_hashes = make_student(trial, device=device)
    factor_optimizer, aux_optimizer, metadata = make_optimizers(
        student,
        trial,
        device=device,
    )
    factor_optimizer.zero_grad(set_to_none=True)
    aux_optimizer.zero_grad(set_to_none=True)
    chunks = microbatch_chunks(trial.effective_batch, microbatch)
    cursor = 0
    loss_value = 0.0
    torch.cuda.synchronize()
    started = time.perf_counter()
    for index, chunk_size in enumerate(chunks):
        student.collect_activation_diagnostics(index == 0)
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
        student.collect_activation_diagnostics(False)
        rows = next_token_cross_entropy_rows(logits, target)
        weight = chunk_size / trial.effective_batch
        (rows.mean() * weight).backward()
        loss_value += float(rows.detach().mean()) * weight
        del ids, target, logits, rows
    torch.cuda.synchronize()
    backward_finished = time.perf_counter()
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
    torch.cuda.synchronize()
    clip_finished = time.perf_counter()
    set_learning_rates(
        factor_optimizer,
        aux_optimizer,
        trial,
        1,
    )
    factor_optimizer.step()
    torch.cuda.synchronize()
    factor_step_finished = time.perf_counter()
    aux_optimizer.step()
    torch.cuda.synchronize()
    aux_step_finished = time.perf_counter()
    diagnostics = (
        student.activation_metrics() | student.factor_metrics()
    )
    torch.cuda.synchronize()
    diagnostics_finished = time.perf_counter()
    properties = torch.cuda.get_device_properties(
        torch.cuda.current_device()
    )
    allocated = torch.cuda.max_memory_allocated()
    reserved = torch.cuda.max_memory_reserved()
    result = {
        "schema": PREFLIGHT_SCHEMA,
        "status": "complete",
        "microbatch": microbatch,
        "accumulation": len(chunks),
        "loss": loss_value,
        "gradient_norm_preclip": float(preclip),
        "factor_gradient_nonzero": factor_gradient_nonzero,
        "embedding_gradient_nonzero": embedding_gradient_nonzero,
        "step_seconds": diagnostics_finished - started,
        "timings": {
            "forward_backward_seconds":
                backward_finished - started,
            "clip_and_gradient_checks_seconds":
                clip_finished - backward_finished,
            "factor_optimizer_seconds":
                factor_step_finished - clip_finished,
            "aux_optimizer_seconds":
                aux_step_finished - factor_step_finished,
            "diagnostics_seconds":
                diagnostics_finished - aux_step_finished,
        },
        "peak_allocated_gib": allocated / 2**30,
        "peak_reserved_gib": reserved / 2**30,
        "memory_ratio": max(allocated, reserved)
            / properties.total_memory,
        "device": properties.name,
        "initial_hashes": initial_hashes,
        "optimizer_routing": metadata,
        "dataset_manifest": manifest,
        "diagnostics": diagnostics,
        "teacher_used": False,
    }
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)
    return result


def status(output_root: str = DEFAULT_OUTPUT_ROOT) -> dict:
    output = Path(output_root)
    try:
        screen_summary = json.loads(
            (output / "screen-summary.json").read_text()
        )
    except (FileNotFoundError, json.JSONDecodeError, OSError):
        screen_summary = None
    cells = []
    for trial in screen_trials():
        result = completed_result(
            output / "screen" / trial.label,
            trial,
        )
        cells.append({
            "stage": "screen",
            "label": trial.label,
            "status": (
                "complete"
                if result
                else (
                    "checkpointed"
                    if (
                        output
                        / "screen"
                        / trial.label
                        / "progress.pt"
                    ).is_file()
                    else "pending"
                )
            ),
            "validation_cross_entropy": (
                result["validation"]["cross_entropy"]
                if result
                else None
            ),
        })
    if screen_summary is not None:
        for trial in final_trials(tuple(screen_summary["top_lrs"])):
            result = completed_result(
                output / "final" / trial.label,
                trial,
            )
            cells.append({
                "stage": "final",
                "label": trial.label,
                "status": (
                    "complete"
                    if result
                    else (
                        "checkpointed"
                        if (
                            output
                            / "final"
                            / trial.label
                            / "progress.pt"
                        ).is_file()
                        else "pending"
                    )
                ),
                "validation_cross_entropy": (
                    result["validation"]["cross_entropy"]
                    if result
                    else None
                ),
            })
    try:
        coordinator = json.loads(
            (output / "coordinator-status.json").read_text()
        )
    except (FileNotFoundError, json.JSONDecodeError, OSError):
        coordinator = {"status": "not_started"}
    value = {
        "coordinator": coordinator,
        "screen_summary": screen_summary,
        "cells": cells,
    }
    print(json.dumps(value, indent=2, sort_keys=True), flush=True)
    return value


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "command",
        choices=(
            "default",
            "plan",
            "prepare",
            "run-screen",
            "collect-screen",
            "run-final",
            "collect-final",
            "preflight",
            "status",
        ),
    )
    parser.add_argument("--cell-index", type=int, default=-1)
    parser.add_argument("--microbatch", type=int, default=0)
    parser.add_argument("--result-path", default="")
    parser.add_argument("--data-root", default=DEFAULT_DATA_ROOT)
    parser.add_argument("--output-root", default=DEFAULT_OUTPUT_ROOT)
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    if args.command == "default":
        print(json.dumps(
            default_optimizer_policy(),
            indent=2,
            sort_keys=True,
        ))
        return
    if args.command == "plan":
        print(json.dumps(study_plan(), indent=2, sort_keys=True))
        return
    if args.command == "prepare":
        print(json.dumps(
            prepare_data(args.data_root),
            indent=2,
            sort_keys=True,
        ))
        return
    if args.command == "collect-screen":
        collect_screen(output_root=args.output_root)
        return
    if args.command == "collect-final":
        collect_final(
            data_root=args.data_root,
            output_root=args.output_root,
        )
        return
    if args.command == "status":
        status(args.output_root)
        return
    if args.command == "preflight":
        if args.microbatch <= 0:
            raise ValueError("preflight requires --microbatch")
        result = preflight_candidate(
            data_root=args.data_root,
            microbatch=args.microbatch,
        )
        if args.result_path:
            atomic_json(Path(args.result_path), result)
        return
    if args.command == "run-screen":
        trials = screen_trials()
    else:
        trials = final_trials(load_screen_finalists(args.output_root))
    if not 0 <= args.cell_index < len(trials):
        raise ValueError(
            f"cell index must be in 0..{len(trials) - 1}"
        )
    run_trial(
        trials[args.cell_index],
        data_root=args.data_root,
        output_root=args.output_root,
    )


if __name__ == "__main__":
    main()
