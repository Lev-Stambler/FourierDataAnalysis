from __future__ import annotations

import hashlib
import json
import math
import os
import time
from pathlib import Path
from typing import Callable

import numpy as np
import torch
import torch.nn.functional as F

from .config import (
    AUDIT_EXAMPLES,
    EFFECTIVE_BATCH,
    MODEL_ID,
    MODEL_REVISION,
    TEST_EXAMPLES,
    TrialConfig,
    microbatch_for,
)
from .data import load_manifest, load_split
from .model import (
    FullWidthStudent,
    optimizer_adam_role_diagnostics,
    optimizer_parameter_groups,
    optimizer_role_diagnostics,
)
from .teacher_cache import (
    TeacherHiddenCache,
    make_cache_identity,
    reconstruct_logits,
)


ADAMW = {
    "betas": (0.9, 0.999),
    "eps": 1e-8,
    "weight_decay": 0.01,
    "fused": True,
}
EVALUATION_BATCH = 64


def lr_schedule_multiplier(trial: TrialConfig, step: int) -> float:
    """Return the scalar applied to every structure-aware optimizer-group LR."""
    if trial.lr <= 0:
        raise ValueError("learning rate must be positive")
    if trial.gradient_clip_norm < 0:
        raise ValueError("gradient clip norm cannot be negative")
    if trial.warmup_examples < 0:
        raise ValueError("warmup examples cannot be negative")
    if trial.warmup_examples and trial.warmup_steps:
        raise ValueError("set warmup in examples or steps, not both")
    if trial.cooldown_examples < 0:
        raise ValueError("cooldown examples cannot be negative")
    if trial.cooldown_examples and trial.cooldown_steps:
        raise ValueError("set cooldown in examples or steps, not both")
    if (
        trial.warmup_examples
        and trial.warmup_examples % trial.effective_batch
    ):
        raise ValueError("warmup examples must divide by effective batch")
    warmup_steps = (
        trial.warmup_examples // trial.effective_batch
        if trial.warmup_examples
        else trial.warmup_steps
    )
    if (
        trial.cooldown_examples
        and trial.cooldown_examples % trial.effective_batch
    ):
        raise ValueError("cooldown examples must divide by effective batch")
    cooldown_steps = (
        trial.cooldown_examples // trial.effective_batch
        if trial.cooldown_examples
        else trial.cooldown_steps
    )
    if trial.lr_schedule == "constant":
        if (
            warmup_steps != 0
            or cooldown_steps != 0
            or trial.min_lr_ratio != 1.0
        ):
            raise ValueError(
                "constant LR requires zero warmup/cooldown and min_lr_ratio=1"
            )
        return 1.0
    if trial.lr_schedule not in ("warmup_cosine", "warmup_hold", "wsd"):
        raise ValueError(f"unsupported LR schedule {trial.lr_schedule}")
    if warmup_steps <= 0:
        raise ValueError(
            f"{trial.lr_schedule.replace('_', '-')} requires "
            "positive warmup_steps"
        )
    if not 0.0 <= trial.min_lr_ratio <= 1.0:
        raise ValueError("min_lr_ratio must be between zero and one")
    if trial.lr_schedule != "wsd" and cooldown_steps:
        raise ValueError("cooldown is only supported by the WSD schedule")
    if trial.lr_schedule == "wsd" and cooldown_steps <= 0:
        raise ValueError("WSD requires a positive cooldown")
    schedule_start = (
        0
        if trial.warm_start_weights_only
        else trial.warm_start_resume_step or trial.warm_start_step
    )
    duration = trial.steps - schedule_start
    if (
        duration <= warmup_steps
        or (
            trial.lr_schedule == "wsd"
            and duration <= warmup_steps + cooldown_steps
        )
    ):
        raise ValueError(
            "warmup/cooldown must leave a stable scheduled training phase"
        )
    relative_step = step - schedule_start
    if not 1 <= relative_step <= duration:
        raise ValueError(
            f"step {step} is outside scheduled phase "
            f"{schedule_start + 1}..{trial.steps}"
        )
    if trial.lr_schedule == "warmup_hold":
        if relative_step > warmup_steps:
            return 1.0
        if warmup_steps == 1:
            return 1.0
        progress = (relative_step - 1) / (warmup_steps - 1)
        return trial.min_lr_ratio + (
            1.0 - trial.min_lr_ratio
        ) * progress
    if trial.lr_schedule == "wsd":
        if relative_step <= warmup_steps:
            return relative_step / warmup_steps
        stable_end = duration - cooldown_steps
        if relative_step <= stable_end:
            return 1.0
        return (duration - relative_step) / cooldown_steps
    if relative_step <= warmup_steps:
        return relative_step / warmup_steps
    progress = (
        (relative_step - warmup_steps)
        / (duration - warmup_steps)
    )
    cosine = 0.5 * (1.0 + math.cos(math.pi * progress))
    return trial.min_lr_ratio + (1.0 - trial.min_lr_ratio) * cosine


def stable_checkpoint_step(trial: TrialConfig) -> int | None:
    """Return the last stable WSD step, or ``None`` for other schedules."""
    if trial.lr_schedule != "wsd":
        return None
    cooldown_steps = (
        trial.cooldown_examples // trial.effective_batch
        if trial.cooldown_examples
        else trial.cooldown_steps
    )
    if cooldown_steps <= 0:
        raise ValueError("WSD requires a positive cooldown")
    return trial.steps - cooldown_steps


def checkpoint_due(trial: TrialConfig, step: int) -> bool:
    """Return whether this local optimizer step is a progress boundary."""
    checkpoint_examples = (
        trial.checkpoint_every_examples
        or (
            1_000 * EFFECTIVE_BATCH
            if trial.is_final
            else 0
        )
    )
    return bool(
        checkpoint_examples
        and step < trial.steps
        and (step * trial.effective_batch) % checkpoint_examples == 0
    )


def _validation_examples_for_step(
    trial: TrialConfig,
    step: int,
    validation_rows: int,
) -> int:
    if step == stable_checkpoint_step(trial):
        return validation_rows
    return min(AUDIT_EXAMPLES, 64 if trial.smoke else AUDIT_EXAMPLES)


def load_teacher(device: str = "cuda"):
    from transformers import AutoModelForImageTextToText, AutoTokenizer

    token = (
        os.environ.get("HF_TOKEN")
        or os.environ.get("HF_HUB_TOKEN")
        or os.environ.get("HUGGING_FACE_HUB_TOKEN")
    )
    tokenizer = AutoTokenizer.from_pretrained(
        MODEL_ID, revision=MODEL_REVISION, token=token
    )
    teacher = AutoModelForImageTextToText.from_pretrained(
        MODEL_ID,
        revision=MODEL_REVISION,
        token=token,
        dtype=torch.bfloat16,
        device_map=device,
        low_cpu_mem_usage=True,
    ).eval()
    teacher.requires_grad_(False)
    try:
        teacher.set_attn_implementation({"text_config": "sdpa"})
    except (AttributeError, TypeError, ValueError):
        pass
    input_weight = teacher.get_input_embeddings().weight
    output_weight = teacher.get_output_embeddings().weight
    if input_weight.data_ptr() != output_weight.data_ptr():
        raise RuntimeError("pinned Qwen checkpoint no longer has tied input/output weights")
    return teacher, tokenizer


def tensor_sha256(tensor: torch.Tensor, chunk_rows: int = 8192) -> str:
    digest = hashlib.sha256()
    value = tensor.detach()
    for lo in range(0, len(value), chunk_rows):
        # NumPy has no native bfloat16 dtype. Hash the exact underlying
        # 16-bit representation so this remains bitwise, not a lossy cast.
        raw = value[lo : lo + chunk_rows].contiguous().view(torch.uint16)
        digest.update(raw.cpu().numpy().tobytes())
    return digest.hexdigest()


def teacher_targets(
    teacher,
    token_ids: torch.Tensor,
    vocab_size: int,
) -> tuple[torch.Tensor, torch.Tensor]:
    with torch.inference_mode():
        output = teacher.model(
            input_ids=token_ids,
            use_cache=False,
            return_dict=True,
        )
        hidden = output.last_hidden_state[:, -1, :]
        logits = F.linear(
            hidden,
            teacher.get_output_embeddings().weight[:vocab_size],
        )
    return logits, hidden


def teacher_logits(teacher, token_ids: torch.Tensor, vocab_size: int) -> torch.Tensor:
    return teacher_targets(teacher, token_ids, vocab_size)[0]


def forward_kl_rows(
    teacher_value: torch.Tensor,
    student_value: torch.Tensor,
    *,
    temperature: float = 1.0,
) -> torch.Tensor:
    if not math.isfinite(temperature) or temperature <= 0:
        raise ValueError("distillation temperature must be positive and finite")
    teacher_logp = F.log_softmax(
        teacher_value.float() / temperature,
        dim=-1,
    )
    teacher_probability = teacher_logp.exp()
    student_logp = F.log_softmax(
        student_value.float() / temperature,
        dim=-1,
    )
    return (
        teacher_probability * (teacher_logp - student_logp)
    ).sum(-1)


def normalized_hidden_mse_rows(
    teacher_hidden: torch.Tensor,
    student_hidden: torch.Tensor,
) -> torch.Tensor:
    if teacher_hidden.shape != student_hidden.shape:
        raise ValueError("teacher/student hidden shapes must match")
    numerator = (
        student_hidden.float() - teacher_hidden.float()
    ).square().mean(-1)
    denominator = teacher_hidden.float().square().mean(-1).clamp_min(1e-8)
    return numerator / denominator


def distribution_rows(
    teacher_value: torch.Tensor,
    student_value: torch.Tensor,
    target: torch.Tensor,
) -> dict[str, torch.Tensor]:
    teacher_logp = F.log_softmax(teacher_value.float(), dim=-1)
    teacher_probability = teacher_logp.exp()
    student_logp = F.log_softmax(student_value.float(), dim=-1)
    kl = forward_kl_rows(teacher_value, student_value)
    teacher_entropy = -(teacher_probability * teacher_logp).sum(-1)
    teacher_top = teacher_value.argmax(-1)
    student_order = student_value.topk(10, dim=-1).indices
    teacher_cross_entropy = -teacher_logp.gather(
        1, target[:, None]
    ).squeeze(1)
    student_cross_entropy = -student_logp.gather(
        1, target[:, None]
    ).squeeze(1)
    return {
        "kl": kl,
        "teacher_entropy": teacher_entropy,
        "teacher_cross_entropy": teacher_cross_entropy,
        "student_cross_entropy": student_cross_entropy,
        # Retained compatibility aliases.
        "teacher_nll": teacher_cross_entropy,
        "student_nll": student_cross_entropy,
        "top1": (student_order[:, 0] == teacher_top).float(),
        "top5": (student_order[:, :5] == teacher_top[:, None]).any(-1).float(),
        "top10": (student_order == teacher_top[:, None]).any(-1).float(),
        "student_entropy": -(student_logp.exp() * student_logp).sum(-1),
    }


def _summarize(rows: dict[str, list[torch.Tensor]]) -> dict[str, float]:
    result: dict[str, float] = {}
    for key, parts in rows.items():
        values = torch.cat(parts).double()
        result[key] = float(values.mean())
        if key == "kl":
            for quantile in (0.5, 0.9, 0.99):
                result[f"kl_p{int(100 * quantile)}"] = float(
                    torch.quantile(values, quantile)
                )
    result["student_perplexity"] = math.exp(
        min(result["student_cross_entropy"], 30.0)
    )
    result["teacher_perplexity"] = math.exp(
        min(result["teacher_cross_entropy"], 30.0)
    )
    return result


@torch.inference_mode()
def evaluate(
    teacher,
    student,
    contexts: np.ndarray,
    targets: np.ndarray,
    *,
    examples: int,
    batch_size: int,
    vocab_size: int,
    device: str,
    forward_fn: Callable[[torch.Tensor], torch.Tensor] | None = None,
) -> dict[str, float]:
    student.eval()
    rows: dict[str, list[torch.Tensor]] = {}
    count = min(examples, len(contexts))
    for lo in range(0, count, batch_size):
        hi = min(lo + batch_size, count)
        ids = torch.from_numpy(np.array(contexts[lo:hi], copy=True)).to(
            device=device, dtype=torch.long
        )
        target = torch.from_numpy(np.array(targets[lo:hi], copy=True)).to(
            device=device, dtype=torch.long
        )
        teacher_value = teacher_logits(teacher, ids, vocab_size)
        with torch.autocast(device_type="cuda", dtype=torch.bfloat16):
            student_value = (
                forward_fn(ids) if forward_fn is not None else student(ids)
            )
        for key, value in distribution_rows(
            teacher_value, student_value, target
        ).items():
            rows.setdefault(key, []).append(value.cpu())
    student.train()
    return _summarize(rows)


def parameter_metrics(model: torch.nn.Module) -> dict[str, float]:
    parameter_sq = 0.0
    gradient_sq = 0.0
    gradient_max = 0.0
    nonfinite = 0
    for parameter in model.parameters():
        parameter_sq += float(parameter.detach().float().square().sum())
        if parameter.grad is not None:
            gradient = parameter.grad.detach().float()
            gradient_sq += float(gradient.square().sum())
            gradient_max = max(gradient_max, float(gradient.abs().max()))
            nonfinite += int((~torch.isfinite(gradient)).sum())
    return {
        "diagnostic/parameter_norm": math.sqrt(parameter_sq),
        "diagnostic/gradient_norm": math.sqrt(gradient_sq),
        "diagnostic/gradient_abs_max": gradient_max,
        "diagnostic/nonfinite_gradients": nonfinite,
    }


def _wandb_init(
    trial: TrialConfig,
    parameters: int,
    frozen: int,
    optimizer_groups: list[dict],
    dataset_manifest: dict,
    wandb_dir: Path,
    resume_step: int = 0,
    examples_seen: int = 0,
    optimizer_steps: int = 0,
    optimizer_state_resumed: bool = False,
):
    import wandb

    config = trial.to_dict()
    if trial.hydra_resolved_config_json:
        config["hydra_resolved_config"] = json.loads(
            trial.hydra_resolved_config_json
        )
    objective_name = (
        "full_vocab_forward_kl"
        if (
            trial.temperature2_weight == 0.0
            and trial.hidden_mse_weight == 0.0
        )
        else "weighted_forward_kl_softened_kl_hidden_mse"
    )
    config.update({
        "model_id": MODEL_ID,
        "model_revision": MODEL_REVISION,
        "objective": objective_name,
        "temperature": trial.temperature,
        "temperature2_weight": trial.temperature2_weight,
        "hidden_mse_weight": trial.hidden_mse_weight,
        "effective_batch": trial.effective_batch,
        "adamw": {
            "lr": trial.lr,
            "betas": list(ADAMW["betas"]),
            "eps": ADAMW["eps"],
            "weight_decay": ADAMW["weight_decay"],
        },
        "trainable_parameters": parameters,
        "frozen_tied_parameters": frozen,
        "resumed_from_step": resume_step,
        "examples_seen_at_start": examples_seen,
        "input_tokens_seen_at_start":
            examples_seen * trial.architecture.context_length,
        "optimizer_steps_at_start": optimizer_steps,
        "optimizer_state_resumed": optimizer_state_resumed,
        "lr_parameterization": trial.lr_parameterization,
        "optimizer_groups": optimizer_groups,
        "dataset_manifest": dataset_manifest,
        "physical_depth": trial.architecture.depth,
        "repetitions": trial.architecture.repetitions,
        "effective_depth": trial.architecture.effective_depth,
        "residual_multiplier": trial.architecture.residual_multiplier,
    })
    if trial.architecture.operator in ("kronecker", "hybrid"):
        config.update({
            "kronecker_backend": os.environ.get(
                "QWEN_KRONECKER_BACKEND", "torch"
            ),
            "runtime_kronecker_rank_chunk": int(os.environ.get(
                "QWEN_KRONECKER_RANK_CHUNK",
                trial.architecture.kronecker_rank_chunk,
            )),
            "runtime_kronecker_microbatch": microbatch_for(
                trial.architecture, trial.effective_batch
            )[0],
            "runtime_kronecker_microbatch_limit": int(os.environ.get(
                "QWEN_KRONECKER_MICROBATCH", 64
            )),
        })
    # Modal functions have a 24-hour execution horizon. Long token-budget
    # trials resume from periodic checkpoints in a retry, so give W&B a stable
    # identity as well; otherwise every timeout would fragment one scientific
    # run into multiple dashboards.
    wandb_id = hashlib.sha256(
        (
            f"qwen-fullwidth:{trial.label}:"
            f"{trial.hydra_config_hash or 'legacy'}"
        ).encode("utf-8")
    ).hexdigest()[:16]
    return wandb.init(
        project="qwen-fullwidth-monarch-distill",
        group=trial.stage,
        name=trial.label,
        job_type=trial.stage,
        id=wandb_id,
        resume="allow",
        config=config,
        dir=str(wandb_dir),
    )


def _save_checkpoint(
    path: Path,
    student: FullWidthStudent,
    trial: TrialConfig,
    embedding_hash: str,
    metrics: dict,
    optimizer: torch.optim.Optimizer | None = None,
    *,
    step: int | None = None,
    examples_seen: int | None = None,
    input_tokens_seen: int | None = None,
    optimizer_steps: int | None = None,
    stable_validation: dict[str, float] | None = None,
) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    step = int(metrics.get("steps_completed", 0) if step is None else step)
    examples_seen = int(
        metrics.get("examples_seen", step * trial.effective_batch)
        if examples_seen is None
        else examples_seen
    )
    input_tokens_seen = int(
        metrics.get(
            "input_tokens_seen",
            examples_seen * trial.architecture.context_length,
        )
        if input_tokens_seen is None
        else input_tokens_seen
    )
    optimizer_steps = int(
        metrics.get("optimizer_steps", step)
        if optimizer_steps is None
        else optimizer_steps
    )
    value = {
        "schema": "qwen-fullwidth-checkpoint-v3",
        "state_dict": student.state_dict(),
        "trial": trial.to_dict(),
        "model_id": MODEL_ID,
        "model_revision": MODEL_REVISION,
        "embedding_sha256": embedding_hash,
        "metrics": metrics,
        "step": step,
        "examples_seen": examples_seen,
        "input_tokens_seen": input_tokens_seen,
        "optimizer_steps": optimizer_steps,
        "optimizer_state_included": optimizer is not None,
    }
    if stable_validation is not None:
        value["stable_validation"] = stable_validation
    if optimizer is not None:
        value["optimizer_state_dict"] = optimizer.state_dict()
    torch.save(value, temporary)
    os.replace(temporary, path)


def _final_checkpoint_optimizer(
    trial: TrialConfig,
    optimizer: torch.optim.Optimizer,
) -> torch.optim.Optimizer | None:
    """Retain AdamW state for every exact-resume boundary."""
    return optimizer if trial.is_final else None


def _save_progress_checkpoint(
    path: Path,
    student: FullWidthStudent,
    optimizer: torch.optim.Optimizer,
    trial: TrialConfig,
    embedding_hash: str,
    step: int,
    initial_validation: dict[str, float],
    elapsed_wall_seconds: float,
    *,
    examples_seen: int | None = None,
    input_tokens_seen: int | None = None,
    optimizer_steps: int | None = None,
    stable_validation: dict[str, float] | None = None,
) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    examples_seen = (
        step * trial.effective_batch
        if examples_seen is None
        else int(examples_seen)
    )
    input_tokens_seen = (
        examples_seen * trial.architecture.context_length
        if input_tokens_seen is None
        else int(input_tokens_seen)
    )
    optimizer_steps = step if optimizer_steps is None else int(optimizer_steps)
    value = {
        "schema": "qwen-fullwidth-checkpoint-v3",
        "state_dict": student.state_dict(),
        "optimizer_state_dict": optimizer.state_dict(),
        "trial": trial.to_dict(),
        "model_id": MODEL_ID,
        "model_revision": MODEL_REVISION,
        "embedding_sha256": embedding_hash,
        "step": step,
        "examples_seen": examples_seen,
        "input_tokens_seen": input_tokens_seen,
        "optimizer_steps": optimizer_steps,
        "optimizer_state_included": True,
        "initial_validation": initial_validation,
        "elapsed_wall_seconds": elapsed_wall_seconds,
    }
    if stable_validation is not None:
        value["stable_validation"] = stable_validation
    torch.save(
        value,
        temporary,
    )
    os.replace(temporary, path)


def _checkpoint_counters(
    checkpoint: dict,
    *,
    schema: str,
    source_trial: dict,
    step: int,
) -> tuple[int, int, int]:
    if schema == "qwen-fullwidth-checkpoint-v3":
        examples_seen = int(checkpoint.get("examples_seen", -1))
        input_tokens_seen = int(checkpoint.get("input_tokens_seen", -1))
        optimizer_steps = int(checkpoint.get("optimizer_steps", -1))
        context_length = int(
            source_trial.get("architecture", {}).get("context_length", 16)
        )
        if examples_seen < 0 or optimizer_steps < 0:
            raise RuntimeError("checkpoint has invalid training counters")
        if (
            context_length <= 0
            or input_tokens_seen != examples_seen * context_length
        ):
            raise RuntimeError("checkpoint has invalid input-token counter")
        return examples_seen, input_tokens_seen, optimizer_steps
    source_batch = int(source_trial.get("effective_batch", EFFECTIVE_BATCH))
    context_length = int(
        source_trial.get("architecture", {}).get("context_length", 16)
    )
    examples_seen = step * source_batch
    return examples_seen, examples_seen * context_length, step


def _load_progress_checkpoint(
    path: Path,
    student: FullWidthStudent,
    optimizer: torch.optim.Optimizer,
    trial: TrialConfig,
    embedding_hash: str,
) -> dict | None:
    if not path.exists():
        return None
    checkpoint = torch.load(
        path,
        map_location="cpu",
        mmap=True,
        weights_only=True,
    )
    checkpoint_trial = checkpoint.get("trial")
    expected_trial = trial.to_dict()
    if not isinstance(checkpoint_trial, dict):
        raise RuntimeError(f"{trial.label}: progress checkpoint mismatched trial")
    # Checkpoint cadence is operational metadata: changing it does not alter
    # model weights, optimizer updates, or data order. Permit a retained exact
    # state to resume under a safer cadence after an interruption.
    checkpoint_trial = dict(checkpoint_trial)
    checkpoint_trial.pop("checkpoint_every_examples", None)
    expected_trial.pop("checkpoint_every_examples", None)
    if checkpoint_trial != expected_trial:
        raise RuntimeError(f"{trial.label}: progress checkpoint mismatched trial")
    schema = str(checkpoint.get("schema"))
    if schema not in (
        "qwen-fullwidth-progress-v2",
        "qwen-fullwidth-checkpoint-v3",
    ):
        raise RuntimeError(
            f"{trial.label}: unsupported progress checkpoint schema {schema}"
        )
    expected = {
        "model_id": MODEL_ID,
        "model_revision": MODEL_REVISION,
        "embedding_sha256": embedding_hash,
    }
    for key, value in expected.items():
        if checkpoint.get(key) != value:
            raise RuntimeError(f"{trial.label}: progress checkpoint mismatched {key}")
    step = int(checkpoint.get("step", 0))
    if not 0 < step < trial.steps:
        raise RuntimeError(f"{trial.label}: invalid progress checkpoint step {step}")
    state = checkpoint.get("state_dict")
    if not isinstance(state, dict) or not state:
        raise RuntimeError(f"{trial.label}: empty progress checkpoint state")
    student.load_state_dict(state, strict=True)
    optimizer_state = checkpoint.get("optimizer_state_dict")
    if not isinstance(optimizer_state, dict) or not optimizer_state.get("state"):
        raise RuntimeError(f"{trial.label}: empty progress optimizer state")
    optimizer.load_state_dict(optimizer_state)
    examples_seen, input_tokens_seen, optimizer_steps = _checkpoint_counters(
        checkpoint,
        schema=schema,
        source_trial=checkpoint_trial,
        step=step,
    )
    initial_validation = checkpoint.get("initial_validation")
    if (
        not isinstance(initial_validation, dict)
        or not math.isfinite(float(initial_validation.get("kl", math.nan)))
    ):
        raise RuntimeError(
            f"{trial.label}: invalid progress initial validation"
        )
    elapsed = float(checkpoint.get("elapsed_wall_seconds", math.nan))
    if not math.isfinite(elapsed) or elapsed < 0:
        raise RuntimeError(f"{trial.label}: invalid progress elapsed wall time")
    return {
        "step": step,
        "examples_seen": examples_seen,
        "input_tokens_seen": input_tokens_seen,
        "optimizer_steps": optimizer_steps,
        "initial_validation": initial_validation,
        "elapsed_wall_seconds": elapsed,
        "stable_validation": checkpoint.get("stable_validation"),
        "optimizer_state_resumed": True,
    }


def _load_warm_start_checkpoint(
    path: Path,
    student: FullWidthStudent,
    optimizer: torch.optim.Optimizer,
    trial: TrialConfig,
    embedding_hash: str,
) -> dict:
    if not path.is_file():
        raise RuntimeError(f"missing warm-start checkpoint: {path}")
    checkpoint = torch.load(
        path,
        map_location="cpu",
        mmap=True,
        weights_only=True,
    )
    schema = checkpoint.get("schema")
    source_trial = checkpoint.get("trial", {})
    desired_group_policy = [
        (float(group["lr"]), float(group["weight_decay"]))
        for group in optimizer.param_groups
    ]
    expected = {
        "model_id": MODEL_ID,
        "model_revision": MODEL_REVISION,
        "embedding_sha256": embedding_hash,
    }
    for key, value in expected.items():
        if checkpoint.get(key) != value:
            raise RuntimeError(f"warm-start checkpoint mismatched {key}")
    if path.parent.name != trial.warm_start_label:
        raise RuntimeError("warm-start checkpoint path label mismatch")
    if schema == "qwen-fullwidth-student-v1":
        source_metrics = checkpoint.get("metrics", {})
        if source_metrics.get("label") != trial.warm_start_label:
            raise RuntimeError("warm-start checkpoint label mismatch")
        if source_metrics.get("architecture") != trial.architecture.to_dict():
            raise RuntimeError("warm-start architecture mismatch")
        if (
            float(source_metrics.get("lr", math.nan)) != trial.lr
            and not trial.warm_start_lr_override
        ):
            raise RuntimeError("warm-start LR mismatch")
        if int(source_metrics.get("seed", -1)) != trial.seed:
            raise RuntimeError("warm-start seed mismatch")
        if int(source_metrics.get("steps_completed", -1)) != (
            trial.warm_start_step
        ):
            raise RuntimeError("warm-start step mismatch")
        initial_validation = source_metrics["validation"]
        checkpoint_step = trial.warm_start_step
    elif schema in (
        "qwen-fullwidth-progress-v2",
        "qwen-fullwidth-checkpoint-v3",
    ):
        source_architecture = dict(source_trial.get("architecture", {}))
        source_architecture.pop("label", None)
        if source_architecture != trial.architecture.to_dict():
            raise RuntimeError("warm-start architecture mismatch")
        if (
            float(source_trial.get("lr", math.nan)) != trial.lr
            and not trial.warm_start_lr_override
        ):
            raise RuntimeError("warm-start LR mismatch")
        if int(source_trial.get("seed", -1)) != trial.seed:
            raise RuntimeError("warm-start seed mismatch")
        checkpoint_step = int(checkpoint.get("step", -1))
        if checkpoint_step != trial.warm_start_step:
            raise RuntimeError("warm-start step mismatch")
        initial_validation = (
            checkpoint.get("stable_validation")
            or checkpoint.get("metrics", {}).get("validation")
            or checkpoint.get("initial_validation")
        )
        if (
            not isinstance(initial_validation, dict)
            or not math.isfinite(
                float(initial_validation.get("kl", math.nan))
            )
        ):
            raise RuntimeError("warm-start initial validation is invalid")
    else:
        raise RuntimeError(f"unsupported warm-start checkpoint schema {schema}")
    if source_trial.get("architecture", {}).get("label") != (
        trial.architecture.label
    ):
        raise RuntimeError("warm-start serialized architecture mismatch")
    examples_seen, input_tokens_seen, source_optimizer_steps = (
        _checkpoint_counters(
            checkpoint,
            schema=schema,
            source_trial=source_trial,
            step=checkpoint_step,
        )
    )
    resume_step = 0 if trial.warm_start_weights_only else (
        trial.warm_start_resume_step or trial.warm_start_step
    )
    if not trial.warm_start_weights_only:
        if resume_step <= 0:
            raise RuntimeError("warm-start resume step must be positive")
        if schema == "qwen-fullwidth-checkpoint-v3":
            if resume_step != source_optimizer_steps:
                raise RuntimeError(
                    "v3 warm-start resume step must match optimizer age"
                )
        elif resume_step * trial.effective_batch != examples_seen:
            raise RuntimeError(
                "warm-start resume step does not preserve example count"
            )
        optimizer_state = checkpoint.get("optimizer_state_dict")
        if (
            checkpoint.get("optimizer_state_included") is not True
            or not isinstance(optimizer_state, dict)
            or not optimizer_state.get("state")
        ):
            raise RuntimeError(
                "warm-start checkpoint lacks AdamW optimizer state"
            )
    elif trial.warm_start_resume_step:
        raise RuntimeError(
            "weights-only warm starts reset local optimizer/schedule steps"
        )
    student.load_state_dict(checkpoint["state_dict"], strict=True)
    if trial.warm_start_weights_only:
        optimizer.state.clear()
    else:
        optimizer.load_state_dict(checkpoint["optimizer_state_dict"])
    if trial.warm_start_lr_override and not trial.warm_start_weights_only:
        if len(optimizer.param_groups) != len(desired_group_policy):
            raise RuntimeError("warm-start optimizer group count mismatch")
        for group, (desired_lr, desired_weight_decay) in zip(
            optimizer.param_groups, desired_group_policy, strict=True
        ):
            group["lr"] = desired_lr
            group["weight_decay"] = desired_weight_decay
    result = {
        "step": resume_step,
        "examples_seen": examples_seen,
        "input_tokens_seen": input_tokens_seen,
        "optimizer_steps": (
            0 if trial.warm_start_weights_only else source_optimizer_steps
        ),
        "initial_validation": initial_validation,
        "elapsed_wall_seconds": 0.0,
        "warm_started": True,
        "optimizer_state_resumed": not trial.warm_start_weights_only,
        "stable_validation": checkpoint.get("stable_validation"),
    }
    if trial.warm_start_lr_override:
        result["lr_overridden"] = True
    return result


def _training_order(
    rows: int,
    required: int,
    seed: int,
    allow_reuse: bool,
) -> np.ndarray:
    if required <= rows:
        return np.random.default_rng(seed).permutation(rows)[:required]
    if not allow_reuse:
        raise ValueError(
            f"trial needs {required} train rows, only {rows} exist"
        )
    order = np.empty(required, dtype=np.int64)
    cursor = 0
    epoch = 0
    while cursor < required:
        permutation = np.random.default_rng(seed + epoch).permutation(rows)
        count = min(rows, required - cursor)
        order[cursor : cursor + count] = permutation[:count]
        cursor += count
        epoch += 1
    return order


def _microbatch_chunks(
    effective_batch: int,
    physical_microbatch: int,
) -> tuple[int, ...]:
    """Partition one optimizer batch, retaining a possibly smaller tail."""
    if effective_batch <= 0 or physical_microbatch <= 0:
        raise ValueError("effective and physical batches must be positive")
    return tuple(
        min(physical_microbatch, effective_batch - start)
        for start in range(0, effective_batch, physical_microbatch)
    )


def _load_completed_result(output: Path, trial: TrialConfig) -> dict | None:
    """Return a committed trial result only when it matches the requested run."""
    result_path = output / "result.json"
    checkpoint_path = output / "student.pt"
    try:
        result = json.loads(result_path.read_text())
    except (FileNotFoundError, json.JSONDecodeError, OSError):
        return None

    expected = {
        "label": trial.label,
        "architecture": trial.architecture.to_dict(),
        "lr": trial.lr,
        "seed": trial.seed,
        "steps": trial.steps,
    }
    if int(result.get("effective_batch", EFFECTIVE_BATCH)) != trial.effective_batch:
        return None
    if result.get("lr_parameterization", "uniform") != trial.lr_parameterization:
        return None
    if any(result.get(key) != value for key, value in expected.items()):
        return None
    status = result.get("status", "complete")
    if status not in ("complete", "diverged"):
        return None
    if status == "diverged" and not trial.allow_divergence:
        return None
    validation = result.get("validation")
    if (
        not isinstance(validation, dict)
        or not math.isfinite(float(validation.get("kl", math.nan)))
    ):
        return None
    embedding_hash = result.get("embedding_sha256")
    if not isinstance(embedding_hash, str) or len(embedding_hash) != 64:
        return None
    if status == "complete" and (trial.is_final or trial.smoke):
        if not isinstance(result.get("test"), dict) or not result["test"]:
            return None
        try:
            if checkpoint_path.stat().st_size <= 0:
                return None
        except OSError:
            return None
    return result


def _run_trial_impl(
    trial: TrialConfig,
    *,
    data_root: str,
    output_root: str,
    device: str = "cuda",
    checkpoint_callback: Callable[[], None] | None = None,
    teacher_cache_root: str | None = None,
) -> dict:
    import wandb

    if trial.objective != "forward_kl":
        raise ValueError(
            "the distillation runner only supports objective='forward_kl'; "
            "use qwen_fullwidth_distill.pretrain for next-token CE"
        )
    if trial.embedding_initialization != "frozen_qwen":
        raise ValueError(
            "the distillation runner requires frozen_qwen embeddings"
        )
    trial.architecture.validate()
    if not math.isfinite(trial.temperature) or trial.temperature < 1.0:
        raise ValueError("temperature must be finite and at least one")
    if not 0.0 <= trial.temperature2_weight <= 1.0:
        raise ValueError("temperature2_weight must be between zero and one")
    if trial.temperature2_weight and trial.temperature == 1.0:
        raise ValueError("a softened-KL weight requires temperature > 1")
    if not math.isfinite(trial.hidden_mse_weight) or trial.hidden_mse_weight < 0:
        raise ValueError("hidden_mse_weight must be finite and nonnegative")
    if trial.hydra_resolved_config_json:
        try:
            resolved_hydra = json.loads(trial.hydra_resolved_config_json)
        except json.JSONDecodeError as error:
            raise ValueError("resolved Hydra config must be valid JSON") from error
        if not isinstance(resolved_hydra, dict):
            raise ValueError("resolved Hydra config must be a JSON object")
    if bool(trial.hydra_config_hash) != bool(
        trial.hydra_resolved_config_json
    ):
        raise ValueError(
            "Hydra config hash and resolved config must be provided together"
        )
    if trial.hydra_resolved_config_json:
        try:
            canonical_hydra = json.dumps(
                resolved_hydra,
                sort_keys=True,
                separators=(",", ":"),
                ensure_ascii=False,
                allow_nan=False,
            )
        except (TypeError, ValueError) as error:
            raise ValueError(
                "resolved Hydra config must contain finite JSON values"
            ) from error
        resolved_hash = hashlib.sha256(
            canonical_hydra.encode("utf-8")
        ).hexdigest()
        if trial.hydra_config_hash != resolved_hash:
            raise ValueError("resolved Hydra config SHA-256 mismatch")
    # Validate optimizer policy before accepting either a cache hit or an H100
    # allocation. The last step exercises every schedule's endpoint contract.
    lr_schedule_multiplier(trial, trial.steps)
    output = Path(output_root) / trial.stage / trial.label
    cached = _load_completed_result(output, trial)
    if cached is not None:
        print(f"[cache] {trial.label}", flush=True)
        return cached
    output.mkdir(parents=True, exist_ok=True)

    torch.manual_seed(trial.seed)
    np.random.seed(trial.seed)
    torch.set_float32_matmul_precision("high")
    torch.backends.cuda.matmul.allow_tf32 = True
    torch.backends.cudnn.allow_tf32 = True
    torch.cuda.reset_peak_memory_stats()

    dataset_manifest = load_manifest(data_root)
    train_contexts, train_targets, _ = load_split(data_root, "train")
    val_contexts, val_targets, _ = load_split(data_root, "validation")
    test_contexts, test_targets, _ = load_split(data_root, "test")

    teacher, tokenizer = load_teacher(device)
    vocab_size = len(tokenizer)
    teacher_input = teacher.get_input_embeddings().weight
    embedding_hash = tensor_sha256(teacher_input)
    frozen_embedding = teacher_input.detach().clone()
    student = FullWidthStudent(
        trial.architecture, frozen_embedding, vocab_size=vocab_size
    ).to(device)
    teacher_hidden_cache = None
    if trial.use_teacher_cache:
        if not teacher_cache_root:
            raise ValueError("cached-teacher trial requires teacher_cache_root")
        cache_identity = make_cache_identity(
            model_id=MODEL_ID,
            model_revision=MODEL_REVISION,
            embedding_sha256=embedding_hash,
            dataset_manifest=dataset_manifest,
            context_length=trial.architecture.context_length,
        )
        teacher_hidden_cache = TeacherHiddenCache(
            teacher_cache_root,
            expected_identity=cache_identity,
            validate_shards=False,
        )
    if student.tied_embedding.requires_grad:
        raise RuntimeError("frozen embedding unexpectedly requires gradients")
    parameter_groups, optimizer_group_metadata = optimizer_parameter_groups(
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
    progress_path = output / "progress.pt"
    resume = _load_progress_checkpoint(
        progress_path, student, optimizer, trial, embedding_hash
    )
    warm_started = False
    if (
        (
            trial.warm_start_resume_step
            or trial.warm_start_weights_only
            or trial.warm_start_from_stable
        )
        and not trial.warm_start_label
    ):
        raise ValueError(
            "warm-start policy requires a warm-start checkpoint"
        )
    if resume is None and trial.warm_start_label:
        if (
            not trial.warm_start_stage
            or trial.warm_start_step <= 0
        ):
            raise ValueError("incomplete warm-start configuration")
        warm_path = (
            Path(output_root)
            / trial.warm_start_stage
            / trial.warm_start_label
            / (
                "stable.pt"
                if trial.warm_start_from_stable
                else "student.pt"
            )
        )
        resume = _load_warm_start_checkpoint(
            warm_path,
            student,
            optimizer,
            trial,
            embedding_hash,
        )
        warm_started = True
    resume_step = int(resume["step"]) if resume is not None else 0
    if resume_step < 0 or resume_step >= trial.steps:
        raise RuntimeError(
            f"resume step {resume_step} must be below target {trial.steps}"
        )
    prior_wall_seconds = (
        float(resume["elapsed_wall_seconds"]) if resume is not None else 0.0
    )
    examples_seen = (
        int(resume["examples_seen"]) if resume is not None else 0
    )
    input_tokens_seen = (
        int(resume["input_tokens_seen"]) if resume is not None else 0
    )
    optimizer_steps_completed = (
        int(resume["optimizer_steps"]) if resume is not None else 0
    )
    remaining_steps = trial.steps - resume_step
    required = examples_seen + remaining_steps * trial.effective_batch
    order = _training_order(
        len(train_contexts),
        required,
        trial.seed,
        trial.allow_data_reuse,
    )
    if resume is not None:
        print(
            f"[resume] {trial.label} local_step={resume_step}/{trial.steps} "
            f"examples_seen={examples_seen} "
            f"optimizer_steps={optimizer_steps_completed} "
            f"adam_resumed={bool(resume.get('optimizer_state_resumed', True))}",
            flush=True,
        )
    trainable = student.trainable_parameter_count()
    microbatch, accumulation = microbatch_for(
        trial.architecture, trial.effective_batch
    )
    wandb_dir = output / "wandb"
    wandb_dir.mkdir(parents=True, exist_ok=True)
    run = _wandb_init(
        trial,
        trainable,
        frozen_embedding.numel(),
        optimizer_group_metadata,
        dataset_manifest,
        wandb_dir,
        resume_step=resume_step,
        examples_seen=examples_seen,
        optimizer_steps=optimizer_steps_completed,
        optimizer_state_resumed=bool(
            resume is not None
            and resume.get("optimizer_state_resumed", True)
        ),
    )
    print(f"[wandb] {run.url}", flush=True)
    if (
        trial.checkpoint_every_examples
        and trial.checkpoint_every_examples % trial.effective_batch
    ):
        raise ValueError(
            "checkpoint interval must divide evenly by effective batch"
        )

    current_validation = evaluate(
        teacher, student, val_contexts, val_targets,
        examples=min(AUDIT_EXAMPLES, 64 if trial.smoke else AUDIT_EXAMPLES),
        batch_size=EVALUATION_BATCH, vocab_size=vocab_size, device=device,
    )
    run.log(
        {f"validation/{k}": v for k, v in current_validation.items()},
        step=resume_step,
    )
    initial_validation = (
        resume["initial_validation"]
        if resume is not None
        else current_validation
    )
    stable_validation = (
        resume.get("stable_validation") if resume is not None else None
    )
    stable_boundary = stable_checkpoint_step(trial)
    # A target-triggered early exit is a scientific result, not merely a
    # monitoring signal. Confirm it against the complete validation split;
    # the cheaper AUDIT_EXAMPLES estimate is only allowed to trigger this
    # confirmation.
    if (
        trial.target_validation_kl is not None
        and current_validation["kl"] <= trial.target_validation_kl
        and len(val_contexts) > min(
            AUDIT_EXAMPLES, 64 if trial.smoke else AUDIT_EXAMPLES
        )
    ):
        current_validation = evaluate(
            teacher, student, val_contexts, val_targets,
            examples=len(val_contexts), batch_size=EVALUATION_BATCH,
            vocab_size=vocab_size, device=device,
        )
        run.log(
            {
                f"target_validation/{key}": value
                for key, value in current_validation.items()
            },
            step=resume_step,
        )
    last_validation = current_validation
    cursor = examples_seen
    wall_start = time.perf_counter()
    compiled_forward = (
        torch.compile(student, dynamic=False)
        if trial.compile_model
        else None
    )
    initial_activation_rms = None
    max_activation_growth = 1.0
    divergence_reason = None
    completed_steps = resume_step
    target_reached = (
        trial.target_validation_kl is not None
        and current_validation["kl"] <= trial.target_validation_kl
    )
    step_times: list[float] = []
    for step in (
        range(resume_step + 1, trial.steps + 1)
        if not target_reached
        else ()
    ):
        step_start = time.perf_counter()
        optimizer.zero_grad(set_to_none=True)
        totals = {
            "kl": 0.0,
            "teacher_entropy": 0.0,
            "teacher_cross_entropy": 0.0,
            "student_cross_entropy": 0.0,
            "teacher_nll": 0.0,
            "student_nll": 0.0,
        }
        diagnostic_step = (
            step == 1 or step % trial.audit_every == 0 or step == trial.steps
        )
        student.collect_activation_diagnostics(False)
        step_cursor_start = cursor
        chunks = _microbatch_chunks(trial.effective_batch, microbatch)
        for microbatch_index, chunk_size in enumerate(chunks):
            chunk_weight = chunk_size / trial.effective_batch
            collect_this_microbatch = (
                diagnostic_step and microbatch_index == 0
            )
            student.collect_activation_diagnostics(collect_this_microbatch)
            indices = order[cursor : cursor + chunk_size]
            cursor += chunk_size
            ids = torch.from_numpy(np.array(train_contexts[indices], copy=True)).to(
                device=device, dtype=torch.long
            )
            target = torch.from_numpy(np.array(train_targets[indices], copy=True)).to(
                device=device, dtype=torch.long
            )
            if teacher_hidden_cache is None:
                target_logits, target_hidden = teacher_targets(
                    teacher, ids, vocab_size
                )
            else:
                target_hidden = teacher_hidden_cache.take(
                    "train",
                    indices,
                    device=device,
                )
                target_logits = reconstruct_logits(
                    target_hidden,
                    student.tied_embedding,
                    vocab_size=vocab_size,
                )
            with torch.autocast(device_type="cuda", dtype=torch.bfloat16):
                predicted_hidden = student.hidden(ids)
                predicted_logits = F.linear(
                    predicted_hidden,
                    student.tied_embedding[:vocab_size],
                )
            student.collect_activation_diagnostics(False)
            rows = distribution_rows(target_logits, predicted_logits, target)
            objective_rows = rows["kl"]
            if trial.temperature2_weight:
                softened_kl = forward_kl_rows(
                    target_logits,
                    predicted_logits,
                    temperature=trial.temperature,
                ) * trial.temperature**2
                objective_rows = (
                    (1.0 - trial.temperature2_weight) * objective_rows
                    + trial.temperature2_weight * softened_kl
                )
                totals.setdefault("softened_kl", 0.0)
                totals["softened_kl"] += (
                    float(softened_kl.detach().mean()) * chunk_weight
                )
            if trial.hidden_mse_weight:
                hidden_mse = normalized_hidden_mse_rows(
                    target_hidden,
                    predicted_hidden,
                )
                objective_rows = (
                    objective_rows
                    + trial.hidden_mse_weight * hidden_mse
                )
                totals.setdefault("hidden_mse", 0.0)
                totals["hidden_mse"] += (
                    float(hidden_mse.detach().mean()) * chunk_weight
                )
            loss = objective_rows.mean() * chunk_weight
            if not torch.isfinite(loss):
                divergence_reason = f"nonfinite training loss at step {step}"
                break
            loss.backward()
            totals["kl"] += (
                float(rows["kl"].detach().mean()) * chunk_weight
            )
            totals["teacher_entropy"] += (
                float(rows["teacher_entropy"].detach().mean()) * chunk_weight
            )
            totals["teacher_cross_entropy"] += (
                float(rows["teacher_cross_entropy"].detach().mean())
                * chunk_weight
            )
            totals["student_cross_entropy"] += (
                float(rows["student_cross_entropy"].detach().mean())
                * chunk_weight
            )
            totals["teacher_nll"] += (
                float(rows["teacher_nll"].detach().mean()) * chunk_weight
            )
            totals["student_nll"] += (
                float(rows["student_nll"].detach().mean()) * chunk_weight
            )
            totals.setdefault("objective", 0.0)
            totals["objective"] += (
                float(objective_rows.detach().mean()) * chunk_weight
            )
        if cursor - step_cursor_start != trial.effective_batch:
            raise RuntimeError("microbatch chunks changed the effective batch")
        student.collect_activation_diagnostics(False)
        if divergence_reason is not None:
            if not trial.allow_divergence:
                raise RuntimeError(divergence_reason)
            break
        lr_multiplier = lr_schedule_multiplier(trial, step)
        actual_lrs = []
        for group, metadata in zip(
            optimizer.param_groups,
            optimizer_group_metadata,
            strict=True,
        ):
            actual_lr = float(metadata["effective_lr"]) * lr_multiplier
            group["lr"] = actual_lr
            actual_lrs.append(actual_lr)
        diagnostics = (
            parameter_metrics(student)
            | optimizer_role_diagnostics(
                optimizer.param_groups,
                optimizer_group_metadata,
            )
            | student.activation_metrics()
            | student.btt_metrics()
            | student.kronecker_metrics()
            if diagnostic_step
            else {}
        )
        if diagnostics.get("diagnostic/nonfinite_gradients", 0):
            divergence_reason = f"nonfinite gradients at step {step}"
        activation_rms = diagnostics.get("diagnostic/activation_rms_max")
        if activation_rms is not None:
            if initial_activation_rms is None:
                initial_activation_rms = max(float(activation_rms), 1e-8)
            growth = float(activation_rms) / initial_activation_rms
            diagnostics["diagnostic/activation_rms_growth"] = growth
            max_activation_growth = max(max_activation_growth, growth)
            if growth > trial.max_activation_rms_growth:
                divergence_reason = (
                    f"activation RMS grew {growth:.3f}x at step {step}"
                )
        if divergence_reason is not None:
            if not trial.allow_divergence:
                raise RuntimeError(divergence_reason)
            run.log(
                {
                    "status/diverged": 1,
                    "status/diverged_step": step,
                    **diagnostics,
                },
                step=step,
            )
            break
        if trial.gradient_clip_norm > 0:
            preclip_norm = torch.nn.utils.clip_grad_norm_(
                student.parameters(),
                max_norm=trial.gradient_clip_norm,
                error_if_nonfinite=False,
            )
            diagnostics["diagnostic/gradient_norm_preclip"] = float(
                preclip_norm
            )
            diagnostics["diagnostic/gradient_clip_coefficient"] = min(
                1.0,
                trial.gradient_clip_norm
                / max(float(preclip_norm), 1e-12),
            )
            diagnostics["diagnostic/gradient_was_clipped"] = float(
                float(preclip_norm) > trial.gradient_clip_norm
            )
            if not torch.isfinite(preclip_norm):
                divergence_reason = f"nonfinite gradient norm at step {step}"
                if not trial.allow_divergence:
                    raise RuntimeError(divergence_reason)
                run.log(
                    {
                        "status/diverged": 1,
                        "status/diverged_step": step,
                        **diagnostics,
                    },
                    step=step,
                )
                break
        optimizer.step()
        optimizer_steps_completed += 1
        examples_seen = cursor
        input_tokens_seen = (
            examples_seen * trial.architecture.context_length
        )
        if diagnostic_step:
            diagnostics.update(optimizer_adam_role_diagnostics(
                optimizer,
                optimizer_group_metadata,
            ))
        torch.cuda.synchronize()
        elapsed = time.perf_counter() - step_start
        step_times.append(elapsed)
        completed_steps = step
        log = {
            "step": step,
            "train/kl": totals["kl"],
            "train/teacher_entropy": totals["teacher_entropy"],
            "train/teacher_cross_entropy":
                totals["teacher_cross_entropy"],
            "train/student_cross_entropy":
                totals["student_cross_entropy"],
            "train/teacher_nll": totals["teacher_nll"],
            "train/student_nll": totals["student_nll"],
            "train/teacher_perplexity":
                math.exp(min(totals["teacher_cross_entropy"], 30.0)),
            "train/student_perplexity":
                math.exp(min(totals["student_cross_entropy"], 30.0)),
            "train/objective": totals["objective"],
            "progress/examples_seen": examples_seen,
            "progress/input_tokens_seen": input_tokens_seen,
            "progress/optimizer_steps": optimizer_steps_completed,
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
        if "hidden_mse" in totals:
            log["train/hidden_mse"] = totals["hidden_mse"]
        if "softened_kl" in totals:
            log["train/softened_kl"] = totals["softened_kl"]
        run.log(log, step=step)
        if (
            step % trial.audit_every == 0
            or step == trial.steps
            or step == stable_boundary
        ):
            audit_n = _validation_examples_for_step(
                trial,
                step,
                len(val_contexts),
            )
            last_validation = evaluate(
                teacher, student, val_contexts, val_targets,
                examples=audit_n, batch_size=EVALUATION_BATCH,
                vocab_size=vocab_size, device=device,
            )
            run.log(
                {f"validation/{k}": v for k, v in last_validation.items()},
                step=step,
            )
            print(
                f"[{trial.label}] step={step}/{trial.steps} "
                f"train_kl={totals['kl']:.5f} val_kl={last_validation['kl']:.5f}",
                flush=True,
            )
        if step == stable_boundary:
            stable_validation = dict(last_validation)
            run.log(
                {
                    f"stable_validation/{key}": value
                    for key, value in stable_validation.items()
                },
                step=step,
            )
            _save_progress_checkpoint(
                output / "stable.pt",
                student,
                optimizer,
                trial,
                embedding_hash,
                step,
                initial_validation,
                prior_wall_seconds + time.perf_counter() - wall_start,
                examples_seen=examples_seen,
                input_tokens_seen=input_tokens_seen,
                optimizer_steps=optimizer_steps_completed,
                stable_validation=stable_validation,
            )
            if checkpoint_callback is not None:
                checkpoint_callback()
            print(
                f"[stable] {trial.label} committed step "
                f"{step}/{trial.steps}",
                flush=True,
            )
        if checkpoint_due(trial, step):
            _save_progress_checkpoint(
                progress_path,
                student,
                optimizer,
                trial,
                embedding_hash,
                step,
                initial_validation,
                prior_wall_seconds + time.perf_counter() - wall_start,
                examples_seen=examples_seen,
                input_tokens_seen=input_tokens_seen,
                optimizer_steps=optimizer_steps_completed,
                stable_validation=stable_validation,
            )
            if checkpoint_callback is not None:
                checkpoint_callback()
            print(
                f"[progress] {trial.label} committed step {step}/{trial.steps}",
                flush=True,
            )
        if (
            trial.target_validation_kl is not None
            and last_validation["kl"] <= trial.target_validation_kl
        ):
            confirmed_validation = evaluate(
                teacher, student, val_contexts, val_targets,
                examples=len(val_contexts), batch_size=EVALUATION_BATCH,
                vocab_size=vocab_size, device=device,
            )
            run.log(
                {
                    f"target_validation/{key}": value
                    for key, value in confirmed_validation.items()
                },
                step=step,
            )
            last_validation = confirmed_validation
            if last_validation["kl"] <= trial.target_validation_kl:
                target_reached = True
                print(
                    f"[target] {trial.label} reached full-validation KL "
                    f"{last_validation['kl']:.6f} <= "
                    f"{trial.target_validation_kl:.6f}",
                    flush=True,
                )
                break

    if divergence_reason is None:
        final_validation = evaluate(
            teacher, student, val_contexts, val_targets,
            examples=len(val_contexts), batch_size=EVALUATION_BATCH,
            vocab_size=vocab_size, device=device,
        )
    else:
        final_validation = last_validation
    if trial.target_validation_kl is not None:
        target_reached = (
            divergence_reason is None
            and final_validation["kl"] <= trial.target_validation_kl
        )
    final_test = {}
    if divergence_reason is None and (trial.is_final or trial.smoke):
        final_test = evaluate(
            teacher, student, test_contexts, test_targets,
            examples=(
                min(64, len(test_contexts))
                if trial.smoke else min(TEST_EXAMPLES, len(test_contexts))
            ),
            batch_size=EVALUATION_BATCH, vocab_size=vocab_size, device=device,
        )
    ending_hash = tensor_sha256(student.tied_embedding)
    if ending_hash != embedding_hash or student.tied_embedding.grad is not None:
        raise RuntimeError("frozen tied embedding changed or received a gradient")
    result = {
        "label": trial.label,
        "architecture": trial.architecture.to_dict(),
        "lr": trial.lr,
        "seed": trial.seed,
        "steps": trial.steps,
        "steps_completed": completed_steps,
        "status": "diverged" if divergence_reason is not None else "complete",
        "divergence_reason": divergence_reason,
        "effective_batch": trial.effective_batch,
        "lr_parameterization": trial.lr_parameterization,
        "lr_schedule": trial.lr_schedule,
        "warmup_steps": trial.warmup_steps,
        "warmup_examples": trial.warmup_examples,
        "cooldown_steps": trial.cooldown_steps,
        "cooldown_examples": trial.cooldown_examples,
        "min_lr_ratio": trial.min_lr_ratio,
        "gradient_clip_norm": trial.gradient_clip_norm,
        "optimizer_role_lr_multipliers": dict(
            trial.optimizer_role_lr_multipliers
        ),
        "optimizer_role_weight_decays": dict(
            trial.optimizer_role_weight_decays
        ),
        "temperature": trial.temperature,
        "temperature2_weight": trial.temperature2_weight,
        "hidden_mse_weight": trial.hidden_mse_weight,
        "dataset_tag": trial.dataset_tag,
        "dataset_manifest": dataset_manifest,
        "teacher_cache_enabled": teacher_hidden_cache is not None,
        "hydra_config_hash": trial.hydra_config_hash or None,
        "hydra_resolved_config": (
            json.loads(trial.hydra_resolved_config_json)
            if trial.hydra_resolved_config_json
            else None
        ),
        "optimizer_groups": optimizer_group_metadata,
        "compile_model": trial.compile_model,
        "kronecker_backend": (
            os.environ.get("QWEN_KRONECKER_BACKEND", "torch")
            if trial.architecture.operator in ("kronecker", "hybrid")
            else None
        ),
        "runtime_kronecker_rank_chunk": (
            int(os.environ.get(
                "QWEN_KRONECKER_RANK_CHUNK",
                trial.architecture.kronecker_rank_chunk,
            ))
            if trial.architecture.operator in ("kronecker", "hybrid")
            else None
        ),
        "runtime_kronecker_microbatch": (
            microbatch_for(
                trial.architecture, trial.effective_batch
            )[0]
            if trial.architecture.operator in ("kronecker", "hybrid")
            else None
        ),
        "runtime_kronecker_microbatch_limit": (
            int(os.environ.get("QWEN_KRONECKER_MICROBATCH", 64))
            if trial.architecture.operator in ("kronecker", "hybrid")
            else None
        ),
        "trainable_parameters": trainable,
        "frozen_parameters": frozen_embedding.numel(),
        "initial_validation": initial_validation,
        "stable_validation": stable_validation,
        "validation": final_validation,
        "test": final_test,
        "embedding_sha256": embedding_hash,
        "resumed_from_step": resume_step,
        "examples_seen": examples_seen,
        "input_tokens_seen": input_tokens_seen,
        "optimizer_steps": optimizer_steps_completed,
        "optimizer_state_resumed": bool(
            resume is not None
            and resume.get("optimizer_state_resumed", True)
        ),
        "warm_started": warm_started,
        "warm_start_lr_override": trial.warm_start_lr_override,
        "warm_start_weights_only": trial.warm_start_weights_only,
        "warm_start_from_stable": trial.warm_start_from_stable,
        "target_validation_kl": trial.target_validation_kl,
        "target_reached": target_reached,
        "validation_kl_gap": (
            max(
                0.0,
                float(final_validation["kl"])
                - trial.target_validation_kl,
            )
            if trial.target_validation_kl is not None
            else None
        ),
        "wall_seconds":
            prior_wall_seconds + time.perf_counter() - wall_start,
        "median_step_seconds": (
            float(np.median(step_times[max(1, len(step_times) // 10) :]))
            if len(step_times) > 1
            else (step_times[0] if step_times else 0.0)
        ),
        "peak_allocated_gib": torch.cuda.max_memory_allocated() / 2**30,
        "diagnostic_summary": {
            "activation_rms_growth_max": max_activation_growth,
            **student.btt_metrics(),
            **student.kronecker_metrics(),
        },
    }
    result["median_examples_per_second"] = (
        trial.effective_batch / result["median_step_seconds"]
        if result["median_step_seconds"] > 0
        else 0.0
    )
    result_path = output / "result.json"
    result_temporary = result_path.with_suffix(result_path.suffix + ".tmp")
    result_temporary.write_text(json.dumps(result, sort_keys=True))
    os.replace(result_temporary, result_path)
    if divergence_reason is None and (trial.is_final or trial.smoke):
        _save_checkpoint(
            output / "student.pt",
            student,
            trial,
            embedding_hash,
            result,
            # Every declared final stage is an exact-resume boundary. Keeping
            # this tied to `is_final` prevents new study stages from silently
            # saving weights-only checkpoints.
            optimizer=_final_checkpoint_optimizer(trial, optimizer),
            step=completed_steps,
            examples_seen=examples_seen,
            input_tokens_seen=input_tokens_seen,
            optimizer_steps=optimizer_steps_completed,
            stable_validation=stable_validation,
        )
    if progress_path.exists():
        progress_path.unlink()
    run.log(
        {f"final_validation/{k}": v for k, v in final_validation.items()}
        | {f"test/{k}": v for k, v in final_test.items()}
        | {
            "status/diverged": int(divergence_reason is not None),
            "performance/median_step_seconds":
                result["median_step_seconds"],
            "performance/median_examples_per_second":
                result["median_examples_per_second"],
        }
    )
    run.summary.update(result)
    run.finish()
    return result


def embedding_head_invariants(
    architecture,
    *,
    device: str = "cuda",
) -> dict:
    """Prove that the student uses one exact frozen Qwen I/O matrix."""
    teacher, tokenizer = load_teacher(device)
    input_weight = teacher.get_input_embeddings().weight
    output_weight = teacher.get_output_embeddings().weight
    if input_weight.data_ptr() != output_weight.data_ptr():
        raise RuntimeError("teacher input and output embeddings are not tied")
    embedding_hash = tensor_sha256(input_weight)
    student = FullWidthStudent(
        architecture,
        input_weight.detach().clone(),
        vocab_size=len(tokenizer),
    ).to(device)
    if not torch.equal(input_weight, student.tied_embedding):
        raise RuntimeError("student did not copy Qwen's tied weight exactly")
    ids = torch.arange(
        2 * architecture.context_length,
        device=device,
        dtype=torch.long,
    ).reshape(2, architecture.context_length)
    teacher_embedding = teacher.get_input_embeddings()(ids)
    student_embedding = F.embedding(ids, student.tied_embedding)
    if not torch.equal(teacher_embedding, student_embedding):
        raise RuntimeError("student embedding lookup differs from teacher")

    generator = torch.Generator(device=device).manual_seed(91)
    hidden = torch.randn(
        2,
        architecture.embedding_width,
        device=device,
        dtype=input_weight.dtype,
        generator=generator,
    )
    teacher_head = teacher.get_output_embeddings()(hidden)[
        :, : len(tokenizer)
    ]
    student_head = F.linear(
        hidden,
        student.tied_embedding[: len(tokenizer)],
    )
    head_error = float(
        (teacher_head.float() - student_head.float()).abs().max()
    )
    if not torch.allclose(
        teacher_head.float(),
        student_head.float(),
        rtol=2e-2,
        atol=2e-2,
    ):
        raise RuntimeError(
            "student unembedding differs beyond BF16 GEMM tolerance: "
            f"max_abs_error={head_error}"
        )

    with torch.autocast(device_type="cuda", dtype=torch.bfloat16):
        logits = student(ids)
        loss = logits[:, :1024].float().square().mean()
    loss.backward()
    if (
        student.tied_embedding.requires_grad
        or student.tied_embedding.grad is not None
    ):
        raise RuntimeError("frozen embedding received a gradient")
    ending_hash = tensor_sha256(student.tied_embedding)
    if ending_hash != embedding_hash:
        raise RuntimeError("frozen embedding changed during backward")
    state = student.state_dict()
    if "tied_embedding" in state:
        raise RuntimeError("frozen Qwen matrix leaked into student checkpoint")
    return {
        "status": "complete",
        "architecture": architecture.to_dict(),
        "teacher_weights_tied": True,
        "embedding_lookup_exact": True,
        "unembedding_weights_exact": True,
        "unembedding_logits_close": True,
        "unembedding_logits_max_abs_error": head_error,
        "requires_grad": False,
        "gradient_is_none": True,
        "excluded_from_state_dict": True,
        "embedding_shape": list(input_weight.shape),
        "embedding_dtype": str(input_weight.dtype),
        "embedding_sha256": embedding_hash,
        "vocab_size": len(tokenizer),
    }


def run_trial(
    trial: TrialConfig,
    *,
    data_root: str,
    output_root: str,
    device: str = "cuda",
    checkpoint_callback: Callable[[], None] | None = None,
    teacher_cache_root: str | None = None,
) -> dict:
    """Run one trial and always release W&B's background file handles."""
    try:
        return _run_trial_impl(
            trial,
            data_root=data_root,
            output_root=output_root,
            device=device,
            checkpoint_callback=checkpoint_callback,
            teacher_cache_root=teacher_cache_root,
        )
    except BaseException:
        # Modal may reuse the container for a function retry. An unfinished
        # W&B process keeps files on the mounted Volume open, which prevents
        # the next input's volume.reload(). Close it even for OOM/cancellation
        # paths, while preserving the original exception.
        try:
            import wandb

            if wandb.run is not None:
                wandb.finish(exit_code=1)
        except Exception:
            pass
        raise


def _benchmark_teacher_logits(
    *,
    teacher,
    teacher_cache: TeacherHiddenCache | None,
    token_ids: torch.Tensor,
    indices: np.ndarray,
    tied_embedding: torch.Tensor,
    vocab_size: int,
    device: str,
) -> torch.Tensor:
    if teacher_cache is None:
        return teacher_logits(teacher, token_ids, vocab_size)
    hidden = teacher_cache.take("train", indices, device=device)
    return reconstruct_logits(
        hidden,
        tied_embedding,
        vocab_size=vocab_size,
    )


def benchmark_trial(
    trial: TrialConfig,
    *,
    data_root: str,
    device: str = "cuda",
    warmup_steps: int = 2,
    measured_steps: int = 5,
    teacher_cache_root: str | None = None,
) -> dict:
    """Measure complete teacher+student optimizer steps without saving a run."""
    if warmup_steps < 1 or measured_steps < 1:
        raise ValueError("benchmark needs positive warmup and measured steps")
    trial.architecture.validate()
    torch.manual_seed(trial.seed)
    np.random.seed(trial.seed)
    torch.set_float32_matmul_precision("high")
    torch.backends.cuda.matmul.allow_tf32 = True
    torch.backends.cudnn.allow_tf32 = True
    torch.cuda.empty_cache()
    torch.cuda.reset_peak_memory_stats()

    dataset_manifest = load_manifest(data_root)
    contexts, targets, _ = load_split(data_root, "train")
    required = (warmup_steps + measured_steps) * trial.effective_batch
    if required > len(contexts):
        raise ValueError("benchmark data split is too small")

    teacher, tokenizer = load_teacher(device)
    teacher_embedding = teacher.get_input_embeddings().weight
    embedding_hash = tensor_sha256(teacher_embedding)
    embedding = teacher_embedding.detach().clone()
    student = FullWidthStudent(
        trial.architecture,
        embedding,
        vocab_size=len(tokenizer),
    ).to(device)
    teacher_hidden_cache = None
    if teacher_cache_root is not None:
        teacher_hidden_cache = TeacherHiddenCache(
            teacher_cache_root,
            expected_identity=make_cache_identity(
                model_id=MODEL_ID,
                model_revision=MODEL_REVISION,
                embedding_sha256=embedding_hash,
                dataset_manifest=dataset_manifest,
                context_length=trial.architecture.context_length,
            ),
            validate_shards=False,
        )
    parameter_groups, metadata = optimizer_parameter_groups(
        student, trial.lr, trial.lr_parameterization
    )
    optimizer = torch.optim.AdamW(parameter_groups, **ADAMW)
    forward_fn = (
        torch.compile(student, dynamic=False)
        if trial.compile_model
        else student
    )
    microbatch, accumulation = microbatch_for(
        trial.architecture, trial.effective_batch
    )
    cursor = 0
    times = []
    for step in range(warmup_steps + measured_steps):
        started = time.perf_counter()
        optimizer.zero_grad(set_to_none=True)
        for chunk_size in _microbatch_chunks(
            trial.effective_batch, microbatch
        ):
            chunk_weight = chunk_size / trial.effective_batch
            indices = np.arange(cursor, cursor + chunk_size)
            cursor += chunk_size
            ids = torch.from_numpy(np.array(contexts[indices], copy=True)).to(
                device=device, dtype=torch.long
            )
            target = torch.from_numpy(np.array(targets[indices], copy=True)).to(
                device=device, dtype=torch.long
            )
            target_value = _benchmark_teacher_logits(
                teacher=teacher,
                teacher_cache=teacher_hidden_cache,
                token_ids=ids,
                indices=indices,
                tied_embedding=student.tied_embedding,
                vocab_size=len(tokenizer),
                device=device,
            )
            with torch.autocast(device_type="cuda", dtype=torch.bfloat16):
                predicted = forward_fn(ids)
            rows = distribution_rows(target_value, predicted, target)
            loss = rows["kl"].mean() * chunk_weight
            if not torch.isfinite(loss):
                raise RuntimeError(
                    f"{trial.architecture.label}: benchmark loss became nonfinite"
                )
            loss.backward()
        diagnostics = parameter_metrics(student)
        if diagnostics["diagnostic/nonfinite_gradients"]:
            raise RuntimeError(
                f"{trial.architecture.label}: benchmark gradients became nonfinite"
            )
        optimizer.step()
        torch.cuda.synchronize()
        elapsed = time.perf_counter() - started
        if step >= warmup_steps:
            times.append(elapsed)

    median = float(np.median(times))
    return {
        "architecture": trial.architecture.to_dict(),
        "architecture_label": trial.architecture.label,
        "depth": trial.architecture.depth,
        "lr": trial.lr,
        "lr_parameterization": trial.lr_parameterization,
        "effective_batch": trial.effective_batch,
        "microbatch": microbatch,
        "gradient_accumulation": accumulation,
        "compile_model": trial.compile_model,
        "teacher_cache_enabled": teacher_hidden_cache is not None,
        "warmup_steps": warmup_steps,
        "measured_steps": measured_steps,
        "median_step_seconds": median,
        "median_examples_per_second": trial.effective_batch / median,
        "peak_allocated_gib": torch.cuda.max_memory_allocated() / 2**30,
        "trainable_parameters": student.trainable_parameter_count(),
        "optimizer_groups": metadata,
    }
