from __future__ import annotations

import json
import math
import os
import time
from collections.abc import Callable
from contextlib import nullcontext
from pathlib import Path

import numpy as np
import torch
import torch.nn.functional as F

from .codebook import (
    build_lsh_codebook,
    load_codebook_artifact,
    save_codebook_artifact,
    signed_codebook_sha256,
    tensor_sha256,
)
from .config import (
    AUDIT_EXAMPLES,
    EFFECTIVE_BATCH,
    LSH_BITS,
    LSH_SEED,
    MODEL_ID,
    MODEL_REVISION,
    TEST_EXAMPLES,
    TrialConfig,
    microbatch_for,
)
from .data import load_split
from .model import LSHMonarchStudent

ADAMW = {
    "betas": (0.9, 0.999),
    "eps": 1e-8,
    "weight_decay": 0.01,
    "fused": True,
}


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
        raise RuntimeError("pinned Qwen checkpoint no longer has tied I/O weights")
    if len(tokenizer) > 2**LSH_BITS:
        raise RuntimeError("tokenizer vocabulary no longer fits in 18 bits")
    return teacher, tokenizer


def prepare_codebook(path: str | Path) -> dict:
    """Create the codebook once, or validate and reuse its committed artifact."""
    path = Path(path)
    if path.exists():
        _, metadata = load_codebook_artifact(path)
        return metadata
    teacher, tokenizer = load_teacher("cpu")
    embedding = teacher.get_input_embeddings().weight
    embedding_hash = tensor_sha256(embedding)
    valid = embedding[: len(tokenizer)].detach().float().cpu().numpy()
    codebook, report = build_lsh_codebook(
        valid, bits=LSH_BITS, seed=LSH_SEED
    )
    return save_codebook_artifact(
        path, codebook, report, embedding_sha256=embedding_hash
    )


def teacher_logits(
    teacher, token_ids: torch.Tensor, vocab_size: int
) -> torch.Tensor:
    with torch.inference_mode():
        output = teacher(
            input_ids=token_ids,
            use_cache=False,
            return_dict=True,
            logits_to_keep=1,
        )
    return output.logits[:, -1, :vocab_size]


def distribution_rows(
    teacher_value: torch.Tensor,
    student_value: torch.Tensor,
    target: torch.Tensor,
) -> dict[str, torch.Tensor]:
    teacher_logp = F.log_softmax(teacher_value.float(), dim=-1)
    teacher_probability = teacher_logp.exp()
    student_logp = F.log_softmax(student_value.float(), dim=-1)
    kl = (teacher_probability * (teacher_logp - student_logp)).sum(-1)
    teacher_entropy = -(teacher_probability * teacher_logp).sum(-1)
    teacher_top = teacher_value.argmax(-1)
    student_order = student_value.topk(10, dim=-1).indices
    return {
        "kl": kl,
        "teacher_entropy": teacher_entropy,
        "teacher_nll": -teacher_logp.gather(1, target[:, None]).squeeze(1),
        "student_nll": -student_logp.gather(1, target[:, None]).squeeze(1),
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
    result["student_perplexity"] = math.exp(min(result["student_nll"], 30.0))
    result["teacher_perplexity"] = math.exp(min(result["teacher_nll"], 30.0))
    return result


def _autocast(device: str):
    if str(device).startswith("cuda"):
        return torch.autocast(device_type="cuda", dtype=torch.bfloat16)
    return nullcontext()


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
        with _autocast(device):
            student_value = student(ids)
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
    codebook_metadata: dict,
    resume_step: int = 0,
):
    import wandb

    config = trial.to_dict()
    config.update({
        "model_id": MODEL_ID,
        "model_revision": MODEL_REVISION,
        "representation": "signed_repaired_lsh18",
        "objective": "full_vocab_forward_kl",
        "temperature": 1.0,
        "effective_batch": EFFECTIVE_BATCH,
        "adamw": {
            "lr": trial.lr,
            "betas": list(ADAMW["betas"]),
            "eps": ADAMW["eps"],
            "weight_decay": ADAMW["weight_decay"],
        },
        "trainable_parameters": parameters,
        "frozen_tied_parameters": frozen,
        "embedding_sha256": codebook_metadata["embedding_sha256"],
        "codebook_sha256": codebook_metadata["codebook_sha256"],
        "codebook_report": codebook_metadata["report"],
        "resumed_from_step": resume_step,
        "optimizer_state_resumed": bool(resume_step),
    })
    return wandb.init(
        project="qwen-lsh18-monarch-distill",
        group=trial.stage,
        name=trial.label,
        job_type=trial.stage,
        config=config,
    )


def _save_checkpoint(
    path: Path,
    student: LSHMonarchStudent,
    trial: TrialConfig,
    embedding_hash: str,
    codebook_hash: str,
    metrics: dict,
) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    torch.save({
        "schema": "qwen-lsh18-monarch-student-v1",
        "state_dict": student.state_dict(),
        "trial": trial.to_dict(),
        "model_id": MODEL_ID,
        "model_revision": MODEL_REVISION,
        "embedding_sha256": embedding_hash,
        "codebook_sha256": codebook_hash,
        "metrics": metrics,
    }, temporary)
    os.replace(temporary, path)


def _save_progress_checkpoint(
    path: Path,
    student: LSHMonarchStudent,
    optimizer: torch.optim.Optimizer,
    trial: TrialConfig,
    embedding_hash: str,
    codebook_hash: str,
    step: int,
    initial_validation: dict[str, float],
    elapsed_wall_seconds: float,
) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    torch.save(
        {
            "schema": "qwen-lsh18-monarch-progress-v1",
            "state_dict": student.state_dict(),
            "optimizer_state_dict": optimizer.state_dict(),
            "trial": trial.to_dict(),
            "model_id": MODEL_ID,
            "model_revision": MODEL_REVISION,
            "embedding_sha256": embedding_hash,
            "codebook_sha256": codebook_hash,
            "step": step,
            "optimizer_state_included": True,
            "initial_validation": initial_validation,
            "elapsed_wall_seconds": elapsed_wall_seconds,
        },
        temporary,
    )
    os.replace(temporary, path)


def _load_progress_checkpoint(
    path: Path,
    student: LSHMonarchStudent,
    optimizer: torch.optim.Optimizer,
    trial: TrialConfig,
    embedding_hash: str,
    codebook_hash: str,
) -> dict | None:
    if not path.exists():
        return None
    checkpoint = torch.load(
        path, map_location="cpu", mmap=True, weights_only=True
    )
    expected = {
        "schema": "qwen-lsh18-monarch-progress-v1",
        "trial": trial.to_dict(),
        "model_id": MODEL_ID,
        "model_revision": MODEL_REVISION,
        "embedding_sha256": embedding_hash,
        "codebook_sha256": codebook_hash,
        "optimizer_state_included": True,
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
    initial_validation = checkpoint.get("initial_validation")
    if (
        not isinstance(initial_validation, dict)
        or not math.isfinite(float(initial_validation.get("kl", math.nan)))
    ):
        raise RuntimeError(f"{trial.label}: invalid initial validation")
    elapsed = float(checkpoint.get("elapsed_wall_seconds", math.nan))
    if not math.isfinite(elapsed) or elapsed < 0:
        raise RuntimeError(f"{trial.label}: invalid elapsed wall time")
    return {
        "step": step,
        "initial_validation": initial_validation,
        "elapsed_wall_seconds": elapsed,
    }


def _load_completed_result(
    output: Path,
    trial: TrialConfig,
    expected_codebook_hash: str | None = None,
) -> dict | None:
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
    if any(result.get(key) != value for key, value in expected.items()):
        return None
    validation = result.get("validation")
    if (
        not isinstance(validation, dict)
        or not math.isfinite(float(validation.get("kl", math.nan)))
    ):
        return None
    for key in ("embedding_sha256", "codebook_sha256"):
        value = result.get(key)
        if not isinstance(value, str) or len(value) != 64:
            return None
    if (
        expected_codebook_hash is not None
        and result["codebook_sha256"] != expected_codebook_hash
    ):
        return None
    if trial.stage == "final" or trial.smoke:
        if not isinstance(result.get("test"), dict) or not result["test"]:
            return None
        try:
            if checkpoint_path.stat().st_size <= 0:
                return None
        except OSError:
            return None
    return result


def run_trial(
    trial: TrialConfig,
    *,
    data_root: str,
    codebook_path: str,
    output_root: str,
    device: str = "cuda",
    checkpoint_callback: Callable[[], None] | None = None,
) -> dict:
    trial.architecture.validate()
    output = Path(output_root) / trial.stage / trial.label
    codebook, codebook_metadata = load_codebook_artifact(codebook_path)
    cached = _load_completed_result(
        output, trial, codebook_metadata["codebook_sha256"]
    )
    if cached is not None:
        print(f"[cache] {trial.label}", flush=True)
        return cached
    output.mkdir(parents=True, exist_ok=True)

    torch.manual_seed(trial.seed)
    np.random.seed(trial.seed)
    torch.set_float32_matmul_precision("high")
    if str(device).startswith("cuda"):
        torch.backends.cuda.matmul.allow_tf32 = True
        torch.backends.cudnn.allow_tf32 = True
        torch.cuda.reset_peak_memory_stats()

    train_contexts, train_targets, _ = load_split(data_root, "train")
    val_contexts, val_targets, _ = load_split(data_root, "validation")
    test_contexts, test_targets, _ = load_split(data_root, "test")
    required = trial.steps * EFFECTIVE_BATCH
    if required > len(train_contexts):
        raise ValueError(
            f"trial needs {required} train rows, only {len(train_contexts)} exist"
        )
    order = np.random.default_rng(trial.seed).permutation(
        len(train_contexts)
    )[:required]

    teacher, tokenizer = load_teacher(device)
    vocab_size = len(tokenizer)
    teacher_input = teacher.get_input_embeddings().weight
    embedding_hash = tensor_sha256(teacher_input)
    if embedding_hash != codebook_metadata["embedding_sha256"]:
        raise RuntimeError("codebook was not built from this teacher embedding")
    codebook, codebook_metadata = load_codebook_artifact(
        codebook_path,
        embedding_sha256=embedding_hash,
        vocab_size=vocab_size,
    )
    student = LSHMonarchStudent(
        trial.architecture, codebook.to(dtype=teacher_input.dtype)
    ).to(device)
    if student.tied_codebook.requires_grad:
        raise RuntimeError("frozen codebook unexpectedly requires gradients")
    optimizer_options = dict(ADAMW)
    if not str(device).startswith("cuda"):
        optimizer_options["fused"] = False
    optimizer = torch.optim.AdamW(
        student.parameters(), lr=trial.lr, **optimizer_options
    )
    progress_path = output / "progress.pt"
    codebook_hash = codebook_metadata["codebook_sha256"]
    resume = _load_progress_checkpoint(
        progress_path,
        student,
        optimizer,
        trial,
        embedding_hash,
        codebook_hash,
    )
    resume_step = int(resume["step"]) if resume is not None else 0
    prior_wall_seconds = (
        float(resume["elapsed_wall_seconds"]) if resume is not None else 0.0
    )
    if resume_step:
        print(
            f"[resume] {trial.label} from step {resume_step}/{trial.steps}",
            flush=True,
        )
    trainable = student.trainable_parameter_count()
    run = _wandb_init(
        trial,
        trainable,
        codebook.numel(),
        codebook_metadata,
        resume_step,
    )
    print(f"[wandb] {run.url}", flush=True)
    microbatch, accumulation = microbatch_for(trial.architecture)
    if microbatch * accumulation != EFFECTIVE_BATCH:
        raise RuntimeError("microbatch configuration changed effective batch")

    audit_n = min(AUDIT_EXAMPLES, 64 if trial.smoke else AUDIT_EXAMPLES)
    current_validation = evaluate(
        teacher,
        student,
        val_contexts,
        val_targets,
        examples=audit_n,
        batch_size=min(microbatch, 64),
        vocab_size=vocab_size,
        device=device,
    )
    run.log(
        {f"validation/{key}": value for key, value in current_validation.items()},
        step=resume_step,
    )
    initial_validation = (
        resume["initial_validation"] if resume is not None else current_validation
    )
    cursor = resume_step * EFFECTIVE_BATCH
    wall_start = time.perf_counter()
    for step in range(resume_step + 1, trial.steps + 1):
        step_start = time.perf_counter()
        optimizer.zero_grad(set_to_none=True)
        totals = {"kl": 0.0, "teacher_entropy": 0.0, "student_nll": 0.0}
        for _ in range(accumulation):
            indices = order[cursor : cursor + microbatch]
            cursor += microbatch
            ids = torch.from_numpy(
                np.array(train_contexts[indices], copy=True)
            ).to(device=device, dtype=torch.long)
            target = torch.from_numpy(
                np.array(train_targets[indices], copy=True)
            ).to(device=device, dtype=torch.long)
            target_logits = teacher_logits(teacher, ids, vocab_size)
            with _autocast(device):
                predicted_logits = student(ids)
            rows = distribution_rows(target_logits, predicted_logits, target)
            loss = rows["kl"].mean() / accumulation
            if not torch.isfinite(loss):
                raise RuntimeError(f"nonfinite training loss at step {step}")
            loss.backward()
            totals["kl"] += float(rows["kl"].detach().mean()) / accumulation
            totals["teacher_entropy"] += (
                float(rows["teacher_entropy"].detach().mean()) / accumulation
            )
            totals["student_nll"] += (
                float(rows["student_nll"].detach().mean()) / accumulation
            )
        diagnostic_step = (
            step == 1 or step % trial.audit_every == 0 or step == trial.steps
        )
        diagnostics = parameter_metrics(student) if diagnostic_step else {}
        if diagnostics.get("diagnostic/nonfinite_gradients", 0):
            raise RuntimeError(f"nonfinite gradients at step {step}")
        optimizer.step()
        if str(device).startswith("cuda"):
            torch.cuda.synchronize()
        elapsed = time.perf_counter() - step_start
        log = {
            "step": step,
            "train/kl": totals["kl"],
            "train/teacher_entropy": totals["teacher_entropy"],
            "train/student_nll": totals["student_nll"],
            "performance/step_seconds": elapsed,
            "performance/examples_per_second": EFFECTIVE_BATCH / elapsed,
            "performance/peak_allocated_gib": (
                torch.cuda.max_memory_allocated() / 2**30
                if str(device).startswith("cuda")
                else 0.0
            ),
            "performance/peak_reserved_gib": (
                torch.cuda.max_memory_reserved() / 2**30
                if str(device).startswith("cuda")
                else 0.0
            ),
            "optimizer/lr": trial.lr,
            **diagnostics,
        }
        run.log(log, step=step)
        if step % trial.audit_every == 0 or step == trial.steps:
            validation = evaluate(
                teacher,
                student,
                val_contexts,
                val_targets,
                examples=audit_n,
                batch_size=min(microbatch, 64),
                vocab_size=vocab_size,
                device=device,
            )
            run.log(
                {f"validation/{key}": value for key, value in validation.items()},
                step=step,
            )
            print(
                f"[{trial.label}] step={step}/{trial.steps} "
                f"train_kl={totals['kl']:.5f} val_kl={validation['kl']:.5f}",
                flush=True,
            )
        if (
            trial.stage == "final"
            and step < trial.steps
            and step % 1_000 == 0
        ):
            _save_progress_checkpoint(
                progress_path,
                student,
                optimizer,
                trial,
                embedding_hash,
                codebook_hash,
                step,
                initial_validation,
                prior_wall_seconds + time.perf_counter() - wall_start,
            )
            if checkpoint_callback is not None:
                checkpoint_callback()
            print(
                f"[progress] {trial.label} committed step {step}/{trial.steps}",
                flush=True,
            )

    final_validation = evaluate(
        teacher,
        student,
        val_contexts,
        val_targets,
        examples=len(val_contexts),
        batch_size=min(microbatch, 64),
        vocab_size=vocab_size,
        device=device,
    )
    final_test = {}
    if trial.stage == "final" or trial.smoke:
        final_test = evaluate(
            teacher,
            student,
            test_contexts,
            test_targets,
            examples=(
                min(64, len(test_contexts))
                if trial.smoke
                else min(TEST_EXAMPLES, len(test_contexts))
            ),
            batch_size=min(microbatch, 64),
            vocab_size=vocab_size,
            device=device,
        )
    ending_hash = signed_codebook_sha256(student.tied_codebook)
    if (
        ending_hash != codebook_hash
        or student.tied_codebook.grad is not None
        or student.tied_codebook.requires_grad
    ):
        raise RuntimeError("frozen tied codebook changed or received a gradient")
    result = {
        "label": trial.label,
        "architecture": trial.architecture.to_dict(),
        "lr": trial.lr,
        "seed": trial.seed,
        "steps": trial.steps,
        "trainable_parameters": trainable,
        "frozen_parameters": codebook.numel(),
        "initial_validation": initial_validation,
        "validation": final_validation,
        "test": final_test,
        "embedding_sha256": embedding_hash,
        "codebook_sha256": codebook_hash,
        "codebook_report": codebook_metadata["report"],
        "resumed_from_step": resume_step,
        "optimizer_state_resumed": bool(resume_step),
        "wall_seconds": prior_wall_seconds + time.perf_counter() - wall_start,
        "peak_allocated_gib": (
            torch.cuda.max_memory_allocated() / 2**30
            if str(device).startswith("cuda")
            else 0.0
        ),
    }
    result_path = output / "result.json"
    temporary = result_path.with_suffix(result_path.suffix + ".tmp")
    temporary.write_text(json.dumps(result, sort_keys=True))
    os.replace(temporary, result_path)
    if trial.stage == "final" or trial.smoke:
        _save_checkpoint(
            output / "student.pt",
            student,
            trial,
            embedding_hash,
            codebook_hash,
            result,
        )
    if progress_path.exists():
        progress_path.unlink()
    run.log(
        {f"final_validation/{key}": value for key, value in final_validation.items()}
        | {f"test/{key}": value for key, value in final_test.items()}
    )
    run.summary.update(result)
    run.finish()
    return result
