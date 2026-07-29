"""Exp V6: the KISS, single-file version of Exp V5."""

from __future__ import annotations

import ast
import json
import math
import os
import sys
import time
from pathlib import Path

import numpy as np
import torch
import torch.distributed as dist
import torch.nn.functional as F
from qwen_kron_distill.objective import (
    exact_kl_rows,
    khatri_rao_logits,
    load_teacher,
    teacher_distribution,
)
from qwen_normuon_pretrain.data import load_split
from qwen_normuon_pretrain.normuon import SingleDeviceNorMuon
from torch import nn
from torch.nn.parallel import DistributedDataParallel


CONFIG = {
    "seed": 0,
    "context_length": 16,
    "width": 64,
    "depth": 32,
    "rank": 8,
    "vocab_modes": (485, 512),
    "physical_local_batch": 65_536,  # one frozen-teacher target buffer / GPU
    "optimizer_local_batch": 8_192,  # 65,536 contexts / 1,048,576 tokens global
    "teacher_microbatch": 2_048,
    "max_updates": 100_000,
    "max_hours": 6.0,
    "lr": 0.2,
    "min_lr": 0.025,
    "plateau_patience": 4,
    "plateau_delta": 0.01,
    "weight_decay": 0.01,
    "warmup_updates": 4,
    "log_every": 8,
    "eval_every": 256,
    "eval_examples": 8_192,
    "eval_batch": 512,
    "checkpoint_every": 256,
    "artifact_every": 2_048,
    "data_root": "/cache/qwen_fullwidth_distill/context16-fineweb-edu-next-token-4m-v1",
    "resume": "/cache/expv6-kiss-long/source/checkpoint.pt",
    "source_artifact": "expv6-kiss-production-kt69ytky:v0",
    "output_dir": "/cache/expv6-kiss-long",
    "wandb_project": "qwen-causal-kron-distill",
    "run_name": "expv6-kiss-long-resume",
    "target_kl": 1.0,
    "compile": True,
    "self_test": False,
}


def overrides() -> None:
    for arg in sys.argv[1:]:
        if not arg.startswith("--") or "=" not in arg:
            raise SystemExit(f"expected --key=value, got {arg}")
        key, raw = arg[2:].split("=", 1)
        if key not in CONFIG:
            raise SystemExit(f"unknown option {key}")
        old = CONFIG[key]
        if isinstance(old, bool):
            CONFIG[key] = raw.lower() == "true"
        elif isinstance(old, str):
            CONFIG[key] = raw
        else:
            value = ast.literal_eval(raw)
            CONFIG[key] = tuple(value) if isinstance(old, tuple) else value


def per_token_rms(value: torch.Tensor) -> torch.Tensor:
    """Parameter-free RMS normalization over channels, independently per token."""
    scale = torch.rsqrt(value.float().square().mean(-1, keepdim=True) + 1e-6)
    return value * scale.to(value.dtype)


def _kron(value: torch.Tensor, a: torch.Tensor, b: torch.Tensor) -> torch.Tensor:
    # [batch, token, channel] -> rank Kronecker sum -> same shape.
    channel = torch.einsum("btc,roc->brto", value, b)
    return torch.einsum("brto,rst->bso", channel, a) / math.sqrt(a.shape[0])


_compiled_kron = torch.compile(_kron, fullgraph=True, dynamic=False)


def _soft_cross_entropy(
    logits: torch.Tensor, probability: torch.Tensor
) -> torch.Tensor:
    return -(probability * F.log_softmax(logits.float(), -1)).sum(-1).mean()


_compiled_soft_cross_entropy = torch.compile(
    _soft_cross_entropy, fullgraph=True, dynamic=False
)


class Block(nn.Module):
    def __init__(self) -> None:
        super().__init__()
        rank, length, width = (
            CONFIG["rank"],
            CONFIG["context_length"],
            CONFIG["width"],
        )
        self.a = nn.Parameter(
            torch.randn(rank, length, length, dtype=torch.bfloat16)
            / math.sqrt(length)
        )
        self.b = nn.Parameter(
            torch.randn(rank, width, width, dtype=torch.bfloat16)
            / math.sqrt(width)
        )

    def forward(self, value: torch.Tensor) -> torch.Tensor:
        normalized = F.silu(per_token_rms(value))
        kernel = _compiled_kron if CONFIG["compile"] and value.is_cuda else _kron
        return value + kernel(normalized, self.a, self.b) / math.sqrt(CONFIG["depth"])


class Student(nn.Module):
    def __init__(self) -> None:
        super().__init__()
        width = CONFIG["width"]
        self.vocabulary = nn.ParameterList(
            [
                nn.Parameter(torch.randn(mode, width, dtype=torch.bfloat16) * 0.02)
                for mode in CONFIG["vocab_modes"]
            ]
        )
        self.blocks = nn.ModuleList(Block() for _ in range(CONFIG["depth"]))

    def hidden(self, token_ids: torch.Tensor) -> torch.Tensor:
        first = token_ids.div(CONFIG["vocab_modes"][1], rounding_mode="floor")
        second = token_ids.remainder(CONFIG["vocab_modes"][1])
        value = F.embedding(first, self.vocabulary[0])
        value.mul_(F.embedding(second, self.vocabulary[1]))
        for block in self.blocks:
            value = block(value)
        return per_token_rms(value)[:, -1]

    def forward(
        self,
        token_ids: torch.Tensor,
        teacher_probability: torch.Tensor,
    ) -> torch.Tensor:
        logits = khatri_rao_logits(
            self.hidden(token_ids),
            self.vocabulary[0],
            self.vocabulary[1],
        )
        loss = (
            _compiled_soft_cross_entropy
            if CONFIG["compile"] and logits.is_cuda
            else _soft_cross_entropy
        )
        return loss(logits, teacher_probability)


@torch.no_grad()
def teacher_targets(
    teacher, token_ids: torch.Tensor
) -> tuple[torch.Tensor, torch.Tensor]:
    """BF16 soft targets plus FP32 entropy for literal KL reporting."""
    probability = torch.empty(
        token_ids.shape[0],
        math.prod(CONFIG["vocab_modes"]),
        device=token_ids.device,
        dtype=torch.bfloat16,
    )
    entropy = torch.empty(
        token_ids.shape[0], device=token_ids.device, dtype=torch.float32
    )
    weight = teacher.get_output_embeddings().weight
    micro = CONFIG["teacher_microbatch"]
    for start in range(0, token_ids.shape[0], micro):
        stop = min(start + micro, token_ids.shape[0])
        hidden = teacher.model(
            input_ids=token_ids[start:stop],
            use_cache=False,
            return_dict=True,
        ).last_hidden_state[:, -1]
        log_probability = F.log_softmax(F.linear(hidden, weight).float(), -1)
        chunk_probability = log_probability.exp()
        probability[start:stop].copy_(chunk_probability)
        entropy[start:stop].copy_(
            -(chunk_probability * log_probability).sum(-1)
        )
    return probability, entropy


def self_test() -> None:
    torch.manual_seed(0)
    value = torch.randn(3, 16, 64, dtype=torch.bfloat16)
    block = Block()
    assert block(value).shape == value.shape
    normalized = per_token_rms(value)
    torch.testing.assert_close(
        normalized.float().square().mean(-1),
        torch.ones(3, 16),
        atol=1e-2,
        rtol=1e-2,
    )
    student = Student()
    assert student.hidden(torch.randint(0, 248_320, (3, 16))).shape == (3, 64)
    assert all(parameter.ndim >= 2 for parameter in student.parameters())
    logits = torch.randn(3, 11)
    probability = torch.softmax(torch.randn(3, 11), -1)
    entropy = -(probability * probability.log()).sum(-1)
    expected = exact_kl_rows(logits, probability, entropy).mean()
    actual = _soft_cross_entropy(logits, probability) - entropy.mean()
    torch.testing.assert_close(actual, expected)
    assert CONFIG["physical_local_batch"] % CONFIG["optimizer_local_batch"] == 0
    print("self-test passed")


def batch_indices(size: int, outer: int, rank: int, world: int) -> np.ndarray:
    local = CONFIG["physical_local_batch"]
    generator = np.random.default_rng(CONFIG["seed"] + outer)
    indices = generator.choice(size, local * world, replace=False)
    return indices[rank * local : (rank + 1) * local]


@torch.inference_mode()
def evaluate(
    student: Student,
    teacher,
    contexts: np.ndarray,
    targets: np.ndarray,
    rank: int,
    world: int,
    device: torch.device,
) -> dict[str, float]:
    student.eval()
    totals = torch.zeros(5, device=device, dtype=torch.float64)
    indices = np.arange(
        rank,
        min(CONFIG["eval_examples"], len(contexts)),
        world,
        dtype=np.int64,
    )
    for start in range(0, len(indices), CONFIG["eval_batch"]):
        index = indices[start : start + CONFIG["eval_batch"]]
        token_ids = torch.as_tensor(
            np.asarray(contexts[index], dtype=np.int64), device=device
        )
        target = torch.as_tensor(
            np.asarray(targets[index], dtype=np.int64), device=device
        )
        probability, log_probability, entropy = teacher_distribution(
            teacher, token_ids
        )
        with torch.autocast("cuda", dtype=torch.bfloat16):
            logits = khatri_rao_logits(
                student.hidden(token_ids),
                student.vocabulary[0],
                student.vocabulary[1],
            )
        totals[0] += exact_kl_rows(logits, probability, entropy).double().sum()
        totals[1] += F.cross_entropy(logits.float(), target, reduction="sum")
        totals[2] += (-log_probability.gather(1, target[:, None])).double().sum()
        totals[3] += (logits.argmax(-1) == target).double().sum()
        totals[4] += len(index)
    dist.all_reduce(totals)
    student.train()
    count = float(totals[4])
    return {
        "validation_kl": float(totals[0] / count),
        "validation_student_nll": float(totals[1] / count),
        "validation_teacher_nll": float(totals[2] / count),
        "validation_accuracy": float(totals[3] / count),
        "validation_examples": int(count),
    }


def save_checkpoint(
    path: Path,
    student: Student,
    optimizer: torch.optim.Optimizer,
    state: dict,
    wandb_url: str,
) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    torch.save(
        {
            "config": CONFIG,
            "model": student.state_dict(),
            "optimizer": optimizer.state_dict(),
            "wandb_url": wandb_url,
            **state,
        },
        temporary,
    )
    os.replace(temporary, path)


def log_artifact(run, checkpoint: Path, aliases: list[str], wait: bool) -> None:
    import wandb

    artifact = wandb.Artifact(
        f"{CONFIG['run_name']}-checkpoint",
        type="model",
        metadata={
            "source_artifact": CONFIG["source_artifact"],
            "checkpoint": checkpoint.name,
        },
    )
    artifact.add_file(str(checkpoint), name="checkpoint.pt")
    logged = run.log_artifact(artifact, aliases=aliases)
    if wait:
        logged.wait()


def train() -> None:
    world = int(os.environ.get("WORLD_SIZE", "1"))
    rank = int(os.environ.get("RANK", "0"))
    local_rank = int(os.environ.get("LOCAL_RANK", "0"))
    if not torch.cuda.is_available() or world != 8:
        raise RuntimeError("paid training requires exactly 8 CUDA ranks")
    torch.cuda.set_device(local_rank)
    dist.init_process_group("nccl", device_id=torch.device(f"cuda:{local_rank}"))
    device = torch.device(f"cuda:{local_rank}")
    primary = rank == 0
    torch.manual_seed(CONFIG["seed"])

    physical_local = CONFIG["physical_local_batch"]
    optimizer_local = CONFIG["optimizer_local_batch"]
    if physical_local % optimizer_local:
        raise RuntimeError("physical batch must be divisible by optimizer batch")
    if physical_local * world > 4_000_000:
        raise RuntimeError("physical global batch exceeds the training split")

    contexts, _, _ = load_split(CONFIG["data_root"], "train")
    validation_contexts, validation_targets, _ = load_split(
        CONFIG["data_root"], "validation"
    )
    teacher = load_teacher(str(device))
    student = Student().to(device)
    optimizer = SingleDeviceNorMuon(
        student.parameters(),
        lr=CONFIG["lr"],
        weight_decay=CONFIG["weight_decay"],
        beta1=0.95,
        beta2=0.95,
        ns_steps=5,
        nesterov=True,
        eps=1e-8,
    )

    resume = Path(CONFIG["resume"])
    if not resume.is_file():
        raise RuntimeError(f"resume checkpoint is missing: {resume}")
    saved = torch.load(resume, map_location=device, weights_only=False)
    student.load_state_dict(saved["model"])
    optimizer.load_state_dict(saved["optimizer"])
    old_config = saved.get("config", {})
    optimizer_updates = int(saved.get("optimizer_updates", saved.get("step", 0)))
    old_local = int(
        old_config.get(
            "optimizer_local_batch",
            old_config.get("local_batch", optimizer_local),
        )
    )
    contexts_seen = int(
        saved.get("contexts_seen", optimizer_updates * old_local * world)
    )
    run_elapsed = float(saved.get("run_elapsed_seconds", 0.0))
    physical_batches = int(saved.get("physical_batches", 0))
    current_lr = float(saved.get("current_lr", CONFIG["lr"]))
    best_kl = float(saved.get("best_validation_kl", math.inf))
    plateau_reference = float(saved.get("plateau_reference_kl", math.inf))
    bad_validations = int(saved.get("bad_validations", 0))
    for group in optimizer.param_groups:
        group["lr"] = current_lr

    model = DistributedDataParallel(
        student,
        device_ids=[local_rank],
        broadcast_buffers=False,
        gradient_as_bucket_view=True,
    )

    run = None
    if primary:
        key = os.environ.get("WANDB_API_KEY")
        if not key:
            raise RuntimeError("WANDB_API_KEY is required")
        import wandb

        wandb.login(key=key, relogin=True)
        run = wandb.init(
            project=CONFIG["wandb_project"],
            name=CONFIG["run_name"],
            config={
                **CONFIG,
                "world_size": world,
                "physical_global_batch": physical_local * world,
                "optimizer_global_batch": optimizer_local * world,
                "optimizer_global_token_batch": (
                    optimizer_local * world * CONFIG["context_length"]
                ),
                "resume_contexts_seen": contexts_seen,
                "resume_optimizer_updates": optimizer_updates,
            },
        )
        if not run.url:
            raise RuntimeError("W&B did not return a direct run URL")
        print(f"WANDB_URL={run.url}", flush=True)

    output = Path(CONFIG["output_dir"])
    checkpoint = output / "checkpoint.pt"
    best_checkpoint = output / "best.pt"
    dist.barrier()
    initial = evaluate(
        student,
        teacher,
        validation_contexts,
        validation_targets,
        rank,
        world,
        device,
    )
    best_kl = min(best_kl, initial["validation_kl"])
    if not math.isfinite(plateau_reference):
        plateau_reference = initial["validation_kl"]
    if primary:
        run.log(
            {
                **initial,
                "optimizer_updates": optimizer_updates,
                "contexts_seen": contexts_seen,
                "input_tokens_seen": contexts_seen * CONFIG["context_length"],
                "lr": current_lr,
            },
            step=optimizer_updates,
        )
        print(json.dumps({"initial_validation": initial}), flush=True)

    started = time.perf_counter()
    updates_this_run = 0
    contexts_this_run = 0
    last_cross_entropy = float("nan")
    last_kl = float("nan")
    status = (
        "target"
        if initial["validation_kl"] <= CONFIG["target_kl"]
        else "running"
    )
    while status == "running" and optimizer_updates < CONFIG["max_updates"]:
        elapsed = run_elapsed + time.perf_counter() - started
        if elapsed >= CONFIG["max_hours"] * 3600:
            status = "timeout"
            break
        physical_batches += 1
        index = batch_indices(
            len(contexts), physical_batches + optimizer_updates, rank, world
        )
        token_ids = torch.as_tensor(
            np.asarray(contexts[index], dtype=np.int64), device=device
        )
        with torch.no_grad(), torch.autocast("cuda", dtype=torch.bfloat16):
            probability, entropy = teacher_targets(teacher, token_ids)
        for start in range(0, physical_local, optimizer_local):
            stop = start + optimizer_local
            optimizer.zero_grad(set_to_none=True)
            with torch.autocast("cuda", dtype=torch.bfloat16):
                cross_entropy = model(
                    token_ids[start:stop], probability[start:stop]
                )
            cross_entropy.backward()
            norm = torch.nn.utils.clip_grad_norm_(student.parameters(), 1.0)
            if not torch.isfinite(norm):
                raise RuntimeError("non-finite gradient")
            optimizer.step()
            optimizer_updates += 1
            updates_this_run += 1
            contexts_seen += optimizer_local * world
            contexts_this_run += optimizer_local * world
            last_cross_entropy = float(cross_entropy.detach())
            last_kl = last_cross_entropy - float(entropy[start:stop].mean())

            elapsed = run_elapsed + time.perf_counter() - started
            if (
                updates_this_run % CONFIG["log_every"] == 0
                or updates_this_run == 1
            ):
                averaged = torch.tensor(
                    [last_cross_entropy, last_kl],
                    device=device,
                    dtype=torch.float64,
                )
                dist.all_reduce(averaged)
                averaged /= world
                memory = {
                    "allocated": torch.cuda.max_memory_allocated() / 2**30,
                    "reserved": torch.cuda.max_memory_reserved() / 2**30,
                }
                gathered = [None] * world
                dist.all_gather_object(gathered, memory)
                metrics = {
                    "optimizer_updates": optimizer_updates,
                    "train_cross_entropy": float(averaged[0]),
                    "train_kl": float(averaged[1]),
                    "lr": current_lr,
                    "contexts_seen": contexts_seen,
                    "input_tokens_seen": contexts_seen * CONFIG["context_length"],
                    "optimizer_global_batch": optimizer_local * world,
                    "optimizer_global_token_batch": (
                        optimizer_local * world * CONFIG["context_length"]
                    ),
                    "contexts_per_second": contexts_this_run
                    / max(elapsed - run_elapsed, 1e-9),
                    "tokens_per_second": contexts_this_run
                    * CONFIG["context_length"]
                    / max(elapsed - run_elapsed, 1e-9),
                    "per_gpu_peak_allocated_gib": [
                        item["allocated"] for item in gathered
                    ],
                    "per_gpu_peak_reserved_gib": [
                        item["reserved"] for item in gathered
                    ],
                }
                if primary:
                    print(json.dumps(metrics), flush=True)
                    run.log(metrics, step=optimizer_updates)

            validation = None
            if updates_this_run % CONFIG["eval_every"] == 0:
                validation = evaluate(
                    student,
                    teacher,
                    validation_contexts,
                    validation_targets,
                    rank,
                    world,
                    device,
                )
                improved = validation["validation_kl"] < best_kl
                best_kl = min(best_kl, validation["validation_kl"])
                if (
                    validation["validation_kl"]
                    <= plateau_reference - CONFIG["plateau_delta"]
                ):
                    plateau_reference = validation["validation_kl"]
                    bad_validations = 0
                else:
                    bad_validations += 1
                if (
                    bad_validations >= CONFIG["plateau_patience"]
                    and current_lr > CONFIG["min_lr"]
                ):
                    current_lr = max(CONFIG["min_lr"], current_lr / 2)
                    for group in optimizer.param_groups:
                        group["lr"] = current_lr
                    plateau_reference = validation["validation_kl"]
                    bad_validations = 0
                if primary:
                    run.log(
                        {
                            **validation,
                            "best_validation_kl": best_kl,
                            "lr": current_lr,
                            "contexts_seen": contexts_seen,
                            "input_tokens_seen": (
                                contexts_seen * CONFIG["context_length"]
                            ),
                        },
                        step=optimizer_updates,
                    )
                    print(json.dumps(validation), flush=True)
                if validation["validation_kl"] <= CONFIG["target_kl"]:
                    status = "target"

            state = {
                "step": optimizer_updates,
                "optimizer_updates": optimizer_updates,
                "contexts_seen": contexts_seen,
                "run_elapsed_seconds": elapsed,
                "physical_batches": physical_batches,
                "current_lr": current_lr,
                "best_validation_kl": best_kl,
                "plateau_reference_kl": plateau_reference,
                "bad_validations": bad_validations,
                "last_validation": validation,
            }
            if (
                updates_this_run % CONFIG["checkpoint_every"] == 0
                or status != "running"
            ):
                if primary:
                    save_checkpoint(
                        checkpoint, student, optimizer, state, run.url
                    )
                    if validation and improved:
                        save_checkpoint(
                            best_checkpoint, student, optimizer, state, run.url
                        )
                dist.barrier()
            if (
                CONFIG["artifact_every"] > 0
                and updates_this_run % CONFIG["artifact_every"] == 0
                and primary
            ):
                log_artifact(run, checkpoint, ["latest"], wait=False)
            if elapsed >= CONFIG["max_hours"] * 3600:
                status = "timeout"
            if optimizer_updates >= CONFIG["max_updates"]:
                status = "max_updates"
            if status != "running":
                break
        del probability, entropy, token_ids

    elapsed = run_elapsed + time.perf_counter() - started
    final_state = {
        "step": optimizer_updates,
        "optimizer_updates": optimizer_updates,
        "contexts_seen": contexts_seen,
        "run_elapsed_seconds": elapsed,
        "physical_batches": physical_batches,
        "current_lr": current_lr,
        "best_validation_kl": best_kl,
        "plateau_reference_kl": plateau_reference,
        "bad_validations": bad_validations,
        "last_validation": locals().get("validation"),
    }
    if primary:
        save_checkpoint(checkpoint, student, optimizer, final_state, run.url)
    dist.barrier()
    if primary:
        result = {
            "status": status,
            "train_cross_entropy": last_cross_entropy,
            "train_kl": last_kl,
            "best_validation_kl": best_kl,
            "optimizer_updates": optimizer_updates,
            "contexts_seen": contexts_seen,
            "input_tokens_seen": contexts_seen * CONFIG["context_length"],
            "run_elapsed_seconds": elapsed,
            "wandb_url": run.url,
            "optimizer_global_batch": optimizer_local * world,
            "optimizer_global_token_batch": (
                optimizer_local * world * CONFIG["context_length"]
            ),
        }
        output.mkdir(parents=True, exist_ok=True)
        (output / "result.json").write_text(json.dumps(result, indent=2))
        run.log(result, step=optimizer_updates)
        log_artifact(run, checkpoint, ["latest", status], wait=True)
        run.finish()
        print(json.dumps(result, indent=2), flush=True)
    dist.destroy_process_group()


if __name__ == "__main__":
    overrides()
    if CONFIG["self_test"]:
        self_test()
    else:
        train()
