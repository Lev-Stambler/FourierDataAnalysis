"""Exp V9: Exp8 distillation resumed with standard Muon, in one file."""

from __future__ import annotations

import ast
import hashlib
import json
import math
import os
import queue
import signal
import subprocess
import sys
import threading
import time
import uuid
from pathlib import Path

import numpy as np
import torch
import torch.distributed as dist
import torch.nn.functional as F
from qwen_kron_distill.objective import exact_kl_rows, load_teacher, teacher_distribution
from qwen_normuon_pretrain.data import load_split
from torch import nn
from torch.nn.parallel import DistributedDataParallel as DDP


CONFIG = {
    "mode": "train",  # self_test | study | benchmark | train | long | long_supervisor
    "seed": 0,
    "context_length": 16,
    "vocab_size": 248_320,
    "width": 64,
    "depth": 32,
    "rank": 8,
    "parameter_dtype": "float32",  # FP32 master weights; autocast keeps compute BF16
    "teacher_probability_dtype": "float32",
    "physical_local_batch": 49_152,
    "optimizer_local_batch": 16_384,
    "teacher_microbatch": 2_048,
    "train_contexts": 16_777_216,
    "train_tokens": 1_000_000_000_000,
    "optimizer": "muon+adamw8bit",
    "loss": "exact_per_token_kl",
    "reset_optimizer": False,
    "reset_stream": False,
    "long_lr": 0.02,
    "long_output_dir": "",
    "long_physical_local_batch": 0,
    "long_optimizer_local_batch": 0,
    "max_hours": 2.0,
    "lr": 0.02,
    "min_lr": 0.002,
    "vocabulary_lr": 0.0003,
    "vocabulary_min_lr": 0.000003,
    "muon_momentum": 0.95,
    "muon_ns_steps": 5,
    "muon_warmup_tokens": 268_435_456,
    "weight_decay": 0.1,
    "warmup_contexts": 2_097_152,
    "log_contexts": 1_048_576,
    "eval_contexts": 16_777_216,
    "checkpoint_contexts": 16_777_216,
    "log_tokens": 16_777_216,
    "eval_tokens": 268_435_456,
    "checkpoint_tokens": 268_435_456,
    "plateau_patience": 4,
    "plateau_delta": 0.005,
    "min_lr_stagnant_evals": 6,
    "eval_examples": 8_192,
    "eval_batch": 512,
    "data_root": "/cache/qwen_fullwidth_distill/context16-fineweb-edu-next-token-4m-v1",
    "stream_root": "/cache/expv8-scale-safe/fineweb-edu-stream",
    "fineweb_id": "HuggingFaceFW/fineweb-edu",
    "fineweb_config": "default",
    "fineweb_revision": "87f09149ef4734204d70ed1d046ddc9ca3f2b8f9",
    "model_id": "Qwen/Qwen3.5-0.8B-Base",
    "model_revision": "5c8a1b97ddef11f79b47ab9d07bf82b9117413f6",
    "stream_prefetch": 4,
    "stream_tokenize_documents": 256,
    "stream_shuffle_buffer": 10_000,
    "stream_benchmark_batches": 3,
    "resume": "",
    "output_dir": "/cache/exp9-standard-muon/preflight",
    "study_root": "/cache/exp9-standard-muon",
    "wandb_project": "qwen-causal-kron-distill",
    "wandb_id": "",
    "run_name": "exp9-standard-muon",
    "target_kl": 1.0,
    "compile": True,
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
    scale = torch.rsqrt(value.float().square().mean(-1, keepdim=True) + 1e-6)
    return value * scale.to(value.dtype)


def fan_in_normalized(weight: torch.Tensor, fan_in: int) -> torch.Tensor:
    """Give every rank matrix element RMS 1/sqrt(fan_in)."""
    mean_square = weight.float().square().mean((-2, -1), keepdim=True)
    scale = torch.rsqrt(mean_square * fan_in + 1e-12)
    return weight * scale.to(weight.dtype)


def kron(value: torch.Tensor, a: torch.Tensor, b: torch.Tensor) -> torch.Tensor:
    channel = torch.einsum("btc,roc->brto", value, b)
    return torch.einsum("brto,rst->bso", channel, a) / math.sqrt(a.shape[0])


class Block(nn.Module):
    def __init__(self) -> None:
        super().__init__()
        rank, length, width = CONFIG["rank"], CONFIG["context_length"], CONFIG["width"]
        dtype = getattr(torch, CONFIG["parameter_dtype"])
        self.a = nn.Parameter(
            torch.randn(rank, length, length, dtype=dtype) / math.sqrt(length)
        )
        self.b = nn.Parameter(
            torch.randn(rank, width, width, dtype=dtype) / math.sqrt(width)
        )
        self.canonicalize_()

    def forward(self, value: torch.Tensor) -> torch.Tensor:
        # A and B are bilinear: A*c and B/c represent exactly the same
        # operator. Exp6/7 left this gauge unconstrained, allowing their RMS
        # values to grow by 20-100x without a corresponding functional change.
        # Per-rank fan-in normalization removes that optimizer-dependent scale.
        a = fan_in_normalized(self.a, CONFIG["context_length"])
        b = fan_in_normalized(self.b, CONFIG["width"])
        normalized = F.silu(per_token_rms(value))
        residual = value + kron(normalized, a, b) / math.sqrt(CONFIG["depth"])
        # Pre-normalization alone hid an RMS-1465 residual stream behind the
        # final RMSNorm. Normalizing every token after the residual addition
        # makes bounded activation scale a structural invariant.
        return per_token_rms(residual)

    @torch.no_grad()
    def canonicalize_(self) -> None:
        """Keep stored weights in the same gauge used by the forward pass."""
        self.a.copy_(fan_in_normalized(self.a, CONFIG["context_length"]))
        self.b.copy_(fan_in_normalized(self.b, CONFIG["width"]))


class Student(nn.Module):
    def __init__(self) -> None:
        super().__init__()
        dtype = getattr(torch, CONFIG["parameter_dtype"])
        self.vocabulary = nn.Parameter(
            torch.empty(CONFIG["vocab_size"], CONFIG["width"], dtype=dtype)
        )
        nn.init.normal_(self.vocabulary, std=0.02)
        self.blocks = nn.ModuleList(Block() for _ in range(CONFIG["depth"]))

    def hidden(self, token_ids: torch.Tensor) -> torch.Tensor:
        value = F.embedding(token_ids, self.vocabulary)
        for block in self.blocks:
            value = block(value)
        return per_token_rms(value)[:, -1]

    def logits(self, token_ids: torch.Tensor) -> torch.Tensor:
        return F.linear(self.hidden(token_ids), self.vocabulary)

    def forward(
        self,
        token_ids: torch.Tensor,
        teacher_probability: torch.Tensor,
        teacher_entropy: torch.Tensor,
    ) -> torch.Tensor:
        logits = self.logits(token_ids)
        return exact_kl_rows(logits, teacher_probability, teacher_entropy).mean()

    @torch.no_grad()
    def canonicalize_factors_(self) -> None:
        for block in self.blocks:
            block.canonicalize_()


def zeropower_via_newton_schulz5(
    gradient: torch.Tensor, steps: int = 5
) -> torch.Tensor:
    """Standard Muon NS5, independently over every matrix in a batch."""
    if gradient.ndim < 2:
        raise ValueError("Muon parameters must contain matrices")
    value = gradient.bfloat16()
    transposed = value.shape[-2] > value.shape[-1]
    if transposed:
        value = value.mT
    value = value / (value.norm(dim=(-2, -1), keepdim=True) + 1e-7)
    for _ in range(steps):
        gram = value @ value.mT
        value = 3.4445 * value + (-4.7750 * gram + 2.0315 * gram @ gram) @ value
    return value.mT if transposed else value


class BatchedMuon(torch.optim.Optimizer):
    """Canonical Muon with fused shape buckets and no NorMuon row moments."""

    def __init__(
        self,
        params,
        *,
        lr: float,
        momentum: float = 0.95,
        nesterov: bool = True,
        ns_steps: int = 5,
    ) -> None:
        super().__init__(
            params,
            {
                "lr": float(lr),
                "momentum": float(momentum),
                "nesterov": bool(nesterov),
                "ns_steps": int(ns_steps),
                "weight_decay": 0.0,
            },
        )
        for group in self.param_groups:
            for parameter in group["params"]:
                if parameter.ndim < 2:
                    raise ValueError("Muon parameters must contain matrices")

    @torch.no_grad()
    def step(self, closure=None):
        loss = None
        if closure is not None:
            with torch.enable_grad():
                loss = closure()
        for group in self.param_groups:
            buckets: dict[tuple, list[torch.Tensor]] = {}
            for parameter in group["params"]:
                if parameter.grad is not None:
                    key = (
                        parameter.device,
                        parameter.dtype,
                        tuple(parameter.shape[-2:]),
                    )
                    buckets.setdefault(key, []).append(parameter)
            for parameters in buckets.values():
                self._step_bucket(parameters, group)
        return loss

    @torch.no_grad()
    def _step_bucket(self, parameters, group: dict) -> None:
        rows, columns = parameters[0].shape[-2:]
        counts = [
            parameter.numel() // (rows * columns) for parameter in parameters
        ]
        directions = torch.empty(
            sum(counts),
            rows,
            columns,
            device=parameters[0].device,
            dtype=torch.bfloat16,
        )
        offset = 0
        for parameter, count in zip(parameters, counts, strict=True):
            state = self.state[parameter]
            if "momentum_buffer" not in state:
                state["momentum_buffer"] = torch.zeros_like(parameter)
                state["step"] = 0
            state["step"] += 1
            momentum = state["momentum_buffer"]
            momentum.lerp_(parameter.grad, 1.0 - group["momentum"])
            direction = (
                parameter.grad.lerp(momentum, group["momentum"])
                if group["nesterov"]
                else momentum
            )
            directions[offset : offset + count].copy_(
                direction.reshape(count, rows, columns)
            )
            offset += count
        updates = zeropower_via_newton_schulz5(
            directions, steps=group["ns_steps"]
        )
        adjusted_lr = group["lr"] * math.sqrt(max(1.0, rows / columns))
        offset = 0
        for parameter, count in zip(parameters, counts, strict=True):
            update = updates[offset : offset + count].reshape(parameter.shape)
            parameter.add_(update.to(parameter.dtype), alpha=-adjusted_lr)
            offset += count


class SplitOptimizer:
    """Small checkpointable pair: Muon body plus AdamW8bit vocabulary."""

    def __init__(self, body: BatchedMuon, vocabulary: torch.optim.Optimizer) -> None:
        self.body = body
        self.vocabulary = vocabulary

    @property
    def param_groups(self) -> list[dict]:
        return self.body.param_groups + self.vocabulary.param_groups

    def zero_grad(self, set_to_none: bool = True) -> None:
        self.body.zero_grad(set_to_none=set_to_none)
        self.vocabulary.zero_grad(set_to_none=set_to_none)

    def step(self) -> None:
        self.body.step()
        self.vocabulary.step()

    def state_dict(self) -> dict:
        return {
            "body_muon": self.body.state_dict(),
            "vocabulary_adamw8bit": self.vocabulary.state_dict(),
        }

    def load_state_dict(self, state: dict) -> None:
        self.body.load_state_dict(state["body_muon"])
        self.vocabulary.load_state_dict(state["vocabulary_adamw8bit"])

    def restore_exp8_vocabulary_state(self, state: dict) -> None:
        """Migrate only Exp8's vocabulary Adam moments; body Adam is discarded."""
        group = dict(state["param_groups"][0])
        source_id = group["params"][0]
        group["params"] = [0]
        source_state = state.get("state", {}).get(source_id)
        migrated = {
            "state": {} if source_state is None else {0: source_state},
            "param_groups": [group],
        }
        self.vocabulary.load_state_dict(migrated)


@torch.no_grad()
def optimizer_states_finite(optimizer: SplitOptimizer) -> bool:
    def finite(value) -> bool:
        if torch.is_tensor(value):
            return bool(torch.isfinite(value).all())
        if isinstance(value, dict):
            return all(finite(item) for item in value.values())
        if isinstance(value, (tuple, list)):
            return all(finite(item) for item in value)
        return True

    for component in (optimizer.body, optimizer.vocabulary):
        for state in component.state.values():
            if not finite(state):
                return False
    return True


def parameter_count() -> int:
    return sum(parameter.numel() for parameter in Student().parameters())


@torch.no_grad()
def update_diagnostics(
    student: Student, before: list[torch.Tensor], prefix: str = ""
) -> dict[str, float]:
    """Measure the exact parameter change made by one optimizer step."""
    groups = ((student.vocabulary,), tuple(student.blocks.parameters()))
    offsets = (0, 1)
    result = {}
    for name, parameters, offset in zip(("vocabulary", "body"), groups, offsets):
        old = before[offset : offset + len(parameters)]
        delta_square = sum(
            (parameter.detach().float() - previous.float()).square().sum()
            for parameter, previous in zip(parameters, old)
        )
        parameter_square = sum(parameter.detach().float().square().sum() for parameter in parameters)
        changed = sum((parameter.detach() != previous).sum() for parameter, previous in zip(parameters, old))
        count = sum(parameter.numel() for parameter in parameters)
        result[f"{prefix}{name}_update_rms"] = float(torch.sqrt(delta_square / count))
        result[f"{prefix}{name}_relative_update"] = float(
            torch.sqrt(delta_square / parameter_square.clamp_min(1e-30))
        )
        result[f"{prefix}{name}_changed_fraction"] = float(changed / count)
    return result


@torch.no_grad()
def body_change_diagnostics(
    student: Student, before: list[torch.Tensor], prefix: str
) -> dict[str, float]:
    parameters = tuple(student.blocks.parameters())
    delta_square = sum(
        (parameter.detach().float() - previous.float()).square().sum()
        for parameter, previous in zip(parameters, before)
    )
    count = sum(parameter.numel() for parameter in parameters)
    return {f"{prefix}body_update_rms": float(torch.sqrt(delta_square / count))}


@torch.no_grad()
def gradient_diagnostics(student: Student) -> dict[str, float]:
    groups = ((student.vocabulary,), tuple(student.blocks.parameters()))
    result = {}
    for name, parameters in zip(("vocabulary", "body"), groups):
        square = sum(
            parameter.grad.detach().float().square().sum()
            for parameter in parameters
            if parameter.grad is not None
        )
        count = sum(parameter.numel() for parameter in parameters)
        result[f"{name}_grad_norm"] = float(torch.sqrt(square))
        result[f"{name}_grad_rms"] = float(torch.sqrt(square / count))
    return result


@torch.no_grad()
def scale_diagnostics(student: Student) -> dict[str, float]:
    """Expose stored gauges so a scale regression cannot hide behind RMSNorm."""
    a_rms = torch.cat(
        [block.a.float().square().mean((-2, -1)).sqrt() for block in student.blocks]
    )
    b_rms = torch.cat(
        [block.b.float().square().mean((-2, -1)).sqrt() for block in student.blocks]
    )
    vocabulary = student.vocabulary.detach().float()
    row_rms = vocabulary.square().mean(-1).sqrt()
    quantiles = torch.quantile(
        row_rms, torch.tensor((0.01, 0.5, 0.99), device=row_rms.device)
    )
    return {
        "factor_a_rms_min": float(a_rms.min()),
        "factor_a_rms_max": float(a_rms.max()),
        "factor_b_rms_min": float(b_rms.min()),
        "factor_b_rms_max": float(b_rms.max()),
        "vocabulary_rms": float(vocabulary.square().mean().sqrt()),
        "vocabulary_row_rms_p01": float(quantiles[0]),
        "vocabulary_row_rms_p50": float(quantiles[1]),
        "vocabulary_row_rms_p99": float(quantiles[2]),
    }


@torch.no_grad()
def activation_scale_diagnostics(
    student: Student, token_ids: torch.Tensor
) -> dict[str, float]:
    value = F.embedding(token_ids, student.vocabulary)
    embedding_rms = value.float().square().mean().sqrt()
    branch_rms = []
    residual_rms = []
    post_rms_error = []
    for block in student.blocks:
        a = fan_in_normalized(block.a, CONFIG["context_length"])
        b = fan_in_normalized(block.b, CONFIG["width"])
        normalized = F.silu(per_token_rms(value))
        branch = kron(normalized, a, b) / math.sqrt(CONFIG["depth"])
        residual = value + branch
        value = per_token_rms(residual)
        branch_rms.append(branch.float().square().mean().sqrt())
        residual_rms.append(residual.float().square().mean().sqrt())
        token_rms = value.float().square().mean(-1).sqrt()
        post_rms_error.append((token_rms - 1).abs().max())
    branches = torch.stack(branch_rms)
    residuals = torch.stack(residual_rms)
    return {
        "embedding_activation_rms": float(embedding_rms),
        "branch_rms_min": float(branches.min()),
        "branch_rms_max": float(branches.max()),
        "residual_pre_norm_rms_min": float(residuals.min()),
        "residual_pre_norm_rms_max": float(residuals.max()),
        "residual_post_norm_max_error": float(torch.stack(post_rms_error).max()),
    }


def clean_state_dict(state: dict[str, torch.Tensor]) -> dict[str, torch.Tensor]:
    prefixes = ("module.", "_orig_mod.")
    clean = {}
    for name, value in state.items():
        while any(name.startswith(prefix) for prefix in prefixes):
            name = name.split(".", 1)[1]
        clean[name] = value
    return clean


def file_sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as source:
        for chunk in iter(lambda: source.read(8 << 20), b""):
            digest.update(chunk)
    return digest.hexdigest()


@torch.no_grad()
def load_initial(student: Student, device: torch.device) -> tuple[dict, dict]:
    """Start fresh or resume a scale-safe Exp8/Exp9 checkpoint."""
    del device
    if not CONFIG["resume"]:
        student.canonicalize_factors_()
        return {}, {"initialization": "fresh_scale_safe"}
    source = Path(CONFIG["resume"])
    if not source.is_file():
        raise RuntimeError(f"resume checkpoint is missing: {source}")
    saved = torch.load(source, map_location="cpu", weights_only=False)
    schema = saved.get("schema")
    if schema not in {"exp8-scale-safe-v1", "exp9-standard-muon-v1"}:
        raise RuntimeError(
            "Exp9 refuses legacy checkpoints because their factor gauges and "
            "residual-state scales are not compatible"
        )
    student.load_state_dict(clean_state_dict(saved["model"]))
    student.canonicalize_factors_()
    return saved, {
        "initialization": f"{schema.removesuffix('-v1')}_resume",
        "source_checkpoint": str(source),
        "source_sha256": file_sha256(source),
    }


def self_test() -> None:
    torch.manual_seed(0)
    assert parameter_count() == 17_006_592
    student = Student()
    assert student.vocabulary.shape == (248_320, 64)
    assert all(parameter.ndim >= 2 for parameter in student.parameters())
    value = torch.randn(3, 16, 64, dtype=torch.bfloat16)
    normalized = per_token_rms(value)
    torch.testing.assert_close(
        normalized.float().square().mean(-1), torch.ones(3, 16), atol=1e-2, rtol=1e-2
    )
    block = student.blocks[0]
    block_output = block(value.float())
    torch.testing.assert_close(
        block_output.square().mean(-1),
        torch.ones(3, 16),
        atol=1e-5,
        rtol=1e-5,
    )
    expected_a = torch.full((CONFIG["rank"],), 1 / math.sqrt(CONFIG["context_length"]))
    expected_b = torch.full((CONFIG["rank"],), 1 / math.sqrt(CONFIG["width"]))
    torch.testing.assert_close(
        block.a.square().mean((-2, -1)).sqrt(), expected_a, atol=1e-6, rtol=1e-6
    )
    torch.testing.assert_close(
        block.b.square().mean((-2, -1)).sqrt(), expected_b, atol=1e-6, rtol=1e-6
    )
    original = block(value.float())
    with torch.no_grad():
        block.a.mul_(7)
        block.b.mul_(0.25)
    torch.testing.assert_close(block(value.float()), original, atol=1e-5, rtol=1e-5)
    block.canonicalize_()
    token_ids = torch.randint(0, CONFIG["vocab_size"], (3, 16))
    torch.testing.assert_close(
        student.logits(token_ids),
        F.linear(student.hidden(token_ids), student.vocabulary),
    )
    logits = torch.randn(3, 11)
    probability = torch.softmax(torch.randn(3, 11), -1)
    entropy = -(probability * probability.log()).sum(-1)
    loss = -(probability * F.log_softmax(logits, -1)).sum(-1).mean()
    torch.testing.assert_close(loss - entropy.mean(), exact_kl_rows(logits, probability, entropy).mean())
    print(json.dumps({"self_test": "passed", "parameters": parameter_count()}))


@torch.no_grad()
def teacher_targets(teacher, token_ids: torch.Tensor) -> tuple[torch.Tensor, torch.Tensor]:
    probability = torch.empty(
        token_ids.shape[0],
        CONFIG["vocab_size"],
        device=token_ids.device,
        dtype=getattr(torch, CONFIG["teacher_probability_dtype"]),
    )
    entropy = torch.empty(token_ids.shape[0], device=token_ids.device, dtype=torch.float32)
    weight = teacher.get_output_embeddings().weight[: CONFIG["vocab_size"]]
    for start in range(0, len(token_ids), CONFIG["teacher_microbatch"]):
        stop = min(start + CONFIG["teacher_microbatch"], len(token_ids))
        hidden = teacher.model(
            input_ids=token_ids[start:stop], use_cache=False, return_dict=True
        ).last_hidden_state[:, -1]
        log_probability = F.log_softmax(F.linear(hidden, weight).float(), -1)
        chunk = log_probability.exp()
        probability[start:stop].copy_(chunk)
        entropy[start:stop].copy_(-(chunk * log_probability).sum(-1))
    return probability, entropy


@torch.inference_mode()
def evaluate(
    student: nn.Module,
    teacher,
    contexts: np.ndarray,
    targets: np.ndarray,
    rank: int,
    world: int,
    device: torch.device,
) -> dict[str, float]:
    student.eval()
    totals = torch.zeros(5, device=device, dtype=torch.float64)
    indices = np.arange(rank, min(CONFIG["eval_examples"], len(contexts)), world)
    for start in range(0, len(indices), CONFIG["eval_batch"]):
        index = indices[start : start + CONFIG["eval_batch"]]
        token_ids = torch.as_tensor(np.asarray(contexts[index], dtype=np.int64), device=device)
        target = torch.as_tensor(np.asarray(targets[index], dtype=np.int64), device=device)
        probability, log_probability, entropy = teacher_distribution(teacher, token_ids)
        with torch.autocast("cuda", dtype=torch.bfloat16):
            logits = student.logits(token_ids)
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


def due(previous: int, current: int, interval: int) -> bool:
    return interval > 0 and previous // interval != current // interval


def batch_indices(size: int, outer: int, rank: int, world: int) -> np.ndarray:
    local = CONFIG["physical_local_batch"]
    generator = np.random.default_rng(CONFIG["seed"] + outer)
    indices = generator.choice(size, local * world, replace=False)
    return indices[rank * local : (rank + 1) * local]


def cosine_lr(
    start: float,
    progress_tokens: int,
    budget_tokens: int,
    minimum: float | None = None,
) -> float:
    """Token-based cosine between explicit endpoints."""
    fraction = min(1.0, max(0.0, progress_tokens / max(1, budget_tokens)))
    minimum = start * 0.01 if minimum is None else minimum
    return minimum + 0.5 * (start - minimum) * (1.0 + math.cos(math.pi * fraction))


def exact_local_contexts(remaining_tokens: int, world: int) -> int:
    """Return the equal per-rank context count for an exact input-token tail."""
    denominator = CONFIG["context_length"] * world
    if remaining_tokens <= 0 or remaining_tokens % denominator:
        raise ValueError(
            f"remaining token budget {remaining_tokens} is not divisible by {denominator}"
        )
    return remaining_tokens // denominator


def benchmark_context_budget(run_contexts: int, physical_local: int, world: int) -> int:
    """Absolute stop counter for three new batches, including after resume."""
    return run_contexts + physical_local * world * 3


def document_contexts(token_ids) -> np.ndarray:
    """All non-overlapping 16-input/one-target windows from one document."""
    length = CONFIG["context_length"] + 1
    values = np.asarray(token_ids, dtype=np.int32)
    count = len(values) // length
    if count == 0:
        return np.empty((0, CONFIG["context_length"]), dtype=np.int32)
    return values[: count * length].reshape(count, length)[:, :-1].copy()


def _hash_set(data_root: str, splits: tuple[str, ...]) -> set[bytes]:
    values = []
    root = Path(data_root)
    for split in splits:
        path = root / f"{split}_hashes.npy"
        if path.is_file():
            values.append(np.asarray(np.load(path, mmap_mode="r")))
    if not values:
        return set()
    return {bytes(value) for value in np.unique(np.concatenate(values))}


class FineWebEduStream:
    """Rank-sharded FineWeb-Edu producer with an in-memory asynchronous queue."""

    def __init__(
        self,
        rank: int,
        world: int,
        local_batch: int,
        resume_state: dict | None = None,
    ) -> None:
        os.environ.setdefault("RAYON_NUM_THREADS", "4")
        os.environ.setdefault("TOKENIZERS_PARALLELISM", "true")
        self.rank = rank
        self.world = world
        self.local_batch = local_batch
        self.resume_state = resume_state or {}
        self.ready: queue.Queue = queue.Queue(maxsize=int(CONFIG["stream_prefetch"]))
        self.stop = threading.Event()
        self.thread = threading.Thread(target=self._produce, daemon=True)
        self.thread.start()

    def _put(self, item) -> bool:
        while not self.stop.is_set():
            try:
                self.ready.put(item, timeout=0.25)
                return True
            except queue.Full:
                pass
        return False

    def _open(self, epoch: int, state: dict | None):
        from datasets import load_dataset

        dataset = load_dataset(
            CONFIG["fineweb_id"],
            name=CONFIG["fineweb_config"],
            split="train",
            streaming=True,
            revision=CONFIG["fineweb_revision"],
        )
        dataset = dataset.shuffle(
            seed=CONFIG["seed"] + epoch,
            buffer_size=int(CONFIG["stream_shuffle_buffer"]),
        )
        dataset = dataset.shard(num_shards=self.world, index=self.rank)
        if state:
            dataset.load_state_dict(state)
        return dataset

    def _produce(self) -> None:
        try:
            from transformers import AutoTokenizer

            tokenizer = AutoTokenizer.from_pretrained(
                CONFIG["model_id"],
                revision=CONFIG["model_revision"],
                use_fast=True,
            )
            epoch = int(self.resume_state.get("epoch", 0))
            ordinal = int(self.resume_state.get("batch_ordinal", 0))
            pending = np.asarray(
                self.resume_state.get(
                    "pending",
                    np.empty((0, CONFIG["context_length"]), dtype=np.int32),
                ),
                dtype=np.int32,
            ).reshape(-1, CONFIG["context_length"])
            dataset = self._open(epoch, self.resume_state.get("dataset_state"))
            iterator = iter(dataset)
            held_out = _hash_set(CONFIG["data_root"], ("validation", "test"))
            seen_by_screen = _hash_set(CONFIG["data_root"], ("train",))
            document_batch = int(CONFIG["stream_tokenize_documents"])

            while not self.stop.is_set():
                started = time.perf_counter()
                while len(pending) < self.local_batch:
                    texts = []
                    while len(texts) < document_batch:
                        try:
                            record = next(iterator)
                        except StopIteration:
                            epoch += 1
                            dataset = self._open(epoch, None)
                            iterator = iter(dataset)
                            continue
                        text = record.get("text") or ""
                        if not text:
                            continue
                        digest = hashlib.sha256(text.encode("utf-8", "ignore")).digest()
                        if digest in held_out or (epoch == 0 and digest in seen_by_screen):
                            continue
                        texts.append(text)
                    tokenized = tokenizer(
                        texts,
                        add_special_tokens=False,
                        padding=False,
                        truncation=False,
                    )["input_ids"]
                    chunks = [
                        contexts
                        for ids in tokenized
                        if len(contexts := document_contexts(ids))
                    ]
                    if chunks:
                        addition = np.concatenate(chunks)
                        pending = (
                            addition
                            if not len(pending)
                            else np.concatenate((pending, addition))
                        )

                contexts = np.ascontiguousarray(pending[: self.local_batch])
                pending = np.ascontiguousarray(pending[self.local_batch :])
                ordinal += 1
                state = {
                    "epoch": epoch,
                    "batch_ordinal": ordinal,
                    "dataset_state": dataset.state_dict(),
                    "pending": pending.copy(),
                }
                if not self._put(
                    {
                        "contexts": contexts,
                        "state": state,
                        "producer_seconds": time.perf_counter() - started,
                    }
                ):
                    return
        except BaseException as error:
            self._put(error)

    def next(self) -> tuple[np.ndarray, dict, float, float, int]:
        started = time.perf_counter()
        item = self.ready.get()
        waited = time.perf_counter() - started
        if isinstance(item, BaseException):
            raise RuntimeError("FineWeb-Edu producer failed") from item
        return (
            item["contexts"],
            item["state"],
            waited,
            float(item["producer_seconds"]),
            self.ready.qsize(),
        )

    def close(self) -> None:
        self.stop.set()
        while True:
            try:
                self.ready.get_nowait()
            except queue.Empty:
                break
        self.thread.join()


def save_checkpoint(
    path: Path,
    student: Student,
    optimizer: SplitOptimizer,
    state: dict,
    metadata: dict,
    wandb_url: str,
) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(".tmp")
    torch.save(
        {
            "schema": "exp9-standard-muon-v1",
            "config": dict(CONFIG),
            "model": student.state_dict(),
            "optimizer": optimizer.state_dict(),
            "metadata": metadata,
            "wandb_url": wandb_url,
            **state,
        },
        temporary,
    )
    os.replace(temporary, path)


def train() -> None:
    world = int(os.environ.get("WORLD_SIZE", "1"))
    rank = int(os.environ.get("RANK", "0"))
    local_rank = int(os.environ.get("LOCAL_RANK", "0"))
    if not torch.cuda.is_available() or world != 8:
        raise RuntimeError("Exp V9 paid training requires exactly eight CUDA ranks")
    torch.cuda.set_device(local_rank)
    device = torch.device(f"cuda:{local_rank}")
    dist.init_process_group("nccl", device_id=device)
    primary = rank == 0
    torch.manual_seed(CONFIG["seed"])
    terminate_requested = False

    def request_checkpoint(signum, frame) -> None:
        del signum, frame
        nonlocal terminate_requested
        terminate_requested = True

    signal.signal(signal.SIGTERM, request_checkpoint)
    signal.signal(signal.SIGINT, request_checkpoint)

    physical_local = int(CONFIG["physical_local_batch"])
    optimizer_local = int(CONFIG["optimizer_local_batch"])
    long_run = CONFIG["mode"] == "long"
    if physical_local % optimizer_local:
        raise RuntimeError("physical_local_batch must be divisible by optimizer_local_batch")
    global_batch = optimizer_local * world
    global_tokens = global_batch * CONFIG["context_length"]
    if global_tokens < 100_000:
        raise RuntimeError("global token batch violates accelerator policy")

    contexts = None
    if not long_run:
        contexts, _, _ = load_split(CONFIG["data_root"], "train")
    validation_contexts, validation_targets, _ = load_split(CONFIG["data_root"], "validation")
    if not long_run and physical_local * world > len(contexts):
        raise RuntimeError("physical teacher batch exceeds the training split")
    teacher = load_teacher(str(device))
    student = Student().to(device)
    saved, metadata = load_initial(student, device)
    if CONFIG["optimizer"] == "muon+adamw8bit":
        from bitsandbytes.optim import AdamW8bit

        body_optimizer = BatchedMuon(
            student.blocks.parameters(),
            lr=CONFIG["lr"],
            momentum=CONFIG["muon_momentum"],
            ns_steps=CONFIG["muon_ns_steps"],
        )
        vocabulary_optimizer = AdamW8bit(
            [student.vocabulary],
            lr=CONFIG["vocabulary_lr"],
            betas=(0.9, 0.95),
            eps=1e-8,
            weight_decay=CONFIG["weight_decay"],
        )
        optimizer = SplitOptimizer(body_optimizer, vocabulary_optimizer)
    else:
        raise ValueError("Exp9 intentionally supports only Muon plus AdamW8bit")

    run_contexts = 0
    total_contexts = 0
    optimizer_updates = 0
    session_optimizer_updates = 0
    current_lr = float(CONFIG["lr"])
    current_vocabulary_lr = float(CONFIG["vocabulary_lr"])
    best_kl = math.inf
    plateau_reference = math.inf
    bad_validations = 0
    minimum_lr_bad_validations = 0
    physical_batches = 0
    long_input_tokens_seen = 0
    muon_start_tokens = 0
    stream_states = None
    wandb_id = CONFIG["wandb_id"]
    resume = Path(CONFIG["resume"])
    if resume.is_file() and saved:
        saved_schema = str(saved.get("schema", ""))
        saved_optimizer = str(saved.get("optimizer_name", ""))
        if (
            saved_schema == "exp9-standard-muon-v1"
            and saved_optimizer == CONFIG["optimizer"]
            and not CONFIG["reset_optimizer"]
        ):
            optimizer.load_state_dict(saved["optimizer"])
        elif saved_schema == "exp8-scale-safe-v1" and not CONFIG["reset_optimizer"]:
            # Standard Muon begins with fresh momentum. The dense vocabulary is
            # still AdamW8bit, so preserving its moments avoids throwing away
            # the optimizer history that produced the resumed model.
            optimizer.restore_exp8_vocabulary_state(saved["optimizer"])
        total_contexts = int(saved.get("contexts_seen", 0))
        # run_contexts is the budget counter for finite train/screen jobs.
        # Restoring only total_contexts made a resumed 2-interval confirmation
        # train two *new* intervals, because its session counter restarted at 0.
        run_contexts = int(saved.get("run_contexts", 0))
        optimizer_updates = int(saved.get("optimizer_updates", saved.get("step", 0)))
        current_lr = float(
            saved.get("current_muon_lr", CONFIG["lr"])
            if saved_schema == "exp9-standard-muon-v1"
            else CONFIG["lr"]
        )
        current_vocabulary_lr = float(
            saved.get("current_vocabulary_lr", CONFIG["vocabulary_lr"])
            if saved_schema == "exp9-standard-muon-v1"
            else CONFIG["vocabulary_lr"]
        )
        best_kl = float(saved.get("best_validation_kl", math.inf))
        plateau_reference = float(saved.get("plateau_reference_kl", math.inf))
        bad_validations = int(saved.get("bad_validations", 0))
        minimum_lr_bad_validations = int(saved.get("minimum_lr_bad_validations", 0))
        physical_batches = int(saved.get("physical_batches", 0))
        long_input_tokens_seen = int(saved.get("long_input_tokens_seen", 0))
        muon_start_tokens = int(
            saved.get("muon_start_tokens", long_input_tokens_seen)
        )
        # The shuffled HF stream state can reference many old parquet shards.
        # Replaying it on a new node creates a burst of range requests and can
        # rate-limit all ranks; model/optimizer continuity does not require
        # exact sample-URL continuity for this stochastic trillion-token run.
        stream_states = None if CONFIG["reset_stream"] else saved.get("stream_states")
        if (
            not wandb_id
            and saved_schema == "exp9-standard-muon-v1"
            and not CONFIG["reset_optimizer"]
        ):
            wandb_id = str(saved.get("wandb_id", ""))
        for group in optimizer.body.param_groups:
            group["lr"] = current_lr
        for group in optimizer.vocabulary.param_groups:
            group["lr"] = current_vocabulary_lr

    stream = None
    local_stream_state = None
    if long_run:
        if int(CONFIG["train_tokens"]) % (CONFIG["context_length"] * world):
            raise RuntimeError("train_tokens must divide evenly across ranks and contexts")
        local_stream_state = stream_states[rank] if stream_states else None
        stream = FineWebEduStream(rank, world, physical_local, local_stream_state)

    compiled = (
        torch.compile(student, fullgraph=True, dynamic=False)
        if CONFIG["compile"]
        else student
    )
    model = DDP(
        compiled, device_ids=[local_rank], broadcast_buffers=False, gradient_as_bucket_view=True
    )

    run = None
    if primary:
        key = os.environ.get("WANDB_API_KEY")
        if not key:
            raise RuntimeError("WANDB_API_KEY is required")
        import wandb

        wandb.login(key=key, relogin=True)
        if not wandb_id:
            wandb_id = wandb.util.generate_id()
        run = wandb.init(
            project=CONFIG["wandb_project"],
            name=CONFIG["run_name"],
            id=wandb_id,
            resume="allow",
            config={
                **CONFIG,
                **metadata,
                "schema": "exp9-standard-muon-v1",
                "parameters": parameter_count(),
                "world_size": world,
                "physical_global_batch": physical_local * world,
                "physical_global_token_batch": physical_local * world * CONFIG["context_length"],
                "optimizer_global_batch": global_batch,
                "optimizer_global_token_batch": global_tokens,
                "long_input_token_budget": int(CONFIG["train_tokens"]) if long_run else 0,
                "muon_start_tokens": muon_start_tokens,
            },
        )
        if not run.url:
            raise RuntimeError("W&B did not return a direct run URL")
        print(f"WANDB_URL={run.url}", flush=True)

    dist.barrier()
    initial = evaluate(
        compiled, teacher, validation_contexts, validation_targets, rank, world, device
    )
    best_kl = min(best_kl, initial["validation_kl"])
    if not math.isfinite(plateau_reference):
        plateau_reference = initial["validation_kl"]
    if primary:
        run.log(
            {
                **initial,
                "contexts_seen": total_contexts,
                "input_tokens_seen": total_contexts * CONFIG["context_length"],
                "long_input_tokens_seen": long_input_tokens_seen,
                "lr": current_lr,
                "muon_lr": current_lr,
                "vocabulary_lr": current_vocabulary_lr,
            },
            step=total_contexts * CONFIG["context_length"],
        )
        print(json.dumps({"initial_validation": initial, **metadata}), flush=True)

    output = Path(CONFIG["output_dir"])
    session_start_contexts = run_contexts
    session_start_long_tokens = long_input_tokens_seen
    started = time.perf_counter()
    max_interval_tokens_per_second = 0.0
    status = "running"
    # Interval rates must start at the resumed counters, not at zero.
    last_log_contexts = run_contexts
    last_log_tokens = long_input_tokens_seen
    last_log_time = started
    teacher_seconds = 0.0
    data_wait_seconds = 0.0
    producer_seconds = 0.0
    stream_queue_depth = 0
    last_loss = math.nan
    last_kl = math.nan
    interval_loss = 0.0
    interval_kl = 0.0
    interval_grad_norm = 0.0
    interval_steps = 0
    train_budget = int(CONFIG["train_contexts"])
    if CONFIG["mode"] == "benchmark":
        # Three teacher passes expose steady end-to-end throughput after the
        # first pass pays compilation. A one-pass result measures compilation,
        # while per-optimizer-step timing can omit the frozen teacher entirely.
        # The stop is absolute because resumed runs retain run_contexts.
        train_budget = benchmark_context_budget(run_contexts, physical_local, world)

    def unfinished() -> bool:
        return (
            long_input_tokens_seen < int(CONFIG["train_tokens"])
            if long_run
            else run_contexts < train_budget
        )

    while status == "running" and unfinished():
        if CONFIG["max_hours"] > 0 and time.perf_counter() - started >= CONFIG["max_hours"] * 3600:
            status = "timeout"
            break
        physical_batches += 1
        previous_outer_tokens = long_input_tokens_seen
        if long_run:
            remaining_tokens = int(CONFIG["train_tokens"]) - long_input_tokens_seen
            local_count = min(
                physical_local,
                exact_local_contexts(remaining_tokens, world),
            )
            streamed, local_stream_state, waited, produced, stream_queue_depth = stream.next()
            data_wait_seconds += waited
            producer_seconds += produced
            token_ids = torch.as_tensor(
                np.asarray(streamed[:local_count], dtype=np.int64),
                device=device,
            )
        else:
            local_count = physical_local
            index = batch_indices(len(contexts), physical_batches, rank, world)
            token_ids = torch.as_tensor(np.asarray(contexts[index], dtype=np.int64), device=device)
        teacher_started = time.perf_counter()
        # no_grad (not inference_mode): the frozen target is later consumed by
        # compiled autograd, which may need to save it for the student backward.
        with torch.no_grad(), torch.autocast("cuda", dtype=torch.bfloat16):
            probability, entropy = teacher_targets(teacher, token_ids)
        teacher_seconds += time.perf_counter() - teacher_started
        for start in range(0, local_count, optimizer_local):
            if not long_run and run_contexts >= train_budget:
                break
            stop = min(start + optimizer_local, local_count)
            step_local_contexts = stop - start
            step_global_contexts = step_local_contexts * world
            step_tokens = step_global_contexts * CONFIG["context_length"]
            previous_contexts = run_contexts
            optimizer.zero_grad(set_to_none=True)
            with torch.autocast("cuda", dtype=torch.bfloat16):
                kl_loss = model(
                    token_ids[start:stop],
                    probability[start:stop],
                    entropy[start:stop],
                )
            kl_loss.backward()
            grad_norm = torch.nn.utils.clip_grad_norm_(student.parameters(), 1.0)
            if not torch.isfinite(kl_loss) or not torch.isfinite(grad_norm):
                raise RuntimeError("non-finite loss or gradient")
            if long_run:
                schedule_progress = long_input_tokens_seen - muon_start_tokens
                schedule_budget = int(CONFIG["train_tokens"]) - muon_start_tokens
                step_lr = cosine_lr(
                    float(CONFIG["lr"]),
                    schedule_progress,
                    schedule_budget,
                    float(CONFIG["min_lr"]),
                )
                muon_warmup = min(
                    1.0,
                    (schedule_progress + step_tokens)
                    / max(1, int(CONFIG["muon_warmup_tokens"])),
                )
                step_lr *= muon_warmup
                step_vocabulary_lr = cosine_lr(
                    float(CONFIG["vocabulary_lr"]),
                    schedule_progress,
                    schedule_budget,
                    float(CONFIG["vocabulary_min_lr"]),
                )
                current_lr = step_lr
                current_vocabulary_lr = step_vocabulary_lr
            else:
                warmup = min(
                    1.0,
                    (run_contexts + global_batch) / max(1, CONFIG["warmup_contexts"]),
                )
                step_lr = current_lr * warmup
                step_vocabulary_lr = current_vocabulary_lr
            for group in optimizer.body.param_groups:
                group["lr"] = step_lr
            for group in optimizer.vocabulary.param_groups:
                group["lr"] = step_vocabulary_lr
            next_run_contexts = run_contexts + step_global_contexts
            next_long_tokens = long_input_tokens_seen + step_tokens
            first_session_update = session_optimizer_updates == 0
            measure_update = (
                due(last_log_tokens, next_long_tokens, CONFIG["log_tokens"])
                if long_run
                else due(last_log_contexts, next_run_contexts, CONFIG["log_contexts"])
            ) or first_session_update
            gradient_metrics = gradient_diagnostics(student) if measure_update else {}
            if measure_update:
                probability_sum_error = (
                    probability[start:stop].sum(-1, dtype=torch.float32) - 1
                ).abs()
                probability_metrics = torch.stack(
                    (probability_sum_error.mean(), probability_sum_error.max())
                )
            else:
                probability_metrics = None
            before_update = (
                [parameter.detach().clone() for parameter in student.parameters()]
                if measure_update
                else None
            )
            optimizer.step()
            optimizer_finite = True
            if measure_update:
                optimizer_finite = optimizer_states_finite(optimizer) and all(
                    torch.isfinite(parameter).all()
                    for parameter in student.parameters()
                )
                if not optimizer_finite:
                    raise RuntimeError("non-finite parameter or optimizer state")
            if measure_update:
                diagnostics = update_diagnostics(
                    student, before_update, prefix="optimizer_raw_"
                )
                before_projection = [
                    parameter.detach().clone() for parameter in student.blocks.parameters()
                ]
            else:
                diagnostics = {}
                before_projection = None
            student.canonicalize_factors_()
            if measure_update:
                diagnostics.update(
                    body_change_diagnostics(
                        student, before_projection, "canonical_projection_"
                    )
                )
                diagnostics.update(update_diagnostics(student, before_update))
                diagnostics.update(scale_diagnostics(student))
                with torch.autocast("cuda", dtype=torch.bfloat16):
                    diagnostics.update(
                        activation_scale_diagnostics(
                            student, token_ids[start : min(stop, start + 32)]
                        )
                    )
            optimizer_updates += 1
            session_optimizer_updates += 1
            run_contexts += step_global_contexts
            total_contexts += step_global_contexts
            if long_run:
                long_input_tokens_seen += step_tokens
            stopping = torch.tensor(
                int(terminate_requested), device=device, dtype=torch.int32
            )
            dist.all_reduce(stopping, op=dist.ReduceOp.MAX)
            if stopping.item():
                status = "interrupted"
            last_kl = float(kl_loss.detach())
            last_loss = last_kl + float(entropy[start:stop].mean())
            interval_loss += last_loss
            interval_kl += last_kl
            interval_grad_norm += float(grad_norm)
            interval_steps += 1

            log_due = (
                due(last_log_tokens, long_input_tokens_seen, CONFIG["log_tokens"])
                if long_run
                else due(last_log_contexts, run_contexts, CONFIG["log_contexts"])
            )
            if log_due or session_optimizer_updates == 1:
                local_interval_kl = interval_kl / interval_steps
                rank_kls = [torch.empty((), device=device) for _ in range(world)]
                dist.all_gather(
                    rank_kls,
                    torch.tensor(local_interval_kl, device=device),
                )
                stacked_rank_kls = torch.stack(rank_kls)
                probability_values = [
                    torch.empty_like(probability_metrics) for _ in range(world)
                ]
                dist.all_gather(probability_values, probability_metrics)
                stacked_probability = torch.stack(probability_values)
                values = torch.tensor(
                    [
                        interval_loss / interval_steps,
                        interval_kl / interval_steps,
                        interval_grad_norm / interval_steps,
                    ],
                    device=device,
                )
                dist.all_reduce(values)
                values /= world
                now = time.perf_counter()
                interval_tokens = (
                    long_input_tokens_seen - last_log_tokens
                    if long_run
                    else (run_contexts - last_log_contexts) * CONFIG["context_length"]
                )
                interval_seconds = now - last_log_time
                interval_tokens_per_second = interval_tokens / interval_seconds
                max_interval_tokens_per_second = max(
                    max_interval_tokens_per_second, interval_tokens_per_second
                )
                memory = torch.tensor(
                    [
                        torch.cuda.max_memory_allocated() / 2**30,
                        torch.cuda.max_memory_reserved() / 2**30,
                    ],
                    device=device,
                )
                memories = [torch.empty_like(memory) for _ in range(world)]
                dist.all_gather(memories, memory)
                metrics = {
                    "train_cross_entropy": float(values[0]),
                    "train_kl": float(values[1]),
                    "train_cross_entropy_last": last_loss,
                    "train_kl_last": last_kl,
                    "train_kl_rank_std": float(stacked_rank_kls.std(unbiased=False)),
                    "train_kl_rank_min": float(stacked_rank_kls.min()),
                    "train_kl_rank_max": float(stacked_rank_kls.max()),
                    "grad_norm_pre_clip": float(values[2]),
                    "grad_norm_pre_clip_last": float(grad_norm),
                    "gradient_clipped": float(values[2] > 1.0),
                    "optimizer_states_finite": float(optimizer_finite),
                    "teacher_probability_sum_abs_error": float(
                        stacked_probability[:, 0].mean()
                    ),
                    "teacher_probability_sum_max_error": float(
                        stacked_probability[:, 1].max()
                    ),
                    "lr": step_lr,
                    "muon_lr": step_lr,
                    "vocabulary_lr": step_vocabulary_lr,
                    "optimizer_updates": optimizer_updates,
                    "session_optimizer_updates": session_optimizer_updates,
                    "run_contexts": run_contexts,
                    "contexts_seen": total_contexts,
                    "input_tokens_seen": total_contexts * CONFIG["context_length"],
                    "long_input_tokens_seen": long_input_tokens_seen,
                    "long_input_token_budget": int(CONFIG["train_tokens"]) if long_run else 0,
                    "optimizer_global_batch": step_global_contexts,
                    "optimizer_global_token_batch": step_tokens,
                    "tokens_per_second": interval_tokens_per_second,
                    "end_to_end_tokens_per_second": (
                        (
                            long_input_tokens_seen - session_start_long_tokens
                            if long_run
                            else (run_contexts - session_start_contexts)
                            * CONFIG["context_length"]
                        )
                        / max(now - started, 1e-9)
                    ),
                    "teacher_target_seconds": teacher_seconds,
                    "data_wait_seconds": data_wait_seconds,
                    "producer_seconds": producer_seconds,
                    "stream_queue_depth": stream_queue_depth if long_run else 0,
                    "stream_epoch": (
                        int(local_stream_state["epoch"])
                        if long_run and local_stream_state is not None
                        else 0
                    ),
                    "per_gpu_peak_allocated_gib": [float(value[0]) for value in memories],
                    "per_gpu_peak_reserved_gib": [float(value[1]) for value in memories],
                    **gradient_metrics,
                    **diagnostics,
                }
                if primary:
                    print(json.dumps(metrics), flush=True)
                    run.log(metrics, step=total_contexts * CONFIG["context_length"])
                last_log_contexts = run_contexts
                last_log_tokens = long_input_tokens_seen
                last_log_time = now
                interval_loss = 0.0
                interval_kl = 0.0
                interval_grad_norm = 0.0
                interval_steps = 0

            validation = None
            if not long_run and due(previous_contexts, run_contexts, CONFIG["eval_contexts"]):
                validation = evaluate(
                    compiled, teacher, validation_contexts, validation_targets, rank, world, device
                )
                improved = validation["validation_kl"] < best_kl
                best_kl = min(best_kl, validation["validation_kl"])
                if validation["validation_kl"] <= plateau_reference - CONFIG["plateau_delta"]:
                    plateau_reference = validation["validation_kl"]
                    bad_validations = 0
                    minimum_lr_bad_validations = 0
                else:
                    bad_validations += 1
                    if current_lr <= CONFIG["min_lr"]:
                        minimum_lr_bad_validations += 1
                if bad_validations >= CONFIG["plateau_patience"] and current_lr > CONFIG["min_lr"]:
                    current_lr = max(CONFIG["min_lr"], current_lr / 2)
                    plateau_reference = validation["validation_kl"]
                    bad_validations = 0
                if primary:
                    run.log(
                        {**validation, "best_validation_kl": best_kl, "lr": current_lr},
                        step=total_contexts * CONFIG["context_length"],
                    )
                    print(json.dumps(validation), flush=True)
                if validation["validation_kl"] <= CONFIG["target_kl"]:
                    status = "target"
                elif minimum_lr_bad_validations >= CONFIG["min_lr_stagnant_evals"]:
                    status = "plateau"

            state = {
                "optimizer_updates": optimizer_updates,
                "optimizer_name": CONFIG["optimizer"],
                "contexts_seen": total_contexts,
                "run_contexts": run_contexts,
                "physical_batches": physical_batches,
                "current_lr": current_lr,
                "current_muon_lr": current_lr,
                "current_vocabulary_lr": current_vocabulary_lr,
                "muon_start_tokens": muon_start_tokens,
                "best_validation_kl": best_kl,
                "plateau_reference_kl": plateau_reference,
                "bad_validations": bad_validations,
                "minimum_lr_bad_validations": minimum_lr_bad_validations,
                "long_input_tokens_seen": long_input_tokens_seen,
                "stream_states": stream_states,
                "wandb_id": wandb_id,
                "last_validation": validation,
            }
            if not long_run and (
                validation is not None
                or due(previous_contexts, run_contexts, CONFIG["checkpoint_contexts"])
            ):
                if primary:
                    save_checkpoint(output / "checkpoint.pt", student, optimizer, state, metadata, run.url)
                    if validation is not None and improved:
                        save_checkpoint(output / "best.pt", student, optimizer, state, metadata, run.url)
                dist.barrier()
            if status != "running":
                break

        if long_run:
            validation = None
            eval_due = due(
                previous_outer_tokens,
                long_input_tokens_seen,
                int(CONFIG["eval_tokens"]),
            ) or long_input_tokens_seen == int(CONFIG["train_tokens"])
            checkpoint_due = due(
                previous_outer_tokens,
                long_input_tokens_seen,
                int(CONFIG["checkpoint_tokens"]),
            ) or long_input_tokens_seen == int(CONFIG["train_tokens"])
            improved = False
            if eval_due:
                validation = evaluate(
                    compiled,
                    teacher,
                    validation_contexts,
                    validation_targets,
                    rank,
                    world,
                    device,
                )
                improved = validation["validation_kl"] < best_kl
                best_kl = min(best_kl, validation["validation_kl"])
                if primary:
                    run.log(
                        {
                            **validation,
                            "best_validation_kl": best_kl,
                            "lr": current_lr,
                            "long_input_tokens_seen": long_input_tokens_seen,
                        },
                        step=total_contexts * CONFIG["context_length"],
                    )
                    print(json.dumps(validation), flush=True)
            if checkpoint_due or eval_due:
                gathered_states = [None for _ in range(world)]
                dist.all_gather_object(gathered_states, local_stream_state)
                stream_states = gathered_states
                state = {
                    "optimizer_updates": optimizer_updates,
                    "optimizer_name": CONFIG["optimizer"],
                    "contexts_seen": total_contexts,
                    "run_contexts": run_contexts,
                    "physical_batches": physical_batches,
                    "current_lr": current_lr,
                    "current_muon_lr": current_lr,
                    "current_vocabulary_lr": current_vocabulary_lr,
                    "muon_start_tokens": muon_start_tokens,
                    "best_validation_kl": best_kl,
                    "plateau_reference_kl": plateau_reference,
                    "bad_validations": bad_validations,
                    "minimum_lr_bad_validations": minimum_lr_bad_validations,
                    "long_input_tokens_seen": long_input_tokens_seen,
                    "stream_states": stream_states,
                    "wandb_id": wandb_id,
                    "last_validation": validation,
                }
                if primary:
                    save_checkpoint(
                        output / "checkpoint.pt",
                        student,
                        optimizer,
                        state,
                        metadata,
                        run.url,
                    )
                    if validation is not None and improved:
                        save_checkpoint(
                            output / "best.pt",
                            student,
                            optimizer,
                            state,
                            metadata,
                            run.url,
                        )
                dist.barrier()
        del token_ids, probability, entropy

    if status == "running":
        status = "complete"
    elapsed = time.perf_counter() - started
    final = evaluate(
        compiled, teacher, validation_contexts, validation_targets, rank, world, device
    )
    best_kl = min(best_kl, final["validation_kl"])
    state = {
        "optimizer_updates": optimizer_updates,
        "optimizer_name": CONFIG["optimizer"],
        "contexts_seen": total_contexts,
        "run_contexts": run_contexts,
        "physical_batches": physical_batches,
        "current_lr": current_lr,
        "current_muon_lr": current_lr,
        "current_vocabulary_lr": current_vocabulary_lr,
        "muon_start_tokens": muon_start_tokens,
        "best_validation_kl": best_kl,
        "plateau_reference_kl": plateau_reference,
        "bad_validations": bad_validations,
        "minimum_lr_bad_validations": minimum_lr_bad_validations,
        "long_input_tokens_seen": long_input_tokens_seen,
        "stream_states": stream_states,
        "wandb_id": wandb_id,
        "last_validation": final,
    }
    if primary:
        save_checkpoint(output / "checkpoint.pt", student, optimizer, state, metadata, run.url)
        if final["validation_kl"] <= best_kl:
            save_checkpoint(output / "best.pt", student, optimizer, state, metadata, run.url)
        result = {
            "schema": "exp9-standard-muon-result-v1",
            "status": status,
            **metadata,
            **final,
            "best_validation_kl": best_kl,
            "parameters": parameter_count(),
            "optimizer_updates": optimizer_updates,
            "run_contexts": run_contexts,
            "contexts_seen": total_contexts,
            "input_tokens_seen": total_contexts * CONFIG["context_length"],
            "long_input_tokens_seen": long_input_tokens_seen,
            "long_input_token_budget": int(CONFIG["train_tokens"]) if long_run else 0,
            "run_elapsed_seconds": elapsed,
            "tokens_per_second": (
                (
                    long_input_tokens_seen - session_start_long_tokens
                    if long_run
                    else (run_contexts - session_start_contexts)
                    * CONFIG["context_length"]
                )
                / max(elapsed, 1e-9)
            ),
            "max_interval_tokens_per_second": max_interval_tokens_per_second,
            "teacher_target_seconds": teacher_seconds,
            "data_wait_seconds": data_wait_seconds,
            "producer_seconds": producer_seconds,
            "physical_local_batch": physical_local,
            "optimizer_local_batch": optimizer_local,
            "optimizer_global_batch": global_batch,
            "optimizer_global_token_batch": global_tokens,
            "wandb_url": run.url,
        }
        output.mkdir(parents=True, exist_ok=True)
        (output / "result.json").write_text(json.dumps(result, indent=2))
        run.log(result, step=total_contexts * CONFIG["context_length"])
        run.finish()
        print(json.dumps(result, indent=2), flush=True)
    dist.barrier()
    dist.destroy_process_group()
    if long_run:
        # The HF/Arrow streaming stack owns background HTTP workers. A direct
        # successful process exit after W&B/checkpoint finalization avoids
        # racing those workers during CPython interpreter teardown.
        os._exit(0)


def study() -> None:
    """Sequential batch/LR screen followed by the exact 1T-token run."""
    if not os.environ.get("WANDB_API_KEY"):
        raise RuntimeError("WANDB_API_KEY is required before the paid study")
    root = Path(CONFIG["study_root"])
    root.mkdir(parents=True, exist_ok=True)
    gpu_memory_gib = torch.cuda.get_device_properties(0).total_memory / 2**30
    h200 = gpu_memory_gib >= 100
    base = [
        sys.executable,
        "-m",
        "torch.distributed.run",
        "--standalone",
        "--nproc_per_node=8",
        str(Path(__file__).resolve()),
    ]

    def launch(arguments: list[str], check: bool = True) -> bool:
        print(json.dumps({"launch": arguments}), flush=True)
        completed = subprocess.run(base + arguments, check=False)
        if check and completed.returncode:
            raise RuntimeError(f"child launch failed with exit code {completed.returncode}")
        return completed.returncode == 0

    # First probe physical teacher-buffer capacity with the known-large
    # 2.1M-token optimizer batch. Once a physical size fits, search optimizer
    # batches downward from the one-step maximum. This avoids recompiling
    # several optimizer shapes after the teacher buffer itself already OOMed.
    physical_candidates = (98_304,) if h200 else (65_536, 49_152)
    benchmarks = []
    failed_benchmarks = []

    def benchmark(physical_local: int, optimizer_local: int) -> bool:
        output = root / f"bench-p{physical_local}-o{optimizer_local}"
        arguments = [
            "--mode=benchmark",
            f"--optimizer_local_batch={optimizer_local}",
            f"--physical_local_batch={physical_local}",
            "--max_hours=0.5",
            f"--log_contexts={physical_local * 8}",
            "--eval_contexts=0",
            "--checkpoint_contexts=0",
            f"--output_dir={output}",
            f"--run_name=exp9-standard-muon-bench-p{physical_local}-o{optimizer_local}",
        ]
        if launch(arguments, check=False) and (output / "result.json").is_file():
            result = json.loads((output / "result.json").read_text())
            benchmarks.append(result)
            return True
        else:
            failed_benchmarks.append(
                {
                    "physical_local_batch": physical_local,
                    "optimizer_local_batch": optimizer_local,
                    "status": "failed_or_oom",
                }
            )
            return False

    for physical_candidate in physical_candidates:
        probe = 16_384
        if not benchmark(physical_candidate, probe):
            continue
        optimizer_candidates = (
            (physical_candidate, physical_candidate // 2, physical_candidate // 3)
            if physical_candidate % 3 == 0
            else (physical_candidate, physical_candidate // 2)
        )
        for optimizer_candidate in optimizer_candidates:
            if optimizer_candidate != probe:
                benchmark(physical_candidate, optimizer_candidate)
    if not benchmarks:
        raise RuntimeError("all ambitious batch candidates failed")
    winner = max(
        benchmarks, key=lambda value: value["max_interval_tokens_per_second"]
    )
    # The physical batch is part of the W&B config; recover it from the result
    # metadata rather than inferring it from optimizer step size.
    physical_local = int(winner["physical_local_batch"])
    winner_batch = int(winner["optimizer_global_batch"]) // 8

    screen_results = []
    for lr in (0.0001, 0.0003, 0.001, 0.003):
        label = str(lr).replace(".", "p")
        output = root / f"lr-{label}"
        launch(
            [
                "--mode=train",
                f"--optimizer_local_batch={winner_batch}",
                f"--physical_local_batch={physical_local}",
                "--train_contexts=16777216",
                "--max_hours=1.0",
                f"--lr={lr}",
                f"--min_lr={lr / 8}",
                "--eval_contexts=16777216",
                "--checkpoint_contexts=16777216",
                f"--output_dir={output}",
                f"--run_name=exp9-standard-muon-lr-{label}",
            ]
        )
        screen_results.append((lr, output, json.loads((output / "result.json").read_text())))
    winner_lr, winner_output, winner = min(
        screen_results, key=lambda item: (item[2]["validation_kl"], item[0])
    )
    first_winner = winner
    # Keep the winning optimizer state and demand a second monotonic validation
    # interval before it is eligible for the 1T-token continuation.
    launch(
        [
            "--mode=train",
            f"--optimizer_local_batch={winner_batch}",
            f"--physical_local_batch={physical_local}",
            "--train_contexts=33554432",
            "--max_hours=1.0",
            f"--lr={winner_lr}",
            f"--min_lr={winner_lr / 100}",
            "--eval_contexts=16777216",
            "--checkpoint_contexts=16777216",
            f"--resume={winner_output / 'checkpoint.pt'}",
            f"--output_dir={winner_output}",
            f"--run_name=exp9-standard-muon-lr-{str(winner_lr).replace('.', 'p')}",
        ]
    )
    winner = json.loads((winner_output / "result.json").read_text())
    if winner["validation_kl"] >= first_winner["validation_kl"]:
        raise RuntimeError(
            "winning LR failed the required second monotonic validation interval"
        )

    summary = {
        "schema": "exp9-standard-muon-study-v1",
        "status": "screen_complete",
        "winner_optimizer_local_batch": winner_batch,
        "gpu_memory_gib": gpu_memory_gib,
        "physical_local_batch": physical_local,
        "winner_lr": winner_lr,
        "benchmarks": benchmarks,
        "failed_benchmarks": failed_benchmarks,
        "screens": [value for _, _, value in screen_results],
        "screen_winner": winner,
    }
    (root / "study.json").write_text(json.dumps(summary, indent=2))
    print(json.dumps(summary, indent=2), flush=True)
    long_supervisor()


def screen_winner() -> tuple[float, Path, dict]:
    root = Path(CONFIG["study_root"])
    candidates = []
    for lr in (0.0001, 0.0003, 0.001, 0.003):
        output = root / f"lr-{str(lr).replace('.', 'p')}"
        result_path = output / "result.json"
        if not result_path.is_file():
            raise RuntimeError(f"screen is incomplete: {result_path}")
        candidates.append((lr, output, json.loads(result_path.read_text())))
    return min(candidates, key=lambda item: (item[2]["validation_kl"], item[0]))


def long_supervisor() -> None:
    """Run and resume the exact 1T-token job after selecting the screen winner."""
    if not os.environ.get("WANDB_API_KEY"):
        raise RuntimeError("WANDB_API_KEY is required before the paid long run")
    if CONFIG["resume"]:
        winner_lr = math.nan
        winner_output = Path(CONFIG["resume"]).parent
        winner = {"status": "explicit_resume", "checkpoint": CONFIG["resume"]}
    else:
        winner_lr, winner_output, winner = screen_winner()
    configured_long_lr = float(CONFIG["long_lr"])
    if configured_long_lr > 0:
        long_lr = configured_long_lr
    elif math.isfinite(winner_lr):
        long_lr = winner_lr
    else:
        raise RuntimeError("an explicit resume also requires --long_lr")
    # An explicit ID lets an infrastructure-only restart continue the same
    # W&B run; normal launches still get a fresh identity.
    wandb_id = str(CONFIG["wandb_id"]) or uuid.uuid4().hex[:8]
    output = (
        Path(CONFIG["long_output_dir"])
        if CONFIG["long_output_dir"]
        else Path(CONFIG["study_root"]) / "long-1t"
    )
    output.mkdir(parents=True, exist_ok=True)
    gpu_memory_gib = torch.cuda.get_device_properties(0).total_memory / 2**30
    physical_local = int(CONFIG["long_physical_local_batch"]) or int(
        winner.get(
            "physical_local_batch",
            98_304 if gpu_memory_gib >= 100 else 49_152,
        )
    )
    optimizer_local = int(CONFIG["long_optimizer_local_batch"]) or int(
        winner.get("optimizer_local_batch", 16_384)
    )
    base = [
        sys.executable,
        "-m",
        "torch.distributed.run",
        "--standalone",
        "--nproc_per_node=8",
        str(Path(__file__).resolve()),
    ]
    failures = 0
    while True:
        checkpoint = output / "checkpoint.pt"
        resume = (
            checkpoint
            if checkpoint.is_file()
            else Path(CONFIG["resume"])
            if CONFIG["resume"]
            else winner_output / "best.pt"
        )
        if not resume.is_file():
            raise RuntimeError(f"long-run resume checkpoint is missing: {resume}")
        reset_optimizer = False
        arguments = [
            "--mode=long",
            f"--optimizer_local_batch={optimizer_local}",
            f"--physical_local_batch={physical_local}",
            "--train_tokens=1000000000000",
            "--max_hours=0",
            f"--lr={long_lr}",
            f"--min_lr={CONFIG['min_lr']}",
            f"--vocabulary_lr={CONFIG['vocabulary_lr']}",
            f"--vocabulary_min_lr={CONFIG['vocabulary_min_lr']}",
            f"--resume={resume}",
            f"--output_dir={output}",
            f"--run_name=exp9-standard-muon-1t-lr-{str(long_lr).replace('.', 'p')}",
            f"--wandb_id={wandb_id}",
            f"--reset_optimizer={str(reset_optimizer).lower()}",
            "--reset_stream=true",
        ]
        print(json.dumps({"long_launch": arguments, "screen_winner": winner}), flush=True)
        completed = subprocess.run(base + arguments, check=False)
        result_path = output / "result.json"
        if completed.returncode == 0 and result_path.is_file():
            result = json.loads(result_path.read_text())
            if (
                result.get("status") == "complete"
                and result.get("long_input_tokens_seen") == 1_000_000_000_000
            ):
                summary = {
                    "schema": "exp9-standard-muon-study-v2",
                    "status": "complete",
                    "winner_lr": winner_lr,
                    "long_lr": long_lr,
                    "screen_winner": winner,
                    "final": result,
                }
                (Path(CONFIG["study_root"]) / "study.json").write_text(
                    json.dumps(summary, indent=2)
                )
                print(json.dumps(summary, indent=2), flush=True)
                return
        failures += 1
        if failures >= 5:
            raise RuntimeError("1T run failed five consecutive times")
        print(
            json.dumps(
                {
                    "long_restart": failures,
                    "returncode": completed.returncode,
                    "resume": str(checkpoint),
                }
            ),
            flush=True,
        )
        time.sleep(min(30, 2**failures))


def stream_benchmark() -> None:
    rank = int(os.environ.get("LOCAL_RANK", "0"))
    world = int(os.environ.get("WORLD_SIZE", "1"))
    stream = FineWebEduStream(
        rank,
        world,
        int(CONFIG["physical_local_batch"]),
    )
    for batch in range(int(CONFIG["stream_benchmark_batches"])):
        contexts, _, waited, produced, depth = stream.next()
        result = {
            "rank": rank,
            "batch": batch,
            "contexts": len(contexts),
            "input_tokens": len(contexts) * CONFIG["context_length"],
            "wait_seconds": waited,
            "producer_seconds": produced,
            "input_tokens_per_second": (
                len(contexts) * CONFIG["context_length"] / max(produced, 1e-9)
            ),
            "queue_depth": depth,
        }
        print(json.dumps(result), flush=True)
    os._exit(0)


if __name__ == "__main__":
    overrides()
    if CONFIG["mode"] == "self_test":
        self_test()
    elif CONFIG["mode"] == "study":
        study()
    elif CONFIG["mode"] == "long_supervisor":
        long_supervisor()
    elif CONFIG["mode"] == "stream_benchmark":
        stream_benchmark()
    else:
        train()
