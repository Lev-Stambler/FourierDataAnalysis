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
    # self_test | study | benchmark | train | long | long_supervisor |
    # debug_prepare | debug_arm | debug_coordinator | debug_fresh_coordinator
    "mode": "train",
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
    "debug_stage": "fixed_kl",  # preflight | oracle | hidden | fixed_kl | fresh_kl
    "debug_init": "checkpoint",  # checkpoint | projection | model
    "debug_label": "",
    "debug_source": "",
    "debug_root": "/cache/exp9-random-projection-debug-v1",
    "debug_context_cache": "",
    "debug_stream_rank": 0,
    "debug_tokens": 536_870_912,
    "debug_replays": 128,
    "debug_warmup_replays": 32,
    "debug_projection_seed": 0,
    "debug_probe_examples": 64,
    "debug_deep_every": 16,
    "debug_eval_tokens": 67_108_864,
    "debug_regression_delta": 0.02,
    "debug_regression_patience": 2,
    "debug_wandb_id": "",
    "debug_barrier": "",
    "debug_ready": "",
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


def rademacher_projection(
    input_width: int,
    output_width: int,
    seed: int,
    *,
    device: torch.device | str = "cpu",
) -> torch.Tensor:
    """Seeded JL map with E[R R^T] = I, stored in an auditable exact form."""
    if input_width <= 0 or output_width <= 0:
        raise ValueError("projection widths must be positive")
    generator = torch.Generator(device="cpu").manual_seed(int(seed))
    signs = torch.randint(
        0,
        2,
        (input_width, output_width),
        generator=generator,
        dtype=torch.int8,
    )
    return (
        signs.to(device=device, dtype=torch.float32).mul_(2).sub_(1)
        / math.sqrt(output_width)
    )


def tensor_sha256(value: torch.Tensor) -> str:
    contiguous = value.detach().to(device="cpu").contiguous()
    return hashlib.sha256(contiguous.numpy().tobytes()).hexdigest()


@torch.no_grad()
def initialize_projected_vocabulary(
    student: "Student",
    teacher,
    projection: torch.Tensor,
    *,
    rows_per_chunk: int = 16_384,
) -> None:
    """Copy Qwen's tied table through the same projection used for hidden states."""
    source = teacher.get_input_embeddings().weight[: CONFIG["vocab_size"]]
    if source.shape[1] != projection.shape[0]:
        raise ValueError("teacher embedding and projection widths disagree")
    if projection.shape[1] != CONFIG["width"]:
        raise ValueError("projection output width must match the student")
    for start in range(0, len(source), rows_per_chunk):
        stop = min(start + rows_per_chunk, len(source))
        student.vocabulary[start:stop].copy_(
            (source[start:stop].float() @ projection).to(student.vocabulary.dtype)
        )


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
        terms = [
            parameter.grad.detach().float().square().sum()
            for parameter in parameters
            if parameter.grad is not None
        ]
        square = (
            sum(terms)
            if terms
            else torch.zeros((), device=parameters[0].device, dtype=torch.float32)
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


def _cosine(left: torch.Tensor, right: torch.Tensor) -> float:
    left = left.detach().float().flatten()
    right = right.detach().float().flatten()
    denominator = left.norm() * right.norm()
    return float(torch.dot(left, right) / denominator.clamp_min(1e-30))


@torch.no_grad()
def deep_optimizer_diagnostics(
    student: Student,
    optimizer: SplitOptimizer,
    before: list[torch.Tensor],
    raw: list[torch.Tensor],
) -> dict[str, float]:
    """Layerwise evidence for where gradients or Muon updates disappear."""
    metrics: dict[str, float] = {}
    final = [parameter.detach() for parameter in student.parameters()]
    for layer, block in enumerate(student.blocks):
        for factor_name, parameter_index, parameter in (
            ("a", 1 + 2 * layer, block.a),
            ("b", 2 + 2 * layer, block.b),
        ):
            old = before[parameter_index].float()
            raw_delta = raw[parameter_index].float() - old
            final_delta = final[parameter_index].float() - old
            gradient = parameter.grad
            prefix = f"layer_{layer:02d}_{factor_name}_"
            metrics[prefix + "grad_rms"] = (
                float(gradient.detach().float().square().mean().sqrt())
                if gradient is not None
                else 0.0
            )
            metrics[prefix + "raw_update_rms"] = float(
                raw_delta.square().mean().sqrt()
            )
            metrics[prefix + "final_update_rms"] = float(
                final_delta.square().mean().sqrt()
            )
            metrics[prefix + "canonical_retained"] = float(
                final_delta.norm() / raw_delta.norm().clamp_min(1e-30)
            )
            metrics[prefix + "raw_final_cosine"] = _cosine(
                raw_delta, final_delta
            )
            if gradient is not None:
                metrics[prefix + "gradient_final_update_cosine"] = _cosine(
                    gradient, -final_delta
                )
            state = optimizer.body.state.get(parameter, {})
            momentum = state.get("momentum_buffer")
            if momentum is not None:
                metrics[prefix + "momentum_rms"] = float(
                    momentum.float().square().mean().sqrt()
                )
                if gradient is not None:
                    metrics[prefix + "gradient_momentum_cosine"] = _cosine(
                        gradient, momentum
                    )

    vocabulary = student.vocabulary.detach().float()
    vocabulary_gradient = student.vocabulary.grad
    vocabulary_delta = final[0].float() - before[0].float()
    for name, rows in (
        ("vocabulary_weight_row_rms", vocabulary.square().mean(-1).sqrt()),
        (
            "vocabulary_update_row_rms",
            vocabulary_delta.square().mean(-1).sqrt(),
        ),
        (
            "vocabulary_grad_row_rms",
            vocabulary_gradient.detach().float().square().mean(-1).sqrt()
            if vocabulary_gradient is not None
            else torch.zeros(len(vocabulary), device=vocabulary.device),
        ),
    ):
        quantiles = torch.quantile(
            rows,
            torch.tensor((0.0, 0.01, 0.1, 0.5, 0.9, 0.99, 1.0), device=rows.device),
        )
        for label, value in zip(
            ("min", "p01", "p10", "p50", "p90", "p99", "max"),
            quantiles,
            strict=True,
        ):
            metrics[f"{name}_{label}"] = float(value)
    return metrics


@torch.no_grad()
def probe_diagnostics(
    student: Student,
    token_ids: torch.Tensor,
    probability: torch.Tensor,
    entropy: torch.Tensor,
    *,
    previous_hidden: torch.Tensor | None = None,
    previous_logits: torch.Tensor | None = None,
) -> tuple[dict[str, float], torch.Tensor, torch.Tensor]:
    hidden = student.hidden(token_ids)
    logits = F.linear(hidden, student.vocabulary)
    rows = exact_kl_rows(logits, probability, entropy)
    quantiles = torch.quantile(
        rows.float(),
        torch.tensor((0.0, 0.01, 0.1, 0.5, 0.9, 0.99, 1.0), device=rows.device),
    )
    metrics = {
        "probe_kl": float(rows.mean()),
        "probe_kl_std": float(rows.float().std(unbiased=False)),
        "probe_logit_rms": float(logits.float().square().mean().sqrt()),
        "probe_hidden_rms": float(hidden.float().square().mean().sqrt()),
        "probe_top1_agreement": float(
            (logits.argmax(-1) == probability.argmax(-1)).float().mean()
        ),
        "probe_teacher_entropy": float(entropy.mean()),
        "probe_teacher_top1_probability": float(probability.max(-1).values.mean()),
    }
    for label, value in zip(
        ("min", "p01", "p10", "p50", "p90", "p99", "max"),
        quantiles,
        strict=True,
    ):
        metrics[f"probe_kl_{label}"] = float(value)
    if previous_hidden is not None:
        metrics["probe_hidden_delta_rms"] = float(
            (hidden.float() - previous_hidden.float()).square().mean().sqrt()
        )
        metrics["probe_hidden_before_after_cosine"] = _cosine(
            hidden, previous_hidden
        )
    if previous_logits is not None:
        metrics["probe_logit_delta_rms"] = float(
            (logits.float() - previous_logits.float()).square().mean().sqrt()
        )
        metrics["probe_logit_before_after_cosine"] = _cosine(
            logits, previous_logits
        )
    return metrics, hidden.detach(), logits.detach()


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


@torch.no_grad()
def teacher_targets_with_hidden(
    teacher,
    token_ids: torch.Tensor,
    projection: torch.Tensor | None = None,
) -> tuple[torch.Tensor, torch.Tensor, torch.Tensor | None]:
    """Debug target pass: exact probabilities and optional projected final hidden."""
    probability = torch.empty(
        token_ids.shape[0],
        CONFIG["vocab_size"],
        device=token_ids.device,
        dtype=getattr(torch, CONFIG["teacher_probability_dtype"]),
    )
    entropy = torch.empty(
        token_ids.shape[0], device=token_ids.device, dtype=torch.float32
    )
    projected = (
        torch.empty(
            token_ids.shape[0],
            projection.shape[1],
            device=token_ids.device,
            dtype=torch.float32,
        )
        if projection is not None
        else None
    )
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
        if projected is not None:
            projected[start:stop].copy_(
                per_token_rms(hidden.float() @ projection)
            )
    return probability, entropy, projected


@torch.inference_mode()
def evaluate_debug(
    student: Student,
    teacher,
    contexts: np.ndarray,
    targets: np.ndarray,
    device: torch.device,
    *,
    examples: int | None = None,
) -> dict[str, float]:
    student.eval()
    limit = min(int(examples or CONFIG["eval_examples"]), len(contexts))
    totals = torch.zeros(5, device=device, dtype=torch.float64)
    for start in range(0, limit, CONFIG["eval_batch"]):
        stop = min(start + CONFIG["eval_batch"], limit)
        token_ids = torch.as_tensor(
            np.asarray(contexts[start:stop], dtype=np.int64), device=device
        )
        target = torch.as_tensor(
            np.asarray(targets[start:stop], dtype=np.int64), device=device
        )
        probability, log_probability, entropy = teacher_distribution(
            teacher, token_ids
        )
        with torch.autocast("cuda", dtype=torch.bfloat16):
            logits = student.logits(token_ids)
        totals[0] += exact_kl_rows(logits, probability, entropy).double().sum()
        totals[1] += F.cross_entropy(logits.float(), target, reduction="sum")
        totals[2] += (-log_probability.gather(1, target[:, None])).double().sum()
        totals[3] += (logits.argmax(-1) == target).double().sum()
        totals[4] += stop - start
    student.train()
    count = float(totals[4])
    return {
        "validation_kl": float(totals[0] / count),
        "validation_student_nll": float(totals[1] / count),
        "validation_teacher_nll": float(totals[2] / count),
        "validation_accuracy": float(totals[3] / count),
        "validation_examples": int(count),
    }


@torch.inference_mode()
def evaluate_projection_oracle(
    teacher,
    contexts: np.ndarray,
    projection: torch.Tensor,
    projected_vocabulary: torch.Tensor,
    device: torch.device,
) -> dict[str, float]:
    limit = min(int(CONFIG["eval_examples"]), len(contexts))
    total_kl = torch.zeros((), device=device, dtype=torch.float64)
    total_hidden_rms = torch.zeros_like(total_kl)
    total = 0
    weight = teacher.get_output_embeddings().weight[: CONFIG["vocab_size"]]
    for start in range(0, limit, CONFIG["eval_batch"]):
        stop = min(start + CONFIG["eval_batch"], limit)
        token_ids = torch.as_tensor(
            np.asarray(contexts[start:stop], dtype=np.int64), device=device
        )
        hidden = teacher.model(
            input_ids=token_ids, use_cache=False, return_dict=True
        ).last_hidden_state[:, -1]
        teacher_log_probability = F.log_softmax(
            F.linear(hidden, weight).float(), -1
        )
        probability = teacher_log_probability.exp()
        entropy = -(probability * teacher_log_probability).sum(-1)
        projected_hidden = per_token_rms(hidden.float() @ projection)
        projected_logits = F.linear(projected_hidden, projected_vocabulary)
        rows = exact_kl_rows(projected_logits, probability, entropy)
        total_kl += rows.double().sum()
        total_hidden_rms += (
            projected_hidden.float().square().mean(-1).sqrt().double().sum()
        )
        total += stop - start
    return {
        "projection_oracle_kl": float(total_kl / total),
        "projection_hidden_rms": float(total_hidden_rms / total),
        "validation_examples": total,
    }


@torch.inference_mode()
def evaluate_hidden_debug(
    student: Student,
    teacher,
    contexts: np.ndarray,
    projection: torch.Tensor,
    device: torch.device,
) -> dict[str, float]:
    limit = min(int(CONFIG["eval_examples"]), len(contexts))
    mse = torch.zeros((), device=device, dtype=torch.float64)
    cosine = torch.zeros_like(mse)
    total = 0
    student.eval()
    for start in range(0, limit, CONFIG["eval_batch"]):
        stop = min(start + CONFIG["eval_batch"], limit)
        token_ids = torch.as_tensor(
            np.asarray(contexts[start:stop], dtype=np.int64), device=device
        )
        teacher_hidden = teacher.model(
            input_ids=token_ids, use_cache=False, return_dict=True
        ).last_hidden_state[:, -1]
        target = per_token_rms(teacher_hidden.float() @ projection)
        with torch.autocast("cuda", dtype=torch.bfloat16):
            actual = student.hidden(token_ids)
        mse += (actual.float() - target).square().mean(-1).double().sum()
        cosine += F.cosine_similarity(actual.float(), target, dim=-1).double().sum()
        total += stop - start
    student.train()
    return {
        "hidden_validation_mse": float(mse / total),
        "hidden_validation_cosine": float(cosine / total),
        "validation_examples": total,
    }


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


def atomic_json(path: Path, value: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_text(json.dumps(value, indent=2, sort_keys=True))
    os.replace(temporary, path)


def wait_for_debug_barrier(run_url: str) -> None:
    if CONFIG["debug_ready"]:
        atomic_json(
            Path(CONFIG["debug_ready"]),
            {"pid": os.getpid(), "wandb_url": run_url, "label": CONFIG["debug_label"]},
        )
    if not CONFIG["debug_barrier"]:
        return
    barrier = Path(CONFIG["debug_barrier"])
    deadline = time.monotonic() + 600
    while not barrier.is_file():
        if time.monotonic() >= deadline:
            raise RuntimeError(f"timed out waiting for debug barrier {barrier}")
        time.sleep(0.25)


def init_debug_wandb(metadata: dict):
    key = os.environ.get("WANDB_API_KEY")
    if not key:
        raise RuntimeError("WANDB_API_KEY is required")
    import wandb

    wandb.login(key=key, relogin=True)
    run = wandb.init(
        project=CONFIG["wandb_project"],
        name=CONFIG["debug_label"],
        id=CONFIG["debug_wandb_id"] or None,
        resume="allow",
        allow_val_change=True,
        config={
            **CONFIG,
            **metadata,
            "schema": "exp9-random-projection-debug-v1",
            "parameters": parameter_count(),
            "world_size": 1,
            "optimizer_global_batch": int(CONFIG["optimizer_local_batch"]),
            "optimizer_global_token_batch": int(CONFIG["optimizer_local_batch"])
            * CONFIG["context_length"],
            "physical_global_batch": int(CONFIG["physical_local_batch"]),
            "physical_global_token_batch": int(CONFIG["physical_local_batch"])
            * CONFIG["context_length"],
        },
    )
    if not run.url:
        raise RuntimeError("W&B did not return a direct run URL")
    print(f"WANDB_URL={run.url}", flush=True)
    wait_for_debug_barrier(run.url)
    return run


@torch.no_grad()
def load_debug_student(
    teacher,
    device: torch.device,
) -> tuple[Student, torch.Tensor | None, dict]:
    torch.manual_seed(CONFIG["seed"])
    student = Student().to(device)
    projection = None
    metadata: dict[str, object] = {"debug_initialization": CONFIG["debug_init"]}
    if CONFIG["debug_init"] == "checkpoint":
        source = Path(CONFIG["debug_source"] or CONFIG["resume"])
        saved = torch.load(source, map_location="cpu", weights_only=False)
        student.load_state_dict(clean_state_dict(saved["model"]))
        student.canonicalize_factors_()
        metadata.update(
            {
                "source_checkpoint": str(source),
                "source_sha256": file_sha256(source),
            }
        )
    elif CONFIG["debug_init"] == "projection":
        teacher_width = teacher.get_input_embeddings().weight.shape[1]
        projection = rademacher_projection(
            teacher_width,
            CONFIG["width"],
            CONFIG["debug_projection_seed"],
            device=device,
        )
        initialize_projected_vocabulary(student, teacher, projection)
        student.canonicalize_factors_()
        metadata.update(
            {
                "projection_seed": int(CONFIG["debug_projection_seed"]),
                "projection_sha256": tensor_sha256(projection),
                "projected_vocabulary_sha256": tensor_sha256(student.vocabulary),
            }
        )
    elif CONFIG["debug_init"] == "model":
        source = Path(CONFIG["debug_source"])
        saved = torch.load(source, map_location="cpu", weights_only=False)
        student.load_state_dict(clean_state_dict(saved["model"]))
        student.canonicalize_factors_()
        projection_seed = int(saved["metadata"]["projection_seed"])
        projection = rademacher_projection(
            teacher.get_input_embeddings().weight.shape[1],
            CONFIG["width"],
            projection_seed,
            device=device,
        )
        metadata.update(saved["metadata"])
        metadata.update(
            {
                "debug_initialization": "projected_hidden_warmstart",
                "source_model": str(source),
                "source_sha256": file_sha256(source),
            }
        )
    else:
        raise ValueError(f"unsupported debug initialization {CONFIG['debug_init']}")
    return student, projection, metadata


def build_debug_optimizer(student: Student) -> SplitOptimizer:
    from bitsandbytes.optim import AdamW8bit

    return SplitOptimizer(
        BatchedMuon(
            student.blocks.parameters(),
            lr=CONFIG["lr"],
            momentum=CONFIG["muon_momentum"],
            ns_steps=CONFIG["muon_ns_steps"],
        ),
        AdamW8bit(
            [student.vocabulary],
            lr=CONFIG["vocabulary_lr"],
            betas=(0.9, 0.95),
            eps=1e-8,
            weight_decay=CONFIG["weight_decay"],
        ),
    )


def save_debug_model(path: Path, student: Student, metadata: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(".tmp")
    torch.save(
        {
            "schema": "exp9-debug-model-v1",
            "model": student.state_dict(),
            "metadata": metadata,
        },
        temporary,
    )
    os.replace(temporary, path)


def debug_numerical_preflight(
    student: Student,
    teacher,
    contexts: np.ndarray,
    device: torch.device,
) -> dict[str, float]:
    token_ids = torch.as_tensor(
        np.asarray(contexts[:16], dtype=np.int64), device=device
    )
    with torch.no_grad(), torch.autocast("cuda", dtype=torch.bfloat16):
        probability, entropy = teacher_targets(teacher, token_ids)
    optimizer = build_debug_optimizer(student)
    optimizer.zero_grad(set_to_none=True)
    with torch.autocast("cuda", dtype=torch.bfloat16):
        eager_loss = student(token_ids, probability, entropy)
    eager_loss.backward()
    eager_gradient = torch.cat(
        [
            student.vocabulary.grad[:1024].detach().float().flatten(),
            *[
                parameter.grad.detach().float().flatten()
                for parameter in student.blocks.parameters()
            ],
        ]
    )
    optimizer.zero_grad(set_to_none=True)
    compiled = (
        torch.compile(student, fullgraph=True, dynamic=False)
        if CONFIG["compile"]
        else student
    )
    with torch.autocast("cuda", dtype=torch.bfloat16):
        compiled_loss = compiled(token_ids, probability, entropy)
    compiled_loss.backward()
    compiled_gradient = torch.cat(
        [
            student.vocabulary.grad[:1024].detach().float().flatten(),
            *[
                parameter.grad.detach().float().flatten()
                for parameter in student.blocks.parameters()
            ],
        ]
    )
    grad_norm = torch.nn.utils.clip_grad_norm_(student.parameters(), 1.0)
    optimizer.step()
    student.canonicalize_factors_()
    finite = optimizer_states_finite(optimizer) and all(
        torch.isfinite(parameter).all() for parameter in student.parameters()
    )
    loss_error = abs(
        float(eager_loss.detach()) - float(compiled_loss.detach())
    )
    gradient_cosine = _cosine(eager_gradient, compiled_gradient)
    # FP32 compiled/eager is bit-identical on H100. BF16 fullgraph uses a
    # different GEMM/reduction fusion and differs by up to ~0.01 KL on this
    # full-vocabulary probe while retaining the same gradient direction.
    if loss_error > 2e-2 or gradient_cosine < 0.995 or not finite:
        raise RuntimeError(
            "compiled/eager preflight failed: "
            f"loss_error={loss_error} gradient_cosine={gradient_cosine} finite={finite}"
        )
    return {
        "preflight_eager_loss": float(eager_loss.detach()),
        "preflight_compiled_loss": float(compiled_loss.detach()),
        "preflight_loss_abs_error": loss_error,
        "preflight_gradient_cosine": gradient_cosine,
        "preflight_grad_norm": float(grad_norm),
        "preflight_finite": float(finite),
    }


def debug_train(
    student: Student,
    teacher,
    projection: torch.Tensor | None,
    train_contexts: np.ndarray | None,
    validation_contexts: np.ndarray,
    validation_targets: np.ndarray,
    device: torch.device,
    run,
    metadata: dict,
    train_stream: FineWebEduStream | None = None,
) -> dict:
    stage = CONFIG["debug_stage"]
    hidden_stage = stage == "hidden"
    if hidden_stage and projection is None:
        raise RuntimeError("hidden warm-start requires a projection")
    student.vocabulary.requires_grad_(not hidden_stage)
    optimizer = build_debug_optimizer(student)
    compiled = (
        torch.compile(student, fullgraph=True, dynamic=False)
        if CONFIG["compile"]
        else student
    )
    compiled_hidden = (
        torch.compile(student.hidden, fullgraph=True, dynamic=False)
        if CONFIG["compile"] and hidden_stage
        else student.hidden
    )
    physical = int(CONFIG["physical_local_batch"])
    optimizer_batch = int(CONFIG["optimizer_local_batch"])
    if physical % optimizer_batch:
        raise RuntimeError("debug physical batch must divide by optimizer batch")
    token_batch = optimizer_batch * CONFIG["context_length"]
    if token_batch < 100_000:
        raise RuntimeError("debug arm violates the 100k-token optimizer policy")

    initial = evaluate_debug(
        student,
        teacher,
        validation_contexts,
        validation_targets,
        device,
    )
    run.log(
        {
            **initial,
            "debug_stage": stage,
            "stage_event": 0,
            "validation_event": 0,
        }
    )
    print(json.dumps({"initial_validation": initial}), flush=True)
    fixed = stage in {"hidden", "fixed_kl"}
    fixed_token_ids = None
    fixed_probability = None
    fixed_entropy = None
    fixed_hidden = None
    if fixed:
        fixed_token_ids = torch.as_tensor(
            np.asarray(train_contexts[:physical], dtype=np.int64), device=device
        )
        with torch.no_grad(), torch.autocast("cuda", dtype=torch.bfloat16):
            fixed_probability, fixed_entropy, fixed_hidden = teacher_targets_with_hidden(
                teacher, fixed_token_ids, projection if hidden_stage else None
            )

    updates = 0
    stage_tokens = 0
    best_train_kl = math.inf
    last_train_kl = math.inf
    initial_train_kl = None
    initial_validation_kl = float(initial["validation_kl"])
    best_validation_kl = initial_validation_kl
    validation_event = 0
    regression_evals = 0
    early_stop_reason = None
    started = time.perf_counter()
    last_time = started
    last_tokens = 0
    deep_every = int(CONFIG["debug_deep_every"])
    next_eval = int(CONFIG["debug_eval_tokens"])
    stop_tokens = (
        int(CONFIG["debug_replays"]) * physical * CONFIG["context_length"]
        if fixed
        else int(CONFIG["debug_tokens"])
    )

    while stage_tokens < stop_tokens and early_stop_reason is None:
        if fixed:
            token_ids = fixed_token_ids
            probability = fixed_probability
            entropy = fixed_entropy
            projected_hidden = fixed_hidden
        else:
            if train_stream is not None:
                streamed, _, _, _, _ = train_stream.next()
                count = min(
                    len(streamed),
                    math.ceil((stop_tokens - stage_tokens) / CONFIG["context_length"]),
                )
                token_ids = torch.as_tensor(
                    np.asarray(streamed[:count], dtype=np.int64),
                    device=device,
                )
            else:
                if train_contexts is None:
                    raise RuntimeError("fresh training has no context source")
                start_context = stage_tokens // CONFIG["context_length"]
                count = min(physical, len(train_contexts) - start_context)
                if count <= 0:
                    break
                token_ids = torch.as_tensor(
                    np.asarray(
                        train_contexts[start_context : start_context + count],
                        dtype=np.int64,
                    ),
                    device=device,
                )
            with torch.no_grad(), torch.autocast("cuda", dtype=torch.bfloat16):
                probability, entropy, _ = teacher_targets_with_hidden(
                    teacher, token_ids
                )
            projected_hidden = None

        for start in range(0, len(token_ids), optimizer_batch):
            stop = min(start + optimizer_batch, len(token_ids))
            if stop - start <= 0 or stage_tokens >= stop_tokens:
                break
            optimizer.zero_grad(set_to_none=True)
            deep = updates == 0 or (updates + 1) % deep_every == 0
            probe_count = min(int(CONFIG["debug_probe_examples"]), stop - start)
            if deep:
                with torch.no_grad():
                    before_probe, probe_hidden, probe_logits = probe_diagnostics(
                        student,
                        token_ids[start : start + probe_count],
                        probability[start : start + probe_count],
                        entropy[start : start + probe_count],
                    )
                before = [
                    parameter.detach().clone() for parameter in student.parameters()
                ]
            else:
                before_probe = {}
                probe_hidden = None
                probe_logits = None
                before = None

            if hidden_stage:
                with torch.autocast("cuda", dtype=torch.bfloat16):
                    actual_hidden = compiled_hidden(token_ids[start:stop])
                    loss = (
                        actual_hidden.float()
                        - projected_hidden[start:stop].float()
                    ).square().mean()
                train_kl = float("nan")
            else:
                with torch.autocast("cuda", dtype=torch.bfloat16):
                    loss = compiled(
                        token_ids[start:stop],
                        probability[start:stop],
                        entropy[start:stop],
                    )
                train_kl = float(loss.detach())
                if initial_train_kl is None:
                    # The first loss is measured before the first optimizer
                    # update. Log it explicitly instead of leaving an
                    # ambiguous NaN at W&B event zero.
                    initial_train_kl = train_kl
                last_train_kl = train_kl
                best_train_kl = min(best_train_kl, train_kl)
            loss.backward()
            gradient_metrics = gradient_diagnostics(student)
            grad_norm = torch.nn.utils.clip_grad_norm_(student.parameters(), 1.0)
            if not torch.isfinite(loss) or not torch.isfinite(grad_norm):
                raise RuntimeError("non-finite debug loss or gradient")
            optimizer.step()
            if deep:
                raw = [
                    parameter.detach().clone() for parameter in student.parameters()
                ]
            else:
                raw = None
            student.canonicalize_factors_()
            if not optimizer_states_finite(optimizer):
                raise RuntimeError("non-finite debug optimizer state")

            contexts_this_step = stop - start
            tokens_this_step = contexts_this_step * CONFIG["context_length"]
            stage_tokens += tokens_this_step
            updates += 1
            torch.cuda.synchronize()
            now = time.perf_counter()
            metrics = {
                "debug_stage": stage,
                "stage_event": updates,
                "stage_tokens": stage_tokens,
                "optimizer_updates": updates,
                "train_loss": float(loss.detach()),
                "grad_norm_pre_clip": float(grad_norm),
                "gradient_clipped": float(grad_norm > 1.0),
                "optimizer_states_finite": 1.0,
                "muon_lr": float(CONFIG["lr"]),
                "vocabulary_lr": float(CONFIG["vocabulary_lr"]),
                "optimizer_global_batch": contexts_this_step,
                "optimizer_global_token_batch": tokens_this_step,
                "tokens_per_second": (stage_tokens - last_tokens)
                / max(now - last_time, 1e-9),
                "end_to_end_tokens_per_second": stage_tokens
                / max(now - started, 1e-9),
                "memory_allocated_gib": torch.cuda.memory_allocated() / 2**30,
                "memory_reserved_gib": torch.cuda.memory_reserved() / 2**30,
                "peak_allocated_gib": torch.cuda.max_memory_allocated() / 2**30,
                "peak_reserved_gib": torch.cuda.max_memory_reserved() / 2**30,
                **gradient_metrics,
            }
            if hidden_stage:
                metrics["train_hidden_mse"] = float(loss.detach())
            else:
                metrics.update(
                    {
                        "train_kl": train_kl,
                        "initial_train_kl": initial_train_kl,
                        "best_train_kl": best_train_kl,
                    }
                )
            if deep:
                metrics.update(
                    update_diagnostics(student, before, prefix="deep_")
                )
                metrics.update(
                    deep_optimizer_diagnostics(student, optimizer, before, raw)
                )
                metrics.update(scale_diagnostics(student))
                metrics.update(
                    activation_scale_diagnostics(
                        student, token_ids[start : start + probe_count]
                    )
                )
                after_probe, _, _ = probe_diagnostics(
                    student,
                    token_ids[start : start + probe_count],
                    probability[start : start + probe_count],
                    entropy[start : start + probe_count],
                    previous_hidden=probe_hidden,
                    previous_logits=probe_logits,
                )
                metrics.update(
                    {f"before_{key}": value for key, value in before_probe.items()}
                )
                metrics.update(after_probe)
                metrics["probe_kl_step_delta"] = (
                    after_probe["probe_kl"] - before_probe["probe_kl"]
                )
            run.log(metrics)
            print(json.dumps(metrics), flush=True)
            last_time = now
            last_tokens = stage_tokens

            if not fixed and stage_tokens >= next_eval:
                validation = evaluate_debug(
                    student,
                    teacher,
                    validation_contexts,
                    validation_targets,
                    device,
                )
                validation_event += 1
                best_validation_kl = min(
                    best_validation_kl, float(validation["validation_kl"])
                )
                if (
                    float(validation["validation_kl"])
                    > initial_validation_kl + float(CONFIG["debug_regression_delta"])
                ):
                    regression_evals += 1
                else:
                    regression_evals = 0
                run.log(
                    {
                        **validation,
                        "debug_stage": stage,
                        "stage_tokens": stage_tokens,
                        "validation_event": validation_event,
                        "initial_validation_kl": initial_validation_kl,
                        "best_validation_kl": best_validation_kl,
                        "regression_evals": regression_evals,
                    }
                )
                print(json.dumps(validation), flush=True)
                next_eval += int(CONFIG["debug_eval_tokens"])
                if regression_evals >= int(CONFIG["debug_regression_patience"]):
                    early_stop_reason = "validation_regression"
                    break

        if not fixed:
            # Release the completed teacher batch before constructing the next
            # one. Python evaluates an assignment's right-hand side first, so
            # merely reassigning `probability` would temporarily retain two
            # 49,152 x 248,320 FP32 tensors and OOM an 80-GiB H100.
            del token_ids, probability, entropy, projected_hidden

    if hidden_stage:
        final = evaluate_hidden_debug(
            student, teacher, validation_contexts, projection, device
        )
    else:
        final = evaluate_debug(
            student,
            teacher,
            validation_contexts,
            validation_targets,
            device,
        )
    result = {
        "schema": "exp9-random-projection-debug-result-v1",
        "status": "complete",
        "debug_stage": stage,
        "debug_label": CONFIG["debug_label"],
        "stage_tokens": stage_tokens,
        "optimizer_updates": updates,
        "best_train_kl": None if hidden_stage else best_train_kl,
        "last_train_kl": None if hidden_stage else last_train_kl,
        "initial_train_kl": None if hidden_stage else initial_train_kl,
        "initial_validation_kl": initial_validation_kl,
        "best_validation_kl": best_validation_kl,
        "early_stop_reason": early_stop_reason,
        "elapsed_seconds": time.perf_counter() - started,
        "muon_lr": float(CONFIG["lr"]),
        "vocabulary_lr": float(CONFIG["vocabulary_lr"]),
        "optimizer_local_batch": optimizer_batch,
        "optimizer_global_token_batch": token_batch,
        "physical_local_batch": physical,
        "peak_allocated_gib": torch.cuda.max_memory_allocated() / 2**30,
        "peak_reserved_gib": torch.cuda.max_memory_reserved() / 2**30,
        "wandb_url": run.url,
        **metadata,
        **final,
    }
    return result


def debug_arm() -> None:
    if not torch.cuda.is_available() or torch.cuda.device_count() != 1:
        raise RuntimeError("a debug arm requires exactly one visible CUDA GPU")
    device = torch.device("cuda:0")
    torch.cuda.set_device(device)
    teacher = load_teacher(str(device))
    student, projection, metadata = load_debug_student(teacher, device)
    run = init_debug_wandb(metadata)
    output = Path(CONFIG["output_dir"])
    output.mkdir(parents=True, exist_ok=True)
    if CONFIG["debug_stage"] == "preflight":
        contexts, _, _ = load_split(CONFIG["data_root"], "train")
        result = {
            "schema": "exp9-random-projection-debug-result-v1",
            "status": "complete",
            "debug_stage": "preflight",
            "debug_label": CONFIG["debug_label"],
            "wandb_url": run.url,
            **metadata,
            **debug_numerical_preflight(student, teacher, contexts, device),
        }
    elif CONFIG["debug_stage"] == "oracle":
        if projection is None:
            raise RuntimeError("projection oracle requires projected initialization")
        validation_contexts, _, _ = load_split(CONFIG["data_root"], "validation")
        result = {
            "schema": "exp9-random-projection-debug-result-v1",
            "status": "complete",
            "debug_stage": "oracle",
            "debug_label": CONFIG["debug_label"],
            "wandb_url": run.url,
            **metadata,
            **evaluate_projection_oracle(
                teacher,
                validation_contexts,
                projection,
                student.vocabulary,
                device,
            ),
        }
    else:
        train_stream = None
        if CONFIG["debug_stage"] == "fresh_kl":
            if CONFIG["debug_context_cache"]:
                train_contexts = np.load(
                    CONFIG["debug_context_cache"], mmap_mode="r"
                )
            else:
                source = Path(CONFIG["debug_source"] or CONFIG["resume"])
                saved = torch.load(source, map_location="cpu", weights_only=False)
                stream_states = saved.get("stream_states")
                stream_rank = int(CONFIG["debug_stream_rank"])
                if not stream_states or not 0 <= stream_rank < len(stream_states):
                    raise RuntimeError(
                        f"checkpoint has no saved stream rank {stream_rank}"
                    )
                train_contexts = None
                train_stream = FineWebEduStream(
                    stream_rank,
                    len(stream_states),
                    int(CONFIG["physical_local_batch"]),
                    stream_states[stream_rank],
                )
        else:
            train_contexts, _, _ = load_split(CONFIG["data_root"], "train")
        validation_contexts, validation_targets, _ = load_split(
            CONFIG["data_root"], "validation"
        )
        try:
            result = debug_train(
                student,
                teacher,
                projection,
                train_contexts,
                validation_contexts,
                validation_targets,
                device,
                run,
                metadata,
                train_stream,
            )
        finally:
            if train_stream is not None:
                train_stream.close()
        save_debug_model(output / "student.pt", student, metadata)
    atomic_json(output / "result.json", result)
    run.log(result)
    run.finish()
    print(json.dumps(result, indent=2), flush=True)


def debug_prepare() -> None:
    output = Path(
        CONFIG["debug_context_cache"]
        or Path(CONFIG["debug_root"]) / "fresh-contexts.npy"
    )
    count = int(CONFIG["debug_tokens"]) // CONFIG["context_length"]
    output.parent.mkdir(parents=True, exist_ok=True)
    temporary = output.with_suffix(".tmp.npy")
    array = np.lib.format.open_memmap(
        temporary,
        mode="w+",
        dtype=np.int32,
        shape=(count, CONFIG["context_length"]),
    )
    stream_world = 1
    resume_states = [None]
    source_sha256 = None
    if CONFIG["debug_source"]:
        source = Path(CONFIG["debug_source"])
        if not source.is_file():
            raise RuntimeError(f"stream source checkpoint is missing: {source}")
        saved = torch.load(source, map_location="cpu", weights_only=False)
        stream_states = saved.get("stream_states")
        if not stream_states:
            raise RuntimeError("stream source checkpoint has no saved stream states")
        stream_world = len(stream_states)
        resume_states = stream_states
        source_sha256 = file_sha256(source)
    if int(CONFIG["physical_local_batch"]) % stream_world:
        raise RuntimeError("physical batch must divide the saved stream count")
    stream_batch = int(CONFIG["physical_local_batch"]) // stream_world
    # Each producer owns one saved dataset shard and tokenizes asynchronously.
    # Consuming them round-robin uses the node's CPUs and preserves all eight
    # post-checkpoint stream positions without making GPU training wait on text.
    streams = [
        FineWebEduStream(rank, stream_world, stream_batch, resume_states[rank])
        for rank in range(stream_world)
    ]
    written = 0
    final_stream_states = [None] * stream_world
    try:
        while written < count:
            for rank, stream in enumerate(streams):
                contexts, state, _, _, _ = stream.next()
                final_stream_states[rank] = state
                take = min(len(contexts), count - written)
                array[written : written + take] = contexts[:take]
                written += take
                if written % (int(CONFIG["physical_local_batch"]) * 16) == 0:
                    print(json.dumps({"debug_contexts_written": written}), flush=True)
                if written >= count:
                    break
    finally:
        for stream in streams:
            stream.close()
    array.flush()
    del array
    os.replace(temporary, output)
    atomic_json(
        output.with_suffix(".json"),
        {
            "schema": "exp9-fresh-context-cache-v1",
            "contexts": count,
            "input_tokens": count * CONFIG["context_length"],
            "shape": [count, CONFIG["context_length"]],
            "dtype": "int32",
            "sha256": file_sha256(output),
            "source_checkpoint": CONFIG["debug_source"] or None,
            "source_sha256": source_sha256,
            "stream_world": stream_world,
            "final_stream_positions": [
                (
                    {
                        "rank": rank,
                        "epoch": int(state["epoch"]),
                        "batch_ordinal": int(state["batch_ordinal"]),
                    }
                    if state
                    else None
                )
                for rank, state in enumerate(final_stream_states)
            ],
        },
    )
    print(json.dumps({"debug_context_cache": str(output), "contexts": count}), flush=True)


def _debug_arm_arguments(
    *,
    stage: str,
    label: str,
    init: str,
    source: Path,
    output: Path,
    wandb_id: str,
    barrier: Path,
    projection_seed: int,
    optimizer_batch: int,
    lr: float,
    vocabulary_lr: float,
    replays: int,
    context_cache: str | Path,
    stream_rank: int,
) -> list[str]:
    return [
        str(Path(__file__).resolve()),
        "--mode=debug_arm",
        f"--debug_stage={stage}",
        f"--debug_label={label}",
        f"--debug_init={init}",
        f"--debug_source={source}",
        f"--debug_projection_seed={projection_seed}",
        f"--optimizer_local_batch={optimizer_batch}",
        f"--physical_local_batch={CONFIG['physical_local_batch']}",
        f"--lr={lr}",
        f"--vocabulary_lr={vocabulary_lr}",
        f"--debug_replays={replays}",
        f"--debug_tokens={CONFIG['debug_tokens']}",
        f"--debug_context_cache={context_cache}",
        f"--debug_stream_rank={stream_rank}",
        f"--debug_wandb_id={wandb_id}",
        f"--debug_barrier={barrier}",
        f"--debug_ready={output / 'ready.json'}",
        f"--output_dir={output}",
        f"--run_name={label}",
        f"--debug_deep_every={CONFIG['debug_deep_every']}",
        f"--debug_eval_tokens={CONFIG['debug_eval_tokens']}",
        f"--debug_regression_delta={CONFIG['debug_regression_delta']}",
        f"--debug_regression_patience={CONFIG['debug_regression_patience']}",
    ]


def _run_debug_phase(root: Path, phase: str, specs: list[dict]) -> list[dict]:
    barrier = root / "barriers" / f"{phase}.go"
    barrier.parent.mkdir(parents=True, exist_ok=True)
    barrier.unlink(missing_ok=True)
    processes = []
    for gpu, spec in enumerate(specs):
        output = root / phase / spec["label"]
        output.mkdir(parents=True, exist_ok=True)
        (output / "ready.json").unlink(missing_ok=True)
        log_path = root / "logs" / f"{phase}-{spec['label']}.log"
        log_path.parent.mkdir(parents=True, exist_ok=True)
        log = log_path.open("ab")
        command = [
            sys.executable,
            *_debug_arm_arguments(
                stage=spec["stage"],
                label=spec["label"],
                init=spec["init"],
                source=Path(spec["source"]),
                output=output,
                wandb_id=spec["wandb_id"],
                barrier=barrier,
                projection_seed=int(spec.get("projection_seed", 0)),
                optimizer_batch=int(spec.get("optimizer_batch", 24_576)),
                lr=float(spec.get("lr", 0.002)),
                vocabulary_lr=float(spec.get("vocabulary_lr", 0.0003)),
                replays=int(spec.get("replays", 0)),
                context_cache=spec.get("context_cache", ""),
                stream_rank=int(spec.get("stream_rank", gpu)),
            ),
        ]
        process = subprocess.Popen(
            command,
            stdout=log,
            stderr=subprocess.STDOUT,
            env={
                **os.environ,
                "CUDA_VISIBLE_DEVICES": str(gpu),
                "PYTHONUNBUFFERED": "1",
                # Eight simultaneous default 32-worker Inductor pools
                # oversubscribe the node during first-shape compilation.
                "TORCHINDUCTOR_COMPILE_THREADS": "4",
            },
        )
        processes.append((spec, output, log_path, log, process))

    deadline = time.monotonic() + 900
    while True:
        failed = [
            (spec["label"], process.returncode)
            for spec, _, _, _, process in processes
            if process.poll() is not None
        ]
        if failed:
            for _, _, _, _, process in processes:
                if process.poll() is None:
                    process.terminate()
            raise RuntimeError(f"debug phase {phase} failed before barrier: {failed}")
        if all((output / "ready.json").is_file() for _, output, _, _, _ in processes):
            break
        if time.monotonic() >= deadline:
            for _, _, _, _, process in processes:
                process.terminate()
            raise RuntimeError(f"debug phase {phase} timed out waiting for W&B")
        time.sleep(1)
    barrier.write_text("go\n")
    atomic_json(
        root / "coordinator-status.json",
        {
            "schema": "exp9-debug-coordinator-v1",
            "status": "running",
            "phase": phase,
            "arms": [
                {
                    "gpu": gpu,
                    "label": spec["label"],
                    "pid": process.pid,
                    "log": str(log_path),
                    "ready": json.loads((output / "ready.json").read_text()),
                }
                for gpu, (spec, output, log_path, _, process) in enumerate(processes)
            ],
        },
    )
    failures = []
    while any(process.poll() is None for _, _, _, _, process in processes):
        for spec, _, _, _, process in processes:
            if process.poll() not in (None, 0):
                failures.append((spec["label"], process.returncode))
        if failures:
            for _, _, _, _, process in processes:
                if process.poll() is None:
                    process.terminate()
            break
        time.sleep(5)
    for _, _, _, log, process in processes:
        process.wait()
        log.close()
    if failures or any(process.returncode for _, _, _, _, process in processes):
        raise RuntimeError(
            f"debug phase {phase} failed: "
            f"{[(spec['label'], process.returncode, str(log_path)) for spec, _, log_path, _, process in processes]}"
        )
    return [
        json.loads((output / "result.json").read_text())
        for _, output, _, _, _ in processes
    ]


def debug_coordinator() -> None:
    if torch.cuda.device_count() != 8:
        raise RuntimeError("debug coordinator requires exactly eight H100/H200 GPUs")
    if not os.environ.get("WANDB_API_KEY"):
        raise RuntimeError("WANDB_API_KEY is required before the paid debug run")
    source = Path(CONFIG["debug_source"] or CONFIG["resume"])
    if not source.is_file():
        raise RuntimeError(f"production source checkpoint is missing: {source}")
    root = Path(CONFIG["debug_root"])
    root.mkdir(parents=True, exist_ok=True)
    context_cache = Path(
        CONFIG["debug_context_cache"] or root / "fresh-contexts.npy"
    )
    plan_path = root / "plan.json"
    if plan_path.is_file():
        plan = json.loads(plan_path.read_text())
    else:
        plan = {
            "schema": "exp9-debug-plan-v1",
            "source_checkpoint": str(source),
            "source_sha256": file_sha256(source),
            "wandb_ids": {
                label: uuid.uuid4().hex[:8]
                for label in (
                    "projection-0",
                    "projection-1",
                    "projection-2",
                    "projection-3",
                    "control-0",
                    "control-1",
                    "control-2",
                    "control-3",
                )
            },
        }
        atomic_json(plan_path, plan)
    ids = plan["wandb_ids"]

    oracle_specs = [
        {
            "label": f"projection-{seed}",
            "stage": "oracle",
            "init": "projection",
            "source": source,
            "projection_seed": seed,
            "wandb_id": ids[f"projection-{seed}"],
        }
        for seed in range(4)
    ] + [
        {
            "label": f"control-{index}",
            "stage": "preflight",
            "init": "checkpoint",
            "source": source,
            "wandb_id": ids[f"control-{index}"],
        }
        for index in range(4)
    ]
    oracle_results = _run_debug_phase(root, "oracle", oracle_specs)
    oracle_projection = min(
        (result for result in oracle_results if result["debug_stage"] == "oracle"),
        key=lambda result: (result["projection_oracle_kl"], result["projection_seed"]),
    )
    projection_seed = int(oracle_projection["projection_seed"])

    hidden_grid = (
        (24_576, 0.002),
        (8_192, 0.002),
        (24_576, 0.004),
        (8_192, 0.004),
    )
    fixed_grid = (
        (24_576, 0.002, 0.0003),
        (8_192, 0.002, 0.0003),
        (24_576, 0.004, 0.0003),
        (24_576, 0.002, 0.001),
    )
    warmup_specs = []
    for index, (optimizer_batch, lr) in enumerate(hidden_grid):
        warmup_specs.append(
            {
                "label": f"projection-{index}",
                "stage": "hidden",
                "init": "projection",
                "source": source,
                "projection_seed": projection_seed,
                "optimizer_batch": optimizer_batch,
                "lr": lr,
                "vocabulary_lr": 0.0003,
                "replays": int(CONFIG["debug_warmup_replays"]),
                "wandb_id": ids[f"projection-{index}"],
            }
        )
    for index, (optimizer_batch, lr, vocabulary_lr) in enumerate(fixed_grid):
        warmup_specs.append(
            {
                "label": f"control-{index}",
                "stage": "fixed_kl",
                "init": "checkpoint",
                "source": source,
                "optimizer_batch": optimizer_batch,
                "lr": lr,
                "vocabulary_lr": vocabulary_lr,
                "replays": int(CONFIG["debug_warmup_replays"]),
                "wandb_id": ids[f"control-{index}"],
            }
        )
    warmup_results = _run_debug_phase(root, "warmup", warmup_specs)
    hidden_winner = min(
        (result for result in warmup_results if result["debug_stage"] == "hidden"),
        key=lambda result: (
            result["hidden_validation_mse"],
            -result["hidden_validation_cosine"],
            result["optimizer_local_batch"],
        ),
    )
    hidden_model = (
        root / "warmup" / hidden_winner["debug_label"] / "student.pt"
    )

    # Tokenization overlaps the fixed screen, after the one-time compile-heavy
    # oracle/warm-start stages. Starting it earlier made the CPU producer fight
    # eight Inductor pools and delayed first GPU work.
    prepare_log_path = root / "logs" / "prepare-contexts.log"
    prepare_log_path.parent.mkdir(parents=True, exist_ok=True)
    prepare_log = prepare_log_path.open("ab")
    prepare = None
    if not context_cache.is_file():
        prepare = subprocess.Popen(
            [
                sys.executable,
                str(Path(__file__).resolve()),
                "--mode=debug_prepare",
                f"--debug_root={root}",
                f"--debug_context_cache={context_cache}",
                f"--debug_tokens={CONFIG['debug_tokens']}",
                f"--physical_local_batch={CONFIG['physical_local_batch']}",
            ],
            stdout=prepare_log,
            stderr=subprocess.STDOUT,
            env={**os.environ, "CUDA_VISIBLE_DEVICES": ""},
        )

    fixed_specs = []
    for initialization in ("projection", "control"):
        for index, (optimizer_batch, lr, vocabulary_lr) in enumerate(fixed_grid):
            label = f"{initialization}-{index}"
            fixed_specs.append(
                {
                    "label": label,
                    "stage": "fixed_kl",
                    "init": "model" if initialization == "projection" else "checkpoint",
                    "source": hidden_model if initialization == "projection" else source,
                    "optimizer_batch": optimizer_batch,
                    "lr": lr,
                    "vocabulary_lr": vocabulary_lr,
                    "replays": int(CONFIG["debug_replays"]),
                    "wandb_id": ids[label],
                }
            )
    fixed_results = _run_debug_phase(root, "fixed", fixed_specs)
    projection_fixed = [
        result
        for result in fixed_results
        if result["debug_label"].startswith("projection-")
    ]
    passed = min(result["best_train_kl"] for result in projection_fixed) < 1.0
    summary = {
        "schema": "exp9-debug-summary-v1",
        "status": "fixed_pass" if passed else "fixed_failed",
        "source_checkpoint": str(source),
        "source_sha256": file_sha256(source),
        "projection_oracle": oracle_projection,
        "hidden_winner": hidden_winner,
        "fixed_results": fixed_results,
        "context_cache": str(context_cache),
    }
    atomic_json(root / "summary.json", summary)
    if not passed:
        if prepare is not None and prepare.poll() is None:
            prepare.terminate()
            prepare.wait()
        prepare_log.close()
        atomic_json(
            root / "coordinator-status.json",
            {
                "schema": "exp9-debug-coordinator-v1",
                "status": "fixed_failed",
                "summary": str(root / "summary.json"),
                "wandb_urls": sorted(
                    {result["wandb_url"] for result in fixed_results}
                ),
            },
        )
        print(json.dumps(summary, indent=2), flush=True)
        return

    if prepare is not None:
        if prepare.wait() != 0:
            prepare_log.close()
            raise RuntimeError(f"debug context preparation failed: {prepare_log_path}")
    prepare_log.close()
    if not context_cache.is_file():
        raise RuntimeError(f"debug context cache is missing: {context_cache}")

    fresh_specs = []
    for initialization in ("projection", "control"):
        for index, (optimizer_batch, lr, vocabulary_lr) in enumerate(fixed_grid):
            label = f"{initialization}-{index}"
            fresh_specs.append(
                {
                    "label": label,
                    "stage": "fresh_kl",
                    "init": "model" if initialization == "projection" else "checkpoint",
                    "source": hidden_model if initialization == "projection" else source,
                    "optimizer_batch": optimizer_batch,
                    "lr": lr,
                    "vocabulary_lr": vocabulary_lr,
                    "replays": 0,
                    "wandb_id": ids[label],
                    "context_cache": context_cache,
                }
            )
    fresh_results = _run_debug_phase(root, "fresh", fresh_specs)
    summary.update({"status": "complete", "fresh_results": fresh_results})
    atomic_json(root / "summary.json", summary)
    atomic_json(
        root / "coordinator-status.json",
        {
            "schema": "exp9-debug-coordinator-v1",
            "status": "complete",
            "summary": str(root / "summary.json"),
            "wandb_urls": sorted(
                {result["wandb_url"] for result in fresh_results}
            ),
        },
    )
    print(json.dumps(summary, indent=2), flush=True)


def debug_fresh_coordinator() -> None:
    """Run eight controlled fresh-data continuations from one checkpoint."""
    if torch.cuda.device_count() != 8:
        raise RuntimeError("fresh coordinator requires exactly eight H100/H200 GPUs")
    if not os.environ.get("WANDB_API_KEY"):
        raise RuntimeError("WANDB_API_KEY is required before the paid debug run")
    source = Path(CONFIG["debug_source"] or CONFIG["resume"])
    context_cache = (
        Path(CONFIG["debug_context_cache"]) if CONFIG["debug_context_cache"] else None
    )
    if not source.is_file():
        raise RuntimeError(f"source checkpoint is missing: {source}")
    if context_cache is not None:
        if not context_cache.is_file():
            raise RuntimeError(f"fresh context cache is missing: {context_cache}")
        contexts = np.load(context_cache, mmap_mode="r")
        required = math.ceil(int(CONFIG["debug_tokens"]) / CONFIG["context_length"])
        if len(contexts) < required:
            raise RuntimeError(f"context cache has {len(contexts)} rows; need {required}")
        # A terminated open_memmap retains its full declared shape. Reject an
        # unwritten tail instead of silently training on all-padding contexts.
        if not np.asarray(contexts[required - 1]).any():
            raise RuntimeError(f"context cache row {required - 1} is unwritten")
    else:
        saved = torch.load(source, map_location="cpu", weights_only=False)
        stream_states = saved.get("stream_states")
        if not stream_states or len(stream_states) != 8:
            raise RuntimeError("live sweep requires eight saved stream states")

    root = Path(CONFIG["debug_root"])
    root.mkdir(parents=True, exist_ok=True)
    grid = [
        # Muon body LR, AdamW8bit tied-vocabulary LR, optimizer contexts.
        (1e-4, 3e-5, 24_576),
        (2e-4, 3e-5, 24_576),
        (4e-4, 3e-5, 24_576),
        (8e-4, 3e-5, 24_576),
        (2e-4, 1e-4, 24_576),
        (4e-4, 1e-4, 24_576),
        (2e-4, 0.0, 24_576),
        (0.0, 3e-4, 24_576),
    ]
    plan_path = root / "plan.json"
    if plan_path.is_file():
        plan = json.loads(plan_path.read_text())
    else:
        labels = [f"fresh-{index}" for index in range(len(grid))]
        plan = {
            "schema": "exp9-fresh-validation-sweep-v1",
            "source_checkpoint": str(source),
            "source_sha256": file_sha256(source),
            "context_source": str(context_cache) if context_cache else "live_checkpoint",
            "fresh_input_tokens_per_arm": int(CONFIG["debug_tokens"]),
            "grid": [
                {
                    "label": label,
                    "muon_lr": muon_lr,
                    "vocabulary_lr": vocabulary_lr,
                    "optimizer_contexts": optimizer_batch,
                    "optimizer_input_tokens": optimizer_batch
                    * CONFIG["context_length"],
                }
                for label, (muon_lr, vocabulary_lr, optimizer_batch) in zip(
                    labels, grid, strict=True
                )
            ],
            "wandb_ids": {label: uuid.uuid4().hex[:8] for label in labels},
        }
        atomic_json(plan_path, plan)

    specs = []
    for item in plan["grid"]:
        specs.append(
            {
                "label": item["label"],
                "stage": "fresh_kl",
                "init": "checkpoint",
                "source": source,
                "optimizer_batch": item["optimizer_contexts"],
                "lr": item["muon_lr"],
                "vocabulary_lr": item["vocabulary_lr"],
                "replays": 0,
                "wandb_id": plan["wandb_ids"][item["label"]],
                "context_cache": context_cache or "",
                "stream_rank": int(item["label"].removeprefix("fresh-")),
            }
        )
    results = _run_debug_phase(root, "fresh", specs)
    summary = {
        "schema": "exp9-fresh-validation-sweep-result-v1",
        "status": "complete",
        "source_checkpoint": str(source),
        "results": results,
        "winner": min(results, key=lambda result: result["best_validation_kl"]),
    }
    atomic_json(root / "summary.json", summary)
    atomic_json(
        root / "coordinator-status.json",
        {
            "schema": "exp9-fresh-validation-sweep-v1",
            "status": "complete",
            "summary": str(root / "summary.json"),
            "wandb_urls": sorted(result["wandb_url"] for result in results),
        },
    )
    print(json.dumps(summary, indent=2), flush=True)


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
    elif CONFIG["mode"] == "debug_prepare":
        debug_prepare()
    elif CONFIG["mode"] == "debug_arm":
        debug_arm()
    elif CONFIG["mode"] == "debug_coordinator":
        debug_coordinator()
    elif CONFIG["mode"] == "debug_fresh_coordinator":
        debug_fresh_coordinator()
    else:
        train()
