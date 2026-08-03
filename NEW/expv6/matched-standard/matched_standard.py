"""Experiment V6: matched Kronecker and Transformer distillation controls."""

from __future__ import annotations

import ast
import hashlib
import json
import math
import os
import sys
import time
from pathlib import Path
from typing import Any

import numpy as np
import torch
import torch.distributed as dist
import torch.nn.functional as F
from qwen_kron_distill.objective import khatri_rao_logits, load_teacher
from qwen_normuon_pretrain.data import load_split
from qwen_normuon_pretrain.normuon import SingleDeviceNorMuon
from torch import nn
from torch.nn.parallel import DistributedDataParallel
from torch.utils.checkpoint import checkpoint


SCHEMA = "expv6-matched-control-v1"
EXPECTED_PARAMETERS = 1_177_920
ARCHITECTURES = ("kronecker", "transformer")
MODES = ("train", "evaluate", "audit")
SPLITS = ("validation", "test", "confirmation")

CONFIG: dict[str, Any] = {
    "seed": 0,
    "architecture": "transformer",
    "mode": "train",
    "evaluation_split": "validation",
    "context_length": 16,
    "width": 64,
    "depth": 32,
    "rank": 8,
    "attention_heads": 4,
    "mlp_width": 96,
    "vocab_modes": (485, 512),
    "local_batch": 58_368,  # 466,944 contexts / 7,471,104 tokens globally
    "teacher_microbatch": 2_048,
    "evaluation_batch": 64,
    "steps": 144,  # 67,239,936 contexts: Experiment V5/V6-scale run
    "lr": 0.2,
    "weight_decay": 0.01,
    "warmup_steps": 4,
    "checkpoint_every": 16,
    "data_root": (
        "/cache/qwen_fullwidth_distill/"
        "context16-fineweb-edu-next-token-4m-v1"
    ),
    "confirmation_root": "/cache/expv6-kiss/matched-standard/confirmation-v1",
    "output_dir": "/cache/expv6-kiss/matched-standard",
    "wandb_project": "qwen-causal-kron-distill",
    "run_name": "",
    "compile": True,
    "activation_checkpointing": True,
    "evaluate_after_train": True,
    "resume": True,
    "legacy_root_layout": True,
    "symmetric_campaign": False,
    "screen_steps": 16,
    "promotion_steps": 48,
    "lr_grid": (0.025, 0.05, 0.1, 0.2, 0.4, 0.8),
    "self_test": False,
}


def overrides(arguments: list[str] | None = None) -> None:
    for arg in sys.argv[1:] if arguments is None else arguments:
        if not arg.startswith("--") or "=" not in arg:
            raise SystemExit(f"expected --key=value, got {arg}")
        key, raw = arg[2:].split("=", 1)
        if key not in CONFIG:
            raise SystemExit(f"unknown option {key}")
        old = CONFIG[key]
        if isinstance(old, bool):
            if raw.lower() not in ("true", "false"):
                raise SystemExit(f"{key} must be true or false")
            CONFIG[key] = raw.lower() == "true"
        elif isinstance(old, str):
            CONFIG[key] = raw
        else:
            value = ast.literal_eval(raw)
            CONFIG[key] = tuple(value) if isinstance(old, tuple) else value
    validate_config()


def validate_config() -> None:
    if CONFIG["architecture"] not in ARCHITECTURES:
        raise ValueError(f"architecture must be one of {ARCHITECTURES}")
    if CONFIG["mode"] not in MODES:
        raise ValueError(f"mode must be one of {MODES}")
    if CONFIG["evaluation_split"] not in SPLITS:
        raise ValueError(f"evaluation_split must be one of {SPLITS}")
    if math.prod(CONFIG["vocab_modes"]) != 248_320:
        raise ValueError("vocabulary modes must exactly cover Qwen's vocabulary")
    if CONFIG["width"] % CONFIG["attention_heads"]:
        raise ValueError("width must be divisible by attention_heads")
    for key in (
        "context_length",
        "width",
        "depth",
        "rank",
        "attention_heads",
        "mlp_width",
        "local_batch",
        "teacher_microbatch",
        "evaluation_batch",
        "steps",
        "checkpoint_every",
        "screen_steps",
        "promotion_steps",
    ):
        if int(CONFIG[key]) <= 0:
            raise ValueError(f"{key} must be positive")


def per_token_rms(value: torch.Tensor) -> torch.Tensor:
    """Parameter-free RMS normalization over channels, independently per token."""
    scale = torch.rsqrt(value.float().square().mean(-1, keepdim=True) + 1e-6)
    return value * scale.to(value.dtype)


def _kron(value: torch.Tensor, a: torch.Tensor, b: torch.Tensor) -> torch.Tensor:
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


class KroneckerBlock(nn.Module):
    def __init__(self) -> None:
        super().__init__()
        rank = CONFIG["rank"]
        length = CONFIG["context_length"]
        width = CONFIG["width"]
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


def _rotate_half(value: torch.Tensor) -> torch.Tensor:
    first = value[..., 0::2]
    second = value[..., 1::2]
    return torch.stack((-second, first), dim=-1).flatten(-2)


def apply_rope(value: torch.Tensor) -> torch.Tensor:
    """Apply parameter-free rotary positions to [batch, heads, tokens, channels]."""
    head_width = value.shape[-1]
    if head_width % 2:
        raise ValueError("RoPE requires an even attention head width")
    position = torch.arange(value.shape[-2], device=value.device, dtype=torch.float32)
    frequency = torch.arange(
        0, head_width, 2, device=value.device, dtype=torch.float32
    )
    frequency = 1.0 / (10_000.0 ** (frequency / head_width))
    angle = torch.outer(position, frequency).repeat_interleave(2, dim=-1)
    cosine = angle.cos().to(value.dtype)[None, None]
    sine = angle.sin().to(value.dtype)[None, None]
    return value * cosine + _rotate_half(value) * sine


class TransformerBlock(nn.Module):
    """Bias-free pre-norm causal Transformer block with no scalar parameters."""

    def __init__(self) -> None:
        super().__init__()
        width = CONFIG["width"]
        mlp_width = CONFIG["mlp_width"]
        depth = CONFIG["depth"]
        self.qkv = nn.Parameter(
            torch.empty(3 * width, width, dtype=torch.bfloat16)
        )
        self.output = nn.Parameter(
            torch.empty(width, width, dtype=torch.bfloat16)
        )
        self.gate_up = nn.Parameter(
            torch.empty(2 * mlp_width, width, dtype=torch.bfloat16)
        )
        self.down = nn.Parameter(
            torch.empty(width, mlp_width, dtype=torch.bfloat16)
        )
        nn.init.normal_(self.qkv, std=0.02)
        nn.init.normal_(self.gate_up, std=0.02)
        residual_std = 0.02 / math.sqrt(2 * depth)
        nn.init.normal_(self.output, std=residual_std)
        nn.init.normal_(self.down, std=residual_std)

    def forward(self, value: torch.Tensor) -> torch.Tensor:
        batch, tokens, width = value.shape
        heads = CONFIG["attention_heads"]
        head_width = width // heads
        qkv = F.linear(per_token_rms(value), self.qkv)
        query, key, values = qkv.chunk(3, dim=-1)

        def heads_view(item: torch.Tensor) -> torch.Tensor:
            return item.view(batch, tokens, heads, head_width).transpose(1, 2)

        query = apply_rope(heads_view(query))
        key = apply_rope(heads_view(key))
        values = heads_view(values)
        attended = F.scaled_dot_product_attention(
            query,
            key,
            values,
            dropout_p=0.0,
            is_causal=True,
        )
        attended = attended.transpose(1, 2).reshape(batch, tokens, width)
        value = value + F.linear(attended, self.output)
        gate, up = F.linear(per_token_rms(value), self.gate_up).chunk(2, dim=-1)
        return value + F.linear(F.silu(gate) * up, self.down)


class Student(nn.Module):
    def __init__(self, architecture: str | None = None) -> None:
        super().__init__()
        self.architecture = architecture or CONFIG["architecture"]
        if self.architecture not in ARCHITECTURES:
            raise ValueError(f"unknown architecture {self.architecture}")
        width = CONFIG["width"]
        self.vocabulary = nn.ParameterList(
            [
                nn.Parameter(torch.randn(mode, width, dtype=torch.bfloat16) * 0.02)
                for mode in CONFIG["vocab_modes"]
            ]
        )
        block_type = (
            KroneckerBlock
            if self.architecture == "kronecker"
            else TransformerBlock
        )
        self.blocks = nn.ModuleList(block_type() for _ in range(CONFIG["depth"]))

    def sequence(self, token_ids: torch.Tensor) -> torch.Tensor:
        if token_ids.ndim != 2 or token_ids.shape[1] != CONFIG["context_length"]:
            raise ValueError(
                f"student requires token ids with shape [batch,{CONFIG['context_length']}]"
            )
        first = token_ids.div(CONFIG["vocab_modes"][1], rounding_mode="floor")
        second = token_ids.remainder(CONFIG["vocab_modes"][1])
        value = F.embedding(first, self.vocabulary[0])
        value = value * F.embedding(second, self.vocabulary[1])
        for block in self.blocks:
            if (
                self.architecture == "transformer"
                and CONFIG["activation_checkpointing"]
                and self.training
                and value.requires_grad
            ):
                value = checkpoint(block, value, use_reentrant=False)
            else:
                value = block(value)
        return per_token_rms(value)

    def hidden(self, token_ids: torch.Tensor) -> torch.Tensor:
        return self.sequence(token_ids)[:, -1]

    def logits(self, token_ids: torch.Tensor) -> torch.Tensor:
        return khatri_rao_logits(
            self.hidden(token_ids),
            self.vocabulary[0],
            self.vocabulary[1],
        )

    def forward(
        self,
        token_ids: torch.Tensor,
        teacher_probability: torch.Tensor,
    ) -> torch.Tensor:
        logits = self.logits(token_ids)
        loss = (
            _compiled_soft_cross_entropy
            if CONFIG["compile"] and logits.is_cuda
            else _soft_cross_entropy
        )
        return loss(logits, teacher_probability)


def parameter_inventory(model: nn.Module) -> dict[str, int]:
    return {
        "trainable_parameters": sum(
            parameter.numel() for parameter in model.parameters()
            if parameter.requires_grad
        ),
        "vocabulary_parameters": sum(
            parameter.numel() for parameter in model.vocabulary.parameters()
        ),
        "body_parameters": sum(
            parameter.numel() for parameter in model.blocks.parameters()
        ),
    }


def assert_parameter_match(model: Student) -> dict[str, int]:
    inventory = parameter_inventory(model)
    if inventory["trainable_parameters"] != EXPECTED_PARAMETERS:
        raise RuntimeError(
            f"{model.architecture} has {inventory['trainable_parameters']:,} "
            f"parameters; expected {EXPECTED_PARAMETERS:,}"
        )
    if any(parameter.ndim < 2 for parameter in model.parameters()):
        raise RuntimeError("NorMuon control models may contain only matrix parameters")
    return inventory


@torch.no_grad()
def teacher_targets(teacher, token_ids: torch.Tensor) -> torch.Tensor:
    """Materialize BF16 soft targets in bounded teacher microbatches."""
    probability = torch.empty(
        token_ids.shape[0],
        math.prod(CONFIG["vocab_modes"]),
        device=token_ids.device,
        dtype=torch.bfloat16,
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
        probability[start:stop].copy_(F.softmax(F.linear(hidden, weight).float(), -1))
    return probability


@torch.no_grad()
def teacher_evaluation_distribution(
    teacher, token_ids: torch.Tensor
) -> tuple[torch.Tensor, torch.Tensor, torch.Tensor]:
    hidden = teacher.model(
        input_ids=token_ids,
        use_cache=False,
        return_dict=True,
    ).last_hidden_state[:, -1]
    logits = F.linear(hidden, teacher.get_output_embeddings().weight).float()
    log_probability = F.log_softmax(logits, -1)
    probability = log_probability.exp()
    entropy = -(probability * log_probability).sum(-1)
    return probability, log_probability, entropy


def batch_indices(size: int, step: int, rank: int, world: int) -> np.ndarray:
    generator = np.random.default_rng(CONFIG["seed"] + step)
    indices = generator.choice(size, CONFIG["local_batch"] * world, replace=False)
    start = rank * CONFIG["local_batch"]
    return indices[start : start + CONFIG["local_batch"]]


def run_slug() -> str:
    lr = f"{CONFIG['lr']:.8g}".replace(".", "p")
    return f"{CONFIG['architecture']}-seed{CONFIG['seed']}-lr{lr}"


def run_directory() -> Path:
    root = Path(CONFIG["output_dir"])
    if (
        CONFIG["legacy_root_layout"]
        and CONFIG["architecture"] == "kronecker"
        and CONFIG["seed"] == 0
        and math.isclose(CONFIG["lr"], 0.2)
    ):
        return root
    return root / run_slug()


def load_evaluation_data(split: str):
    if split != "confirmation":
        return load_split(CONFIG["data_root"], split)
    root = Path(CONFIG["confirmation_root"])
    manifest_path = root / "manifest.json"
    if not manifest_path.is_file():
        raise RuntimeError(f"missing confirmation manifest at {manifest_path}")
    manifest = json.loads(manifest_path.read_text())
    if manifest.get("schema") != "expv6-confirmation-v1":
        raise RuntimeError("invalid confirmation manifest schema")
    return (
        np.load(root / "confirmation_contexts.npy", mmap_mode="r"),
        np.load(root / "confirmation_targets.npy", mmap_mode="r"),
        np.load(root / "confirmation_hashes.npy", mmap_mode="r"),
    )


def checkpoint_identity(world: int) -> dict[str, Any]:
    keys = (
        "seed",
        "architecture",
        "context_length",
        "width",
        "depth",
        "rank",
        "attention_heads",
        "mlp_width",
        "vocab_modes",
        "local_batch",
        "lr",
        "weight_decay",
        "warmup_steps",
    )
    value = {key: CONFIG[key] for key in keys}
    value.update(
        {
            "schema": SCHEMA,
            "world_size": world,
            "global_batch": CONFIG["local_batch"] * world,
            "global_token_batch": (
                CONFIG["local_batch"] * world * CONFIG["context_length"]
            ),
        }
    )
    return value


def identity_hash(identity: dict[str, Any]) -> str:
    return hashlib.sha256(
        json.dumps(identity, sort_keys=True, separators=(",", ":")).encode()
    ).hexdigest()


def save_json(path: Path, value: dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_text(json.dumps(value, indent=2, sort_keys=True))
    os.replace(temporary, path)


def save_checkpoint(
    path: Path,
    *,
    model: Student,
    optimizer: torch.optim.Optimizer,
    step: int,
    wandb_url: str,
    world: int,
) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(".tmp")
    torch.save(
        {
            "schema": SCHEMA,
            "identity": checkpoint_identity(world),
            "identity_hash": identity_hash(checkpoint_identity(world)),
            "config": dict(CONFIG),
            "step": step,
            "model": model.state_dict(),
            "optimizer": optimizer.state_dict(),
            "wandb_url": wandb_url,
        },
        temporary,
    )
    os.replace(temporary, path)


def load_checkpoint(
    path: Path,
    *,
    model: Student,
    optimizer: torch.optim.Optimizer | None,
    world: int,
) -> dict[str, Any]:
    value = torch.load(path, map_location="cpu", weights_only=True)
    # The first V6 checkpoint predates schemas but has a compatible seed-0
    # Kronecker state. Keep that artifact evaluable without weakening new runs.
    if value.get("schema") is None:
        if CONFIG["architecture"] != "kronecker" or CONFIG["seed"] != 0:
            raise RuntimeError("legacy checkpoints are seed-0 Kronecker only")
    else:
        expected = checkpoint_identity(world)
        if value.get("identity_hash") != identity_hash(expected):
            raise RuntimeError("checkpoint identity does not match this run")
    if int(value["step"]) > CONFIG["steps"] and CONFIG["mode"] == "train":
        raise RuntimeError("checkpoint is beyond the requested training target")
    model.load_state_dict(value["model"])
    if optimizer is not None:
        optimizer.load_state_dict(value["optimizer"])
    return value


def distributed_context() -> tuple[int, int, int, torch.device, bool]:
    world = int(os.environ.get("WORLD_SIZE", "1"))
    rank = int(os.environ.get("RANK", "0"))
    local_rank = int(os.environ.get("LOCAL_RANK", "0"))
    if not torch.cuda.is_available() or world != 8:
        raise RuntimeError("paid runs require exactly eight CUDA ranks")
    torch.cuda.set_device(local_rank)
    dist.init_process_group("nccl", device_id=torch.device(f"cuda:{local_rank}"))
    return world, rank, local_rank, torch.device(f"cuda:{local_rank}"), rank == 0


def create_optimizer(student: Student) -> SingleDeviceNorMuon:
    return SingleDeviceNorMuon(
        student.parameters(),
        lr=CONFIG["lr"],
        weight_decay=CONFIG["weight_decay"],
        beta1=0.95,
        beta2=0.95,
        ns_steps=5,
        nesterov=True,
        eps=1e-8,
    )


def initialize_wandb(
    *,
    primary: bool,
    world: int,
    inventory: dict[str, int],
) -> tuple[Any, str]:
    url_holder = [""]
    run = None
    if primary:
        key = os.environ.get("WANDB_API_KEY")
        if not key:
            raise RuntimeError("WANDB_API_KEY is required")
        import wandb

        wandb.login(key=key, relogin=True)
        name = CONFIG["run_name"] or f"expv6-{run_slug()}"
        run = wandb.init(
            project=CONFIG["wandb_project"],
            name=name,
            config={
                **CONFIG,
                **inventory,
                "schema": SCHEMA,
                "world_size": world,
                "global_batch": CONFIG["local_batch"] * world,
                "global_token_batch": (
                    CONFIG["local_batch"] * world * CONFIG["context_length"]
                ),
                "target_contexts": CONFIG["steps"] * CONFIG["local_batch"] * world,
                "target_tokens": (
                    CONFIG["steps"]
                    * CONFIG["local_batch"]
                    * world
                    * CONFIG["context_length"]
                ),
            },
        )
        if not run.url:
            raise RuntimeError("W&B did not return a direct run URL")
        url_holder[0] = run.url
        print(f"WANDB_URL={run.url}", flush=True)
    dist.broadcast_object_list(url_holder, src=0)
    if not url_holder[0]:
        raise RuntimeError("missing direct W&B run URL")
    return run, url_holder[0]


@torch.inference_mode()
def evaluate(
    student: Student,
    teacher,
    contexts: np.ndarray,
    targets: np.ndarray,
    *,
    world: int,
    rank: int,
    device: torch.device,
) -> dict[str, Any]:
    student.eval()
    totals = torch.zeros(5, device=device, dtype=torch.float64)
    local_indices = np.arange(rank, len(contexts), world, dtype=np.int64)
    batch = CONFIG["evaluation_batch"]
    for start in range(0, len(local_indices), batch):
        index = local_indices[start : start + batch]
        token_ids = torch.as_tensor(
            np.asarray(contexts[index], dtype=np.int64), device=device
        )
        target = torch.as_tensor(
            np.asarray(targets[index], dtype=np.int64), device=device
        )
        probability, log_probability, entropy = teacher_evaluation_distribution(
            teacher, token_ids
        )
        with torch.autocast("cuda", dtype=torch.bfloat16):
            logits = student.logits(token_ids)
        student_log_probability = F.log_softmax(logits.float(), -1)
        rows_kl = (
            probability * (log_probability - student_log_probability)
        ).sum(-1)
        totals[0] += rows_kl.double().sum()
        totals[1] += (-student_log_probability.gather(1, target[:, None])[:, 0]).double().sum()
        totals[2] += (-log_probability.gather(1, target[:, None])[:, 0]).double().sum()
        totals[3] += (logits.argmax(-1) == target).double().sum()
        totals[4] += len(index)
        del probability, log_probability, entropy, logits, student_log_probability
    dist.all_reduce(totals, op=dist.ReduceOp.SUM)
    count = float(totals[4])
    student.train()
    return {
        "kl": float(totals[0] / count),
        "student_nll": float(totals[1] / count),
        "teacher_nll": float(totals[2] / count),
        "accuracy": float(totals[3] / count),
        "examples": int(count),
    }


def train() -> None:
    world, rank, local_rank, device, primary = distributed_context()
    torch.manual_seed(CONFIG["seed"])
    student = Student().to(device)
    inventory = assert_parameter_match(student)
    optimizer = create_optimizer(student)
    directory = run_directory()
    path = directory / "checkpoint.pt"
    start_step = 0
    prior_url = ""
    if CONFIG["resume"] and path.is_file():
        restored = load_checkpoint(
            path, model=student, optimizer=optimizer, world=world
        )
        start_step = int(restored["step"])
        prior_url = str(restored.get("wandb_url", ""))
    model = DistributedDataParallel(
        student,
        device_ids=[local_rank],
        broadcast_buffers=False,
        gradient_as_bucket_view=True,
    )
    run, wandb_url = initialize_wandb(
        primary=primary, world=world, inventory=inventory
    )
    if prior_url and primary:
        print(f"RESUMED_FROM_WANDB_URL={prior_url}", flush=True)
    contexts, _, _ = load_split(CONFIG["data_root"], "train")
    validation_data = (
        load_split(CONFIG["data_root"], "validation")
        if CONFIG["evaluate_after_train"]
        else None
    )
    teacher = load_teacher(str(device))

    started = time.perf_counter()
    last_loss = float("nan")
    last_metrics: dict[str, Any] = {}
    for step in range(start_step + 1, CONFIG["steps"] + 1):
        index = batch_indices(len(contexts), step, rank, world)
        token_ids = torch.as_tensor(
            np.asarray(contexts[index], dtype=np.int64), device=device
        )
        with torch.no_grad(), torch.autocast("cuda", dtype=torch.bfloat16):
            probability = teacher_targets(teacher, token_ids)
        optimizer.zero_grad(set_to_none=True)
        with torch.autocast("cuda", dtype=torch.bfloat16):
            loss = model(token_ids, probability)
        loss.backward()
        norm = torch.nn.utils.clip_grad_norm_(student.parameters(), 1.0)
        if not torch.isfinite(loss) or not torch.isfinite(norm):
            raise RuntimeError("non-finite loss or gradient")
        multiplier = min(1.0, step / CONFIG["warmup_steps"])
        optimizer.param_groups[0]["lr"] = CONFIG["lr"] * multiplier
        optimizer.step()
        for state in optimizer.state.values():
            for value in state.values():
                if isinstance(value, torch.Tensor) and not torch.isfinite(value).all():
                    raise RuntimeError("non-finite optimizer state")
        last_loss = float(loss.detach())

        elapsed = time.perf_counter() - started
        completed = step - start_step
        global_batch = CONFIG["local_batch"] * world
        metrics = {
            "step": step,
            "contexts_seen": step * global_batch,
            "tokens_seen": step * global_batch * CONFIG["context_length"],
            "train_cross_entropy": last_loss,
            "grad_norm": float(norm),
            "contexts_per_second": completed * global_batch / elapsed,
            "tokens_per_second": (
                completed * global_batch * CONFIG["context_length"] / elapsed
            ),
            "global_batch": global_batch,
            "global_token_batch": global_batch * CONFIG["context_length"],
            "peak_allocated_gib": torch.cuda.max_memory_allocated() / 2**30,
            "peak_reserved_gib": torch.cuda.max_memory_reserved() / 2**30,
        }
        try:
            metrics["gpu_utilization_percent"] = float(
                torch.cuda.utilization(device)
            )
        except (AttributeError, ModuleNotFoundError, RuntimeError):
            metrics["gpu_utilization_percent"] = float("nan")
        gathered: list[Any] = [None] * world
        dist.all_gather_object(gathered, metrics)
        last_metrics = metrics
        if primary:
            metrics["per_gpu_peak_allocated_gib"] = [
                item["peak_allocated_gib"] for item in gathered
            ]
            metrics["per_gpu_peak_reserved_gib"] = [
                item["peak_reserved_gib"] for item in gathered
            ]
            metrics["per_gpu_utilization_percent"] = [
                item["gpu_utilization_percent"] for item in gathered
            ]
            print(json.dumps(metrics), flush=True)
            run.log(metrics, step=step)

        if step % CONFIG["checkpoint_every"] == 0 or step == CONFIG["steps"]:
            if primary:
                save_checkpoint(
                    path,
                    model=student,
                    optimizer=optimizer,
                    step=step,
                    wandb_url=wandb_url,
                    world=world,
                )
            dist.barrier()

    validation = None
    if validation_data is not None:
        validation_contexts, validation_targets, _ = validation_data
        validation = evaluate(
            student,
            teacher,
            validation_contexts,
            validation_targets,
            world=world,
            rank=rank,
            device=device,
        )
    if primary:
        result_path = directory / "result.json"
        result = (
            json.loads(result_path.read_text())
            if result_path.is_file()
            else {}
        )
        performance = {
            key: last_metrics[key]
            for key in (
                "contexts_per_second",
                "tokens_per_second",
                "peak_allocated_gib",
                "peak_reserved_gib",
            )
            if key in last_metrics
        }
        validation_history = dict(result.get("validation_history", {}))
        performance_history = dict(result.get("performance_history", {}))
        if validation is not None:
            validation_history[str(CONFIG["steps"])] = validation
        performance_history[str(CONFIG["steps"])] = performance
        result.update(
            {
                "schema": SCHEMA,
                "status": "complete",
                "architecture": CONFIG["architecture"],
                "seed": CONFIG["seed"],
                "lr": CONFIG["lr"],
                "step": CONFIG["steps"],
                "contexts_seen": (
                    CONFIG["steps"] * CONFIG["local_batch"] * world
                ),
                "tokens_seen": (
                    CONFIG["steps"]
                    * CONFIG["local_batch"]
                    * world
                    * CONFIG["context_length"]
                ),
                "global_batch": CONFIG["local_batch"] * world,
                "global_token_batch": (
                    CONFIG["local_batch"] * world * CONFIG["context_length"]
                ),
                "inventory": inventory,
                "validation_history": validation_history,
                "performance": performance,
                "performance_history": performance_history,
                "wandb_url": wandb_url,
                "checkpoint": str(path),
            }
        )
        if validation is not None:
            result["validation"] = validation
        save_json(result_path, result)
        if validation is not None:
            run.log(
                {f"validation/{key}": value for key, value in validation.items()},
                step=CONFIG["steps"],
            )
        run.finish()
        print(json.dumps(result, indent=2), flush=True)
    dist.destroy_process_group()


def evaluate_checkpoint() -> None:
    world, rank, _, device, primary = distributed_context()
    torch.manual_seed(CONFIG["seed"])
    contexts, targets, _ = load_evaluation_data(CONFIG["evaluation_split"])
    teacher = load_teacher(str(device))
    student = Student().to(device)
    inventory = assert_parameter_match(student)
    path = run_directory() / "checkpoint.pt"
    if not path.is_file():
        raise RuntimeError(f"missing checkpoint {path}")
    restored = load_checkpoint(path, model=student, optimizer=None, world=world)
    metrics = evaluate(
        student,
        teacher,
        contexts,
        targets,
        world=world,
        rank=rank,
        device=device,
    )
    if primary:
        result_path = run_directory() / "result.json"
        result = (
            json.loads(result_path.read_text())
            if result_path.is_file()
            else {}
        )
        result.update(
            {
                "schema": SCHEMA,
                "architecture": CONFIG["architecture"],
                "seed": CONFIG["seed"],
                "lr": CONFIG["lr"],
                "step": int(restored["step"]),
                "inventory": inventory,
                "wandb_url": restored.get("wandb_url", ""),
                "checkpoint": str(path),
            }
        )
        result[CONFIG["evaluation_split"]] = metrics
        save_json(result_path, result)
        print(json.dumps(result, indent=2), flush=True)
    dist.destroy_process_group()


def comparison_summary(results: list[dict[str, Any]]) -> dict[str, Any]:
    def stage_rows(architecture: str, step: int) -> list[dict[str, float]]:
        rows = []
        for result in results:
            if result.get("architecture") != architecture or result.get("seed") != 0:
                continue
            validation = result.get("validation_history", {}).get(str(step))
            if validation is None and result.get("step") == step:
                validation = result.get("validation")
            if not validation:
                continue
            performance = result.get("performance_history", {}).get(
                str(step), result.get("performance", {})
            )
            rows.append(
                {
                    "lr": float(result["lr"]),
                    "validation_kl": float(validation["kl"]),
                    "tokens_per_second": float(
                        performance.get("tokens_per_second", 0.0)
                    ),
                }
            )
        return sorted(rows, key=lambda item: item["lr"])

    def best(rows: list[dict[str, float]]) -> dict[str, float] | None:
        if not rows:
            return None
        best_kl = min(item["validation_kl"] for item in rows)
        tie = [item for item in rows if item["validation_kl"] <= best_kl + 0.01]
        return max(tie, key=lambda item: item["tokens_per_second"])

    screens = {
        architecture: stage_rows(architecture, int(CONFIG["screen_steps"]))
        for architecture in ARCHITECTURES
    }
    promotions = {
        architecture: stage_rows(architecture, int(CONFIG["promotion_steps"]))
        for architecture in ARCHITECTURES
    }
    selected = {
        architecture: best(promotions[architecture])
        for architecture in ARCHITECTURES
    }

    finals = {
        (result.get("architecture"), int(result.get("seed", -1))): result
        for result in results
        if int(result.get("step", -1)) == CONFIG["steps"]
        and result.get("validation")
    }
    required = [(architecture, seed) for architecture in ARCHITECTURES for seed in (0, 1, 2)]
    replication_status = "complete" if all(key in finals for key in required) else "required_pending"
    paired_validation_deltas = {
        str(seed): (
            float(finals[("transformer", seed)]["validation"]["kl"])
            - float(finals[("kronecker", seed)]["validation"]["kl"])
        )
        for seed in (0, 1, 2)
        if ("transformer", seed) in finals and ("kronecker", seed) in finals
    }
    paired_confirmation_deltas = {
        str(seed): (
            float(finals[("transformer", seed)]["confirmation"]["kl"])
            - float(finals[("kronecker", seed)]["confirmation"]["kl"])
        )
        for seed in (0, 1, 2)
        if ("transformer", seed) in finals
        and ("kronecker", seed) in finals
        and "confirmation" in finals[("transformer", seed)]
        and "confirmation" in finals[("kronecker", seed)]
    }
    confirmation_complete = replication_status == "complete" and all(
        "confirmation" in finals[key] for key in required
    )
    return {
        "screens": screens,
        "promotions": promotions,
        "selected": selected,
        "replication_status": replication_status,
        "paired_validation_kl_deltas": paired_validation_deltas,
        "paired_confirmation_kl_deltas": paired_confirmation_deltas,
        "confirmation_evaluation_complete": confirmation_complete,
    }


def audit() -> None:
    root = Path(CONFIG["output_dir"])
    paths = sorted(root.glob("**/result.json"))
    results = [json.loads(path.read_text()) for path in paths]
    failures = []
    for path, result in zip(paths, results, strict=True):
        if result.get("inventory", {}).get("trainable_parameters") != EXPECTED_PARAMETERS:
            failures.append(f"{path}: parameter mismatch")
        if not result.get("wandb_url"):
            failures.append(f"{path}: missing W&B URL")
        if not Path(result.get("checkpoint", "")).is_file():
            failures.append(f"{path}: missing checkpoint")
    summary = comparison_summary(results)
    if CONFIG["symmetric_campaign"]:
        expected_grid = sorted(float(value) for value in CONFIG["lr_grid"])
        for architecture in ARCHITECTURES:
            actual_grid = sorted(item["lr"] for item in summary["screens"][architecture])
            if actual_grid != expected_grid:
                failures.append(f"{architecture}: incomplete LR screen {actual_grid}")
            if len(summary["promotions"][architecture]) != 2:
                failures.append(f"{architecture}: expected exactly two promoted candidates")
            selected = summary["selected"][architecture]
            if selected is None:
                failures.append(f"{architecture}: missing selected recipe")
                continue
            for seed in (0, 1, 2):
                matches = [
                    result
                    for result in results
                    if result.get("architecture") == architecture
                    and int(result.get("seed", -1)) == seed
                    and int(result.get("step", -1)) == CONFIG["steps"]
                    and math.isclose(float(result.get("lr", -1)), selected["lr"])
                ]
                if len(matches) != 1:
                    failures.append(f"{architecture} seed {seed}: missing locked final")
                elif "confirmation" not in matches[0]:
                    failures.append(f"{architecture} seed {seed}: missing confirmation")
        confirmation = Path(CONFIG["confirmation_root"])
        if not (confirmation / "manifest.json").is_file():
            failures.append("missing confirmation manifest")
        else:
            confirmation_hashes = np.unique(
                np.load(confirmation / "confirmation_hashes.npy", mmap_mode="r")
            )
            for split in ("train", "validation", "test"):
                _, _, hashes = load_split(CONFIG["data_root"], split)
                if np.intersect1d(confirmation_hashes, np.unique(hashes)).size:
                    failures.append(f"confirmation overlaps {split} documents")
    comparison = {
        "schema": SCHEMA,
        "status": "failed" if failures else "complete",
        "expected_parameters": EXPECTED_PARAMETERS,
        "screen_learning_rates": list(CONFIG["lr_grid"]),
        "screen_steps": CONFIG["screen_steps"],
        "promotion_steps": CONFIG["promotion_steps"],
        "comparison": summary,
        "results": results,
        "failures": failures,
    }
    save_json(root / "comparison.json", comparison)
    print(json.dumps(comparison, indent=2))
    if failures:
        raise RuntimeError("Experiment V6 audit failed")


def self_test() -> None:
    torch.manual_seed(0)
    prior = dict(CONFIG)
    try:
        CONFIG.update(
            {
                "compile": False,
                "activation_checkpointing": False,
            }
        )
        kronecker = Student("kronecker")
        transformer = Student("transformer")
        assert_parameter_match(kronecker)
        assert_parameter_match(transformer)
        token_ids = torch.randint(
            0, math.prod(CONFIG["vocab_modes"]), (2, CONFIG["context_length"])
        )
        assert kronecker.hidden(token_ids).shape == (2, CONFIG["width"])
        assert transformer.hidden(token_ids).shape == (2, CONFIG["width"])
        normalized = per_token_rms(
            torch.randn(3, CONFIG["context_length"], CONFIG["width"])
        )
        torch.testing.assert_close(
            normalized.float().square().mean(-1),
            torch.ones(3, CONFIG["context_length"]),
            atol=1e-5,
            rtol=1e-5,
        )
        print("self-test passed")
    finally:
        CONFIG.clear()
        CONFIG.update(prior)


def main() -> None:
    overrides()
    if CONFIG["self_test"]:
        self_test()
    elif CONFIG["mode"] == "train":
        train()
    elif CONFIG["mode"] == "evaluate":
        evaluate_checkpoint()
    else:
        audit()


if __name__ == "__main__":
    main()
