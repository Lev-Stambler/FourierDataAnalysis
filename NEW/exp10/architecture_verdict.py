"""Experiment 10: causal 256-token Kronecker versus whole Transformer models."""

from __future__ import annotations

import argparse
import hashlib
import json
import math
import os
import time
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Any

import numpy as np
import torch
import torch.distributed as dist
import torch.nn.functional as F
from torch import nn
from torch.nn.parallel import DistributedDataParallel
from torch.utils.checkpoint import checkpoint


SCHEMA = "exp10-causal-architecture-verdict-v1"
ARCHITECTURES = ("kronecker", "transformer")
DEFAULT_ARCHITECTURE = "transformer"
SCALES = ("small", "large")
MODEL_SCALES = (*SCALES, "xlarge")
CONTEXT_LENGTH = 256
VOCAB_SIZE = 16_384
VOCAB_MODES = (128, 128)


@dataclass(frozen=True)
class ModelSpec:
    architecture: str
    scale: str
    width: int
    depth: int
    rank: int = 0
    heads: int = 0
    mlp_width: int = 0


def kronecker_parameter_count(width: int, depth: int, rank: int) -> int:
    triangle = CONTEXT_LENGTH * (CONTEXT_LENGTH + 1) // 2
    vocabulary = sum(VOCAB_MODES) * width
    return vocabulary + depth * rank * (triangle + width * width)


def transformer_parameter_count(width: int, depth: int, mlp_width: int) -> int:
    return VOCAB_SIZE * width + depth * (4 * width * width + 3 * width * mlp_width)


def solve_transformer_match(
    target: int, *, width: int, depth: int, heads: int
) -> ModelSpec:
    candidates = []
    for mlp_width in range(16, 8 * width + 1, 16):
        count = transformer_parameter_count(width, depth, mlp_width)
        candidates.append((abs(count - target), mlp_width))
    _, mlp_width = min(candidates)
    return ModelSpec(
        "transformer", "", width, depth, heads=heads, mlp_width=mlp_width
    )


_SMALL_KRON = ModelSpec("kronecker", "small", 128, 11, rank=8)
_LARGE_KRON = ModelSpec("kronecker", "large", 128, 45, rank=8)
_SMALL_TRANSFORMER = solve_transformer_match(
    kronecker_parameter_count(128, 11, 8), width=128, depth=5, heads=4
)
_LARGE_TRANSFORMER = solve_transformer_match(
    kronecker_parameter_count(128, 45, 8), width=256, depth=12, heads=8
)
_XLARGE_TRANSFORMER = ModelSpec(
    "transformer", "xlarge", width=1024, depth=20, heads=16, mlp_width=2512
)

PRESETS = {
    ("kronecker", "small"): _SMALL_KRON,
    ("transformer", "small"): ModelSpec(
        **{**asdict(_SMALL_TRANSFORMER), "scale": "small"}
    ),
    ("kronecker", "large"): _LARGE_KRON,
    ("transformer", "large"): ModelSpec(
        **{**asdict(_LARGE_TRANSFORMER), "scale": "large"}
    ),
    ("transformer", "xlarge"): _XLARGE_TRANSFORMER,
}


def per_token_rms(value: torch.Tensor) -> torch.Tensor:
    scale = torch.rsqrt(value.float().square().mean(-1, keepdim=True) + 1e-6)
    return value * scale.to(value.dtype)


def fan_in_normalized(weight: torch.Tensor, fan_in: int) -> torch.Tensor:
    scale = torch.rsqrt(
        weight.float().square().mean(tuple(range(1, weight.ndim)), keepdim=True)
        * fan_in
        + 1e-12
    )
    return weight * scale.to(weight.dtype)


class FactorizedVocabulary(nn.Module):
    def __init__(self, width: int) -> None:
        super().__init__()
        self.first = nn.Parameter(torch.randn(VOCAB_MODES[0], width) * 0.02)
        self.second = nn.Parameter(torch.randn(VOCAB_MODES[1], width) * 0.02)

    def embed(self, token_ids: torch.Tensor) -> torch.Tensor:
        first = token_ids.div(VOCAB_MODES[1], rounding_mode="floor")
        second = token_ids.remainder(VOCAB_MODES[1])
        return F.embedding(first, self.first) * F.embedding(second, self.second)

    def logits(self, hidden: torch.Tensor) -> torch.Tensor:
        shape = hidden.shape[:-1]
        logits = torch.einsum("...c,ic,jc->...ij", hidden, self.first, self.second)
        return logits.reshape(*shape, VOCAB_SIZE)


class DenseVocabulary(nn.Module):
    def __init__(self, width: int) -> None:
        super().__init__()
        self.weight = nn.Parameter(torch.randn(VOCAB_SIZE, width) * 0.02)

    def embed(self, token_ids: torch.Tensor) -> torch.Tensor:
        return F.embedding(token_ids, self.weight)

    def logits(self, hidden: torch.Tensor) -> torch.Tensor:
        return F.linear(hidden, self.weight)


class CausalKroneckerBlock(nn.Module):
    def __init__(self, width: int, rank: int, depth: int) -> None:
        super().__init__()
        self.width = width
        self.rank = rank
        self.depth = depth
        rows, columns = torch.tril_indices(CONTEXT_LENGTH, CONTEXT_LENGTH)
        self.register_buffer("rows", rows, persistent=False)
        self.register_buffer("columns", columns, persistent=False)
        self.position = nn.Parameter(
            torch.randn(rank, rows.numel()) / math.sqrt(CONTEXT_LENGTH)
        )
        self.channel = nn.Parameter(
            torch.randn(rank, width, width) / math.sqrt(width)
        )

    def position_matrix(self) -> torch.Tensor:
        linear = self.rows * CONTEXT_LENGTH + self.columns
        full = self.position.new_zeros(self.rank, CONTEXT_LENGTH * CONTEXT_LENGTH)
        return full.scatter(1, linear.expand(self.rank, -1), self.position).reshape(
            self.rank, CONTEXT_LENGTH, CONTEXT_LENGTH
        )

    def forward(self, value: torch.Tensor) -> torch.Tensor:
        position = fan_in_normalized(self.position, CONTEXT_LENGTH)
        linear = self.rows * CONTEXT_LENGTH + self.columns
        full = position.new_zeros(self.rank, CONTEXT_LENGTH * CONTEXT_LENGTH)
        position_matrix = full.scatter(
            1, linear.expand(self.rank, -1), position
        ).reshape(self.rank, CONTEXT_LENGTH, CONTEXT_LENGTH)
        channel = fan_in_normalized(self.channel, self.width)
        normalized = F.silu(per_token_rms(value))
        mixed = torch.einsum("btc,roc->brto", normalized, channel)
        mixed = torch.einsum("brto,rst->bso", mixed, position_matrix)
        residual = value + mixed / math.sqrt(self.rank * self.depth)
        return per_token_rms(residual)


def _rotate_half(value: torch.Tensor) -> torch.Tensor:
    first, second = value[..., 0::2], value[..., 1::2]
    return torch.stack((-second, first), dim=-1).flatten(-2)


def apply_rope(value: torch.Tensor, offset: int = 0) -> torch.Tensor:
    width = value.shape[-1]
    positions = torch.arange(
        offset, offset + value.shape[-2], device=value.device
    ).float()
    frequencies = 1.0 / (
        10_000.0
        ** (torch.arange(0, width, 2, device=value.device).float() / width)
    )
    angles = torch.outer(positions, frequencies).repeat_interleave(2, -1)
    cosine, sine = angles.cos().to(value.dtype), angles.sin().to(value.dtype)
    return value * cosine[None, None] + _rotate_half(value) * sine[None, None]


class TransformerBlock(nn.Module):
    def __init__(self, width: int, heads: int, mlp_width: int) -> None:
        super().__init__()
        self.width = width
        self.heads = heads
        self.qkv = nn.Linear(width, 3 * width, bias=False)
        self.output = nn.Linear(width, width, bias=False)
        self.gate_up = nn.Linear(width, 2 * mlp_width, bias=False)
        self.down = nn.Linear(mlp_width, width, bias=False)

    def forward_cached(
        self,
        value: torch.Tensor,
        cache: tuple[torch.Tensor, torch.Tensor] | None = None,
        *,
        offset: int = 0,
    ) -> tuple[torch.Tensor, tuple[torch.Tensor, torch.Tensor]]:
        batch, tokens, width = value.shape
        head_width = width // self.heads
        query, key, content = self.qkv(per_token_rms(value)).chunk(3, -1)

        def heads(item: torch.Tensor) -> torch.Tensor:
            return item.view(batch, tokens, self.heads, head_width).transpose(1, 2)

        query, key, content = (
            apply_rope(heads(query), offset),
            apply_rope(heads(key), offset),
            heads(content),
        )
        if cache is not None:
            key = torch.cat((cache[0], key), dim=-2)
            content = torch.cat((cache[1], content), dim=-2)
        attended = F.scaled_dot_product_attention(
            query,
            key,
            content,
            dropout_p=0.0,
            is_causal=cache is None,
        )
        attended = attended.transpose(1, 2).reshape(batch, tokens, width)
        value = value + self.output(attended)
        gate, up = self.gate_up(per_token_rms(value)).chunk(2, -1)
        output = per_token_rms(value + self.down(F.silu(gate) * up))
        return output, (key, content)

    def forward(self, value: torch.Tensor) -> torch.Tensor:
        return self.forward_cached(value)[0]


class LanguageModel(nn.Module):
    def __init__(self, spec: ModelSpec, *, activation_checkpointing: bool = True) -> None:
        super().__init__()
        if spec.architecture not in ARCHITECTURES:
            raise ValueError(f"unknown architecture: {spec.architecture}")
        self.spec = spec
        self.activation_checkpointing = activation_checkpointing
        if spec.architecture == "kronecker":
            self.vocabulary: nn.Module = FactorizedVocabulary(spec.width)
            self.blocks = nn.ModuleList(
                CausalKroneckerBlock(spec.width, spec.rank, spec.depth)
                for _ in range(spec.depth)
            )
        else:
            if spec.width % spec.heads:
                raise ValueError("width must be divisible by heads")
            self.vocabulary = DenseVocabulary(spec.width)
            self.blocks = nn.ModuleList(
                TransformerBlock(spec.width, spec.heads, spec.mlp_width)
                for _ in range(spec.depth)
            )

    def hidden(self, token_ids: torch.Tensor) -> torch.Tensor:
        if token_ids.ndim != 2 or token_ids.shape[1] != CONTEXT_LENGTH:
            raise ValueError(f"expected [batch,{CONTEXT_LENGTH}] token ids")
        value = self.vocabulary.embed(token_ids)
        for block in self.blocks:
            if self.activation_checkpointing and self.training and value.requires_grad:
                value = checkpoint(block, value, use_reentrant=False)
            else:
                value = block(value)
        return per_token_rms(value)

    def forward(self, token_ids: torch.Tensor) -> torch.Tensor:
        return self.vocabulary.logits(self.hidden(token_ids))

    def prefill_cache(
        self, token_ids: torch.Tensor
    ) -> tuple[torch.Tensor, list[tuple[torch.Tensor, torch.Tensor]]]:
        if self.spec.architecture != "transformer":
            raise RuntimeError("Kronecker mixer has no incremental cache")
        value = self.vocabulary.embed(token_ids)
        caches = []
        for block in self.blocks:
            value, cache = block.forward_cached(value)
            caches.append(cache)
        return self.vocabulary.logits(per_token_rms(value)), caches

    def decode_step(
        self,
        token_ids: torch.Tensor,
        caches: list[tuple[torch.Tensor, torch.Tensor]],
        position: int,
    ) -> tuple[torch.Tensor, list[tuple[torch.Tensor, torch.Tensor]]]:
        if self.spec.architecture != "transformer" or token_ids.shape[1] != 1:
            raise RuntimeError("cached decode requires a Transformer single-token input")
        value = self.vocabulary.embed(token_ids)
        updated = []
        for block, cache in zip(self.blocks, caches, strict=True):
            value, next_cache = block.forward_cached(value, cache, offset=position)
            updated.append(next_cache)
        return self.vocabulary.logits(per_token_rms(value)), updated


def parameter_inventory(model: LanguageModel) -> dict[str, int]:
    vocabulary = sum(parameter.numel() for parameter in model.vocabulary.parameters())
    body = sum(parameter.numel() for parameter in model.blocks.parameters())
    return {"total": vocabulary + body, "vocabulary": vocabulary, "body": body}


def preset_inventory() -> dict[str, dict[str, int]]:
    return {
        f"{architecture}-{scale}": parameter_inventory(
            LanguageModel(PRESETS[(architecture, scale)], activation_checkpointing=False)
        )
        for architecture in ARCHITECTURES
        for scale in SCALES
    }


def assert_matched_presets(max_fraction: float = 0.0025) -> None:
    inventory = preset_inventory()
    for scale in SCALES:
        kronecker = inventory[f"kronecker-{scale}"]["total"]
        transformer = inventory[f"transformer-{scale}"]["total"]
        mismatch = abs(kronecker - transformer) / min(kronecker, transformer)
        if mismatch > max_fraction:
            raise RuntimeError(f"{scale} parameter mismatch is {mismatch:.4%}")


def cross_entropy(logits: torch.Tensor, targets: torch.Tensor) -> torch.Tensor:
    return F.cross_entropy(logits.float().flatten(0, 1), targets.flatten())


def permutation_multiplier(size: int, seed: int) -> int:
    value = max(1, 2 * seed + 1)
    while math.gcd(value, size) != 1:
        value += 2
    return value


def batch_indices(size: int, step: int, local_batch: int, rank: int, world: int, seed: int) -> np.ndarray:
    needed = local_batch * world
    if needed > size:
        raise ValueError("global batch exceeds dataset windows")
    start = (step - 1) * needed
    if start + needed > size:
        raise ValueError("step exceeds one deterministic corpus pass")
    positions = np.arange(start, start + needed, dtype=np.int64)
    multiplier = permutation_multiplier(size, seed)
    offset = int.from_bytes(hashlib.sha256(str(seed).encode()).digest()[:8], "big") % size
    indices = (positions * multiplier + offset) % size
    return indices[rank * local_batch : (rank + 1) * local_batch]


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(8 << 20), b""):
            digest.update(chunk)
    return digest.hexdigest()


def load_windows(data_root: Path, split: str) -> np.ndarray:
    manifest = json.loads((data_root / "manifest.json").read_text())
    if manifest.get("schema") != "exp10-tokenized-corpus-v1":
        raise RuntimeError("invalid Exp10 dataset manifest")
    path = data_root / f"{split}.npy"
    expected = manifest["files"][path.name]
    if expected["sha256"] != sha256(path):
        raise RuntimeError(f"checksum mismatch for {path}")
    return np.load(path, mmap_mode="r")


@torch.inference_mode()
def evaluate(
    model: LanguageModel,
    windows: np.ndarray,
    batch: int,
    device: torch.device,
    *,
    rank: int = 0,
    world: int = 1,
) -> dict[str, float]:
    model.eval()
    total_loss, total_tokens = 0.0, 0
    local = np.arange(rank, len(windows), world)
    for start in range(0, len(local), batch):
        values = torch.as_tensor(
            np.asarray(windows[local[start : start + batch]], dtype=np.int64),
            device=device,
        )
        inputs, targets = values[:, :-1], values[:, 1:]
        with torch.autocast(device.type, dtype=torch.bfloat16, enabled=device.type == "cuda"):
            logits = model(inputs)
        total_loss += float(F.cross_entropy(logits.float().flatten(0, 1), targets.flatten(), reduction="sum"))
        total_tokens += targets.numel()
    totals = torch.tensor([total_loss, total_tokens], device=device, dtype=torch.float64)
    if world > 1:
        dist.all_reduce(totals)
    loss = float(totals[0] / totals[1])
    model.train()
    return {
        "nll": loss,
        "perplexity": math.exp(min(loss, 20)),
        "tokens": int(totals[1]),
    }


def distributed_context(require_eight: bool) -> tuple[int, int, int, torch.device]:
    world = int(os.environ.get("WORLD_SIZE", "1"))
    rank = int(os.environ.get("RANK", "0"))
    local_rank = int(os.environ.get("LOCAL_RANK", "0"))
    if require_eight and (world != 8 or not torch.cuda.is_available()):
        raise RuntimeError("paid Exp10 runs require exactly eight CUDA ranks")
    if world > 1:
        torch.cuda.set_device(local_rank)
        dist.init_process_group("nccl", device_id=torch.device(f"cuda:{local_rank}"))
        return world, rank, local_rank, torch.device(f"cuda:{local_rank}")
    return 1, 0, 0, torch.device("cuda" if torch.cuda.is_available() else "cpu")


def initialize_wandb(args: argparse.Namespace, inventory: dict[str, int], world: int, rank: int):
    if rank:
        holder: list[Any] = [None]
        dist.broadcast_object_list(holder, src=0)
        return None, holder[0]
    import wandb

    if not os.environ.get("WANDB_API_KEY"):
        raise RuntimeError("WANDB_API_KEY is required before a paid launch")
    wandb.login(key=os.environ["WANDB_API_KEY"], verify=True)
    run = wandb.init(
        project=args.wandb_project,
        name=args.run_name,
        config={**vars(args), "inventory": inventory, "world_size": world},
    )
    if not run.url:
        raise RuntimeError("W&B did not provide a direct run URL")
    holder = [run.url]
    if world > 1:
        dist.broadcast_object_list(holder, src=0)
    print(f"WANDB_URL={run.url}", flush=True)
    return run, run.url


def train(args: argparse.Namespace) -> None:
    world, rank, local_rank, device = distributed_context(args.require_eight_gpus)
    torch.manual_seed(args.seed)
    spec = PRESETS[(args.architecture, args.scale)]
    model = LanguageModel(spec).to(device)
    inventory = parameter_inventory(model)
    optimizer = torch.optim.AdamW(
        model.parameters(), lr=args.lr, betas=(0.9, 0.95), weight_decay=args.weight_decay
    )
    wrapped: nn.Module = model
    if world > 1:
        wrapped = DistributedDataParallel(
            model, device_ids=[local_rank], broadcast_buffers=False, gradient_as_bucket_view=True
        )
    if args.compile and device.type == "cuda":
        wrapped = torch.compile(wrapped)
    train_windows = load_windows(Path(args.data_root), "train")
    validation_windows = load_windows(Path(args.data_root), "validation")
    if args.validation_windows:
        validation_windows = validation_windows[: args.validation_windows]
    run, wandb_url = initialize_wandb(args, inventory, world, rank)
    started = time.perf_counter()
    timed_seconds = 0.0
    timed_steps = 0
    last: dict[str, float] = {}
    for step in range(1, args.steps + 1):
        if device.type == "cuda":
            torch.cuda.synchronize(device)
        step_started = time.perf_counter()
        indices = batch_indices(len(train_windows), step, args.local_batch, rank, world, args.seed)
        values = torch.as_tensor(np.asarray(train_windows[indices], dtype=np.int64), device=device)
        inputs, targets = values[:, :-1], values[:, 1:]
        optimizer.zero_grad(set_to_none=True)
        with torch.autocast(device.type, dtype=torch.bfloat16, enabled=device.type == "cuda"):
            loss = cross_entropy(wrapped(inputs), targets)
        loss.backward()
        norm = torch.nn.utils.clip_grad_norm_(model.parameters(), 1.0)
        if not torch.isfinite(loss) or not torch.isfinite(norm):
            raise RuntimeError("non-finite forward/backward state")
        optimizer.step()
        for state in optimizer.state.values():
            if any(isinstance(value, torch.Tensor) and not torch.isfinite(value).all() for value in state.values()):
                raise RuntimeError("non-finite optimizer state")
        if device.type == "cuda":
            torch.cuda.synchronize(device)
        step_seconds = time.perf_counter() - step_started
        if step > 1:
            timed_seconds += step_seconds
            timed_steps += 1
        elapsed = time.perf_counter() - started
        global_tokens = args.local_batch * world * CONTEXT_LENGTH
        last = {
            "step": step,
            "tokens_seen": step * global_tokens,
            "global_token_batch": global_tokens,
            "train_nll": float(loss.detach()),
            "grad_norm": float(norm.detach()),
            "step_seconds": step_seconds,
            "tokens_per_second": step * global_tokens / elapsed,
            "sustained_tokens_per_second": (
                timed_steps * global_tokens / timed_seconds if timed_steps else None
            ),
            "peak_allocated_gib": torch.cuda.max_memory_allocated(device) / 2**30 if device.type == "cuda" else 0.0,
            "peak_reserved_gib": torch.cuda.max_memory_reserved(device) / 2**30 if device.type == "cuda" else 0.0,
        }
        if rank == 0:
            print(json.dumps(last), flush=True)
            run.log(last, step=step)
    if world > 1:
        dist.barrier()
    validation = evaluate(
        model, validation_windows, args.eval_batch, device, rank=rank, world=world
    )
    if rank == 0:
        output = Path(args.output_dir)
        output.mkdir(parents=True, exist_ok=True)
        checkpoint = output / "checkpoint.pt"
        torch.save({"schema": SCHEMA, "spec": asdict(spec), "model": model.state_dict(), "step": args.steps}, checkpoint)
        result = {
            "schema": SCHEMA,
            "architecture": args.architecture,
            "scale": args.scale,
            "seed": args.seed,
            "lr": args.lr,
            "phase": args.phase,
            "steps": args.steps,
            "inventory": inventory,
            "performance": last,
            "validation": validation,
            "wandb_url": wandb_url,
            "checkpoint": str(checkpoint),
        }
        (output / "result.json").write_text(json.dumps(result, indent=2, sort_keys=True))
        run.log({f"validation/{key}": value for key, value in validation.items()}, step=args.steps)
        run.finish()
        print(json.dumps(result, indent=2), flush=True)
    if world > 1:
        dist.destroy_process_group()


def evaluate_checkpoint(args: argparse.Namespace) -> None:
    world, rank, _, device = distributed_context(args.require_eight_gpus)
    spec = PRESETS[(args.architecture, args.scale)]
    model = LanguageModel(spec, activation_checkpointing=False).to(device)
    checkpoint_path = Path(args.output_dir) / "checkpoint.pt"
    saved = torch.load(checkpoint_path, map_location="cpu", weights_only=True)
    if saved.get("schema") != SCHEMA or saved.get("spec") != asdict(spec):
        raise RuntimeError("checkpoint identity mismatch")
    model.load_state_dict(saved["model"])
    test = evaluate(
        model,
        load_windows(Path(args.data_root), "test"),
        args.eval_batch,
        device,
        rank=rank,
        world=world,
    )
    if rank == 0:
        result_path = Path(args.output_dir) / "result.json"
        result = json.loads(result_path.read_text())
        result["test"] = test
        result_path.write_text(json.dumps(result, indent=2, sort_keys=True))
        print(json.dumps(result, indent=2), flush=True)
    if world > 1:
        dist.destroy_process_group()


@torch.inference_mode()
def benchmark_checkpoint(args: argparse.Namespace) -> None:
    world, rank, _, device = distributed_context(args.require_eight_gpus)
    spec = PRESETS[(args.architecture, args.scale)]
    model = LanguageModel(spec, activation_checkpointing=False).to(device).eval()
    checkpoint_path = Path(args.output_dir) / "checkpoint.pt"
    saved = torch.load(checkpoint_path, map_location="cpu", weights_only=True)
    model.load_state_dict(saved["model"])
    windows = load_windows(Path(args.data_root), "test")
    local = np.arange(rank, len(windows), world)
    prefix_sums = torch.zeros(4, device=device, dtype=torch.float64)
    prefix_count = torch.zeros(1, device=device, dtype=torch.float64)
    positions = (31, 63, 127, 255)
    for start in range(0, len(local), args.eval_batch):
        values = torch.as_tensor(
            np.asarray(windows[local[start : start + args.eval_batch]], dtype=np.int64),
            device=device,
        )
        with torch.autocast(device.type, dtype=torch.bfloat16, enabled=device.type == "cuda"):
            logits = model(values[:, :-1])
        for index, position in enumerate(positions):
            prefix_sums[index] += F.cross_entropy(
                logits[:, position].float(), values[:, position + 1], reduction="sum"
            ).double()
        prefix_count += len(values)
    if world > 1:
        dist.all_reduce(prefix_sums)
        dist.all_reduce(prefix_count)

    sample = torch.as_tensor(
        np.asarray(windows[rank : rank + 1, :-1], dtype=np.int64), device=device
    )
    for _ in range(2):
        model(sample)
    if device.type == "cuda":
        torch.cuda.synchronize(device)
    started = time.perf_counter()
    for _ in range(args.benchmark_repetitions):
        model(sample)
    if device.type == "cuda":
        torch.cuda.synchronize(device)
    prefill_seconds = (time.perf_counter() - started) / args.benchmark_repetitions

    decode_tokens = args.generation_tokens
    if args.architecture == "transformer":
        prefill_logits, caches = model.prefill_cache(sample)
        token = prefill_logits[:, -1].argmax(-1, keepdim=True)
        if device.type == "cuda":
            torch.cuda.synchronize(device)
        started = time.perf_counter()
        for position in range(CONTEXT_LENGTH, CONTEXT_LENGTH + decode_tokens):
            logits, caches = model.decode_step(token, caches, position)
            token = logits[:, -1].argmax(-1, keepdim=True)
        if device.type == "cuda":
            torch.cuda.synchronize(device)
        cache_supported = True
    else:
        rolling = sample
        if device.type == "cuda":
            torch.cuda.synchronize(device)
        started = time.perf_counter()
        for _ in range(decode_tokens):
            logits = model(rolling)
            token = logits[:, -1].argmax(-1, keepdim=True)
            rolling = torch.cat((rolling[:, 1:], token), dim=1)
        if device.type == "cuda":
            torch.cuda.synchronize(device)
        cache_supported = False
    decode_seconds = time.perf_counter() - started
    payload = {
        "prefix_position_nll": {
            str(position + 1): float(prefix_sums[index] / prefix_count)
            for index, position in enumerate(positions)
        },
        "prefill_seconds_batch1": prefill_seconds,
        "prefill_tokens_per_second_batch1": CONTEXT_LENGTH / prefill_seconds,
        "decode_tokens": decode_tokens,
        "decode_tokens_per_second_batch1": decode_tokens / decode_seconds,
        "incremental_cache_supported": cache_supported,
    }
    gathered = [None] * world
    if world > 1:
        dist.all_gather_object(gathered, payload)
    else:
        gathered = [payload]
    if rank == 0:
        result_path = Path(args.output_dir) / "result.json"
        result = json.loads(result_path.read_text())
        result["benchmark"] = {
            **payload,
            "per_gpu": gathered,
        }
        result_path.write_text(json.dumps(result, indent=2, sort_keys=True))
        print(json.dumps(result, indent=2), flush=True)
    if world > 1:
        dist.destroy_process_group()


def self_test() -> None:
    assert_matched_presets()
    torch.manual_seed(0)
    specs = (
        ModelSpec("kronecker", "tiny", 8, 1, rank=2),
        ModelSpec("transformer", "tiny", 8, 1, heads=2, mlp_width=16),
    )
    for spec in specs:
        model = LanguageModel(spec, activation_checkpointing=False)
        tokens = torch.randint(0, VOCAB_SIZE, (1, CONTEXT_LENGTH))
        logits = model(tokens)
        assert logits.shape == (1, CONTEXT_LENGTH, VOCAB_SIZE)
        loss = cross_entropy(logits, tokens)
        loss.backward()
        assert torch.isfinite(loss)
    print(json.dumps(preset_inventory(), indent=2))
    print("self-test passed")


def parser() -> argparse.ArgumentParser:
    value = argparse.ArgumentParser()
    value.add_argument(
        "--mode",
        choices=("train", "evaluate", "benchmark", "self-test", "inventory"),
        default="train",
    )
    value.add_argument(
        "--architecture", choices=ARCHITECTURES, default=DEFAULT_ARCHITECTURE
    )
    value.add_argument("--scale", choices=MODEL_SCALES, default="small")
    value.add_argument("--seed", type=int, default=0)
    value.add_argument("--lr", type=float, default=3e-4)
    value.add_argument("--weight-decay", type=float, default=0.1)
    value.add_argument("--steps", type=int, default=10)
    value.add_argument("--local-batch", type=int, default=1)
    value.add_argument("--eval-batch", type=int, default=1)
    value.add_argument(
        "--validation-windows",
        type=int,
        default=0,
        help="limit validation windows for preflight probes; zero evaluates the full split",
    )
    value.add_argument("--data-root", default="/cache/exp10/tinystories")
    value.add_argument("--output-dir", default="/cache/exp10/runs/default")
    value.add_argument("--wandb-project", default="exp10-causal-architecture-verdict")
    value.add_argument("--run-name", default="exp10")
    value.add_argument("--phase", choices=("screen", "promotion", "final"), default="final")
    value.add_argument("--benchmark-repetitions", type=int, default=10)
    value.add_argument("--generation-tokens", type=int, default=32)
    value.add_argument("--compile", action=argparse.BooleanOptionalAction, default=True)
    value.add_argument("--require-eight-gpus", action=argparse.BooleanOptionalAction, default=True)
    return value


def main() -> None:
    args = parser().parse_args()
    if args.mode == "self-test":
        self_test()
    elif args.mode == "inventory":
        assert_matched_presets()
        print(json.dumps(preset_inventory(), indent=2))
    elif args.mode == "evaluate":
        evaluate_checkpoint(args)
    elif args.mode == "benchmark":
        benchmark_checkpoint(args)
    else:
        train(args)


if __name__ == "__main__":
    main()
