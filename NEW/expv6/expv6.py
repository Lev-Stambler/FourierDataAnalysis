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
    khatri_rao_logits,
    load_teacher,
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
    "local_batch": 49_152,       # 393,216 contexts / 6,291,456 tokens globally
    "teacher_microbatch": 2_048,
    "steps": 171,                # 67,239,936 contexts: Exp V5-scale run
    "lr": 0.2,
    "weight_decay": 0.01,
    "warmup_steps": 4,
    "checkpoint_every": 16,
    "data_root": "/cache/qwen_fullwidth_distill/context16-fineweb-edu-next-token-4m-v1",
    "output_dir": "/cache/expv6-kiss",
    "wandb_project": "qwen-causal-kron-distill",
    "run_name": "expv6-kiss-production",
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
def teacher_targets(teacher, token_ids: torch.Tensor) -> torch.Tensor:
    """BF16 soft targets; dropping teacher entropy leaves gradients unchanged."""
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
        probability[start:stop].copy_(
            F.softmax(F.linear(hidden, weight).float(), -1)
        )
    return probability


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
    print("self-test passed")


def batch_indices(size: int, step: int, rank: int, world: int) -> np.ndarray:
    generator = np.random.default_rng(CONFIG["seed"] + step)
    indices = generator.choice(size, CONFIG["local_batch"] * world, replace=False)
    start = rank * CONFIG["local_batch"]
    return indices[start : start + CONFIG["local_batch"]]


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

    contexts, _, _ = load_split(CONFIG["data_root"], "train")
    teacher = load_teacher(str(device))
    student = Student().to(device)
    model = DistributedDataParallel(
        student,
        device_ids=[local_rank],
        broadcast_buffers=False,
        gradient_as_bucket_view=True,
    )
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
                "global_batch": CONFIG["local_batch"] * world,
                "global_token_batch": (
                    CONFIG["local_batch"] * world * CONFIG["context_length"]
                ),
            },
        )
        if not run.url:
            raise RuntimeError("W&B did not return a direct run URL")
        print(f"WANDB_URL={run.url}", flush=True)

    output = Path(CONFIG["output_dir"])
    started = time.perf_counter()
    last_loss = float("nan")
    for step in range(1, CONFIG["steps"] + 1):
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
        if not torch.isfinite(norm):
            raise RuntimeError("non-finite gradient")
        multiplier = min(1.0, step / CONFIG["warmup_steps"])
        optimizer.param_groups[0]["lr"] = CONFIG["lr"] * multiplier
        optimizer.step()
        last_loss = float(loss.detach())

        elapsed = time.perf_counter() - started
        global_batch = CONFIG["local_batch"] * world
        metrics = {
            "step": step,
            "loss": last_loss,
            "contexts_per_second": step * global_batch / elapsed,
            "tokens_per_second": (
                step * global_batch * CONFIG["context_length"] / elapsed
            ),
            "global_batch": global_batch,
            "global_token_batch": global_batch * CONFIG["context_length"],
            "peak_allocated_gib": torch.cuda.max_memory_allocated() / 2**30,
            "peak_reserved_gib": torch.cuda.max_memory_reserved() / 2**30,
        }
        gathered = [None] * world
        dist.all_gather_object(gathered, metrics)
        if primary:
            metrics["per_gpu_peak_allocated_gib"] = [
                item["peak_allocated_gib"] for item in gathered
            ]
            print(json.dumps(metrics), flush=True)
            run.log(metrics, step=step)

        if step % CONFIG["checkpoint_every"] == 0 or step == CONFIG["steps"]:
            if primary:
                output.mkdir(parents=True, exist_ok=True)
                torch.save(
                    {
                        "config": CONFIG,
                        "step": step,
                        "model": student.state_dict(),
                        "optimizer": optimizer.state_dict(),
                        "wandb_url": run.url,
                    },
                    output / "checkpoint.pt",
                )
            dist.barrier()

    if primary:
        result = {
            "status": "complete",
            "loss": last_loss,
            "wandb_url": run.url,
            "global_batch": CONFIG["local_batch"] * world,
            "global_token_batch": (
                CONFIG["local_batch"] * world * CONFIG["context_length"]
            ),
        }
        output.mkdir(parents=True, exist_ok=True)
        (output / "result.json").write_text(json.dumps(result, indent=2))
        run.log(result)
        run.finish()
        print(json.dumps(result, indent=2), flush=True)
    dist.destroy_process_group()


if __name__ == "__main__":
    overrides()
    if CONFIG["self_test"]:
        self_test()
    else:
        train()
