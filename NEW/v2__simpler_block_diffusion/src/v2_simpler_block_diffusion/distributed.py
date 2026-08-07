from __future__ import annotations

import os
from dataclasses import dataclass

import torch
import torch.distributed as dist


EXPECTED_GPU_COUNT = 8


@dataclass(frozen=True)
class DistributedContext:
    rank: int
    local_rank: int
    world_size: int
    device: torch.device

    @property
    def primary(self) -> bool:
        return self.rank == 0


def initialize_distributed() -> DistributedContext:
    if not torch.cuda.is_available():
        raise RuntimeError("V2 paid execution requires CUDA")
    world = int(os.environ.get("WORLD_SIZE", "1"))
    rank = int(os.environ.get("RANK", "0"))
    local_rank = int(os.environ.get("LOCAL_RANK", "0"))
    if world != EXPECTED_GPU_COUNT or torch.cuda.device_count() != EXPECTED_GPU_COUNT:
        raise RuntimeError(
            f"V2 requires exactly {EXPECTED_GPU_COUNT} visible GPUs and ranks; "
            f"found world={world}, visible={torch.cuda.device_count()}"
        )
    torch.cuda.set_device(local_rank)
    if not dist.is_initialized():
        dist.init_process_group("nccl")
    context = DistributedContext(rank, local_rank, world, torch.device("cuda", local_rank))
    local_name = torch.cuda.get_device_name(local_rank)
    local_memory = torch.cuda.get_device_properties(local_rank).total_memory / 2**30
    inventory: list[tuple[str, float] | None] = [None] * world
    dist.all_gather_object(inventory, (local_name, local_memory))
    if any(name is None or "H100" not in name[0] or name[1] < 75 for name in inventory):
        raise RuntimeError(f"expected 8xH100-80GB, found {inventory}")
    return context


def global_sum(value: int | float, context: DistributedContext) -> float:
    tensor = torch.tensor(float(value), device=context.device, dtype=torch.float64)
    dist.all_reduce(tensor, op=dist.ReduceOp.SUM)
    return float(tensor.item())


def global_mean(value: torch.Tensor, context: DistributedContext) -> float:
    tensor = value.detach().float().to(context.device)
    dist.all_reduce(tensor, op=dist.ReduceOp.SUM)
    return float((tensor / context.world_size).item())

