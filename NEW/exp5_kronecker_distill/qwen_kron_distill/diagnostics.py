from __future__ import annotations

import json

import numpy as np
import torch
import torch.distributed as dist
from qwen_normuon_pretrain.data import load_split

from .config import DEFAULT_DATA_ROOT, EVALUATION_BATCH, VALIDATION_EXAMPLES
from .objective import load_teacher, teacher_training_targets
from .train import close_distributed, distributed_context


@torch.inference_mode()
def measure_product_floor(
    *,
    data_root: str = DEFAULT_DATA_ROOT,
    examples: int = VALIDATION_EXAMPLES,
    batch_size: int = EVALUATION_BATCH,
) -> dict:
    """Measure KL(P || P0⊗P1), the best possible additive-head KL."""

    context = distributed_context()
    try:
        validation_contexts, _, _ = load_split(data_root, "validation")
        if not 0 < examples <= len(validation_contexts):
            raise ValueError("diagnostic example count is out of range")
        if batch_size <= 0:
            raise ValueError("diagnostic batch size must be positive")
        teacher = load_teacher(str(context.device))
        totals = torch.zeros(4, device=context.device, dtype=torch.float64)
        indices = np.arange(
            context.rank,
            examples,
            context.world_size,
            dtype=np.int64,
        )
        for start in range(0, len(indices), batch_size):
            index = indices[start : start + batch_size]
            token_ids = torch.from_numpy(
                np.array(validation_contexts[index], copy=True)
            ).to(context.device, dtype=torch.long)
            marginal0, marginal1, entropy = teacher_training_targets(
                teacher,
                token_ids,
            )
            entropy0 = -(
                marginal0 * marginal0.clamp_min(1e-30).log()
            ).sum(dim=-1)
            entropy1 = -(
                marginal1 * marginal1.clamp_min(1e-30).log()
            ).sum(dim=-1)
            mutual_information = entropy0 + entropy1 - entropy
            totals[0] += mutual_information.double().sum()
            totals[1] += entropy0.double().sum()
            totals[2] += entropy1.double().sum()
            totals[3] += len(index)
        if context.world_size > 1:
            dist.all_reduce(totals, op=dist.ReduceOp.SUM)
        count = float(totals[3])
        result = {
            "schema": "qwen-kron-product-floor-v1",
            "status": "complete",
            "examples": int(count),
            "world_size": context.world_size,
            "irreducible_product_kl": float(totals[0] / count),
            "teacher_marginal0_entropy": float(totals[1] / count),
            "teacher_marginal1_entropy": float(totals[2] / count),
        }
        if context.primary:
            print(json.dumps(result, indent=2, sort_keys=True), flush=True)
        return result
    finally:
        close_distributed(context)
