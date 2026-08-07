"""One training run: fixed config, fresh stream, checkpointed val curve, manifest."""

from __future__ import annotations

import json
import math
import time
from pathlib import Path

import numpy as np
import torch

from ..families.base import Family
from ..learners.transformer import (
    CausalTransformer,
    TransformerConfig,
    cross_entropy_bits,
)
from ..protocol import write_manifest
from ..seeding import seed_from
from .metrics import final_gap, grokking_jump, tokens_to_threshold
from .stream import FamilyStream

THETA_DEFAULT = 0.05  # preregistered threshold above Bayes floor (bits/token)


def _seed_everything(seed: int):
    np.random.seed(seed)
    torch.manual_seed(seed)


@torch.no_grad()
def _validate(model, family, rng, device, ctx_len, n_batches=8, batch=32) -> float:
    model.eval()
    val_fam = family.val_view() if hasattr(family, "val_view") else family
    stream = FamilyStream(val_fam, rng, ctx_len)
    total = 0.0
    for _ in range(n_batches):
        xb, yb = stream.next_batch(batch)
        logits = model(torch.from_numpy(xb).to(device))
        total += cross_entropy_bits(logits, torch.from_numpy(yb).to(device))
    model.train()
    return total / n_batches


def train_run(family: Family, cfg: TransformerConfig, budget_tokens: int, seed: int,
              out_dir: Path, cell_id: str, protocol_hash: str, device: str = "cpu",
              theta: float = THETA_DEFAULT, n_checkpoints: int = 20,
              tokens_per_step: int = 1024, record_initial: bool = False) -> dict:
    """Train one cell; returns the metrics dict (also written to out_dir)."""
    out_dir = Path(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)
    seed = int(seed_from(protocol_hash, family.version, cell_id, "train") % (2**32)) if seed < 0 else seed
    _seed_everything(seed)

    dev = torch.device(device)
    model = CausalTransformer(cfg).to(dev)
    stream = FamilyStream(family, np.random.default_rng(seed_from(protocol_hash,
                        family.version, cell_id, "stream")), cfg.ctx_len)
    val_rng = np.random.default_rng(seed_from(protocol_hash, family.version, cell_id, "val"))

    batch = max(1, tokens_per_step // cfg.ctx_len)
    total_steps = max(1, budget_tokens // (batch * cfg.ctx_len))
    opt = torch.optim.AdamW(model.parameters(), lr=cfg.lr, weight_decay=cfg.weight_decay)
    sched = torch.optim.lr_scheduler.CosineAnnealingLR(opt, T_max=total_steps,
                                                       eta_min=cfg.lr * 0.1)

    floor = family.entropy_rate()
    every = max(1, total_steps // n_checkpoints)
    token_grid: list[int] = []
    val_curve: list[float] = []
    train_curve: list[float] = []

    t0 = time.time()
    initial_val_ce = None
    if record_initial:
        # Use a separate validation stream so recording t=0 does not perturb the
        # checkpoint validation RNG or the optimization trajectory.
        init_rng = np.random.default_rng(seed_from(protocol_hash, family.version,
                                                    cell_id, "val-init"))
        initial_val_ce = _validate(model, family, init_rng, dev, cfg.ctx_len)
        token_grid.append(0)
        val_curve.append(initial_val_ce)
    tokens_seen = 0
    for step in range(1, total_steps + 1):
        xb, yb = stream.next_batch(batch)
        logits = model(torch.from_numpy(xb).to(dev))
        loss = torch.nn.functional.cross_entropy(
            logits.reshape(-1, cfg.vocab), torch.from_numpy(yb).reshape(-1).to(dev))
        opt.zero_grad(set_to_none=True)
        loss.backward()
        torch.nn.utils.clip_grad_norm_(model.parameters(), cfg.grad_clip)
        opt.step()
        sched.step()
        tokens_seen += batch * cfg.ctx_len
        if step % every == 0 or step == total_steps:
            ce = _validate(model, family, val_rng, dev, cfg.ctx_len)
            token_grid.append(tokens_seen)
            val_curve.append(ce)
            train_curve.append(float(loss.item()) / math.log(2.0))

    wall = time.time() - t0
    metrics = {
        "cell_id": cell_id,
        "family": family.name,
        "family_version": family.version,
        "config_hash": cfg.config_hash,
        "seed": seed,
        "budget_tokens": tokens_seen,
        "bayes_floor_bits": floor,
        "theta": theta,
        "token_grid": token_grid,
        "val_ce_bits": val_curve,
        "train_ce_bits": train_curve,
        "initial_val_ce_bits": initial_val_ce,
        "initial_checkpoint_recorded": bool(record_initial),
        "T_star": tokens_to_threshold(token_grid, val_curve, floor, theta),
        "final_gap_bits": final_gap(val_curve, floor),
        "grokking_jump_at": grokking_jump(token_grid, val_curve),
        "wallclock_seconds": wall,
        "n_params": model.n_params(),
        "device": device,
    }
    metrics_path = out_dir / "metrics.json"
    metrics_path.write_text(json.dumps(metrics, indent=2))

    write_manifest(out_dir, {
        "protocol_hash": protocol_hash,
        "family_version": family.version,
        "cell_id": cell_id,
        "seed": seed,
        "budget_tokens": tokens_seen,
        "device": device,
        "torch_version": torch.__version__,
        "wallclock_seconds": wall,
        "config_hash": cfg.config_hash,
        "metrics_path": str(metrics_path),
        "status": "complete",
    })
    return metrics
