"""Exact degree × radius × energy categorical factorial on one H100."""

from __future__ import annotations

import hashlib
import json
from pathlib import Path

import modal

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs/local/v24_spectrum_predictor"
RESULT_PATH = OUT / "factorial_remote_results.json"
CONFIGURATIONS = (
    {"id": "learned_absolute_d64_l2", "position_encoding": "learned_absolute"},
    {"id": "sinusoidal_d64_l2", "position_encoding": "sinusoidal"},
    {"id": "alibi_d64_l2", "position_encoding": "alibi"},
)


def _ignore(path: Path) -> bool:
    value = str(path)
    return any(
        part in value
        for part in (
            "__pycache__",
            ".venv",
            ".pytest_cache",
            ".ruff_cache",
            "runs/",
            "dlx/data_cache/",
        )
    )


def _load_protocol() -> dict:
    protocol = json.loads((ROOT / "configs/protocol_v2.4.json").read_text())
    recorded = protocol["protocol_hash"]
    unhashed = dict(protocol)
    unhashed.pop("protocol_hash")
    expected = hashlib.sha256(json.dumps(unhashed, sort_keys=True).encode()).hexdigest()
    if recorded != expected:
        raise ValueError(f"protocol hash mismatch: {recorded} != {expected}")
    return protocol


app = modal.App("dlx-v24-exact-factorial")
image = (
    modal.Image.debian_slim(python_version="3.12")
    .pip_install("numpy>=1.26", "torch>=2.3")
    .add_local_dir(ROOT, "/root/pkg", ignore=_ignore)
)


@app.function(
    image=image,
    gpu="H100",
    cpu=8.0,
    memory=16384,
    timeout=1800,
    retries=0,
    max_containers=1,
    block_network=True,
)
def train_factorial_cell(payload: dict) -> dict:
    import math
    import sys
    import time

    import numpy as np
    import torch

    sys.path.insert(0, "/root/pkg")
    from dlx.analysis.floor_independent import curve_metrics
    from dlx.learners.transformer import CausalTransformer, TransformerConfig
    from dlx.seeding import seed_from

    protocol = payload["protocol"]
    factor = protocol["exact_factorial"]
    training = protocol["training"]
    configuration = payload["configuration"]
    degree = int(payload["degree"])
    radius = int(payload["radius"])
    signal_probability = float(payload["signal_probability"])
    seed = int(payload["seed"])
    q = int(factor["alphabet"])
    ctx_len = int(factor["context"])
    support = (radius,) if degree == 1 else (1, radius)
    cell_id = (
        f"V24F/{configuration['id']}/k{degree}/r{radius}/"
        f"p{signal_probability:.2f}/s{seed}"
    )
    torch.set_num_threads(8)
    torch.manual_seed(seed)
    np.random.seed(seed)
    model = CausalTransformer(
        TransformerConfig(
            vocab=q,
            ctx_len=ctx_len,
            d_model=training["d_model"],
            n_layers=training["n_layers"],
            n_heads=training["n_heads"],
            mlp_mult=training["mlp_mult"],
            dropout=training["dropout"],
            tie_weights=training["tie_weights"],
            lr=training["lr"],
            weight_decay=training["weight_decay"],
            grad_clip=training["grad_clip"],
            position_encoding=configuration["position_encoding"],
        )
    ).cuda()

    def sample(rng: np.random.Generator, count: int) -> tuple[np.ndarray, np.ndarray]:
        contexts = rng.integers(0, q, size=(count, ctx_len), dtype=np.int64)
        targets = np.zeros(count, dtype=np.int64)
        for lag in support:
            targets += contexts[:, ctx_len - lag]
        targets %= q
        signal = rng.random(count) < signal_probability
        noise = rng.integers(0, q, size=count, dtype=np.int64)
        return contexts, np.where(signal, targets, noise)

    train_rng = np.random.default_rng(
        seed_from(protocol["protocol_hash"], cell_id, "factorial-train")
    )
    val_rng = np.random.default_rng(
        seed_from(protocol["protocol_hash"], cell_id, "factorial-val")
    )
    val_x, val_y = sample(val_rng, 4096)

    @torch.no_grad()
    def validate() -> float:
        model.eval()
        total = 0.0
        batch_count = 0
        for start in range(0, len(val_x), 256):
            x = torch.from_numpy(val_x[start : start + 256]).cuda()
            y = torch.from_numpy(val_y[start : start + 256]).cuda()
            logits = model(x)[:, -1, :]
            total += float(torch.nn.functional.cross_entropy(logits, y).item())
            batch_count += 1
        model.train()
        return total / batch_count / math.log(2.0)

    examples_per_step = 256
    total_steps = max(
        1, training["budget_tokens"] // (examples_per_step * ctx_len)
    )
    every = max(1, total_steps // training["checkpoints"])
    optimizer = torch.optim.AdamW(
        model.parameters(), lr=training["lr"], weight_decay=training["weight_decay"]
    )
    scheduler = torch.optim.lr_scheduler.CosineAnnealingLR(
        optimizer, T_max=total_steps, eta_min=training["lr"] * 0.1
    )
    token_grid = [0]
    val_curve = [validate()]
    started = time.monotonic()
    for step in range(1, total_steps + 1):
        x_np, y_np = sample(train_rng, examples_per_step)
        x = torch.from_numpy(x_np).cuda()
        y = torch.from_numpy(y_np).cuda()
        logits = model(x)[:, -1, :]
        loss = torch.nn.functional.cross_entropy(logits, y)
        optimizer.zero_grad(set_to_none=True)
        loss.backward()
        torch.nn.utils.clip_grad_norm_(model.parameters(), training["grad_clip"])
        optimizer.step()
        scheduler.step()
        if step % every == 0 or step == total_steps:
            token_grid.append(step * examples_per_step * ctx_len)
            val_curve.append(validate())
    summary = curve_metrics(token_grid, val_curve, val_curve[0])
    rule_probability = signal_probability + (1.0 - signal_probability) / q
    other_probability = (1.0 - signal_probability) / q
    floor = -rule_probability * math.log2(rule_probability)
    floor -= (q - 1) * other_probability * math.log2(other_probability)
    return {
        "cell_id": cell_id,
        "protocol_hash": protocol["protocol_hash"],
        "configuration": configuration["id"],
        "position_encoding": configuration["position_encoding"],
        "degree": degree,
        "radius": radius,
        "support_lags": list(support),
        "signal_probability": signal_probability,
        "seed": seed,
        "exact_conditional_entropy_bits": floor,
        "token_grid": token_grid,
        "val_ce_bits": val_curve,
        "initial_val_ce_bits": val_curve[0],
        "floor_independent": summary,
        "n_params": model.n_params(),
        "remote": {
            "gpu": torch.cuda.get_device_name(0),
            "torch_version": torch.__version__,
            "wallclock_seconds": time.monotonic() - started,
        },
    }


@app.local_entrypoint()
def main(configuration: str = "all", limit: int = 0) -> None:
    protocol = _load_protocol()
    selected_configurations = [
        row
        for row in CONFIGURATIONS
        if configuration == "all"
        or configuration in {row["id"], row["position_encoding"]}
    ]
    if not selected_configurations:
        raise ValueError(f"unknown configuration selector: {configuration}")
    existing = json.loads(RESULT_PATH.read_text()) if RESULT_PATH.exists() else []
    completed = {row["cell_id"] for row in existing}
    factor = protocol["exact_factorial"]
    cells = [
        {
            "configuration": config,
            "degree": degree,
            "radius": radius,
            "signal_probability": probability,
            "seed": seed,
        }
        for config in selected_configurations
        for degree in factor["degrees"]
        for radius in factor["radii"]
        for probability in factor["signal_probabilities"]
        for seed in protocol["training"]["seeds"]
        if (
            f"V24F/{config['id']}/k{degree}/r{radius}/p{probability:.2f}/s{seed}"
            not in completed
        )
    ]
    if limit:
        cells = cells[:limit]
    print(f"launching {len(cells)} missing exact-factorial H100 cells sequentially")
    for result in train_factorial_cell.map(
        [{"protocol": protocol, **cell} for cell in cells]
    ):
        existing.append(result)
        existing.sort(key=lambda row: row["cell_id"])
        RESULT_PATH.write_text(json.dumps(existing, indent=2))
        summary = result["floor_independent"]
        print(
            f"{result['cell_id']}: final/init="
            f"{summary['final_ce_bits'] / summary['initial_ce_bits']:.6f}",
            flush=True,
        )


if __name__ == "__main__":
    raise SystemExit("run with: modal run scripts/v24_modal_factorial.py")
