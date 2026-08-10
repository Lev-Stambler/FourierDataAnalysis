"""Modal profiles and sequential H100 cells for the v2.8 sampling repair."""

from __future__ import annotations

import gzip
import hashlib
import json
import sys
import time
from pathlib import Path

import modal

LOCAL_ROOT = Path(__file__).parent.parent
IS_REMOTE = str(Path(__file__).resolve()).startswith("/root/")
ROOT = Path("/root/pkg") if IS_REMOTE else LOCAL_ROOT
sys.path.insert(0, str(ROOT))

from dlx.protocol.frozen import (
    file_sha256,
    load_frozen_protocol,
    verify_hash_lock,
    write_hash_once,
    write_json_once,
)

OUT = ROOT / "runs/local/v28_random_windows"
PROTOCOL = load_frozen_protocol(ROOT / "configs/protocol_v2.8.json")
MANIFEST = (
    {"corpora": []}
    if IS_REMOTE
    else json.loads((OUT / "data_manifest.json").read_text())
)
CORPORA = MANIFEST["corpora"]


def _ignore(path: Path) -> bool:
    return any(
        part in str(path)
        for part in (
            "__pycache__",
            ".venv",
            ".pytest_cache",
            ".ruff_cache",
            "runs/",
            "dlx/data_cache/",
        )
    )


app = modal.App("dlx-v28-random-windows")
image = (
    modal.Image.debian_slim(python_version="3.12")
    .pip_install("numpy>=1.26", "torch>=2.3")
    .add_local_dir(ROOT, "/root/pkg", ignore=_ignore)
)
if not IS_REMOTE:
    for row in CORPORA:
        image = image.add_local_file(ROOT / row["path"], f"/data/{row['dataset']}.npy")


def _split_seed(protocol: dict, data_hash: str) -> int:
    from dlx.seeding import seed_from

    return int(
        seed_from(
            protocol["protocol_hash"],
            data_hash,
            protocol["sampling"]["split_seed"],
        )
        % (2**32)
    )


@app.function(
    image=image,
    cpu=4.0,
    memory=16384,
    timeout=3600,
    max_containers=8,
    block_network=True,
)
def profile_cell(payload: dict) -> dict:
    import numpy as np

    from dlx.data.random_windows import RandomWindowCorpus
    from dlx.profiles.sampled_degree import (
        marginal_locality_features,
        sampled_nested_degree_profile,
    )
    from dlx.seeding import seed_from

    protocol = payload["protocol"]
    row = payload["corpus"]
    values = np.load(f"/data/{row['dataset']}.npy", mmap_mode="r")
    sampling = protocol["sampling"]
    spec = protocol["profile"]
    corpus = RandomWindowCorpus(
        values,
        ctx_len=max(spec["lags"]),
        block_size=sampling["block_size"],
        split_seed=_split_seed(protocol, row["byte_stream_sha256"]),
        split_fractions=tuple(sampling["split_fractions"]),
    )
    rng = np.random.default_rng(
        seed_from(
            protocol["protocol_hash"],
            row["byte_stream_sha256"],
            "profile_positions",
            spec["seed"],
        )
    )
    starts = corpus.sample_starts("profile", spec["positions"], rng, replace=False)
    targets = starts + max(spec["lags"])
    contexts = np.column_stack([values[targets - lag] for lag in spec["lags"]])
    started = time.monotonic()
    profile = sampled_nested_degree_profile(
        contexts,
        values[targets],
        max_degree=spec["max_degree"],
        n_chains=spec["chains"],
        seed=spec["seed"],
        delta=spec["delta"],
        q=spec["q"],
        coordinate_radii=tuple(spec["lags"]),
        include_chains=True,
    )
    marginal = marginal_locality_features(
        profile["chains"], tuple(spec["lags"]), feature_degree=3
    )
    return {
        "dataset": row["dataset"],
        "panel": row["panel"],
        "protocol_hash": protocol["protocol_hash"],
        "data_sha256": row["byte_stream_sha256"],
        "split": corpus.summary(),
        "sampled_position_sha256": hashlib.sha256(starts.tobytes()).hexdigest(),
        "features": {**profile["sampled_features"], **marginal},
        "remote_wallclock_seconds": time.monotonic() - started,
        **profile,
    }


@app.function(
    image=image,
    gpu="H100",
    cpu=8.0,
    memory=16384,
    timeout=1800,
    max_containers=1,
    block_network=True,
)
def train_cell(payload: dict) -> dict:
    import numpy as np
    import torch

    from dlx.analysis.floor_independent import curve_metrics
    from dlx.data.random_windows import RandomWindowCorpus
    from dlx.learners.transformer import (
        CausalTransformer,
        TransformerConfig,
        cross_entropy_bits,
    )
    from dlx.seeding import seed_from

    protocol = payload["protocol"]
    row = payload["corpus"]
    seed = int(payload["seed"])
    values = np.load(f"/data/{row['dataset']}.npy", mmap_mode="r")
    spec = protocol["training"]
    corpus = RandomWindowCorpus(
        values,
        ctx_len=spec["ctx_len"],
        block_size=protocol["sampling"]["block_size"],
        split_seed=_split_seed(protocol, row["byte_stream_sha256"]),
        split_fractions=tuple(protocol["sampling"]["split_fractions"]),
    )
    config = TransformerConfig(
        vocab=spec["vocab"],
        ctx_len=spec["ctx_len"],
        d_model=spec["d_model"],
        n_layers=spec["n_layers"],
        n_heads=spec["n_heads"],
        mlp_mult=spec["mlp_mult"],
        dropout=spec["dropout"],
        tie_weights=spec["tie_weights"],
        lr=spec["lr"],
        weight_decay=spec["weight_decay"],
        grad_clip=spec["grad_clip"],
        position_encoding=spec["position_encoding"],
    )
    np.random.seed(seed)
    torch.manual_seed(seed)
    device = torch.device("cuda")
    model = CausalTransformer(config).to(device)
    train_rng = np.random.default_rng(
        seed_from(protocol["protocol_hash"], row["dataset"], seed, "train_windows")
    )
    val_rng = np.random.default_rng(
        seed_from(protocol["protocol_hash"], row["dataset"], "fixed_validation")
    )
    val_starts = corpus.sample_starts(
        "validation", spec["validation_windows"], val_rng, replace=False
    )

    @torch.no_grad()
    def validate() -> float:
        model.eval()
        total = 0.0
        batch_size = 64
        for start in range(0, len(val_starts), batch_size):
            x, y = corpus.batch(val_starts[start : start + batch_size])
            logits = model(torch.from_numpy(x).to(device))
            total += cross_entropy_bits(logits, torch.from_numpy(y).to(device)) * len(x)
        model.train()
        return total / len(val_starts)

    batch = spec["tokens_per_step"] // spec["ctx_len"]
    steps = spec["budget_tokens"] // spec["tokens_per_step"]
    every = max(1, steps // spec["checkpoints"])
    optimizer = torch.optim.AdamW(
        model.parameters(), lr=config.lr, weight_decay=config.weight_decay
    )
    scheduler = torch.optim.lr_scheduler.CosineAnnealingLR(
        optimizer, T_max=steps, eta_min=config.lr * 0.1
    )
    token_grid = [0]
    curve = [validate()]
    started = time.monotonic()
    for step in range(1, steps + 1):
        starts = corpus.sample_starts("train", batch, train_rng, replace=True)
        x, y = corpus.batch(starts)
        logits = model(torch.from_numpy(x).to(device))
        loss = torch.nn.functional.cross_entropy(
            logits.reshape(-1, config.vocab), torch.from_numpy(y).reshape(-1).to(device)
        )
        optimizer.zero_grad(set_to_none=True)
        loss.backward()
        torch.nn.utils.clip_grad_norm_(model.parameters(), config.grad_clip)
        optimizer.step()
        scheduler.step()
        if step % every == 0 or step == steps:
            token_grid.append(step * spec["tokens_per_step"])
            curve.append(validate())
    summary = curve_metrics(token_grid, curve, curve[0])
    cell_id = f"V28{row['panel'][0].upper()}/{row['dataset']}/s{seed}"
    return {
        "cell_id": cell_id,
        "dataset": row["dataset"],
        "panel": row["panel"],
        "seed": seed,
        "configuration": "random_windows_learned_absolute_d64_l2",
        "protocol_hash": protocol["protocol_hash"],
        "data_manifest_hash": payload["data_manifest_hash"],
        "profile_manifest_hash": payload["profile_manifest_hash"],
        "prediction_lock_hash": payload.get("prediction_lock_hash"),
        "data_sha256": row["byte_stream_sha256"],
        "config_hash": config.config_hash,
        "validation_starts_sha256": hashlib.sha256(val_starts.tobytes()).hexdigest(),
        "token_grid": token_grid,
        "val_ce_bits": curve,
        "initial_val_ce_bits": curve[0],
        "floor_independent": summary,
        "remote": {
            "gpu": torch.cuda.get_device_name(0),
            "wallclock_seconds": time.monotonic() - started,
        },
    }


def _save_result(path: Path, row: dict) -> None:
    existing = json.loads(path.read_text()) if path.exists() else []
    if row["cell_id"] not in {item["cell_id"] for item in existing}:
        existing.append(row)
        path.write_text(
            json.dumps(sorted(existing, key=lambda item: item["cell_id"]), indent=2)
            + "\n"
        )


@app.local_entrypoint()
def main(stage: str = "profile", limit: int = 0) -> None:
    data_hash = verify_hash_lock(
        OUT / "data_manifest.json", OUT / "data_manifest.sha256"
    )
    if stage == "profile":
        records = []
        for result in profile_cell.map(
            [{"protocol": PROTOCOL, "corpus": row} for row in CORPORA]
        ):
            chains = result.pop("chains")
            path = OUT / "profiles" / f"{result['dataset']}.json"
            write_json_once(path, result)
            audit = {
                "dataset": result["dataset"],
                "summary_sha256": file_sha256(path),
                "chains": chains,
            }
            audit_path = OUT / "audit_chains" / f"{result['dataset']}.json.gz"
            audit_path.parent.mkdir(parents=True, exist_ok=True)
            audit_path.write_bytes(
                gzip.compress(
                    json.dumps(audit, separators=(",", ":")).encode(), mtime=0
                )
            )
            records.append(
                {
                    "dataset": result["dataset"],
                    "profile_sha256": file_sha256(path),
                    "audit_sha256": file_sha256(audit_path),
                }
            )
            print(
                result["dataset"],
                result["features"][PROTOCOL["profile"]["primary_feature"]],
                flush=True,
            )
        manifest = {
            "protocol_hash": PROTOCOL["protocol_hash"],
            "data_manifest_hash": data_hash,
            "profiles": sorted(records, key=lambda row: row["dataset"]),
        }
        digest = write_json_once(OUT / "profile_manifest.json", manifest)
        write_hash_once(OUT / "profile_manifest.sha256", digest)
        return
    if stage not in {"development", "confirmation"}:
        raise ValueError("stage must be profile, development, or confirmation")
    profile_hash = verify_hash_lock(
        OUT / "profile_manifest.json", OUT / "profile_manifest.sha256"
    )
    prediction_hash = None
    if stage == "confirmation":
        prediction_hash = verify_hash_lock(
            OUT / "predictions.json", OUT / "predictions.sha256"
        )
    result_path = OUT / f"{stage}_results.json"
    existing = json.loads(result_path.read_text()) if result_path.exists() else []
    completed = {row["cell_id"] for row in existing}
    cells = [
        (row, seed)
        for row in CORPORA
        if row["panel"] == stage
        for seed in PROTOCOL["training"]["seeds"]
    ]
    if limit:
        cells = cells[:limit]
    for row, seed in cells:
        cell_id = f"V28{stage[0].upper()}/{row['dataset']}/s{seed}"
        if cell_id in completed:
            continue
        result = train_cell.remote(
            {
                "protocol": PROTOCOL,
                "corpus": row,
                "seed": seed,
                "data_manifest_hash": data_hash,
                "profile_manifest_hash": profile_hash,
                "prediction_lock_hash": prediction_hash,
            }
        )
        _save_result(result_path, result)
        print(
            cell_id,
            result["floor_independent"]["final_ce_bits"]
            / result["floor_independent"]["initial_ce_bits"],
            flush=True,
        )
