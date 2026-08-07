"""Corrected contiguous-code image learning curves for the v2.4 image panel."""

from __future__ import annotations

import hashlib
import json
from pathlib import Path

import modal

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs/local/v24_spectrum_predictor"
RESULT_PATH = OUT / "image_remote_results.json"
DATASETS = (
    "gaussian_noise_control",
    "MNIST",
    "FashionMNIST",
    "SVHN",
    "CIFAR10",
    "STL10_downsampled",
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


app = modal.App("dlx-v24-image-panel")
image = (
    modal.Image.debian_slim(python_version="3.12")
    .pip_install("numpy>=1.26", "torch>=2.3")
    .add_local_dir(ROOT, "/root/pkg", ignore=_ignore)
)
for dataset in DATASETS:
    image = image.add_local_file(
        ROOT / f"dlx/data_cache/images/{dataset}_codes.npy",
        f"/data/{dataset}_codes.npy",
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
def train_image_cell(payload: dict) -> dict:
    import hashlib as remote_hashlib
    import sys
    import time
    from pathlib import Path as RemotePath

    import numpy as np
    import torch

    sys.path.insert(0, "/root/pkg")
    from dlx.analysis.floor_independent import curve_metrics
    from dlx.data.corpus_family import CorpusFamily
    from dlx.learners.transformer import TransformerConfig
    from dlx.training.run import train_run

    protocol = payload["protocol"]
    dataset = payload["dataset"]
    seed = int(payload["seed"])
    codes = np.load(f"/data/{dataset}_codes.npy", mmap_mode="r")
    counts = np.bincount(codes.astype(np.int64), minlength=512)
    probabilities = counts[counts > 0].astype(float) / counts.sum()
    entropy = float(-(probabilities * np.log2(probabilities)).sum())
    data_hash = remote_hashlib.sha256(np.ascontiguousarray(codes).tobytes()).hexdigest()
    config = TransformerConfig(
        vocab=512,
        ctx_len=16,
        d_model=64,
        n_layers=2,
        n_heads=4,
        mlp_mult=4,
        dropout=0.0,
        tie_weights=True,
        lr=0.001,
        weight_decay=0.1,
        grad_clip=1.0,
        position_encoding="learned_absolute",
    )
    family = CorpusFamily(
        codes,
        q=512,
        L=16,
        name=dataset,
        floor_bits=entropy,
        cyclic=True,
        shuffle_seed=seed,
        data_version=data_hash[:16],
    )
    cell_id = f"V24I/{dataset}/s{seed}"
    output = RemotePath("/tmp") / cell_id.replace("/", "__")
    started = time.monotonic()
    metrics = train_run(
        family,
        config,
        budget_tokens=protocol["training"]["budget_tokens"],
        seed=seed,
        out_dir=output,
        cell_id=cell_id,
        protocol_hash=protocol["protocol_hash"],
        device="cuda",
        n_checkpoints=protocol["training"]["checkpoints"],
        tokens_per_step=protocol["training"]["tokens_per_step"],
        record_initial=True,
    )
    summary = curve_metrics(
        metrics["token_grid"], metrics["val_ce_bits"], metrics["initial_val_ce_bits"]
    )
    return {
        "cell_id": cell_id,
        "protocol_hash": protocol["protocol_hash"],
        "dataset": dataset,
        "seed": seed,
        "sequence_semantics": "contiguous raster-order VQ codes; old shuffled M8 curves not reused",
        "data_sha256": data_hash,
        "n_codes": len(codes),
        "marginal_code_entropy_bits": entropy,
        "config_hash": config.config_hash,
        "n_params": metrics["n_params"],
        "token_grid": metrics["token_grid"],
        "val_ce_bits": metrics["val_ce_bits"],
        "initial_val_ce_bits": metrics["initial_val_ce_bits"],
        "floor_independent": summary,
        "remote": {
            "gpu": torch.cuda.get_device_name(0),
            "torch_version": torch.__version__,
            "wallclock_seconds": time.monotonic() - started,
        },
    }


@app.local_entrypoint()
def main(limit: int = 0) -> None:
    protocol = _load_protocol()
    existing = json.loads(RESULT_PATH.read_text()) if RESULT_PATH.exists() else []
    completed = {row["cell_id"] for row in existing}
    cells = [
        {"dataset": dataset, "seed": seed}
        for dataset in DATASETS
        for seed in protocol["training"]["seeds"]
        if f"V24I/{dataset}/s{seed}" not in completed
    ]
    if limit:
        cells = cells[:limit]
    print(f"launching {len(cells)} missing corrected image H100 cells sequentially")
    for result in train_image_cell.map(
        [{"protocol": protocol, **cell} for cell in cells]
    ):
        existing.append(result)
        existing.sort(key=lambda row: (row["dataset"], row["seed"]))
        RESULT_PATH.write_text(json.dumps(existing, indent=2))
        summary = result["floor_independent"]
        print(
            f"{result['cell_id']}: final/init="
            f"{summary['final_ce_bits'] / summary['initial_ce_bits']:.6f} "
            f"area={summary['normalized_curve_area']:.6f}",
            flush=True,
        )


if __name__ == "__main__":
    raise SystemExit("run with: modal run scripts/v24_modal_image.py")
