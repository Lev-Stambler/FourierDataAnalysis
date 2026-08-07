"""Frozen v2.4 confirmatory profiler and sequential single-H100 runner."""

from __future__ import annotations

import hashlib
import json
from pathlib import Path

import modal

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs/local/v24_spectrum_predictor"
PROFILE_DIR = OUT / "confirmatory_profiles"
RESULT_PATH = OUT / "confirmatory_remote_results.json"
PREDICTION_PATH = OUT / "confirmatory_predictions.json"
PREDICTION_HASH_PATH = OUT / "confirmatory_predictions.sha256"
MANIFEST_HASH_PATH = OUT / "confirmatory_data_manifest.sha256"

DATASETS = (
    "mdn_docs",
    "kubernetes_docs",
    "rust_reference_docs",
    "rfc_legacy",
    "pubmed_independent",
    "go_source",
    "typescript_source",
    "llvm_cpp_source",
    "coq_source",
    "julia_source",
    "ghc_haskell_source",
    "postgresql_source",
)
INTERVENTION_DATASETS = (
    "mdn_docs",
    "rfc_legacy",
    "pubmed_independent",
    "go_source",
    "llvm_cpp_source",
    "coq_source",
)
CONFIGURATIONS = (
    {
        "id": "learned_absolute_d64_l2",
        "position_encoding": "learned_absolute",
    },
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


def _locked_prediction_hash() -> str:
    if not PREDICTION_PATH.exists() or not PREDICTION_HASH_PATH.exists():
        raise FileNotFoundError("confirmatory predictions must be frozen before training")
    actual = hashlib.sha256(PREDICTION_PATH.read_bytes()).hexdigest()
    recorded = PREDICTION_HASH_PATH.read_text().strip()
    if actual != recorded:
        raise ValueError(f"prediction lock mismatch: {actual} != {recorded}")
    return actual


app = modal.App("dlx-v24-confirmatory")
image = (
    modal.Image.debian_slim(python_version="3.12")
    .pip_install("numpy>=1.26", "torch>=2.3")
    .add_local_dir(ROOT, "/root/pkg", ignore=_ignore)
)
for dataset in DATASETS:
    image = image.add_local_file(
        ROOT / f"dlx/data_cache/v24_{dataset}_bytes_n2000000.npy",
        f"/data/{dataset}.npy",
    )


@app.function(
    image=image,
    cpu=4.0,
    memory=8192,
    timeout=1800,
    retries=0,
    max_containers=12,
    block_network=True,
)
def profile_cell(payload: dict) -> dict:
    import hashlib as remote_hashlib
    import sys
    import time

    import numpy as np

    sys.path.insert(0, "/root/pkg")
    from dlx.profiles.locality_surface import resolved_locality_profile

    protocol = payload["protocol"]
    dataset = payload["dataset"]
    stride = int(payload["stride"])
    original = np.load(f"/data/{dataset}.npy", mmap_mode="r")
    tokens = np.ascontiguousarray(
        np.asarray(original, dtype=np.uint8).reshape(stride, -1).T.reshape(-1)
    )
    spec = protocol["text_spectrum"]
    n_positions = int(spec["profile_positions"])
    n_times = n_positions // stride
    first_time = int(np.ceil(max(spec["lags"]) / stride)) + 1
    sampled_times = np.linspace(
        first_time, len(tokens) // stride - 1, num=n_times, dtype=np.int64
    )
    positions = (
        sampled_times[:, None] * stride + np.arange(stride)[None, :]
    ).reshape(-1)
    fold_ids = np.repeat(np.arange(n_times, dtype=np.int64) & 1, stride)
    started = time.monotonic()
    profile = resolved_locality_profile(
        tokens,
        q=spec["q"],
        lags=tuple(spec["lags"]),
        radii=tuple(spec["radii"]),
        max_tokens=n_positions,
        smoothing=spec["smoothing"],
        positions=positions,
        fold_ids=fold_ids,
    )
    return {
        "cell_id": f"V24CP/{dataset}/stride{stride}",
        "protocol_hash": protocol["protocol_hash"],
        "data_manifest_hash": payload["data_manifest_hash"],
        "dataset": dataset,
        "stride": stride,
        "data_sha256": remote_hashlib.sha256(tokens.tobytes()).hexdigest(),
        "position_sha256": remote_hashlib.sha256(positions.tobytes()).hexdigest(),
        "remote": {
            "compute": "Modal CPU",
            "wallclock_seconds": time.monotonic() - started,
        },
        **profile,
    }


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
def train_cell(payload: dict) -> dict:
    import hashlib as remote_hashlib
    import json as remote_json
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
    configuration = payload["configuration"]
    dataset = payload["dataset"]
    stride = int(payload["stride"])
    seed = int(payload["seed"])
    torch.set_num_threads(8)
    original = np.load(f"/data/{dataset}.npy", mmap_mode="r")
    tokens = np.ascontiguousarray(
        np.asarray(original, dtype=np.uint8).reshape(stride, -1).T.reshape(-1)
    )
    counts = np.bincount(tokens.astype(np.int64), minlength=256)
    probabilities = counts[counts > 0].astype(float) / counts.sum()
    unigram_entropy = float(-(probabilities * np.log2(probabilities)).sum())
    data_hash = remote_hashlib.sha256(tokens.tobytes()).hexdigest()
    spec = protocol["training"]
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
        position_encoding=configuration["position_encoding"],
    )
    family = CorpusFamily(
        tokens,
        q=spec["vocab"],
        L=spec["ctx_len"],
        name=f"{dataset}_stride{stride}",
        floor_bits=unigram_entropy,
        cyclic=True,
        shuffle_seed=seed,
        data_version=data_hash[:16],
    )
    cell_id = f"V24C/{configuration['id']}/{dataset}/stride{stride}/s{seed}"
    output = RemotePath("/tmp") / cell_id.replace("/", "__")
    started = time.monotonic()
    metrics = train_run(
        family,
        config,
        budget_tokens=spec["budget_tokens"],
        seed=seed,
        out_dir=output,
        cell_id=cell_id,
        protocol_hash=protocol["protocol_hash"],
        device="cuda",
        n_checkpoints=spec["checkpoints"],
        tokens_per_step=spec["tokens_per_step"],
        record_initial=True,
    )
    summary = curve_metrics(
        metrics["token_grid"], metrics["val_ce_bits"], metrics["initial_val_ce_bits"]
    )
    manifest = remote_json.loads((output / "manifest.json").read_text())
    return {
        "cell_id": cell_id,
        "configuration": configuration["id"],
        "position_encoding": configuration["position_encoding"],
        "dataset": dataset,
        "stride": stride,
        "seed": seed,
        "protocol_hash": protocol["protocol_hash"],
        "data_manifest_hash": payload["data_manifest_hash"],
        "prediction_lock_hash": payload["prediction_lock_hash"],
        "data_sha256": data_hash,
        "byte_counts_sha256": remote_hashlib.sha256(counts.tobytes()).hexdigest(),
        "config_hash": config.config_hash,
        "n_params": metrics["n_params"],
        "n_bytes": len(tokens),
        "unigram_entropy_bits": unigram_entropy,
        "token_grid": metrics["token_grid"],
        "val_ce_bits": metrics["val_ce_bits"],
        "initial_val_ce_bits": metrics["initial_val_ce_bits"],
        "floor_independent": summary,
        "remote": {
            "gpu": torch.cuda.get_device_name(0),
            "torch_version": torch.__version__,
            "wallclock_seconds": time.monotonic() - started,
            "manifest_wallclock_seconds": manifest["wallclock_seconds"],
        },
    }


@app.local_entrypoint()
def main(stage: str = "profile", configuration: str = "all", limit: int = 0) -> None:
    if stage not in {"profile", "train"}:
        raise ValueError("stage must be profile or train")
    protocol = _load_protocol()
    data_manifest_hash = MANIFEST_HASH_PATH.read_text().strip()
    strides_by_dataset = {
        dataset: ([1, 4, 8, 16] if dataset in INTERVENTION_DATASETS else [1])
        for dataset in DATASETS
    }
    if stage == "profile":
        PROFILE_DIR.mkdir(parents=True, exist_ok=True)
        cells = [
            {"dataset": dataset, "stride": stride}
            for dataset in DATASETS
            for stride in strides_by_dataset[dataset]
            if not (PROFILE_DIR / f"{dataset}__stride{stride}.json").exists()
        ]
        if limit:
            cells = cells[:limit]
        print(f"launching {len(cells)} missing confirmatory profiles")
        for result in profile_cell.map(
            [
                {
                    "protocol": protocol,
                    "data_manifest_hash": data_manifest_hash,
                    **cell,
                }
                for cell in cells
            ]
        ):
            path = PROFILE_DIR / f"{result['dataset']}__stride{result['stride']}.json"
            path.write_text(json.dumps(result, indent=2))
            print(
                f"{result['cell_id']}: lambda="
                f"{result['features']['energy_weighted_log_radius']:.5f}",
                flush=True,
            )
        return

    prediction_lock_hash = _locked_prediction_hash()
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
    cells = [
        {
            "configuration": config,
            "dataset": dataset,
            "stride": stride,
            "seed": seed,
        }
        for config in selected_configurations
        for dataset in DATASETS
        for stride in strides_by_dataset[dataset]
        for seed in protocol["training"]["seeds"]
        if f"V24C/{config['id']}/{dataset}/stride{stride}/s{seed}" not in completed
    ]
    if limit:
        cells = cells[:limit]
    print(f"launching {len(cells)} missing confirmatory H100 cells sequentially")
    for result in train_cell.map(
        [
            {
                "protocol": protocol,
                "data_manifest_hash": data_manifest_hash,
                "prediction_lock_hash": prediction_lock_hash,
                **cell,
            }
            for cell in cells
        ]
    ):
        existing.append(result)
        existing.sort(
            key=lambda row: (
                row["configuration"],
                row["dataset"],
                row["stride"],
                row["seed"],
            )
        )
        RESULT_PATH.write_text(json.dumps(existing, indent=2))
        summary = result["floor_independent"]
        print(
            f"{result['cell_id']}: final/init="
            f"{summary['final_ce_bits'] / summary['initial_ce_bits']:.6f} "
            f"area={summary['normalized_curve_area']:.6f}",
            flush=True,
        )


if __name__ == "__main__":
    raise SystemExit("run with: modal run scripts/v24_modal_confirm.py")
