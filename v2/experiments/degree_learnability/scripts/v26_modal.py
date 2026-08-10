"""Modal CPU profiling and sequential single-H100 v2.6 training."""

from __future__ import annotations

import gzip
import json
import sys
from pathlib import Path

import modal

LOCAL_ROOT = Path(__file__).parent.parent
ROOT = (
    Path("/root/pkg")
    if str(Path(__file__).resolve()).startswith("/root/")
    else LOCAL_ROOT
)
sys.path.insert(0, str(ROOT))

from dlx.protocol.frozen import (
    file_sha256,
    load_frozen_protocol,
    verify_hash_lock,
    write_hash_once,
    write_json_once,
)

PROTOCOL_PATH = ROOT / "configs/protocol_v2.6.json"
OUT = ROOT / "runs/local/v26_sampled_locality"
PROFILE_DIR = OUT / "profiles"
AUDIT_DIR = OUT / "audit_chains"
RESULT_PATH = OUT / "remote_results.json"


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


PROTOCOL = load_frozen_protocol(PROTOCOL_PATH)
SOURCES = PROTOCOL["corpora"]["sources"]
N_BYTES = int(PROTOCOL["corpora"]["bytes_per_corpus"])

app = modal.App("dlx-v26-sampled-locality")
image = (
    modal.Image.debian_slim(python_version="3.12")
    .pip_install("numpy>=1.26", "torch>=2.3")
    .add_local_dir(ROOT, "/root/pkg", ignore=_ignore)
)
for source in SOURCES:
    image = image.add_local_file(
        ROOT / f"dlx/data_cache/v26_{source['id']}_bytes_n{N_BYTES}.npy",
        f"/data/{source['id']}.npy",
    )


@app.function(
    image=image,
    cpu=4.0,
    memory=16384,
    timeout=3600,
    retries=0,
    max_containers=8,
    block_network=True,
)
def profile_cell(payload: dict) -> dict:
    import hashlib as remote_hashlib
    import sys as remote_sys
    import time

    import numpy as np

    remote_sys.path.insert(0, "/root/pkg")
    from dlx.profiles.sampled_degree import sampled_token_degree_profile
    from dlx.profiles.simple_controls import simple_text_controls

    dataset = payload["dataset"]
    protocol = payload["protocol"]
    tokens = np.load(f"/data/{dataset}.npy", mmap_mode="r")
    spec = protocol["profile"]
    started = time.monotonic()
    profile = sampled_token_degree_profile(
        tokens,
        lags=tuple(spec["lags"]),
        max_degree=spec["max_degree"],
        n_chains=spec["chains"],
        max_positions=spec["positions"],
        seed=spec["seed"],
        delta=spec["delta"],
        include_chains=True,
        include_product_reference=False,
    )
    controls = simple_text_controls(tokens, q=256, max_tokens=1_000_000)
    return {
        "cell_id": f"V26P/{dataset}",
        "dataset": dataset,
        "protocol_hash": protocol["protocol_hash"],
        "data_manifest_hash": payload["data_manifest_hash"],
        "data_sha256": remote_hashlib.sha256(np.asarray(tokens).tobytes()).hexdigest(),
        "features": {**profile["sampled_features"], **controls},
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
    import sys as remote_sys
    import time
    from pathlib import Path as RemotePath

    import numpy as np
    import torch

    remote_sys.path.insert(0, "/root/pkg")
    from dlx.analysis.floor_independent import curve_metrics
    from dlx.data.corpus_family import CorpusFamily
    from dlx.learners.transformer import TransformerConfig
    from dlx.training.run import train_run

    protocol = payload["protocol"]
    dataset = payload["dataset"]
    seed = int(payload["seed"])
    tokens = np.load(f"/data/{dataset}.npy", mmap_mode="r")
    data_hash = remote_hashlib.sha256(np.asarray(tokens).tobytes()).hexdigest()
    counts = np.bincount(np.asarray(tokens).astype(np.int64), minlength=256)
    probabilities = counts[counts > 0].astype(float) / counts.sum()
    entropy = float(-np.dot(probabilities, np.log2(probabilities)))
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
        position_encoding=spec["position_encoding"],
    )
    family = CorpusFamily(
        tokens,
        q=256,
        L=spec["ctx_len"],
        name=dataset,
        floor_bits=entropy,
        cyclic=True,
        shuffle_seed=seed,
        data_version=data_hash[:16],
    )
    cell_id = f"V26/{dataset}/s{seed}"
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
        "dataset": dataset,
        "seed": seed,
        "configuration": "learned_absolute_d64_l2",
        "protocol_hash": protocol["protocol_hash"],
        "data_manifest_hash": payload["data_manifest_hash"],
        "profile_manifest_hash": payload["profile_manifest_hash"],
        "prediction_lock_hash": payload["prediction_lock_hash"],
        "data_sha256": data_hash,
        "config_hash": config.config_hash,
        "n_params": metrics["n_params"],
        "n_bytes": len(tokens),
        "unigram_entropy_bits": entropy,
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


def _freeze_profile_manifest(protocol: dict, data_hash: str) -> None:
    records = []
    for source in SOURCES:
        profile = PROFILE_DIR / f"{source['id']}.json"
        audit = AUDIT_DIR / f"{source['id']}.json.gz"
        if not profile.exists() or not audit.exists():
            return
        records.append(
            {
                "dataset": source["id"],
                "profile_sha256": file_sha256(profile),
                "audit_sha256": file_sha256(audit),
            }
        )
    manifest = {
        "protocol_hash": protocol["protocol_hash"],
        "data_manifest_hash": data_hash,
        "profiles": records,
    }
    digest = write_json_once(OUT / "profile_manifest.json", manifest)
    write_hash_once(OUT / "profile_manifest.sha256", digest)
    print(f"froze profile manifest: {digest}")


@app.local_entrypoint()
def main(stage: str = "profile", limit: int = 0) -> None:
    if stage not in {"profile", "train"}:
        raise ValueError("stage must be profile or train")
    protocol = load_frozen_protocol(PROTOCOL_PATH)
    data_hash = verify_hash_lock(
        OUT / "data_manifest.json", OUT / "data_manifest.sha256"
    )
    if stage == "profile":
        PROFILE_DIR.mkdir(parents=True, exist_ok=True)
        AUDIT_DIR.mkdir(parents=True, exist_ok=True)
        cells = []
        for source in SOURCES:
            summary_exists = (PROFILE_DIR / f"{source['id']}.json").exists()
            audit_exists = (AUDIT_DIR / f"{source['id']}.json.gz").exists()
            if summary_exists != audit_exists:
                raise FileExistsError(f"partial profile state for {source['id']}")
            if not summary_exists:
                cells.append(source["id"])
        if limit:
            cells = cells[:limit]
        print(f"launching {len(cells)} missing CPU profiles")
        for result in profile_cell.map(
            [
                {
                    "dataset": dataset,
                    "protocol": protocol,
                    "data_manifest_hash": data_hash,
                }
                for dataset in cells
            ]
        ):
            chains = result.pop("chains")
            dataset = result["dataset"]
            summary_path = PROFILE_DIR / f"{dataset}.json"
            write_json_once(summary_path, result)
            audit_payload = json.dumps(
                {
                    "dataset": dataset,
                    "data_sha256": result["data_sha256"],
                    "summary_sha256": file_sha256(summary_path),
                    "chains": chains,
                },
                separators=(",", ":"),
            ).encode()
            (AUDIT_DIR / f"{dataset}.json.gz").write_bytes(
                gzip.compress(audit_payload, compresslevel=9, mtime=0)
            )
            print(
                f"{dataset}: G={result['features']['sampled_geometric_complexity_through_degree3']:.6f}"
            )
        _freeze_profile_manifest(protocol, data_hash)
        return

    profile_hash = verify_hash_lock(
        OUT / "profile_manifest.json", OUT / "profile_manifest.sha256"
    )
    prediction_hash = verify_hash_lock(
        OUT / "predictions.json", OUT / "predictions.sha256"
    )
    existing = json.loads(RESULT_PATH.read_text()) if RESULT_PATH.exists() else []
    completed = {row["cell_id"] for row in existing}
    cells = [
        {"dataset": source["id"], "seed": seed}
        for source in SOURCES
        for seed in protocol["training"]["seeds"]
        if f"V26/{source['id']}/s{seed}" not in completed
    ]
    if limit:
        cells = cells[:limit]
    print(f"launching {len(cells)} missing H100 cells sequentially")
    for cell in cells:
        result = train_cell.remote(
            {
                **cell,
                "protocol": protocol,
                "data_manifest_hash": data_hash,
                "profile_manifest_hash": profile_hash,
                "prediction_lock_hash": prediction_hash,
            }
        )
        existing.append(result)
        existing.sort(key=lambda row: (row["dataset"], row["seed"]))
        RESULT_PATH.write_text(json.dumps(existing, indent=2) + "\n")
        print(
            f"{result['cell_id']}: final/init={result['floor_independent']['final_ce_bits'] / result['floor_independent']['initial_ce_bits']:.6f}"
        )


if __name__ == "__main__":
    raise SystemExit("run with: modal run scripts/v26_modal.py")
