"""Single-H100 runner for the frozen v2.3 Transformer robustness grid."""

from __future__ import annotations

import hashlib
import json
from pathlib import Path

import modal

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs/local/v23_transformer_robustness"


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
    protocol = json.loads((ROOT / "configs/protocol_v2.3.json").read_text())
    recorded = protocol["protocol_hash"]
    unhashed = dict(protocol)
    unhashed.pop("protocol_hash")
    expected = hashlib.sha256(json.dumps(unhashed, sort_keys=True).encode()).hexdigest()
    if recorded != expected:
        raise ValueError(f"protocol hash mismatch: {recorded} != {expected}")
    return protocol


app = modal.App("dlx-v23-transformer-robustness")
image = (
    modal.Image.debian_slim(python_version="3.12")
    .pip_install("numpy>=1.26", "torch>=2.3")
    .add_local_dir(ROOT, "/root/pkg", ignore=_ignore)
)
for dataset in (
    "gutenberg_books",
    "reuters_news",
    "brown_balanced",
    "pubmed_abstracts",
    "cpython_source",
    "linux_c_source",
):
    image = image.add_local_file(
        ROOT / f"dlx/data_cache/v21_{dataset}_bytes_n5499984.npy",
        f"/data/{dataset}.npy",
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
    config_id = configuration["id"]
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

    shared = protocol["shared_training"]
    config = TransformerConfig(
        vocab=shared["vocab"],
        ctx_len=shared["ctx_len"],
        d_model=configuration["d_model"],
        n_layers=configuration["n_layers"],
        n_heads=configuration["n_heads"],
        mlp_mult=shared["mlp_mult"],
        dropout=shared["dropout"],
        tie_weights=shared["tie_weights"],
        lr=shared["lr"],
        weight_decay=shared["weight_decay"],
        grad_clip=shared["grad_clip"],
        position_encoding=configuration["position_encoding"],
    )
    family = CorpusFamily(
        tokens,
        q=shared["vocab"],
        L=shared["ctx_len"],
        name=f"{dataset}_stride{stride}",
        floor_bits=unigram_entropy,
        cyclic=True,
        shuffle_seed=seed,
        data_version=data_hash[:16],
    )
    cell_id = f"V23/{config_id}/{dataset}/stride{stride}/s{seed}"
    source_cell_id = f"V21/{dataset}/stride{stride}/s{seed}"
    output = RemotePath("/tmp") / cell_id.replace("/", "__")
    started = time.monotonic()
    metrics = train_run(
        family,
        config,
        budget_tokens=shared["budget_tokens"],
        seed=seed,
        out_dir=output,
        cell_id=cell_id,
        protocol_hash=protocol["protocol_hash"],
        device="cuda",
        n_checkpoints=shared["checkpoints"],
        tokens_per_step=shared["tokens_per_step"],
        record_initial=True,
        data_seed_protocol_hash=protocol["source_protocol_hash"],
        data_seed_key=source_cell_id,
    )
    summary = curve_metrics(
        metrics["token_grid"], metrics["val_ce_bits"], metrics["initial_val_ce_bits"]
    )
    manifest = remote_json.loads((output / "manifest.json").read_text())
    return {
        "cell_id": cell_id,
        "configuration": config_id,
        "position_encoding": configuration["position_encoding"],
        "d_model": configuration["d_model"],
        "n_layers": configuration["n_layers"],
        "n_heads": configuration["n_heads"],
        "config_hash": config.config_hash,
        "n_params": metrics["n_params"],
        "dataset": dataset,
        "stride": stride,
        "seed": seed,
        "protocol_hash": protocol["protocol_hash"],
        "data_seed_protocol_hash": metrics["data_seed_protocol_hash"],
        "data_seed_key": metrics["data_seed_key"],
        "data_sha256": data_hash,
        "byte_counts_sha256": remote_hashlib.sha256(counts.tobytes()).hexdigest(),
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
def main(limit: int = 0) -> None:
    protocol = _load_protocol()
    OUT.mkdir(parents=True, exist_ok=True)
    result_path = OUT / "remote_results.json"
    existing = json.loads(result_path.read_text()) if result_path.exists() else []
    completed = {row["cell_id"] for row in existing}
    configurations = [
        row
        for row in protocol["configurations"]
        if row["source"] == "new v2.3 H100 cells"
    ]
    cells = [
        {
            "configuration": configuration,
            "dataset": dataset["id"],
            "stride": stride,
            "seed": seed,
        }
        for configuration in configurations
        for dataset in protocol["datasets"]
        for stride in protocol["intervention"]["strides"]
        for seed in protocol["replication"]["seeds"]
        if (
            f"V23/{configuration['id']}/{dataset['id']}/stride{stride}/s{seed}"
            not in completed
        )
    ]
    if limit:
        cells = cells[:limit]
    print(f"launching {len(cells)} missing v2.3 H100 cells sequentially")
    for result in train_cell.map(
        [{"protocol": protocol, **cell} for cell in cells]
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
        result_path.write_text(json.dumps(existing, indent=2))
        summary = result["floor_independent"]
        fraction = summary["final_ce_bits"] / summary["initial_ce_bits"]
        print(
            f"{result['cell_id']}: area={summary['normalized_curve_area']:.6f} "
            f"final/init={fraction:.6f} wall={result['remote']['wallclock_seconds']:.1f}s",
            flush=True,
        )


if __name__ == "__main__":
    raise SystemExit("run with: modal run scripts/v23_modal_run.py")
