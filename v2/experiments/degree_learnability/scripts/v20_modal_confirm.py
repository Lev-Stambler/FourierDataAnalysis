"""Modal A10G runner for the frozen v2.0 multi-corpus confirmation."""

from __future__ import annotations

import hashlib
import json
from pathlib import Path

import modal

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs/local/v20_confirmatory_geometry"


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
    protocol = json.loads((ROOT / "configs/protocol_v2.0.json").read_text())
    recorded = protocol["protocol_hash"]
    unhashed = dict(protocol)
    unhashed.pop("protocol_hash")
    expected = hashlib.sha256(json.dumps(unhashed, sort_keys=True).encode()).hexdigest()
    if recorded != expected:
        raise ValueError(f"protocol hash mismatch: {recorded} != {expected}")
    return protocol


DATA_FILES = {
    "enwik8": ROOT / "dlx/data_cache/v19_enwik8_bytes_n5500000.npy",
    "tinystories": ROOT / "dlx/data_cache/h5_tinystories_bytes_n5500000.npy",
    "wikitext2": ROOT / "dlx/data_cache/v18_wikitext2_bytes_n5500000.npy",
    "codeparrot_python": ROOT
    / "dlx/data_cache/v18_codeparrot_python_bytes_n5500000.npy",
}

app = modal.App("dlx-v20-confirmatory-geometry")
image = (
    modal.Image.debian_slim(python_version="3.12")
    .pip_install("numpy>=1.26", "torch>=2.3")
    .add_local_dir(ROOT, "/root/pkg", ignore=_ignore)
)
for dataset, path in DATA_FILES.items():
    image = image.add_local_file(path, f"/data/{dataset}.npy")


@app.function(image=image, gpu="A10G", timeout=1800, retries=0, cpu=2.0, memory=4096)
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
    dataset = payload["dataset"]
    stride = int(payload["stride"])
    seed = int(payload["seed"])
    torch.set_num_threads(2)

    original = np.load(f"/data/{dataset}.npy", mmap_mode="r")
    if len(original) % stride:
        raise ValueError("frozen corpus length must divide evenly by stride")
    lanes = np.asarray(original, dtype=np.uint8).reshape(stride, -1)
    tokens = np.ascontiguousarray(lanes.T.reshape(-1))
    counts = np.bincount(tokens.astype(np.int64), minlength=256)
    probabilities = counts[counts > 0].astype(float) / counts.sum()
    unigram_entropy = float(-(probabilities * np.log2(probabilities)).sum())
    data_hash = remote_hashlib.sha256(tokens.tobytes()).hexdigest()
    count_hash = remote_hashlib.sha256(counts.tobytes()).hexdigest()

    learner = protocol["learner"]
    config = TransformerConfig(
        **{
            key: learner[key]
            for key in (
                "vocab",
                "ctx_len",
                "d_model",
                "n_layers",
                "n_heads",
                "mlp_mult",
                "dropout",
                "tie_weights",
                "lr",
                "weight_decay",
                "grad_clip",
            )
        }
    )
    family = CorpusFamily(
        tokens,
        q=256,
        L=learner["ctx_len"],
        name=f"{dataset}_stride{stride}",
        floor_bits=unigram_entropy,
        cyclic=True,
        shuffle_seed=seed,
        data_version=data_hash[:16],
    )
    cell_id = f"V20/{dataset}/stride{stride}/s{seed}"
    output = RemotePath("/tmp") / cell_id.replace("/", "__")
    started = time.monotonic()
    metrics = train_run(
        family,
        config,
        budget_tokens=learner["budget_tokens"],
        seed=seed,
        out_dir=output,
        cell_id=cell_id,
        protocol_hash=protocol["protocol_hash"],
        device="cuda",
        n_checkpoints=learner["checkpoints"],
        tokens_per_step=learner["tokens_per_step"],
        record_initial=True,
    )
    summary = curve_metrics(
        metrics["token_grid"], metrics["val_ce_bits"], metrics["initial_val_ce_bits"]
    )
    manifest = remote_json.loads((output / "manifest.json").read_text())
    return {
        "cell_id": cell_id,
        "dataset": dataset,
        "stride": stride,
        "seed": seed,
        "protocol_hash": protocol["protocol_hash"],
        "data_sha256": data_hash,
        "byte_counts_sha256": count_hash,
        "n_bytes": len(tokens),
        "unigram_entropy_bits": unigram_entropy,
        "sequence_semantics": "contiguous circular read of stride-interleaved stream; tokens never permuted during training",
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
    cells = [
        {"dataset": dataset["id"], "stride": stride, "seed": seed}
        for dataset in protocol["datasets"]
        for stride in protocol["intervention"]["strides"]
        for seed in protocol["replication"]["seeds"]
        if f"V20/{dataset['id']}/stride{stride}/s{seed}" not in completed
    ]
    if limit:
        cells = cells[:limit]
    print(f"launching {len(cells)} missing v2.0 cells")
    payloads = [{"protocol": protocol, **cell} for cell in cells]
    for result in train_cell.map(payloads):
        existing.append(result)
        existing.sort(key=lambda row: (row["dataset"], row["stride"], row["seed"]))
        result_path.write_text(json.dumps(existing, indent=2))
        print(
            f"{result['cell_id']}: area={result['floor_independent']['normalized_curve_area']:.6f} "
            f"final/init={result['floor_independent']['final_ce_bits'] / result['floor_independent']['initial_ce_bits']:.6f} "
            f"gpu={result['remote']['gpu']} wall={result['remote']['wallclock_seconds']:.1f}s",
            flush=True,
        )


if __name__ == "__main__":
    raise SystemExit("run with: modal run scripts/v20_modal_confirm.py")
