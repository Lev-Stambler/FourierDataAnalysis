"""Modal A10G runner for the frozen v1.9 real-data stride pilot."""

from __future__ import annotations

import hashlib
import json
from pathlib import Path

import modal

ROOT = Path(__file__).parent.parent
DATA_FILE = ROOT / "dlx/data_cache/v19_enwik8_bytes_n5500000.npy"
OUT = ROOT / "runs/local/v19_parseval_stride"


def _ignore(path: Path) -> bool:
    value = str(path)
    excluded = (
        "__pycache__",
        ".venv",
        ".pytest_cache",
        ".ruff_cache",
        "runs/",
        "dlx/data_cache/",
    )
    return any(part in value for part in excluded)


app = modal.App("dlx-v19-real-stride")
image = (
    modal.Image.debian_slim(python_version="3.12")
    .pip_install("numpy>=1.26", "torch>=2.3")
    .add_local_dir(ROOT, "/root/pkg", ignore=_ignore)
    .add_local_file(DATA_FILE, "/data/enwik8.npy")
)


def _load_protocol() -> dict:
    protocol = json.loads((ROOT / "configs/protocol_v1.9.json").read_text())
    recorded = protocol["protocol_hash"]
    unhashed = dict(protocol)
    unhashed.pop("protocol_hash")
    expected = hashlib.sha256(json.dumps(unhashed, sort_keys=True).encode()).hexdigest()
    if recorded != expected:
        raise ValueError(f"protocol hash mismatch: {recorded} != {expected}")
    return protocol


@app.function(image=image, gpu="A10G", timeout=1800, retries=0, cpu=2.0, memory=4096)
def train_stride(payload: dict) -> dict:
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

    cell = payload["cell"]
    protocol = payload["protocol"]
    torch.set_num_threads(2)
    stride = int(cell["stride"])
    seed = int(cell["seed"])
    original = np.load("/data/enwik8.npy", mmap_mode="r")
    usable = (len(original) // stride) * stride
    lanes = np.asarray(original[:usable], dtype=np.uint8).reshape(stride, -1)
    tokens = np.ascontiguousarray(lanes.T.reshape(-1))
    counts = np.bincount(tokens.astype(np.int64), minlength=256)
    probabilities = counts[counts > 0].astype(float) / counts.sum()
    unigram_entropy = float(-(probabilities * np.log2(probabilities)).sum())
    data_hash = remote_hashlib.sha256(tokens.tobytes()).hexdigest()
    count_hash = remote_hashlib.sha256(counts.tobytes()).hexdigest()

    spec = protocol["stride_pilot"]
    family = CorpusFamily(
        tokens,
        q=256,
        L=spec["context_length"],
        name=f"enwik8_stride{stride}",
        floor_bits=unigram_entropy,
        cyclic=True,
        shuffle_seed=seed,
        data_version=data_hash[:16],
    )
    config = TransformerConfig(**spec["learner"])
    cell_id = f"V19/enwik8_stride{stride}/s{seed}"
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
    spec = protocol["stride_pilot"]
    OUT.mkdir(parents=True, exist_ok=True)
    result_path = OUT / "remote_results.json"
    existing = json.loads(result_path.read_text()) if result_path.exists() else []
    completed = {row["cell_id"] for row in existing}
    cells = [
        {"stride": stride, "seed": seed}
        for stride in spec["strides"]
        for seed in spec["seeds"]
        if f"V19/enwik8_stride{stride}/s{seed}" not in completed
    ]
    if limit:
        cells = cells[:limit]
    print(f"launching {len(cells)} missing v1.9 cells")
    payloads = [{"cell": cell, "protocol": protocol} for cell in cells]
    for result in train_stride.map(payloads):
        existing.append(result)
        existing.sort(key=lambda row: (row["stride"], row["seed"]))
        result_path.write_text(json.dumps(existing, indent=2))
        print(
            f"{result['cell_id']}: area={result['floor_independent']['normalized_curve_area']:.6f} "
            f"gpu={result['remote']['gpu']} wall={result['remote']['wallclock_seconds']:.1f}s",
            flush=True,
        )


if __name__ == "__main__":
    raise SystemExit("run with: modal run scripts/v19_modal_stride.py")
