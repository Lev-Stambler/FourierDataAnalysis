"""Remote profile and A10G runner for the frozen v2.1 experiment."""

from __future__ import annotations

import hashlib
import json
from pathlib import Path

import modal

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs/local/v21_predictor_selection"


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
    protocol = json.loads((ROOT / "configs/protocol_v2.1.json").read_text())
    recorded = protocol["protocol_hash"]
    unhashed = dict(protocol)
    unhashed.pop("protocol_hash")
    expected = hashlib.sha256(json.dumps(unhashed, sort_keys=True).encode()).hexdigest()
    if recorded != expected:
        raise ValueError(f"protocol hash mismatch: {recorded} != {expected}")
    return protocol


DATA_FILES = {
    dataset: ROOT / f"dlx/data_cache/v21_{dataset}_bytes_n5499984.npy"
    for dataset in (
        "gutenberg_books",
        "reuters_news",
        "brown_balanced",
        "pubmed_abstracts",
        "cpython_source",
        "linux_c_source",
    )
}

app = modal.App("dlx-v21-predictor-selection")
image = (
    modal.Image.debian_slim(python_version="3.12")
    .pip_install("numpy>=1.26", "torch>=2.3")
    .add_local_dir(ROOT, "/root/pkg", ignore=_ignore)
)
for dataset, path in DATA_FILES.items():
    image = image.add_local_file(path, f"/data/{dataset}.npy")


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
    from dlx.profiles.text_anova import text_inverse_likelihood_profile

    protocol = payload["protocol"]
    dataset = payload["dataset"]
    stride = int(payload["stride"])
    original = np.load(f"/data/{dataset}.npy", mmap_mode="r")
    lanes = np.asarray(original, dtype=np.uint8).reshape(stride, -1)
    tokens = np.ascontiguousarray(lanes.T.reshape(-1))

    n_positions = protocol["spectrum"]["profile_positions"]
    n_times = n_positions // stride
    sampled_times = np.linspace(
        3,
        len(tokens) // stride - 1,
        num=n_times,
        dtype=np.int64,
    )
    if len(np.unique(sampled_times)) != len(sampled_times):
        raise ValueError("profile time sampling produced duplicate targets")
    positions = (
        sampled_times[:, None] * stride + np.arange(stride)[None, :]
    ).reshape(-1)
    fold_ids = np.repeat(np.arange(n_times) & 1, stride)

    started = time.monotonic()
    profile = text_inverse_likelihood_profile(
        tokens,
        q=protocol["learner"]["vocab"],
        lags=(stride, 2 * stride),
        max_tokens=n_positions,
        positions=positions,
        fold_ids=fold_ids,
    )
    original_counts = np.bincount(original.astype(np.int64), minlength=256)
    transformed_counts = np.bincount(tokens.astype(np.int64), minlength=256)
    profile.update(
        {
            "cell_id": f"V21P/{dataset}/stride{stride}",
            "protocol_hash": protocol["protocol_hash"],
            "dataset": dataset,
            "stride": stride,
            "expected_pair": [stride, 2 * stride],
            "data_sha256": remote_hashlib.sha256(tokens.tobytes()).hexdigest(),
            "byte_counts_preserved": bool(
                np.array_equal(original_counts, transformed_counts)
            ),
            "position_sampling": protocol["spectrum"]["position_sampling"],
            "position_sha256": remote_hashlib.sha256(positions.tobytes()).hexdigest(),
            "remote": {
                "compute": "Modal CPU",
                "wallclock_seconds": time.monotonic() - started,
            },
        }
    )
    return profile


@app.function(
    image=image,
    gpu="A10G",
    cpu=2.0,
    memory=4096,
    timeout=1800,
    retries=0,
    max_containers=24,
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
    dataset = payload["dataset"]
    stride = int(payload["stride"])
    seed = int(payload["seed"])
    torch.set_num_threads(2)

    original = np.load(f"/data/{dataset}.npy", mmap_mode="r")
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
        q=learner["vocab"],
        L=learner["ctx_len"],
        name=f"{dataset}_stride{stride}",
        floor_bits=unigram_entropy,
        cyclic=True,
        shuffle_seed=seed,
        data_version=data_hash[:16],
    )
    cell_id = f"V21/{dataset}/stride{stride}/s{seed}"
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
def main(stage: str = "all", limit: int = 0) -> None:
    if stage not in {"profile", "train", "all"}:
        raise ValueError("stage must be profile, train, or all")
    protocol = _load_protocol()
    OUT.mkdir(parents=True, exist_ok=True)
    datasets = [entry["id"] for entry in protocol["datasets"]]
    strides = protocol["intervention"]["strides"]

    if stage in {"profile", "all"}:
        profile_dir = OUT / "profiles"
        profile_dir.mkdir(parents=True, exist_ok=True)
        cells = [
            {"dataset": dataset, "stride": stride}
            for dataset in datasets
            for stride in strides
            if not (profile_dir / f"{dataset}__stride{stride}.json").exists()
        ]
        if limit:
            cells = cells[:limit]
        print(f"launching {len(cells)} missing v2.1 profiles")
        for result in profile_cell.map(
            [{"protocol": protocol, **cell} for cell in cells]
        ):
            path = profile_dir / f"{result['dataset']}__stride{result['stride']}.json"
            path.write_text(json.dumps(result, indent=2))
            spectrum = result["pairs"][0]["conditional_fourier_spectrum"]
            print(
                f"{result['cell_id']}: V={spectrum['nonconstant_energy']:.6f} "
                f"degree={spectrum['mean_nonconstant_spectral_degree']:.4f} "
                f"wall={result['remote']['wallclock_seconds']:.1f}s",
                flush=True,
            )

    if stage in {"train", "all"}:
        result_path = OUT / "remote_results.json"
        existing = json.loads(result_path.read_text()) if result_path.exists() else []
        completed = {row["cell_id"] for row in existing}
        cells = [
            {"dataset": dataset, "stride": stride, "seed": seed}
            for dataset in datasets
            for stride in strides
            for seed in protocol["replication"]["seeds"]
            if f"V21/{dataset}/stride{stride}/s{seed}" not in completed
        ]
        if limit:
            cells = cells[:limit]
        print(f"launching {len(cells)} missing v2.1 training cells")
        for result in train_cell.map(
            [{"protocol": protocol, **cell} for cell in cells]
        ):
            existing.append(result)
            existing.sort(
                key=lambda row: (row["dataset"], row["stride"], row["seed"])
            )
            result_path.write_text(json.dumps(existing, indent=2))
            summary = result["floor_independent"]
            final_fraction = summary["final_ce_bits"] / summary["initial_ce_bits"]
            print(
                f"{result['cell_id']}: area={summary['normalized_curve_area']:.6f} "
                f"final/init={final_fraction:.6f} "
                f"gpu={result['remote']['gpu']} "
                f"wall={result['remote']['wallclock_seconds']:.1f}s",
                flush=True,
            )


if __name__ == "__main__":
    raise SystemExit("run with: modal run scripts/v21_modal_run.py")
