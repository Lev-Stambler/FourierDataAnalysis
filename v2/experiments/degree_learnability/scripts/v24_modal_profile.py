"""Remote CPU profiler for the frozen v2.4 development and prediction data."""

from __future__ import annotations

import hashlib
import json
from pathlib import Path

import modal

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs/local/v24_spectrum_predictor/text_profiles"

DATASETS = {
    "enwik8": ("dlx/data_cache/v19_enwik8_bytes_n5500000.npy", [1, 2, 4, 8]),
    "tinystories": ("dlx/data_cache/h5_tinystories_bytes_n5500000.npy", [1, 2, 4, 8]),
    "wikitext2": ("dlx/data_cache/v18_wikitext2_bytes_n5500000.npy", [1, 2, 4, 8]),
    "codeparrot_python": (
        "dlx/data_cache/v18_codeparrot_python_bytes_n5500000.npy",
        [1, 2, 4, 8],
    ),
    "gutenberg_books": (
        "dlx/data_cache/v21_gutenberg_books_bytes_n5499984.npy",
        [1, 4, 8, 16],
    ),
    "reuters_news": (
        "dlx/data_cache/v21_reuters_news_bytes_n5499984.npy",
        [1, 4, 8, 16],
    ),
    "brown_balanced": (
        "dlx/data_cache/v21_brown_balanced_bytes_n5499984.npy",
        [1, 4, 8, 16],
    ),
    "pubmed_abstracts": (
        "dlx/data_cache/v21_pubmed_abstracts_bytes_n5499984.npy",
        [1, 4, 8, 16],
    ),
    "cpython_source": (
        "dlx/data_cache/v21_cpython_source_bytes_n5499984.npy",
        [1, 4, 8, 16],
    ),
    "linux_c_source": (
        "dlx/data_cache/v21_linux_c_source_bytes_n5499984.npy",
        [1, 4, 8, 16],
    ),
    "mathlib_lean": ("dlx/data_cache/v22_mathlib_lean_bytes_n8000000.npy", [1, 4, 8, 16]),
    "rust_source": ("dlx/data_cache/v22_rust_source_bytes_n8000000.npy", [1, 4, 8, 16]),
    "rfc_technical": (
        "dlx/data_cache/v22_rfc_technical_bytes_n8000000.npy",
        [1, 4, 8, 16],
    ),
}


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


app = modal.App("dlx-v24-spectrum-profile")
image = (
    modal.Image.debian_slim(python_version="3.12")
    .pip_install("numpy>=1.26")
    .add_local_dir(ROOT, "/root/pkg", ignore=_ignore)
)
for dataset, (relative_path, _) in DATASETS.items():
    image = image.add_local_file(ROOT / relative_path, f"/data/{dataset}.npy")


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
    original_counts = np.bincount(original.astype(np.int64), minlength=256)
    transformed_counts = np.bincount(tokens.astype(np.int64), minlength=256)
    return {
        "cell_id": f"V24P/{dataset}/stride{stride}",
        "protocol_hash": protocol["protocol_hash"],
        "dataset": dataset,
        "stride": stride,
        "data_sha256": remote_hashlib.sha256(tokens.tobytes()).hexdigest(),
        "byte_counts_preserved": bool(np.array_equal(original_counts, transformed_counts)),
        "position_sha256": remote_hashlib.sha256(positions.tobytes()).hexdigest(),
        "remote": {
            "compute": "Modal CPU",
            "wallclock_seconds": time.monotonic() - started,
        },
        **profile,
    }


@app.local_entrypoint()
def main(limit: int = 0) -> None:
    protocol = _load_protocol()
    OUT.mkdir(parents=True, exist_ok=True)
    cells = [
        {"dataset": dataset, "stride": stride}
        for dataset, (_, strides) in DATASETS.items()
        for stride in strides
        if not (OUT / f"{dataset}__stride{stride}.json").exists()
    ]
    if limit:
        cells = cells[:limit]
    print(f"launching {len(cells)} missing v2.4 resolved profiles")
    for result in profile_cell.map([{"protocol": protocol, **cell} for cell in cells]):
        path = OUT / f"{result['dataset']}__stride{result['stride']}.json"
        path.write_text(json.dumps(result, indent=2))
        features = result["features"]
        print(
            f"{result['cell_id']}: E={features['resolved_nonconstant_energy']:.6f} "
            f"lambda={features['energy_weighted_log_radius']} "
            f"wall={result['remote']['wallclock_seconds']:.1f}s",
            flush=True,
        )


if __name__ == "__main__":
    raise SystemExit("run with: modal run scripts/v24_modal_profile.py")
