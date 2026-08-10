"""Reprofile the prior OLS panel on the exact v2.6 support distribution.

This is a post-outcome compatibility repair, not a new confirmatory experiment.
The original v2.6 predictor accidentally mixed historical profiles sampled on
lags 1..16 with new profiles sampled on the frozen geometric lag bank.
"""

from __future__ import annotations

import gzip
import hashlib
import json
import sys
from pathlib import Path

import modal

LOCAL_ROOT = Path(__file__).parent.parent
IS_REMOTE = str(Path(__file__).resolve()).startswith("/root/")
ROOT = Path("/root/pkg") if IS_REMOTE else LOCAL_ROOT
sys.path.insert(0, str(ROOT))

from dlx.analysis.text_panel import v24_profile_data_files
from dlx.protocol.frozen import load_frozen_protocol

OUT = LOCAL_ROOT / "runs/local/v26_lag_grid_repair"
PROTOCOL_PATH = LOCAL_ROOT / "configs/protocol_v2.6.json"
V25_PROFILES = LOCAL_ROOT / "runs/local/v25_kiss_diagnostic/profiles"
PRIOR_DATASETS = (
    "brown_balanced",
    "codeparrot_python",
    "cpython_source",
    "enwik8",
    "gutenberg_books",
    "linux_c_source",
    "pubmed_abstracts",
    "reuters_news",
    "tinystories",
    "wikitext2",
    "coq_source",
    "ghc_haskell_source",
    "go_source",
    "julia_source",
    "kubernetes_docs",
    "llvm_cpp_source",
    "mdn_docs",
    "postgresql_source",
    "pubmed_independent",
    "rfc_legacy",
    "rust_reference_docs",
    "typescript_source",
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


def _data_paths() -> dict[str, Path]:
    paths = {
        **v24_profile_data_files(LOCAL_ROOT, "development"),
        **v24_profile_data_files(LOCAL_ROOT, "confirmation"),
    }
    selected = {dataset: paths[dataset] for dataset in PRIOR_DATASETS}
    missing = [dataset for dataset, path in selected.items() if not path.exists()]
    if missing:
        raise FileNotFoundError(f"missing prior corpus arrays: {missing}")
    return selected


app = modal.App("dlx-v26-reprofile-prior")
image = (
    modal.Image.debian_slim(python_version="3.12")
    .pip_install("numpy>=1.26")
    .add_local_dir(LOCAL_ROOT, "/root/pkg", ignore=_ignore)
)
if not IS_REMOTE:
    for _dataset, _path in _data_paths().items():
        image = image.add_local_file(_path, f"/data/{_dataset}.npy")


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
    return {
        "cell_id": f"V26RP/{dataset}",
        "dataset": dataset,
        "protocol_hash": protocol["protocol_hash"],
        "data_sha256": remote_hashlib.sha256(np.asarray(tokens).tobytes()).hexdigest(),
        "remote": {
            "compute": "Modal CPU",
            "wallclock_seconds": time.monotonic() - started,
        },
        **profile,
    }


def _write_result(result: dict) -> None:
    dataset = result["dataset"]
    historical = json.loads((V25_PROFILES / f"{dataset}.json").read_text())
    if result["data_sha256"] != historical["data_sha256"]:
        raise ValueError(f"prior corpus hash mismatch: {dataset}")
    chains = result.pop("chains")
    summary_path = OUT / "profiles" / f"{dataset}.json"
    summary_path.parent.mkdir(parents=True, exist_ok=True)
    summary_path.write_text(json.dumps(result, indent=2) + "\n")
    audit = {
        "dataset": dataset,
        "data_sha256": result["data_sha256"],
        "summary_sha256": hashlib.sha256(summary_path.read_bytes()).hexdigest(),
        "chains": chains,
    }
    payload = json.dumps(audit, separators=(",", ":")).encode()
    audit_path = OUT / "audit_chains" / f"{dataset}.json.gz"
    audit_path.parent.mkdir(parents=True, exist_ok=True)
    audit_path.write_bytes(gzip.compress(payload, compresslevel=9, mtime=0))


@app.local_entrypoint()
def main(dataset: str = "all") -> None:
    protocol = load_frozen_protocol(PROTOCOL_PATH)
    selected = PRIOR_DATASETS if dataset == "all" else (dataset,)
    unknown = sorted(set(selected) - set(PRIOR_DATASETS))
    if unknown:
        raise ValueError(f"unknown prior datasets: {unknown}")
    payloads = [
        {"dataset": name, "protocol": protocol}
        for name in selected
        if not (OUT / "profiles" / f"{name}.json").exists()
    ]
    for result in profile_cell.map(payloads, order_outputs=False):
        _write_result(result)
        print(
            f"{result['dataset']}: "
            f"G={result['sampled_features']['sampled_geometric_complexity_through_degree3']:.6f}",
            flush=True,
        )
    print(f"compatible profiles complete: {len(selected)}", flush=True)
