"""Size-matched tabular cells for the revised H5 experiment (protocol v1.3)."""

from __future__ import annotations

import hashlib
import json
import time
from pathlib import Path

import numpy as np
import torch

from ..analysis.floor_independent import curve_metrics
from ..learners.mlp import FixedMLP
from ..protocol import write_manifest
from .tabular_run import _balanced_ce, permutation_influence

EXPOSURE_GRID = (256, 512, 1024, 2048, 4096, 8192)


def _stratified_split(y: np.ndarray, seed: int) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    from sklearn.model_selection import train_test_split

    idx = np.arange(len(y))
    train, rest = train_test_split(idx, test_size=0.30, random_state=seed,
                                   stratify=y)
    val, test = train_test_split(rest, test_size=0.50, random_state=seed + 1,
                                 stratify=y[rest])
    return np.asarray(train), np.asarray(val), np.asarray(test)


def _data_hash(data: dict) -> str:
    h = hashlib.sha256()
    h.update(np.ascontiguousarray(data["X"]).tobytes())
    h.update(np.ascontiguousarray(data["y"]).tobytes())
    return h.hexdigest()


def matched_tabular_run(data: dict, seed: int, out_dir: Path, cell_id: str,
                        protocol_hash: str, band: str, selection_seed: int,
                        device: str = "cpu") -> dict:
    """Train one fixed-MLP cell on an identical exposure grid.

    Rows are selected before this function. Training cycles deterministically over
    the train split so every size band sees exactly 8,192 examples, independent of
    the finite table size.
    """
    out_dir = Path(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)
    rng = np.random.default_rng(seed)
    torch.manual_seed(seed)

    X, y = data["X"], data["y"]
    tr_idx, va_idx, te_idx = _stratified_split(y, seed)
    Xtr, ytr = X[tr_idx], y[tr_idx]
    Xva, yva = X[va_idx], y[va_idx]
    Xte, yte = X[te_idx], y[te_idx]

    model = FixedMLP(X.shape[1], data["q_features"], data["n_classes"]).to(device)
    opt = torch.optim.AdamW(model.parameters(), lr=1e-3, weight_decay=1e-4)

    initial = _balanced_ce(model, Xva, yva, device)
    curve = [{"samples_seen": 0, "val_ce_bits": initial}]
    order = rng.permutation(len(Xtr))
    ptr = 0
    seen = 0
    batch_size = 256
    t0 = time.time()
    for target in EXPOSURE_GRID:
        while seen < target:
            if ptr >= len(order):
                order = rng.permutation(len(Xtr))
                ptr = 0
            take = min(batch_size, target - seen, len(order) - ptr)
            ids = order[ptr:ptr + take]
            xb = torch.from_numpy(Xtr[ids]).to(device)
            yb = torch.from_numpy(ytr[ids]).to(device)
            ptr += take
            seen += take
            loss = torch.nn.functional.cross_entropy(model(xb), yb)
            opt.zero_grad(set_to_none=True)
            loss.backward()
            opt.step()
        curve.append({"samples_seen": seen,
                      "val_ce_bits": _balanced_ce(model, Xva, yva, device)})

    summary = curve_metrics([p["samples_seen"] for p in curve],
                            [p["val_ce_bits"] for p in curve], initial)
    infl = permutation_influence(model, Xva, yva, device, rng, n_repeats=3)
    counts = np.bincount(y, minlength=data["n_classes"]).astype(float)
    probs = counts / counts.sum()
    label_entropy = float(-(probs[probs > 0] * np.log2(probs[probs > 0])).sum())
    data_sha = _data_hash(data)
    metrics = {
        "cell_id": cell_id,
        "band": band,
        "dataset": data["name"],
        "openml": data["openml"],
        "data_sha256": data_sha,
        "selection_seed": selection_seed,
        "n_rows": len(X),
        "n_features": int(X.shape[1]),
        "n_classes": int(data["n_classes"]),
        "q_features": [int(q) for q in data["q_features"]],
        "seed": seed,
        "curve": curve,
        **summary,
        "test_ce_bits": _balanced_ce(model, Xte, yte, device),
        "label_entropy_bits": label_entropy,
        "majority_fraction": float(probs.max()),
        "permutation_influence": infl.tolist(),
        "total_influence_perm": float(infl.sum()),
        "n_params": model.n_params(),
        "wallclock_seconds": time.time() - t0,
    }
    metrics_path = out_dir / "metrics.json"
    metrics_path.write_text(json.dumps(metrics, indent=2))
    version_payload = {"openml": data["openml"], "data_sha256": data_sha,
                       "selection_seed": selection_seed, "band": band}
    family_version = hashlib.sha256(json.dumps(version_payload, sort_keys=True).encode()).hexdigest()[:16]
    config_hash = hashlib.sha256(json.dumps({
        "model": "FixedMLP-2x256-emb8", "exposure_grid": EXPOSURE_GRID,
        "split": "70/15/15-stratified", "lr": 1e-3, "weight_decay": 1e-4,
    }, sort_keys=True).encode()).hexdigest()[:16]
    write_manifest(out_dir, {
        "protocol_hash": protocol_hash,
        "family_version": family_version,
        "cell_id": cell_id,
        "seed": seed,
        "budget_tokens": seen,
        "device": device,
        "torch_version": torch.__version__,
        "wallclock_seconds": metrics["wallclock_seconds"],
        "config_hash": config_hash,
        "metrics_path": str(metrics_path),
        "status": "complete",
    })
    return metrics
