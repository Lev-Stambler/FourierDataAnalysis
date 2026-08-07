"""R3 tabular training harness: fixed-MLP sample-complexity runs with manifests.

Difficulty metric (preregistered for M9): val balanced CE curve over the sample
budget grid; T*_rows = first budget reaching (best observed val CE) + theta;
secondary: gap at smallest budget minus gap at largest (learning amount), and
total influence (permutation importance; spectral influences where enumerable).
"""

from __future__ import annotations

import hashlib
import json
import time
from pathlib import Path

import numpy as np
import torch

from ..learners.mlp import FixedMLP, cross_entropy_bits
from ..protocol import write_manifest

THETA = 0.05
BUDGET_GRID = (512, 2048, 8192, 32768)


def _dataset_version(meta: dict) -> str:
    h = hashlib.sha256(json.dumps(meta, sort_keys=True, default=str).encode())
    return h.hexdigest()[:16]


def _balanced_ce(model, X, y, device, batch=4096) -> float:
    model.eval()
    total, n = 0.0, 0
    with torch.no_grad():
        for i in range(0, len(X), batch):
            xb = torch.from_numpy(X[i:i + batch]).to(device)
            yb = torch.from_numpy(y[i:i + batch]).to(device)
            total += float(torch.nn.functional.cross_entropy(model(xb), yb,
                                                             reduction="sum").item())
            n += len(xb)
    model.train()
    return total / n / float(np.log(2.0))


def permutation_influence(model, X, y, device, rng, n_repeats=3) -> np.ndarray:
    base = _balanced_ce(model, X, y, device)
    n_feat = X.shape[1]
    out = np.zeros(n_feat)
    for j in range(n_feat):
        acc = 0.0
        for _ in range(n_repeats):
            Xp = X.copy()
            Xp[:, j] = Xp[rng.integers(0, len(X), size=len(X)), j]
            acc += _balanced_ce(model, Xp, y, device)
        out[j] = acc / n_repeats - base
    return out


def tabular_run(data: dict, seed: int, out_dir: Path, cell_id: str,
                protocol_hash: str, device: str = "cpu") -> dict:
    out_dir = Path(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)
    rng = np.random.default_rng(seed)
    torch.manual_seed(seed)

    X, y = data["X"], data["y"]
    n = len(X)
    perm = rng.permutation(n)
    tr = int(n * 0.7)
    va = int(n * 0.15)
    Xtr, ytr = X[perm[:tr]], y[perm[:tr]]
    Xva, yva = X[perm[tr:tr + va]], y[perm[tr:tr + va]]
    Xte, yte = X[perm[tr + va:]], y[perm[tr + va:]]

    model = FixedMLP(X.shape[1], data["q_features"], data["n_classes"]).to(device)
    opt = torch.optim.AdamW(model.parameters(), lr=1e-3, weight_decay=1e-4)

    budgets = [b for b in BUDGET_GRID if b <= tr]
    if tr < BUDGET_GRID[0]:
        # tiny dataset: fractional budgets instead of the absolute grid
        budgets = sorted({max(64, tr // 4), tr // 2, (3 * tr) // 4, tr})
    if not budgets or budgets[-1] < tr:
        budgets.append(tr)
    order = rng.permutation(tr)
    ptr = 0
    seen = 0
    curve: list[dict] = []
    t0 = time.time()
    batch = 256
    next_budget = budgets[0]
    while seen < budgets[-1]:
        take = min(batch, budgets[-1] - seen)
        if ptr + take > tr:
            order = rng.permutation(tr)
            ptr = 0
        xb = torch.from_numpy(Xtr[order[ptr:ptr + take]]).to(device)
        yb = torch.from_numpy(ytr[order[ptr:ptr + take]]).to(device)
        ptr += take
        seen += take
        loss = torch.nn.functional.cross_entropy(model(xb), yb)
        opt.zero_grad(set_to_none=True)
        loss.backward()
        opt.step()
        if seen >= next_budget:
            ce = _balanced_ce(model, Xva, yva, device)
            curve.append({"samples_seen": seen, "val_ce_bits": ce})
            next_budget = budgets[min(budgets.index(next_budget) + 1, len(budgets) - 1)] \
                if next_budget in budgets else budgets[-1]

    best = min(p["val_ce_bits"] for p in curve)
    t_star = next((p["samples_seen"] for p in curve
                   if p["val_ce_bits"] <= best + THETA), None)
    test_ce = _balanced_ce(model, Xte, yte, device)
    infl = permutation_influence(model, Xva, yva, device, rng)

    metrics = {
        "cell_id": cell_id, "dataset": data["name"],
        "n_rows": n, "n_features": X.shape[1], "n_classes": data["n_classes"],
        "seed": seed, "curve": curve, "best_val_ce_bits": best,
        "T_star_rows": t_star, "theta": THETA,
        "learning_amount_bits": curve[0]["val_ce_bits"] - curve[-1]["val_ce_bits"],
        "test_ce_bits": test_ce,
        "permutation_influence": infl.tolist(),
        "total_influence_perm": float(infl.sum()),
        "n_params": model.n_params(),
        "wallclock_seconds": time.time() - t0,
    }
    metrics_path = out_dir / "metrics.json"
    metrics_path.write_text(json.dumps(metrics, indent=2))
    write_manifest(out_dir, {
        "protocol_hash": protocol_hash,
        "family_version": _dataset_version(data["openml"]),
        "cell_id": cell_id,
        "seed": seed,
        "budget_tokens": seen,
        "device": device,
        "torch_version": torch.__version__,
        "wallclock_seconds": metrics["wallclock_seconds"],
        "config_hash": hashlib.sha256(b"FixedMLP-2x256-emb8").hexdigest()[:16],
        "metrics_path": str(metrics_path),
        "status": "complete",
    })
    return metrics
