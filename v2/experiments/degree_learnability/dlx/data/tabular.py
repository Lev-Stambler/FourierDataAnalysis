"""R3 tabular ladder: OpenML resolution, quantile binning, and fixed-MLP cells.

Dataset resolution rule (protocol v1): first exact-name OpenML match resolved at
download time; dataset_id + version recorded in configs/openml_resolution.json and
in each cell manifest BEFORE learner training.
"""

from __future__ import annotations

import json
from pathlib import Path

import numpy as np

N_BINS = 16  # protocol: numerics -> 16 quantile bins
RESOLUTION_PATH = Path(__file__).parent.parent.parent / "configs" / "openml_resolution.json"

LADDER = [
    # (name, enumerable_expected)
    ("iris", True),
    ("balance-scale", True),
    ("tic-tac-toe", True),
    ("car", True),
    ("mushroom", False),
    ("kr-vs-kp", False),
    ("credit-g", False),
    ("churn", False),
    ("adult", False),           # subset
    ("bank-marketing", False),  # 8k subset
    ("connect-4", False),       # subsampled
    ("mini-boone", False),
]


def resolve_openml(names: list[str]) -> dict:
    """Resolve dataset names to OpenML ids/versions (cached)."""
    import openml  # type: ignore

    cache = {}
    if RESOLUTION_PATH.exists():
        cache = json.loads(RESOLUTION_PATH.read_text())
    out = {}
    listing = None
    for name in names:
        if name in cache:
            out[name] = cache[name]
            continue
        if listing is None:  # one heavy listing fetch per invocation
            listing = openml.datasets.list_datasets(output_format="dataframe")
        exact = listing[listing["name"] == name]
        if exact.empty:  # normalized fallback: case-insensitive, ignoring non-alphanumerics
            import re
            norm = lambda s: re.sub(r"[^a-z0-9]", "", str(s).lower())
            exact = listing[listing["name"].map(norm) == norm(name)]
        if exact.empty:
            raise ValueError(f"no exact-name OpenML match for {name}")
        # 'first exact-name match' = first registered dataset (lowest did),
        # latest version within that did (canonical dataset, not derivatives)
        first_did = int(exact["did"].min())
        row = exact[exact["did"] == first_did].sort_values("version").iloc[-1]
        out[name] = {"openml_id": int(row["did"]), "version": int(row["version"]),
                     "n_features": int(row.get("NumberOfFeatures", -1)),
                     "n_rows": int(row.get("NumberOfInstances", -1))}
        cache[name] = out[name]
        RESOLUTION_PATH.parent.mkdir(parents=True, exist_ok=True)
        RESOLUTION_PATH.write_text(json.dumps(cache, indent=2))
    return out


def load_tabular(name: str, max_rows: int = 20_000, seed: int = 0) -> dict:
    """Load + bin a dataset. Returns dict with X (int codes), y (int labels),
    per-feature alphabet sizes, and metadata."""
    from sklearn.datasets import fetch_openml

    res = resolve_openml([name])[name]
    raw = fetch_openml(data_id=res["openml_id"], as_frame="auto", parser="auto")
    X, y = raw.data, raw.target
    if not hasattr(X, "iloc"):  # sparse/array ARFF -> DataFrame of objects
        import pandas as pd
        import scipy.sparse as sp
        Xa = X.toarray() if sp.issparse(X) else np.asarray(X)
        names = raw.feature_names if getattr(raw, "feature_names", None) else \
            [f"f{j}" for j in range(Xa.shape[1])]
        X = pd.DataFrame(Xa, columns=names)
        y = np.asarray(y)

    rng = np.random.default_rng(seed)
    if len(X) > max_rows:
        idx = rng.choice(len(X), size=max_rows, replace=False)
        X, y = X.iloc[idx], y.iloc[idx] if hasattr(y, "iloc") else y[idx]

    # encode labels
    classes = sorted(set(map(str, np.asarray(y))))
    y_codes = np.array([classes.index(str(v)) for v in np.asarray(y)], dtype=np.int64)

    # encode features: categories as-is, numerics -> quantile bins
    n = X.shape[1]
    Xc = np.zeros((len(X), n), dtype=np.int64)
    qsizes = []
    for j in range(n):
        col = X.iloc[:, j]
        if str(col.dtype) == "category" or col.dtype == object:
            col = col.astype(object)
            cats = sorted(set(map(str, col.fillna("__NA__"))))
            Xc[:, j] = [cats.index(str(v if v is not None and str(v) != "nan" else "__NA__"))
                        for v in col]
            qsizes.append(len(cats))
        else:
            vals = col.astype(float).to_numpy()
            vals = np.where(np.isnan(vals), np.nanmin(vals), vals)
            edges = np.quantile(vals, np.linspace(0, 1, N_BINS + 1)[1:-1])
            Xc[:, j] = np.searchsorted(edges, vals, side="right")
            qsizes.append(N_BINS)

    return {"X": Xc, "y": y_codes, "q_features": qsizes, "n_classes": len(classes),
            "name": name, "openml": res}


def grid_size(qs: list[int]) -> int:
    g = 1
    for q in qs:
        g *= q
    return g


def dataset_spectrum(X: np.ndarray, y: np.ndarray, n_classes: int,
                     cap: int = 1 << 22) -> dict:
    """Exact dataset Fourier level weights for an enumerable table.

    f: rows -> one-hot label, empirical measure over rows. Mixed-radix FFT:
    h[x] = sum of one-hot labels over rows equal to x; fhat_D = ffn(h)/m.
    Returns level weights W^k (k = categorical degree over feature coordinates).
    """
    m, n = X.shape
    qs = [int(X[:, j].max()) + 1 for j in range(n)]
    total = grid_size(qs)
    if total > cap:
        raise ValueError(f"table not enumerable: grid {total} > cap {cap}")
    h = np.zeros(tuple(qs) + (n_classes,), dtype=np.complex128)
    idx = tuple(X[:, j] for j in range(n))
    np.add.at(h, idx + (y,), 1.0)
    coeffs = np.fft.fftn(h, axes=range(n)) / m
    W = np.zeros(n + 1, dtype=np.float64)
    flat = coeffs.reshape(-1, n_classes)
    mass = (np.abs(flat) ** 2).sum(axis=1)
    grid_idx = np.indices(tuple(qs)).reshape(n, -1)
    deg = np.count_nonzero(grid_idx, axis=0)
    np.add.at(W, deg, mass)
    distinct = len(np.unique(X, axis=0))
    C_D = grid_size(qs) / distinct
    return {"W": W.tolist(), "W_normalized": (W / C_D).tolist(), "C_D": C_D,
            "distinct_rows": int(distinct), "n_rows": int(m),
            "grid_size": int(total), "q_features": qs, "n_features": n}


def spectral_influences(X: np.ndarray, y: np.ndarray, n_classes: int,
                        cap: int = 1 << 22) -> np.ndarray:
    """Sens_i = sum_{alpha: alpha_i != 0} ||fhat_D(alpha)||^2 (spectral form)."""
    m, n = X.shape
    qs = [int(X[:, j].max()) + 1 for j in range(n)]
    if grid_size(qs) > cap:
        raise ValueError("table not enumerable")
    h = np.zeros(tuple(qs) + (n_classes,), dtype=np.complex128)
    idx = tuple(X[:, j] for j in range(n))
    np.add.at(h, idx + (y,), 1.0)
    coeffs = np.fft.fftn(h, axes=range(n)) / m
    mass = (np.abs(coeffs.reshape(-1, n_classes)) ** 2).sum(axis=1)
    grid_idx = np.indices(tuple(qs)).reshape(n, -1)
    sens = np.zeros(n, dtype=np.float64)
    for j in range(n):
        sel = grid_idx[j] != 0
        sens[j] = mass[sel].sum()
    return sens
