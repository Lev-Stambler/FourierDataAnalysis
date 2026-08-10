"""Post-hoc diagnostic: repeat v2.7 locality on middle and tail corpus windows."""

from __future__ import annotations

import json
import sys
from pathlib import Path

import numpy as np

LOCAL_ROOT = Path(__file__).parent.parent
IS_REMOTE = str(Path(__file__).resolve()).startswith("/root/")
ROOT = Path("/root/pkg") if IS_REMOTE else LOCAL_ROOT
sys.path.insert(0, str(ROOT))

from scripts.v27_modal import N_BYTES, OUT, PROTOCOL, SOURCES, app, image

WINDOW_EXAMPLES = int(PROTOCOL["profile"]["positions"])
WINDOW_BYTES = WINDOW_EXAMPLES + max(PROTOCOL["profile"]["lags"])
OFFSETS = {
    "middle": (N_BYTES - WINDOW_BYTES) // 2,
    "tail": N_BYTES - WINDOW_BYTES,
}


@app.function(
    image=image,
    cpu=4.0,
    memory=16384,
    timeout=3600,
    retries=0,
    max_containers=8,
    block_network=True,
)
def profile_window(payload: dict) -> dict:
    import sys as remote_sys
    import time

    import numpy as remote_numpy

    remote_sys.path.insert(0, "/root/pkg")
    from dlx.profiles.sampled_degree import (
        marginal_locality_features,
        sampled_token_degree_profile,
    )

    spec = payload["protocol"]["profile"]
    values = remote_numpy.load(f"/data/{payload['dataset']}.npy", mmap_mode="r")
    offset = int(payload["offset"])
    selected = values[offset : offset + WINDOW_BYTES]
    started = time.monotonic()
    profile = sampled_token_degree_profile(
        selected,
        lags=tuple(spec["lags"]),
        max_degree=spec["max_degree"],
        n_chains=spec["chains"],
        max_positions=spec["positions"],
        seed=spec["seed"],
        delta=spec["delta"],
        include_chains=True,
        include_product_reference=False,
    )
    marginal = marginal_locality_features(
        profile["chains"], tuple(profile["coordinate_radii"]), feature_degree=3
    )
    return {
        "dataset": payload["dataset"],
        "stratum": payload["stratum"],
        "window": payload["window"],
        "offset": offset,
        "n_examples": profile["n_examples"],
        "features": {**profile["sampled_features"], **marginal},
        "remote_wallclock_seconds": time.monotonic() - started,
    }


def _summary(records: list[dict]) -> dict:
    from scipy.stats import spearmanr

    analysis = json.loads((OUT / "analysis.json").read_text())
    target = {
        row["dataset"]: row["normalized_learning_time"] for row in analysis["rows"]
    }
    strata = sorted({row["stratum"] for row in analysis["rows"]})
    feature = PROTOCOL["profile"]["primary_feature"]
    by_window = {}
    for window in ("start", "middle", "tail", "three_window_mean"):
        if window == "three_window_mean":
            values = {
                source["id"]: float(
                    np.mean(
                        [
                            row["features"][feature]
                            for row in records
                            if row["dataset"] == source["id"]
                        ]
                    )
                )
                for source in SOURCES
            }
        else:
            values = {
                row["dataset"]: row["features"][feature]
                for row in records
                if row["window"] == window
            }
        rhos = {}
        for stratum in strata:
            datasets = [
                source["id"] for source in SOURCES if source["stratum"] == stratum
            ]
            rhos[stratum] = float(
                spearmanr(
                    [values[dataset] for dataset in datasets],
                    [target[dataset] for dataset in datasets],
                ).statistic
            )
        datasets = [source["id"] for source in SOURCES]
        by_window[window] = {
            "pooled_spearman_rho": float(
                spearmanr(
                    [values[dataset] for dataset in datasets],
                    [target[dataset] for dataset in datasets],
                ).statistic
            ),
            "mean_within_stratum_spearman_rho": float(np.mean(list(rhos.values()))),
            "within_stratum_rhos": rhos,
        }
    return {
        "status": "POSTHOC_DIAGNOSTIC_NOT_A_FROZEN_TEST",
        "protocol_hash": PROTOCOL["protocol_hash"],
        "window_examples": WINDOW_EXAMPLES,
        "offsets": {"start": 0, **OFFSETS},
        "summaries": by_window,
        "records": records,
    }


@app.local_entrypoint(name="window_stability")
def main() -> None:
    records = []
    for source in SOURCES:
        profile = json.loads((OUT / "profiles" / f"{source['id']}.json").read_text())
        records.append(
            {
                "dataset": source["id"],
                "stratum": source["stratum"],
                "window": "start",
                "offset": 0,
                "n_examples": profile["n_examples"],
                "features": profile["features"],
                "remote_wallclock_seconds": profile["remote"]["wallclock_seconds"],
            }
        )
    payloads = [
        {
            "dataset": source["id"],
            "stratum": source["stratum"],
            "window": window,
            "offset": offset,
            "protocol": PROTOCOL,
        }
        for source in SOURCES
        for window, offset in OFFSETS.items()
    ]
    for result in profile_window.map(payloads):
        records.append(result)
        print(
            f"{result['dataset']}/{result['window']}: "
            f"Lambda={result['features'][PROTOCOL['profile']['primary_feature']]:.6f}",
            flush=True,
        )
    result = _summary(records)
    path = OUT / "posthoc_window_stability.json"
    path.write_text(json.dumps(result, indent=2) + "\n")
    print(json.dumps(result["summaries"], indent=2))
