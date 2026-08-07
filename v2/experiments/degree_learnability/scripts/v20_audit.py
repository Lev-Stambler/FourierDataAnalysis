"""Fail-closed audit for the frozen v2.0 geometry confirmation."""

from __future__ import annotations

import hashlib
import json
import sys
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).parent.parent))
sys.path.insert(0, str(Path(__file__).parent))

from v19_analyze import parseval_metrics, stride_interleave
from v20_analyze import (
    DATA_FILES,
    OUT,
    PILOT_DATASET,
    PROFILE_DIR,
    _median_cell_metrics,
    _profile_positions,
    _seed_specific_rhos,
    exact_blocked_permutation,
    load_protocol,
)

from dlx.analysis.floor_independent import curve_metrics

ROOT = Path(__file__).parent.parent


def _strict_json(path: Path) -> dict | list:
    return json.loads(
        path.read_text(),
        parse_constant=lambda value: (_ for _ in ()).throw(
            ValueError(f"non-finite JSON constant {value} in {path}")
        ),
    )


def _sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _sha256_tokens(tokens: np.ndarray) -> str:
    return hashlib.sha256(np.ascontiguousarray(tokens).tobytes()).hexdigest()


def _close(left: float, right: float, tolerance: float = 1e-12) -> bool:
    return bool(np.isclose(left, right, rtol=0.0, atol=tolerance))


def main() -> None:
    protocol = load_protocol()
    result = _strict_json(OUT / "integrated_analysis.json")
    remote = _strict_json(OUT / "remote_results.json")
    source_audit = _strict_json(ROOT / "runs/local/v19_parseval_stride/audit.json")
    failures: list[str] = []

    protocol_hash = protocol["protocol_hash"]
    source_hash = protocol["source_protocol_hash"]
    if result["protocol_hash"] != protocol_hash:
        failures.append("integrated result protocol hash mismatch")
    if source_audit["status"] != "PASS" or source_audit["protocol_hash"] != source_hash:
        failures.append("v1.9 source audit/hash mismatch")

    datasets = [entry["id"] for entry in protocol["datasets"]]
    strides = protocol["intervention"]["strides"]
    seeds = protocol["replication"]["seeds"]
    expected_cells = {
        (dataset, stride, seed)
        for dataset in datasets
        for stride in strides
        for seed in seeds
    }
    observed_cells = {
        (row["dataset"], row["stride"], row["seed"]) for row in remote
    }
    if observed_cells != expected_cells or len(remote) != len(expected_cells):
        failures.append("remote cell grid is incomplete or duplicated")
    if len({row["cell_id"] for row in remote}) != len(remote):
        failures.append("remote cell IDs are duplicated")

    arrays: dict[str, np.ndarray] = {}
    transforms: dict[tuple[str, int], np.ndarray] = {}
    count_hashes: dict[str, str] = {}
    for entry in protocol["datasets"]:
        dataset = entry["id"]
        path = ROOT / entry["path"]
        if DATA_FILES[dataset] != path:
            failures.append(f"{dataset}: analysis path differs from frozen path")
        if _sha256_file(path) != entry["file_sha256"]:
            failures.append(f"{dataset}: frozen input file hash mismatch")
        original = np.load(path, mmap_mode="r")
        arrays[dataset] = original
        if len(original) != 5_500_000 or original.dtype != np.uint8:
            failures.append(f"{dataset}: invalid frozen byte array")
        counts = np.bincount(original.astype(np.int64), minlength=256)
        count_hashes[dataset] = hashlib.sha256(counts.tobytes()).hexdigest()
        for stride in strides:
            transforms[(dataset, stride)] = stride_interleave(original, stride)

    learner = protocol["learner"]
    for row in remote:
        dataset, stride, seed = row["dataset"], row["stride"], row["seed"]
        label = f"{dataset}/stride{stride}/s{seed}"
        transformed = transforms[(dataset, stride)]
        if row["cell_id"] != f"V20/{dataset}/stride{stride}/s{seed}":
            failures.append(f"{label}: cell ID mismatch")
        if row["protocol_hash"] != protocol_hash:
            failures.append(f"{label}: protocol mismatch")
        if row["data_sha256"] != _sha256_tokens(transformed):
            failures.append(f"{label}: transformed data hash mismatch")
        if row["byte_counts_sha256"] != count_hashes[dataset]:
            failures.append(f"{label}: byte multiset mismatch")
        if row["n_bytes"] != len(arrays[dataset]):
            failures.append(f"{label}: corpus length mismatch")
        if "A10" not in row["remote"]["gpu"]:
            failures.append(f"{label}: expected remote A10/A10G GPU")

        grid = np.asarray(row["token_grid"], dtype=np.int64)
        values = np.asarray(row["val_ce_bits"], dtype=float)
        if (
            len(grid) != len(values)
            or grid[0] != 0
            or not np.all(np.diff(grid) > 0)
            or grid[-1] < learner["budget_tokens"] - learner["tokens_per_step"]
            or grid[-1] > learner["budget_tokens"]
            or not np.all(np.isfinite(values))
            or not np.all(values > 0)
        ):
            failures.append(f"{label}: invalid held-out learning curve")
        if not _close(row["initial_val_ce_bits"], values[0]):
            failures.append(f"{label}: zero-step CE mismatch")
        recomputed_curve = curve_metrics(grid.tolist(), values.tolist(), values[0])
        for key, value in recomputed_curve.items():
            recorded = row["floor_independent"][key]
            if (
                isinstance(value, float)
                and not _close(value, recorded)
                or not isinstance(value, float)
                and value != recorded
            ):
                failures.append(f"{label}: curve metric {key} mismatch")

    expected_rows = {(dataset, stride) for dataset in datasets for stride in strides}
    analysis_rows = {
        (row["dataset"], row["stride"]): row for row in result["dataset_stride_rows"]
    }
    if set(analysis_rows) != expected_rows or len(result["dataset_stride_rows"]) != 16:
        failures.append("integrated dataset-stride grid is incomplete or duplicated")

    rows_by_dataset: dict[str, list[dict]] = {}
    expected_pair_recovered = True
    for dataset in datasets:
        dataset_rows = []
        for stride in strides:
            label = f"{dataset}/stride{stride}"
            path = PROFILE_DIR / f"{dataset}__stride{stride}.json"
            profile = _strict_json(path)
            transformed = transforms[(dataset, stride)]
            positions, _ = _profile_positions(
                len(transformed), stride, protocol["spectrum"]["profile_positions"]
            )
            expected_pair = [stride, 2 * stride]
            if profile["protocol_hash"] != protocol_hash:
                failures.append(f"{label}: profile protocol mismatch")
            if profile["dataset"] != dataset or profile["stride"] != stride:
                failures.append(f"{label}: profile identity mismatch")
            if profile["data_sha256"] != _sha256_tokens(transformed):
                failures.append(f"{label}: profile data hash mismatch")
            if not profile["byte_counts_preserved"]:
                failures.append(f"{label}: profile byte multiset changed")
            if profile["position_sha256"] != hashlib.sha256(positions.tobytes()).hexdigest():
                failures.append(f"{label}: sampled profile positions mismatch")
            if (
                len(profile["pairs"]) != 10
                or any(
                    pair["n_positions"] != protocol["spectrum"]["profile_positions"]
                    or pair["q"] != learner["vocab"]
                    for pair in profile["pairs"]
                )
            ):
                failures.append(f"{label}: incomplete Fourier profile")
            if profile["best_pair_by_function_variance"] != expected_pair:
                expected_pair_recovered = False
                failures.append(f"{label}: planted dependency pair not recovered")

            row = analysis_rows[(dataset, stride)]
            if row["best_pair"] != expected_pair or row["expected_pair"] != expected_pair:
                failures.append(f"{label}: integrated best-pair mismatch")
            recomputed_spectrum = parseval_metrics(row["level_weights"], max(expected_pair))
            for key, value in recomputed_spectrum.items():
                if isinstance(value, list):
                    if not np.allclose(value, row[key], rtol=0.0, atol=1e-12):
                        failures.append(f"{label}: {key} recomputation mismatch")
                elif not _close(value, row[key]):
                    failures.append(f"{label}: {key} recomputation mismatch")

            cells = [
                cell
                for cell in remote
                if cell["dataset"] == dataset and cell["stride"] == stride
            ]
            medians = _median_cell_metrics(cells)
            for key, value in medians.items():
                if not _close(value, row[key]):
                    failures.append(f"{label}: median {key} mismatch")
            dataset_rows.append(row)
        rows_by_dataset[dataset] = dataset_rows

    exact = exact_blocked_permutation(rows_by_dataset)
    recorded_exact = result["primary_exact_blocked_test"]
    for key in ("observed_rho", "exact_two_sided_p"):
        if not _close(exact[key], recorded_exact[key]):
            failures.append(f"exact test {key} mismatch")
    for key in ("extreme_permutations", "total_permutations"):
        if exact[key] != recorded_exact[key]:
            failures.append(f"exact test {key} mismatch")
    for dataset in datasets:
        if not _close(exact["rho_by_dataset"][dataset], recorded_exact["rho_by_dataset"][dataset]):
            failures.append(f"{dataset}: exact-test rho mismatch")

    fresh_only = exact_blocked_permutation(
        {
            dataset: rows
            for dataset, rows in rows_by_dataset.items()
            if dataset != PILOT_DATASET
        }
    )
    recorded_fresh = result["fresh_corpora_only_sensitivity"]
    if recorded_fresh["excluded_pilot_dataset"] != PILOT_DATASET:
        failures.append("fresh-only sensitivity excluded the wrong pilot")
    for key in ("observed_rho", "exact_two_sided_p"):
        if not _close(fresh_only[key], recorded_fresh[key]):
            failures.append(f"fresh-only sensitivity {key} mismatch")
    for key in ("extreme_permutations", "total_permutations"):
        if fresh_only[key] != recorded_fresh[key]:
            failures.append(f"fresh-only sensitivity {key} mismatch")

    seed_specific = _seed_specific_rhos(rows_by_dataset, remote, seeds)
    for seed in seeds:
        key = str(seed)
        if not _close(
            seed_specific[key]["mean_within_dataset_rho"],
            result["seed_specific_blocked_rhos"][key]["mean_within_dataset_rho"],
        ):
            failures.append(f"seed {seed}: blocked rho mismatch")

    directions = {
        dataset: rows[-1]["final_ce_fraction"] - rows[0]["final_ce_fraction"]
        for dataset, rows in rows_by_dataset.items()
    }
    positive_directions = sum(value > 0 for value in directions.values())
    all_seed_positive = all(
        row["mean_within_dataset_rho"] > 0 for row in seed_specific.values()
    )
    support = (
        exact["observed_rho"] >= 0.70
        and exact["exact_two_sided_p"] < 0.05
        and positive_directions >= 3
        and all_seed_positive
    )
    decision = (
        "SUPPORTED"
        if support
        else "REFUTED"
        if exact["observed_rho"] <= 0 or positive_directions < 3
        else "INCONCLUSIVE"
    )
    if decision != result["confirmatory_verdict"]:
        failures.append("frozen decision rule was not applied mechanically")
    if result["compute"]["remote_gpu_cells"] != 48 or len(remote) != 48:
        failures.append("remote compute accounting mismatch")
    if result["compute"]["local_training_cells"] != 0:
        failures.append("unexpected local training")

    audit = {
        "status": "PASS" if not failures else "FAIL",
        "protocol_hash": protocol_hash,
        "source_protocol_hash": source_hash,
        "failures": failures,
        "confirmatory_verdict": result["confirmatory_verdict"],
        "remote_gpu_cells": len(remote),
        "local_training_cells": result["compute"]["local_training_cells"],
        "total_remote_cell_wallclock_seconds": result["compute"][
            "total_remote_cell_wallclock_seconds"
        ],
        "input_file_hashes_match": not any("input file hash" in item for item in failures),
        "same_byte_multiset_all_cells": not any("byte multiset" in item for item in failures),
        "all_16_dependency_pairs_recovered": expected_pair_recovered,
        "parseval_metrics_recomputed": not any("recomputation mismatch" in item for item in failures),
        "primary_blocked_rho": exact["observed_rho"],
        "exact_two_sided_p": exact["exact_two_sided_p"],
        "exact_permutations": exact["total_permutations"],
        "fresh_corpora_only_blocked_rho": fresh_only["observed_rho"],
        "fresh_corpora_only_exact_two_sided_p": fresh_only["exact_two_sided_p"],
        "positive_endpoint_directions": positive_directions,
        "all_seed_specific_blocked_rhos_positive": all_seed_positive,
    }
    OUT.mkdir(parents=True, exist_ok=True)
    (OUT / "audit.json").write_text(json.dumps(audit, indent=2))
    if failures:
        raise SystemExit("v2.0 audit FAIL: " + "; ".join(failures))
    print("v2.0 audit PASS")


if __name__ == "__main__":
    main()
