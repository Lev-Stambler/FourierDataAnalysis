"""Fail-closed audit for the frozen v2.2 hard-domain H100 experiment."""

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
    _median_cell_metrics,
    _rank_correlation,
    _seed_specific_rhos,
    exact_blocked_permutation,
)
from v22_analyze import OUT, PROFILE_DIR, load_protocol

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


def _positions(n_tokens: int, stride: int, n_positions: int) -> np.ndarray:
    n_times = n_positions // stride
    sampled_times = np.linspace(
        3, n_tokens // stride - 1, num=n_times, dtype=np.int64
    )
    return (
        sampled_times[:, None] * stride + np.arange(stride)[None, :]
    ).reshape(-1)


def main() -> None:
    protocol = load_protocol()
    protocol_hash = protocol["protocol_hash"]
    result = _strict_json(OUT / "integrated_analysis.json")
    remote = _strict_json(OUT / "remote_results.json")
    manifest = _strict_json(OUT / "data_manifest.json")
    source_audit = _strict_json(ROOT / "runs/local/v21_predictor_selection/audit.json")
    failures: list[str] = []

    if result["protocol_hash"] != protocol_hash:
        failures.append("integrated result protocol hash mismatch")
    if (
        source_audit["status"] != "PASS"
        or source_audit["protocol_hash"] != protocol["source_protocol_hash"]
    ):
        failures.append("v2.1 source audit/hash mismatch")

    datasets = [entry["id"] for entry in protocol["datasets"]]
    strides = protocol["intervention"]["strides"]
    seeds = protocol["replication"]["seeds"]
    metadata = {row["dataset"]: row for row in manifest}
    if set(metadata) != set(datasets) or len(manifest) != 3:
        failures.append("data manifest is incomplete or duplicated")

    arrays: dict[str, np.ndarray] = {}
    transforms: dict[tuple[str, int], np.ndarray] = {}
    count_hashes: dict[str, str] = {}
    for entry in protocol["datasets"]:
        dataset = entry["id"]
        meta = metadata[dataset]
        path = ROOT / entry["path"]
        if _sha256_file(path) != entry["file_sha256"]:
            failures.append(f"{dataset}: frozen input file hash mismatch")
        if meta["output_file_sha256"] != entry["file_sha256"]:
            failures.append(f"{dataset}: data-manifest file hash mismatch")
        original = np.load(path, mmap_mode="r")
        if len(original) != 8_000_000 or original.dtype != np.uint8:
            failures.append(f"{dataset}: invalid frozen byte array")
        if _sha256_tokens(original) != entry["byte_stream_sha256"]:
            failures.append(f"{dataset}: frozen byte-stream hash mismatch")
        if not meta["divisible_by_16"]:
            failures.append(f"{dataset}: corpus is not divisible by all strides")
        if "source_archive" in meta:
            if _sha256_file(ROOT / meta["source_archive"]) != meta[
                "source_archive_sha256"
            ]:
                failures.append(f"{dataset}: source archive hash mismatch")
        else:
            records = []
            source_dir = ROOT / "dlx/data_cache/v22_sources/rfc_text"
            for record in meta["source_documents"]:
                value = (source_dir / f"rfc{record['rfc']}.txt").read_bytes()
                value_hash = hashlib.sha256(value).hexdigest()
                if value_hash != record["sha256"]:
                    failures.append(f"RFC {record['rfc']}: source hash mismatch")
                records.append(record)
            if hashlib.sha256(json.dumps(records, sort_keys=True).encode()).hexdigest() != meta[
                "source_set_sha256"
            ]:
                failures.append("RFC source-set hash mismatch")
        arrays[dataset] = original
        counts = np.bincount(original.astype(np.int64), minlength=256)
        count_hashes[dataset] = hashlib.sha256(counts.tobytes()).hexdigest()
        for stride in strides:
            transforms[(dataset, stride)] = stride_interleave(original, stride)

    expected_training = {
        (dataset, stride, seed)
        for dataset in datasets
        for stride in strides
        for seed in seeds
    }
    observed_training = {
        (row["dataset"], row["stride"], row["seed"]) for row in remote
    }
    if observed_training != expected_training or len(remote) != 36:
        failures.append("remote training grid is incomplete or duplicated")
    if len({row["cell_id"] for row in remote}) != len(remote):
        failures.append("remote cell IDs are duplicated")

    learner = protocol["learner"]
    for row in remote:
        dataset, stride, seed = row["dataset"], row["stride"], row["seed"]
        label = f"{dataset}/stride{stride}/s{seed}"
        transformed = transforms[(dataset, stride)]
        if row["cell_id"] != f"V22/{dataset}/stride{stride}/s{seed}":
            failures.append(f"{label}: cell ID mismatch")
        if row["protocol_hash"] != protocol_hash:
            failures.append(f"{label}: protocol mismatch")
        if row["data_sha256"] != _sha256_tokens(transformed):
            failures.append(f"{label}: transformed data hash mismatch")
        if row["byte_counts_sha256"] != count_hashes[dataset]:
            failures.append(f"{label}: byte multiset mismatch")
        if row["n_bytes"] != len(arrays[dataset]):
            failures.append(f"{label}: corpus length mismatch")
        if "H100" not in row["remote"]["gpu"]:
            failures.append(f"{label}: expected remote H100 GPU")
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
            failures.append(f"{label}: invalid held-out curve")
        if not _close(row["initial_val_ce_bits"], values[0]):
            failures.append(f"{label}: zero-step CE mismatch")
        recomputed = curve_metrics(grid.tolist(), values.tolist(), values[0])
        for key, value in recomputed.items():
            recorded = row["floor_independent"][key]
            if (
                isinstance(value, float)
                and not _close(value, recorded)
                or not isinstance(value, float)
                and value != recorded
            ):
                failures.append(f"{label}: curve metric {key} mismatch")

    rows = {
        (row["dataset"], row["stride"]): row for row in result["dataset_stride_rows"]
    }
    if len(rows) != 12:
        failures.append("integrated dataset-stride grid is incomplete or duplicated")
    rows_by_dataset: dict[str, list[dict]] = {}
    for dataset in datasets:
        dataset_rows = []
        for stride in strides:
            label = f"{dataset}/stride{stride}"
            profile = _strict_json(PROFILE_DIR / f"{dataset}__stride{stride}.json")
            transformed = transforms[(dataset, stride)]
            expected_pair = [stride, 2 * stride]
            positions = _positions(
                len(transformed), stride, protocol["spectrum"]["profile_positions"]
            )
            if profile["cell_id"] != f"V22P/{dataset}/stride{stride}":
                failures.append(f"{label}: profile cell ID mismatch")
            if profile["protocol_hash"] != protocol_hash:
                failures.append(f"{label}: profile protocol mismatch")
            if profile["data_sha256"] != _sha256_tokens(transformed):
                failures.append(f"{label}: profile data hash mismatch")
            if not profile["byte_counts_preserved"]:
                failures.append(f"{label}: profile byte multiset changed")
            if profile["position_sha256"] != hashlib.sha256(positions.tobytes()).hexdigest():
                failures.append(f"{label}: profile position hash mismatch")
            if (
                len(profile["pairs"]) != 1
                or profile["pairs"][0]["lags"] != expected_pair
                or profile["pairs"][0]["n_positions"]
                != protocol["spectrum"]["profile_positions"]
            ):
                failures.append(f"{label}: invalid preregistered-pair profile")
            row = rows[(dataset, stride)]
            spectrum = profile["pairs"][0]["conditional_fourier_spectrum"]
            recomputed_spectrum = parseval_metrics(
                spectrum["level_weights"], 2 * stride
            )
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
            for key, value in _median_cell_metrics(cells).items():
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
    seed_specific = _seed_specific_rhos(rows_by_dataset, remote, seeds)
    all_seed_positive = all(
        row["mean_within_dataset_rho"] > 0 for row in seed_specific.values()
    )
    directions = {
        dataset: dataset_rows[-1]["final_ce_fraction"]
        - dataset_rows[0]["final_ce_fraction"]
        for dataset, dataset_rows in rows_by_dataset.items()
    }
    positive_directions = sum(value > 0 for value in directions.values())
    supported = (
        exact["observed_rho"] >= 0.80
        and exact["exact_two_sided_p"] < 0.05
        and positive_directions == 3
        and all_seed_positive
    )
    verdict = (
        "SUPPORTED"
        if supported
        else "REFUTED"
        if exact["observed_rho"] <= 0 or positive_directions < 2
        else "INCONCLUSIVE"
    )
    if verdict != result["verdict"]:
        failures.append("frozen verdict rule was not applied mechanically")
    area_rhos = {
        dataset: _rank_correlation(
            [row["geometric_moment_total"] for row in dataset_rows],
            [row["normalized_curve_area"] for row in dataset_rows],
        )
        for dataset, dataset_rows in rows_by_dataset.items()
    }
    mean_area = float(np.mean(list(area_rhos.values())))
    diagnostic = protocol["cleanliness_diagnostic"]
    cleaner = (
        all(value >= 0.80 for value in exact["rho_by_dataset"].values())
        and exact["observed_rho"] > diagnostic["v2_1_final_rho"]
        and mean_area > diagnostic["v2_1_area_rho"]
    )
    if cleaner != result["cleanliness_diagnostic"]["cleaner_than_v2_1"]:
        failures.append("cleanliness diagnostic mismatch")
    if result["compute"]["remote_h100_training_cells"] != 36:
        failures.append("remote H100 compute accounting mismatch")
    if result["compute"]["maximum_concurrent_h100s"] != 1:
        failures.append("single-H100 cap accounting mismatch")
    if result["compute"]["remote_cpu_profile_cells"] != 12:
        failures.append("remote profile accounting mismatch")
    if result["compute"]["local_training_cells"] != 0:
        failures.append("unexpected local training")

    audit = {
        "status": "PASS" if not failures else "FAIL",
        "protocol_hash": protocol_hash,
        "source_protocol_hash": protocol["source_protocol_hash"],
        "failures": failures,
        "verdict": result["verdict"],
        "remote_h100_training_cells": len(remote),
        "maximum_concurrent_h100s": 1,
        "remote_cpu_profile_cells": len(list(PROFILE_DIR.glob("*.json"))),
        "local_training_cells": 0,
        "local_profile_cells": 0,
        "all_source_and_input_hashes_match": not any("hash mismatch" in item for item in failures),
        "same_byte_multiset_all_cells": not any("byte multiset" in item for item in failures),
        "parseval_metrics_recomputed": not any("recomputation mismatch" in item for item in failures),
        "primary_blocked_rho": exact["observed_rho"],
        "exact_two_sided_p": exact["exact_two_sided_p"],
        "positive_endpoint_directions": positive_directions,
        "all_seed_specific_blocked_rhos_positive": all_seed_positive,
        "secondary_mean_curve_area_rho": mean_area,
        "cleaner_than_v2_1": cleaner,
    }
    (OUT / "audit.json").write_text(json.dumps(audit, indent=2))
    if failures:
        raise SystemExit("v2.2 audit FAIL: " + "; ".join(failures))
    print("v2.2 audit PASS")


if __name__ == "__main__":
    main()
