"""Fail-closed audit for the frozen v2.3 Transformer robustness test."""

from __future__ import annotations

import hashlib
import json
import sys
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).parent.parent))
sys.path.insert(0, str(Path(__file__).parent))

from v19_analyze import stride_interleave
from v20_analyze import _median_cell_metrics
from v23_analyze import OUT, SOURCE, exact_blocked_spearman, load_cells, load_protocol

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
    new_cells = _strict_json(OUT / "remote_results.json")
    source_audit = _strict_json(SOURCE / "audit.json")
    source_cells = _strict_json(SOURCE / "remote_results.json")
    failures: list[str] = []

    if result["protocol_hash"] != protocol["protocol_hash"]:
        failures.append("integrated result protocol hash mismatch")
    if (
        source_audit["status"] != "PASS"
        or source_audit["protocol_hash"] != protocol["source_protocol_hash"]
    ):
        failures.append("v2.1 source audit/hash mismatch")

    datasets = [row["id"] for row in protocol["datasets"]]
    strides = protocol["intervention"]["strides"]
    seeds = protocol["replication"]["seeds"]
    new_configurations = [
        row
        for row in protocol["configurations"]
        if row["source"] == "new v2.3 H100 cells"
    ]
    expected_new = {
        (configuration["id"], dataset, stride, seed)
        for configuration in new_configurations
        for dataset in datasets
        for stride in strides
        for seed in seeds
    }
    observed_new = {
        (row["configuration"], row["dataset"], row["stride"], row["seed"])
        for row in new_cells
    }
    if observed_new != expected_new or len(new_cells) != 216:
        failures.append("new H100 training grid is incomplete or duplicated")
    if len({row["cell_id"] for row in new_cells}) != len(new_cells):
        failures.append("new cell IDs are duplicated")

    source_subset = [
        row
        for row in source_cells
        if row["dataset"] in datasets and row["stride"] in strides
    ]
    if len(source_subset) != 72:
        failures.append("reused v2.1 baseline grid is incomplete")

    transformed: dict[tuple[str, int], np.ndarray] = {}
    count_hashes = {}
    for entry in protocol["datasets"]:
        path = ROOT / entry["path"]
        if _sha256_file(path) != entry["file_sha256"]:
            failures.append(f"{entry['id']}: source file hash mismatch")
        tokens = np.load(path, mmap_mode="r")
        if _sha256_tokens(tokens) != entry["byte_stream_sha256"]:
            failures.append(f"{entry['id']}: source byte hash mismatch")
        counts = np.bincount(tokens.astype(np.int64), minlength=256)
        count_hashes[entry["id"]] = hashlib.sha256(counts.tobytes()).hexdigest()
        for stride in strides:
            transformed[(entry["id"], stride)] = stride_interleave(tokens, stride)

    configurations = {row["id"]: row for row in new_configurations}
    shared = protocol["shared_training"]
    for row in new_cells:
        config_id = row["configuration"]
        dataset, stride, seed = row["dataset"], row["stride"], row["seed"]
        label = f"{config_id}/{dataset}/stride{stride}/s{seed}"
        configuration = configurations[config_id]
        if row["cell_id"] != f"V23/{label}":
            failures.append(f"{label}: cell ID mismatch")
        if row["protocol_hash"] != protocol["protocol_hash"]:
            failures.append(f"{label}: protocol mismatch")
        if row["data_sha256"] != _sha256_tokens(transformed[(dataset, stride)]):
            failures.append(f"{label}: transformed data hash mismatch")
        if row["byte_counts_sha256"] != count_hashes[dataset]:
            failures.append(f"{label}: byte multiset mismatch")
        if row["position_encoding"] != configuration["position_encoding"]:
            failures.append(f"{label}: positional configuration mismatch")
        for key in ("d_model", "n_layers", "n_heads"):
            if row[key] != configuration[key]:
                failures.append(f"{label}: {key} mismatch")
        if row["data_seed_protocol_hash"] != protocol["source_protocol_hash"]:
            failures.append(f"{label}: data-seed protocol mismatch")
        if row["data_seed_key"] != f"V21/{dataset}/stride{stride}/s{seed}":
            failures.append(f"{label}: data-seed key mismatch")
        if "H100" not in row["remote"]["gpu"]:
            failures.append(f"{label}: expected H100 execution")
        grid = np.asarray(row["token_grid"], dtype=np.int64)
        values = np.asarray(row["val_ce_bits"], dtype=float)
        if (
            len(grid) != len(values)
            or grid[0] != 0
            or not np.all(np.diff(grid) > 0)
            or grid[-1] < shared["budget_tokens"] - shared["tokens_per_step"]
            or grid[-1] > shared["budget_tokens"]
            or not np.all(np.isfinite(values))
            or not np.all(values > 0)
        ):
            failures.append(f"{label}: invalid held-out curve")
            continue
        if not _close(row["initial_val_ce_bits"], values[0]):
            failures.append(f"{label}: initial CE mismatch")
        recomputed = curve_metrics(grid.tolist(), values.tolist(), values[0])
        for key, value in recomputed.items():
            recorded = row["floor_independent"][key]
            if isinstance(value, float):
                if not _close(value, recorded):
                    failures.append(f"{label}: curve metric {key} mismatch")
            elif value != recorded:
                failures.append(f"{label}: curve metric {key} mismatch")

    all_cells = load_cells(protocol)
    rows = {
        (row["configuration"], row["dataset"], row["stride"]): row
        for row in result["configuration_dataset_stride_rows"]
    }
    if len(rows) != 96:
        failures.append("integrated configuration/corpus/stride grid is incomplete")
    supported_count = 0
    all_positive = True
    for configuration in protocol["configurations"]:
        config_id = configuration["id"]
        config_cells = [
            row for row in all_cells if row["configuration"] == config_id
        ]
        rows_by_dataset = {}
        for dataset in datasets:
            dataset_rows = []
            for stride in strides:
                label = f"{config_id}/{dataset}/stride{stride}"
                selected = [
                    row
                    for row in config_cells
                    if row["dataset"] == dataset and row["stride"] == stride
                ]
                if len(selected) != 3:
                    failures.append(f"{label}: expected three seeds")
                    continue
                recorded = rows[(config_id, dataset, stride)]
                for key, value in _median_cell_metrics(selected).items():
                    if not _close(value, recorded[key]):
                        failures.append(f"{label}: median {key} mismatch")
                dataset_rows.append(recorded)
            rows_by_dataset[dataset] = dataset_rows
        exact = exact_blocked_spearman(rows_by_dataset, "final_ce_fraction")
        recorded_result = result["configuration_results"][config_id]
        recorded_exact = recorded_result["primary_final_ce_test"]
        for key in ("observed_rho", "exact_two_sided_p"):
            if not _close(exact[key], recorded_exact[key]):
                failures.append(f"{config_id}: exact {key} mismatch")
        endpoints = {
            dataset: dataset_rows[-1]["final_ce_fraction"]
            - dataset_rows[0]["final_ce_fraction"]
            for dataset, dataset_rows in rows_by_dataset.items()
        }
        positive_endpoints = sum(value > 0 for value in endpoints.values())
        all_seed_positive = recorded_result["all_seed_specific_rhos_positive"]
        supported = (
            exact["observed_rho"] >= 0.60
            and exact["exact_two_sided_p"] < 0.05
            and positive_endpoints >= 4
            and all_seed_positive
        )
        if supported != recorded_result["supported"]:
            failures.append(f"{config_id}: support decision mismatch")
        supported_count += supported
        all_positive &= exact["observed_rho"] > 0

    if supported_count == 4:
        verdict = "SUPPORTED"
    elif supported_count >= 2 and all_positive:
        verdict = "MIXED"
    else:
        verdict = "REFUTED"
    if verdict != result["verdict"]:
        failures.append("overall verdict mismatch")

    audit = {
        "status": "FAIL" if failures else "PASS",
        "protocol_hash": protocol["protocol_hash"],
        "failures": failures,
        "checks": {
            "new_training_cells": len(new_cells),
            "reused_training_cells": len(source_subset),
            "integrated_rows": len(rows),
            "supported_configurations": supported_count,
            "local_training_cells": 0,
        },
    }
    (OUT / "audit.json").write_text(json.dumps(audit, indent=2))
    if failures:
        raise SystemExit("v2.3 audit FAIL:\n- " + "\n- ".join(failures))
    print("v2.3 audit PASS")


if __name__ == "__main__":
    main()
