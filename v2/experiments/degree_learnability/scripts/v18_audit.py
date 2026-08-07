"""Fail-closed audit for the prospective v1.8 natural-text experiment."""

from __future__ import annotations

import json
import sys
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).parent.parent))
sys.path.insert(0, str(Path(__file__).parent))

from v18_real_natural import (
    ALL_DATASETS,
    CELL_DIR,
    CORPUS_TOKENS,
    FRESH,
    OUT,
    PROFILE_DIR,
    ROOT,
    SOURCE_FILES,
    _sha256_file,
    _sha256_tokens,
    decoded_path,
    load_protocol,
)


def _strict_json(path: Path) -> dict:
    return json.loads(
        path.read_text(),
        parse_constant=lambda value: (_ for _ in ()).throw(
            ValueError(f"non-finite JSON constant {value} in {path}")
        ),
    )


def main() -> None:
    protocol = load_protocol()
    result_path = OUT / "integrated_analysis.json"
    result = _strict_json(result_path)
    v14_audit = _strict_json(ROOT / "runs/local/v14_text_anova/audit.json")
    v17_audit = _strict_json(ROOT / "runs/local/v17_local_geometry/audit.json")
    failures = []

    if result["protocol_hash"] != protocol["protocol_hash"]:
        failures.append("result protocol hash mismatch")
    sources = protocol["source_protocol_hashes"]
    if (
        v14_audit["status"] != "PASS"
        or v14_audit["protocol_hash"] != sources["text_v1_4"]
    ):
        failures.append("v1.4 source audit/hash mismatch")
    if (
        v17_audit["status"] != "PASS"
        or v17_audit["protocol_hash"] != sources["geometry_v1_7"]
    ):
        failures.append("v1.7 source audit/hash mismatch")

    for dataset in FRESH:
        metadata = _strict_json(OUT / f"{dataset}__data.json")
        ids_path, tokenizer_path = SOURCE_FILES[dataset]
        if metadata["source_ids_sha256"] != _sha256_file(ids_path):
            failures.append(f"{dataset}: token-id source hash mismatch")
        if metadata["tokenizer_sha256"] != _sha256_file(tokenizer_path):
            failures.append(f"{dataset}: tokenizer hash mismatch")
        tokens = np.load(decoded_path(dataset), mmap_mode="r")
        if len(tokens) != CORPUS_TOKENS or tokens.dtype != np.uint8:
            failures.append(f"{dataset}: decoded byte stream shape/dtype mismatch")
        if metadata["byte_stream_sha256"] != _sha256_tokens(tokens):
            failures.append(f"{dataset}: decoded byte stream hash mismatch")

        profile = _strict_json(PROFILE_DIR / f"{dataset}.json")
        if profile["protocol_hash"] != protocol["protocol_hash"]:
            failures.append(f"{dataset}: profile protocol hash mismatch")
        if profile["data_sha256"] != metadata["byte_stream_sha256"]:
            failures.append(f"{dataset}: profile data hash mismatch")
        if len(profile["pairs"]) != 10:
            failures.append(f"{dataset}: expected ten lag-pair profiles")
        if any(pair["n_positions"] != 250_000 for pair in profile["pairs"]):
            failures.append(f"{dataset}: profile position count mismatch")
        if "no q-by-q-by-q" not in profile["memory_strategy"]:
            failures.append(f"{dataset}: memory-safe profile strategy not recorded")

        for seed in protocol["execution"]["seeds"]:
            directory = CELL_DIR / f"V18__{dataset}__s{seed}"
            manifest = _strict_json(directory / "manifest.json")
            metrics = _strict_json(directory / "metrics.json")
            if manifest["protocol_hash"] != protocol["protocol_hash"]:
                failures.append(f"{dataset}/s{seed}: manifest protocol mismatch")
            if manifest.get("data_sha256") != metadata["byte_stream_sha256"]:
                failures.append(f"{dataset}/s{seed}: manifest data hash mismatch")
            if metrics.get("data_sha256") != metadata["byte_stream_sha256"]:
                failures.append(f"{dataset}/s{seed}: metrics data hash mismatch")
            if (
                metrics.get("sequence_semantics")
                != protocol["sequence_semantics"]["cyclic_reuse"]
            ):
                failures.append(f"{dataset}/s{seed}: invalid sequence semantics")
            if metrics.get("initial_val_ce_bits") is None:
                failures.append(f"{dataset}/s{seed}: missing true zero-step CE")
            if (
                len(metrics["token_grid"]) < protocol["learner"]["checkpoints"]
                or len(metrics["token_grid"]) != len(metrics["val_ce_bits"])
                or metrics["token_grid"][0] != 0
            ):
                failures.append(f"{dataset}/s{seed}: checkpoint grid mismatch")

    if len(result["datasets"]) != len(ALL_DATASETS):
        failures.append("expected four natural datasets")
    if len(result["cells"]) != 12:
        failures.append("expected twelve three-seed cells including valid reuse")
    if sum(row["execution"] == "fresh" for row in result["cells"]) != 6:
        failures.append("expected six fresh cells")
    if any(row["best_pair"] != [1, 2] for row in result["datasets"]):
        failures.append("selected-pair radius is not constant as reported")
    if result["primary"]["decision"] not in {
        "SUPPORTED",
        "REFUTED",
        "INCONCLUSIVE",
    }:
        failures.append("invalid decision label")
    if result["new_profiles"] != 2 or result["new_training_cells"] != 6:
        failures.append("new-compute accounting mismatch")
    if result["invalid_pre_v1_4_cells_reused"] != 0:
        failures.append("invalid pre-v1.4 language evidence reused")
    if result["invalid_m8_image_cells_reused"] != 0:
        failures.append("invalid M8 image evidence reused")

    audit = {
        "status": "PASS" if not failures else "FAIL",
        "protocol_hash": protocol["protocol_hash"],
        "source_protocol_hashes": sources,
        "failures": failures,
        "natural_datasets": len(result["datasets"]),
        "valid_reused_cells": 6,
        "fresh_training_cells": 6,
        "fresh_profiles": 2,
        "profile_pairs_per_fresh_dataset": 10,
        "all_training_reads_contiguous": not any(
            "sequence semantics" in failure for failure in failures
        ),
        "invalid_language_cells_reused": 0,
        "invalid_image_cells_reused": 0,
    }
    (OUT / "audit.json").write_text(json.dumps(audit, indent=2))
    if failures:
        raise SystemExit("v1.8 audit FAIL: " + "; ".join(failures))
    print("v1.8 audit PASS")


if __name__ == "__main__":
    main()
