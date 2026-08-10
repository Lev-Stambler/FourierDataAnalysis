"""Feasibility-select and materialize the source-disjoint v3.0 panel."""

from __future__ import annotations

import argparse
import hashlib
import json
import sys
from pathlib import Path

import numpy as np

ROOT = Path(__file__).parent.parent
sys.path.insert(0, str(ROOT))

from dlx.protocol.frozen import file_sha256, write_hash_once, write_json_once
from scripts.v27_prepare_data import source_payload

CANDIDATES = ROOT / "configs/source_candidates_v3.0.json"
OUT = ROOT / "runs/local/v30_architecture_spectrum"
DATA_DIR = OUT / "data"
PILOT_MANIFEST = ROOT / "runs/local/v28_random_windows/data_manifest.json"
PILOT_DATASETS = {
    "dickens_prose",
    "twain_prose",
    "doyle_prose",
    "hugo_prose",
    "pubmed_abstracts_3",
    "arxiv_articles",
    "pubmed_articles",
    "uspto_patents",
    "ansible_docs",
    "airflow_docs",
    "home_assistant_docs",
    "mongodb_docs",
    "perl5_language",
    "gcc_language",
    "groovy_language",
    "ballerina_language",
    "tensorflow_code",
    "arrow_code",
    "redis_code",
    "blender_code",
    "lean4_formal",
    "unimath_formal",
    "sel4_formal",
    "vst_formal",
}


def feasibility() -> dict:
    registry = json.loads(CANDIDATES.read_text())
    sources = registry["sources"]
    strata = sorted({row["stratum"] for row in sources})
    if len({row["id"] for row in sources}) != len(sources) or any(
        sum(row["stratum"] == stratum for row in sources) < 10 for stratum in strata
    ):
        raise ValueError(
            "v3.0 requires unique IDs and at least ten candidates per stratum"
        )
    limit = int(registry["bytes_per_corpus"])
    attempts = []
    selected = []
    for stratum in strata:
        for source in (row for row in sources if row["stratum"] == stratum):
            try:
                payload, record = source_payload(source, limit)
                feasible = len(payload) == limit
                attempts.append(
                    {
                        "id": source["id"],
                        "stratum": stratum,
                        "feasible": feasible,
                        "selected_bytes": len(payload),
                        "reason": None if feasible else "short_payload",
                    }
                )
                if feasible:
                    selected.append(record)
                    print(f"{stratum}/{source['id']}: selected", flush=True)
            except Exception as error:  # noqa: BLE001 - feasibility records source failures
                attempts.append(
                    {
                        "id": source["id"],
                        "stratum": stratum,
                        "feasible": False,
                        "selected_bytes": 0,
                        "reason": f"{type(error).__name__}: {error}",
                    }
                )
                print(f"{stratum}/{source['id']}: infeasible ({error})", flush=True)
            if sum(row["stratum"] == stratum for row in selected) == 8:
                break
        count = sum(row["stratum"] == stratum for row in selected)
        if count != 8:
            raise RuntimeError(
                f"{stratum}: selected {count} feasible sources, require 8"
            )
    result = {
        "status": "PASS",
        "candidate_registry_sha256": file_sha256(CANDIDATES),
        "bytes_per_corpus": limit,
        "attempts": attempts,
        "selected_sources": selected,
    }
    OUT.mkdir(parents=True, exist_ok=True)
    (OUT / "source_feasibility.json").write_text(json.dumps(result, indent=2) + "\n")
    return result


def _pilot_records() -> list[dict]:
    manifest = json.loads(PILOT_MANIFEST.read_text())
    records = []
    for row in manifest["corpora"]:
        if row["dataset"] not in PILOT_DATASETS:
            continue
        records.append(
            {
                "dataset": row["dataset"],
                "panel": "pilot",
                "stratum": row["stratum"],
                "path": row["path"],
                "n_bytes": row["n_bytes"],
                "file_sha256": row["file_sha256"],
                "byte_stream_sha256": row["byte_stream_sha256"],
                "source": "byte-identical v2.8 pilot reuse",
            }
        )
    if len(records) != 24:
        raise ValueError("the v3.0 pilot requires the exact 24-corpus v2.8 panel")
    return records


def materialize() -> dict:
    feasibility_path = OUT / "source_feasibility.json"
    report = json.loads(feasibility_path.read_text())
    if report["status"] != "PASS" or report["candidate_registry_sha256"] != file_sha256(
        CANDIDATES
    ):
        raise ValueError("source feasibility is absent or does not match the registry")
    limit = int(report["bytes_per_corpus"])
    records = _pilot_records()
    for selected in report["selected_sources"]:
        payload, metadata = source_payload(selected, limit)
        if len(payload) != limit:
            raise RuntimeError(f"short materialization: {selected['id']}")
        if metadata["source_artifacts"] != selected["source_artifacts"]:
            raise ValueError(f"source artifact changed: {selected['id']}")
        tokens = np.frombuffer(payload, dtype=np.uint8).copy()
        output = ROOT / f"dlx/data_cache/v30_{selected['id']}_bytes_n{limit}.npy"
        np.save(output, tokens)
        metadata.update(
            {
                "dataset": selected["id"],
                "panel": "confirmation",
                "n_bytes": limit,
                "path": str(output.relative_to(ROOT)),
                "file_sha256": file_sha256(output),
                "byte_stream_sha256": hashlib.sha256(tokens.tobytes()).hexdigest(),
            }
        )
        write_json_once(DATA_DIR / f"{selected['id']}.json", metadata)
        records.append(metadata)
        print(f"materialized {selected['id']}", flush=True)
    if len(records) != 72 or len({row["dataset"] for row in records}) != 72:
        raise ValueError("v3.0 requires 24 pilot and 48 confirmation corpora")
    result = {
        "status": "frozen before any v3.0 learner outcome",
        "candidate_registry_sha256": file_sha256(CANDIDATES),
        "source_feasibility_sha256": file_sha256(feasibility_path),
        "corpora": sorted(records, key=lambda row: (row["panel"], row["dataset"])),
    }
    digest = write_json_once(OUT / "data_manifest.json", result)
    write_hash_once(OUT / "data_manifest.sha256", digest)
    print(digest)
    return result


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("stage", choices=("feasibility", "materialize"))
    arguments = parser.parse_args()
    feasibility() if arguments.stage == "feasibility" else materialize()
