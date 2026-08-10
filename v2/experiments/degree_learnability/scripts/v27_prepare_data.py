"""Feasibility-audit and materialize the mixed-source v2.7 corpus panel."""

from __future__ import annotations

import argparse
import gzip
import hashlib
import json
import sys
import tarfile
import urllib.request
import xml.etree.ElementTree as ET
from pathlib import Path

import numpy as np

ROOT = Path(__file__).parent.parent
sys.path.insert(0, str(ROOT))

from dlx.protocol.frozen import (
    file_sha256,
    load_frozen_protocol,
    write_hash_once,
    write_json_once,
)

CANDIDATES = ROOT / "configs/source_candidates_v2.7.json"
PROTOCOL_PATH = ROOT / "configs/protocol_v2.7.json"
OUT = ROOT / "runs/local/v27_marginal_locality"
CACHE = ROOT / "dlx/data_cache/v27_sources"
DATA_DIR = OUT / "data"
FEASIBILITY_BYTES = 2_500_000


def _download(url: str, name: str) -> Path:
    CACHE.mkdir(parents=True, exist_ok=True)
    output = CACHE / name
    if not output.exists():
        partial = Path(f"{output}.partial")
        request = urllib.request.Request(url, headers={"User-Agent": "dlx-v2.7/1.0"})
        with (
            urllib.request.urlopen(request, timeout=600) as response,
            partial.open("wb") as handle,
        ):
            for chunk in iter(lambda: response.read(1024 * 1024), b""):
                handle.write(chunk)
        partial.replace(output)
    return output


def _add_document(
    payload: bytearray,
    records: list[dict],
    label: str,
    value: bytes,
    limit: int,
) -> None:
    if len(payload) >= limit or not value:
        return
    records.append({"document": label, "sha256": hashlib.sha256(value).hexdigest()})
    payload.extend(f"\n\nDOCUMENT {label}\n".encode())
    payload.extend(value)
    if len(payload) > limit:
        del payload[limit:]


def _github(source: dict, limit: int) -> tuple[bytes, list[dict], list[dict]]:
    url = f"https://codeload.github.com/{source['repo']}/tar.gz/{source['commit']}"
    archive = _download(url, f"{source['id']}-{source['commit']}.tar.gz")
    payload = bytearray()
    records: list[dict] = []
    suffixes = tuple(value.lower() for value in source["suffixes"])
    with tarfile.open(archive, "r:gz") as handle:
        members = sorted(
            (
                member
                for member in handle.getmembers()
                if member.isfile() and member.name.lower().endswith(suffixes)
            ),
            key=lambda member: member.name,
        )
        for member in members:
            extracted = handle.extractfile(member)
            if extracted is not None:
                _add_document(payload, records, member.name, extracted.read(), limit)
            if len(payload) >= limit:
                break
    artifacts = [{"url": url, "sha256": file_sha256(archive)}]
    return bytes(payload), records, artifacts


def _strip_gutenberg(value: bytes) -> bytes:
    text = value.decode("utf-8", errors="replace")
    start_markers = (
        "*** START OF THE PROJECT GUTENBERG",
        "***START OF THE PROJECT GUTENBERG",
    )
    end_markers = (
        "*** END OF THE PROJECT GUTENBERG",
        "***END OF THE PROJECT GUTENBERG",
    )
    start = min(
        (text.find(marker) for marker in start_markers if marker in text), default=-1
    )
    if start >= 0:
        start = text.find("\n", start) + 1
    else:
        start = 0
    end = min(
        (text.find(marker, start) for marker in end_markers if marker in text[start:]),
        default=len(text),
    )
    return text[start:end].encode()


def _gutenberg(source: dict, limit: int) -> tuple[bytes, list[dict], list[dict]]:
    payload = bytearray()
    records: list[dict] = []
    artifacts = []
    for document_id in source["document_ids"]:
        url = f"https://www.gutenberg.org/cache/epub/{document_id}/pg{document_id}.txt"
        path = _download(url, f"gutenberg-{document_id}.txt")
        artifacts.append({"url": url, "sha256": file_sha256(path)})
        _add_document(
            payload,
            records,
            f"gutenberg/{document_id}",
            _strip_gutenberg(path.read_bytes()),
            limit,
        )
        if len(payload) >= limit:
            break
    return bytes(payload), records, artifacts


def _pubmed(source: dict, limit: int) -> tuple[bytes, list[dict], list[dict]]:
    path = _download(source["url"], f"{source['id']}.xml.gz")
    payload = bytearray()
    records: list[dict] = []
    with gzip.open(path, "rb") as handle:
        for _, element in ET.iterparse(handle, events=("end",)):
            if element.tag != "PubmedArticle":
                continue
            pmid = element.findtext(".//PMID", default=str(len(records)))
            paragraphs = [
                "".join(node.itertext()) for node in element.findall(".//AbstractText")
            ]
            _add_document(
                payload,
                records,
                f"pubmed/{pmid}",
                "\n".join(paragraphs).encode(),
                limit,
            )
            element.clear()
            if len(payload) >= limit:
                break
    return (
        bytes(payload),
        records,
        [{"url": source["url"], "sha256": file_sha256(path)}],
    )


def _parquet(source: dict, limit: int) -> tuple[bytes, list[dict], list[dict]]:
    from pyarrow import parquet

    url = (
        f"https://huggingface.co/datasets/{source['repo']}/resolve/"
        f"{source['revision']}/{source['file']}"
    )
    path = _download(url, f"{source['id']}.parquet")
    payload = bytearray()
    records: list[dict] = []
    reader = parquet.ParquetFile(path)
    row_index = 0
    for batch in reader.iter_batches(batch_size=128, columns=source["text_fields"]):
        for row in batch.to_pylist():
            values = [str(row.get(field) or "") for field in source["text_fields"]]
            _add_document(
                payload,
                records,
                f"{source['id']}/{row_index}",
                "\n".join(values).encode(),
                limit,
            )
            row_index += 1
            if len(payload) >= limit:
                break
        if len(payload) >= limit:
            break
    return bytes(payload), records, [{"url": url, "sha256": file_sha256(path)}]


def source_payload(source: dict, limit: int) -> tuple[bytes, dict]:
    loaders = {
        "github": _github,
        "gutenberg": _gutenberg,
        "pubmed_xml": _pubmed,
        "parquet": _parquet,
    }
    payload, records, artifacts = loaders[source["kind"]](source, limit)
    return payload, {
        **source,
        "source_artifacts": artifacts,
        "documents_consumed": len(records),
        "source_set_sha256": hashlib.sha256(
            json.dumps(records, sort_keys=True).encode()
        ).hexdigest(),
        "canonicalization": "ordered documents, explicit DOCUMENT separators, exact byte prefix",
        "selected_bytes": len(payload),
    }


def feasibility() -> dict:
    registry = json.loads(CANDIDATES.read_text())
    sources = registry["sources"]
    if len(sources) != 24 or len({row["id"] for row in sources}) != 24:
        raise ValueError("v2.7 requires 24 unique source candidates")
    records = []
    for source in sources:
        payload, record = source_payload(source, FEASIBILITY_BYTES)
        if len(payload) != FEASIBILITY_BYTES:
            raise RuntimeError(
                f"{source['id']} yielded {len(payload)} bytes; require {FEASIBILITY_BYTES}"
            )
        records.append(record)
        print(
            f"{source['id']}: feasible ({record['documents_consumed']} documents)",
            flush=True,
        )
    result = {
        "status": "PASS",
        "minimum_selected_bytes": FEASIBILITY_BYTES,
        "sources": records,
    }
    (OUT / "source_feasibility.json").parent.mkdir(parents=True, exist_ok=True)
    (OUT / "source_feasibility.json").write_text(json.dumps(result, indent=2) + "\n")
    return result


def materialize() -> dict:
    protocol = load_frozen_protocol(PROTOCOL_PATH)
    corpus_spec = protocol["corpora"]
    if file_sha256(CANDIDATES) != corpus_spec["source_registry_sha256"]:
        raise ValueError("source registry changed after protocol freeze")
    feasibility_path = ROOT / corpus_spec["source_feasibility"]
    if file_sha256(feasibility_path) != corpus_spec["source_feasibility_sha256"]:
        raise ValueError("source feasibility report changed after protocol freeze")
    registry = {row["id"]: row for row in json.loads(CANDIDATES.read_text())["sources"]}
    feasible = {
        row["id"]: row for row in json.loads(feasibility_path.read_text())["sources"]
    }
    n_bytes = int(protocol["corpora"]["bytes_per_corpus"])
    records = []
    for selected in protocol["corpora"]["sources"]:
        source = {
            **registry[selected["id"]],
            "stratum": selected["stratum"],
            "source_artifacts": feasible[selected["id"]]["source_artifacts"],
        }
        payload, metadata = source_payload(source, n_bytes)
        if len(payload) != n_bytes:
            raise RuntimeError(f"short materialization: {source['id']}")
        expected = source["source_artifacts"]
        if any(artifact not in expected for artifact in metadata["source_artifacts"]):
            raise ValueError(f"source artifact hash changed: {source['id']}")
        tokens = np.frombuffer(payload, dtype=np.uint8).copy()
        output = ROOT / f"dlx/data_cache/v27_{source['id']}_bytes_n{n_bytes}.npy"
        np.save(output, tokens)
        metadata.update(
            {
                "n_bytes": n_bytes,
                "output": str(output.relative_to(ROOT)),
                "output_file_sha256": file_sha256(output),
                "byte_stream_sha256": hashlib.sha256(tokens.tobytes()).hexdigest(),
            }
        )
        write_json_once(DATA_DIR / f"{source['id']}.json", metadata)
        records.append(metadata)
        print(f"{source['id']}: {metadata['byte_stream_sha256'][:12]}", flush=True)
    manifest = {"protocol_hash": protocol["protocol_hash"], "corpora": records}
    digest = write_json_once(OUT / "data_manifest.json", manifest)
    write_hash_once(OUT / "data_manifest.sha256", digest)
    return manifest


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("stage", choices=("feasibility", "materialize"))
    args = parser.parse_args()
    feasibility() if args.stage == "feasibility" else materialize()
