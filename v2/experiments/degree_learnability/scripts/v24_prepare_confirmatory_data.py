"""Materialize the untouched, hash-pinned v2.4 confirmatory byte corpora."""

from __future__ import annotations

import argparse
import gzip
import hashlib
import json
import tarfile
import urllib.error
import urllib.request
from pathlib import Path
from xml.etree import ElementTree

import numpy as np

ROOT = Path(__file__).parent.parent
CACHE = ROOT / "dlx/data_cache"
SOURCE_DIR = CACHE / "v24_sources"
OUT = ROOT / "runs/local/v24_spectrum_predictor"
DATA_DIR = OUT / "confirmatory_data"
N_BYTES = 2_000_000

REPOSITORIES = {
    "mdn_docs": {
        "repo": "mdn/content",
        "commit": "236238ccebcda8ad6eb68bee1f3aaea720c07b11",
        "suffixes": [".md"],
        "paths": ["/files/en-us/"],
    },
    "kubernetes_docs": {
        "repo": "kubernetes/website",
        "commit": "30e44e23d4dca48f4ee5f0c74574016f5c4d8130",
        "suffixes": [".md"],
        "paths": ["/content/en/docs/"],
    },
    "rust_reference_docs": {
        "repo": "rust-lang/rust",
        "commit": "29483883eed69d5fb4db01964cdf2af4d86e9cb2",
        "suffixes": [".md"],
        "paths": ["/src/doc/"],
    },
    "go_source": {
        "repo": "golang/go",
        "commit": "6e676ab2b809d46623acb5988248d95d1eb7939c",
        "suffixes": [".go"],
        "paths": ["/src/"],
    },
    "typescript_source": {
        "repo": "microsoft/TypeScript",
        "commit": "5be33469d551655d878876faa9e30aa3b49f8ee9",
        "suffixes": [".ts"],
        "paths": ["/src/"],
    },
    "llvm_cpp_source": {
        "repo": "llvm/llvm-project",
        "commit": "023ec9011c9a92cfa8922030eb266d66a10f78f8",
        "suffixes": [".cpp", ".h"],
        "paths": ["/llvm/"],
    },
    "coq_source": {
        "repo": "rocq-prover/rocq",
        "commit": "a8913306300b6a90c2f991e287cbb7b562509b39",
        "suffixes": [".v"],
        "paths": ["/theories/", "/test-suite/"],
    },
    "julia_source": {
        "repo": "JuliaLang/julia",
        "commit": "9615af0f269df4d371b8010e9507ed5bae86103b",
        "suffixes": [".jl"],
        "paths": ["/base/", "/stdlib/"],
    },
    "ghc_haskell_source": {
        "repo": "ghc/ghc",
        "commit": "71791bc3284756a960a3367afa2c0aef07f09353",
        "suffixes": [".hs"],
        "paths": ["/compiler/"],
    },
    "postgresql_source": {
        "repo": "postgres/postgres",
        "commit": "3d6a828938a5fa0444275d3d2f67b64ec3199eb7",
        "suffixes": [".c", ".h", ".sql"],
        "paths": ["/src/"],
    },
}

SPECIAL = ("rfc_legacy", "pubmed_independent")


def _sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _download(url: str, path: Path) -> None:
    if path.exists():
        return
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".partial")
    request = urllib.request.Request(url, headers={"User-Agent": "dlx-v2.4/1.0"})
    with (
        urllib.request.urlopen(request, timeout=180) as response,
        temporary.open("wb") as handle,
    ):
        while chunk := response.read(1024 * 1024):
            handle.write(chunk)
    temporary.replace(path)


def _append(payload: bytearray, value: bytes) -> bool:
    remaining = N_BYTES - len(payload)
    if remaining <= 0:
        return True
    payload.extend(value[:remaining])
    return len(payload) == N_BYTES


def _repository_payload(dataset: str, spec: dict) -> tuple[bytes, dict]:
    archive = SOURCE_DIR / f"{dataset}-{spec['commit']}.tar.gz"
    url = f"https://codeload.github.com/{spec['repo']}/tar.gz/{spec['commit']}"
    _download(url, archive)
    payload = bytearray()
    records = []
    suffixes = tuple(spec["suffixes"])
    with tarfile.open(archive, mode="r:gz") as handle:
        members = sorted(
            (
                member
                for member in handle.getmembers()
                if member.isfile()
                and member.name.lower().endswith(suffixes)
                and any(path in member.name for path in spec["paths"])
            ),
            key=lambda member: member.name,
        )
        for member in members:
            extracted = handle.extractfile(member)
            if extracted is None:
                continue
            value = extracted.read()
            records.append(
                {
                    "path": member.name,
                    "sha256": hashlib.sha256(value).hexdigest(),
                }
            )
            if _append(payload, f"\n\nFILE {member.name}\n".encode()) or _append(
                payload, value
            ):
                break
    return bytes(payload), {
        "source_kind": "hash-pinned GitHub repository archive",
        "source_url": url,
        "repository": spec["repo"],
        "commit": spec["commit"],
        "source_archive": str(archive.relative_to(ROOT)),
        "source_archive_sha256": _sha256_file(archive),
        "documents_consumed": len(records),
        "source_set_sha256": hashlib.sha256(
            json.dumps(records, sort_keys=True).encode()
        ).hexdigest(),
    }


def _rfc_payload() -> tuple[bytes, dict]:
    directory = SOURCE_DIR / "rfc_legacy"
    payload = bytearray()
    records = []
    for number in range(1000, 9000):
        path = directory / f"rfc{number}.txt"
        url = f"https://www.rfc-editor.org/rfc/rfc{number}.txt"
        if not path.exists():
            try:
                _download(url, path)
            except urllib.error.HTTPError as error:
                if error.code == 404:
                    continue
                raise
        value = path.read_bytes()
        records.append({"rfc": number, "sha256": hashlib.sha256(value).hexdigest()})
        if _append(payload, f"\n\nRFC {number}\n".encode()) or _append(payload, value):
            break
    return bytes(payload), {
        "source_kind": "ordered RFC Editor plain-text documents",
        "source_url_pattern": "https://www.rfc-editor.org/rfc/rfc{number}.txt",
        "source_range": [1000, records[-1]["rfc"]],
        "documents_consumed": len(records),
        "source_set_sha256": hashlib.sha256(
            json.dumps(records, sort_keys=True).encode()
        ).hexdigest(),
    }


def _pubmed_payload() -> tuple[bytes, dict]:
    url = "https://ftp.ncbi.nlm.nih.gov/pubmed/baseline/pubmed26n0002.xml.gz"
    archive = SOURCE_DIR / "pubmed26n0002.xml.gz"
    _download(url, archive)
    payload = bytearray()
    documents = 0
    with gzip.open(archive, "rb") as compressed:
        for _, element in ElementTree.iterparse(compressed, events=("end",)):
            if element.tag != "PubmedArticle":
                continue
            fields = []
            for tag in ("ArticleTitle", "AbstractText"):
                for node in element.iter(tag):
                    text = "".join(node.itertext()).strip()
                    if text:
                        fields.append(text)
            element.clear()
            if not fields:
                continue
            documents += 1
            if _append(payload, ("\n".join(fields) + "\n\n").encode()):
                break
    return bytes(payload), {
        "source_kind": "independent PubMed baseline shard titles and abstracts",
        "source_url": url,
        "source_archive": str(archive.relative_to(ROOT)),
        "source_archive_sha256": _sha256_file(archive),
        "documents_consumed": documents,
    }


def prepare(dataset: str) -> dict:
    DATA_DIR.mkdir(parents=True, exist_ok=True)
    output = CACHE / f"v24_{dataset}_bytes_n{N_BYTES}.npy"
    metadata_path = DATA_DIR / f"{dataset}.json"
    if output.exists():
        if not metadata_path.exists():
            raise FileExistsError(f"refusing unverified cached payload: {output}")
        metadata = json.loads(metadata_path.read_text())
        if metadata["output_file_sha256"] != _sha256_file(output):
            raise ValueError(f"cached payload hash mismatch: {output}")
        return metadata
    if dataset == "rfc_legacy":
        raw, provenance = _rfc_payload()
    elif dataset == "pubmed_independent":
        raw, provenance = _pubmed_payload()
    else:
        raw, provenance = _repository_payload(dataset, REPOSITORIES[dataset])
    if len(raw) != N_BYTES:
        raise RuntimeError(f"{dataset} produced {len(raw)} bytes; need {N_BYTES}")
    tokens = np.frombuffer(raw, dtype=np.uint8).copy()
    np.save(output, tokens)
    counts = np.bincount(tokens.astype(np.int64), minlength=256)
    probabilities = counts[counts > 0].astype(float) / counts.sum()
    metadata = {
        "dataset": dataset,
        **provenance,
        "canonicalization": "lexicographic file/document order with explicit separators",
        "output": str(output.relative_to(ROOT)),
        "output_file_sha256": _sha256_file(output),
        "byte_stream_sha256": hashlib.sha256(tokens.tobytes()).hexdigest(),
        "byte_counts_sha256": hashlib.sha256(counts.tobytes()).hexdigest(),
        "n_bytes": len(tokens),
        "dtype": str(tokens.dtype),
        "unigram_entropy_bits": float(-np.dot(probabilities, np.log2(probabilities))),
    }
    metadata_path.write_text(json.dumps(metadata, indent=2))
    return metadata


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--dataset", choices=(*REPOSITORIES, *SPECIAL))
    args = parser.parse_args()
    selected = [args.dataset] if args.dataset else [*REPOSITORIES, *SPECIAL]
    for dataset in selected:
        metadata = prepare(dataset)
        print(
            f"{dataset}: bytes={metadata['n_bytes']} "
            f"H1={metadata['unigram_entropy_bits']:.4f} "
            f"sha={metadata['output_file_sha256'][:12]}",
            flush=True,
        )
    records = [
        json.loads((DATA_DIR / f"{dataset}.json").read_text())
        for dataset in (*REPOSITORIES, *SPECIAL)
        if (DATA_DIR / f"{dataset}.json").exists()
    ]
    (OUT / "confirmatory_data_manifest.json").write_text(
        json.dumps(records, indent=2)
    )
    if len(records) == len(REPOSITORIES) + len(SPECIAL):
        manifest_hash = hashlib.sha256(
            json.dumps(records, sort_keys=True).encode()
        ).hexdigest()
        (OUT / "confirmatory_data_manifest.sha256").write_text(manifest_hash + "\n")
        print(f"complete manifest hash={manifest_hash}", flush=True)


if __name__ == "__main__":
    main()
