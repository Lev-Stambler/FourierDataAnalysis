"""Materialize hash-pinned, previously unseen byte corpora for protocol v2.1."""

from __future__ import annotations

import argparse
import gzip
import hashlib
import json
import tarfile
import urllib.request
import zipfile
from pathlib import Path
from xml.etree import ElementTree

import numpy as np

ROOT = Path(__file__).parent.parent
CACHE = ROOT / "dlx/data_cache"
SOURCE_DIR = CACHE / "v21_sources"
OUT = ROOT / "runs/local/v21_predictor_selection"
N_BYTES = 5_499_984  # Largest value <=5.5M divisible by lcm(strides)=48.
NLTK_COMMIT = "550b6625bcef1f2abff2ff770a5a0d272c9c6b2a"

SOURCES = {
    "gutenberg_books": {
        "url": f"https://raw.githubusercontent.com/nltk/nltk_data/{NLTK_COMMIT}/packages/corpora/gutenberg.zip",
        "archive": "gutenberg.zip",
        "kind": "zip_plain",
        "suffixes": [".txt"],
    },
    "reuters_news": {
        "url": f"https://raw.githubusercontent.com/nltk/nltk_data/{NLTK_COMMIT}/packages/corpora/reuters.zip",
        "archive": "reuters.zip",
        "kind": "zip_plain",
        "exclude_names": ["README", ".DS_Store"],
    },
    "brown_balanced": {
        "url": f"https://raw.githubusercontent.com/nltk/nltk_data/{NLTK_COMMIT}/packages/corpora/brown.zip",
        "archive": "brown.zip",
        "kind": "brown_tagged",
    },
    "pubmed_abstracts": {
        "url": "https://ftp.ncbi.nlm.nih.gov/pubmed/baseline/pubmed26n0001.xml.gz",
        "archive": "pubmed26n0001.xml.gz",
        "kind": "pubmed_xml",
    },
    "cpython_source": {
        "url": "https://www.python.org/ftp/python/3.14.0/Python-3.14.0.tar.xz",
        "archive": "Python-3.14.0.tar.xz",
        "kind": "tar_code",
        "suffixes": [".py"],
    },
    "linux_c_source": {
        "url": "https://cdn.kernel.org/pub/linux/kernel/v6.x/linux-6.12.27.tar.xz",
        "archive": "linux-6.12.27.tar.xz",
        "kind": "tar_code",
        "suffixes": [".c", ".h"],
    },
}


def sha256_file(path: Path) -> str:
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
    request = urllib.request.Request(url, headers={"User-Agent": "dlx-v2.1/1.0"})
    with (
        urllib.request.urlopen(request, timeout=120) as response,
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


def _zip_plain(path: Path, spec: dict) -> tuple[bytes, int]:
    payload = bytearray()
    documents = 0
    suffixes = tuple(spec.get("suffixes", []))
    excluded = tuple(spec.get("exclude_names", []))
    with zipfile.ZipFile(path) as archive:
        names = sorted(
            name
            for name in archive.namelist()
            if not name.endswith("/")
            and (not suffixes or name.lower().endswith(suffixes))
            and not any(item in Path(name).name for item in excluded)
        )
        for name in names:
            value = archive.read(name)
            documents += 1
            if _append(payload, value) or _append(payload, b"\n\n"):
                break
    return bytes(payload), documents


def _brown_tagged(path: Path) -> tuple[bytes, int]:
    payload = bytearray()
    documents = 0
    with zipfile.ZipFile(path) as archive:
        names = sorted(
            name
            for name in archive.namelist()
            if not name.endswith("/")
            and Path(name).name.startswith(("c",))
            and len(Path(name).name) == 4
        )
        for name in names:
            raw = archive.read(name).decode("latin-1")
            words = []
            for token in raw.split():
                word, separator, _ = token.rpartition("/")
                words.append(word if separator else token)
            documents += 1
            if _append(payload, (" ".join(words) + "\n\n").encode("utf-8")):
                break
    return bytes(payload), documents


def _tar_code(path: Path, spec: dict) -> tuple[bytes, int]:
    payload = bytearray()
    documents = 0
    suffixes = tuple(spec["suffixes"])
    with tarfile.open(path, mode="r:xz") as archive:
        members = sorted(
            (
                member
                for member in archive.getmembers()
                if member.isfile() and member.name.lower().endswith(suffixes)
            ),
            key=lambda member: member.name,
        )
        for member in members:
            extracted = archive.extractfile(member)
            if extracted is None:
                continue
            documents += 1
            header = f"\n\n# FILE: {member.name}\n".encode()
            if _append(payload, header) or _append(payload, extracted.read()):
                break
    return bytes(payload), documents


def _pubmed_xml(path: Path) -> tuple[bytes, int]:
    payload = bytearray()
    documents = 0
    with gzip.open(path, "rb") as compressed:
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
            if _append(payload, ("\n".join(fields) + "\n\n").encode("utf-8")):
                break
    return bytes(payload), documents


def _decode_source(path: Path, spec: dict) -> tuple[bytes, int]:
    if spec["kind"] == "zip_plain":
        return _zip_plain(path, spec)
    if spec["kind"] == "brown_tagged":
        return _brown_tagged(path)
    if spec["kind"] == "tar_code":
        return _tar_code(path, spec)
    if spec["kind"] == "pubmed_xml":
        return _pubmed_xml(path)
    raise ValueError(f"unknown source kind: {spec['kind']}")


def prepare(dataset: str) -> dict:
    spec = SOURCES[dataset]
    archive = SOURCE_DIR / spec["archive"]
    output = CACHE / f"v21_{dataset}_bytes_n{N_BYTES}.npy"
    _download(spec["url"], archive)
    if output.exists():
        tokens = np.load(output, mmap_mode="r")
        documents = None
    else:
        raw, documents = _decode_source(archive, spec)
        if len(raw) < N_BYTES:
            raise RuntimeError(
                f"{dataset} produced {len(raw)} bytes; need exactly {N_BYTES}"
            )
        tokens = np.frombuffer(raw, dtype=np.uint8).copy()
        np.save(output, tokens)
    if len(tokens) != N_BYTES or tokens.dtype != np.uint8:
        raise ValueError(f"invalid prepared payload: {output}")
    counts = np.bincount(np.asarray(tokens, dtype=np.int64), minlength=256)
    probabilities = counts[counts > 0].astype(float) / counts.sum()
    metadata = {
        "dataset": dataset,
        "source_url": spec["url"],
        "source_archive": str(archive.relative_to(ROOT)),
        "source_archive_sha256": sha256_file(archive),
        "canonicalization": spec["kind"],
        "documents_consumed": documents,
        "output": str(output.relative_to(ROOT)),
        "output_file_sha256": sha256_file(output),
        "byte_stream_sha256": hashlib.sha256(
            np.ascontiguousarray(tokens).tobytes()
        ).hexdigest(),
        "byte_counts_sha256": hashlib.sha256(counts.tobytes()).hexdigest(),
        "n_bytes": len(tokens),
        "dtype": str(tokens.dtype),
        "unigram_entropy_bits": float(
            -(probabilities * np.log2(probabilities)).sum()
        ),
        "divisible_by_48": len(tokens) % 48 == 0,
    }
    return metadata


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--dataset", choices=tuple(SOURCES))
    args = parser.parse_args()
    selected = [args.dataset] if args.dataset else list(SOURCES)
    records = []
    for dataset in selected:
        metadata = prepare(dataset)
        records.append(metadata)
        print(
            f"{dataset}: bytes={metadata['n_bytes']} "
            f"H1={metadata['unigram_entropy_bits']:.4f} "
            f"sha={metadata['output_file_sha256'][:12]}",
            flush=True,
        )
    OUT.mkdir(parents=True, exist_ok=True)
    metadata_path = OUT / "data_manifest.json"
    existing = json.loads(metadata_path.read_text()) if metadata_path.exists() else []
    by_dataset = {row["dataset"]: row for row in existing}
    by_dataset.update({row["dataset"]: row for row in records})
    metadata_path.write_text(
        json.dumps([by_dataset[key] for key in SOURCES if key in by_dataset], indent=2)
    )


if __name__ == "__main__":
    main()
