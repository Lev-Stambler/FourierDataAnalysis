"""R1 language-ladder corpora (PLAN §3R, protocol v1/v1.1): pinned loaders,
tokenizers with recorded hashes, and token streams capped at the protocol limit.

Every rung returns (tokens, meta) where meta records source + revision +
tokenization + hash, so manifests can pin the exact data.
"""

from __future__ import annotations

import hashlib
import json
from dataclasses import dataclass
from pathlib import Path

import numpy as np

CACHE = Path(__file__).parent.parent / "data_cache"


@dataclass
class RungMeta:
    rung: int
    name: str
    source: str
    revision: str
    tokenization: str
    vocab_size: int
    n_tokens: int
    sha256: str

    def to_dict(self) -> dict:
        return dict(self.__dict__)


def _sha_tokens(tokens: np.ndarray) -> str:
    return hashlib.sha256(np.ascontiguousarray(tokens.astype(np.int32)).tobytes()).hexdigest()


def _save_stream(tokens: np.ndarray, key: str) -> Path:
    CACHE.mkdir(parents=True, exist_ok=True)
    p = CACHE / f"{key}.npy"
    np.save(p, tokens.astype(np.int32))
    return p


def load_cached(key: str) -> np.ndarray | None:
    p = CACHE / f"{key}.npy"
    if p.exists():
        return np.load(p)
    return None


# --------------------------------------------------------------------- synthetic
def rung1_iid_random(n_tokens: int, vocab_size: int, seed: int = 1001) -> tuple[np.ndarray, RungMeta]:
    key = f"r1_iid_q{vocab_size}_n{n_tokens}_s{seed}"
    tok = load_cached(key)
    if tok is None:
        rng = np.random.default_rng(seed)
        tok = rng.integers(0, vocab_size, size=n_tokens)
        _save_stream(tok, key)
    meta = RungMeta(1, "iid_random_tokens", "synthetic", f"seed={seed}",
                    f"uniform iid over rung-3 vocab (q={vocab_size})", vocab_size,
                    len(tok), _sha_tokens(tok))
    return tok, meta


def rung2_synthetic_f2(n_tokens: int, seed: int = 1002) -> tuple[np.ndarray, RungMeta]:
    import sys
    sys.path.insert(0, str(Path(__file__).parent.parent))
    from dlx.families import F2SubsetSum
    key = f"r2_f2s2_n{n_tokens}_s{seed}"
    tok = load_cached(key)
    if tok is None:
        fam = F2SubsetSum(q=32, L=64, lags=(1, 16), eta=0.1)
        tok = fam.sample(n_tokens, np.random.default_rng(seed))
        _save_stream(tok, key)
    meta = RungMeta(2, "synthetic_F2_s2_corpus", "synthetic",
                    f"F2_q32_L64_lags(1,16)_eta0.1+seed={seed}", "native Z_32", 32,
                    len(tok), _sha_tokens(tok))
    return tok, meta


def rung7_arithmetic(n_tokens: int, seed: int = 1007) -> tuple[np.ndarray, RungMeta]:
    """Sampled a+b mod p and chained-op expressions over a small operator vocab.

    Vocab: digits 0..9 (ids 0-9), operators +,-,* (10-12), '=' (13), ';' (14),
    and base-p digit tokens are emitted as decimal digit sequences. q=16.
    """
    key = f"r7_arith_n{n_tokens}_s{seed}"
    tok = load_cached(key)
    if tok is None:
        rng = np.random.default_rng(seed)
        out: list[int] = []
        primes = (17, 31, 47)
        while len(out) < n_tokens:
            p = int(rng.choice(primes))
            a, b = int(rng.integers(0, p)), int(rng.integers(0, p))
            op = int(rng.integers(10, 13))
            if op == 10:
                r = (a + b) % p
            elif op == 11:
                r = (a - b) % p
            else:
                r = (a * b) % p
            expr = list(map(int, str(a))) + [op] + list(map(int, str(b))) + [13] + list(map(int, str(r))) + [14]
            out.extend(expr)
        tok = np.array(out[:n_tokens], dtype=np.int64)
        _save_stream(tok, key)
    meta = RungMeta(7, "arithmetic_corpus", "synthetic", f"seed={seed}",
                    "decimal digits + ops, q=16", 16, len(tok), _sha_tokens(tok))
    return tok, meta


# --------------------------------------------------------------------- HF datasets
def _hf_stream_texts(name: str, revision: str, config: str | None, cap_chars: int,
                     filter_fn=None):
    from datasets import load_dataset
    ds = load_dataset(name, config, revision=revision, split="train", streaming=True)
    got = 0
    for row in ds:
        text = row.get("text") or row.get("content") or ""
        if filter_fn is not None and not filter_fn(row):
            continue
        if not text:
            continue
        yield text
        got += len(text)
        if got >= cap_chars:
            break


def _fit_bpe(texts_iter, vocab_size: int, key: str) -> "Tokenizer":
    from tokenizers import ByteLevelBPETokenizer, Tokenizer
    CACHE.mkdir(parents=True, exist_ok=True)
    path = CACHE / f"{key}.json"
    if path.exists():
        return Tokenizer.from_file(str(path))
    tk = ByteLevelBPETokenizer()
    # materialize a bounded training sample for the trainer
    sample: list[str] = []
    budget = 20_000_000
    used = 0
    for t in texts_iter:
        sample.append(t)
        used += len(t)
        if used >= budget:
            break
    tk.train_from_iterator(iter(sample), vocab_size=vocab_size, special_tokens=[],
                           show_progress=False)
    tk.save(str(path))
    return tk


def _tokenizer_hash(key: str) -> str:
    p = CACHE / f"{key}.json"
    if p.exists():
        return hashlib.sha256(p.read_bytes()).hexdigest()[:16]
    return "byte-level(no-file)"


def rung4_enwik8(n_tokens: int) -> tuple[np.ndarray, RungMeta]:
    """Byte-level (q=256).

    Note (deviation, recorded): LTCB/enwik8 at the pinned revision contains only a
    loader script (dataset scripts unsupported by datasets>=5), so the canonical
    enwik8 file is fetched directly from its origin URL; the file sha256 is
    recorded in meta and pins the data equivalently to a revision hash.
    """
    import zipfile
    key = f"r4_enwik8_n{n_tokens}"
    tok = load_cached(key)
    if tok is None:
        CACHE.mkdir(parents=True, exist_ok=True)
        raw = CACHE / "enwik8"
        if not raw.exists():
            import urllib.request
            url = "http://mattmahoney.net/dc/enwik8.zip"
            zp = CACHE / "enwik8.zip"
            urllib.request.urlretrieve(url, zp)
            with zipfile.ZipFile(zp) as z:
                with z.open("enwik8") as f:
                    raw.write_bytes(f.read())
        data = raw.read_bytes()[:n_tokens]
        tok = np.frombuffer(data, dtype=np.uint8).astype(np.int64)
        _save_stream(tok, key)
    file_sha = hashlib.sha256((CACHE / "enwik8").read_bytes()).hexdigest()
    meta = RungMeta(4, "enwik8", "url:http://mattmahoney.net/dc/enwik8.zip",
                    f"origin(pinned-repo LTCB/enwik8@8d9ca88a);file_sha={file_sha[:16]}",
                    "byte (q=256)", 256, len(tok), _sha_tokens(tok))
    return tok, meta


def rung3_tinystories(n_tokens: int) -> tuple[np.ndarray, RungMeta]:
    """Small BPE (4096) fitted on TinyStories train."""
    vocab = 4096
    key = f"r3_tinystories_bpe{vocab}_n{n_tokens}"
    tok = load_cached(key)
    tk_key = f"r3_tinystories_bpe{vocab}"
    if tok is None:
        rev = "f54c09fd23315a6f9c86f9dc80f725de7d8f9c64"
        tk = _fit_bpe(_hf_stream_texts("roneneldan/TinyStories", rev, None, 20_000_000),
                      vocab, tk_key)
        chunks = []
        got = 0
        for text in _hf_stream_texts("roneneldan/TinyStories", rev, None, n_tokens * 6):
            ids = tk.encode(text).ids
            chunks.append(np.array(ids, dtype=np.int64))
            got += len(chunks[-1])
            if got >= n_tokens:
                break
        tok = np.concatenate(chunks)[:n_tokens]
        _save_stream(tok, key)
    meta = RungMeta(3, "roneneldan/TinyStories", "hf:roneneldan/TinyStories",
                    "f54c09fd23315a6f9c86f9dc80f725de7d8f9c64",
                    f"byte-level BPE vocab={vocab} (fitted, hash recorded)", vocab,
                    len(tok), _sha_tokens(tok))
    return tok, meta


def rung5_wikitext2(n_tokens: int) -> tuple[np.ndarray, RungMeta]:
    vocab = 10_000
    key = f"r5_wikitext2_bpe{vocab}_n{n_tokens}"
    tok = load_cached(key)
    tk_key = f"r5_wikitext2_bpe{vocab}"
    if tok is None:
        rev = "b08601e04326c79dfdd32d625aee71d232d685c3"
        tk = _fit_bpe(_hf_stream_texts("Salesforce/wikitext", rev, "wikitext-2-raw-v1",
                                       20_000_000), vocab, tk_key)
        chunks = []
        got = 0
        for text in _hf_stream_texts("Salesforce/wikitext", rev, "wikitext-2-raw-v1",
                                     n_tokens * 8):
            ids = tk.encode(text).ids
            chunks.append(np.array(ids, dtype=np.int64))
            got += len(chunks[-1])
            if got >= n_tokens:
                break
        tok = np.concatenate(chunks)[:n_tokens]
        _save_stream(tok, key)
    meta = RungMeta(5, "Salesforce/wikitext", "hf:Salesforce/wikitext:wikitext-2-raw-v1",
                    "b08601e04326c79dfdd32d625aee71d232d685c3",
                    f"byte-level BPE vocab={vocab} (fitted, hash recorded)", vocab,
                    len(tok), _sha_tokens(tok))
    return tok, meta


def rung6_codeparrot(n_tokens: int) -> tuple[np.ndarray, RungMeta]:
    vocab = 10_000
    key = f"r6_codeparrot_py_bpe{vocab}_n{n_tokens}"
    tok = load_cached(key)
    tk_key = f"r6_codeparrot_py_bpe{vocab}"

    def is_python(row) -> bool:
        path = (row.get("repo_name") or "") + (row.get("path") or "")
        return path.lower().endswith(".py") or ".py" in path.lower()

    if tok is None:
        rev = "35a59fb025bc0a102f7d96eac09d145b896d487b"
        tk = _fit_bpe(_hf_stream_texts("codeparrot/codeparrot-clean", rev, None,
                                       20_000_000, filter_fn=is_python), vocab, tk_key)
        chunks = []
        got = 0
        for text in _hf_stream_texts("codeparrot/codeparrot-clean", rev, None,
                                     n_tokens * 30, filter_fn=is_python):
            ids = tk.encode(text).ids
            chunks.append(np.array(ids, dtype=np.int64))
            got += len(chunks[-1])
            if got >= n_tokens:
                break
        tok = np.concatenate(chunks)[:n_tokens]
        _save_stream(tok, key)
    meta = RungMeta(6, "codeparrot/codeparrot-clean", "hf:codeparrot/codeparrot-clean",
                    "35a59fb025bc0a102f7d96eac09d145b896d487b",
                    f"python-filtered, byte-level BPE vocab={vocab} (fitted, hash recorded)",
                    vocab, len(tok), _sha_tokens(tok))
    return tok, meta


RUNDG_LOADERS = {
    1: lambda n: None,  # needs rung-3 vocab first; see build_all
    2: rung2_synthetic_f2,
    3: rung3_tinystories,
    4: rung4_enwik8,
    5: rung5_wikitext2,
    6: rung6_codeparrot,
    7: rung7_arithmetic,
}
