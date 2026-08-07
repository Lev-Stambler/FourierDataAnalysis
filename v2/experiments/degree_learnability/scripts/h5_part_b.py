"""Part B: q=256 vocabulary-matched language ladder."""

from __future__ import annotations

import json
import sys
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).parent.parent))
sys.path.insert(0, str(Path(__file__).parent))

from h5_common import (
    OUT,
    load_v13,
    reconstruct_initial_ce,
    set_bounded_threads,
    sha256_file,
    sha256_tokens,
)

from dlx.analysis.floor_independent import curve_metrics
from dlx.data.corpora import CACHE
from dlx.data.corpus_family import CorpusFamily
from dlx.families import F1Markov, F2SubsetSum
from dlx.learners.transformer import TransformerConfig
from dlx.profiles.corpus_profile import suffix_filtration_profile_crossfit
from dlx.protocol import load_manifest
from dlx.training.run import train_run

CORPUS_TOKENS = 5_500_000


def unigram_entropy(tokens: np.ndarray, q: int = 256) -> float:
    c = np.bincount(np.asarray(tokens, dtype=np.int64), minlength=q).astype(float)
    p = c[c > 0] / c.sum()
    return float(-(p * np.log2(p)).sum())


def build_tinystories_bytes() -> tuple[np.ndarray, dict]:
    path = CACHE / f"h5_tinystories_bytes_n{CORPUS_TOKENS}.npy"
    if path.exists():
        tok = np.load(path)
    else:
        from tokenizers import Tokenizer

        ids = np.load(CACHE / "r3_tinystories_bpe4096_n20000000.npy", mmap_mode="r")
        tokenizer_path = CACHE / "r3_tinystories_bpe4096.json"
        tokenizer = Tokenizer.from_file(str(tokenizer_path))
        out = bytearray()
        chunk = 50_000
        for i in range(0, len(ids), chunk):
            text = tokenizer.decode(np.asarray(ids[i:i + chunk]).tolist(),
                                    skip_special_tokens=False)
            out.extend(text.encode("utf-8"))
            if len(out) >= CORPUS_TOKENS:
                break
        if len(out) < CORPUS_TOKENS:
            raise RuntimeError(f"decoded TinyStories stream too short: {len(out)}")
        tok = np.frombuffer(bytes(out[:CORPUS_TOKENS]), dtype=np.uint8).copy()
        np.save(path, tok)
    meta = {
        "id": "tinystories_bytes", "source_kind": "natural",
        "source": "cached pinned TinyStories BPE ids decoded to UTF-8 bytes",
        "source_revision": "f54c09fd23315a6f9c86f9dc80f725de7d8f9c64",
        "tokenizer_sha256": sha256_file(CACHE / "r3_tinystories_bpe4096.json"),
    }
    return tok, meta


def build_synthetic(rung: str, proto: dict) -> tuple[np.ndarray, dict]:
    spec = proto["matched_h5"]["part_b_language"]
    eta = float(spec["synthetic_eta"])
    if rung == "markov2_bytes":
        fam = F1Markov(q=256, L=64, k=2, eta=eta)
        source = "F1Markov(q=256,k=2,lags=(1,2))"
        planted_degree, span = 2, 2
    elif rung == "copy_lag16_bytes":
        fam = F2SubsetSum(q=256, L=64, lags=(16,), eta=eta)
        source = "F2SubsetSum(q=256,lags=(16,))"
        planted_degree, span = 1, 16
    else:
        raise ValueError(rung)
    path = CACHE / f"h5_{rung}_n{CORPUS_TOKENS}_eta{eta}.npy"
    if path.exists():
        tok = np.load(path)
    else:
        tok = fam.sample(CORPUS_TOKENS, np.random.default_rng(1313)).astype(np.uint8)
        np.save(path, tok)
    meta = {
        "id": rung, "source_kind": "synthetic", "source": source,
        "generator_seed": 1313, "eta": eta, "planted_degree": planted_degree,
        "span": span, "generating_entropy_bits": fam.entropy_rate(),
        "family_version": fam.version,
    }
    return tok, meta


def profile(tokens: np.ndarray, proto: dict) -> dict:
    ps = proto["matched_h5"]["part_b_language"]["profile"]
    n = min(len(tokens), int(ps["max_profile_tokens"]))
    return suffix_filtration_profile_crossfit(np.asarray(tokens[:n]), q=256, L=64,
                                              k_max=int(ps["k_max"]))


def reuse_enwik(proto: dict, root: Path) -> tuple[dict, list[dict]]:
    stage_a = json.loads((Path("runs/local/m7/stage_a.json")).read_text())
    rung_meta = stage_a["rungs"]["4"]
    tokens = np.load(CACHE / "r4_enwik8_n20000000.npy").astype(np.int64, copy=False)
    data_sha = rung_meta["sha256"]
    records = []
    for seed in (0, 1, 2):
        src = Path(f"runs/local/m7/M7__rung4__enwik8__s{seed}")
        met = json.loads((src / "metrics.json").read_text())
        init = reconstruct_initial_ce(tokens, 256, "enwik8", met["bayes_floor_bits"],
                                      seed, proto, data_sha[:16])
        summary = curve_metrics([0, *met["token_grid"]], [init, *met["val_ce_bits"]], init)
        records.append({
            "cell_id": f"H5B/enwik8_bytes/s{seed}", "rung": "enwik8_bytes",
            "seed": seed, "execution": "reuse", "source_cell": met["cell_id"],
            "source_manifest": str(src / "manifest.json"),
            "source_manifest_sha256": sha256_file(src / "manifest.json"),
            **summary,
        })
        load_manifest(src)
    meta = {"id": "enwik8_bytes", "source_kind": "natural", "q": 256,
            "data_sha256": data_sha, "profile": profile(tokens, proto),
            "source": rung_meta}
    (root / "enwik8_reuse.json").write_text(json.dumps(records, indent=2))
    return meta, records


def run_new_rung(rung: str, tokens: np.ndarray, meta: dict, proto: dict,
                 root: Path) -> tuple[dict, list[dict]]:
    spec = proto["matched_h5"]["part_b_language"]
    data_sha = sha256_tokens(tokens)
    prof = profile(tokens, proto)
    reference_entropy = (float(meta["generating_entropy_bits"])
                         if meta["source_kind"] == "synthetic"
                         else unigram_entropy(tokens))
    full_meta = {**meta, "q": 256, "n_tokens": len(tokens),
                 "data_sha256": data_sha, "profile": prof,
                 "reference_entropy_bits": reference_entropy,
                 "reference_entropy_kind": ("generating" if meta["source_kind"] == "synthetic"
                                             else "empirical-unigram-not-a-floor")}
    records = []
    base_cfg = proto["learner_config"]
    for seed in spec["seeds"]:
        cell_id = f"H5B/{rung}/s{seed}"
        cell_dir = root / cell_id.replace("/", "__")
        if (cell_dir / "manifest.json").exists():
            met = json.loads((cell_dir / "metrics.json").read_text())
        else:
            fam = CorpusFamily(tokens, q=256, L=64, name=rung,
                               floor_bits=reference_entropy, cyclic=True,
                               shuffle_seed=seed, data_version=data_sha[:16])
            cfg = TransformerConfig(vocab=256, ctx_len=base_cfg["ctx_len"],
                                    d_model=base_cfg["d_model"],
                                    n_layers=base_cfg["n_layers"],
                                    n_heads=base_cfg["n_heads"])
            met = train_run(fam, cfg, budget_tokens=spec["student_budget_tokens"],
                            seed=seed, out_dir=cell_dir, cell_id=cell_id,
                            protocol_hash=proto["protocol_hash"], device="cpu",
                            n_checkpoints=20, tokens_per_step=spec["tokens_per_step"],
                            record_initial=True)
            summary = curve_metrics(met["token_grid"], met["val_ce_bits"],
                                    met["initial_val_ce_bits"])
            met["floor_independent"] = summary
            (cell_dir / "metrics.json").write_text(json.dumps(met, indent=2))
        (cell_dir / "rung_meta.json").write_text(json.dumps(full_meta, indent=2))
        manifest = json.loads((cell_dir / "manifest.json").read_text())
        manifest.update({"data_sha256": data_sha,
                         "rung_meta_path": str(cell_dir / "rung_meta.json")})
        (cell_dir / "manifest.json").write_text(json.dumps(manifest, indent=2,
                                                            sort_keys=True))
        load_manifest(cell_dir)
        summary = met.get("floor_independent") or curve_metrics(
            met["token_grid"], met["val_ce_bits"], met["initial_val_ce_bits"])
        records.append({"cell_id": cell_id, "rung": rung, "seed": seed,
                        "execution": "new", **summary})
    return full_meta, records


def main() -> None:
    proto = load_v13()
    set_bounded_threads(proto)
    root = OUT / "part_b"
    root.mkdir(parents=True, exist_ok=True)
    report = {"protocol_hash": proto["protocol_hash"], "rungs": {}, "cells": []}

    enwik_meta, records = reuse_enwik(proto, root)
    report["rungs"]["enwik8_bytes"] = enwik_meta
    report["cells"].extend(records)

    for rung in ("tinystories_bytes", "markov2_bytes", "copy_lag16_bytes"):
        if rung == "tinystories_bytes":
            tokens, meta = build_tinystories_bytes()
        else:
            tokens, meta = build_synthetic(rung, proto)
        rung_meta, records = run_new_rung(rung, tokens, meta, proto, root)
        report["rungs"][rung] = rung_meta
        report["cells"].extend(records)
        del tokens

    (root / "part_b_results.json").write_text(json.dumps(report, indent=2))
    print(f"Part B complete: {len(report['cells'])} cells (3 reused, 9 new)")


if __name__ == "__main__":
    main()
