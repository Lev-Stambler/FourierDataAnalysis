"""Corrected q=256 language run with contiguous Definition-3.1 profiles."""

from __future__ import annotations

import argparse
import hashlib
import json
import sys
import time
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).parent.parent))
sys.path.insert(0, str(Path(__file__).parent))

from h5_part_b import build_tinystories_bytes
from v14_common import OUT, load_v14, set_bounded_threads

from dlx.analysis.floor_independent import curve_metrics
from dlx.data.corpora import CACHE
from dlx.data.corpus_family import CorpusFamily
from dlx.families import F1Markov, F2SubsetSum
from dlx.learners.transformer import TransformerConfig
from dlx.profiles.text_anova import text_inverse_likelihood_profile
from dlx.protocol import load_manifest
from dlx.training.run import train_run

CORPUS_TOKENS = 5_500_000


def _token_hash(tokens: np.ndarray) -> str:
    return hashlib.sha256(np.ascontiguousarray(tokens, dtype=np.uint8)).hexdigest()


def _unigram_entropy(tokens: np.ndarray, q: int = 256) -> float:
    counts = np.bincount(np.asarray(tokens, dtype=np.int64), minlength=q).astype(float)
    p = counts[counts > 0] / counts.sum()
    return float(-(p * np.log2(p)).sum())


def load_rung(rung: str) -> tuple[np.ndarray, dict]:
    if rung == "enwik8_bytes":
        tokens = np.asarray(np.load(CACHE / "r4_enwik8_n20000000.npy", mmap_mode="r")[:CORPUS_TOKENS],
                            dtype=np.uint8)
        meta = {"id": rung, "source_kind": "natural",
                "source": "cached canonical enwik8 byte prefix"}
    elif rung == "tinystories_bytes":
        tokens, old_meta = build_tinystories_bytes()
        meta = {**old_meta, "id": rung}
    elif rung == "markov2_bytes":
        tokens = np.load(CACHE / "h5_markov2_bytes_n5500000_eta0.25.npy", mmap_mode="r")
        family = F1Markov(q=256, L=64, k=2, eta=0.25)
        meta = {"id": rung, "source_kind": "synthetic", "eta": 0.25,
                "planted_degree": 2, "span": 2, "lags": [1, 2],
                "generating_entropy_bits": family.entropy_rate(),
                "family_version": family.version}
    elif rung == "copy_lag16_bytes":
        tokens = np.load(CACHE / "h5_copy_lag16_bytes_n5500000_eta0.25.npy", mmap_mode="r")
        family = F2SubsetSum(q=256, L=64, lags=(16,), eta=0.25)
        meta = {"id": rung, "source_kind": "synthetic", "eta": 0.25,
                "planted_degree": 1, "span": 16, "lags": [16],
                "generating_entropy_bits": family.entropy_rate(),
                "family_version": family.version}
    else:
        raise ValueError(rung)
    return np.asarray(tokens, dtype=np.uint8), meta


def _config(protocol: dict) -> TransformerConfig:
    spec = protocol["learner"]
    return TransformerConfig(vocab=spec["vocab"], ctx_len=spec["ctx_len"],
                             d_model=spec["d_model"], n_layers=spec["n_layers"],
                             n_heads=spec["n_heads"], mlp_mult=spec["mlp_mult"],
                             dropout=spec["dropout"], tie_weights=spec["tie_weights"],
                             lr=spec["lr"], weight_decay=spec["weight_decay"],
                             grad_clip=spec["grad_clip"])


def run_cell(tokens: np.ndarray, meta: dict, protocol: dict, seed: int,
             budget: int, cell_id: str, cell_dir: Path) -> dict:
    data_hash = _token_hash(tokens)
    if (cell_dir / "manifest.json").exists():
        manifest = load_manifest(cell_dir)
        if manifest["protocol_hash"] != protocol["protocol_hash"]:
            raise ValueError(f"stale protocol in {cell_dir}")
        return json.loads((cell_dir / "metrics.json").read_text())

    floor = (float(meta["generating_entropy_bits"])
             if meta["source_kind"] == "synthetic" else _unigram_entropy(tokens))
    family = CorpusFamily(tokens, q=256, L=64, name=meta["id"], floor_bits=floor,
                          cyclic=True, shuffle_seed=seed, data_version=data_hash[:16])
    learner = protocol["learner"]
    metrics = train_run(
        family, _config(protocol), budget_tokens=budget, seed=seed,
        out_dir=cell_dir, cell_id=cell_id, protocol_hash=protocol["protocol_hash"],
        device="cpu", n_checkpoints=learner["checkpoints"],
        tokens_per_step=learner["tokens_per_step"], record_initial=True)
    metrics["floor_independent"] = curve_metrics(
        metrics["token_grid"], metrics["val_ce_bits"], metrics["initial_val_ce_bits"])
    metrics["data_sha256"] = data_hash
    metrics["sequence_semantics"] = protocol["sequence_semantics"]["cyclic_reuse"]
    (cell_dir / "metrics.json").write_text(json.dumps(metrics, indent=2))
    manifest = json.loads((cell_dir / "manifest.json").read_text())
    manifest.update({"data_sha256": data_hash,
                     "sequence_semantics": metrics["sequence_semantics"]})
    (cell_dir / "manifest.json").write_text(json.dumps(manifest, indent=2, sort_keys=True))
    load_manifest(cell_dir)
    return metrics


def run_preflight(protocol: dict) -> dict:
    spec = protocol["preflight"]
    root = OUT / "preflight"
    root.mkdir(parents=True, exist_ok=True)
    cells = []
    for rung in ("copy_lag16_bytes", "markov2_bytes"):
        tokens, meta = load_rung(rung)
        cell_id = f"V14P/{rung}/s{spec['seed']}"
        metrics = run_cell(tokens, meta, protocol, spec["seed"], spec["budget_tokens"],
                           cell_id, root / cell_id.replace("/", "__"))
        cells.append({"rung": rung, "seed": spec["seed"],
                      **metrics["floor_independent"]})
    by_rung = {cell["rung"]: cell for cell in cells}
    delta = (by_rung["copy_lag16_bytes"]["learning_amount_bits"]
             - by_rung["markov2_bytes"]["learning_amount_bits"])
    result = {"protocol_hash": protocol["protocol_hash"], "cells": cells,
              "copy_minus_markov_learning_bits": delta,
              "gate_threshold_bits": 2.0, "gate_pass": delta >= 2.0,
              "excluded_from_confirmation": True}
    (root / "preflight_results.json").write_text(json.dumps(result, indent=2))
    if not result["gate_pass"]:
        raise RuntimeError(f"v1.4 preflight gate failed: {delta:.3f} bits")
    return result


def run_confirmation(protocol: dict) -> dict:
    preflight_path = OUT / "preflight" / "preflight_results.json"
    if not preflight_path.exists() or not json.loads(preflight_path.read_text())["gate_pass"]:
        raise RuntimeError("passing preflight artifact required before confirmation")
    root = OUT / "confirmation"
    root.mkdir(parents=True, exist_ok=True)
    report = {"protocol_hash": protocol["protocol_hash"], "rungs": {}, "cells": []}
    started = time.monotonic()
    for rung_spec in protocol["rungs"]:
        rung = rung_spec["id"]
        tokens, meta = load_rung(rung)
        data_hash = _token_hash(tokens)
        profile_path = root / f"{rung}__inverse_likelihood_profile.json"
        if profile_path.exists():
            profile = json.loads(profile_path.read_text())
        else:
            profile = text_inverse_likelihood_profile(
                tokens, q=protocol["profile"]["q"],
                lags=tuple(protocol["profile"]["lags"]),
                max_tokens=protocol["profile"]["max_positions"])
            profile_path.write_text(json.dumps(profile, indent=2))
        report["rungs"][rung] = {**meta, "q": 256, "n_tokens": len(tokens),
                                  "data_sha256": data_hash,
                                  "inverse_likelihood_profile": profile}
        for seed in protocol["confirmation"]["seeds"]:
            if time.monotonic() - started > protocol["execution"]["wallclock_limit_seconds"] - 300:
                raise TimeoutError("stopped safely before the frozen two-hour ceiling")
            cell_id = f"V14/{rung}/s{seed}"
            metrics = run_cell(
                tokens, meta, protocol, seed,
                protocol["learner"]["student_budget_tokens"], cell_id,
                root / cell_id.replace("/", "__"))
            report["cells"].append({"cell_id": cell_id, "rung": rung, "seed": seed,
                                    **metrics["floor_independent"]})
    (root / "confirmation_results.json").write_text(json.dumps(report, indent=2))
    return report


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--stage", choices=("preflight", "confirmation", "all"), default="all")
    args = parser.parse_args()
    protocol = load_v14()
    set_bounded_threads(protocol)
    OUT.mkdir(parents=True, exist_ok=True)
    if args.stage in ("preflight", "all"):
        result = run_preflight(protocol)
        print(f"v1.4 preflight PASS: delta={result['copy_minus_markov_learning_bits']:.3f} bits",
              flush=True)
    if args.stage in ("confirmation", "all"):
        report = run_confirmation(protocol)
        print(f"v1.4 confirmation complete: {len(report['cells'])} cells", flush=True)


if __name__ == "__main__":
    main()
