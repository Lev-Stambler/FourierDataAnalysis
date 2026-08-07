"""Prospective four-corpus natural-text geometry experiment (protocol v1.8)."""

from __future__ import annotations

import argparse
import gc
import hashlib
import itertools
import json
import sys
from math import log2
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).parent.parent))
sys.path.insert(0, str(Path(__file__).parent))

from v14_common import set_bounded_threads
from v14_text_run import load_rung as load_v14_rung
from v14_text_run import run_cell
from v16_conditional_spectrum import _estimated_spectrum
from v17_local_geometry_analysis import _spearman

from dlx.analysis.local_geometry import (
    local_ball_cardinalities,
    spectral_search_complexity,
)
from dlx.data.corpora import CACHE
from dlx.profiles.text_anova import text_inverse_likelihood_profile

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs" / "local" / "v18_real_natural"
PROFILE_DIR = OUT / "profiles"
CELL_DIR = OUT / "cells"
CORPUS_TOKENS = 5_500_000
FRESH = ("wikitext2_bytes", "codeparrot_python_bytes")
ALL_DATASETS = (
    "enwik8_bytes",
    "tinystories_bytes",
    "wikitext2_bytes",
    "codeparrot_python_bytes",
)
SOURCE_FILES = {
    "wikitext2_bytes": (
        CACHE / "r5_wikitext2_bpe10000_n2000000.npy",
        CACHE / "r5_wikitext2_bpe10000.json",
    ),
    "codeparrot_python_bytes": (
        CACHE / "r6_codeparrot_py_bpe10000_n20000000.npy",
        CACHE / "r6_codeparrot_py_bpe10000.json",
    ),
}


def _sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _sha256_tokens(tokens: np.ndarray) -> str:
    return hashlib.sha256(
        np.ascontiguousarray(tokens, dtype=np.uint8).tobytes()
    ).hexdigest()


def _unigram_entropy(tokens: np.ndarray) -> float:
    counts = np.bincount(np.asarray(tokens, dtype=np.int64), minlength=256)
    probabilities = counts[counts > 0].astype(float) / counts.sum()
    return float(-(probabilities * np.log2(probabilities)).sum())


def load_protocol() -> dict:
    path = ROOT / "configs" / "protocol_v1.8.json"
    protocol = json.loads(path.read_text())
    recorded = protocol["protocol_hash"]
    unhashed = dict(protocol)
    unhashed.pop("protocol_hash")
    expected = hashlib.sha256(json.dumps(unhashed, sort_keys=True).encode()).hexdigest()
    if recorded != expected:
        raise ValueError(f"protocol_v1.8 hash mismatch: {recorded} != {expected}")
    return protocol


def decoded_path(dataset: str) -> Path:
    return CACHE / f"v18_{dataset}_n{CORPUS_TOKENS}.npy"


def prepare_dataset(dataset: str) -> dict:
    if dataset not in FRESH:
        raise ValueError(f"not a fresh v1.8 dataset: {dataset}")
    ids_path, tokenizer_path = SOURCE_FILES[dataset]
    output_path = decoded_path(dataset)
    if output_path.exists():
        tokens = np.load(output_path, mmap_mode="r")
        if len(tokens) != CORPUS_TOKENS or tokens.dtype != np.uint8:
            raise ValueError(f"invalid cached decoded stream: {output_path}")
    else:
        from tokenizers import Tokenizer

        ids = np.load(ids_path, mmap_mode="r")
        tokenizer = Tokenizer.from_file(str(tokenizer_path))
        decoded = bytearray()
        for start in range(0, len(ids), 50_000):
            text = tokenizer.decode(
                np.asarray(ids[start : start + 50_000]).tolist(),
                skip_special_tokens=False,
            )
            decoded.extend(text.encode("utf-8"))
            if len(decoded) >= CORPUS_TOKENS:
                break
        if len(decoded) < CORPUS_TOKENS:
            raise RuntimeError(
                f"{dataset} decoded to only {len(decoded)} bytes; need {CORPUS_TOKENS}"
            )
        tokens = np.frombuffer(bytes(decoded[:CORPUS_TOKENS]), dtype=np.uint8).copy()
        np.save(output_path, tokens)
    metadata = {
        "dataset": dataset,
        "n_bytes": len(tokens),
        "byte_stream_sha256": _sha256_tokens(tokens),
        "source_ids": str(ids_path.relative_to(ROOT)),
        "source_ids_sha256": _sha256_file(ids_path),
        "tokenizer": str(tokenizer_path.relative_to(ROOT)),
        "tokenizer_sha256": _sha256_file(tokenizer_path),
        "decoded_cache": str(output_path.relative_to(ROOT)),
        "unigram_entropy_bits": _unigram_entropy(tokens),
    }
    OUT.mkdir(parents=True, exist_ok=True)
    (OUT / f"{dataset}__data.json").write_text(json.dumps(metadata, indent=2))
    return metadata


def load_tokens(dataset: str) -> np.ndarray:
    if dataset in FRESH:
        path = decoded_path(dataset)
        if not path.exists():
            prepare_dataset(dataset)
        return np.load(path, mmap_mode="r")
    tokens, _ = load_v14_rung(dataset)
    return tokens


def profile_dataset(dataset: str, protocol: dict) -> dict:
    if dataset not in FRESH:
        raise ValueError("v1.8 computes only the two new profiles")
    tokens = load_tokens(dataset)
    spec = protocol["profile"]
    profile = text_inverse_likelihood_profile(
        tokens,
        q=protocol["data"]["q"],
        lags=tuple(spec["lags"]),
        max_tokens=spec["max_positions"],
    )
    profile.update(
        {
            "protocol_hash": protocol["protocol_hash"],
            "dataset": dataset,
            "data_sha256": _sha256_tokens(tokens),
        }
    )
    PROFILE_DIR.mkdir(parents=True, exist_ok=True)
    path = PROFILE_DIR / f"{dataset}.json"
    path.write_text(json.dumps(profile, indent=2))
    print(
        f"profiled {dataset}: best={profile['best_pair_by_function_variance']} "
        f"V={profile['pair_function_variance_best_pair']:.6f}",
        flush=True,
    )
    return profile


def train_cell(dataset: str, seed: int, protocol: dict) -> dict:
    if dataset not in FRESH:
        raise ValueError("v1.8 trains only the two new datasets")
    tokens = load_tokens(dataset)
    data_meta = json.loads((OUT / f"{dataset}__data.json").read_text())
    meta = {
        "id": dataset,
        "source_kind": "natural",
        "source": data_meta["source_ids"],
    }
    cell_id = f"V18/{dataset}/s{seed}"
    directory = CELL_DIR / cell_id.replace("/", "__")
    metrics = run_cell(
        tokens,
        meta,
        protocol,
        seed,
        protocol["learner"]["student_budget_tokens"],
        cell_id,
        directory,
    )
    summary = metrics["floor_independent"]
    print(
        f"trained {cell_id}: area={summary['normalized_curve_area']:.6f} "
        f"learned={summary['learning_amount_bits']:.6f} bits",
        flush=True,
    )
    return metrics


def _median_cells(cells: list[dict], dataset: str) -> dict:
    rows = [row for row in cells if row["dataset"] == dataset]
    keys = (
        "initial_ce_bits",
        "final_ce_bits",
        "best_ce_bits",
        "learning_amount_bits",
        "fractional_learning",
        "normalized_curve_area",
    )
    return {key: float(np.median([row[key] for row in rows])) for key in keys}


def _geometry(weights: list[float], pair: list[int]) -> dict:
    q = 256
    lower_radius, upper_radius = min(pair), max(pair)
    upper_cardinality = local_ball_cardinalities(q, upper_radius, 2)
    lower_cardinality = list(upper_cardinality)
    lower_cardinality[1] = lower_radius * (q - 1)
    lower_bits = spectral_search_complexity(weights, lower_cardinality)
    upper_bits = spectral_search_complexity(weights, upper_cardinality)
    return {
        "search_complexity_bits": upper_bits,
        "search_complexity_bits_interval": [lower_bits, upper_bits],
        "geometric_degree_qary": upper_bits / log2(q),
        "degree1_radius_interval": [lower_radius, upper_radius],
        "degree2_radius": upper_radius,
        "upper_level_cardinalities": upper_cardinality,
    }


def _leave_one_out(x: list[float], y: list[float], names: list[str]) -> list[dict]:
    records = []
    for excluded in range(len(names)):
        keep = [index for index in range(len(names)) if index != excluded]
        records.append(
            {
                "excluded": names[excluded],
                "rho": _spearman(
                    [x[index] for index in keep], [y[index] for index in keep]
                )["rho"],
            }
        )
    return records


def analyze(protocol: dict) -> dict:
    old_report = json.loads(
        (
            ROOT / "runs/local/v14_text_anova/confirmation/confirmation_results.json"
        ).read_text()
    )
    old_spectra = json.loads(
        (
            ROOT / "runs/local/v16_conditional_spectrum/integrated_analysis.json"
        ).read_text()
    )
    old_spectrum_by_name = {row["rung"]: row for row in old_spectra["rungs"]}

    cells = []
    for row in old_report["cells"]:
        if row["rung"] in ALL_DATASETS[:2]:
            cells.append({**row, "dataset": row["rung"], "execution": "reuse"})
    for dataset in FRESH:
        for seed in protocol["execution"]["seeds"]:
            directory = CELL_DIR / f"V18__{dataset}__s{seed}"
            metrics = json.loads((directory / "metrics.json").read_text())
            cells.append(
                {
                    "cell_id": metrics["cell_id"],
                    "dataset": dataset,
                    "seed": seed,
                    "execution": "fresh",
                    **metrics["floor_independent"],
                }
            )

    rows = []
    for dataset in ALL_DATASETS:
        tokens = load_tokens(dataset)
        if dataset in old_spectrum_by_name:
            source = old_spectrum_by_name[dataset]
            pair = source["best_pair"]
            spectrum = source["spectrum"]
            profile_source = "valid v1.6 analysis of v1.4 profile"
        else:
            profile_path = PROFILE_DIR / f"{dataset}.json"
            profile = json.loads(profile_path.read_text())
            pair, spectrum = _estimated_spectrum(profile)
            profile_source = str(profile_path.relative_to(ROOT))
        geometry = _geometry(spectrum["level_weights"], pair)
        rows.append(
            {
                "dataset": dataset,
                "execution": "reuse" if dataset in ALL_DATASETS[:2] else "fresh",
                "n_bytes": len(tokens),
                "data_sha256": _sha256_tokens(tokens),
                "unigram_entropy_bits": _unigram_entropy(tokens),
                "best_pair": pair,
                "profile_source": profile_source,
                "level_weights": spectrum["level_weights"],
                "nonconstant_energy": spectrum["nonconstant_energy"],
                "mean_nonconstant_spectral_degree": spectrum[
                    "mean_nonconstant_spectral_degree"
                ],
                **geometry,
                **_median_cells(cells, dataset),
            }
        )
        del tokens
        gc.collect()

    names = [row["dataset"] for row in rows]
    area = [row["normalized_curve_area"] for row in rows]
    degree = [row["mean_nonconstant_spectral_degree"] for row in rows]
    geometry = [row["search_complexity_bits"] for row in rows]
    radius = [max(row["best_pair"]) for row in rows]
    energy = [row["nonconstant_energy"] for row in rows]
    entropy = [row["unigram_entropy_bits"] for row in rows]
    final_ce = [row["final_ce_bits"] for row in rows]
    final_ce_fraction = [row["final_ce_bits"] / row["initial_ce_bits"] for row in rows]
    raw_correlation = _spearman(degree, area)
    geometry_correlation = _spearman(geometry, area)

    endpoint_rhos = []
    for endpoints in itertools.product((0, 1), repeat=len(rows)):
        costs = [
            row["search_complexity_bits_interval"][endpoint]
            for row, endpoint in zip(rows, endpoints, strict=True)
        ]
        endpoint_rhos.append(_spearman(costs, area)["rho"])
    rho_interval = [min(endpoint_rhos), max(endpoint_rhos)]
    threshold = raw_correlation["rho"] + 0.20
    if rho_interval[0] >= threshold - 1e-12:
        decision = "SUPPORTED"
    elif rho_interval[1] <= raw_correlation["rho"] + 1e-12:
        decision = "REFUTED"
    else:
        decision = "INCONCLUSIVE"

    result = {
        "protocol_hash": protocol["protocol_hash"],
        "prospective_status": (
            "protocol frozen before new WikiText-2/CodeParrot profiles and curves"
        ),
        "datasets": rows,
        "cells": cells,
        "primary": {
            "decision": decision,
            "rule": protocol["analysis"]["decision_rule"],
            "raw_degree_vs_curve_area": raw_correlation,
            "geometry_vs_curve_area": geometry_correlation,
            "geometry_rho_all_radius_endpoints": rho_interval,
            "rho_improvement_at_conservative_endpoint": (
                geometry_correlation["rho"] - raw_correlation["rho"]
            ),
        },
        "comparators": {
            "selected_pair_radius_vs_curve_area": _spearman(radius, area),
            "nonconstant_energy_vs_curve_area": _spearman(energy, area),
            "unigram_entropy_vs_curve_area": _spearman(entropy, area),
        },
        "secondary_held_out_ce": {
            "note": (
                "all losses are on the held-out tail; raw final CE mixes training "
                "difficulty with irreducible corpus entropy"
            ),
            "final_ce_fraction_by_dataset": {
                row["dataset"]: row["final_ce_bits"] / row["initial_ce_bits"]
                for row in rows
            },
            "raw_degree_vs_final_ce_fraction": _spearman(degree, final_ce_fraction),
            "geometry_vs_final_ce_fraction": _spearman(geometry, final_ce_fraction),
            "raw_degree_vs_raw_final_ce": _spearman(degree, final_ce),
            "geometry_vs_raw_final_ce": _spearman(geometry, final_ce),
            "unigram_entropy_vs_raw_final_ce": _spearman(entropy, final_ce),
        },
        "leave_one_out": {
            "raw_degree": _leave_one_out(degree, area, names),
            "geometry": _leave_one_out(geometry, area, names),
        },
        "scope": (
            "four natural byte corpora, identical q/model/budget; strongest-pair "
            "degree-0/1/2 slices rather than complete 64-position spectra"
        ),
        "new_profiles": 2,
        "new_training_cells": 6,
        "invalid_pre_v1_4_cells_reused": 0,
        "invalid_m8_image_cells_reused": 0,
    }
    OUT.mkdir(parents=True, exist_ok=True)
    (OUT / "integrated_analysis.json").write_text(json.dumps(result, indent=2))
    print(json.dumps(result["primary"], indent=2), flush=True)
    return result


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("stage", choices=("prepare", "profile", "train", "analyze"))
    parser.add_argument("--dataset", choices=FRESH)
    parser.add_argument("--seed", type=int)
    args = parser.parse_args()
    protocol = load_protocol()
    OUT.mkdir(parents=True, exist_ok=True)
    if args.stage in {"prepare", "profile", "train"} and args.dataset is None:
        parser.error(f"{args.stage} requires --dataset")
    if args.stage == "prepare":
        print(json.dumps(prepare_dataset(args.dataset), indent=2), flush=True)
    elif args.stage == "profile":
        profile_dataset(args.dataset, protocol)
    elif args.stage == "train":
        if args.seed not in protocol["execution"]["seeds"]:
            parser.error("train requires --seed from the frozen seed list")
        set_bounded_threads(protocol)
        train_cell(args.dataset, args.seed, protocol)
    else:
        analyze(protocol)


if __name__ == "__main__":
    main()
