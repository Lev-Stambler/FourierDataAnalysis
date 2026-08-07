"""Freeze the size/vocabulary-matched H5 follow-up before any v1.3 runs."""

from __future__ import annotations

import hashlib
import json
from pathlib import Path

import numpy as np


def mixture_entropy(q: int, eta: float) -> float:
    p0 = 1.0 - eta + eta / q
    p1 = eta / q
    return float(-p0 * np.log2(p0) - (q - 1) * p1 * np.log2(p1))


def main() -> None:
    cfg_dir = Path(__file__).parent.parent / "configs"
    v12 = json.loads((cfg_dir / "protocol_v1.2.json").read_text())
    v13 = dict(v12)
    v13.pop("protocol_hash", None)
    v13["protocol_id"] = "dlx-v1.3-matched-h5"
    v13["supersedes"] = v12["protocol_hash"]
    v13["amendment_date"] = "2026-08-06"
    v13["amendment_note"] = (
        "Re-tests H5 after removing the original real-ladder size/vocabulary and "
        "image-floor confounds. All new runs are local CPU; image cells are only "
        "re-scored; original VERDICT body remains immutable and receives an addendum."
    )
    v13["matched_h5"] = {
        "status": "frozen-before-runs",
        "execution": {"device": "local-cpu", "max_torch_threads": 2,
                      "modal_or_gpu_spend": 0},
        "difficulty_metrics": {
            "initial_ce_bits": "validation CE at exactly zero optimizer updates",
            "learning_amount_bits": "initial CE - final CE (higher means easier)",
            "best_ce_bits": "minimum validation CE on the fixed curve",
            "fractional_learning": "(initial CE - final CE) / initial CE",
            "normalized_curve_area": (
                "integral of CE/initial_CE over normalized log1p(resource); "
                "higher means harder"
            ),
            "half_best_learning_at": (
                "first resource checkpoint achieving half of init-to-best improvement"
            ),
            "floor_policy": "no Bayes-floor estimate enters any revised-H5 difficulty scalar",
        },
        "part_a_tabular": {
            "row_selection": "deterministic subsample before categorical encoding",
            "row_target": 500,
            "selection_seed": 1300,
            "row_tolerance": 0,
            "feature_tolerance": 0,
            "learner": "FixedMLP, embeddings(8), hidden widths 256x256, GELU",
            "split": "70/15/15 stratified per training seed",
            "exposure_grid": [256, 512, 1024, 2048, 4096, 8192],
            "seeds": [0, 1, 2],
            "bands": {
                "f4_n500": ["analcatdata_dmft", "balance-scale",
                              "blood-transfusion-service-center"],
                "f6_n500": ["monks-problems-1", "monks-problems-2",
                              "monks-problems-3"],
                "f9_n500": ["xd6", "website_phishing", "tic-tac-toe"],
            },
            "degree_features": ["mean_degree", "tail_above_2", "degree_90"],
            "analysis": (
                "within each band, OLS on dataset medians; show all seed points and "
                "report n=3 limitation; class entropy and imbalance are diagnostics"
            ),
        },
        "part_b_language": {
            "q": 256,
            "context_length": 64,
            "student_budget_tokens": 5_000_000,
            "tokens_per_step": 1024,
            "seeds": [0, 1, 2],
            "rungs": [
                {"id": "enwik8_bytes", "source": "existing M7 enwik8 q=256 cells",
                 "execution": "reuse", "structure": "natural text"},
                {"id": "tinystories_bytes",
                 "source": "deterministic UTF-8 decode of pinned cached TinyStories BPE stream",
                 "execution": "new", "structure": "natural text"},
                {"id": "markov2_bytes", "source": "synthetic F1 q=256 lags=(1,2)",
                 "execution": "new", "planted_degree": 2, "span": 2},
                {"id": "copy_lag16_bytes", "source": "synthetic F2 q=256 lags=(16,)",
                 "execution": "new", "planted_degree": 1, "span": 16},
            ],
            "synthetic_eta": 0.25,
            "synthetic_entropy_bits": mixture_entropy(256, 0.25),
            "entropy_matching": "the two synthetic rungs have exactly matched generating entropy",
            "profile": {"estimator": "suffix filtration", "k_max": 3,
                        "max_profile_tokens": 1_000_000},
            "analysis": (
                "univariate OLS/Spearman of median curve difficulty on suffix-profile "
                "features; matched-entropy synthetic pair reported as planted structure contrast"
            ),
        },
        "part_c_image": {
            "cells": 18,
            "training": "none",
            "initial_ce_reconstruction": (
                "recreate the seeded initial model/config and evaluate the unchanged held-out stream"
            ),
            "curve_source": "existing M8 checkpoint curves",
            "floor_policy": "marginal code entropy retained only as historical metadata",
        },
        "part_d": {
            "output": "runs/local/h5_matched/integrated_analysis.json",
            "verdict_rule": {
                "tabular_support": (
                    "at least two of three bands have Spearman rho >= 0.5 for "
                    "mean spectral degree vs normalized curve area, with no band <= -0.5"
                ),
                "language_support": (
                    "Spearman rho <= -0.5 for total measured suffix gain vs "
                    "normalized curve area (more low-order predictability is easier)"
                ),
                "PASS": "tabular_support and language_support",
                "FAIL": (
                    "at least two tabular bands strongly contradict (rho <= -0.5), "
                    "or tabular has no supporting band and language strongly contradicts"
                ),
                "INCONCLUSIVE": "all other outcomes; report directions and tiny-n limits",
            },
            "verdict_policy": (
                "append a number-backed revised-H5 addendum; do not edit original VERDICT body"
            ),
        },
    }
    canonical = json.dumps(v13, sort_keys=True)
    v13["protocol_hash"] = hashlib.sha256(canonical.encode()).hexdigest()
    out = cfg_dir / "protocol_v1.3.json"
    out.write_text(json.dumps(v13, indent=2, sort_keys=True))
    print(f"protocol_hash(v1.3) = {v13['protocol_hash']}")
    print(f"wrote {out}")


if __name__ == "__main__":
    main()
