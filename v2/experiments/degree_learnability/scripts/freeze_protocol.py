"""M5 protocol freeze: assemble configs/protocol_v1.json with every PLAN §9 item.

The protocol hash pins: family source+param hashes, pilot-selected learner config,
tier table, budgets/seeds/theta, metric + trend-test definitions, falsification
criteria (verbatim from PLAN §7 item 6), real-ladder dataset pins, and Modal
resource names.
"""

from __future__ import annotations

import hashlib
import json
from pathlib import Path

from dlx.families import (F1Markov, F2SubsetSum, F3RandomPoly, F4MixedProfile,
                          F5IID, F5MaxSum)

Q, L, ETA = 32, 64, 0.1
SEEDS = [0, 1, 2]
BUDGETS = [100_000, 300_000, 1_000_000, 3_000_000]
THETA = 0.05

FALSIFICATION_CRITERIA_VERBATIM = """H1 falsified if any family shows non-monotone `T*` in its degree knob at fixed
entropy (beyond seed noise, per a pre-specified trend test);
H4 falsified if model (ii) explains ≥ 90% of what model (iii) explains.
Either falsification is written up as a negative result with the same rigor."""

TREND_TEST_SPEC = {
    "T_star": "first checkpoint token count where val CE <= bayes_floor + theta; None if never",
    "theta": THETA,
    "censoring": "T* = None within max budget is recorded as censored, not dropped silently",
    "H1_test": ("per family: Spearman rho between knob value and median T* across seeds "
                "(uncensored cells); families with >=2 censored cells are classified "
                "censored-hard and reported separately, not trend-tested. Falsification "
                "operationalized as Spearman rho < 0.8 or a pairwise median inversion "
                "exceeding 2x the seed IQR."),
    "H4_test": ("OLS of log T* on (i) degree features only, (ii) entropy + Markov span + "
                "total sensitivity only, (iii) both; adjusted R^2 comparison; censored "
                "cells excluded and flagged."),
    "H5_test": ("per ladder OLS difficulty ~ measured profile + covariates, then pooled "
                "model with domain fixed effects; report per-ladder and pooled R^2."),
}

TIER_TABLE_VERBATIM = """| Tier | Scale | Compute | Wall target | Est. cost |
|---|---|---|---|---|
| S0 smoke | q=8, L=8 (tabular toy) | Modal CPU or laptop | < 30 s | ~$0 |
| S1 enumerable | q=16, L=16; enumerable tabular set | Modal CPU | < 5 min | <$1 |
| S2 protocol size | q=32, L=64; full tabular ladder | Modal CPU, parallel | < 30 min | <$2 |
| G1 GPU smoke | q=32, L=64 | 1x cheap GPU (A10G/T4) | < 10 min | <$1 |
| G2 grid | all cells incl. real ladders | 1 GPU per cell, fanned out | <= GPU cap | ~$60-90 |"""


def family_versions() -> dict:
    """Instantiate every frozen grid family and record its source+param hash."""
    cells = {}
    for k in (1, 2, 3, 4, 6, 8):
        cells[f"F1_k{k}"] = F1Markov(q=Q, L=L, k=k, eta=ETA).version
    for lags in ((4,), (16,)):
        cells[f"copy_lag{lags[0]}"] = F2SubsetSum(q=Q, L=L, lags=lags, eta=ETA).version
    for lags in ((1, 4), (1, 16), (1, 16, 32)):
        tag = "F2_s" + str(len(lags)) + "_" + "-".join(map(str, lags))
        cells[tag] = F2SubsetSum(q=Q, L=L, lags=lags, eta=ETA).version
    for d in (2, 4):
        cells[f"F3_d{d}_M6"] = F3RandomPoly(q=Q, L=L, d=d, M=6, amp=0.9 / Q, eta=ETA,
                                             draw_seed=100 + d).version
    for r in (0.3, 0.7):
        cells[f"F4_r{r}_K6_M2"] = F4MixedProfile(q=Q, L=L, K=6, M=2, r=r, eta=ETA,
                                                  draw_seed=200 + int(r * 10)).version
    cells["F5_iid"] = F5IID(q=Q, L=L).version
    cells["F5_max_sum"] = F5MaxSum(q=Q, L=L, eta=ETA).version
    cells["F1_k1_entropy_matched_control"] = cells["F1_k1"]  # alias (PLAN F5b)
    return cells


def main():
    pilot = json.loads(Path("runs/local/m5_pilot/pilot.json").read_text())
    protocol = {
        "protocol_id": "dlx-v1",
        "date": "2026-08-04",
        "plan_section": "v2/PLAN.md §9 checklist, all items",
        "prior_art_recheck": {
            "date": "2026-08-04",
            "verdict": "no direct collision",
            "delta_since_section_0": (
                "added neighbor: 'A Sharper Picture of Generalization in Transformers' "
                "(arXiv:2605.20988) — generalization bounds parameterized by max Fourier "
                "degree and spectral width on PARITY-style tasks; still not a data-side "
                "difficulty proxy from measured profiles on sequence laws."),
        },
        "domain": {"q": Q, "L": L, "eta": ETA},
        "budget_grid_tokens": BUDGETS,
        "seeds": SEEDS,
        "theta_bits": THETA,
        "family_versions": family_versions(),
        "pilot": {
            "rule": pilot["rule"],
            "mid_family": pilot["mid_family"],
            "budget_tokens": pilot["pilot_budget_tokens"],
            "results_final_val_ce": {k: v["final_val_ce"] for k, v in pilot["results"].items()},
            "selected": pilot["selected"],
            "note": ("large config underfit at the pilot budget (final CE worse than small); "
                     "the literal preregistered rule selects the smallest config within 10% "
                     "of the largest config's final loss; recorded as a finding, rule applied "
                     "verbatim without post-hoc change."),
        },
        "learner_config": {**pilot["selected_config"], "vocab": Q, "ctx_len": L,
                           "lr": 1e-3, "weight_decay": 0.1, "grad_clip": 1.0,
                           "tie_weights": True, "dropout": 0.0, "mlp_mult": 4},
        "tokens_per_step": 1024,
        "metrics": TREND_TEST_SPEC,
        "falsification_criteria_verbatim": FALSIFICATION_CRITERIA_VERBATIM,
        "tier_table_verbatim": TIER_TABLE_VERBATIM,
        "milestone_gates": "v2/PLAN.md §8 milestone table (M0-M10), frozen verbatim",
        "real_ladders": {
            "R1_language": {
                "teacher": {"model": "Qwen/Qwen2.5-0.5B",
                            "revision": "060db6499f32faf8b98477b0a26969ef7d8b9987"},
                "train_token_cap": 20_000_000,
                "val_tokens": 256_000,
                "datasets": [
                    {"rung": 1, "name": "iid_random_tokens", "source": "synthetic",
                     "tokenization": "vocab-matched to rung-3"},
                    {"rung": 2, "name": "synthetic_F2_s2_corpus", "source": "synthetic",
                     "family": "F2_q32_L64_lags(1,16)_eta0.1"},
                    {"rung": 3, "name": "roneneldan/TinyStories",
                     "revision": "f54c09fd23315a6f9c86f9dc80f725de7d8f9c64",
                     "tokenization": "char/small-BPE (fitted, hash recorded)"},
                    {"rung": 4, "name": "LTCB/enwik8",
                     "revision": "8d9ca88afe67dc9713ae7aa970f3fd946cc41b10",
                     "tokenization": "character (byte values 0..255)"},
                    {"rung": 5, "name": "Salesforce/wikitext", "config": "wikitext-2-raw-v1",
                     "revision": "b08601e04326c79dfdd32d625aee71d232d685c3",
                     "tokenization": "~10k BPE (fitted, hash recorded)"},
                    {"rung": 6, "name": "codeparrot/codeparrot-clean",
                     "revision": "35a59fb025bc0a102f7d96eac09d145b896d487b",
                     "filter": "python files only",
                     "tokenization": "BPE (fitted, hash recorded)"},
                    {"rung": 7, "name": "arithmetic_corpus", "source": "synthetic",
                     "generator": "a+b mod p and chained ops, p in {17,31,47}"},
                ],
                "q_covariate": "q varies per rung and is logged; profile features are relative tail weights",
            },
            "R2_image": {
                "datasets": ["gaussian_noise_control", "MNIST", "FashionMNIST", "SVHN",
                             "CIFAR10", "STL10_downsampled"],
                "source": "torchvision official splits; checksums verified at download, recorded in manifests",
                "vq_convention": {"patch": "8x8", "codebook_K": 512,
                                  "arch": "fixed tiny VQ-VAE (d=64, 2 enc/dec blocks)",
                                  "training": "one per dataset, 50k steps, seed 0",
                                  "hash": "tokenizer config + data hash recorded in manifest"},
                "floor": "code-prior entropy per dataset",
            },
            "R3_tabular": {
                "source": "OpenML / sklearn",
                "resolution_rule": ("first exact-name OpenML match resolved at download; "
                                    "dataset_id + version recorded in configs/openml_resolution.json "
                                    "and in each cell manifest BEFORE learner training"),
                "datasets": [
                    "iris", "balance-scale", "tic-tac-toe", "car",
                    "mushroom", "kr-vs-kp", "credit-g", "churn",
                    "adult (subset)", "bank-marketing-8k",
                    "connect-4 (subsampled)", "mini-boone",
                ],
                "binning": "numerics -> 16 quantile bins; per-coordinate alphabet sizes logged",
                "learner": "fixed MLP 2x256, identical across datasets, early stopping",
                "extra_measurement": "coordinate-masking total influence per dataset",
            },
        },
        "modal": {
            "app": "dlx-degree-learnability",
            "volume": "dlx-runs",
            "gpu_cap_hours": 80,
            "spend_limit_multiplier": 1.5,
            "cpu_budget_usd": 5.0,
        },
        "deviations_log": ("see v2/experiments/degree_learnability/README.md deviation log "
                           "(M3 margin, int64 overflow, M4 tuning, F2 unlearnable finding)"),
    }

    canonical = json.dumps(protocol, sort_keys=True)
    protocol_hash = hashlib.sha256(canonical.encode()).hexdigest()
    protocol["protocol_hash"] = protocol_hash

    out = Path("configs/protocol_v1.json")
    out.parent.mkdir(parents=True, exist_ok=True)
    out.write_text(json.dumps(protocol, indent=2, sort_keys=True))
    print(f"protocol_hash = {protocol_hash}")
    print(f"wrote {out} ({len(canonical)} bytes canonical)")


if __name__ == "__main__":
    main()
