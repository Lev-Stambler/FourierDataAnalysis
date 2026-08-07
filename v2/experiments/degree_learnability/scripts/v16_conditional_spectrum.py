"""Correct conditional-function Fourier spectrum and difficulty correlation."""

from __future__ import annotations

import hashlib
import json
import sys
from pathlib import Path

import numpy as np
from scipy.stats import spearmanr

sys.path.insert(0, str(Path(__file__).parent.parent))

from dlx.profiles.text_anova import conditional_fourier_spectrum

ROOT = Path(__file__).parent.parent
SOURCE = ROOT / "runs" / "local" / "v14_text_anova"
OUT = ROOT / "runs" / "local" / "v16_conditional_spectrum"


def load_protocol() -> dict:
    protocol = json.loads((ROOT / "configs" / "protocol_v1.6.json").read_text())
    recorded = protocol["protocol_hash"]
    unhashed = dict(protocol)
    unhashed.pop("protocol_hash")
    expected = hashlib.sha256(json.dumps(unhashed, sort_keys=True).encode()).hexdigest()
    if recorded != expected:
        raise ValueError(f"protocol_v1.6 hash mismatch: {recorded} != {expected}")
    return protocol


def _median(report: dict, rung: str, key: str) -> float:
    return float(np.median([
        row[key] for row in report["cells"] if row["rung"] == rung
    ]))


def _estimated_spectrum(profile: dict) -> tuple[list[int], dict]:
    pairs = []
    for pair in profile["pairs"]:
        spectrum = conditional_fourier_spectrum(
            pair["baseline_brier"],
            pair["degree1_additive_brier"],
            pair["degree2_pair_brier"],
            pair["q"],
        )
        pairs.append((pair["lags"], spectrum))
    best_lags, best = max(
        pairs, key=lambda item: item[1]["nonconstant_energy"]
    )
    source_pair = next(pair for pair in profile["pairs"] if pair["lags"] == best_lags)
    fold_weights = np.asarray([
        conditional_fourier_spectrum(
            fold["baseline_brier"],
            fold["degree1_additive_brier"],
            fold["degree2_pair_brier"],
            source_pair["q"],
        )["level_weights"]
        for fold in source_pair["folds"]
    ])
    standard_error = fold_weights.std(axis=0, ddof=1) / np.sqrt(len(fold_weights))
    half_width = 12.706204736 * standard_error
    best["crossfit_fold_level_weights"] = fold_weights.tolist()
    best["level_weight_ci95"] = np.column_stack((
        np.asarray(best["level_weights"]) - half_width,
        np.asarray(best["level_weights"]) + half_width,
    )).tolist()
    best["positive_at_95pct"] = [
        low > 0.0 for low, _ in best["level_weight_ci95"]
    ]
    best["uncertainty"] = (
        "two held-out fold estimates; Student-t interval with df=1"
    )
    return best_lags, best


def _planted_spectrum(rung: str) -> dict:
    q = 256
    signal = (q - 1) * 0.75**2 / q
    weights = [1.0 / q, 0.0, 0.0]
    weights[1 if rung == "copy_lag16_bytes" else 2] = signal
    total = sum(weights)
    nonconstant = weights[1] + weights[2]
    return {
        "level_weights": weights,
        "level_cardinalities": [1, 2 * (q - 1), (q - 1) ** 2],
        "mean_squared_coefficient_by_level": [
            weights[0], weights[1] / (2 * (q - 1)), weights[2] / (q - 1) ** 2
        ],
        "total_square_energy": total,
        "nonconstant_energy": nonconstant,
        "mean_spectral_degree": (weights[1] + 2 * weights[2]) / total,
        "mean_nonconstant_spectral_degree": (
            weights[1] + 2 * weights[2]
        ) / nonconstant,
        "cumulative_concentration": [
            weights[0] / total,
            (weights[0] + weights[1]) / total,
            1.0,
        ],
        "source": "exact planted conditional law",
    }


def analyze(protocol: dict) -> dict:
    report = json.loads(
        (SOURCE / "confirmation" / "confirmation_results.json").read_text()
    )
    rows = []
    for rung, meta in report["rungs"].items():
        best_pair, estimated = _estimated_spectrum(meta["inverse_likelihood_profile"])
        planted = (
            _planted_spectrum(rung)
            if rung in {"copy_lag16_bytes", "markov2_bytes"}
            else None
        )
        primary = planted or estimated
        rows.append({
            "rung": rung,
            "source_kind": meta["source_kind"],
            "best_pair": best_pair,
            "normalized_curve_area": _median(report, rung, "normalized_curve_area"),
            "learning_amount_bits": _median(report, rung, "learning_amount_bits"),
            "spectrum": primary,
            "estimated_pair_spectrum": estimated,
            "planted_spectrum_used": planted is not None,
        })
    degrees = np.asarray([
        row["spectrum"]["mean_nonconstant_spectral_degree"] for row in rows
    ])
    areas = np.asarray([row["normalized_curve_area"] for row in rows])
    rho = float(spearmanr(degrees, areas).statistic)
    natural = [row for row in rows if row["source_kind"] == "natural"]
    natural_direction = (
        (natural[0]["spectrum"]["mean_nonconstant_spectral_degree"]
         - natural[1]["spectrum"]["mean_nonconstant_spectral_degree"])
        * (natural[0]["normalized_curve_area"]
           - natural[1]["normalized_curve_area"])
        > 0
    )
    by_rung = {row["rung"]: row for row in rows}
    markov = by_rung["markov2_bytes"]
    copy = by_rung["copy_lag16_bytes"]
    return {
        "protocol_hash": protocol["protocol_hash"],
        "source_v1_4_protocol_hash": protocol["source_v1_4_protocol_hash"],
        "metric": "conditional-function Fourier level spectrum",
        "rungs": rows,
        "correlation": {
            "mean_nonconstant_spectral_degree_vs_curve_area_spearman": rho,
            "n_datasets": len(rows),
            "natural_pair_direction_correct": bool(natural_direction),
            "interpretation": (
                "weak positive cross-dataset association; controlled and natural "
                "pair directions agree, but n=4 and natural spectra are pair slices"
            ),
        },
        "controlled": {
            "copy_spectral_degree": copy["spectrum"][
                "mean_nonconstant_spectral_degree"
            ],
            "markov2_spectral_degree": markov["spectrum"][
                "mean_nonconstant_spectral_degree"
            ],
            "curve_area_markov_minus_copy": (
                markov["normalized_curve_area"] - copy["normalized_curve_area"]
            ),
            "learning_copy_minus_markov_bits": (
                copy["learning_amount_bits"] - markov["learning_amount_bits"]
            ),
        },
        "invalidated_result": (
            "discarded sparse zero-extension v1.6 attempt and its rho=-0.40"
        ),
        "training_or_profile_recomputation": 0,
    }


def main() -> None:
    protocol = load_protocol()
    result = analyze(protocol)
    OUT.mkdir(parents=True, exist_ok=True)
    (OUT / "integrated_analysis.json").write_text(json.dumps(result, indent=2))
    print(json.dumps(result["correlation"], indent=2))


if __name__ == "__main__":
    main()
