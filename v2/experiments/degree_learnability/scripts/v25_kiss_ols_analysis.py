"""Post-v2.4 KISS diagnostic: plain OLS, minimal features, and covariate shift."""

from __future__ import annotations

import json
import math
import sys
from pathlib import Path

import numpy as np
from scipy.stats import spearmanr
from sklearn.metrics import mean_squared_error, r2_score

sys.path.insert(0, str(Path(__file__).parent.parent))
from dlx.analysis.spectrum_predictor import (
    fit_frozen_ols,
    nested_loco_predictions,
    predict_frozen_ols,
)
from dlx.analysis.text_panel import (
    CONTROL_FEATURES,
    TARGETS,
    attach_sampled_features,
    load_v24_confirmation_rows,
    load_v24_development_rows,
)

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs/local/v25_kiss_diagnostic"
RESULT_PATH = OUT / "analysis.json"
HIGHER_DEGREE_DIR = OUT / "profiles"

FEATURE_SETS = {
    "configuration_only": (),
    "locality_only": ("energy_weighted_log_radius",),
    "degree_energy_locality": (
        "resolved_nonconstant_energy",
        "mean_nonconstant_degree_best_pair",
        "energy_weighted_log_radius",
    ),
    "fourier_compact": (
        "resolved_nonconstant_energy",
        "energy_weighted_log_radius",
        "mean_nonconstant_degree_best_pair",
        "radial_spectral_entropy_bits",
    ),
    "non_fourier_only": CONTROL_FEATURES,
    "combined_kiss": (
        "resolved_nonconstant_energy",
        "mean_nonconstant_degree_best_pair",
        "energy_weighted_log_radius",
        *CONTROL_FEATURES,
    ),
    "sampled_degree_kiss": (
        "sampled_nonconstant_energy_through_degree3",
        "sampled_mean_degree_through_degree3",
    ),
    "combined_sampled_kiss": (
        "sampled_nonconstant_energy_through_degree3",
        "sampled_mean_degree_through_degree3",
        *CONTROL_FEATURES,
    ),
    "combined_all_spectra_kiss": (
        "resolved_nonconstant_energy",
        "mean_nonconstant_degree_best_pair",
        "sampled_nonconstant_energy_through_degree3",
        "sampled_mean_degree_through_degree3",
        *CONTROL_FEATURES,
    ),
}
SAMPLED_FEATURE_SETS = {
    "sampled_degree_kiss",
    "combined_sampled_kiss",
    "combined_all_spectra_kiss",
}


def _fit_predict(
    train_rows: list[dict],
    test_rows: list[dict],
    feature_names: tuple[str, ...],
    target: str,
) -> dict:
    y_test = np.asarray([row[target] for row in test_rows], dtype=float)
    artifact = fit_frozen_ols(
        train_rows,
        target=target,
        feature_names=feature_names,
        reject_constant_features=False,
    )
    predicted = predict_frozen_ols(artifact, test_rows)
    return {
        "n_train_rows": len(train_rows),
        "n_test_rows": len(test_rows),
        "rmse": math.sqrt(mean_squared_error(y_test, predicted)),
        "r2": float(r2_score(y_test, predicted)),
        "standardized_coefficients": artifact["coefficients"],
        "intercept": artifact["intercept"],
    }


def _shift(development: list[dict], confirmation: list[dict]) -> dict:
    result = {}
    feature_names = tuple(
        dict.fromkeys(name for names in FEATURE_SETS.values() for name in names)
    )
    for name in feature_names:
        train = np.asarray([row["features"][name] for row in development], dtype=float)
        test = np.asarray([row["features"][name] for row in confirmation], dtype=float)
        scale = float(train.std()) or 1.0
        z = (test - float(train.mean())) / scale
        result[name] = {
            "development_min": float(train.min()),
            "development_max": float(train.max()),
            "confirmation_min": float(test.min()),
            "confirmation_max": float(test.max()),
            "confirmation_max_abs_development_z": float(np.max(np.abs(z))),
            "confirmation_fraction_outside_development_range": float(
                np.mean((test < train.min()) | (test > train.max()))
            ),
        }
    return result


def _confirmation_correlations(rows: list[dict]) -> dict:
    features = (
        "sampled_nonconstant_energy_through_degree3",
        "sampled_mean_degree_through_degree3",
    )
    output = {}
    for target in TARGETS:
        output[target] = {}
        for configuration in sorted({row["configuration"] for row in rows}):
            selected = [row for row in rows if row["configuration"] == configuration]
            output[target][configuration] = {}
            for feature in features:
                rho, p_value = spearmanr(
                    [row["features"][feature] for row in selected],
                    [row[target] for row in selected],
                )
                output[target][configuration][feature] = {
                    "spearman_rho": float(rho),
                    "two_sided_p": float(p_value),
                }
    return output


def analyze() -> dict:
    development = load_v24_development_rows(ROOT)
    confirmation = load_v24_confirmation_rows(ROOT)
    development_natural = attach_sampled_features(
        [row for row in development if row["stride"] == 1], HIGHER_DEGREE_DIR
    )
    confirmation_natural = attach_sampled_features(confirmation, HIGHER_DEGREE_DIR)
    all_natural = development_natural + confirmation_natural
    scores = {}
    for target in TARGETS:
        scores[target] = {}
        for label, features in FEATURE_SETS.items():
            all_development = (
                None
                if label in SAMPLED_FEATURE_SETS
                else {
                    key: value
                    for key, value in nested_loco_predictions(
                        development,
                        target=target,
                        feature_names=features,
                        kind="ols",
                    ).items()
                    if key in {"rmse", "group_balanced_rmse", "r2"}
                }
            )
            confirmation_all = (
                None
                if label in SAMPLED_FEATURE_SETS
                else _fit_predict(development, confirmation_natural, features, target)
            )
            scores[target][label] = {
                "development_loco_all_rows": all_development,
                "development_loco_natural_only": {
                    key: value
                    for key, value in nested_loco_predictions(
                        development_natural,
                        target=target,
                        feature_names=features,
                        kind="ols",
                    ).items()
                    if key in {"rmse", "group_balanced_rmse", "r2"}
                },
                "all_25_corpora_loco_posthoc": {
                    key: value
                    for key, value in nested_loco_predictions(
                        all_natural,
                        target=target,
                        feature_names=features,
                        kind="ols",
                    ).items()
                    if key in {"rmse", "group_balanced_rmse", "r2"}
                },
                "confirmation_fit_all_development": confirmation_all,
                "confirmation_fit_natural_development": _fit_predict(
                    development_natural, confirmation_natural, features, target
                ),
            }
    return {
        "status": "post-confirmatory KISS diagnostic; not a frozen test",
        "model": "unpenalized linear regression with standardized inputs",
        "n_development_corpora": len({row["dataset"] for row in development}),
        "n_development_rows": len(development),
        "n_development_natural_rows": len(development_natural),
        "n_confirmation_corpora": len({row["dataset"] for row in confirmation_natural}),
        "n_confirmation_natural_rows": len(confirmation_natural),
        "feature_sets": {key: list(value) for key, value in FEATURE_SETS.items()},
        "scores": scores,
        "feature_shift_natural_confirmation_vs_all_development": _shift(
            development_natural, confirmation_natural
        ),
        "posthoc_confirmation_spearman": _confirmation_correlations(
            confirmation_natural
        ),
    }


def main() -> None:
    result = analyze()
    OUT.mkdir(parents=True, exist_ok=True)
    RESULT_PATH.write_text(json.dumps(result, indent=2))
    compact = {
        target: {
            label: {
                fit: {
                    "rmse": values["rmse"],
                    "r2": values["r2"],
                }
                for fit, values in models.items()
                if fit.startswith("confirmation_") and values is not None
            }
            for label, models in by_feature.items()
        }
        for target, by_feature in result["scores"].items()
    }
    print(json.dumps(compact, indent=2))


if __name__ == "__main__":
    main()
