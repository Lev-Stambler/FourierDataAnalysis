"""Analyze paired architecture differences against paired Fourier-CE overlap."""

from __future__ import annotations

import itertools
import json
from pathlib import Path

import numpy as np

from dlx.analysis.architecture_matching import (
    prediction_scores,
    stratified_corpus_bootstrap_improvement,
)
from dlx.analysis.character_response import architecture_spectrum_overlap
from dlx.analysis.floor_independent import normalized_learning_time
from dlx.analysis.paired_contrast import (
    association,
    blocked_spearman_permutation,
    fit_standardized_contrast,
    holm_adjust,
    predict_standardized_contrast,
    stratified_association_bootstrap,
)
from dlx.protocol.frozen import (
    file_sha256,
    load_frozen_protocol,
    write_hash_once,
    write_json_once,
)

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs/local/v30_architecture_spectrum"
FEATURE = "delta_fourier_ce_overlap"


def _verify_inputs(protocol: dict) -> dict[str, str]:
    mapping = {
        "data_manifest_sha256": "data_manifest.json",
        "profile_manifest_sha256": "profile_manifest.json",
        "fourier_ce_kernel_sha256": "fourier_ce_kernel.json",
        "pilot_results_sha256": "pilot_results.json",
        "pilot_analysis_sha256": "pilot_analysis.json",
        "expansion_results_sha256": "expansion_results.json",
        "expansion_analysis_sha256": "expansion_analysis.json",
        "expansion_audit_sha256": "expansion_audit.json",
    }
    observed = {name: file_sha256(OUT / path) for name, path in mapping.items()}
    if observed != protocol["locked_inputs"]:
        raise ValueError("v3.3 locked input mismatch")
    return observed


def _metric(row: dict, target: str) -> float:
    if target == "normalized_learning_time":
        return normalized_learning_time(
            row["normalized_curve_area"], row["final_ce_fraction"]
        )
    return float(row[target])


def _panel_rows(
    *,
    analysis_name: str,
    panel: str,
    left: str,
    right: str,
    hardness: dict,
    targets: tuple[str, ...],
) -> list[dict]:
    analysis = json.loads((OUT / analysis_name).read_text())
    source_rows = analysis["rows"]
    rows = []
    for dataset in sorted({row["dataset"] for row in source_rows}):
        selected = [row for row in source_rows if row["dataset"] == dataset]
        by_architecture = {row["architecture"]: row for row in selected}
        if len(selected) != len(by_architecture) or {left, right} - by_architecture.keys():
            raise ValueError(f"incomplete architecture rows for {dataset}")
        profile = json.loads((OUT / "profiles" / f"{dataset}.json").read_text())
        overlaps = {
            architecture: architecture_spectrum_overlap(
                profile["support_energy"], hardness[architecture]
            )
            for architecture in (left, right)
        }
        row = {
            "dataset": dataset,
            "panel": panel,
            "stratum": by_architecture[left]["stratum"],
            "left_architecture": left,
            "right_architecture": right,
            "left_fourier_ce_overlap": overlaps[left],
            "right_fourier_ce_overlap": overlaps[right],
            FEATURE: overlaps[left] - overlaps[right],
        }
        for target in targets:
            left_value = _metric(by_architecture[left], target)
            right_value = _metric(by_architecture[right], target)
            row[f"left_{target}"] = left_value
            row[f"right_{target}"] = right_value
            row[f"delta_{target}"] = left_value - right_value
        rows.append(row)
    return rows


def _association_panel(
    rows: list[dict],
    *,
    target: str,
    bootstrap_samples: int,
    bootstrap_seed: int,
    permutations: int,
    permutation_seed: int,
) -> dict:
    target_name = f"delta_{target}"
    x = np.asarray([row[FEATURE] for row in rows], dtype=float)
    y = np.asarray([row[target_name] for row in rows], dtype=float)
    return {
        "n_corpora": len(rows),
        **association(x, y),
        "stratified_corpus_bootstrap": stratified_association_bootstrap(
            rows,
            feature=FEATURE,
            target=target_name,
            samples=bootstrap_samples,
            seed=bootstrap_seed,
        ),
        "blocked_within_stratum_spearman": blocked_spearman_permutation(
            rows,
            feature=FEATURE,
            target=target_name,
            permutations=permutations,
            seed=permutation_seed,
        ),
    }


def _fit_fixed_effects(rows: list[dict], target: str, *, add_feature: bool) -> dict:
    strata = sorted({row["stratum"] for row in rows})
    x = np.asarray([row[FEATURE] for row in rows], dtype=float)
    mean = float(x.mean())
    scale = float(x.std())
    if scale <= 0.0:
        raise ValueError("contrast feature is constant")
    columns = [np.ones(len(rows))]
    names = ["intercept"]
    for stratum in strata[1:]:
        columns.append(np.asarray([row["stratum"] == stratum for row in rows], dtype=float))
        names.append(f"stratum[{stratum}]")
    if add_feature:
        columns.append((x - mean) / scale)
        names.append(FEATURE)
    coefficients = np.linalg.lstsq(
        np.column_stack(columns),
        np.asarray([row[f"delta_{target}"] for row in rows], dtype=float),
        rcond=None,
    )[0]
    return {
        "kind": "contrast_stratum_fixed_effects",
        "target": target,
        "strata": strata,
        "feature_mean": mean,
        "feature_scale": scale,
        "columns": names,
        "coefficients": coefficients.tolist(),
        "training_datasets": sorted(row["dataset"] for row in rows),
    }


def _predict_fixed_effects(model: dict, rows: list[dict]) -> np.ndarray:
    strata = model["strata"]
    columns = [np.ones(len(rows))]
    for stratum in strata[1:]:
        columns.append(np.asarray([row["stratum"] == stratum for row in rows], dtype=float))
    if FEATURE in model["columns"]:
        x = np.asarray([row[FEATURE] for row in rows], dtype=float)
        columns.append((x - model["feature_mean"]) / model["feature_scale"])
    return np.column_stack(columns) @ np.asarray(model["coefficients"], dtype=float)


def _transfer(
    development: list[dict],
    expansion: list[dict],
    *,
    target: str,
    samples: int,
    seed: int,
) -> dict:
    x_development = np.asarray([row[FEATURE] for row in development], dtype=float)
    y_development = np.asarray(
        [row[f"delta_{target}"] for row in development], dtype=float
    )
    x_expansion = np.asarray([row[FEATURE] for row in expansion], dtype=float)
    actual = np.asarray([row[f"delta_{target}"] for row in expansion], dtype=float)
    model = fit_standardized_contrast(x_development, y_development)
    model.update(
        {
            "target": target,
            "training_datasets": sorted(row["dataset"] for row in development),
        }
    )
    baseline = np.full(len(expansion), y_development.mean())
    predicted = predict_standardized_contrast(model, x_expansion)
    score_rows = [
        {"dataset": row["dataset"], "stratum": row["stratum"], "actual": value}
        for row, value in zip(expansion, actual, strict=True)
    ]
    primary_bootstrap = stratified_corpus_bootstrap_improvement(
        score_rows, baseline, predicted, samples=samples, seed=seed
    )

    fixed_baseline_model = _fit_fixed_effects(development, target, add_feature=False)
    fixed_fourier_model = _fit_fixed_effects(development, target, add_feature=True)
    fixed_baseline = _predict_fixed_effects(fixed_baseline_model, expansion)
    fixed_fourier = _predict_fixed_effects(fixed_fourier_model, expansion)
    fixed_bootstrap = stratified_corpus_bootstrap_improvement(
        score_rows, fixed_baseline, fixed_fourier, samples=samples, seed=seed + 100
    )
    return {
        "primary_mean_baseline": {
            "baseline": prediction_scores(actual, baseline),
            "fourier_contrast": prediction_scores(actual, predicted),
            "stratified_paired_corpus_bootstrap": primary_bootstrap,
            "model": model,
            "expected_positive_coefficient": model["coefficient"] > 0.0,
        },
        "stratum_fixed_effect_sensitivity": {
            "baseline": prediction_scores(actual, fixed_baseline),
            "fourier_contrast": prediction_scores(actual, fixed_fourier),
            "stratified_paired_corpus_bootstrap": fixed_bootstrap,
            "baseline_model": fixed_baseline_model,
            "fourier_model": fixed_fourier_model,
        },
    }


def main() -> dict:
    protocol = load_frozen_protocol(ROOT / "configs/protocol_v3.3.json")
    measurement = load_frozen_protocol(ROOT / "configs/protocol_v3.1.json")
    locks = _verify_inputs(protocol)
    kernel = json.loads((OUT / "fourier_ce_kernel.json").read_text())
    hardness = kernel["architecture_hardness"]
    targets = tuple(protocol["co_primary_targets"] + protocol["diagnostic_targets"])
    development = _panel_rows(
        analysis_name="pilot_analysis.json",
        panel="development",
        left="rope",
        right="nope",
        hardness=hardness,
        targets=targets,
    )
    expansion = _panel_rows(
        analysis_name="expansion_analysis.json",
        panel="expansion",
        left="rope",
        right="nope",
        hardness=hardness,
        targets=targets,
    )
    if len(development) != 24 or len(expansion) != 48:
        raise ValueError("paired analysis requires 24 development and 48 expansion rows")

    inference = protocol["inference"]
    panels = {
        "development": development,
        "expansion": expansion,
        "combined": development + expansion,
    }
    associations = {}
    for target_index, target in enumerate(targets):
        associations[target] = {}
        for panel_index, (panel, rows) in enumerate(panels.items()):
            offset = 10 * target_index + panel_index
            associations[target][panel] = _association_panel(
                rows,
                target=target,
                bootstrap_samples=int(inference["association_bootstrap_samples"]),
                bootstrap_seed=int(inference["association_bootstrap_seed"]) + offset,
                permutations=int(inference["blocked_permutations"]),
                permutation_seed=int(inference["blocked_permutation_seed"]) + offset,
            )

    raw_primary_p = {
        target: associations[target]["expansion"]["pearson_two_sided_p"]
        for target in protocol["co_primary_targets"]
    }
    adjusted_primary_p = holm_adjust(raw_primary_p)
    primary_inference = {
        target: {
            "raw_expansion_pearson_p": raw_primary_p[target],
            "holm_adjusted_p": adjusted_primary_p[target],
            "holm_significant": adjusted_primary_p[target] <= inference["alpha"],
            "directionally_aligned": associations[target]["expansion"][
                "ols_slope_raw_units"
            ]
            > 0.0,
        }
        for target in protocol["co_primary_targets"]
    }

    transfers = {
        target: _transfer(
            development,
            expansion,
            target=target,
            samples=int(inference["transfer_bootstrap_samples"]),
            seed=int(inference["transfer_bootstrap_seed"]) + index,
        )
        for index, target in enumerate(targets)
    }

    all_pairs = {}
    architectures = [row["id"] for row in measurement["architectures"]]
    for left, right in itertools.combinations(architectures, 2):
        pair_development = _panel_rows(
            analysis_name="pilot_analysis.json",
            panel="development",
            left=left,
            right=right,
            hardness=hardness,
            targets=tuple(protocol["co_primary_targets"]),
        )
        pair_expansion = _panel_rows(
            analysis_name="expansion_analysis.json",
            panel="expansion",
            left=left,
            right=right,
            hardness=hardness,
            targets=tuple(protocol["co_primary_targets"]),
        )
        all_pairs[f"{left}_minus_{right}"] = {
            target: {
                "expansion": association(
                    np.asarray([row[FEATURE] for row in pair_expansion]),
                    np.asarray([row[f"delta_{target}"] for row in pair_expansion]),
                ),
                "combined": association(
                    np.asarray([row[FEATURE] for row in pair_development + pair_expansion]),
                    np.asarray(
                        [
                            row[f"delta_{target}"]
                            for row in pair_development + pair_expansion
                        ]
                    ),
                ),
            }
            for target in protocol["co_primary_targets"]
        }

    result = {
        "status": "post-outcome exploratory paired architecture analysis complete",
        "protocol_hash": protocol["protocol_hash"],
        "measurement_protocol_hash": measurement["protocol_hash"],
        "locked_inputs": locks,
        "orientation": protocol["orientation"],
        "primary_inference": primary_inference,
        "associations": associations,
        "development_to_expansion_transfer": transfers,
        "all_architecture_pair_diagnostic": all_pairs,
        "interpretation": {
            "confirmatory_verdict": None,
            "rule": "prediction improvement and positive directional alignment are reported separately",
            "bridge_status": (
                "DIRECTIONALLY_ALIGNED"
                if all(row["directionally_aligned"] for row in primary_inference.values())
                else "DIRECTIONALLY_INVERTED"
            ),
        },
        "rows": development + expansion,
    }
    digest = write_json_once(OUT / "paired_contrast_analysis.json", result)
    write_hash_once(OUT / "paired_contrast_analysis.sha256", digest)
    print(
        json.dumps(
            {
                "status": result["status"],
                "bridge_status": result["interpretation"]["bridge_status"],
                "primary_inference": primary_inference,
                "primary_transfer": {
                    target: transfers[target]["primary_mean_baseline"]
                    for target in protocol["co_primary_targets"]
                },
                "analysis_sha256": digest,
            },
            indent=2,
        )
    )
    return result


if __name__ == "__main__":
    main()
