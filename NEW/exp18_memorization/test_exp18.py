from __future__ import annotations

import numpy as np

from exp18_memorization.campaign import (
    CONFIRMATION_SEEDS,
    MODEL_NAMES,
    Recipe,
    balanced_sample_indices,
    first_threshold_hits,
    memorization_recipes,
    promote_optimizer_winners,
    recipe_slug,
    row_score,
    summarize_confirmation,
)


def row(model: str, family: str, nll: float, *, success: bool, step: int = 12):
    recipe = Recipe(
        family,
        0.01 if family == "adamw" else 0.06,
        0.01 if family == "adamw" else 0.012,
        weight_decay=0.0,
    )
    return {
        "status": "complete",
        "model": model,
        "recipe": recipe.__dict__,
        "success": success,
        "strict_success": nll <= 0.001 and success,
        "final": {"nll": nll},
        "threshold_hits": {
            "nll_le_0.01": {"step": step, "elapsed_seconds": float(step)}
            if success
            else None
        },
        "performance": {"elapsed_seconds": float(step)},
    }


def test_exactly_eight_serious_matched_tracks_are_screened() -> None:
    assert len(MODEL_NAMES) == 8
    assert len(set(MODEL_NAMES)) == 8
    assert "dense-group" not in MODEL_NAMES


def test_lr_grid_is_wide_unique_and_regularization_free() -> None:
    recipes = memorization_recipes()
    assert len(recipes) == 8
    assert sum(value.family == "adamw" for value in recipes) == 4
    assert sum(value.family == "muon" for value in recipes) == 4
    assert max(value.body_lr for value in recipes) == 0.96
    assert all(value.weight_decay == 0 for value in recipes)
    assert len({recipe_slug(value) for value in recipes}) == len(recipes)


def test_physical_repetition_is_balanced_and_explicit() -> None:
    indices = balanced_sample_indices((3, 9), 7)
    np.testing.assert_array_equal(indices, [3, 9, 3, 9, 3, 9, 3])
    assert balanced_sample_indices((4,), 4).tolist() == [4, 4, 4, 4]


def test_threshold_hits_retain_first_step_and_time() -> None:
    hits = first_threshold_hits(
        [
            {"step": 0, "elapsed_seconds": 0.0, "nll": 10.0, "token_accuracy": 0.0},
            {"step": 4, "elapsed_seconds": 2.0, "nll": 0.5, "token_accuracy": 0.9},
            {"step": 8, "elapsed_seconds": 4.0, "nll": 0.005, "token_accuracy": 1.0},
        ]
    )
    assert hits["nll_le_1"]["step"] == 4
    assert hits["nll_le_0.01"]["step"] == 8
    assert hits["nll_le_0.001"] is None
    assert hits["perfect_token_accuracy"]["elapsed_seconds"] == 4.0


def test_success_and_speed_dominate_final_nll_selection() -> None:
    fast_success = row(MODEL_NAMES[0], "adamw", 0.009, success=True, step=8)
    slow_success = row(MODEL_NAMES[0], "adamw", 0.001, success=True, step=20)
    failed = row(MODEL_NAMES[0], "adamw", 0.02, success=False, step=4)
    assert min([slow_success, failed, fast_success], key=row_score) is fast_success


def test_promotion_and_confirmation_summary_are_family_balanced() -> None:
    screen = []
    confirmation = []
    for model in MODEL_NAMES:
        screen.extend(
            [
                row(model, "adamw", 0.008, success=True, step=12),
                row(model, "muon", 0.006, success=True, step=8),
            ]
        )
        for seed in CONFIRMATION_SEEDS:
            for family in ("adamw", "muon"):
                value = row(model, family, 0.005, success=True, step=10)
                value["seed"] = seed
                confirmation.append(value)
    promoted = promote_optimizer_winners(screen)
    assert set(promoted) == set(MODEL_NAMES)
    assert all(set(values) == {"adamw", "muon"} for values in promoted.values())
    summary, verdict = summarize_confirmation(confirmation)
    assert set(summary) == set(MODEL_NAMES)
    assert verdict == "all_matched_architectures_memorize_two_blocks"


def test_confirmation_failure_is_evidence_not_a_reporting_crash() -> None:
    confirmation = []
    for model in MODEL_NAMES:
        for seed in CONFIRMATION_SEEDS:
            successful = row(model, "adamw", 0.005, success=True, step=10)
            successful["seed"] = seed
            confirmation.append(successful)
            failed = {
                "status": "failed",
                "model": model,
                "recipe": Recipe("muon", 0.96, 0.096).__dict__,
                "seed": seed,
                "success": False,
                "strict_success": False,
                "failure": "non-finite optimizer state",
            }
            confirmation.append(failed)
    summary, verdict = summarize_confirmation(confirmation)
    assert verdict == "all_matched_architectures_memorize_two_blocks"
    assert all(row["winner"]["family"] == "adamw" for row in summary.values())
