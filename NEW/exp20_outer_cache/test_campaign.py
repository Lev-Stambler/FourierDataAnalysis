from __future__ import annotations

from exp20_outer_cache.campaign import (
    EXECUTION_VARIANTS,
    fill_full_waves,
    promote_recipes,
    screen_recipes,
    tuning_extensions,
)


def test_execution_matrix_fills_exactly_eight_gpus() -> None:
    assert len(EXECUTION_VARIANTS) == 8
    assert len({row["variant"] for row in EXECUTION_VARIANTS}) == 8


def test_fill_full_waves_preserves_original_prefix() -> None:
    tasks = [{"kind": "cache-benchmark", "model": str(index)} for index in range(15)]
    filled, count = fill_full_waves(tasks)
    assert count == 15
    assert len(filled) == 16
    assert filled[:count] == tasks
    assert filled[-1]["full_node_filler_replica"] == 1


def test_recipe_grid_tunes_both_optimizer_families() -> None:
    recipes = screen_recipes()
    assert len([recipe for recipe in recipes if recipe.family == "adamw"]) == 3
    assert len([recipe for recipe in recipes if recipe.family == "muon"]) == 3


def test_recipe_promotion_is_independent_by_model_and_family() -> None:
    from exp20_outer_cache.model import DENSE_R8_PACKED, RECURRENT3_R8_PACKED

    rows = []
    for model in (DENSE_R8_PACKED, RECURRENT3_R8_PACKED):
        for index, recipe in enumerate(screen_recipes()):
            rows.append(
                {
                    "status": "complete",
                    "model": model,
                    "recipe": recipe.__dict__,
                    "threshold_hits": {
                        "nll_le_0.01": {
                            "step": index + 1,
                            "elapsed_seconds": index + 1,
                        }
                    },
                    "final": {"nll": 0.0},
                    "performance": {"elapsed_seconds": index + 1},
                }
            )
    promoted = promote_recipes(rows)
    assert promoted[DENSE_R8_PACKED]["adamw"].body_lr == 0.003
    assert promoted[DENSE_R8_PACKED]["muon"].body_lr == 0.06


def test_boundary_winner_expands_without_repeating_a_recipe() -> None:
    from exp20_outer_cache.model import DENSE_R8_PACKED, RECURRENT3_R8_PACKED

    rows = []
    for model in (DENSE_R8_PACKED, RECURRENT3_R8_PACKED):
        for index, recipe in enumerate(screen_recipes()):
            rows.append(
                {
                    "status": "complete",
                    "model": model,
                    "recipe": recipe.__dict__,
                    "threshold_hits": {
                        "nll_le_0.01": {
                            "step": 10 - index,
                            "elapsed_seconds": 10 - index,
                        }
                    },
                    "final": {"nll": 0.0},
                    "performance": {"elapsed_seconds": 10 - index},
                }
            )
    extensions = tuning_extensions(rows)
    assert extensions
    existing = {
        (row["model"], row["recipe"]["body_lr"], row["recipe"]["auxiliary_lr"])
        for row in rows
    }
    assert all(
        (model, recipe.body_lr, recipe.auxiliary_lr) not in existing
        for model, recipe in extensions
    )
