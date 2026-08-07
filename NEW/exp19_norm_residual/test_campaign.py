from __future__ import annotations

import math
from dataclasses import asdict

import pytest
import torch

from exp19_norm_residual.campaign import (
    MAXIMUM_BRANCH_STATE_RATIO,
    MODEL_NAMES,
    Recipe,
    TRAINING_MODEL_NAMES,
    _tensor_comparison,
    all_complete,
    balanced_repetition,
    conditioning_gate,
    exposure_accounting,
    fill_full_node,
    initial_branch_gate,
    load_stage_snapshot,
    promote_screen,
    screen_recipes,
    write_stage_snapshot,
)


def test_physical_repetition_is_balanced_and_accounted_separately() -> None:
    assert balanced_repetition(3, 8).tolist() == [0, 1, 2, 0, 1, 2, 0, 1]
    row = exposure_accounting(unique_count=3, physical_batch=400, steps=64)
    assert row["global_tokens_per_step"] == 102_400
    assert row["unique_tokens_per_step"] == 768
    assert row["physical_tokens_processed"] == 6_553_600
    assert row["unique_token_exposures"] == 49_152
    assert row["gradient_accumulation"] == 1


def test_screen_contains_independent_adamw_and_muon_lr_sweeps() -> None:
    recipes = screen_recipes()
    assert len(recipes) == 6
    assert [recipe.family for recipe in recipes].count("adamw") == 3
    assert [recipe.family for recipe in recipes].count("muon") == 3
    assert all(recipe.weight_decay == 0 for recipe in recipes)


def test_short_preflight_stages_still_fill_all_eight_gpus() -> None:
    tasks = [{"kind": "benchmark", "model": name} for name in TRAINING_MODEL_NAMES]
    filled = fill_full_node(tasks)
    assert len(tasks) == 6
    assert len(filled) == 8
    assert filled[:6] == tasks
    assert all("full_node_filler_replica" in task for task in filled[6:])


def test_completed_stage_snapshot_round_trips(tmp_path) -> None:
    path = tmp_path / "screen.json"
    rows = [{"status": "complete", "model": "a"}, {"status": "complete", "model": "b"}]
    write_stage_snapshot(path, stage="screen", rows=rows)
    assert load_stage_snapshot(path, stage="screen", expected_rows=2) == rows


def test_stage_snapshot_rejects_partial_row_count(tmp_path) -> None:
    path = tmp_path / "screen.json"
    write_stage_snapshot(
        path, stage="screen", rows=[{"status": "complete", "model": "a"}]
    )
    with pytest.raises(RuntimeError, match="invalid Exp19 stage snapshot"):
        load_stage_snapshot(path, stage="screen", expected_rows=2)


def test_promotion_never_compares_optimizer_families_as_one_recipe() -> None:
    low_adamw, high_adamw, low_muon, high_muon = (
        screen_recipes()[0],
        screen_recipes()[2],
        screen_recipes()[3],
        screen_recipes()[5],
    )
    rows = []
    for name in MODEL_NAMES:
        for recipe, nll in (
            (low_adamw, 0.2),
            (high_adamw, 0.1),
            (low_muon, 0.3),
            (high_muon, 0.05),
        ):
            rows.append(
                {
                    "status": "complete",
                    "model": name,
                    "recipe": asdict(recipe),
                    "success": False,
                    "final": {"nll": nll},
                    "performance": {"elapsed_seconds": 1.0},
                    "threshold_hits": {"nll_le_0.01": None},
                }
            )
    promoted = promote_screen(rows)
    assert all(value["adamw"].body_lr == high_adamw.body_lr for value in promoted.values())
    assert all(value["muon"].body_lr == high_muon.body_lr for value in promoted.values())


def test_conditioning_gate_rejects_historical_collapse_and_oversized_branch() -> None:
    architecture = {
        "conditioning": {
            "scaled_update_to_state_ratios": [0.1, MAXIMUM_BRANCH_STATE_RATIO + 0.01],
            "first_to_last_hidden_variance_ratio": 1_000.0,
        }
    }
    optimization = {
        "layer_gradient_imbalance": 2.0,
        "minimum_effective_normalized_update_fraction": 0.9,
    }
    report = conditioning_gate(architecture, optimization)
    assert not report["pass"]
    assert "branch_state_ratio" in report["failures"]
    assert "hidden_variance_imbalance" in report["failures"]


@pytest.mark.parametrize(
    ("ratios", "expected"),
    [
        ([0.05] * 64, True),
        ([0.01] * 64, True),
        ([0.005] * 64, False),
        ([0.10] * 64, False),
        ([0.51], False),
        ([math.nan, 0.05], False),
    ],
)
def test_initial_branch_gate_uses_aggregate_depth_aware_energy(
    ratios: list[float], expected: bool
) -> None:
    report = initial_branch_gate(
        {"conditioning": {"scaled_update_to_state_ratios": ratios}}
    )
    assert report["pass"] is expected


def test_initial_branch_gate_keeps_per_branch_band_as_diagnostic() -> None:
    report = initial_branch_gate(
        {"conditioning": {"scaled_update_to_state_ratios": [0.01] * 64}}
    )
    assert report["pass"]
    assert report["fraction_in_target_interval"] == 0.0
    assert report["fraction_is_diagnostic_only"] is True


def test_global_parity_is_stable_when_one_tensor_is_nearly_zero() -> None:
    first = {
        "body": torch.ones(1024),
        "tiny": torch.tensor([1e-12]),
    }
    second = {
        "body": torch.ones(1024),
        "tiny": torch.tensor([2e-12]),
    }
    report = _tensor_comparison(first, second)
    assert report["maximum_tensor_relative_error_raw"] == pytest.approx(1.0)
    assert report["maximum_tensor_relative_error_parameter"] == "tiny"
    assert report["global_relative_error"] < 1e-12
    assert report["global_cosine"] == pytest.approx(1.0)


def test_failed_gate_exception_does_not_serialize_large_telemetry() -> None:
    row = {
        "status": "failed",
        "model": "candidate",
        "compiled": {"pass": False},
        "telemetry": {"large": "x" * 100_000},
    }
    with pytest.raises(RuntimeError) as captured:
        all_complete([row], "parity")
    message = str(captured.value)
    assert "compiled" in message
    assert len(message) < 1_000
