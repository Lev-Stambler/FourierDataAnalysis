from __future__ import annotations

import json
from dataclasses import asdict

import numpy as np
import pytest
import torch

from exp13_wikitext_confirmation.campaign import (
    _selection,
    create_optimizer,
    split_muon_parameters,
)
from exp13_wikitext_confirmation.gates import (
    exp12_candidate_parity,
    manual_attention_errors,
    sampler_error,
)
from exp13_wikitext_confirmation.holdout import (
    CONFIRMATION_WINDOWS,
    FINAL_WINDOWS,
    load_holdout,
    prepare_holdout,
    sha256,
    validate_holdout,
)
from exp13_wikitext_confirmation.model import (
    CANDIDATE,
    MODEL_NAMES,
    STANDARD_CONTROLS,
    build_model,
    model_inventory,
)
from exp13_wikitext_confirmation.study import (
    CONFIRMATION_SEEDS,
    Recipe,
    boundary_extension,
    coarse_recipes,
    finalist_recipes,
    paired_decision,
    recipe_slug,
    robust_recipes,
    schedule_multiplier,
)


def test_candidate_is_exact_exp12_copy_and_attention_reference_matches() -> None:
    parity = exp12_candidate_parity()
    assert parity == {
        "state_keys_equal": True,
        "parameter_max_error": 0.0,
        "logit_max_error": 0.0,
    }
    forward, backward = manual_attention_errors()
    assert forward <= 1e-10
    assert backward <= 1e-9


def test_controls_span_aspect_ratios_and_are_parameter_matched() -> None:
    candidate = model_inventory(build_model(CANDIDATE))
    assert candidate["total_parameters"] == 6_410_560
    shapes = set()
    for name in STANDARD_CONTROLS:
        inventory = model_inventory(build_model(name))
        shapes.add((inventory["spec"]["depth"], inventory["spec"]["width"]))
        assert inventory["total_parameters"] / candidate["total_parameters"] <= 1.005
    assert shapes == {(32, 128), (10, 160), (6, 192), (3, 256)}


def test_muon_routing_is_semantic_disjoint_and_complete() -> None:
    for name in MODEL_NAMES:
        model = build_model(name)
        muon, auxiliary, routing = split_muon_parameters(model)
        assert len({id(value) for value in [*muon, *auxiliary]}) == len(
            [*muon, *auxiliary]
        )
        assert routing["muon_parameters"] + routing["auxiliary_parameters"] == sum(
            value.numel() for value in model.parameters()
        )
        assert "vocabulary" in routing["auxiliary_parameter_names"]
        optimizer, created = create_optimizer(model, Recipe("muon", 0.03, 0.001))
        assert optimizer.muon.param_groups[0]["lr"] == pytest.approx(0.03)
        assert created == {"family": "muon", **routing}


def test_holdout_is_one_time_disjoint_and_checksum_sealed(tmp_path) -> None:
    source, destination = tmp_path / "source", tmp_path / "holdout"
    source.mkdir()
    windows = np.arange(1163 * 257, dtype=np.uint32).reshape(1163, 257) % 16384
    np.save(source / "test.npy", windows.astype(np.uint16))
    source_manifest = {
        "schema": "exp10-tokenized-corpus-v1",
        "dataset_revision": "frozen-revision",
        "files": {
            "test.npy": {
                "sha256": sha256(source / "test.npy"),
                "windows": 1163,
            }
        },
    }
    (source / "manifest.json").write_text(json.dumps(source_manifest))
    manifest = prepare_holdout(source, destination)
    assert manifest["partition"]["overlap_windows"] == 0
    assert validate_holdout(destination) == manifest
    confirmation = load_holdout(destination, "confirmation")
    final = load_holdout(destination, "final")
    assert confirmation.shape == (CONFIRMATION_WINDOWS, 257)
    assert final.shape == (FINAL_WINDOWS, 257)
    assert np.array_equal(confirmation[-1], windows[580])
    assert np.array_equal(final[0], windows[581])
    with (destination / "final.npy").open("r+b") as handle:
        handle.seek(-1, 2)
        handle.write(b"x")
    with pytest.raises(RuntimeError, match="checksum mismatch"):
        validate_holdout(destination)


def _cell(model: str, recipe: Recipe, seed: int, nll: float) -> dict:
    return {
        "status": "complete",
        "model": model,
        "recipe_slug": recipe_slug(recipe),
        "recipe": asdict(recipe),
        "seed": seed,
        "validation": {"nll": nll},
    }


def test_tuning_policy_and_exact_recipe_selection() -> None:
    coarse = coarse_recipes()
    assert len(coarse) == 8
    extended = boundary_extension(
        Recipe("muon", 0.3, 0.001),
        [recipe for recipe in coarse if recipe.family == "muon"],
    )
    assert extended is not None and extended.body_lr == pytest.approx(0.9)
    coarse_rows = []
    for model_index, model in enumerate(STANDARD_CONTROLS):
        for recipe_index, recipe in enumerate(coarse):
            coarse_rows.append(
                _cell(model, recipe, 0, 4.0 + model_index + recipe_index / 100)
            )
    robust = robust_recipes(coarse_rows)
    assert len(robust) >= 4 * 4
    robust_rows = [
        _cell(model, recipe, seed, 4.0 + index / 100 + seed / 1000)
        for index, (model, recipe) in enumerate(robust)
        for seed in (0, 1)
    ]
    finalists = finalist_recipes(robust_rows)
    assert len(finalists) == 2

    same_model = STANDARD_CONTROLS[0]
    first, second = Recipe("adamw", 0.001, 0.001), Recipe("muon", 0.03, 0.001)
    rows = [
        _cell(same_model, recipe, seed, 4.0 if recipe == first else 3.0)
        for recipe in (first, second)
        for seed in CONFIRMATION_SEEDS
    ]
    selected = _selection(rows, same_model, first)
    assert selected["recipe_slug"] == recipe_slug(first)
    assert selected["mean_validation_nll"] == 4.0


def test_confirmation_rule_is_strict_and_sampler_is_batch_invariant() -> None:
    candidate = dict(zip(CONFIRMATION_SEEDS, (3.7, 3.8, 3.9, 4.0), strict=True))
    control = dict(zip(CONFIRMATION_SEEDS, (3.81, 3.91, 4.01, 4.11), strict=True))
    assert paired_decision(candidate, control, minimum_mean_win=0.10)["status"] == "pass"
    tied = {**candidate, 6: 4.1}
    assert paired_decision(tied, control, minimum_mean_win=0.10)["status"] == "fail"
    assert sampler_error() == 0
    cosine = Recipe("adamw", 0.001, 0.001, schedule="warmup-cosine")
    assert schedule_multiplier(cosine, 1_000_000) == pytest.approx(0.5)
