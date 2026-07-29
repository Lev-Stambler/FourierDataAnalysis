import json

import pytest
import torch

from qwen_fullwidth_distill.audit import (
    _expected_trainable_parameters,
    _validate_result,
    _wandb_config_matches,
    audit_depth_study,
    audit_study,
    audit_wandb,
)
from qwen_fullwidth_distill.config import MODEL_ID, MODEL_REVISION
from qwen_fullwidth_distill.study import (
    depth_final_trials,
    depth_reference_trials,
    depth_screen_trials,
    final_trials,
    optimization_trials,
    optimized_depth_final_trials,
    screen_trials,
    select_common_topology,
    select_depth_winners,
    select_optimized_winners,
    select_unrestricted_monarch,
    tuning_trials,
)


def _result(trial, kl):
    value = {
        "label": trial.label,
        "architecture": trial.architecture.to_dict(),
        "lr": trial.lr,
        "seed": trial.seed,
        "steps": trial.steps,
        "trainable_parameters": 1,
        "frozen_parameters": 1,
        "initial_validation": {"kl": kl + 1},
        "validation": {"kl": kl},
        "test": {"kl": kl + 0.1} if trial.is_final else {},
        "embedding_sha256": "b" * 64,
    }
    if trial.effective_batch != 1_024:
        value["effective_batch"] = trial.effective_batch
    return value


def _write_result(root, trial, value):
    output = root / trial.stage / trial.label
    output.mkdir(parents=True)
    (output / "result.json").write_text(json.dumps(value))
    return output


def test_full_study_auditor_validates_grid_plan_summary_and_checkpoints(tmp_path):
    screen_grid = screen_trials()
    screen = []
    for trial in screen_grid:
        kl = 10.0
        label = trial.architecture.label
        if label == "dense-residual_ffn-d1-x4":
            kl = 2.0
        if label == "monarch-r1-residual_ffn-d1-x4":
            kl = 2.1
        if label == "monarch-r1-residual_ffn-d4-x4":
            kl = 1.5
        value = _result(trial, kl)
        _write_result(tmp_path, trial, value)
        screen.append(value)

    common = select_common_topology(screen)
    deep = select_unrestricted_monarch(screen)
    tune_grid = tuning_trials(common, deep)
    tune = []
    for trial in tune_grid:
        value = _result(trial, 1.0 + trial.lr)
        _write_result(tmp_path, trial, value)
        tune.append(value)

    final_grid = final_trials(screen, tune)
    final = []
    for trial in final_grid:
        value = _result(trial, 0.5 + 0.01 * trial.seed)
        output = _write_result(tmp_path, trial, value)
        torch.save(
            {
                "schema": "qwen-fullwidth-student-v1",
                "state_dict": {"weight": torch.zeros(1)},
                "trial": trial.to_dict(),
                "model_id": MODEL_ID,
                "model_revision": MODEL_REVISION,
                "embedding_sha256": value["embedding_sha256"],
                "metrics": value,
            },
            output / "student.pt",
        )
        final.append(value)

    plan = {
        "selection_basis": "validation_only",
        "common_topology": common.to_dict(),
        "unrestricted_monarch": deep.to_dict(),
        "final_trials": [trial.to_dict() for trial in final_grid],
    }
    (tmp_path / "study-plan.json").write_text(json.dumps(plan))
    (tmp_path / "study-summary.json").write_text(
        json.dumps({"screen": screen, "tune": tune, "final": final})
    )

    result = audit_study(tmp_path)
    assert result["status"] == "complete"
    assert result["screen_trials"] == 23
    assert result["tune_trials"] == 9
    assert result["final_trials"] == 12
    assert all(row["seeds"] == 3 for row in result["test"].values())


def test_wandb_config_match_requires_exact_trial_and_objective():
    trial = screen_trials()[0]
    result = _result(trial, 1.0)
    config = trial.to_dict() | {
        "objective": "full_vocab_forward_kl",
        "effective_batch": 1_024,
    }
    assert _wandb_config_matches(config, result, "screen")
    assert not _wandb_config_matches(
        config | {"effective_batch": 512}, result, "screen"
    )
    assert not _wandb_config_matches({}, result, "screen")
    with pytest.raises(ValueError, match="retry policy"):
        audit_wandb(".", attempts=0)


def test_tensor_probe_audit_accepts_expected_divergence_only():
    from qwen_fullwidth_distill.config import ArchitectureConfig, TrialConfig

    architecture = ArchitectureConfig(
        "btt", "residual_ffn", 20, 4, btt_cores=4, btt_rank=1
    )
    trial = TrialConfig(
        architecture,
        lr=1e-2,
        steps=256,
        stage="tensor_lr_probe",
        lr_parameterization="mup",
        allow_divergence=True,
    )
    value = _result(trial, 2.0)
    value.update({
        "status": "diverged",
        "steps_completed": 17,
        "lr_parameterization": "mup",
        "divergence_reason": "activation RMS grew",
    })
    _validate_result(value, trial)
    forbidden = TrialConfig(
        architecture,
        lr=1e-2,
        steps=256,
        stage="tensor_param",
        lr_parameterization="mup",
    )
    forbidden_value = dict(value)
    forbidden_value.update({
        "label": forbidden.label,
        "steps": forbidden.steps,
    })
    with pytest.raises(RuntimeError, match="unexpected divergence"):
        _validate_result(forbidden_value, forbidden)


def test_tensor_expected_parameter_count_uses_full_stack():
    from qwen_fullwidth_distill.config import ArchitectureConfig, TrialConfig

    trial = TrialConfig(
        ArchitectureConfig(
            "btt", "residual_gated", 35, btt_cores=4, btt_rank=1
        ),
        stage="tensor_param",
    )
    assert _expected_trainable_parameters(trial) == 84_869_540


def test_depth_auditor_checks_selection_grid_and_shared_parameters(tmp_path):
    references = []
    for trial in depth_reference_trials():
        value = _result(trial, 2.0)
        value["trainable_parameters"] = (
            84_279_296 if trial.architecture.depth == 4 else 168_558_592
        )
        _write_result(tmp_path, trial, value)
        references.append(value)

    screens = []
    for trial in depth_screen_trials():
        config = trial.architecture
        kl = 3.0
        if config.repetitions == 1 and config.depth == 16 and trial.lr == 3e-4:
            kl = 1.1
        if (
            config.repetitions == 8
            and config.residual_scale == "inverse_repetitions"
            and trial.lr == 1e-3
        ):
            kl = 1.0
        value = _result(trial, kl)
        if config.repetitions > 1:
            value["trainable_parameters"] = 84_279_296
        _write_result(tmp_path, trial, value)
        screens.append(value)

    selection_rows = references + screens
    untied, untied_lr, looped, looped_lr = select_depth_winners(selection_rows)
    optimization = []
    optimization_grid = optimization_trials(selection_rows)
    for trial in optimization_grid:
        kl = 0.9
        if trial.architecture.repetitions > 1 and trial.effective_batch == 256:
            kl = 0.7 + trial.lr
        if trial.architecture.repetitions == 1 and trial.effective_batch == 512:
            kl = 0.8 + trial.lr
        value = _result(trial, kl)
        value["effective_batch"] = trial.effective_batch
        _write_result(tmp_path, trial, value)
        optimization.append(value)
    (
        optimized_untied,
        optimized_untied_lr,
        optimized_untied_batch,
        optimized_looped,
        optimized_looped_lr,
        optimized_looped_batch,
    ) = select_optimized_winners(selection_rows, optimization)
    finals = []
    final_grid = optimized_depth_final_trials(selection_rows, optimization)
    for trial in final_grid:
        value = _result(trial, 0.8 + 0.01 * trial.seed)
        count = (
            84_279_296
            if trial.architecture.repetitions > 1
            else 1
        )
        value["trainable_parameters"] = count
        output = _write_result(tmp_path, trial, value)
        state = (
            torch.sparse_coo_tensor(size=(count,), check_invariants=False)
            if count > 1
            else torch.zeros(1)
        )
        torch.save(
            {
                "schema": "qwen-fullwidth-student-v1",
                "state_dict": {"weight": state},
                "trial": trial.to_dict(),
                "model_id": MODEL_ID,
                "model_revision": MODEL_REVISION,
                "embedding_sha256": value["embedding_sha256"],
                "metrics": value,
            },
            output / "student.pt",
        )
        finals.append(value)

    plan = {
        "selection_basis": "endpoint_validation_only",
        "reference_trials": [
            trial.to_dict() for trial in depth_reference_trials()
        ],
        "screen_trials": [
            trial.to_dict() for trial in depth_screen_trials()
        ],
        "structural_untied_winner": {
            "architecture": untied.to_dict(),
            "lr": untied_lr,
        },
        "structural_looped_winner": {
            "architecture": looped.to_dict(),
            "lr": looped_lr,
        },
        "optimization_trials": [
            trial.to_dict() for trial in optimization_grid
        ],
        "optimized_untied_winner": {
            "architecture": optimized_untied.to_dict(),
            "lr": optimized_untied_lr,
            "effective_batch": optimized_untied_batch,
        },
        "optimized_looped_winner": {
            "architecture": optimized_looped.to_dict(),
            "lr": optimized_looped_lr,
            "effective_batch": optimized_looped_batch,
        },
        "final_trials": [trial.to_dict() for trial in final_grid],
    }
    (tmp_path / "depth-study-plan.json").write_text(json.dumps(plan))
    (tmp_path / "depth-study-summary.json").write_text(
        json.dumps(
            {
                "reference": references,
                "depth_screen": screens,
                "depth_opt": optimization,
                "depth_final": finals,
            }
        )
    )

    result = audit_depth_study(tmp_path)
    assert result["status"] == "complete"
    assert result["reference_trials"] == 3
    assert result["depth_screen_trials"] == 12
    assert result["optimization_trials"] == 8
    assert result["depth_final_trials"] == 6
    assert result["looped_winner"]["effective_batch"] == 256
