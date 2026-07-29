import pytest
import torch

from qwen_fullwidth_distill.model import FullWidthStack
from qwen_fullwidth_distill.study import (
    KRONECKER_EDU_ARCHITECTURE_SCREEN_EXAMPLES,
    KRONECKER_EDU_OBJECTIVE_EXAMPLES,
    KRONECKER_EDU_POLICY_EXAMPLES,
    KRONECKER_EDU_SCALE_BOUNDARIES,
    KRONECKER_EDU_WARMUP_EXAMPLES,
    kronecker_edu_architecture_trials,
    kronecker_edu_architectures,
    kronecker_edu_lr_trials,
    kronecker_edu_objective_trials,
    kronecker_edu_optimizer_trials,
    kronecker_edu_scale_trials,
    select_kronecker_edu_lr_winner,
    select_kronecker_edu_stage_winner,
    select_kronecker_edu_stage_top_k,
)


def completed_result(trial, kl, **extra):
    return {
        "label": trial.label,
        "status": "complete",
        "steps_completed": trial.steps,
        "validation": {"kl": kl},
        **extra,
    }


def test_edu_lr_winner_uses_only_completed_final_validation_kl():
    trials = kronecker_edu_lr_trials()
    rows = [
        completed_result(
            trials[0],
            1.4,
            test={"kl": 0.01},
            initial_validation={"kl": 0.01},
            train_kl=0.01,
        ),
        completed_result(
            trials[1],
            1.2,
            test={"kl": 99.0},
            initial_validation={"kl": 99.0},
            train_kl=99.0,
        ),
        {
            **completed_result(trials[2], 0.1),
            "status": "diverged",
        },
        {
            **completed_result(trials[3], 0.2),
            "steps_completed": trials[3].steps - 1,
        },
        {
            "label": "unrelated-fineweb-edu-looking-result",
            "status": "complete",
            "steps_completed": trials[4].steps,
            "validation": {"kl": 0.0},
        },
    ]
    assert select_kronecker_edu_lr_winner(rows) == trials[1]
    with pytest.raises(RuntimeError, match="completed full-validation"):
        select_kronecker_edu_lr_winner(rows[2:])


def test_edu_stage_winner_rejects_partial_and_unrelated_results():
    source = kronecker_edu_lr_trials()[0]
    trials = kronecker_edu_optimizer_trials(source)
    rows = [
        completed_result(trials[0], 1.3, test={"kl": 0.01}),
        completed_result(trials[1], 1.2, test={"kl": 99.0}),
        {
            **completed_result(trials[2], 0.1),
            "steps_completed": trials[2].steps - 1,
        },
        {
            **completed_result(trials[3], 0.2),
            "status": "diverged",
        },
        completed_result(trials[4], float("nan")),
        {
            "label": "unrelated",
            "status": "complete",
            "steps_completed": trials[5].steps,
            "validation": {"kl": 0.0},
        },
    ]
    assert select_kronecker_edu_stage_winner(trials, rows) == trials[1]
    with pytest.raises(RuntimeError, match="completed full-validation"):
        select_kronecker_edu_stage_winner(trials, rows[2:])


def test_edu_optimizer_policy_grid_is_exact_and_warm_started():
    source = kronecker_edu_lr_trials()[0]
    trials = kronecker_edu_optimizer_trials(source)
    assert len(trials) == 8
    assert len({trial.label for trial in trials}) == 8
    assert {
        (trial.steps - source.steps) * trial.effective_batch
        for trial in trials
    } == {KRONECKER_EDU_POLICY_EXAMPLES}
    assert {trial.effective_batch for trial in trials} == {32}
    assert {trial.warmup_examples for trial in trials} == {
        KRONECKER_EDU_WARMUP_EXAMPLES
    }
    assert {trial.warmup_steps for trial in trials} == {0}
    assert {trial.min_lr_ratio for trial in trials} == {0.1}
    assert {trial.lr_schedule for trial in trials} == {"warmup_hold"}
    assert all(trial.use_teacher_cache for trial in trials)
    assert all(trial.warm_start_stage == source.stage for trial in trials)
    assert all(trial.warm_start_label == source.label for trial in trials)
    assert all(trial.warm_start_step == source.steps for trial in trials)
    assert all(trial.warm_start_lr_override for trial in trials)

    assert trials[0].optimizer_role_lr_multipliers == ()
    assert trials[0].optimizer_role_weight_decays == ()
    assert dict(trials[1].optimizer_role_weight_decays) == {
        "kronecker_factor": 0.0,
        "kronecker_gain": 0.0,
        "kronecker_mixing": 0.0,
        "standard": 0.0,
        "bias": 0.0,
    }
    assert [
        dict(trial.optimizer_role_lr_multipliers)
        for trial in trials[2:5]
    ] == [
        {"kronecker_gain": 10.0},
        {"kronecker_gain": 33.333},
        {"kronecker_gain": 100.0},
    ]
    assert dict(trials[5].optimizer_role_lr_multipliers) == {
        "kronecker_gain": 33.333,
        "kronecker_mixing": 3.333,
    }
    expected_full_policy = {
        "kronecker_gain": 33.333,
        "kronecker_mixing": 3.333,
        "standard": 33.333,
        "bias": 33.333,
    }
    assert dict(trials[6].optimizer_role_lr_multipliers) == expected_full_policy
    assert dict(trials[7].optimizer_role_lr_multipliers) == expected_full_policy
    assert {trial.gradient_clip_norm for trial in trials[:7]} == {
        source.gradient_clip_norm
    }
    assert trials[7].gradient_clip_norm == 5.0


def test_edu_objective_grid_is_exact_and_inherits_optimizer_policy():
    lr_source = kronecker_edu_lr_trials()[0]
    source = kronecker_edu_optimizer_trials(lr_source)[6]
    trials = kronecker_edu_objective_trials(source)
    assert len(trials) == 8
    assert len({trial.label for trial in trials}) == 8
    assert {
        (trial.steps - source.steps) * trial.effective_batch
        for trial in trials
    } == {KRONECKER_EDU_OBJECTIVE_EXAMPLES}
    assert [
        (trial.temperature, trial.temperature2_weight, trial.hidden_mse_weight)
        for trial in trials
    ] == [
        (1.0, 0.0, 0.0),
        (1.0, 0.0, 0.03),
        (1.0, 0.0, 0.1),
        (1.0, 0.0, 0.3),
        (1.0, 0.0, 1.0),
        (1.0, 0.0, 3.0),
        (2.0, 0.25, 0.0),
        (2.0, 0.25, 0.3),
    ]
    assert all(
        trial.optimizer_role_lr_multipliers
        == source.optimizer_role_lr_multipliers
        for trial in trials
    )
    assert all(trial.use_teacher_cache for trial in trials)
    assert all(trial.warm_start_label == source.label for trial in trials)


def test_edu_architecture_grid_and_parameter_counts_are_exact():
    architectures = kronecker_edu_architectures()
    assert [
        (
            architecture.operator,
            architecture.form,
            architecture.depth,
            architecture.kronecker_factors,
            architecture.kronecker_rank,
            architecture.repetitions,
            architecture.gated_repetitions,
        )
        for architecture in architectures
    ] == [
        ("kronecker", "residual_ffn", 4, 3, 2407, 1, False),
        ("kronecker", "residual_ffn", 4, 3, 2407, 2, True),
        ("kronecker", "residual_gated", 4, 3, 3033, 1, False),
        ("kronecker", "residual_ffn", 8, 4, 6750, 1, False),
        ("kronecker", "residual_ffn", 16, 4, 3343, 1, False),
        ("kronecker", "residual_ffn", 32, 4, 1640, 1, False),
        ("kronecker", "residual_gated", 16, 4, 2688, 1, False),
        ("hybrid", "residual_ffn", 10, 4, 3343, 1, False),
    ]
    expected_parameters = [
        84_272_352,
        84_272_360,
        84_264_112,
        84_270_432,
        84_265_312,
        84_279_808,
        84_269_056,
        84_272_304,
    ]
    actual_parameters = []
    for architecture in architectures:
        with torch.device("meta"):
            stack = FullWidthStack(architecture)
        actual_parameters.append(
            sum(parameter.numel() for parameter in stack.parameters())
        )
    assert actual_parameters == expected_parameters
    assert max(actual_parameters) - min(actual_parameters) < 16_000


def test_edu_architecture_trials_are_fresh_matched_screens():
    lr_source = kronecker_edu_lr_trials()[0]
    source = kronecker_edu_objective_trials(lr_source)[3]
    trials = kronecker_edu_architecture_trials(source)
    assert len(trials) == 8
    assert len({trial.label for trial in trials}) == 8
    assert {
        trial.steps * trial.effective_batch for trial in trials
    } == {KRONECKER_EDU_ARCHITECTURE_SCREEN_EXAMPLES}
    assert all(not trial.warm_start_label for trial in trials)
    assert all(not trial.warm_start_stage for trial in trials)
    assert all(trial.hidden_mse_weight == 0.3 for trial in trials)
    assert all(trial.use_teacher_cache for trial in trials)
    assert {trial.warmup_examples for trial in trials} == {
        KRONECKER_EDU_WARMUP_EXAMPLES
    }


def test_edu_scale_boundaries_successively_halve_by_full_validation():
    lr_source = kronecker_edu_lr_trials()[0]
    architecture_trials = kronecker_edu_architecture_trials(lr_source)
    architecture_results = [
        completed_result(trial, 2.0 - index * 0.1)
        for index, trial in enumerate(architecture_trials)
    ]
    top = select_kronecker_edu_stage_top_k(
        architecture_trials,
        architecture_results,
        4,
    )
    assert top == list(reversed(architecture_trials[-4:]))

    previous_trials = architecture_trials
    previous_results = architecture_results
    expected_keeps = []
    for target_examples, keep in KRONECKER_EDU_SCALE_BOUNDARIES:
        trials = kronecker_edu_scale_trials(
            previous_trials,
            previous_results,
            target_examples=target_examples,
            keep=keep,
        )
        expected_keeps.append(len(trials))
        assert len(trials) == keep
        assert {
            trial.steps * trial.effective_batch for trial in trials
        } == {target_examples}
        assert all(trial.is_final for trial in trials)
        assert all(trial.use_teacher_cache for trial in trials)
        assert all(trial.warm_start_label for trial in trials)
        assert {trial.warmup_examples for trial in trials} == {
            KRONECKER_EDU_WARMUP_EXAMPLES
        }
        previous_trials = trials
        previous_results = [
            completed_result(trial, 1.5 - index * 0.1)
            for index, trial in enumerate(trials)
        ]
    assert expected_keeps == [4, 2, 1]

    with pytest.raises(ValueError, match="unsupported"):
        kronecker_edu_scale_trials(
            architecture_trials,
            architecture_results,
            target_examples=123_456,
            keep=4,
        )
