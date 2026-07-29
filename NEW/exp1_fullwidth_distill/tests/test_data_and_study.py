import numpy as np

from qwen_fullwidth_distill.config import (
    ArchitectureConfig,
    same_width_screen,
)
from qwen_fullwidth_distill.data import (
    deterministic_span,
    deterministic_spans,
    document_digest,
    split_for_digest,
)
from qwen_fullwidth_distill.study import (
    btt_parameter_architectures,
    depth_final_trials,
    depth_reference_trials,
    depth_screen_trials,
    evaluate_tensor_gate,
    final_trials,
    kronecker_depth_trials,
    kronecker_edu_long_trial,
    kronecker_edu_lr_trials,
    kronecker_lr_probe_trials,
    kronecker_parameter_architecture,
    kronecker_rank_boundary_trials,
    kronecker_rank_batch_boundary_trials,
    kronecker_rank_batch_lr_boundary_trials,
    kronecker_rank_batch_lr_continuation_trials,
    kronecker_rank_chunk_benchmark_trials,
    kronecker_rank_compile_benchmark_trial,
    kronecker_rank_continuation_trial,
    kronecker_rank_depth_optimized_trials,
    kronecker_rank_depth_winner_continuation_trials,
    kronecker_rank_control_trials,
    kronecker_rank_monarch_batch_trials,
    kronecker_rank_monarch_batch_continuation_trials,
    kronecker_rank_monarch_batch_frontier_trials,
    kronecker_rank_monarch_batch_long_trials,
    kronecker_rank_monarch_batch_million_trials,
    kronecker_rank_monarch_batch_three_million_trials,
    kronecker_rank_monarch_batch_two_million_trials,
    kronecker_rank_monarch_batch_transition_trials,
    kronecker_rank_monarch_batch_transition_lr_trials,
    kronecker_rank_monarch_capacity_checkpoint_trial,
    kronecker_rank_monarch_capacity_continuation_trial,
    kronecker_rank_monarch_capacity_trials,
    kronecker_rank_monarch_continuation_trial,
    kronecker_rank_monarch_decay_continuation_trial,
    kronecker_rank_monarch_depth_parameter_matched_trials,
    kronecker_rank_monarch_decay_trials,
    kronecker_rank_monarch_long_trial,
    kronecker_rank_monarch_lr_boundary_trials,
    kronecker_rank_monarch_loop_trials,
    kronecker_rank_monarch_mature_continuation_trials,
    kronecker_rank_monarch_rank2_retry_trial,
    kronecker_rank_next_eight_trials,
    kronecker_rank_resume_after_spend_trials,
    kronecker_rank_monarch_scaled_depth_trial,
    kronecker_rank_probe_trials,
    kronecker_rank_architectures,
    kronecker_parameter_trials,
    optimization_trials,
    optimized_depth_final_trials,
    next_tensor_boundary_probe,
    screen_trials,
    select_common_topology,
    select_depth_winners,
    select_optimized_winners,
    select_kronecker_lr_settings,
    select_tensor_finalists,
    select_tensor_lr_settings,
    select_tensor_reference,
    select_unrestricted_monarch,
    tensor_family_key,
    tensor_final_trials,
    tensor_gate_btt_trials,
    tensor_gate_control_trial,
    tensor_lr_probe_trials,
    tensor_long_trial,
    tensor_parameter_trials,
    tensor_time_trials,
    tuning_trials,
)


def test_deterministic_span_and_split():
    digest = document_digest("a stable document")
    assert split_for_digest(digest) == split_for_digest(digest)
    ids = np.arange(100)
    first = deterministic_span(ids, digest)
    second = deterministic_span(ids, digest)
    np.testing.assert_array_equal(first, second)
    assert len(first) == 17


def test_deterministic_spans_are_repeatable_and_nonoverlapping():
    digest = document_digest("many stable windows")
    ids = np.arange(17 * 40 + 9)
    first = deterministic_spans(ids, digest)
    second = deterministic_spans(ids, digest)
    assert len(first) == 32
    for left, right in zip(first, second, strict=True):
        np.testing.assert_array_equal(left, right)
    starts = [int(window[0]) for window in first]
    assert all(b - a == 17 for a, b in zip(starts, starts[1:]))


def test_screen_grid_is_exact():
    grid = same_width_screen()
    dense = [config for config in grid if config.operator == "dense"]
    monarch = [config for config in grid if config.operator == "monarch"]
    assert len(dense) == 7
    assert len(monarch) == 16
    assert max(config.depth for config in dense) == 2
    assert max(config.depth for config in monarch) == 8
    assert [
        config.depth for config in dense
        if config.form == "residual_ffn" and config.expansion == 4
    ] == [1]


def result(config, kl, params=10):
    return {
        "architecture": config.to_dict(),
        "validation": {"kl": kl},
        "trainable_parameters": params,
    }


def test_selection_is_validation_only_and_deterministic():
    dense_a = ArchitectureConfig("dense", "sequential", 1)
    monarch_a = ArchitectureConfig("monarch", "sequential", 1)
    dense_b = ArchitectureConfig("dense", "residual_one", 1)
    monarch_b = ArchitectureConfig("monarch", "residual_one", 1)
    deep = ArchitectureConfig("monarch", "sequential", 8)
    rows = [
        result(dense_a, 3.0), result(monarch_a, 1.0),
        result(dense_b, 1.5), result(monarch_b, 1.5),
        result(deep, 0.9, params=20),
    ]
    assert select_common_topology(rows) == dense_b
    assert select_unrestricted_monarch(rows) == deep


def test_final_grid_is_four_configurations_three_seeds_and_4000_steps():
    screens = []
    for trial in screen_trials():
        kl = 10.0
        config = trial.architecture
        if config == ArchitectureConfig("dense", "residual_ffn", 1, 4):
            kl = 2.0
        if config == ArchitectureConfig("monarch", "residual_ffn", 1, 4):
            kl = 2.1
        if config == ArchitectureConfig("monarch", "residual_ffn", 4, 4):
            kl = 1.5
        screens.append(
            result(config, kl, params=100)
            | {"lr": trial.lr}
        )

    common = select_common_topology(screens)
    deep = select_unrestricted_monarch(screens)
    tunes = [
        result(trial.architecture, 1.0 + trial.lr, params=100)
        | {"lr": trial.lr}
        for trial in tuning_trials(common, deep)
    ]
    finals = final_trials(screens, tunes)

    assert len(finals) == 12
    assert len({trial.architecture for trial in finals}) == 4
    assert {trial.seed for trial in finals} == {0, 1, 2}
    assert {trial.steps for trial in finals} == {4_000}
    assert {trial.stage for trial in finals} == {"final"}
    assert {trial.audit_every for trial in finals} == {250}


def test_depth_screen_is_exact_and_uses_three_committed_references():
    references = depth_reference_trials()
    trials = depth_screen_trials()
    assert len(references) == 3
    assert len(trials) == 12
    assert len({trial.label for trial in trials}) == 12
    assert {trial.stage for trial in trials} == {"depth_screen"}
    assert {trial.steps for trial in trials} == {1_000}

    looped = [
        trial.architecture
        for trial in trials
        if trial.architecture.repetitions > 1
    ]
    assert {
        config.effective_depth
        for config in looped
        if config.residual_scale == "inverse_repetitions"
    } == {8, 16, 32}
    assert sum(config.residual_scale == "none" for config in looped) == 1


def test_depth_selection_and_final_grid_are_validation_only():
    trials = depth_reference_trials() + depth_screen_trials()
    rows = []
    for trial in trials:
        config = trial.architecture
        kl = 5.0
        if config.repetitions == 1 and config.depth == 16 and trial.lr == 3e-4:
            kl = 1.0
        if (
            config.repetitions == 8
            and config.residual_scale == "inverse_repetitions"
            and trial.lr == 1e-3
        ):
            kl = 0.9
        if config.repetitions == 4 and config.residual_scale == "none":
            kl = 0.1
        rows.append(
            result(config, kl, params=84_279_296) | {"lr": trial.lr}
        )

    untied, untied_lr, looped, looped_lr = select_depth_winners(rows)
    assert untied.effective_depth == 16
    assert untied_lr == 3e-4
    assert looped.repetitions == 8
    assert looped.effective_depth == 32
    assert looped.residual_scale == "inverse_repetitions"
    assert looped_lr == 1e-3

    finals = depth_final_trials(rows)
    assert len(finals) == 6
    assert len({trial.architecture for trial in finals}) == 2
    assert {trial.seed for trial in finals} == {0, 1, 2}
    assert {trial.steps for trial in finals} == {4_000}
    assert {trial.stage for trial in finals} == {"depth_final"}


def test_optimizer_grid_fixes_examples_and_selects_batch_and_lr():
    rows = []
    for trial in depth_reference_trials() + depth_screen_trials():
        config = trial.architecture
        kl = 4.0
        if config.repetitions == 1 and config.depth == 16 and trial.lr == 3e-4:
            kl = 1.2
        if (
            config.repetitions == 4
            and config.residual_scale == "inverse_repetitions"
            and trial.lr == 1e-3
        ):
            kl = 1.1
        rows.append(result(config, kl) | {"lr": trial.lr})

    grid = optimization_trials(rows)
    assert len(grid) == 8
    assert {trial.effective_batch for trial in grid} == {256, 512}
    assert {trial.steps * trial.effective_batch for trial in grid} == {
        1_048_576
    }

    optimized = []
    for trial in grid:
        kl = 2.0
        if trial.architecture.repetitions == 1 and trial.effective_batch == 256:
            kl = 0.9 + trial.lr
        if (
            trial.architecture.repetitions > 1
            and trial.effective_batch == 512
        ):
            kl = 0.8 + trial.lr
        optimized.append(
            result(trial.architecture, kl)
            | {
                "lr": trial.lr,
                "effective_batch": trial.effective_batch,
            }
        )

    selected = select_optimized_winners(rows, optimized)
    assert selected[2] == 256
    assert selected[5] == 512
    finals = optimized_depth_final_trials(rows, optimized)
    assert len(finals) == 6
    assert {
        trial.steps * trial.effective_batch for trial in finals
    } == {4_096_000}
    assert {trial.stage for trial in finals} == {"depth_final"}


def test_btt_probe_and_parameter_frontier_are_exact_fixed_example_grids():
    reference = {
        "label": "reference",
        "architecture": ArchitectureConfig(
            "monarch", "residual_ffn", 4, 4
        ).to_dict(),
        "lr": 1e-3,
        "steps": 2_048,
        "effective_batch": 512,
        "status": "complete",
        "validation": {"kl": 1.5},
    }
    assert select_tensor_reference([reference]) == reference
    probes = tensor_lr_probe_trials(reference)
    assert len(probes) == 10
    assert {trial.steps * trial.effective_batch for trial in probes} == {
        262_144
    }
    assert sum(trial.lr_parameterization == "mup" for trial in probes) == 8
    assert all(trial.allow_divergence for trial in probes)

    probe_results = []
    for trial in probes:
        probe_results.append({
            "label": trial.label,
            "lr": trial.lr,
            "lr_parameterization": trial.lr_parameterization,
            "status": "complete",
            "validation": {
                "kl": (
                    1.0
                    if trial.lr == 3e-3
                    and trial.lr_parameterization == "mup"
                    else 2.0 + trial.lr
                )
            },
        })
    settings = select_tensor_lr_settings(probe_results)
    assert settings[0] == (3e-3, "mup")
    frontier = tensor_parameter_trials(reference, probe_results)
    assert len(frontier) == 8
    assert len({trial.architecture for trial in frontier}) == 4
    assert {trial.steps * trial.effective_batch for trial in frontier} == {
        1_048_576
    }
    assert [config.depth for config in btt_parameter_architectures()] == [
        11, 20, 7, 35
    ]


def test_tensor_boundary_time_grid_and_final_selection_are_deterministic():
    reference = {
        "label": "reference",
        "architecture": ArchitectureConfig(
            "monarch", "residual_ffn", 4, 4
        ).to_dict(),
        "lr": 1e-3,
        "steps": 4_096,
        "effective_batch": 256,
        "status": "complete",
        "validation": {"kl": 1.5},
    }
    probes = []
    for trial in tensor_lr_probe_trials(reference):
        probes.append({
            "label": trial.label,
            "lr": trial.lr,
            "lr_parameterization": trial.lr_parameterization,
            "status": "complete",
            "validation": {"kl": 2.0 - trial.lr},
        })
    boundary = next_tensor_boundary_probe(reference, probes)
    assert boundary is not None
    assert boundary.lr == 2e-2
    probes[-1]["validation"]["kl"] = 3.0

    kronecker_probes = []
    for trial in kronecker_lr_probe_trials(reference):
        kronecker_probes.append({
            "label": trial.label,
            "lr": trial.lr,
            "lr_parameterization": trial.lr_parameterization,
            "status": "complete",
            "validation": {"kl": 2.1 + trial.lr},
        })
    assert len(select_kronecker_lr_settings(kronecker_probes)) == 2

    depths = {
        tensor_family_key(config): index + 2
        for index, config in enumerate(btt_parameter_architectures())
    }
    depths[tensor_family_key(kronecker_parameter_architecture())] = 12
    time_grid = tensor_time_trials(
        reference,
        probes,
        depths,
        kronecker_probes,
    )
    assert len(time_grid) == 10
    assert {trial.steps * trial.effective_batch for trial in time_grid} == {
        1_048_576
    }
    parameter_grid = [
        *tensor_parameter_trials(reference, probes),
        *kronecker_parameter_trials(reference, kronecker_probes),
    ]
    parameter_results = []
    for index, trial in enumerate(parameter_grid):
        parameter_results.append({
            "label": trial.label,
            "architecture": trial.architecture.to_dict(),
            "lr": trial.lr,
            "lr_parameterization": trial.lr_parameterization,
            "effective_batch": trial.effective_batch,
            "status": "complete",
            "validation": {"kl": 1.0 + index / 10},
            "trainable_parameters": 80_000_000 + index,
        })
    time_results = []
    for index, trial in enumerate(time_grid):
        time_results.append({
            "label": trial.label,
            "architecture": trial.architecture.to_dict(),
            "lr": trial.lr,
            "lr_parameterization": trial.lr_parameterization,
            "effective_batch": trial.effective_batch,
            "status": "complete",
            "validation": {"kl": 0.9 + index / 10},
            "trainable_parameters": 70_000_000 + index,
        })
    btt_winner, kronecker_winner, time_winner = select_tensor_finalists(
        parameter_results, time_results
    )
    assert btt_winner == parameter_results[0]
    assert kronecker_winner == parameter_results[8]
    assert time_winner == time_results[0]
    finals = tensor_final_trials(parameter_results, time_results)
    assert len(finals) == 9
    assert {trial.seed for trial in finals} == {0, 1, 2}
    assert {trial.steps * trial.effective_batch for trial in finals} == {
        4_096_000
    }
    final_results = [
        {
            "label": trial.label,
            "architecture": trial.architecture.to_dict(),
            "lr": trial.lr,
            "lr_parameterization": trial.lr_parameterization,
            "effective_batch": trial.effective_batch,
            "seed": trial.seed,
            "steps": trial.steps,
            "steps_completed": trial.steps,
            "status": "complete",
            "validation": {"kl": 0.9 + 0.01 * index},
        }
        for index, trial in enumerate(finals)
    ]
    long_trial = tensor_long_trial(
        parameter_results,
        time_results,
        final_results,
    )
    assert long_trial.stage == "tensor_long"
    assert long_trial.allow_data_reuse
    assert long_trial.target_validation_kl == 1.0
    assert (
        long_trial.steps * long_trial.effective_batch == 32_768_000
    )
    assert long_trial.warm_start_label in {
        result["label"] for result in final_results
    }


def test_controlled_gate_and_kronecker_depth_grid_are_explicit():
    reference = {
        "label": "reference",
        "architecture": ArchitectureConfig(
            "monarch", "residual_ffn", 4, 4
        ).to_dict(),
        "lr": 1e-3,
        "steps": 4_096,
        "effective_batch": 256,
        "status": "complete",
        "validation": {"kl": 1.5},
        "embedding_sha256": "a" * 64,
    }
    control_trial = tensor_gate_control_trial(reference)
    control = {
        "label": control_trial.label,
        "status": "complete",
        "validation": {"kl": 2.4},
        "embedding_sha256": "a" * 64,
    }
    gate_trials = tensor_gate_btt_trials(reference)
    results = [
        {
            "label": trial.label,
            "status": "complete",
            "initial_validation": {"kl": 8.0},
            "validation": {"kl": 3.0 + index},
            "diagnostic_summary": {
                "activation_rms_growth_max": 1.5,
            },
            "embedding_sha256": "a" * 64,
        }
        for index, trial in enumerate(gate_trials)
    ]
    gate = evaluate_tensor_gate(control, results)
    assert gate["status"] == "passed"

    probes = []
    for trial in kronecker_lr_probe_trials(reference):
        probes.append({
            "label": trial.label,
            "lr": trial.lr,
            "lr_parameterization": trial.lr_parameterization,
            "status": "complete",
            "validation": {"kl": 2.0 + trial.lr},
        })
    depth_grid = kronecker_depth_trials(reference, probes)
    assert [
        (trial.architecture.depth, trial.architecture.kronecker_rank)
        for trial in depth_grid
    ] == [(32, 291), (64, 140), (128, 64)]
    assert {
        config.depth for config in kronecker_rank_architectures()
    } == {32, 64, 128}
    assert {
        trial.steps * trial.effective_batch for trial in depth_grid
    } == {262_144}


def test_kronecker_rank_probe_is_parameter_matched_and_unique():
    trials = kronecker_rank_probe_trials({"effective_batch": 512})
    assert len(trials) == 6
    assert len({trial.label for trial in trials}) == 6
    assert {
        (trial.architecture.depth, trial.architecture.kronecker_rank)
        for trial in trials
    } == {(32, 291), (64, 140), (128, 64)}
    assert {
        trial.architecture.kronecker_rank_chunk for trial in trials
    } == {32}
    assert {trial.lr for trial in trials} == {1e-6, 3e-6}
    assert {trial.steps for trial in trials} == {128}
    assert {trial.lr_parameterization for trial in trials} == {"mup"}
    assert {
        trial.steps * trial.effective_batch for trial in trials
    } == {65_536}


def test_kronecker_rank_controls_match_examples_and_parameter_scale():
    trials = kronecker_rank_control_trials({"effective_batch": 512})
    assert len(trials) == 2
    assert len({trial.label for trial in trials}) == 2
    assert {trial.architecture.label for trial in trials} == {
        "monarch-r1-residual_ffn-d4-x4"
    }
    assert {trial.lr for trial in trials} == {5e-4, 1e-3}
    assert {trial.steps for trial in trials} == {128}
    assert {trial.audit_every for trial in trials} == {32}
    assert {trial.lr_parameterization for trial in trials} == {"uniform"}
    assert {
        trial.steps * trial.effective_batch for trial in trials
    } == {65_536}


def test_kronecker_rank_boundary_probes_lr_and_batch():
    trials = kronecker_rank_boundary_trials()
    assert all(trial.is_final for trial in trials)
    assert {
        (
            trial.architecture.depth,
            trial.architecture.kronecker_rank,
        )
        for trial in trials
    } == {(32, 291)}
    assert {
        (trial.effective_batch, trial.lr, trial.steps)
        for trial in trials
    } == {
        (512, 6e-6, 128),
        (256, 3e-6, 256),
    }
    assert {trial.lr_parameterization for trial in trials} == {"mup"}
    assert {
        trial.steps * trial.effective_batch for trial in trials
    } == {65_536}


def test_kronecker_rank_batch_boundary_extends_update_scaling():
    trials = kronecker_rank_batch_boundary_trials()
    assert len(trials) == 5
    assert {
        (trial.effective_batch, trial.lr, trial.steps)
        for trial in trials
    } == {
        (256, 6e-6, 256),
        (128, 3e-6, 512),
        (64, 3e-6, 1_024),
        (32, 3e-6, 2_048),
        (16, 3e-6, 4_096),
    }
    assert {trial.audit_every for trial in trials} == {
        64, 128, 256, 512, 1_024,
    }
    assert all(trial.allow_divergence for trial in trials)
    assert all(trial.is_final for trial in trials)
    assert {
        trial.checkpoint_every_examples for trial in trials
    } == {16_384}
    assert {
        trial.steps * trial.effective_batch for trial in trials
    } == {65_536}
    b32 = next(
        trial for trial in trials if trial.effective_batch == 32
    )
    from qwen_fullwidth_distill.config import microbatch_for
    assert microbatch_for(b32.architecture, 32) == (32, 1)
    assert microbatch_for(b32.architecture, 16) == (16, 1)


def test_kronecker_microbatch_limit_retains_a_tail(monkeypatch):
    trial = kronecker_rank_batch_boundary_trials()[0]
    from qwen_fullwidth_distill.config import microbatch_for

    monkeypatch.setenv("QWEN_KRONECKER_MICROBATCH", "352")
    assert microbatch_for(trial.architecture, 512) == (352, 2)
    assert microbatch_for(trial.architecture, 704) == (352, 2)


def test_kronecker_rank_batch_lr_boundary_retains_frontier_states():
    trials = kronecker_rank_batch_lr_boundary_trials()
    assert len(trials) == 5
    assert all(trial.is_final for trial in trials)
    assert {
        (trial.effective_batch, trial.lr, trial.steps, trial.audit_every)
        for trial in trials
    } == {
        (64, 6e-6, 1_024, 256),
        (32, 6e-6, 2_048, 512),
        (16, 6e-6, 4_096, 1_024),
        (8, 6e-6, 8_192, 2_048),
        (8, 3e-6, 8_192, 2_048),
    }
    assert {
        trial.checkpoint_every_examples for trial in trials
    } == {16_384}
    from qwen_fullwidth_distill.config import microbatch_for
    assert all(
        microbatch_for(trial.architecture, trial.effective_batch)
        == (trial.effective_batch, 1)
        for trial in trials
    )


def test_kronecker_rank_batch_lr_continuation_branches_exact_b32_state():
    trials = kronecker_rank_batch_lr_continuation_trials()
    source = next(
        trial
        for trial in kronecker_rank_batch_lr_boundary_trials()
        if trial.effective_batch == 32 and trial.lr == 6e-6
    )
    assert len(trials) == 2
    assert {trial.lr for trial in trials} == {6e-6, 1.2e-5}
    assert all(trial.is_final for trial in trials)
    assert all(trial.steps == 8_192 for trial in trials)
    assert all(trial.audit_every == 512 for trial in trials)
    assert {
        trial.checkpoint_every_examples for trial in trials
    } == {16_384}
    assert all(trial.warm_start_step == source.steps for trial in trials)
    assert all(trial.warm_start_stage == source.stage for trial in trials)
    assert all(trial.warm_start_label == source.label for trial in trials)
    assert {
        trial.lr: trial.warm_start_lr_override for trial in trials
    } == {6e-6: False, 1.2e-5: True}
    assert {
        (trial.steps - trial.warm_start_step) * trial.effective_batch
        for trial in trials
    } == {196_608}


def test_kronecker_rank_chunk_benchmark_changes_only_chunking():
    trials = kronecker_rank_chunk_benchmark_trials()
    assert {
        trial.architecture.kronecker_rank_chunk for trial in trials
    } == {16, 32, 48, 64, 128, 291}
    assert {
        (
            trial.architecture.depth,
            trial.architecture.kronecker_rank,
            trial.effective_batch,
            trial.lr,
            trial.lr_parameterization,
        )
        for trial in trials
    } == {(32, 291, 32, 6e-6, "mup")}
    without_chunk = []
    for trial in trials:
        value = trial.architecture.to_dict()
        value.pop("kronecker_rank_chunk")
        without_chunk.append(value)
    assert without_chunk[1:] == without_chunk[:-1]


def test_kronecker_rank_compile_benchmark_keeps_production_shape():
    trial = kronecker_rank_compile_benchmark_trial()
    assert trial.compile_model
    assert trial.effective_batch == 32
    assert trial.lr == 6e-6
    assert (
        trial.architecture.depth,
        trial.architecture.kronecker_rank,
        trial.architecture.kronecker_rank_chunk,
    ) == (32, 291, 32)


def test_kronecker_rank_continuation_resumes_exact_leader_state():
    trial = kronecker_rank_continuation_trial(96)
    assert trial.is_final
    assert (
        trial.architecture.depth,
        trial.architecture.kronecker_rank,
    ) == (32, 291)
    assert trial.lr == 3e-6
    assert trial.lr_parameterization == "mup"
    assert trial.steps == 512
    assert trial.steps * trial.effective_batch == 262_144
    assert trial.warm_start_step == 96
    assert trial.warm_start_stage == "tensor_kron_rank_checkpoint"
    assert trial.warm_start_label == (
        "tensor_kron_rank_early-kronecker-c3-r291-"
        "residual_ffn-d32-x4-lr3e-06-b512-pmup-s0"
    )
    assert trial.target_validation_kl == 1.0


def test_kronecker_rank_monarch_long_matches_continuation_budget():
    trial = kronecker_rank_monarch_long_trial()
    assert trial.is_final
    assert trial.architecture.label == "monarch-r1-residual_ffn-d4-x4"
    assert trial.lr == 1e-3
    assert trial.lr_parameterization == "uniform"
    assert trial.steps == 512
    assert trial.steps * trial.effective_batch == 262_144
    assert trial.audit_every == 128
    assert trial.target_validation_kl == 1.0


def test_kronecker_rank_monarch_continuation_resumes_exact_long_state():
    trial = kronecker_rank_monarch_continuation_trial(512)
    source = kronecker_rank_monarch_long_trial()
    assert trial.is_final
    assert trial.architecture == source.architecture
    assert trial.lr == source.lr == 1e-3
    assert trial.steps == 4_096
    assert trial.steps * trial.effective_batch == 2_097_152
    assert trial.audit_every == 512
    assert trial.warm_start_step == 512
    assert trial.warm_start_stage == source.stage
    assert trial.warm_start_label == source.label
    assert trial.target_validation_kl == 1.0


def test_kronecker_rank_monarch_decay_branches_from_2m_state():
    trials = kronecker_rank_monarch_decay_trials()
    source = kronecker_rank_monarch_continuation_trial()
    assert len(trials) == 2
    assert {trial.lr for trial in trials} == {5e-4, 3e-4}
    assert all(trial.is_final for trial in trials)
    assert all(trial.warm_start_lr_override for trial in trials)
    assert {
        (
            trial.steps,
            trial.warm_start_step,
            trial.warm_start_stage,
            trial.warm_start_label,
        )
        for trial in trials
    } == {
        (6_144, 4_096, source.stage, source.label),
    }
    assert {
        (trial.steps - trial.warm_start_step) * trial.effective_batch
        for trial in trials
    } == {1_048_576}
    assert {trial.audit_every for trial in trials} == {512}
    assert {trial.checkpoint_every_examples for trial in trials} == {524_288}


def test_kronecker_rank_monarch_batch_screen_is_fixed_example():
    trials = kronecker_rank_monarch_batch_trials()
    assert len(trials) == 9
    assert {
        (
            trial.architecture.monarch_rank,
            trial.effective_batch,
            trial.lr,
        )
        for trial in trials
    } == {
        (1, 256, 1e-3),
        (1, 256, 5e-4),
        (2, 256, 1e-3),
        (1, 128, 1e-3),
        (1, 128, 5e-4),
        (1, 64, 1e-3),
        (1, 64, 5e-4),
        (1, 32, 1e-3),
        (2, 32, 1e-3),
    }
    assert {
        trial.effective_batch for trial in trials
    } == {32, 64, 128, 256}
    assert {trial.steps for trial in trials} == {
        256, 512, 1_024, 2_048,
    }
    assert {trial.audit_every for trial in trials} == {
        64, 128, 256, 512,
    }
    assert {
        trial.steps * trial.effective_batch for trial in trials
    } == {65_536}
    assert all(trial.is_final for trial in trials)
    assert all(trial.allow_divergence for trial in trials)
    assert {
        (trial.architecture.monarch_rank, trial.effective_batch):
        trial.max_activation_rms_growth
        for trial in trials
        if trial.effective_batch == 32
    } == {(1, 32): 10.0, (2, 32): 50.0}
    assert {
        trial.checkpoint_every_examples for trial in trials
    } == {65_536}


def test_kronecker_rank_monarch_batch_transition_preserves_examples():
    trials = kronecker_rank_monarch_batch_transition_trials()
    source = kronecker_rank_monarch_decay_continuation_trial(
        3e-4,
        1.5e-4,
    )
    assert len(trials) == 2
    assert {trial.effective_batch for trial in trials} == {128, 512}
    assert all(trial.is_final for trial in trials)
    assert all(trial.warm_start_stage == source.stage for trial in trials)
    assert all(trial.warm_start_label == source.label for trial in trials)
    assert all(trial.warm_start_step == source.steps for trial in trials)
    assert {
        trial.effective_batch: trial.warm_start_resume_step
        for trial in trials
    } == {512: 8_192, 128: 32_768}
    assert {
        trial.warm_start_resume_step * trial.effective_batch
        for trial in trials
    } == {4_194_304}
    assert {
        trial.steps * trial.effective_batch for trial in trials
    } == {5_242_880}
    assert {
        trial.audit_every * trial.effective_batch for trial in trials
    } == {262_144}
    assert {
        trial.checkpoint_every_examples for trial in trials
    } == {524_288}
    assert all(trial.allow_data_reuse for trial in trials)


def test_kronecker_rank_monarch_batch_transition_scales_lr():
    trials = kronecker_rank_monarch_batch_transition_lr_trials()
    source = kronecker_rank_monarch_decay_continuation_trial(
        3e-4,
        1.5e-4,
    )
    assert len(trials) == 4
    assert {
        (trial.effective_batch, trial.lr) for trial in trials
    } == {
        (128, 7.5e-5),
        (128, 3.75e-5),
        (64, 1.875e-5),
        (32, 9.375e-6),
    }
    assert all(trial.warm_start_stage == source.stage for trial in trials)
    assert all(trial.warm_start_label == source.label for trial in trials)
    assert all(trial.warm_start_step == 8_192 for trial in trials)
    assert {
        trial.effective_batch: trial.warm_start_resume_step
        for trial in trials
    } == {128: 32_768, 64: 65_536, 32: 131_072}
    assert all(trial.warm_start_lr_override for trial in trials)
    assert {
        trial.warm_start_resume_step * trial.effective_batch
        for trial in trials
    } == {4_194_304}
    assert {
        trial.steps * trial.effective_batch for trial in trials
    } == {5_242_880}
    assert {
        trial.audit_every * trial.effective_batch for trial in trials
    } == {262_144}
    assert all(trial.allow_data_reuse and trial.is_final for trial in trials)


def test_kronecker_rank_monarch_batch_frontier_relaxes_false_guard():
    trials = kronecker_rank_monarch_batch_frontier_trials()
    assert len(trials) == 3
    assert {
        (trial.effective_batch, trial.lr) for trial in trials
    } == {
        (32, 1e-3),
        (16, 1e-3),
        (16, 5e-4),
    }
    assert {
        trial.steps * trial.effective_batch for trial in trials
    } == {65_536}
    assert {
        trial.audit_every * trial.effective_batch for trial in trials
    } == {16_384}
    assert {trial.max_activation_rms_growth for trial in trials} == {50.0}
    assert all(trial.allow_divergence and trial.is_final for trial in trials)


def test_kronecker_rank_monarch_batch_continuation_is_exact():
    trials = kronecker_rank_monarch_batch_continuation_trials()
    source = next(
        trial
        for trial in kronecker_rank_monarch_batch_frontier_trials()
        if trial.effective_batch == 32 and trial.lr == 1e-3
    )
    assert len(trials) == 2
    assert {trial.lr for trial in trials} == {1e-3, 5e-4}
    assert all(trial.effective_batch == 32 for trial in trials)
    assert all(trial.steps == 8_192 for trial in trials)
    assert all(trial.warm_start_stage == source.stage for trial in trials)
    assert all(trial.warm_start_label == source.label for trial in trials)
    assert all(trial.warm_start_step == source.steps for trial in trials)
    assert {
        trial.lr: trial.warm_start_lr_override for trial in trials
    } == {1e-3: False, 5e-4: True}
    assert {
        (trial.steps - trial.warm_start_step) * trial.effective_batch
        for trial in trials
    } == {196_608}
    assert all(trial.allow_divergence and trial.is_final for trial in trials)


def test_kronecker_rank_monarch_batch_long_is_exact():
    trials = kronecker_rank_monarch_batch_long_trials()
    source = next(
        trial
        for trial in kronecker_rank_monarch_batch_continuation_trials()
        if trial.lr == 5e-4
    )
    assert len(trials) == 2
    assert {trial.lr for trial in trials} == {5e-4, 2.5e-4}
    assert all(trial.effective_batch == 32 for trial in trials)
    assert all(trial.steps == 32_768 for trial in trials)
    assert all(trial.warm_start_stage == source.stage for trial in trials)
    assert all(trial.warm_start_label == source.label for trial in trials)
    assert all(trial.warm_start_step == 8_192 for trial in trials)
    assert {
        trial.lr: trial.warm_start_lr_override for trial in trials
    } == {5e-4: False, 2.5e-4: True}
    assert {
        (trial.steps - trial.warm_start_step) * trial.effective_batch
        for trial in trials
    } == {786_432}
    assert {
        trial.audit_every * trial.effective_batch for trial in trials
    } == {131_072}
    assert all(trial.allow_divergence and trial.is_final for trial in trials)


def test_kronecker_rank_monarch_batch_million_is_exact():
    trials = kronecker_rank_monarch_batch_million_trials()
    source = next(
        trial
        for trial in kronecker_rank_monarch_batch_long_trials()
        if trial.lr == 2.5e-4
    )
    assert len(trials) == 2
    assert {trial.lr for trial in trials} == {2.5e-4, 1.25e-4}
    assert all(trial.effective_batch == 32 for trial in trials)
    assert all(trial.steps == 65_536 for trial in trials)
    assert all(trial.warm_start_stage == source.stage for trial in trials)
    assert all(trial.warm_start_label == source.label for trial in trials)
    assert all(trial.warm_start_step == 32_768 for trial in trials)
    assert {
        trial.lr: trial.warm_start_lr_override for trial in trials
    } == {2.5e-4: False, 1.25e-4: True}
    assert {
        (trial.steps - trial.warm_start_step) * trial.effective_batch
        for trial in trials
    } == {1_048_576}
    assert {
        trial.audit_every * trial.effective_batch for trial in trials
    } == {262_144}
    assert {
        trial.checkpoint_every_examples for trial in trials
    } == {524_288}
    assert all(trial.allow_divergence and trial.is_final for trial in trials)


def test_kronecker_rank_monarch_batch_two_million_is_exact():
    trials = kronecker_rank_monarch_batch_two_million_trials()
    source = next(
        trial
        for trial in kronecker_rank_monarch_batch_million_trials()
        if trial.lr == 1.25e-4
    )
    assert len(trials) == 2
    assert {trial.lr for trial in trials} == {1.25e-4, 6.25e-5}
    assert all(trial.effective_batch == 32 for trial in trials)
    assert all(trial.steps == 98_304 for trial in trials)
    assert all(trial.warm_start_stage == source.stage for trial in trials)
    assert all(trial.warm_start_label == source.label for trial in trials)
    assert all(trial.warm_start_step == 65_536 for trial in trials)
    assert {
        trial.lr: trial.warm_start_lr_override for trial in trials
    } == {1.25e-4: False, 6.25e-5: True}
    assert {
        (trial.steps - trial.warm_start_step) * trial.effective_batch
        for trial in trials
    } == {1_048_576}
    assert {
        trial.audit_every * trial.effective_batch for trial in trials
    } == {262_144}
    assert {
        trial.checkpoint_every_examples for trial in trials
    } == {262_144}
    assert all(trial.allow_divergence and trial.is_final for trial in trials)


def test_spend_limit_restart_slate_is_exactly_eight_cells():
    trials = kronecker_rank_resume_after_spend_trials()
    assert len(trials) == 8
    assert len({trial.label for trial in trials}) == 8
    assert {
        trial.stage for trial in trials
    } == {
        "tensor_kron_rank_monarch_batch_two_million",
        "tensor_kron_rank_monarch_batch_transition_lr",
        "tensor_kron_rank_batch_lr_continue",
        "tensor_kron_rank_depth_optimized",
    }
    assert {
        trial.lr
        for trial in trials
        if trial.stage == "tensor_kron_rank_monarch_batch_two_million"
    } == {1.25e-4, 6.25e-5}
    assert {
        trial.effective_batch
        for trial in trials
        if trial.stage == "tensor_kron_rank_monarch_batch_transition_lr"
    } == {32, 64}
    assert {
        trial.architecture.depth
        for trial in trials
        if trial.stage == "tensor_kron_rank_depth_optimized"
    } == {4, 8, 16}


def test_next_winner_slate_is_exactly_eight_cells():
    trials = kronecker_rank_next_eight_trials()
    assert len(trials) == 8
    assert len({trial.label for trial in trials}) == 8
    assert {
        trial.stage for trial in trials
    } == {
        "tensor_kron_rank_depth_winner_continue",
        "tensor_kron_rank_monarch_mature_continue",
        "tensor_kron_rank_monarch_batch_three_million",
        "tensor_kron_rank_monarch_batch_rank2_retry",
    }
    assert all(trial.is_final for trial in trials)


def test_shallow_kronecker_winner_continuation_is_exact():
    trials = kronecker_rank_depth_winner_continuation_trials()
    sources = {
        trial.architecture.depth: trial
        for trial in kronecker_rank_depth_optimized_trials()
        if trial.architecture.depth in (4, 8)
    }
    assert {
        (trial.architecture.depth, trial.lr) for trial in trials
    } == {(4, 6e-6), (4, 3e-6), (8, 6e-6)}
    assert {trial.steps for trial in trials} == {4_096}
    assert {trial.audit_every for trial in trials} == {512}
    assert {trial.checkpoint_every_examples for trial in trials} == {16_384}
    for trial in trials:
        source = sources[trial.architecture.depth]
        assert trial.warm_start_stage == source.stage
        assert trial.warm_start_label == source.label
        assert trial.warm_start_step == 2_048
        assert trial.warm_start_lr_override == (trial.lr != source.lr)


def test_fineweb_edu_lr_sweep_is_eight_guarded_parallel_cells():
    trials = kronecker_edu_lr_trials()
    assert len(trials) == 8
    assert len({trial.label for trial in trials}) == 8
    assert {trial.architecture.depth for trial in trials} == {4}
    assert {trial.architecture.kronecker_rank for trial in trials} == {2407}
    assert {trial.effective_batch for trial in trials} == {32}
    assert {trial.steps for trial in trials} == {12_288}
    assert {trial.warm_start_step for trial in trials} == {4_096}
    assert {trial.lr_schedule for trial in trials} == {"warmup_cosine"}
    assert {trial.warmup_steps for trial in trials} == {256}
    assert {trial.min_lr_ratio for trial in trials} == {0.1}
    assert {trial.gradient_clip_norm for trial in trials} == {1.0}
    assert {trial.dataset_tag for trial in trials} == {"fwedu350bt-v1p4"}
    assert min(trial.lr for trial in trials) == 3e-6
    assert max(trial.lr for trial in trials) == 3.6e-5


def test_fineweb_edu_long_trial_resumes_pilot_to_four_million_examples():
    trial = kronecker_edu_long_trial(1.2e-5)
    source = next(
        value for value in kronecker_edu_lr_trials()
        if value.lr == trial.lr
    )
    assert trial.is_final
    assert trial.steps == 131_072
    assert trial.warm_start_stage == source.stage
    assert trial.warm_start_label == source.label
    assert trial.warm_start_step == source.steps
    assert trial.lr_schedule == "warmup_cosine"
    assert trial.warmup_steps == 512
    assert trial.min_lr_ratio == 0.03


def test_mature_monarch_continuation_is_exact():
    trials = kronecker_rank_monarch_mature_continuation_trials()
    assert len(trials) == 2
    assert {trial.lr for trial in trials} == {3.75e-5, 1.875e-5}
    assert {trial.effective_batch for trial in trials} == {128}
    assert {trial.steps for trial in trials} == {49_152}
    assert {trial.warm_start_step for trial in trials} == {40_960}
    assert {
        (trial.steps - trial.warm_start_step) * trial.effective_batch
        for trial in trials
    } == {1_048_576}
    assert all(trial.allow_data_reuse for trial in trials)


def test_optimized_monarch_three_million_continuation_is_exact():
    trials = kronecker_rank_monarch_batch_three_million_trials()
    assert len(trials) == 2
    assert {trial.lr for trial in trials} == {6.25e-5, 3.125e-5}
    assert {trial.effective_batch for trial in trials} == {32}
    assert {trial.steps for trial in trials} == {131_072}
    assert {trial.warm_start_step for trial in trials} == {98_304}
    assert {
        (trial.steps - trial.warm_start_step) * trial.effective_batch
        for trial in trials
    } == {1_048_576}


def test_monarch_rank2_retry_has_fresh_label_and_relaxed_guard():
    trial = kronecker_rank_monarch_rank2_retry_trial()
    assert trial.architecture.monarch_rank == 2
    assert trial.effective_batch == 32
    assert trial.steps == 2_048
    assert trial.max_activation_rms_growth == 50.0
    assert trial.stage == "tensor_kron_rank_monarch_batch_rank2_retry"
    assert trial.is_final


def test_kronecker_rank_depth_optimized_is_parameter_matched():
    trials = kronecker_rank_depth_optimized_trials()
    assert len(trials) == 5
    assert {
        (trial.architecture.depth, trial.architecture.kronecker_rank)
        for trial in trials
    } == {
        (4, 2407),
        (8, 1198),
        (16, 593),
        (64, 140),
        (128, 64),
    }
    assert {trial.effective_batch for trial in trials} == {32}
    assert {trial.lr for trial in trials} == {6e-6}
    assert {trial.lr_parameterization for trial in trials} == {"mup"}
    assert {
        trial.steps * trial.effective_batch for trial in trials
    } == {65_536}
    assert {
        trial.checkpoint_every_examples for trial in trials
    } == {16_384}
    assert all(trial.allow_divergence and trial.is_final for trial in trials)


def test_monarch_depth_trades_factor_size_at_fixed_factor_count():
    trials = kronecker_rank_monarch_depth_parameter_matched_trials()
    assert len(trials) == 2
    assert {trial.lr for trial in trials} == {1e-3, 5e-4}
    assert all(trial.effective_batch == 32 for trial in trials)
    assert all(trial.architecture.depth == 8 for trial in trials)
    assert all(trial.architecture.monarch_blocks == 256 for trial in trials)
    assert all(trial.max_activation_rms_growth == 100.0 for trial in trials)
    # Each FFN has up/down factors. Doubling blocks halves factor parameters
    # per layer, so d8/b256 exactly matches d4/b128 structured capacity.
    width = trials[0].architecture.full_width
    expansion = trials[0].architecture.expansion

    def factor_parameters(depth, blocks):
        input_block = width // blocks
        output_block = width * expansion // blocks
        per_map = blocks * (
            input_block * input_block
            + output_block * input_block
        )
        return depth * 2 * per_map

    assert factor_parameters(8, 256) == factor_parameters(4, 128)
    assert factor_parameters(8, 256) == 83_886_080
    assert {
        trial.steps * trial.effective_batch for trial in trials
    } == {65_536}
    assert all(trial.allow_divergence and trial.is_final for trial in trials)


def test_kronecker_rank_monarch_capacity_matches_depth_times_rank():
    trials = kronecker_rank_monarch_capacity_trials()
    assert len(trials) == 2
    assert {
        (trial.architecture.depth, trial.architecture.monarch_rank)
        for trial in trials
    } == {(8, 1), (4, 2)}
    assert {
        trial.architecture.depth * trial.architecture.monarch_rank
        for trial in trials
    } == {8}
    assert {trial.steps for trial in trials} == {128}
    assert {trial.audit_every for trial in trials} == {32}
    assert {trial.lr for trial in trials} == {1e-3}
    assert all(trial.is_final for trial in trials)
    assert all(trial.allow_divergence for trial in trials)
    assert {
        trial.checkpoint_every_examples for trial in trials
    } == {65_536}


def test_kronecker_rank_monarch_scaled_depth_is_parameter_identical():
    scaled = kronecker_rank_monarch_scaled_depth_trial()
    unscaled = next(
        trial
        for trial in kronecker_rank_monarch_capacity_trials()
        if trial.architecture.depth == 8
    )
    assert scaled.is_final
    assert scaled.architecture.residual_scale == "inverse_sqrt_depth"
    assert np.isclose(
        scaled.architecture.residual_multiplier,
        8 ** -0.5,
    )
    scaled_architecture = scaled.architecture.to_dict()
    unscaled_architecture = unscaled.architecture.to_dict()
    scaled_architecture.pop("residual_scale")
    assert scaled_architecture == unscaled_architecture
    assert scaled.lr == unscaled.lr
    assert scaled.steps == unscaled.steps


def test_kronecker_rank_monarch_capacity_continuation_is_exact():
    trial = kronecker_rank_monarch_capacity_continuation_trial()
    source = kronecker_rank_monarch_capacity_checkpoint_trial()
    assert trial.is_final
    assert trial.architecture == source.architecture
    assert trial.steps == 512
    assert trial.warm_start_step == source.steps == 128
    assert trial.warm_start_stage == source.stage
    assert trial.warm_start_label == source.label
    assert not trial.warm_start_lr_override
    assert (
        trial.steps - trial.warm_start_step
    ) * trial.effective_batch == 196_608
    assert trial.audit_every == 128
    assert trial.checkpoint_every_examples == 262_144


def test_kronecker_rank_monarch_decay_continuation_is_exact_and_selectable():
    source = next(
        trial
        for trial in kronecker_rank_monarch_decay_trials()
        if trial.lr == 5e-4
    )
    same_lr = kronecker_rank_monarch_decay_continuation_trial(5e-4, 5e-4)
    lower_lr = kronecker_rank_monarch_decay_continuation_trial(
        5e-4,
        2.5e-4,
    )
    assert same_lr.is_final
    assert same_lr.steps == 8_192
    assert same_lr.warm_start_step == source.steps == 6_144
    assert same_lr.warm_start_stage == source.stage
    assert same_lr.warm_start_label == source.label
    assert not same_lr.warm_start_lr_override
    assert lower_lr.warm_start_lr_override
    assert lower_lr.lr == 2.5e-4
    assert {
        (trial.steps - trial.warm_start_step) * trial.effective_batch
        for trial in (same_lr, lower_lr)
    } == {1_048_576}
    assert same_lr.audit_every == lower_lr.audit_every == 512
    assert (
        same_lr.checkpoint_every_examples
        == lower_lr.checkpoint_every_examples
        == 524_288
    )


def test_kronecker_rank_monarch_lr_boundary_extends_open_edge():
    trials = kronecker_rank_monarch_lr_boundary_trials()
    assert len(trials) == 2
    assert {trial.architecture.label for trial in trials} == {
        "monarch-r1-residual_ffn-d4-x4"
    }
    assert {trial.lr for trial in trials} == {2e-3, 3e-3}
    assert {trial.steps for trial in trials} == {128}
    assert {trial.audit_every for trial in trials} == {32}
    assert all(trial.allow_divergence for trial in trials)
    assert {
        trial.steps * trial.effective_batch for trial in trials
    } == {65_536}


def test_kronecker_rank_monarch_loop_controls_share_parameters():
    trials = kronecker_rank_monarch_loop_trials()
    assert len(trials) == 2
    assert {
        (
            trial.architecture.depth,
            trial.architecture.repetitions,
            trial.architecture.effective_depth,
        )
        for trial in trials
    } == {
        (4, 2, 8),
        (4, 4, 16),
    }
    assert {
        trial.architecture.residual_scale for trial in trials
    } == {"inverse_repetitions"}
    assert {trial.lr for trial in trials} == {1e-3}
    assert {
        trial.steps * trial.effective_batch for trial in trials
    } == {65_536}
