from __future__ import annotations

import math
from dataclasses import replace

from .config import ArchitectureConfig, TrialConfig, matching_monarch, same_width_screen


LR_GRID = (3e-4, 1e-3, 3e-3)
DEPTH_LR_GRID = (3e-4, 1e-3)
OPTIMIZATION_GRID = {
    512: (5e-4, 1e-3),
    256: (3e-4, 6e-4),
}
TENSOR_LR_GRID = (1e-5, 3e-5, 1e-4, 3e-4, 1e-3, 3e-3, 6e-3, 1e-2)
TENSOR_PROBE_EXAMPLES = 262_144
TENSOR_SCREEN_EXAMPLES = 1_048_576
TENSOR_FINAL_EXAMPLES = 4_096_000
KRONECKER_PROBE_EXAMPLES = 65_536
KRONECKER_LR_GRID = (1e-6, 3e-6, 1e-5, 3e-5, 1e-4)
KRONECKER_UNIFORM_LR_GRID = (3e-4, 1e-3, 3e-3)
TARGET_VALIDATION_KL = 1.0
TENSOR_LONG_EXAMPLES = 32_768_000
KRONECKER_EDU_LR_GRID = (
    3e-6,
    4.5e-6,
    6e-6,
    9e-6,
    1.2e-5,
    1.8e-5,
    2.4e-5,
    3.6e-5,
)
KRONECKER_EDU_PILOT_EXAMPLES = 262_144
KRONECKER_EDU_LONG_TOTAL_EXAMPLES = 4_194_304
KRONECKER_EDU_POLICY_EXAMPLES = 262_144
KRONECKER_EDU_OBJECTIVE_EXAMPLES = 524_288
KRONECKER_EDU_ARCHITECTURE_SCREEN_EXAMPLES = 65_536
KRONECKER_EDU_WARMUP_EXAMPLES = 8_192
KRONECKER_EDU_SCALE_BOUNDARIES = (
    (262_144, 4),
    (1_048_576, 2),
    (4_194_304, 1),
)


def architecture_key(config: ArchitectureConfig) -> tuple:
    return config.form, config.depth, config.expansion


def screen_trials() -> list[TrialConfig]:
    return [
        TrialConfig(config, lr=1e-3, seed=0, steps=1_000, stage="screen")
        for config in same_width_screen()
    ]


def select_common_topology(results: list[dict]) -> ArchitectureConfig:
    grouped: dict[tuple, dict[str, dict]] = {}
    for result in results:
        config = ArchitectureConfig(**result["architecture"])
        grouped.setdefault(architecture_key(config), {})[config.operator] = result
    candidates = []
    for key, operators in grouped.items():
        if set(operators) != {"dense", "monarch"}:
            continue
        score = sum(float(operators[op]["validation"]["kl"]) for op in operators) / 2
        dense = ArchitectureConfig(**operators["dense"]["architecture"])
        candidates.append((score, dense.depth, dense.expansion, dense))
    if not candidates:
        raise RuntimeError("screen produced no shared dense/Monarch cells")
    return min(candidates, key=lambda item: item[:3])[3]


def select_unrestricted_monarch(results: list[dict]) -> ArchitectureConfig:
    candidates = []
    for result in results:
        config = ArchitectureConfig(**result["architecture"])
        if config.operator != "monarch":
            continue
        candidates.append((
            float(result["validation"]["kl"]),
            int(result["trainable_parameters"]),
            config.depth,
            config,
        ))
    if not candidates:
        raise RuntimeError("screen produced no Monarch cells")
    return min(candidates, key=lambda item: item[:3])[3]


def tuning_trials(
    common_dense: ArchitectureConfig,
    deep_monarch: ArchitectureConfig,
) -> list[TrialConfig]:
    common_monarch = ArchitectureConfig(
        "monarch",
        common_dense.form,
        common_dense.depth,
        common_dense.expansion,
    )
    configs_and_lrs: list[tuple[ArchitectureConfig, tuple[float, ...]]] = [
        (common_dense, (3e-4, 3e-3)),
        (common_monarch, (3e-4, 3e-3)),
        (matching_monarch(common_dense), LR_GRID),
    ]
    if architecture_key(deep_monarch) != architecture_key(common_monarch):
        configs_and_lrs.append((deep_monarch, (3e-4, 3e-3)))
    return [
        TrialConfig(config, lr=lr, seed=0, steps=1_000, stage="tune")
        for config, lrs in configs_and_lrs
        for lr in lrs
    ]


def select_lr(config: ArchitectureConfig, results: list[dict]) -> float:
    matching = []
    key = architecture_key(config)
    for result in results:
        candidate = ArchitectureConfig(**result["architecture"])
        if (
            candidate.operator == config.operator
            and candidate.monarch_rank == config.monarch_rank
            and architecture_key(candidate) == key
        ):
            matching.append((float(result["validation"]["kl"]), float(result["lr"])))
    if not matching:
        raise RuntimeError(f"no LR results for {config.label}")
    return min(matching)[1]


def final_trials(
    screen_results: list[dict],
    tune_results: list[dict],
) -> list[TrialConfig]:
    common_dense = select_common_topology(screen_results)
    common_monarch = ArchitectureConfig(
        "monarch",
        common_dense.form,
        common_dense.depth,
        common_dense.expansion,
    )
    parameter_matched = matching_monarch(common_dense)
    deep_monarch = select_unrestricted_monarch(screen_results)
    all_results = screen_results + tune_results
    finalists = [
        (common_dense, select_lr(common_dense, all_results)),
        (common_monarch, select_lr(common_monarch, all_results)),
        (parameter_matched, select_lr(parameter_matched, all_results)),
    ]
    if architecture_key(deep_monarch) != architecture_key(common_monarch):
        finalists.append((deep_monarch, select_lr(deep_monarch, all_results)))
    return [
        TrialConfig(
            architecture=config,
            lr=lr,
            seed=seed,
            steps=4_000,
            stage="final",
            audit_every=250,
        )
        for config, lr in finalists
        for seed in (0, 1, 2)
    ]


def depth_reference_trials() -> list[TrialConfig]:
    """Committed baseline cells reused without allocating another H100."""
    config4 = ArchitectureConfig("monarch", "residual_ffn", 4, 4)
    config8 = ArchitectureConfig("monarch", "residual_ffn", 8, 4)
    return [
        TrialConfig(
            config4, lr=3e-4, seed=0, steps=1_000, stage="tune"
        ),
        TrialConfig(
            config4, lr=1e-3, seed=0, steps=1_000, stage="screen"
        ),
        TrialConfig(
            config8, lr=1e-3, seed=0, steps=1_000, stage="screen"
        ),
    ]


def depth_screen_trials() -> list[TrialConfig]:
    """The twelve uncached trials in the depth/sharing screen."""
    untied = [
        ArchitectureConfig("monarch", "residual_ffn", depth, 4)
        for depth in (8, 16, 32)
    ]
    looped = [
        ArchitectureConfig(
            "monarch",
            "residual_ffn",
            4,
            4,
            repetitions=repetitions,
            residual_scale="inverse_repetitions",
        )
        for repetitions in (2, 4, 8)
    ]
    trials = [
        TrialConfig(
            config,
            lr=lr,
            seed=0,
            steps=1_000,
            stage="depth_screen",
            audit_every=250,
        )
        for config in untied + looped
        for lr in DEPTH_LR_GRID
        if not (
            config.repetitions == 1
            and config.depth == 8
            and lr == 1e-3
        )
    ]
    # One diagnostic isolates the effect of loop damping at effective depth 16.
    trials.append(
        TrialConfig(
            ArchitectureConfig(
                "monarch",
                "residual_ffn",
                4,
                4,
                repetitions=4,
                residual_scale="none",
            ),
            lr=1e-3,
            seed=0,
            steps=1_000,
            stage="depth_screen",
            audit_every=250,
        )
    )
    if len(trials) != 12 or len({trial.label for trial in trials}) != 12:
        raise RuntimeError("depth screen must contain twelve unique new trials")
    return trials


def select_depth_winners(
    results: list[dict],
) -> tuple[ArchitectureConfig, float, ArchitectureConfig, float]:
    """Select untied and damped-loop winners using endpoint validation only."""
    untied = []
    looped = []
    for result in results:
        config = ArchitectureConfig(**result["architecture"])
        score = float(result["validation"]["kl"])
        candidate = (
            score,
            config.effective_depth,
            float(result["lr"]),
            config,
        )
        if config.repetitions == 1:
            untied.append(candidate)
        elif config.residual_scale == "inverse_repetitions":
            looped.append(candidate)
    if not untied or not looped:
        raise RuntimeError("depth selection requires untied and damped-loop cells")
    untied_winner = min(untied, key=lambda row: row[:3])
    looped_winner = min(looped, key=lambda row: row[:3])
    return (
        untied_winner[3],
        untied_winner[2],
        looped_winner[3],
        looped_winner[2],
    )


def depth_final_trials(results: list[dict]) -> list[TrialConfig]:
    untied, untied_lr, looped, looped_lr = select_depth_winners(results)
    return [
        TrialConfig(
            architecture=config,
            lr=lr,
            seed=seed,
            steps=4_000,
            stage="depth_final",
            audit_every=250,
        )
        for config, lr in ((untied, untied_lr), (looped, looped_lr))
        for seed in (0, 1, 2)
    ]


def optimization_trials(depth_results: list[dict]) -> list[TrialConfig]:
    """Batch/LR cells with exactly 1,048,576 training examples each."""
    untied, _, looped, _ = select_depth_winners(depth_results)
    trials = [
        TrialConfig(
            architecture=config,
            lr=lr,
            seed=0,
            steps=1_048_576 // batch,
            stage="depth_opt",
            audit_every=262_144 // batch,
            effective_batch=batch,
        )
        for config in (untied, looped)
        for batch, lrs in OPTIMIZATION_GRID.items()
        for lr in lrs
    ]
    if len(trials) != 8 or len({trial.label for trial in trials}) != 8:
        raise RuntimeError("optimization grid must contain eight unique trials")
    if {trial.steps * trial.effective_batch for trial in trials} != {1_048_576}:
        raise RuntimeError("optimization screen must use a fixed example budget")
    return trials


def select_optimized_winners(
    depth_results: list[dict],
    optimization_results: list[dict],
) -> tuple[
    ArchitectureConfig, float, int, ArchitectureConfig, float, int
]:
    untied, _, looped, _ = select_depth_winners(depth_results)

    def best_for(config: ArchitectureConfig) -> tuple:
        candidates = []
        for result in depth_results + optimization_results:
            candidate = ArchitectureConfig(**result["architecture"])
            if candidate != config:
                continue
            candidates.append((
                float(result["validation"]["kl"]),
                int(result.get("effective_batch", 1_024)),
                float(result["lr"]),
            ))
        if not candidates:
            raise RuntimeError(f"no optimization results for {config.label}")
        return min(candidates)

    untied_score, untied_batch, untied_lr = best_for(untied)
    looped_score, looped_batch, looped_lr = best_for(looped)
    del untied_score, looped_score
    return (
        untied,
        untied_lr,
        untied_batch,
        looped,
        looped_lr,
        looped_batch,
    )


def optimized_depth_final_trials(
    depth_results: list[dict],
    optimization_results: list[dict],
) -> list[TrialConfig]:
    (
        untied,
        untied_lr,
        untied_batch,
        looped,
        looped_lr,
        looped_batch,
    ) = select_optimized_winners(depth_results, optimization_results)
    return [
        TrialConfig(
            architecture=config,
            lr=lr,
            seed=seed,
            steps=4_096_000 // batch,
            stage="depth_final",
            audit_every=256_000 // batch,
            effective_batch=batch,
        )
        for config, lr, batch in (
            (untied, untied_lr, untied_batch),
            (looped, looped_lr, looped_batch),
        )
        for seed in (0, 1, 2)
    ]


def btt_parameter_architectures() -> list[ArchitectureConfig]:
    """The four approximately 84M-parameter BTT depth allocations."""
    result = [
        ArchitectureConfig(
            "btt",
            "residual_ffn",
            11,
            4,
            btt_cores=3,
            btt_rank=1,
        ),
        ArchitectureConfig(
            "btt",
            "residual_ffn",
            20,
            4,
            btt_cores=4,
            btt_rank=1,
        ),
        ArchitectureConfig(
            "btt",
            "residual_ffn",
            7,
            4,
            btt_cores=4,
            btt_rank=2,
        ),
        ArchitectureConfig(
            "btt",
            "residual_gated",
            35,
            btt_cores=4,
            btt_rank=1,
        ),
    ]
    for config in result:
        config.validate()
    return result


def kronecker_rank_architectures() -> list[ArchitectureConfig]:
    """Parameter-matched depth/rank allocations for sums of Kroneckers."""
    result = [
        ArchitectureConfig(
            "kronecker", "residual_ffn", depth, 4,
            kronecker_rank=rank,
            # A chunk of 32 keeps the largest temporary well below H100
            # capacity while cutting checkpoint/recompute dispatches by 4x.
            kronecker_rank_chunk=32,
        )
        for depth, rank in ((32, 291), (64, 140), (128, 64))
    ]
    for config in result:
        config.validate()
    return result


def kronecker_parameter_architecture() -> ArchitectureConfig:
    """Primary balanced parameter/time compromise: depth 64, rank 140."""
    return kronecker_rank_architectures()[1]


def kronecker_rank_probe_trials(
    reference: dict,
) -> list[TrialConfig]:
    """Short parameter-matched rank/depth grid informed by rank-one probes."""
    batch = int(reference.get("effective_batch", 512))
    trials = [
        TrialConfig(
            architecture,
            lr=lr,
            seed=0,
            steps=KRONECKER_PROBE_EXAMPLES // batch,
            stage="tensor_kron_rank_early",
            audit_every=16_384 // batch,
            compile_model=False,
            effective_batch=batch,
            lr_parameterization="mup",
            allow_divergence=True,
            checkpoint_every_examples=16_384,
        )
        for architecture in kronecker_rank_architectures()
        for lr in (1e-6, 3e-6)
    ]
    if len(trials) != 6 or len({trial.label for trial in trials}) != 6:
        raise RuntimeError("Kronecker rank probe must contain six unique cells")
    return trials


def kronecker_rank_control_trials(
    reference: dict,
) -> list[TrialConfig]:
    """Parameter- and example-matched Monarch controls for the rank grid."""
    batch = int(reference.get("effective_batch", 512))
    architecture = ArchitectureConfig(
        "monarch",
        "residual_ffn",
        4,
        4,
    )
    trials = [
        TrialConfig(
            architecture,
            lr=lr,
            seed=0,
            steps=KRONECKER_PROBE_EXAMPLES // batch,
            stage="tensor_kron_rank_control",
            audit_every=16_384 // batch,
            compile_model=False,
            effective_batch=batch,
            lr_parameterization="uniform",
        )
        for lr in (5e-4, 1e-3)
    ]
    if len(trials) != 2 or len({trial.label for trial in trials}) != 2:
        raise RuntimeError("Kronecker rank controls must contain two unique cells")
    return trials


def kronecker_rank_boundary_trials() -> list[TrialConfig]:
    """Probe the open LR edge and a smaller-batch alternative on the leader."""
    architecture = kronecker_rank_architectures()[0]
    settings = (
        # The 1e-5 smoke was unstable; 6e-6 is the geometric-ish midpoint
        # above the stable, currently leading 3e-6 setting.
        (512, 6e-6),
        # Same examples and base LR, twice as many AdamW updates.
        (256, 3e-6),
    )
    trials = [
        TrialConfig(
            architecture,
            lr=lr,
            seed=0,
            steps=KRONECKER_PROBE_EXAMPLES // batch,
            stage="tensor_kron_rank_boundary",
            audit_every=16_384 // batch,
            compile_model=False,
            effective_batch=batch,
            lr_parameterization="mup",
            allow_divergence=True,
            checkpoint_every_examples=16_384,
        )
        for batch, lr in settings
    ]
    if len(trials) != 2 or len({trial.label for trial in trials}) != 2:
        raise RuntimeError("Kronecker boundary probe must contain two unique cells")
    return trials


def kronecker_rank_batch_boundary_trials() -> list[TrialConfig]:
    """Extend the winning small-batch edge at a fixed example budget."""
    architecture = kronecker_rank_architectures()[0]
    settings = (
        (256, 6e-6),
        (128, 3e-6),
        (64, 3e-6),
        (32, 3e-6),
        (16, 3e-6),
    )
    trials = [
        TrialConfig(
            architecture,
            lr=lr,
            seed=0,
            steps=KRONECKER_PROBE_EXAMPLES // batch,
            stage="tensor_kron_rank_batch_boundary",
            audit_every=16_384 // batch,
            compile_model=False,
            effective_batch=batch,
            lr_parameterization="mup",
            allow_divergence=True,
            checkpoint_every_examples=16_384,
        )
        for batch, lr in settings
    ]
    if len(trials) != 5 or len({trial.label for trial in trials}) != 5:
        raise RuntimeError("Kronecker batch boundary needs five unique cells")
    return trials


def kronecker_rank_batch_lr_boundary_trials() -> list[TrialConfig]:
    """Probe the next update/LR frontier while retaining exact final states."""
    architecture = kronecker_rank_architectures()[0]
    settings = (
        # Apply the higher LR to the current full-validation batch winner.
        (64, 6e-6),
        # Combine the practical throughput frontier with the higher LR.
        (32, 6e-6),
        # Higher LR is already decisively ahead at batch 256.
        (16, 6e-6),
        # Complete the LR comparison at the smallest sampled batch.
        (8, 6e-6),
        # Continue the monotonically improving fixed-example batch sweep.
        (8, 3e-6),
    )
    trials = [
        TrialConfig(
            architecture,
            lr=lr,
            seed=0,
            steps=KRONECKER_PROBE_EXAMPLES // batch,
            stage="tensor_kron_rank_batch_lr_boundary",
            audit_every=16_384 // batch,
            compile_model=False,
            effective_batch=batch,
            lr_parameterization="mup",
            allow_divergence=True,
            checkpoint_every_examples=16_384,
        )
        for batch, lr in settings
    ]
    if len(trials) != 5 or len({trial.label for trial in trials}) != 5:
        raise RuntimeError(
            "Kronecker batch/LR boundary needs five unique cells"
        )
    return trials


def kronecker_rank_batch_lr_continuation_trials() -> list[TrialConfig]:
    """Branch the practical b32 winner to test its next LR boundary."""
    source = next(
        trial
        for trial in kronecker_rank_batch_lr_boundary_trials()
        if trial.effective_batch == 32 and trial.lr == 6e-6
    )
    trials = [
        TrialConfig(
            source.architecture,
            lr=lr,
            seed=source.seed,
            steps=262_144 // source.effective_batch,
            stage="tensor_kron_rank_batch_lr_continue",
            audit_every=16_384 // source.effective_batch,
            compile_model=False,
            effective_batch=source.effective_batch,
            lr_parameterization=source.lr_parameterization,
            allow_divergence=True,
            checkpoint_every_examples=16_384,
            warm_start_stage=source.stage,
            warm_start_label=source.label,
            warm_start_step=source.steps,
            warm_start_lr_override=lr != source.lr,
            target_validation_kl=TARGET_VALIDATION_KL,
        )
        for lr in (6e-6, 1.2e-5)
    ]
    if len(trials) != 2 or len({trial.label for trial in trials}) != 2:
        raise RuntimeError("Kronecker continuation needs two unique LR cells")
    return trials


def kronecker_rank_depth_optimized_trials() -> list[TrialConfig]:
    """Retest the deeper parameter frontier at the optimized batch and LR."""
    architectures = [
        ArchitectureConfig(
            "kronecker",
            "residual_ffn",
            4,
            4,
            kronecker_rank=2407,
            kronecker_rank_chunk=32,
        ),
        ArchitectureConfig(
            "kronecker",
            "residual_ffn",
            8,
            4,
            kronecker_rank=1198,
            kronecker_rank_chunk=32,
        ),
        ArchitectureConfig(
            "kronecker",
            "residual_ffn",
            16,
            4,
            kronecker_rank=593,
            kronecker_rank_chunk=32,
        ),
        *kronecker_rank_architectures()[1:],
    ]
    trials = [
        TrialConfig(
            architecture,
            lr=6e-6,
            seed=0,
            steps=KRONECKER_PROBE_EXAMPLES // 32,
            stage="tensor_kron_rank_depth_optimized",
            audit_every=16_384 // 32,
            compile_model=False,
            effective_batch=32,
            lr_parameterization="mup",
            allow_divergence=True,
            checkpoint_every_examples=16_384,
        )
        for architecture in architectures
    ]
    if (
        len(trials) != 5
        or {trial.architecture.depth for trial in trials}
        != {4, 8, 16, 64, 128}
        or len({trial.label for trial in trials}) != 5
    ):
        raise RuntimeError("optimized Kronecker depth frontier needs five cells")
    return trials


def kronecker_rank_chunk_benchmark_trials() -> list[TrialConfig]:
    """Benchmark fewer, larger rank contractions without changing capacity."""
    architecture = kronecker_rank_architectures()[0]
    trials = [
        TrialConfig(
            replace(architecture, kronecker_rank_chunk=chunk),
            lr=6e-6,
            seed=0,
            steps=7,
            stage="tensor_kron_rank_chunk_benchmark",
            audit_every=7,
            compile_model=False,
            effective_batch=32,
            lr_parameterization="mup",
        )
        for chunk in (
            16,
            32,
            48,
            64,
            128,
            architecture.kronecker_rank,
        )
    ]
    if len(trials) != 6:
        raise RuntimeError("Kronecker chunk benchmark needs six cells")
    return trials


def kronecker_rank_compile_benchmark_trial() -> TrialConfig:
    """Benchmark compilation on the unchanged production Kronecker cell."""
    return TrialConfig(
        kronecker_rank_architectures()[0],
        lr=6e-6,
        seed=0,
        steps=7,
        stage="tensor_kron_rank_compile_benchmark",
        audit_every=7,
        compile_model=True,
        effective_batch=32,
        lr_parameterization="mup",
    )


def kronecker_rank_continuation_trial(
    warm_start_step: int = 96,
) -> TrialConfig:
    """Continue the stable d32/r291 leader with its exact AdamW state."""
    if warm_start_step not in (32, 64, 96):
        raise ValueError("rank continuation needs a committed probe boundary")
    architecture = kronecker_rank_architectures()[0]
    source = TrialConfig(
        architecture,
        lr=3e-6,
        seed=0,
        steps=KRONECKER_PROBE_EXAMPLES // 512,
        stage="tensor_kron_rank_early",
        audit_every=16_384 // 512,
        compile_model=False,
        effective_batch=512,
        lr_parameterization="mup",
        allow_divergence=True,
        checkpoint_every_examples=16_384,
    )
    return TrialConfig(
        architecture,
        lr=source.lr,
        seed=source.seed,
        steps=262_144 // source.effective_batch,
        stage="tensor_kron_rank_continue",
        audit_every=65_536 // source.effective_batch,
        compile_model=False,
        effective_batch=source.effective_batch,
        lr_parameterization=source.lr_parameterization,
        checkpoint_every_examples=65_536,
        warm_start_stage="tensor_kron_rank_checkpoint",
        warm_start_label=source.label,
        warm_start_step=warm_start_step,
        target_validation_kl=TARGET_VALIDATION_KL,
    )


def kronecker_rank_monarch_long_trial() -> TrialConfig:
    """Give the winning matched Monarch control the same long data budget."""
    return TrialConfig(
        ArchitectureConfig(
            "monarch",
            "residual_ffn",
            4,
            4,
        ),
        lr=1e-3,
        seed=0,
        steps=262_144 // 512,
        stage="tensor_kron_rank_monarch_long",
        audit_every=65_536 // 512,
        compile_model=False,
        effective_batch=512,
        lr_parameterization="uniform",
        # The short control did not retain AdamW state. Re-running its first
        # 128 cheap steps also gives an exact deterministic curve comparison.
        checkpoint_every_examples=262_144,
        target_validation_kl=TARGET_VALIDATION_KL,
    )


def kronecker_rank_monarch_continuation_trial(
    warm_start_step: int = 512,
) -> TrialConfig:
    """Continue the matched Monarch winner toward the <=1 KL target."""
    if warm_start_step != 512:
        raise ValueError("Monarch continuation needs the 262k checkpoint")
    source = kronecker_rank_monarch_long_trial()
    return TrialConfig(
        source.architecture,
        lr=source.lr,
        seed=source.seed,
        steps=2_097_152 // source.effective_batch,
        stage="tensor_kron_rank_monarch_continue",
        audit_every=262_144 // source.effective_batch,
        compile_model=False,
        effective_batch=source.effective_batch,
        lr_parameterization=source.lr_parameterization,
        checkpoint_every_examples=1_048_576,
        warm_start_stage=source.stage,
        warm_start_label=source.label,
        warm_start_step=warm_start_step,
        target_validation_kl=TARGET_VALIDATION_KL,
    )


def kronecker_rank_monarch_capacity_trials() -> list[TrialConfig]:
    """Compare ~2x Monarch parameters allocated to depth versus rank."""
    architectures = (
        ArchitectureConfig(
            "monarch",
            "residual_ffn",
            8,
            4,
            monarch_rank=1,
        ),
        ArchitectureConfig(
            "monarch",
            "residual_ffn",
            4,
            4,
            monarch_rank=2,
        ),
    )
    trials = [
        TrialConfig(
            architecture,
            lr=1e-3,
            seed=0,
            steps=KRONECKER_PROBE_EXAMPLES // 512,
            stage="tensor_kron_rank_monarch_capacity",
            audit_every=16_384 // 512,
            compile_model=False,
            effective_batch=512,
            lr_parameterization="uniform",
            allow_divergence=True,
            checkpoint_every_examples=65_536,
        )
        for architecture in architectures
    ]
    if len(trials) != 2 or len({trial.label for trial in trials}) != 2:
        raise RuntimeError("Monarch capacity screen needs two unique cells")
    return trials


def kronecker_rank_monarch_scaled_depth_trial() -> TrialConfig:
    """Control depth-8 activation growth with a fixed residual scale."""
    return TrialConfig(
        ArchitectureConfig(
            "monarch",
            "residual_ffn",
            8,
            4,
            monarch_rank=1,
            residual_scale="inverse_sqrt_depth",
        ),
        lr=1e-3,
        seed=0,
        steps=KRONECKER_PROBE_EXAMPLES // 512,
        stage="tensor_kron_rank_monarch_capacity",
        audit_every=16_384 // 512,
        compile_model=False,
        effective_batch=512,
        lr_parameterization="uniform",
        allow_divergence=True,
        checkpoint_every_examples=65_536,
    )


def kronecker_rank_monarch_capacity_checkpoint_trial() -> TrialConfig:
    """Reproduce rank-2 while retaining an exact final optimizer state."""
    source = next(
        trial
        for trial in kronecker_rank_monarch_capacity_trials()
        if trial.architecture.monarch_rank == 2
    )
    return replace(
        source,
        stage="tensor_kron_rank_monarch_capacity_checkpoint",
    )


def kronecker_rank_monarch_capacity_continuation_trial() -> TrialConfig:
    """Continue the better 2x-capacity allocation with exact AdamW state."""
    source = kronecker_rank_monarch_capacity_checkpoint_trial()
    return TrialConfig(
        source.architecture,
        lr=source.lr,
        seed=source.seed,
        steps=262_144 // source.effective_batch,
        stage="tensor_kron_rank_monarch_capacity_continue",
        audit_every=65_536 // source.effective_batch,
        compile_model=False,
        effective_batch=source.effective_batch,
        lr_parameterization=source.lr_parameterization,
        checkpoint_every_examples=262_144,
        warm_start_stage=source.stage,
        warm_start_label=source.label,
        warm_start_step=source.steps,
        target_validation_kl=TARGET_VALIDATION_KL,
    )


def kronecker_rank_monarch_batch_trials() -> list[TrialConfig]:
    """Test whether Monarch is update-limited at fixed examples."""
    settings = (
        (1, 256, 1e-3),
        (1, 256, 5e-4),
        (2, 256, 1e-3),
        (1, 128, 1e-3),
        (1, 128, 5e-4),
        (1, 64, 1e-3),
        (1, 64, 5e-4),
        (1, 32, 1e-3),
        (2, 32, 1e-3),
    )
    trials = [
        TrialConfig(
            ArchitectureConfig(
                "monarch",
                "residual_ffn",
                4,
                4,
                monarch_rank=rank,
            ),
            lr=lr,
            seed=0,
            steps=KRONECKER_PROBE_EXAMPLES // batch,
            stage="tensor_kron_rank_monarch_batch",
            audit_every=16_384 // batch,
            compile_model=False,
            effective_batch=batch,
            lr_parameterization="uniform",
            allow_divergence=True,
            max_activation_rms_growth=(
                50.0 if rank == 2 and batch == 32 else 10.0
            ),
            checkpoint_every_examples=65_536,
        )
        for rank, batch, lr in settings
    ]
    if len(trials) != 9 or len({trial.label for trial in trials}) != 9:
        raise RuntimeError("Monarch batch screen needs nine unique cells")
    return trials


def kronecker_rank_monarch_batch_frontier_trials() -> list[TrialConfig]:
    """Resolve the small-batch frontier without the tiny-baseline false stop."""
    settings = (
        (32, 1e-3),
        (16, 1e-3),
        (16, 5e-4),
    )
    trials = [
        TrialConfig(
            ArchitectureConfig(
                "monarch",
                "residual_ffn",
                4,
                4,
                monarch_rank=1,
            ),
            lr=lr,
            seed=0,
            steps=KRONECKER_PROBE_EXAMPLES // batch,
            stage="tensor_kron_rank_monarch_batch_frontier",
            audit_every=16_384 // batch,
            compile_model=False,
            effective_batch=batch,
            lr_parameterization="uniform",
            allow_divergence=True,
            # The earlier batch-32 run stayed finite and reached activation
            # RMS 1.39, but exceeded the relative guard because its step-one
            # baseline was only 0.117. Keep the diagnostic while permitting
            # normal learning away from that unusually small baseline.
            max_activation_rms_growth=50.0,
            checkpoint_every_examples=65_536,
        )
        for batch, lr in settings
    ]
    if len(trials) != 3 or len({trial.label for trial in trials}) != 3:
        raise RuntimeError("Monarch batch frontier needs three unique cells")
    return trials


def kronecker_rank_monarch_batch_continuation_trials() -> list[TrialConfig]:
    """Continue the batch-32 frontier winner and test its first LR decay."""
    source = next(
        trial
        for trial in kronecker_rank_monarch_batch_frontier_trials()
        if trial.effective_batch == 32 and trial.lr == 1e-3
    )
    trials = [
        TrialConfig(
            source.architecture,
            lr=lr,
            seed=source.seed,
            steps=262_144 // source.effective_batch,
            stage="tensor_kron_rank_monarch_batch_continue",
            audit_every=65_536 // source.effective_batch,
            compile_model=False,
            effective_batch=source.effective_batch,
            lr_parameterization=source.lr_parameterization,
            allow_divergence=True,
            checkpoint_every_examples=65_536,
            warm_start_stage=source.stage,
            warm_start_label=source.label,
            warm_start_step=source.steps,
            warm_start_lr_override=lr != source.lr,
            target_validation_kl=TARGET_VALIDATION_KL,
        )
        for lr in (1e-3, 5e-4)
    ]
    if len(trials) != 2 or len({trial.label for trial in trials}) != 2:
        raise RuntimeError(
            "Monarch small-batch continuation needs two unique LR cells"
        )
    return trials


def kronecker_rank_monarch_batch_long_trials() -> list[TrialConfig]:
    """Extend the optimized batch-32 checkpoint to one million examples."""
    source = next(
        trial
        for trial in kronecker_rank_monarch_batch_continuation_trials()
        if trial.lr == 5e-4
    )
    trials = [
        TrialConfig(
            source.architecture,
            lr=lr,
            seed=source.seed,
            steps=1_048_576 // source.effective_batch,
            stage="tensor_kron_rank_monarch_batch_long",
            audit_every=131_072 // source.effective_batch,
            compile_model=False,
            effective_batch=source.effective_batch,
            lr_parameterization=source.lr_parameterization,
            allow_divergence=True,
            checkpoint_every_examples=262_144,
            warm_start_stage=source.stage,
            warm_start_label=source.label,
            warm_start_step=source.steps,
            warm_start_lr_override=lr != source.lr,
            target_validation_kl=TARGET_VALIDATION_KL,
        )
        for lr in (5e-4, 2.5e-4)
    ]
    if len(trials) != 2 or len({trial.label for trial in trials}) != 2:
        raise RuntimeError(
            "Monarch optimized long run needs two unique LR cells"
        )
    return trials


def kronecker_rank_monarch_batch_million_trials() -> list[TrialConfig]:
    """Extend optimized batch 32 from one to two million examples."""
    source = next(
        trial
        for trial in kronecker_rank_monarch_batch_long_trials()
        if trial.lr == 2.5e-4
    )
    trials = [
        TrialConfig(
            source.architecture,
            lr=lr,
            seed=source.seed,
            steps=2_097_152 // source.effective_batch,
            stage="tensor_kron_rank_monarch_batch_million",
            audit_every=262_144 // source.effective_batch,
            compile_model=False,
            effective_batch=source.effective_batch,
            lr_parameterization=source.lr_parameterization,
            allow_divergence=True,
            checkpoint_every_examples=524_288,
            warm_start_stage=source.stage,
            warm_start_label=source.label,
            warm_start_step=source.steps,
            warm_start_lr_override=lr != source.lr,
            target_validation_kl=TARGET_VALIDATION_KL,
        )
        for lr in (2.5e-4, 1.25e-4)
    ]
    if len(trials) != 2 or len({trial.label for trial in trials}) != 2:
        raise RuntimeError(
            "Monarch optimized million continuation needs two LR cells"
        )
    return trials


def kronecker_rank_monarch_batch_two_million_trials() -> list[TrialConfig]:
    """Extend optimized batch 32 from two to three million examples."""
    source = next(
        trial
        for trial in kronecker_rank_monarch_batch_million_trials()
        if trial.lr == 1.25e-4
    )
    trials = [
        TrialConfig(
            source.architecture,
            lr=lr,
            seed=source.seed,
            steps=3_145_728 // source.effective_batch,
            stage="tensor_kron_rank_monarch_batch_two_million",
            audit_every=262_144 // source.effective_batch,
            compile_model=False,
            effective_batch=source.effective_batch,
            lr_parameterization=source.lr_parameterization,
            allow_divergence=True,
            checkpoint_every_examples=262_144,
            warm_start_stage=source.stage,
            warm_start_label=source.label,
            warm_start_step=source.steps,
            warm_start_lr_override=lr != source.lr,
            target_validation_kl=TARGET_VALIDATION_KL,
        )
        for lr in (1.25e-4, 6.25e-5)
    ]
    if len(trials) != 2 or len({trial.label for trial in trials}) != 2:
        raise RuntimeError(
            "Monarch optimized two-million continuation needs two LR cells"
        )
    return trials


def kronecker_rank_resume_after_spend_trials() -> list[TrialConfig]:
    """Recreate the exact eight-cell slate interrupted by the spend limit."""
    mature_small_batch = [
        trial
        for trial in kronecker_rank_monarch_batch_transition_lr_trials()
        if trial.effective_batch in (64, 32)
    ]
    kronecker_continue = [
        trial
        for trial in kronecker_rank_batch_lr_continuation_trials()
        if trial.lr == 6e-6
    ]
    shallow_frontier = [
        trial
        for trial in kronecker_rank_depth_optimized_trials()
        if trial.architecture.depth in (4, 8, 16)
    ]
    trials = [
        *kronecker_rank_monarch_batch_two_million_trials(),
        *mature_small_batch,
        *kronecker_continue,
        *shallow_frontier,
    ]
    if len(trials) != 8 or len({trial.label for trial in trials}) != 8:
        raise RuntimeError("spend-limit restart slate needs eight unique cells")
    return trials


def kronecker_rank_depth_winner_continuation_trials() -> list[TrialConfig]:
    """Continue the shallow Kronecker frontier and branch the d4 LR."""
    sources = {
        trial.architecture.depth: trial
        for trial in kronecker_rank_depth_optimized_trials()
        if trial.architecture.depth in (4, 8)
    }
    settings = (
        (4, 6e-6),
        (4, 3e-6),
        (8, 6e-6),
    )
    trials = [
        TrialConfig(
            sources[depth].architecture,
            lr=lr,
            seed=sources[depth].seed,
            steps=131_072 // sources[depth].effective_batch,
            stage="tensor_kron_rank_depth_winner_continue",
            audit_every=16_384 // sources[depth].effective_batch,
            compile_model=False,
            effective_batch=sources[depth].effective_batch,
            lr_parameterization=sources[depth].lr_parameterization,
            allow_divergence=True,
            checkpoint_every_examples=16_384,
            warm_start_stage=sources[depth].stage,
            warm_start_label=sources[depth].label,
            warm_start_step=sources[depth].steps,
            warm_start_lr_override=lr != sources[depth].lr,
            target_validation_kl=TARGET_VALIDATION_KL,
        )
        for depth, lr in settings
    ]
    if len(trials) != 3 or len({trial.label for trial in trials}) != 3:
        raise RuntimeError(
            "shallow Kronecker continuation needs three unique cells"
        )
    return trials


def kronecker_edu_lr_trials() -> list[TrialConfig]:
    """Branch the best d4/r2407 state across a guarded FineWeb-Edu LR sweep."""
    source = next(
        trial
        for trial in kronecker_rank_depth_winner_continuation_trials()
        if trial.architecture.depth == 4 and trial.lr == 3e-6
    )
    additional_steps = KRONECKER_EDU_PILOT_EXAMPLES // source.effective_batch
    trials = [
        TrialConfig(
            source.architecture,
            lr=lr,
            seed=source.seed,
            steps=source.steps + additional_steps,
            stage="tensor_kron_edu_lr",
            audit_every=32_768 // source.effective_batch,
            compile_model=False,
            effective_batch=source.effective_batch,
            lr_parameterization=source.lr_parameterization,
            allow_divergence=True,
            max_activation_rms_growth=10.0,
            checkpoint_every_examples=65_536,
            warm_start_stage=source.stage,
            warm_start_label=source.label,
            warm_start_step=source.steps,
            warm_start_lr_override=lr != source.lr,
            target_validation_kl=TARGET_VALIDATION_KL,
            lr_schedule="warmup_cosine",
            warmup_steps=256,
            min_lr_ratio=0.1,
            gradient_clip_norm=1.0,
            dataset_tag="fwedu350bt-v1p4",
        )
        for lr in KRONECKER_EDU_LR_GRID
    ]
    if len(trials) != 8 or len({trial.label for trial in trials}) != 8:
        raise RuntimeError("FineWeb-Edu LR sweep needs eight unique cells")
    return trials


def kronecker_edu_long_trial(lr: float) -> TrialConfig:
    """Continue one validated FineWeb-Edu pilot to a 4.2M-example boundary."""
    try:
        source = next(
            trial for trial in kronecker_edu_lr_trials() if trial.lr == lr
        )
    except StopIteration as error:
        raise ValueError(
            f"long FineWeb-Edu LR must be one of {KRONECKER_EDU_LR_GRID}"
        ) from error
    steps = KRONECKER_EDU_LONG_TOTAL_EXAMPLES // source.effective_batch
    return TrialConfig(
        source.architecture,
        lr=source.lr,
        seed=source.seed,
        steps=steps,
        stage="tensor_kron_edu_long",
        audit_every=65_536 // source.effective_batch,
        compile_model=False,
        effective_batch=source.effective_batch,
        lr_parameterization=source.lr_parameterization,
        allow_divergence=True,
        max_activation_rms_growth=10.0,
        checkpoint_every_examples=65_536,
        warm_start_stage=source.stage,
        warm_start_label=source.label,
        warm_start_step=source.steps,
        target_validation_kl=TARGET_VALIDATION_KL,
        lr_schedule="warmup_cosine",
        warmup_steps=512,
        min_lr_ratio=0.03,
        gradient_clip_norm=1.0,
        dataset_tag=source.dataset_tag,
    )


def select_kronecker_edu_lr_winner(results: list[dict]) -> TrialConfig:
    """Select a completed LR pilot using only its final full-validation KL."""
    trials_by_label = {
        trial.label: trial for trial in kronecker_edu_lr_trials()
    }
    candidates: list[tuple[float, str, TrialConfig]] = []
    for result in results:
        label = str(result.get("label", ""))
        trial = trials_by_label.get(label)
        if trial is None or result.get("status") != "complete":
            continue
        if int(result.get("steps_completed", -1)) != trial.steps:
            continue
        validation = result.get("validation")
        if not isinstance(validation, dict):
            continue
        kl = float(validation.get("kl", math.nan))
        if not math.isfinite(kl):
            continue
        candidates.append((kl, label, trial))
    if not candidates:
        raise RuntimeError(
            "FineWeb-Edu LR selection needs a completed full-validation result"
        )
    return min(candidates, key=lambda item: (item[0], item[1]))[2]


def select_kronecker_edu_stage_winner(
    trials: list[TrialConfig],
    results: list[dict],
) -> TrialConfig:
    """Select one exact, completed stage cell by final validation KL only."""
    return select_kronecker_edu_stage_top_k(trials, results, 1)[0]


def select_kronecker_edu_stage_top_k(
    trials: list[TrialConfig],
    results: list[dict],
    keep: int,
) -> list[TrialConfig]:
    """Rank exact completed cells using final full-validation KL only."""
    if keep <= 0:
        raise ValueError("stage selection keep count must be positive")
    trials_by_label = {trial.label: trial for trial in trials}
    if len(trials_by_label) != len(trials):
        raise ValueError("stage selection requires unique trial labels")
    candidates: list[tuple[float, str, TrialConfig]] = []
    for result in results:
        label = str(result.get("label", ""))
        trial = trials_by_label.get(label)
        if trial is None or result.get("status") != "complete":
            continue
        if int(result.get("steps_completed", -1)) != trial.steps:
            continue
        validation = result.get("validation")
        if not isinstance(validation, dict):
            continue
        kl = float(validation.get("kl", math.nan))
        if math.isfinite(kl):
            candidates.append((kl, label, trial))
    if not candidates:
        raise RuntimeError(
            "stage selection needs a completed full-validation result"
        )
    if len(candidates) < keep:
        raise RuntimeError(
            f"stage selection needs {keep} completed full-validation "
            f"results, found {len(candidates)}"
        )
    return [
        trial
        for _, _, trial in sorted(
            candidates,
            key=lambda item: (item[0], item[1]),
        )[:keep]
    ]


def _kronecker_edu_continuation(
    source: TrialConfig,
    *,
    stage: str,
    additional_examples: int,
    **changes,
) -> TrialConfig:
    if source.dataset_tag != "fwedu350bt-v1p4":
        raise ValueError("FineWeb-Edu continuation requires the pinned dataset tag")
    if source.effective_batch != 32:
        raise ValueError("FineWeb-Edu continuation requires effective batch 32")
    if additional_examples <= 0 or additional_examples % source.effective_batch:
        raise ValueError(
            "additional examples must be positive and divide by effective batch"
        )
    return replace(
        source,
        steps=source.steps + additional_examples // source.effective_batch,
        stage=stage,
        audit_every=32_768 // source.effective_batch,
        compile_model=False,
        allow_divergence=True,
        checkpoint_every_examples=65_536,
        allow_data_reuse=False,
        warm_start_stage=source.stage,
        warm_start_label=source.label,
        warm_start_step=source.steps,
        warm_start_resume_step=0,
        target_validation_kl=TARGET_VALIDATION_KL,
        lr_schedule="warmup_hold",
        warmup_steps=0,
        warmup_examples=KRONECKER_EDU_WARMUP_EXAMPLES,
        min_lr_ratio=0.1,
        use_teacher_cache=True,
        **changes,
    )


def kronecker_edu_optimizer_trials(
    source: TrialConfig,
) -> list[TrialConfig]:
    """Branch one LR winner across eight isolated AdamW role policies."""
    all_roles_no_decay = tuple(
        (role, 0.0)
        for role in (
            "kronecker_factor",
            "kronecker_gain",
            "kronecker_mixing",
            "standard",
            "bias",
        )
    )
    gain_33_mix_3 = (
        ("kronecker_gain", 33.333),
        ("kronecker_mixing", 3.333),
    )
    gain_mix_standard_bias = (
        *gain_33_mix_3,
        ("standard", 33.333),
        ("bias", 33.333),
    )
    policies = (
        ((), (), source.gradient_clip_norm),
        ((), all_roles_no_decay, source.gradient_clip_norm),
        ((("kronecker_gain", 10.0),), (), source.gradient_clip_norm),
        ((("kronecker_gain", 33.333),), (), source.gradient_clip_norm),
        ((("kronecker_gain", 100.0),), (), source.gradient_clip_norm),
        (gain_33_mix_3, (), source.gradient_clip_norm),
        (gain_mix_standard_bias, (), source.gradient_clip_norm),
        (gain_mix_standard_bias, (), 5.0),
    )
    trials = [
        _kronecker_edu_continuation(
            source,
            stage="tensor_kron_edu_optimizer",
            additional_examples=KRONECKER_EDU_POLICY_EXAMPLES,
            optimizer_role_lr_multipliers=role_lrs,
            optimizer_role_weight_decays=role_decays,
            gradient_clip_norm=clip,
            # AdamW moments are retained, while the target role LR/WD policy
            # is explicitly restored after loading source optimizer groups.
            warm_start_lr_override=True,
        )
        for role_lrs, role_decays, clip in policies
    ]
    if len(trials) != 8 or len({trial.label for trial in trials}) != 8:
        raise RuntimeError("FineWeb-Edu optimizer screen needs eight unique cells")
    return trials


def kronecker_edu_objective_trials(
    source: TrialConfig,
) -> list[TrialConfig]:
    """Branch one source across hidden-state and softened-KL objectives."""
    settings = (
        (1.0, 0.0, 0.0),
        (1.0, 0.0, 0.03),
        (1.0, 0.0, 0.1),
        (1.0, 0.0, 0.3),
        (1.0, 0.0, 1.0),
        (1.0, 0.0, 3.0),
        (2.0, 0.25, 0.0),
        (2.0, 0.25, 0.3),
    )
    trials = [
        _kronecker_edu_continuation(
            source,
            stage="tensor_kron_edu_objective",
            additional_examples=KRONECKER_EDU_OBJECTIVE_EXAMPLES,
            temperature=temperature,
            temperature2_weight=temperature2_weight,
            hidden_mse_weight=hidden_mse_weight,
        )
        for temperature, temperature2_weight, hidden_mse_weight in settings
    ]
    if len(trials) != 8 or len({trial.label for trial in trials}) != 8:
        raise RuntimeError("FineWeb-Edu objective screen needs eight unique cells")
    return trials


def kronecker_edu_architectures() -> list[ArchitectureConfig]:
    """Return the exact approximately 84M-parameter architecture screen."""
    order4 = {
        "kronecker_factors": 4,
        "kronecker_layout": "context_channel_order4_v1",
        "kronecker_rank_chunk": 32,
    }
    architectures = [
        ArchitectureConfig(
            "kronecker",
            "residual_ffn",
            4,
            4,
            kronecker_rank=2407,
            kronecker_rank_chunk=32,
        ),
        ArchitectureConfig(
            "kronecker",
            "residual_ffn",
            4,
            4,
            kronecker_rank=2407,
            kronecker_rank_chunk=32,
            repetitions=2,
            gated_repetitions=True,
        ),
        ArchitectureConfig(
            "kronecker",
            "residual_gated",
            4,
            kronecker_rank=3033,
            kronecker_rank_chunk=32,
        ),
        ArchitectureConfig(
            "kronecker",
            "residual_ffn",
            8,
            4,
            kronecker_rank=6750,
            **order4,
        ),
        ArchitectureConfig(
            "kronecker",
            "residual_ffn",
            16,
            4,
            kronecker_rank=3343,
            **order4,
        ),
        ArchitectureConfig(
            "kronecker",
            "residual_ffn",
            32,
            4,
            kronecker_rank=1640,
            **order4,
        ),
        ArchitectureConfig(
            "kronecker",
            "residual_gated",
            16,
            kronecker_rank=2688,
            **order4,
        ),
        ArchitectureConfig(
            "hybrid",
            "residual_ffn",
            10,
            4,
            kronecker_rank=3343,
            **order4,
        ),
    ]
    for architecture in architectures:
        architecture.validate()
    if len(architectures) != 8 or len({
        architecture.label for architecture in architectures
    }) != 8:
        raise RuntimeError(
            "FineWeb-Edu architecture screen needs eight unique cells"
        )
    return architectures


def kronecker_edu_architecture_trials(
    source: TrialConfig,
) -> list[TrialConfig]:
    """Train the exact architecture screen from scratch on matched data."""
    if source.dataset_tag != "fwedu350bt-v1p4":
        raise ValueError("architecture screen requires the pinned dataset tag")
    trials = [
        TrialConfig(
            architecture,
            lr=source.lr,
            seed=source.seed,
            steps=(
                KRONECKER_EDU_ARCHITECTURE_SCREEN_EXAMPLES
                // source.effective_batch
            ),
            stage="tensor_kron_edu_architecture",
            audit_every=16_384 // source.effective_batch,
            compile_model=False,
            effective_batch=source.effective_batch,
            lr_parameterization=source.lr_parameterization,
            allow_divergence=True,
            max_activation_rms_growth=source.max_activation_rms_growth,
            checkpoint_every_examples=65_536,
            target_validation_kl=TARGET_VALIDATION_KL,
            lr_schedule="warmup_hold",
            warmup_examples=KRONECKER_EDU_WARMUP_EXAMPLES,
            min_lr_ratio=0.1,
            gradient_clip_norm=source.gradient_clip_norm,
            dataset_tag=source.dataset_tag,
            optimizer_role_lr_multipliers=(
                source.optimizer_role_lr_multipliers
            ),
            optimizer_role_weight_decays=(
                source.optimizer_role_weight_decays
            ),
            temperature=source.temperature,
            temperature2_weight=source.temperature2_weight,
            hidden_mse_weight=source.hidden_mse_weight,
            use_teacher_cache=True,
        )
        for architecture in kronecker_edu_architectures()
    ]
    if len(trials) != 8 or len({trial.label for trial in trials}) != 8:
        raise RuntimeError(
            "FineWeb-Edu architecture trials need eight unique cells"
        )
    return trials


def kronecker_edu_scale_trials(
    source_trials: list[TrialConfig],
    source_results: list[dict],
    *,
    target_examples: int,
    keep: int,
) -> list[TrialConfig]:
    """Successively halve the architecture frontier at fixed data boundaries."""
    if (target_examples, keep) not in KRONECKER_EDU_SCALE_BOUNDARIES:
        raise ValueError("unsupported FineWeb-Edu scale boundary")
    selected = select_kronecker_edu_stage_top_k(
        source_trials,
        source_results,
        keep,
    )
    trials = []
    for source in selected:
        source_examples = source.steps * source.effective_batch
        if target_examples <= source_examples:
            raise ValueError("scale boundary must extend its source trial")
        trial = replace(
            source,
            steps=target_examples // source.effective_batch,
            stage=f"tensor_kron_edu_scale_{target_examples}",
            audit_every=65_536 // source.effective_batch,
            checkpoint_every_examples=65_536,
            warm_start_stage=source.stage,
            warm_start_label=source.label,
            warm_start_step=source.steps,
            warm_start_resume_step=0,
            lr_schedule="warmup_hold",
            warmup_steps=0,
            warmup_examples=KRONECKER_EDU_WARMUP_EXAMPLES,
            min_lr_ratio=0.1,
            use_teacher_cache=True,
        )
        trials.append(trial)
    if len(trials) != keep or len({trial.label for trial in trials}) != keep:
        raise RuntimeError("FineWeb-Edu scale boundary labels are not unique")
    return trials


def kronecker_rank_monarch_mature_continuation_trials() -> list[TrialConfig]:
    """Continue the mature b128 winner through its next LR boundary."""
    source = next(
        trial
        for trial in kronecker_rank_monarch_batch_transition_lr_trials()
        if trial.effective_batch == 128 and trial.lr == 3.75e-5
    )
    target_examples = 6_291_456
    trials = [
        TrialConfig(
            source.architecture,
            lr=lr,
            seed=source.seed,
            steps=target_examples // source.effective_batch,
            stage="tensor_kron_rank_monarch_mature_continue",
            audit_every=262_144 // source.effective_batch,
            compile_model=False,
            effective_batch=source.effective_batch,
            lr_parameterization=source.lr_parameterization,
            checkpoint_every_examples=262_144,
            allow_data_reuse=True,
            warm_start_stage=source.stage,
            warm_start_label=source.label,
            warm_start_step=source.steps,
            warm_start_lr_override=lr != source.lr,
            target_validation_kl=TARGET_VALIDATION_KL,
        )
        for lr in (3.75e-5, 1.875e-5)
    ]
    if len(trials) != 2 or len({trial.label for trial in trials}) != 2:
        raise RuntimeError(
            "mature Monarch continuation needs two unique LR cells"
        )
    return trials


def kronecker_rank_monarch_batch_three_million_trials() -> list[TrialConfig]:
    """Continue optimized b32 from three to four million examples."""
    source = next(
        trial
        for trial in kronecker_rank_monarch_batch_two_million_trials()
        if trial.lr == 6.25e-5
    )
    trials = [
        TrialConfig(
            source.architecture,
            lr=lr,
            seed=source.seed,
            steps=4_194_304 // source.effective_batch,
            stage="tensor_kron_rank_monarch_batch_three_million",
            audit_every=262_144 // source.effective_batch,
            compile_model=False,
            effective_batch=source.effective_batch,
            lr_parameterization=source.lr_parameterization,
            allow_divergence=True,
            checkpoint_every_examples=262_144,
            warm_start_stage=source.stage,
            warm_start_label=source.label,
            warm_start_step=source.steps,
            warm_start_lr_override=lr != source.lr,
            target_validation_kl=TARGET_VALIDATION_KL,
        )
        for lr in (6.25e-5, 3.125e-5)
    ]
    if len(trials) != 2 or len({trial.label for trial in trials}) != 2:
        raise RuntimeError(
            "optimized Monarch three-million continuation needs two LR cells"
        )
    return trials


def kronecker_rank_monarch_rank2_retry_trial() -> TrialConfig:
    """Retry the finite rank-2/b32 boundary under a non-spurious guard."""
    source = next(
        trial
        for trial in kronecker_rank_monarch_batch_trials()
        if (
            trial.architecture.monarch_rank == 2
            and trial.effective_batch == 32
        )
    )
    return replace(
        source,
        stage="tensor_kron_rank_monarch_batch_rank2_retry",
        max_activation_rms_growth=50.0,
    )


def kronecker_rank_next_eight_trials() -> list[TrialConfig]:
    """Allocate the next eight H100s to the measured winners."""
    trials = [
        *kronecker_rank_depth_winner_continuation_trials(),
        *kronecker_rank_monarch_mature_continuation_trials(),
        *kronecker_rank_monarch_batch_three_million_trials(),
        kronecker_rank_monarch_rank2_retry_trial(),
    ]
    if len(trials) != 8 or len({trial.label for trial in trials}) != 8:
        raise RuntimeError("next winner continuation slate needs eight cells")
    return trials


def kronecker_rank_monarch_depth_parameter_matched_trials() -> list[TrialConfig]:
    """Trade per-layer Monarch factors for depth at fixed factor count."""
    architecture = ArchitectureConfig(
        "monarch",
        "residual_ffn",
        8,
        4,
        monarch_blocks=256,
        monarch_rank=1,
    )
    trials = [
        TrialConfig(
            architecture,
            lr=lr,
            seed=0,
            steps=KRONECKER_PROBE_EXAMPLES // 32,
            stage="tensor_kron_rank_monarch_depth_parameter_matched",
            audit_every=16_384 // 32,
            compile_model=False,
            effective_batch=32,
            lr_parameterization="uniform",
            allow_divergence=True,
            max_activation_rms_growth=100.0,
            checkpoint_every_examples=65_536,
        )
        for lr in (1e-3, 5e-4)
    ]
    if len(trials) != 2 or len({trial.label for trial in trials}) != 2:
        raise RuntimeError(
            "parameter-matched Monarch depth frontier needs two LR cells"
        )
    return trials


def kronecker_rank_monarch_decay_trials() -> list[TrialConfig]:
    """Branch from 2M examples to test whether Monarch is LR-limited."""
    source = kronecker_rank_monarch_continuation_trial()
    trials = [
        TrialConfig(
            source.architecture,
            lr=lr,
            seed=source.seed,
            steps=3_145_728 // source.effective_batch,
            stage="tensor_kron_rank_monarch_decay",
            audit_every=262_144 // source.effective_batch,
            compile_model=False,
            effective_batch=source.effective_batch,
            lr_parameterization=source.lr_parameterization,
            checkpoint_every_examples=524_288,
            warm_start_stage=source.stage,
            warm_start_label=source.label,
            warm_start_step=source.steps,
            warm_start_lr_override=True,
            target_validation_kl=TARGET_VALIDATION_KL,
        )
        for lr in (5e-4, 3e-4)
    ]
    if len(trials) != 2 or len({trial.label for trial in trials}) != 2:
        raise RuntimeError("Monarch decay boundary needs two unique cells")
    return trials


def kronecker_rank_monarch_decay_continuation_trial(
    source_lr: float,
    target_lr: float,
) -> TrialConfig:
    """Continue an exact 3M decay checkpoint for another 1M examples."""
    sources = {
        trial.lr: trial for trial in kronecker_rank_monarch_decay_trials()
    }
    if source_lr not in sources:
        raise ValueError("source LR must be a completed Monarch decay cell")
    if target_lr <= 0:
        raise ValueError("target LR must be positive")
    source = sources[source_lr]
    return TrialConfig(
        source.architecture,
        lr=target_lr,
        seed=source.seed,
        steps=4_194_304 // source.effective_batch,
        stage="tensor_kron_rank_monarch_decay_continue",
        audit_every=262_144 // source.effective_batch,
        compile_model=False,
        effective_batch=source.effective_batch,
        lr_parameterization=source.lr_parameterization,
        checkpoint_every_examples=524_288,
        warm_start_stage=source.stage,
        warm_start_label=source.label,
        warm_start_step=source.steps,
        warm_start_lr_override=target_lr != source_lr,
        target_validation_kl=TARGET_VALIDATION_KL,
    )


def kronecker_rank_monarch_batch_transition_trials() -> list[TrialConfig]:
    """Branch the mature 4M winner across batch at fixed example cursor."""
    source = kronecker_rank_monarch_decay_continuation_trial(
        3e-4,
        1.5e-4,
    )
    target_examples = 5_242_880
    trials = [
        TrialConfig(
            source.architecture,
            lr=source.lr,
            seed=source.seed,
            steps=target_examples // batch,
            stage="tensor_kron_rank_monarch_batch_transition",
            audit_every=262_144 // batch,
            compile_model=False,
            effective_batch=batch,
            lr_parameterization=source.lr_parameterization,
            checkpoint_every_examples=524_288,
            allow_data_reuse=True,
            warm_start_stage=source.stage,
            warm_start_label=source.label,
            warm_start_step=source.steps,
            warm_start_resume_step=(
                source.steps * source.effective_batch // batch
            ),
            target_validation_kl=TARGET_VALIDATION_KL,
        )
        for batch in (512, 128)
    ]
    if len(trials) != 2 or len({trial.label for trial in trials}) != 2:
        raise RuntimeError(
            "Monarch mature-batch transition needs two unique cells"
        )
    return trials


def kronecker_rank_monarch_batch_transition_lr_trials() -> list[TrialConfig]:
    """Scale LR with mature small-batch transitions."""
    source = kronecker_rank_monarch_decay_continuation_trial(
        3e-4,
        1.5e-4,
    )
    target_examples = 5_242_880
    trials = [
        TrialConfig(
            source.architecture,
            lr=lr,
            seed=source.seed,
            steps=target_examples // batch,
            stage="tensor_kron_rank_monarch_batch_transition_lr",
            audit_every=262_144 // batch,
            compile_model=False,
            effective_batch=batch,
            lr_parameterization=source.lr_parameterization,
            checkpoint_every_examples=524_288,
            allow_data_reuse=True,
            warm_start_stage=source.stage,
            warm_start_label=source.label,
            warm_start_step=source.steps,
            warm_start_resume_step=(
                source.steps * source.effective_batch // batch
            ),
            warm_start_lr_override=True,
            target_validation_kl=TARGET_VALIDATION_KL,
        )
        for batch, lr in (
            (128, 7.5e-5),
            (128, 3.75e-5),
            # Continue the empirically successful linear batch/LR scaling:
            # 512/1.5e-4 -> 128/3.75e-5 -> 64/1.875e-5 ->
            # 32/9.375e-6.
            (64, 1.875e-5),
            (32, 9.375e-6),
        )
    ]
    if len(trials) != 4 or len({trial.label for trial in trials}) != 4:
        raise RuntimeError(
            "Monarch mature batch/LR transition needs four unique cells"
        )
    return trials


def kronecker_rank_monarch_lr_boundary_trials() -> list[TrialConfig]:
    """Test whether the matched Monarch control remains LR-limited."""
    architecture = ArchitectureConfig(
        "monarch",
        "residual_ffn",
        4,
        4,
    )
    trials = [
        TrialConfig(
            architecture,
            lr=lr,
            seed=0,
            steps=KRONECKER_PROBE_EXAMPLES // 512,
            stage="tensor_kron_rank_monarch_lr_boundary",
            audit_every=16_384 // 512,
            compile_model=False,
            effective_batch=512,
            lr_parameterization="uniform",
            allow_divergence=True,
        )
        for lr in (2e-3, 3e-3)
    ]
    if len(trials) != 2 or len({trial.label for trial in trials}) != 2:
        raise RuntimeError("Monarch LR boundary needs two unique cells")
    return trials


def kronecker_rank_monarch_loop_trials() -> list[TrialConfig]:
    """Parameter-matched tied-depth controls at the winning Monarch LR."""
    architectures = [
        ArchitectureConfig(
            "monarch",
            "residual_ffn",
            4,
            4,
            repetitions=repetitions,
            residual_scale="inverse_repetitions",
        )
        for repetitions in (2, 4)
    ]
    trials = [
        TrialConfig(
            architecture,
            lr=1e-3,
            seed=0,
            steps=KRONECKER_PROBE_EXAMPLES // 512,
            stage="tensor_kron_rank_monarch_loop",
            audit_every=16_384 // 512,
            compile_model=False,
            effective_batch=512,
            lr_parameterization="uniform",
            allow_divergence=True,
        )
        for architecture in architectures
    ]
    if len(trials) != 2 or len({trial.label for trial in trials}) != 2:
        raise RuntimeError("Monarch loop control needs two unique cells")
    return trials


def select_tensor_reference(optimization_results: list[dict]) -> dict:
    """Use the best fixed-example Monarch optimizer cell as BTT reference."""
    candidates = [
        result
        for result in optimization_results
        if result.get("status", "complete") == "complete"
        and int(result.get("steps", 0))
        * int(result.get("effective_batch", 1_024))
        == TENSOR_SCREEN_EXAMPLES
    ]
    if not candidates:
        raise RuntimeError("tensor study needs completed fixed-example depth_opt results")
    return min(
        candidates,
        key=lambda result: (
            float(result["validation"]["kl"]),
            int(result.get("effective_batch", 1_024)),
            float(result["lr"]),
            str(result["label"]),
        ),
    )


def tensor_lr_probe_trials(reference: dict) -> list[TrialConfig]:
    batch = int(reference.get("effective_batch", 1_024))
    architecture = btt_parameter_architectures()[1]
    trials = [
        TrialConfig(
            architecture,
            lr=lr,
            seed=0,
            steps=TENSOR_PROBE_EXAMPLES // batch,
            stage="tensor_lr_probe",
            audit_every=65_536 // batch,
            compile_model=True,
            effective_batch=batch,
            lr_parameterization="mup",
            allow_divergence=True,
        )
        for lr in TENSOR_LR_GRID
    ]
    trials.extend(
        TrialConfig(
            architecture,
            lr=lr,
            seed=0,
            steps=TENSOR_PROBE_EXAMPLES // batch,
            stage="tensor_lr_probe",
            audit_every=65_536 // batch,
            compile_model=True,
            effective_batch=batch,
            lr_parameterization="uniform",
            allow_divergence=True,
        )
        for lr in (3e-3, 6e-3)
    )
    if len(trials) != 10 or len({trial.label for trial in trials}) != 10:
        raise RuntimeError("tensor LR probe must contain ten unique trials")
    return trials


def tensor_gate_control_trial(reference: dict) -> TrialConfig:
    batch = int(reference.get("effective_batch", 1_024))
    return TrialConfig(
        ArchitectureConfig(**reference["architecture"]),
        lr=float(reference["lr"]),
        seed=0,
        steps=TENSOR_PROBE_EXAMPLES // batch,
        stage="tensor_gate_control",
        audit_every=65_536 // batch,
        effective_batch=batch,
        lr_parameterization=str(
            reference.get("lr_parameterization", "uniform")
        ),
    )


def tensor_gate_btt_trials(reference: dict) -> list[TrialConfig]:
    wanted = {
        (1e-5, "mup"),
        (3e-5, "mup"),
        (3e-3, "uniform"),
    }
    trials = [
        trial
        for trial in tensor_lr_probe_trials(reference)
        if (trial.lr, trial.lr_parameterization) in wanted
    ]
    if len(trials) != 3:
        raise RuntimeError("BTT gate must contain three LR cells")
    return trials


def evaluate_tensor_gate(control: dict, btt_results: list[dict]) -> dict:
    if control.get("status", "complete") != "complete":
        raise RuntimeError("tensor gate Monarch control did not complete")
    control_kl = float(control["validation"]["kl"])
    rows = []
    for result in btt_results:
        initial = float(result["initial_validation"]["kl"])
        final = float(result["validation"]["kl"])
        growth = float(
            result.get("diagnostic_summary", {}).get(
                "activation_rms_growth_max",
                float("inf"),
            )
        )
        passed = (
            result.get("status", "complete") == "complete"
            and final <= 0.5 * initial
            and final <= control_kl + 1.0
            and growth <= 10.0
            and result.get("embedding_sha256")
            == control.get("embedding_sha256")
        )
        rows.append({
            "label": result["label"],
            "initial_kl": initial,
            "validation_kl": final,
            "monarch_control_kl": control_kl,
            "activation_rms_growth_max": growth,
            "passed": passed,
        })
    passed = any(row["passed"] for row in rows)
    return {
        "status": "passed" if passed else "failed",
        "criteria": {
            "minimum_fractional_kl_reduction": 0.5,
            "maximum_kl_above_control": 1.0,
            "maximum_activation_rms_growth": 10.0,
            "embedding_hash_must_match": True,
        },
        "control": control,
        "btt": rows,
    }


def kronecker_lr_probe_trials(reference: dict) -> list[TrialConfig]:
    batch = int(reference.get("effective_batch", 1_024))
    architecture = kronecker_parameter_architecture()
    trials = [
        TrialConfig(
            architecture,
            lr=lr,
            seed=0,
            steps=KRONECKER_PROBE_EXAMPLES // batch,
            stage="tensor_kron_probe",
            audit_every=16_384 // batch,
            compile_model=False,
            effective_batch=batch,
            lr_parameterization="mup",
            allow_divergence=True,
            checkpoint_every_examples=16_384,
        )
        for lr in KRONECKER_LR_GRID
    ]
    trials.extend(
        TrialConfig(
            architecture,
            lr=lr,
            seed=0,
            steps=KRONECKER_PROBE_EXAMPLES // batch,
            stage="tensor_kron_probe",
            audit_every=16_384 // batch,
            compile_model=False,
            effective_batch=batch,
            lr_parameterization="uniform",
            allow_divergence=True,
            checkpoint_every_examples=16_384,
        )
        for lr in KRONECKER_UNIFORM_LR_GRID
    )
    if len(trials) != 8 or len({trial.label for trial in trials}) != 8:
        raise RuntimeError("Kronecker LR probe must contain eight cells")
    return trials


def next_kronecker_boundary_probe(
    reference: dict,
    probe_results: list[dict],
) -> TrialConfig | None:
    stable = [
        result
        for result in probe_results
        if result.get("status", "complete") == "complete"
    ]
    if not stable:
        return None
    best = min(
        stable,
        key=lambda result: (
            float(result["validation"]["kl"]),
            float(result["lr"]),
        ),
    )
    parameterization = str(
        best.get("lr_parameterization", "uniform")
    )
    same = [
        result
        for result in probe_results
        if str(result.get("lr_parameterization", "uniform"))
        == parameterization
    ]
    tested = sorted({float(result["lr"]) for result in same})
    best_lr = float(best["lr"])
    if best_lr == tested[-1]:
        next_lr = best_lr * 2
    elif best_lr == tested[0]:
        next_lr = best_lr / 2
    else:
        return None
    if any(
        float(result["lr"]) == next_lr
        and str(result.get("lr_parameterization", "uniform"))
        == parameterization
        for result in probe_results
    ):
        return None
    batch = int(reference.get("effective_batch", 1_024))
    return TrialConfig(
        kronecker_parameter_architecture(),
        lr=next_lr,
        seed=0,
        steps=KRONECKER_PROBE_EXAMPLES // batch,
        stage="tensor_kron_probe",
        audit_every=16_384 // batch,
        compile_model=False,
        effective_batch=batch,
        lr_parameterization=parameterization,
        allow_divergence=True,
        checkpoint_every_examples=16_384,
    )


def select_kronecker_lr_settings(
    probe_results: list[dict],
) -> list[tuple[float, str]]:
    stable = [
        result
        for result in probe_results
        if result.get("status", "complete") == "complete"
    ]
    ordered = sorted(
        stable,
        key=lambda result: (
            float(result["validation"]["kl"]),
            float(result["lr"]),
            str(result.get("lr_parameterization", "uniform")),
        ),
    )
    settings = []
    for result in ordered:
        value = (
            float(result["lr"]),
            str(result.get("lr_parameterization", "uniform")),
        )
        if value not in settings:
            settings.append(value)
        if len(settings) == 2:
            return settings
    raise RuntimeError(
        "Kronecker LR probe produced fewer than two stable settings"
    )


def kronecker_depth_trials(
    reference: dict,
    probe_results: list[dict],
) -> list[TrialConfig]:
    batch = int(reference.get("effective_batch", 1_024))
    lr, parameterization = select_kronecker_lr_settings(probe_results)[0]
    trials = [
        TrialConfig(
            architecture,
            lr=lr,
            seed=0,
            steps=TENSOR_PROBE_EXAMPLES // batch,
            stage="tensor_kron_depth",
            audit_every=65_536 // batch,
            # Compiling hundreds of distinct untied layers creates an
            # unnecessarily enormous Inductor graph. The d1 H100 smoke
            # separately proves compile compatibility; depth evidence uses
            # eager contractions so startup remains bounded.
            compile_model=False,
            effective_batch=batch,
            lr_parameterization=parameterization,
            checkpoint_every_examples=65_536,
        )
        for architecture in kronecker_rank_architectures()
    ]
    return trials


def kronecker_parameter_trials(
    reference: dict,
    probe_results: list[dict],
) -> list[TrialConfig]:
    batch = int(reference.get("effective_batch", 1_024))
    trials = [
        TrialConfig(
            architecture,
            lr=lr,
            seed=0,
            steps=TENSOR_SCREEN_EXAMPLES // batch,
            stage="tensor_param",
            audit_every=65_536 // batch,
            compile_model=False,
            effective_batch=batch,
            lr_parameterization=parameterization,
            checkpoint_every_examples=65_536,
        )
        for architecture in kronecker_rank_architectures()
        for lr, parameterization in select_kronecker_lr_settings(probe_results)
    ]
    if len(trials) != 6 or len({trial.label for trial in trials}) != 6:
        raise RuntimeError("Kronecker parameter frontier must contain six trials")
    return trials


def next_tensor_boundary_probe(
    reference: dict,
    probe_results: list[dict],
) -> TrialConfig | None:
    """Extend one edge of the winning LR ladder, if it remains unbounded."""
    stable = [
        result
        for result in probe_results
        if result.get("status", "complete") == "complete"
    ]
    if not stable:
        return None
    best = min(
        stable,
        key=lambda result: (
            float(result["validation"]["kl"]),
            float(result["lr"]),
        ),
    )
    parameterization = str(best.get("lr_parameterization", "uniform"))
    same = [
        result
        for result in probe_results
        if str(result.get("lr_parameterization", "uniform"))
        == parameterization
    ]
    tested = sorted({float(result["lr"]) for result in same})
    best_lr = float(best["lr"])
    if best_lr == tested[-1]:
        higher = [
            result for result in same if float(result["lr"]) > best_lr
        ]
        if higher:
            return None
        next_lr = best_lr * 2
    elif best_lr == tested[0]:
        lower = [
            result for result in same if float(result["lr"]) < best_lr
        ]
        if lower:
            return None
        next_lr = best_lr / 2
    else:
        return None
    batch = int(reference.get("effective_batch", 1_024))
    return TrialConfig(
        btt_parameter_architectures()[1],
        lr=next_lr,
        seed=0,
        steps=TENSOR_PROBE_EXAMPLES // batch,
        stage="tensor_lr_probe",
        audit_every=65_536 // batch,
        compile_model=True,
        effective_batch=batch,
        lr_parameterization=parameterization,
        allow_divergence=True,
    )


def select_tensor_lr_settings(
    probe_results: list[dict],
) -> list[tuple[float, str]]:
    stable = [
        result
        for result in probe_results
        if result.get("status", "complete") == "complete"
    ]
    ordered = sorted(
        stable,
        key=lambda result: (
            float(result["validation"]["kl"]),
            float(result["lr"]),
            str(result.get("lr_parameterization", "uniform")),
        ),
    )
    settings = []
    for result in ordered:
        value = (
            float(result["lr"]),
            str(result.get("lr_parameterization", "uniform")),
        )
        if value not in settings:
            settings.append(value)
        if len(settings) == 2:
            return settings
    raise RuntimeError("tensor LR probe produced fewer than two stable settings")


def tensor_parameter_trials(
    reference: dict,
    probe_results: list[dict],
) -> list[TrialConfig]:
    batch = int(reference.get("effective_batch", 1_024))
    settings = select_tensor_lr_settings(probe_results)
    trials = [
        TrialConfig(
            architecture,
            lr=lr,
            seed=0,
            steps=TENSOR_SCREEN_EXAMPLES // batch,
            stage="tensor_param",
            audit_every=262_144 // batch,
            compile_model=True,
            effective_batch=batch,
            lr_parameterization=parameterization,
        )
        for architecture in btt_parameter_architectures()
        for lr, parameterization in settings
    ]
    if len(trials) != 8 or len({trial.label for trial in trials}) != 8:
        raise RuntimeError("tensor parameter frontier must contain eight trials")
    return trials


def tensor_time_trials(
    reference: dict,
    probe_results: list[dict],
    matched_depths: dict[str, int],
    kronecker_probe_results: list[dict] | None = None,
) -> list[TrialConfig]:
    batch = int(reference.get("effective_batch", 1_024))
    btt_settings = select_tensor_lr_settings(probe_results)
    kronecker_settings = (
        select_kronecker_lr_settings(kronecker_probe_results)
        if kronecker_probe_results is not None
        else []
    )
    architectures = []
    for architecture in btt_parameter_architectures():
        key = tensor_family_key(architecture)
        if key not in matched_depths:
            continue
        architectures.append(replace(architecture, depth=int(matched_depths[key])))
    if kronecker_probe_results is not None:
        architecture = kronecker_parameter_architecture()
        key = tensor_family_key(architecture)
        if key in matched_depths:
            architectures.append(
                replace(architecture, depth=int(matched_depths[key]))
            )
    trials = [
        TrialConfig(
            architecture,
            lr=lr,
            seed=0,
            steps=TENSOR_SCREEN_EXAMPLES // batch,
            stage="tensor_time",
            audit_every=262_144 // batch,
            compile_model=architecture.operator == "btt",
            effective_batch=batch,
            lr_parameterization=parameterization,
            checkpoint_every_examples=(
                65_536 if architecture.operator == "kronecker" else 0
            ),
        )
        for architecture in architectures
        for lr, parameterization in (
            btt_settings
            if architecture.operator == "btt"
            else kronecker_settings
        )
    ]
    if len(trials) > 10 or len({trial.label for trial in trials}) != len(trials):
        raise RuntimeError("invalid tensor time frontier")
    return trials


def tensor_family_key(config: ArchitectureConfig) -> str:
    if config.operator == "btt":
        return (
            f"btt-c{config.btt_cores}-r{config.btt_rank}-"
            f"{config.form}-x{config.expansion}"
        )
    if config.operator == "kronecker":
        return (
            f"kronecker-c{config.kronecker_factors}-"
            f"{config.kronecker_layout}-{config.form}-x{config.expansion}"
        )
    raise ValueError("tensor family keys require BTT or Kronecker")


def select_tensor_finalists(
    parameter_results: list[dict],
    time_results: list[dict],
) -> tuple[dict, dict, dict]:
    completed_parameter = [
        result
        for result in parameter_results
        if result.get("status", "complete") == "complete"
    ]
    completed_time = [
        result
        for result in time_results
        if result.get("status", "complete") == "complete"
    ]
    btt_parameter = [
        result
        for result in completed_parameter
        if result["architecture"]["operator"] == "btt"
    ]
    kronecker_parameter = [
        result
        for result in completed_parameter
        if result["architecture"]["operator"] == "kronecker"
    ]
    if not btt_parameter or not kronecker_parameter or not completed_time:
        raise RuntimeError(
            "BTT/Kronecker parameter and time frontiers need results"
        )
    key = lambda result: (
        float(result["validation"]["kl"]),
        int(result["trainable_parameters"]),
        str(result["label"]),
    )
    btt_winner = min(btt_parameter, key=key)
    kronecker_winner = min(kronecker_parameter, key=key)
    time_winner = min(completed_time, key=key)
    return btt_winner, kronecker_winner, time_winner


def _tensor_result_identity(result: dict) -> tuple:
    return (
        result["architecture"],
        float(result["lr"]),
        int(result.get("effective_batch", 1_024)),
        str(result.get("lr_parameterization", "uniform")),
    )


def tensor_final_trials(
    parameter_results: list[dict],
    time_results: list[dict],
) -> list[TrialConfig]:
    btt_winner, kronecker_winner, time_winner = select_tensor_finalists(
        parameter_results, time_results
    )
    trials = []
    identities = set()
    for result in (btt_winner, kronecker_winner, time_winner):
        raw_identity = _tensor_result_identity(result)
        identity = (
            str(raw_identity[0]),
            raw_identity[1],
            raw_identity[2],
            raw_identity[3],
        )
        if identity in identities:
            continue
        identities.add(identity)
        batch = identity[2]
        architecture = ArchitectureConfig(**result["architecture"])
        for seed in (0, 1, 2):
            trials.append(TrialConfig(
                architecture,
                lr=identity[1],
                seed=seed,
                steps=TENSOR_FINAL_EXAMPLES // batch,
                stage="tensor_final",
                audit_every=256_000 // batch,
                compile_model=architecture.operator == "btt",
                effective_batch=batch,
                lr_parameterization=identity[3],
                checkpoint_every_examples=(
                    65_536
                    if architecture.operator == "kronecker"
                    else 0
                ),
            ))
    if len(trials) not in (6, 9):
        raise RuntimeError(
            "tensor finals must contain two or three three-seed cells"
        )
    return trials


def tensor_long_trial(
    parameter_results: list[dict],
    time_results: list[dict],
    final_results: list[dict],
) -> TrialConfig:
    btt_winner, kronecker_winner, _ = select_tensor_finalists(
        parameter_results,
        time_results,
    )
    primary_identities = {
        str(_tensor_result_identity(btt_winner)),
        str(_tensor_result_identity(kronecker_winner)),
    }
    candidates = [
        result
        for result in final_results
        if str(_tensor_result_identity(result)) in primary_identities
        and result.get("status", "complete") == "complete"
    ]
    if not candidates:
        raise RuntimeError("tensor long run needs a parameter-primary final")
    winner = min(
        candidates,
        key=lambda result: (
            float(result["validation"]["kl"]),
            int(result["seed"]),
            str(result["label"]),
        ),
    )
    batch = int(winner.get("effective_batch", 1_024))
    return TrialConfig(
        ArchitectureConfig(**winner["architecture"]),
        lr=float(winner["lr"]),
        seed=int(winner["seed"]),
        steps=TENSOR_LONG_EXAMPLES // batch,
        stage="tensor_long",
        audit_every=262_144 // batch,
        compile_model=winner["architecture"]["operator"] == "btt",
        effective_batch=batch,
        lr_parameterization=str(
            winner.get("lr_parameterization", "uniform")
        ),
        checkpoint_every_examples=65_536,
        allow_data_reuse=True,
        warm_start_stage="tensor_final",
        warm_start_label=str(winner["label"]),
        warm_start_step=int(winner["steps_completed"]),
        target_validation_kl=TARGET_VALIDATION_KL,
    )
