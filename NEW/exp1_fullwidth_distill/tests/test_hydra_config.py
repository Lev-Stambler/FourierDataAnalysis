import json

import pytest
from omegaconf.errors import ConfigAttributeError

from qwen_fullwidth_distill.hydra_config import (
    CELL_SCHEMA,
    DRY_RUN_SCHEMA,
    compose_experiment,
    dry_run_experiment,
    dry_run_json,
    expand_experiment,
    resolved_config_hash,
)


def test_hydra_composition_resolves_typed_groups():
    config = compose_experiment("edu_wsd_large_batch_lr")
    assert config.schema_version == "qwen-fullwidth-hydra-v1"
    assert config.architecture.operator == "kronecker"
    assert config.architecture.depth == 4
    assert config.architecture.kronecker_rank == 2407
    assert config.data.dataset_tag == "fwedu350bt-v1p4"
    assert config.optimizer.name == "adamw"
    assert list(config.optimizer.betas) == [0.9, 0.999]
    assert config.schedule.name == "wsd"
    assert config.objective.use_teacher_cache is True
    assert config.runtime.accelerator == "H100"
    assert config.runtime.max_parallel == 8
    assert config.runtime.microbatch.mode == "auto"
    assert config.runtime.microbatch.maximum == 1024
    assert config.runtime.microbatch.require_effective_batch_divisor is False
    assert config.runtime.microbatch.target_memory_fraction == 0.88
    assert config.runtime.microbatch.hard_memory_fraction == 0.92
    assert (
        config.data.teacher_cache_root
        == "/cache/qwen_fullwidth_distill/"
        "context16-fineweb-edu-350bt-v1p4-teacher-hidden-v1"
    )
    with pytest.raises(ConfigAttributeError):
        config.runtime.not_a_real_field = 1


def test_large_batch_lr_experiment_expands_exact_eight_cells():
    cells = expand_experiment(
        compose_experiment("edu_wsd_large_batch_lr")
    )
    assert len(cells) == 8
    assert len({cell["config_hash"] for cell in cells}) == 8
    assert all(cell["schema"] == CELL_SCHEMA for cell in cells)
    assert all(
        resolved_config_hash(cell) == cell["config_hash"]
        == cell["hydra"]["resolved_cell_config_hash"]
        for cell in cells
    )
    assert [
        (cell["trial"]["effective_batch"], cell["trial"]["lr"])
        for cell in cells
    ] == [
        (3168, 5e-6),
        (3168, 1e-5),
        (3168, 1.5e-5),
        (3168, 2e-5),
        (6336, 5e-6),
        (6336, 1e-5),
        (6336, 1.5e-5),
        (6336, 2e-5),
    ]
    for cell in cells[:4]:
        assert cell["trial"]["steps"] == 256
        assert (
            cell["schedule"]["warmup_steps"],
            cell["schedule"]["stable_steps"],
            cell["schedule"]["decay_steps"],
        ) == (16, 216, 24)
    for cell in cells[4:]:
        assert cell["trial"]["steps"] == 128
        assert (
            cell["schedule"]["warmup_steps"],
            cell["schedule"]["stable_steps"],
            cell["schedule"]["decay_steps"],
        ) == (8, 108, 12)
    assert {cell["fresh_contexts"] for cell in cells} == {811_008}
    assert all(
        "target_validation_kl" not in cell["trial"]
        for cell in cells
    )
    assert {
        cell["trial"]["steps"] * cell["trial"]["effective_batch"]
        for cell in cells
    } == {811_008}
    assert {
        (
            cell["schedule"]["warmup_start_lr_ratio"],
            cell["schedule"]["decay_end_lr_ratio"],
            cell["schedule"]["decay_shape"],
        )
        for cell in cells
    } == {(0.0, 0.0, "linear")}
    assert all(cell["trial"]["lr_schedule"] == "wsd" for cell in cells)
    assert all(cell["trial"]["warm_start_weights_only"] for cell in cells)
    assert [cell["trial"]["cooldown_steps"] for cell in cells] == (
        [24] * 4 + [12] * 4
    )


def test_high_token_optimizer_sweep_consumes_remaining_unique_train_split():
    config = compose_experiment("edu_wsd_optimizer_tokens_262m")
    cells = expand_experiment(config)
    assert config.experiment.dispatch_mode == "detached"
    assert len(cells) == 8
    assert [
        (cell["trial"]["effective_batch"], cell["trial"]["lr"])
        for cell in cells
    ] == [
        (256, 3e-6),
        (256, 6e-6),
        (512, 3e-6),
        (512, 6e-6),
        (1024, 3e-6),
        (1024, 6e-6),
        (2048, 3e-6),
        (2048, 6e-6),
    ]
    expected = {
        256: (64_000, 32, 57_568, 6_400, 256),
        512: (32_000, 16, 28_784, 3_200, 128),
        1024: (16_000, 8, 14_392, 1_600, 64),
        2048: (8_000, 4, 7_196, 800, 32),
    }
    for cell in cells:
        batch = cell["trial"]["effective_batch"]
        steps, warmup, stable, decay, audit_every = expected[batch]
        assert cell["trial"]["steps"] == steps
        assert cell["trial"]["warmup_steps"] == warmup
        assert cell["schedule"]["stable_steps"] == stable
        assert cell["trial"]["cooldown_steps"] == decay
        assert cell["trial"]["audit_every"] == audit_every
        assert cell["trial"]["checkpoint_every_examples"] == 327_680
        assert cell["fresh_contexts"] == 16_384_000
        assert cell["fresh_contexts"] * 16 == 262_144_000
    assert sum(cell["fresh_contexts"] * 16 for cell in cells) == 2_097_152_000


def test_cells_preserve_fresh_adam_source_gate_and_runtime_contract():
    cell = expand_experiment(
        compose_experiment("edu_wsd_large_batch_lr")
    )[0]
    assert cell["warm_start"]["mode"] == "weights_only"
    assert cell["warm_start"]["optimizer"] == "fresh_adam"
    assert (
        compose_experiment(
            "edu_wsd_large_batch_lr"
        ).experiment.reporting_target_validation_kl
        == 1.0
    )
    source = cell["warm_start"]["source"]
    assert source == {
        "selector": "completed_validation_winner",
        "stage": "tensor_kron_edu_lr",
        "dataset_tag": "fwedu350bt-v1p4",
        "architecture_label":
            "kronecker-c3-r2407-residual_ffn-d4-x4",
        "metric": "validation.kl",
        "direction": "min",
        "require_status": "complete",
        "require_exact_steps": True,
        "require_full_validation": True,
        "require_checkpoint": True,
    }
    assert cell["trial"]["warm_start_weights_only"] is True
    assert cell["data"]["allow_data_reuse"] is False
    assert cell["objective"]["use_teacher_cache"] is True
    assert cell["runtime"]["microbatch"]["mode"] == "auto"
    assert cell["runtime"]["microbatch"]["oom_backoff"] is True
    assert cell["optimizer"] == {
        "name": "adamw",
        "parameterization": "mup",
        "betas": [0.9, 0.999],
        "eps": 1e-8,
        "weight_decay": 0.01,
        "fused": True,
        "gradient_clip_norm": 1.0,
        "role_lr_multipliers": {},
        "role_weight_decays": {},
    }


def test_resolved_hash_and_dry_run_are_deterministic_and_override_sensitive():
    first = compose_experiment("edu_wsd_large_batch_lr")
    second = compose_experiment("edu_wsd_large_batch_lr")
    assert resolved_config_hash(first) == resolved_config_hash(second)
    assert len(resolved_config_hash(first)) == 64

    overridden = compose_experiment(
        "edu_wsd_large_batch_lr",
        overrides=["runtime.microbatch.maximum=6336"],
    )
    assert resolved_config_hash(first) != resolved_config_hash(overridden)
    assert expand_experiment(overridden)[0]["runtime"]["microbatch"][
        "maximum"
    ] == 6336

    dry_run = dry_run_experiment("edu_wsd_large_batch_lr")
    assert dry_run["schema"] == DRY_RUN_SCHEMA
    assert dry_run["cell_count"] == 8
    assert len(dry_run["resolved_config_hash"]) == 64
    rendered = dry_run_json("edu_wsd_large_batch_lr")
    assert json.loads(rendered) == dry_run


def test_expansion_rejects_wsd_phase_counts_that_change_context_budget():
    config = compose_experiment(
        "edu_wsd_large_batch_lr",
        overrides=["experiment.batches.0.stable_steps=215"],
    )
    with pytest.raises(ValueError, match="expected 811008"):
        expand_experiment(config)


def test_expansion_rejects_misaligned_monitoring_cadence():
    config = compose_experiment(
        "edu_wsd_optimizer_tokens_262m",
        overrides=["experiment.audit_every_contexts=65535"],
    )
    with pytest.raises(ValueError, match="intervals"):
        expand_experiment(config)
