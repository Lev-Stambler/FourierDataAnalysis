from __future__ import annotations

import hashlib
import json
import math
from dataclasses import dataclass, field
from typing import Any, Mapping, Sequence

from hydra import compose, initialize_config_module
from omegaconf import DictConfig, MISSING, OmegaConf

from .config import ArchitectureConfig


CONFIG_MODULE = "qwen_fullwidth_distill.conf"
CELL_SCHEMA = "qwen-fullwidth-hydra-cell-v1"
DRY_RUN_SCHEMA = "qwen-fullwidth-hydra-dry-run-v1"


@dataclass
class ArchitectureGroup:
    operator: str = MISSING
    form: str = MISSING
    depth: int = MISSING
    expansion: int = 1
    context_length: int = 16
    embedding_width: int = 1024
    monarch_blocks: int = 128
    monarch_rank: int = 1
    repetitions: int = 1
    residual_scale: str = "none"
    btt_cores: int = 2
    btt_rank: int = 1
    btt_layout: str = "context_preserving_v1"
    btt_weight_norm: bool = True
    kronecker_factors: int = 3
    kronecker_rank: int = 1
    kronecker_rank_chunk: int = 8
    kronecker_layout: str = "context_channel_v1"
    kronecker_weight_norm: bool = True
    gated_repetitions: bool = False


@dataclass
class DataGroup:
    dataset_id: str = MISSING
    dataset_config: str = MISSING
    dataset_revision: str = MISSING
    dataset_tag: str = MISSING
    data_root: str = MISSING
    teacher_cache_root: str = MISSING
    context_length: int = 16
    document_disjoint: bool = True
    allow_data_reuse: bool = False


@dataclass
class OptimizerGroup:
    name: str = "adamw"
    parameterization: str = "mup"
    betas: list[float] = field(default_factory=lambda: [0.9, 0.999])
    eps: float = 1e-8
    weight_decay: float = 0.01
    fused: bool = True
    gradient_clip_norm: float = 1.0
    role_lr_multipliers: dict[str, float] = field(default_factory=dict)
    role_weight_decays: dict[str, float] = field(default_factory=dict)


@dataclass
class ScheduleGroup:
    name: str = "wsd"
    unit: str = "optimizer_steps"
    warmup_start_lr_ratio: float = 0.1
    decay_end_lr_ratio: float = 0.1
    decay_shape: str = "cosine"


@dataclass
class MicrobatchGroup:
    mode: str = "auto"
    require_effective_batch_divisor: bool = False
    maximum: int = 1024
    minimum: int = 1
    target_memory_fraction: float = 0.88
    hard_memory_fraction: float = 0.92
    oom_backoff: bool = True


@dataclass
class RuntimeGroup:
    platform: str = "modal"
    accelerator: str = "H100"
    gpu_count: int = 1
    max_parallel: int = 8
    precision: str = "bfloat16"
    matmul_tf32: bool = True
    kronecker_backend: str = "cutensor"
    rank_chunk: int = 512
    microbatch: MicrobatchGroup = field(default_factory=MicrobatchGroup)


@dataclass
class ObjectiveGroup:
    name: str = "forward_kl"
    temperature: float = 1.0
    temperature2_weight: float = 0.0
    hidden_mse_weight: float = 0.0
    use_teacher_cache: bool = True


@dataclass
class BatchVariant:
    effective_batch: int = MISSING
    warmup_steps: int = MISSING
    stable_steps: int = MISSING
    decay_steps: int = MISSING


@dataclass
class SourceSelector:
    selector: str = "completed_validation_winner"
    stage: str = MISSING
    dataset_tag: str = MISSING
    architecture_label: str = MISSING
    metric: str = "validation.kl"
    direction: str = "min"
    require_status: str = "complete"
    require_exact_steps: bool = True
    require_full_validation: bool = True
    require_checkpoint: bool = True


@dataclass
class WarmStartGroup:
    mode: str = "weights_only"
    optimizer: str = "fresh_adam"
    source: SourceSelector = field(default_factory=SourceSelector)


@dataclass
class ExperimentGroup:
    name: str = MISSING
    stage: str = MISSING
    seed: int = 0
    fresh_contexts: int = MISSING
    audit_every_contexts: int = 0
    checkpoint_every_contexts: int = 0
    dispatch_mode: str = "wait"
    reporting_target_validation_kl: float = 1.0
    learning_rates: list[float] = field(default_factory=list)
    batches: list[BatchVariant] = field(default_factory=list)
    warm_start: WarmStartGroup = field(default_factory=WarmStartGroup)


@dataclass
class RootConfig:
    schema_version: str = "qwen-fullwidth-hydra-v1"
    architecture: ArchitectureGroup = field(default_factory=ArchitectureGroup)
    data: DataGroup = field(default_factory=DataGroup)
    optimizer: OptimizerGroup = field(default_factory=OptimizerGroup)
    schedule: ScheduleGroup = field(default_factory=ScheduleGroup)
    runtime: RuntimeGroup = field(default_factory=RuntimeGroup)
    objective: ObjectiveGroup = field(default_factory=ObjectiveGroup)
    experiment: ExperimentGroup = field(default_factory=ExperimentGroup)


def _resolved_mapping(config: DictConfig | Mapping[str, Any]) -> dict[str, Any]:
    if isinstance(config, DictConfig):
        value = OmegaConf.to_container(
            config,
            resolve=True,
            throw_on_missing=True,
        )
    else:
        value = OmegaConf.to_container(
            OmegaConf.create(dict(config)),
            resolve=True,
            throw_on_missing=True,
        )
    if not isinstance(value, dict):
        raise TypeError("resolved Hydra configuration must be a mapping")
    return value


def _validate_config(config: DictConfig) -> DictConfig:
    merged = OmegaConf.merge(OmegaConf.structured(RootConfig), config)
    OmegaConf.set_struct(merged, True)
    value = _resolved_mapping(merged)

    architecture = ArchitectureConfig(**value["architecture"])
    architecture.validate()
    if value["data"]["context_length"] != architecture.context_length:
        raise ValueError("data and architecture context lengths must match")
    if value["optimizer"]["name"] != "adamw":
        raise ValueError("the compiled experiment requires AdamW")
    betas = value["optimizer"]["betas"]
    if len(betas) != 2 or not all(0.0 <= float(beta) < 1.0 for beta in betas):
        raise ValueError("AdamW betas must contain two values in [0,1)")
    if value["schedule"]["name"] != "wsd":
        raise ValueError("the compiled experiment requires a WSD schedule")
    if value["schedule"]["unit"] != "optimizer_steps":
        raise ValueError("WSD phases must be expressed in optimizer steps")
    if value["runtime"]["accelerator"] != "H100":
        raise ValueError("this experiment is pinned to H100")
    microbatch = value["runtime"]["microbatch"]
    if (
        microbatch["mode"] != "auto"
        or int(microbatch["minimum"]) <= 0
        or int(microbatch["maximum"]) < int(microbatch["minimum"])
    ):
        raise ValueError("invalid automatic microbatch policy")
    if not (
        0.0 < float(microbatch["target_memory_fraction"])
        < float(microbatch["hard_memory_fraction"])
        < 1.0
    ):
        raise ValueError("invalid automatic microbatch memory fractions")
    if value["experiment"]["warm_start"]["mode"] != "weights_only":
        raise ValueError("experiment requires a weights-only warm start")
    if value["experiment"]["warm_start"]["optimizer"] != "fresh_adam":
        raise ValueError("weights-only warm start must use fresh Adam")
    if value["objective"]["use_teacher_cache"] is not True:
        raise ValueError("experiment requires the cached teacher")
    if value["experiment"]["dispatch_mode"] not in ("wait", "detached"):
        raise ValueError("dispatch_mode must be wait or detached")
    return merged


def compose_experiment(
    config_name: str = "edu_wsd_large_batch_lr",
    overrides: Sequence[str] | None = None,
) -> DictConfig:
    """Compose and validate one packaged experiment configuration."""
    if not config_name or "/" in config_name or config_name.endswith(".yaml"):
        raise ValueError("config_name must be an experiment group name")
    user_overrides = list(overrides or ())
    if any(item.startswith("experiment=") for item in user_overrides):
        raise ValueError("experiment group is selected by config_name")
    with initialize_config_module(
        config_module=CONFIG_MODULE,
        version_base="1.3",
    ):
        config = compose(
            config_name="config",
            overrides=[f"experiment={config_name}", *user_overrides],
        )
    return _validate_config(config)


def resolved_config_hash(
    config: DictConfig | Mapping[str, Any],
) -> str:
    """Hash a fully resolved config using canonical UTF-8 JSON."""
    def without_self_hashes(value: Any) -> Any:
        if isinstance(value, Mapping):
            return {
                key: without_self_hashes(item)
                for key, item in value.items()
                if key not in ("config_hash", "resolved_cell_config_hash")
            }
        if isinstance(value, list):
            return [without_self_hashes(item) for item in value]
        return value

    payload = json.dumps(
        without_self_hashes(_resolved_mapping(config)),
        sort_keys=True,
        separators=(",", ":"),
        ensure_ascii=False,
        allow_nan=False,
    ).encode("utf-8")
    return hashlib.sha256(payload).hexdigest()


def _validated_architecture(value: Mapping[str, Any]) -> ArchitectureConfig:
    architecture = ArchitectureConfig(**dict(value))
    architecture.validate()
    return architecture


def expand_experiment(
    config: DictConfig | Mapping[str, Any],
) -> list[dict[str, Any]]:
    """Expand the resolved batch/LR matrix into Modal-ready cell payloads."""
    if not isinstance(config, DictConfig):
        config = _validate_config(OmegaConf.create(dict(config)))
    value = _resolved_mapping(config)
    architecture = _validated_architecture(value["architecture"])
    experiment = value["experiment"]
    data = value["data"]
    optimizer = value["optimizer"]
    objective = value["objective"]
    runtime = value["runtime"]
    base_schedule = value["schedule"]
    warm_start = experiment["warm_start"]

    source = warm_start["source"]
    if source["architecture_label"] != architecture.label:
        raise ValueError("warm-start source architecture does not match experiment")
    if source["dataset_tag"] != data["dataset_tag"]:
        raise ValueError("warm-start source dataset does not match experiment")
    if experiment["fresh_contexts"] <= 0:
        raise ValueError("fresh_contexts must be positive")
    if (
        not math.isfinite(float(experiment["reporting_target_validation_kl"]))
        or float(experiment["reporting_target_validation_kl"]) <= 0
    ):
        raise ValueError("reporting_target_validation_kl must be positive")
    if not experiment["learning_rates"]:
        raise ValueError("experiment requires at least one learning rate")
    if not experiment["batches"]:
        raise ValueError("experiment requires at least one batch variant")
    fresh_contexts = int(experiment["fresh_contexts"])
    audit_contexts = (
        int(experiment["audit_every_contexts"]) or fresh_contexts
    )
    checkpoint_contexts = (
        int(experiment["checkpoint_every_contexts"]) or fresh_contexts
    )
    if (
        audit_contexts <= 0
        or checkpoint_contexts <= 0
        or fresh_contexts % audit_contexts
        or fresh_contexts % checkpoint_contexts
    ):
        raise ValueError(
            "audit/checkpoint context intervals must be positive divisors "
            "of fresh_contexts"
        )

    cells = []
    for batch_variant in experiment["batches"]:
        effective_batch = int(batch_variant["effective_batch"])
        phases = {
            "warmup_steps": int(batch_variant["warmup_steps"]),
            "stable_steps": int(batch_variant["stable_steps"]),
            "decay_steps": int(batch_variant["decay_steps"]),
        }
        if effective_batch <= 0 or min(phases.values()) <= 0:
            raise ValueError("batch and WSD phase counts must be positive")
        steps = sum(phases.values())
        contexts = steps * effective_batch
        if contexts != int(experiment["fresh_contexts"]):
            raise ValueError(
                f"batch {effective_batch} WSD phases cover {contexts}, "
                f"expected {experiment['fresh_contexts']}"
            )
        if (
            audit_contexts % effective_batch
            or checkpoint_contexts % effective_batch
        ):
            raise ValueError(
                f"batch {effective_batch} must divide the audit and "
                "checkpoint context intervals"
            )
        for learning_rate in experiment["learning_rates"]:
            lr = float(learning_rate)
            if not math.isfinite(lr) or lr <= 0:
                raise ValueError("learning rates must be positive and finite")
            schedule = {
                **base_schedule,
                **phases,
                "total_steps": steps,
                "peak_lr": lr,
            }
            cell_index = len(cells)
            cell = {
                "schema": CELL_SCHEMA,
                "experiment": experiment["name"],
                "cell_index": cell_index,
                "trial": {
                    "architecture": architecture.to_dict(),
                    "lr": lr,
                    "seed": int(experiment["seed"]),
                    "steps": steps,
                    "stage": experiment["stage"],
                    "audit_every": audit_contexts // effective_batch,
                    "compile_model": False,
                    "effective_batch": effective_batch,
                    "lr_parameterization": optimizer["parameterization"],
                    "lr_schedule": "wsd",
                    "warmup_steps": phases["warmup_steps"],
                    "cooldown_steps": phases["decay_steps"],
                    "min_lr_ratio": 0.0,
                    "allow_divergence": True,
                    "checkpoint_every_examples": checkpoint_contexts,
                    "allow_data_reuse": bool(data["allow_data_reuse"]),
                    "gradient_clip_norm": float(
                        optimizer["gradient_clip_norm"]
                    ),
                    "dataset_tag": data["dataset_tag"],
                    "temperature": float(objective["temperature"]),
                    "temperature2_weight": float(
                        objective["temperature2_weight"]
                    ),
                    "hidden_mse_weight": float(
                        objective["hidden_mse_weight"]
                    ),
                    "use_teacher_cache": bool(
                        objective["use_teacher_cache"]
                    ),
                    # The coordinator resolves and injects source identifiers
                    # only after applying the validation/checkpoint gate below.
                    "warm_start_weights_only": True,
                },
                "fresh_contexts": contexts,
                "data": data,
                "optimizer": optimizer,
                "schedule": schedule,
                "objective": objective,
                "runtime": runtime,
                "warm_start": warm_start,
            }
            cell["hydra"] = {
                "root_config_hash": resolved_config_hash(config),
                "config_module": CONFIG_MODULE,
                "experiment_group": experiment["name"],
            }
            cell_hash = resolved_config_hash(cell)
            cell["config_hash"] = cell_hash
            cell["hydra"]["resolved_cell_config_hash"] = cell_hash
            cells.append(cell)
    expected_cells = (
        len(experiment["batches"]) * len(experiment["learning_rates"])
    )
    if len(cells) != expected_cells or len({
        cell["config_hash"] for cell in cells
    }) != len(cells):
        raise RuntimeError("experiment expansion did not produce unique cells")
    return cells


def dry_run_experiment(
    config_name: str = "edu_wsd_large_batch_lr",
    overrides: Sequence[str] | None = None,
) -> dict[str, Any]:
    """Return a JSON-serializable resolved plan without launching work."""
    config = compose_experiment(config_name, overrides)
    cells = expand_experiment(config)
    return {
        "schema": DRY_RUN_SCHEMA,
        "experiment": config.experiment.name,
        "resolved_config_hash": resolved_config_hash(config),
        "resolved_config": _resolved_mapping(config),
        "cell_count": len(cells),
        "cells": cells,
    }


def dry_run_json(
    config_name: str = "edu_wsd_large_batch_lr",
    overrides: Sequence[str] | None = None,
) -> str:
    """Render the resolved dry run as deterministic, human-readable JSON."""
    return json.dumps(
        dry_run_experiment(config_name, overrides),
        indent=2,
        sort_keys=True,
        allow_nan=False,
    )
