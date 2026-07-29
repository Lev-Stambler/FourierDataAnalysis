from __future__ import annotations

import math
import os
from dataclasses import asdict, dataclass
from typing import Literal


MODEL_ID = "Qwen/Qwen3.5-0.8B-Base"
MODEL_REVISION = "5c8a1b97ddef11f79b47ab9d07bf82b9117413f6"
FINEWEB_ID = "HuggingFaceFW/fineweb"
FINEWEB_CONFIG = "CC-MAIN-2024-10"
FINEWEB_REVISION = "9bb295ddab0e05d785b879661af7260fed5140fc"
FINEWEB_EDU_ID = "HuggingFaceFW/fineweb-edu"
FINEWEB_EDU_CONFIG = "sample-350BT"
FINEWEB_EDU_REVISION = "87f09149ef4734204d70ed1d046ddc9ca3f2b8f9"

CONTEXT_LENGTH = 16
EMBEDDING_WIDTH = 1024
FULL_WIDTH = CONTEXT_LENGTH * EMBEDDING_WIDTH
MONARCH_BLOCKS = 128

TRAIN_EXAMPLES = 4_194_304
VALIDATION_EXAMPLES = 8_192
TEST_EXAMPLES = 8_192
AUDIT_EXAMPLES = 2_048
EFFECTIVE_BATCH = 1_024

Operator = Literal["dense", "monarch", "btt", "kronecker", "hybrid"]
Form = Literal[
    "sequential",
    "residual_one",
    "residual_ffn",
    "residual_gated",
]
ResidualScale = Literal[
    "none",
    "inverse_repetitions",
    "inverse_sqrt_depth",
]
LRParameterization = Literal["uniform", "mup"]
LRSchedule = Literal["constant", "warmup_cosine", "warmup_hold", "wsd"]
Objective = Literal["forward_kl", "next_token_ce"]
EmbeddingInitialization = Literal["frozen_qwen", "trainable_random"]
BTTLayout = Literal["context_preserving_v1"]
KroneckerLayout = Literal[
    "context_channel_v1",
    "context_channel_order4_v1",
]


@dataclass(frozen=True)
class ArchitectureConfig:
    operator: Operator
    form: Form
    depth: int
    expansion: int = 1
    context_length: int = CONTEXT_LENGTH
    embedding_width: int = EMBEDDING_WIDTH
    monarch_blocks: int = MONARCH_BLOCKS
    monarch_rank: int = 1
    repetitions: int = 1
    residual_scale: ResidualScale = "none"
    btt_cores: int = 2
    btt_rank: int = 1
    btt_layout: BTTLayout = "context_preserving_v1"
    btt_weight_norm: bool = True
    kronecker_factors: int = 3
    kronecker_rank: int = 1
    kronecker_rank_chunk: int = 8
    kronecker_layout: KroneckerLayout = "context_channel_v1"
    kronecker_weight_norm: bool = True
    gated_repetitions: bool = False

    @property
    def full_width(self) -> int:
        return self.context_length * self.embedding_width

    @property
    def effective_depth(self) -> int:
        return self.depth * self.repetitions

    @property
    def residual_multiplier(self) -> float:
        if self.residual_scale == "inverse_repetitions":
            return 1.0 / self.repetitions
        if self.residual_scale == "inverse_sqrt_depth":
            return 1.0 / math.sqrt(self.depth)
        if self.operator in ("btt", "kronecker", "hybrid"):
            return 1.0 / math.sqrt(self.depth)
        return 1.0

    @property
    def label(self) -> str:
        if self.operator == "monarch":
            rank = f"-r{self.monarch_rank}"
        elif self.operator == "btt":
            rank = f"-c{self.btt_cores}-r{self.btt_rank}"
        elif self.operator in ("kronecker", "hybrid"):
            rank = f"-c{self.kronecker_factors}"
            if self.kronecker_rank > 1:
                rank += f"-r{self.kronecker_rank}"
        else:
            rank = ""
        expansion = f"-x{self.expansion}" if self.form == "residual_ffn" else ""
        loop = ""
        if self.repetitions > 1:
            scale = (
                "invrep"
                if self.residual_scale == "inverse_repetitions"
                else "none"
            )
            loop = f"-repeat{self.repetitions}-scale{scale}"
            if self.gated_repetitions:
                loop += "-gated"
        elif self.residual_scale == "inverse_sqrt_depth":
            loop = "-scaleinvsqrtdepth"
        return (
            f"{self.operator}{rank}-{self.form}-d{self.depth}"
            f"{expansion}{loop}"
        )

    def validate(self) -> None:
        if self.operator not in (
            "dense",
            "monarch",
            "btt",
            "kronecker",
            "hybrid",
        ):
            raise ValueError(f"unsupported operator {self.operator}")
        if self.form not in (
            "sequential",
            "residual_one",
            "residual_ffn",
            "residual_gated",
        ):
            raise ValueError(f"unsupported form {self.form}")
        if self.depth <= 0:
            raise ValueError("depth must be positive")
        if self.repetitions <= 0:
            raise ValueError("repetitions must be positive")
        if self.form != "residual_ffn" and self.expansion != 1:
            raise ValueError("expansion only applies to residual_ffn")
        if self.expansion not in (1, 4):
            raise ValueError("expansion must be 1 or 4")
        if self.form == "residual_gated" and self.operator not in (
            "btt",
            "kronecker",
        ):
            raise ValueError("residual_gated is restricted to tensor layers")
        if self.operator == "dense" and self.monarch_rank != 1:
            raise ValueError("dense layers do not have Monarch rank")
        if self.operator != "monarch" and self.monarch_rank != 1:
            raise ValueError("monarch_rank only applies to Monarch layers")
        if self.operator == "btt":
            if self.btt_cores not in (3, 4):
                raise ValueError("the BTT study supports exactly three or four cores")
            if self.btt_rank not in (1, 2):
                raise ValueError("the BTT study supports rank one or two")
            if self.btt_layout != "context_preserving_v1":
                raise ValueError("unsupported BTT layout")
            if not self.btt_weight_norm:
                raise ValueError("BTT core weight normalization is required")
            if (
                self.context_length != CONTEXT_LENGTH
                or self.embedding_width != EMBEDDING_WIDTH
            ):
                raise ValueError(
                    "context_preserving_v1 is pinned to context 16 and width 1024"
                )
        elif (
            self.btt_cores != 2
            or self.btt_rank != 1
            or self.btt_layout != "context_preserving_v1"
            or not self.btt_weight_norm
        ):
            raise ValueError("BTT fields only apply to BTT layers")
        if self.operator in ("kronecker", "hybrid"):
            if self.kronecker_factors not in (3, 4):
                raise ValueError(
                    "the Kronecker study supports exactly three or four factors"
                )
            if self.kronecker_rank <= 0:
                raise ValueError("Kronecker rank must be positive")
            if self.kronecker_rank_chunk <= 0:
                raise ValueError("Kronecker rank chunk must be positive")
            expected_layout = (
                "context_channel_v1"
                if self.kronecker_factors == 3
                else "context_channel_order4_v1"
            )
            if self.kronecker_layout != expected_layout:
                raise ValueError(
                    f"factor order {self.kronecker_factors} requires "
                    f"{expected_layout}"
                )
            if not self.kronecker_weight_norm:
                raise ValueError(
                    "Kronecker factor weight normalization is required"
                )
            channel = math.isqrt(self.embedding_width)
            if (
                self.kronecker_factors == 3
                and channel * channel != self.embedding_width
            ):
                raise ValueError(
                    "context_channel_v1 requires a square embedding width"
                )
        elif (
            self.kronecker_factors != 3
            or self.kronecker_rank != 1
            or self.kronecker_rank_chunk != 8
            or self.kronecker_layout != "context_channel_v1"
            or not self.kronecker_weight_norm
        ):
            raise ValueError(
                "Kronecker fields only apply to Kronecker layers"
            )
        if self.residual_scale not in (
            "none",
            "inverse_repetitions",
            "inverse_sqrt_depth",
        ):
            raise ValueError("invalid residual scaling mode")
        if (
            self.repetitions == 1
            and self.residual_scale == "inverse_repetitions"
        ):
            raise ValueError("single-pass stacks must use unscaled residuals")
        if self.repetitions > 1 and (
            self.operator not in ("monarch", "kronecker")
            or self.form != "residual_ffn"
        ):
            raise ValueError(
                "repetition is restricted to structured residual FFN stacks"
            )
        if self.gated_repetitions and (
            self.repetitions <= 1
            or self.operator != "kronecker"
            or self.form != "residual_ffn"
        ):
            raise ValueError(
                "gated repetitions require a repeated Kronecker residual FFN"
            )
        if (
            self.residual_scale == "inverse_repetitions"
            and self.repetitions == 1
        ):
            raise ValueError("inverse repetition scaling requires a loop")
        if self.residual_scale == "inverse_sqrt_depth" and (
            self.operator != "monarch" or self.form != "residual_ffn"
        ):
            raise ValueError(
                "inverse-sqrt-depth scaling is restricted to Monarch FFNs"
            )
        if self.operator == "monarch" and self.full_width % self.monarch_blocks:
            raise ValueError("full width must divide evenly into Monarch blocks")
        if self.operator == "hybrid" and (
            self.form != "residual_ffn"
            or self.expansion != 4
            or self.depth < 3
            or self.kronecker_factors != 4
        ):
            raise ValueError(
                "hybrid requires expansion-four residual FFNs, at least "
                "three blocks, and order-four Kronecker interiors"
            )

    def to_dict(self) -> dict:
        value = asdict(self)
        # Preserve the exact serialized architecture of every already-committed
        # baseline result. New loop fields are material only for repeated stacks.
        if self.repetitions == 1:
            value.pop("repetitions")
            if self.residual_scale == "none":
                value.pop("residual_scale")
        if self.operator != "btt":
            value.pop("btt_cores")
            value.pop("btt_rank")
            value.pop("btt_layout")
            value.pop("btt_weight_norm")
        if self.operator not in ("kronecker", "hybrid"):
            value.pop("kronecker_factors")
            value.pop("kronecker_rank")
            value.pop("kronecker_rank_chunk")
            value.pop("kronecker_layout")
            value.pop("kronecker_weight_norm")
        elif self.kronecker_rank == 1:
            # Preserve rank-one artifact identities already on the Volume.
            value.pop("kronecker_rank")
            value.pop("kronecker_rank_chunk")
        if not self.gated_repetitions:
            value.pop("gated_repetitions")
        return value


@dataclass(frozen=True)
class TrialConfig:
    architecture: ArchitectureConfig
    lr: float = 1e-3
    seed: int = 0
    steps: int = 1_000
    stage: str = "screen"
    audit_every: int = 100
    compile_model: bool = False
    smoke: bool = False
    effective_batch: int = EFFECTIVE_BATCH
    lr_parameterization: LRParameterization = "uniform"
    allow_divergence: bool = False
    max_activation_rms_growth: float = 10.0
    checkpoint_every_examples: int = 0
    allow_data_reuse: bool = False
    warm_start_stage: str = ""
    warm_start_label: str = ""
    warm_start_step: int = 0
    warm_start_resume_step: int = 0
    warm_start_lr_override: bool = False
    warm_start_weights_only: bool = False
    warm_start_from_stable: bool = False
    target_validation_kl: float | None = None
    lr_schedule: LRSchedule = "constant"
    warmup_steps: int = 0
    cooldown_steps: int = 0
    min_lr_ratio: float = 1.0
    gradient_clip_norm: float = 0.0
    dataset_tag: str = ""
    warmup_examples: int = 0
    cooldown_examples: int = 0
    optimizer_role_lr_multipliers: tuple[tuple[str, float], ...] = ()
    optimizer_role_weight_decays: tuple[tuple[str, float], ...] = ()
    temperature: float = 1.0
    temperature2_weight: float = 0.0
    hidden_mse_weight: float = 0.0
    use_teacher_cache: bool = False
    objective: Objective = "forward_kl"
    embedding_initialization: EmbeddingInitialization = "frozen_qwen"
    hydra_config_hash: str = ""
    hydra_resolved_config_json: str = ""

    def __post_init__(self) -> None:
        object.__setattr__(
            self,
            "optimizer_role_lr_multipliers",
            tuple(
                (str(role), float(value))
                for role, value in self.optimizer_role_lr_multipliers
            ),
        )
        object.__setattr__(
            self,
            "optimizer_role_weight_decays",
            tuple(
                (str(role), float(value))
                for role, value in self.optimizer_role_weight_decays
            ),
        )

    @property
    def label(self) -> str:
        lr = f"{self.lr:g}".replace(".", "p")
        batch = (
            f"-b{self.effective_batch}"
            if self.effective_batch != EFFECTIVE_BATCH
            else ""
        )
        parameterization = (
            f"-p{self.lr_parameterization}"
            if self.lr_parameterization != "uniform"
            else ""
        )
        schedule = ""
        if self.lr_schedule == "wsd":
            schedule = (
                f"-wsd-w{self.warmup_steps}-c{self.cooldown_steps}"
            )
        elif self.lr_schedule != "constant":
            minimum = f"{self.min_lr_ratio:g}".replace(".", "p")
            schedule = (
                f"-{self.lr_schedule}-w{self.warmup_steps}-min{minimum}"
            )
        clipping = (
            f"-clip{self.gradient_clip_norm:g}".replace(".", "p")
            if self.gradient_clip_norm > 0
            else ""
        )
        dataset = f"-data{self.dataset_tag}" if self.dataset_tag else ""
        warmup_examples = (
            f"-we{self.warmup_examples}"
            if self.warmup_examples
            else ""
        )
        cooldown_examples = (
            f"-ce{self.cooldown_examples}"
            if self.cooldown_examples
            else ""
        )
        warm_start_policy = (
            ("-stable" if self.warm_start_from_stable else "")
            + ("-freshadam" if self.warm_start_weights_only else "")
        )
        role_names = {
            "kronecker_factor": "kf",
            "kronecker_gain": "kg",
            "kronecker_mixing": "km",
            "standard": "st",
            "bias": "bi",
            "tied_embedding": "te",
        }
        role_lr = "".join(
            f"-{role_names.get(role, role)}x"
            f"{value:g}".replace(".", "p")
            for role, value in self.optimizer_role_lr_multipliers
        )
        role_wd = "".join(
            f"-{role_names.get(role, role)}wd"
            f"{value:g}".replace(".", "p")
            for role, value in self.optimizer_role_weight_decays
        )
        objective = ""
        if self.objective == "next_token_ce":
            objective += "-ce"
        if self.temperature != 1.0 or self.temperature2_weight:
            objective += (
                f"-t{self.temperature:g}w{self.temperature2_weight:g}"
            ).replace(".", "p")
        if self.hidden_mse_weight:
            objective += (
                f"-h{self.hidden_mse_weight:g}".replace(".", "p")
            )
        cache = "-tc" if self.use_teacher_cache else ""
        embedding = (
            "-erand"
            if self.embedding_initialization == "trainable_random"
            else ""
        )
        hydra = (
            f"-hc{self.hydra_config_hash[:8]}"
            if self.hydra_config_hash
            else ""
        )
        return (
            f"{self.stage}-{self.architecture.label}-lr{lr}"
            f"{batch}{parameterization}{schedule}{warmup_examples}"
            f"{cooldown_examples}{warm_start_policy}"
            f"{clipping}{role_lr}{role_wd}{objective}{embedding}"
            f"{cache}{hydra}{dataset}"
            f"-s{self.seed}"
        )

    @property
    def is_final(self) -> bool:
        return (
            self.stage.startswith("next_token_pretrain_")
            or self.stage.startswith("tensor_kron_edu_scale_")
            or self.stage in (
            "final",
            "depth_final",
            "tensor_final",
            "tensor_long",
            "tensor_kron_rank_boundary",
            "tensor_kron_rank_batch_boundary",
            "tensor_kron_rank_batch_lr_boundary",
            "tensor_kron_rank_batch_lr_continue",
            "tensor_kron_rank_depth_optimized",
            "tensor_kron_rank_depth_winner_continue",
            "tensor_kron_rank_continue",
            "tensor_kron_rank_monarch_long",
            "tensor_kron_rank_monarch_continue",
            "tensor_kron_rank_monarch_capacity",
            "tensor_kron_rank_monarch_capacity_checkpoint",
            "tensor_kron_rank_monarch_capacity_continue",
            "tensor_kron_rank_monarch_batch",
            "tensor_kron_rank_monarch_batch_continue",
            "tensor_kron_rank_monarch_batch_frontier",
            "tensor_kron_rank_monarch_batch_long",
            "tensor_kron_rank_monarch_batch_million",
            "tensor_kron_rank_monarch_batch_two_million",
            "tensor_kron_rank_monarch_batch_three_million",
            "tensor_kron_rank_monarch_batch_rank2_retry",
            "tensor_kron_rank_monarch_batch_transition",
            "tensor_kron_rank_monarch_batch_transition_lr",
            "tensor_kron_rank_monarch_mature_continue",
            "tensor_kron_rank_monarch_decay",
            "tensor_kron_rank_monarch_decay_continue",
            "tensor_kron_rank_monarch_depth_parameter_matched",
            "tensor_kron_edu_lr",
            "tensor_kron_edu_long",
            "tensor_kron_edu_optimizer",
            "tensor_kron_edu_objective",
            "tensor_kron_edu_architecture",
            "tensor_kron_edu_scale",
            "tensor_kron_edu_wsd_large_batch_lr",
            )
        )

    def to_dict(self) -> dict:
        value = asdict(self)
        value["architecture"] = self.architecture.to_dict()
        value["architecture"]["label"] = self.architecture.label
        if self.effective_batch == EFFECTIVE_BATCH:
            value.pop("effective_batch")
        if self.lr_parameterization == "uniform":
            value.pop("lr_parameterization")
        if not self.allow_divergence:
            value.pop("allow_divergence")
        if self.max_activation_rms_growth == 10.0:
            value.pop("max_activation_rms_growth")
        if self.checkpoint_every_examples == 0:
            value.pop("checkpoint_every_examples")
        if not self.allow_data_reuse:
            value.pop("allow_data_reuse")
        if not self.warm_start_stage:
            value.pop("warm_start_stage")
        if not self.warm_start_label:
            value.pop("warm_start_label")
        if self.warm_start_step == 0:
            value.pop("warm_start_step")
        if self.warm_start_resume_step == 0:
            value.pop("warm_start_resume_step")
        if not self.warm_start_lr_override:
            value.pop("warm_start_lr_override")
        if not self.warm_start_weights_only:
            value.pop("warm_start_weights_only")
        if not self.warm_start_from_stable:
            value.pop("warm_start_from_stable")
        if self.target_validation_kl is None:
            value.pop("target_validation_kl")
        if self.lr_schedule == "constant":
            value.pop("lr_schedule")
        if self.warmup_steps == 0:
            value.pop("warmup_steps")
        if self.cooldown_steps == 0:
            value.pop("cooldown_steps")
        if self.min_lr_ratio == 1.0:
            value.pop("min_lr_ratio")
        if self.gradient_clip_norm == 0.0:
            value.pop("gradient_clip_norm")
        if not self.dataset_tag:
            value.pop("dataset_tag")
        if self.warmup_examples == 0:
            value.pop("warmup_examples")
        if self.cooldown_examples == 0:
            value.pop("cooldown_examples")
        if not self.optimizer_role_lr_multipliers:
            value.pop("optimizer_role_lr_multipliers")
        if not self.optimizer_role_weight_decays:
            value.pop("optimizer_role_weight_decays")
        if self.temperature == 1.0:
            value.pop("temperature")
        if self.temperature2_weight == 0.0:
            value.pop("temperature2_weight")
        if self.hidden_mse_weight == 0.0:
            value.pop("hidden_mse_weight")
        if not self.use_teacher_cache:
            value.pop("use_teacher_cache")
        if self.objective == "forward_kl":
            value.pop("objective")
        if self.embedding_initialization == "frozen_qwen":
            value.pop("embedding_initialization")
        if not self.hydra_config_hash:
            value.pop("hydra_config_hash")
        if not self.hydra_resolved_config_json:
            value.pop("hydra_resolved_config_json")
        return value


def microbatch_for(
    architecture: ArchitectureConfig,
    effective_batch: int = EFFECTIVE_BATCH,
) -> tuple[int, int]:
    if architecture.operator in ("kronecker", "hybrid"):
        # Modal can raise this after an H100 memory sweep. Keep the serialized
        # optimization batch distinct from this physical execution choice.
        limit = int(os.environ.get("QWEN_KRONECKER_MICROBATCH", 64))
        if limit <= 0:
            raise ValueError("Kronecker microbatch limit must be positive")
        microbatch = min(limit, effective_batch)
    elif architecture.operator == "btt":
        microbatch = min(16, effective_batch)
    elif architecture.form == "residual_ffn" and architecture.expansion == 4:
        microbatch = min(16, effective_batch)
    elif architecture.operator == "dense" or architecture.monarch_rank > 1:
        microbatch = min(64, effective_batch)
    else:
        microbatch = min(256, effective_batch)
    if effective_batch <= 0:
        raise ValueError("effective batch must be positive")
    return microbatch, math.ceil(effective_batch / microbatch)


def same_width_screen() -> list[ArchitectureConfig]:
    dense = [
        *[ArchitectureConfig("dense", "sequential", d) for d in (1, 2)],
        *[ArchitectureConfig("dense", "residual_one", d) for d in (1, 2)],
        *[ArchitectureConfig("dense", "residual_ffn", d, 1) for d in (1, 2)],
        ArchitectureConfig("dense", "residual_ffn", 1, 4),
    ]
    monarch = [
        *[ArchitectureConfig("monarch", "sequential", d) for d in (1, 2, 4, 8)],
        *[ArchitectureConfig("monarch", "residual_one", d) for d in (1, 2, 4, 8)],
        *[ArchitectureConfig("monarch", "residual_ffn", d, 1) for d in (1, 2, 4, 8)],
        *[ArchitectureConfig("monarch", "residual_ffn", d, 4) for d in (1, 2, 4, 8)],
    ]
    result = dense + monarch
    for config in result:
        config.validate()
    return result


def matching_monarch(dense: ArchitectureConfig) -> ArchitectureConfig:
    if dense.operator != "dense":
        raise ValueError("parameter matching requires a dense reference")
    # Square factors are exactly 64x smaller. Rectangular factors at expansion
    # four are 102.4x smaller, and rank 102 is the nearest integer match.
    rank = 102 if dense.form == "residual_ffn" and dense.expansion == 4 else 64
    return ArchitectureConfig(
        "monarch", dense.form, dense.depth, dense.expansion,
        dense.context_length, dense.embedding_width, dense.monarch_blocks, rank,
    )
