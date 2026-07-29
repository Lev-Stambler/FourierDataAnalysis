from __future__ import annotations

import hashlib
import json
import math
import os
from dataclasses import asdict, dataclass
from typing import Literal

from qwen_normuon_pretrain.config import (
    ADAMW_BETAS,
    FINEWEB_EDU_CONFIG,
    FINEWEB_EDU_ID,
    FINEWEB_EDU_REVISION,
    MODEL_ID,
    MODEL_REVISION,
    NORMUON_BETA1,
    NORMUON_BETA2,
    OPTIMIZER_EPS,
    TEST_EXAMPLES,
    VALIDATION_EXAMPLES,
    VOCAB_SIZE,
    WEIGHT_DECAY,
)
from qwen_normuon_pretrain.config import (
    TRAIN_EXAMPLES as DATASET_TRAIN_EXAMPLES,
)

CONTEXT_LENGTH = 16
EMBEDDING_WIDTH = 64
STUDY_VARIANT = os.environ.get("QWEN_KRON_STUDY", "v3-khatri-rao")
SUPPORTED_STUDY_VARIANTS = (
    "v3-khatri-rao",
    "v4-wide",
    "v4-isolated",
    "v5-normuon-lr",
    "v6-dense-tied",
)
if STUDY_VARIANT not in SUPPORTED_STUDY_VARIANTS:
    raise RuntimeError(
        f"QWEN_KRON_STUDY must be one of {SUPPORTED_STUDY_VARIANTS}"
    )
WIDE_WIDTHS = (128, 256, 384)
ISOLATED_WIDTHS = (64, 128, 256, 384)
WIDE_LR_PAIRS = (
    (3e-2, 3e-3),
    (1e-1, 1e-2),
)
NORMUON_ONLY_LRS = (1e-1, 2e-1, 3e-1, 5e-1)
DENSE_VOCABULARY_WIDTHS = (64, 128, 256)
DENSE_VOCABULARY_LR = 2e-1
NORMUON_ONLY_VARIANTS = ("v5-normuon-lr", "v6-dense-tied")
# Eight H200s each process LOCAL_BATCH contexts. At 16 input tokens this is an
# exact 1 Mi-token global optimizer batch with no gradient accumulation.
WORLD_SIZE = 8
GLOBAL_BATCH = 65_536
LOCAL_BATCH = GLOBAL_BATCH // WORLD_SIZE
GLOBAL_TOKEN_BATCH = GLOBAL_BATCH * CONTEXT_LENGTH
PREFLIGHT_MICROBATCHES = (8_192, 4_096, 2_048, 1_024)
MINIMUM_TOKEN_BATCH = 100_000
UNDERFILLED_BASELINE_CONTEXTS_PER_SECOND = 1_209.449
TARGET_THROUGHPUT_MULTIPLIER = 10.0
MINIMUM_ACCEPTED_THROUGHPUT_MULTIPLIER = 5.0
VOCAB_CHUNK_SIZE = 32_768
EVALUATION_BATCH = 1_024
TEACHER_MICROBATCH = 4_096
VOCAB_FACTOR_MODES = (485, 512)
if math.prod(VOCAB_FACTOR_MODES) != VOCAB_SIZE:
    raise RuntimeError("vocabulary factor modes must exactly cover Qwen's vocabulary")
STUDENT_LOSS_IMPLEMENTATION = "exact_materialized_khatri_rao_full_vocab"
TEACHER_VOCAB_ROWS_PER_CHUNK = 64
DEPTHS = (1, 2, 4, 8, 16, 32)
RANKS = (1, 2, 4, 8)
SCREEN_EXAMPLES = 2_097_152
MID_EXAMPLES = 16_777_216
FINAL_EXAMPLES = 67_108_864
EXTENSION_2X_EXAMPLES = 134_217_728
EXTENSION_4X_EXAMPLES = 268_435_456
EXTENSION_EXAMPLES = (EXTENSION_2X_EXAMPLES, EXTENSION_4X_EXAMPLES)
MAX_OPTIMIZATION_EXAMPLES = (
    EXTENSION_4X_EXAMPLES
    if STUDY_VARIANT == "v6-dense-tied"
    else FINAL_EXAMPLES
)
CHECKPOINT_EVERY_EXAMPLES = 1_048_576
WARMUP_EXAMPLES = 262_144
COOLDOWN_EXAMPLES = 8_388_608
STABLE_EXAMPLES = (
    MAX_OPTIMIZATION_EXAMPLES - WARMUP_EXAMPLES - COOLDOWN_EXAMPLES
)
NORMUON_LR = float(os.environ.get("QWEN_KRON_NORMUON_LR", "0.01"))
AUX_ADAMW_LR = float(os.environ.get("QWEN_KRON_AUX_LR", "0.001"))
LR_PAIRS = (
    (3e-3, 3e-4),
    (1e-2, 1e-3),
    (3e-2, 3e-3),
)
GRADIENT_CLIP_NORM = 1.0
MIN_CONTINUATION_IMPROVEMENT = 1e-3
MIN_EXTENSION_IMPROVEMENT = 0.1
INITIALIZER_RANGE = 0.02
VOCABULARY_FACTOR_INITIALIZER_STD = math.sqrt(INITIALIZER_RANGE)
TARGET_VALIDATION_KL = 1.0

DEFAULT_DATA_ROOT = (
    "/cache/qwen_fullwidth_distill/context16-fineweb-edu-next-token-4m-v1"
)
DEFAULT_OUTPUT_ROOT = (
    f"/cache/exp5_kronecker_distill/{STUDY_VARIANT}"
)
WANDB_PROJECT = "qwen-kronecker-distill"
PLAN_SCHEMA = "qwen-kron-distill-plan-v1"
RESULT_SCHEMA = "qwen-kron-distill-result-v1"
CHECKPOINT_SCHEMA = "qwen-kron-distill-checkpoint-v1"
SUMMARY_SCHEMA = "qwen-kron-distill-summary-v1"

FactorOrder = Literal[2, 3]
Stage = Literal[
    "depth",
    "rank",
    "width",
    "lr",
    "mid",
    "final",
    "extend2x",
    "extend4x",
]


def learning_rate_tag(value: float) -> str:
    coefficient, exponent = f"{value:.0e}".split("e")
    return f"{coefficient}e{int(exponent)}"


def canonical_hash(value: dict) -> str:
    payload = json.dumps(
        value,
        sort_keys=True,
        separators=(",", ":"),
        allow_nan=False,
    ).encode()
    return hashlib.sha256(payload).hexdigest()


@dataclass(frozen=True)
class Architecture:
    factor_order: FactorOrder
    depth: int
    rank: int = 1
    context_length: int = CONTEXT_LENGTH
    embedding_width: int = EMBEDDING_WIDTH
    vocabulary_width: int | None = None

    @property
    def input_modes(self) -> tuple[int, ...]:
        if self.factor_order == 2:
            return (self.context_length, self.embedding_width)
        side = math.isqrt(self.embedding_width)
        return (self.context_length, side, side)

    @property
    def label(self) -> str:
        width = (
            ""
            if self.embedding_width == EMBEDDING_WIDTH
            else f"-w{self.embedding_width}"
        )
        vocabulary = (
            ""
            if self.vocabulary_width is None
            else f"-v{self.vocabulary_width}"
        )
        return (
            f"kron-o{self.factor_order}-r{self.rank}-d{self.depth}"
            f"{width}{vocabulary}"
        )

    @property
    def residual_multiplier(self) -> float:
        return self.depth**-0.5

    def validate(self) -> None:
        if self.factor_order not in (2, 3):
            raise ValueError("factor order must be two or three")
        if self.depth not in DEPTHS:
            raise ValueError(f"depth must be one of {DEPTHS}")
        if self.rank not in RANKS:
            raise ValueError(f"rank must be one of {RANKS}")
        if self.context_length != CONTEXT_LENGTH:
            raise ValueError("experiment 5 requires exactly 16 input tokens")
        if (
            self.embedding_width < EMBEDDING_WIDTH
            or self.embedding_width % EMBEDDING_WIDTH
        ):
            raise ValueError("embedding width must be a positive multiple of 64")
        if self.factor_order == 3:
            side = math.isqrt(self.embedding_width)
            if side * side != self.embedding_width:
                raise ValueError("order three needs a square embedding width")
        if (
            self.vocabulary_width is not None
            and (
                self.vocabulary_width < EMBEDDING_WIDTH
                or self.vocabulary_width % EMBEDDING_WIDTH
            )
        ):
            raise ValueError(
                "vocabulary width must be a positive multiple of 64"
            )

    def to_dict(self) -> dict:
        value = asdict(self)
        if self.vocabulary_width is None:
            value.pop("vocabulary_width")
        return value


@dataclass(frozen=True)
class Cell:
    stage: Stage
    architecture: Architecture
    target_examples: int
    factor_lr: float = NORMUON_LR
    auxiliary_lr: float = AUX_ADAMW_LR
    seed: int = 0

    @property
    def label(self) -> str:
        return (
            f"{self.stage}-{self.architecture.label}"
            f"-nlr{learning_rate_tag(self.factor_lr)}"
            f"-alr{learning_rate_tag(self.auxiliary_lr)}"
            f"-b{GLOBAL_BATCH}-s{self.seed}"
        )

    def validate(self) -> None:
        self.architecture.validate()
        if self.stage not in (
            "depth",
            "rank",
            "width",
            "lr",
            "mid",
            "final",
            "extend2x",
            "extend4x",
        ):
            raise ValueError(f"unsupported stage {self.stage}")
        if self.target_examples not in (
            SCREEN_EXAMPLES,
            MID_EXAMPLES,
            FINAL_EXAMPLES,
            *EXTENSION_EXAMPLES,
        ):
            raise ValueError("cell target is not a committed study milestone")
        extension_targets = {
            "extend2x": EXTENSION_2X_EXAMPLES,
            "extend4x": EXTENSION_4X_EXAMPLES,
        }
        if (
            self.stage in extension_targets
            and self.target_examples != extension_targets[self.stage]
        ):
            raise ValueError("extension stage and target do not match")
        if self.seed != 0:
            raise ValueError("the first study is pinned to seed zero")
        if self.factor_lr <= 0 or self.auxiliary_lr <= 0:
            raise ValueError("learning rates must be positive")

    def to_dict(self) -> dict:
        return {
            "stage": self.stage,
            "architecture": self.architecture.to_dict(),
            "target_examples": self.target_examples,
            "factor_lr": self.factor_lr,
            "auxiliary_lr": self.auxiliary_lr,
            "seed": self.seed,
            "label": self.label,
        }


def depth_cells() -> list[Cell]:
    cells = [
        Cell(
            stage="depth",
            architecture=Architecture(
                factor_order=order,
                depth=depth,
                rank=1,
            ),
            target_examples=SCREEN_EXAMPLES,
        )
        for order in (2, 3)
        for depth in DEPTHS
    ]
    for cell in cells:
        cell.validate()
    return cells


def rank_cells(selected_depths: dict[int, int]) -> list[Cell]:
    if set(selected_depths) != {2, 3}:
        raise ValueError("selected depths must cover factor orders two and three")
    cells = [
        Cell(
            stage="rank",
            architecture=Architecture(
                factor_order=order,
                depth=int(selected_depths[order]),
                rank=rank,
            ),
            target_examples=SCREEN_EXAMPLES,
        )
        for order in (2, 3)
        for rank in (2, 4, 8)
    ]
    for cell in cells:
        cell.validate()
    return cells


def push_lr_cells() -> list[Cell]:
    cells = [
        Cell(
            stage="depth",
            architecture=Architecture(
                factor_order=2,
                depth=32,
                rank=8,
            ),
            target_examples=SCREEN_EXAMPLES,
            factor_lr=factor_lr,
            auxiliary_lr=auxiliary_lr,
        )
        for factor_lr, auxiliary_lr in LR_PAIRS
    ]
    for cell in cells:
        cell.validate()
    return cells


def push_wide_cells() -> list[Cell]:
    widths = (
        ISOLATED_WIDTHS
        if STUDY_VARIANT == "v4-isolated"
        else WIDE_WIDTHS
    )
    cells = [
        Cell(
            stage="width",
            architecture=Architecture(
                factor_order=2,
                depth=32,
                rank=8,
                embedding_width=width,
            ),
            target_examples=SCREEN_EXAMPLES,
            factor_lr=factor_lr,
            auxiliary_lr=auxiliary_lr,
        )
        for width in widths
        for factor_lr, auxiliary_lr in WIDE_LR_PAIRS
        if (
            STUDY_VARIANT == "v4-isolated"
            or width != WIDE_WIDTHS[0]
            or factor_lr == WIDE_LR_PAIRS[0][0]
        )
    ]
    for cell in cells:
        cell.validate()
    return cells


def push_normuon_lr_cells() -> list[Cell]:
    cells = [
        Cell(
            stage="lr",
            architecture=Architecture(
                factor_order=2,
                depth=32,
                rank=8,
                embedding_width=64,
            ),
            target_examples=SCREEN_EXAMPLES,
            factor_lr=learning_rate,
            auxiliary_lr=learning_rate,
        )
        for learning_rate in NORMUON_ONLY_LRS
    ]
    for cell in cells:
        cell.validate()
    return cells


def push_dense_vocabulary_cells() -> list[Cell]:
    cells = [
        Cell(
            stage="width",
            architecture=Architecture(
                factor_order=2,
                depth=32,
                rank=8,
                embedding_width=64,
                vocabulary_width=width,
            ),
            target_examples=SCREEN_EXAMPLES,
            factor_lr=DENSE_VOCABULARY_LR,
            auxiliary_lr=DENSE_VOCABULARY_LR,
        )
        for width in DENSE_VOCABULARY_WIDTHS
    ]
    for cell in cells:
        cell.validate()
    return cells


def preflight_architecture() -> Architecture:
    if STUDY_VARIANT == "v6-dense-tied":
        architecture = Architecture(
            factor_order=2,
            depth=32,
            rank=8,
            embedding_width=64,
            vocabulary_width=max(DENSE_VOCABULARY_WIDTHS),
        )
        architecture.validate()
        return architecture
    architecture = Architecture(
        factor_order=2,
        depth=32,
        rank=8,
        embedding_width=(
            max(ISOLATED_WIDTHS)
            if STUDY_VARIANT in ("v4-wide", "v4-isolated")
            else EMBEDDING_WIDTH
        ),
    )
    architecture.validate()
    return architecture


def wsd_multiplier(examples_seen: int) -> float:
    if not 0 < examples_seen <= MAX_OPTIMIZATION_EXAMPLES:
        raise ValueError("examples_seen must be in the training budget")
    if examples_seen <= WARMUP_EXAMPLES:
        return examples_seen / WARMUP_EXAMPLES
    stable_end = WARMUP_EXAMPLES + STABLE_EXAMPLES
    if examples_seen <= stable_end:
        return 1.0
    return (
        MAX_OPTIMIZATION_EXAMPLES - examples_seen
    ) / COOLDOWN_EXAMPLES


def study_plan() -> dict:
    if STUDY_VARIANT == "v6-dense-tied":
        value = {
            "schema": PLAN_SCHEMA,
            "status": "planned",
            "study_variant": STUDY_VARIANT,
            "teacher": {
                "model_id": MODEL_ID,
                "model_revision": MODEL_REVISION,
                "live": True,
                "temperature": 1.0,
                "objective": "exact_full_vocabulary_forward_kl",
            },
            "data": {
                "dataset_id": FINEWEB_EDU_ID,
                "dataset_config": FINEWEB_EDU_CONFIG,
                "dataset_revision": FINEWEB_EDU_REVISION,
                "context_length": CONTEXT_LENGTH,
                "dataset_train_examples": DATASET_TRAIN_EXAMPLES,
                "optimization_examples": MAX_OPTIMIZATION_EXAMPLES,
                "milestone_examples": [
                    SCREEN_EXAMPLES,
                    MID_EXAMPLES,
                    FINAL_EXAMPLES,
                    *EXTENSION_EXAMPLES,
                ],
                "validation_examples": VALIDATION_EXAMPLES,
                "test_examples": TEST_EXAMPLES,
            },
            "student": {
                "body_embedding_width": EMBEDDING_WIDTH,
                "vocabulary_widths": list(DENSE_VOCABULARY_WIDTHS),
                "depth": 32,
                "rank": 8,
                "factor_order": 2,
                "vocab_size": VOCAB_SIZE,
                "tied_embedding": True,
                "vocabulary_distribution": "dense_tied_low_dimension",
                "vocabulary_bridge": (
                    "learned_input_compression_and_output_expansion"
                ),
                "tensor_native_body": True,
            },
            "optimizer": {
                "all_trainable_parameters": "normuon",
                "adamw": False,
                "learning_rate": DENSE_VOCABULARY_LR,
                "factor_betas": [NORMUON_BETA1, NORMUON_BETA2],
                "factor_state": "independent_matrix_or_matrix_batch",
                "epsilon": OPTIMIZER_EPS,
                "weight_decay": WEIGHT_DECAY,
                "gradient_clip_norm": GRADIENT_CLIP_NORM,
                "schedule": {
                    "kind": "wsd_by_examples",
                    "warmup": WARMUP_EXAMPLES,
                    "stable": STABLE_EXAMPLES,
                    "cooldown": COOLDOWN_EXAMPLES,
                },
            },
            "runtime": {
                "world_size": WORLD_SIZE,
                "global_batch": GLOBAL_BATCH,
                "global_token_batch": GLOBAL_TOKEN_BATCH,
                "local_batch": LOCAL_BATCH,
                "preflight_microbatches": list(PREFLIGHT_MICROBATCHES),
                "minimum_token_batch": MINIMUM_TOKEN_BATCH,
                "student_loss_implementation": (
                    "exact_materialized_dense_tied_full_vocab"
                ),
            },
            "dense_vocabulary_cells": [
                cell.to_dict() for cell in push_dense_vocabulary_cells()
            ],
            "selection": {
                "screen": "lowest_validation_kl",
                "mid": "smallest_model_within_0.1_kl_of_best",
                "extensions": (
                    "continue_while_above_target_and_validation_kl_improves_"
                    "by_at_least_0.1"
                ),
                "extension_examples": list(EXTENSION_EXAMPLES),
                "minimum_extension_improvement": (
                    MIN_EXTENSION_IMPROVEMENT
                ),
                "target_validation_kl": TARGET_VALIDATION_KL,
                "minimum_continuation_improvement": (
                    MIN_CONTINUATION_IMPROVEMENT
                ),
            },
        }
        value["plan_sha256"] = canonical_hash(value)
        return value
    if STUDY_VARIANT == "v5-normuon-lr":
        value = {
            "schema": PLAN_SCHEMA,
            "status": "planned",
            "study_variant": STUDY_VARIANT,
            "teacher": {
                "model_id": MODEL_ID,
                "model_revision": MODEL_REVISION,
                "live": True,
                "temperature": 1.0,
                "objective": "exact_full_vocabulary_forward_kl",
            },
            "data": {
                "dataset_id": FINEWEB_EDU_ID,
                "dataset_config": FINEWEB_EDU_CONFIG,
                "dataset_revision": FINEWEB_EDU_REVISION,
                "context_length": CONTEXT_LENGTH,
                "dataset_train_examples": DATASET_TRAIN_EXAMPLES,
                "optimization_examples": FINAL_EXAMPLES,
                "validation_examples": VALIDATION_EXAMPLES,
                "test_examples": TEST_EXAMPLES,
            },
            "student": {
                "embedding_width": 64,
                "depth": 32,
                "rank": 8,
                "factor_order": 2,
                "vocab_size": VOCAB_SIZE,
                "tied_embedding": True,
                "vocabulary_factor_modes": list(VOCAB_FACTOR_MODES),
                "vocabulary_distribution": "tied_khatri_rao_cp_width",
                "tensor_native": True,
            },
            "optimizer": {
                "all_trainable_parameters": "normuon",
                "adamw": False,
                "learning_rate_grid": list(NORMUON_ONLY_LRS),
                "factor_betas": [NORMUON_BETA1, NORMUON_BETA2],
                "factor_state": "independent_matrix_or_matrix_batch",
                "epsilon": OPTIMIZER_EPS,
                "weight_decay": WEIGHT_DECAY,
                "gradient_clip_norm": GRADIENT_CLIP_NORM,
                "schedule": {
                    "kind": "wsd_by_examples",
                    "warmup": WARMUP_EXAMPLES,
                    "stable": STABLE_EXAMPLES,
                    "cooldown": COOLDOWN_EXAMPLES,
                },
            },
            "runtime": {
                "world_size": WORLD_SIZE,
                "global_batch": GLOBAL_BATCH,
                "global_token_batch": GLOBAL_TOKEN_BATCH,
                "local_batch": LOCAL_BATCH,
                "preflight_microbatches": list(PREFLIGHT_MICROBATCHES),
                "minimum_token_batch": MINIMUM_TOKEN_BATCH,
                "student_loss_implementation": STUDENT_LOSS_IMPLEMENTATION,
            },
            "push_normuon_lr_cells": [
                cell.to_dict() for cell in push_normuon_lr_cells()
            ],
            "selection": {
                "lr": "lowest_validation_kl",
                "minimum_continuation_improvement": (
                    MIN_CONTINUATION_IMPROVEMENT
                ),
            },
        }
        value["plan_sha256"] = canonical_hash(value)
        return value
    wide = STUDY_VARIANT in ("v4-wide", "v4-isolated")
    planned_widths = (
        list(ISOLATED_WIDTHS)
        if STUDY_VARIANT == "v4-isolated"
        else list(WIDE_WIDTHS)
        if wide
        else [EMBEDDING_WIDTH]
    )
    lr_pairs = WIDE_LR_PAIRS if wide else LR_PAIRS
    value = {
        "schema": PLAN_SCHEMA,
        "status": "planned",
        "study_variant": STUDY_VARIANT,
        "teacher": {
            "model_id": MODEL_ID,
            "model_revision": MODEL_REVISION,
            "live": True,
            "temperature": 1.0,
            "objective": "exact_full_vocabulary_forward_kl",
        },
        "data": {
            "dataset_id": FINEWEB_EDU_ID,
            "dataset_config": FINEWEB_EDU_CONFIG,
            "dataset_revision": FINEWEB_EDU_REVISION,
            "context_length": CONTEXT_LENGTH,
            "dataset_train_examples": DATASET_TRAIN_EXAMPLES,
            "optimization_examples": FINAL_EXAMPLES,
            "validation_examples": VALIDATION_EXAMPLES,
            "test_examples": TEST_EXAMPLES,
        },
        "student": {
            "embedding_width": (
                "selected_by_screen" if wide else EMBEDDING_WIDTH
            ),
            "embedding_widths": planned_widths,
            "vocab_size": VOCAB_SIZE,
            "tied_embedding": True,
            "implicit_full_vocabulary_head": True,
            "vocabulary_factor_modes": list(VOCAB_FACTOR_MODES),
            "vocabulary_distribution": "tied_khatri_rao_cp_width",
            "vocabulary_factor_parameters_by_width": {
                str(width): sum(VOCAB_FACTOR_MODES) * width
                for width in planned_widths
            },
            "embedding_initialization": {
                "distribution": "normal",
                "factor_std": VOCABULARY_FACTOR_INITIALIZER_STD,
                "combined_embedding_std": INITIALIZER_RANGE,
                "seed": 0,
            },
            "tensor_native": True,
            "terminal_context_outputs": 1,
            "factor_orders": [2] if wide else [2, 3],
            "ranks": list(RANKS),
            "depths": list(DEPTHS),
        },
        "optimizer": {
            "factor": "normuon",
            "factor_lr_grid": [value[0] for value in lr_pairs],
            "factor_betas": [NORMUON_BETA1, NORMUON_BETA2],
            "factor_state": "independent_rank_slice_matrix",
            "auxiliary": "fused_adamw",
            "auxiliary_lr_grid": [value[1] for value in lr_pairs],
            "auxiliary_betas": list(ADAMW_BETAS),
            "epsilon": OPTIMIZER_EPS,
            "weight_decay": WEIGHT_DECAY,
            "gradient_clip_norm": GRADIENT_CLIP_NORM,
            "schedule": {
                "kind": "wsd_by_examples",
                "warmup": WARMUP_EXAMPLES,
                "stable": STABLE_EXAMPLES,
                "cooldown": COOLDOWN_EXAMPLES,
            },
        },
        "runtime": {
            "world_size": WORLD_SIZE,
            "global_batch": GLOBAL_BATCH,
            "global_token_batch": GLOBAL_TOKEN_BATCH,
            "local_batch": LOCAL_BATCH,
            "preflight_microbatches": list(PREFLIGHT_MICROBATCHES),
            "minimum_token_batch": MINIMUM_TOKEN_BATCH,
            "vocab_chunk_size": VOCAB_CHUNK_SIZE,
            "evaluation_batch": EVALUATION_BATCH,
            "teacher_microbatch": TEACHER_MICROBATCH,
            "teacher_vocab_rows_per_chunk": TEACHER_VOCAB_ROWS_PER_CHUNK,
            "student_loss_implementation": STUDENT_LOSS_IMPLEMENTATION,
            "underfilled_baseline_contexts_per_second": (
                UNDERFILLED_BASELINE_CONTEXTS_PER_SECOND
            ),
            "target_throughput_multiplier": TARGET_THROUGHPUT_MULTIPLIER,
            "minimum_accepted_throughput_multiplier": (
                MINIMUM_ACCEPTED_THROUGHPUT_MULTIPLIER
            ),
            "teacher_forward_shared_across_cells": True,
        },
        "depth_cells": (
            [] if wide else [cell.to_dict() for cell in depth_cells()]
        ),
        "push_lr_cells": (
            [] if wide else [cell.to_dict() for cell in push_lr_cells()]
        ),
        "push_wide_cells": (
            [cell.to_dict() for cell in push_wide_cells()] if wide else []
        ),
        "selection": {
            "depth": "lowest_validation_kl_then_lower_depth",
            "rank": "lowest_validation_kl_then_lower_rank",
            "overall": "lowest_validation_kl_then_parameters_rank_depth",
            "minimum_continuation_improvement": MIN_CONTINUATION_IMPROVEMENT,
        },
    }
    if STUDY_VARIANT == "v4-isolated":
        value["selection"]["isolated_width_mid"] = (
            "smallest_model_within_0.1_kl_of_best"
        )
    value["plan_sha256"] = canonical_hash(value)
    return value
