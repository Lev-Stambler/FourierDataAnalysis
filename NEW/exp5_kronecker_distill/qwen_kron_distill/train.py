from __future__ import annotations

import contextlib
import hashlib
import json
import os
import time
from dataclasses import dataclass
from pathlib import Path

import numpy as np
import torch
import torch.distributed as dist
from qwen_normuon_pretrain.config import (
    ADAMW_BETAS,
    NORMUON_BETA1,
    NORMUON_BETA2,
    OPTIMIZER_EPS,
    WEIGHT_DECAY,
)
from qwen_normuon_pretrain.data import load_manifest, load_split
from qwen_normuon_pretrain.normuon import (
    SOURCE_COMMIT as NORMUON_SOURCE_COMMIT,
)
from qwen_normuon_pretrain.normuon import (
    SingleDeviceNorMuon,
)
from torch.nn.parallel import DistributedDataParallel

from .config import (
    AUX_ADAMW_LR,
    CHECKPOINT_EVERY_EXAMPLES,
    CHECKPOINT_SCHEMA,
    CONTEXT_LENGTH,
    DATASET_TRAIN_EXAMPLES,
    DEFAULT_DATA_ROOT,
    DEFAULT_OUTPUT_ROOT,
    EVALUATION_BATCH,
    FINAL_EXAMPLES,
    GLOBAL_BATCH,
    GLOBAL_TOKEN_BATCH,
    GRADIENT_CLIP_NORM,
    LOCAL_BATCH,
    MODEL_ID,
    MODEL_REVISION,
    NORMUON_LR,
    NORMUON_ONLY_LRS,
    PLAN_SCHEMA,
    RESULT_SCHEMA,
    SCREEN_EXAMPLES,
    STUDY_VARIANT,
    TEST_EXAMPLES,
    VALIDATION_EXAMPLES,
    WANDB_PROJECT,
    WARMUP_EXAMPLES,
    WIDE_LR_PAIRS,
    WORLD_SIZE,
    Architecture,
    Cell,
    canonical_hash,
    preflight_architecture,
    study_plan,
    wsd_multiplier,
)
from .model import TensorKroneckerStudent
from .objective import (
    distribution_metrics,
    exact_kl_rows,
    khatri_rao_forward_kl,
    load_teacher,
    teacher_distribution,
    teacher_probability_targets,
    validate_probability_rows,
)


def atomic_json(path: Path, value: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_text(json.dumps(value, indent=2, sort_keys=True))
    os.replace(temporary, path)


def atomic_torch_save(path: Path, value: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    torch.save(value, temporary)
    os.replace(temporary, path)


def tensor_sha256(tensor: torch.Tensor) -> str:
    value = tensor.detach().contiguous().cpu()
    return hashlib.sha256(value.view(torch.uint8).numpy().tobytes()).hexdigest()


def cell_from_dict(value: dict) -> Cell:
    payload = dict(value)
    payload.pop("label", None)
    architecture = Architecture(**payload.pop("architecture"))
    cell = Cell(architecture=architecture, **payload)
    cell.validate()
    return cell


def result_path(output_root: str, cell: Cell) -> Path:
    return Path(output_root) / cell.stage / cell.label / "result.json"


def checkpoint_path(output_root: str, cell: Cell) -> Path:
    return Path(output_root) / cell.stage / cell.label / "checkpoint.pt"


def student_path(output_root: str, cell: Cell) -> Path:
    return Path(output_root) / cell.stage / cell.label / "student.pt"


def completed_result(output_root: str, cell: Cell) -> dict | None:
    path = result_path(output_root, cell)
    try:
        value = json.loads(path.read_text())
    except (FileNotFoundError, json.JSONDecodeError, OSError):
        return None
    digest = value.get("result_sha256")
    unsigned = dict(value)
    unsigned.pop("result_sha256", None)
    if (
        value.get("schema") != RESULT_SCHEMA
        or value.get("status") != "complete"
        or value.get("label") != cell.label
        or int(value.get("examples_seen", -1)) != cell.target_examples
        or not student_path(output_root, cell).is_file()
        or digest != canonical_hash(unsigned)
    ):
        return None
    return value


def deterministic_order(
    size: int,
    seed: int = 0,
    *,
    total_examples: int = FINAL_EXAMPLES,
) -> np.ndarray:
    if size <= 0 or total_examples <= 0:
        raise ValueError("training order sizes must be positive")
    generator = np.random.default_rng(seed)
    epochs = []
    remaining = total_examples
    while remaining:
        permutation = generator.permutation(size)
        take = min(size, remaining)
        epochs.append(permutation[:take])
        remaining -= take
    return np.concatenate(epochs)


@dataclass
class DistributedContext:
    rank: int
    local_rank: int
    world_size: int
    device: torch.device
    owns_process_group: bool

    @property
    def primary(self) -> bool:
        return self.rank == 0


def distributed_context() -> DistributedContext:
    world_size = int(os.environ.get("WORLD_SIZE", "1"))
    rank = int(os.environ.get("RANK", "0"))
    local_rank = int(os.environ.get("LOCAL_RANK", "0"))
    owns = False
    if world_size > 1 and not dist.is_initialized():
        torch.cuda.set_device(local_rank)
        dist.init_process_group(backend="nccl")
        owns = True
    device = torch.device(f"cuda:{local_rank}" if torch.cuda.is_available() else "cpu")
    return DistributedContext(
        rank=rank,
        local_rank=local_rank,
        world_size=world_size,
        device=device,
        owns_process_group=owns,
    )


def close_distributed(context: DistributedContext) -> None:
    if context.owns_process_group and dist.is_initialized():
        dist.barrier()
        dist.destroy_process_group()


def unwrap_model(model):
    return model.module if isinstance(model, DistributedDataParallel) else model


def make_optimizers(
    student: TensorKroneckerStudent,
    *,
    device: torch.device,
    factor_lr: float = NORMUON_LR,
    auxiliary_lr: float = AUX_ADAMW_LR,
):
    factors = student.factor_parameters()
    auxiliary = student.auxiliary_parameters()
    factor_ids = {id(value) for value in factors}
    auxiliary_ids = {id(value) for value in auxiliary}
    if factor_ids & auxiliary_ids:
        raise RuntimeError("factor and auxiliary optimizer routes overlap")
    if factor_ids | auxiliary_ids != {id(value) for value in student.parameters()}:
        raise RuntimeError("optimizer routes do not cover the student")
    factor_optimizer = SingleDeviceNorMuon(
        factors,
        lr=factor_lr,
        weight_decay=WEIGHT_DECAY,
        beta1=NORMUON_BETA1,
        beta2=NORMUON_BETA2,
        ns_steps=5,
        nesterov=True,
        eps=OPTIMIZER_EPS,
    )
    if STUDY_VARIANT == "v5-normuon-lr":
        if any(value.ndim < 2 for value in auxiliary):
            raise RuntimeError(
                "NorMuon-only routing requires every trainable tensor "
                "to contain matrices"
            )
        auxiliary_optimizer = SingleDeviceNorMuon(
            auxiliary,
            lr=factor_lr,
            weight_decay=WEIGHT_DECAY,
            beta1=NORMUON_BETA1,
            beta2=NORMUON_BETA2,
            ns_steps=5,
            nesterov=True,
            eps=OPTIMIZER_EPS,
        )
    else:
        auxiliary_optimizer = torch.optim.AdamW(
            auxiliary,
            lr=auxiliary_lr,
            betas=ADAMW_BETAS,
            eps=OPTIMIZER_EPS,
            weight_decay=WEIGHT_DECAY,
            fused=device.type == "cuda",
        )
    return factor_optimizer, auxiliary_optimizer


def set_learning_rates(
    factor_optimizer,
    auxiliary_optimizer,
    examples_seen: int,
    *,
    factor_base_lr: float = NORMUON_LR,
    auxiliary_base_lr: float = AUX_ADAMW_LR,
) -> dict[str, float]:
    multiplier = wsd_multiplier(examples_seen)
    factor_lr = factor_base_lr * multiplier
    auxiliary_lr = auxiliary_base_lr * multiplier
    for group in factor_optimizer.param_groups:
        group["lr"] = factor_lr
    for group in auxiliary_optimizer.param_groups:
        group["lr"] = auxiliary_lr
    return {
        "multiplier": multiplier,
        "factor_lr": factor_lr,
        "auxiliary_lr": auxiliary_lr,
    }


@dataclass
class ActiveCell:
    cell: Cell
    model: torch.nn.Module
    factor_optimizer: torch.optim.Optimizer
    auxiliary_optimizer: torch.optim.Optimizer
    initial_hashes: dict[str, str]
    initial_validation: dict
    source_label: str | None
    examples_seen: int = 0


def _checkpoint_value(active: ActiveCell, elapsed: float) -> dict:
    student = unwrap_model(active.model)
    return {
        "schema": CHECKPOINT_SCHEMA,
        "cell": active.cell.to_dict(),
        "model_state_dict": student.state_dict(),
        "factor_optimizer_state_dict": active.factor_optimizer.state_dict(),
        "auxiliary_optimizer_state_dict": active.auxiliary_optimizer.state_dict(),
        "examples_seen": active.examples_seen,
        "initial_hashes": active.initial_hashes,
        "initial_validation": active.initial_validation,
        "source_label": active.source_label,
        "elapsed_wall_seconds": elapsed,
        "torch_rng_state": torch.get_rng_state(),
        "numpy_seed": active.cell.seed,
    }


def save_active_checkpoint(
    output_root: str,
    active: ActiveCell,
    elapsed: float,
) -> None:
    atomic_torch_save(
        checkpoint_path(output_root, active.cell),
        _checkpoint_value(active, elapsed),
    )


def _load_checkpoint(
    path: Path,
    active: ActiveCell,
    *,
    require_current_cell: bool,
) -> dict | None:
    if not path.is_file():
        return None
    value = torch.load(
        path,
        map_location="cpu",
        mmap=True,
        weights_only=True,
    )
    if value.get("schema") != CHECKPOINT_SCHEMA:
        raise RuntimeError(f"checkpoint schema mismatch at {path}")
    stored_cell = cell_from_dict(value["cell"])
    if require_current_cell and stored_cell.to_dict() != active.cell.to_dict():
        raise RuntimeError(f"current checkpoint identity mismatch at {path}")
    if (
        not require_current_cell
        and (
            stored_cell.architecture != active.cell.architecture
            or stored_cell.factor_lr != active.cell.factor_lr
            or stored_cell.auxiliary_lr != active.cell.auxiliary_lr
        )
    ):
        raise RuntimeError(f"source architecture or optimizer mismatch at {path}")
    unwrap_model(active.model).load_state_dict(value["model_state_dict"])
    active.factor_optimizer.load_state_dict(value["factor_optimizer_state_dict"])
    active.auxiliary_optimizer.load_state_dict(value["auxiliary_optimizer_state_dict"])
    active.examples_seen = int(value["examples_seen"])
    active.initial_hashes = dict(value["initial_hashes"])
    active.initial_validation = dict(value["initial_validation"])
    return value


def build_active_cells(
    entries: list[dict],
    *,
    context: DistributedContext,
    output_root: str,
) -> list[ActiveCell]:
    active_cells = []
    for entry in entries:
        cell = cell_from_dict(entry["cell"])
        torch.manual_seed(cell.seed)
        student = TensorKroneckerStudent(
            cell.architecture,
            seed=cell.seed,
            dtype=(torch.bfloat16 if context.device.type == "cuda" else torch.float32),
        ).to(
            device=context.device,
            dtype=(
                torch.bfloat16
                if context.device.type == "cuda"
                else torch.float32
            ),
        )
        factor_optimizer, auxiliary_optimizer = make_optimizers(
            student,
            device=context.device,
            factor_lr=cell.factor_lr,
            auxiliary_lr=cell.auxiliary_lr,
        )
        initial_hashes = {
            "embedding": hashlib.sha256(
                b"".join(
                    value.detach()
                    .contiguous()
                    .cpu()
                    .view(torch.uint8)
                    .numpy()
                    .tobytes()
                    for value in student.vocabulary_parameters()
                )
            ).hexdigest(),
            "factors": hashlib.sha256(
                b"".join(
                    value.detach()
                    .contiguous()
                    .cpu()
                    .view(torch.uint8)
                    .numpy()
                    .tobytes()
                    for value in student.factor_parameters()
                )
            ).hexdigest(),
        }
        model = student
        if context.world_size > 1:
            model = DistributedDataParallel(
                student,
                device_ids=[context.local_rank],
                output_device=context.local_rank,
                broadcast_buffers=False,
                gradient_as_bucket_view=True,
            )
        active = ActiveCell(
            cell=cell,
            model=model,
            factor_optimizer=factor_optimizer,
            auxiliary_optimizer=auxiliary_optimizer,
            initial_hashes=initial_hashes,
            initial_validation=dict(entry.get("initial_validation") or {}),
            source_label=entry.get("source_label"),
        )
        current = _load_checkpoint(
            checkpoint_path(output_root, cell),
            active,
            require_current_cell=True,
        )
        if current is None and entry.get("source_checkpoint"):
            source = _load_checkpoint(
                Path(entry["source_checkpoint"]),
                active,
                require_current_cell=False,
            )
            if source is None:
                raise RuntimeError(
                    f"missing source checkpoint {entry['source_checkpoint']}"
                )
            active.source_label = entry.get("source_label")
            if entry.get("initial_validation"):
                active.initial_validation = dict(entry["initial_validation"])
        active_cells.append(active)
    cursors = {active.examples_seen for active in active_cells}
    if len(cursors) != 1:
        raise RuntimeError(f"active cells have different cursors: {cursors}")
    return active_cells


@torch.inference_mode()
def evaluate_cells(
    active_cells: list[ActiveCell],
    teacher,
    contexts: np.ndarray,
    targets: np.ndarray,
    *,
    context: DistributedContext,
    examples: int,
    batch_size: int = EVALUATION_BATCH,
) -> list[dict[str, float]]:
    for active in active_cells:
        active.model.eval()
    totals = torch.zeros(
        len(active_cells),
        5,
        device=context.device,
        dtype=torch.float64,
    )
    local_indices = np.arange(
        context.rank,
        min(examples, len(contexts)),
        context.world_size,
        dtype=np.int64,
    )
    for start in range(0, len(local_indices), batch_size):
        index = local_indices[start : start + batch_size]
        token_ids = torch.from_numpy(np.array(contexts[index], copy=True)).to(
            context.device, dtype=torch.long
        )
        target = torch.from_numpy(np.array(targets[index], copy=True)).to(
            context.device, dtype=torch.long
        )
        probability, log_probability, entropy = teacher_distribution(
            teacher,
            token_ids,
        )
        for cell_index, active in enumerate(active_cells):
            with torch.autocast(
                device_type="cuda",
                dtype=torch.bfloat16,
                enabled=context.device.type == "cuda",
            ):
                logits = active.model(token_ids)
            rows = distribution_metrics(
                logits,
                probability,
                log_probability,
                entropy,
                target,
            )
            totals[cell_index, 0] += rows["kl"].double().sum()
            totals[cell_index, 1] += rows["student_nll"].double().sum()
            totals[cell_index, 2] += rows["teacher_nll"].double().sum()
            totals[cell_index, 3] += rows["accuracy"].double().sum()
            totals[cell_index, 4] += len(index)
    if context.world_size > 1:
        dist.all_reduce(totals, op=dist.ReduceOp.SUM)
    results = []
    for row in totals:
        count = float(row[4])
        results.append(
            {
                "kl": float(row[0] / count),
                "student_nll": float(row[1] / count),
                "teacher_nll": float(row[2] / count),
                "accuracy": float(row[3] / count),
                "examples": int(count),
            }
        )
    for active in active_cells:
        active.model.train()
    return results


def _microbatch_size(local_batch: int) -> int:
    requested = int(os.environ.get("QWEN_KRON_MICROBATCH", str(LOCAL_BATCH)))
    if requested <= 0 or local_batch % requested:
        raise ValueError("QWEN_KRON_MICROBATCH must be a positive local-batch divisor")
    return requested


def _rank_indices(
    order: np.ndarray,
    cursor: int,
    *,
    context: DistributedContext,
    local_batch: int,
) -> np.ndarray:
    global_batch = local_batch * context.world_size
    selected = order[cursor : cursor + global_batch]
    if len(selected) != global_batch:
        raise RuntimeError("training order ended before the target milestone")
    start = context.rank * local_batch
    return selected[start : start + local_batch]


def _wandb_stage(
    enabled: bool,
    *,
    stage_name: str,
    spec: dict,
):
    if not enabled:
        return None
    import wandb

    return wandb.init(
        project=WANDB_PROJECT,
        group="tensor-native-kron-distill-v1",
        job_type=stage_name,
        name=f"{stage_name}-{int(time.time())}",
        config={
            "stage_spec": spec,
            "study_plan": study_plan(),
            "normuon_source_commit": NORMUON_SOURCE_COMMIT,
        },
    )


def _write_results(
    active_cells: list[ActiveCell],
    validations: list[dict],
    tests: list[dict] | None,
    *,
    context: DistributedContext,
    output_root: str,
    data_manifest: dict,
    elapsed: float,
    stage_examples: int,
    stage_run,
) -> list[dict]:
    results = []
    for index, (active, validation) in enumerate(
        zip(active_cells, validations, strict=True)
    ):
        student = unwrap_model(active.model)
        result = {
            "schema": RESULT_SCHEMA,
            "status": "complete",
            "label": active.cell.label,
            "cell": active.cell.to_dict(),
            "examples_seen": active.examples_seen,
            "input_tokens_seen": active.examples_seen
            * active.cell.architecture.context_length,
            "objective": "exact_full_vocabulary_forward_kl",
            "teacher": {
                "model_id": MODEL_ID,
                "model_revision": MODEL_REVISION,
                "live": True,
                "cache_used": False,
            },
            "optimizer": {
                "factor": "normuon",
                "factor_lr": active.cell.factor_lr,
                "factor_state": "independent_rank_slice_matrix",
                "normuon_source_commit": NORMUON_SOURCE_COMMIT,
                "auxiliary": (
                    "normuon"
                    if STUDY_VARIANT == "v5-normuon-lr"
                    else "fused_adamw"
                ),
                "auxiliary_lr": active.cell.auxiliary_lr,
                "schedule": "wsd_by_examples",
            },
            "initial_hashes": active.initial_hashes,
            "initial_validation": active.initial_validation,
            "source_label": active.source_label,
            "inventory": student.parameter_inventory(),
            "validation": validation,
            "test": (None if tests is None else tests[index]),
            "data_manifest": data_manifest,
            "world_size": context.world_size,
            "global_batch": LOCAL_BATCH * context.world_size,
            "global_token_batch": (
                LOCAL_BATCH
                * context.world_size
                * active.cell.architecture.context_length
            ),
            "microbatch": _microbatch_size(LOCAL_BATCH),
            "gradient_accumulation_steps": (
                LOCAL_BATCH // _microbatch_size(LOCAL_BATCH)
            ),
            "elapsed_wall_seconds": elapsed,
            "stage_examples": stage_examples,
            "unique_teacher_contexts_per_second": stage_examples / max(elapsed, 1e-9),
            "plan_sha256": study_plan()["plan_sha256"],
            "wandb_url": (None if stage_run is None else stage_run.url),
            "wandb": (
                None
                if stage_run is None
                else {
                    "mode": os.environ.get("WANDB_MODE", "online"),
                    "run_id": stage_run.id,
                    "run_path": stage_run.path,
                    "run_dir": stage_run.dir,
                    "url": stage_run.url,
                }
            ),
        }
        result["result_sha256"] = canonical_hash(result)
        atomic_json(result_path(output_root, active.cell), result)
        atomic_torch_save(
            student_path(output_root, active.cell),
            {
                "schema": "qwen-kron-distill-weights-v1",
                "cell": active.cell.to_dict(),
                "state_dict": student.state_dict(),
                "inventory": result["inventory"],
                "result_sha256": result["result_sha256"],
            },
        )
        results.append(result)
    return results


def run_stage(
    spec_path: str,
    *,
    data_root: str = DEFAULT_DATA_ROOT,
    output_root: str = DEFAULT_OUTPUT_ROOT,
    require_world_size: int = WORLD_SIZE,
) -> list[dict]:
    context = distributed_context()
    try:
        if require_world_size and context.world_size != require_world_size:
            raise RuntimeError(
                f"stage requires world size {require_world_size}, "
                f"got {context.world_size}"
            )
        if LOCAL_BATCH * context.world_size != GLOBAL_BATCH:
            raise RuntimeError("runtime world size does not match the planned global batch")
        if GLOBAL_BATCH * CONTEXT_LENGTH != GLOBAL_TOKEN_BATCH:
            raise RuntimeError("planned global token batch identity is inconsistent")
        spec = json.loads(Path(spec_path).read_text())
        if spec.get("schema") != "qwen-kron-stage-spec-v1":
            raise RuntimeError("invalid stage spec schema")
        plan = study_plan()
        if spec.get("study_plan_sha256") != plan["plan_sha256"]:
            raise RuntimeError("stage spec study-plan identity mismatch")
        entries = list(spec["entries"])
        cells = [cell_from_dict(value["cell"]) for value in entries]
        if not cells:
            raise RuntimeError("stage has no active cells")
        targets = {cell.target_examples for cell in cells}
        if len(targets) != 1:
            raise RuntimeError("all active cells must share a target")
        target_examples = targets.pop()
        if target_examples % (LOCAL_BATCH * context.world_size):
            raise RuntimeError("stage target must align to the global batch")

        data_manifest = load_manifest(data_root)
        if data_manifest.get("split_sizes") != {
            "train": DATASET_TRAIN_EXAMPLES,
            "validation": VALIDATION_EXAMPLES,
            "test": TEST_EXAMPLES,
        }:
            raise RuntimeError("dataset manifest does not contain exact split sizes")
        train_contexts, _, _ = load_split(data_root, "train")
        validation_contexts, validation_targets, _ = load_split(data_root, "validation")
        test_contexts = test_targets = None
        if spec.get("evaluate_test"):
            test_contexts, test_targets, _ = load_split(data_root, "test")

        active_cells = build_active_cells(
            entries,
            context=context,
            output_root=output_root,
        )
        teacher = load_teacher(str(context.device))
        if context.device.type == "cuda":
            torch.cuda.empty_cache()

        missing_initial = [
            active for active in active_cells if not active.initial_validation
        ]
        if missing_initial:
            initial = evaluate_cells(
                missing_initial,
                teacher,
                validation_contexts,
                validation_targets,
                context=context,
                examples=VALIDATION_EXAMPLES,
            )
            for active, metrics in zip(missing_initial, initial, strict=True):
                active.initial_validation = metrics

        cursor = active_cells[0].examples_seen
        stage_start_examples = cursor
        if cursor > target_examples:
            raise RuntimeError("checkpoint is beyond this stage target")
        order = deterministic_order(
            len(train_contexts),
            total_examples=target_examples,
        )
        microbatch = _microbatch_size(LOCAL_BATCH)
        accumulation = LOCAL_BATCH // microbatch
        if plan.get("schema") != PLAN_SCHEMA:
            raise RuntimeError("study plan schema changed unexpectedly")
        stage_run = _wandb_stage(
            (
                bool(os.environ.get("WANDB_API_KEY"))
                or os.environ.get("WANDB_MODE") == "offline"
            )
            and context.primary,
            stage_name=spec["stage"],
            spec=spec,
        )

        started = time.perf_counter()
        last_checkpoint = cursor
        probability_validated = False
        while cursor < target_examples:
            local_indices = _rank_indices(
                order,
                cursor,
                context=context,
                local_batch=LOCAL_BATCH,
            )
            for active in active_cells:
                active.factor_optimizer.zero_grad(set_to_none=True)
                active.auxiliary_optimizer.zero_grad(set_to_none=True)
            cell_losses = [0.0 for _ in active_cells]
            for micro_index, start in enumerate(range(0, LOCAL_BATCH, microbatch)):
                index = local_indices[start : start + microbatch]
                token_ids = torch.from_numpy(
                    np.array(train_contexts[index], copy=True)
                ).to(context.device, dtype=torch.long)
                probability, entropy = teacher_probability_targets(
                    teacher,
                    token_ids,
                )
                if not probability_validated:
                    validate_probability_rows(probability)
                    probability_validated = True
                for cell_index, active in enumerate(active_cells):
                    synchronize = micro_index + 1 == accumulation
                    sync_context = (
                        contextlib.nullcontext()
                        if synchronize
                        or not isinstance(
                            active.model,
                            DistributedDataParallel,
                        )
                        else active.model.no_sync()
                    )
                    with sync_context:
                        with torch.autocast(
                            device_type="cuda",
                            dtype=torch.bfloat16,
                            enabled=context.device.type == "cuda",
                        ):
                            loss = active.model(
                                token_ids,
                                probability,
                                entropy,
                            )
                        scaled_loss = loss / accumulation
                        scaled_loss.backward()
                    cell_losses[cell_index] += float(
                        scaled_loss.detach()
                    )

            next_cursor = cursor + LOCAL_BATCH * context.world_size
            lr_values = None
            for active in active_cells:
                lr_values = set_learning_rates(
                    active.factor_optimizer,
                    active.auxiliary_optimizer,
                    next_cursor,
                    factor_base_lr=active.cell.factor_lr,
                    auxiliary_base_lr=active.cell.auxiliary_lr,
                )
                norm = torch.nn.utils.clip_grad_norm_(
                    active.model.parameters(),
                    GRADIENT_CLIP_NORM,
                )
                if not torch.isfinite(norm):
                    raise RuntimeError(
                        f"non-finite gradient norm for {active.cell.label}"
                    )
                active.factor_optimizer.step()
                active.auxiliary_optimizer.step()
                active.examples_seen = next_cursor
            cursor = next_cursor

            if context.primary and stage_run is not None and cursor % 16_384 == 0:
                payload = {
                    "examples_seen": cursor,
                    **{
                        f"{active.cell.label}/train_kl": loss
                        for active, loss in zip(
                            active_cells,
                            cell_losses,
                            strict=True,
                        )
                    },
                    **{
                        f"optimizer/{key}": value
                        for key, value in (lr_values or {}).items()
                    },
                }
                stage_run.log(payload, step=cursor)

            if (
                cursor == target_examples
                or cursor - last_checkpoint >= CHECKPOINT_EVERY_EXAMPLES
            ):
                if context.world_size > 1:
                    dist.barrier()
                if context.primary:
                    elapsed = time.perf_counter() - started
                    for active in active_cells:
                        save_active_checkpoint(
                            output_root,
                            active,
                            elapsed,
                        )
                if context.world_size > 1:
                    dist.barrier()
                last_checkpoint = cursor

        validations = evaluate_cells(
            active_cells,
            teacher,
            validation_contexts,
            validation_targets,
            context=context,
            examples=VALIDATION_EXAMPLES,
        )
        tests = None
        if spec.get("evaluate_test"):
            if len(active_cells) != 1:
                raise RuntimeError("test evaluation is winner-only")
            tests = evaluate_cells(
                active_cells,
                teacher,
                test_contexts,
                test_targets,
                context=context,
                examples=TEST_EXAMPLES,
            )
        elapsed = time.perf_counter() - started
        results = []
        if context.primary:
            results = _write_results(
                active_cells,
                validations,
                tests,
                context=context,
                output_root=output_root,
                data_manifest=data_manifest,
                elapsed=elapsed,
                stage_examples=target_examples - stage_start_examples,
                stage_run=stage_run,
            )
            if stage_run is not None:
                stage_run.log(
                    {
                        **{
                            f"{active.cell.label}/validation_kl": metrics["kl"]
                            for active, metrics in zip(
                                active_cells,
                                validations,
                                strict=True,
                            )
                        },
                        "elapsed_wall_seconds": elapsed,
                    },
                    step=target_examples,
                )
                stage_run.finish()
        if context.world_size > 1:
            dist.barrier()
        return results
    finally:
        close_distributed(context)


def run_preflight_worker(
    result_file: str,
    *,
    data_root: str = DEFAULT_DATA_ROOT,
) -> dict:
    """Exercise the complete depth-stage graph without publishing checkpoints."""
    context = distributed_context()
    try:
        train_contexts, _, _ = load_split(data_root, "train")
        teacher = load_teacher(str(context.device))
        architecture = preflight_architecture()
        preflight_cell = Cell(
            stage=(
                "lr"
                if STUDY_VARIANT == "v5-normuon-lr"
                else "width"
                if architecture.embedding_width > 64
                else "depth"
            ),
            architecture=architecture,
            target_examples=SCREEN_EXAMPLES,
            factor_lr=(
                max(NORMUON_ONLY_LRS)
                if STUDY_VARIANT == "v5-normuon-lr"
                else max(value[0] for value in WIDE_LR_PAIRS)
                if STUDY_VARIANT in ("v4-wide", "v4-isolated")
                else NORMUON_LR
            ),
            auxiliary_lr=(
                max(NORMUON_ONLY_LRS)
                if STUDY_VARIANT == "v5-normuon-lr"
                else max(value[1] for value in WIDE_LR_PAIRS)
                if STUDY_VARIANT in ("v4-wide", "v4-isolated")
                else AUX_ADAMW_LR
            ),
        )
        entries = [{"cell": preflight_cell.to_dict()}]
        # A preflight-only root prevents an existing production checkpoint from
        # changing the graph or cursor exercised here.
        preflight_root = f"/tmp/qwen-kron-preflight-r{context.rank}-{os.getpid()}"
        active_cells = build_active_cells(
            entries,
            context=context,
            output_root=preflight_root,
        )
        preflight_examples = (
            WARMUP_EXAMPLES
            if STUDY_VARIANT == "v5-normuon-lr"
            else 2 * LOCAL_BATCH * context.world_size
        )
        order = deterministic_order(
            len(train_contexts),
            total_examples=preflight_examples,
        )
        microbatch = _microbatch_size(LOCAL_BATCH)
        accumulation = LOCAL_BATCH // microbatch

        agreement_indices = _rank_indices(
            order,
            0,
            context=context,
            local_batch=LOCAL_BATCH,
        )[:2]
        agreement_tokens = torch.from_numpy(
            np.array(train_contexts[agreement_indices], copy=True)
        ).to(context.device, dtype=torch.long)
        (
            agreement_probability,
            _,
            agreement_entropy,
        ) = teacher_distribution(
            teacher,
            agreement_tokens,
        )
        agreement_student = unwrap_model(active_cells[0].model)
        with torch.no_grad(), torch.autocast(
            device_type="cuda",
            dtype=torch.bfloat16,
            enabled=context.device.type == "cuda",
        ):
            agreement_hidden = agreement_student.hidden(agreement_tokens)
            dense_agreement = exact_kl_rows(
                agreement_student(agreement_tokens),
                agreement_probability,
                agreement_entropy,
            ).mean()
            explicit_logits = (
                agreement_hidden[:, None, None, :]
                * agreement_student.vocabulary_factors[0][None, :, None, :]
                * agreement_student.vocabulary_factors[1][None, None, :, :]
            ).sum(dim=-1).reshape_as(agreement_probability)
            optimized_agreement = khatri_rao_forward_kl(
                agreement_hidden,
                agreement_student.vocabulary_factors[0],
                agreement_student.vocabulary_factors[1],
                agreement_probability,
                agreement_entropy,
            )
            explicit_agreement = exact_kl_rows(
                explicit_logits,
                agreement_probability,
                agreement_entropy,
            ).mean()
        dense_optimized_loss_error = float(
            torch.maximum(
                (dense_agreement - optimized_agreement).abs(),
                (explicit_agreement - optimized_agreement).abs(),
            )
        )
        if dense_optimized_loss_error > 5e-4:
            raise RuntimeError(
                "optimized exact KL disagrees with the dense reference"
            )

        def step(cursor: int) -> tuple[list[float], dict[str, float]]:
            local_indices = _rank_indices(
                order,
                cursor,
                context=context,
                local_batch=LOCAL_BATCH,
            )
            for active in active_cells:
                active.factor_optimizer.zero_grad(set_to_none=True)
                active.auxiliary_optimizer.zero_grad(set_to_none=True)
            losses = [0.0 for _ in active_cells]
            for micro_index, start in enumerate(range(0, LOCAL_BATCH, microbatch)):
                index = local_indices[start : start + microbatch]
                token_ids = torch.from_numpy(
                    np.array(train_contexts[index], copy=True)
                ).to(context.device, dtype=torch.long)
                probability, entropy = teacher_probability_targets(
                    teacher,
                    token_ids,
                )
                if cursor == 0 and micro_index == 0:
                    validate_probability_rows(probability)
                for cell_index, active in enumerate(active_cells):
                    synchronize = micro_index + 1 == accumulation
                    sync_context = (
                        contextlib.nullcontext()
                        if synchronize
                        or not isinstance(
                            active.model,
                            DistributedDataParallel,
                        )
                        else active.model.no_sync()
                    )
                    with sync_context:
                        with torch.autocast(
                            device_type="cuda",
                            dtype=torch.bfloat16,
                            enabled=context.device.type == "cuda",
                        ):
                            loss = active.model(
                                token_ids,
                                probability,
                                entropy,
                            )
                        scaled_loss = loss / accumulation
                        scaled_loss.backward()
                    losses[cell_index] += float(scaled_loss.detach())
            next_cursor = cursor + LOCAL_BATCH * context.world_size
            learning_rates = {}
            for active in active_cells:
                learning_rates = set_learning_rates(
                    active.factor_optimizer,
                    active.auxiliary_optimizer,
                    next_cursor,
                    factor_base_lr=active.cell.factor_lr,
                    auxiliary_base_lr=active.cell.auxiliary_lr,
                )
                gradient_norm = torch.nn.utils.clip_grad_norm_(
                    active.model.parameters(),
                    GRADIENT_CLIP_NORM,
                )
                if not torch.isfinite(gradient_norm):
                    raise RuntimeError("preflight found non-finite gradients")
                active.factor_optimizer.step()
                active.auxiliary_optimizer.step()
            return losses, learning_rates

        # Warm every teacher/student kernel and DDP bucket before timing.
        step(0)
        if context.device.type == "cuda":
            torch.cuda.synchronize()
            torch.cuda.reset_peak_memory_stats()
        started = time.perf_counter()
        losses, learning_rates = step(LOCAL_BATCH * context.world_size)
        if context.device.type == "cuda":
            torch.cuda.synchronize()
        elapsed = time.perf_counter() - started
        full_lr_losses = losses
        full_learning_rates = learning_rates
        cursor = 2 * LOCAL_BATCH * context.world_size
        while (
            STUDY_VARIANT == "v5-normuon-lr"
            and cursor < WARMUP_EXAMPLES
        ):
            full_lr_losses, full_learning_rates = step(cursor)
            cursor += LOCAL_BATCH * context.world_size

        def optimizer_state_finite(optimizer) -> bool:
            return all(
                not isinstance(value, torch.Tensor)
                or bool(torch.isfinite(value).all())
                for state in optimizer.state.values()
                for value in state.values()
            )

        factor_nonzero = all(
            any(state for state in active.factor_optimizer.state.values())
            for active in active_cells
        )
        embedding_changed = all(
            all(
                active.factor_optimizer.state.get(parameter, {}).get(
                    "step",
                    0,
                )
                for parameter in unwrap_model(
                    active.model
                ).vocabulary_parameters()
            )
            for active in active_cells
        )
        auxiliary_nonzero = all(
            any(state for state in active.auxiliary_optimizer.state.values())
            for active in active_cells
        )
        optimizer_states_finite = all(
            optimizer_state_finite(optimizer)
            for active in active_cells
            for optimizer in (
                active.factor_optimizer,
                active.auxiliary_optimizer,
            )
        )
        allocated = (
            torch.cuda.max_memory_allocated(context.device)
            if context.device.type == "cuda"
            else 0
        )
        total_memory = (
            torch.cuda.get_device_properties(context.device).total_memory
            if context.device.type == "cuda"
            else 1
        )
        result = {
            "schema": "qwen-kron-preflight-worker-v1",
            "status": "complete",
            "world_size": context.world_size,
            "local_batch": LOCAL_BATCH,
            "global_batch": LOCAL_BATCH * context.world_size,
            "global_token_batch": (
                LOCAL_BATCH * context.world_size * CONTEXT_LENGTH
            ),
            "microbatch": microbatch,
            "physical_token_batch": (
                microbatch * context.world_size * CONTEXT_LENGTH
            ),
            "gradient_accumulation_steps": accumulation,
            "cells": len(active_cells),
            "step_seconds": elapsed,
            "contexts_per_second": (LOCAL_BATCH * context.world_size / elapsed),
            "tokens_per_second": (
                LOCAL_BATCH
                * context.world_size
                * CONTEXT_LENGTH
                / elapsed
            ),
            "peak_allocated_gib": allocated / 2**30,
            "memory_ratio": allocated / total_memory,
            "factor_state_initialized": factor_nonzero,
            "auxiliary_state_initialized": auxiliary_nonzero,
            "embedding_state_initialized": bool(embedding_changed),
            "loss_min": min(losses),
            "loss_max": max(losses),
            "full_lr_loss_min": min(full_lr_losses),
            "full_lr_loss_max": max(full_lr_losses),
            "dense_optimized_loss_error": dense_optimized_loss_error,
            "learning_rates": learning_rates,
            "full_learning_rates": full_learning_rates,
            "full_base_lr_exercised": (
                STUDY_VARIANT != "v5-normuon-lr"
                or full_learning_rates["multiplier"] == 1.0
            ),
            "optimizer_states_finite": optimizer_states_finite,
        }
        if context.primary:
            atomic_json(Path(result_file), result)
        if context.world_size > 1:
            dist.barrier()
        return result
    finally:
        close_distributed(context)
