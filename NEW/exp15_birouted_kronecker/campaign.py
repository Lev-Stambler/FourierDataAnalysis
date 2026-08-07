"""Evidence-gated Exp15 campaign for one eight-H100 node."""

from __future__ import annotations

import argparse
import gc
import hashlib
import json
import math
import multiprocessing as mp
import os
import queue
import threading
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Any, Iterable

import numpy as np
import torch

import exp14_block_kronecker.campaign as base
from exp14_block_kronecker.data import GROUP_SIZE
from exp11_kronecker_debug.muon_tuning import BatchedMuon, MuonWithAuxAdamW

from .model import (
    CURRENT_R8,
    FFN_ONLY,
    MODEL_NAMES,
    LanguageModel,
    build_model,
    model_inventory,
)


SCHEMA = "exp15-birouted-kronecker-campaign-v1"
GPU_COUNT = 8
SCREEN_SEED = 15
SCREEN_TOKENS = 10_000_000
TUNING_SEED = 16
TUNING_TOKENS = 10_000_000
FINAL_SEEDS = (17, 18, 19, 20)
FINAL_TOKENS = 40_000_000
MINIMUM_SCREEN_WIN = 0.02
MINIMUM_FINAL_WIN = 0.03
MINIMUM_UTILIZATION = 85.0
BATCH_SWEEP = (768, 640, 512, 400)
UPWARD_BATCH_SWEEP = (1024, 1536, 2048, 3072, 4096)
EXECUTION_MODE = "default"
SELECTED_BATCH_LINEAGE = {
    CURRENT_R8: 640,
    FFN_ONLY: 2048,
    "source-r8": 512,
    "bi-r8": 512,
    "decoupled-r8": 512,
    "bi-decoupled-r8": 512,
    "dense-workspace-r8": 640,
    "bi-r12": 400,
}
PREFLIGHT_LINEAGE_URLS = (
    "https://wandb.ai/lev-tear-tear-labs/exp15-birouted-kronecker/runs/n8tmsk8d",
    "https://wandb.ai/lev-tear-tear-labs/exp15-birouted-kronecker/runs/yt0bp1cr",
    "https://wandb.ai/lev-tear-tear-labs/exp15-birouted-kronecker/runs/21a4s6tm",
)


_GPU_WINDOW_CACHE: dict[tuple[str, tuple[int, ...], int], torch.Tensor] = {}


def gpu_cached_block_batch(
    windows: np.ndarray, indices: np.ndarray, device: torch.device
) -> tuple[torch.Tensor, torch.Tensor]:
    """Gather the immutable corpus on-device, removing CPU/H2D step bubbles."""

    if device.type != "cuda":
        return base.data_block_batch(windows, indices, device)
    filename = str(getattr(windows, "filename", f"memory-{id(windows)}"))
    key = (filename, tuple(windows.shape), int(device.index or 0))
    cached = _GPU_WINDOW_CACHE.get(key)
    if cached is None:
        # Int32 is accepted by embedding and halves persistent corpus memory;
        # labels are converted to int64 only after the small indexed gather.
        host = np.array(windows, dtype=np.int32, copy=True)
        cached = torch.from_numpy(host).to(device=device, non_blocking=False)
        _GPU_WINDOW_CACHE[key] = cached
    logical = torch.as_tensor(indices, dtype=torch.long, device=device)
    first = cached.index_select(0, logical)
    continuation = cached.index_select(0, logical + 1)[:, : GROUP_SIZE - 1]
    stream = torch.cat((first, continuation), dim=1).long()
    return stream[:, : base.CONTEXT_LENGTH], stream[:, GROUP_SIZE:]


@dataclass(frozen=True)
class Recipe:
    family: str
    body_lr: float
    auxiliary_lr: float
    schedule: str = "constant"
    weight_decay: float = 0.01
    beta2: float = 0.95
    momentum: float = 0.95
    nesterov: bool = True
    ns_steps: int = 5
    clip_norm: float = 1.0
    warmup_tokens: int = 2_000_000
    horizon_tokens: int = FINAL_TOKENS
    minimum_lr_ratio: float = 0.1

    def __post_init__(self) -> None:
        if self.family not in {"adamw", "muon", "hybrid"}:
            raise ValueError("optimizer must be AdamW, Muon, or hybrid")
        if self.schedule not in {"constant", "warmup-cosine"}:
            raise ValueError("invalid schedule")
        if min(self.body_lr, self.auxiliary_lr, self.clip_norm) <= 0:
            raise ValueError("learning rates and clipping must be positive")


def recipe_slug(recipe: Recipe) -> str:
    payload = json.dumps(asdict(recipe), sort_keys=True, separators=(",", ":"))
    return hashlib.sha256(payload.encode()).hexdigest()[:16]


def schedule_multiplier(recipe: Recipe, tokens_seen: int) -> float:
    if recipe.schedule == "constant":
        return 1.0
    if tokens_seen <= recipe.warmup_tokens:
        return max(tokens_seen, 1) / recipe.warmup_tokens
    progress = min(
        1.0,
        (tokens_seen - recipe.warmup_tokens)
        / max(1, recipe.horizon_tokens - recipe.warmup_tokens),
    )
    cosine = 0.5 * (1.0 + math.cos(math.pi * progress))
    return recipe.minimum_lr_ratio + (1.0 - recipe.minimum_lr_ratio) * cosine


def _muon_route(name: str, parameter: torch.nn.Parameter, family: str) -> bool:
    if not name.startswith("blocks.") or parameter.ndim < 2:
        return False
    if family == "muon":
        return True
    return any(
        marker in name
        for marker in (".ffn.", ".source_router.", ".destination_router.")
    )


def create_optimizer(model: LanguageModel, recipe: Recipe) -> tuple[Any, dict[str, Any]]:
    if recipe.family == "adamw":
        return (
            torch.optim.AdamW(
                base.grouped_parameters(model.parameters(), recipe.weight_decay),
                lr=recipe.body_lr,
                betas=(0.9, recipe.beta2),
            ),
            {
                "family": "adamw",
                "parameters": sum(parameter.numel() for parameter in model.parameters()),
            },
        )
    muon, auxiliary, muon_names, auxiliary_names = [], [], [], []
    for name, parameter in model.named_parameters():
        if _muon_route(name, parameter, recipe.family):
            muon.append(parameter)
            muon_names.append(name)
        else:
            auxiliary.append(parameter)
            auxiliary_names.append(name)
    if not muon or not auxiliary:
        raise RuntimeError("optimizer routing did not produce two nonempty groups")
    if len({id(item) for item in [*muon, *auxiliary]}) != len([*muon, *auxiliary]):
        raise RuntimeError("optimizer groups overlap")
    optimizer = MuonWithAuxAdamW(
        BatchedMuon(
            muon,
            lr=recipe.body_lr,
            weight_decay=recipe.weight_decay,
            momentum=recipe.momentum,
            nesterov=recipe.nesterov,
            ns_steps=recipe.ns_steps,
        ),
        torch.optim.AdamW(
            base.grouped_parameters(auxiliary, recipe.weight_decay),
            lr=recipe.auxiliary_lr,
            betas=(0.9, recipe.beta2),
        ),
    )
    return optimizer, {
        "family": recipe.family,
        "policy": (
            "all-block-matrices-muon"
            if recipe.family == "muon"
            else "ffn-and-router-matrices-muon_factors-adamw"
        ),
        "muon_parameter_names": muon_names,
        "auxiliary_parameter_names": auxiliary_names,
        "muon_parameters": sum(parameter.numel() for parameter in muon),
        "auxiliary_parameters": sum(parameter.numel() for parameter in auxiliary),
    }


def set_learning_rates(
    optimizer: Any, recipe: Recipe, tokens_seen: int
) -> tuple[float, float]:
    multiplier = schedule_multiplier(recipe, tokens_seen)
    body = recipe.body_lr * multiplier
    auxiliary = recipe.auxiliary_lr * multiplier
    if recipe.family == "adamw":
        for group in optimizer.param_groups:
            group["lr"] = body
        return body, body
    for group in optimizer.muon.param_groups:
        group["lr"] = body
    for group in optimizer.auxiliary.param_groups:
        group["lr"] = auxiliary
    return body, auxiliary


def _activate_base() -> None:
    """Point the hardened Exp14 GPU cell engine at the Exp15 model family."""

    base.Recipe = Recipe
    base.recipe_slug = recipe_slug
    base.schedule_multiplier = schedule_multiplier
    base.create_optimizer = create_optimizer
    base.set_learning_rates = set_learning_rates
    base.build_model = build_model
    base.model_inventory = model_inventory
    base.LanguageModel = LanguageModel
    if not hasattr(base, "data_block_batch"):
        base.data_block_batch = base.block_batch
    base.block_batch = gpu_cached_block_batch


def write_json(path: str | Path, value: Any) -> None:
    base.write_json(path, value)


def worker_loop(gpu_id: int, tasks: mp.Queue, results: mp.Queue) -> None:
    _activate_base()
    torch.cuda.set_device(gpu_id)
    device = torch.device(f"cuda:{gpu_id}")
    while True:
        item = tasks.get()
        if item is None:
            return
        index, task = item
        try:
            result = base.execute_task(task, device)
        except Exception as error:
            result = {
                "status": "failed",
                "kind": task.get("kind"),
                "model": task.get("model"),
                "seed": task.get("seed"),
                "batch": task.get("batch"),
                "failure": (
                    "out_of_memory"
                    if isinstance(error, torch.OutOfMemoryError)
                    or "out of memory" in str(error).lower()
                    else f"{type(error).__name__}: {str(error)[:1500]}"
                ),
            }
            gc.collect()
            torch.cuda.empty_cache()
        results.put((index, result))


def run_tasks(tasks: list[dict[str, Any]]) -> list[dict[str, Any]]:
    if not tasks:
        return []
    context = mp.get_context("spawn")
    task_queue, result_queue = context.Queue(), context.Queue()
    count = min(GPU_COUNT, len(tasks))
    processes = [
        context.Process(target=worker_loop, args=(gpu, task_queue, result_queue))
        for gpu in range(count)
    ]
    for process in processes:
        process.start()
    for index, task in enumerate(tasks):
        task_queue.put((index, task))
    for _ in processes:
        task_queue.put(None)
    rows: list[dict[str, Any] | None] = [None] * len(tasks)
    try:
        for _ in tasks:
            index, row = result_queue.get(timeout=3600)
            rows[index] = row
    except queue.Empty as error:
        raise TimeoutError("Exp15 GPU worker timed out") from error
    finally:
        for process in processes:
            process.join(timeout=10)
            if process.is_alive():
                process.terminate()
    if any(row is None for row in rows):
        raise RuntimeError("missing Exp15 GPU result")
    return [row for row in rows if row is not None]


def pad_to_full_node(tasks: list[dict[str, Any]]) -> list[dict[str, Any]]:
    if not tasks:
        return []
    padded = list(tasks)
    while len(padded) < GPU_COUNT:
        padded.append(dict(tasks[(len(padded) - len(tasks)) % len(tasks)]))
    return padded


def _all_complete(rows: Iterable[dict[str, Any]], label: str) -> None:
    failures = [row for row in rows if row.get("status") != "complete"]
    if failures:
        raise RuntimeError(f"{label} failed: {failures}")


def _task(
    model: str,
    recipe: Recipe,
    seed: int,
    tokens: int,
    batch: int,
    cells: Path,
    data: Path,
    evaluation_windows: int,
) -> dict[str, Any]:
    return {
        "kind": "train",
        "model": model,
        "recipe": asdict(recipe),
        "seed": seed,
        "target_tokens": tokens,
        "evaluation_windows": evaluation_windows,
        "batch": batch,
        "output_root": str(cells),
        "data_root": str(data),
        "execution_mode": EXECUTION_MODE,
    }


def _valid_benchmark(row: dict[str, Any]) -> bool:
    return (
        row.get("status") == "complete"
        and row.get("finite_forward_backward_optimizer") is True
        and int(row.get("gpu_samples", 0)) > 0
        and float(row.get("median_gpu_utilization_percent", 0))
        >= MINIMUM_UTILIZATION
    )


def _finite_complete_benchmark(row: dict[str, Any]) -> bool:
    """A stable row may need a larger batch to reach the utilization gate."""

    return (
        row.get("status") == "complete"
        and row.get("finite_forward_backward_optimizer") is True
        and int(row.get("gpu_samples", 0)) > 0
    )


def paid_preflight(data: Path, cells: Path) -> dict[str, Any]:
    path = cells / "preflight.json"
    if path.is_file():
        prior = json.loads(path.read_text())
        if prior.get("status") == "pass":
            return prior
    if torch.cuda.device_count() != GPU_COUNT:
        raise RuntimeError("Exp15 requires exactly eight visible GPUs")
    names = [torch.cuda.get_device_name(index) for index in range(GPU_COUNT)]
    memory = [
        torch.cuda.get_device_properties(index).total_memory / 2**30
        for index in range(GPU_COUNT)
    ]
    if any("H100" not in name for name in names) or any(size < 75 for size in memory):
        raise RuntimeError(f"Exp15 requires 8xH100-80GB; found {names} / {memory}")
    progress: dict[str, Any] = {
        "schema": "exp15-paid-preflight-v1",
        "status": "running",
        "gpu_names": names,
        "gpu_memory_gib": memory,
        "execution_mode": EXECUTION_MODE,
        "loss_implementation": "fla-fused-linear-cross-entropy-exact",
        "gradient_accumulation": 1,
        "batch_curves": {name: [] for name in MODEL_NAMES},
    }
    write_json(path, progress)
    agreement = run_tasks(
        [
            {
                "kind": "loss-agreement",
                "model": name,
                "data_root": str(data),
                "execution_mode": EXECUTION_MODE,
            }
            for name in MODEL_NAMES
        ]
    )
    _all_complete(agreement, "exact BF16 fused-loss agreement")
    progress["loss_agreement"] = dict(zip(MODEL_NAMES, agreement, strict=True))
    write_json(path, progress)
    representative = Recipe("adamw", 0.006, 0.006, "warmup-cosine")
    for batch in BATCH_SWEEP:
        rows = run_tasks(
            [
                {
                    "kind": "benchmark",
                    "model": name,
                    "recipe": asdict(representative),
                    "batch": batch,
                    "measured_steps": 8,
                    "data_root": str(data),
                    "execution_mode": EXECUTION_MODE,
                }
                for name in MODEL_NAMES
            ]
        )
        for name, row in zip(MODEL_NAMES, rows, strict=True):
            progress["batch_curves"][name].append(row)
        progress["last_batch"] = batch
        write_json(path, progress)
    # Variants that survive the known-near-capacity 768-context point search
    # upward until the first failure, approaching one million tokens/step when
    # activation memory permits.  Duplicate measurements occupy otherwise
    # idle GPUs when only a subset remains active.
    upward_active = [
        name
        for name in MODEL_NAMES
        if _finite_complete_benchmark(progress["batch_curves"][name][0])
    ]
    for batch in UPWARD_BATCH_SWEEP:
        if not upward_active:
            break
        tasks = [
            {
                "kind": "benchmark",
                "model": name,
                "recipe": asdict(representative),
                "batch": batch,
                "measured_steps": 8,
                "data_root": str(data),
                "execution_mode": EXECUTION_MODE,
            }
            for name in upward_active
        ]
        rows = run_tasks(pad_to_full_node(tasks))[: len(tasks)]
        survivors = []
        for name, row in zip(upward_active, rows, strict=True):
            progress["batch_curves"][name].append(row)
            if _finite_complete_benchmark(row):
                survivors.append(name)
        upward_active = survivors
        progress["last_upward_batch"] = batch
        write_json(path, progress)
    baselines = run_tasks(
        [
            {
                "kind": "benchmark",
                "model": name,
                "recipe": asdict(representative),
                "batch": 128,
                "measured_steps": 8,
                "data_root": str(data),
                "execution_mode": EXECUTION_MODE,
            }
            for name in MODEL_NAMES
        ]
    )
    selected: dict[str, Any] = {}
    for name, baseline in zip(MODEL_NAMES, baselines, strict=True):
        stable = [row for row in progress["batch_curves"][name] if _valid_benchmark(row)]
        if not stable:
            raise RuntimeError(f"no high-utilization stable batch for {name}")
        winner = max(stable, key=lambda row: float(row["tokens_per_second"]))
        if int(winner["global_tokens_per_step"]) < 100_000:
            raise RuntimeError(f"selected token batch underfills paid GPU for {name}")
        selected[name] = winner
        progress["batch_curves"][name] = {
            "ambitious_downward_sweep": progress["batch_curves"][name],
            "underfilled_batch128": baseline,
            "selected": winner,
            "selected_over_batch128_throughput": (
                float(winner["tokens_per_second"]) / float(baseline["tokens_per_second"])
                if baseline.get("status") == "complete"
                else None
            ),
        }
    # Run eight identical cells to quantify whole-node cell scaling under the
    # exact workload that anchors all comparisons.
    anchor = selected[CURRENT_R8]
    scaling = run_tasks(
        [
            {
                "kind": "benchmark",
                "model": CURRENT_R8,
                "recipe": asdict(representative),
                "batch": int(anchor["batch"]),
                "measured_steps": 8,
                "data_root": str(data),
                "execution_mode": EXECUTION_MODE,
            }
            for _ in range(GPU_COUNT)
        ]
    )
    _all_complete(scaling, "full-node scaling")
    efficiency = sum(float(row["tokens_per_second"]) for row in scaling) / (
        GPU_COUNT * float(anchor["tokens_per_second"])
    )
    if efficiency < 0.80:
        raise RuntimeError(f"full-node cell scaling efficiency too low: {efficiency:.3f}")
    progress.update(
        {
            "status": "pass",
            "selected": selected,
            "full_node_workers": scaling,
            "full_node_aggregate_tokens_per_second": sum(
                float(row["tokens_per_second"]) for row in scaling
            ),
            "full_node_cell_scaling_efficiency": efficiency,
            "inventories": {
                name: model_inventory(build_model(name)) for name in MODEL_NAMES
            },
        }
    )
    write_json(path, progress)
    return progress


def tuning_recipes() -> tuple[Recipe, ...]:
    return (
        Recipe("adamw", 0.003, 0.003),
        Recipe("adamw", 0.006, 0.006, "warmup-cosine"),
        Recipe("adamw", 0.01, 0.01, "warmup-cosine"),
        Recipe("muon", 0.03, 0.003),
        Recipe("muon", 0.06, 0.003),
        Recipe("muon", 0.1, 0.003),
        Recipe("hybrid", 0.03, 0.003),
        Recipe("hybrid", 0.06, 0.003),
    )


def paid_preflight_from_measured_lineage(data: Path, cells: Path) -> dict[str, Any]:
    """Revalidate selected batches after the GPU-resident input fast path.

    Three stopped runs already measured the full ambitious/downward/upward
    curves for the unchanged operators.  Repeating every compiled curve after
    an input-pipeline-only correction would waste the node, so this gate
    rechecks exactness and every selected point and preserves direct lineage.
    """

    path = cells / "selected-preflight.json"
    if path.is_file():
        prior = json.loads(path.read_text())
        if prior.get("status") == "pass":
            return prior
    if torch.cuda.device_count() != GPU_COUNT:
        raise RuntimeError("Exp15 requires exactly eight visible GPUs")
    names = [torch.cuda.get_device_name(index) for index in range(GPU_COUNT)]
    memory = [
        torch.cuda.get_device_properties(index).total_memory / 2**30
        for index in range(GPU_COUNT)
    ]
    if any("H100" not in name for name in names) or any(size < 75 for size in memory):
        raise RuntimeError(f"Exp15 requires 8xH100-80GB; found {names} / {memory}")
    progress: dict[str, Any] = {
        "schema": "exp15-selected-paid-preflight-v2",
        "status": "running",
        "gpu_names": names,
        "gpu_memory_gib": memory,
        "execution_mode": EXECUTION_MODE,
        "loss_implementation": "fla-fused-linear-cross-entropy-exact",
        "input_pipeline": "immutable-int32-corpus-on-each-gpu_indexed-gather",
        "gradient_accumulation": 1,
        "batch_sweep_lineage_wandb_urls": PREFLIGHT_LINEAGE_URLS,
        "batch_sweep_lineage_audits": (
            "exp15_birouted_kronecker/cloud_state/attempt-n8tmsk8d/audit.json",
            "exp15_birouted_kronecker/cloud_state/attempt-yt0bp1cr/audit.json",
        ),
        "selected_batch_lineage": SELECTED_BATCH_LINEAGE,
    }
    write_json(path, progress)
    agreement = run_tasks(
        [
            {
                "kind": "loss-agreement",
                "model": name,
                "data_root": str(data),
                "execution_mode": EXECUTION_MODE,
            }
            for name in MODEL_NAMES
        ]
    )
    _all_complete(agreement, "exact BF16 fused-loss agreement")
    progress["loss_agreement"] = dict(zip(MODEL_NAMES, agreement, strict=True))
    write_json(path, progress)
    representative = Recipe("adamw", 0.006, 0.006, "warmup-cosine")
    benchmarks = run_tasks(
        [
            {
                "kind": "benchmark",
                "model": name,
                "recipe": asdict(representative),
                "batch": SELECTED_BATCH_LINEAGE[name],
                # The very fast FFN control needs a longer observation window
                # for a meaningful nvidia-smi utilization median.
                "measured_steps": 100 if name == FFN_ONLY else 10,
                "data_root": str(data),
                "execution_mode": EXECUTION_MODE,
            }
            for name in MODEL_NAMES
        ]
    )
    _all_complete(benchmarks, "selected batch validation")
    selected = dict(zip(MODEL_NAMES, benchmarks, strict=True))
    invalid = {
        name: row for name, row in selected.items() if not _valid_benchmark(row)
    }
    if invalid:
        raise RuntimeError(f"selected batch failed utilization gate: {invalid}")
    if any(int(row["global_tokens_per_step"]) < 100_000 for row in benchmarks):
        raise RuntimeError("selected physical token batch is below 100,000")
    progress["selected"] = selected
    write_json(path, progress)
    # A low-batch comparison is retained as a measured underfill baseline;
    # only the selected rows are eligible for training.
    baselines = run_tasks(
        [
            {
                "kind": "benchmark",
                "model": name,
                "recipe": asdict(representative),
                "batch": 128,
                "measured_steps": 10,
                "data_root": str(data),
                "execution_mode": EXECUTION_MODE,
            }
            for name in MODEL_NAMES
        ]
    )
    _all_complete(baselines, "underfilled batch-128 baseline")
    progress["underfilled_batch128"] = dict(
        zip(MODEL_NAMES, baselines, strict=True)
    )
    progress["selected_over_batch128_throughput"] = {
        name: float(selected[name]["tokens_per_second"])
        / float(baseline["tokens_per_second"])
        for name, baseline in zip(MODEL_NAMES, baselines, strict=True)
    }
    anchor = selected[CURRENT_R8]
    scaling_task = {
        "kind": "benchmark",
        "model": CURRENT_R8,
        "recipe": asdict(representative),
        "batch": int(anchor["batch"]),
        "measured_steps": 10,
        "data_root": str(data),
        "execution_mode": EXECUTION_MODE,
    }
    scaling = run_tasks([dict(scaling_task) for _ in range(GPU_COUNT)])
    _all_complete(scaling, "full-node scaling")
    efficiency = sum(float(row["tokens_per_second"]) for row in scaling) / (
        GPU_COUNT * float(anchor["tokens_per_second"])
    )
    if efficiency < 0.80:
        raise RuntimeError(f"full-node cell scaling efficiency too low: {efficiency:.3f}")
    progress.update(
        {
            "status": "pass",
            "full_node_workers": scaling,
            "full_node_aggregate_tokens_per_second": sum(
                float(row["tokens_per_second"]) for row in scaling
            ),
            "full_node_cell_scaling_efficiency": efficiency,
            "inventories": {
                name: model_inventory(build_model(name)) for name in MODEL_NAMES
            },
        }
    )
    write_json(path, progress)
    return progress


def run_campaign(
    output: str | Path,
    *,
    data_root: str | Path,
    heartbeat: str | Path | None = None,
) -> dict[str, Any]:
    if not os.environ.get("WANDB_API_KEY"):
        raise RuntimeError("WANDB_API_KEY is required before paid training")
    import wandb

    _activate_base()
    data = Path(data_root)
    output_path = Path(output)
    cells = output_path.parent / "campaign-cells"
    base.load_windows(data, "train")
    base.load_windows(data, "validation")
    wandb.login(key=os.environ["WANDB_API_KEY"], verify=True)
    run = wandb.init(
        project="exp15-birouted-kronecker",
        name="exp15-eight-way-mechanistic-screen",
        config={
            "schema": SCHEMA,
            "models": MODEL_NAMES,
            "screen_tokens": SCREEN_TOKENS,
            "tuning_tokens": TUNING_TOKENS,
            "final_tokens": FINAL_TOKENS,
            "execution_mode": EXECUTION_MODE,
        },
    )
    if not run.url:
        raise RuntimeError("W&B did not provide a direct run URL")
    print(f"WANDB_URL={run.url}", flush=True)
    write_json(output_path, {"schema": SCHEMA, "status": "running", "wandb_url": run.url})
    heartbeat_path = Path(heartbeat) if heartbeat else None
    stop = threading.Event()
    heartbeat_thread: threading.Thread | None = None
    if heartbeat_path:
        heartbeat_path.parent.mkdir(parents=True, exist_ok=True)
        heartbeat_path.touch()

        def pulse() -> None:
            while not stop.wait(30):
                heartbeat_path.touch()

        heartbeat_thread = threading.Thread(target=pulse, daemon=True)
        heartbeat_thread.start()

    log_step = 0

    def publish(stage: str, rows: list[dict[str, Any]]) -> None:
        nonlocal log_step
        for row in rows:
            log_step += 1
            run.log(
                {
                    f"{stage}/model": row.get("model", ""),
                    f"{stage}/optimizer": row.get("recipe", {}).get("family", ""),
                    f"{stage}/validation_block_nll": row.get("validation", {}).get("nll", float("nan")),
                    f"{stage}/tokens_per_second": row.get("performance", {}).get("tokens_per_second", 0),
                    f"{stage}/global_tokens_per_step": row.get("global_tokens_per_step", 0),
                },
                step=log_step,
            )
        if heartbeat_path:
            heartbeat_path.touch()

    try:
        preflight = paid_preflight_from_measured_lineage(data, cells)
        batches = {name: int(row["batch"]) for name, row in preflight["selected"].items()}
        screen_recipe = Recipe("adamw", 0.006, 0.006, "warmup-cosine")
        screen = run_tasks(
            [
                _task(name, screen_recipe, SCREEN_SEED, SCREEN_TOKENS, batches[name], cells, data, 1024)
                for name in MODEL_NAMES
            ]
        )
        _all_complete(screen, "mechanistic screen")
        publish("screen", screen)
        nll = {row["model"]: float(row["validation"]["nll"]) for row in screen}
        eligible = [name for name in MODEL_NAMES if name not in {CURRENT_R8, FFN_ONLY}]
        successor = min(eligible, key=nll.__getitem__)
        successor_delta = nll[successor] - nll[CURRENT_R8]
        successor_vs_ffn = nll[successor] - nll[FFN_ONLY]
        advance = (
            successor_delta <= -MINIMUM_SCREEN_WIN
            and successor_vs_ffn <= -MINIMUM_SCREEN_WIN
        )
        common: dict[str, Any] = {
            "schema": SCHEMA,
            "wandb_url": run.url,
            "preflight": preflight,
            "screen": screen,
            "screen_nll": nll,
            "best_successor": successor,
            "screen_successor_minus_current_nll": successor_delta,
            "screen_successor_minus_ffn_only_nll": successor_vs_ffn,
            "minimum_screen_win": MINIMUM_SCREEN_WIN,
            "cloud_only_training": True,
            "gpu_count": GPU_COUNT,
        }
        if not advance:
            result = {
                **common,
                "status": "complete",
                "verdict": "stop_no_mechanistic_signal",
                "tuning": [],
                "final": [],
            }
        else:
            recipes = tuning_recipes()
            tuning = run_tasks(
                [
                    _task(model, recipe, TUNING_SEED, TUNING_TOKENS, batches[model], cells, data, 1024)
                    for model in (CURRENT_R8, successor)
                    for recipe in recipes
                ]
            )
            _all_complete(tuning, "optimizer/LR tuning")
            publish("tuning", tuning)
            selected: dict[str, dict[str, Any]] = {}
            for model in (CURRENT_R8, successor):
                rows = [row for row in tuning if row["model"] == model]
                selected[model] = min(rows, key=lambda row: float(row["validation"]["nll"]))
            final = run_tasks(
                [
                    _task(
                        model,
                        Recipe(**selected[model]["recipe"]),
                        seed,
                        FINAL_TOKENS,
                        batches[model],
                        cells,
                        data,
                        2048,
                    )
                    for model in (CURRENT_R8, successor)
                    for seed in FINAL_SEEDS
                ]
            )
            _all_complete(final, "paired final")
            publish("final", final)
            by_model = {
                model: {
                    int(row["seed"]): float(row["validation"]["nll"])
                    for row in final
                    if row["model"] == model
                }
                for model in (CURRENT_R8, successor)
            }
            paired = [
                by_model[successor][seed] - by_model[CURRENT_R8][seed]
                for seed in FINAL_SEEDS
            ]
            mean_delta = sum(paired) / len(paired)
            win = all(delta < 0 for delta in paired) and mean_delta <= -MINIMUM_FINAL_WIN
            result = {
                **common,
                "status": "complete",
                "verdict": "promote_birouted_successor" if win else "do_not_promote_successor",
                "tuning": tuning,
                "selected_recipes": {
                    model: selected[model]["recipe"] for model in selected
                },
                "final": final,
                "paired_successor_minus_current_nll": paired,
                "mean_successor_minus_current_nll": mean_delta,
                "minimum_final_win": MINIMUM_FINAL_WIN,
                "all_four_successor_wins": all(delta < 0 for delta in paired),
            }
        write_json(output_path, result)
        run.summary.update(
            {
                "verdict": result["verdict"],
                "best_successor": successor,
                "screen_successor_minus_current_nll": successor_delta,
                "screen_successor_minus_ffn_only_nll": successor_vs_ffn,
                "mean_successor_minus_current_nll": result.get("mean_successor_minus_current_nll"),
            }
        )
        run.finish()
        stop.set()
        if heartbeat_thread:
            heartbeat_thread.join(timeout=2)
        return result
    except Exception:
        stop.set()
        if heartbeat_thread:
            heartbeat_thread.join(timeout=2)
        run.finish(exit_code=1)
        raise


def main() -> None:
    parser = argparse.ArgumentParser(description="Run Exp15 on cloud 8xH100")
    parser.add_argument("--output", required=True)
    parser.add_argument("--data-root", required=True)
    parser.add_argument("--heartbeat")
    args = parser.parse_args()
    result = run_campaign(args.output, data_root=args.data_root, heartbeat=args.heartbeat)
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)


if __name__ == "__main__":
    main()
