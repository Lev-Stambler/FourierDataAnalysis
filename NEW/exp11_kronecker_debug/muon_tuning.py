"""Muon optimizer ablation for the parameter-matched Experiment 11 models."""

from __future__ import annotations

import gc
import hashlib
import json
import math
import os
import time
from dataclasses import asdict, dataclass, replace
from pathlib import Path
from typing import Any, Callable, Iterable

import numpy as np
import torch

from .lm import (
    batch_indices,
    bootstrap_interval,
    cross_entropy,
    evaluate,
    load_windows,
    optimizer_is_finite,
    preflight,
    tensor_batch,
    write_json,
)
from .model import LanguageModel, build_model, model_inventory


SCHEMA = "exp11-muon-tuning-v1"
CELL_SCHEMA = "exp11-muon-cell-v1"
VARIANTS = ("order3-r8", "transformer")
SEEDS = (0, 1, 2)
MUON_LR_GRID = (0.003, 0.01, 0.03, 0.1)
ADAMW_AUX_LR = {"order3-r8": 0.06, "transformer": 0.0015}
SCHEDULES = ("constant", "warmup-cosine")
SCREEN_TOKENS = 10_000_000
FINAL_TOKENS = 40_000_000
WARMUP_TOKENS = 2_000_000
MAX_BOUNDARY_EXPANSIONS = 4
BATCH_CANDIDATES = (2048, 1536, 1280, 1024, 768, 640, 512, 384, 256)


@dataclass(frozen=True)
class MuonRecipe:
    muon_lr: float
    auxiliary_lr: float
    schedule: str
    weight_decay: float = 0.01
    momentum: float = 0.95
    nesterov: bool = True
    clip_norm: float = 1.0
    warmup_tokens: int = WARMUP_TOKENS
    horizon_tokens: int = FINAL_TOKENS
    minimum_lr_ratio: float = 0.1
    ns_steps: int = 5

    def __post_init__(self) -> None:
        if self.schedule not in SCHEDULES:
            raise ValueError(f"unsupported schedule: {self.schedule}")
        if self.muon_lr <= 0 or self.auxiliary_lr <= 0 or self.clip_norm <= 0:
            raise ValueError("learning rates and clipping norm must be positive")
        if self.weight_decay < 0 or not 0 <= self.minimum_lr_ratio <= 1:
            raise ValueError("invalid decay configuration")
        if not 0 <= self.momentum < 1 or self.ns_steps <= 0:
            raise ValueError("invalid Muon configuration")


def schedule_multiplier(recipe: MuonRecipe, tokens_seen: int) -> float:
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


def recipe_slug(recipe: MuonRecipe) -> str:
    payload = json.dumps(asdict(recipe), sort_keys=True, separators=(",", ":"))
    return hashlib.sha256(payload.encode()).hexdigest()[:16]


@torch.no_grad()
def zeropower_via_newton_schulz5(
    gradient: torch.Tensor, steps: int = 5
) -> torch.Tensor:
    """Orthogonalize independent matrices over the final two dimensions."""
    if gradient.ndim < 2:
        raise ValueError("Muon parameters must contain matrices")
    value = gradient.bfloat16()
    transposed = value.shape[-2] > value.shape[-1]
    if transposed:
        value = value.mT
    value = value / (value.norm(dim=(-2, -1), keepdim=True) + 1e-7)
    for _ in range(steps):
        gram = value @ value.mT
        value = 3.4445 * value + (-4.7750 * gram + 2.0315 * gram @ gram) @ value
    return value.mT if transposed else value


class BatchedMuon(torch.optim.Optimizer):
    """Canonical Muon with independent leading-dimension matrix slices."""

    def __init__(
        self,
        params: Iterable[torch.nn.Parameter],
        *,
        lr: float,
        weight_decay: float = 0.01,
        momentum: float = 0.95,
        nesterov: bool = True,
        ns_steps: int = 5,
    ) -> None:
        super().__init__(
            params,
            {
                "lr": float(lr),
                "weight_decay": float(weight_decay),
                "momentum": float(momentum),
                "nesterov": bool(nesterov),
                "ns_steps": int(ns_steps),
            },
        )
        for group in self.param_groups:
            for parameter in group["params"]:
                if parameter.ndim < 2:
                    raise ValueError("Muon parameters must contain matrices")

    @torch.no_grad()
    def step(self, closure: Callable[[], torch.Tensor] | None = None) -> Any:
        loss = None
        if closure is not None:
            with torch.enable_grad():
                loss = closure()
        for group in self.param_groups:
            buckets: dict[tuple[Any, ...], list[torch.Tensor]] = {}
            for parameter in group["params"]:
                if parameter.grad is not None:
                    key = (
                        parameter.device,
                        parameter.dtype,
                        *parameter.shape[-2:],
                    )
                    buckets.setdefault(key, []).append(parameter)
            for parameters in buckets.values():
                self._step_bucket(parameters, group)
        return loss

    @torch.no_grad()
    def _step_bucket(
        self, parameters: list[torch.Tensor], group: dict[str, Any]
    ) -> None:
        rows, columns = parameters[0].shape[-2:]
        counts = [parameter.numel() // (rows * columns) for parameter in parameters]
        directions = torch.empty(
            sum(counts),
            rows,
            columns,
            device=parameters[0].device,
            dtype=torch.bfloat16,
        )
        offset = 0
        for parameter, count in zip(parameters, counts, strict=True):
            state = self.state[parameter]
            if "momentum_buffer" not in state:
                state["momentum_buffer"] = torch.zeros_like(parameter)
            momentum_buffer = state["momentum_buffer"]
            momentum_buffer.lerp_(parameter.grad, 1.0 - group["momentum"])
            direction = (
                parameter.grad.lerp(momentum_buffer, group["momentum"])
                if group["nesterov"]
                else momentum_buffer
            )
            directions[offset : offset + count].copy_(
                direction.reshape(count, rows, columns)
            )
            offset += count
        updates = zeropower_via_newton_schulz5(
            directions, steps=group["ns_steps"]
        )
        adjusted_lr = group["lr"] * math.sqrt(max(1.0, rows / columns))
        offset = 0
        for parameter, count in zip(parameters, counts, strict=True):
            parameter.mul_(1.0 - group["lr"] * group["weight_decay"])
            update = updates[offset : offset + count].reshape(parameter.shape)
            parameter.add_(update.to(parameter.dtype), alpha=-adjusted_lr)
            offset += count


class MuonWithAuxAdamW:
    """Checkpointable optimizer pair with separately scheduled learning rates."""

    def __init__(self, muon: BatchedMuon, auxiliary: torch.optim.AdamW) -> None:
        self.muon = muon
        self.auxiliary = auxiliary

    @property
    def state(self) -> dict[Any, Any]:
        return {**self.muon.state, **self.auxiliary.state}

    @property
    def param_groups(self) -> list[dict[str, Any]]:
        return self.muon.param_groups + self.auxiliary.param_groups

    def zero_grad(self, set_to_none: bool = True) -> None:
        self.muon.zero_grad(set_to_none=set_to_none)
        self.auxiliary.zero_grad(set_to_none=set_to_none)

    def step(self) -> None:
        self.muon.step()
        self.auxiliary.step()

    def state_dict(self) -> dict[str, Any]:
        return {
            "muon": self.muon.state_dict(),
            "auxiliary_adamw": self.auxiliary.state_dict(),
        }

    def load_state_dict(self, state: dict[str, Any]) -> None:
        self.muon.load_state_dict(state["muon"])
        self.auxiliary.load_state_dict(state["auxiliary_adamw"])


def split_muon_parameters(
    model: LanguageModel,
) -> tuple[list[torch.nn.Parameter], list[torch.nn.Parameter], dict[str, Any]]:
    """Route body matrices to Muon and vocabulary/vectors to AdamW."""
    muon: list[torch.nn.Parameter] = []
    auxiliary: list[torch.nn.Parameter] = []
    muon_names: list[str] = []
    auxiliary_names: list[str] = []
    for name, parameter in model.named_parameters():
        if name.startswith("blocks.") and parameter.ndim >= 2:
            muon.append(parameter)
            muon_names.append(name)
        else:
            auxiliary.append(parameter)
            auxiliary_names.append(name)
    if not muon or not auxiliary:
        raise RuntimeError("Muon routing must produce both optimizer groups")
    if len({id(parameter) for parameter in [*muon, *auxiliary]}) != len(
        [*muon, *auxiliary]
    ):
        raise RuntimeError("optimizer parameter groups overlap")
    if sum(parameter.numel() for parameter in [*muon, *auxiliary]) != sum(
        parameter.numel() for parameter in model.parameters()
    ):
        raise RuntimeError("optimizer parameter routing is incomplete")
    inventory = {
        "muon_parameter_names": muon_names,
        "auxiliary_parameter_names": auxiliary_names,
        "muon_parameters": sum(parameter.numel() for parameter in muon),
        "auxiliary_parameters": sum(parameter.numel() for parameter in auxiliary),
        "muon_tensor_count": len(muon),
        "auxiliary_tensor_count": len(auxiliary),
    }
    return muon, auxiliary, inventory


def create_optimizer(
    model: LanguageModel, recipe: MuonRecipe
) -> tuple[MuonWithAuxAdamW, dict[str, Any]]:
    muon_parameters, auxiliary_parameters, routing = split_muon_parameters(model)
    decay = [parameter for parameter in auxiliary_parameters if parameter.ndim >= 2]
    no_decay = [parameter for parameter in auxiliary_parameters if parameter.ndim < 2]
    groups = []
    if decay:
        groups.append({"params": decay, "weight_decay": recipe.weight_decay})
    if no_decay:
        groups.append({"params": no_decay, "weight_decay": 0.0})
    muon = BatchedMuon(
        muon_parameters,
        lr=recipe.muon_lr,
        weight_decay=recipe.weight_decay,
        momentum=recipe.momentum,
        nesterov=recipe.nesterov,
        ns_steps=recipe.ns_steps,
    )
    auxiliary = torch.optim.AdamW(
        groups,
        lr=recipe.auxiliary_lr,
        betas=(0.9, 0.95),
    )
    return MuonWithAuxAdamW(muon, auxiliary), routing


def set_learning_rates(
    optimizer: MuonWithAuxAdamW, recipe: MuonRecipe, tokens_seen: int
) -> tuple[float, float]:
    multiplier = schedule_multiplier(recipe, tokens_seen)
    muon_lr = recipe.muon_lr * multiplier
    auxiliary_lr = recipe.auxiliary_lr * multiplier
    for group in optimizer.muon.param_groups:
        group["lr"] = muon_lr
    for group in optimizer.auxiliary.param_groups:
        group["lr"] = auxiliary_lr
    return muon_lr, auxiliary_lr


def cell_directory(
    root: Path, variant: str, recipe: MuonRecipe, seed: int
) -> Path:
    return root / "cells" / variant / f"recipe-{recipe_slug(recipe)}" / f"seed-{seed}"


def load_cell(path: Path) -> dict[str, Any] | None:
    if not path.is_file():
        return None
    value = json.loads(path.read_text())
    return value if value.get("schema") == CELL_SCHEMA else None


def train_cell(
    variant: str,
    recipe: MuonRecipe,
    seed: int,
    target_tokens: int,
    *,
    output_root: Path,
    train_windows: np.ndarray,
    validation_windows: np.ndarray,
    batch: int,
    eval_batch: int,
    device: torch.device,
    log: Callable[[dict[str, Any]], None],
    heartbeat: Path | None,
    use_compile: bool,
) -> dict[str, Any]:
    directory = cell_directory(output_root, variant, recipe, seed)
    directory.mkdir(parents=True, exist_ok=True)
    result_path = directory / f"result-{target_tokens}.json"
    existing = load_cell(result_path)
    if existing and existing.get("status") == "complete":
        return existing

    torch.manual_seed(seed)
    model = build_model(variant).to(device)
    wrapped = torch.compile(model) if use_compile else model
    optimizer, routing = create_optimizer(model, recipe)
    identity = {"variant": variant, "recipe": asdict(recipe), "seed": seed}
    checkpoint_path = directory / "checkpoint.pt"
    completed_steps = 0
    if checkpoint_path.is_file():
        saved = torch.load(checkpoint_path, map_location="cpu", weights_only=True)
        if saved.get("identity") != identity:
            raise RuntimeError(f"checkpoint identity mismatch: {checkpoint_path}")
        model.load_state_dict(saved["model"])
        optimizer.load_state_dict(saved["optimizer"])
        completed_steps = int(saved["steps"])

    tokens_per_step = batch * 256
    target_steps = math.ceil(target_tokens / tokens_per_step)
    started = time.perf_counter()
    timed_seconds = 0.0
    timed_tokens = 0
    clipped_steps = 0
    measured_steps = 0
    last: dict[str, Any] = {}
    for step in range(completed_steps, target_steps):
        inputs, targets = tensor_batch(
            train_windows, batch_indices(len(train_windows), step, batch, seed), device
        )
        optimizer.zero_grad(set_to_none=True)
        tokens_seen = (step + 1) * tokens_per_step
        muon_lr, auxiliary_lr = set_learning_rates(optimizer, recipe, tokens_seen)
        torch.cuda.synchronize(device)
        step_started = time.perf_counter()
        with torch.autocast("cuda", dtype=torch.bfloat16):
            loss = cross_entropy(wrapped(inputs), targets)
        loss.backward()
        norm = torch.nn.utils.clip_grad_norm_(model.parameters(), recipe.clip_norm)
        if not torch.isfinite(loss) or not torch.isfinite(norm):
            raise RuntimeError(
                f"non-finite state in {variant} recipe={recipe_slug(recipe)} seed={seed}"
            )
        clipped_steps += int(float(norm.detach()) > recipe.clip_norm)
        measured_steps += 1
        optimizer.step()
        if not optimizer_is_finite(optimizer):
            raise RuntimeError(
                f"non-finite optimizer in {variant} recipe={recipe_slug(recipe)} seed={seed}"
            )
        torch.cuda.synchronize(device)
        duration = time.perf_counter() - step_started
        if step > completed_steps:
            timed_seconds += duration
            timed_tokens += tokens_per_step
        last = {
            "train/variant": variant,
            "train/optimizer": "muon+adamw",
            "train/recipe": recipe_slug(recipe),
            "train/muon_peak_lr": recipe.muon_lr,
            "train/muon_current_lr": muon_lr,
            "train/auxiliary_peak_lr": recipe.auxiliary_lr,
            "train/auxiliary_current_lr": auxiliary_lr,
            "train/schedule": recipe.schedule,
            "train/weight_decay": recipe.weight_decay,
            "train/momentum": recipe.momentum,
            "train/seed": seed,
            "train/step": step + 1,
            "train/tokens_seen": tokens_seen,
            "train/global_examples_per_step": batch,
            "train/global_tokens_per_step": tokens_per_step,
            "train/gradient_accumulation": 1,
            "train/execution_mode": "compiled" if use_compile else "eager",
            "train/nll": float(loss.detach()),
            "train/grad_norm": float(norm.detach()),
            "train/clip_fraction_this_invocation": clipped_steps / measured_steps,
            "train/step_seconds": duration,
            "train/tokens_per_second": (
                timed_tokens / timed_seconds if timed_seconds else tokens_per_step / duration
            ),
            "train/peak_allocated_gib": torch.cuda.max_memory_allocated(device) / 2**30,
            "train/peak_reserved_gib": torch.cuda.max_memory_reserved(device) / 2**30,
        }
        if (step + 1) % 20 == 0 or step + 1 == target_steps:
            log(last)
            print(json.dumps(last, sort_keys=True), flush=True)
            if heartbeat:
                heartbeat.parent.mkdir(parents=True, exist_ok=True)
                heartbeat.touch()

    validation = evaluate(model, validation_windows, eval_batch, device)
    saved = {
        "schema": "exp11-muon-checkpoint-v1",
        "identity": identity,
        "model": model.state_dict(),
        "optimizer": optimizer.state_dict(),
        "steps": target_steps,
        "tokens_seen": target_steps * tokens_per_step,
        "global_examples_per_step": batch,
        "global_tokens_per_step": tokens_per_step,
        "optimizer_routing": routing,
    }
    temporary = checkpoint_path.with_name(checkpoint_path.name + ".tmp")
    torch.save(saved, temporary)
    temporary.replace(checkpoint_path)
    result = {
        "schema": CELL_SCHEMA,
        "status": "complete",
        "variant": variant,
        "optimizer": "muon+adamw",
        "recipe": asdict(recipe),
        "recipe_slug": recipe_slug(recipe),
        "seed": seed,
        "target_tokens": target_tokens,
        "tokens_seen": target_steps * tokens_per_step,
        "inventory": model_inventory(model),
        "optimizer_routing": routing,
        "validation": validation,
        "performance": last,
        "elapsed_seconds_this_invocation": time.perf_counter() - started,
        "checkpoint": str(checkpoint_path),
    }
    write_json(result_path, result)
    log(
        {
            f"muon/{variant}/{recipe_slug(recipe)}/seed-{seed}/validation_nll": validation[
                "nll"
            ],
            f"muon/{variant}/{recipe_slug(recipe)}/seed-{seed}/tokens": result[
                "tokens_seen"
            ],
        }
    )
    del optimizer, wrapped, model, saved
    gc.collect()
    torch.cuda.empty_cache()
    return result


def safe_train_cell(*args: Any, **kwargs: Any) -> dict[str, Any]:
    try:
        return train_cell(*args, **kwargs)
    except RuntimeError as error:
        if "non-finite" not in str(error).lower():
            raise
        variant = str(args[0])
        recipe = args[1]
        seed = int(args[2])
        result = {
            "schema": CELL_SCHEMA,
            "status": "failed",
            "variant": variant,
            "optimizer": "muon+adamw",
            "recipe": asdict(recipe),
            "recipe_slug": recipe_slug(recipe),
            "seed": seed,
            "target_tokens": int(args[3]),
            "failure": str(error),
        }
        kwargs["log"](
            {f"muon/{variant}/{recipe_slug(recipe)}/seed-{seed}/diverged": 1}
        )
        gc.collect()
        torch.cuda.empty_cache()
        return result


def summarize(rows: list[dict[str, Any]]) -> list[dict[str, Any]]:
    grouped: dict[str, list[dict[str, Any]]] = {}
    for row in rows:
        grouped.setdefault(str(row["recipe_slug"]), []).append(row)
    values = []
    for slug, cells in grouped.items():
        if {int(cell["seed"]) for cell in cells} != set(SEEDS):
            continue
        failed = [cell for cell in cells if cell.get("status") != "complete"]
        nlls = [float(cell["validation"]["nll"]) for cell in cells if not failed]
        values.append(
            {
                "recipe_slug": slug,
                "recipe": cells[0]["recipe"],
                "validation_nll_by_seed": {
                    str(cell["seed"]): (
                        float(cell["validation"]["nll"])
                        if cell.get("status") == "complete"
                        else None
                    )
                    for cell in sorted(cells, key=lambda item: item["seed"])
                },
                "failed_seeds": sorted(int(cell["seed"]) for cell in failed),
                "mean_validation_nll": 1e30 if failed else float(np.mean(nlls)),
                "std_validation_nll": 1e30 if failed else float(np.std(nlls)),
                "worst_validation_nll": 1e30 if failed else max(nlls),
            }
        )
    return sorted(
        values,
        key=lambda item: (
            item["mean_validation_nll"],
            item["std_validation_nll"],
            item["worst_validation_nll"],
        ),
    )


def initial_recipes(variant: str) -> list[MuonRecipe]:
    return [
        MuonRecipe(
            muon_lr=muon_lr,
            auxiliary_lr=ADAMW_AUX_LR[variant],
            schedule=schedule,
        )
        for schedule in SCHEDULES
        for muon_lr in MUON_LR_GRID
    ]


def boundary_extension(
    best: MuonRecipe, observed: list[MuonRecipe]
) -> MuonRecipe | None:
    comparable = sorted(
        {
            recipe.muon_lr
            for recipe in observed
            if recipe.schedule == best.schedule
            and recipe.auxiliary_lr == best.auxiliary_lr
            and recipe.weight_decay == best.weight_decay
            and recipe.momentum == best.momentum
        }
    )
    if best.muon_lr == comparable[0]:
        return replace(best, muon_lr=best.muon_lr / 3)
    if best.muon_lr == comparable[-1]:
        return replace(best, muon_lr=best.muon_lr * 3)
    return None


def optimizer_ablations(best: MuonRecipe) -> list[MuonRecipe]:
    return [
        replace(best, auxiliary_lr=best.auxiliary_lr / 2),
        replace(best, auxiliary_lr=best.auxiliary_lr * 2),
        replace(best, weight_decay=0.0),
        replace(best, momentum=0.9),
    ]


def projected_seconds(measured: dict[str, Any]) -> float:
    recipes = len(MUON_LR_GRID) * len(SCHEDULES) + MAX_BOUNDARY_EXPANSIONS + 4
    seconds = 0.0
    for variant in VARIANTS:
        speed = float(measured["selected"][variant]["tokens_per_second"])
        seconds += recipes * len(SEEDS) * SCREEN_TOKENS / speed
        seconds += len(SEEDS) * (FINAL_TOKENS - SCREEN_TOKENS) / speed
    return seconds


def load_checkpoint_model(
    result: dict[str, Any], device: torch.device
) -> LanguageModel:
    saved = torch.load(result["checkpoint"], map_location="cpu", weights_only=True)
    model = build_model(result["variant"]).to(device)
    model.load_state_dict(saved["model"])
    return model


def paired_comparison(
    left: list[float], right: list[float], *, label: str
) -> dict[str, Any]:
    paired = [a - b for a, b in zip(left, right, strict=True)]
    return {
        "label": label,
        "paired_left_minus_right_test_nll": paired,
        "mean_left_minus_right_test_nll": float(np.mean(paired)),
        "bootstrap_95_percent_interval": bootstrap_interval(paired),
    }


def load_adamw_reference() -> dict[str, Any]:
    path = Path(__file__).with_name("paid_tuning_v6_summary.json")
    value = json.loads(path.read_text())
    if value.get("experiment_id") != "exp11-optimizer-tuning-gpu1-v6":
        raise RuntimeError("unexpected AdamW reference artifact")
    return value


def muon_correctness(output: str | Path | None = None) -> dict[str, Any]:
    """CPU gate for canonical equivalence, routing, and finite optimizer state."""
    torch.manual_seed(12)
    batched = torch.nn.Parameter(torch.randn(2, 16, 16))
    references = [
        torch.nn.Parameter(matrix.detach().clone()) for matrix in batched.detach()
    ]
    ours = BatchedMuon(
        [batched], lr=0.02, momentum=0.95, ns_steps=5, weight_decay=0.0
    )
    standard = torch.optim.Muon(
        references,
        lr=0.02,
        momentum=0.95,
        nesterov=True,
        ns_steps=5,
        adjust_lr_fn="original",
        weight_decay=0.0,
    )
    for _ in range(2):
        gradient = torch.randn_like(batched)
        batched.grad = gradient.clone()
        for parameter, matrix_gradient in zip(references, gradient, strict=True):
            parameter.grad = matrix_gradient.clone()
        ours.step()
        standard.step()
    difference = float(
        (batched.detach() - torch.stack(references).detach()).abs().max()
    )

    routing: dict[str, Any] = {}
    finite = True
    for variant in VARIANTS:
        model = build_model(
            variant,
            context_length=8,
            vocab_size=32,
            width=16,
            depth=2,
            mode1=4,
            mode2=4,
            heads=4,
            mlp_width=32,
        )
        recipe = MuonRecipe(0.01, 0.001, "constant")
        optimizer, inventory = create_optimizer(model, recipe)
        tokens = torch.randint(0, 32, (2, 8))
        loss = model(tokens).float().square().mean()
        loss.backward()
        optimizer.step()
        finite = finite and bool(torch.isfinite(loss)) and optimizer_is_finite(optimizer)
        routing[variant] = inventory

    result = {
        "schema": "exp11-muon-correctness-v1",
        "status": "pass" if difference <= 0.003 and finite else "fail",
        "batched_vs_torch_muon_max_difference": difference,
        "maximum_allowed_difference": 0.003,
        "finite_forward_backward_optimizer": finite,
        "routing": routing,
    }
    if output is not None:
        write_json(Path(output), result)
    if result["status"] != "pass":
        raise RuntimeError(f"Muon correctness gate failed: {result}")
    return result


def run_muon_campaign(
    output: str | Path,
    *,
    data_root: str | Path,
    heartbeat: str | Path | None = None,
    wall_limit_seconds: float = 2400.0,
) -> dict[str, Any]:
    if not torch.cuda.is_available() or torch.cuda.device_count() != 1:
        raise RuntimeError("Muon tuning requires exactly one visible CUDA GPU")
    if not os.environ.get("WANDB_API_KEY"):
        raise RuntimeError("WANDB_API_KEY is required before the paid launch")
    import wandb

    wandb.login(key=os.environ["WANDB_API_KEY"], verify=True)
    run = wandb.init(
        project="exp11-kronecker-debug",
        name="exp11-muon-optimizer-ablation",
        config={
            "schema": SCHEMA,
            "variants": VARIANTS,
            "muon_lr_grid": MUON_LR_GRID,
            "adamw_auxiliary_lr": ADAMW_AUX_LR,
            "schedules": SCHEDULES,
            "seeds": SEEDS,
            "screen_tokens": SCREEN_TOKENS,
            "final_tokens": FINAL_TOKENS,
            "routing": "body matrices to Muon; tied vocabulary and vectors to AdamW",
            "wall_limit_seconds": wall_limit_seconds,
        },
    )
    if not run.url:
        raise RuntimeError("W&B did not provide a direct run URL")
    print(f"WANDB_URL={run.url}", flush=True)
    output_path = Path(output)
    root = output_path.parent / "muon-cells"
    heartbeat_path = Path(heartbeat) if heartbeat else None
    write_json(
        output_path,
        {
            "schema": SCHEMA,
            "status": "running",
            "wandb_url": run.url,
            "started_at_unix_seconds": time.time(),
        },
    )
    log_index = 0

    def log(values: dict[str, Any]) -> None:
        nonlocal log_index
        log_index += 1
        run.log(values, step=log_index)
        if heartbeat_path:
            heartbeat_path.parent.mkdir(parents=True, exist_ok=True)
            heartbeat_path.touch()

    if heartbeat_path:
        heartbeat_path.parent.mkdir(parents=True, exist_ok=True)
        heartbeat_path.touch()

    train_windows = load_windows(Path(data_root), "train")
    validation_windows = load_windows(Path(data_root), "validation")
    device = torch.device("cuda:0")
    preflight_recipes = {
        variant: MuonRecipe(0.03, ADAMW_AUX_LR[variant], "constant")
        for variant in VARIANTS
    }

    def optimizer_factory(
        variant: str, model: LanguageModel
    ) -> MuonWithAuxAdamW:
        optimizer, _ = create_optimizer(model, preflight_recipes[variant])
        return optimizer

    preflight_started = time.perf_counter()
    measured = preflight(
        train_windows,
        device,
        candidates=BATCH_CANDIDATES,
        variants=VARIANTS,
        log=log,
        optimizer_factory=optimizer_factory,
    )
    measured["optimizer"] = "muon+adamw"
    measured["elapsed_seconds"] = time.perf_counter() - preflight_started
    write_json(root / "preflight.json", measured)
    projected = projected_seconds(measured)
    available = wall_limit_seconds * 0.94 - measured["elapsed_seconds"]
    if projected > available:
        result = {
            "schema": SCHEMA,
            "status": "complete",
            "verdict": "insufficient_measured_throughput_for_muon_tuning",
            "projected_training_seconds": projected,
            "available_training_seconds": available,
            "preflight": measured,
            "wandb_url": run.url,
        }
        write_json(output_path, result)
        run.finish()
        return result

    batches = {
        variant: int(measured["selected"][variant]["batch"]) for variant in VARIANTS
    }
    compile_modes = {
        variant: measured["selected"][variant]["execution_mode"] == "compiled"
        for variant in VARIANTS
    }
    eval_batches = {variant: min(512, batches[variant]) for variant in VARIANTS}
    screens: dict[str, list[dict[str, Any]]] = {}
    screen_summaries: dict[str, list[dict[str, Any]]] = {}
    selected: dict[str, dict[str, Any]] = {}

    for variant in VARIANTS:
        rows: list[dict[str, Any]] = []
        recipes = initial_recipes(variant)
        for recipe in recipes:
            for seed in SEEDS:
                rows.append(
                    safe_train_cell(
                        variant,
                        recipe,
                        seed,
                        SCREEN_TOKENS,
                        output_root=root,
                        train_windows=train_windows,
                        validation_windows=validation_windows,
                        batch=batches[variant],
                        eval_batch=eval_batches[variant],
                        device=device,
                        log=log,
                        heartbeat=heartbeat_path,
                        use_compile=compile_modes[variant],
                    )
                )
        for _ in range(MAX_BOUNDARY_EXPANSIONS):
            summaries = summarize(rows)
            extension = boundary_extension(MuonRecipe(**summaries[0]["recipe"]), recipes)
            if extension is None:
                break
            recipes.append(extension)
            for seed in SEEDS:
                rows.append(
                    safe_train_cell(
                        variant,
                        extension,
                        seed,
                        SCREEN_TOKENS,
                        output_root=root,
                        train_windows=train_windows,
                        validation_windows=validation_windows,
                        batch=batches[variant],
                        eval_batch=eval_batches[variant],
                        device=device,
                        log=log,
                        heartbeat=heartbeat_path,
                        use_compile=compile_modes[variant],
                    )
                )
        best = MuonRecipe(**summarize(rows)[0]["recipe"])
        for recipe in optimizer_ablations(best):
            if recipe in recipes:
                continue
            recipes.append(recipe)
            for seed in SEEDS:
                rows.append(
                    safe_train_cell(
                        variant,
                        recipe,
                        seed,
                        SCREEN_TOKENS,
                        output_root=root,
                        train_windows=train_windows,
                        validation_windows=validation_windows,
                        batch=batches[variant],
                        eval_batch=eval_batches[variant],
                        device=device,
                        log=log,
                        heartbeat=heartbeat_path,
                        use_compile=compile_modes[variant],
                    )
                )
        summaries = summarize(rows)
        screens[variant] = rows
        screen_summaries[variant] = summaries
        selected[variant] = summaries[0]
        log(
            {
                f"selection/{variant}/muon_mean_validation_nll": summaries[0][
                    "mean_validation_nll"
                ]
            }
        )

    finals: dict[str, list[dict[str, Any]]] = {variant: [] for variant in VARIANTS}
    for variant in VARIANTS:
        recipe = MuonRecipe(**selected[variant]["recipe"])
        for seed in SEEDS:
            finals[variant].append(
                safe_train_cell(
                    variant,
                    recipe,
                    seed,
                    FINAL_TOKENS,
                    output_root=root,
                    train_windows=train_windows,
                    validation_windows=validation_windows,
                    batch=batches[variant],
                    eval_batch=eval_batches[variant],
                    device=device,
                    log=log,
                    heartbeat=heartbeat_path,
                    use_compile=compile_modes[variant],
                )
            )
    unstable = {
        variant: [int(cell["seed"]) for cell in cells if cell.get("status") != "complete"]
        for variant, cells in finals.items()
    }
    if any(unstable.values()):
        result = {
            "schema": SCHEMA,
            "status": "complete",
            "verdict": "muon_unstable_at_final_horizon",
            "wandb_url": run.url,
            "preflight": measured,
            "screens": screens,
            "screen_summaries": screen_summaries,
            "selected": selected,
            "finals": finals,
            "unstable_final_seeds": unstable,
            "test_opened_after_selection": False,
        }
        write_json(output_path, result)
        run.finish()
        return result

    test_windows = load_windows(Path(data_root), "test")
    for variant in VARIANTS:
        tested = []
        for cell in finals[variant]:
            model = load_checkpoint_model(cell, device)
            tested.append(
                {
                    **cell,
                    "test": evaluate(
                        model,
                        test_windows,
                        eval_batches[variant],
                        device,
                        include_mechanism=variant.startswith("order3"),
                    ),
                }
            )
            del model
            gc.collect()
            torch.cuda.empty_cache()
        finals[variant] = tested

    reference = load_adamw_reference()
    muon_tests = {
        variant: [float(cell["test"]["nll"]) for cell in finals[variant]]
        for variant in VARIANTS
    }
    adamw_tests = {
        variant: [
            float(reference["models"][variant]["test_nll_by_seed"][str(seed)])
            for seed in SEEDS
        ]
        for variant in VARIANTS
    }
    within_architecture = {
        variant: paired_comparison(
            muon_tests[variant],
            adamw_tests[variant],
            label=f"{variant} Muon minus AdamW",
        )
        for variant in VARIANTS
    }
    muon_matched = paired_comparison(
        muon_tests["order3-r8"],
        muon_tests["transformer"],
        label="order3-r8 Muon minus Transformer Muon",
    )
    selected_families = {}
    best_tests = {}
    for variant in VARIANTS:
        muon_validation = float(selected[variant]["mean_validation_nll"])
        adamw_validation = float(
            reference["selection"][variant]["mean_validation_nll"]
        )
        family = "muon" if muon_validation < adamw_validation else "adamw"
        selected_families[variant] = {
            "optimizer": family,
            "muon_validation_nll": muon_validation,
            "adamw_validation_nll": adamw_validation,
            "selection_basis": "three-seed 10M-token validation NLL",
        }
        best_tests[variant] = muon_tests[variant] if family == "muon" else adamw_tests[variant]
    best_matched = paired_comparison(
        best_tests["order3-r8"],
        best_tests["transformer"],
        label="order3-r8 best optimizer minus Transformer best optimizer",
    )
    upper = best_matched["bootstrap_95_percent_interval"][1]
    pass_quality = upper <= 0.02
    result = {
        "schema": SCHEMA,
        "status": "complete",
        "verdict": "muon_rescues_order3" if pass_quality else "muon_does_not_rescue_order3",
        "wandb_url": run.url,
        "preflight": measured,
        "projected_training_seconds": projected,
        "selection_basis": "lowest three-seed mean validation NLL at 10M tokens",
        "test_opened_after_selection": True,
        "optimizer_routing": "all body matrices to Muon; tied vocabulary and vectors to AdamW",
        "screens": screens,
        "screen_summaries": screen_summaries,
        "selected_muon_recipes": selected,
        "finals": finals,
        "adamw_reference_experiment": reference["experiment_id"],
        "selected_optimizer_families": selected_families,
        "comparisons": {
            "within_architecture": within_architecture,
            "muon_parameter_matched": muon_matched,
            "best_optimizer_parameter_matched": {
                **best_matched,
                "maximum_allowed_upper_nll_bound": 0.02,
                "pass_quality": pass_quality,
                "order3_to_transformer_body_parameter_ratio": (
                    reference["matched_comparison"][
                        "order3_to_transformer_body_parameter_ratio"
                    ]
                ),
            },
        },
    }
    write_json(output_path, result)
    log(
        {
            "gate/muon_rescues_order3": int(pass_quality),
            "gate/best_optimizer_upper_nll": upper,
        }
    )
    run.finish()
    return result
