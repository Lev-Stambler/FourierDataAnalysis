"""Cloud-only conditioning repair and small-data evidence ladder for Exp19.

This module deliberately stops at the corpus boundary.  A corpus launch is a
new experiment and is authorized only by the paired held-out gate emitted here.
"""

from __future__ import annotations

import argparse
import gc
import hashlib
import inspect
import json
import math
import multiprocessing as mp
import os
import queue
import statistics
import threading
import time
from dataclasses import asdict
from pathlib import Path
from typing import Any, Iterable, Mapping, Sequence

import numpy as np
import torch
import torch.nn.functional as F
from fla.modules import FusedLinearCrossEntropyLoss

import exp14_block_kronecker.campaign as base
import exp17_group_density.campaign as exp17
from exp14_block_kronecker.data import CONTEXT_LENGTH, GROUP_SIZE, block_batch, load_windows
from exp17_group_density.campaign import Recipe

from . import model as architectures
from .telemetry import telemetry as architecture_telemetry


SCHEMA = "exp19-norm-residual-campaign-v1"
CELL_SCHEMA = "exp19-norm-residual-cell-v1"
PREFLIGHT_SCHEMA = "exp19-norm-residual-preflight-v1"
STAGE_SNAPSHOT_SCHEMA = "exp19-norm-residual-stage-snapshot-v1"
GPU_COUNT = 8
MODEL_NAMES = tuple(architectures.MODEL_NAMES)
LEGACY = architectures.LEGACY_POSTNORM_R1
PRIMARY = architectures.CLEAN_GROUP_R1_TOKEN
TOKEN_CONTROL = architectures.CORRECTED_NO_ROUTER_TOKEN
SHAPE_TRANSFORMER = architectures.CORRECTED_TRANSFORMER_DEEP
PRACTICAL_TRANSFORMER = architectures.CORRECTED_TRANSFORMER_WIDE
NONPROMOTABLE_DIAGNOSTIC_MODELS = (
    architectures.PRENORM_AFFINE_FREE_SCALED,
    architectures.PRENORM_LEARNED_SCALED,
)
TRAINING_MODEL_NAMES = tuple(
    name for name in MODEL_NAMES if name not in NONPROMOTABLE_DIAGNOSTIC_MODELS
)
CORRECTED_GROUP_MODELS = (
    architectures.CLEAN_GROUP_R1_JOINT,
    PRIMARY,
)
CONDITIONING_REQUIRED_MODELS = (
    architectures.CLEAN_GROUP_R1_JOINT,
    PRIMARY,
    TOKEN_CONTROL,
    SHAPE_TRANSFORMER,
    PRACTICAL_TRANSFORMER,
)
TARGET_PARAMETERS = 5_400_896
MAXIMUM_PARAMETER_MISMATCH = 0.001
# This cumulative ablation intentionally adds 8,320 learned RMS gains before
# the following clean-scale track removes redundant gauges.  Preserve that
# single semantic delta instead of quietly changing its FFN dimensions.
DIAGNOSTIC_PARAMETER_TOLERANCES = {
    architectures.PRENORM_LEARNED_SCALED: 0.002,
}
EXECUTION_MODE = "default"
MINIMUM_GLOBAL_TOKENS = 100_000
MINIMUM_UTILIZATION = 85.0
MINIMUM_INITIAL_AGGREGATE_BRANCH_ENERGY = 0.05
MAXIMUM_INITIAL_AGGREGATE_BRANCH_ENERGY = 0.75
BATCH_SEARCH = (4096, 3072, 2048, 1536, 1024, 768, 640, 512, 400)
UNDERFILLED_BATCH = 128
SAMPLE_INDICES = (17, 997)
SCREEN_SEED = 1901
CONFIRMATION_SEEDS = (1902, 1903, 1904)
LADDER_SEEDS = (1911, 1912, 1913)
SCREEN_STEPS = 64
CONFIRMATION_STEPS = 128
LADDER_STEPS = 128
LADDER_SIZES = (8, 32, 128)
HELDOUT_EXAMPLES = 2048
SUCCESS_NLL = 0.01
EVALUATION_INTERVAL = 4
LADDER_EVALUATION_INTERVAL = 16
MINIMUM_NLL_WIN = 0.02
MAXIMUM_BRANCH_STATE_RATIO = 0.5
MAXIMUM_LAYER_IMBALANCE = 100.0
MINIMUM_EFFECTIVE_UPDATE_FRACTION = 0.25


def write_json(path: str | Path, value: Any) -> None:
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_text(json.dumps(value, indent=2, sort_keys=True))
    os.replace(temporary, path)


def recipe_slug(recipe: Recipe) -> str:
    return exp17.recipe_slug(recipe)


def screen_recipes() -> tuple[Recipe, ...]:
    """The preregistered six-cell, regularization-free optimizer screen."""

    adamw = tuple(
        Recipe("adamw", lr, lr, weight_decay=0.0, warmup_tokens=1)
        for lr in (0.003, 0.012, 0.048)
    )
    muon = tuple(
        Recipe("muon", body, auxiliary, weight_decay=0.0, warmup_tokens=1)
        for body, auxiliary in ((0.06, 0.012), (0.24, 0.048), (0.96, 0.096))
    )
    return (*adamw, *muon)


def balanced_repetition(unique_count: int, physical_batch: int) -> np.ndarray:
    """Return a deterministic balanced physical batch over unique examples."""

    if unique_count <= 0 or physical_batch < unique_count:
        raise ValueError("physical batch must contain every unique example")
    return np.arange(physical_batch, dtype=np.int64) % unique_count


def exposure_accounting(
    *, unique_count: int, physical_batch: int, steps: int
) -> dict[str, int | float]:
    """Keep duplicate physical work distinct from unique-data exposure."""

    if min(unique_count, physical_batch, steps) <= 0 or physical_batch < unique_count:
        raise ValueError("invalid exposure accounting inputs")
    return {
        "unique_examples": unique_count,
        "physical_batch": physical_batch,
        "global_examples_per_step": physical_batch,
        "global_tokens_per_step": physical_batch * CONTEXT_LENGTH,
        "unique_examples_per_step": unique_count,
        "unique_tokens_per_step": unique_count * CONTEXT_LENGTH,
        "duplicate_factor": physical_batch / unique_count,
        "optimizer_steps": steps,
        "physical_example_exposures": steps * physical_batch,
        "physical_tokens_processed": steps * physical_batch * CONTEXT_LENGTH,
        "unique_example_exposures": steps * unique_count,
        "unique_token_exposures": steps * unique_count * CONTEXT_LENGTH,
        "gradient_accumulation": 1,
    }


def _threshold_hit(row: Mapping[str, Any]) -> int | None:
    hit = row.get("threshold_hits", {}).get("nll_le_0.01")
    return None if hit is None else int(hit["step"])


def cell_score(row: Mapping[str, Any]) -> tuple[Any, ...]:
    hit = _threshold_hit(row)
    return (
        not bool(row.get("success")),
        math.inf if hit is None else hit,
        float(row.get("final", {}).get("nll", math.inf)),
        float(row.get("performance", {}).get("elapsed_seconds", math.inf)),
    )


def promote_screen(rows: Sequence[dict[str, Any]]) -> dict[str, dict[str, Recipe]]:
    """Promote one stable recipe per model and optimizer family."""

    result: dict[str, dict[str, Recipe]] = {}
    for name in TRAINING_MODEL_NAMES:
        result[name] = {}
        for family in ("adamw", "muon"):
            candidates = [
                row
                for row in rows
                if row.get("status") == "complete"
                and row.get("model") == name
                and row.get("recipe", {}).get("family") == family
            ]
            if not candidates:
                raise RuntimeError(f"no stable {family} screen cell for {name}")
            result[name][family] = Recipe(**min(candidates, key=cell_score)["recipe"])
    return result


def summarize_confirmation(
    rows: Sequence[dict[str, Any]],
) -> tuple[dict[str, Any], dict[str, Recipe]]:
    """Select the best replicated family, never a lucky individual seed."""

    summary: dict[str, Any] = {}
    winners: dict[str, Recipe] = {}
    for name in TRAINING_MODEL_NAMES:
        families: list[dict[str, Any]] = []
        for family in ("adamw", "muon"):
            values = [
                row
                for row in rows
                if row.get("model") == name
                and row.get("recipe", {}).get("family") == family
            ]
            if len(values) != len(CONFIRMATION_SEEDS):
                raise RuntimeError(f"incomplete confirmation for {name}/{family}")
            hits = [_threshold_hit(row) for row in values]
            families.append(
                {
                    "family": family,
                    "recipe": values[0]["recipe"],
                    "successful_seeds": sum(bool(row.get("success")) for row in values),
                    "mean_steps_to_success": (
                        statistics.fmean(int(hit) for hit in hits if hit is not None)
                        if all(hit is not None for hit in hits)
                        else None
                    ),
                    "mean_final_nll": statistics.fmean(
                        float(row.get("final", {}).get("nll", math.inf)) for row in values
                    ),
                }
            )
        winner = min(
            families,
            key=lambda row: (
                -int(row["successful_seeds"]),
                math.inf
                if row["mean_steps_to_success"] is None
                else float(row["mean_steps_to_success"]),
                float(row["mean_final_nll"]),
            ),
        )
        summary[name] = {"optimizer_results": families, "winner": winner}
        winners[name] = Recipe(**winner["recipe"])
    return summary, winners


def paired_interval(differences: Sequence[float]) -> dict[str, float | int]:
    """Deterministic paired normal interval; three seeds are preregistered."""

    if len(differences) < 2:
        raise ValueError("paired interval needs at least two values")
    mean = statistics.fmean(differences)
    standard_error = statistics.stdev(differences) / math.sqrt(len(differences))
    return {
        "pairs": len(differences),
        "mean": mean,
        "lower_95": mean - 1.96 * standard_error,
        "upper_95": mean + 1.96 * standard_error,
    }


def ladder_decision(rows: Sequence[dict[str, Any]]) -> dict[str, Any]:
    """Apply the preregistered held-out/conditioning gate at the 128 rung."""

    by_model_seed = {
        (str(row["model"]), int(row["seed"])): row
        for row in rows
        if int(row.get("unique_count", 0)) == max(LADDER_SIZES)
        and row.get("status") == "complete"
    }
    comparisons: dict[str, Any] = {}
    passing: list[str] = []
    required_controls = (LEGACY, TOKEN_CONTROL, SHAPE_TRANSFORMER)
    for candidate in CORRECTED_GROUP_MODELS:
        candidate_rows = [by_model_seed.get((candidate, seed)) for seed in LADDER_SEEDS]
        if any(row is None for row in candidate_rows):
            comparisons[candidate] = {"pass": False, "reason": "missing candidate seed"}
            continue
        if not all(bool(row.get("conditioning_gate", {}).get("pass")) for row in candidate_rows):
            comparisons[candidate] = {"pass": False, "reason": "conditioning gate failed"}
            continue
        controls: dict[str, Any] = {}
        candidate_pass = True
        for control in required_controls:
            differences: list[float] = []
            for seed, candidate_row in zip(LADDER_SEEDS, candidate_rows, strict=True):
                control_row = by_model_seed.get((control, seed))
                if control_row is None:
                    differences = []
                    break
                differences.append(
                    float(candidate_row["heldout"]["nll"])
                    - float(control_row["heldout"]["nll"])
                )
            if len(differences) != len(LADDER_SEEDS):
                controls[control] = {"pass": False, "reason": "missing control seed"}
                candidate_pass = False
                continue
            interval = paired_interval(differences)
            passed = (
                all(value <= -MINIMUM_NLL_WIN for value in differences)
                and float(interval["upper_95"]) < 0.0
            )
            controls[control] = {
                "pass": passed,
                "candidate_minus_control_nll": differences,
                "paired_95_interval": interval,
                "required_every_seed_margin": -MINIMUM_NLL_WIN,
            }
            candidate_pass &= passed
        comparisons[candidate] = {"pass": candidate_pass, "controls": controls}
        if candidate_pass:
            passing.append(candidate)
    return {
        "corpus_ready": bool(passing),
        "passing_corrected_group_models": passing,
        "comparisons": comparisons,
        "required_controls": list(required_controls),
        "minimum_nll_win": MINIMUM_NLL_WIN,
        "next_step": (
            "separately authorize a corpus experiment for the passing candidate"
            if passing
            else "stop before corpus and debug the failed small-data mechanism gate"
        ),
    }


def _build_model(name: str, checkpointing: bool | None = None) -> torch.nn.Module:
    """Small compatibility adapter while keeping checkpoint parity mandatory."""

    if checkpointing is None:
        return architectures.build_model(name)
    return architectures.build_model(name, activation_checkpointing=checkpointing)


def compile_hidden(model: torch.nn.Module) -> Any:
    return torch.compile(
        model.hidden,
        backend="inductor",
        mode=EXECUTION_MODE,
        fullgraph=True,
        dynamic=True,
    )


def _tensor_comparison(
    first: Mapping[str, torch.Tensor], second: Mapping[str, torch.Tensor]
) -> dict[str, Any]:
    if first.keys() != second.keys():
        raise RuntimeError("tensor maps do not have identical keys")
    relative: list[tuple[float, str, float, float]] = []
    cosines: list[tuple[float, str]] = []
    absolute: list[tuple[float, str]] = []
    left_square = right_square = delta_square = dot = 0.0
    for name in first:
        left, right = first[name].float(), second[name].float()
        delta = left - right
        left_norm = float(left.norm())
        right_norm = float(right.norm())
        delta_norm = float(delta.norm())
        relative.append(
            (delta_norm / max(left_norm, 1e-30), name, left_norm, right_norm)
        )
        absolute.append((float(delta.abs().max()), name))
        left_square += left_norm * left_norm
        right_square += right_norm * right_norm
        delta_square += delta_norm * delta_norm
        dot += float((left.flatten() @ right.flatten()))
        if left.numel() and left_norm and right_norm:
            cosines.append(
                (
                    float(
                        F.cosine_similarity(
                            left.flatten(), right.flatten(), dim=0
                        )
                    ),
                    name,
                )
            )
    worst_relative = max(relative, default=(0.0, "", 0.0, 0.0))
    worst_absolute = max(absolute, default=(0.0, ""))
    worst_cosine = min(cosines, default=(1.0, ""))
    return {
        "global_relative_error": math.sqrt(delta_square)
        / max(math.sqrt(left_square), 1e-30),
        "global_cosine": dot
        / max(math.sqrt(left_square * right_square), 1e-30),
        # Keep the raw per-tensor extrema as named diagnostics, but do not use
        # a relative error with a near-zero denominator as the hard gate.
        "maximum_tensor_relative_error_raw": worst_relative[0],
        "maximum_tensor_relative_error_parameter": worst_relative[1],
        "maximum_tensor_relative_error_left_norm": worst_relative[2],
        "maximum_tensor_relative_error_right_norm": worst_relative[3],
        "maximum_absolute_error": worst_absolute[0],
        "maximum_absolute_error_parameter": worst_absolute[1],
        "minimum_tensor_cosine": worst_cosine[0],
        "minimum_tensor_cosine_parameter": worst_cosine[1],
    }


def _gradient_map(model: torch.nn.Module) -> dict[str, torch.Tensor]:
    missing = [name for name, parameter in model.named_parameters() if parameter.grad is None]
    if missing:
        raise RuntimeError(f"parameters without gradients: {missing[:20]}")
    result = {name: parameter.grad.detach().clone() for name, parameter in model.named_parameters()}
    if not all(torch.isfinite(value).all() for value in result.values()):
        raise RuntimeError("non-finite parameter gradient")
    return result


def _loss_and_backward(
    model: torch.nn.Module,
    inputs: torch.Tensor,
    targets: torch.Tensor,
    *,
    compiled: bool,
    bf16: bool,
) -> tuple[float, dict[str, torch.Tensor]]:
    model.zero_grad(set_to_none=True)
    hidden = compile_hidden(model) if compiled else model.hidden
    context = torch.autocast("cuda", dtype=torch.bfloat16, enabled=bf16)
    with context:
        loss = base.fused_loss(
            model, inputs, targets, FusedLinearCrossEntropyLoss(), hidden
        )
    loss.backward()
    return float(loss.detach()), _gradient_map(model)


def _architecture_checks(name: str, model: torch.nn.Module, device: torch.device) -> dict[str, Any]:
    function = getattr(architectures, "correctness_checks", None)
    if function is None:
        function = getattr(model, "correctness_checks", None)
    if function is None:
        raise RuntimeError("Exp19 model must expose correctness_checks")
    signature = inspect.signature(function)
    keyword: dict[str, Any] = {}
    if "model" in signature.parameters:
        keyword["model"] = model
    if "name" in signature.parameters:
        keyword["name"] = name
    if "device" in signature.parameters:
        keyword["device"] = device
    result = function(**keyword)
    if not isinstance(result, dict) or result.get("pass") is not True:
        raise RuntimeError(f"architecture correctness failed for {name}: {result}")
    return result


def correctness_cell(task: dict[str, Any], device: torch.device) -> dict[str, Any]:
    """Hard forward/gradient/checkpoint/compile/optimizer parity gate."""

    name = str(task["model"])
    windows = load_windows(task["data_root"], "train")
    torch.manual_seed(1900)
    inputs, targets = block_batch(windows, np.asarray([17, 997]), device)
    eager = _build_model(name, checkpointing=False).to(device)
    checkpointed = _build_model(name, checkpointing=True).to(device)
    checkpointed.load_state_dict(eager.state_dict())
    architecture = _architecture_checks(name, eager, device)

    eager_loss, eager_gradients = _loss_and_backward(
        eager, inputs, targets, compiled=False, bf16=False
    )
    checkpoint_loss, checkpoint_gradients = _loss_and_backward(
        checkpointed, inputs, targets, compiled=False, bf16=False
    )
    checkpoint_parity = _tensor_comparison(eager_gradients, checkpoint_gradients)
    checkpoint_parity["absolute_loss_error"] = abs(eager_loss - checkpoint_loss)
    checkpoint_parity["pass"] = (
        checkpoint_parity["global_relative_error"] <= 1e-5
        and checkpoint_parity["maximum_absolute_error"] <= 1e-6
        and checkpoint_parity["absolute_loss_error"] <= 1e-6
    )

    bf16_eager = _build_model(name, checkpointing=False).to(device)
    bf16_compiled = _build_model(name, checkpointing=False).to(device)
    bf16_eager.load_state_dict(eager.state_dict())
    bf16_compiled.load_state_dict(eager.state_dict())
    bf16_loss, bf16_gradients = _loss_and_backward(
        bf16_eager, inputs, targets, compiled=False, bf16=True
    )
    compiled_loss, compiled_gradients = _loss_and_backward(
        bf16_compiled, inputs, targets, compiled=True, bf16=True
    )
    compiled_parity = _tensor_comparison(bf16_gradients, compiled_gradients)
    compiled_parity["relative_loss_error"] = abs(bf16_loss - compiled_loss) / max(
        abs(bf16_loss), 1e-12
    )
    compiled_parity["pass"] = (
        compiled_parity["global_relative_error"] <= 0.02
        and compiled_parity["global_cosine"] >= 0.999
        and compiled_parity["maximum_absolute_error"] <= 0.002
        and compiled_parity["relative_loss_error"] <= 0.001
    )

    # Compare the same weights: the prior implementation accidentally formed
    # this reference after applying the optimizer update.
    with torch.no_grad():
        eager_reference = float(
            F.cross_entropy(eager(inputs).float().flatten(0, 1), targets.flatten())
        )

    # A real optimizer step catches checkpoint recomputation side effects that
    # gradient comparison alone can miss.
    before = {name: value.detach().clone() for name, value in eager.named_parameters()}
    before_checkpoint = {
        name: value.detach().clone() for name, value in checkpointed.named_parameters()
    }
    eager_optimizer = torch.optim.AdamW(eager.parameters(), lr=0.003, weight_decay=0.0)
    checkpoint_optimizer = torch.optim.AdamW(
        checkpointed.parameters(), lr=0.003, weight_decay=0.0
    )
    eager_optimizer.step()
    checkpoint_optimizer.step()
    eager_delta = {
        name: parameter.detach() - before[name] for name, parameter in eager.named_parameters()
    }
    checkpoint_delta = {
        name: parameter.detach() - before_checkpoint[name]
        for name, parameter in checkpointed.named_parameters()
    }
    step_parity = _tensor_comparison(eager_delta, checkpoint_delta)
    step_parity["pass"] = (
        step_parity["global_relative_error"] <= 1e-4
        and step_parity["global_cosine"] >= 0.99999
        and step_parity["maximum_absolute_error"] <= 5e-4
    )

    fused_relative = abs(eager_loss - eager_reference) / max(abs(eager_reference), 1e-12)
    fused_agreement = {
        "fp32_materialized_nll": eager_reference,
        "fp32_fused_nll": eager_loss,
        "relative_error": fused_relative,
        "pass": fused_relative <= 0.001,
    }
    routing: dict[str, Any] = {}
    for recipe in (screen_recipes()[0], screen_recipes()[3]):
        probe = _build_model(name).to(device)
        optimizer, record = exp17.create_optimizer(probe, recipe)
        routing[recipe.family] = record
        del optimizer, probe
    initial_telemetry = architecture_telemetry(
        bf16_eager,
        inputs[:1],
        layers=range(len(bf16_eager.blocks)),
        include_spectra=False,
    )
    branch_gate = initial_branch_gate(initial_telemetry)
    branch_gate_required = name in CONDITIONING_REQUIRED_MODELS
    passed = all(
        row["pass"]
        for row in (architecture, checkpoint_parity, compiled_parity, step_parity, fused_agreement)
    ) and (branch_gate["pass"] or not branch_gate_required)
    result = {
        "status": "complete" if passed else "failed",
        "kind": "correctness",
        "model": name,
        "architecture": architecture,
        "checkpoint_gradient_parity": checkpoint_parity,
        "compiled_bf16_gradient_parity": compiled_parity,
        "checkpoint_optimizer_step_parity": step_parity,
        "fused_loss_agreement": fused_agreement,
        "optimizer_routing": routing,
        "all_parameters_receive_finite_gradients": True,
        "initial_telemetry": initial_telemetry,
        "initial_branch_gate": branch_gate,
        "initial_branch_gate_required": branch_gate_required,
    }
    del eager, checkpointed, bf16_eager, bf16_compiled, inputs, targets
    gc.collect()
    torch.cuda.empty_cache()
    return result


def _rms(tensor: torch.Tensor) -> float:
    return float(tensor.float().square().mean().sqrt())


def _parameter_family(name: str) -> str:
    if name.startswith(("embedding", "vocabulary")):
        return "vocabulary"
    if ".norm" in name or "rms" in name or name.endswith("gain"):
        return "norm_or_scale"
    if "factor" in name or any(token in name for token in ("outer", "workspace", "channel")):
        return "factor"
    if "ffn" in name or any(token in name for token in ("gate", "up", "down")):
        return "ffn"
    return "other"


def parameter_update_telemetry(
    model: torch.nn.Module, before: Mapping[str, torch.Tensor]
) -> dict[str, Any]:
    """Gradient and realized-update telemetry grouped by layer and family."""

    rows: list[dict[str, Any]] = []
    for name, parameter in model.named_parameters():
        gradient = parameter.grad
        if gradient is None:
            continue
        old = before[name].to(parameter.device)
        update = parameter.detach() - old
        weight_rms = _rms(old)
        update_rms = _rms(update)
        gradient_rms = _rms(gradient)
        flat_old, flat_gradient, flat_update = (
            value.float().flatten() for value in (old, gradient, update)
        )
        old_norm = flat_old.norm().clamp_min(1e-12)
        radial_gradient = (flat_gradient @ flat_old) / old_norm
        radial_update = (flat_update @ flat_old) / old_norm
        tangent_gradient = (flat_gradient.square().sum() - radial_gradient.square()).clamp_min(0).sqrt()
        tangent_update = (flat_update.square().sum() - radial_update.square()).clamp_min(0).sqrt()
        layer = None
        pieces = name.split(".")
        if "blocks" in pieces:
            index = pieces.index("blocks")
            if index + 1 < len(pieces) and pieces[index + 1].isdigit():
                layer = int(pieces[index + 1])
        row = {
            "name": name,
            "layer": layer,
            "family": _parameter_family(name),
            "weight_rms": weight_rms,
            "gradient_rms": gradient_rms,
            "update_rms": update_rms,
            "update_to_weight_rms": update_rms / max(weight_rms, 1e-12),
            "gradient_tangent_fraction": float(
                tangent_gradient / flat_gradient.norm().clamp_min(1e-12)
            ),
            "update_tangent_fraction": float(
                tangent_update / flat_update.norm().clamp_min(1e-12)
            ),
        }
        if row["family"] == "factor" and parameter.ndim >= 2:
            normalized_old = F.normalize(old.float(), dim=-1)
            normalized_new = F.normalize(parameter.detach().float(), dim=-1)
            effective = (normalized_new - normalized_old).norm()
            raw = update.float().norm().clamp_min(1e-12)
            row["effective_normalized_update_fraction"] = float(effective / raw)
        rows.append(row)
    layer_gradients: dict[int, float] = {}
    for layer in sorted({int(row["layer"]) for row in rows if row["layer"] is not None}):
        values = [float(row["gradient_rms"]) for row in rows if row["layer"] == layer]
        layer_gradients[layer] = statistics.fmean(values)
    positive = [value for value in layer_gradients.values() if value > 0]
    factor_effective = [
        float(row["effective_normalized_update_fraction"])
        for row in rows
        if "effective_normalized_update_fraction" in row
    ]
    return {
        "parameters": rows,
        "layer_gradient_rms": {str(key): value for key, value in layer_gradients.items()},
        "layer_gradient_imbalance": (
            max(positive) / min(positive) if positive else math.inf
        ),
        "minimum_effective_normalized_update_fraction": (
            min(factor_effective) if factor_effective else 1.0
        ),
    }


def conditioning_gate(
    architecture_telemetry: Mapping[str, Any],
    optimization_telemetry: Mapping[str, Any] | None = None,
) -> dict[str, Any]:
    """Consume the stable summary contract returned by ``model.telemetry``."""

    summary = architecture_telemetry.get("conditioning", architecture_telemetry)
    ratios = [float(value) for value in summary.get("scaled_update_to_state_ratios", [])]
    variance_ratio = float(summary.get("first_to_last_hidden_variance_ratio", 1.0))
    gradient_imbalance = (
        float(optimization_telemetry.get("layer_gradient_imbalance", math.inf))
        if optimization_telemetry
        else None
    )
    effective_fraction = (
        float(
            optimization_telemetry.get(
                "minimum_effective_normalized_update_fraction", 1.0
            )
        )
        if optimization_telemetry
        else None
    )
    failures: list[str] = []
    if not ratios:
        failures.append("telemetry_missing_scaled_update_to_state_ratios")
    elif max(ratios) > MAXIMUM_BRANCH_STATE_RATIO:
        failures.append("branch_state_ratio")
    if not math.isfinite(variance_ratio) or variance_ratio > MAXIMUM_LAYER_IMBALANCE:
        failures.append("hidden_variance_imbalance")
    if gradient_imbalance is not None and (
        not math.isfinite(gradient_imbalance)
        or gradient_imbalance > MAXIMUM_LAYER_IMBALANCE
    ):
        failures.append("gradient_imbalance")
    if effective_fraction is not None and effective_fraction < MINIMUM_EFFECTIVE_UPDATE_FRACTION:
        failures.append("ineffective_normalized_factor_update")
    return {
        "pass": not failures,
        "failures": failures,
        "maximum_scaled_update_to_state_ratio": max(ratios, default=math.inf),
        "first_to_last_hidden_variance_ratio": variance_ratio,
        "layer_gradient_imbalance": gradient_imbalance,
        "minimum_effective_normalized_update_fraction": effective_fraction,
        "thresholds": {
            "maximum_branch_state_ratio": MAXIMUM_BRANCH_STATE_RATIO,
            "maximum_layer_imbalance": MAXIMUM_LAYER_IMBALANCE,
            "minimum_effective_update_fraction": MINIMUM_EFFECTIVE_UPDATE_FRACTION,
        },
    }


def initial_branch_gate(report: Mapping[str, Any]) -> dict[str, Any]:
    ratios = [
        float(value)
        for value in report.get("conditioning", {}).get(
            "scaled_update_to_state_ratios", []
        )
    ]
    finite = bool(ratios) and all(math.isfinite(value) for value in ratios)
    in_target = [math.isfinite(value) and 0.02 <= value <= 0.20 for value in ratios]
    target_fraction = sum(in_target) / max(len(in_target), 1)
    aggregate_energy = (
        math.sqrt(math.fsum(value * value for value in ratios))
        if finite
        else math.nan
    )
    passed = (
        finite
        and max(ratios, default=math.inf) <= MAXIMUM_BRANCH_STATE_RATIO
        and MINIMUM_INITIAL_AGGREGATE_BRANCH_ENERGY
        <= aggregate_energy
        <= MAXIMUM_INITIAL_AGGREGATE_BRANCH_ENERGY
    )
    return {
        "pass": passed,
        "finite_nonempty": finite,
        "ratio_count": len(ratios),
        "target_interval": [0.02, 0.20],
        "fraction_in_target_interval": target_fraction,
        "fraction_is_diagnostic_only": True,
        "minimum": min(ratios, default=math.nan),
        "maximum": max(ratios, default=math.nan),
        "absolute_maximum": MAXIMUM_BRANCH_STATE_RATIO,
        "aggregate_rss": aggregate_energy,
        "aggregate_rss_interval": [
            MINIMUM_INITIAL_AGGREGATE_BRANCH_ENERGY,
            MAXIMUM_INITIAL_AGGREGATE_BRANCH_ENERGY,
        ],
    }


@torch.inference_mode()
def exact_metrics(
    model: torch.nn.Module,
    inputs: torch.Tensor,
    targets: torch.Tensor,
    *,
    evaluation_batch: int = 16,
) -> dict[str, Any]:
    """Exact NLL/accuracy, including causal-group and workspace position."""

    model.eval()
    loss_sum = 0.0
    correct_sum = 0
    token_count = 0
    logit_square_sum = 0.0
    logit_count = 0
    group_loss = torch.zeros(CONTEXT_LENGTH // GROUP_SIZE, device=inputs.device)
    group_correct = torch.zeros_like(group_loss)
    position_loss = torch.zeros(GROUP_SIZE, device=inputs.device)
    position_correct = torch.zeros_like(position_loss)
    example_nll: list[float] = []
    example_accuracy: list[float] = []
    for start in range(0, len(inputs), evaluation_batch):
        batch_inputs = inputs[start : start + evaluation_batch]
        batch_targets = targets[start : start + evaluation_batch]
        logits = model(batch_inputs).float()
        losses = F.cross_entropy(
            logits.flatten(0, 1), batch_targets.flatten(), reduction="none"
        ).reshape_as(batch_targets)
        correct = logits.argmax(-1).eq(batch_targets)
        logit_square_sum += float(torch.linalg.vector_norm(logits)) ** 2
        logit_count += logits.numel()
        loss_sum += float(losses.sum())
        correct_sum += int(correct.sum())
        token_count += correct.numel()
        shaped_loss = losses.reshape(len(losses), -1, GROUP_SIZE)
        shaped_correct = correct.reshape_as(shaped_loss)
        group_loss += shaped_loss.sum((0, 2))
        group_correct += shaped_correct.float().sum((0, 2))
        position_loss += shaped_loss.sum((0, 1))
        position_correct += shaped_correct.float().sum((0, 1))
        example_nll.extend(losses.mean(1).cpu().tolist())
        example_accuracy.extend(correct.float().mean(1).cpu().tolist())
        del logits
    group_denominator = len(inputs) * GROUP_SIZE
    position_denominator = len(inputs) * (CONTEXT_LENGTH // GROUP_SIZE)
    return {
        "nll": loss_sum / token_count,
        "token_accuracy": correct_sum / token_count,
        "correct_tokens": correct_sum,
        "total_tokens": token_count,
        "logit_rms": math.sqrt(logit_square_sum / max(logit_count, 1)),
        "worst_example_nll": max(example_nll),
        "worst_example_token_accuracy": min(example_accuracy),
        "example_nll": example_nll,
        "example_token_accuracy": example_accuracy,
        "causal_group_nll": (group_loss / group_denominator).cpu().tolist(),
        "causal_group_token_accuracy": (group_correct / group_denominator).cpu().tolist(),
        "workspace_position_nll": (position_loss / position_denominator).cpu().tolist(),
        "workspace_position_token_accuracy": (
            position_correct / position_denominator
        ).cpu().tolist(),
    }


def _threshold_hits(curve: Sequence[Mapping[str, Any]]) -> dict[str, Any]:
    result: dict[str, Any] = {}
    for threshold in (1.0, 0.1, SUCCESS_NLL, 0.001):
        values = [row for row in curve if float(row["nll"]) <= threshold]
        result[f"nll_le_{threshold:g}"] = (
            {
                "step": int(values[0]["step"]),
                "elapsed_seconds": float(values[0]["elapsed_seconds"]),
            }
            if values
            else None
        )
    return result


def _sample_digest(inputs: torch.Tensor, targets: torch.Tensor) -> str:
    digest = hashlib.sha256()
    digest.update(inputs.detach().cpu().numpy().tobytes())
    digest.update(targets.detach().cpu().numpy().tobytes())
    return digest.hexdigest()


def _cell_path(root: Path, task: Mapping[str, Any], recipe: Recipe) -> Path:
    return (
        root
        / "cells"
        / str(task["stage"])
        / str(task["model"])
        / f"unique-{len(task['sample_indices'])}"
        / f"recipe-{recipe_slug(recipe)}-seed-{int(task['seed'])}.json"
    )


def fit_cell(task: dict[str, Any], device: torch.device) -> dict[str, Any]:
    name = str(task["model"])
    recipe = Recipe(**task["recipe"])
    seed = int(task["seed"])
    indices = tuple(int(value) for value in task["sample_indices"])
    unique_count = len(indices)
    physical_batch = int(task["batch"])
    maximum_steps = int(task["maximum_steps"])
    stage = str(task["stage"])
    result_path = _cell_path(Path(task["output_root"]), task, recipe)
    if result_path.is_file():
        prior = json.loads(result_path.read_text())
        if prior.get("status") == "complete":
            return prior
    train_windows = load_windows(task["data_root"], "train")
    validation_windows = load_windows(task["data_root"], "validation")
    if max(indices) >= len(train_windows) - 1:
        raise IndexError("sample index lacks a continuation window")
    torch.manual_seed(seed)
    torch.cuda.empty_cache()
    torch.cuda.reset_peak_memory_stats(device)
    model = _build_model(name).to(device)
    hidden = compile_hidden(model)
    optimizer, routing = exp17.create_optimizer(model, recipe)
    loss_function = FusedLinearCrossEntropyLoss()
    unique_inputs, unique_targets = block_batch(
        train_windows, np.asarray(indices, dtype=np.int64), device
    )
    gather = torch.as_tensor(
        balanced_repetition(unique_count, physical_batch), device=device
    )
    inputs = unique_inputs.index_select(0, gather)
    targets = unique_targets.index_select(0, gather)
    heldout_inputs = heldout_targets = None
    if stage == "ladder":
        heldout_count = min(HELDOUT_EXAMPLES, len(validation_windows) - 1)
        heldout_inputs, heldout_targets = block_batch(
            validation_windows, np.arange(heldout_count), device
        )
    initial = exact_metrics(model, unique_inputs, unique_targets)
    initial_architecture = architecture_telemetry(
        model, unique_inputs[: min(8, unique_count)]
    )
    interval = LADDER_EVALUATION_INTERVAL if stage == "ladder" else EVALUATION_INTERVAL
    curve: list[dict[str, Any]] = [{"step": 0, "elapsed_seconds": 0.0, **initial}]
    telemetry_curve: list[dict[str, Any]] = [
        {"step": 0, "architecture": initial_architecture}
    ]
    elapsed = 0.0
    clipped = 0
    confirmations = 0
    final_optimization: dict[str, Any] | None = None
    with base.GpuSampler(device) as sampler:
        for step in range(1, maximum_steps + 1):
            model.train()
            optimizer.zero_grad(set_to_none=True)
            body_lr, auxiliary_lr = exp17.set_learning_rates(
                optimizer, recipe, step * unique_count * CONTEXT_LENGTH
            )
            should_measure = step == 1 or step == maximum_steps or step % interval == 0
            before = (
                {key: value.detach().clone() for key, value in model.named_parameters()}
                if should_measure
                else None
            )
            torch.cuda.synchronize(device)
            started = time.perf_counter()
            with torch.autocast("cuda", dtype=torch.bfloat16):
                loss = base.fused_loss(model, inputs, targets, loss_function, hidden)
            loss.backward()
            norm = torch.nn.utils.clip_grad_norm_(model.parameters(), recipe.clip_norm)
            if not torch.isfinite(loss) or not torch.isfinite(norm):
                raise RuntimeError("non-finite forward/backward")
            clipped += int(float(norm) > recipe.clip_norm)
            optimizer.step()
            if not base.optimizer_is_finite(optimizer):
                raise RuntimeError("non-finite optimizer state")
            torch.cuda.synchronize(device)
            elapsed += time.perf_counter() - started
            training_nll = float(loss.detach())
            if not should_measure:
                continue
            assert before is not None
            optimization = parameter_update_telemetry(model, before)
            measured = exact_metrics(model, unique_inputs, unique_targets)
            curve.append(
                {
                    "step": step,
                    "elapsed_seconds": elapsed,
                    "training_nll": training_nll,
                    "grad_norm": float(norm),
                    "body_lr": body_lr,
                    "auxiliary_lr": auxiliary_lr,
                    **measured,
                }
            )
            architecture = architecture_telemetry(
                model, unique_inputs[: min(8, unique_count)]
            )
            telemetry_curve.append(
                {"step": step, "architecture": architecture, "optimization": optimization}
            )
            final_optimization = optimization
            successful = measured["nll"] <= SUCCESS_NLL and measured["token_accuracy"] == 1.0
            confirmations = confirmations + 1 if successful else 0
            if stage != "ladder" and confirmations >= 2:
                break
    final = exact_metrics(model, unique_inputs, unique_targets)
    heldout = (
        exact_metrics(model, heldout_inputs, heldout_targets)
        if heldout_inputs is not None and heldout_targets is not None
        else None
    )
    final_architecture = architecture_telemetry(
        model, unique_inputs[: min(8, unique_count)]
    )
    gate = conditioning_gate(final_architecture, final_optimization)
    accounting = exposure_accounting(
        unique_count=unique_count, physical_batch=physical_batch, steps=step
    )
    success = final["nll"] <= SUCCESS_NLL and final["token_accuracy"] == 1.0
    result = {
        "schema": CELL_SCHEMA,
        "status": "complete",
        "stage": stage,
        "model": name,
        "recipe": asdict(recipe),
        "recipe_slug": recipe_slug(recipe),
        "seed": seed,
        "unique_count": unique_count,
        "sample_indices": list(indices),
        "sample_sha256": _sample_digest(unique_inputs, unique_targets),
        **accounting,
        "maximum_steps": maximum_steps,
        "success": success,
        "initial": initial,
        "final": final,
        "heldout": heldout,
        "threshold_hits": _threshold_hits(curve),
        "curve": curve,
        "telemetry_curve": telemetry_curve,
        "conditioning_gate": gate,
        "inventory": architectures.model_inventory(model),
        "optimizer_routing": routing,
        "performance": {
            "elapsed_seconds": elapsed,
            "physical_tokens_per_second": accounting["physical_tokens_processed"]
            / max(elapsed, 1e-12),
            "unique_tokens_per_second": accounting["unique_token_exposures"]
            / max(elapsed, 1e-12),
            "clip_fraction": clipped / step,
            "peak_allocated_gib": torch.cuda.max_memory_allocated(device) / 2**30,
            "peak_reserved_gib": torch.cuda.max_memory_reserved(device) / 2**30,
            **sampler.summary(),
        },
        "execution_mode": EXECUTION_MODE,
        "loss_implementation": "fla-fused-linear-cross-entropy-exact",
    }
    write_json(result_path, result)
    del model, optimizer, inputs, targets, unique_inputs, unique_targets
    gc.collect()
    torch.cuda.empty_cache()
    return result


def benchmark(task: dict[str, Any], device: torch.device) -> dict[str, Any]:
    """One-GPU physical-batch benchmark used concurrently on all eight GPUs."""

    name = str(task["model"])
    batch = int(task["batch"])
    measured_steps = int(task.get("measured_steps", 8))
    recipe = Recipe(**task.get("recipe", asdict(screen_recipes()[0])))
    windows = load_windows(task["data_root"], "train")
    torch.manual_seed(1910)
    torch.cuda.empty_cache()
    torch.cuda.reset_peak_memory_stats(device)
    model = _build_model(name).to(device)
    hidden = compile_hidden(model)
    optimizer, routing = exp17.create_optimizer(model, recipe)
    loss_function = FusedLinearCrossEntropyLoss()

    def update(step: int) -> tuple[float, float, float]:
        indices = base._logical_indices(len(windows), step, batch, 1910)
        inputs, targets = block_batch(windows, indices, device)
        optimizer.zero_grad(set_to_none=True)
        torch.cuda.synchronize(device)
        started = time.perf_counter()
        with torch.autocast("cuda", dtype=torch.bfloat16):
            loss = base.fused_loss(model, inputs, targets, loss_function, hidden)
        loss.backward()
        norm = torch.nn.utils.clip_grad_norm_(model.parameters(), recipe.clip_norm)
        if not torch.isfinite(loss) or not torch.isfinite(norm):
            raise RuntimeError("non-finite benchmark forward/backward")
        optimizer.step()
        if not base.optimizer_is_finite(optimizer):
            raise RuntimeError("non-finite benchmark optimizer")
        torch.cuda.synchronize(device)
        return time.perf_counter() - started, float(loss), float(norm)

    warmup, _, _ = update(0)
    elapsed = 0.0
    last_nll = last_norm = math.nan
    with base.GpuSampler(device) as sampler:
        for step in range(1, measured_steps + 1):
            duration, last_nll, last_norm = update(step)
            elapsed += duration
    tokens = measured_steps * batch * CONTEXT_LENGTH
    result = {
        "status": "complete",
        "kind": "benchmark",
        "model": name,
        "batch": batch,
        "global_examples_per_step": batch,
        "global_tokens_per_step": batch * CONTEXT_LENGTH,
        "gradient_accumulation": 1,
        "tokens_per_second": tokens / elapsed,
        "step_seconds": elapsed / measured_steps,
        "warmup_seconds": warmup,
        "last_nll": last_nll,
        "last_grad_norm": last_norm,
        "finite_forward_backward_optimizer": True,
        "peak_allocated_gib": torch.cuda.max_memory_allocated(device) / 2**30,
        "peak_reserved_gib": torch.cuda.max_memory_reserved(device) / 2**30,
        "optimizer_routing": routing,
        **sampler.summary(),
    }
    del model, optimizer, loss_function
    gc.collect()
    torch.cuda.empty_cache()
    return result


def batch_sweep(task: dict[str, Any], device: torch.device) -> dict[str, Any]:
    name = str(task["model"])
    curve: list[dict[str, Any]] = []
    stable = 0
    for batch in BATCH_SEARCH:
        try:
            row = benchmark(
                {
                    "model": name,
                    "batch": batch,
                    "measured_steps": 8,
                    "data_root": task["data_root"],
                },
                device,
            )
        except Exception as error:
            row = {
                "status": "failed",
                "kind": "benchmark",
                "model": name,
                "batch": batch,
                "failure": (
                    "out_of_memory"
                    if isinstance(error, torch.OutOfMemoryError)
                    or "out of memory" in str(error).lower()
                    else f"{type(error).__name__}: {str(error)[:1600]}"
                ),
            }
            gc.collect()
            torch.cuda.empty_cache()
        curve.append(row)
        if row.get("status") == "complete":
            stable += 1
            if stable >= 3:
                break
    baseline = benchmark(
        {
            "model": name,
            "batch": UNDERFILLED_BATCH,
            "measured_steps": 8,
            "data_root": task["data_root"],
        },
        device,
    )
    eligible = [
        row
        for row in curve
        if row.get("status") == "complete"
        and row.get("finite_forward_backward_optimizer") is True
        and int(row.get("global_tokens_per_step", 0)) >= MINIMUM_GLOBAL_TOKENS
        and float(row.get("median_gpu_utilization_percent", 0)) >= MINIMUM_UTILIZATION
    ]
    if not eligible:
        raise RuntimeError(f"no stable utilized >=100k-token batch for {name}")
    selected = max(eligible, key=lambda row: float(row["tokens_per_second"]))
    return {
        "status": "complete",
        "kind": "batch-sweep",
        "model": name,
        "curve": curve,
        "underfilled_baseline": baseline,
        "selected": selected,
        "selected_over_underfilled_throughput": float(selected["tokens_per_second"])
        / float(baseline["tokens_per_second"]),
    }


def execute_task(task: dict[str, Any], device: torch.device) -> dict[str, Any]:
    kind = str(task["kind"])
    if kind == "correctness":
        return correctness_cell(task, device)
    if kind == "batch-sweep":
        return batch_sweep(task, device)
    if kind == "benchmark":
        return benchmark(task, device)
    if kind == "fit":
        return fit_cell(task, device)
    raise ValueError(f"unknown Exp19 task kind: {kind}")


def worker_loop(gpu_id: int, tasks: mp.Queue, results: mp.Queue) -> None:
    torch.cuda.set_device(gpu_id)
    device = torch.device(f"cuda:{gpu_id}")
    while True:
        item = tasks.get()
        if item is None:
            return
        index, task = item
        try:
            row = execute_task(task, device)
        except Exception as error:
            row = {
                "status": "failed",
                "kind": task.get("kind"),
                "stage": task.get("stage"),
                "model": task.get("model"),
                "seed": task.get("seed"),
                "failure": (
                    "out_of_memory"
                    if isinstance(error, torch.OutOfMemoryError)
                    or "out of memory" in str(error).lower()
                    else f"{type(error).__name__}: {str(error)[:2400]}"
                ),
            }
            gc.collect()
            torch.cuda.empty_cache()
        results.put((index, row))


def run_tasks(
    tasks: Sequence[dict[str, Any]], *, timeout: int = 21_600
) -> list[dict[str, Any]]:
    if not tasks:
        return []
    context = mp.get_context("spawn")
    task_queue, result_queue = context.Queue(), context.Queue()
    processes = [
        context.Process(target=worker_loop, args=(gpu, task_queue, result_queue))
        for gpu in range(min(GPU_COUNT, len(tasks)))
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
            index, row = result_queue.get(timeout=timeout)
            rows[index] = row
    except queue.Empty as error:
        raise TimeoutError("Exp19 GPU worker timed out") from error
    finally:
        for process in processes:
            process.join(timeout=10)
            if process.is_alive():
                process.terminate()
    if any(row is None for row in rows):
        raise RuntimeError("missing Exp19 task result")
    return [row for row in rows if row is not None]


def all_complete(rows: Iterable[Mapping[str, Any]], label: str) -> None:
    failed = [row for row in rows if row.get("status") != "complete"]
    if failed:
        summary = [
            {
                "model": row.get("model"),
                "kind": row.get("kind"),
                "stage": row.get("stage"),
                "failure": row.get("failure"),
                "failed_gates": [
                    key
                    for key, value in row.items()
                    if isinstance(value, Mapping) and value.get("pass") is False
                ],
            }
            for row in failed
        ]
        raise RuntimeError(f"{label} failed: {summary}")


def load_stage_snapshot(
    path: Path, *, stage: str, expected_rows: int
) -> list[dict[str, Any]] | None:
    if not path.is_file():
        return None
    payload = json.loads(path.read_text())
    rows = payload.get("rows", [])
    valid = (
        payload.get("schema") == STAGE_SNAPSHOT_SCHEMA
        and payload.get("status") == "complete"
        and payload.get("stage") == stage
        and isinstance(rows, list)
        and len(rows) == expected_rows
        and all(row.get("status") == "complete" for row in rows)
    )
    if not valid:
        raise RuntimeError(f"invalid Exp19 stage snapshot: {path}")
    return rows


def write_stage_snapshot(
    path: Path,
    *,
    stage: str,
    rows: Sequence[Mapping[str, Any]],
    provenance: Mapping[str, Any] | None = None,
) -> None:
    write_json(
        path,
        {
            "schema": STAGE_SNAPSHOT_SCHEMA,
            "status": "complete",
            "stage": stage,
            "row_count": len(rows),
            "provenance": dict(provenance or {"kind": "live-cloud-campaign"}),
            "rows": list(rows),
        },
    )


def fill_full_node(tasks: Sequence[dict[str, Any]]) -> list[dict[str, Any]]:
    """Fill short preflight stages without inventing extra model identities."""

    filled = [dict(task) for task in tasks]
    if not filled:
        return filled
    replica = 0
    while len(filled) < GPU_COUNT:
        replica += 1
        filler = dict(filled[replica % len(filled)])
        filler["full_node_filler_replica"] = replica
        filled.append(filler)
    return filled


def paid_preflight(data: Path, cells: Path) -> dict[str, Any]:
    path = cells / "preflight.json"
    if path.is_file():
        prior = json.loads(path.read_text())
        if prior.get("status") == "pass" and prior.get("schema") == PREFLIGHT_SCHEMA:
            return prior
    if torch.cuda.device_count() != GPU_COUNT:
        raise RuntimeError("Exp19 requires exactly eight visible GPUs")
    names = [torch.cuda.get_device_name(index) for index in range(GPU_COUNT)]
    memory = [
        torch.cuda.get_device_properties(index).total_memory / 2**30
        for index in range(GPU_COUNT)
    ]
    if any("H100" not in name for name in names) or any(value < 75 for value in memory):
        raise RuntimeError(f"Exp19 requires 8xH100-80GB; found {names}/{memory}")
    inventories = {
        name: architectures.model_inventory(architectures.build_model(name))
        for name in MODEL_NAMES
    }
    mismatches = {
        name: abs(int(row["total_parameters"]) / TARGET_PARAMETERS - 1.0)
        for name, row in inventories.items()
    }
    tolerances = {
        name: DIAGNOSTIC_PARAMETER_TOLERANCES.get(name, MAXIMUM_PARAMETER_MISMATCH)
        for name in MODEL_NAMES
    }
    if any(mismatches[name] > tolerances[name] for name in MODEL_NAMES):
        raise RuntimeError(f"parameter matching gate failed: {mismatches}")
    progress: dict[str, Any] = {
        "schema": PREFLIGHT_SCHEMA,
        "status": "running",
        "gpu_names": names,
        "gpu_memory_gib": memory,
        "inventories": inventories,
        "parameter_mismatch_fractions": mismatches,
        "parameter_mismatch_tolerances": tolerances,
        "diagnostic_parameter_delta_policy": (
            "learned-RMS cumulative ablation retains its 8,320 added gains; "
            "all primary and control tracks remain within 0.1%"
        ),
        "gradient_accumulation": 1,
        "execution_mode": EXECUTION_MODE,
    }
    write_json(path, progress)
    correctness = run_tasks(
        [
            {"kind": "correctness", "model": name, "data_root": str(data)}
            for name in MODEL_NAMES
        ]
    )
    progress["correctness"] = dict(zip(MODEL_NAMES, correctness, strict=True))
    write_json(path, progress)
    required_correctness = [
        row
        for name, row in zip(MODEL_NAMES, correctness, strict=True)
        if name in TRAINING_MODEL_NAMES
    ]
    progress["nonpromotable_diagnostic_models"] = list(
        NONPROMOTABLE_DIAGNOSTIC_MODELS
    )
    progress["training_models"] = list(TRAINING_MODEL_NAMES)
    progress["diagnostic_policy"] = (
        "redundant-scale ablations retain initialization telemetry and parity "
        "results but are not trained after measured compiled/checkpoint delta "
        "instability; primary and matched controls remain hard-gated"
    )
    write_json(path, progress)
    all_complete(required_correctness, "training-model correctness/parity gate")
    sweep_tasks = [
            {"kind": "batch-sweep", "model": name, "data_root": str(data)}
            for name in TRAINING_MODEL_NAMES
        ]
    sweep_rows = run_tasks(fill_full_node(sweep_tasks))
    sweeps = sweep_rows[: len(sweep_tasks)]
    progress["batch_sweeps"] = {
        str(row.get("model")): row for row in sweeps
    }
    progress["batch_sweep_filler_workers"] = sweep_rows[len(sweep_tasks) :]
    write_json(path, progress)
    all_complete(sweeps, "ambitious batch sweep")
    selected = {str(row["model"]): row["selected"] for row in sweeps}
    benchmark_tasks = [
            {
                "kind": "benchmark",
                "model": name,
                "batch": int(selected[name]["batch"]),
                "measured_steps": 10,
                "data_root": str(data),
            }
            for name in TRAINING_MODEL_NAMES
        ]
    full_node = run_tasks(fill_full_node(benchmark_tasks))
    all_complete(full_node, "full-node benchmark")
    if any(
        int(row.get("gpu_samples", 0)) <= 0
        or float(row.get("median_gpu_utilization_percent", 0)) < MINIMUM_UTILIZATION
        for row in full_node
    ):
        raise RuntimeError(f"full-node utilization gate failed: {full_node}")
    progress.update(
        {
            "status": "pass",
            "batch_sweeps": {str(row["model"]): row for row in sweeps},
            "selected": selected,
            "full_node_workers": full_node,
            "full_node_aggregate_tokens_per_second": sum(
                float(row["tokens_per_second"]) for row in full_node
            ),
            "minimum_global_tokens_per_step": MINIMUM_GLOBAL_TOKENS,
            "minimum_utilization_percent": MINIMUM_UTILIZATION,
        }
    )
    write_json(path, progress)
    return progress


def fit_task(
    *,
    stage: str,
    model: str,
    recipe: Recipe,
    seed: int,
    indices: Sequence[int],
    maximum_steps: int,
    batch: int,
    cells: Path,
    data: Path,
) -> dict[str, Any]:
    return {
        "kind": "fit",
        "stage": stage,
        "model": model,
        "recipe": asdict(recipe),
        "seed": seed,
        "sample_indices": list(indices),
        "maximum_steps": maximum_steps,
        "batch": batch,
        "output_root": str(cells),
        "data_root": str(data),
    }


def publish_preflight_to_wandb(run: Any, path: Path) -> None:
    """Put the complete preflight and layer telemetry on the direct W&B run."""

    import wandb

    report = json.loads(path.read_text())
    columns = [
        "model",
        "training_eligible",
        "correctness_status",
        "layer",
        "state_rms",
        "state_variance",
        "state_participation_ratio",
        "mixer_raw_branch_rms",
        "mixer_scale",
        "mixer_update_to_state_rms",
        "mixer_residual_cosine",
        "ffn_raw_branch_rms",
        "ffn_scale",
        "ffn_update_to_state_rms",
        "ffn_residual_cosine",
        "group_hidden_variance",
        "group_hidden_participation_ratio",
        "group_hidden_zero_variance_fraction",
        "group_hidden_silu_saturation_fraction",
    ]
    table = wandb.Table(columns=columns)
    correctness = report.get("correctness", {})
    summary_metrics: dict[str, Any] = {
        "preflight/status": report.get("status", "running"),
        "preflight/training_model_count": len(TRAINING_MODEL_NAMES),
        "preflight/audit_model_count": len(MODEL_NAMES),
    }
    for name, row in correctness.items():
        gate = row.get("initial_branch_gate", {})
        compiled = row.get("compiled_bf16_gradient_parity", {})
        checkpoint_step = row.get("checkpoint_optimizer_step_parity", {})
        prefix = f"preflight/{name}"
        summary_metrics.update(
            {
                f"{prefix}/training_eligible": name in TRAINING_MODEL_NAMES,
                f"{prefix}/correctness_complete": row.get("status") == "complete",
                f"{prefix}/branch_maximum": gate.get("maximum", math.nan),
                f"{prefix}/branch_aggregate_rss": gate.get(
                    "aggregate_rss", math.nan
                ),
                f"{prefix}/branch_fraction_0.02_0.20": gate.get(
                    "fraction_in_target_interval", math.nan
                ),
                f"{prefix}/compiled_gradient_relative_error": compiled.get(
                    "global_relative_error", math.nan
                ),
                f"{prefix}/compiled_gradient_cosine": compiled.get(
                    "global_cosine", math.nan
                ),
                f"{prefix}/checkpoint_step_relative_error": checkpoint_step.get(
                    "global_relative_error", math.nan
                ),
            }
        )
        layers = row.get("initial_telemetry", {}).get("layers", [])
        for layer in layers:
            state = layer.get("state", {})
            mixer = layer.get("mixer", {})
            ffn = layer.get("ffn", {})
            group = layer.get("group_hidden") or {}
            table.add_data(
                name,
                name in TRAINING_MODEL_NAMES,
                row.get("status"),
                layer.get("layer"),
                state.get("rms"),
                state.get("variance"),
                state.get("participation_ratio"),
                layer.get("mixer_raw_branch_rms"),
                layer.get("mixer_scale"),
                mixer.get("update_to_state_rms"),
                mixer.get("residual_input_output_cosine"),
                layer.get("ffn_raw_branch_rms"),
                layer.get("ffn_scale"),
                ffn.get("update_to_state_rms"),
                ffn.get("residual_input_output_cosine"),
                group.get("variance_mean"),
                group.get("participation_ratio"),
                group.get("zero_variance_fraction"),
                group.get("silu_saturation_fraction"),
            )
    run.log({"preflight/initial_layer_telemetry": table})
    run.summary.update(summary_metrics)
    artifact = wandb.Artifact(
        name=f"exp19-preflight-{run.id}",
        type="preflight",
        metadata={
            "schema": report.get("schema"),
            "status": report.get("status"),
            "training_models": list(TRAINING_MODEL_NAMES),
            "diagnostic_models": list(NONPROMOTABLE_DIAGNOSTIC_MODELS),
        },
    )
    artifact.add_file(str(path), name="preflight.json")
    run.log_artifact(artifact)


def publish_stage_snapshot_to_wandb(run: Any, path: Path, stage: str) -> None:
    import wandb

    artifact = wandb.Artifact(
        name=f"exp19-{stage}-{run.id}",
        type="stage-snapshot",
        metadata={"schema": STAGE_SNAPSHOT_SCHEMA, "stage": stage},
    )
    artifact.add_file(str(path), name=path.name)
    run.log_artifact(artifact)


def run_campaign(
    output: str | Path,
    *,
    data_root: str | Path,
    heartbeat: str | Path | None = None,
) -> dict[str, Any]:
    """Run the evidence-gated cloud campaign and stop at the corpus boundary."""

    if not os.environ.get("WANDB_API_KEY"):
        raise RuntimeError("WANDB_API_KEY is required before paid training")
    import wandb

    data = Path(data_root)
    output_path = Path(output)
    cells = output_path.parent / "norm-cells"
    stage_snapshots = cells / "stage-snapshots"
    load_windows(data, "train")
    load_windows(data, "validation")
    wandb.login(key=os.environ["WANDB_API_KEY"], verify=True)
    run = wandb.init(
        project="exp19-norm-residual",
        name="exp19-matched-5m-conditioning-repair",
        config={
            "schema": SCHEMA,
            "audit_models": MODEL_NAMES,
            "training_models": TRAINING_MODEL_NAMES,
            "nonpromotable_diagnostic_models": NONPROMOTABLE_DIAGNOSTIC_MODELS,
            "cloud_only": True,
            "hardware": "8xH100-80GB",
            "minimum_global_tokens_per_step": MINIMUM_GLOBAL_TOKENS,
            "gradient_accumulation": 1,
            "screen_steps": SCREEN_STEPS,
            "confirmation_steps": CONFIRMATION_STEPS,
            "ladder_steps": LADDER_STEPS,
            "ladder_unique_examples": LADDER_SIZES,
            "parameter_match_target": TARGET_PARAMETERS,
        },
    )
    if not run.url:
        raise RuntimeError("W&B did not provide a direct run URL")
    print(f"WANDB_URL={run.url}", flush=True)
    write_json(
        output_path,
        {"schema": SCHEMA, "status": "running", "wandb_url": run.url},
    )

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

    def publish(stage: str, rows: Sequence[Mapping[str, Any]]) -> None:
        nonlocal log_step
        for row in rows:
            telemetry_by_step = {
                int(point["step"]): point for point in row.get("telemetry_curve", [])
            }
            for point in row.get("curve", []):
                log_step += 1
                telemetry_point = telemetry_by_step.get(int(point.get("step", -1)), {})
                conditioning = telemetry_point.get("architecture", {}).get(
                    "conditioning", {}
                )
                optimization = telemetry_point.get("optimization", {})
                run.log(
                    {
                        f"{stage}/model": row.get("model", ""),
                        f"{stage}/optimizer": row.get("recipe", {}).get("family", ""),
                        f"{stage}/body_lr": row.get("recipe", {}).get("body_lr", 0),
                        f"{stage}/auxiliary_lr": row.get("recipe", {}).get(
                            "auxiliary_lr", 0
                        ),
                        f"{stage}/seed": row.get("seed", 0),
                        f"{stage}/unique_examples": row.get("unique_count", 0),
                        f"{stage}/physical_batch": row.get("physical_batch", 0),
                        f"{stage}/global_tokens_per_step": row.get(
                            "global_tokens_per_step", 0
                        ),
                        f"{stage}/unique_tokens_per_step": row.get(
                            "unique_tokens_per_step", 0
                        ),
                        f"{stage}/optimizer_step": point.get("step", 0),
                        f"{stage}/nll": point.get("nll", math.nan),
                        f"{stage}/token_accuracy": point.get("token_accuracy", math.nan),
                        f"{stage}/elapsed_seconds": point.get("elapsed_seconds", 0),
                        f"{stage}/max_branch_state_ratio": conditioning.get(
                            "maximum_scaled_update_to_state_ratio", math.nan
                        ),
                        f"{stage}/hidden_variance_first_over_last": conditioning.get(
                            "first_to_last_hidden_variance_ratio", math.nan
                        ),
                        f"{stage}/gradient_layer_imbalance": optimization.get(
                            "layer_gradient_imbalance", math.nan
                        ),
                        f"{stage}/minimum_effective_factor_update": optimization.get(
                            "minimum_effective_normalized_update_fraction", math.nan
                        ),
                    },
                    step=log_step,
                )
            performance = row.get("performance", {})
            if "physical_tokens_per_second" not in performance:
                continue
            log_step += 1
            run.log(
                {
                    f"{stage}/model": row.get("model", ""),
                    f"{stage}/physical_tokens_per_second": performance.get(
                        "physical_tokens_per_second", 0
                    ),
                    f"{stage}/median_gpu_utilization_percent": performance.get(
                        "median_gpu_utilization_percent", 0
                    ),
                    f"{stage}/peak_allocated_gib": performance.get(
                        "peak_allocated_gib", 0
                    ),
                    f"{stage}/peak_reserved_gib": performance.get(
                        "peak_reserved_gib", 0
                    ),
                },
                step=log_step,
            )
        if heartbeat_path:
            heartbeat_path.touch()

    preflight_published = False
    try:
        preflight = paid_preflight(data, cells)
        publish_preflight_to_wandb(run, cells / "preflight.json")
        preflight_published = True
        batches = {
            name: int(row["batch"]) for name, row in preflight["selected"].items()
        }
        screen_path = stage_snapshots / "one-example-screen.json"
        screen = load_stage_snapshot(
            screen_path,
            stage="one-example-screen",
            expected_rows=len(TRAINING_MODEL_NAMES) * len(screen_recipes()),
        )
        if screen is None:
            screen = run_tasks(
                [
                    fit_task(
                        stage="one-example-screen",
                        model=name,
                        recipe=recipe,
                        seed=SCREEN_SEED,
                        indices=(SAMPLE_INDICES[0],),
                        maximum_steps=SCREEN_STEPS,
                        batch=batches[name],
                        cells=cells,
                        data=data,
                    )
                    for name in TRAINING_MODEL_NAMES
                    for recipe in screen_recipes()
                ]
            )
            write_stage_snapshot(
                screen_path, stage="one-example-screen", rows=screen
            )
        publish_stage_snapshot_to_wandb(
            run, screen_path, "one-example-screen"
        )
        publish("one_example", screen)
        promoted = promote_screen(screen)

        confirmation_path = stage_snapshots / "two-example-confirmation.json"
        confirmation = load_stage_snapshot(
            confirmation_path,
            stage="two-example-confirmation",
            expected_rows=(
                len(TRAINING_MODEL_NAMES) * 2 * len(CONFIRMATION_SEEDS)
            ),
        )
        if confirmation is None:
            confirmation = run_tasks(
                [
                    fit_task(
                        stage="two-example-confirmation",
                        model=name,
                        recipe=promoted[name][family],
                        seed=seed,
                        indices=SAMPLE_INDICES,
                        maximum_steps=CONFIRMATION_STEPS,
                        batch=batches[name],
                        cells=cells,
                        data=data,
                    )
                    for name in TRAINING_MODEL_NAMES
                    for family in ("adamw", "muon")
                    for seed in CONFIRMATION_SEEDS
                ]
            )
            all_complete(confirmation, "two-example optimizer confirmation")
            write_stage_snapshot(
                confirmation_path,
                stage="two-example-confirmation",
                rows=confirmation,
            )
        publish_stage_snapshot_to_wandb(
            run, confirmation_path, "two-example-confirmation"
        )
        all_complete(confirmation, "two-example optimizer confirmation")
        publish("two_example", confirmation)
        confirmation_summary, winners = summarize_confirmation(confirmation)

        ladder_path = stage_snapshots / "generalization-ladder.json"
        ladder = load_stage_snapshot(
            ladder_path,
            stage="ladder",
            expected_rows=(
                len(LADDER_SIZES)
                * len(TRAINING_MODEL_NAMES)
                * len(LADDER_SEEDS)
            ),
        )
        if ladder is None:
            ladder = run_tasks(
                [
                    fit_task(
                        stage="ladder",
                        model=name,
                        recipe=winners[name],
                        seed=seed,
                        indices=tuple(range(unique_count)),
                        maximum_steps=LADDER_STEPS,
                        batch=batches[name],
                        cells=cells,
                        data=data,
                    )
                    for unique_count in LADDER_SIZES
                    for name in TRAINING_MODEL_NAMES
                    for seed in LADDER_SEEDS
                ],
                timeout=43_200,
            )
            all_complete(ladder, "8/32/128-example generalization ladder")
            write_stage_snapshot(ladder_path, stage="ladder", rows=ladder)
        publish_stage_snapshot_to_wandb(
            run, ladder_path, "generalization-ladder"
        )
        all_complete(ladder, "8/32/128-example generalization ladder")
        publish("generalization_ladder", ladder)
        decision = ladder_decision(ladder)
        result = {
            "schema": SCHEMA,
            "status": "complete",
            "verdict": (
                "corpus_gate_passed" if decision["corpus_ready"] else "corpus_gate_failed"
            ),
            "wandb_url": run.url,
            "cloud_only_training": True,
            "gpu_count": GPU_COUNT,
            "preflight": preflight,
            "screen": screen,
            "screen_promotions": {
                name: {family: asdict(recipe) for family, recipe in values.items()}
                for name, values in promoted.items()
            },
            "confirmation": confirmation,
            "confirmation_summary": confirmation_summary,
            "ladder": ladder,
            "ladder_decision": decision,
        }
        write_json(output_path, result)
        result_artifact = wandb.Artifact(
            name=f"exp19-result-{run.id}",
            type="result",
            metadata={"schema": SCHEMA, "verdict": result["verdict"]},
        )
        result_artifact.add_file(str(output_path), name="result.json")
        run.log_artifact(result_artifact)
        run.summary.update(
            {
                "verdict": result["verdict"],
                "corpus_ready": decision["corpus_ready"],
                "passing_corrected_group_models": ",".join(
                    decision["passing_corrected_group_models"]
                ),
            }
        )
        run.finish()
        stop.set()
        if heartbeat_thread:
            heartbeat_thread.join(timeout=2)
        return result
    except Exception as campaign_error:
        stop.set()
        if heartbeat_thread:
            heartbeat_thread.join(timeout=2)
        write_json(
            output_path,
            {
                "schema": SCHEMA,
                "status": "failed",
                "wandb_url": run.url,
                "failure": (
                    f"{type(campaign_error).__name__}: {str(campaign_error)[:4000]}"
                ),
            },
        )
        preflight_path = cells / "preflight.json"
        try:
            if not preflight_published and preflight_path.is_file():
                publish_preflight_to_wandb(run, preflight_path)
        except Exception as telemetry_error:
            print(
                "WARNING: failed to publish preflight telemetry to W&B: "
                f"{type(telemetry_error).__name__}: {telemetry_error}",
                flush=True,
            )
        finally:
            run.finish(exit_code=1)
        raise


def main() -> None:
    parser = argparse.ArgumentParser(description="Run Exp19 conditioning repair")
    parser.add_argument("--output", required=True)
    parser.add_argument("--data-root", required=True)
    parser.add_argument("--heartbeat")
    args = parser.parse_args()
    result = run_campaign(
        args.output,
        data_root=args.data_root,
        heartbeat=args.heartbeat,
    )
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)


if __name__ == "__main__":
    main()
