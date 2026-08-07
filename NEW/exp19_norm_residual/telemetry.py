"""Conditioning telemetry for Exp19 models.

All returned objects are JSON-serializable.  Statistics are accumulated in
FP32; callers may run the model itself under BF16/autocast.
"""

from __future__ import annotations

import math
from typing import Any, Iterable

import torch
import torch.nn.functional as F
from torch import nn

from .model import (
    CorrectedKronLayer,
    CorrectedTransformerLayer,
    LanguageModel,
    LearnedRMSNorm,
    PackedLowerFactor,
    RowNormalizedFactor,
)


def rms(value: torch.Tensor) -> torch.Tensor:
    return value.detach().float().square().mean().sqrt()


def participation_ratio(value: torch.Tensor) -> dict[str, float]:
    """Covariance effective rank over the final representation dimension."""

    flattened = value.detach().float().reshape(-1, value.shape[-1])
    # Bound diagnostic memory and make results deterministic.
    flattened = flattened[: min(4096, flattened.shape[0])]
    centered = flattened - flattened.mean(0, keepdim=True)
    covariance = centered.mT @ centered / max(1, centered.shape[0] - 1)
    eigenvalues = torch.linalg.eigvalsh(covariance).clamp_min(0)
    effective = eigenvalues.sum().square() / eigenvalues.square().sum().clamp_min(1e-12)
    return {
        "participation_ratio": float(effective),
        "participation_fraction": float(effective / value.shape[-1]),
    }


def representation_summary(value: torch.Tensor) -> dict[str, float]:
    detached = value.detach().float()
    variance = detached.var(unbiased=False)
    result = {
        "rms": float(detached.square().mean().sqrt()),
        "mean": float(detached.mean()),
        "variance": float(variance),
        "finite_fraction": float(torch.isfinite(detached).float().mean()),
    }
    result.update(participation_ratio(detached))
    return result


def update_summary(update: torch.Tensor, state: torch.Tensor) -> dict[str, float]:
    update_rms = rms(update)
    state_rms = rms(state).clamp_min(1e-12)
    state_flat = state.detach().float().flatten()
    output_flat = (state + update).detach().float().flatten()
    return {
        "update_rms": float(update_rms),
        "state_rms": float(state_rms),
        "update_to_state_rms": float(update_rms / state_rms),
        "residual_input_output_cosine": float(
            F.cosine_similarity(state_flat, output_flat, dim=0)
        ),
    }


def hidden_summary(hidden: torch.Tensor) -> dict[str, float | int]:
    detached = hidden.detach().float()
    flattened = detached.reshape(-1, detached.shape[-1])
    flattened = flattened[: min(4096, flattened.shape[0])]
    variance = flattened.var(0, unbiased=False)
    result: dict[str, float | int] = {
        "sites": int(detached.numel()),
        "variance_mean": float(variance.mean()),
        "nonzero_variance_fraction": float((variance > 1e-7).float().mean()),
        "zero_variance_fraction": float((variance <= 1e-7).float().mean()),
        "silu_saturation_fraction": float((detached.abs() >= 6.0).float().mean()),
    }
    result.update(participation_ratio(flattened))
    return result


def norm_gamma_telemetry(model: nn.Module) -> list[dict[str, Any]]:
    result: list[dict[str, Any]] = []
    for name, module in model.named_modules():
        if not isinstance(module, LearnedRMSNorm):
            continue
        row: dict[str, Any] = {"name": name, "kind": module.kind}
        if module.weight is None:
            row["learned"] = False
        else:
            gamma = module.weight.detach().float()
            row.update(
                {
                    "learned": True,
                    "minimum": float(gamma.min()),
                    "maximum": float(gamma.max()),
                    "mean": float(gamma.mean()),
                    "std": float(gamma.std(unbiased=False)),
                    "rms": float(gamma.square().mean().sqrt()),
                }
            )
        result.append(row)
    return result


def scale_parameter_telemetry(model: nn.Module) -> list[dict[str, Any]]:
    """Record every explicit rank/residual/group scale without hiding gauges."""

    markers = (
        "rank_amplitudes",
        "mixer_gain",
        "ffn_gain",
        "path_amplitudes",
        "input_channel_scale",
        "output_channel_scale",
    )
    result: list[dict[str, Any]] = []
    for name, parameter in model.named_parameters():
        if not any(marker in name for marker in markers):
            continue
        value = parameter.detach().float()
        result.append(
            {
                "name": name,
                "elements": value.numel(),
                "minimum": float(value.min()),
                "maximum": float(value.max()),
                "mean": float(value.mean()),
                "rms": float(value.square().mean().sqrt()),
            }
        )
    return result


def spectrum_summary(matrix: torch.Tensor) -> dict[str, Any]:
    values = torch.linalg.svdvals(matrix.detach().float())
    maximum = values.max()
    minimum = values.min()
    squared = values.square()
    row_energy = matrix.detach().float().square().sum(-1)
    column_energy = matrix.detach().float().square().sum(-2)
    return {
        "rows": int(matrix.shape[-2]),
        "columns": int(matrix.shape[-1]),
        "singular_values": [float(item) for item in values],
        "minimum": float(minimum),
        "maximum": float(maximum),
        "condition": float(maximum / minimum.clamp_min(1e-12)),
        "stable_rank": float(squared.sum() / squared.max().clamp_min(1e-12)),
        "row_energy_minimum": float(row_energy.min()),
        "row_energy_maximum": float(row_energy.max()),
        "column_energy_minimum": float(column_energy.min()),
        "column_energy_maximum": float(column_energy.max()),
    }


def _matrix_bank(name: str, value: torch.Tensor) -> Iterable[tuple[str, torch.Tensor]]:
    if value.ndim == 2:
        yield name, value
    elif value.ndim == 3:
        for index, matrix in enumerate(value):
            yield f"{name}[{index}]", matrix


def small_factor_spectra(
    model: nn.Module, *, layers: Iterable[int] | None = None
) -> list[dict[str, Any]]:
    """Report spectra for the normalized small factors, not large dense weights."""

    if not isinstance(model, LanguageModel):
        return []
    selected = set(layers) if layers is not None else set(range(len(model.blocks)))
    rows: list[dict[str, Any]] = []
    factor_names = {
        "gate_workspace",
        "gate_channel",
        "up_workspace",
        "up_channel",
        "down_workspace",
        "down_channel",
    }
    for layer_index in sorted(selected):
        block = model.blocks[layer_index]
        if not isinstance(block, CorrectedKronLayer):
            continue
        for name, module in block.mixer.named_modules():
            if isinstance(module, (RowNormalizedFactor, PackedLowerFactor)):
                for bank_name, matrix in _matrix_bank(name, module.value()):
                    rows.append(
                        {
                            "layer": layer_index,
                            "factor": f"mixer.{bank_name}",
                            **spectrum_summary(matrix),
                        }
                    )
        if block.group_ffn is not None:
            for name, parameter in block.group_ffn.named_parameters():
                if name not in factor_names:
                    continue
                normalized = parameter / parameter.detach().float().square().sum(
                    -1, keepdim=True
                ).sqrt().clamp_min(1e-8).to(parameter.dtype)
                for bank_name, matrix in _matrix_bank(name, normalized):
                    rows.append(
                        {
                            "layer": layer_index,
                            "factor": f"group_ffn.{bank_name}",
                            **spectrum_summary(matrix),
                        }
                    )
    return rows


def _selected_layers(count: int, layers: Iterable[int] | None) -> set[int]:
    if layers is not None:
        result = {int(item) for item in layers}
    else:
        result = {0, count // 2, count - 1}
    if any(item < 0 or item >= count for item in result):
        raise ValueError("telemetry layer index out of range")
    return result


@torch.inference_mode()
def telemetry(
    model: nn.Module,
    inputs: torch.Tensor,
    *,
    layers: Iterable[int] | None = None,
    include_spectra: bool = True,
) -> dict[str, Any]:
    """Collect state/update, hidden, norm, and factor telemetry.

    The manual traversal is algebraically identical to ``model.hidden`` for
    Exp19 models.  Frozen Exp17 models receive a reduced legacy-compatible
    traversal and are explicitly labeled as such.
    """

    selected = _selected_layers(len(model.blocks), layers)
    spec = model.spec
    value = F.embedding(inputs, model.vocabulary)
    if isinstance(model, LanguageModel) and spec.scale_embedding_residual:
        value = value * math.sqrt(spec.width)
    initial = representation_summary(value)
    layer_rows: list[dict[str, Any]] = []

    if isinstance(model, LanguageModel):
        for index, block in enumerate(model.blocks):
            before = value
            row: dict[str, Any] | None = None
            if isinstance(block, CorrectedKronLayer):
                mixer_input = block.mixer_norm(before)
                raw_mixer = block.mixer(mixer_input)
                mixer_update = block.mixer_scale * raw_mixer
                mixed = before + mixer_update
                ffn_input = block.ffn_norm(mixed)
                raw_ffn = block.nonlinear_branch(ffn_input)
                ffn_update = block.ffn_scale * raw_ffn
                value = mixed + ffn_update
                if index in selected:
                    row = {
                        "layer": index,
                        "state": representation_summary(before),
                        "mixer": update_summary(mixer_update, before),
                        "mixer_raw_branch_rms": float(rms(raw_mixer)),
                        "mixer_scale": float(block.mixer_scale),
                        "after_mixer": representation_summary(mixed),
                        "ffn": update_summary(ffn_update, mixed),
                        "ffn_raw_branch_rms": float(rms(raw_ffn)),
                        "ffn_scale": float(block.ffn_scale),
                        "output": representation_summary(value),
                    }
                    if block.group_ffn is not None:
                        groups = ffn_input.reshape(
                            ffn_input.shape[0],
                            spec.group_count,
                            spec.group_size,
                            spec.width,
                        )
                        row["group_hidden"] = hidden_summary(
                            block.group_ffn.hidden(groups)
                        )
            elif isinstance(block, CorrectedTransformerLayer):
                raw_attention = block.attention(block.attention_norm(before))
                attention_update = spec.branch_scale * raw_attention
                attended = before + attention_update
                raw_ffn = block.ffn(block.ffn_norm(attended))
                ffn_update = spec.branch_scale * raw_ffn
                value = attended + ffn_update
                if index in selected:
                    row = {
                        "layer": index,
                        "state": representation_summary(before),
                        "mixer": update_summary(attention_update, before),
                        "mixer_raw_branch_rms": float(rms(raw_attention)),
                        "mixer_scale": float(spec.branch_scale),
                        "after_mixer": representation_summary(attended),
                        "ffn": update_summary(ffn_update, attended),
                        "ffn_raw_branch_rms": float(rms(raw_ffn)),
                        "ffn_scale": float(spec.branch_scale),
                        "output": representation_summary(value),
                    }
            else:  # pragma: no cover - construction prevents this
                value = block(value)
            if row is not None:
                layer_rows.append(row)
        final_value = model.final_norm(value)
        norm_rows = norm_gamma_telemetry(model)
        spectra = small_factor_spectra(model, layers=selected) if include_spectra else []
        identity = spec.name
        implementation = "exp19"
    else:
        # Frozen Exp17 post-norm reference.  Its branch/update interpretation
        # is recorded without changing or wrapping the original modules.
        from exp14_block_kronecker.model import rms_norm

        for index, block in enumerate(model.blocks):
            before = value
            mixer_update = block.mixer_gain * block.mixer(before)
            mixed = rms_norm(before + mixer_update)
            ffn_update = block.ffn_gain * block.nonlinear_branch(mixed)
            value = rms_norm(mixed + ffn_update)
            if index in selected:
                layer_rows.append(
                    {
                        "layer": index,
                        "state": representation_summary(before),
                        "mixer": update_summary(mixer_update, before),
                        "after_mixer": representation_summary(mixed),
                        "ffn": update_summary(ffn_update, mixed),
                        "output": representation_summary(value),
                    }
                )
        final_value = rms_norm(value)
        norm_rows = []
        spectra = []
        identity = "legacy-exp17-postnorm-r1"
        implementation = "external-frozen-exp17"

    ratios = [
        float(row[branch]["update_to_state_rms"])
        for row in layer_rows
        for branch in ("mixer", "ffn")
    ]
    hidden_rows = [row["group_hidden"] for row in layer_rows if "group_hidden" in row]
    if len(hidden_rows) >= 2:
        first_variance = float(hidden_rows[0]["variance_mean"])
        last_variance = float(hidden_rows[-1]["variance_mean"])
        variance_basis = "group_ffn_hidden"
    elif layer_rows:
        first_variance = float(layer_rows[0]["state"]["variance"])
        last_variance = float(layer_rows[-1]["output"]["variance"])
        variance_basis = "residual_stream"
    else:  # pragma: no cover - selected-layer validation makes this unreachable
        first_variance = last_variance = float("nan")
        variance_basis = "unavailable"
    # First/last makes collapse toward the final layer directly comparable to
    # the preregistered 100-fold imbalance ceiling.
    variance_ratio = first_variance / max(last_variance, 1e-12)

    return {
        "architecture_identity": identity,
        "implementation": implementation,
        "embedding_residual_scaled": bool(
            isinstance(model, LanguageModel) and spec.scale_embedding_residual
        ),
        "initial_state": initial,
        "layers": layer_rows,
        "final_state_before_norm": representation_summary(value),
        "final_state": representation_summary(final_value),
        "norm_gammas": norm_rows,
        "scale_parameters": scale_parameter_telemetry(model),
        "small_factor_spectra": spectra,
        "conditioning": {
            "scaled_update_to_state_ratios": ratios,
            "minimum_scaled_update_to_state_ratio": min(ratios, default=float("nan")),
            "maximum_scaled_update_to_state_ratio": max(ratios, default=float("nan")),
            # Defined as first/last; collapse toward the final layer is >1.
            "first_to_last_hidden_variance_ratio": variance_ratio,
            "hidden_variance_ratio_definition": "first_variance / last_variance",
            "hidden_variance_basis": variance_basis,
        },
    }
