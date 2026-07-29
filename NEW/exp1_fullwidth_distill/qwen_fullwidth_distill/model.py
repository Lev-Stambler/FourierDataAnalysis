from __future__ import annotations

import math
from dataclasses import replace
from collections.abc import Mapping

import torch
import torch.nn.functional as F
from torch import nn

from .btt import BTTLinear, context_preserving_modes
from .config import ArchitectureConfig
from .kronecker import KroneckerLinear, context_channel_modes
from .monarch import MonarchLinear


def _linear(config: ArchitectureConfig, input_width: int, output_width: int) -> nn.Module:
    if config.operator == "dense":
        return nn.Linear(input_width, output_width, bias=True)
    if config.operator == "monarch":
        return MonarchLinear(
            input_width,
            output_width,
            nblocks=config.monarch_blocks,
            rank=config.monarch_rank,
            bias=True,
        )
    if config.operator == "btt":
        return BTTLinear(
            input_width,
            output_width,
            input_modes=context_preserving_modes(
                config.full_width, input_width, config.btt_cores
            ),
            output_modes=context_preserving_modes(
                config.full_width, output_width, config.btt_cores
            ),
            rank=config.btt_rank,
            bias=True,
            weight_norm=config.btt_weight_norm,
        )
    return KroneckerLinear(
        input_width,
        output_width,
        input_modes=context_channel_modes(
            config.context_length,
            config.embedding_width,
            input_width,
            order=config.kronecker_factors,
        ),
        output_modes=context_channel_modes(
            config.context_length,
            config.embedding_width,
            output_width,
            order=config.kronecker_factors,
        ),
        bias=True,
        weight_norm=config.kronecker_weight_norm,
        rank=config.kronecker_rank,
        rank_chunk=config.kronecker_rank_chunk,
    )


class OneMapResidual(nn.Module):
    def __init__(self, config: ArchitectureConfig) -> None:
        super().__init__()
        width = config.full_width
        self.norm = nn.RMSNorm(width, eps=1e-6)
        self.linear = _linear(config, width, width)

    def forward(
        self, x: torch.Tensor, residual_multiplier: float = 1.0
    ) -> torch.Tensor:
        return x + residual_multiplier * self.linear(F.silu(self.norm(x)))


class TwoMapResidual(nn.Module):
    def __init__(self, config: ArchitectureConfig) -> None:
        super().__init__()
        width = config.full_width
        hidden = width * config.expansion
        self.norm = nn.RMSNorm(width, eps=1e-6)
        self.up = _linear(config, width, hidden)
        self.down = _linear(config, hidden, width)

    def forward(
        self, x: torch.Tensor, residual_multiplier: float = 1.0
    ) -> torch.Tensor:
        return x + residual_multiplier * self.down(
            F.silu(self.up(self.norm(x)))
        )


class GatedResidual(nn.Module):
    def __init__(self, config: ArchitectureConfig) -> None:
        super().__init__()
        width = config.full_width
        self.norm = nn.RMSNorm(width, eps=1e-6)
        self.up = _linear(config, width, width)
        self.gate = _linear(config, width, width)
        self.down = _linear(config, width, width)

    def forward(
        self, x: torch.Tensor, residual_multiplier: float = 1.0
    ) -> torch.Tensor:
        value = self.norm(x)
        return x + residual_multiplier * self.down(
            F.silu(self.gate(value)) * self.up(value)
        )


class FullWidthStack(nn.Module):
    def __init__(self, config: ArchitectureConfig) -> None:
        super().__init__()
        config.validate()
        width = config.full_width
        def layer_config(index: int) -> ArchitectureConfig:
            if config.operator != "hybrid":
                return config
            return replace(
                config,
                operator=(
                    "monarch"
                    if index in (0, config.depth - 1)
                    else "kronecker"
                ),
            )

        if config.form == "sequential":
            self.layers = nn.ModuleList([
                _linear(layer_config(index), width, width)
                for index in range(config.depth)
            ])
        elif config.form == "residual_one":
            self.layers = nn.ModuleList([
                OneMapResidual(layer_config(index))
                for index in range(config.depth)
            ])
        elif config.form == "residual_ffn":
            self.layers = nn.ModuleList([
                TwoMapResidual(layer_config(index))
                for index in range(config.depth)
            ])
        else:
            self.layers = nn.ModuleList([
                GatedResidual(layer_config(index))
                for index in range(config.depth)
            ])
        self.form = config.form
        self.repetitions = config.repetitions
        self.residual_multiplier = config.residual_multiplier
        self.repetition_gates = (
            nn.Parameter(torch.zeros(config.repetitions, config.depth))
            if config.gated_repetitions
            else None
        )
        if self.repetition_gates is not None:
            with torch.no_grad():
                self.repetition_gates[0].fill_(1.0)
            self.repetition_gates.optimizer_role = "standard"
            self.repetition_gates.lr_multiplier = 1.0
        self.collect_activations = False
        self.last_activation_rms: list[float] = []

    def _record(self, x: torch.Tensor) -> None:
        if self.collect_activations:
            self.last_activation_rms.append(float(
                torch.sqrt(x.detach().float().square().mean())
            ))

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        if self.collect_activations:
            self.last_activation_rms = []
        if self.form == "sequential":
            for repetition in range(self.repetitions):
                for index, layer in enumerate(self.layers):
                    x = layer(x)
                    final_application = (
                        repetition + 1 == self.repetitions
                        and index + 1 == len(self.layers)
                    )
                    if not final_application:
                        x = F.silu(x)
                    self._record(x)
            return x
        for repetition in range(self.repetitions):
            for index, layer in enumerate(self.layers):
                multiplier = self.residual_multiplier
                if self.repetition_gates is not None:
                    multiplier = (
                        multiplier
                        * self.repetition_gates[repetition, index]
                    )
                x = layer(x, multiplier)
                self._record(x)
        return x


class FullWidthStudent(nn.Module):
    """No-bottleneck student: embed -> flatten -> N-wide stack -> last slot -> head."""

    def __init__(
        self,
        config: ArchitectureConfig,
        tied_embedding: torch.Tensor,
        *,
        vocab_size: int,
        trainable_embedding: bool = False,
    ) -> None:
        super().__init__()
        config.validate()
        if tied_embedding.ndim != 2:
            raise ValueError("tied embedding must be a matrix")
        if tied_embedding.shape[1] != config.embedding_width:
            raise ValueError("embedding width disagrees with architecture")
        if not 0 < vocab_size <= tied_embedding.shape[0]:
            raise ValueError("invalid tokenizer vocabulary size")
        self.config = config
        self.vocab_size = int(vocab_size)
        if trainable_embedding:
            embedding = nn.Parameter(tied_embedding.detach())
            embedding.lr_multiplier = 1.0
            embedding.optimizer_role = "tied_embedding"
            self.register_parameter("tied_embedding", embedding)
        else:
            # persistent=False keeps identical frozen Qwen weights out of every
            # trainable-only checkpoint. This tensor serves both I/O directions.
            self.register_buffer(
                "tied_embedding", tied_embedding.detach(), persistent=False
            )
        self.stack = FullWidthStack(config)

    def hidden(self, token_ids: torch.Tensor) -> torch.Tensor:
        expected = (token_ids.shape[0], self.config.context_length)
        if token_ids.ndim != 2 or token_ids.shape != expected:
            raise ValueError(
                f"expected [batch,{self.config.context_length}] token ids"
            )
        embedded = F.embedding(token_ids, self.tied_embedding)
        flat = embedded.reshape(token_ids.shape[0], self.config.full_width)
        mixed = self.stack(flat)
        return mixed.reshape(
            token_ids.shape[0],
            self.config.context_length,
            self.config.embedding_width,
        )[:, -1, :]

    def forward(self, token_ids: torch.Tensor) -> torch.Tensor:
        terminal = self.hidden(token_ids)
        return F.linear(terminal, self.tied_embedding[: self.vocab_size])

    def trainable_parameter_count(self) -> int:
        return sum(parameter.numel() for parameter in self.parameters())

    def collect_activation_diagnostics(self, enabled: bool) -> None:
        self.stack.collect_activations = bool(enabled)

    def activation_metrics(self) -> dict[str, float]:
        values = self.stack.last_activation_rms
        if not values:
            return {}
        result = {
            "diagnostic/activation_rms_max": max(values),
            "diagnostic/activation_rms_last": values[-1],
        }
        result.update({
            f"diagnostic/layer_{index:03d}_activation_rms": value
            for index, value in enumerate(values)
        })
        return result

    def btt_metrics(self) -> dict[str, float]:
        cores = []
        gains = []
        multipliers = []
        for module in self.modules():
            if not isinstance(module, BTTLinear):
                continue
            for core, gain in zip(module.cores, module.gains, strict=True):
                cores.append(float(torch.sqrt(
                    core.detach().float().square().mean()
                )))
                gains.append(float(gain.detach()))
                multipliers.append(float(core.lr_multiplier))
        if not cores:
            return {}
        return {
            "diagnostic/btt_core_raw_rms_min": min(cores),
            "diagnostic/btt_core_raw_rms_max": max(cores),
            "diagnostic/btt_core_raw_rms_mean": sum(cores) / len(cores),
            "diagnostic/btt_gain_min": min(gains),
            "diagnostic/btt_gain_max": max(gains),
            "diagnostic/btt_gain_mean": sum(gains) / len(gains),
            "optimizer/btt_lr_multiplier_min": min(multipliers),
            "optimizer/btt_lr_multiplier_max": max(multipliers),
        }

    def kronecker_metrics(self) -> dict[str, float]:
        factors = []
        factor_target_ratios = []
        gains = []
        mixing_max_shares = []
        mixing_effective_ranks = []
        multipliers = []
        for module in self.modules():
            if not isinstance(module, KroneckerLinear):
                continue
            for index, (factor, gain) in enumerate(zip(
                module.factors, module.gains, strict=True
            )):
                factor_rms = float(torch.sqrt(
                    factor.detach().float().square().mean()
                ))
                factors.append(factor_rms)
                factor_target_ratios.append(
                    factor_rms / module.target_rms_values[index]
                )
                gains.extend(
                    float(value)
                    for value in gain.detach().float().reshape(-1)
                )
                multipliers.append(float(factor.lr_multiplier))
            if module.mixing is not None:
                absolute = module.mixing.detach().float().abs()
                probability = absolute / absolute.sum().clamp_min(1e-12)
                entropy = -sum(
                    float(value) * math.log(max(float(value), 1e-12))
                    for value in probability
                )
                mixing_max_shares.append(float(probability.max()))
                mixing_effective_ranks.append(math.exp(entropy))
        if not factors:
            return {}
        result = {
            "diagnostic/kronecker_factor_raw_rms_min": min(factors),
            "diagnostic/kronecker_factor_raw_rms_max": max(factors),
            "diagnostic/kronecker_factor_raw_rms_mean":
                sum(factors) / len(factors),
            "diagnostic/kronecker_factor_target_ratio_min":
                min(factor_target_ratios),
            "diagnostic/kronecker_factor_target_ratio_max":
                max(factor_target_ratios),
            "diagnostic/kronecker_factor_target_ratio_mean":
                sum(factor_target_ratios) / len(factor_target_ratios),
            "diagnostic/kronecker_gain_min": min(gains),
            "diagnostic/kronecker_gain_max": max(gains),
            "diagnostic/kronecker_gain_mean": sum(gains) / len(gains),
            "optimizer/kronecker_lr_multiplier_min": min(multipliers),
            "optimizer/kronecker_lr_multiplier_max": max(multipliers),
        }
        result.update({
            f"diagnostic/kronecker_factor_{index:03d}_target_ratio": ratio
            for index, ratio in enumerate(factor_target_ratios)
        })
        if mixing_max_shares:
            result.update({
                "diagnostic/kronecker_mixing_max_share_max":
                    max(mixing_max_shares),
                "diagnostic/kronecker_mixing_max_share_mean":
                    sum(mixing_max_shares) / len(mixing_max_shares),
                "diagnostic/kronecker_mixing_effective_rank_min":
                    min(mixing_effective_ranks),
                "diagnostic/kronecker_mixing_effective_rank_mean":
                    sum(mixing_effective_ranks) / len(mixing_effective_ranks),
            })
        return result


def optimizer_parameter_groups(
    model: nn.Module,
    base_lr: float,
    parameterization: str,
    *,
    role_lr_multipliers: Mapping[str, float] | None = None,
    role_lr_overrides: Mapping[str, float] | None = None,
    role_weight_decay_overrides: Mapping[str, float] | None = None,
    default_weight_decay: float = 0.01,
) -> tuple[list[dict], list[dict]]:
    """Build structure- and role-aware AdamW parameter groups.

    A role LR multiplier is applied after the optional structural μP
    multiplier. An absolute role override replaces both multipliers. A role
    cannot use both policies, which keeps experiment configurations
    unambiguous.
    """
    if parameterization not in ("uniform", "mup"):
        raise ValueError(f"unsupported LR parameterization {parameterization}")
    role_lr_multipliers = dict(role_lr_multipliers or {})
    role_lr_overrides = dict(role_lr_overrides or {})
    role_weight_decay_overrides = dict(role_weight_decay_overrides or {})
    conflicting_roles = role_lr_multipliers.keys() & role_lr_overrides.keys()
    if conflicting_roles:
        roles = ", ".join(sorted(conflicting_roles))
        raise ValueError(
            f"roles cannot have LR multipliers and absolute overrides: {roles}"
        )

    def validate_values(
        label: str,
        values: Mapping[str, float],
    ) -> None:
        for role, value in values.items():
            if not isinstance(role, str) or not role:
                raise ValueError(f"{label} roles must be non-empty strings")
            if not math.isfinite(float(value)) or float(value) < 0:
                raise ValueError(
                    f"{label} for role {role!r} must be finite and nonnegative"
                )

    if not math.isfinite(float(base_lr)) or float(base_lr) < 0:
        raise ValueError("base learning rate must be finite and nonnegative")
    if (
        not math.isfinite(float(default_weight_decay))
        or float(default_weight_decay) < 0
    ):
        raise ValueError(
            "default weight decay must be finite and nonnegative"
        )
    validate_values("LR multiplier", role_lr_multipliers)
    validate_values("LR override", role_lr_overrides)
    validate_values("weight-decay override", role_weight_decay_overrides)

    grouped: dict[tuple[float, str, float, float], list[nn.Parameter]] = {}
    names: dict[tuple[float, str, float, float], list[str]] = {}
    observed_roles: set[str] = set()
    for name, parameter in model.named_parameters():
        multiplier = (
            float(getattr(parameter, "lr_multiplier", 1.0))
            if parameterization == "mup"
            else 1.0
        )
        role = str(getattr(parameter, "optimizer_role", "standard"))
        observed_roles.add(role)
        role_multiplier = float(role_lr_multipliers.get(role, 1.0))
        effective_lr = float(role_lr_overrides.get(
            role,
            base_lr * multiplier * role_multiplier,
        ))
        weight_decay = float(role_weight_decay_overrides.get(
            role, default_weight_decay
        ))
        key = (multiplier, role, effective_lr, weight_decay)
        grouped.setdefault(key, []).append(parameter)
        names.setdefault(key, []).append(name)

    configured_roles = (
        role_lr_multipliers.keys()
        | role_lr_overrides.keys()
        | role_weight_decay_overrides.keys()
    )
    unknown_roles = configured_roles - observed_roles
    if unknown_roles:
        roles = ", ".join(sorted(unknown_roles))
        raise ValueError(f"optimizer overrides reference absent roles: {roles}")

    parameter_groups = []
    metadata = []
    for (multiplier, role, effective_lr, weight_decay), parameters in sorted(
        grouped.items(), key=lambda item: item[0]
    ):
        parameter_groups.append({
            "params": parameters,
            "lr": effective_lr,
            "weight_decay": weight_decay,
        })
        metadata.append({
            "role": role,
            "multiplier": multiplier,
            "role_lr_multiplier": float(
                role_lr_multipliers.get(role, 1.0)
            ),
            "role_lr_override": (
                float(role_lr_overrides[role])
                if role in role_lr_overrides
                else None
            ),
            "effective_lr": effective_lr,
            "weight_decay": weight_decay,
            "parameter_tensors": len(parameters),
            "parameters": sum(value.numel() for value in parameters),
            "sample_names":
                names[(multiplier, role, effective_lr, weight_decay)][:4],
        })
    return parameter_groups, metadata


def optimizer_role_diagnostics(
    parameter_groups: list[dict],
    metadata: list[dict],
) -> dict[str, float]:
    """Summarize gradients and parameters by optimizer role without mutation.

    The update ratio uses ``lr * raw_gradient`` and is therefore a diagnostic
    proxy, not a reconstruction of an AdamW update from its moment state.
    """
    if len(parameter_groups) != len(metadata):
        raise ValueError("optimizer groups and metadata must have equal length")
    totals: dict[str, dict[str, float]] = {}
    for group, row in zip(parameter_groups, metadata, strict=True):
        role = str(row["role"])
        lr = float(group["lr"])
        role_totals = totals.setdefault(role, {
            "parameter_squared": 0.0,
            "gradient_squared": 0.0,
            "raw_update_squared": 0.0,
            "parameters": 0.0,
            "gradient_tensors": 0.0,
        })
        for parameter in group["params"]:
            value = parameter.detach().float()
            role_totals["parameter_squared"] += float(value.square().sum())
            role_totals["parameters"] += float(parameter.numel())
            if parameter.grad is None:
                continue
            gradient = parameter.grad.detach().float()
            gradient_squared = float(gradient.square().sum())
            role_totals["gradient_squared"] += gradient_squared
            role_totals["raw_update_squared"] += lr * lr * gradient_squared
            role_totals["gradient_tensors"] += 1.0

    result: dict[str, float] = {}
    for role, role_totals in totals.items():
        parameters = role_totals["parameters"]
        parameter_squared = role_totals["parameter_squared"]
        parameter_norm = math.sqrt(parameter_squared)
        prefix = f"optimizer_role/{role}"
        result[f"{prefix}/parameter_rms"] = math.sqrt(
            parameter_squared / max(parameters, 1.0)
        )
        result[f"{prefix}/gradient_norm"] = math.sqrt(
            role_totals["gradient_squared"]
        )
        result[f"{prefix}/raw_gradient_update_parameter_ratio"] = (
            math.sqrt(role_totals["raw_update_squared"])
            / max(parameter_norm, 1e-30)
        )
        result[f"{prefix}/parameters"] = parameters
        result[f"{prefix}/gradient_tensors"] = role_totals[
            "gradient_tensors"
        ]
    return result


@torch.no_grad()
def optimizer_adam_role_diagnostics(
    optimizer: torch.optim.Optimizer,
    metadata: list[dict],
    *,
    chunk_elements: int = 1_048_576,
) -> dict[str, float]:
    """Summarize Adam moments and its adaptive update by optimizer role.

    This reconstructs the adaptive Adam update from the post-step moment
    state, including bias correction and each group's current LR and epsilon.
    It intentionally excludes decoupled weight decay. The ratio uses the
    post-step parameter norm and is a proxy rather than a copied before/after
    measurement; no full-model parameter snapshot is allocated.
    """
    if len(optimizer.param_groups) != len(metadata):
        raise ValueError("optimizer groups and metadata must have equal length")
    if chunk_elements <= 0:
        raise ValueError("diagnostic chunk size must be positive")
    totals: dict[str, dict[str, float]] = {}
    for group, row in zip(
        optimizer.param_groups, metadata, strict=True
    ):
        role = str(row["role"])
        lr = float(group["lr"])
        beta1, beta2 = (float(value) for value in group["betas"])
        eps = float(group["eps"])
        role_totals = totals.setdefault(role, {
            "parameter_squared": 0.0,
            "exp_avg_squared": 0.0,
            "exp_avg_sq_sum": 0.0,
            "adam_update_squared": 0.0,
            "state_parameters": 0.0,
            "state_tensors": 0.0,
        })
        for parameter in group["params"]:
            state = optimizer.state.get(parameter, {})
            exp_avg = state.get("exp_avg")
            exp_avg_sq = state.get("exp_avg_sq")
            step_value = state.get("step")
            if (
                not isinstance(exp_avg, torch.Tensor)
                or not isinstance(exp_avg_sq, torch.Tensor)
                or step_value is None
            ):
                continue
            step = int(
                step_value.item()
                if isinstance(step_value, torch.Tensor)
                else step_value
            )
            if step <= 0:
                continue
            bias_correction1 = 1.0 - beta1**step
            bias_correction2_sqrt = math.sqrt(1.0 - beta2**step)
            parameter_flat = parameter.detach().reshape(-1)
            exp_avg_flat = exp_avg.detach().reshape(-1)
            exp_avg_sq_flat = exp_avg_sq.detach().reshape(-1)
            for start in range(0, parameter.numel(), chunk_elements):
                stop = min(start + chunk_elements, parameter.numel())
                parameter_chunk = parameter_flat[start:stop].float()
                exp_avg_chunk = exp_avg_flat[start:stop].float()
                exp_avg_sq_chunk = exp_avg_sq_flat[start:stop].float()
                denominator = exp_avg_sq_chunk.sqrt()
                denominator.div_(bias_correction2_sqrt).add_(eps)
                update = exp_avg_chunk / denominator
                update.mul_(lr / bias_correction1)
                role_totals["parameter_squared"] += float(
                    parameter_chunk.square().sum()
                )
                role_totals["exp_avg_squared"] += float(
                    exp_avg_chunk.square().sum()
                )
                role_totals["exp_avg_sq_sum"] += float(
                    exp_avg_sq_chunk.sum()
                )
                role_totals["adam_update_squared"] += float(
                    update.square().sum()
                )
            role_totals["state_parameters"] += float(parameter.numel())
            role_totals["state_tensors"] += 1.0

    result: dict[str, float] = {}
    for role, role_totals in totals.items():
        state_parameters = role_totals["state_parameters"]
        parameter_norm = math.sqrt(role_totals["parameter_squared"])
        update_norm = math.sqrt(role_totals["adam_update_squared"])
        prefix = f"optimizer_role/{role}"
        result[f"{prefix}/exp_avg_norm"] = math.sqrt(
            role_totals["exp_avg_squared"]
        )
        result[f"{prefix}/sqrt_exp_avg_sq_rms"] = math.sqrt(
            role_totals["exp_avg_sq_sum"] / max(state_parameters, 1.0)
        )
        result[f"{prefix}/adam_update_norm"] = update_norm
        result[f"{prefix}/adam_update_parameter_ratio"] = (
            update_norm / max(parameter_norm, 1e-30)
        )
        result[f"{prefix}/optimizer_state_tensors"] = role_totals[
            "state_tensors"
        ]
    return result
