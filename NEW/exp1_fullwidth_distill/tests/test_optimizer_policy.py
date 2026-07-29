import math

import pytest
import torch
from torch import nn

from qwen_fullwidth_distill.model import (
    optimizer_adam_role_diagnostics,
    optimizer_parameter_groups,
    optimizer_role_diagnostics,
)


class RoleModel(nn.Module):
    def __init__(self) -> None:
        super().__init__()
        self.factor = nn.Parameter(torch.tensor([3.0, 4.0]))
        self.factor.lr_multiplier = 4.0
        self.factor.optimizer_role = "kronecker_factor"
        self.bias = nn.Parameter(torch.tensor([0.0, 2.0]))
        self.bias.optimizer_role = "bias"


def groups_by_role(groups: list[dict], metadata: list[dict]) -> dict:
    return {
        row["role"]: (group, row)
        for group, row in zip(groups, metadata, strict=True)
    }


def test_role_optimizer_policy_supports_multipliers_absolute_lrs_and_decay():
    model = RoleModel()
    groups, metadata = optimizer_parameter_groups(
        model,
        1e-3,
        "mup",
        role_lr_multipliers={"kronecker_factor": 0.5},
        role_lr_overrides={"bias": 3e-4},
        role_weight_decay_overrides={
            "kronecker_factor": 0.0,
            "bias": 0.02,
        },
    )
    by_role = groups_by_role(groups, metadata)

    factor_group, factor_row = by_role["kronecker_factor"]
    assert factor_group["lr"] == pytest.approx(2e-3)
    assert factor_group["weight_decay"] == 0.0
    assert factor_row["multiplier"] == 4.0
    assert factor_row["role_lr_multiplier"] == 0.5
    assert factor_row["role_lr_override"] is None
    assert factor_row["effective_lr"] == pytest.approx(2e-3)
    assert factor_row["weight_decay"] == 0.0
    assert factor_row["parameters"] == 2

    bias_group, bias_row = by_role["bias"]
    assert bias_group["lr"] == pytest.approx(3e-4)
    assert bias_group["weight_decay"] == pytest.approx(0.02)
    assert bias_row["role_lr_override"] == pytest.approx(3e-4)
    assert bias_row["parameters"] == 2


def test_default_optimizer_group_behavior_remains_standard_adamw():
    groups, metadata = optimizer_parameter_groups(
        RoleModel(), 1e-3, "mup"
    )
    by_role = groups_by_role(groups, metadata)
    assert by_role["kronecker_factor"][0]["lr"] == pytest.approx(4e-3)
    assert by_role["bias"][0]["lr"] == pytest.approx(1e-3)
    assert {group["weight_decay"] for group in groups} == {0.01}
    assert {row["weight_decay"] for row in metadata} == {0.01}


def test_role_optimizer_policy_rejects_ambiguous_or_absent_roles():
    with pytest.raises(ValueError, match="cannot have"):
        optimizer_parameter_groups(
            RoleModel(),
            1e-3,
            "mup",
            role_lr_multipliers={"bias": 2.0},
            role_lr_overrides={"bias": 3e-4},
        )
    with pytest.raises(ValueError, match="absent roles"):
        optimizer_parameter_groups(
            RoleModel(),
            1e-3,
            "mup",
            role_lr_overrides={"not_a_role": 3e-4},
        )


def test_optimizer_role_diagnostics_are_pure_and_aggregate_by_role():
    model = RoleModel()
    groups, metadata = optimizer_parameter_groups(
        model,
        1e-3,
        "mup",
        role_lr_overrides={"kronecker_factor": 2e-2},
    )
    model.factor.grad = torch.tensor([0.3, 0.4])
    original_factor = model.factor.detach().clone()

    metrics = optimizer_role_diagnostics(groups, metadata)

    assert metrics[
        "optimizer_role/kronecker_factor/parameter_rms"
    ] == pytest.approx(math.sqrt(25.0 / 2.0))
    assert metrics[
        "optimizer_role/kronecker_factor/gradient_norm"
    ] == pytest.approx(0.5)
    assert metrics[
        "optimizer_role/kronecker_factor/"
        "raw_gradient_update_parameter_ratio"
    ] == pytest.approx(0.002)
    assert metrics[
        "optimizer_role/kronecker_factor/gradient_tensors"
    ] == 1.0
    assert metrics["optimizer_role/bias/gradient_norm"] == 0.0
    torch.testing.assert_close(model.factor, original_factor)


def test_adam_role_diagnostics_reconstruct_bias_corrected_update():
    model = RoleModel()
    groups, metadata = optimizer_parameter_groups(
        model,
        1e-3,
        "mup",
        role_lr_overrides={"kronecker_factor": 2e-2},
        default_weight_decay=0.0,
    )
    optimizer = torch.optim.AdamW(
        groups,
        betas=(0.9, 0.999),
        eps=1e-8,
    )
    model.factor.grad = torch.tensor([0.3, 0.4])
    optimizer.step()

    metrics = optimizer_adam_role_diagnostics(
        optimizer,
        metadata,
        chunk_elements=1,
    )

    prefix = "optimizer_role/kronecker_factor"
    assert metrics[f"{prefix}/exp_avg_norm"] == pytest.approx(0.05)
    assert metrics[f"{prefix}/sqrt_exp_avg_sq_rms"] == pytest.approx(
        math.sqrt(0.000125)
    )
    expected_update_norm = math.sqrt(2.0) * 2e-2
    assert metrics[f"{prefix}/adam_update_norm"] == pytest.approx(
        expected_update_norm
    )
    assert metrics[f"{prefix}/adam_update_parameter_ratio"] == pytest.approx(
        expected_update_norm / float(model.factor.detach().norm()),
        rel=1e-5,
    )
    assert metrics[f"{prefix}/optimizer_state_tensors"] == 1.0
    assert metrics["optimizer_role/bias/exp_avg_norm"] == 0.0


def test_adam_role_diagnostics_reject_invalid_contracts():
    model = RoleModel()
    groups, metadata = optimizer_parameter_groups(model, 1e-3, "mup")
    optimizer = torch.optim.AdamW(groups)
    with pytest.raises(ValueError, match="equal length"):
        optimizer_adam_role_diagnostics(optimizer, metadata[:-1])
    with pytest.raises(ValueError, match="chunk size"):
        optimizer_adam_role_diagnostics(
            optimizer,
            metadata,
            chunk_elements=0,
        )
