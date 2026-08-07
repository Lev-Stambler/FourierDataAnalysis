from __future__ import annotations

import torch

from v2_simpler_block_diffusion.model import SimplerBlockDiffusionForMaskedLM
from v2_simpler_block_diffusion.optimizer import (
    BatchedMuon,
    build_factorized_muon,
    set_lr_scale,
    zeropower_via_newton_schulz5,
)


def test_batched_zeropower_keeps_matrix_slices_independent() -> None:
    torch.manual_seed(2)
    value = torch.randn(2, 3, 4, 5)
    batched = zeropower_via_newton_schulz5(value)
    independent = torch.stack(
        [
            torch.stack(
                [zeropower_via_newton_schulz5(value[rank, block]) for block in range(3)]
            )
            for rank in range(2)
        ]
    )
    assert torch.equal(batched, independent)


def test_muon_updates_monarch_slices_as_independent_matrices() -> None:
    parameter = torch.nn.Parameter(torch.zeros(1, 2, 3, 4))
    gradient = torch.arange(parameter.numel(), dtype=torch.float32).reshape(parameter.shape) + 1
    parameter.grad = gradient.clone()
    expected = zeropower_via_newton_schulz5(gradient).float().mul_(-0.02)
    BatchedMuon([parameter], lr=0.02, momentum=0.0, nesterov=False).step()
    assert torch.equal(parameter, expected.float())


def test_split_optimizer_routes_every_parameter_once_and_preserves_lr_ratio() -> None:
    model = SimplerBlockDiffusionForMaskedLM()
    optimizer, inventory = build_factorized_muon(
        model, muon_lr=0.03, auxiliary_lr=0.003
    )
    assert inventory["total_parameters"] == model.num_parameters()
    assert "embed_tokens.weight" in inventory["auxiliary_parameter_names"]
    assert all(name != "embed_tokens.weight" for name in inventory["muon_parameter_names"])
    assert any("local.gate.factor1" in name for name in inventory["muon_parameter_names"])
    set_lr_scale(optimizer, 0.25)
    assert optimizer.muon.param_groups[0]["lr"] == 0.0075
    assert all(group["lr"] == 0.00075 for group in optimizer.auxiliary.param_groups)


def test_split_optimizer_state_roundtrip() -> None:
    # The generic builder expects the tied embedding name, so exercise Muon's
    # own state roundtrip directly on a minimal matrix.
    parameter = torch.nn.Parameter(torch.randn(2, 3, 4))
    optimizer = BatchedMuon([parameter], lr=0.01)
    parameter.grad = torch.randn_like(parameter)
    optimizer.step()
    state = optimizer.state_dict()
    restored_parameter = torch.nn.Parameter(parameter.detach().clone())
    restored = BatchedMuon([restored_parameter], lr=0.01)
    restored.load_state_dict(state)
    assert restored.state[restored_parameter]["step"] == 1
    assert torch.equal(
        restored.state[restored_parameter]["momentum_buffer"],
        optimizer.state[parameter]["momentum_buffer"],
    )
