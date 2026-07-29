from __future__ import annotations

from pathlib import Path

import torch

from qwen_normuon_pretrain.config import (
    AUX_ADAMW_LR,
    DEFAULT_NORMUON_LR,
    FINAL_EXAMPLES,
    NORMUON_LR_GRID,
    SCREEN_EXAMPLES,
    Architecture,
    Trial,
    default_optimizer_policy,
    final_trials,
    screen_trials,
)
from qwen_normuon_pretrain.model import (
    EXPECTED_AUX_PARAMETERS,
    EXPECTED_FACTOR_MATRICES,
    EXPECTED_FACTOR_PARAMETERS,
    EXPECTED_FACTOR_TENSORS,
    EXPECTED_TRAINABLE_PARAMETERS,
    NextTokenStudent,
)
from qwen_normuon_pretrain.normuon import (
    SingleDeviceNorMuon,
    normuon_update,
    zeropower_via_newton_schulz5,
)
from qwen_normuon_pretrain.pretrain import (
    load_checkpoint,
    make_optimizers,
    next_token_cross_entropy_rows,
    save_checkpoint,
    set_learning_rates,
    study_plan,
)


def tiny_architecture() -> Architecture:
    return Architecture(
        context_length=4,
        embedding_width=4,
        depth=1,
        expansion=4,
        repetitions=1,
        rank=2,
        rank_chunk=2,
    )


def tiny_trial() -> Trial:
    return Trial(
        stage="screen",
        normuon_lr=1e-3,
        effective_batch=2,
        examples=8,
        warmup_steps=1,
        stable_steps=2,
        cooldown_steps=1,
        architecture=tiny_architecture(),
    )


def test_batched_normuon_has_independent_matrix_state_and_norm():
    torch.manual_seed(4)
    gradient = torch.randn(3, 4, 6)
    gradient[2].zero_()
    momentum = torch.zeros_like(gradient)
    second = torch.zeros_like(gradient[..., 0:1])

    update = normuon_update(
        gradient,
        momentum,
        second,
    )

    assert update.shape == gradient.shape
    assert second.shape == (3, 4, 1)
    assert torch.count_nonzero(momentum[2]) == 0
    assert torch.count_nonzero(second[2]) == 0
    assert torch.count_nonzero(update[2]) == 0
    direction = gradient * (0.95 * 0.05 + 0.05)
    orthogonalized = zeropower_via_newton_schulz5(direction)
    torch.testing.assert_close(
        update[:2].norm(dim=(-2, -1)),
        orthogonalized[:2].float().norm(dim=(-2, -1)),
        rtol=2e-2,
        atol=2e-2,
    )


def test_shape_bucket_matches_individual_official_updates():
    torch.manual_seed(9)
    parameters = [
        torch.nn.Parameter(torch.randn(2, 4, 6))
        for _ in range(3)
    ]
    gradients = [torch.randn_like(parameter) for parameter in parameters]
    expected_parameters = [
        parameter.detach().clone() for parameter in parameters
    ]
    expected_momenta = [
        torch.zeros_like(parameter) for parameter in parameters
    ]
    expected_seconds = [
        torch.zeros_like(parameter[..., 0:1])
        for parameter in parameters
    ]
    for index, gradient in enumerate(gradients):
        update = normuon_update(
            gradient,
            expected_momenta[index],
            expected_seconds[index],
        )
        expected_parameters[index].mul_(1.0 - 0.01 * 0.02)
        expected_parameters[index].add_(update, alpha=-0.02)
        parameters[index].grad = gradient.clone()

    optimizer = SingleDeviceNorMuon(
        parameters,
        lr=0.02,
        weight_decay=0.01,
    )
    optimizer.step()

    for index, parameter in enumerate(parameters):
        torch.testing.assert_close(
            parameter,
            expected_parameters[index],
        )
        torch.testing.assert_close(
            optimizer.state[parameter]["momentum_buffer"],
            expected_momenta[index],
        )
        torch.testing.assert_close(
            optimizer.state[parameter]["second_momentum_buffer"],
            expected_seconds[index],
        )


def test_shape_bucket_skips_parameter_without_gradient():
    active = torch.nn.Parameter(torch.randn(2, 3, 4))
    inactive = torch.nn.Parameter(torch.randn(2, 3, 4))
    active_before = active.detach().clone()
    inactive_before = inactive.detach().clone()
    active.grad = torch.randn_like(active)
    optimizer = SingleDeviceNorMuon(
        [active, inactive],
        lr=1e-2,
    )
    optimizer.step()
    assert not torch.equal(active, active_before)
    assert torch.equal(inactive, inactive_before)
    assert inactive not in optimizer.state


def test_tiny_model_routes_every_factor_slice_and_updates_auxiliary():
    trial = tiny_trial()
    student = NextTokenStudent(
        trial.architecture,
        torch.randn(11, 4),
        vocab_size=11,
    )
    factors = student.factor_parameters()
    auxiliary = student.auxiliary_parameters()
    assert len(factors) == 6
    assert sum(parameter.shape[0] for parameter in factors) == 12
    assert all(parameter.ndim == 3 for parameter in factors)
    assert any(
        parameter is student.tied_embedding
        for parameter in auxiliary
    )
    assert not ({id(value) for value in factors} & {
        id(value) for value in auxiliary
    })
    factor_optimizer, aux_optimizer, metadata = make_optimizers(
        student,
        trial,
        device="cpu",
    )
    factor_before = factors[0].detach().clone()
    embedding_before = student.tied_embedding.detach().clone()
    ids = torch.tensor([[0, 1, 2, 3], [3, 4, 5, 6]])
    targets = torch.tensor([7, 8])
    next_token_cross_entropy_rows(
        student(ids),
        targets,
    ).mean().backward()
    factor_optimizer.step()
    aux_optimizer.step()
    assert not torch.equal(factors[0], factor_before)
    assert not torch.equal(student.tied_embedding, embedding_before)
    assert metadata["factor_tensors"] == 6
    assert metadata["factor_matrices"] == 12


def test_full_model_inventory_is_exact_on_meta_device():
    architecture = Architecture()
    with torch.device("meta"):
        student = NextTokenStudent(
            architecture,
            torch.empty(248_320, 1_024),
            vocab_size=248_320,
        )
    inventory = student.validate_study_inventory()
    assert inventory["factor_tensors"] == EXPECTED_FACTOR_TENSORS
    assert inventory["factor_matrices"] == EXPECTED_FACTOR_MATRICES
    assert inventory["factor_parameters"] == EXPECTED_FACTOR_PARAMETERS
    assert inventory["auxiliary_parameters"] == EXPECTED_AUX_PARAMETERS
    assert (
        inventory["trainable_parameters"]
        == EXPECTED_TRAINABLE_PARAMETERS
        == 338_552_032
    )


def test_staged_grid_and_direct_learning_rates_are_exact():
    screen = screen_trials()
    assert [trial.normuon_lr for trial in screen] == list(
        NORMUON_LR_GRID
    )
    assert {trial.examples for trial in screen} == {SCREEN_EXAMPLES}
    assert {trial.effective_batch for trial in screen} == {2_048}
    assert {trial.steps for trial in screen} == {128}
    finalists = final_trials((1e-3, 1e-2))
    assert {
        (trial.normuon_lr, trial.effective_batch)
        for trial in finalists
    } == {
        (lr, batch)
        for lr in (1e-3, 1e-2)
        for batch in (2_048, 4_096)
    }
    assert {trial.examples for trial in finalists} == {FINAL_EXAMPLES}
    plan = study_plan()
    assert plan["teacher_used"] is False
    assert plan["teacher_cache_used"] is False
    assert plan["optimizer"]["direct_lr_grid"] == list(NORMUON_LR_GRID)
    assert plan["optimizer"]["shape_batched_newton_schulz"] is True
    assert DEFAULT_NORMUON_LR == 3e-3
    policy = default_optimizer_policy()
    assert policy["factor_lr"] == DEFAULT_NORMUON_LR
    assert (
        policy["factor_update_granularity"]
        == "independent_rank_slice_matrix"
    )
    assert policy["shape_batched_newton_schulz"] is True
    assert plan["optimizer"]["selected_default"] == policy


def test_schedule_sets_direct_factor_lr_without_extra_scaling():
    trial = tiny_trial()
    student = NextTokenStudent(
        trial.architecture,
        torch.randn(11, 4),
        vocab_size=11,
    )
    factor_optimizer, aux_optimizer, _ = make_optimizers(
        student,
        trial,
        device="cpu",
    )
    multiplier, factor_lr, aux_lr = set_learning_rates(
        factor_optimizer,
        aux_optimizer,
        trial,
        step=2,
    )
    assert multiplier == 1.0
    assert factor_lr == trial.normuon_lr
    assert aux_lr == AUX_ADAMW_LR
    assert {group["lr"] for group in factor_optimizer.param_groups} == {
        trial.normuon_lr
    }
    assert {group["lr"] for group in aux_optimizer.param_groups} == {
        AUX_ADAMW_LR
    }


def test_checkpoint_restores_model_and_both_optimizers(tmp_path):
    trial = tiny_trial()
    torch.manual_seed(8)
    student = NextTokenStudent(
        trial.architecture,
        torch.randn(11, 4),
        vocab_size=11,
    )
    factor_optimizer, aux_optimizer, _ = make_optimizers(
        student,
        trial,
        device="cpu",
    )
    ids = torch.tensor([[0, 1, 2, 3], [3, 4, 5, 6]])
    targets = torch.tensor([7, 8])
    next_token_cross_entropy_rows(
        student(ids),
        targets,
    ).mean().backward()
    factor_optimizer.step()
    aux_optimizer.step()
    expected = {
        name: value.detach().clone()
        for name, value in student.state_dict().items()
    }
    factor_state = factor_optimizer.state_dict()
    aux_state = aux_optimizer.state_dict()
    path = tmp_path / "progress.pt"
    save_checkpoint(
        path,
        student,
        factor_optimizer,
        aux_optimizer,
        trial,
        step=1,
        examples_seen=2,
        initial_hashes={"embedding": "a" * 64, "factors": "b" * 64},
        initial_validation={"cross_entropy": 3.0},
        elapsed_wall_seconds=1.5,
    )
    with torch.no_grad():
        for parameter in student.parameters():
            parameter.zero_()
    factor_optimizer.state.clear()
    aux_optimizer.state.clear()
    resumed = load_checkpoint(
        path,
        student,
        factor_optimizer,
        aux_optimizer,
        trial,
        {"embedding": "a" * 64, "factors": "b" * 64},
    )
    assert resumed["step"] == 1
    assert resumed["examples_seen"] == 2
    for name, value in student.state_dict().items():
        torch.testing.assert_close(value, expected[name])
    assert (
        factor_optimizer.state_dict()["state"].keys()
        == factor_state["state"].keys()
    )
    assert (
        aux_optimizer.state_dict()["state"].keys()
        == aux_state["state"].keys()
    )


def test_new_package_has_no_legacy_scaling_identifiers():
    root = (
        Path(__file__).resolve().parents[1]
        / "qwen_normuon_pretrain"
    )
    source = "\n".join(
        path.read_text()
        for path in sorted(root.glob("*.py"))
    ).lower()
    assert "lr_parameterization" not in source
    assert "lr_multiplier" not in source
    assert '"mup"' not in source
    assert "'mup'" not in source
