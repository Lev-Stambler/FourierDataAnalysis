import qwen_kron_distill.train as train_module
import torch
from qwen_kron_distill.config import (
    AUX_ADAMW_LR,
    NORMUON_LR,
    Architecture,
)
from qwen_kron_distill.model import TensorKroneckerStudent
from qwen_kron_distill.train import make_optimizers
from qwen_normuon_pretrain.normuon import SingleDeviceNorMuon


def test_student_uses_small_trainable_tied_vocabulary_factors():
    architecture = Architecture(factor_order=3, depth=2, rank=2)
    student = TensorKroneckerStudent(
        architecture,
        vocab_size=31,
        dtype=torch.float32,
        seed=19,
    )
    token_ids = torch.randint(0, 31, (3, 16))

    hidden = student.hidden(token_ids)
    logits = student(token_ids)

    assert student.vocab_modes == (1, 31)
    assert [tuple(value.shape) for value in student.vocabulary_factors] == [
        (1, 64),
        (31, 64),
    ]
    assert hidden.shape == (3, 64)
    assert logits.shape == (3, 31)
    assert student.terminal.linear.output_modes == (1, 8, 8)
    torch.testing.assert_close(
        logits,
        (
            hidden[:, None, None, :]
            * student.vocabulary_factors[0][None, :, None, :]
            * student.vocabulary_factors[1][None, None, :, :]
        ).sum(dim=-1).reshape(3, 31),
    )


def test_student_uses_architecture_width_for_tied_vocabulary():
    architecture = Architecture(
        factor_order=2,
        depth=1,
        rank=2,
        embedding_width=128,
    )
    student = TensorKroneckerStudent(
        architecture,
        vocab_size=31,
        dtype=torch.float32,
    )
    token_ids = torch.randint(0, 31, (2, 16))

    assert [tuple(value.shape) for value in student.vocabulary_factors] == [
        (1, 128),
        (31, 128),
    ]
    assert student.hidden(token_ids).shape == (2, 128)
    assert student(token_ids).shape == (2, 31)


def test_factor_and_auxiliary_parameter_routes_are_exact():
    student = TensorKroneckerStudent(
        Architecture(factor_order=2, depth=2, rank=4),
        vocab_size=29,
        dtype=torch.float32,
    )
    factors = student.factor_parameters()
    auxiliary = student.auxiliary_parameters()

    assert factors
    assert all(value.ndim in (2, 3) for value in factors)
    assert {id(value) for value in factors}.isdisjoint(
        {id(value) for value in auxiliary}
    )
    assert {id(value) for value in factors + auxiliary} == {
        id(value) for value in student.parameters()
    }
    factor_ids = {id(value) for value in factors}
    assert all(
        id(value) in factor_ids for value in student.vocabulary_factors
    )
    assert student.parameter_inventory()["trainable_parameters"] == sum(
        value.numel() for value in student.parameters()
    )


def test_normuon_and_adamw_both_initialize_state_and_update():
    torch.manual_seed(23)
    student = TensorKroneckerStudent(
        Architecture(factor_order=2, depth=1, rank=2),
        vocab_size=37,
        dtype=torch.float32,
    )
    factor_optimizer, auxiliary_optimizer = make_optimizers(
        student,
        device=torch.device("cpu"),
    )
    factor_before = student.factor_parameters()[0].detach().clone()
    vocabulary_before = [
        value.detach().clone() for value in student.vocabulary_factors
    ]

    loss = student(torch.randint(0, 37, (2, 16))).float().square().mean()
    loss.backward()
    factor_optimizer.step()
    auxiliary_optimizer.step()

    assert factor_optimizer.param_groups[0]["lr"] == NORMUON_LR
    assert auxiliary_optimizer.param_groups[0]["lr"] == AUX_ADAMW_LR
    assert factor_optimizer.state[student.factor_parameters()[0]]["step"] == 1
    assert all(
        factor_optimizer.state[value]["step"] == 1
        for value in student.vocabulary_factors
    )
    assert not torch.equal(
        factor_before,
        student.factor_parameters()[0],
    )
    assert all(
        not torch.equal(before, after)
        for before, after in zip(
            vocabulary_before,
            student.vocabulary_factors,
            strict=True,
        )
    )


def test_normuon_only_routes_every_trainable_tensor(monkeypatch):
    monkeypatch.setattr(
        train_module,
        "STUDY_VARIANT",
        "v5-normuon-lr",
    )
    student = TensorKroneckerStudent(
        Architecture(factor_order=2, depth=2, rank=2),
        vocab_size=37,
        dtype=torch.float32,
    )

    factor_optimizer, auxiliary_optimizer = make_optimizers(
        student,
        device=torch.device("cpu"),
        factor_lr=0.5,
        auxiliary_lr=0.5,
    )

    assert isinstance(factor_optimizer, SingleDeviceNorMuon)
    assert isinstance(auxiliary_optimizer, SingleDeviceNorMuon)
    assert all(value.ndim >= 2 for value in student.parameters())
    assert factor_optimizer.param_groups[0]["lr"] == 0.5
    assert auxiliary_optimizer.param_groups[0]["lr"] == 0.5
