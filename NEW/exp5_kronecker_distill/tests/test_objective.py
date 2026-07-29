from types import SimpleNamespace

import pytest
import torch
import torch.nn.functional as F
from qwen_kron_distill.objective import (
    chunked_linear_forward_kl,
    distribution_metrics,
    exact_kl_rows,
    factorized_vocabulary_forward_kl,
    khatri_rao_forward_kl,
    khatri_rao_logits,
    materialized_probability_linear_forward_kl,
    teacher_distribution,
    teacher_marginals_from_hidden,
    validate_probability_rows,
)


def test_exact_forward_kl_matches_direct_definition():
    torch.manual_seed(31)
    teacher_logits = torch.randn(5, 17)
    student_logits = torch.randn(5, 17, requires_grad=True)
    teacher_log_probability = F.log_softmax(
        teacher_logits,
        dim=-1,
    )
    teacher_probability = teacher_log_probability.exp()
    teacher_entropy = -(teacher_probability * teacher_log_probability).sum(-1)

    actual = exact_kl_rows(
        student_logits,
        teacher_probability,
        teacher_entropy,
    )
    expected = (
        teacher_probability
        * (teacher_log_probability - F.log_softmax(student_logits, dim=-1))
    ).sum(-1)

    torch.testing.assert_close(actual, expected, rtol=1e-6, atol=2e-7)
    actual.mean().backward()
    assert torch.isfinite(student_logits.grad).all()


def test_chunked_tied_head_kl_matches_dense_value_and_gradients():
    torch.manual_seed(37)
    hidden_dense = torch.randn(7, 5, requires_grad=True)
    weight_dense = torch.randn(19, 5, requires_grad=True)
    teacher_log_probability = torch.log_softmax(
        torch.randn(7, 19),
        dim=-1,
    )
    teacher_probability = teacher_log_probability.exp()
    teacher_entropy = -(
        teacher_probability * teacher_log_probability
    ).sum(dim=-1)

    dense = exact_kl_rows(
        torch.nn.functional.linear(hidden_dense, weight_dense),
        teacher_probability,
        teacher_entropy,
    ).mean()
    dense.backward()
    dense_hidden_gradient = hidden_dense.grad.detach().clone()
    dense_weight_gradient = weight_dense.grad.detach().clone()

    hidden_chunked = hidden_dense.detach().clone().requires_grad_(True)
    weight_chunked = weight_dense.detach().clone().requires_grad_(True)
    chunked = chunked_linear_forward_kl(
        hidden_chunked,
        weight_chunked,
        teacher_probability,
        teacher_entropy,
        chunk_size=6,
    )
    chunked.backward()

    torch.testing.assert_close(chunked, dense, rtol=2e-6, atol=2e-6)
    torch.testing.assert_close(
        hidden_chunked.grad,
        dense_hidden_gradient,
        rtol=3e-6,
        atol=3e-6,
    )
    torch.testing.assert_close(
        weight_chunked.grad,
        dense_weight_gradient,
        rtol=3e-6,
        atol=3e-6,
    )


def test_materialized_probability_kl_matches_dense_value_and_gradients():
    torch.manual_seed(41)
    hidden = torch.randn(6, 7, requires_grad=True)
    weight = torch.randn(23, 7, requires_grad=True)
    teacher_log_probability = torch.log_softmax(
        torch.randn(6, 23),
        dim=-1,
    )
    teacher_probability = teacher_log_probability.exp()
    teacher_entropy = -(
        teacher_probability * teacher_log_probability
    ).sum(dim=-1)
    dense = exact_kl_rows(
        torch.nn.functional.linear(hidden, weight),
        teacher_probability,
        teacher_entropy,
    ).mean()
    dense_gradients = torch.autograd.grad(dense, (hidden, weight))

    optimized_hidden = hidden.detach().clone().requires_grad_(True)
    optimized_weight = weight.detach().clone().requires_grad_(True)
    optimized = materialized_probability_linear_forward_kl(
        optimized_hidden,
        optimized_weight,
        teacher_probability,
        teacher_entropy,
        chunk_size=8,
    )
    optimized_gradients = torch.autograd.grad(
        optimized,
        (optimized_hidden, optimized_weight),
    )

    torch.testing.assert_close(optimized, dense, rtol=2e-6, atol=2e-6)
    for actual, expected in zip(
        optimized_gradients,
        dense_gradients,
        strict=True,
    ):
        torch.testing.assert_close(actual, expected, rtol=3e-6, atol=3e-6)


def test_factorized_vocabulary_kl_is_exact_over_full_product():
    torch.manual_seed(43)
    hidden = torch.randn(5, 7, requires_grad=True)
    factor0 = torch.randn(3, 7, requires_grad=True)
    factor1 = torch.randn(4, 7, requires_grad=True)
    teacher_log_probability = torch.log_softmax(
        torch.randn(5, 12),
        dim=-1,
    )
    teacher_probability = teacher_log_probability.exp()
    teacher_matrix = teacher_probability.reshape(5, 3, 4)
    teacher_entropy = -(
        teacher_probability * teacher_log_probability
    ).sum(dim=-1)

    optimized = factorized_vocabulary_forward_kl(
        hidden,
        factor0,
        factor1,
        teacher_matrix.sum(dim=2),
        teacher_matrix.sum(dim=1),
        teacher_entropy,
    )
    logits = (
        (hidden @ factor0.mT)[:, :, None]
        + (hidden @ factor1.mT)[:, None, :]
    ).reshape(5, 12)
    dense = exact_kl_rows(
        logits,
        teacher_probability,
        teacher_entropy,
    ).mean()

    torch.testing.assert_close(optimized, dense, rtol=2e-6, atol=2e-6)


def test_khatri_rao_kl_matches_explicit_tied_tensor_and_gradients():
    torch.manual_seed(45)
    hidden = torch.randn(5, 7, requires_grad=True)
    factor0 = torch.randn(3, 7, requires_grad=True)
    factor1 = torch.randn(4, 7, requires_grad=True)
    teacher_log_probability = torch.log_softmax(torch.randn(5, 12), dim=-1)
    teacher_probability = teacher_log_probability.exp()
    teacher_entropy = -(
        teacher_probability * teacher_log_probability
    ).sum(dim=-1)
    optimized = khatri_rao_forward_kl(
        hidden,
        factor0,
        factor1,
        teacher_probability,
        teacher_entropy,
    )
    optimized_gradients = torch.autograd.grad(
        optimized,
        (hidden, factor0, factor1),
    )

    explicit_hidden = hidden.detach().clone().requires_grad_(True)
    explicit_factor0 = factor0.detach().clone().requires_grad_(True)
    explicit_factor1 = factor1.detach().clone().requires_grad_(True)
    explicit_logits = (
        explicit_hidden[:, None, None, :]
        * explicit_factor0[None, :, None, :]
        * explicit_factor1[None, None, :, :]
    ).sum(dim=-1).reshape(5, 12)
    dense = exact_kl_rows(
        explicit_logits,
        teacher_probability,
        teacher_entropy,
    ).mean()
    dense_gradients = torch.autograd.grad(
        dense,
        (explicit_hidden, explicit_factor0, explicit_factor1),
    )

    torch.testing.assert_close(
        khatri_rao_logits(hidden, factor0, factor1),
        explicit_logits,
    )
    torch.testing.assert_close(optimized, dense)
    for actual, expected in zip(
        optimized_gradients,
        dense_gradients,
        strict=True,
    ):
        torch.testing.assert_close(actual, expected)


def test_streamed_teacher_marginals_match_full_softmax():
    torch.manual_seed(47)
    hidden = torch.randn(4, 7)
    weight = torch.randn(15, 7)
    logits = torch.nn.functional.linear(hidden, weight).float()
    probability = torch.softmax(logits, dim=-1)
    matrix = probability.reshape(4, 3, 5)
    log_probability = torch.log_softmax(logits, dim=-1)
    expected_entropy = -(probability * log_probability).sum(dim=-1)

    marginal0, marginal1, entropy = teacher_marginals_from_hidden(
        hidden,
        weight,
        vocab_modes=(3, 5),
        rows_per_chunk=2,
    )

    torch.testing.assert_close(
        marginal0,
        matrix.sum(dim=2),
        rtol=2e-6,
        atol=2e-6,
    )
    torch.testing.assert_close(
        marginal1,
        matrix.sum(dim=1),
        rtol=2e-6,
        atol=2e-6,
    )
    torch.testing.assert_close(
        entropy,
        expected_entropy,
        rtol=2e-6,
        atol=2e-6,
    )


def test_distribution_metrics_use_exact_full_vocabulary_rows():
    teacher_log_probability = torch.log_softmax(
        torch.tensor([[1.0, 2.0, -1.0], [0.0, 3.0, 1.0]]),
        dim=-1,
    )
    teacher_probability = teacher_log_probability.exp()
    entropy = -(teacher_probability * teacher_log_probability).sum(-1)
    student_logits = torch.tensor([[2.0, 0.0, -1.0], [0.0, 1.0, 4.0]])

    metrics = distribution_metrics(
        student_logits,
        teacher_probability,
        teacher_log_probability,
        entropy,
        torch.tensor([0, 1]),
    )

    assert set(metrics) == {
        "kl",
        "student_nll",
        "teacher_nll",
        "accuracy",
    }
    assert metrics["accuracy"].tolist() == [1.0, 0.0]
    assert torch.all(metrics["kl"] >= -1e-6)


def test_probability_validation_rejects_invalid_rows():
    validate_probability_rows(torch.tensor([[0.25, 0.75]]))
    with pytest.raises(RuntimeError, match="not normalized"):
        validate_probability_rows(torch.tensor([[0.25, 0.5]]))
    with pytest.raises(RuntimeError, match="non-finite"):
        validate_probability_rows(torch.tensor([[float("nan"), 1.0]]))


def test_live_teacher_probabilities_can_participate_in_student_backward():
    class FakeTeacher(torch.nn.Module):
        def __init__(self):
            super().__init__()
            self.embedding = torch.nn.Embedding(11, 7)
            self.model = self

        def forward(self, *, input_ids, use_cache, return_dict):
            assert not use_cache
            assert return_dict
            return SimpleNamespace(
                last_hidden_state=self.embedding(input_ids),
            )

        def get_output_embeddings(self):
            return self.embedding

    teacher = FakeTeacher()
    probability, _, entropy = teacher_distribution(
        teacher,
        torch.randint(0, 11, (3, 16)),
        vocab_size=11,
    )
    student_logits = torch.randn(3, 11, requires_grad=True)

    exact_kl_rows(student_logits, probability, entropy).mean().backward()

    assert not probability.is_inference()
    assert torch.isfinite(student_logits.grad).all()
