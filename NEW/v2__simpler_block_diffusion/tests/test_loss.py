from __future__ import annotations

import torch
import pytest

from v2_simpler_block_diffusion.loss import (
    DistillationLoss,
    ExactFullKLLoss,
    HardCrossEntropyLoss,
    streaming_topk,
)


def test_streaming_topk_matches_materialized_logits() -> None:
    torch.manual_seed(2)
    hidden = torch.randn(7, 5)
    weight = torch.randn(31, 5)
    targets = streaming_topk(hidden, weight, topk=4, vocab_chunk_size=7, excluded_token_id=30)
    logits = hidden @ weight.T
    logits[:, 30] = -torch.inf
    log_probs = logits.log_softmax(-1)
    values, ids = log_probs.topk(4, dim=-1)
    assert torch.equal(targets.top_ids, ids)
    assert torch.allclose(targets.top_log_probs, values, atol=1e-6)
    assert torch.allclose(targets.tail_prob, 1 - values.exp().sum(-1), atol=1e-6)


def test_distillation_loss_is_finite_and_differentiable() -> None:
    torch.manual_seed(4)
    teacher_hidden = torch.randn(6, 8)
    student_hidden = torch.randn(6, 8, requires_grad=True)
    teacher_weight = torch.randn(40, 8)
    student_weight = torch.randn(40, 8, requires_grad=True)
    targets = streaming_topk(teacher_hidden, teacher_weight, topk=5, vocab_chunk_size=11)
    output = DistillationLoss(vocab_chunk_size=9, excluded_token_id=39)(
        student_hidden,
        student_weight,
        targets,
        torch.arange(6),
        torch.tensor([0, 0, 1, 1, 2, 2]),
    )
    output.loss.backward()
    assert bool(torch.isfinite(output.loss))
    assert student_hidden.grad is not None and bool(torch.isfinite(student_hidden.grad).all())
    assert student_weight.grad is not None and bool(torch.isfinite(student_weight.grad).all())


def test_custom_backward_matches_materialized_grouped_loss() -> None:
    torch.manual_seed(12)
    teacher_hidden = torch.randn(4, 6)
    teacher_weight = torch.randn(19, 6)
    targets = streaming_topk(
        teacher_hidden, teacher_weight, topk=3, vocab_chunk_size=5, excluded_token_id=18
    )
    hidden = torch.randn(4, 6, requires_grad=True)
    weight = torch.randn(19, 6, requires_grad=True)
    hard = torch.tensor([1, 3, 5, 7])
    groups = torch.tensor([0, 0, 1, 2])
    custom = DistillationLoss(vocab_chunk_size=4, excluded_token_id=18)(
        hidden, weight, targets, hard, groups
    ).loss
    custom.backward()
    custom_hidden_grad = hidden.grad.clone()
    custom_weight_grad = weight.grad.clone()

    reference_hidden = hidden.detach().clone().requires_grad_(True)
    reference_weight = weight.detach().clone().requires_grad_(True)
    logits = reference_hidden @ reference_weight.T
    logits[:, 18] = -torch.inf
    log_probs = logits.log_softmax(-1)
    student_top_log = log_probs.gather(1, targets.top_ids)
    student_tail_log = torch.log1p(-student_top_log.exp().sum(-1))
    position_kl = (targets.top_probs * (targets.top_log_probs - student_top_log)).sum(-1)
    position_kl += targets.tail_prob * (targets.tail_log_prob - student_tail_log)
    position_nll = -log_probs.gather(1, hard[:, None]).squeeze(1)
    weights = torch.tensor([1 / 6, 1 / 6, 1 / 3, 1 / 3])
    reference = (weights * (0.8 * position_kl + 0.2 * position_nll)).sum()
    reference.backward()
    assert torch.allclose(custom, reference, atol=2e-6)
    assert torch.allclose(custom_hidden_grad, reference_hidden.grad, atol=2e-5, rtol=2e-5)
    assert torch.allclose(custom_weight_grad, reference_weight.grad, atol=2e-5, rtol=2e-5)


def test_ce80_corrective_objective_is_finite() -> None:
    torch.manual_seed(21)
    teacher_hidden = torch.randn(5, 6)
    teacher_weight = torch.randn(23, 6)
    targets = streaming_topk(
        teacher_hidden, teacher_weight, topk=4, vocab_chunk_size=7, excluded_token_id=22
    )
    hidden = torch.randn(5, 6, requires_grad=True)
    weight = torch.randn(23, 6, requires_grad=True)
    output = DistillationLoss(
        kd_weight=0.2,
        hard_weight=0.8,
        vocab_chunk_size=6,
        excluded_token_id=22,
    )(
        hidden,
        weight,
        targets,
        torch.tensor([1, 2, 3, 4, 5]),
        torch.tensor([0, 0, 1, 1, 2]),
    )
    output.loss.backward()
    assert bool(torch.isfinite(output.loss))
    assert hidden.grad is not None and bool(torch.isfinite(hidden.grad).all())
    assert weight.grad is not None and bool(torch.isfinite(weight.grad).all())


def test_exact_hard_ce_matches_materialized_loss_and_gradients() -> None:
    torch.manual_seed(29)
    hidden = torch.randn(5, 7, requires_grad=True)
    weight = torch.randn(17, 7, requires_grad=True)
    labels = torch.tensor([1, 2, 3, 4, 5])
    groups = torch.tensor([0, 0, 1, 2, 2])
    custom = HardCrossEntropyLoss(vocab_chunk_size=5, excluded_token_id=16)(
        hidden, weight, labels, groups
    ).loss
    custom.backward()
    custom_hidden_grad = hidden.grad.clone()
    custom_weight_grad = weight.grad.clone()

    reference_hidden = hidden.detach().clone().requires_grad_(True)
    reference_weight = weight.detach().clone().requires_grad_(True)
    logits = reference_hidden @ reference_weight.T
    logits[:, 16] = -torch.inf
    position_nll = -logits.log_softmax(-1).gather(1, labels[:, None]).squeeze(1)
    group_weights = torch.tensor([1 / 6, 1 / 6, 1 / 3, 1 / 6, 1 / 6])
    reference = (group_weights * position_nll).sum()
    reference.backward()
    assert torch.allclose(custom, reference, atol=2e-6)
    assert torch.allclose(custom_hidden_grad, reference_hidden.grad, atol=2e-5, rtol=2e-5)
    assert torch.allclose(custom_weight_grad, reference_weight.grad, atol=2e-5, rtol=2e-5)


def test_exact_full_kl_matches_materialized_loss_metrics_and_gradients() -> None:
    torch.manual_seed(31)
    rows, student_width, teacher_width, vocab = 7, 5, 9, 23
    excluded = 18
    student_hidden = torch.randn(rows, student_width, requires_grad=True)
    student_weight = torch.randn(vocab, student_width, requires_grad=True)
    teacher_hidden = torch.randn(rows, teacher_width)
    teacher_weight = torch.randn(vocab, teacher_width)
    hard_labels = torch.tensor([1, 2, 3, 4, 5, 6, 7])
    block_ids = torch.tensor([0, 0, 0, 1, 1, 2, 2])

    custom_hidden = student_hidden.detach().clone().requires_grad_(True)
    custom_weight = student_weight.detach().clone().requires_grad_(True)
    output = ExactFullKLLoss(
        position_chunk_size=3,
        vocab_chunk_size=6,
        excluded_token_id=excluded,
    )(
        custom_hidden,
        custom_weight,
        teacher_hidden,
        teacher_weight,
        hard_labels,
        block_ids,
    )
    output.loss.backward()

    student_logits = student_hidden @ student_weight.transpose(0, 1)
    teacher_logits = teacher_hidden @ teacher_weight.transpose(0, 1)
    student_logits = student_logits.clone()
    teacher_logits = teacher_logits.clone()
    student_logits[:, excluded] = -torch.inf
    teacher_logits[:, excluded] = -torch.inf
    teacher_log_probability = teacher_logits.log_softmax(-1)
    student_log_probability = student_logits.log_softmax(-1)
    teacher_probability = teacher_log_probability.exp()
    log_ratio = torch.where(
        teacher_probability > 0,
        teacher_log_probability - student_log_probability,
        torch.zeros_like(teacher_log_probability),
    )
    position_kl = (teacher_probability * log_ratio).sum(-1)
    counts = torch.tensor([3.0, 3.0, 3.0, 2.0, 2.0, 2.0, 2.0])
    weights = 1 / (3 * counts)
    reference_loss = (weights * position_kl).sum()
    reference_loss.backward()

    student_nll = (
        weights
        * -student_log_probability.gather(1, hard_labels[:, None]).squeeze(1)
    ).sum()
    teacher_nll = (
        weights
        * -teacher_log_probability.gather(1, hard_labels[:, None]).squeeze(1)
    ).sum()
    agreement = (student_logits.argmax(-1) == teacher_logits.argmax(-1)).float().mean()

    assert torch.allclose(output.loss, reference_loss, atol=2e-5, rtol=2e-5)
    assert torch.allclose(output.full_kl, reference_loss.detach(), atol=2e-5, rtol=2e-5)
    assert torch.allclose(output.student_hard_nll, student_nll, atol=2e-5, rtol=2e-5)
    assert torch.allclose(output.teacher_hard_nll, teacher_nll, atol=2e-5, rtol=2e-5)
    assert torch.allclose(output.top1_agreement, agreement)
    assert torch.allclose(custom_hidden.grad, student_hidden.grad, atol=2e-5, rtol=2e-5)
    assert torch.allclose(custom_weight.grad, student_weight.grad, atol=2e-5, rtol=2e-5)


def test_exact_full_kl_accepts_inference_teacher_and_frozen_student_head() -> None:
    torch.manual_seed(37)
    student_hidden = torch.randn(3, 4, requires_grad=True)
    student_weight = torch.randn(11, 4)
    with torch.inference_mode():
        inference_teacher_hidden = torch.randn(3, 6)
    teacher_hidden = inference_teacher_hidden.clone()
    teacher_weight = torch.randn(11, 6)
    output = ExactFullKLLoss(
        position_chunk_size=2, vocab_chunk_size=5, excluded_token_id=10
    )(
        student_hidden,
        student_weight,
        teacher_hidden,
        teacher_weight,
        torch.tensor([1, 2, 3]),
        torch.tensor([0, 0, 1]),
    )
    output.loss.backward()
    assert student_hidden.grad is not None
    assert student_weight.grad is None


def test_exact_full_kl_rejects_empty_rows_and_mask_labels() -> None:
    loss = ExactFullKLLoss(excluded_token_id=6)
    with torch.no_grad():
        teacher_weight = torch.randn(7, 3)
    with pytest.raises(ValueError):
        loss(
            torch.empty(0, 2),
            torch.randn(7, 2),
            torch.empty(0, 3),
            teacher_weight,
            torch.empty(0, dtype=torch.long),
            torch.empty(0, dtype=torch.long),
        )
    with pytest.raises(ValueError):
        loss(
            torch.randn(1, 2),
            torch.randn(7, 2),
            torch.randn(1, 3),
            teacher_weight,
            torch.tensor([6]),
            torch.tensor([0]),
        )
