from __future__ import annotations

from dataclasses import dataclass

import torch
import torch.nn.functional as F
from torch import nn


@dataclass
class GroupedTeacherTargets:
    top_ids: torch.Tensor
    top_log_probs: torch.Tensor
    tail_log_prob: torch.Tensor

    @property
    def top_probs(self) -> torch.Tensor:
        return self.top_log_probs.exp()

    @property
    def tail_prob(self) -> torch.Tensor:
        return self.tail_log_prob.exp()


@dataclass
class DistillationLossOutput:
    loss: torch.Tensor
    grouped_kl: torch.Tensor
    hard_nll: torch.Tensor
    top1_agreement: torch.Tensor


@dataclass
class HardCrossEntropyOutput:
    loss: torch.Tensor
    hard_nll: torch.Tensor
    hard_top1_accuracy: torch.Tensor


@dataclass
class FullKLDistillationOutput:
    loss: torch.Tensor
    full_kl: torch.Tensor
    student_hard_nll: torch.Tensor
    teacher_hard_nll: torch.Tensor
    top1_agreement: torch.Tensor


def _chunk_logits(
    hidden: torch.Tensor,
    weight: torch.Tensor,
    start: int,
    stop: int,
    bias: torch.Tensor | None,
) -> torch.Tensor:
    value = F.linear(hidden, weight[start:stop], None if bias is None else bias[start:stop])
    return value.float()


def streaming_topk(
    hidden: torch.Tensor,
    weight: torch.Tensor,
    *,
    bias: torch.Tensor | None = None,
    topk: int = 16,
    vocab_chunk_size: int = 8_192,
    excluded_token_id: int | None = None,
) -> GroupedTeacherTargets:
    """Compute exact top-k probabilities and exact grouped tail without full logits."""
    if hidden.ndim != 2 or weight.ndim != 2 or hidden.shape[1] != weight.shape[1]:
        raise ValueError("hidden/weight shapes do not describe a vocabulary projection")
    rows, vocab = hidden.shape[0], weight.shape[0]
    if not 0 < topk < vocab:
        raise ValueError("topk must be between zero and vocabulary size")
    log_z = torch.full((rows,), -torch.inf, device=hidden.device, dtype=torch.float32)
    best_values = torch.full((rows, topk), -torch.inf, device=hidden.device, dtype=torch.float32)
    best_ids = torch.zeros((rows, topk), device=hidden.device, dtype=torch.long)
    for start in range(0, vocab, vocab_chunk_size):
        stop = min(start + vocab_chunk_size, vocab)
        logits = _chunk_logits(hidden, weight, start, stop, bias)
        if excluded_token_id is not None and start <= excluded_token_id < stop:
            logits[:, excluded_token_id - start] = -torch.inf
        log_z = torch.logaddexp(log_z, torch.logsumexp(logits, dim=-1))
        local_k = min(topk, stop - start)
        values, ids = logits.topk(local_k, dim=-1)
        ids = ids + start
        merged_values = torch.cat((best_values, values), dim=-1)
        merged_ids = torch.cat((best_ids, ids), dim=-1)
        best_values, order = merged_values.topk(topk, dim=-1)
        best_ids = merged_ids.gather(1, order)
    top_log_probs = best_values - log_z[:, None]
    top_mass = top_log_probs.exp().sum(-1).clamp(max=1 - torch.finfo(torch.float32).eps)
    tail_log_prob = torch.log1p(-top_mass)
    return GroupedTeacherTargets(best_ids, top_log_probs, tail_log_prob)


def streaming_logsumexp(
    hidden: torch.Tensor,
    weight: torch.Tensor,
    *,
    bias: torch.Tensor | None = None,
    vocab_chunk_size: int = 8_192,
    excluded_token_id: int | None = None,
) -> torch.Tensor:
    log_z = torch.full((hidden.shape[0],), -torch.inf, device=hidden.device, dtype=torch.float32)
    for start in range(0, weight.shape[0], vocab_chunk_size):
        stop = min(start + vocab_chunk_size, weight.shape[0])
        logits = _chunk_logits(hidden, weight, start, stop, bias)
        if excluded_token_id is not None and start <= excluded_token_id < stop:
            logits[:, excluded_token_id - start] = -torch.inf
        log_z = torch.logaddexp(log_z, torch.logsumexp(logits, dim=-1))
    return log_z


def _selected_logits(hidden: torch.Tensor, weight: torch.Tensor, ids: torch.Tensor) -> torch.Tensor:
    selected_weight = F.embedding(ids, weight)
    if ids.ndim == 1:
        return (hidden * selected_weight).sum(-1).float()
    return torch.einsum("nd,nkd->nk", hidden, selected_weight).float()


def _mean_by_group(values: torch.Tensor, group_ids: torch.Tensor) -> torch.Tensor:
    if values.numel() == 0:
        raise ValueError("distillation batch contains no supervised positions")
    _, inverse = torch.unique(group_ids, sorted=False, return_inverse=True)
    sums = torch.zeros(int(inverse.max().item()) + 1, device=values.device, dtype=values.dtype)
    counts = torch.zeros_like(sums)
    sums.scatter_add_(0, inverse, values)
    counts.scatter_add_(0, inverse, torch.ones_like(values))
    return (sums / counts).mean()


def _position_group_weights(group_ids: torch.Tensor) -> torch.Tensor:
    _, inverse = torch.unique(group_ids, sorted=False, return_inverse=True)
    counts = torch.bincount(inverse).to(torch.float32)
    return (counts.numel() * counts.index_select(0, inverse)).reciprocal()


def _safe_weighted_value_sum(
    weights: torch.Tensor, values: torch.Tensor
) -> torch.Tensor:
    """Avoid the undefined zero-times-negative-infinity product."""
    return (weights * torch.where(torch.isfinite(values), values, torch.zeros_like(values))).sum(-1)


class _ExactFullVocabularyDistillation(torch.autograd.Function):
    """Exact forward KL with streamed teacher and student vocabulary heads.

    Teacher probabilities are reconstructed from frozen hidden states during
    backward.  Only per-row normalizers are retained, so memory is bounded by
    the configured position/vocabulary chunks rather than rows times vocab.
    """

    @staticmethod
    def forward(
        ctx,
        student_hidden: torch.Tensor,
        student_weight: torch.Tensor,
        teacher_hidden: torch.Tensor,
        teacher_weight: torch.Tensor,
        hard_labels: torch.Tensor,
        position_weights: torch.Tensor,
        position_chunk_size: int,
        vocab_chunk_size: int,
        excluded_token_id: int,
    ) -> tuple[torch.Tensor, torch.Tensor, torch.Tensor, torch.Tensor, torch.Tensor]:
        rows, vocab = student_hidden.shape[0], student_weight.shape[0]
        if student_hidden.ndim != 2 or teacher_hidden.ndim != 2:
            raise ValueError("student and teacher hidden states must be matrices")
        if student_weight.ndim != 2 or teacher_weight.ndim != 2:
            raise ValueError("student and teacher vocabulary weights must be matrices")
        if teacher_hidden.shape[0] != rows or teacher_weight.shape[0] != vocab:
            raise ValueError("teacher and student rows/vocabulary must agree")
        if student_hidden.shape[1] != student_weight.shape[1]:
            raise ValueError("student hidden and vocabulary widths disagree")
        if teacher_hidden.shape[1] != teacher_weight.shape[1]:
            raise ValueError("teacher hidden and vocabulary widths disagree")
        if hard_labels.shape != (rows,) or position_weights.shape != (rows,):
            raise ValueError("one hard label and position weight are required per row")
        if not 0 <= excluded_token_id < vocab:
            raise ValueError("excluded token ID is outside the vocabulary")
        if position_chunk_size <= 0 or vocab_chunk_size <= 0:
            raise ValueError("streaming chunk sizes must be positive")

        teacher_log_z = torch.empty(rows, device=student_hidden.device, dtype=torch.float32)
        student_log_z = torch.empty_like(teacher_log_z)
        position_kl = torch.empty_like(teacher_log_z)
        teacher_best_id = torch.zeros(rows, device=student_hidden.device, dtype=torch.long)
        student_best_id = torch.zeros_like(teacher_best_id)

        for row_start in range(0, rows, position_chunk_size):
            row_stop = min(row_start + position_chunk_size, rows)
            sh = student_hidden[row_start:row_stop]
            th = teacher_hidden[row_start:row_stop]
            count = row_stop - row_start
            teacher_max = torch.full(
                (count,), -torch.inf, device=student_hidden.device, dtype=torch.float32
            )
            teacher_sum = torch.zeros_like(teacher_max)
            teacher_logit_moment = torch.zeros_like(teacher_max)
            student_logit_moment = torch.zeros_like(teacher_max)
            local_student_log_z = torch.full_like(teacher_max, -torch.inf)
            local_teacher_best = torch.full_like(teacher_max, -torch.inf)
            local_student_best = torch.full_like(teacher_max, -torch.inf)
            local_teacher_id = torch.zeros(count, device=student_hidden.device, dtype=torch.long)
            local_student_id = torch.zeros_like(local_teacher_id)

            for vocab_start in range(0, vocab, vocab_chunk_size):
                vocab_stop = min(vocab_start + vocab_chunk_size, vocab)
                teacher_logits = _chunk_logits(
                    th, teacher_weight, vocab_start, vocab_stop, None
                )
                student_logits = _chunk_logits(
                    sh, student_weight, vocab_start, vocab_stop, None
                )
                if vocab_start <= excluded_token_id < vocab_stop:
                    local_id = excluded_token_id - vocab_start
                    teacher_logits[:, local_id] = -torch.inf
                    student_logits[:, local_id] = -torch.inf

                local_student_log_z = torch.logaddexp(
                    local_student_log_z, torch.logsumexp(student_logits, dim=-1)
                )
                chunk_max = teacher_logits.max(-1).values
                new_max = torch.maximum(teacher_max, chunk_max)
                prior_scale = torch.where(
                    torch.isfinite(teacher_max), (teacher_max - new_max).exp(), torch.zeros_like(new_max)
                )
                teacher_probability_scale = (teacher_logits - new_max[:, None]).exp()
                teacher_sum = teacher_sum * prior_scale + teacher_probability_scale.sum(-1)
                teacher_logit_moment = (
                    teacher_logit_moment * prior_scale
                    + _safe_weighted_value_sum(teacher_probability_scale, teacher_logits)
                )
                student_logit_moment = (
                    student_logit_moment * prior_scale
                    + _safe_weighted_value_sum(teacher_probability_scale, student_logits)
                )
                teacher_max = new_max

                chunk_teacher_value, chunk_teacher_id = teacher_logits.max(-1)
                replace_teacher = chunk_teacher_value > local_teacher_best
                local_teacher_best = torch.where(
                    replace_teacher, chunk_teacher_value, local_teacher_best
                )
                local_teacher_id = torch.where(
                    replace_teacher, chunk_teacher_id + vocab_start, local_teacher_id
                )
                chunk_student_value, chunk_student_id = student_logits.max(-1)
                replace_student = chunk_student_value > local_student_best
                local_student_best = torch.where(
                    replace_student, chunk_student_value, local_student_best
                )
                local_student_id = torch.where(
                    replace_student, chunk_student_id + vocab_start, local_student_id
                )

            local_teacher_log_z = teacher_max + teacher_sum.log()
            expected_teacher_logit = teacher_logit_moment / teacher_sum
            expected_student_logit = student_logit_moment / teacher_sum
            teacher_log_z[row_start:row_stop] = local_teacher_log_z
            student_log_z[row_start:row_stop] = local_student_log_z
            position_kl[row_start:row_stop] = (
                local_student_log_z
                - local_teacher_log_z
                + expected_teacher_logit
                - expected_student_logit
            )
            teacher_best_id[row_start:row_stop] = local_teacher_id
            student_best_id[row_start:row_stop] = local_student_id

        student_hard = student_log_z - _selected_logits(
            student_hidden, student_weight, hard_labels
        )
        teacher_hard = teacher_log_z - _selected_logits(
            teacher_hidden, teacher_weight, hard_labels
        )
        loss = (position_weights * position_kl).sum()
        student_hard_nll = (position_weights * student_hard).sum()
        teacher_hard_nll = (position_weights * teacher_hard).sum()
        agreement = (student_best_id == teacher_best_id).float().mean()
        ctx.save_for_backward(
            student_hidden,
            student_weight,
            teacher_hidden,
            teacher_weight,
            position_weights,
            student_log_z,
            teacher_log_z,
        )
        ctx.position_chunk_size = int(position_chunk_size)
        ctx.vocab_chunk_size = int(vocab_chunk_size)
        ctx.excluded_token_id = int(excluded_token_id)
        full_kl = loss.detach()
        ctx.mark_non_differentiable(
            full_kl, student_hard_nll, teacher_hard_nll, agreement
        )
        return loss, full_kl, student_hard_nll, teacher_hard_nll, agreement

    @staticmethod
    def backward(
        ctx,
        grad_loss: torch.Tensor,
        _grad_kl: torch.Tensor,
        _grad_student_nll: torch.Tensor,
        _grad_teacher_nll: torch.Tensor,
        _grad_agreement: torch.Tensor,
    ):
        (
            student_hidden,
            student_weight,
            teacher_hidden,
            teacher_weight,
            position_weights,
            student_log_z,
            teacher_log_z,
        ) = ctx.saved_tensors
        rows, vocab = student_hidden.shape[0], student_weight.shape[0]
        need_hidden, need_weight = ctx.needs_input_grad[:2]
        grad_hidden = (
            torch.zeros_like(student_hidden, dtype=torch.float32) if need_hidden else None
        )
        grad_weight = (
            torch.zeros_like(student_weight, dtype=torch.float32) if need_weight else None
        )
        for row_start in range(0, rows, ctx.position_chunk_size):
            row_stop = min(row_start + ctx.position_chunk_size, rows)
            sh = student_hidden[row_start:row_stop]
            th = teacher_hidden[row_start:row_stop]
            row_scale = position_weights[row_start:row_stop] * grad_loss.float()
            for vocab_start in range(0, vocab, ctx.vocab_chunk_size):
                vocab_stop = min(vocab_start + ctx.vocab_chunk_size, vocab)
                student_logits = _chunk_logits(
                    sh, student_weight, vocab_start, vocab_stop, None
                )
                teacher_logits = _chunk_logits(
                    th, teacher_weight, vocab_start, vocab_stop, None
                )
                if vocab_start <= ctx.excluded_token_id < vocab_stop:
                    local_id = ctx.excluded_token_id - vocab_start
                    student_logits[:, local_id] = -torch.inf
                    teacher_logits[:, local_id] = -torch.inf
                student_probability = (
                    student_logits - student_log_z[row_start:row_stop, None]
                ).exp()
                teacher_probability = (
                    teacher_logits - teacher_log_z[row_start:row_stop, None]
                ).exp()
                delta = (student_probability - teacher_probability) * row_scale[:, None]
                if vocab_start <= ctx.excluded_token_id < vocab_stop:
                    delta[:, ctx.excluded_token_id - vocab_start] = 0
                if grad_hidden is not None:
                    grad_hidden[row_start:row_stop].add_(
                        delta @ student_weight[vocab_start:vocab_stop].float()
                    )
                if grad_weight is not None:
                    grad_weight[vocab_start:vocab_stop].add_(
                        delta.transpose(0, 1) @ sh.float()
                    )
        return (
            None if grad_hidden is None else grad_hidden.to(student_hidden.dtype),
            None if grad_weight is None else grad_weight.to(student_weight.dtype),
            None,
            None,
            None,
            None,
            None,
            None,
            None,
        )


class ExactFullKLLoss(nn.Module):
    def __init__(
        self,
        *,
        position_chunk_size: int = 512,
        vocab_chunk_size: int = 8_192,
        excluded_token_id: int | None = None,
    ) -> None:
        super().__init__()
        self.position_chunk_size = int(position_chunk_size)
        self.vocab_chunk_size = int(vocab_chunk_size)
        self.excluded_token_id = excluded_token_id

    def forward(
        self,
        student_hidden: torch.Tensor,
        student_weight: torch.Tensor,
        teacher_hidden: torch.Tensor,
        teacher_weight: torch.Tensor,
        hard_labels: torch.Tensor,
        block_ids: torch.Tensor,
    ) -> FullKLDistillationOutput:
        if self.excluded_token_id is None:
            raise ValueError("exact full KL requires an excluded mask token ID")
        if student_hidden.shape[0] == 0:
            raise ValueError("exact full KL requires at least one supervised position")
        if bool((hard_labels == self.excluded_token_id).any()):
            raise ValueError("hard labels cannot contain the excluded mask token")
        position_weights = _position_group_weights(block_ids).to(student_hidden.device)
        loss, full_kl, student_nll, teacher_nll, agreement = (
            _ExactFullVocabularyDistillation.apply(
                student_hidden,
                student_weight,
                teacher_hidden,
                teacher_weight,
                hard_labels,
                position_weights,
                self.position_chunk_size,
                self.vocab_chunk_size,
                self.excluded_token_id,
            )
        )
        return FullKLDistillationOutput(
            loss,
            full_kl.detach(),
            student_nll.detach(),
            teacher_nll.detach(),
            agreement.detach(),
        )


class _ExactGroupedDistillation(torch.autograd.Function):
    """Memory-bounded exact grouped KL + CE with vocabulary recomputation."""

    @staticmethod
    def forward(
        ctx,
        hidden: torch.Tensor,
        weight: torch.Tensor,
        top_ids: torch.Tensor,
        top_log_probs: torch.Tensor,
        tail_log_prob: torch.Tensor,
        hard_labels: torch.Tensor,
        position_weights: torch.Tensor,
        kd_weight: float,
        hard_weight: float,
        chunk_size: int,
        excluded_token_id: int,
    ) -> tuple[torch.Tensor, torch.Tensor, torch.Tensor, torch.Tensor]:
        rows, vocab = hidden.shape[0], weight.shape[0]
        log_z = torch.full((rows,), -torch.inf, device=hidden.device, dtype=torch.float32)
        best_value = torch.full_like(log_z, -torch.inf)
        best_id = torch.zeros(rows, device=hidden.device, dtype=torch.long)
        for start in range(0, vocab, chunk_size):
            stop = min(start + chunk_size, vocab)
            logits = _chunk_logits(hidden, weight, start, stop, None)
            if start <= excluded_token_id < stop:
                logits[:, excluded_token_id - start] = -torch.inf
            log_z = torch.logaddexp(log_z, torch.logsumexp(logits, dim=-1))
            local_value, local_id = logits.max(-1)
            replace = local_value > best_value
            best_value = torch.where(replace, local_value, best_value)
            best_id = torch.where(replace, local_id + start, best_id)
        student_top_log = _selected_logits(hidden, weight, top_ids) - log_z[:, None]
        student_top_mass = student_top_log.exp().sum(-1).clamp(
            max=1 - torch.finfo(torch.float32).eps
        )
        student_tail_log = torch.log1p(-student_top_mass)
        teacher_top = top_log_probs.exp()
        teacher_tail = tail_log_prob.exp()
        position_kl = (teacher_top * (top_log_probs - student_top_log)).sum(-1)
        position_kl += teacher_tail * (tail_log_prob - student_tail_log)
        position_nll = log_z - _selected_logits(hidden, weight, hard_labels)
        position_loss = kd_weight * position_kl + hard_weight * position_nll
        loss = (position_weights * position_loss).sum()
        grouped_kl = (position_weights * position_kl).sum()
        hard_nll = (position_weights * position_nll).sum()
        agreement = (best_id == top_ids[:, 0]).float().mean()
        ctx.save_for_backward(
            hidden,
            weight,
            top_ids,
            teacher_top,
            teacher_tail,
            hard_labels,
            position_weights,
            log_z,
            student_tail_log.exp(),
        )
        ctx.kd_weight = kd_weight
        ctx.hard_weight = hard_weight
        ctx.chunk_size = chunk_size
        ctx.excluded_token_id = excluded_token_id
        return loss, grouped_kl, hard_nll, agreement

    @staticmethod
    def backward(ctx, grad_loss, _grad_kl, _grad_nll, _grad_agreement):
        (
            hidden,
            weight,
            top_ids,
            teacher_top,
            teacher_tail,
            hard_labels,
            position_weights,
            log_z,
            student_tail,
        ) = ctx.saved_tensors
        grad_hidden = torch.zeros_like(hidden, dtype=torch.float32)
        grad_weight = torch.zeros_like(weight)
        row_scale = position_weights * grad_loss.float()
        tail_factor = 1 - teacher_tail / student_tail.clamp_min(torch.finfo(torch.float32).tiny)
        for start in range(0, weight.shape[0], ctx.chunk_size):
            stop = min(start + ctx.chunk_size, weight.shape[0])
            logits = _chunk_logits(hidden, weight, start, stop, None)
            if start <= ctx.excluded_token_id < stop:
                logits[:, ctx.excluded_token_id - start] = -torch.inf
            probability = (logits - log_z[:, None]).exp()
            grad_logits = ctx.kd_weight * probability * tail_factor[:, None]
            in_chunk = (top_ids >= start) & (top_ids < stop)
            rows, columns = torch.nonzero(in_chunk, as_tuple=True)
            if rows.numel():
                local_ids = top_ids[rows, columns] - start
                correction = ctx.kd_weight * (
                    probability[rows, local_ids] * (1 - tail_factor[rows])
                    - teacher_top[rows, columns]
                )
                grad_logits.index_put_((rows, local_ids), correction, accumulate=True)
            grad_logits += ctx.hard_weight * probability
            hard_rows = torch.nonzero(
                (hard_labels >= start) & (hard_labels < stop), as_tuple=False
            ).flatten()
            if hard_rows.numel():
                hard_local = hard_labels[hard_rows] - start
                grad_logits[hard_rows, hard_local] -= ctx.hard_weight
            if start <= ctx.excluded_token_id < stop:
                grad_logits[:, ctx.excluded_token_id - start] = 0
            grad_logits *= row_scale[:, None]
            grad_hidden += grad_logits @ weight[start:stop].float()
            grad_weight[start:stop] = (grad_logits.transpose(0, 1) @ hidden.float()).to(weight.dtype)
        return (
            grad_hidden.to(hidden.dtype),
            grad_weight,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
        )


class _ExactHardCrossEntropy(torch.autograd.Function):
    """Memory-bounded exact vocabulary CE with recomputation in backward."""

    @staticmethod
    def forward(
        ctx,
        hidden: torch.Tensor,
        weight: torch.Tensor,
        hard_labels: torch.Tensor,
        position_weights: torch.Tensor,
        chunk_size: int,
        excluded_token_id: int,
    ) -> tuple[torch.Tensor, torch.Tensor, torch.Tensor]:
        rows, vocab = hidden.shape[0], weight.shape[0]
        log_z = torch.full((rows,), -torch.inf, device=hidden.device, dtype=torch.float32)
        best_value = torch.full_like(log_z, -torch.inf)
        best_id = torch.zeros(rows, device=hidden.device, dtype=torch.long)
        for start in range(0, vocab, chunk_size):
            stop = min(start + chunk_size, vocab)
            logits = _chunk_logits(hidden, weight, start, stop, None)
            if start <= excluded_token_id < stop:
                logits[:, excluded_token_id - start] = -torch.inf
            log_z = torch.logaddexp(log_z, torch.logsumexp(logits, dim=-1))
            local_value, local_id = logits.max(-1)
            replace = local_value > best_value
            best_value = torch.where(replace, local_value, best_value)
            best_id = torch.where(replace, local_id + start, best_id)
        position_nll = log_z - _selected_logits(hidden, weight, hard_labels)
        loss = (position_weights * position_nll).sum()
        accuracy = (best_id == hard_labels).float().mean()
        ctx.save_for_backward(hidden, weight, hard_labels, position_weights, log_z)
        ctx.chunk_size = chunk_size
        ctx.excluded_token_id = excluded_token_id
        return loss, loss.detach(), accuracy

    @staticmethod
    def backward(ctx, grad_loss, _grad_nll, _grad_accuracy):
        hidden, weight, hard_labels, position_weights, log_z = ctx.saved_tensors
        grad_hidden = torch.zeros_like(hidden, dtype=torch.float32)
        grad_weight = torch.zeros_like(weight)
        row_scale = position_weights * grad_loss.float()
        for start in range(0, weight.shape[0], ctx.chunk_size):
            stop = min(start + ctx.chunk_size, weight.shape[0])
            logits = _chunk_logits(hidden, weight, start, stop, None)
            if start <= ctx.excluded_token_id < stop:
                logits[:, ctx.excluded_token_id - start] = -torch.inf
            grad_logits = (logits - log_z[:, None]).exp()
            hard_rows = torch.nonzero(
                (hard_labels >= start) & (hard_labels < stop), as_tuple=False
            ).flatten()
            if hard_rows.numel():
                local_ids = hard_labels[hard_rows] - start
                grad_logits[hard_rows, local_ids] -= 1
            if start <= ctx.excluded_token_id < stop:
                grad_logits[:, ctx.excluded_token_id - start] = 0
            grad_logits *= row_scale[:, None]
            grad_hidden += grad_logits @ weight[start:stop].float()
            grad_weight[start:stop] = (grad_logits.transpose(0, 1) @ hidden.float()).to(weight.dtype)
        return grad_hidden.to(hidden.dtype), grad_weight, None, None, None, None


class HardCrossEntropyLoss(nn.Module):
    def __init__(
        self,
        *,
        vocab_chunk_size: int = 8_192,
        excluded_token_id: int | None = None,
    ) -> None:
        super().__init__()
        self.vocab_chunk_size = vocab_chunk_size
        self.excluded_token_id = excluded_token_id

    def forward(
        self,
        student_hidden: torch.Tensor,
        output_weight: torch.Tensor,
        hard_labels: torch.Tensor,
        block_ids: torch.Tensor,
    ) -> HardCrossEntropyOutput:
        if student_hidden.shape[0] != hard_labels.numel():
            raise ValueError("one hard label is required per selected hidden state")
        if self.excluded_token_id is None:
            raise ValueError("exact hard CE requires an excluded mask token ID")
        position_weights = _position_group_weights(block_ids).to(student_hidden.device)
        loss, hard_nll, accuracy = _ExactHardCrossEntropy.apply(
            student_hidden,
            output_weight,
            hard_labels,
            position_weights,
            self.vocab_chunk_size,
            self.excluded_token_id,
        )
        return HardCrossEntropyOutput(loss, hard_nll.detach(), accuracy.detach())


class DistillationLoss(nn.Module):
    def __init__(
        self,
        *,
        kd_weight: float = 0.8,
        hard_weight: float = 0.2,
        vocab_chunk_size: int = 8_192,
        excluded_token_id: int | None = None,
    ) -> None:
        super().__init__()
        if abs(kd_weight + hard_weight - 1.0) > 1e-8:
            raise ValueError("distillation and hard weights must sum to one")
        self.kd_weight = kd_weight
        self.hard_weight = hard_weight
        self.vocab_chunk_size = vocab_chunk_size
        self.excluded_token_id = excluded_token_id

    def forward(
        self,
        student_hidden: torch.Tensor,
        output_weight: torch.Tensor,
        teacher: GroupedTeacherTargets,
        hard_labels: torch.Tensor,
        block_ids: torch.Tensor,
    ) -> DistillationLossOutput:
        if student_hidden.shape[0] != hard_labels.numel():
            raise ValueError("one hard label is required per selected hidden state")
        if self.excluded_token_id is None:
            raise ValueError("exact grouped loss requires an excluded mask token ID")
        position_weights = _position_group_weights(block_ids).to(student_hidden.device)
        loss, grouped_kl, hard_nll, agreement = _ExactGroupedDistillation.apply(
            student_hidden,
            output_weight,
            teacher.top_ids,
            teacher.top_log_probs,
            teacher.tail_log_prob,
            hard_labels,
            position_weights,
            self.kd_weight,
            self.hard_weight,
            self.vocab_chunk_size,
            self.excluded_token_id,
        )
        return DistillationLossOutput(loss, grouped_kl.detach(), hard_nll.detach(), agreement.detach())
