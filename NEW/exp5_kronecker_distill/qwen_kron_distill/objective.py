from __future__ import annotations

import math
import os

import torch
import torch.nn.functional as F

from .config import (
    MODEL_ID,
    MODEL_REVISION,
    TEACHER_MICROBATCH,
    TEACHER_VOCAB_ROWS_PER_CHUNK,
    VOCAB_CHUNK_SIZE,
    VOCAB_FACTOR_MODES,
    VOCAB_SIZE,
)


def load_teacher(device: str):
    from liger_kernel.transformers import apply_liger_kernel_to_qwen3_5
    from transformers import AutoModelForImageTextToText

    apply_liger_kernel_to_qwen3_5(
        rms_norm=True,
        swiglu=True,
        cross_entropy=False,
        fused_linear_cross_entropy=False,
    )
    token = (
        os.environ.get("HF_TOKEN")
        or os.environ.get("HF_HUB_TOKEN")
        or os.environ.get("HUGGING_FACE_HUB_TOKEN")
    )
    teacher = AutoModelForImageTextToText.from_pretrained(
        MODEL_ID,
        revision=MODEL_REVISION,
        token=token,
        dtype=torch.bfloat16,
        device_map=device,
        low_cpu_mem_usage=True,
    ).eval()
    teacher.requires_grad_(False)
    try:
        teacher.set_attn_implementation({"text_config": "sdpa"})
    except (AttributeError, TypeError, ValueError):
        pass
    input_weight = teacher.get_input_embeddings().weight
    output_weight = teacher.get_output_embeddings().weight
    if input_weight.data_ptr() != output_weight.data_ptr():
        raise RuntimeError("pinned Qwen input and output embeddings are not tied")
    return teacher


@torch.no_grad()
def teacher_distribution(
    teacher,
    token_ids: torch.Tensor,
    *,
    vocab_size: int = VOCAB_SIZE,
) -> tuple[torch.Tensor, torch.Tensor, torch.Tensor]:
    output = teacher.model(
        input_ids=token_ids,
        use_cache=False,
        return_dict=True,
    )
    hidden = output.last_hidden_state[:, -1, :]
    logits = F.linear(
        hidden,
        teacher.get_output_embeddings().weight[:vocab_size],
    )
    log_probability = F.log_softmax(logits.float(), dim=-1)
    probability = log_probability.exp()
    entropy = -(probability * log_probability).sum(-1)
    return probability, log_probability, entropy


@torch.no_grad()
def teacher_marginals_from_hidden(
    hidden: torch.Tensor,
    vocabulary_weight: torch.Tensor,
    *,
    vocab_modes: tuple[int, int] = VOCAB_FACTOR_MODES,
    rows_per_chunk: int = TEACHER_VOCAB_ROWS_PER_CHUNK,
) -> tuple[torch.Tensor, torch.Tensor, torch.Tensor]:
    """Stream exact softmax marginals and entropy without full logits."""

    if hidden.ndim != 2 or vocabulary_weight.ndim != 2:
        raise ValueError("teacher hidden and vocabulary weight must be matrices")
    if hidden.shape[1] != vocabulary_weight.shape[1]:
        raise ValueError("teacher hidden and vocabulary widths disagree")
    if math.prod(vocab_modes) != vocabulary_weight.shape[0]:
        raise ValueError("vocabulary modes must exactly cover the vocabulary")
    if rows_per_chunk <= 0:
        raise ValueError("teacher vocabulary row chunk must be positive")

    batch = hidden.shape[0]
    mode0, mode1 = vocab_modes
    running_max = torch.full(
        (batch,),
        -torch.inf,
        device=hidden.device,
        dtype=torch.float32,
    )
    running_sum = torch.zeros_like(running_max)
    running_weighted_logit = torch.zeros_like(running_max)
    marginal0 = torch.zeros(
        batch,
        mode0,
        device=hidden.device,
        dtype=torch.float32,
    )
    marginal1 = torch.zeros(
        batch,
        mode1,
        device=hidden.device,
        dtype=torch.float32,
    )

    for row_start in range(0, mode0, rows_per_chunk):
        row_stop = min(row_start + rows_per_chunk, mode0)
        start = row_start * mode1
        stop = row_stop * mode1
        logits = F.linear(
            hidden,
            vocabulary_weight[start:stop],
        ).float().reshape(
            batch,
            row_stop - row_start,
            mode1,
        )
        chunk_max = logits.amax(dim=(1, 2))
        next_max = torch.maximum(running_max, chunk_max)
        old_scale = torch.exp(running_max - next_max)
        running_sum.mul_(old_scale)
        running_weighted_logit.mul_(old_scale)
        marginal0.mul_(old_scale[:, None])
        marginal1.mul_(old_scale[:, None])

        exponential = torch.exp(
            logits - next_max[:, None, None]
        )
        running_sum.add_(exponential.sum(dim=(1, 2)))
        running_weighted_logit.add_(
            (exponential * logits).sum(dim=(1, 2))
        )
        marginal0[:, row_start:row_stop].add_(
            exponential.sum(dim=2)
        )
        marginal1.add_(exponential.sum(dim=1))
        running_max = next_max

    inverse_sum = running_sum.reciprocal()
    marginal0.mul_(inverse_sum[:, None])
    marginal1.mul_(inverse_sum[:, None])
    log_normalizer = running_max + running_sum.log()
    entropy = (
        log_normalizer
        - running_weighted_logit * inverse_sum
    )
    return marginal0, marginal1, entropy


@torch.no_grad()
def teacher_training_targets(
    teacher,
    token_ids: torch.Tensor,
    *,
    vocab_size: int = VOCAB_SIZE,
    microbatch: int = TEACHER_MICROBATCH,
    vocab_modes: tuple[int, int] = VOCAB_FACTOR_MODES,
    rows_per_chunk: int = TEACHER_VOCAB_ROWS_PER_CHUNK,
) -> tuple[torch.Tensor, torch.Tensor, torch.Tensor]:
    """Create exact teacher marginals while bounding all activation axes."""

    if microbatch <= 0:
        raise ValueError("teacher microbatch must be positive")
    if math.prod(vocab_modes) != vocab_size:
        raise ValueError("vocabulary modes must exactly cover the vocabulary")
    marginal0 = []
    marginal1 = []
    entropies = []
    vocabulary_weight = teacher.get_output_embeddings().weight[:vocab_size]
    for start in range(0, token_ids.shape[0], microbatch):
        output = teacher.model(
            input_ids=token_ids[start : start + microbatch],
            use_cache=False,
            return_dict=True,
        )
        hidden = output.last_hidden_state[:, -1, :]
        first, second, entropy = teacher_marginals_from_hidden(
            hidden,
            vocabulary_weight,
            vocab_modes=vocab_modes,
            rows_per_chunk=rows_per_chunk,
        )
        marginal0.append(first)
        marginal1.append(second)
        entropies.append(entropy)
    return (
        torch.cat(marginal0),
        torch.cat(marginal1),
        torch.cat(entropies),
    )


@torch.no_grad()
def teacher_probability_targets(
    teacher,
    token_ids: torch.Tensor,
    *,
    vocab_size: int = VOCAB_SIZE,
    microbatch: int = TEACHER_MICROBATCH,
) -> tuple[torch.Tensor, torch.Tensor]:
    """Materialize exact teacher probabilities on high-memory accelerators."""

    if microbatch <= 0:
        raise ValueError("teacher microbatch must be positive")
    probability = torch.empty(
        token_ids.shape[0],
        vocab_size,
        device=token_ids.device,
        dtype=torch.float32,
    )
    entropy = torch.empty(
        token_ids.shape[0],
        device=token_ids.device,
        dtype=torch.float32,
    )
    vocabulary_weight = teacher.get_output_embeddings().weight[:vocab_size]
    for start in range(0, token_ids.shape[0], microbatch):
        stop = min(start + microbatch, token_ids.shape[0])
        output = teacher.model(
            input_ids=token_ids[start:stop],
            use_cache=False,
            return_dict=True,
        )
        hidden = output.last_hidden_state[:, -1, :]
        logits = F.linear(hidden, vocabulary_weight)
        log_probability = F.log_softmax(logits.float(), dim=-1)
        chunk_probability = log_probability.exp()
        probability[start:stop].copy_(chunk_probability)
        entropy[start:stop].copy_(
            -(chunk_probability * log_probability).sum(dim=-1)
        )
    return probability, entropy


def exact_kl_rows(
    student_logits: torch.Tensor,
    teacher_probability: torch.Tensor,
    teacher_entropy: torch.Tensor,
) -> torch.Tensor:
    if student_logits.shape != teacher_probability.shape:
        raise ValueError("teacher and student vocabulary logits disagree")
    if teacher_entropy.shape != student_logits.shape[:-1]:
        raise ValueError("teacher entropy has the wrong shape")
    student_log_probability = F.log_softmax(
        student_logits.float(),
        dim=-1,
    )
    soft_cross_entropy = -(teacher_probability * student_log_probability).sum(-1)
    return soft_cross_entropy - teacher_entropy


def khatri_rao_logits(
    hidden: torch.Tensor,
    factor0: torch.Tensor,
    factor1: torch.Tensor,
) -> torch.Tensor:
    """Apply a tied CP/Khatri-Rao vocabulary without a dense V×D matrix."""

    if hidden.ndim != 2 or factor0.ndim != 2 or factor1.ndim != 2:
        raise ValueError("hidden and vocabulary factors must be matrices")
    if hidden.shape[1] != factor0.shape[1] or hidden.shape[1] != factor1.shape[1]:
        raise ValueError("hidden and vocabulary factor widths disagree")
    left = hidden[:, None, :] * factor0[None, :, :]
    return torch.matmul(left, factor1.mT).reshape(
        hidden.shape[0],
        factor0.shape[0] * factor1.shape[0],
    )


def khatri_rao_forward_kl(
    hidden: torch.Tensor,
    factor0: torch.Tensor,
    factor1: torch.Tensor,
    teacher_probability: torch.Tensor,
    teacher_entropy: torch.Tensor,
) -> torch.Tensor:
    """Exact full-vocabulary KL for a tied multiplicative Kronecker head."""

    logits = khatri_rao_logits(hidden, factor0, factor1)
    return exact_kl_rows(
        logits,
        teacher_probability,
        teacher_entropy,
    ).mean()


def factorized_vocabulary_forward_kl(
    hidden: torch.Tensor,
    factor0: torch.Tensor,
    factor1: torch.Tensor,
    teacher_marginal0: torch.Tensor,
    teacher_marginal1: torch.Tensor,
    teacher_entropy: torch.Tensor,
) -> torch.Tensor:
    """Exact KL to a full q0⊗q1 vocabulary distribution."""

    logits0 = F.linear(hidden, factor0).float()
    logits1 = F.linear(hidden, factor1).float()
    if tuple(teacher_marginal0.shape) != tuple(logits0.shape):
        raise ValueError("first teacher vocabulary marginal has the wrong shape")
    if tuple(teacher_marginal1.shape) != tuple(logits1.shape):
        raise ValueError("second teacher vocabulary marginal has the wrong shape")
    if tuple(teacher_entropy.shape) != (hidden.shape[0],):
        raise ValueError("teacher entropy has the wrong shape")
    cross_entropy0 = -(
        teacher_marginal0 * F.log_softmax(logits0, dim=-1)
    ).sum(dim=-1)
    cross_entropy1 = -(
        teacher_marginal1 * F.log_softmax(logits1, dim=-1)
    ).sum(dim=-1)
    return (cross_entropy0 + cross_entropy1 - teacher_entropy).mean()


class _ChunkedLinearForwardKL(torch.autograd.Function):
    """Exact full-vocabulary KL without materializing full student logits.

    The teacher distribution remains FP32. The student vocabulary projection is
    recomputed chunk-by-chunk in backward, which trades a small amount of GEMM
    work for a much smaller activation footprint at very large physical batches.
    """

    @staticmethod
    def forward(
        ctx,
        hidden: torch.Tensor,
        weight: torch.Tensor,
        teacher_probability: torch.Tensor,
        teacher_entropy: torch.Tensor,
        chunk_size: int,
    ) -> torch.Tensor:
        if hidden.ndim != 2 or weight.ndim != 2:
            raise ValueError("hidden and tied vocabulary weight must be matrices")
        if hidden.shape[1] != weight.shape[1]:
            raise ValueError("hidden width and tied vocabulary width disagree")
        expected = (hidden.shape[0], weight.shape[0])
        if tuple(teacher_probability.shape) != expected:
            raise ValueError("teacher and student vocabulary dimensions disagree")
        if tuple(teacher_entropy.shape) != (hidden.shape[0],):
            raise ValueError("teacher entropy has the wrong shape")
        if chunk_size <= 0:
            raise ValueError("vocabulary chunk size must be positive")

        batch = hidden.shape[0]
        log_normalizer = torch.full(
            (batch,),
            -torch.inf,
            device=hidden.device,
            dtype=torch.float32,
        )
        teacher_expected_logit = torch.zeros_like(log_normalizer)
        for start in range(0, weight.shape[0], chunk_size):
            stop = min(start + chunk_size, weight.shape[0])
            logits = F.linear(hidden, weight[start:stop])
            logits_float = logits.float()
            log_normalizer = torch.logaddexp(
                log_normalizer,
                torch.logsumexp(logits_float, dim=-1),
            )
            teacher_expected_logit.add_(
                (
                    teacher_probability[:, start:stop]
                    * logits_float
                ).sum(dim=-1)
            )

        ctx.chunk_size = int(chunk_size)
        ctx.save_for_backward(
            hidden,
            weight,
            teacher_probability,
            log_normalizer,
        )
        rows = log_normalizer - teacher_expected_logit - teacher_entropy
        return rows.mean()

    @staticmethod
    def backward(ctx, gradient: torch.Tensor):
        hidden, weight, teacher_probability, log_normalizer = ctx.saved_tensors
        batch = hidden.shape[0]
        gradient_hidden = torch.zeros_like(hidden)
        gradient_weight = torch.empty_like(weight)
        row_scale = gradient.float() / batch

        for start in range(0, weight.shape[0], ctx.chunk_size):
            stop = min(start + ctx.chunk_size, weight.shape[0])
            weight_chunk = weight[start:stop]
            logits = F.linear(hidden, weight_chunk)
            student_probability = torch.exp(
                logits.float() - log_normalizer[:, None]
            )
            delta = (
                student_probability
                - teacher_probability[:, start:stop]
            )
            delta.mul_(row_scale)
            compute_delta = delta.to(dtype=hidden.dtype)
            gradient_hidden.add_(
                F.linear(compute_delta, weight_chunk.mT)
            )
            gradient_weight[start:stop].copy_(
                compute_delta.mT @ hidden
            )

        return gradient_hidden, gradient_weight, None, None, None


def chunked_linear_forward_kl(
    hidden: torch.Tensor,
    tied_weight: torch.Tensor,
    teacher_probability: torch.Tensor,
    teacher_entropy: torch.Tensor,
    *,
    chunk_size: int = VOCAB_CHUNK_SIZE,
) -> torch.Tensor:
    """Return mean exact forward KL using every vocabulary entry."""

    return _ChunkedLinearForwardKL.apply(
        hidden,
        tied_weight,
        teacher_probability,
        teacher_entropy,
        chunk_size,
    )


class _MaterializedProbabilityLinearForwardKL(torch.autograd.Function):
    """Exact KL value with a BF16 student-probability backward cache."""

    @staticmethod
    def forward(
        ctx,
        hidden: torch.Tensor,
        weight: torch.Tensor,
        teacher_probability: torch.Tensor,
        teacher_entropy: torch.Tensor,
        chunk_size: int,
    ) -> torch.Tensor:
        if tuple(teacher_probability.shape) != (
            hidden.shape[0],
            weight.shape[0],
        ):
            raise ValueError("teacher and student vocabulary dimensions disagree")
        if tuple(teacher_entropy.shape) != (hidden.shape[0],):
            raise ValueError("teacher entropy has the wrong shape")
        if chunk_size <= 0:
            raise ValueError("vocabulary chunk size must be positive")

        student_probability = F.linear(hidden, weight)
        logits_float = student_probability.float()
        log_normalizer = torch.logsumexp(logits_float, dim=-1)
        teacher_expected_logit = torch.zeros_like(log_normalizer)
        for start in range(0, weight.shape[0], chunk_size):
            stop = min(start + chunk_size, weight.shape[0])
            teacher_expected_logit.add_(
                (
                    teacher_probability[:, start:stop]
                    * logits_float[:, start:stop]
                ).sum(dim=-1)
            )

        logits_float.sub_(log_normalizer[:, None]).exp_()
        student_probability.copy_(logits_float)
        ctx.chunk_size = int(chunk_size)
        ctx.save_for_backward(
            hidden,
            weight,
            student_probability,
            teacher_probability,
        )
        rows = log_normalizer - teacher_expected_logit - teacher_entropy
        return rows.mean()

    @staticmethod
    def backward(ctx, gradient: torch.Tensor):
        hidden, weight, student_probability, teacher_probability = (
            ctx.saved_tensors
        )
        gradient_hidden = torch.zeros_like(hidden)
        gradient_weight = torch.empty_like(weight)
        row_scale = gradient.float() / hidden.shape[0]

        for start in range(0, weight.shape[0], ctx.chunk_size):
            stop = min(start + ctx.chunk_size, weight.shape[0])
            weight_chunk = weight[start:stop]
            delta = (
                student_probability[:, start:stop].float()
                - teacher_probability[:, start:stop]
            )
            delta.mul_(row_scale)
            compute_delta = delta.to(dtype=hidden.dtype)
            gradient_hidden.add_(
                F.linear(compute_delta, weight_chunk.mT)
            )
            gradient_weight[start:stop].copy_(
                compute_delta.mT @ hidden
            )

        return gradient_hidden, gradient_weight, None, None, None


def materialized_probability_linear_forward_kl(
    hidden: torch.Tensor,
    tied_weight: torch.Tensor,
    teacher_probability: torch.Tensor,
    teacher_entropy: torch.Tensor,
    *,
    chunk_size: int = VOCAB_CHUNK_SIZE,
) -> torch.Tensor:
    """Return exact full-vocabulary KL with no head recomputation."""

    return _MaterializedProbabilityLinearForwardKL.apply(
        hidden,
        tied_weight,
        teacher_probability,
        teacher_entropy,
        chunk_size,
    )


def distribution_metrics(
    student_logits: torch.Tensor,
    teacher_probability: torch.Tensor,
    teacher_log_probability: torch.Tensor,
    teacher_entropy: torch.Tensor,
    targets: torch.Tensor,
) -> dict[str, torch.Tensor]:
    rows = exact_kl_rows(
        student_logits,
        teacher_probability,
        teacher_entropy,
    )
    student_log_probability = F.log_softmax(
        student_logits.float(),
        dim=-1,
    )
    return {
        "kl": rows,
        "student_nll": -student_log_probability.gather(1, targets[:, None]).squeeze(1),
        "teacher_nll": -teacher_log_probability.gather(1, targets[:, None]).squeeze(1),
        "accuracy": (student_logits.argmax(-1) == targets).float(),
    }


def validate_probability_rows(probability: torch.Tensor) -> None:
    if probability.ndim != 2:
        raise ValueError("teacher probabilities must be rank two")
    if not torch.isfinite(probability).all():
        raise RuntimeError("teacher probability contains non-finite values")
    error = float((probability.sum(-1) - 1.0).abs().max())
    if not math.isfinite(error) or error > 5e-5:
        raise RuntimeError(f"teacher probability rows are not normalized: {error}")
