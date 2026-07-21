"""Sparse, screened Walsh student utilities for the Fourier rescue runs.

The deployable model is deliberately small: character coordinate tuples,
coefficients, an affine probability calibration, and an independent decision
threshold.  Candidate screening and all fitting happen on a GPU in Modal.
"""

from __future__ import annotations

import math
from collections.abc import Mapping
from typing import Any

import numpy as np
import torch
from torch import nn


def canonical_input_columns(
    bits: np.ndarray,
) -> tuple[np.ndarray, np.ndarray, dict[str, int]]:
    """Return one representative of each nonconstant column up to complement.

    Complementary columns represent Walsh degree-one features that differ only
    by sign, so retaining both wastes a character.  Sign is absorbed by the
    learned coefficient.  Signatures are computed on the supplied audit set.
    """
    values = np.asarray(bits, dtype=np.uint8)
    if values.ndim != 2:
        raise ValueError("bits must be a two-dimensional binary array")
    if np.any((values != 0) & (values != 1)):
        raise ValueError("bits must be binary")
    packed = np.packbits(values, axis=0, bitorder="little").T
    inverted = np.packbits(1 - values, axis=0, bitorder="little").T
    representatives: list[int] = []
    signs: list[int] = []
    seen: dict[bytes, int] = {}
    constant = 0
    complements = 0
    duplicates = 0
    for column, (raw, inv) in enumerate(zip(packed, inverted, strict=True)):
        ones = int(values[:, column].sum())
        if ones == 0 or ones == len(values):
            constant += 1
            continue
        raw_key, inv_key = raw.tobytes(), inv.tobytes()
        key = raw_key if raw_key <= inv_key else inv_key
        sign = 1 if raw_key <= inv_key else -1
        if key in seen:
            duplicates += 1
            complements += int(signs[seen[key]] != sign)
            continue
        seen[key] = len(representatives)
        representatives.append(column)
        signs.append(sign)
    return (
        np.asarray(representatives, dtype=np.int32),
        np.asarray(signs, dtype=np.int8),
        {
            "input_columns": int(values.shape[1]),
            "constant_columns_removed": constant,
            "equivalent_columns_removed": duplicates,
            "complement_columns_removed": complements,
            "canonical_columns": len(representatives),
        },
    )


@torch.no_grad()
def screen_degree_one_two(
    bits: torch.Tensor,
    teacher_probability: torch.Tensor,
    columns: torch.Tensor,
    keep: int,
    *,
    sample_chunk: int = 32768,
) -> tuple[torch.Tensor, torch.Tensor, torch.Tensor, dict[str, float]]:
    """Screen all degree-one/two characters by initial logistic gradient.

    The degree-two score matrix is accumulated by TF32 GEMMs.  The target is
    centered, making the score exactly the negative gradient at the optimal
    bias-only soft-BCE model.
    """
    if bits.device.type != "cuda":
        raise ValueError("screening must run on a GPU")
    if keep <= 0:
        raise ValueError("keep must be positive")
    columns = columns.to(device=bits.device, dtype=torch.long)
    target = teacher_probability.float().reshape(-1)
    residual = target - target.mean()
    width = len(columns)
    pair_score = torch.zeros((width, width), device=bits.device)
    degree_one = torch.zeros(width, device=bits.device)
    for lo in range(0, len(bits), sample_chunk):
        hi = min(lo + sample_chunk, len(bits))
        sign = 1.0 - 2.0 * bits[lo:hi, columns].float()
        r = residual[lo:hi]
        degree_one.add_(sign.t() @ r)
        pair_score.add_(sign.t() @ (sign * r[:, None]))
    scale = 1.0 / len(bits)
    degree_one.mul_(scale)
    pair_score.mul_(scale)
    lower = torch.tril(
        torch.ones_like(pair_score, dtype=torch.bool), diagonal=0
    )
    # Invalid lower-triangular/diagonal entries must not participate in the
    # absolute-value top-k below.  Using ``-inf`` here would turn them into
    # ``+inf`` after ``abs`` and select only invalid pairs.
    pair_score.masked_fill_(lower, 0.0)
    pair_count = width * (width - 1) // 2
    # Low-degree terms are useful, but a bank made entirely of degree 1/2
    # characters cannot express the higher-order interactions that carried the
    # earlier Fourier baseline. Reserve a substantial tail for scored degree
    # 3--8 candidates instead of spending the whole budget on pairs.
    degree_one_count = min(width, max(1, keep // 16))
    pair_keep = min(max(0, (keep * 7) // 16), pair_count)
    high_keep = max(0, keep - degree_one_count - pair_keep)
    one_order = torch.argsort(degree_one.abs(), descending=True)[:degree_one_count]
    if pair_keep:
        flat = torch.topk(pair_score.abs().flatten(), pair_keep).indices
        left = torch.div(flat, width, rounding_mode="floor")
        right = flat.remainder(width)
        pair_rows = torch.stack((columns[left], columns[right]), dim=1)
        pair_values = pair_score[left, right]
    else:
        pair_rows = torch.empty((0, 2), dtype=torch.long, device=bits.device)
        pair_values = torch.empty(0, device=bits.device)
    one_rows = torch.stack((columns[one_order], columns[one_order]), dim=1)
    rows = torch.cat((one_rows, pair_rows), dim=0)
    degrees = torch.cat((
        torch.ones(degree_one_count, dtype=torch.uint8, device=bits.device),
        torch.full((pair_keep,), 2, dtype=torch.uint8, device=bits.device),
    ))
    scores = torch.cat((degree_one[one_order], pair_values))

    if high_keep:
        # Score a deterministic oversampled bank of higher-degree characters
        # in GPU chunks.  The extra candidates make the top-k robust to the
        # very sparse residual signal without storing a dense feature matrix.
        from fourier_kiss.model import deterministic_mask_rows

        pool = min(max(high_keep * 2, 32768), 1_000_000)
        high_rows_np, high_degrees_np = deterministic_mask_rows(
            width, pool, max_degree=8, seed=0x51A7
        )
        high_rows = torch.as_tensor(
            high_rows_np, device=bits.device, dtype=torch.long
        )
        high_degrees = torch.as_tensor(
            high_degrees_np, device=bits.device, dtype=torch.long
        )
        higher = high_degrees >= 3
        high_rows = high_rows[higher]
        high_degrees = high_degrees[higher]
        pool = len(high_rows)
        high_scores = torch.empty(pool, device=bits.device)
        candidate_chunk = 2048
        for lo in range(0, pool, candidate_chunk):
            hi = min(lo + candidate_chunk, pool)
            candidate = high_rows[lo:hi]
            active = torch.arange(8, device=bits.device)[None, :] < \
                high_degrees[lo:hi, None]
            score = torch.zeros(hi - lo, device=bits.device)
            for sample_lo in range(0, len(bits), sample_chunk):
                sample_hi = min(sample_lo + sample_chunk, len(bits))
                selected = bits[
                    sample_lo:sample_hi,
                    columns[candidate.clamp_max(width - 1)],
                ].float()
                parity = (selected * active[None]).sum(dim=2).remainder(2.0)
                feature = 1.0 - 2.0 * parity
                score.add_(feature.t() @ residual[sample_lo:sample_hi])
            high_scores[lo:hi] = score / len(bits)
        high_order = torch.topk(
            high_scores.abs(), min(high_keep, pool), sorted=True
        ).indices
        padded_rows = torch.full(
            (len(rows), 8), width, dtype=rows.dtype, device=rows.device
        )
        padded_rows[:, :2] = rows
        rows = padded_rows
        rows = torch.cat((rows, high_rows[high_order]), dim=0)
        degrees = torch.cat((degrees, high_degrees[high_order].to(torch.uint8)))
        scores = torch.cat((scores, high_scores[high_order]))
    order = torch.argsort(scores.abs(), descending=True)
    rows, degrees, scores = rows[order], degrees[order], scores[order]
    diagnostics = {
        "candidate_degree_one": float(degree_one_count),
        "candidate_degree_two_space": float(pair_count),
        "selected_degree_two": float(pair_keep),
        "selected_degree_three_plus": float(high_keep),
        "top_abs_gradient": float(scores.abs().max()),
        "median_selected_abs_gradient": float(scores.abs().median()),
    }
    return rows, degrees, scores, diagnostics


def walsh_features(bits: torch.Tensor, indices: torch.Tensor,
                   degrees: torch.Tensor) -> torch.Tensor:
    """Evaluate sentinel-free degree-one/two Walsh characters."""
    safe_indices = indices.clamp_max(bits.shape[1] - 1)
    selected = bits[:, safe_indices]
    active = (
        torch.arange(indices.shape[1], device=indices.device)[None, :]
        < degrees[:, None]
    )
    parity = (selected * active[None]).sum(dim=2)
    return 1.0 - 2.0 * parity.remainder(2.0)


class SparseWalshStudent(nn.Module):
    """A fixed sparse character bank with trainable AdamW coefficients."""

    def __init__(self, indices: torch.Tensor, degrees: torch.Tensor, *,
                 n_bits: int,
                 initial_probability: float, char_chunk: int = 4096,
                 initial_scores: torch.Tensor | None = None):
        super().__init__()
        if indices.ndim != 2 or len(indices) != len(degrees):
            raise ValueError("character arrays disagree")
        self.register_buffer("indices", indices.long())
        self.register_buffer("degrees", degrees.to(torch.uint8))
        self.n_bits = int(n_bits)
        self.coefficient = nn.Parameter(torch.zeros(len(indices)))
        if initial_scores is not None:
            score = initial_scores.float()
            score = score / score.abs().max().clamp_min(1e-8)
            self.coefficient.data.copy_(score * 1e-3)
        p = float(np.clip(initial_probability, 1e-6, 1.0 - 1e-6))
        self.bias = nn.Parameter(torch.tensor(math.log(p / (1.0 - p))))
        self.char_chunk = int(char_chunk)

    def forward(self, bits: torch.Tensor) -> torch.Tensor:
        output = self.bias.expand(len(bits)).float()
        for lo in range(0, len(self.coefficient), self.char_chunk):
            hi = min(lo + self.char_chunk, len(self.coefficient))
            feature = walsh_features(
                bits, self.indices[lo:hi], self.degrees[lo:hi]
            )
            output = output + feature.float() @ self.coefficient[lo:hi]
        return output

    @torch.no_grad()
    def sparse_state(self) -> dict[str, Any]:
        degree = self.degrees.cpu().numpy().astype(np.uint8)
        rows = self.indices.cpu().numpy().astype(np.uint32)
        active = np.arange(rows.shape[1])[None, :] < degree[:, None]
        return {
            "n_bits": self.n_bits,
            "degrees": degree,
            "indices": rows[active].astype(np.uint16),
            "coefficient": self.coefficient.float().cpu().numpy(),
            "bias": float(self.bias),
        }


def fit_affine_calibration(
    logits: torch.Tensor,
    teacher_probability: torch.Tensor,
    *,
    steps: int = 100,
) -> tuple[float, float]:
    """Fit positive logit scale and bias to validation soft BCE on-device."""
    z = logits.detach().float()
    target = teacher_probability.detach().float()
    log_scale = torch.zeros((), device=z.device, requires_grad=True)
    bias = torch.zeros((), device=z.device, requires_grad=True)
    optimizer = torch.optim.LBFGS(
        [log_scale, bias], lr=0.5, max_iter=steps,
        line_search_fn="strong_wolfe",
    )

    def closure():
        optimizer.zero_grad(set_to_none=True)
        output = z * log_scale.exp() + bias
        loss = torch.nn.functional.binary_cross_entropy_with_logits(
            output, target
        )
        loss.backward()
        return loss

    optimizer.step(closure)
    return float(log_scale.detach().exp()), float(bias.detach())


@torch.no_grad()
def calibrate_decision_threshold(
    logits: torch.Tensor,
    teacher_probability: torch.Tensor,
    minimum_positive_recall: float = 0.60,
) -> tuple[float, float, float]:
    """Maximize hard agreement subject to a teacher-positive recall floor."""
    if not 0.0 <= minimum_positive_recall <= 1.0:
        raise ValueError("minimum_positive_recall must lie in [0,1]")
    z, order = torch.sort(logits.detach().double())
    y = (teacher_probability.detach().reshape(-1)[order] >= 0.5)
    positives = y.sum().clamp_min(1)
    prefix_positive = torch.cumsum(y.long(), dim=0)
    predicted_positive_true = positives - prefix_positive
    predicted_negative_true = torch.arange(
        1, len(y) + 1, device=y.device
    ) - prefix_positive
    correct = predicted_positive_true + predicted_negative_true
    recall = predicted_positive_true.double() / positives.double()
    valid = recall >= minimum_positive_recall
    valid[-1] = False
    score = torch.where(valid, correct, torch.full_like(correct, -1))
    index = int(score.argmax())
    threshold = float((z[index] + z[index + 1]) * 0.5)
    return threshold, float(correct[index] / len(y)), float(recall[index])


def calibrated_metrics(
    logits: torch.Tensor | np.ndarray,
    teacher_probability: torch.Tensor | np.ndarray,
    *,
    logit_scale: float = 1.0,
    logit_bias: float = 0.0,
    decision_threshold: float = 0.0,
) -> dict[str, float]:
    """Probability-fidelity and calibrated hard-decision metrics."""
    from fourier_kiss.model import binary_metrics

    z = torch.as_tensor(logits, dtype=torch.float64).reshape(-1)
    target = torch.as_tensor(
        teacher_probability, dtype=torch.float64
    ).reshape(-1)
    z = z * logit_scale + logit_bias
    result = binary_metrics(z, target)
    teacher = target >= 0.5
    student = z >= decision_threshold
    positive = teacher.sum().clamp_min(1)
    negative = (~teacher).sum().clamp_min(1)
    tp = (teacher & student).sum()
    tn = ((~teacher) & (~student)).sum()
    fp = ((~teacher) & student).sum()
    fn = (teacher & (~student)).sum()
    result.update({
        "calibrated_agreement": float((teacher == student).double().mean()),
        "calibrated_teacher_positive_recall": float(tp / positive),
        "calibrated_teacher_negative_recall": float(tn / negative),
        "calibrated_student_positive_rate": float(student.double().mean()),
        "true_positive": float(tp), "true_negative": float(tn),
        "false_positive": float(fp), "false_negative": float(fn),
        "decision_logit_threshold": float(decision_threshold),
    })
    return result


def with_calibration(state: Mapping[str, Any], *, logit_scale: float,
                     logit_bias: float, decision_threshold: float,
                     ) -> dict[str, Any]:
    result = dict(state)
    result.update({
        "logit_scale": float(logit_scale),
        "logit_bias": float(logit_bias),
        "decision_logit_threshold": float(decision_threshold),
    })
    return result
