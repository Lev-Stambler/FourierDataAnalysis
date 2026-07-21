"""Data-aware residual pursuit for exact Walsh--Fourier students.

The deployed model is still a flat sum of exact characters.  The training
representation additionally records every support as a staircase node
(``parent``, ``appended_bit``), which makes the serialized support bank eight
bytes per node including an fp16 coefficient.

Unlike the direct STE model, support discovery is an explicit discrete step:
score all one-bit extensions of a selected parent frontier against the current
soft-BCE residual, reject functionally redundant columns on a fixed probe, add
the best characters, and refit all coefficients.
"""

from __future__ import annotations

import hashlib
import math
from collections.abc import Iterable, Sequence
from typing import Any

import numpy as np
import torch
from torch import nn


ROOT_PARENT = -1


def _as_numpy(value: torch.Tensor | np.ndarray) -> np.ndarray:
    if torch.is_tensor(value):
        return value.detach().cpu().numpy()
    return np.asarray(value)


def canonical_signature_digest(parity: np.ndarray) -> bytes:
    """Hash a binary feature signature canonically up to global sign."""
    value = np.asarray(parity, dtype=np.uint8).reshape(-1)
    packed = np.packbits(value, bitorder="little").tobytes()
    inverse = np.packbits(1 - value, bitorder="little").tobytes()
    canonical = packed if packed <= inverse else inverse
    return hashlib.blake2b(canonical, digest_size=16).digest()


def support_key(indices: Sequence[int]) -> bytes:
    """Canonical compact key for one sorted Walsh support."""
    value = np.asarray(tuple(indices), dtype=np.uint16)
    return bytes((len(value),)) + value.tobytes()


def evaluate_supports(bits: torch.Tensor, indices: torch.Tensor,
                      degrees: torch.Tensor) -> torch.Tensor:
    """Evaluate padded exact Walsh supports as a dense +/-1 feature block."""
    if indices.ndim != 2 or len(indices) != len(degrees):
        raise ValueError("support arrays disagree")
    safe = indices.long().clamp_max(bits.shape[1] - 1)
    selected = bits[:, safe]
    active = (
        torch.arange(indices.shape[1], device=indices.device)[None, :]
        < degrees[:, None].long()
    )
    parity = (selected * active[None]).sum(dim=2, dtype=torch.int32)
    return 1.0 - 2.0 * parity.bitwise_and_(1).float()


class StaircaseWalshStudent(nn.Module):
    """A growing exact character bank with fixed-capacity coefficient storage."""

    def __init__(self, n_bits: int, max_terms: int, max_degree: int, *,
                 initial_probability: float, char_chunk: int = 1024):
        super().__init__()
        if not 0 < max_degree <= min(n_bits, 255):
            raise ValueError("invalid maximum degree")
        if max_terms <= 0 or char_chunk <= 0:
            raise ValueError("max_terms and char_chunk must be positive")
        sentinel = int(n_bits)
        self.register_buffer(
            "indices",
            torch.full((max_terms, max_degree), sentinel, dtype=torch.int32),
        )
        self.register_buffer("degrees", torch.zeros(max_terms, dtype=torch.uint8))
        self.register_buffer(
            "parent", torch.full((max_terms,), ROOT_PARENT, dtype=torch.int32)
        )
        self.register_buffer(
            "appended_bit", torch.full((max_terms,), sentinel, dtype=torch.int32)
        )
        self.coefficient = nn.Parameter(torch.zeros(max_terms))
        p = float(np.clip(initial_probability, 1e-6, 1.0 - 1e-6))
        self.bias = nn.Parameter(torch.tensor(math.log(p / (1.0 - p))))
        self.n_bits = int(n_bits)
        self.max_terms = int(max_terms)
        self.max_degree = int(max_degree)
        self.char_chunk = int(char_chunk)
        # Match the stable parameterization used by the earlier direct model.
        # Adam updates raw O(1) coefficients while the represented Fourier
        # coefficients are normalized by the fixed deployment budget.
        self.output_scale = max_terms ** -0.5
        self.active_terms = 0

    @torch.no_grad()
    def append(self, supports: Sequence[Sequence[int]], parents: Sequence[int],
               appended_bits: Sequence[int], initial_coefficients: Sequence[float]
               | None = None) -> tuple[int, int]:
        count = len(supports)
        if not (len(parents) == len(appended_bits) == count):
            raise ValueError("append arrays disagree")
        start, stop = self.active_terms, self.active_terms + count
        if stop > self.max_terms:
            raise ValueError("character bank capacity exceeded")
        host_indices = np.full(
            (count, self.max_degree), self.n_bits, dtype=np.int32
        )
        host_degrees = np.empty(count, dtype=np.uint8)
        for row, support in enumerate(supports):
            canonical = np.asarray(sorted(set(map(int, support))), dtype=np.int32)
            if not 0 < len(canonical) <= self.max_degree:
                raise ValueError("invalid support degree")
            if canonical[0] < 0 or canonical[-1] >= self.n_bits:
                raise ValueError("support coordinate out of range")
            host_indices[row, :len(canonical)] = canonical
            host_degrees[row] = len(canonical)
        device = self.indices.device
        self.indices[start:stop].copy_(torch.from_numpy(host_indices).to(device))
        self.degrees[start:stop].copy_(torch.from_numpy(host_degrees).to(device))
        self.parent[start:stop].copy_(torch.as_tensor(
            parents, device=device, dtype=torch.int32
        ))
        self.appended_bit[start:stop].copy_(torch.as_tensor(
            appended_bits, device=device, dtype=torch.int32
        ))
        if initial_coefficients is not None:
            self.coefficient[start:stop].copy_(torch.as_tensor(
                initial_coefficients, device=device,
                dtype=self.coefficient.dtype,
            ))
        else:
            self.coefficient[start:stop].zero_()
        self.active_terms = stop
        return start, stop

    def forward(self, bits: torch.Tensor) -> torch.Tensor:
        output = self.bias.expand(len(bits)).float()
        for lo in range(0, self.active_terms, self.char_chunk):
            hi = min(lo + self.char_chunk, self.active_terms)
            feature = evaluate_supports(
                bits, self.indices[lo:hi], self.degrees[lo:hi]
            )
            output = output + (
                feature @ self.coefficient[lo:hi]
            ) * self.output_scale
        return output

    @torch.no_grad()
    def feature_block(self, bits: torch.Tensor,
                      rows: torch.Tensor) -> torch.Tensor:
        rows = rows.long()
        return evaluate_supports(bits, self.indices[rows], self.degrees[rows])

    @torch.no_grad()
    def active_supports(self) -> list[tuple[int, ...]]:
        indices = self.indices[:self.active_terms].cpu().numpy()
        degrees = self.degrees[:self.active_terms].cpu().numpy()
        return [tuple(map(int, indices[row, :int(degree)]))
                for row, degree in enumerate(degrees)]


@torch.no_grad()
def initial_low_degree_bank(
    bits: torch.Tensor,
    teacher_probability: torch.Tensor,
    columns: torch.Tensor,
    keep: int,
    *,
    sample_chunk: int = 32768,
) -> tuple[list[tuple[int, ...]], np.ndarray, dict[str, float]]:
    """Keep every canonical singleton and fill with strongest degree-2 terms."""
    if bits.device.type != "cuda":
        raise ValueError("low-degree screening must run on CUDA")
    columns = columns.long().to(bits.device)
    width = len(columns)
    pair_count = width * (width - 1) // 2
    if keep < width or keep - width > pair_count:
        raise ValueError("requested bank size is incompatible with candidate space")
    target = teacher_probability.float()
    residual = target - target.mean()
    singleton_score = torch.zeros(width, device=bits.device)
    pair_score = torch.zeros((width, width), device=bits.device)
    for lo in range(0, len(bits), sample_chunk):
        hi = min(lo + sample_chunk, len(bits))
        sign = 1.0 - 2.0 * bits[lo:hi, columns].float()
        r = residual[lo:hi]
        singleton_score.add_(sign.t() @ r)
        pair_score.add_(sign.t() @ (sign * r[:, None]))
    singleton_score /= len(bits)
    pair_score /= len(bits)
    invalid = torch.tril(torch.ones_like(pair_score, dtype=torch.bool))
    pair_score.masked_fill_(invalid, 0.0)
    pair_keep = keep - width
    flat = torch.topk(pair_score.abs().flatten(), pair_keep).indices
    left = torch.div(flat, width, rounding_mode="floor")
    right = flat.remainder(width)
    columns_cpu = columns.cpu().numpy()
    supports: list[tuple[int, ...]] = [
        (int(column),) for column in columns_cpu
    ]
    supports.extend(
        tuple(sorted((int(columns_cpu[a]), int(columns_cpu[b]))))
        for a, b in zip(left.cpu().numpy(), right.cpu().numpy(), strict=True)
    )
    scores = torch.cat((
        singleton_score,
        pair_score[left, right],
    )).cpu().numpy().astype(np.float32)
    return supports, scores, {
        "canonical_columns": float(width),
        "degree_two_space": float(pair_count),
        "initial_singletons": float(width),
        "initial_pairs": float(pair_keep),
        "initial_top_abs_residual_correlation": float(np.abs(scores).max()),
        "initial_median_abs_residual_correlation": float(
            np.median(np.abs(scores))
        ),
    }


@torch.no_grad()
def filter_seed_candidates(
    bits: torch.Tensor,
    supports: Sequence[Sequence[int]],
    scores: Sequence[float] | np.ndarray,
    keep: int,
    *,
    feature_chunk: int = 512,
) -> tuple[list[tuple[int, ...]], np.ndarray, dict[str, float]]:
    """Keep a staircase-valid seed bank unique on the empirical data probe.

    Exact support uniqueness is not enough on correlated text inputs: distinct
    Walsh supports can be constant, equal, or negated on every observed token.
    Candidates are assumed to be ordered by preference (all singletons first,
    then correlation-ranked pairs).  A pair is retained only when at least one
    of its singleton parents survived, so the compact staircase encoding stays
    valid without inventing hidden nodes.
    """
    if bits.device.type != "cuda":
        raise ValueError("seed filtering must run on CUDA")
    if keep <= 0 or len(supports) != len(scores):
        raise ValueError("invalid seed candidate arrays")
    canonical_supports = [tuple(sorted(set(map(int, item))))
                          for item in supports]
    if any(not item for item in canonical_supports):
        raise ValueError("empty Walsh support in seed candidates")
    score_array = np.asarray(scores, dtype=np.float32)
    accepted_supports: list[tuple[int, ...]] = []
    accepted_scores: list[float] = []
    accepted_support_set: set[tuple[int, ...]] = set()
    signature_keys: set[bytes] = set()
    duplicate_support = 0
    duplicate_signature = 0
    constant_signature = 0
    missing_parent = 0
    for lo in range(0, len(canonical_supports), feature_chunk):
        block = canonical_supports[lo:lo + feature_chunk]
        maximum_degree = max(map(len, block))
        host_indices = np.full(
            (len(block), maximum_degree), bits.shape[1], dtype=np.int32
        )
        host_degrees = np.empty(len(block), dtype=np.uint8)
        for row, support in enumerate(block):
            host_indices[row, :len(support)] = support
            host_degrees[row] = len(support)
        indices = torch.from_numpy(host_indices).to(bits.device)
        degrees = torch.from_numpy(host_degrees).to(bits.device)
        parity = (evaluate_supports(bits, indices, degrees) < 0)
        parity = parity.cpu().numpy().astype(np.uint8, copy=False)
        for row, support in enumerate(block):
            if support in accepted_support_set:
                duplicate_support += 1
                continue
            if len(support) > 1 and not any(
                (coordinate,) in accepted_support_set for coordinate in support
            ):
                missing_parent += 1
                continue
            value = parity[:, row]
            ones = int(value.sum())
            if ones == 0 or ones == len(value):
                constant_signature += 1
                continue
            digest = canonical_signature_digest(value)
            if digest in signature_keys:
                duplicate_signature += 1
                continue
            accepted_support_set.add(support)
            signature_keys.add(digest)
            accepted_supports.append(support)
            accepted_scores.append(float(score_array[lo + row]))
            if len(accepted_supports) == keep:
                break
        if len(accepted_supports) == keep:
            break
    return accepted_supports, np.asarray(accepted_scores, dtype=np.float32), {
        "seed_candidates": float(len(supports)),
        "seed_accepted": float(len(accepted_supports)),
        "seed_duplicate_supports_rejected": float(duplicate_support),
        "seed_duplicate_signatures_rejected": float(duplicate_signature),
        "seed_constant_signatures_rejected": float(constant_signature),
        "seed_missing_parent_rejected": float(missing_parent),
    }


def staircase_metadata(supports: Sequence[Sequence[int]]) -> tuple[np.ndarray,
                                                                    np.ndarray]:
    """Build a valid parent/appended-bit encoding for a low-degree seed bank."""
    location: dict[tuple[int, ...], int] = {}
    parent = np.full(len(supports), ROOT_PARENT, dtype=np.int32)
    appended = np.empty(len(supports), dtype=np.uint16)
    for row, raw in enumerate(supports):
        support = tuple(sorted(map(int, raw)))
        if support in location:
            raise ValueError("duplicate support in seed bank")
        if len(support) == 1:
            appended[row] = support[0]
        else:
            prefix = support[:-1]
            if prefix not in location:
                # The seed contains all singletons, but an arbitrary degree-two
                # pair need not be ordered with its smaller coordinate as the
                # retained singleton. Try either singleton parent.
                prefix = (support[-1],)
                bit = support[0]
            else:
                bit = support[-1]
            if prefix not in location:
                raise ValueError("seed support has no earlier staircase parent")
            parent[row] = location[prefix]
            appended[row] = bit
        location[support] = row
    return parent, appended


@torch.no_grad()
def select_parent_frontier(model: StaircaseWalshStudent, count: int,
                           degree_limit: int) -> torch.Tensor:
    """Mix recent frontier nodes with high-energy fitted nodes."""
    active = model.active_terms
    eligible = torch.nonzero(
        model.degrees[:active].long() < degree_limit,
        as_tuple=False,
    ).flatten()
    if len(eligible) <= count:
        return eligible
    recent_count = count // 2
    recent = eligible[-recent_count:]
    coefficient = model.coefficient.detach()[eligible].abs()
    energetic = eligible[torch.topk(coefficient, count - recent_count).indices]
    return torch.unique(torch.cat((recent, energetic)))[:count]


@torch.no_grad()
def score_staircase_extensions(
    model: StaircaseWalshStudent,
    bits: torch.Tensor,
    teacher_probability: torch.Tensor,
    columns: torch.Tensor,
    parent_rows: torch.Tensor,
    *,
    weights: torch.Tensor | None = None,
    parents_per_chunk: int = 128,
    top_per_parent: int = 32,
) -> list[tuple[float, float, int, int]]:
    """Score hard one-bit extensions by normalized soft-BCE residual gradient."""
    logits = model(bits)
    probability = torch.sigmoid(logits)
    residual = probability - teacher_probability.float()
    if weights is not None:
        residual = residual * weights.float()
    curvature = float((probability * (1.0 - probability)).mean().clamp_min(1e-5))
    columns = columns.long().to(bits.device)
    input_sign = 1.0 - 2.0 * bits[:, columns].float()
    weighted_input = input_sign * residual[:, None]
    proposals: list[tuple[float, float, int, int]] = []
    column_values = columns.cpu().numpy()
    for lo in range(0, len(parent_rows), parents_per_chunk):
        rows = parent_rows[lo:lo + parents_per_chunk]
        parent_feature = model.feature_block(bits, rows)
        raw_gradient = parent_feature.t() @ weighted_input / len(bits)
        mean = parent_feature.t() @ input_sign / len(bits)
        variance = (1.0 - mean.square()).clamp_min(1e-6)
        normalized = raw_gradient.abs() / variance.sqrt()
        # Extending by a bit already present would remove that bit rather than
        # grow the staircase. Mask those coordinates explicitly.
        row_supports = model.indices[rows].cpu().numpy()
        row_degrees = model.degrees[rows].cpu().numpy()
        coordinate_to_local = {
            int(value): local for local, value in enumerate(column_values)
        }
        for local_row, degree in enumerate(row_degrees):
            for coordinate in row_supports[local_row, :int(degree)]:
                local = coordinate_to_local.get(int(coordinate))
                if local is not None:
                    normalized[local_row, local] = -torch.inf
        top = torch.topk(
            normalized, min(top_per_parent, normalized.shape[1]), dim=1
        )
        raw_cpu = raw_gradient.cpu().numpy()
        for local_row, parent in enumerate(rows.cpu().numpy()):
            local_columns = top.indices[local_row].cpu().numpy()
            local_scores = top.values[local_row].cpu().numpy()
            for local_column, score in zip(
                local_columns, local_scores, strict=True
            ):
                gradient = float(raw_cpu[local_row, local_column])
                proposals.append((
                    float(score),
                    float(np.clip(-gradient / curvature, -0.25, 0.25)),
                    int(parent), int(column_values[local_column]),
                ))
    proposals.sort(key=lambda item: item[0], reverse=True)
    return proposals


@torch.no_grad()
def filter_extension_candidates(
    model: StaircaseWalshStudent,
    proposals: Iterable[tuple[float, float, int, int]],
    signature_bits: torch.Tensor,
    support_keys: set[bytes],
    signature_keys: set[bytes],
    keep: int,
) -> tuple[list[tuple[int, ...]], list[int], list[int], list[float],
           dict[str, float]]:
    """Enforce exact support and empirical functional uniqueness."""
    accepted_supports: list[tuple[int, ...]] = []
    accepted_parents: list[int] = []
    accepted_bits: list[int] = []
    accepted_coefficients: list[float] = []
    duplicate_support = 0
    duplicate_signature = 0
    constant_signature = 0
    host_indices = model.indices[:model.active_terms].cpu().numpy()
    host_degrees = model.degrees[:model.active_terms].cpu().numpy()
    pending: list[tuple[tuple[int, ...], bytes, int, int, float]] = []
    pending_keys: set[bytes] = set()
    for _score, coefficient, parent, bit in proposals:
        raw_parent = host_indices[parent, :int(host_degrees[parent])]
        support = tuple(sorted((*map(int, raw_parent), int(bit))))
        key = support_key(support)
        if key in support_keys or key in pending_keys:
            duplicate_support += 1
            continue
        pending_keys.add(key)
        pending.append((support, key, parent, bit, coefficient))
        # Functional collisions are usually rare after residual ranking. An
        # 8x oversample leaves room to remove them without evaluating the full
        # proposal list on the signature probe.
        if len(pending) >= keep * 8:
            break
    feature_chunk = 512
    for lo in range(0, len(pending), feature_chunk):
        block = pending[lo:lo + feature_chunk]
        maximum_degree = max(len(item[0]) for item in block)
        host_block = np.full(
            (len(block), maximum_degree), model.n_bits, dtype=np.int32
        )
        host_block_degrees = np.empty(len(block), dtype=np.uint8)
        for row, item in enumerate(block):
            support = item[0]
            host_block[row, :len(support)] = support
            host_block_degrees[row] = len(support)
        indices = torch.from_numpy(host_block).to(signature_bits.device)
        degrees = torch.from_numpy(host_block_degrees).to(signature_bits.device)
        parity = (evaluate_supports(signature_bits, indices, degrees) < 0)
        parity = parity.cpu().numpy().astype(np.uint8, copy=False)
        for column, (support, key, parent, bit, coefficient) in enumerate(block):
            value = parity[:, column]
            ones = int(value.sum())
            if ones == 0 or ones == len(value):
                constant_signature += 1
                continue
            digest = canonical_signature_digest(value)
            if digest in signature_keys:
                duplicate_signature += 1
                continue
            support_keys.add(key)
            signature_keys.add(digest)
            accepted_supports.append(support)
            accepted_parents.append(parent)
            accepted_bits.append(bit)
            accepted_coefficients.append(coefficient)
            if len(accepted_supports) == keep:
                break
        if len(accepted_supports) == keep:
            break
    return (
        accepted_supports, accepted_parents, accepted_bits,
        accepted_coefficients, {
            "candidate_duplicate_supports": float(duplicate_support),
            "candidate_duplicate_signatures": float(duplicate_signature),
            "candidate_constant_signatures": float(constant_signature),
            "candidate_accepted": float(len(accepted_supports)),
        },
    )


@torch.no_grad()
def initialize_uniqueness_sets(
    model: StaircaseWalshStudent,
    signature_bits: torch.Tensor,
    *,
    feature_chunk: int = 1024,
) -> tuple[set[bytes], set[bytes], dict[str, float]]:
    supports = model.active_supports()
    support_keys = {support_key(support) for support in supports}
    signature_keys: set[bytes] = set()
    duplicates = 0
    constants = 0
    for lo in range(0, model.active_terms, feature_chunk):
        hi = min(lo + feature_chunk, model.active_terms)
        rows = torch.arange(lo, hi, device=signature_bits.device)
        parity = (model.feature_block(signature_bits, rows) < 0).cpu().numpy()
        for column in range(parity.shape[1]):
            value = parity[:, column].astype(np.uint8)
            ones = int(value.sum())
            if ones == 0 or ones == len(value):
                constants += 1
                continue
            digest = canonical_signature_digest(value)
            duplicates += int(digest in signature_keys)
            signature_keys.add(digest)
    return support_keys, signature_keys, {
        "initial_exact_unique_supports": float(len(support_keys)),
        "initial_functional_unique_signatures": float(len(signature_keys)),
        "initial_functional_duplicates": float(duplicates),
        "initial_constant_signatures": float(constants),
    }


def encode_staircase_student(model: StaircaseWalshStudent,
                             block_size: int = 4096) -> dict[str, Any]:
    """Quantize coefficients and export compact parent/bit character nodes."""
    count = model.active_terms
    coefficient = (
        model.coefficient[:count].detach().float() * model.output_scale
    ).cpu().numpy()
    blocks = math.ceil(count / block_size)
    quantized = np.empty(count, dtype=np.float16)
    scale = np.ones(blocks, dtype=np.float32)
    for block in range(blocks):
        lo, hi = block * block_size, min((block + 1) * block_size, count)
        maximum = float(np.abs(coefficient[lo:hi]).max(initial=0.0))
        value = maximum if maximum > 1e-12 else 1.0
        scale[block] = value
        quantized[lo:hi] = (coefficient[lo:hi] / value).astype(np.float16)
    return {
        "n_bits": np.asarray(model.n_bits, dtype=np.int32),
        "terms": np.asarray(count, dtype=np.int32),
        "parent": model.parent[:count].detach().cpu().numpy().astype(np.int32),
        "appended_bit": model.appended_bit[:count].detach().cpu().numpy().astype(
            np.uint16
        ),
        "coefficient_fp16": quantized,
        "coefficient_scale": scale,
        "coefficient_block_size": np.asarray(block_size, dtype=np.int32),
        "bias": np.asarray(float(model.bias.detach()), dtype=np.float32),
    }


def decode_staircase_student(state: dict[str, Any]) -> dict[str, Any]:
    parent = np.asarray(state["parent"], dtype=np.int32)
    appended = np.asarray(state["appended_bit"], dtype=np.uint16)
    block_size = int(np.asarray(state["coefficient_block_size"]))
    scale = np.asarray(state["coefficient_scale"], dtype=np.float32)
    quantized = np.asarray(state["coefficient_fp16"], dtype=np.float16)
    coefficient = quantized.astype(np.float32) * np.repeat(
        scale, block_size
    )[:len(quantized)]
    supports: list[tuple[int, ...]] = []
    for row, (ancestor, bit) in enumerate(zip(parent, appended, strict=True)):
        if ancestor == ROOT_PARENT:
            support = (int(bit),)
        else:
            if not 0 <= ancestor < row:
                raise ValueError("staircase parent is not topologically earlier")
            support = tuple(sorted((*supports[int(ancestor)], int(bit))))
        if len(set(support)) != len(support):
            raise ValueError("serialized staircase toggles an existing bit")
        supports.append(support)
    return {
        "n_bits": int(np.asarray(state["n_bits"])),
        "supports": supports,
        "coefficient": coefficient,
        "bias": float(np.asarray(state["bias"])),
    }


def staircase_predict(bits: np.ndarray, state: dict[str, Any],
                      chunk: int = 1024) -> np.ndarray:
    """Reference inference from a compact staircase artifact."""
    if "supports" not in state:
        state = decode_staircase_student(state)
    x = np.asarray(bits, dtype=np.uint8)
    supports = state["supports"]
    coefficient = np.asarray(state["coefficient"], dtype=np.float32)
    maximum_degree = max(map(len, supports), default=0)
    padded = np.zeros((len(supports), maximum_degree), dtype=np.int64)
    active = np.zeros_like(padded, dtype=bool)
    for row, support in enumerate(supports):
        padded[row, :len(support)] = support
        active[row, :len(support)] = True
    output = np.full(len(x), float(state["bias"]), dtype=np.float32)
    for lo in range(0, len(supports), chunk):
        hi = min(lo + chunk, len(supports))
        parity = np.bitwise_xor.reduce(
            x[:, padded[lo:hi]], axis=2,
            where=active[None, lo:hi], initial=0,
        )
        output += (1.0 - 2.0 * parity.astype(np.float32)) @ coefficient[lo:hi]
    return output


@torch.no_grad()
def constrained_threshold(logits: torch.Tensor, teacher_probability: torch.Tensor,
                          minimum_positive_recall: float = 0.85,
                          minimum_negative_recall: float = 0.95,
                          ) -> tuple[float, dict[str, float]]:
    """Maximize validation agreement under both recall constraints."""
    z, order = torch.sort(logits.detach().double())
    teacher = teacher_probability.detach().reshape(-1)[order] >= 0.5
    positive = teacher.sum().clamp_min(1)
    negative = (~teacher).sum().clamp_min(1)
    prefix_positive = torch.cumsum(teacher.long(), 0)
    prefix_negative = torch.cumsum((~teacher).long(), 0)
    true_positive = positive - prefix_positive
    true_negative = prefix_negative
    positive_recall = true_positive.double() / positive.double()
    negative_recall = true_negative.double() / negative.double()
    agreement = (true_positive + true_negative).double() / len(teacher)
    valid = ((positive_recall >= minimum_positive_recall)
             & (negative_recall >= minimum_negative_recall))
    valid[-1] = False
    score = torch.where(valid, agreement, torch.full_like(agreement, -1.0))
    index = int(score.argmax())
    feasible = bool(valid[index])
    if not feasible:
        violation = (
            (minimum_positive_recall - positive_recall).clamp_min(0)
            + (minimum_negative_recall - negative_recall).clamp_min(0)
        )
        index = int(torch.argmin(violation))
    threshold = float((z[index] + z[index + 1]) * 0.5)
    return threshold, {
        "threshold_feasible": float(feasible),
        "calibrated_agreement": float(agreement[index]),
        "calibrated_teacher_positive_recall": float(positive_recall[index]),
        "calibrated_teacher_negative_recall": float(negative_recall[index]),
        "calibrated_balanced_agreement": 0.5 * float(
            positive_recall[index] + negative_recall[index]
        ),
        "decision_logit_threshold": threshold,
    }
