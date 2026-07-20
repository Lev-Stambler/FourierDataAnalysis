"""One directly trained bank of exact Walsh characters.

The forward pass is always the exact Boolean character. Mask gradients use a
log-space straight-through surrogate, while fixed top-k degrees keep exported
characters sparse and cheap.
"""

from __future__ import annotations

import math
from collections.abc import Mapping
from typing import Any

import numpy as np
import torch
from torch import nn
from torch.utils.checkpoint import checkpoint


def _splitmix64(value: np.ndarray) -> np.ndarray:
    """Vectorized deterministic integer mixer used for prefix-stable masks."""
    value = value.astype(np.uint64, copy=False)
    value = value + np.uint64(0x9E3779B97F4A7C15)
    value = (value ^ (value >> np.uint64(30))) * np.uint64(0xBF58476D1CE4E5B9)
    value = (value ^ (value >> np.uint64(27))) * np.uint64(0x94D049BB133111EB)
    return value ^ (value >> np.uint64(31))


def deterministic_mask_rows(n_bits: int, terms: int, max_degree: int = 8,
                            seed: int = 0) -> tuple[np.ndarray, np.ndarray]:
    """Build a deterministic bank whose prefixes do not depend on ``terms``."""
    if not 1 <= max_degree < n_bits or terms <= 0:
        raise ValueError("invalid mask bank shape")
    rows = np.full((terms, max_degree), n_bits, dtype=np.uint32)
    degrees = np.ones(terms, dtype=np.uint8)
    covered = min(n_bits, terms)
    rows[:covered, 0] = np.arange(covered, dtype=np.uint32)
    if covered == terms:
        return rows, degrees

    row_ids = np.arange(covered, terms, dtype=np.uint64)
    seed_degree = np.uint64((seed * 0xD1B54A32D192ED03) & ((1 << 64) - 1))
    seed_index = np.uint64((seed * 0xA24BAED4963EE407) & ((1 << 64) - 1))
    degrees[covered:] = (2 + (_splitmix64(
        row_ids + seed_degree
    ) % np.uint64(max_degree - 1))).astype(np.uint8)
    chosen = np.empty((len(row_ids), max_degree), dtype=np.uint32)
    base = row_ids ^ seed_index
    for column in range(max_degree):
        column_salt = np.uint64(
            (column * 0x9E3779B97F4A7C15) & ((1 << 64) - 1)
        )
        state = _splitmix64(base + column_salt)
        candidate = (state % np.uint64(n_bits)).astype(np.uint32)
        if column:
            collision = (candidate[:, None] == chosen[:, :column]).any(axis=1)
            attempt = 0
            while collision.any():
                attempt += 1
                state[collision] = _splitmix64(
                    state[collision] + np.uint64(attempt)
                )
                candidate[collision] = (
                    state[collision] % np.uint64(n_bits)
                ).astype(np.uint32)
                collision = (
                    candidate[:, None] == chosen[:, :column]
                ).any(axis=1)
        chosen[:, column] = candidate
    active = np.arange(max_degree)[None, :] < degrees[covered:, None]
    rows[covered:] = np.where(active, chosen, n_bits)
    rows[covered:].sort(axis=1)
    return rows, degrees


def hard_topk_mask(theta: torch.Tensor, degree: torch.Tensor,
                   max_degree: int) -> torch.Tensor:
    """Return fixed-degree hard masks from score rows."""
    indices = theta.topk(max_degree, dim=1, sorted=True).indices
    active = torch.arange(max_degree, device=theta.device)[None, :] < degree[:, None]
    return torch.zeros_like(theta, dtype=torch.bool).scatter(1, indices, active)


def logspace_ste_characters(bits: torch.Tensor, theta: torch.Tensor,
                            degree: torch.Tensor, max_degree: int,
                            epsilon: float = 1e-3) -> torch.Tensor:
    """Exact hard characters with the log-magnitude surrogate gradient."""
    probability = torch.sigmoid(theta.float())
    factor = (1.0 - 2.0 * probability).abs().clamp_min(epsilon)
    log_magnitude = bits @ torch.log(factor).t()
    hard = hard_topk_mask(theta, degree, max_degree).to(bits.dtype)
    parity = torch.remainder(bits @ hard.t(), 2.0)
    sign = 1.0 - 2.0 * parity
    surrogate = sign.detach() * torch.exp(log_magnitude.clamp(-30.0, 0.0))
    return surrogate + (sign - surrogate).detach()


def _chunk_contribution(bits: torch.Tensor, theta: torch.Tensor,
                        degree: torch.Tensor, coefficient: torch.Tensor,
                        max_degree: int, output_scale: float) -> torch.Tensor:
    characters = logspace_ste_characters(bits, theta, degree, max_degree)
    return output_scale * (characters @ coefficient.float())


def _exact_chunk_contribution(bits: torch.Tensor, theta: torch.Tensor,
                              degree: torch.Tensor,
                              coefficient: torch.Tensor,
                              max_degree: int,
                              output_scale: float) -> torch.Tensor:
    hard = hard_topk_mask(theta, degree, max_degree).to(bits.dtype)
    parity = torch.remainder(bits @ hard.t(), 2.0)
    characters = 1.0 - 2.0 * parity
    return output_scale * (characters @ coefficient.float())


class DirectWalshStudent(nn.Module):
    """A single jointly trained bank of sparse exact Walsh characters."""

    def __init__(self, n_bits: int, terms: int, *, seed: int = 0,
                 max_degree: int = 8, char_chunk: int = 4096,
                 initial_probability: float = 0.5,
                 mask_magnitude: float = 8.0,
                 checkpoint_chunks: bool = True,
                 compile_chunks: bool = False):
        super().__init__()
        if char_chunk <= 0 or mask_magnitude <= 0:
            raise ValueError("chunk and mask magnitude must be positive")
        index_rows, degrees = deterministic_mask_rows(
            n_bits, terms, max_degree=max_degree, seed=seed
        )
        theta = torch.full((terms, n_bits), -mask_magnitude, dtype=torch.float32)
        for lo in range(0, terms, char_chunk):
            hi = min(lo + char_chunk, terms)
            block = torch.from_numpy(index_rows[lo:hi].astype(np.int64))
            active = block < n_bits
            row = torch.arange(hi - lo)[:, None].expand_as(block)[active]
            theta[lo:hi][row, block[active]] = mask_magnitude
        self.theta = nn.Parameter(theta)
        self.coefficient = nn.Parameter(torch.zeros(terms, dtype=torch.float32))
        p = float(np.clip(initial_probability, 1e-5, 1.0 - 1e-5))
        self.bias = nn.Parameter(torch.tensor(math.log(p / (1.0 - p))))
        self.register_buffer("degree", torch.from_numpy(degrees.astype(np.int64)))
        self.n_bits = int(n_bits)
        self.terms = int(terms)
        self.max_degree = int(max_degree)
        self.char_chunk = int(char_chunk)
        self.output_scale = terms ** -0.5
        self.checkpoint_chunks = bool(checkpoint_chunks)
        chunk_function = _chunk_contribution
        if compile_chunks and torch.cuda.is_available():
            chunk_function = torch.compile(
                chunk_function, mode="max-autotune-no-cudagraphs",
                fullgraph=True, dynamic=False,
            )
        self._chunk_function = chunk_function

    def forward(self, bits: torch.Tensor) -> torch.Tensor:
        if bits.ndim != 2 or bits.shape[1] != self.n_bits:
            raise ValueError(
                f"expected bits [batch,{self.n_bits}], got {tuple(bits.shape)}"
            )
        bits = bits.float()
        output = self.bias.expand(len(bits)).float()
        for lo in range(0, self.terms, self.char_chunk):
            hi = min(lo + self.char_chunk, self.terms)
            arguments = (
                bits, self.theta[lo:hi], self.degree[lo:hi],
                self.coefficient[lo:hi], self.max_degree, self.output_scale,
            )
            if self.training:
                contribution = (
                    checkpoint(
                        self._chunk_function, *arguments, use_reentrant=False
                    ) if self.checkpoint_chunks else
                    self._chunk_function(*arguments)
                )
            else:
                contribution = _exact_chunk_contribution(*arguments)
            output = output + contribution
        return output

    @torch.no_grad()
    def hard_index_rows(self) -> torch.Tensor:
        """Return canonical sentinel-padded coordinate tuples."""
        rows = []
        for lo in range(0, self.terms, self.char_chunk):
            hi = min(lo + self.char_chunk, self.terms)
            ranked = self.theta[lo:hi].topk(
                self.max_degree, dim=1, sorted=True
            ).indices
            active = (
                torch.arange(self.max_degree, device=self.theta.device)[None, :]
                < self.degree[lo:hi, None]
            )
            rows.append(
                ranked.masked_fill(~active, self.n_bits).sort(dim=1).values.cpu()
            )
        return torch.cat(rows)

    @torch.no_grad()
    def sparse_state(self) -> dict[str, Any]:
        rows = self.hard_index_rows().numpy().astype(np.uint32)
        valid = rows < self.n_bits
        degrees = valid.sum(axis=1, dtype=np.int32)
        return {
            "n_bits": self.n_bits,
            "degrees": degrees.astype(np.uint8),
            "indices": rows[valid].astype(np.uint16),
            "coefficient": (
                self.coefficient.detach().float().cpu().numpy()
                * self.output_scale
            ).astype(np.float32),
            "bias": float(self.bias.detach()),
        }


def sampled_diversity_loss(model: DirectWalshStudent, sample_size: int = 1024,
                           margin: float = 0.8,
                           temperature: float = 0.5,
                           generator: torch.Generator | None = None,
                           report: bool = True,
                           ) -> tuple[torch.Tensor, dict[str, float]]:
    """Penalize high soft-Jaccard overlap among a random character subset."""
    if sample_size < 2 or not 0.0 <= margin < 1.0 or temperature <= 0:
        raise ValueError("invalid diversity configuration")
    count = min(model.terms, sample_size)
    sampled = (
        torch.arange(model.terms, device=model.theta.device)
        if count == model.terms else
        torch.randint(
            model.terms, (count + 16,), device=model.theta.device,
            generator=generator,
        ).unique()[:count]
    )
    if len(sampled) < 2:
        return model.theta.sum() * 0.0, {
            "pair_count": 0.0, "mean_jaccard": 0.0,
            "p95_jaccard": 0.0, "max_jaccard": 0.0,
        }
    theta = model.theta[sampled]
    degree = model.degree[sampled]
    ranked_values, ranked_indices = theta.topk(
        model.max_degree + 1, dim=1, sorted=True
    )
    selected = ranked_indices[:, :model.max_degree]
    active = (
        torch.arange(model.max_degree, device=theta.device)[None, :]
        < degree[:, None]
    )
    hard = torch.zeros_like(theta).scatter(1, selected, active.to(theta.dtype))
    row = torch.arange(len(theta), device=theta.device)
    lower = ranked_values[row, degree - 1]
    upper = ranked_values[row, degree]
    threshold = (lower + upper).mul(0.5).detach()
    soft = torch.sigmoid((theta - threshold[:, None]) / temperature)
    relaxed = hard + soft - soft.detach()
    intersection = relaxed @ relaxed.t()
    cardinality = degree.to(theta.dtype)
    union = cardinality[:, None] + cardinality[None, :] - intersection
    jaccard = intersection / union.clamp_min(1e-6)
    pair_mask = torch.triu(
        torch.ones_like(jaccard, dtype=torch.bool), diagonal=1
    )
    pairs = jaccard[pair_mask]
    loss = torch.relu(pairs - margin).square().mean()
    if not report:
        return loss, {}
    detached = pairs.detach().float()
    return loss, {
        "pair_count": float(len(detached)),
        "mean_jaccard": float(detached.mean()),
        "p95_jaccard": float(torch.quantile(detached, 0.95)),
        "max_jaccard": float(detached.max()),
    }


@torch.no_grad()
def repair_duplicate_masks(model: DirectWalshStudent,
                           optimizer: torch.optim.Optimizer,
                           seed: int) -> dict[str, int | float]:
    """Repair exact duplicates without changing the represented function."""
    rows = model.hard_index_rows().numpy().astype(np.uint32)
    _, inverse, counts = np.unique(
        rows, axis=0, return_inverse=True, return_counts=True
    )
    duplicate_groups = np.flatnonzero(counts > 1)
    duplicates = int((counts[duplicate_groups] - 1).sum())
    if not duplicates:
        return {"duplicates_before": 0, "duplicates_repaired": 0,
                "unique_after": model.terms}
    coefficient = model.coefficient.detach().cpu().numpy()
    used = {tuple(row.tolist()) for row in rows}
    rng = np.random.default_rng(seed)
    repaired_rows: list[int] = []
    keepers: list[int] = []
    replacements: list[np.ndarray] = []
    for group in duplicate_groups:
        members = np.flatnonzero(inverse == group)
        keeper = int(members[np.argmax(np.abs(coefficient[members]))])
        model.coefficient[keeper] = float(coefficient[members].sum())
        keepers.append(keeper)
        for duplicate in members:
            duplicate = int(duplicate)
            if duplicate == keeper:
                continue
            model.coefficient[duplicate] = 0.0
            degree = int(model.degree[duplicate])
            while True:
                selected = np.sort(
                    rng.choice(model.n_bits, degree, replace=False)
                ).astype(np.uint32)
                candidate = np.full(model.max_degree, model.n_bits, dtype=np.uint32)
                candidate[:degree] = selected
                key = tuple(candidate.tolist())
                if key not in used:
                    used.add(key)
                    break
            repaired_rows.append(duplicate)
            replacements.append(selected)
    repaired = torch.as_tensor(repaired_rows, device=model.theta.device)
    model.theta[repaired] = -8.0
    for row, selected in zip(repaired_rows, replacements, strict=True):
        columns = torch.as_tensor(
            selected, dtype=torch.long, device=model.theta.device
        )
        model.theta[row, columns] = 8.0

    affected = torch.as_tensor(
        keepers + repaired_rows, device=model.theta.device
    )
    for parameter in (model.theta, model.coefficient):
        for value in optimizer.state.get(parameter, {}).values():
            if not torch.is_tensor(value) or value.ndim == 0:
                continue
            if value.shape == parameter.shape:
                value[affected] = 0
    final_rows = model.hard_index_rows().numpy()
    unique_after = len(np.unique(final_rows, axis=0))
    if unique_after != model.terms:
        raise RuntimeError("duplicate repair did not produce unique characters")
    return {
        "duplicates_before": duplicates,
        "duplicates_repaired": len(repaired_rows),
        "unique_after": unique_after,
    }


def pack_fixed_width(values: np.ndarray, bit_width: int) -> np.ndarray:
    values = np.asarray(values)
    if values.ndim != 1 or not 1 <= bit_width <= 32:
        raise ValueError("invalid fixed-width integer input")
    unsigned = values.astype(np.uint64, copy=False)
    if np.any(values < 0) or np.any(unsigned >= (1 << bit_width)):
        raise ValueError("integer does not fit bit width")
    positions = np.arange(bit_width, dtype=np.uint64)
    bits = ((unsigned[:, None] >> positions) & 1).astype(np.uint8)
    return np.packbits(bits.reshape(-1), bitorder="little")


def unpack_fixed_width(packed: np.ndarray, count: int,
                       bit_width: int) -> np.ndarray:
    raw = np.asarray(packed, dtype=np.uint8).reshape(-1)
    if len(raw) != (count * bit_width + 7) // 8:
        raise ValueError("packed byte count mismatch")
    bits = np.unpackbits(
        raw, count=count * bit_width, bitorder="little"
    ).reshape(count, bit_width)
    weights = np.left_shift(
        np.uint64(1), np.arange(bit_width, dtype=np.uint64)
    )
    return (bits.astype(np.uint64) @ weights).astype(np.uint32)


def encode_compact_student(state: Mapping[str, Any],
                           block_size: int = 256) -> dict[str, Any]:
    """Encode unique sparse characters with 12-bit indices and scaled FP16."""
    n_bits = int(state["n_bits"])
    degrees = np.asarray(state["degrees"], dtype=np.uint8)
    indices = np.asarray(state["indices"], dtype=np.uint32)
    coefficient = np.asarray(state["coefficient"], dtype=np.float32)
    if len(degrees) != len(coefficient) or int(degrees.sum()) != len(indices):
        raise ValueError("sparse character arrays disagree")
    index_bits = max(1, (n_bits - 1).bit_length())
    block_count = (len(coefficient) + block_size - 1) // block_size
    scales = np.ones(block_count, dtype=np.float32)
    quantized = np.empty(len(coefficient), dtype=np.float16)
    for block in range(block_count):
        lo, hi = block * block_size, min((block + 1) * block_size, len(coefficient))
        scale = float(np.max(np.abs(coefficient[lo:hi]), initial=0.0))
        if scale > 0:
            scales[block] = scale
            quantized[lo:hi] = (coefficient[lo:hi] / scale).astype(np.float16)
        else:
            quantized[lo:hi] = 0
    return {
        "n_bits": n_bits,
        "degrees": degrees,
        "index_bits": index_bits,
        "index_count": len(indices),
        "packed_indices": pack_fixed_width(indices, index_bits),
        "coefficient_fp16": quantized,
        "coefficient_scale": scales,
        "coefficient_block_size": block_size,
        "bias": float(state["bias"]),
    }


def decode_compact_student(state: Mapping[str, Any]) -> dict[str, Any]:
    degrees = np.asarray(state["degrees"], dtype=np.uint8)
    block_size = int(state["coefficient_block_size"])
    quantized = np.asarray(state["coefficient_fp16"], dtype=np.float16)
    scales = np.asarray(state["coefficient_scale"], dtype=np.float32)
    coefficient = quantized.astype(np.float32) * np.repeat(
        scales, block_size
    )[:len(quantized)]
    return {
        "n_bits": int(state["n_bits"]),
        "degrees": degrees,
        "indices": unpack_fixed_width(
            state["packed_indices"], int(state["index_count"]),
            int(state["index_bits"]),
        ),
        "coefficient": coefficient,
        "bias": float(state["bias"]),
    }


def sparse_logits(bits: np.ndarray, state: Mapping[str, Any],
                  chunk: int = 4096) -> np.ndarray:
    """CPU reference inference for sparse or compact state."""
    if "packed_indices" in state:
        state = decode_compact_student(state)
    x = np.asarray(bits, dtype=np.uint8)
    degree = np.asarray(state["degrees"], dtype=np.int64)
    indices = np.asarray(state["indices"], dtype=np.int64)
    coefficient = np.asarray(state["coefficient"], dtype=np.float32)
    offsets = np.concatenate(([0], np.cumsum(degree)))
    width = int(degree.max(initial=0))
    padded = np.zeros((len(degree), width), dtype=np.int64)
    active = np.arange(width)[None, :] < degree[:, None]
    rows = np.repeat(np.arange(len(degree)), degree)
    columns = np.arange(len(indices)) - np.repeat(offsets[:-1], degree)
    padded[rows, columns] = indices
    output = np.full(len(x), float(state["bias"]), dtype=np.float32)
    for lo in range(0, len(degree), chunk):
        hi = min(lo + chunk, len(degree))
        parity = np.bitwise_xor.reduce(
            x[:, padded[lo:hi]], axis=2,
            where=active[None, lo:hi], initial=0,
        )
        output += (1.0 - 2.0 * parity.astype(np.float32)) @ coefficient[lo:hi]
    return output


def binary_metrics(logits: np.ndarray | torch.Tensor,
                   teacher_probability: np.ndarray | torch.Tensor,
                   ) -> dict[str, float]:
    z = torch.as_tensor(logits, dtype=torch.float64).reshape(-1)
    target = torch.as_tensor(
        teacher_probability, dtype=torch.float64
    ).reshape(-1).clamp(1e-8, 1.0 - 1e-8)
    probability = torch.sigmoid(z)
    residual = probability - target
    absolute = residual.abs()
    ce = torch.nn.functional.binary_cross_entropy_with_logits(z, target)
    entropy = torch.nn.functional.binary_cross_entropy(target, target)
    teacher_hard = target >= 0.5
    student_hard = probability >= 0.5
    positive = teacher_hard.sum().clamp_min(1)
    negative = (~teacher_hard).sum().clamp_min(1)
    teacher_variance = (target - target.mean()).square().sum()

    def cosine(left: torch.Tensor, right: torch.Tensor) -> float:
        denominator = left.norm() * right.norm()
        return float(torch.dot(left, right) / denominator.clamp_min(1e-12))

    quantiles = torch.quantile(
        absolute, torch.tensor([0.5, 0.9, 0.95, 0.99], dtype=torch.float64)
    )
    result = {
        "ce": float(ce),
        "kl": float(ce - entropy),
        "mae": float(absolute.mean()),
        "rmse": float(residual.square().mean().sqrt()),
        "r_squared": 1.0 - float(
            residual.square().sum() / teacher_variance.clamp_min(1e-12)
        ),
        "probability_cosine": cosine(target, probability),
        "centered_probability_cosine": cosine(
            target - target.mean(), probability - probability.mean()
        ),
        "centered_logit_cosine": cosine(
            torch.logit(target) - torch.logit(target).mean(), z - z.mean()
        ),
        "agreement": float((teacher_hard == student_hard).double().mean()),
        "teacher_positive_recall": float(
            (teacher_hard & student_hard).sum() / positive
        ),
        "teacher_negative_recall": float(
            ((~teacher_hard) & (~student_hard)).sum() / negative
        ),
        "absolute_error_max": float(absolute.max()),
        "absolute_error_p50": float(quantiles[0]),
        "absolute_error_p90": float(quantiles[1]),
        "absolute_error_p95": float(quantiles[2]),
        "absolute_error_p99": float(quantiles[3]),
        "residual_mean": float(residual.mean()),
        "residual_variance": float(residual.var(unbiased=False)),
    }
    confidence = torch.maximum(target, 1.0 - target)
    for name, lower, upper in (
        ("50_60", 0.50, 0.60), ("60_75", 0.60, 0.75),
        ("75_90", 0.75, 0.90), ("90_100", 0.90, 1.01),
    ):
        selected = (confidence >= lower) & (confidence < upper)
        result[f"mae_confidence_{name}"] = (
            float(absolute[selected].mean()) if selected.any() else 0.0
        )
    return result
