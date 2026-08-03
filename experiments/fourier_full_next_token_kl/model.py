"""Exact input/output Walsh student for full next-token KL distillation.

The student represents a scalar energy ``f(x, y)`` on input bits ``x`` and
output-token bits ``y``.  Every character contains nonempty hard-TopK input
and output supports.  Both supports are learned with exact discrete forwards
and product-vertex straight-through gradients; output-independent characters
are excluded because they cancel under the vocabulary softmax.

Output tokens use a collision-free 18-bit code.  This makes the complete
``2**18`` output cube only slightly wider than Qwen's 248,077-token alphabet.
Sparse output Fourier coefficients can therefore be evaluated for every token
with a batched fast Walsh-Hadamard transform instead of a terms-by-vocabulary
matrix multiplication.
"""

from __future__ import annotations

import itertools
import hashlib
import math
from collections.abc import Iterable, Mapping
from typing import Any

import numpy as np
import torch
from torch import nn
from torch.utils.checkpoint import checkpoint


ARTIFACT_SCHEMA = "full-next-token-input-output-walsh-v3"


def _splitmix64(value: np.ndarray | np.uint64) -> np.ndarray:
    value = np.asarray(value, dtype=np.uint64)
    with np.errstate(over="ignore"):
        value = value + np.uint64(0x9E3779B97F4A7C15)
        value = ((value ^ (value >> np.uint64(30)))
                 * np.uint64(0xBF58476D1CE4E5B9))
        value = ((value ^ (value >> np.uint64(27)))
                 * np.uint64(0x94D049BB133111EB))
    return value ^ (value >> np.uint64(31))


def unique_output_vertices(token_codes: np.ndarray, output_bits: int = 18,
                           seed: int = 0) -> np.ndarray:
    """Truncate semantic token codes and deterministically repair collisions."""
    codes = np.asarray(token_codes, dtype=np.uint8)
    if codes.ndim != 2 or not 1 <= output_bits <= codes.shape[1] or output_bits > 24:
        raise ValueError("invalid token-code/output-bit shape")
    if len(codes) > 1 << output_bits:
        raise ValueError("the output cube is smaller than the vocabulary")
    weights = np.left_shift(
        np.uint64(1), np.arange(output_bits, dtype=np.uint64)
    )
    original = codes[:, :output_bits].astype(np.uint64) @ weights
    result = np.empty(len(codes), dtype=np.uint32)
    occupied = np.zeros(1 << output_bits, dtype=bool)
    collision_rows: list[int] = []
    for token_id, raw in enumerate(original):
        candidate = int(raw)
        if occupied[candidate]:
            collision_rows.append(token_id)
        else:
            occupied[candidate] = True
            result[token_id] = candidate
    free = np.flatnonzero(~occupied).astype(np.uint32)
    if len(collision_rows) > len(free):
        raise RuntimeError("output-code repair ran out of free vertices")
    order = np.random.default_rng(seed ^ 0xC0111DE).permutation(len(free))
    result[np.asarray(collision_rows, dtype=np.int64)] = free[
        order[:len(collision_rows)]
    ]
    if len(np.unique(result)) != len(codes):
        raise RuntimeError("output-code collision repair failed")
    return result


def balanced_projection_vertices(projection_scores: np.ndarray,
                                 output_bits: int = 18) -> np.ndarray:
    """Build unique semantic vertices by recursively balanced projections.

    At depth ``d``, every existing cell is split at its median using projection
    ``d``.  After ``ceil(log2(vocab))`` depths there is at most one token per
    cell, without the arbitrary far-away repairs caused by LSH truncation.
    """
    scores = np.asarray(projection_scores, dtype=np.float32)
    if (scores.ndim != 2 or not len(scores)
            or scores.shape[1] < output_bits or output_bits > 24
            or len(scores) > 1 << output_bits
            or not np.isfinite(scores[:, :output_bits]).all()):
        raise ValueError("invalid projection scores/output-bit shape")
    vertices = np.zeros(len(scores), dtype=np.uint32)
    for depth in range(output_bits):
        # Sort by current cell first and the next semantic projection second.
        order = np.lexsort((scores[:, depth], vertices))
        grouped = vertices[order]
        starts = np.concatenate((
            np.asarray([0], dtype=np.int64),
            np.flatnonzero(grouped[1:] != grouped[:-1]).astype(np.int64) + 1,
        ))
        ends = np.concatenate((starts[1:], np.asarray([len(scores)])))
        sizes = ends - starts
        midpoint = starts + (sizes + 1) // 2
        group_midpoint = np.repeat(midpoint, sizes)
        right = np.arange(len(scores), dtype=np.int64) >= group_midpoint
        vertices[order[right]] |= np.uint32(1 << depth)
    if len(np.unique(vertices)) != len(vertices):
        raise RuntimeError("balanced semantic output partition is not injective")
    return vertices


def validate_balanced_token_artifact(
    packed_codes: np.ndarray,
    output_vertices: np.ndarray,
    metadata: Mapping[str, Any],
    *,
    model_revision: str,
    vocab_size: int,
    raw_logit_width: int,
    input_bits: int = 32,
    output_bits: int = 18,
    seed: int = 0,
) -> None:
    """Fail closed unless a codebook is the pinned balanced semantic artifact."""
    packed = np.asarray(packed_codes, dtype=np.uint8)
    vertices = np.asarray(output_vertices, dtype=np.uint32)
    expected = {
        "model_revision": model_revision,
        "vocab_size": vocab_size,
        "raw_logit_width": raw_logit_width,
        "lsh_bits": input_bits,
        "output_bits": output_bits,
        "seed": seed,
        "output_vertex_scheme": "balanced_recursive_embedding_projections",
    }
    if any(metadata.get(key) != value for key, value in expected.items()):
        raise ValueError("balanced token artifact metadata mismatch")
    if packed.shape != (vocab_size, (input_bits + 7) // 8):
        raise ValueError("balanced token artifact input-code shape mismatch")
    if (vertices.shape != (vocab_size,)
            or int(vertices.min(initial=0)) < 0
            or int(vertices.max(initial=0)) >= 1 << output_bits
            or len(np.unique(vertices)) != vocab_size):
        raise ValueError("balanced token artifact output vertices are not injective")
    if len(np.unique(packed, axis=0)) != vocab_size:
        raise ValueError("balanced token artifact input codes are not injective")
    packed_sha = hashlib.sha256(packed.tobytes()).hexdigest()
    vertex_sha = hashlib.sha256(vertices.tobytes()).hexdigest()
    if (metadata.get("packed_sha256") != packed_sha
            or metadata.get("output_vertices_sha256") != vertex_sha):
        raise ValueError("balanced token artifact hash mismatch")


def _degree_cycle(max_degree: int) -> np.ndarray:
    weights = {2: 30, 3: 25, 4: 20, 5: 10, 6: 7, 7: 5}
    return np.asarray([
        degree for degree, count in weights.items()
        if degree <= max_degree for _ in range(count)
    ], dtype=np.uint8)


def deterministic_input_supports(
    n_bits: int,
    terms: int,
    *,
    max_degree: int = 7,
    seed: int = 0,
    support_layout: str = "uniform",
    token_bits: int = 32,
) -> tuple[np.ndarray, np.ndarray]:
    """Create unique, prefix-stable input supports for contextual terms."""
    if terms == 0:
        return (np.empty((0, max_degree), dtype=np.uint32),
                np.empty(0, dtype=np.uint8))
    if not 1 <= max_degree < n_bits or terms < 0:
        raise ValueError("invalid input support-bank shape")
    if support_layout not in {"uniform", "causal_token_structured"}:
        raise ValueError(f"unknown support layout {support_layout!r}")
    if support_layout == "causal_token_structured" and n_bits % token_bits:
        raise ValueError("token-structured supports require whole token fields")
    capacity = sum(math.comb(n_bits, d) for d in range(1, max_degree + 1))
    if terms > capacity:
        raise ValueError("requested more unique supports than the bank holds")

    rows = np.full((terms, max_degree), n_bits, dtype=np.uint32)
    degrees = np.ones(terms, dtype=np.uint8)
    singleton_count = min(n_bits, terms)
    rows[:singleton_count, 0] = np.arange(singleton_count, dtype=np.uint32)
    if singleton_count == terms:
        return rows, degrees
    cycle = _degree_cycle(max_degree)
    if not len(cycle):
        raise ValueError("more than n_bits degree-one terms need max_degree >= 2")
    degrees[singleton_count:] = np.resize(cycle, terms - singleton_count)
    rng = np.random.default_rng(seed ^ 0x51A7C7)
    token_count = n_bits // token_bits
    recent_tokens = np.arange(max(0, token_count - 16), token_count)
    used = {tuple(map(int, rows[i, :int(degrees[i])]))
            for i in range(singleton_count)}

    def token_pool(token: int) -> np.ndarray:
        return np.arange(token * token_bits, (token + 1) * token_bits,
                         dtype=np.uint32)

    strata = (
        "latest_local", "recent_cross", "recent_local", "recent_cross",
        "global", "latest_local", "recent_cross", "distant_cross",
        "recent_local", "recent_cross",
    )
    for row in range(singleton_count, terms):
        degree = int(degrees[row])
        attempts = 0
        while True:
            attempts += 1
            # Small local strata (especially degree-two characters inside the
            # final 32-bit token) eventually exhaust their combinatorial
            # capacity.  Fall back promptly instead of spending 128 doomed
            # rejection samples per row at large model sizes.
            if support_layout == "uniform" or attempts > 8:
                support = np.sort(
                    rng.choice(n_bits, degree, replace=False)
                ).astype(np.uint32)
            else:
                stratum = strata[(row - singleton_count) % len(strata)]
                if stratum == "latest_local":
                    support = np.sort(rng.choice(
                        token_pool(token_count - 1), degree, replace=False
                    ))
                elif stratum == "recent_local":
                    token = int(rng.choice(recent_tokens))
                    support = np.sort(rng.choice(
                        token_pool(token), degree, replace=False
                    ))
                elif stratum == "recent_cross":
                    chosen_tokens = rng.choice(
                        recent_tokens, min(degree, len(recent_tokens)), replace=False
                    )
                    parts = [int(token) * token_bits + int(rng.integers(token_bits))
                             for token in chosen_tokens]
                    while len(parts) < degree:
                        candidate = int(rng.choice(recent_tokens)) * token_bits + int(
                            rng.integers(token_bits)
                        )
                        if candidate not in parts:
                            parts.append(candidate)
                    support = np.sort(np.asarray(parts, dtype=np.uint32))
                elif stratum == "distant_cross" and token_count > len(recent_tokens):
                    chosen_tokens = rng.choice(
                        np.arange(token_count - len(recent_tokens)),
                        min(degree, token_count - len(recent_tokens)), replace=False,
                    )
                    parts = [int(token) * token_bits + int(rng.integers(token_bits))
                             for token in chosen_tokens]
                    while len(parts) < degree:
                        candidate = int(rng.integers(n_bits))
                        if candidate not in parts:
                            parts.append(candidate)
                    support = np.sort(np.asarray(parts, dtype=np.uint32))
                else:
                    support = np.sort(
                        rng.choice(n_bits, degree, replace=False)
                    ).astype(np.uint32)
            key = tuple(map(int, support))
            if key not in used:
                used.add(key)
                rows[row, :degree] = support
                break
    return rows, degrees


def _frequency_bank(output_bits: int, max_degree: int, count: int,
                    seed: int) -> np.ndarray:
    available = [
        sum(1 << bit for bit in support)
        for degree in range(1, min(output_bits, max_degree) + 1)
        for support in itertools.combinations(range(output_bits), degree)
    ]
    if count > len(available):
        raise ValueError("not enough nonconstant output characters")
    order = np.random.default_rng(seed ^ 0x0A17B17).permutation(len(available))
    return np.asarray(available, dtype=np.uint32)[order[:count]]


def deterministic_input_output_supports(
    n_input_bits: int,
    terms: int,
    *,
    output_bits: int = 18,
    max_total_degree: int = 8,
    unigram_terms: int = 0,
    seed: int = 0,
    support_layout: str = "uniform",
    token_bits: int = 32,
) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """Return input supports/degrees and a nonzero output frequency per term."""
    if not 0 <= unigram_terms <= terms or max_total_degree < 2:
        raise ValueError("invalid term split or total degree")
    if output_bits <= 0:
        raise ValueError("output_bits must be positive")
    output_frequency = np.empty(terms, dtype=np.uint32)
    if unigram_terms:
        output_frequency[:unigram_terms] = _frequency_bank(
            output_bits, max_total_degree, unigram_terms, seed
        )
    contextual = terms - unigram_terms
    rows = np.full((terms, max_total_degree - 1), n_input_bits, dtype=np.uint32)
    input_degree = np.zeros(terms, dtype=np.uint8)

    if support_layout == "hashed":
        # Production initialization: make all supports in a few vectorized
        # passes.  Exact duplicates are harmless and are merged by the
        # training loop; spending minutes rejecting them here defeats the
        # point of learning the supports with STEs.
        cycle = _degree_cycle(max_total_degree - 1)
        if contextual and not len(cycle):
            raise ValueError("hashed supports need max_total_degree >= 3")
        local = np.arange(contextual, dtype=np.uint64)
        contextual_degree = np.resize(cycle, contextual)
        contextual_rows = np.full(
            (contextual, max_total_degree - 1), n_input_bits,
            dtype=np.uint32,
        )
        for column in range(max_total_degree - 1):
            candidate = (
                _splitmix64(local + np.uint64(seed + 0x9E37 * (column + 1)))
                % np.uint64(n_input_bits)
            ).astype(np.uint32)
            for _ in range(column):
                collision = np.any(
                    candidate[:, None] == contextual_rows[:, :column], axis=1
                )
                candidate[collision] = (
                    candidate[collision] + np.uint32(1)
                ) % np.uint32(n_input_bits)
            active = column < contextual_degree
            contextual_rows[active, column] = candidate[active]
        contextual_rows.sort(axis=1)
        rows[unigram_terms:] = contextual_rows
        input_degree[unigram_terms:] = contextual_degree

        output_degree = 1 + (
            np.arange(contextual, dtype=np.uint32)
            % (max_total_degree - contextual_degree).astype(np.uint32)
        )
        output_frequency[unigram_terms:] = 0
        chosen = np.full(
            (contextual, max_total_degree - 1), output_bits,
            dtype=np.uint32,
        )
        for column in range(max_total_degree - 1):
            candidate = (
                _splitmix64(local + np.uint64(seed + 0xB529 * (column + 1)))
                % np.uint64(output_bits)
            ).astype(np.uint32)
            for _ in range(column):
                collision = np.any(
                    candidate[:, None] == chosen[:, :column], axis=1
                )
                candidate[collision] = (
                    candidate[collision] + np.uint32(1)
                ) % np.uint32(output_bits)
            active = column < output_degree
            chosen[active, column] = candidate[active]
            output_frequency[unigram_terms:][active] |= np.left_shift(
                np.uint32(1), candidate[active]
            )
    elif support_layout == "hashed_low_degree":
        # Capacity-focused low-degree initializer: deterministic fixed input
        # degree mix 50% degree 1, 35% degree 2, 15% degree 3.  STEs still
        # learn the selected input/output bits; the input degree is fixed and
        # exact duplicate merge/recycle remains unchanged.  Output degree is
        # chosen so total degree stays <= max_total_degree (valid for 8).
        if max_total_degree < 3:
            raise ValueError("hashed_low_degree needs max_total_degree >= 3")
        max_input_degree = min(3, max_total_degree - 1)
        # Deterministic fixed mix proportions, tiled to contextual length.
        mix_counts = np.array([0.50, 0.35, 0.15], dtype=np.float64)
        mix_degrees = np.array([1, 2, 3], dtype=np.uint8)
        mix_degrees = mix_degrees[mix_degrees <= max_input_degree]
        mix_counts = mix_counts[:len(mix_degrees)]
        mix_counts = mix_counts / mix_counts.sum()
        cut = np.cumsum(mix_counts) * contextual
        counts = np.diff(np.concatenate(([0], cut.round().astype(np.int64))))
        counts[-1] = contextual - counts[:-1].sum()
        contextual_degree = np.empty(contextual, dtype=np.uint8)
        pos = 0
        for deg, cnt in zip(mix_degrees, counts):
            contextual_degree[pos:pos+int(cnt)] = deg
            pos += int(cnt)
        # Deterministic permutation of the degree assignment keeps it stable
        # while avoiding a contiguous block of identical degrees.
        perm = np.argsort(_splitmix64(np.arange(contextual, dtype=np.uint64) + np.uint64(seed ^ 0x10D3C4B3)))
        contextual_degree = contextual_degree[perm]
        local = np.arange(contextual, dtype=np.uint64)
        contextual_rows = np.full(
            (contextual, max_total_degree - 1), n_input_bits, dtype=np.uint32,
        )
        for column in range(max_total_degree - 1):
            candidate = (
                _splitmix64(local + np.uint64(seed + 0x7A11 * (column + 1)))
                % np.uint64(n_input_bits)
            ).astype(np.uint32)
            for _ in range(column):
                collision = np.any(
                    candidate[:, None] == contextual_rows[:, :column], axis=1
                )
                candidate[collision] = (
                    candidate[collision] + np.uint32(1)
                ) % np.uint32(n_input_bits)
            active = column < contextual_degree
            contextual_rows[active, column] = candidate[active]
        contextual_rows.sort(axis=1)
        rows[unigram_terms:] = contextual_rows
        input_degree[unigram_terms:] = contextual_degree

        # Give repeated input supports different output frequencies by
        # construction.  This keeps the multi-million-row initial joint bank
        # collision-free without rejection sampling.
        output_frequency[unigram_terms:] = 0
        for degree in map(int, np.unique(contextual_degree)):
            selected = np.flatnonzero(contextual_degree == degree)
            supports = contextual_rows[selected, :degree]
            _, inverse, counts = np.unique(
                supports, axis=0, return_inverse=True, return_counts=True
            )
            maximum_output_degree = min(
                output_bits, max_total_degree - degree
            )
            bank = np.asarray([
                sum(1 << bit for bit in support)
                for output_degree in range(1, maximum_output_degree + 1)
                for support in itertools.combinations(
                    range(output_bits), output_degree
                )
            ], dtype=np.uint32)
            if int(counts.max(initial=0)) > len(bank):
                raise ValueError(
                    "hashed_low_degree joint support bucket is over capacity"
                )
            order = np.argsort(inverse, kind="stable")
            sorted_inverse = inverse[order]
            starts = np.concatenate((
                np.asarray([0], dtype=np.int64),
                np.flatnonzero(
                    sorted_inverse[1:] != sorted_inverse[:-1]
                ).astype(np.int64) + 1,
            ))
            sizes = np.diff(np.concatenate((
                starts, np.asarray([len(order)], dtype=np.int64)
            )))
            rank = np.empty(len(selected), dtype=np.int64)
            rank[order] = (
                np.arange(len(order), dtype=np.int64)
                - np.repeat(starts, sizes)
            )
            offsets = (
                _splitmix64(
                    np.arange(len(counts), dtype=np.uint64)
                    + np.uint64(seed ^ (0xC271 + degree))
                )
                % np.uint64(len(bank))
            ).astype(np.int64)
            output_frequency[unigram_terms + selected] = bank[
                (offsets[inverse] + rank) % len(bank)
            ]
    elif support_layout == "joint_cartesian":
        # Tile the useful low-degree tensor basis before inventing higher-order
        # characters.  Ordering in complete rounds keeps every prefix balanced:
        # the first 73,728 contextual rows at production shape are precisely
        # all 4,096 input singletons crossed with all 18 output singletons.
        # The next rows cross the same input bits with output bit-pairs.  This
        # is both simpler and much better conditioned than assigning only one
        # random output frequency to each unique input support.
        cursor = unigram_terms
        stop = terms

        def tile(input_rows: np.ndarray, degree: int,
                 frequencies: np.ndarray) -> None:
            nonlocal cursor
            if cursor == stop or not len(input_rows) or not len(frequencies):
                return
            count = min(stop - cursor, len(input_rows) * len(frequencies))
            local = np.arange(count, dtype=np.int64)
            support_row = local % len(input_rows)
            round_index = local // len(input_rows)
            # A deterministic offset spreads even an incomplete first round
            # across output coordinates while preserving a collision-free full
            # Cartesian product over all rounds.
            offset = (support_row * 0x9E3779B1) % len(frequencies)
            frequency_row = (round_index + offset) % len(frequencies)
            rows[cursor:cursor + count, :degree] = input_rows[support_row, :degree]
            input_degree[cursor:cursor + count] = degree
            output_frequency[cursor:cursor + count] = frequencies[frequency_row]
            cursor += count

        singleton_inputs = np.arange(
            n_input_bits, dtype=np.uint32
        )[:, None]
        singleton_outputs = np.left_shift(
            np.uint32(1), np.arange(output_bits, dtype=np.uint32)
        )
        tile(singleton_inputs, 1, singleton_outputs)
        if max_total_degree >= 3:
            pair_outputs = np.asarray([
                (1 << first) | (1 << second)
                for first, second in itertools.combinations(range(output_bits), 2)
            ], dtype=np.uint32)
            tile(singleton_inputs, 1, pair_outputs)

            if cursor < stop:
                pair_capacity = math.comb(n_input_bits, 2)
                pair_count = min(32768, pair_capacity)
                pair_rows, pair_degree = deterministic_input_supports(
                    n_input_bits, n_input_bits + pair_count, max_degree=2,
                    seed=seed ^ 0x2B17,
                    support_layout="causal_token_structured",
                    token_bits=token_bits,
                )
                pair_rows = pair_rows[n_input_bits:]
                if not np.all(pair_degree[n_input_bits:] == 2):
                    raise RuntimeError("failed to construct degree-two input bank")
                tile(pair_rows, 2, singleton_outputs)
                if max_total_degree >= 4:
                    tile(pair_rows, 2, pair_outputs)
        if cursor != stop:
            raise ValueError(
                "requested more contextual terms than the structured joint bank holds"
            )
    else:
        contextual_rows, contextual_degree = deterministic_input_supports(
            n_input_bits, contextual, max_degree=max_total_degree - 1,
            seed=seed, support_layout=support_layout, token_bits=token_bits,
        )
        rows[unigram_terms:] = contextual_rows
        input_degree[unigram_terms:] = contextual_degree
        rng = np.random.default_rng(seed ^ 0x0BADC0DE)
        for local_row, degree in enumerate(contextual_degree):
            maximum = max_total_degree - int(degree)
            output_degree = 1 + (local_row % maximum)
            support = rng.choice(output_bits, output_degree, replace=False)
            output_frequency[unigram_terms + local_row] = sum(
                1 << int(bit) for bit in support
            )
    total_degree = input_degree + np.asarray(
        [int(x).bit_count() for x in output_frequency], dtype=np.uint8
    )
    if np.any(output_frequency == 0) or np.any(total_degree > max_total_degree):
        raise RuntimeError("invalid input/output support construction")
    return rows, input_degree, output_frequency


def hard_topk_mask(theta: torch.Tensor, degree: torch.Tensor,
                   max_degree: int) -> torch.Tensor:
    indices = theta.topk(max_degree, dim=1, sorted=True).indices
    active = torch.arange(max_degree, device=theta.device)[None] < degree[:, None]
    return torch.zeros_like(theta, dtype=torch.bool).scatter(1, indices, active)


def hard_output_mask(theta: torch.Tensor, degree: torch.Tensor,
                     max_degree: int) -> torch.Tensor:
    """Hard TopK selector over the small output-bit axis."""
    return hard_topk_mask(theta, degree, max_degree)


@torch.no_grad()
def hard_support_margin(theta: torch.Tensor, degree: torch.Tensor,
                        max_degree: int) -> torch.Tensor:
    """TopK boundary margin for nonempty fixed-degree support selectors."""
    if (theta.ndim != 2 or degree.shape != (len(theta),)
            or max_degree <= 0 or theta.shape[1] <= max_degree
            or bool((degree <= 0).any()) or bool((degree > max_degree).any())):
        raise ValueError("invalid nonempty support selector")
    ranked = theta.topk(max_degree + 1, dim=1, sorted=True).values
    selected = ranked.gather(1, (degree - 1)[:, None]).squeeze(1)
    competitor = ranked.gather(1, degree[:, None]).squeeze(1)
    return selected - competitor


@torch.no_grad()
def support_flip_metrics(
    before: torch.Tensor, after: torch.Tensor,
    two_steps_ago: torch.Tensor | None = None,
) -> dict[str, float]:
    """Exact sampled hard-mask transitions for one optimizer step."""
    if (before.dtype != torch.bool or after.dtype != torch.bool
            or before.ndim != 2 or after.shape != before.shape
            or (two_steps_ago is not None
                and (two_steps_ago.dtype != torch.bool
                     or two_steps_ago.shape != before.shape))):
        raise ValueError("support masks must be matching boolean matrices")
    changed_bits = torch.logical_xor(before, after).sum(1)
    changed_rows = changed_bits > 0
    row_count = int(changed_rows.sum())
    metrics = {
        "rows_flipped": float(row_count),
        "row_flip_fraction": float(changed_rows.float().mean()),
        "bits_flipped": float(changed_bits.sum()),
        "bits_per_flipped_row": (
            float(changed_bits[changed_rows].float().mean())
            if row_count else 0.0
        ),
    }
    if two_steps_ago is not None:
        flipped_back = changed_rows & torch.all(after == two_steps_ago, dim=1)
        flip_back_count = int(flipped_back.sum())
        metrics.update({
            "rows_flipped_back": float(flip_back_count),
            "flip_back_fraction": float(flipped_back.float().mean()),
            "flip_back_fraction_of_flips": (
                flip_back_count / row_count if row_count else 0.0
            ),
        })
    return metrics


class _ExactWalshProductVertexSTE(torch.autograd.Function):
    @staticmethod
    def forward(ctx, bits: torch.Tensor, theta: torch.Tensor,
                degree: torch.Tensor, max_degree: int) -> torch.Tensor:
        mask = hard_topk_mask(theta, degree, max_degree)
        parity = torch.remainder(bits @ mask.to(bits.dtype).t(), 2.0)
        sign = 1.0 - 2.0 * parity
        ctx.save_for_backward(bits, mask, sign, degree)
        return sign

    @staticmethod
    def backward(ctx, output_gradient: torch.Tensor):
        bits, mask, sign, degree = ctx.saved_tensors
        weighted = output_gradient.float() * sign.float()
        gradient = -2.0 * (weighted.t() @ bits.float())
        gradient.mul_(1.0 - 2.0 * mask.float())
        gradient.sub_(gradient.mean(dim=1, keepdim=True))
        gradient.masked_fill_(degree[:, None] == 0, 0.0)
        return None, gradient, None, None


def exact_walsh_ste(bits: torch.Tensor, theta: torch.Tensor,
                    degree: torch.Tensor, max_degree: int) -> torch.Tensor:
    return _ExactWalshProductVertexSTE.apply(bits, theta, degree, max_degree)


class _HardFrequencyScatterSTE(torch.autograd.Function):
    """Scatter exact output characters and STE their selected output bits.

    If ``G`` is the gradient on the sparse output spectrum, the derivative for
    output bit ``d`` needs the Walsh transform of the score gradient at
    frequencies ``f`` and ``f xor 2**d``.  Those are exactly two entries of
    ``G`` after backpropagating through the FWHT, so all output-mask gradients
    cost only ``output_bits`` indexed differences.
    """

    @staticmethod
    def forward(ctx, weighted: torch.Tensor, theta: torch.Tensor,
                degree: torch.Tensor, output_bits: int,
                max_degree: int) -> torch.Tensor:
        mask = hard_output_mask(theta, degree, max_degree)
        bit_value = torch.bitwise_left_shift(
            torch.ones(output_bits, dtype=torch.long, device=theta.device),
            torch.arange(output_bits, device=theta.device),
        )
        frequency = (mask.long() * bit_value[None]).sum(1)
        spectrum = weighted.new_zeros((len(weighted), 1 << output_bits))
        spectrum.index_add_(1, frequency, weighted)
        ctx.save_for_backward(weighted, mask, frequency)
        ctx.output_bits = output_bits
        return spectrum

    @staticmethod
    def backward(ctx, spectrum_gradient: torch.Tensor):
        weighted, mask, frequency = ctx.saved_tensors
        weighted_gradient = torch.empty_like(weighted)
        theta_gradient = torch.empty_like(mask, dtype=torch.float32)
        # A full [batch, terms] selected/neighbor/product triple is tens of
        # GiB at capacity.  Stream the term axis while producing the one full
        # weighted gradient that autograd actually requires.
        term_chunk = 262_144
        for lo in range(0, len(frequency), term_chunk):
            hi = min(lo + term_chunk, len(frequency))
            chunk_frequency = frequency[lo:hi]
            selected = spectrum_gradient.index_select(1, chunk_frequency)
            weighted_gradient[:, lo:hi] = selected
            for bit in range(ctx.output_bits):
                neighbor = spectrum_gradient.index_select(
                    1, torch.bitwise_xor(chunk_frequency, 1 << bit)
                )
                # -2*y_d*chi_f = -(chi_f - chi_{f xor e_d}).
                theta_gradient[lo:hi, bit] = -(
                    weighted[:, lo:hi].float()
                    * (selected - neighbor).float()
                ).sum(0) * (
                    1.0 - 2.0 * mask[lo:hi, bit].float()
                )
        theta_gradient.sub_(theta_gradient.mean(1, keepdim=True))
        return weighted_gradient, theta_gradient, None, None, None


def hard_frequency_scatter_ste(weighted: torch.Tensor, theta: torch.Tensor,
                               degree: torch.Tensor, output_bits: int,
                               max_degree: int) -> torch.Tensor:
    return _HardFrequencyScatterSTE.apply(
        weighted, theta, degree, output_bits, max_degree
    )


def _weighted_character_chunk(bits: torch.Tensor, theta: torch.Tensor,
                              degree: torch.Tensor, coefficient: torch.Tensor,
                              max_degree: int, scale: float) -> torch.Tensor:
    return exact_walsh_ste(bits, theta, degree, max_degree).float() * (
        coefficient.float() * scale
    )[None]


def _exact_weighted_character_chunk(
    bits: torch.Tensor, theta: torch.Tensor, degree: torch.Tensor,
    coefficient: torch.Tensor, max_degree: int, scale: float,
) -> torch.Tensor:
    mask = hard_topk_mask(theta, degree, max_degree).to(bits.dtype)
    parity = torch.remainder(bits @ mask.t(), 2.0)
    return (1.0 - 2.0 * parity).float() * (coefficient.float() * scale)[None]


def walsh_synthesis(spectrum: torch.Tensor) -> torch.Tensor:
    """Unnormalized Walsh synthesis along the final, power-of-two axis."""
    if spectrum.ndim < 1:
        raise ValueError("spectrum needs at least one dimension")
    width = spectrum.shape[-1]
    if width <= 0 or width & (width - 1):
        raise ValueError("Walsh width must be a positive power of two")
    value = spectrum
    block = 1
    while block < width:
        shape = (*value.shape[:-1], -1, 2, block)
        paired = value.reshape(shape)
        left, right = paired[..., 0, :], paired[..., 1, :]
        value = torch.cat((left + right, left - right), dim=-1).reshape_as(value)
        block *= 2
    return value


class InputOutputWalshStudent(nn.Module):
    """Sparse Walsh energy ``f(input_bits, output_token_bits)``."""

    def __init__(
        self, n_input_bits: int, token_vertices: np.ndarray | torch.Tensor,
        terms: int, *, output_bits: int = 18, unigram_terms: int = 0,
        seed: int = 0, max_total_degree: int = 8, char_chunk: int = 1024,
        support_layout: str = "uniform", token_bits: int = 32,
        initial_score_gap: float = 0.02, coefficient_std: float = 0.02,
        checkpoint_chunks: bool = True, compile_chunks: bool = False,
        compile_transforms: bool = False,
        parity_dtype: torch.dtype = torch.float32,
        output_frequency_override: np.ndarray | torch.Tensor | None = None,
    ):
        super().__init__()
        if char_chunk <= 0 or initial_score_gap <= 0 or coefficient_std < 0:
            raise ValueError("invalid model initialization")
        vertices = torch.as_tensor(token_vertices, dtype=torch.long)
        if (vertices.ndim != 1 or not len(vertices)
                or int(vertices.min()) < 0 or int(vertices.max()) >= 1 << output_bits
                or len(torch.unique(vertices)) != len(vertices)):
            raise ValueError("token vertices must be unique members of the output cube")
        rows, input_degree, output_frequency = deterministic_input_output_supports(
            n_input_bits, terms, output_bits=output_bits,
            max_total_degree=max_total_degree, unigram_terms=unigram_terms,
            seed=seed, support_layout=support_layout, token_bits=token_bits,
        )
        if output_frequency_override is not None:
            replacement = np.asarray(
                torch.as_tensor(output_frequency_override).cpu(), dtype=np.uint32
            ).reshape(-1)
            if (len(replacement) != terms or np.any(replacement == 0)
                    or np.any(replacement >= 1 << output_bits)):
                raise ValueError("output frequency override must contain one nonzero cube frequency per term")
            output_frequency = replacement
        max_input_degree = max_total_degree - 1
        theta = torch.zeros((terms, n_input_bits), dtype=torch.float32)
        for lo in range(0, terms, char_chunk):
            hi = min(lo + char_chunk, terms)
            block_rows = torch.from_numpy(rows[lo:hi].astype(np.int64))
            active = block_rows < n_input_bits
            rr = torch.arange(hi - lo)[:, None].expand_as(block_rows)[active]
            theta[lo:hi][rr, block_rows[active]] = initial_score_gap
        generator = torch.Generator().manual_seed(seed + 17)
        coefficient = torch.randn(terms, generator=generator) * coefficient_std
        output_degree = np.asarray(
            [int(value).bit_count() for value in output_frequency], dtype=np.int64
        )
        max_output_degree = int(output_degree.max(initial=1))
        output_theta = torch.zeros((terms, output_bits), dtype=torch.float32)
        for bit in range(output_bits):
            selected = torch.from_numpy(
                ((output_frequency >> bit) & 1).astype(bool)
            )
            output_theta[selected, bit] = initial_score_gap
        self.theta = nn.Parameter(theta)
        self.output_theta = nn.Parameter(output_theta)
        self.coefficient = nn.Parameter(coefficient.float())
        self.token_bias = nn.Parameter(torch.zeros(len(vertices), dtype=torch.float32))
        self.register_buffer(
            "input_degree", torch.from_numpy(input_degree.astype(np.int64))
        )
        self.register_buffer("output_degree", torch.from_numpy(output_degree))
        self.register_buffer("token_vertices", vertices)
        self.register_buffer(
            "active_term", torch.ones(terms, dtype=torch.bool)
        )
        self.n_input_bits = int(n_input_bits)
        self.output_bits = int(output_bits)
        self.output_vertices = 1 << output_bits
        self.vocab_size = len(vertices)
        self.terms = int(terms)
        self.unigram_terms = int(unigram_terms)
        self.max_total_degree = int(max_total_degree)
        self.max_input_degree = int(max_input_degree)
        self.max_output_degree = max_output_degree
        self.char_chunk = int(char_chunk)
        self.support_layout = support_layout
        self.token_bits = int(token_bits)
        self.output_scale = terms ** -0.5
        self.checkpoint_chunks = bool(checkpoint_chunks)
        if parity_dtype not in {torch.float32, torch.bfloat16}:
            raise ValueError("parity dtype must be float32 or bfloat16")
        self.parity_dtype = parity_dtype
        chunk_function = _weighted_character_chunk
        walsh_function = walsh_synthesis
        self.compile_error: str | None = None
        if compile_chunks and torch.cuda.is_available():
            try:
                chunk_function = torch.compile(
                    chunk_function, fullgraph=False, dynamic=False,
                )
                if compile_transforms:
                    walsh_function = torch.compile(
                        walsh_synthesis, fullgraph=True, dynamic=False,
                    )
            except Exception as error:
                self.compile_error = repr(error)
        self._chunk_function = chunk_function
        self._walsh_function = walsh_function

    def forward(self, input_bits: torch.Tensor) -> torch.Tensor:
        if input_bits.ndim != 2 or input_bits.shape[1] != self.n_input_bits:
            raise ValueError(
                f"expected bits [batch,{self.n_input_bits}], got {tuple(input_bits.shape)}"
            )
        bits = input_bits.to(dtype=self.parity_dtype)
        chunks = []
        for lo in range(0, self.terms, self.char_chunk):
            hi = min(lo + self.char_chunk, self.terms)
            arguments = (
                bits, self.theta[lo:hi], self.input_degree[lo:hi],
                self.coefficient[lo:hi]
                * self.active_term[lo:hi].to(self.coefficient.dtype),
                self.max_input_degree, self.output_scale,
            )
            if self.training:
                value = (checkpoint(self._chunk_function, *arguments,
                                    use_reentrant=False)
                         if self.checkpoint_chunks else self._chunk_function(*arguments))
            else:
                value = _exact_weighted_character_chunk(*arguments)
            chunks.append(value)
        weighted = torch.cat(chunks, dim=1)
        spectrum = hard_frequency_scatter_ste(
            weighted, self.output_theta, self.output_degree,
            self.output_bits, self.max_output_degree,
        )
        vertices = self._walsh_function(spectrum)
        scores = vertices.index_select(1, self.token_vertices) + self.token_bias[None]
        return scores - scores.mean(dim=1, keepdim=True)

    def score(self, input_bits: torch.Tensor,
              output_token: int | torch.Tensor) -> torch.Tensor:
        scores = self(input_bits)
        if isinstance(output_token, int):
            if not 0 <= output_token < self.vocab_size:
                raise ValueError("output token is outside the vocabulary")
            return scores[:, output_token]
        selector = torch.as_tensor(output_token, device=scores.device).long()
        if (selector.shape != (len(input_bits),) or bool((selector < 0).any())
                or bool((selector >= self.vocab_size).any())):
            raise ValueError("output selector must be one valid token per input")
        return scores.gather(1, selector[:, None]).squeeze(1)

    @torch.no_grad()
    def initialize_output_prior(self, mean_teacher_logits: torch.Tensor) -> None:
        """Project a training-only mean teacher law onto output-only terms."""
        logits = torch.as_tensor(
            mean_teacher_logits, device=self.coefficient.device, dtype=torch.float32
        ).reshape(-1)
        if len(logits) != self.vocab_size or not bool(torch.isfinite(logits).all()):
            raise ValueError("mean teacher logits have the wrong shape or values")
        self.token_bias.copy_(logits - logits.mean())

    def use_eager_chunks(self) -> None:
        self._chunk_function = _weighted_character_chunk
        self._walsh_function = walsh_synthesis

    @torch.no_grad()
    def hard_output_frequency(self) -> torch.Tensor:
        mask = hard_output_mask(
            self.output_theta, self.output_degree, self.max_output_degree
        )
        bit_value = torch.bitwise_left_shift(
            torch.ones(self.output_bits, dtype=torch.long,
                       device=self.output_theta.device),
            torch.arange(self.output_bits, device=self.output_theta.device),
        )
        return (mask.long() * bit_value[None]).sum(1).cpu()

    @torch.no_grad()
    def hard_index_rows(self) -> torch.Tensor:
        output = []
        for lo in range(0, self.terms, self.char_chunk):
            hi = min(lo + self.char_chunk, self.terms)
            ranked = self.theta[lo:hi].topk(
                self.max_input_degree, dim=1, sorted=True
            ).indices
            active = (torch.arange(self.max_input_degree, device=self.theta.device)[None]
                      < self.input_degree[lo:hi, None])
            output.append(ranked.masked_fill(~active, self.n_input_bits)
                          .sort(dim=1).values.cpu())
        return torch.cat(output)

    @torch.no_grad()
    def sparse_state(self) -> dict[str, Any]:
        rows = self.hard_index_rows().numpy().astype(np.uint32)
        active = self.active_term.cpu().numpy().astype(bool, copy=False)
        rows = rows[active]
        valid = rows < self.n_input_bits
        return {
            "schema": ARTIFACT_SCHEMA,
            "n_input_bits": self.n_input_bits,
            "output_bits": self.output_bits,
            "vocab_size": self.vocab_size,
            "unigram_terms": self.unigram_terms,
            "input_degrees": valid.sum(1, dtype=np.int32).astype(np.uint8),
            "input_indices": rows[valid].astype(np.uint16),
            "output_frequency": (
                self.hard_output_frequency().numpy()[active].astype(np.uint32)
            ),
            "token_vertices": self.token_vertices.cpu().numpy().astype(np.uint32),
            "coefficient": (
                self.coefficient.detach().float().cpu().numpy()[active]
                * self.output_scale
            ).astype(np.float32),
            "token_bias": self.token_bias.detach().float().cpu().numpy().astype(np.float32),
        }


def full_teacher_student_kl_rows(student_scores: torch.Tensor,
                                 teacher_logits: torch.Tensor,
                                 temperature: float = 1.0) -> torch.Tensor:
    if (student_scores.ndim != 2 or teacher_logits.shape != student_scores.shape
            or temperature <= 0):
        raise ValueError("student and teacher must be matching [batch,vocab] matrices")
    teacher_log_probability = torch.log_softmax(
        teacher_logits.float() / temperature, dim=-1
    )
    teacher_probability = teacher_log_probability.exp()
    student_log_probability = torch.log_softmax(
        student_scores.float() / temperature, dim=-1
    )
    return (teacher_probability * (
        teacher_log_probability - student_log_probability
    )).sum(dim=-1)


def full_teacher_student_kl(student_scores: torch.Tensor,
                            teacher_logits: torch.Tensor,
                            temperature: float = 1.0) -> torch.Tensor:
    return full_teacher_student_kl_rows(
        student_scores, teacher_logits, temperature
    ).mean()


def exact_kl_backward_loss(kl: torch.Tensor, terms: int,
                           accumulation_steps: int = 1) -> torch.Tensor:
    """Condition exact-KL gradients without changing the reported objective.

    The student forward uses ``terms**-0.5`` fan-in normalization.  Multiplying
    the scalar KL by the reciprocal only for backward keeps the same optimum
    and forward logits while preventing AdamW epsilon from suppressing weak
    Fourier and STE gradients.
    """
    if kl.ndim or terms <= 0 or accumulation_steps <= 0:
        raise ValueError("KL must be scalar and training dimensions positive")
    return kl * (math.sqrt(terms) / accumulation_steps)


def distribution_metrics(student_scores: np.ndarray | torch.Tensor,
                         teacher_logits: np.ndarray | torch.Tensor,
                         temperature: float = 1.0) -> dict[str, float]:
    # Evaluation runs on 248,077-way batches.  Float32 is both numerically
    # adequate after log-softmax and avoids doubling several full-vocab buffers.
    student = torch.as_tensor(student_scores, dtype=torch.float32)
    teacher = torch.as_tensor(teacher_logits, dtype=torch.float32)
    if student.ndim != 2 or teacher.shape != student.shape or temperature <= 0:
        raise ValueError("student and teacher must be matching [batch,vocab] matrices")
    teacher_logp = torch.log_softmax(teacher / temperature, -1)
    student_logp = torch.log_softmax(student / temperature, -1)
    teacher_p, student_p = teacher_logp.exp(), student_logp.exp()
    kl_rows = (teacher_p * (teacher_logp - student_logp)).sum(-1)
    ce_rows = -(teacher_p * student_logp).sum(-1)
    entropy_rows = -(teacher_p * teacher_logp).sum(-1)
    teacher_top = teacher.argmax(-1)
    student_top = student.argmax(-1)
    k = min(5, student.shape[1])
    student_topk = student.topk(k, -1).indices
    teacher_topk = teacher.topk(k, -1).indices
    teacher_confidence = teacher_p.gather(1, teacher_top[:, None]).squeeze(1)
    student_confidence = student_p.gather(1, student_top[:, None]).squeeze(1)
    return {
        "kl": float(kl_rows.mean()),
        "cross_entropy": float(ce_rows.mean()),
        "teacher_entropy": float(entropy_rows.mean()),
        "perplexity": float(torch.exp(ce_rows.mean().clamp(max=50))),
        "teacher_perplexity": float(torch.exp(entropy_rows.mean().clamp(max=50))),
        "total_variation": float((0.5 * (teacher_p - student_p).abs().sum(-1)).mean()),
        "probability_cosine": float(torch.nn.functional.cosine_similarity(
            teacher_p, student_p, dim=-1
        ).mean()),
        "top1_agreement": float((teacher_top == student_top).double().mean()),
        "teacher_top1_in_student_top5": float(
            (student_topk == teacher_top[:, None]).any(-1).double().mean()
        ),
        "student_top1_in_teacher_top5": float(
            (teacher_topk == student_top[:, None]).any(-1).double().mean()
        ),
        "teacher_top1_confidence": float(teacher_confidence.mean()),
        "student_top1_confidence": float(student_confidence.mean()),
        "entropy_gap": float((-(student_p * student_logp).sum(-1)
                              - entropy_rows).mean()),
    }


@torch.no_grad()
def repair_duplicate_supports(
    model: InputOutputWalshStudent,
    optimizer: torch.optim.Optimizer | Iterable[torch.optim.Optimizer], *,
    seed: int, birth_score_gap: float = 0.1,
) -> dict[str, int | float]:
    """Merge identical characters and deterministically recycle freed rows."""
    if birth_score_gap <= 0:
        raise ValueError("birth score gap must be positive")
    optimizers = ((optimizer,) if isinstance(optimizer, torch.optim.Optimizer)
                  else tuple(optimizer))
    if not optimizers or not all(
            isinstance(value, torch.optim.Optimizer) for value in optimizers):
        raise TypeError("optimizer must contain at least one torch optimizer")
    rows = model.hard_index_rows().numpy().astype(np.uint32)
    frequency = model.hard_output_frequency().numpy().astype(np.uint32)
    keys = np.concatenate((frequency[:, None], rows), axis=1)
    _, inverse, counts = np.unique(keys, axis=0, return_inverse=True, return_counts=True)
    groups = np.flatnonzero(counts > 1)
    duplicate_count = int((counts[groups] - 1).sum())
    if not duplicate_count:
        return {"duplicates_before": 0, "duplicates_repaired": 0,
                "unique_after": model.terms, "recycled": 0,
                "merged_coefficient_l1": 0.0}
    coefficient = model.coefficient.detach().cpu().numpy()
    used = {tuple(map(int, row)) for row in keys}
    rng = np.random.default_rng(seed)
    keepers, freed = [], []
    merged_coefficient_l1 = 0.0

    def fresh_support(row: int) -> tuple[np.ndarray, int, int]:
        degree = int(model.input_degree[row])
        maximum_output_degree = min(
            model.output_bits, model.max_total_degree - degree
        )
        for attempt in range(100_000):
            output_degree = 1 + (
                row + seed + attempt
            ) % maximum_output_degree
            support = np.sort(rng.choice(
                model.n_input_bits, degree, replace=False
            )).astype(np.int64) if degree else np.empty(0, dtype=np.int64)
            output_bits = np.sort(rng.choice(
                model.output_bits, output_degree, replace=False
            )).astype(np.int64)
            replacement_frequency = sum(1 << int(bit) for bit in output_bits)
            padded = np.full(
                model.max_input_degree, model.n_input_bits, dtype=np.int64
            )
            padded[:degree] = support
            key = (replacement_frequency, *map(int, padded))
            if key not in used:
                used.add(key)
                return support, replacement_frequency, output_degree
        raise RuntimeError("could not find a unique recycled joint support")

    duplicate_rows = np.flatnonzero(counts[inverse] > 1)
    duplicate_order = np.argsort(inverse[duplicate_rows], kind="stable")
    duplicate_rows = duplicate_rows[duplicate_order]
    duplicate_labels = inverse[duplicate_rows]
    group_starts = np.concatenate((
        np.asarray([0], dtype=np.int64),
        np.flatnonzero(
            duplicate_labels[1:] != duplicate_labels[:-1]
        ).astype(np.int64) + 1,
        np.asarray([len(duplicate_rows)], dtype=np.int64),
    ))
    for start, stop in zip(group_starts[:-1], group_starts[1:], strict=True):
        members = duplicate_rows[start:stop]
        keeper = int(members[np.argmax(np.abs(coefficient[members]))])
        merged = float(coefficient[members].sum())
        model.coefficient[keeper] = merged
        merged_coefficient_l1 += float(np.abs(coefficient[members]).sum())
        keepers.append(keeper)
        for duplicate in map(int, members):
            if duplicate == keeper:
                continue
            degree = int(model.input_degree[duplicate])
            model.coefficient[duplicate] = 0.0
            support, replacement_frequency, output_degree = fresh_support(
                duplicate
            )
            model.theta[duplicate].zero_()
            if degree:
                model.theta[duplicate, torch.as_tensor(
                    support, dtype=torch.long, device=model.theta.device
                )] = birth_score_gap
            model.output_theta[duplicate].zero_()
            model.output_degree[duplicate] = output_degree
            output_bits = [
                bit for bit in range(model.output_bits)
                if (replacement_frequency >> bit) & 1
            ]
            model.output_theta[duplicate, torch.as_tensor(
                output_bits, dtype=torch.long, device=model.output_theta.device
            )] = birth_score_gap
            freed.append(duplicate)

    keeper_rows = torch.as_tensor(keepers, device=model.theta.device)
    freed_rows = torch.as_tensor(freed, device=model.theta.device)
    for parameter, affected in (
        (model.theta, freed_rows),
        (model.output_theta, freed_rows),
        (model.coefficient, torch.cat((keeper_rows, freed_rows))),
    ):
        for value in optimizers:
            for state in value.state.get(parameter, {}).values():
                if (torch.is_tensor(state) and state.ndim
                        and state.shape == parameter.shape):
                    state[affected] = 0
    final_rows = model.hard_index_rows().numpy().astype(np.uint32)
    final_frequency = model.hard_output_frequency().numpy().astype(np.uint32)
    final_keys = np.concatenate((final_frequency[:, None], final_rows), axis=1)
    unique_after = len(np.unique(final_keys, axis=0))
    if unique_after != model.terms:
        raise RuntimeError("duplicate repair failed")
    return {"duplicates_before": duplicate_count,
            "duplicates_repaired": len(freed), "unique_after": unique_after,
            "recycled": len(freed),
            "merged_coefficient_l1": merged_coefficient_l1}


@torch.no_grad()
def support_locality_audit(model: InputOutputWalshStudent) -> dict[str, float]:
    rows = model.hard_index_rows().numpy()
    degree = model.input_degree.cpu().numpy()
    active = np.arange(model.max_input_degree)[None] < degree[:, None]
    token = rows // model.token_bits
    same_token = np.all((token == token[:, :1]) | ~active, axis=1)
    recent_boundary = model.n_input_bits - 16 * model.token_bits
    any_recent = ((rows >= recent_boundary) & active).any(1)
    frequency = model.hard_output_frequency().numpy()
    output_degree = np.asarray([int(x).bit_count() for x in frequency])
    contextual = degree > 0
    coefficient = model.coefficient.detach().float().cpu().numpy() * model.output_scale
    energy = np.square(coefficient)
    total = max(float(energy.sum()), 1e-20)
    return {
        "output_only_fraction": float((degree == 0).mean()),
        "mean_input_degree": float(degree.mean()),
        "mean_output_degree": float(output_degree.mean()),
        "mean_total_degree": float((degree + output_degree).mean()),
        "contextual_same_token_fraction": float(same_token[contextual].mean()),
        "contextual_any_recent_16_fraction": float(any_recent[contextual].mean()),
        "output_only_coefficient_energy_fraction": float(
            energy[degree == 0].sum() / total
        ),
    }


@torch.no_grad()
def support_retention_audit(model: InputOutputWalshStudent,
                            reference_rows: np.ndarray | torch.Tensor
                            ) -> dict[str, float]:
    current = model.hard_index_rows().numpy()
    reference = np.asarray(reference_rows)
    if reference.shape != current.shape:
        raise ValueError("reference support bank has the wrong shape")
    degree = model.input_degree.cpu().numpy()
    exact = np.all(current == reference, axis=1)
    contextual = degree > 0
    jaccard = np.ones(model.terms, dtype=np.float64)
    for value in np.unique(degree[contextual]):
        selected = degree == value
        overlap = (current[selected, :value, None]
                   == reference[selected, None, :value]).any(2).sum(1)
        jaccard[selected] = overlap / (2 * value - overlap)
    return {
        "exact_row_fraction": float(exact.mean()),
        "contextual_exact_row_fraction": float(exact[contextual].mean()),
        "contextual_mean_jaccard": float(jaccard[contextual].mean()),
    }


def pack_fixed_width(values: np.ndarray, bit_width: int) -> np.ndarray:
    values = np.asarray(values)
    if values.ndim != 1 or not 1 <= bit_width <= 32:
        raise ValueError("invalid fixed-width values")
    unsigned = values.astype(np.uint64, copy=False)
    if np.any(values < 0) or np.any(unsigned >= 1 << bit_width):
        raise ValueError("integer does not fit bit width")
    positions = np.arange(bit_width, dtype=np.uint64)
    bits = ((unsigned[:, None] >> positions) & 1).astype(np.uint8)
    return np.packbits(bits.reshape(-1), bitorder="little")


def unpack_fixed_width(packed: np.ndarray, count: int,
                       bit_width: int) -> np.ndarray:
    raw = np.asarray(packed, dtype=np.uint8).reshape(-1)
    if len(raw) != (count * bit_width + 7) // 8:
        raise ValueError("packed byte count mismatch")
    bits = np.unpackbits(raw, count=count * bit_width,
                         bitorder="little").reshape(count, bit_width)
    weights = np.left_shift(np.uint64(1), np.arange(bit_width, dtype=np.uint64))
    return (bits.astype(np.uint64) @ weights).astype(np.uint32)


def estimate_compact_artifact_bytes(
    terms: int, *, vocab_size: int, max_total_degree: int = 8,
    output_bits: int = 18, n_input_bits: int = 512,
    input_code_bits: int = 32, avg_input_degree: float = 2.0,
) -> int:
    """Conservatively estimate the saved packed student artifact size."""
    if (terms <= 0 or vocab_size <= 0 or output_bits <= 0
            or input_code_bits <= 0 or n_input_bits <= 1
            or not 0 <= avg_input_degree <= max_total_degree - 1):
        raise ValueError("invalid compact-artifact dimensions")
    input_index_bits = max(1, (n_input_bits - 1).bit_length())
    bits_per_term = (
        8
        + avg_input_degree * input_index_bits
        + output_bits
        + 16
    )
    blocks = max(1, math.ceil(terms / 256))
    bytes_terms = math.ceil(bits_per_term * terms / 8)
    bytes_vocab = (
        math.ceil(output_bits * vocab_size / 8)
        + 2 * vocab_size
        + math.ceil(input_code_bits * vocab_size / 8)
    )
    return 8192 + bytes_terms + bytes_vocab + 4 * blocks


def joint_cartesian_degree_counts(
    terms: int, *, n_input_bits: int, output_bits: int,
    max_total_degree: int, unigram_terms: int = 0,
) -> dict[str, int]:
    """Return exact joint_cartesian allocation counts by input degree.

    The corrected capacity estimator uses this instead of an average-degree
    guess.  At the production shape (n_input_bits=512, output_bits=18,
    max_total_degree=4) the first 87,552 contextual rows are input-degree-1
    (512 input singletons x 18 output singletons, then the same input
    singletons x 153 output pairs), and the remainder are input-degree-2.
    """
    if terms < 0 or n_input_bits <= 0 or output_bits <= 0 or max_total_degree < 2:
        raise ValueError("invalid joint_cartesian dimensions")
    if not 0 <= unigram_terms <= terms:
        raise ValueError("invalid unigram_terms")
    contextual = terms - unigram_terms
    counts: dict[str, int] = {"degree1": 0, "degree2": 0}
    if contextual == 0:
        return counts
    singleton_inputs = n_input_bits
    singleton_outputs = output_bits
    pair_outputs = output_bits * (output_bits - 1) // 2
    remaining = contextual
    # degree-1 x output singleton
    take = min(remaining, singleton_inputs * singleton_outputs)
    counts["degree1"] += take
    remaining -= take
    if remaining and max_total_degree >= 3:
        take = min(remaining, singleton_inputs * pair_outputs)
        counts["degree1"] += take
        remaining -= take
    if remaining and max_total_degree >= 3:
        pair_capacity = n_input_bits * (n_input_bits - 1) // 2
        pair_count = min(32768, pair_capacity)
        take = min(remaining, pair_count * singleton_outputs)
        counts["degree2"] += take
        remaining -= take
        if remaining and max_total_degree >= 4:
            take = min(remaining, pair_count * pair_outputs)
            counts["degree2"] += take
            remaining -= take
    if remaining:
        raise ValueError(
            "requested more contextual terms than the structured joint bank holds"
        )
    return counts


def joint_cartesian_avg_input_degree(
    terms: int, *, n_input_bits: int, output_bits: int,
    max_total_degree: int, unigram_terms: int = 0,
) -> float:
    """Exact average input degree for the joint_cartesian layout."""
    counts = joint_cartesian_degree_counts(
        terms, n_input_bits=n_input_bits, output_bits=output_bits,
        max_total_degree=max_total_degree, unigram_terms=unigram_terms,
    )
    total = counts["degree1"] + counts["degree2"]
    if total == 0:
        return 0.0
    return (counts["degree1"] * 1 + counts["degree2"] * 2) / total


def sample_collision_u_statistic(hard_keys: torch.Tensor) -> torch.Tensor:
    """Return E[i != j][1[key_i == key_j]] for one term sample."""
    if hard_keys.ndim not in {1, 2}:
        raise ValueError("hard_keys must be one- or two-dimensional")
    count = len(hard_keys)
    if count < 2:
        return hard_keys.new_zeros((), dtype=torch.float32)
    dimension = 0 if hard_keys.ndim == 2 else None
    _, counts = torch.unique(
        hard_keys.to(torch.int64), dim=dimension, return_counts=True
    )
    numerator = (counts.long() * (counts.long() - 1)).sum().float()
    return numerator / float(count * (count - 1))


def _sampled_hard_joint_supports(
    model: InputOutputWalshStudent, sampled: torch.Tensor,
) -> tuple[torch.Tensor, torch.Tensor, torch.Tensor]:
    input_theta = model.theta.index_select(0, sampled)
    input_degree = model.input_degree.index_select(0, sampled)
    ranked_input = input_theta.topk(
        model.max_input_degree, dim=1, sorted=True
    ).indices
    input_active = (
        torch.arange(model.max_input_degree, device=sampled.device)[None]
        < input_degree[:, None]
    )
    input_rows = ranked_input.masked_fill(
        ~input_active, model.n_input_bits
    ).sort(dim=1).values
    output_theta = model.output_theta.index_select(0, sampled)
    output_degree = model.output_degree.index_select(0, sampled)
    output_mask = hard_output_mask(
        output_theta, output_degree, model.max_output_degree
    )
    bit_value = torch.bitwise_left_shift(
        torch.ones(model.output_bits, dtype=torch.long, device=sampled.device),
        torch.arange(model.output_bits, device=sampled.device),
    )
    output_frequency = (output_mask.long() * bit_value[None]).sum(1)
    keys = torch.cat((output_frequency[:, None], input_rows), dim=1)
    return keys, input_rows, output_mask


def _relaxed_topk_rows(
    theta: torch.Tensor, degree: torch.Tensor, max_degree: int,
    temperature: float,
) -> torch.Tensor:
    if len(theta) == 0:
        return theta
    ranked = theta.topk(max_degree + 1, dim=1, sorted=True).values
    row = torch.arange(len(theta), device=theta.device)
    lower = ranked[row, degree - 1]
    upper = ranked[row, degree]
    threshold = ((lower + upper) * 0.5).detach()
    soft = torch.sigmoid((theta - threshold[:, None]) / temperature)
    hard = hard_topk_mask(theta, degree, max_degree).to(theta.dtype)
    return hard + soft - soft.detach()


class _ZeroForwardIdentityGradient(torch.autograd.Function):
    """Carry a scalar gradient while contributing exactly zero forward."""

    @staticmethod
    def forward(ctx, value: torch.Tensor) -> torch.Tensor:
        return value.new_zeros(())

    @staticmethod
    def backward(ctx, output_gradient: torch.Tensor):
        return output_gradient


def _duplicate_pair_surrogate(
    model: InputOutputWalshStudent,
    keep: torch.Tensor,
    lose: torch.Tensor,
    temperature: float,
) -> torch.Tensor:
    keep_input = hard_topk_mask(
        model.theta.index_select(0, keep).detach(),
        model.input_degree.index_select(0, keep),
        model.max_input_degree,
    ).float()
    keep_output = hard_output_mask(
        model.output_theta.index_select(0, keep).detach(),
        model.output_degree.index_select(0, keep),
        model.max_output_degree,
    ).float()
    lose_input_degree = model.input_degree.index_select(0, lose)
    lose_output_degree = model.output_degree.index_select(0, lose)
    lose_input = _relaxed_topk_rows(
        model.theta.index_select(0, lose), lose_input_degree,
        model.max_input_degree, temperature,
    )
    lose_output = _relaxed_topk_rows(
        model.output_theta.index_select(0, lose), lose_output_degree,
        model.max_output_degree, temperature,
    )
    input_match = (keep_input * lose_input).sum(1) / lose_input_degree.float()
    output_match = (
        (keep_output * lose_output).sum(1) / lose_output_degree.float()
    )
    return (0.5 * (input_match + output_match)).mean()


def _duplicate_pair_logit_margin(
    model: InputOutputWalshStudent,
    keep: torch.Tensor,
    lose: torch.Tensor,
    margin: float,
) -> torch.Tensor:
    """Distance in TopK-logit space to changing a duplicate loser's support."""
    keep_input = hard_topk_mask(
        model.theta.index_select(0, keep).detach(),
        model.input_degree.index_select(0, keep),
        model.max_input_degree,
    )
    keep_output = hard_output_mask(
        model.output_theta.index_select(0, keep).detach(),
        model.output_degree.index_select(0, keep),
        model.max_output_degree,
    )
    lose_input = model.theta.index_select(0, lose)
    lose_output = model.output_theta.index_select(0, lose)

    input_selected_boundary = lose_input.masked_fill(
        ~keep_input, torch.inf
    ).min(1).values
    input_alternate_boundary = lose_input.masked_fill(
        keep_input, -torch.inf
    ).max(1).values
    output_selected_boundary = lose_output.masked_fill(
        ~keep_output, torch.inf
    ).min(1).values
    output_alternate_boundary = lose_output.masked_fill(
        keep_output, -torch.inf
    ).max(1).values
    input_escape = torch.relu(
        margin + input_selected_boundary - input_alternate_boundary
    )
    output_escape = torch.relu(
        margin + output_selected_boundary - output_alternate_boundary
    )
    return (0.5 * (input_escape + output_escape)).mean()


def sampled_joint_collision_loss(
    model: InputOutputWalshStudent, *, sample_size: int = 262_144,
    temperature: float = 0.25,
    generator: torch.Generator | None = None,
) -> tuple[torch.Tensor, dict[str, float]]:
    """Hard pair-collision expectation with duplicate-only STE gradients."""
    if sample_size < 2 or temperature <= 0:
        raise ValueError("invalid collision-loss configuration")
    sample_count = min(model.terms, sample_size)
    sampled = torch.randperm(
        model.terms, device=model.theta.device, generator=generator
    )[:sample_count]
    with torch.no_grad():
        keys, _, _ = _sampled_hard_joint_supports(model, sampled)
        unique, inverse, counts = torch.unique(
            keys, dim=0, return_inverse=True, return_counts=True
        )
        del unique
        hard = (
            (counts.long() * (counts.long() - 1)).sum().float()
            / float(sample_count * (sample_count - 1))
        )
        duplicate_rows = torch.nonzero(
            counts.index_select(0, inverse) > 1, as_tuple=False
        ).flatten()
        duplicate_pair_count = int(
            ((counts.long() * (counts.long() - 1)) // 2).sum()
        )

    metrics = {
        "expectation": float(hard),
        "pair_count": float(duplicate_pair_count),
        "sample_rows": float(sample_count),
    }
    if not len(duplicate_rows):
        zero = model.theta.new_zeros(())
        return zero, metrics

    local = duplicate_rows.cpu().numpy()
    labels = inverse.index_select(0, duplicate_rows).cpu().numpy()
    global_rows = sampled.index_select(0, duplicate_rows).cpu().numpy()
    order = np.lexsort((global_rows, labels))
    local, labels, global_rows = local[order], labels[order], global_rows[order]
    starts = np.concatenate((
        np.asarray([0], dtype=np.int64),
        np.flatnonzero(labels[1:] != labels[:-1]).astype(np.int64) + 1,
    ))
    ends = np.concatenate((starts[1:], np.asarray([len(local)])))
    keep_local = np.concatenate([
        np.repeat(local[start], end - start - 1)
        for start, end in zip(starts, ends, strict=True)
    ])
    lose_local = np.concatenate([
        local[start + 1:end]
        for start, end in zip(starts, ends, strict=True)
    ])
    keep = sampled.index_select(
        0, torch.from_numpy(keep_local).to(sampled.device)
    )
    lose = sampled.index_select(
        0, torch.from_numpy(lose_local).to(sampled.device)
    )
    surrogate = _duplicate_pair_surrogate(
        model, keep, lose, temperature
    )
    return hard.detach() + _ZeroForwardIdentityGradient.apply(surrogate), metrics


@torch.no_grad()
def exact_joint_collision_groups(
    model: InputOutputWalshStudent,
) -> tuple[dict[str, float], torch.Tensor, torch.Tensor]:
    """Audit active rows and return deterministic lowest-row keeper/loser pairs."""
    active = torch.nonzero(
        model.active_term.detach().cpu(), as_tuple=False
    ).flatten().numpy().astype(np.int64)
    rows = model.hard_index_rows().numpy()[active].astype(np.uint32)
    frequency = (
        model.hard_output_frequency().numpy()[active].astype(np.uint32)
    )
    keys = np.concatenate((frequency[:, None], rows), axis=1)
    _, inverse, counts = np.unique(
        keys, axis=0, return_inverse=True, return_counts=True
    )
    duplicate_rows = int((counts - 1).sum())
    pair_numerator = int((counts.astype(np.int64) * (counts - 1)).sum())
    expectation = (
        pair_numerator / (len(active) * (len(active) - 1))
        if len(active) > 1 else 0.0
    )
    metrics = {
        "expectation": float(expectation),
        "duplicate_rows": float(duplicate_rows),
        "duplicate_fraction": duplicate_rows / len(active) if len(active) else 0.0,
        "unique_rows": float(len(counts)),
        "active_rows": float(len(active)),
        "inactive_rows": float(model.terms - len(active)),
    }
    if duplicate_rows == 0:
        empty = torch.empty(0, dtype=torch.long)
        return metrics, empty, empty
    global_rows = active
    order = np.lexsort((global_rows, inverse))
    sorted_labels = inverse[order]
    starts = np.concatenate((
        np.asarray([0], dtype=np.int64),
        np.flatnonzero(
            sorted_labels[1:] != sorted_labels[:-1]
        ).astype(np.int64) + 1,
    ))
    ends = np.concatenate((starts[1:], np.asarray([len(active)])))
    duplicate_group = ends - starts > 1
    starts = starts[duplicate_group]
    ends = ends[duplicate_group]
    keep = np.concatenate([
        np.repeat(global_rows[order[start]], end - start - 1)
        for start, end in zip(starts, ends, strict=True)
    ])
    lose = np.concatenate([
        global_rows[order[start + 1:end]]
        for start, end in zip(starts, ends, strict=True)
    ])
    return (
        metrics,
        torch.from_numpy(keep.astype(np.int64, copy=False)),
        torch.from_numpy(lose.astype(np.int64, copy=False)),
    )


@torch.no_grad()
def exact_joint_collision_audit(
    model: InputOutputWalshStudent,
) -> dict[str, float]:
    """Full collision audit; never mutates characters or optimizer state."""
    metrics, _, _ = exact_joint_collision_groups(model)
    return metrics


@torch.no_grad()
def merge_duplicate_coefficients(
    model: InputOutputWalshStudent,
) -> dict[str, float]:
    """Exactly quotient duplicate active characters; never repair or recycle."""
    before, keep, lose = exact_joint_collision_groups(model)
    merged = len(lose)
    if merged:
        keep = keep.to(model.coefficient.device)
        lose = lose.to(model.coefficient.device)
        model.coefficient.index_add_(
            0, keep, model.coefficient.index_select(0, lose)
        )
        model.coefficient.index_fill_(0, lose, 0.0)
        model.active_term.index_fill_(0, lose, False)
    active_after = int(model.active_term.sum())
    return {
        **{f"before_{key}": value for key, value in before.items()},
        "merged_rows": float(merged),
        "active_after": float(active_after),
        "inactive_after": float(model.terms - active_after),
        "duplicate_fraction_after": 0.0,
    }


def cached_duplicate_collision_loss(
    model: InputOutputWalshStudent,
    keep: torch.Tensor,
    lose: torch.Tensor,
    *,
    sample_size: int = 262_144,
    margin: float = 0.1,
    generator: torch.Generator | None = None,
) -> tuple[torch.Tensor, dict[str, float]]:
    """Apply a zero-forward TopK logit-margin gradient to audited duplicates."""
    if (keep.ndim != 1 or lose.shape != keep.shape or sample_size <= 0
            or margin < 0):
        raise ValueError("invalid cached duplicate-pair loss configuration")
    if keep.device != model.theta.device or lose.device != model.theta.device:
        raise ValueError("cached duplicate pairs must be on the model device")
    if not len(keep):
        zero = model.theta.new_zeros(())
        return zero, {"candidate_pairs": 0.0, "active_pairs": 0.0}
    count = min(len(keep), sample_size)
    if count == len(keep):
        chosen_keep, chosen_lose = keep, lose
    else:
        chosen = torch.randperm(
            len(keep), device=keep.device, generator=generator
        )[:count]
        chosen_keep = keep.index_select(0, chosen)
        chosen_lose = lose.index_select(0, chosen)
    with torch.no_grad():
        keep_keys, _, _ = _sampled_hard_joint_supports(model, chosen_keep)
        lose_keys, _, _ = _sampled_hard_joint_supports(model, chosen_lose)
        active = torch.all(keep_keys == lose_keys, dim=1)
        chosen_keep = chosen_keep[active]
        chosen_lose = chosen_lose[active]
    metrics = {
        "candidate_pairs": float(count),
        "active_pairs": float(len(chosen_keep)),
    }
    if not len(chosen_keep):
        zero = model.theta.new_zeros(())
        return zero, metrics
    surrogate = _duplicate_pair_logit_margin(
        model, chosen_keep, chosen_lose, margin
    )
    return _ZeroForwardIdentityGradient.apply(surrogate), metrics


def encode_compact_student(state: Mapping[str, Any],
                           block_size: int = 256) -> dict[str, Any]:
    if state.get("schema") != ARTIFACT_SCHEMA:
        raise ValueError("unsupported full-next-token artifact")
    degree = np.asarray(state["input_degrees"], dtype=np.uint8)
    indices = np.asarray(state["input_indices"], dtype=np.uint32)
    frequency = np.asarray(state["output_frequency"], dtype=np.uint32)
    vertices = np.asarray(state["token_vertices"], dtype=np.uint32)
    coefficient = np.asarray(state["coefficient"], dtype=np.float32)
    token_bias = np.asarray(state["token_bias"], dtype=np.float32)
    n_input_bits, output_bits = int(state["n_input_bits"]), int(state["output_bits"])
    if (len(degree) != len(coefficient) or len(frequency) != len(degree)
            or int(degree.sum()) != len(indices)
            or len(vertices) != int(state["vocab_size"])
            or len(token_bias) != len(vertices)):
        raise ValueError("sparse character arrays disagree")
    blocks = math.ceil(len(coefficient) / block_size)
    scale = np.ones(blocks, dtype=np.float32)
    quantized = np.empty(len(coefficient), dtype=np.float16)
    for block in range(blocks):
        lo, hi = block * block_size, min((block + 1) * block_size, len(coefficient))
        maximum = float(np.abs(coefficient[lo:hi]).max(initial=0.0))
        scale[block] = maximum if maximum > 1e-12 else 1.0
        quantized[lo:hi] = (coefficient[lo:hi] / scale[block]).astype(np.float16)
    input_index_bits = max(1, (n_input_bits - 1).bit_length())
    return {
        "schema": ARTIFACT_SCHEMA,
        "n_input_bits": n_input_bits,
        "output_bits": output_bits,
        "vocab_size": len(vertices),
        "unigram_terms": int(state["unigram_terms"]),
        "input_degrees": degree,
        "input_index_bits": input_index_bits,
        "input_index_count": len(indices),
        "packed_input_indices": pack_fixed_width(indices, input_index_bits),
        "packed_output_frequency": pack_fixed_width(frequency, output_bits),
        "packed_token_vertices": pack_fixed_width(vertices, output_bits),
        "coefficient_fp16": quantized,
        "coefficient_scale": scale,
        "coefficient_block_size": block_size,
        "token_bias_fp16": token_bias.astype(np.float16),
    }


def decode_compact_student(state: Mapping[str, Any]) -> dict[str, Any]:
    if state.get("schema") != ARTIFACT_SCHEMA:
        raise ValueError("unsupported full-next-token artifact")
    degree = np.asarray(state["input_degrees"], dtype=np.uint8)
    count = len(degree)
    block_size = int(state["coefficient_block_size"])
    quantized = np.asarray(state["coefficient_fp16"], dtype=np.float16)
    scale = np.asarray(state["coefficient_scale"], dtype=np.float32)
    output_bits = int(state["output_bits"])
    vocab_size = int(state["vocab_size"])
    return {
        "schema": ARTIFACT_SCHEMA,
        "n_input_bits": int(state["n_input_bits"]),
        "output_bits": output_bits,
        "vocab_size": vocab_size,
        "unigram_terms": int(state["unigram_terms"]),
        "input_degrees": degree,
        "input_indices": unpack_fixed_width(
            state["packed_input_indices"], int(state["input_index_count"]),
            int(state["input_index_bits"]),
        ),
        "output_frequency": unpack_fixed_width(
            state["packed_output_frequency"], count, output_bits
        ),
        "token_vertices": unpack_fixed_width(
            state["packed_token_vertices"], vocab_size, output_bits
        ),
        "coefficient": quantized.astype(np.float32) * np.repeat(
            scale, block_size
        )[:len(quantized)],
        "token_bias": np.asarray(state["token_bias_fp16"], dtype=np.float16).astype(np.float32),
    }


@torch.no_grad()
def load_compact_student(model: InputOutputWalshStudent,
                         state: Mapping[str, Any], *, score_gap: float = 0.02) -> None:
    loaded = load_compact_prefix(model, state, score_gap=score_gap)
    if loaded != model.terms:
        raise ValueError("artifact has fewer terms than the model")


@torch.no_grad()
def load_compact_prefix(model: InputOutputWalshStudent,
                        state: Mapping[str, Any], *, score_gap: float = 0.02) -> int:
    if score_gap <= 0:
        raise ValueError("score_gap must be positive")
    decoded = decode_compact_student(state) if "packed_input_indices" in state else state
    degree = np.asarray(decoded["input_degrees"], dtype=np.int64)
    frequency = np.asarray(decoded["output_frequency"], dtype=np.int64)
    output_degree = np.asarray(
        [int(value).bit_count() for value in frequency], dtype=np.int64
    )
    loaded = len(degree)
    if (int(decoded["n_input_bits"]) != model.n_input_bits
            or int(decoded["output_bits"]) != model.output_bits
            or int(decoded["vocab_size"]) != model.vocab_size
            or int(decoded["unigram_terms"]) != model.unigram_terms
            or not 0 < loaded <= model.terms
            or np.any(degree != model.input_degree[:loaded].cpu().numpy())
            or np.any(output_degree != model.output_degree[:loaded].cpu().numpy())
            or np.any(np.asarray(decoded["token_vertices"])
                      != model.token_vertices.cpu().numpy())):
        raise ValueError("artifact shape or deterministic support bank does not match")
    indices = np.asarray(decoded["input_indices"], dtype=np.int64)
    offsets = np.concatenate(([0], np.cumsum(degree)))
    model.theta[:loaded].zero_()
    for row in range(loaded):
        support = indices[offsets[row]:offsets[row + 1]]
        if len(support):
            model.theta[row, torch.as_tensor(
                support, dtype=torch.long, device=model.theta.device
            )] = score_gap
    model.output_theta[:loaded].zero_()
    loaded_frequency = torch.as_tensor(
        frequency, dtype=torch.long, device=model.output_theta.device
    )
    loaded_output_theta = model.output_theta[:loaded]
    for bit in range(model.output_bits):
        selected = ((loaded_frequency >> bit) & 1).bool()
        selected_rows = torch.nonzero(selected, as_tuple=False).squeeze(1)
        loaded_output_theta[selected_rows, bit] = score_gap
    coefficient = torch.as_tensor(
        decoded["coefficient"], device=model.coefficient.device,
        dtype=model.coefficient.dtype,
    ) / model.output_scale
    model.coefficient.zero_()
    model.coefficient[:loaded].copy_(coefficient)
    model.token_bias.copy_(torch.as_tensor(
        decoded["token_bias"], device=model.token_bias.device,
        dtype=model.token_bias.dtype,
    ))
    return loaded


def _numpy_walsh_synthesis(value: np.ndarray) -> np.ndarray:
    output = np.asarray(value, dtype=np.float32).copy()
    width, block = output.shape[-1], 1
    while block < width:
        paired = output.reshape(*output.shape[:-1], -1, 2, block)
        left, right = paired[..., 0, :].copy(), paired[..., 1, :].copy()
        paired[..., 0, :] = left + right
        paired[..., 1, :] = left - right
        block *= 2
    return output


def sparse_scores(input_bits: np.ndarray, state: Mapping[str, Any],
                  chunk: int = 4096) -> np.ndarray:
    if "packed_input_indices" in state:
        state = decode_compact_student(state)
    x = np.asarray(input_bits, dtype=np.uint8)
    n_input_bits = int(state["n_input_bits"])
    if x.ndim != 2 or x.shape[1] != n_input_bits:
        raise ValueError("input bits have the wrong shape")
    degree = np.asarray(state["input_degrees"], dtype=np.int64)
    indices = np.asarray(state["input_indices"], dtype=np.int64)
    frequency = np.asarray(state["output_frequency"], dtype=np.int64)
    coefficient = np.asarray(state["coefficient"], dtype=np.float32)
    offsets = np.concatenate(([0], np.cumsum(degree)))
    spectrum = np.zeros((len(x), 1 << int(state["output_bits"])), dtype=np.float32)
    for lo in range(0, len(degree), chunk):
        hi = min(lo + chunk, len(degree))
        local_degree = degree[lo:hi]
        width = int(local_degree.max(initial=0))
        if width:
            padded = np.zeros((hi - lo, width), dtype=np.int64)
            active = np.arange(width)[None] < local_degree[:, None]
            for row, global_row in enumerate(range(lo, hi)):
                padded[row, :local_degree[row]] = indices[
                    offsets[global_row]:offsets[global_row + 1]
                ]
            parity = np.bitwise_xor.reduce(
                x[:, padded], axis=2, where=active[None], initial=0
            )
            character = 1.0 - 2.0 * parity.astype(np.float32)
        else:
            character = np.ones((len(x), hi - lo), dtype=np.float32)
        weighted = character * coefficient[None, lo:hi]
        for row in range(len(x)):
            np.add.at(spectrum[row], frequency[lo:hi], weighted[row])
    all_scores = _numpy_walsh_synthesis(spectrum)
    scores = (all_scores[:, np.asarray(state["token_vertices"], dtype=np.int64)]
              + np.asarray(state["token_bias"], dtype=np.float32)[None])
    return scores - scores.mean(1, keepdims=True)
