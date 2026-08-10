"""Non-Fourier text-complexity controls for predictor comparisons."""

from __future__ import annotations

import math
import zlib
from collections.abc import Iterable

import numpy as np


def simple_text_controls(
    tokens: np.ndarray, *, q: int = 256, max_tokens: int = 1_000_000
) -> dict:
    values = np.asarray(tokens, dtype=np.int64)[:max_tokens]
    if len(values) < 100 or np.any(values < 0) or np.any(values >= q):
        raise ValueError("invalid categorical token stream")
    counts = np.bincount(values, minlength=q).astype(float)
    probabilities = counts[counts > 0] / counts.sum()
    unigram_entropy = float(-np.dot(probabilities, np.log2(probabilities)))

    split = len(values) // 2
    train = values[:split]
    score = values[split:]
    pair_counts = np.bincount(train[:-1] * q + train[1:], minlength=q * q).reshape(q, q)
    context_counts = pair_counts.sum(axis=1)
    smoothing = 1.0
    probabilities_score = (pair_counts[score[:-1], score[1:]] + smoothing) / (
        context_counts[score[:-1]] + smoothing * q
    )
    bigram_ce = float(-np.mean(np.log2(probabilities_score)))

    full_pairs = np.bincount(values[:-1] * q + values[1:], minlength=q * q).reshape(
        q, q
    )
    joint = full_pairs / full_pairs.sum()
    left = joint.sum(axis=1)
    right = joint.sum(axis=0)
    rows, columns = np.nonzero(joint)
    mutual_information = float(
        sum(
            joint[row, column]
            * math.log2(joint[row, column] / (left[row] * right[column]))
            for row, column in zip(rows, columns, strict=True)
        )
    )
    raw = np.ascontiguousarray(values.astype(np.uint8)).tobytes()
    compression_bits_per_byte = 8.0 * len(zlib.compress(raw, level=6)) / len(raw)
    return {
        "unigram_entropy_bits": unigram_entropy,
        "heldout_bigram_ce_bits": bigram_ce,
        "lag1_mutual_information_bits": mutual_information,
        "zlib_bits_per_byte": compression_bits_per_byte,
    }


def blockwise_simple_text_controls(
    tokens: np.ndarray,
    segments: Iterable[tuple[int, int]],
    *,
    q: int = 256,
    seed: int = 0,
    max_tokens: int = 1_000_000,
) -> dict:
    """Compute controls within randomly ordered disjoint blocks.

    Bigram and compression statistics never create an artificial pair across a
    block boundary.  Half of the randomized blocks estimate the bigram table and
    the other half score it.
    """
    values = np.asarray(tokens, dtype=np.int64)
    ranges = [(int(start), int(stop)) for start, stop in segments]
    if values.ndim != 1 or len(ranges) < 4 or max_tokens < 100:
        raise ValueError("tokens and at least four segments are required")
    if any(start < 0 or stop <= start or stop > len(values) for start, stop in ranges):
        raise ValueError("segment bounds are invalid")
    order = np.random.default_rng(seed).permutation(len(ranges))
    selected: list[np.ndarray] = []
    consumed = 0
    for index in order:
        start, stop = ranges[int(index)]
        take = min(stop - start, max_tokens - consumed)
        if take:
            selected.append(np.asarray(values[start : start + take]))
            consumed += take
        if consumed >= max_tokens:
            break
    if consumed < 100 or any(np.any((block < 0) | (block >= q)) for block in selected):
        raise ValueError("selected controls contain invalid categorical values")

    counts = sum(
        (np.bincount(block, minlength=q).astype(float) for block in selected),
        start=np.zeros(q, dtype=float),
    )
    probabilities = counts[counts > 0] / counts.sum()
    unigram_entropy = float(-np.dot(probabilities, np.log2(probabilities)))

    split = max(1, len(selected) // 2)
    train_blocks = selected[:split]
    score_blocks = selected[split:]
    if not score_blocks:
        raise ValueError("control split left no held-out blocks")

    def pair_counts(blocks: list[np.ndarray]) -> np.ndarray:
        output = np.zeros((q, q), dtype=np.int64)
        for block in blocks:
            if len(block) > 1:
                output += np.bincount(
                    block[:-1] * q + block[1:], minlength=q * q
                ).reshape(q, q)
        return output

    train_pairs = pair_counts(train_blocks)
    context_counts = train_pairs.sum(axis=1)
    score_loss = 0.0
    score_pairs = 0
    for block in score_blocks:
        if len(block) < 2:
            continue
        probabilities_score = (train_pairs[block[:-1], block[1:]] + 1.0) / (
            context_counts[block[:-1]] + q
        )
        score_loss -= float(np.log2(probabilities_score).sum())
        score_pairs += len(probabilities_score)
    if score_pairs == 0:
        raise ValueError("control score blocks contain no pairs")

    full_pairs = pair_counts(selected)
    joint = full_pairs / full_pairs.sum()
    left = joint.sum(axis=1)
    right = joint.sum(axis=0)
    rows, columns = np.nonzero(joint)
    mutual_information = float(
        sum(
            joint[row, column]
            * math.log2(joint[row, column] / (left[row] * right[column]))
            for row, column in zip(rows, columns, strict=True)
        )
    )
    compressed_bytes = sum(
        len(
            zlib.compress(
                np.ascontiguousarray(block.astype(np.uint8)).tobytes(), level=6
            )
        )
        for block in selected
    )
    return {
        "unigram_entropy_bits": unigram_entropy,
        "heldout_bigram_ce_bits": score_loss / score_pairs,
        "lag1_mutual_information_bits": mutual_information,
        "zlib_bits_per_byte": 8.0 * compressed_bytes / consumed,
    }
