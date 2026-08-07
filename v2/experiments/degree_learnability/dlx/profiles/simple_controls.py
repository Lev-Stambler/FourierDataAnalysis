"""Non-Fourier text-complexity controls for predictor comparisons."""

from __future__ import annotations

import math
import zlib

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
    pair_counts = np.bincount(
        train[:-1] * q + train[1:], minlength=q * q
    ).reshape(q, q)
    context_counts = pair_counts.sum(axis=1)
    smoothing = 1.0
    probabilities_score = (
        pair_counts[score[:-1], score[1:]] + smoothing
    ) / (context_counts[score[:-1]] + smoothing * q)
    bigram_ce = float(-np.mean(np.log2(probabilities_score)))

    full_pairs = np.bincount(
        values[:-1] * q + values[1:], minlength=q * q
    ).reshape(q, q)
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
