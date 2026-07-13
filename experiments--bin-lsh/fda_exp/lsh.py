"""Sign-LSH binary token codes from embedding rows.

Every vocabulary token gets one row of a fixed 0/1 code table.  Projection
bits are sign(centered_embedding @ gaussian); rows that remain identical at
the projection cap (exactly duplicated embedding rows are inseparable by any
projection) receive deterministic tie-break bits so the full table is unique.
"""

from __future__ import annotations

import numpy as np


def _duplicate_groups(codes):
    """Index arrays of rows sharing a code, largest groups first."""
    _, inverse, counts = np.unique(codes, axis=0, return_inverse=True,
                                   return_counts=True)
    groups = [np.flatnonzero(inverse == g) for g in np.flatnonzero(counts > 1)]
    return sorted(groups, key=len, reverse=True)


def _append_tiebreak_bits(codes, groups):
    """Distinguish colliding rows by the binary rank of their id in the group."""
    width = int(max(len(g) - 1 for g in groups)).bit_length()
    extra = np.zeros((len(codes), width), dtype=np.uint8)
    for group in groups:
        for rank, row in enumerate(np.sort(group)):
            extra[row] = (rank >> np.arange(width)) & 1
    return np.concatenate([codes, extra], axis=1), width


def _unresolved_rows(codes, embedding_gid):
    """Rows whose code collides with a row of a *different* embedding."""
    _, code_gid = np.unique(codes, axis=0, return_inverse=True)
    combos = np.unique(np.stack([code_gid, embedding_gid], axis=1), axis=0)
    mixed = np.flatnonzero(np.bincount(combos[:, 0]) > 1)
    return int(np.isin(code_gid, mixed).sum())


def build_lsh_codes(E, B0=64, cap=512, seed=0):
    """(q, B_total) uint8 codes, unique per row, plus a construction report.

    Rows are mean-centered (sign-LSH is scale invariant, so L2 normalization
    is a no-op; centering removes the shared mean direction and maximizes
    per-bit entropy).  The Gaussian matrix is drawn once at ``cap`` so that
    doubling ``B`` yields nested, deterministic prefixes.

    Exactly duplicated embedding rows are inseparable by any projection, so
    they are excluded from the doubling criterion (they always receive
    tie-break bits); ``B`` therefore measures what the distinct rows need,
    and ``collisions_vs_B`` reports the unresolved-row curve.
    """
    E = np.asarray(E, dtype=np.float32)
    centered = E - E.mean(axis=0, keepdims=True)
    _, embedding_gid = np.unique(centered, axis=0, return_inverse=True)
    G = np.random.default_rng(seed).standard_normal((cap, E.shape[1])).astype(np.float32)
    projections = (centered @ G.T > 0).astype(np.uint8)
    B = min(B0, cap)
    curve = {B: _unresolved_rows(projections[:, :B], embedding_gid)}
    while curve[B] > 0 and B < cap:
        B = min(2 * B, cap)
        curve[B] = _unresolved_rows(projections[:, :B], embedding_gid)
    codes, tiebreak, groups = _finalize_unique(projections[:, :B])
    exact = _duplicate_groups(centered)
    return codes, {
        "B_proj": int(B),
        "tiebreak_bits": int(tiebreak),
        "B_total": int(codes.shape[1]),
        "collisions_vs_B": {str(b): int(n) for b, n in curve.items()},
        "duplicate_groups": [int(len(g)) for g in groups],
        "exact_duplicate_embedding_rows": [int(len(g)) for g in exact],
        "bit_balance_mean": float(codes.mean()),
        "seed": int(seed),
    }


def _finalize_unique(codes):
    """Tie-break any residual duplicate code rows; assert global uniqueness."""
    groups = _duplicate_groups(codes)
    tiebreak = 0
    if groups:
        codes, tiebreak = _append_tiebreak_bits(codes, groups)
    if len(np.unique(codes, axis=0)) < len(codes):
        raise RuntimeError("codes are not unique after tie-breaking")
    return codes, tiebreak, groups


def build_pca_codes(E, B=128):
    """Sign codes from the top-B principal directions of the embedding rows.

    Unlike random hyperplanes, bits are ordered coarse-to-fine: bit j is the
    sign along the j-th largest-variance direction, so low-degree characters
    over leading bits cut the vocabulary along its dominant semantic axes.
    """
    E = np.asarray(E, dtype=np.float32)
    centered = E - E.mean(axis=0, keepdims=True)
    cov = (centered.T @ centered).astype(np.float64)
    _, vecs = np.linalg.eigh(cov)
    top = vecs[:, ::-1][:, :B].astype(np.float32)
    codes, tiebreak, groups = _finalize_unique((centered @ top > 0).astype(np.uint8))
    return codes, {
        "B_proj": int(B),
        "tiebreak_bits": int(tiebreak),
        "B_total": int(codes.shape[1]),
        "duplicate_groups": [int(len(g)) for g in groups],
        "bit_balance_mean": float(codes.mean()),
    }


def build_control_codes(q, B_total, seed=1):
    """i.i.d. random codes at the same width: unique but locality-free."""
    codes = np.random.default_rng(seed).integers(0, 2, (q, B_total), dtype=np.uint8)
    if len(np.unique(codes, axis=0)) < q:
        raise RuntimeError("control codes collided; expected probability ~q^2/2^B")
    return codes


def token_id_codes(q):
    """Raw token-id bits: the original arbitrary-ordering binary encoding."""
    width = max(1, int(q - 1).bit_length())
    ids = np.arange(q, dtype=np.int64)
    return ((ids[:, None] >> np.arange(width)) & 1).astype(np.uint8)


def bit_expand(tokens, codes):
    """Look tokens up in the code table: shape tokens.shape + (B_total,)."""
    tokens = np.asarray(tokens, dtype=np.int64)
    return np.asarray(codes, dtype=np.uint8)[tokens]
