"""Deterministic, injective 18-bit sign-LSH codes for tokenizer rows."""

from __future__ import annotations

import hashlib
import itertools
import os
from pathlib import Path

import numpy as np
import torch

from .config import LSH_BITS, LSH_SEED, MODEL_ID, MODEL_REVISION

SCHEMA = "qwen-lsh18-codebook-v1"


def tensor_sha256(tensor: torch.Tensor, chunk_rows: int = 8192) -> str:
    """Hash the exact tensor representation, including bfloat16 payloads."""
    digest = hashlib.sha256()
    value = tensor.detach()
    for lo in range(0, len(value), chunk_rows):
        part = value[lo : lo + chunk_rows].contiguous()
        if part.dtype == torch.bfloat16:
            part = part.view(torch.uint16)
        digest.update(part.cpu().numpy().tobytes())
    return digest.hexdigest()


def signed_codebook_sha256(codebook: np.ndarray | torch.Tensor) -> str:
    if isinstance(codebook, torch.Tensor):
        value = codebook.detach().to(device="cpu", dtype=torch.int8).numpy()
    else:
        value = np.asarray(codebook, dtype=np.int8)
    return hashlib.sha256(np.ascontiguousarray(value).tobytes()).hexdigest()


def _pack_bits(bits: np.ndarray) -> np.ndarray:
    width = bits.shape[1]
    weights = np.left_shift(
        np.uint32(1), np.arange(width, dtype=np.uint32)
    )
    return bits.astype(np.uint32) @ weights


def _unpack_signed(codes: np.ndarray, bits: int) -> np.ndarray:
    shifts = np.arange(bits, dtype=np.uint32)
    binary = ((codes[:, None] >> shifts) & 1).astype(np.int8)
    return 2 * binary - 1


def _candidate_shells(bits: int):
    for radius in range(1, bits + 1):
        combinations = np.asarray(
            list(itertools.combinations(range(bits), radius)),
            dtype=np.int16,
        ).reshape(-1, radius)
        masks = np.zeros(len(combinations), dtype=np.uint32)
        for column in range(radius):
            masks |= np.left_shift(
                np.uint32(1), combinations[:, column].astype(np.uint32)
            )
        yield radius, combinations, masks


def build_lsh_codebook(
    embedding,
    *,
    bits: int = LSH_BITS,
    seed: int = LSH_SEED,
) -> tuple[np.ndarray, dict]:
    """Return a unique signed code per row plus an auditable repair report.

    Raw codes are signs of centered embedding rows projected onto fixed
    Gaussian hyperplanes. One representative keeps each occupied raw bucket.
    Colliding rows are greedily assigned to the nearest free Hamming code.
    Projection margins choose the bucket representative, displaced-row order,
    and which bits to flip when several free codes have the same distance.
    """
    value = np.asarray(embedding, dtype=np.float32)
    if value.ndim != 2 or min(value.shape) <= 0:
        raise ValueError("embedding must be a nonempty matrix")
    if not np.isfinite(value).all():
        raise ValueError("embedding contains nonfinite values")
    if bits <= 0 or bits > 30:
        raise ValueError("bits must be in 1..30")
    vocabulary, dimension = value.shape
    capacity = 1 << bits
    if vocabulary > capacity:
        raise ValueError(
            f"{vocabulary} rows cannot fit injectively in {bits} bits"
        )

    centered = value - value.mean(axis=0, keepdims=True)
    gaussian = np.random.default_rng(seed).standard_normal(
        (bits, dimension)
    ).astype(np.float32)
    projections = centered @ gaussian.T
    raw_bits = (projections > 0).astype(np.uint8)
    raw_codes = _pack_bits(raw_bits)
    hyperplane_norms = np.linalg.norm(gaussian, axis=1)
    margins = np.abs(projections) / hyperplane_norms[None, :]
    minimum_margin = margins.min(axis=1)

    unique, inverse, counts = np.unique(
        raw_codes, return_inverse=True, return_counts=True
    )
    used = np.zeros(capacity, dtype=np.bool_)
    used[unique] = True
    assigned = raw_codes.copy()
    displaced: list[int] = []

    grouped_order = np.argsort(inverse, kind="stable")
    offsets = np.concatenate([[0], np.cumsum(counts)])
    collision_sizes: list[int] = []
    for group_id, count in enumerate(counts):
        if count <= 1:
            continue
        members = grouped_order[offsets[group_id] : offsets[group_id + 1]]
        # The row farthest from its nearest hyperplane is the least natural
        # candidate to move. Token id is the stable final tie-break.
        ranking = np.lexsort((members, -minimum_margin[members]))
        representative = int(members[ranking[0]])
        displaced.extend(
            int(row) for row in members if int(row) != representative
        )
        collision_sizes.append(int(count))

    displaced_array = np.asarray(displaced, dtype=np.int64)
    if len(displaced_array):
        move_order = displaced_array[
            np.lexsort((displaced_array, -minimum_margin[displaced_array]))
        ]
    else:
        move_order = displaced_array

    shells = list(_candidate_shells(bits))
    distance_counts: dict[int, int] = {}
    total_distance = 0
    for row_value in move_order:
        row = int(row_value)
        base = raw_codes[row]
        chosen = None
        chosen_radius = None
        for radius, combinations, masks in shells:
            candidates = np.bitwise_xor(base, masks)
            available = np.flatnonzero(~used[candidates])
            if not len(available):
                continue
            available_combinations = combinations[available]
            flip_cost = margins[row, available_combinations].sum(axis=1)
            available_candidates = candidates[available]
            choice_order = np.lexsort((available_candidates, flip_cost))
            choice = int(available[choice_order[0]])
            chosen = int(candidates[choice])
            chosen_radius = radius
            break
        if chosen is None or chosen_radius is None:
            raise RuntimeError("no free Hamming code remained during repair")
        assigned[row] = chosen
        used[chosen] = True
        distance_counts[chosen_radius] = distance_counts.get(chosen_radius, 0) + 1
        total_distance += chosen_radius

    if len(np.unique(assigned)) != vocabulary:
        raise RuntimeError("collision repair did not produce unique codes")
    signed = _unpack_signed(assigned, bits)
    if not np.isin(signed, (-1, 1)).all():
        raise RuntimeError("signed codebook contains values outside {-1,+1}")

    moved = len(displaced)
    report = {
        "schema": SCHEMA,
        "bits": int(bits),
        "seed": int(seed),
        "vocab_size": int(vocabulary),
        "embedding_width": int(dimension),
        "capacity": int(capacity),
        "raw_unique_codes": len(unique),
        "raw_collision_groups": len(collision_sizes),
        "raw_colliding_rows": int(sum(collision_sizes)),
        "largest_collision_group": int(max(collision_sizes, default=1)),
        "repaired_rows": int(moved),
        "repair_fraction": float(moved / vocabulary),
        "repair_distance_histogram": {
            str(distance): int(count)
            for distance, count in sorted(distance_counts.items())
        },
        "mean_repair_distance": float(total_distance / moved) if moved else 0.0,
        "max_repair_distance": int(max(distance_counts, default=0)),
        "raw_bit_balance": float(raw_bits.mean()),
        "repaired_bit_balance": float(((signed + 1) // 2).mean()),
        "projection_sha256": hashlib.sha256(
            np.ascontiguousarray(gaussian).tobytes()
        ).hexdigest(),
        "codebook_sha256": signed_codebook_sha256(signed),
    }
    return signed, report


def save_codebook_artifact(
    path: str | Path,
    codebook: np.ndarray,
    report: dict,
    *,
    embedding_sha256: str,
) -> dict:
    path = Path(path)
    signed = np.asarray(codebook, dtype=np.int8)
    code_hash = signed_codebook_sha256(signed)
    if report.get("codebook_sha256") != code_hash:
        raise ValueError("report does not match codebook")
    payload = {
        "schema": SCHEMA,
        "model_id": MODEL_ID,
        "model_revision": MODEL_REVISION,
        "embedding_sha256": embedding_sha256,
        "codebook_sha256": code_hash,
        "bits": int(signed.shape[1]),
        "seed": int(report["seed"]),
        "vocab_size": int(signed.shape[0]),
        "codebook": torch.from_numpy(signed.copy()),
        "report": dict(report),
    }
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    torch.save(payload, temporary)
    os.replace(temporary, path)
    return {
        key: value for key, value in payload.items() if key != "codebook"
    }


def load_codebook_artifact(
    path: str | Path,
    *,
    embedding_sha256: str | None = None,
    vocab_size: int | None = None,
) -> tuple[torch.Tensor, dict]:
    payload = torch.load(
        Path(path), map_location="cpu", mmap=True, weights_only=True
    )
    expected = {
        "schema": SCHEMA,
        "model_id": MODEL_ID,
        "model_revision": MODEL_REVISION,
        "bits": LSH_BITS,
        "seed": LSH_SEED,
    }
    if embedding_sha256 is not None:
        expected["embedding_sha256"] = embedding_sha256
    if vocab_size is not None:
        expected["vocab_size"] = int(vocab_size)
    for key, expected_value in expected.items():
        if payload.get(key) != expected_value:
            raise RuntimeError(f"codebook artifact mismatched {key}")
    codebook = payload.get("codebook")
    if (
        not isinstance(codebook, torch.Tensor)
        or codebook.ndim != 2
        or codebook.shape[1] != LSH_BITS
        or codebook.dtype != torch.int8
    ):
        raise RuntimeError("codebook artifact has invalid tensor")
    if payload.get("vocab_size") != len(codebook):
        raise RuntimeError("codebook artifact has invalid vocabulary size")
    if not bool(torch.all((codebook == -1) | (codebook == 1))):
        raise RuntimeError("codebook artifact contains non-signed values")
    code_hash = signed_codebook_sha256(codebook)
    if payload.get("codebook_sha256") != code_hash:
        raise RuntimeError("codebook artifact hash changed")
    report = payload.get("report")
    if not isinstance(report, dict) or report.get("codebook_sha256") != code_hash:
        raise RuntimeError("codebook artifact report changed")
    metadata = {
        key: value for key, value in payload.items() if key != "codebook"
    }
    return codebook, metadata
