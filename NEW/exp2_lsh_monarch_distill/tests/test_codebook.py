import numpy as np
import pytest
import torch
from qwen_lsh_monarch.codebook import (
    build_lsh_codebook,
    load_codebook_artifact,
    save_codebook_artifact,
    signed_codebook_sha256,
)


def _pack_signed(codes):
    binary = ((np.asarray(codes) + 1) // 2).astype(np.uint32)
    return binary @ (np.uint32(1) << np.arange(codes.shape[1], dtype=np.uint32))


def test_collision_repair_is_nearest_free_unique_and_deterministic():
    embedding = np.zeros((7, 4), dtype=np.float32)
    first, report = build_lsh_codebook(embedding, bits=3, seed=7)
    second, second_report = build_lsh_codebook(embedding, bits=3, seed=7)
    np.testing.assert_array_equal(first, second)
    assert report == second_report
    assert _pack_signed(first).tolist() == [0, 1, 2, 4, 3, 5, 6]
    assert len(np.unique(first, axis=0)) == 7
    assert report["raw_unique_codes"] == 1
    assert report["repaired_rows"] == 6
    assert report["repair_distance_histogram"] == {"1": 3, "2": 3}
    assert report["max_repair_distance"] == 2


def test_random_projection_codebook_is_signed_balanced_and_injective():
    embedding = np.random.default_rng(4).normal(size=(200, 12))
    codes, report = build_lsh_codebook(embedding, bits=8, seed=0)
    assert codes.shape == (200, 8)
    assert codes.dtype == np.int8
    assert set(np.unique(codes)) == {-1, 1}
    assert len(np.unique(codes, axis=0)) == len(codes)
    assert report["codebook_sha256"] == signed_codebook_sha256(codes)
    assert 0.0 < report["repaired_bit_balance"] < 1.0


def test_codebook_rejects_capacity_and_nonfinite_embeddings():
    with pytest.raises(ValueError, match="cannot fit"):
        build_lsh_codebook(np.zeros((9, 2)), bits=3)
    value = np.zeros((2, 2))
    value[0, 0] = np.nan
    with pytest.raises(ValueError, match="nonfinite"):
        build_lsh_codebook(value, bits=2)


def test_artifact_roundtrip_validates_source_and_payload(tmp_path):
    embedding = np.random.default_rng(2).normal(size=(16, 5))
    codes, report = build_lsh_codebook(embedding, bits=18, seed=0)
    path = tmp_path / "codes.pt"
    metadata = save_codebook_artifact(
        path, codes, report, embedding_sha256="a" * 64
    )
    loaded, loaded_metadata = load_codebook_artifact(
        path, embedding_sha256="a" * 64, vocab_size=16
    )
    torch.testing.assert_close(loaded, torch.from_numpy(codes))
    assert loaded_metadata == metadata
    with pytest.raises(RuntimeError, match="embedding_sha256"):
        load_codebook_artifact(path, embedding_sha256="b" * 64)

    payload = torch.load(path, weights_only=True)
    payload["codebook"][0, 0] *= -1
    torch.save(payload, path)
    with pytest.raises(RuntimeError, match="hash changed"):
        load_codebook_artifact(path)
