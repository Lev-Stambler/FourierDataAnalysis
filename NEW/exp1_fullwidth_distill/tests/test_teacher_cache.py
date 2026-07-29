import numpy as np
import pytest
import torch

from qwen_fullwidth_distill.teacher_cache import (
    HIDDEN_SIZE,
    TeacherHiddenCache,
    finalize_teacher_cache,
    load_teacher_cache_manifest,
    make_cache_identity,
    open_hidden_state_shard,
    read_hidden_state_shard,
    reconstruct_logits,
    write_hidden_state_shard,
)


def _identity(*, revision: str = "teacher-revision"):
    return make_cache_identity(
        model_id="teacher/model",
        model_revision=revision,
        embedding_sha256="a" * 64,
        dataset_manifest={
            "schema": "dataset-v1",
            "revision": "dataset-revision",
            "split_sizes": {"train": 5},
        },
        context_length=16,
    )


def test_bfloat16_memmap_roundtrip_and_final_manifest(tmp_path):
    generator = torch.Generator().manual_seed(7)
    first = torch.randn(
        3, HIDDEN_SIZE, generator=generator, dtype=torch.bfloat16
    )
    second = torch.randn(
        2, HIDDEN_SIZE, generator=generator, dtype=torch.bfloat16
    )
    records = [
        write_hidden_state_shard(tmp_path, "train", 0, 0, first),
        write_hidden_state_shard(tmp_path, "train", 1, 3, second),
    ]

    mapped = open_hidden_state_shard(tmp_path, records[0])
    assert isinstance(mapped, np.memmap)
    assert mapped.dtype == np.dtype("<u2")
    assert mapped.shape == (3, HIDDEN_SIZE)
    assert np.array_equal(
        np.asarray(mapped),
        first.contiguous().view(torch.uint16).numpy(),
    )
    assert torch.equal(
        read_hidden_state_shard(tmp_path, records[0]),
        first,
    )

    manifest = finalize_teacher_cache(
        tmp_path,
        _identity(),
        {"train": records},
        expected_split_rows={"train": 5},
    )
    assert manifest["splits"]["train"]["rows"] == 5
    assert load_teacher_cache_manifest(
        tmp_path,
        expected_identity=_identity(),
    ) == manifest
    assert not list(tmp_path.glob("*.tmp-*"))


def test_manifest_rejects_stale_identity_and_corrupt_shard(tmp_path):
    hidden = torch.zeros(2, HIDDEN_SIZE, dtype=torch.bfloat16)
    record = write_hidden_state_shard(
        tmp_path, "validation", 0, 0, hidden
    )
    finalize_teacher_cache(
        tmp_path,
        _identity(),
        {"validation": [record]},
    )

    with pytest.raises(RuntimeError, match="identity mismatch"):
        load_teacher_cache_manifest(
            tmp_path,
            expected_identity=_identity(revision="other-revision"),
        )

    shard_path = tmp_path / record["file"]
    with shard_path.open("r+b") as stream:
        stream.seek(0)
        stream.write(b"\x01")
    with pytest.raises(RuntimeError, match="checksum mismatch"):
        load_teacher_cache_manifest(tmp_path)


def test_finalize_rejects_noncontiguous_shards(tmp_path):
    hidden = torch.zeros(1, HIDDEN_SIZE, dtype=torch.bfloat16)
    first = write_hidden_state_shard(tmp_path, "train", 0, 0, hidden)
    second = write_hidden_state_shard(tmp_path, "train", 1, 2, hidden)
    with pytest.raises(RuntimeError, match="non-contiguous"):
        finalize_teacher_cache(
            tmp_path,
            _identity(),
            {"train": [first, second]},
        )
    assert not (tmp_path / "manifest.json").exists()


def test_reconstruct_logits_matches_frozen_tied_linear():
    generator = torch.Generator().manual_seed(11)
    hidden = torch.randn(
        4, HIDDEN_SIZE, generator=generator, dtype=torch.bfloat16
    )
    embedding = torch.randn(
        23, HIDDEN_SIZE, generator=generator, dtype=torch.bfloat16
    )
    embedding.requires_grad_(False)

    actual = reconstruct_logits(hidden, embedding, vocab_size=19)
    expected = torch.nn.functional.linear(hidden, embedding[:19])
    assert torch.equal(actual, expected)

    with pytest.raises(ValueError, match="must be frozen"):
        reconstruct_logits(hidden, embedding.requires_grad_(True))


def test_random_access_reader_preserves_order_across_shards(tmp_path):
    values = torch.arange(5 * HIDDEN_SIZE, dtype=torch.float32).reshape(
        5, HIDDEN_SIZE
    ).to(torch.bfloat16)
    records = [
        write_hidden_state_shard(tmp_path, "train", 0, 0, values[:3]),
        write_hidden_state_shard(tmp_path, "train", 1, 3, values[3:]),
    ]
    finalize_teacher_cache(
        tmp_path,
        _identity(),
        {"train": records},
        expected_split_rows={"train": 5},
    )
    cache = TeacherHiddenCache(tmp_path, expected_identity=_identity())
    assert torch.equal(
        cache.take("train", np.array([4, 0, 3, 1])),
        values[[4, 0, 3, 1]],
    )
    with pytest.raises(IndexError):
        cache.take("train", [5])
