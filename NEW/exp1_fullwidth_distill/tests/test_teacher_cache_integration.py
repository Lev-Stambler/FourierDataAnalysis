import numpy as np
import pytest
import torch

import qwen_fullwidth_distill.teacher_cache as teacher_cache_module
import qwen_fullwidth_distill.train as train_module
from qwen_fullwidth_distill.teacher_cache import (
    HIDDEN_SIZE,
    TeacherHiddenCache,
    finalize_teacher_cache,
    load_hidden_state_shard_record,
    make_cache_identity,
    read_hidden_state_shard,
    reconstruct_logits,
    write_hidden_state_shard,
)
from qwen_fullwidth_distill.train import (
    _benchmark_teacher_logits,
    tensor_sha256,
)


def _dataset_manifest(rows: int = 9):
    return {
        "schema": "qwen-fullwidth-context16-data-v2",
        "model_id": "teacher/model",
        "model_revision": "revision-1",
        "context_length": 16,
        "fineweb_id": "dataset/id",
        "fineweb_config": "sample",
        "fineweb_revision": "dataset-revision",
        "split_sizes": {
            "train": rows,
            "validation": 2,
            "test": 2,
        },
    }


def _identity(
    embedding: torch.Tensor,
    *,
    revision: str = "revision-1",
    dataset_manifest=None,
):
    return make_cache_identity(
        model_id="teacher/model",
        model_revision=revision,
        embedding_sha256=tensor_sha256(embedding),
        dataset_manifest=dataset_manifest or _dataset_manifest(),
        context_length=16,
    )


def test_train_identity_and_copied_tied_head_logits_are_exact(tmp_path):
    generator = torch.Generator().manual_seed(101)
    teacher_embedding = torch.randn(
        31,
        HIDDEN_SIZE,
        generator=generator,
        dtype=torch.bfloat16,
    )
    teacher_embedding.requires_grad_(False)
    copied_frozen_embedding = teacher_embedding.detach().clone()
    hidden = torch.randn(
        7,
        HIDDEN_SIZE,
        generator=generator,
        dtype=torch.bfloat16,
    )
    identity = _identity(teacher_embedding)
    assert identity["embedding_sha256"] == tensor_sha256(
        copied_frozen_embedding
    )
    assert identity["dataset_manifest"] == _dataset_manifest()

    records = [
        write_hidden_state_shard(
            tmp_path,
            "train",
            0,
            0,
            hidden[:4],
            cache_identity=identity,
        ),
        write_hidden_state_shard(
            tmp_path,
            "train",
            1,
            4,
            hidden[4:],
            cache_identity=identity,
        ),
    ]
    finalize_teacher_cache(
        tmp_path,
        identity,
        {"train": records},
        expected_split_rows={"train": 7},
    )
    cached_hidden = TeacherHiddenCache(
        tmp_path,
        expected_identity=identity,
    ).take("train", [6, 1, 4, 0])
    direct_logits = torch.nn.functional.linear(
        hidden[[6, 1, 4, 0]],
        teacher_embedding[:29],
    )
    cached_logits = reconstruct_logits(
        cached_hidden,
        copied_frozen_embedding,
        vocab_size=29,
    )
    assert not copied_frozen_embedding.requires_grad
    assert torch.equal(cached_hidden, hidden[[6, 1, 4, 0]])
    assert torch.equal(cached_logits, direct_logits)


def test_benchmark_target_helper_uses_cache_or_live_teacher(monkeypatch):
    hidden = torch.arange(
        4 * HIDDEN_SIZE, dtype=torch.float32
    ).reshape(4, HIDDEN_SIZE)
    embedding = torch.arange(
        5 * HIDDEN_SIZE, dtype=torch.float32
    ).reshape(5, HIDDEN_SIZE)
    indices = np.array([3, 1], dtype=np.int64)

    class FakeCache:
        def take(self, split, requested, *, device):
            assert split == "train"
            assert device == "cpu"
            return hidden[requested]

    cached = _benchmark_teacher_logits(
        teacher=object(),
        teacher_cache=FakeCache(),
        token_ids=torch.zeros(2, 2, dtype=torch.long),
        indices=indices,
        tied_embedding=embedding,
        vocab_size=5,
        device="cpu",
    )
    torch.testing.assert_close(
        cached,
        torch.nn.functional.linear(hidden[indices], embedding),
    )

    expected_live = torch.randn(2, 5)
    monkeypatch.setattr(
        train_module,
        "teacher_logits",
        lambda teacher, token_ids, vocab_size: expected_live,
    )
    live = _benchmark_teacher_logits(
        teacher=object(),
        teacher_cache=None,
        token_ids=torch.zeros(2, 2, dtype=torch.long),
        indices=indices,
        tied_embedding=embedding,
        vocab_size=5,
        device="cpu",
    )
    assert live is expected_live


def test_random_shuffled_take_crosses_shards_and_preserves_duplicates(
    tmp_path,
):
    values = torch.arange(
        9 * HIDDEN_SIZE,
        dtype=torch.float32,
    ).reshape(9, HIDDEN_SIZE).to(torch.bfloat16)
    identity = _identity(torch.zeros(5, HIDDEN_SIZE, dtype=torch.bfloat16))
    records = [
        write_hidden_state_shard(tmp_path, "train", 0, 0, values[:2]),
        write_hidden_state_shard(tmp_path, "train", 1, 2, values[2:6]),
        write_hidden_state_shard(tmp_path, "train", 2, 6, values[6:]),
    ]
    finalize_teacher_cache(
        tmp_path,
        identity,
        {"train": records},
        expected_split_rows={"train": 9},
    )
    cache = TeacherHiddenCache(tmp_path, expected_identity=identity)
    indices = np.array([8, 0, 6, 2, 8, 5, 1], dtype=np.int64)
    assert torch.equal(cache.take("train", indices), values[indices])
    assert cache.take("train", []).shape == (0, HIDDEN_SIZE)
    with pytest.raises(ValueError, match="must be integers"):
        cache.take("train", [1.5])


def test_same_name_retry_rejects_stale_then_explicitly_rebuilds(tmp_path):
    first = torch.zeros(2, HIDDEN_SIZE, dtype=torch.bfloat16)
    second = torch.ones(2, HIDDEN_SIZE, dtype=torch.bfloat16)
    embedding = torch.zeros(5, HIDDEN_SIZE, dtype=torch.bfloat16)
    first_identity = _identity(embedding)
    second_identity = _identity(embedding, revision="revision-2")
    original = write_hidden_state_shard(
        tmp_path,
        "train",
        0,
        0,
        first,
        cache_identity=first_identity,
    )

    with pytest.raises(RuntimeError, match="checksum mismatch"):
        write_hidden_state_shard(
            tmp_path,
            "train",
            0,
            0,
            second,
            cache_identity=second_identity,
        )
    assert torch.equal(read_hidden_state_shard(tmp_path, original), first)
    with pytest.raises(RuntimeError, match="identity mismatch"):
        load_hidden_state_shard_record(
            tmp_path,
            "train",
            0,
            expected_identity=second_identity,
        )

    rebuilt = write_hidden_state_shard(
        tmp_path,
        "train",
        0,
        0,
        second,
        cache_identity=second_identity,
        replace_existing=True,
    )
    resumed = load_hidden_state_shard_record(
        tmp_path,
        "train",
        0,
        expected_identity=second_identity,
        expected_row_start=0,
        expected_rows=2,
    )
    assert resumed == rebuilt
    assert torch.equal(read_hidden_state_shard(tmp_path, rebuilt), second)


def test_partial_resume_sidecars_have_a_fast_structural_scan(
    tmp_path,
    monkeypatch,
):
    embedding = torch.zeros(5, HIDDEN_SIZE, dtype=torch.bfloat16)
    identity = _identity(
        embedding,
        dataset_manifest=_dataset_manifest(rows=4),
    )
    values = torch.randn(4, HIDDEN_SIZE, dtype=torch.bfloat16)
    first = write_hidden_state_shard(
        tmp_path,
        "train",
        0,
        0,
        values[:2],
        cache_identity=identity,
    )
    assert not (tmp_path / "manifest.json").exists()

    def unexpected_full_file_hash(*args, **kwargs):
        raise AssertionError("fast resume scan read the whole shard")

    monkeypatch.setattr(
        teacher_cache_module,
        "_file_sha256",
        unexpected_full_file_hash,
    )
    assert load_hidden_state_shard_record(
        tmp_path,
        "train",
        0,
        expected_identity=identity,
        expected_row_start=0,
        expected_rows=2,
        validate_checksum=False,
    ) == first
    assert load_hidden_state_shard_record(
        tmp_path,
        "train",
        1,
        expected_identity=identity,
        expected_row_start=2,
        expected_rows=2,
        validate_checksum=False,
    ) is None
