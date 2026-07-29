import json

import numpy as np
import pytest
from qwen_kron_distill.config import (
    RESULT_SCHEMA,
    SCREEN_EXAMPLES,
    Architecture,
    Cell,
    canonical_hash,
)
from qwen_kron_distill.train import (
    DistributedContext,
    _microbatch_size,
    _rank_indices,
    completed_result,
    result_path,
    student_path,
)


def context(rank: int) -> DistributedContext:
    return DistributedContext(
        rank=rank,
        local_rank=rank,
        world_size=4,
        device=None,
        owns_process_group=False,
    )


def test_ddp_rank_indices_are_disjoint_and_cover_global_batch():
    order = np.arange(2048)
    shards = [
        _rank_indices(order, 512, context=context(rank), local_batch=128)
        for rank in range(4)
    ]

    assert all(len(shard) == 128 for shard in shards)
    np.testing.assert_array_equal(
        np.concatenate(shards),
        order[512:1024],
    )
    assert len(set(np.concatenate(shards).tolist())) == 512


def test_microbatch_fallback_is_an_exact_accumulation(monkeypatch):
    monkeypatch.setenv("QWEN_KRON_MICROBATCH", "64")
    assert _microbatch_size(128) == 64
    monkeypatch.setenv("QWEN_KRON_MICROBATCH", "96")
    with pytest.raises(ValueError, match="divisor"):
        _microbatch_size(128)


def test_completed_result_requires_matching_checksum_and_weights(tmp_path):
    cell = Cell(
        stage="depth",
        architecture=Architecture(factor_order=2, depth=1, rank=1),
        target_examples=SCREEN_EXAMPLES,
    )
    value = {
        "schema": RESULT_SCHEMA,
        "status": "complete",
        "label": cell.label,
        "cell": cell.to_dict(),
        "examples_seen": SCREEN_EXAMPLES,
    }
    value["result_sha256"] = canonical_hash(value)
    path = result_path(str(tmp_path), cell)
    path.parent.mkdir(parents=True)
    path.write_text(json.dumps(value))
    student_path(str(tmp_path), cell).write_bytes(b"weights")

    assert completed_result(str(tmp_path), cell) == value

    value["examples_seen"] += 1
    path.write_text(json.dumps(value))
    assert completed_result(str(tmp_path), cell) is None
