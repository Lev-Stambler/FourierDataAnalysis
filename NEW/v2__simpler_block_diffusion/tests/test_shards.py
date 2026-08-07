import json

import pytest

from v2_simpler_block_diffusion.data import manifest_sha256
from v2_simpler_block_diffusion.shards import _eligible_path, _select_paths, load_resolved_shards
from v2_simpler_block_diffusion.data import DataSource
from v2_simpler_block_diffusion.streaming_data import _rank_urls, _shared_file_partition


def source(source_id="docs", kind="document", subset="default"):
    return DataSource(source_id, "owner/repo", subset, "abc", "mit", 1.0, 1.0, True, kind)


def test_selection_is_stable_and_bounded():
    paths = [f"data/train-{index:05d}.parquet" for index in range(20)]
    assert _select_paths(source(), paths, 4) == _select_paths(source(), list(reversed(paths)), 4)
    assert len(_select_paths(source(), paths, 4)) == 4
    assert len(_select_paths(source(kind="conversation"), paths, 4)) == 20


def test_named_subset_filter():
    candidate = source("finemath-4plus", subset="finemath-4plus")
    assert _eligible_path(candidate, "finemath-4plus/train.parquet")
    assert not _eligible_path(candidate, "finemath-3plus/train.parquet")


def test_resolved_manifest_must_match_data_lock(tmp_path):
    data_path = tmp_path / "data.json"
    data_path.write_text("{}")
    resolved = tmp_path / "resolved.json"
    resolved.write_text(json.dumps({"schema": "v2-sbd-resolved-shards-v1", "data_manifest_sha256": "bad", "sources": []}))
    with pytest.raises(ValueError, match="do not match"):
        load_resolved_shards(resolved, data_path)


def test_resolved_urls_are_partitioned_before_dataset_load():
    urls = [f"u{index}" for index in range(16)]
    assert _rank_urls(urls, 2, 8) == ["u2", "u10"]
    assert _rank_urls(["a", "b"], 7, 8) == ["b"]
    assert _shared_file_partition(["a", "b"], 7, 8) == (3, 4)
    assert _shared_file_partition(["a", "b"], 0, 8) == (0, 4)
    assert _shared_file_partition([str(index) for index in range(8)], 7, 8) == (0, 1)


def test_resolved_manifest_prefers_verified_local_files(tmp_path):
    data_path = tmp_path / "data.json"
    data_path.write_text("locked")
    local = tmp_path / "train.parquet"
    local.write_bytes(b"test")
    resolved = tmp_path / "resolved.json"
    resolved.write_text(json.dumps({
        "schema": "v2-sbd-resolved-shards-v1",
        "data_manifest_sha256": manifest_sha256(data_path),
        "sources": [{
            "source_id": "docs",
            "dataset": "owner/repo",
            "revision": "abc",
            "format": "parquet",
            "files": ["data/train.parquet"],
            "local_files": [str(local)],
        }],
    }))
    assert load_resolved_shards(resolved, data_path)["docs"]["urls"] == [str(local)]
