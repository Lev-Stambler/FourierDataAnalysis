import json

import torch
from safetensors.torch import save_file

from v2_simpler_block_diffusion.data import manifest_sha256
from v2_simpler_block_diffusion.data import DataSource
from v2_simpler_block_diffusion.token_cache import TokenCacheMixture, _context_quotas


def test_rank_cache_cycles_without_network(tmp_path):
    manifest = tmp_path / "manifest.json"
    manifest.write_text("locked")
    cache = tmp_path / "cache"
    cache.mkdir()
    save_file(
        {
            "input_ids": torch.tensor([[1, 2], [3, 4]], dtype=torch.int32),
            "eligible_mask": torch.tensor([[True, False], [True, True]]),
        },
        cache / "rank-00-chunk-00000.safetensors",
    )
    (cache / "rank-00.json").write_text(json.dumps({
        "schema": "v2-sbd-token-cache-v1",
        "data_manifest_sha256": manifest_sha256(manifest),
        "stage": 0,
        "files": ["rank-00-chunk-00000.safetensors"],
    }))
    iterator = TokenCacheMixture(cache, rank=0, data_manifest_path=manifest, stage=0)
    assert next(iterator).input_ids.tolist() == [1, 2]
    state = iterator.state_dict()
    assert next(iterator).eligible_mask.tolist() == [True, True]
    assert next(iterator).input_ids.tolist() == [1, 2]
    resumed = TokenCacheMixture(cache, rank=0, data_manifest_path=manifest, stage=0)
    resumed.load_state_dict(state)
    assert next(resumed).input_ids.tolist() == [3, 4]


def test_context_quotas_are_exact_and_deterministic():
    sources = [
        DataSource("a", "x/a", None, "r", "mit", 0.55, 0.5, True),
        DataSource("b", "x/b", None, "r", "mit", 0.20, 0.5, True),
        DataSource("c", "x/c", None, "r", "mit", 0.10, 0.0, True),
        DataSource("d", "x/d", None, "r", "mit", 0.10, 0.0, True),
        DataSource("e", "x/e", None, "r", "mit", 0.05, 0.0, True),
    ]
    quotas = _context_quotas(sources, stage=0, contexts=4096)
    assert quotas == {"a": 2253, "b": 819, "c": 410, "d": 409, "e": 205}
    assert sum(quotas.values()) == 4096
