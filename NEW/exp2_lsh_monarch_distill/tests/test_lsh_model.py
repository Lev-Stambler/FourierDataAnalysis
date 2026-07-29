import pytest
import torch
import torch.nn.functional as F
from qwen_lsh_monarch.config import ArchitectureConfig
from qwen_lsh_monarch.model import LSHMonarchStudent
from qwen_lsh_monarch.monarch import MonarchLinear
from torch import nn


def codebook(vocab=31):
    generator = torch.Generator().manual_seed(3)
    return (
        torch.randint(0, 2, (vocab, 18), generator=generator).float() * 2 - 1
    )


def test_encode_is_288_active_features_plus_one_global_zero():
    codes = codebook()
    model = LSHMonarchStudent(ArchitectureConfig("sequential", 1), codes)
    ids = torch.arange(16)[None]
    encoded = model.encode(ids)
    assert encoded.shape == (1, 289)
    torch.testing.assert_close(encoded[0, :288], codes[ids].reshape(-1))
    assert encoded[0, 288] == 0


def test_identity_stack_reads_last_code_and_uses_tied_unembedding():
    codes = codebook()
    model = LSHMonarchStudent(ArchitectureConfig("sequential", 1), codes)
    model.stack = nn.Identity()
    ids = torch.stack([torch.arange(16), torch.arange(15, -1, -1)])
    expected = codes[ids[:, -1]]
    torch.testing.assert_close(model.hidden(ids), expected)
    torch.testing.assert_close(model(ids), F.linear(expected, codes))
    assert "tied_codebook" not in model.state_dict()
    assert not model.tied_codebook.requires_grad


@pytest.mark.parametrize(
    "form,depth,expansion,expected_maps",
    [
        ("sequential", 2, 1, 2),
        ("residual_one", 2, 1, 2),
        ("residual_ffn", 2, 1, 4),
        ("residual_ffn", 2, 4, 4),
    ],
)
def test_all_topologies_keep_exact_monarch_dimensions(
    form, depth, expansion, expected_maps
):
    config = ArchitectureConfig(form, depth, expansion)
    model = LSHMonarchStudent(config, codebook())
    maps = [
        module for module in model.modules()
        if isinstance(module, MonarchLinear)
    ]
    assert len(maps) == expected_maps
    assert all(module.nblocks == 17 for module in maps)
    allowed = {289, 289 * expansion}
    assert all(
        module.in_features in allowed and module.out_features in allowed
        for module in maps
    )
    assert model(torch.randint(0, 31, (2, 16))).shape == (2, 31)


def test_production_square_map_has_17_by_17_factors():
    layer = MonarchLinear(289, 289, nblocks=17)
    assert layer.factor1.shape == (1, 17, 17, 17)
    assert layer.factor2.shape == (1, 17, 17, 17)
    assert layer.factor1.numel() + layer.factor2.numel() == 9_826
    assert sum(parameter.numel() for parameter in layer.parameters()) == 10_115


def test_bad_shapes_and_code_values_fail_closed():
    with pytest.raises(ValueError, match="code width"):
        LSHMonarchStudent(
            ArchitectureConfig("sequential", 1), torch.ones(4, 17)
        )
    bad = codebook()
    bad[0, 0] = 0
    with pytest.raises(ValueError, match=r"\{-1,\+1\}"):
        LSHMonarchStudent(ArchitectureConfig("sequential", 1), bad)
    model = LSHMonarchStudent(
        ArchitectureConfig("sequential", 1), codebook()
    )
    with pytest.raises(ValueError, match="expected"):
        model(torch.randint(0, 31, (2, 15)))
