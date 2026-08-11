from __future__ import annotations

import pytest
import torch

from dlx.analysis.character_response import (
    architecture_spectrum_overlap,
    empirical_character_ce_kernel,
    enumerate_supports,
    walsh_character,
)
from dlx.learners.transformer import (
    CausalTransformer,
    TransformerConfig,
    _apply_rope,
)
from dlx.profiles.sampled_degree import marginal_support_energy


def test_rope_preserves_norm_and_depends_only_on_relative_position() -> None:
    torch.manual_seed(3)
    values = torch.randn(1, 1, 4, 8)
    rotated = _apply_rope(values, base=10_000.0)
    torch.testing.assert_close(rotated.norm(dim=-1), values.norm(dim=-1))

    query = torch.randn(8)
    key = torch.randn(8)
    first = torch.zeros(1, 1, 4, 8)
    second = torch.zeros(1, 1, 4, 8)
    first[0, 0, 2] = query
    first[0, 0, 0] = key
    second[0, 0, 3] = query
    second[0, 0, 1] = key
    first = _apply_rope(first, base=10_000.0)
    second = _apply_rope(second, base=10_000.0)
    torch.testing.assert_close(
        torch.dot(first[0, 0, 2], first[0, 0, 0]),
        torch.dot(second[0, 0, 3], second[0, 0, 1]),
    )


def test_sliding_attention_excludes_tokens_beyond_window() -> None:
    cfg = TransformerConfig(
        vocab=8,
        ctx_len=128,
        d_model=32,
        n_layers=2,
        n_heads=4,
        position_encoding="nope",
        attention_window=64,
    )
    model = CausalTransformer(cfg).eval()
    left = torch.randint(0, 8, (1, 128))
    right = left.clone()
    right[:, 0] = (right[:, 0] + 1) % 8
    with torch.no_grad():
        left_logits = model(left)
        right_logits = model(right)
    torch.testing.assert_close(left_logits[:, -1], right_logits[:, -1])


def test_fourier_character_and_support_enumeration() -> None:
    contexts = torch.tensor(
        [[0, 0, 0, 0], [0, 0, 1, 1], [1, 1, 0, 1], [1, 1, 1, 0]],
        dtype=torch.long,
    )
    character = walsh_character(contexts, (1, 2))
    torch.testing.assert_close(character, torch.tensor([1.0, 1.0, -1.0, -1.0]))
    assert len(enumerate_supports((1, 2, 4, 8, 16, 32, 64))) == 63


def test_support_energy_and_ce_hardness_overlap_are_exact() -> None:
    chains = [
        [
            {"conditional_collision_energy": 0.1, "support_columns": []},
            {"conditional_collision_energy": 0.3, "support_columns": [1]},
            {"conditional_collision_energy": 0.4, "support_columns": [1, 0]},
        ],
        [
            {"conditional_collision_energy": 0.1, "support_columns": []},
            {"conditional_collision_energy": 0.2, "support_columns": [0]},
            {"conditional_collision_energy": 0.5, "support_columns": [0, 1]},
        ],
    ]
    energy = marginal_support_energy(chains, (1, 4), feature_degree=2)
    assert energy == pytest.approx({"1": 0.05, "4": 0.1, "1,4": 0.2})
    overlap = architecture_spectrum_overlap(energy, {"1": 0.2, "4": 0.8, "1,4": 0.5})
    assert overlap == pytest.approx(0.19 / 0.35)


def test_empirical_character_kernel_is_median_heldout_ce_hardness() -> None:
    rows = []
    for architecture, offset in (("a", 0.0), ("b", 0.2)):
        for support, base in (((1,), 0.1), ((2,), 0.7)):
            for seed, noise in enumerate((0.02, -0.01, 0.0)):
                rows.append(
                    {
                        "architecture": architecture,
                        "support": support,
                        "seed": seed,
                        "character_hardness": base + offset + noise,
                    }
                )
    kernel = empirical_character_ce_kernel(
        rows,
        architectures=("a", "b"),
        supports=((1,), (2,)),
        seeds=(0, 1, 2),
    )
    assert kernel["a"] == pytest.approx({"1": 0.1, "2": 0.7})
    assert kernel["b"] == pytest.approx({"1": 0.3, "2": 0.9})


def test_empirical_character_kernel_rejects_missing_cells() -> None:
    with pytest.raises(ValueError, match="incomplete"):
        empirical_character_ce_kernel(
            [], architectures=("a",), supports=((1,),), seeds=(0,)
        )
