from __future__ import annotations

import numpy as np
import pytest
import torch

from dlx.analysis.character_response import (
    architecture_spectrum_overlap,
    character_mechanism_analysis,
    character_ntk_rayleigh,
    enumerate_supports,
    standardize_character_kernel,
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


def test_walsh_supports_and_ntk_gradient_identity() -> None:
    contexts = torch.tensor(
        [[0, 0, 0, 0], [0, 0, 1, 1], [1, 1, 0, 1], [1, 1, 1, 0]],
        dtype=torch.long,
    )
    character = walsh_character(contexts, (1, 2))
    torch.testing.assert_close(character, torch.tensor([1.0, 1.0, -1.0, -1.0]))
    assert len(enumerate_supports((1, 2, 4, 8, 16, 32, 64))) == 63

    torch.manual_seed(8)
    model = CausalTransformer(
        TransformerConfig(
            vocab=2,
            ctx_len=4,
            d_model=8,
            n_layers=1,
            n_heads=2,
            position_encoding="nope",
        )
    )
    direct = character_ntk_rayleigh(model, contexts, (1, 2))
    gradients = []
    for context in contexts:
        model.zero_grad(set_to_none=True)
        logits = model(context[None, :])[:, -1, :]
        gradient = torch.autograd.grad(
            logits[0, 1] - logits[0, 0], tuple(model.parameters())
        )
        gradients.append(torch.cat([value.reshape(-1) for value in gradient]))
    jacobian = torch.stack(gradients)
    kernel = jacobian @ jacobian.T
    expected = float(character @ kernel @ character / len(character))
    assert direct == pytest.approx(expected, rel=2e-5)


def test_support_energy_and_architecture_overlap_are_exact() -> None:
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
    overlap = architecture_spectrum_overlap(energy, {"1": -1.0, "4": 2.0, "1,4": 0.5})
    assert overlap == pytest.approx(0.25 / 0.35)


def test_character_kernel_standardizes_each_architecture() -> None:
    rows = []
    for architecture, multiplier in (("a", 1.0), ("b", 10.0)):
        for support, response in (((1,), 1.0), ((2,), 2.0), ((1, 2), 4.0)):
            for seed in (0, 1):
                rows.append(
                    {
                        "architecture": architecture,
                        "support": support,
                        "seed": seed,
                        "ntk_rayleigh": multiplier * response,
                    }
                )
    kernel = standardize_character_kernel(rows)
    for values in kernel.values():
        assert np.mean(list(values.values())) == pytest.approx(0.0, abs=1e-12)
        assert np.std(list(values.values())) == pytest.approx(1.0)


def test_mechanism_analysis_clusters_supports_and_detects_direction() -> None:
    character = []
    ntk = []
    supports = ((1,), (2,), (4,), (1, 2), (1, 4), (1, 2, 4))
    for architecture in ("alibi", "reverse_alibi"):
        response_rows = []
        for support in supports:
            radius = max(support)
            signed_radius = np.log(radius) * (1 if architecture == "alibi" else -1)
            response = np.exp(4.0 - signed_radius - 0.2 * len(support))
            response_rows.append({"support": support, "ntk_rayleigh": response})
            for seed in range(3):
                character.append(
                    {
                        "architecture": architecture,
                        "support": support,
                        "character_hardness": -np.log(response) + seed * 1e-4,
                        "example_grid": [0, 10, 100],
                        "floor_independent": {"half_best_learning_at": 100},
                    }
                )
        for seed in range(2):
            ntk.append(
                {
                    "architecture": architecture,
                    "rows": response_rows,
                }
            )
    result = character_mechanism_analysis(
        character, ntk, bootstrap_samples=200, bootstrap_seed=7
    )
    assert result["regression"]["mean_log_ntk_response_coefficient"] < 0.0
    assert result["degree_one_radius_spearman"]["alibi"] > 0.0
    assert result["degree_one_radius_spearman"]["reverse_alibi"] < 0.0
    assert result["mechanism_gate_passed"]
