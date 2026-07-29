import pytest
import torch
import torch.nn.functional as F

from qwen_fullwidth_distill.config import ArchitectureConfig, matching_monarch
from qwen_fullwidth_distill.model import FullWidthStudent, FullWidthStack
from qwen_fullwidth_distill.monarch import MonarchLinear


def tiny_config(operator="dense", form="sequential", depth=1, expansion=1):
    return ArchitectureConfig(
        operator, form, depth, expansion,
        context_length=4, embedding_width=4, monarch_blocks=4,
    )


def test_student_has_no_bottleneck_and_reads_last_slot():
    embedding = torch.arange(28, dtype=torch.float32).reshape(7, 4) / 10
    model = FullWidthStudent(tiny_config(), embedding, vocab_size=7)
    linear = model.stack.layers[0]
    with torch.no_grad():
        linear.weight.copy_(torch.eye(16))
        linear.bias.zero_()
    ids = torch.tensor([[0, 1, 2, 3], [3, 2, 1, 0]])
    expected_hidden = embedding[ids[:, -1]]
    torch.testing.assert_close(model.hidden(ids), expected_hidden)
    torch.testing.assert_close(model(ids), F.linear(expected_hidden, embedding))
    assert "tied_embedding" not in model.state_dict()
    assert not model.tied_embedding.requires_grad


@pytest.mark.parametrize(
    "form,depth,expansion,expected_maps",
    [
        ("sequential", 2, 1, 2),
        ("residual_one", 2, 1, 2),
        ("residual_ffn", 2, 1, 4),
        ("residual_ffn", 2, 4, 4),
    ],
)
def test_every_logical_map_keeps_full_width_except_explicit_expansion(
    form, depth, expansion, expected_maps
):
    config = tiny_config("monarch", form, depth, expansion)
    model = FullWidthStudent(config, torch.randn(9, 4), vocab_size=9)
    maps = [module for module in model.modules() if isinstance(module, MonarchLinear)]
    assert len(maps) == expected_maps
    allowed = {16, 16 * expansion}
    assert all(layer.in_features in allowed and layer.out_features in allowed for layer in maps)
    assert not any(
        layer.in_features == 16 and layer.out_features == 4 for layer in maps
    )
    assert model(torch.randint(0, 9, (2, 4))).shape == (2, 9)


def test_bad_context_shape_fails_closed():
    model = FullWidthStudent(tiny_config(), torch.randn(9, 4), vocab_size=9)
    with pytest.raises(ValueError, match="expected"):
        model(torch.randint(0, 9, (2, 3)))


def test_parameter_matching_ranks_are_fixed():
    square = matching_monarch(ArchitectureConfig("dense", "sequential", 2))
    expanded = matching_monarch(
        ArchitectureConfig("dense", "residual_ffn", 1, 4)
    )
    assert square.monarch_rank == 64
    assert expanded.monarch_rank == 102


def test_repetition_one_preserves_legacy_serialization_and_label():
    config = ArchitectureConfig("monarch", "residual_ffn", 4, 4)
    assert config.effective_depth == 4
    assert config.label == "monarch-r1-residual_ffn-d4-x4"
    assert "repetitions" not in config.to_dict()
    assert "residual_scale" not in config.to_dict()
    assert ArchitectureConfig(**config.to_dict()) == config


def test_loop_reuses_exact_modules_and_matches_manual_application():
    config = ArchitectureConfig(
        "monarch",
        "residual_ffn",
        2,
        1,
        context_length=4,
        embedding_width=4,
        monarch_blocks=4,
        repetitions=3,
        residual_scale="inverse_repetitions",
    )
    stack = FullWidthStack(config)
    assert len(stack.layers) == 2
    assert config.effective_depth == 6
    assert config.residual_multiplier == pytest.approx(1 / 3)

    value = torch.randn(3, 16, requires_grad=True)
    actual = stack(value)
    expected = value
    for _ in range(3):
        for layer in stack.layers:
            expected = layer(expected, 1 / 3)
    torch.testing.assert_close(actual, expected)
    actual.square().mean().backward()
    assert all(parameter.grad is not None for parameter in stack.parameters())


def test_loop_parameter_count_is_independent_of_repetition_count():
    embedding = torch.randn(9, 4)
    once = FullWidthStudent(
        ArchitectureConfig(
            "monarch",
            "residual_ffn",
            2,
            1,
            context_length=4,
            embedding_width=4,
            monarch_blocks=4,
        ),
        embedding,
        vocab_size=9,
    )
    looped = FullWidthStudent(
        ArchitectureConfig(
            "monarch",
            "residual_ffn",
            2,
            1,
            context_length=4,
            embedding_width=4,
            monarch_blocks=4,
            repetitions=8,
            residual_scale="inverse_repetitions",
        ),
        embedding,
        vocab_size=9,
    )
    assert once.trainable_parameter_count() == looped.trainable_parameter_count()
    assert looped.config.effective_depth == 16


def test_repetition_validation_fails_closed():
    with pytest.raises(ValueError, match="restricted"):
        ArchitectureConfig(
            "dense", "residual_ffn", 4, 4, repetitions=2
        ).validate()
    with pytest.raises(ValueError, match="single-pass"):
        ArchitectureConfig(
            "monarch",
            "residual_ffn",
            4,
            4,
            residual_scale="inverse_repetitions",
        ).validate()


def test_gated_kronecker_second_pass_initially_preserves_one_pass_function():
    config = ArchitectureConfig(
        "kronecker",
        "residual_ffn",
        2,
        1,
        context_length=4,
        embedding_width=4,
        monarch_blocks=4,
        repetitions=2,
        gated_repetitions=True,
    )
    stack = FullWidthStack(config)
    assert stack.repetition_gates is not None
    torch.testing.assert_close(
        stack.repetition_gates.detach(),
        torch.tensor([[1.0, 1.0], [0.0, 0.0]]),
    )
    value = torch.randn(3, 16)
    expected = value
    for layer in stack.layers:
        expected = layer(expected, config.residual_multiplier)
    torch.testing.assert_close(stack(value), expected)


def test_inverse_sqrt_depth_scale_is_serialized_and_applied():
    config = ArchitectureConfig(
        "monarch",
        "residual_ffn",
        8,
        1,
        context_length=4,
        embedding_width=4,
        monarch_blocks=4,
        residual_scale="inverse_sqrt_depth",
    )
    config.validate()
    assert config.residual_multiplier == pytest.approx(8 ** -0.5)
    assert config.label.endswith("-scaleinvsqrtdepth")
    assert config.to_dict()["residual_scale"] == "inverse_sqrt_depth"
    with pytest.raises(ValueError, match="restricted"):
        ArchitectureConfig(
            "dense",
            "residual_ffn",
            8,
            1,
            context_length=4,
            embedding_width=4,
            monarch_blocks=4,
            residual_scale="inverse_sqrt_depth",
        ).validate()
