import math

import numpy as np
import torch

from architecture_verdict import (
    CONTEXT_LENGTH,
    DEFAULT_ARCHITECTURE,
    CausalKroneckerBlock,
    LanguageModel,
    ModelSpec,
    PRESETS,
    assert_matched_presets,
    batch_indices,
    cross_entropy,
    parameter_inventory,
    preset_inventory,
    solve_transformer_match,
    transformer_parameter_count,
    parser,
)
from coordinator import bootstrap_ci
import prepare_data


def test_rejected_architecture_is_not_the_executable_default():
    assert DEFAULT_ARCHITECTURE == "transformer"
    assert parser().parse_args([]).architecture == "transformer"


def test_preset_parameter_pairs_are_within_quarter_percent():
    assert_matched_presets()
    inventory = preset_inventory()
    assert inventory["kronecker-small"]["total"] == 4_369_408
    assert inventory["transformer-small"]["total"] == 4_360_192
    assert inventory["kronecker-large"]["total"] == 17_773_568
    assert inventory["transformer-large"]["total"] == 17_809_408
    for scale in ("small", "large"):
        left = inventory[f"kronecker-{scale}"]["total"]
        right = inventory[f"transformer-{scale}"]["total"]
        assert abs(left - right) / min(left, right) <= 0.0025


def test_xlarge_transformer_is_a_real_256m_reference():
    spec = PRESETS[("transformer", "xlarge")]
    assert spec == ModelSpec(
        "transformer", "xlarge", width=1024, depth=20, heads=16, mlp_width=2512
    )
    assert transformer_parameter_count(spec.width, spec.depth, spec.mlp_width) == 255_000_576


def test_packed_position_matrix_has_no_future_parameters():
    block = CausalKroneckerBlock(width=4, rank=2, depth=1)
    matrix = block.position_matrix()
    assert matrix.shape == (2, CONTEXT_LENGTH, CONTEXT_LENGTH)
    assert torch.count_nonzero(torch.triu(matrix, diagonal=1)) == 0
    expected = 2 * (CONTEXT_LENGTH * (CONTEXT_LENGTH + 1) // 2)
    assert block.position.numel() == expected


def test_kronecker_hidden_states_are_prefix_invariant():
    torch.manual_seed(1)
    spec = ModelSpec("kronecker", "tiny", width=8, depth=2, rank=2)
    model = LanguageModel(spec, activation_checkpointing=False).eval()
    first = torch.randint(0, 16_384, (1, CONTEXT_LENGTH))
    second = first.clone()
    second[:, 80:] = torch.randint(0, 16_384, (1, CONTEXT_LENGTH - 80))
    with torch.no_grad():
        left = model.hidden(first)
        right = model.hidden(second)
    torch.testing.assert_close(left[:, :80], right[:, :80], atol=1e-5, rtol=1e-5)


def test_vectorized_logits_agree_with_padded_prefix_reference():
    torch.manual_seed(2)
    spec = ModelSpec("kronecker", "tiny", width=4, depth=1, rank=1)
    model = LanguageModel(spec, activation_checkpointing=False).eval()
    tokens = torch.randint(0, 16_384, (1, CONTEXT_LENGTH))
    with torch.no_grad():
        vectorized = model(tokens)
        for position in (0, 31, 127, 255):
            padded = tokens.clone()
            padded[:, position + 1 :] = 0
            reference = model(padded)
            torch.testing.assert_close(
                vectorized[:, position], reference[:, position], atol=2e-5, rtol=2e-5
            )


def test_all_token_cross_entropy_is_finite_and_differentiable():
    torch.manual_seed(3)
    spec = ModelSpec("transformer", "tiny", width=8, depth=1, heads=2, mlp_width=16)
    model = LanguageModel(spec, activation_checkpointing=False)
    values = torch.randint(0, 16_384, (1, CONTEXT_LENGTH + 1))
    loss = cross_entropy(model(values[:, :-1]), values[:, 1:])
    loss.backward()
    assert torch.isfinite(loss)
    assert all(parameter.grad is not None for parameter in model.parameters())


def test_batches_are_rank_disjoint_and_cover_one_pass_without_replacement():
    size, local, world = 120, 5, 4
    values = []
    for step in range(1, size // (local * world) + 1):
        ranks = [batch_indices(size, step, local, rank, world, seed=7) for rank in range(world)]
        flat = np.concatenate(ranks)
        assert len(np.unique(flat)) == len(flat)
        values.extend(flat.tolist())
    assert len(set(values)) == size


def test_transformer_uses_dense_vocabulary_while_kronecker_is_factorized():
    kron = parameter_inventory(LanguageModel(PRESETS[("kronecker", "small")]))
    transformer = parameter_inventory(LanguageModel(PRESETS[("transformer", "small")]))
    assert kron["vocabulary"] == 2 * 128 * 128
    assert transformer["vocabulary"] == 16_384 * 128
    assert math.isclose(kron["total"] / transformer["total"], 1.0, rel_tol=0.0025)


def test_transformer_cached_decode_matches_full_causal_forward():
    torch.manual_seed(4)
    spec = ModelSpec("transformer", "tiny", width=8, depth=2, heads=2, mlp_width=16)
    model = LanguageModel(spec, activation_checkpointing=False).eval()
    tokens = torch.randint(0, 16_384, (1, CONTEXT_LENGTH))
    with torch.no_grad():
        full = model(tokens)[:, -1]
        _, caches = model.prefill_cache(tokens[:, :-1])
        cached, _ = model.decode_step(tokens[:, -1:], caches, CONTEXT_LENGTH - 1)
    torch.testing.assert_close(full, cached[:, -1], atol=2e-5, rtol=2e-5)


def test_configuration_solver_reproduces_locked_small_transformer():
    target = parameter_inventory(LanguageModel(PRESETS[("kronecker", "small")]))["total"]
    solved = solve_transformer_match(target, width=128, depth=5, heads=4)
    assert solved.mlp_width == 1008


def test_bootstrap_interval_is_deterministic():
    assert bootstrap_ci([0.1, 0.2, 0.3], draws=1000) == bootstrap_ci(
        [0.1, 0.2, 0.3], draws=1000
    )


def test_window_writer_keeps_exact_257_token_training_examples(tmp_path, monkeypatch):
    class Encoding:
        def __init__(self, ids):
            self.ids = ids

    class Tokenizer:
        def token_to_id(self, token):
            assert token == "<eos>"
            return 3

        def encode(self, text):
            return Encoding([5 + index % 10 for index, _ in enumerate(text)])

    monkeypatch.setattr(
        prepare_data,
        "records",
        lambda dataset, split, revision: iter(
            ({"text": "x" * 300}, {"text": "y" * 300})
        ),
    )
    raw = tmp_path / "train.raw"
    windows = prepare_data.write_windows(
        "wikitext", "train", Tokenizer(), raw, 0, "pinned"
    )
    assert windows == 2
    values = np.fromfile(raw, dtype=np.uint16).reshape(windows, CONTEXT_LENGTH + 1)
    assert values.shape == (2, 257)
    assert values.max() < 16_384


def test_dataset_revisions_are_immutable_commits():
    for source in prepare_data.DATASETS.values():
        assert len(source["revision"]) == 40
        int(source["revision"], 16)
