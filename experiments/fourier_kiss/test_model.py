import math

import numpy as np
import pytest
import torch

from fourier_kiss.model import (
    DirectWalshStudent,
    binary_metrics,
    decode_compact_student,
    deterministic_mask_rows,
    encode_compact_student,
    logspace_ste_characters,
    repair_duplicate_masks,
    sampled_diversity_loss,
    sparse_logits,
)


def test_binary_metrics_report_positive_rates_and_normalized_information():
    target = np.array([0.1, 0.2, 0.8, 0.9])
    logits = np.log(target / (1.0 - target))
    metrics = binary_metrics(logits, target)
    assert metrics["teacher_positive_rate"] == pytest.approx(0.5)
    assert metrics["student_positive_rate"] == pytest.approx(0.5)
    assert metrics["balanced_agreement"] == pytest.approx(1.0)
    assert metrics["hard_mutual_information_bits"] == pytest.approx(1.0)
    assert metrics["hard_mutual_information_fraction"] == pytest.approx(1.0)
    assert metrics["absolute_error_variance"] == pytest.approx(0.0, abs=1e-15)


def test_deterministic_masks_are_prefix_stable_and_cover_bits():
    small, small_degree = deterministic_mask_rows(31, 64, seed=7)
    large, large_degree = deterministic_mask_rows(31, 128, seed=7)
    np.testing.assert_array_equal(large[:64], small)
    np.testing.assert_array_equal(large_degree[:64], small_degree)
    np.testing.assert_array_equal(small[:31, 0], np.arange(31))
    assert np.all(small_degree[:31] == 1)
    assert np.all((small_degree[31:] >= 2) & (small_degree[31:] <= 8))


def test_logspace_ste_forward_is_exact_xor_and_gradients_live():
    bits = torch.tensor(
        [[0, 0, 0, 0], [1, 0, 1, 0], [1, 1, 1, 1]],
        dtype=torch.float32,
    )
    theta = torch.tensor(
        [[8.0, -8.0, 8.0, -8.0], [-8.0, 8.0, -8.0, -8.0]],
        requires_grad=True,
    )
    degree = torch.tensor([2, 1])
    actual = logspace_ste_characters(bits, theta, degree, max_degree=2)
    hard = torch.tensor([[1, 0, 1, 0], [0, 1, 0, 0]], dtype=torch.float32)
    expected = 1.0 - 2.0 * torch.remainder(bits @ hard.t(), 2.0)
    torch.testing.assert_close(actual, expected, atol=0, rtol=0)
    actual.square().sum().backward()
    assert theta.grad is not None
    assert torch.isfinite(theta.grad).all()
    assert float(theta.grad.abs().sum()) > 0


def test_model_hard_masks_keep_fixed_degrees():
    model = DirectWalshStudent(19, 48, seed=2, char_chunk=16)
    rows = model.hard_index_rows().numpy()
    np.testing.assert_array_equal((rows < 19).sum(axis=1), model.degree.numpy())


def test_diversity_penalizes_duplicate_but_not_disjoint_masks():
    model = DirectWalshStudent(16, 4, seed=1, max_degree=2, char_chunk=4)
    with torch.no_grad():
        model.degree.fill_(2)
        model.theta.fill_(-8)
        for row, columns in enumerate(((0, 1), (0, 1), (4, 5), (8, 9))):
            model.theta[row, list(columns)] = 8
    loss, diagnostics = sampled_diversity_loss(model, sample_size=4, margin=0.8)
    assert float(loss.detach()) > 0
    assert diagnostics["max_jaccard"] == pytest.approx(1.0)
    with torch.no_grad():
        model.theta[1].fill_(-8)
        model.theta[1, [2, 3]] = 8
    disjoint, diagnostics = sampled_diversity_loss(
        model, sample_size=4, margin=0.8
    )
    assert float(disjoint.detach()) == pytest.approx(0.0)
    assert diagnostics["max_jaccard"] == pytest.approx(0.0)


def test_duplicate_repair_preserves_logits_and_makes_masks_unique():
    model = DirectWalshStudent(12, 8, seed=4, char_chunk=4)
    with torch.no_grad():
        model.theta[1].copy_(model.theta[0])
        model.degree[1] = model.degree[0]
        model.coefficient.copy_(torch.linspace(-0.4, 0.3, 8))
        model.bias.fill_(0.2)
    optimizer = torch.optim.AdamW(model.parameters(), lr=0.01)
    bits = torch.randint(0, 2, (17, 12), dtype=torch.float32)
    model.eval()
    before = model(bits).detach()
    result = repair_duplicate_masks(model, optimizer, seed=91)
    after = model(bits).detach()
    torch.testing.assert_close(after, before, atol=1e-6, rtol=1e-6)
    assert result["duplicates_repaired"] == 1
    rows = model.hard_index_rows().numpy()
    assert len(np.unique(rows, axis=0)) == len(rows)


def test_compact_codec_round_trip_and_logit_error_is_small():
    rng = np.random.default_rng(17)
    model = DirectWalshStudent(4096, 1024, seed=3, char_chunk=256)
    with torch.no_grad():
        model.coefficient.normal_(std=0.1)
        model.bias.fill_(-0.3)
    state = model.sparse_state()
    compact = encode_compact_student(state, block_size=64)
    decoded = decode_compact_student(compact)
    assert compact["index_bits"] == 12
    assert len(compact["packed_indices"]) == (
        len(state["indices"]) * 12 + 7
    ) // 8
    np.testing.assert_array_equal(decoded["degrees"], state["degrees"])
    np.testing.assert_array_equal(decoded["indices"], state["indices"])
    bits = rng.integers(0, 2, (23, 4096), dtype=np.uint8)
    original = sparse_logits(bits, state, chunk=256)
    quantized = sparse_logits(bits, compact, chunk=256)
    np.testing.assert_allclose(quantized, original, atol=2e-4)


def test_tiny_direct_training_beats_bias_only():
    torch.manual_seed(5)
    bits = torch.randint(0, 2, (512, 8), dtype=torch.float32)
    target_logit = 1.8 * (1.0 - 2.0 * bits[:, 0])
    target = torch.sigmoid(target_logit)
    model = DirectWalshStudent(
        8, 32, seed=2, max_degree=4, char_chunk=16,
        initial_probability=float(target.mean()), checkpoint_chunks=False,
    )
    optimizer = torch.optim.AdamW([
        {"params": [model.theta], "lr": 0.1, "weight_decay": 0.0},
        {"params": [model.coefficient, model.bias], "lr": 0.1},
    ])
    baseline = torch.nn.functional.binary_cross_entropy_with_logits(
        model(bits).detach(), target
    )
    for _ in range(80):
        optimizer.zero_grad(set_to_none=True)
        loss = torch.nn.functional.binary_cross_entropy_with_logits(
            model(bits), target
        )
        loss.backward()
        torch.nn.utils.clip_grad_norm_(model.parameters(), 1.0)
        optimizer.step()
    final = torch.nn.functional.binary_cross_entropy_with_logits(
        model(bits).detach(), target
    )
    assert math.isfinite(float(final))
    assert float(final) < float(baseline) - 0.05
