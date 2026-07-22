import math

import numpy as np
import pytest
import torch

from fourier_output_kl.model import (
    OutputSelectorWalshStudent,
    decode_compact_student,
    deterministic_unique_supports,
    distribution_metrics,
    encode_compact_student,
    exact_walsh_ste,
    functional_diversity_loss,
    hard_topk_mask,
    repair_duplicate_supports,
    sparse_scores,
    teacher_student_kl,
)


def test_unique_support_initialization_has_expected_degrees():
    rows, degree = deterministic_unique_supports(31, 4096, max_degree=6, seed=7)
    assert len({tuple(row) for row in rows}) == len(rows)
    np.testing.assert_array_equal((rows < 31).sum(1), degree)
    np.testing.assert_array_equal(rows[:31, 0], np.arange(31))
    assert degree.min() == 1
    assert degree.max() == 6


def test_target_structured_supports_are_unique_and_local():
    rows, degree = deterministic_unique_supports(
        4096, 16384, max_degree=8, seed=3,
        support_layout="semantic_structured_v2", token_bits=32, target_tokens=16,
    )
    assert len({tuple(row[:int(k)]) for row, k in zip(rows, degree)}) == len(rows)
    nonsingleton = degree > 1
    active = np.arange(8)[None, :] < degree[:, None]
    all_target = np.all((rows < 512) | ~active, axis=1)
    same_token = np.all(
        (rows // 32 == (rows // 32)[:, :1]) | ~active, axis=1
    )
    assert all_target[nonsingleton].mean() > 0.30
    assert same_token[nonsingleton].mean() > 0.30


def test_exact_walsh_forward_is_exhaustive_parity():
    n_bits = 5
    bits = torch.tensor([
        [(value >> bit) & 1 for bit in range(n_bits)]
        for value in range(1 << n_bits)
    ], dtype=torch.float32)
    theta = torch.zeros((3, n_bits), requires_grad=True)
    supports = ((0,), (1, 3), (0, 2, 4))
    degree = torch.tensor([len(value) for value in supports])
    with torch.no_grad():
        for row, support in enumerate(supports):
            theta[row, list(support)] = 0.1
    actual = exact_walsh_ste(bits, theta, degree, max_degree=3)
    expected = torch.stack([
        1.0 - 2.0 * torch.remainder(bits[:, list(support)].sum(1), 2.0)
        for support in supports
    ], dim=1)
    torch.testing.assert_close(actual, expected, atol=0, rtol=0)


def test_product_vertex_backward_and_tangent_projection():
    bits = torch.tensor(
        [[0, 1, 1, 0], [1, 0, 1, 1], [1, 1, 0, 0]],
        dtype=torch.float32,
    )
    theta = torch.tensor(
        [[0.1, 0.0, 0.2, 0.0], [0.0, 0.2, 0.0, 0.0]],
        requires_grad=True,
    )
    degree = torch.tensor([2, 1])
    upstream = torch.tensor(
        [[0.2, -0.4], [0.7, 0.3], [-0.1, 0.5]], dtype=torch.float32
    )
    character = exact_walsh_ste(bits, theta, degree, 2)
    (character * upstream).sum().backward()
    mask = hard_topk_mask(theta.detach(), degree, 2)
    expected = -2.0 * ((upstream * character.detach()).t() @ bits)
    expected *= 1.0 - 2.0 * mask.float()
    expected -= expected.mean(1, keepdim=True)
    torch.testing.assert_close(theta.grad, expected)
    torch.testing.assert_close(theta.grad.sum(1), torch.zeros(2), atol=1e-6, rtol=0)


def test_topk_gauge_and_centered_selector_scores():
    torch.manual_seed(3)
    model = OutputSelectorWalshStudent(
        8, 32, seed=1, max_degree=4, char_chunk=16,
        checkpoint_chunks=False,
    )
    bits = torch.randint(0, 2, (17, 8), dtype=torch.float32)
    model.eval()
    before = model(bits)
    with torch.no_grad():
        model.theta.add_(torch.randn(32, 1))
    after = model(bits)
    torch.testing.assert_close(after, before)
    torch.testing.assert_close(after.sum(1), torch.zeros(17), atol=1e-6, rtol=0)
    torch.testing.assert_close(model.score(bits, 0), after[:, 0])
    torch.testing.assert_close(model.score(bits, torch.ones(17)), after[:, 1])


def test_softmax_kl_equals_bce_minus_teacher_entropy_and_gradients():
    gap = torch.tensor([-2.0, -0.2, 0.4, 1.8], requires_grad=True)
    target = torch.tensor([0.1, 0.49, 0.7, 0.95])
    scores = torch.stack((-0.5 * gap, 0.5 * gap), dim=-1)
    kl = teacher_student_kl(scores, target)
    bce = torch.nn.functional.binary_cross_entropy_with_logits(gap, target)
    entropy = torch.nn.functional.binary_cross_entropy(target, target)
    torch.testing.assert_close(kl, bce - entropy)
    kl_gradient = torch.autograd.grad(kl, gap, retain_graph=True)[0]
    bce_gradient = torch.autograd.grad(bce, gap)[0]
    torch.testing.assert_close(kl_gradient, bce_gradient)
    probability = scores.softmax(-1)
    torch.testing.assert_close(probability.sum(1), torch.ones(4))


def test_duplicate_repair_preserves_scores_and_optimizer_state_shape():
    model = OutputSelectorWalshStudent(
        9, 24, seed=4, max_degree=4, char_chunk=8,
        checkpoint_chunks=False,
    )
    optimizer = torch.optim.AdamW(model.parameters(), lr=0.01)
    bits = torch.randint(0, 2, (37, 9), dtype=torch.float32)
    model.train()
    teacher_student_kl(model(bits), torch.rand(37)).backward()
    optimizer.step()
    with torch.no_grad():
        model.theta[1].copy_(model.theta[0])
        model.degree[1] = model.degree[0]
    model.eval()
    before = model(bits).detach()
    result = repair_duplicate_supports(model, optimizer, seed=19)
    after = model(bits).detach()
    torch.testing.assert_close(after, before, atol=2e-6, rtol=2e-6)
    # The optimizer step may independently move other tiny-gap rows onto the
    # same support; repair must fix at least the injected collision and all
    # additional collisions without changing the represented function.
    assert result["duplicates_repaired"] >= 1
    rows = model.hard_index_rows().numpy()
    assert len(np.unique(rows, axis=0)) == len(rows)


def test_compact_output_selector_round_trip():
    rng = np.random.default_rng(9)
    model = OutputSelectorWalshStudent(
        64, 512, seed=2, max_degree=8, char_chunk=128,
        checkpoint_chunks=False,
    )
    with torch.no_grad():
        model.coefficient.normal_(std=0.1)
        model.bias.fill_(0.3)
    model.eval()
    bits = rng.integers(0, 2, (31, 64), dtype=np.uint8)
    compact = encode_compact_student(model.sparse_state(), block_size=64)
    decoded = decode_compact_student(compact)
    assert decoded["schema"] == "centered-binary-output-selector-v1"
    live = model(torch.from_numpy(bits).float()).detach().numpy()
    restored = sparse_scores(bits, compact, chunk=128)
    np.testing.assert_allclose(restored, live, atol=3e-4, rtol=3e-4)
    metrics = distribution_metrics(restored, rng.uniform(0.01, 0.99, len(bits)))
    assert math.isfinite(metrics["kl"])
    assert "mae_confidence_90_100" in metrics


def test_mask_gradients_are_live_with_nonzero_coefficients():
    model = OutputSelectorWalshStudent(
        8, 32, seed=5, max_degree=4, char_chunk=16,
        coefficient_std=0.02, checkpoint_chunks=False,
    )
    bits = torch.randint(0, 2, (64, 8), dtype=torch.float32)
    target = torch.sigmoid(1.5 * (1.0 - 2.0 * bits[:, 0]))
    loss = teacher_student_kl(model(bits), target)
    loss.backward()
    assert model.theta.grad is not None
    assert torch.isfinite(model.theta.grad).all()
    assert float(model.theta.grad.abs().sum()) > 0
    assert float(model.theta.grad.sum(1).abs().max()) < 1e-5


def test_diversity_gradient_stays_finite_for_constant_features():
    model = OutputSelectorWalshStudent(
        8, 32, seed=5, max_degree=4, char_chunk=16,
        checkpoint_chunks=False,
    )
    # Every input column is constant, which previously differentiated through
    # sqrt(0) and poisoned the full mask bank with NaNs.
    bits = torch.zeros((64, 8), dtype=torch.float32)
    loss, metrics = functional_diversity_loss(
        model, bits, character_sample=16
    )
    loss.backward()
    assert math.isfinite(float(loss.detach()))
    assert model.theta.grad is not None
    assert torch.isfinite(model.theta.grad).all()
    assert metrics["mean_abs_correlation"] == pytest.approx(0.0)


@pytest.mark.parametrize("probability", [0.2, 0.5, 0.8])
def test_initial_bias_tracks_teacher_prior(probability):
    model = OutputSelectorWalshStudent(
        7, 16, seed=1, max_degree=3,
        initial_probability=probability, coefficient_std=0.0,
        checkpoint_chunks=False,
    )
    bits = torch.zeros((3, 7))
    predicted = model(bits).softmax(-1)[:, 1]
    torch.testing.assert_close(
        predicted, torch.full((3,), probability), atol=1e-6, rtol=0
    )
