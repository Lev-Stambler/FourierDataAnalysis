import hashlib
import numpy as np
import pytest
import torch
from collections import Counter, deque

from fourier_full_next_token_kl.streaming import (
    CONTEXT_TOKENS, TOKEN_BITS, fresh_context_batch,
)
from fourier_full_next_token_kl.sweep import (
    LR_SWEEP_GRID, lr_label, select_best_trial,
)

from fourier_full_next_token_kl.model import (
    ARTIFACT_SCHEMA,
    InputOutputWalshStudent,
    balanced_projection_vertices,
    cached_duplicate_collision_loss,
    decode_compact_student,
    deterministic_input_output_supports,
    distribution_metrics,
    encode_compact_student,
    estimate_compact_artifact_bytes,
    exact_joint_collision_audit,
    exact_joint_collision_groups,
    exact_kl_backward_loss,
    exact_walsh_ste,
    full_teacher_student_kl,
    full_teacher_student_kl_rows,
    hard_frequency_scatter_ste,
    hard_output_mask,
    hard_support_margin,
    hard_topk_mask,
    joint_cartesian_avg_input_degree,
    joint_cartesian_degree_counts,
    load_compact_prefix,
    load_compact_student,
    merge_duplicate_coefficients,
    repair_duplicate_supports,
    sample_collision_u_statistic,
    sampled_joint_collision_loss,
    sparse_scores,
    support_flip_metrics,
    unique_output_vertices,
    validate_balanced_token_artifact,
    walsh_synthesis,
)


def _vertices(vocab=23, output_bits=5):
    return np.arange(vocab, dtype=np.uint32) % (1 << output_bits)


def test_lr_sweep_grid_and_labels_are_fixed():
    assert LR_SWEEP_GRID == (
        (0.03, 0.01), (0.03, 0.03),
        (0.1, 0.01), (0.1, 0.03),
        (0.3, 0.01), (0.3, 0.03),
    )
    assert lr_label(0.03) == "0p03"
    assert lr_label(0.3) == "0p3"


def test_lr_sweep_selection_uses_finite_final_fresh_kl():
    results = [
        {"final_kl": float("nan"), "ste_lr": 0.3, "coefficient_lr": 0.03},
        {"final_kl": 2.0, "ste_lr": 0.1, "coefficient_lr": 0.03},
        {"final_kl": 2.0, "ste_lr": 0.03, "coefficient_lr": 0.01},
        {"final_kl": 2.2, "ste_lr": 0.03, "coefficient_lr": 0.03},
    ]
    assert select_best_trial(results) == results[2]
    with pytest.raises(RuntimeError, match="finite final KL"):
        select_best_trial([results[0]])


def test_output_vertices_are_collision_free_and_deterministic():
    codes = np.zeros((29, 8), dtype=np.uint8)
    codes[:, 0] = np.arange(29) & 1
    first = unique_output_vertices(codes, output_bits=5, seed=7)
    second = unique_output_vertices(codes, output_bits=5, seed=7)
    np.testing.assert_array_equal(first, second)
    assert len(np.unique(first)) == len(first)
    assert first.max() < 32


def test_balanced_projection_vertices_are_unique_balanced_and_deterministic():
    rng = np.random.default_rng(41)
    scores = rng.normal(size=(29, 5)).astype(np.float32)
    first = balanced_projection_vertices(scores, output_bits=5)
    second = balanced_projection_vertices(scores, output_bits=5)
    np.testing.assert_array_equal(first, second)
    assert len(np.unique(first)) == 29
    assert first.max() < 32
    for bit in range(5):
        prefix_mask = (1 << bit) - 1
        for prefix in np.unique(first & prefix_mask):
            cell = first[(first & prefix_mask) == prefix]
            left = np.count_nonzero(((cell >> bit) & 1) == 0)
            right = len(cell) - left
            assert abs(left - right) <= 1


def test_balanced_token_artifact_validation_is_hash_pinned():
    rng = np.random.default_rng(42)
    vocab_size = 29
    packed = rng.integers(0, 256, (vocab_size, 1), dtype=np.uint8)
    while len(np.unique(packed, axis=0)) != vocab_size:
        packed = rng.integers(0, 256, (vocab_size, 1), dtype=np.uint8)
    vertices = balanced_projection_vertices(
        rng.normal(size=(vocab_size, 5)).astype(np.float32), output_bits=5
    )
    metadata = {
        "model_revision": "pinned",
        "vocab_size": vocab_size,
        "raw_logit_width": 32,
        "lsh_bits": 8,
        "output_bits": 5,
        "seed": 7,
        "output_vertex_scheme":
            "balanced_recursive_embedding_projections",
        "packed_sha256": hashlib.sha256(packed.tobytes()).hexdigest(),
        "output_vertices_sha256":
            hashlib.sha256(vertices.tobytes()).hexdigest(),
    }
    validate_balanced_token_artifact(
        packed, vertices, metadata, model_revision="pinned",
        vocab_size=vocab_size, raw_logit_width=32,
        input_bits=8, output_bits=5, seed=7,
    )
    broken = dict(metadata)
    broken["output_vertices_sha256"] = "0" * 64
    with pytest.raises(ValueError, match="hash mismatch"):
        validate_balanced_token_artifact(
            packed, vertices, broken, model_revision="pinned",
            vocab_size=vocab_size, raw_logit_width=32,
            input_bits=8, output_bits=5, seed=7,
        )


def test_joint_support_bank_is_unique_output_conditioned_and_degree_bounded():
    rows, input_degree, output_frequency = deterministic_input_output_supports(
        96, 2048, output_bits=8, max_total_degree=7,
        unigram_terms=128, seed=3, support_layout="causal_token_structured",
    )
    output_degree = np.asarray([int(x).bit_count() for x in output_frequency])
    assert np.all(output_frequency != 0)
    assert np.all(input_degree[:128] == 0)
    assert np.all(input_degree[128:] > 0)
    assert np.all(input_degree + output_degree <= 7)
    keys = [(int(frequency), tuple(row[:int(degree)]))
            for row, degree, frequency in zip(
                rows, input_degree, output_frequency, strict=True
            )]
    assert len(set(keys)) == len(keys)


def test_structured_joint_bank_tiles_low_degree_cartesian_basis_first():
    n_input_bits, output_bits = 32, 5
    first_stage = n_input_bits * output_bits
    rows, input_degree, output_frequency = deterministic_input_output_supports(
        n_input_bits, first_stage + 37, output_bits=output_bits,
        max_total_degree=5, seed=9,
        support_layout="joint_cartesian",
    )
    assert np.all(input_degree[:first_stage] == 1)
    pairs = {
        (int(rows[index, 0]), int(output_frequency[index]))
        for index in range(first_stage)
    }
    expected = {
        (input_bit, 1 << output_bit)
        for input_bit in range(n_input_bits)
        for output_bit in range(output_bits)
    }
    assert pairs == expected
    assert np.all(input_degree[first_stage:] == 1)
    assert np.all([
        int(value).bit_count() == 2
        for value in output_frequency[first_stage:]
    ])


def test_hashed_support_bank_is_deterministic_and_degree_bounded():
    first = deterministic_input_output_supports(
        512, 16_384, output_bits=18, max_total_degree=8,
        seed=7, support_layout="hashed",
    )
    second = deterministic_input_output_supports(
        512, 16_384, output_bits=18, max_total_degree=8,
        seed=7, support_layout="hashed",
    )
    assert all(np.array_equal(left, right) for left, right in zip(first, second))
    rows, input_degree, output_frequency = first
    output_degree = np.asarray([int(value).bit_count() for value in output_frequency])
    assert np.all(input_degree > 0)
    assert np.all(output_frequency != 0)
    assert np.all(input_degree + output_degree <= 8)
    for row, degree in zip(rows, input_degree, strict=True):
        degree = int(degree)
        assert len(np.unique(row[:degree])) == degree
        assert np.all(row[:degree] < 512)
        assert np.all(row[degree:] == 512)


def test_exact_walsh_forward_and_product_vertex_gradient():
    bits = torch.tensor(
        [[0, 1, 1, 0], [1, 0, 1, 1], [1, 1, 0, 0]],
        dtype=torch.float32,
    )
    theta = torch.tensor(
        [[0.1, 0.0, 0.2, 0.0], [0.0, 0.2, 0.0, 0.0],
         [0.0, 0.0, 0.0, 0.0]], requires_grad=True,
    )
    degree = torch.tensor([2, 1, 0])
    upstream = torch.tensor(
        [[0.2, -0.4, 0.7], [0.7, 0.3, -0.2], [-0.1, 0.5, 0.4]]
    )
    character = exact_walsh_ste(bits, theta, degree, 2)
    mask = hard_topk_mask(theta.detach(), degree, 2)
    expected_character = 1.0 - 2.0 * torch.remainder(
        bits @ mask.float().t(), 2.0
    )
    torch.testing.assert_close(character, expected_character, atol=0, rtol=0)
    (character * upstream).sum().backward()
    expected_gradient = -2.0 * ((upstream * character.detach()).t() @ bits)
    expected_gradient *= 1.0 - 2.0 * mask.float()
    expected_gradient -= expected_gradient.mean(1, keepdim=True)
    expected_gradient[degree == 0] = 0
    torch.testing.assert_close(theta.grad, expected_gradient)


def test_support_margin_and_per_step_flip_metrics_are_exact():
    theta = torch.tensor([
        [1.0, 0.7, 0.2, -0.1],
        [0.9, 0.8, 0.6, 0.1],
    ])
    degree = torch.tensor([1, 2])
    torch.testing.assert_close(
        hard_support_margin(theta, degree, max_degree=2),
        torch.tensor([0.3, 0.2]),
    )
    before = torch.tensor([
        [True, False, False, False],
        [True, True, False, False],
        [False, False, True, False],
    ])
    after = torch.tensor([
        [False, True, False, False],
        [True, True, False, False],
        [False, False, False, True],
    ])
    two_steps_ago = torch.tensor([
        [False, True, False, False],
        [True, False, True, False],
        [False, True, False, False],
    ])
    metrics = support_flip_metrics(before, after, two_steps_ago)
    assert metrics["rows_flipped"] == 2
    assert metrics["row_flip_fraction"] == pytest.approx(2 / 3)
    assert metrics["bits_flipped"] == 4
    assert metrics["bits_per_flipped_row"] == 2
    assert metrics["rows_flipped_back"] == 1
    assert metrics["flip_back_fraction"] == pytest.approx(1 / 3)
    assert metrics["flip_back_fraction_of_flips"] == pytest.approx(0.5)


def test_walsh_synthesis_matches_explicit_characters():
    torch.manual_seed(4)
    output_bits = 5
    spectrum = torch.randn(3, 1 << output_bits)
    actual = walsh_synthesis(spectrum)
    frequency = torch.arange(1 << output_bits)
    vertex = torch.arange(1 << output_bits)
    bit = torch.arange(output_bits)
    frequency_bits = ((frequency[:, None] >> bit) & 1).float()
    vertex_bits = ((vertex[:, None] >> bit) & 1).float()
    character = 1.0 - 2.0 * torch.remainder(
        vertex_bits @ frequency_bits.t(), 2.0
    )
    expected = spectrum @ character.t()
    torch.testing.assert_close(actual, expected, atol=1e-5, rtol=1e-5)


def test_output_frequency_ste_matches_product_vertex_derivative():
    torch.manual_seed(12)
    batch, terms, output_bits = 3, 5, 4
    weighted = torch.randn(batch, terms, requires_grad=True)
    theta = torch.randn(terms, output_bits, requires_grad=True)
    degree = torch.tensor([1, 2, 3, 1, 2])
    upstream = torch.randn(batch, 1 << output_bits)
    spectrum = hard_frequency_scatter_ste(
        weighted, theta, degree, output_bits, 3
    )
    scores = walsh_synthesis(spectrum)
    (scores * upstream).sum().backward()

    mask = hard_output_mask(theta.detach(), degree, 3)
    vertices = torch.arange(1 << output_bits)
    bit = torch.arange(output_bits)
    vertex_bits = ((vertices[:, None] >> bit) & 1).float()
    character = 1.0 - 2.0 * torch.remainder(
        vertex_bits @ mask.float().t(), 2.0
    )
    expected_weighted = upstream @ character
    expected_theta = torch.empty_like(theta)
    for d in range(output_bits):
        expected_theta[:, d] = -2.0 * (
            upstream[:, :, None]
            * weighted.detach()[:, None, :]
            * character[None]
            * vertex_bits[None, :, d, None]
        ).sum((0, 1)) * (1.0 - 2.0 * mask[:, d].float())
    expected_theta -= expected_theta.mean(1, keepdim=True)
    torch.testing.assert_close(weighted.grad, expected_weighted, atol=1e-5, rtol=1e-5)
    torch.testing.assert_close(theta.grad, expected_theta, atol=1e-5, rtol=1e-5)


def test_model_scores_match_sparse_joint_artifact():
    rng = np.random.default_rng(9)
    model = InputOutputWalshStudent(
        32, _vertices(), 96, output_bits=5, unigram_terms=16,
        max_total_degree=5, char_chunk=24, seed=2,
        checkpoint_chunks=False,
    )
    with torch.no_grad():
        model.coefficient.normal_(std=0.1)
    model.eval()
    bits = rng.integers(0, 2, (7, 32), dtype=np.uint8)
    live = model(torch.from_numpy(bits).float()).detach().numpy()
    restored = sparse_scores(bits, model.sparse_state(), chunk=24)
    np.testing.assert_allclose(restored, live, atol=2e-5, rtol=2e-5)
    np.testing.assert_allclose(live.mean(1), 0.0, atol=2e-6)
    torch.testing.assert_close(
        model.score(torch.from_numpy(bits).float(), 3),
        torch.from_numpy(live[:, 3]), atol=2e-5, rtol=2e-5,
    )


def test_full_kl_and_metrics_use_the_entire_vocabulary():
    student = torch.tensor([[0.2, -0.4, 1.1], [-0.2, 0.5, 0.1]],
                           requires_grad=True)
    teacher = torch.tensor([[0.7, -0.1, 0.3], [0.2, -0.5, 1.4]])
    actual = full_teacher_student_kl(student, teacher)
    actual_rows = full_teacher_student_kl_rows(student, teacher)
    teacher_logp = torch.log_softmax(teacher, -1)
    expected = (teacher_logp.exp() * (
        teacher_logp - torch.log_softmax(student, -1)
    )).sum(-1).mean()
    torch.testing.assert_close(actual, expected)
    torch.testing.assert_close(actual_rows.mean(), expected)
    assert actual_rows.shape == (2,)
    actual.backward()
    assert torch.isfinite(student.grad).all()
    metrics = distribution_metrics(student.detach(), teacher)
    assert metrics["kl"] == pytest.approx(float(expected.detach()), abs=1e-6)
    assert 0 <= metrics["top1_agreement"] <= 1
    # Changing a non-argmax class still changes full-distribution KL.
    changed = student.detach().clone()
    changed[:, 1] += 2.0
    assert full_teacher_student_kl(changed, teacher) != actual.detach()


def test_equal_microbatch_kl_gradient_average_matches_physical_batch():
    physical = InputOutputWalshStudent(
        16, _vertices(vocab=13, output_bits=4), 48,
        output_bits=4, max_total_degree=4, char_chunk=16,
        support_layout="joint_cartesian", seed=31,
        checkpoint_chunks=False,
    )
    accumulated = InputOutputWalshStudent(
        16, _vertices(vocab=13, output_bits=4), 48,
        output_bits=4, max_total_degree=4, char_chunk=16,
        support_layout="joint_cartesian", seed=31,
        checkpoint_chunks=False,
    )
    accumulated.load_state_dict(physical.state_dict())
    bits = torch.randint(0, 2, (8, 16), dtype=torch.float32)
    teacher = torch.randn(8, 13)
    full_loss = full_teacher_student_kl(physical(bits), teacher)
    full_loss.backward()
    micro_loss = 0.0
    for lo in (0, 4):
        value = full_teacher_student_kl(
            accumulated(bits[lo:lo + 4]), teacher[lo:lo + 4]
        )
        (value / 2).backward()
        micro_loss += float(value.detach()) / 2
    assert micro_loss == pytest.approx(float(full_loss.detach()), abs=1e-7)
    for full_parameter, micro_parameter in zip(
        physical.parameters(), accumulated.parameters(), strict=True
    ):
        torch.testing.assert_close(
            micro_parameter.grad, full_parameter.grad,
            atol=2e-6, rtol=2e-5,
        )


def test_scaled_exact_kl_accumulation_matches_physical_batch():
    physical = InputOutputWalshStudent(
        16, _vertices(vocab=13, output_bits=4), 48,
        output_bits=4, max_total_degree=4, char_chunk=16,
        support_layout="joint_cartesian", seed=32,
        checkpoint_chunks=False,
    )
    accumulated = InputOutputWalshStudent(
        16, _vertices(vocab=13, output_bits=4), 48,
        output_bits=4, max_total_degree=4, char_chunk=16,
        support_layout="joint_cartesian", seed=32,
        checkpoint_chunks=False,
    )
    accumulated.load_state_dict(physical.state_dict())
    bits = torch.randint(0, 2, (8, 16), dtype=torch.float32)
    teacher = torch.randn(8, 13)
    physical_kl = full_teacher_student_kl(physical(bits), teacher)
    exact_kl_backward_loss(physical_kl, physical.terms).backward()
    for lo in (0, 4):
        micro_kl = full_teacher_student_kl(
            accumulated(bits[lo:lo + 4]), teacher[lo:lo + 4]
        )
        exact_kl_backward_loss(
            micro_kl, accumulated.terms, accumulation_steps=2
        ).backward()
    for full_parameter, micro_parameter in zip(
        physical.parameters(), accumulated.parameters(), strict=True
    ):
        torch.testing.assert_close(
            micro_parameter.grad, full_parameter.grad,
            atol=3e-5, rtol=3e-5,
        )


def test_scaled_loss_adamw_matches_reduced_epsilon():
    terms = 3_950_000
    scale = terms ** 0.5
    scaled = torch.tensor([0.4, -0.7, 1.2], dtype=torch.float64,
                          requires_grad=True)
    natural = scaled.detach().clone().requires_grad_()
    common = {
        "lr": 0.03, "betas": (0.9, 0.999), "weight_decay": 0.01,
    }
    scaled_optimizer = torch.optim.AdamW(
        [scaled], eps=1e-8, **common
    )
    natural_optimizer = torch.optim.AdamW(
        [natural], eps=1e-8 / scale, **common
    )
    target = torch.tensor([-0.2, 0.6, -1.4], dtype=torch.float64)
    for _ in range(5):
        scaled_optimizer.zero_grad(set_to_none=True)
        natural_optimizer.zero_grad(set_to_none=True)
        scaled_loss = (scaled - target).square().mean()
        natural_loss = (natural - target).square().mean()
        exact_kl_backward_loss(scaled_loss, terms).backward()
        natural_loss.backward()
        scaled_optimizer.step()
        natural_optimizer.step()
        torch.testing.assert_close(scaled, natural, atol=1e-12, rtol=1e-12)


def test_live_duplicate_ste_rows_can_split_on_the_next_adamw_step():
    theta = torch.tensor(
        [[0.05, 0.0], [0.05, 0.0]], requires_grad=True
    )
    degree = torch.ones(2, dtype=torch.long)
    coefficient = torch.tensor([1.0, -1.0])
    optimizer = torch.optim.AdamW(
        [theta], lr=0.1, betas=(0.9, 0.999),
        eps=1e-8, weight_decay=0.01,
    )
    before = hard_topk_mask(theta.detach(), degree, 1)
    assert torch.equal(before[0], before[1])
    character = exact_walsh_ste(
        torch.tensor([[1.0, 0.0]]), theta, degree, 1
    )
    (character * coefficient[None]).sum().backward()
    optimizer.step()
    after = hard_topk_mask(theta.detach(), degree, 1)
    assert not torch.equal(after[0], after[1])


def test_compact_round_trip_and_prefix_growth():
    rng = np.random.default_rng(11)
    small = InputOutputWalshStudent(
        32, _vertices(), 96, output_bits=5, unigram_terms=16,
        max_total_degree=5, char_chunk=24, seed=2,
        checkpoint_chunks=False,
    )
    with torch.no_grad():
        small.coefficient.normal_(std=0.1)
    bits = rng.integers(0, 2, (9, 32), dtype=np.uint8)
    small.eval()
    expected = small(torch.from_numpy(bits).float()).detach()
    compact = encode_compact_student(small.sparse_state(), block_size=16)
    decoded = decode_compact_student(compact)
    assert decoded["schema"] == ARTIFACT_SCHEMA
    reloaded = InputOutputWalshStudent(
        32, _vertices(), 96, output_bits=5, unigram_terms=16,
        max_total_degree=5, char_chunk=24, seed=2,
        checkpoint_chunks=False,
    )
    load_compact_student(reloaded, compact, score_gap=1.5)
    reloaded.eval()
    torch.testing.assert_close(
        reloaded(torch.from_numpy(bits).float()), expected,
        atol=4e-4, rtol=4e-4,
    )
    large = InputOutputWalshStudent(
        32, _vertices(), 144, output_bits=5, unigram_terms=16,
        max_total_degree=5, char_chunk=24, seed=2,
        checkpoint_chunks=False,
    )
    assert load_compact_prefix(large, compact, score_gap=1.25) == 96
    assert torch.count_nonzero(large.coefficient[96:]) == 0
    large.eval()
    torch.testing.assert_close(
        large(torch.from_numpy(bits).float()), expected,
        atol=4e-4, rtol=4e-4,
    )


def test_duplicate_repair_uses_joint_input_output_support():
    model = InputOutputWalshStudent(
        24, _vertices(), 64, output_bits=5, unigram_terms=8,
        max_total_degree=5, char_chunk=16, seed=4,
        checkpoint_chunks=False,
    )
    optimizer = torch.optim.AdamW(model.parameters(), lr=0.01)
    first, second = 12, 13
    with torch.no_grad():
        model.theta[second].copy_(model.theta[first])
        model.input_degree[second] = model.input_degree[first]
        model.output_theta[second].copy_(model.output_theta[first])
        model.output_degree[second] = model.output_degree[first]
    bits = torch.randint(0, 2, (5, 24), dtype=torch.float32)
    model.eval()
    before = model(bits).detach()
    result = repair_duplicate_supports(model, optimizer, seed=19)
    after = model(bits).detach()
    torch.testing.assert_close(after, before, atol=2e-6, rtol=2e-6)
    assert result["duplicates_repaired"] >= 1


def test_full_kl_keeps_both_ste_masks_and_coefficients_live():
    model = InputOutputWalshStudent(
        16, _vertices(vocab=13, output_bits=4), 48,
        output_bits=4, unigram_terms=8, max_total_degree=4,
        char_chunk=16, seed=5, checkpoint_chunks=False,
    )
    bits = torch.randint(0, 2, (7, 16), dtype=torch.float32)
    target = torch.randn(7, 13)
    loss = full_teacher_student_kl(model(bits), target)
    loss.backward()
    contextual = model.input_degree > 0
    assert torch.isfinite(model.theta.grad).all()
    assert float(model.theta.grad[contextual].abs().sum()) > 0
    assert torch.isfinite(model.output_theta.grad).all()
    assert float(model.output_theta.grad.abs().sum()) > 0
    assert torch.isfinite(model.coefficient.grad).all()
    assert float(model.coefficient.grad.abs().sum()) > 0
    assert torch.isfinite(model.token_bias.grad).all()
    assert float(model.token_bias.grad.abs().sum()) > 0


def test_kiss16_stream_is_forward_only_and_has_512_input_bits():
    class Tokenizer:
        def __call__(self, texts, add_special_tokens=False):
            assert add_special_tokens is False
            return {"input_ids": [
                [int(text.split(":", 1)[0])] * (CONTEXT_TOKENS + 4)
                for text in texts
            ]}

    records = iter({"text": f"{index}:document"} for index in range(1024))
    pending = deque()
    first, first_consumed = fresh_context_batch(
        records, Tokenizer(), pending, 32
    )
    second, second_consumed = fresh_context_batch(
        records, Tokenizer(), pending, 32
    )
    assert first.shape == second.shape == (32, CONTEXT_TOKENS)
    assert CONTEXT_TOKENS * TOKEN_BITS == 512
    assert first_consumed == 256 and second_consumed == 0
    assert set(first[:, 0]).isdisjoint(set(second[:, 0]))
    model = InputOutputWalshStudent(
        CONTEXT_TOKENS * TOKEN_BITS, _vertices(vocab=13, output_bits=4), 64,
        output_bits=4, max_total_degree=4, char_chunk=16,
        seed=6, checkpoint_chunks=False,
    )
    assert model(torch.zeros(2, 512)).shape == (2, 13)


def test_kiss16_uses_two_disjoint_standard_adamw_optimizers():
    model = InputOutputWalshStudent(
        16, _vertices(vocab=13, output_bits=4), 48,
        output_bits=4, max_total_degree=4, char_chunk=16,
        seed=5, checkpoint_chunks=False,
    )
    common = {
        "betas": (0.9, 0.999), "eps": 1e-8, "weight_decay": 0.01,
    }
    ste = torch.optim.AdamW(
        [model.theta, model.output_theta], lr=0.03, **common
    )
    coefficient = torch.optim.AdamW(
        [model.coefficient, model.token_bias], lr=0.01, **common
    )
    ste_parameters = {id(value) for group in ste.param_groups
                      for value in group["params"]}
    coefficient_parameters = {
        id(value) for group in coefficient.param_groups
        for value in group["params"]
    }
    assert ste_parameters.isdisjoint(coefficient_parameters)
    assert ste.param_groups[0]["lr"] == 0.03
    assert coefficient.param_groups[0]["lr"] == 0.01
    for optimizer in (ste, coefficient):
        group = optimizer.param_groups[0]
        assert group["betas"] == (0.9, 0.999)
        assert group["eps"] == 1e-8
        assert group["weight_decay"] == 0.01


@pytest.mark.skipif(not torch.cuda.is_available(), reason="CUDA compile test")
def test_compiled_bf16_parity_scores_and_gradients_match_eager_fp32():
    vertices = _vertices(vocab=13, output_bits=4)
    eager = InputOutputWalshStudent(
        16, vertices, 48, output_bits=4, unigram_terms=8,
        max_total_degree=4, char_chunk=16, seed=15,
        checkpoint_chunks=False,
    ).cuda()
    compiled = InputOutputWalshStudent(
        16, vertices, 48, output_bits=4, unigram_terms=8,
        max_total_degree=4, char_chunk=16, seed=15,
        checkpoint_chunks=False, compile_chunks=True,
        compile_transforms=True, parity_dtype=torch.bfloat16,
    ).cuda()
    compiled.load_state_dict(eager.state_dict())
    bits = torch.randint(0, 2, (7, 16), device="cuda").float()
    teacher = torch.randn(7, 13, device="cuda")
    eager_scores = eager(bits)
    eager_loss = full_teacher_student_kl(eager_scores, teacher)
    eager_loss.backward()
    compiled_scores = compiled(bits)
    compiled_loss = full_teacher_student_kl(compiled_scores, teacher)
    compiled_loss.backward()
    torch.testing.assert_close(compiled_scores, eager_scores, atol=2e-5, rtol=2e-5)
    torch.testing.assert_close(compiled_loss, eager_loss, atol=2e-6, rtol=2e-6)
    for compiled_parameter, eager_parameter in zip(
        compiled.parameters(), eager.parameters(), strict=True
    ):
        torch.testing.assert_close(
            compiled_parameter.grad, eager_parameter.grad,
            atol=3e-5, rtol=3e-5,
        )


def test_output_only_duplicates_merge_cancel_recycle_and_reset_optimizer():
    model = InputOutputWalshStudent(
        16, _vertices(vocab=13, output_bits=4), 32,
        output_bits=4, unigram_terms=8, max_total_degree=4,
        char_chunk=16, seed=8, checkpoint_chunks=False,
    )
    ste_optimizer = torch.optim.AdamW(
        [model.theta, model.output_theta], lr=0.01
    )
    coefficient_optimizer = torch.optim.AdamW(
        [model.coefficient, model.token_bias], lr=0.003
    )
    optimizers = (ste_optimizer, coefficient_optimizer)
    for optimizer in optimizers:
        optimizer.zero_grad(set_to_none=True)
    sum(parameter.square().sum() for parameter in model.parameters()).backward()
    for optimizer in optimizers:
        optimizer.step()
    first, second = 0, 1
    with torch.no_grad():
        model.output_theta[second].copy_(model.output_theta[first])
        model.output_degree[second] = model.output_degree[first]
        model.coefficient[first] = 0.75
        model.coefficient[second] = -0.75
    bits = torch.randint(0, 2, (5, 16), dtype=torch.float32)
    model.eval()
    before = model(bits).detach()
    result = repair_duplicate_supports(
        model, optimizers, seed=23, birth_score_gap=0.1
    )
    after = model(bits).detach()
    torch.testing.assert_close(after, before, atol=2e-6, rtol=2e-6)
    assert result["duplicates_repaired"] == 1
    assert result["recycled"] == 1
    assert result["unique_after"] == model.terms
    assert int(model.hard_output_frequency()[first]) != int(
        model.hard_output_frequency()[second]
    )
    for parameter, optimizer in (
        (model.theta, ste_optimizer),
        (model.output_theta, ste_optimizer),
        (model.coefficient, coefficient_optimizer),
    ):
        state = optimizer.state[parameter]
        for value in state.values():
            if torch.is_tensor(value) and value.shape == parameter.shape:
                assert float(value[second].abs().sum()) == 0.0
    coefficient_state = coefficient_optimizer.state[model.coefficient]
    for value in coefficient_state.values():
        if torch.is_tensor(value) and value.shape == model.coefficient.shape:
            assert float(value[first].abs().sum()) == 0.0


def test_output_prior_initialization_is_finite_and_nonuniform():
    model = InputOutputWalshStudent(
        16, _vertices(vocab=13, output_bits=4), 32,
        output_bits=4, unigram_terms=12, max_total_degree=4,
        char_chunk=16, seed=8, coefficient_std=0.0,
        checkpoint_chunks=False,
    )
    mean_logits = torch.linspace(-2, 2, 13)
    model.initialize_output_prior(mean_logits)
    model.eval()
    scores = model(torch.zeros(2, 16))
    assert torch.isfinite(scores).all()
    assert float(scores.detach().std()) > 0
    torch.testing.assert_close(scores[0], scores[1])


def test_hashed_low_degree_mix_is_deterministic_and_valid():
    """hashed_low_degree uses fixed 50/35/15 degree mix and stays <= max_total_degree."""
    rows, degree, freq = deterministic_input_output_supports(
        64, 1000, output_bits=8, max_total_degree=8,
        support_layout="hashed_low_degree", seed=7,
    )
    rows2, degree2, freq2 = deterministic_input_output_supports(
        64, 1000, output_bits=8, max_total_degree=8,
        support_layout="hashed_low_degree", seed=7,
    )
    np.testing.assert_array_equal(rows, rows2)
    np.testing.assert_array_equal(degree, degree2)
    np.testing.assert_array_equal(freq, freq2)
    counts = Counter(degree.tolist())
    total = sum(counts.values())
    # Fixed mix 50% deg1, 35% deg2, 15% deg3 (within rounding).
    assert abs(counts[1] / total - 0.50) < 0.02
    assert abs(counts[2] / total - 0.35) < 0.02
    assert abs(counts[3] / total - 0.15) < 0.02
    total_degree = degree + np.array(
        [int(x).bit_count() for x in freq], dtype=np.uint8
    )
    assert total_degree.max() <= 8
    assert np.all(freq > 0)
    keys = [
        (int(frequency), tuple(row[:int(input_degree)]))
        for row, input_degree, frequency in zip(
            rows, degree, freq, strict=True
        )
    ]
    assert len(set(keys)) == len(keys)


def test_hashed_low_degree_rejects_small_max_degree():
    with pytest.raises(ValueError):
        deterministic_input_output_supports(
            32, 10, output_bits=4, max_total_degree=2,
            support_layout="hashed_low_degree",
        )


def test_estimate_compact_artifact_bytes_within_budget():
    """The corrected 3.95M-term degree-exact config stays above 50x."""
    counts = joint_cartesian_degree_counts(
        3_950_000, n_input_bits=512, output_bits=18,
        max_total_degree=4,
    )
    assert counts == {"degree1": 87_552, "degree2": 3_862_448}
    average = joint_cartesian_avg_input_degree(
        3_950_000, n_input_bits=512, output_bits=18,
        max_total_degree=4,
    )
    assert average == pytest.approx(
        (87_552 + 2 * 3_862_448) / 3_950_000
    )
    projected = estimate_compact_artifact_bytes(
        3_950_000, vocab_size=248_077, max_total_degree=4,
        output_bits=18, n_input_bits=512, input_code_bits=32,
        avg_input_degree=average,
    )
    assert projected <= 32_000_000
    assert 1_600_000_000 / projected >= 50.0


def test_collision_u_statistic_counts_ordered_distinct_pairs():
    keys = torch.tensor([
        [1, 2], [1, 2], [1, 2],
        [3, 4], [3, 4], [5, 6],
    ])
    assert float(sample_collision_u_statistic(keys)) == pytest.approx(8 / 30)


def test_collision_aux_has_exact_forward_duplicate_only_ste_and_no_coeff_grad():
    model = InputOutputWalshStudent(
        16, _vertices(vocab=13, output_bits=4), 32,
        output_bits=4, max_total_degree=4, char_chunk=16,
        support_layout="joint_cartesian", seed=11,
        checkpoint_chunks=False,
    )
    with torch.no_grad():
        model.theta[1].copy_(model.theta[0])
        model.input_degree[1] = model.input_degree[0]
        model.output_theta[1].copy_(model.output_theta[0])
        model.output_degree[1] = model.output_degree[0]
    before = {
        key: value.detach().clone()
        for key, value in model.state_dict().items()
    }
    audit = exact_joint_collision_audit(model)
    assert audit["duplicate_rows"] == 1
    assert audit["duplicate_fraction"] == pytest.approx(1 / model.terms)
    for key, value in model.state_dict().items():
        torch.testing.assert_close(value, before[key], atol=0, rtol=0)

    loss, metrics = sampled_joint_collision_loss(
        model, sample_size=model.terms, temperature=0.25,
        generator=torch.Generator().manual_seed(99),
    )
    expected = 2 / (model.terms * (model.terms - 1))
    assert float(loss.detach()) == pytest.approx(expected)
    assert metrics["expectation"] == pytest.approx(expected)
    assert metrics["pair_count"] == 1
    loss.backward()
    assert float(model.theta.grad[0].abs().sum()) == 0.0
    assert float(model.output_theta.grad[0].abs().sum()) == 0.0
    assert float(model.theta.grad[1].abs().sum()) > 0.0
    assert float(model.output_theta.grad[1].abs().sum()) > 0.0
    assert model.coefficient.grad is None
    assert model.token_bias.grad is None


def test_audited_duplicate_pairs_drive_direct_zero_forward_aux():
    model = InputOutputWalshStudent(
        16, _vertices(vocab=13, output_bits=4), 32,
        output_bits=4, max_total_degree=4, char_chunk=16,
        support_layout="joint_cartesian", seed=17,
        checkpoint_chunks=False,
    )
    with torch.no_grad():
        model.theta[7].copy_(model.theta[3])
        model.input_degree[7] = model.input_degree[3]
        model.output_theta[7].copy_(model.output_theta[3])
        model.output_degree[7] = model.output_degree[3]
    metrics, keep, lose = exact_joint_collision_groups(model)
    assert metrics["duplicate_rows"] == 1
    assert keep.tolist() == [3]
    assert lose.tolist() == [7]
    loss, direct = cached_duplicate_collision_loss(
        model, keep, lose, sample_size=32, margin=0.1,
        generator=torch.Generator().manual_seed(101),
    )
    assert float(loss.detach()) == 0.0
    assert direct == {"candidate_pairs": 1.0, "active_pairs": 1.0}
    loss.backward()
    assert float(model.theta.grad[3].abs().sum()) == 0.0
    assert float(model.output_theta.grad[3].abs().sum()) == 0.0
    assert float(model.theta.grad[7].abs().sum()) > 0.0
    assert float(model.output_theta.grad[7].abs().sum()) > 0.0
    assert float(model.theta.grad[7].min()) < 0.0
    assert float(model.theta.grad[7].max()) > 0.0
    assert float(model.output_theta.grad[7].min()) < 0.0
    assert float(model.output_theta.grad[7].max()) > 0.0
    assert model.coefficient.grad is None
    assert model.token_bias.grad is None


def test_exact_merge_sums_coefficients_deactivates_and_preserves_logits():
    model = InputOutputWalshStudent(
        16, _vertices(vocab=13, output_bits=4), 32,
        output_bits=4, max_total_degree=4, char_chunk=16,
        support_layout="joint_cartesian", seed=23,
        checkpoint_chunks=False,
    )
    keep, lose = 3, 7
    with torch.no_grad():
        model.theta[lose].copy_(model.theta[keep])
        model.input_degree[lose] = model.input_degree[keep]
        model.output_theta[lose].copy_(model.output_theta[keep])
        model.output_degree[lose] = model.output_degree[keep]
        model.coefficient[keep] = 0.75
        model.coefficient[lose] = -0.2
    bits = torch.randint(0, 2, (11, 16), dtype=torch.float32)
    model.eval()
    before = model(bits).detach()
    result = merge_duplicate_coefficients(model)
    after = model(bits).detach()
    torch.testing.assert_close(after, before, atol=2e-6, rtol=2e-6)
    assert result["merged_rows"] == 1
    assert result["active_after"] == model.terms - 1
    assert bool(model.active_term[keep])
    assert not bool(model.active_term[lose])
    assert float(model.coefficient[keep].detach()) == pytest.approx(0.55)
    assert float(model.coefficient[lose].detach()) == 0.0
    audit = exact_joint_collision_audit(model)
    assert audit["duplicate_rows"] == 0
    assert audit["active_rows"] == model.terms - 1

    with torch.no_grad():
        model.coefficient[lose] = 1000.0
    torch.testing.assert_close(model(bits), after, atol=2e-6, rtol=2e-6)
    state = model.sparse_state()
    assert len(state["input_degrees"]) == model.terms - 1
    np.testing.assert_allclose(
        sparse_scores(bits.numpy().astype(np.uint8), state),
        after.numpy(), atol=2e-5, rtol=2e-5,
    )
