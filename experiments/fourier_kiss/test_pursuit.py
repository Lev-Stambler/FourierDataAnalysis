import numpy as np
import pytest
import torch

from fourier_kiss.pursuit import (
    StaircaseWalshStudent,
    canonical_signature_digest,
    constrained_threshold,
    decode_staircase_student,
    encode_staircase_student,
    evaluate_supports,
    filter_extension_candidates,
    filter_seed_candidates,
    initial_low_degree_bank,
    initialize_uniqueness_sets,
    score_staircase_extensions,
    staircase_metadata,
    staircase_predict,
    support_key,
)


pytestmark = pytest.mark.skipif(
    not torch.cuda.is_available(), reason="pursuit tests are Modal/CUDA-only"
)


def test_exact_walsh_evaluation():
    bits = torch.tensor([
        [0, 0, 0], [1, 0, 1], [1, 1, 0], [0, 1, 1],
    ], dtype=torch.uint8, device="cuda")
    indices = torch.tensor([[0, 3], [0, 2]], dtype=torch.int32, device="cuda")
    degree = torch.tensor([1, 2], dtype=torch.uint8, device="cuda")
    value = evaluate_supports(bits, indices, degree).cpu().numpy()
    host = bits.cpu().numpy().astype(np.int32)
    expected = np.stack((
        1 - 2 * host[:, 0],
        1 - 2 * np.bitwise_xor(host[:, 0], host[:, 2]),
    ), axis=1).astype(np.float32)
    np.testing.assert_array_equal(value, expected)


def test_signature_is_canonical_up_to_sign():
    value = np.asarray([0, 1, 1, 0, 1, 0, 0, 1], dtype=np.uint8)
    assert canonical_signature_digest(value) == canonical_signature_digest(1 - value)


def test_staircase_roundtrip_and_zero_append_preserve_logits():
    rng = np.random.default_rng(3)
    bits_np = rng.integers(0, 2, size=(32, 8), dtype=np.uint8)
    bits = torch.from_numpy(bits_np).cuda()
    supports = [(0,), (1,), (0, 2), (1, 3), (0, 2, 4)]
    parent = [-1, -1, 0, 1, 2]
    appended = [0, 1, 2, 3, 4]
    model = StaircaseWalshStudent(
        8, 8, 5, initial_probability=0.4, char_chunk=3
    ).cuda()
    model.append(supports[:4], parent[:4], appended[:4], [0.2, -0.1, 0.3, 0.05])
    before = model(bits).detach().cpu().numpy()
    model.append(supports[4:], parent[4:], appended[4:], [0.0])
    after = model(bits).detach().cpu().numpy()
    np.testing.assert_allclose(before, after, rtol=0, atol=0)
    compact = encode_staircase_student(model, block_size=2)
    decoded = decode_staircase_student(compact)
    assert decoded["supports"] == supports
    np.testing.assert_allclose(
        decoded["coefficient"],
        model.coefficient.detach().cpu().numpy()[:5] * model.output_scale,
        atol=2e-4, rtol=2e-4,
    )
    reference = staircase_predict(bits_np, compact, chunk=2)
    np.testing.assert_allclose(after, reference, atol=2e-4, rtol=2e-4)
    continued = StaircaseWalshStudent(
        8, 16, 5, initial_probability=0.4, char_chunk=3
    ).cuda()
    continued.append(
        decoded["supports"], parent, appended,
        decoded["coefficient"] / continued.output_scale,
    )
    with torch.no_grad():
        continued.bias.fill_(decoded["bias"])
    continued_logits = continued(bits).detach().cpu().numpy()
    np.testing.assert_allclose(
        continued_logits, reference, atol=2e-4, rtol=2e-4
    )


def test_initial_screen_contains_all_singletons_and_unique_pairs():
    generator = torch.Generator(device="cuda").manual_seed(4)
    bits = torch.randint(0, 2, (512, 8), generator=generator,
                         dtype=torch.uint8, device="cuda")
    target = torch.sigmoid(
        1.2 * (1 - 2 * bits[:, 0].float())
        - 0.8 * (1 - 2 * bits[:, 1].bitwise_xor(bits[:, 3]).float())
    )
    columns = torch.arange(8, device="cuda")
    supports, scores, _ = initial_low_degree_bank(
        bits, target, columns, keep=20, sample_chunk=128
    )
    assert set(supports[:8]) == {(index,) for index in range(8)}
    assert len(set(supports)) == 20
    assert len(scores) == 20
    parent, appended = staircase_metadata(supports)
    assert np.all(parent[:8] == -1)
    assert np.array_equal(appended[:8], np.arange(8))


def test_seed_filter_enforces_functional_uniqueness_and_staircase_parents():
    # x2 == x0 xor x1 makes support (0, 1) functionally equal to singleton 2.
    generator = torch.Generator(device="cuda").manual_seed(5)
    base = torch.randint(0, 2, (1024, 4), generator=generator,
                         dtype=torch.uint8, device="cuda")
    bits = torch.cat((
        base[:, :2], base[:, :1].bitwise_xor(base[:, 1:2]), base[:, 2:],
    ), dim=1)
    supports = [(index,) for index in range(bits.shape[1])]
    supports += [(0, 1), (0, 3), (1, 4), (3, 4)]
    scores = np.linspace(1.0, 0.1, len(supports), dtype=np.float32)
    kept, kept_scores, info = filter_seed_candidates(
        bits, supports, scores, keep=8, feature_chunk=4
    )
    assert len(kept) == len(kept_scores) == 8
    assert info["seed_duplicate_signatures_rejected"] >= 1
    assert (0, 1) not in kept
    staircase_metadata(kept)


def test_residual_extension_recovers_planted_child():
    generator = torch.Generator(device="cuda").manual_seed(7)
    bits = torch.randint(0, 2, (4096, 12), generator=generator,
                         dtype=torch.uint8, device="cuda")
    model = StaircaseWalshStudent(
        12, 16, 4, initial_probability=0.5, char_chunk=8
    ).cuda()
    supports = [(index,) for index in range(12)]
    parent = [-1] * 12
    model.append(supports, parent, list(range(12)), [0.0] * 12)
    planted = 1.5 * (
        1 - 2 * bits[:, 2].bitwise_xor(bits[:, 9]).float()
    )
    target = torch.sigmoid(planted)
    proposals = score_staircase_extensions(
        model, bits, target, torch.arange(12, device="cuda"),
        torch.tensor([2], device="cuda"), parents_per_chunk=1,
        top_per_parent=3,
    )
    assert any(parent_index == 2 and bit == 9
               for _score, _coefficient, parent_index, bit in proposals)


def test_functional_duplicate_filter_rejects_different_support():
    # x2 == x0 xor x1, hence chi_{0,1} == chi_2 on this data manifold.
    generator = torch.Generator(device="cuda").manual_seed(11)
    base = torch.randint(0, 2, (1024, 2), generator=generator,
                         dtype=torch.uint8, device="cuda")
    bits = torch.cat((base, base[:, :1].bitwise_xor(base[:, 1:2])), dim=1)
    model = StaircaseWalshStudent(
        3, 8, 3, initial_probability=0.5, char_chunk=4
    ).cuda()
    model.append([(0,), (1,), (2,)], [-1, -1, -1], [0, 1, 2], [0, 0, 0])
    support_keys, signature_keys, info = initialize_uniqueness_sets(model, bits)
    assert info["initial_functional_duplicates"] >= 0
    proposals = [(1.0, 0.1, 0, 1)]
    supports, *_rest, diagnostics = filter_extension_candidates(
        model, proposals, bits, support_keys, signature_keys, keep=1
    )
    assert supports == []
    assert diagnostics["candidate_duplicate_signatures"] == 1


def test_constrained_threshold_satisfies_both_recalls_when_feasible():
    target = torch.tensor([0.1] * 20 + [0.9] * 20, device="cuda")
    logits = torch.tensor(list(np.linspace(-3, -0.2, 20))
                          + list(np.linspace(0.2, 3, 20)), device="cuda")
    threshold, info = constrained_threshold(logits, target, 0.85, 0.95)
    assert info["threshold_feasible"] == 1
    assert info["calibrated_teacher_positive_recall"] >= 0.85
    assert info["calibrated_teacher_negative_recall"] >= 0.95
    assert -0.2 < threshold < 0.2


def test_support_key_is_degree_sensitive_and_canonical():
    assert support_key([1, 3]) == support_key((1, 3))
    assert support_key([1, 3]) != support_key([1, 3, 4])
