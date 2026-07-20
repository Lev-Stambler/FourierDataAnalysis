import math
import json

import numpy as np
import pytest
import torch

from fda_exp.fourier_noun import (
    HardWalshStudent,
    TrainConfig,
    build_lsh_codes_numpy,
    calibrate_agreement_threshold,
    export_sparse_student,
    format_noun_payload,
    format_student_fields,
    iter_text_examples,
    load_compact_student,
    logspace_ste_characters,
    pack_bits,
    predict_token_layout,
    product_ste_characters,
    restricted_binary_probability,
    sample_ud_examples,
    sparse_student_logits,
    tokens_to_lsh_bits,
    tokenize_student_fields,
    train_student,
    unpack_bits,
    warmup_cosine_factor,
    _repair_duplicate_topk_masks,
)


def test_payload_and_balanced_ud_sampling_are_target_aware():
    rows = [
        {
            "sent_id": "a",
            "tokens": ["The", "cat", "runs", "."],
            "upos": ["DET", "NOUN", "VERB", "PUNCT"],
        },
        {
            "sent_id": "b",
            "tokens": ["Lev", "writes", "quickly", "."],
            "upos": ["PROPN", "VERB", "ADV", "PUNCT"],
        },
    ]
    examples = sample_ud_examples(rows, None, count=4, seed=3)
    assert sum(x["gold"] for x in examples) == 2
    assert all("[TARGET]" in x["payload"] for x in examples)
    assert all(x["upos"] not in {"PUNCT", "SYM", "X"} for x in examples)
    assert format_noun_payload(["a", "word", "word"], 2).startswith("Target: word")
    fields = format_student_fields(["The", "cat", "runs", "fast"], 1)
    assert fields[:6] == [
        " cat", " cat", " prefix-cat", " suffix-cat", " The", " runs"
    ]
    assert len(fields) == 16


def test_fixed_student_fields_do_not_shift_across_long_words():
    class FakeTokenizer:
        padding_side = "left"
        truncation_side = "left"

        def __call__(self, texts, **_kwargs):
            ids, attention = [], []
            for text in texts:
                values = [ord(c) % 31 + 1 for c in text][:4]
                ids.append(values + [0] * (4 - len(values)))
                attention.append([1] * len(values) + [0] * (4 - len(values)))
            return {"input_ids": ids, "attention_mask": attention}

    tokenizer = FakeTokenizer()
    rows = [format_student_fields(["a", "extraordinarily", "runs"], 1)]
    ids, attention = tokenize_student_fields(tokenizer, rows, length=64, field_width=4)
    assert ids.shape == attention.shape == (1, 64)
    assert attention[0, :4].sum() == 4
    assert attention[0, 4:8].sum() == 4
    assert attention[0, 8:12].sum() == 4
    assert tokenizer.padding_side == tokenizer.truncation_side == "left"


def test_streamed_text_targets_have_unique_occurrence_ids():
    rows = [
        {"id": "doc-a", "text": "The same word appears twice: word."},
        {"id": "doc-b", "text": "Another short document has targets."},
    ]
    examples = list(iter_text_examples(rows, count=8, seed=3, max_targets_per_document=4))
    keys = {(item["sent_id"], item["token_index"]) for item in examples}
    assert len(keys) == len(examples) == 8
    assert all(not item["gold_known"] for item in examples)


def test_lsh_padding_and_pack_round_trip():
    embedding = np.arange(48, dtype=np.float32).reshape(12, 4)
    codes = build_lsh_codes_numpy(embedding, bits=9, seed=7)
    ids = np.array([[1, 2, 3], [4, 5, 6]], dtype=np.int32)
    attention = np.array([[1, 1, 0], [1, 0, 0]], dtype=np.uint8)
    bits = tokens_to_lsh_bits(ids, attention, codes)
    assert bits.shape == (2, 27)
    assert not bits[0, 18:].any() and not bits[1, 9:].any()
    np.testing.assert_array_equal(unpack_bits(pack_bits(bits), 27), bits)


def test_lsh_codes_are_unique_even_when_raw_signatures_collide():
    embedding = np.zeros((25, 4), dtype=np.float32)
    codes, repairs = build_lsh_codes_numpy(
        embedding, bits=8, seed=17, return_repairs=True
    )
    assert repairs == 24
    assert len(np.unique(pack_bits(codes), axis=0)) == len(embedding)


def test_restricted_teacher_probability_is_two_class_softmax():
    logits = torch.tensor([[0.0, 2.0, -1.0], [4.0, 1.0, 3.0]])
    actual = restricted_binary_probability(logits, negative_id=0, positive_id=2)
    expected = torch.softmax(logits[:, [0, 2]], dim=1)[:, 1]
    torch.testing.assert_close(actual, expected)


def test_agreement_threshold_maximizes_validation_accuracy():
    logits = np.array([-3.0, -2.0, 0.1, 0.2, 0.3])
    teacher = np.array([0.1, 0.9, 0.8, 0.2, 0.9])
    threshold, agreement = calibrate_agreement_threshold(logits, teacher)
    assert agreement == pytest.approx(0.8)
    assert ((logits >= threshold) == (teacher >= 0.5)).mean() == pytest.approx(0.8)
    constrained, _ = calibrate_agreement_threshold(
        logits, teacher, minimum_positive_recall=1.0
    )
    teacher_positive = teacher >= 0.5
    recall = ((logits >= constrained) & teacher_positive).sum() / teacher_positive.sum()
    assert recall == pytest.approx(1.0)


def test_hard_walsh_forward_is_exact_xor_and_gradients_are_alive():
    bits = torch.tensor(
        [[0, 0, 0, 0], [1, 0, 1, 0], [1, 1, 1, 1]], dtype=torch.uint8
    )
    theta = torch.tensor(
        [[8.0, -8.0, 8.0, -8.0], [-8.0, 8.0, -8.0, -8.0]],
        requires_grad=True,
    )
    phi = logspace_ste_characters(bits, theta)
    expected = torch.tensor([[1.0, 1.0], [1.0, 1.0], [1.0, -1.0]])
    torch.testing.assert_close(phi, expected, rtol=0, atol=0)
    (phi * torch.tensor([[0.3, -0.2]])).sum().backward()
    assert torch.isfinite(theta.grad).all()
    assert float(theta.grad.abs().sum()) > 0


def test_product_ste_is_exact_xor_with_nonsaturating_vertex_gradient():
    bits = torch.tensor([[1, 0, 1], [1, 1, 0]], dtype=torch.uint8)
    theta = torch.tensor([[20.0, -20.0, 20.0]], requires_grad=True)
    actual = product_ste_characters(bits, theta, scale=0.25)
    torch.testing.assert_close(actual, torch.tensor([[1.0], [-1.0]]))
    actual.sum().backward()
    assert torch.isfinite(theta.grad).all()
    assert float(theta.grad.abs().sum()) > 0.1


def test_topk_product_masks_change_membership_but_preserve_degree():
    model = HardWalshStudent(
        32, 64, seed=2, initialization="fixed_context",
        ste_variant="product", mask_parameterization="topk",
        off_init=-1.0, on_init=1.0,
    )
    before = model.hardened_masks()
    with torch.no_grad():
        model.theta[:, 20] += 4.0
    after = model.hardened_masks()
    torch.testing.assert_close(after.sum(1), before.sum(1))
    assert bool((after != before).any())


def test_topk_masks_select_each_rows_actual_highest_scores():
    model = HardWalshStudent(
        16, 2, seed=2, min_degree=1, max_degree=8,
        initialization="random", ste_variant="product",
        mask_parameterization="topk",
    )
    with torch.no_grad():
        model.mask_degree.copy_(torch.tensor([1, 3]))
        model.theta.copy_(torch.arange(32, dtype=torch.float32).reshape(2, 16))
    masks = model.hardened_masks()
    assert torch.equal(masks[0].nonzero().flatten(), torch.tensor([15]))
    assert torch.equal(masks[1].nonzero().flatten(), torch.tensor([13, 14, 15]))


def test_fixed_context_initialization_uses_explicit_token_width():
    model = HardWalshStudent(
        128, 256, seed=4, bits_per_token=4, initialization="fixed_context",
        ste_variant="product", mask_parameterization="topk",
    )
    masks = model.hardened_masks()
    covered = 128
    target_focused = round(0.35 * (256 - covered))
    context_focused = round(0.25 * (256 - covered))
    assert not bool(masks[covered:covered + target_focused, 64:].any())
    context = masks[
        covered + target_focused:
        covered + target_focused + context_focused
    ]
    assert not bool(context[:, 96:].any())


def test_duplicate_mask_repair_preserves_degree_and_adds_diversity():
    model = HardWalshStudent(
        16, 4, seed=2, min_degree=3, max_degree=3,
        initialization="random", ste_variant="product",
        mask_parameterization="topk",
    )
    with torch.no_grad():
        model.theta.copy_(torch.arange(16, dtype=torch.float32).repeat(4, 1))
    optimizer = torch.optim.AdamW(model.parameters(), lr=0.1)
    fraction, repaired = _repair_duplicate_topk_masks(model, optimizer)
    rows = model.hardened_index_rows().cpu().numpy()
    assert fraction == pytest.approx(0.25)
    assert repaired == 3
    assert len(np.unique(rows[:, :3], axis=0)) == 4
    torch.testing.assert_close(
        model.hardened_masks().sum(1), torch.full((4,), 3, dtype=torch.long)
    )


def test_singleton_cover_initialization_spans_every_input_bit():
    model = HardWalshStudent(17, 40, seed=5)
    masks = model.hardened_masks()
    torch.testing.assert_close(masks[:17].sum(1), torch.ones(17, dtype=torch.long))
    assert torch.equal(masks[:17].sum(0), torch.ones(17, dtype=torch.long))
    assert bool((masks[17:].sum(1) >= 2).all())


def test_target_token_initialization_focuses_most_interactions():
    model = HardWalshStudent(128, 512, seed=8, initialization="target_token")
    masks = model.hardened_masks()[128:]
    focused = round(0.75 * len(masks))
    assert not bool(masks[:focused, :4].any())
    assert not bool(masks[:focused, 12:].any())
    assert bool(masks[focused:, 12:].any())


def test_fixed_context_initialization_allocates_target_and_neighbor_terms():
    model = HardWalshStudent(128, 512, seed=8, initialization="fixed_context")
    masks = model.hardened_masks()[128:]
    target_count = round(0.40 * len(masks))
    context_count = round(0.45 * len(masks))
    assert not bool(masks[:target_count, 32:].any())
    assert not bool(masks[target_count:target_count + context_count, 48:].any())
    assert bool(masks[target_count + context_count:, 48:].any())


def test_sparse_export_matches_dense_hard_model_and_folds_duplicates():
    model = HardWalshStudent(10, 6, seed=9, char_chunk=3)
    with torch.no_grad():
        model.theta[1].copy_(model.theta[0])
        model.coefficient.copy_(torch.tensor([0.3, -0.1, 0.2, 0.4, -0.2, 0.1]))
        model.bias.fill_(-0.3)
    bits = np.random.default_rng(2).integers(0, 2, (31, 10), dtype=np.uint8)
    with torch.no_grad():
        dense = model(torch.tensor(bits)).numpy()
    artifact = export_sparse_student(model)
    sparse = sparse_student_logits(bits, artifact)
    np.testing.assert_allclose(sparse, dense, atol=1e-6)
    assert len(artifact["coefficient"]) <= 5


def test_standalone_token_layout_inference_needs_only_packed_lsh_table():
    rng = np.random.default_rng(13)
    codes = rng.integers(0, 2, (17, 5), dtype=np.uint8)
    ids = rng.integers(0, len(codes), (9, 3), dtype=np.int32)
    attention = np.ones_like(ids, dtype=np.uint8)
    bits = tokens_to_lsh_bits(ids, attention, codes)
    model = HardWalshStudent(bits.shape[1], 12, seed=3)
    exported = {
        "metadata": {"lsh_bits": 5},
        "lsh_codes_packed": torch.from_numpy(pack_bits(codes)),
        "student": export_sparse_student(model),
    }
    expected = sparse_student_logits(bits, exported["student"])
    np.testing.assert_allclose(
        predict_token_layout(ids, attention, exported), expected, atol=0
    )


def test_compact_npz_student_round_trip(tmp_path):
    rng = np.random.default_rng(19)
    codes = rng.integers(0, 2, (23, 7), dtype=np.uint8)
    model = HardWalshStudent(21, 16, seed=4)
    sparse = export_sparse_student(model)
    path = tmp_path / "student.npz"
    with open(path, "wb") as handle:
        np.savez(
            handle,
            n_bits=np.asarray(21, dtype=np.int32),
            offsets=sparse["offsets"], indices=sparse["indices"],
            coefficient=sparse["coefficient"],
            bias=np.asarray(sparse["bias"], dtype=np.float32),
            lsh_codes_packed=pack_bits(codes),
            metadata_json=np.asarray(json.dumps({
                "schema_version": 7, "lsh_bits": 7,
                "student_layout": "test",
            })),
        )
    loaded = load_compact_student(path)
    bits = rng.integers(0, 2, (13, 21), dtype=np.uint8)
    np.testing.assert_allclose(
        sparse_student_logits(bits, loaded["student"]),
        sparse_student_logits(bits, sparse), atol=1e-6,
    )


def test_warmup_cosine_has_expected_endpoints():
    assert warmup_cosine_factor(0, 100, 10, 0.1) == pytest.approx(0.1)
    assert warmup_cosine_factor(9, 100, 10, 0.1) == pytest.approx(1.0)
    assert warmup_cosine_factor(100, 100, 10, 0.1) == pytest.approx(0.1)


def test_training_beats_bias_on_a_planted_initial_character():
    rng = np.random.default_rng(11)
    n_bits, terms, seed = 8, 64, 4
    initial = HardWalshStudent(n_bits, terms, seed=seed)
    planted = initial.hardened_masks()[0].numpy().astype(np.uint8)
    bits = rng.integers(0, 2, (1536, n_bits), dtype=np.uint8)
    sign = 1.0 - 2.0 * ((bits @ planted) % 2)
    probability = 1.0 / (1.0 + np.exp(-1.7 * sign))
    gold = probability >= 0.5
    config = TrainConfig(
        terms=terms, steps=100, batch_size=256, char_chunk=64,
        mask_lr=1e-2, coefficient_lr=1e-2, warmup_fraction=0.05,
        eval_every=20, patience=10, compile_model=False, seed=seed,
        ste_variant="logspace", mask_parameterization="threshold",
        mask_init_magnitude=8.0,
    )
    _, summary = train_student(
        bits[:1024], probability[:1024], bits[1024:], probability[1024:],
        gold[1024:], config, device="cpu",
    )
    assert math.isfinite(summary["val"]["ce"])
    assert summary["val"]["ce"] < summary["baseline"]["ce"]
    assert summary["val"]["agreement"] > summary["baseline"]["agreement"] + 0.1
