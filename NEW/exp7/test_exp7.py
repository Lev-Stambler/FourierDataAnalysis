import math

import torch

from exp7 import (
    CONFIG,
    Student,
    clean_state_dict,
    cosine_lr,
    document_contexts,
    exact_local_contexts,
    exact_kl_rows,
    gradient_diagnostics,
    parameter_count,
    per_token_rms,
    update_diagnostics,
)


def test_dense_free_shape_and_parameter_count():
    student = Student()
    assert student.vocabulary.shape == (248_320, 64)
    assert student.vocabulary.dtype == torch.float32
    assert CONFIG["teacher_probability_dtype"] == "float32"
    assert CONFIG["physical_local_batch"] == 49_152
    assert parameter_count() == 17_006_592
    assert all(parameter.ndim >= 2 for parameter in student.parameters())


def test_every_token_has_an_independent_dense_row():
    student = Student()
    assert student.vocabulary.stride() == (64, 1)
    assert student.vocabulary[1].data_ptr() - student.vocabulary[0].data_ptr() == 256


def test_tied_dense_embedding_and_unembedding():
    torch.manual_seed(7)
    student = Student()
    token_ids = torch.randint(0, CONFIG["vocab_size"], (2, 16))
    hidden = student.hidden(token_ids)
    torch.testing.assert_close(student.logits(token_ids), hidden @ student.vocabulary.T)


def test_forward_is_exact_mean_per_token_kl():
    torch.manual_seed(8)
    student = Student()
    token_ids = torch.randint(0, CONFIG["vocab_size"], (2, 16))
    logits = student.logits(token_ids)
    teacher_probability = torch.softmax(torch.randn_like(logits), -1)
    teacher_entropy = -(teacher_probability * teacher_probability.log()).sum(-1)
    torch.testing.assert_close(
        student(token_ids, teacher_probability, teacher_entropy),
        exact_kl_rows(logits, teacher_probability, teacher_entropy).mean(),
    )


def test_rms_is_independent_per_token():
    value = torch.randn(3, 16, 64, dtype=torch.bfloat16)
    normalized = per_token_rms(value)
    torch.testing.assert_close(
        normalized.float().square().mean(-1),
        torch.ones(3, 16),
        atol=1e-2,
        rtol=1e-2,
    )


def test_factor_materialization_is_exact():
    first = torch.randn(5, 7)
    second = torch.randn(3, 7)
    dense = (first[:, None, :] * second[None, :, :]).reshape(15, 7)
    for token in range(15):
        i, j = divmod(token, 3)
        torch.testing.assert_close(dense[token], first[i] * second[j])


def test_checkpoint_prefix_cleanup():
    value = torch.randn(2, 2)
    clean = clean_state_dict({"module._orig_mod.blocks.0.a": value})
    assert clean == {"blocks.0.a": value}


def test_residual_scale_matches_depth():
    assert math.isclose(1 / math.sqrt(CONFIG["depth"]), 1 / math.sqrt(32))


def test_stream_uses_all_nonoverlapping_next_token_windows():
    contexts = document_contexts(list(range(36)))
    assert contexts.shape == (2, 16)
    assert contexts[0].tolist() == list(range(16))
    assert contexts[1].tolist() == list(range(17, 33))


def test_one_trillion_token_tail_is_exact_across_eight_ranks():
    full_step = 8 * 8_192 * 16
    full_updates, tail = divmod(1_000_000_000_000, full_step)
    assert full_updates == 953_674
    assert tail == 331_776
    assert exact_local_contexts(tail, 8) == 2_592
    assert full_updates * full_step + 8 * 2_592 * 16 == 1_000_000_000_000


def test_cosine_lr_is_token_based_and_ends_at_one_percent():
    start = 0.025
    budget = 1_000_000_000_000
    assert cosine_lr(start, 0, budget) == start
    assert math.isclose(cosine_lr(start, budget, budget), start * 0.01)
    assert start * 0.01 < cosine_lr(start, budget // 2, budget) < start


def test_update_diagnostics_detect_weight_changes():
    student = Student()
    before = [parameter.detach().clone() for parameter in student.parameters()]
    with torch.no_grad():
        student.vocabulary[0, 0] += 0.125
        student.blocks[0].a[0, 0, 0] += 0.125
    metrics = update_diagnostics(student, before)
    assert metrics["vocabulary_update_rms"] > 0
    assert metrics["vocabulary_changed_fraction"] > 0
    assert metrics["body_update_rms"] > 0
    assert metrics["body_changed_fraction"] > 0


def test_gradient_diagnostics_split_vocabulary_and_body():
    student = Student()
    for parameter in student.parameters():
        parameter.grad = torch.ones_like(parameter)
    metrics = gradient_diagnostics(student)
    assert metrics["vocabulary_grad_norm"] > 0
    assert metrics["vocabulary_grad_rms"] == 1
    assert metrics["body_grad_norm"] > 0
    assert metrics["body_grad_rms"] == 1
