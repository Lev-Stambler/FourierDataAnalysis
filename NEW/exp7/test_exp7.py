import math

import torch

from exp7 import CONFIG, Student, clean_state_dict, parameter_count, per_token_rms


def test_dense_free_shape_and_parameter_count():
    student = Student()
    assert student.vocabulary.shape == (248_320, 64)
    assert parameter_count() == 17_006_592
    assert all(parameter.ndim >= 2 for parameter in student.parameters())


def test_every_token_has_an_independent_dense_row():
    student = Student()
    assert student.vocabulary.stride() == (64, 1)
    assert student.vocabulary[1].data_ptr() - student.vocabulary[0].data_ptr() == 128


def test_tied_dense_embedding_and_unembedding():
    torch.manual_seed(7)
    student = Student()
    token_ids = torch.randint(0, CONFIG["vocab_size"], (2, 16))
    hidden = student.hidden(token_ids)
    torch.testing.assert_close(student.logits(token_ids), hidden @ student.vocabulary.T)


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
