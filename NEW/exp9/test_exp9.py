import math

import torch

from exp9 import (
    BatchedMuon,
    Block,
    CONFIG,
    SplitOptimizer,
    Student,
    activation_scale_diagnostics,
    clean_state_dict,
    cosine_lr,
    document_contexts,
    exact_local_contexts,
    exact_kl_rows,
    fan_in_normalized,
    gradient_diagnostics,
    load_initial,
    parameter_count,
    per_token_rms,
    save_checkpoint,
    scale_diagnostics,
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


def test_factor_normalization_removes_the_bilinear_scale_gauge():
    torch.manual_seed(9)
    block = Block()
    value = torch.randn(2, 16, 64)
    expected = block(value)
    with torch.no_grad():
        block.a.mul_(17)
        block.b.mul_(0.125)
    torch.testing.assert_close(block(value), expected, atol=1e-5, rtol=1e-5)
    normalized_a = fan_in_normalized(block.a, CONFIG["context_length"])
    normalized_b = fan_in_normalized(block.b, CONFIG["width"])
    torch.testing.assert_close(
        normalized_a.square().mean((-2, -1)),
        torch.full((CONFIG["rank"],), 1 / CONFIG["context_length"]),
    )
    torch.testing.assert_close(
        normalized_b.square().mean((-2, -1)),
        torch.full((CONFIG["rank"],), 1 / CONFIG["width"]),
    )


def test_canonical_projection_and_post_residual_rms_are_exact():
    torch.manual_seed(10)
    student = Student()
    with torch.no_grad():
        student.blocks[0].a.mul_(31)
        student.blocks[0].b.mul_(0.03125)
    student.canonicalize_factors_()
    scales = scale_diagnostics(student)
    assert math.isclose(
        scales["factor_a_rms_min"], 1 / math.sqrt(CONFIG["context_length"]), rel_tol=1e-6
    )
    assert math.isclose(
        scales["factor_a_rms_max"], 1 / math.sqrt(CONFIG["context_length"]), rel_tol=1e-6
    )
    assert math.isclose(
        scales["factor_b_rms_min"], 1 / math.sqrt(CONFIG["width"]), rel_tol=1e-6
    )
    assert math.isclose(
        scales["factor_b_rms_max"], 1 / math.sqrt(CONFIG["width"]), rel_tol=1e-6
    )
    token_ids = torch.randint(0, CONFIG["vocab_size"], (2, 16))
    activations = activation_scale_diagnostics(student, token_ids)
    assert activations["residual_post_norm_max_error"] < 1e-4


def test_fresh_initialization_refuses_legacy_checkpoints(tmp_path, monkeypatch):
    student = Student()
    legacy = tmp_path / "legacy.pt"
    torch.save({"schema": "exp7-dense-free-v1", "model": student.state_dict()}, legacy)
    monkeypatch.setitem(CONFIG, "resume", str(legacy))
    try:
        load_initial(Student(), torch.device("cpu"))
    except RuntimeError as error:
        assert "refuses legacy checkpoints" in str(error)
    else:
        raise AssertionError("legacy checkpoint was accepted")


def test_finite_optimizer_step_and_checkpoint_round_trip(tmp_path, monkeypatch):
    for key, value in {
        "vocab_size": 32,
        "width": 8,
        "depth": 2,
        "rank": 2,
        "context_length": 4,
    }.items():
        monkeypatch.setitem(CONFIG, key, value)
    torch.manual_seed(11)
    student = Student()
    optimizer = SplitOptimizer(
        BatchedMuon(student.blocks.parameters(), lr=0.02),
        torch.optim.AdamW([student.vocabulary], lr=3e-4),
    )
    token_ids = torch.randint(0, CONFIG["vocab_size"], (3, CONFIG["context_length"]))
    before = [parameter.detach().clone() for parameter in student.parameters()]
    loss = student.logits(token_ids).float().square().mean()
    loss.backward()
    optimizer.step()
    student.canonicalize_factors_()
    assert torch.isfinite(loss)
    assert all(torch.isfinite(parameter).all() for parameter in student.parameters())
    assert update_diagnostics(student, before)["body_changed_fraction"] > 0

    checkpoint = tmp_path / "checkpoint.pt"
    save_checkpoint(checkpoint, student, optimizer, {}, {}, "https://wandb.invalid/run")
    monkeypatch.setitem(CONFIG, "resume", str(checkpoint))
    restored = Student()
    saved, metadata = load_initial(restored, torch.device("cpu"))
    assert saved["schema"] == "exp9-standard-muon-v1"
    assert metadata["initialization"] == "exp9-standard-muon_resume"
    for expected, actual in zip(student.parameters(), restored.parameters()):
        torch.testing.assert_close(actual, expected)


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


def test_batched_muon_matches_independent_torch_muon_matrices():
    torch.manual_seed(12)
    for size in (16, 64):
        batched = torch.nn.Parameter(torch.randn(2, size, size))
        references = [
            torch.nn.Parameter(matrix.detach().clone()) for matrix in batched
        ]
        ours = BatchedMuon([batched], lr=0.02, momentum=0.95, ns_steps=5)
        standard = torch.optim.Muon(
            references,
            lr=0.02,
            momentum=0.95,
            nesterov=True,
            ns_steps=5,
            adjust_lr_fn="original",
            weight_decay=0.0,
        )
        for _ in range(2):
            gradient = torch.randn_like(batched)
            batched.grad = gradient.clone()
            for parameter, matrix_gradient in zip(
                references, gradient, strict=True
            ):
                parameter.grad = matrix_gradient.clone()
            ours.step()
            standard.step()
        torch.testing.assert_close(
            batched,
            torch.stack(references),
            atol=2e-3,
            rtol=2e-2,
        )
        assert "second_momentum_buffer" not in ours.state[batched]


def test_batched_muon_does_not_mix_rank_slices():
    parameter = torch.nn.Parameter(torch.randn(3, 4, 4))
    before = parameter.detach().clone()
    parameter.grad = torch.zeros_like(parameter)
    parameter.grad[1].normal_()
    optimizer = BatchedMuon([parameter], lr=0.02)
    optimizer.step()
    torch.testing.assert_close(parameter[0], before[0])
    torch.testing.assert_close(parameter[2], before[2])
    assert not torch.equal(parameter[1], before[1])


def test_exp8_migration_preserves_only_vocabulary_adam_state():
    vocabulary = torch.nn.Parameter(torch.randn(8, 4))
    body = torch.nn.Parameter(torch.randn(2, 4, 4))
    old = torch.optim.AdamW(
        [{"params": [vocabulary]}, {"params": [body]}],
        lr=3e-4,
    )
    vocabulary.grad = torch.randn_like(vocabulary)
    body.grad = torch.randn_like(body)
    old.step()

    new_vocabulary = torch.nn.Parameter(vocabulary.detach().clone())
    new_body = torch.nn.Parameter(body.detach().clone())
    split = SplitOptimizer(
        BatchedMuon([new_body], lr=0.02),
        torch.optim.AdamW([new_vocabulary], lr=3e-4),
    )
    split.restore_exp8_vocabulary_state(old.state_dict())
    restored = split.vocabulary.state[new_vocabulary]
    torch.testing.assert_close(
        restored["exp_avg"], old.state[vocabulary]["exp_avg"]
    )
    assert split.body.state == {}
    SplitOptimizer,
