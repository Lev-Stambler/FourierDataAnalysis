"""Part 2 affine-SFT algebra and oracle-separation tests."""

import json
from types import SimpleNamespace

import numpy as np
import pytest

from fda_exp.affine_sft import (
    affine_bins_from_coefficients,
    affine_bins_from_time,
    affine_energy_buckets,
    affine_energy_from_pairs,
    affine_line_membership,
    dataset_coefficients,
    group_grid,
    load_protocol,
    modular_rank,
    noiseless_qsft_control,
    normalized_importance_ess,
    random_full_column_rank,
    run_toy_audit,
    run_qwen_affine_audit,
)


def _small_problem(seed=0):
    q, n, b, m = 3, 3, 2, 4
    rng = np.random.default_rng(seed)
    points = group_grid(q, n)
    distribution = rng.random(len(points)); distribution /= distribution.sum()
    values = rng.random((len(points), m)); values /= values.sum(1, keepdims=True)
    matrix = random_full_column_rank(rng, n, b, q)
    offset = rng.integers(0, q, size=n)
    return q, n, distribution, values, matrix, offset


def test_protocol_is_dated_part_2_and_uses_energy_threshold():
    protocol, digest = load_protocol()
    assert protocol["part"] == 2
    assert protocol["date_iso"] == "2026-07-13"
    assert protocol["q"] == 248077
    assert protocol["n"] == 128
    assert protocol["energy_threshold"] == 1e-3
    assert protocol["rms_cutoff"] == pytest.approx(np.sqrt(1e-3))
    assert "absolute" in protocol["threshold_semantics"]
    assert len(digest) == 64


def test_affine_alias_identity_has_correct_normalization_and_phase():
    q, n, distribution, values, matrix, offset = _small_problem()
    coefficients = dataset_coefficients(distribution, values, q, n)
    h = q ** n * distribution[:, None] * values
    left = affine_bins_from_time(h, matrix, offset, q)
    right = affine_bins_from_coefficients(coefficients, matrix, offset, q)
    assert np.max(np.abs(left - right)) < 1e-11


def test_vector_energy_pair_identity():
    q, n, distribution, values, matrix, _ = _small_problem(seed=4)
    coefficients = dataset_coefficients(distribution, values, q, n)
    left = affine_energy_buckets(coefficients, matrix, q)
    right = affine_energy_from_pairs(distribution, values, matrix, q)
    assert np.max(np.abs(right.imag)) < 1e-12
    assert np.max(np.abs(left - right.real)) < 1e-11


def test_random_context_energy_identity_averages_shared_context_sections():
    totals = []
    pair_totals = []
    for seed in (7, 8):
        q, n, distribution, values, matrix, _ = _small_problem(seed=seed)
        coefficients = dataset_coefficients(distribution, values, q, n)
        totals.append(affine_energy_buckets(coefficients, matrix, q))
        pair_totals.append(affine_energy_from_pairs(distribution, values, matrix, q))
    assert np.allclose(np.mean(totals, axis=0), np.mean(pair_totals, axis=0), atol=1e-11)


def test_prime_field_rank_and_composite_rejection():
    assert modular_rank([[1, 0], [0, 1]], 17) == 2
    with pytest.raises(ValueError, match="prime"):
        modular_rank([[1]], 15)


def test_affine_line_membership_is_not_prefix_equality():
    q = 17
    direction = np.array([[1, 3, 8, 4], [2, 0, 1, 9]])
    scalars = np.array([5, 7])
    differences = scalars[:, None] * direction % q
    member, recovered = affine_line_membership(differences, direction, q)
    assert member.tolist() == [True, True]
    assert recovered.tolist() == scalars.tolist()
    differences[1, -1] += 1
    member, _ = affine_line_membership(differences, direction, q)
    assert member.tolist() == [True, False]


def test_importance_ess_reports_uniform_and_point_mass_limits():
    assert normalized_importance_ess(np.zeros((2, 8))).tolist() == [1.0, 1.0]
    concentrated = normalized_importance_ess(np.array([[0.0] + [-1000.0] * 7]))[0]
    assert concentrated == pytest.approx(1 / 8)


def test_noiseless_qsft_control_recovers_native_qary_support():
    report = noiseless_qsft_control(q=7, n=4, b=2, sparsity=12, seed=9, groups=4)
    assert report["all_recovered"]
    assert report["false_positives"] == 0
    assert report["max_degree"] <= 4
    assert report["oracle"] == "arbitrary chosen point evaluations on complete affine grids"


def test_registered_toy_audit_matches_all_identities():
    report = run_toy_audit()
    assert report["density_transform_max_error"] < 1e-10
    assert report["affine_alias_max_error"] < 1e-10
    assert report["affine_energy_max_error"] < 1e-10
    assert report["q_sft"]["all_trials_recovered"]
    assert report["theorem_status"] == "oracle_control"
    assert not report["strict_dataset_gl"]


def test_oracle_mismatch_artifact_cannot_be_loaded_as_frequency_bank(tmp_path):
    from fda_exp.qwen_argl import load_frequency_file

    path = tmp_path / "audit.json"
    path.write_text(json.dumps({
        "q": 248077,
        "theorem_status": "oracle_mismatch",
        "frequencies": [[1] + [0] * 127],
    }))
    with pytest.raises(ValueError, match="not frequency banks"):
        load_frequency_file(path)


def test_qwen_audit_keeps_natural_sampling_and_chosen_points_separate(
        tmp_path, monkeypatch):
    torch = pytest.importorskip("torch")
    import fda_exp.affine_sft as affine_module

    protocol, _ = load_protocol()
    protocol["q"] = 7
    protocol["qwen_audit"].update(contexts=2, affine_contexts=2, points_per_context=2)
    protocol["complexity_scenario"].update(sparsity=3, groups_for_cost_projection=2)
    path = tmp_path / "protocol.json"
    path.write_text(json.dumps(protocol))

    class FakeTeacher(torch.nn.Module):
        def __init__(self, q):
            super().__init__()
            self.anchor = torch.nn.Parameter(torch.zeros(()), requires_grad=False)
            self.q = q

        def forward(self, input_ids, past_key_values=None, **_):
            last = input_ids[:, -1] % self.q
            columns = torch.arange(self.q, device=input_ids.device)[None]
            logits = -(columns - ((last + 1) % self.q)[:, None]).abs().float()
            return SimpleNamespace(logits=logits[:, None], past_key_values=None)

    monkeypatch.setattr(
        affine_module,
        "torch_affine_points",
        lambda d, m, ell, q: torch.remainder(
            d[:, None, :] + ell[:, :, None] * m[:, None, :], q
        ),
    )
    report = run_qwen_affine_audit(
        FakeTeacher(7), np.zeros((2, 128), dtype=np.int64), 7,
        protocol_file=path, generation_batch=2, query_batch=2,
    )
    assert report["theorem_status"] == "oracle_mismatch"
    assert report["token_129_sampled_for_label"] is False
    assert report["natural_generator"]["contexts"] == 2
    assert report["chosen_affine_point_control"]["contexts"] == 2
    assert report["prefix_compatible_control"]["pairs_in_final_coordinate_subspace"] == 2
    assert report["frequencies"] == []
