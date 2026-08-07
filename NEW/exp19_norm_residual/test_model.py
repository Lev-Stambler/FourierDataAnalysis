from __future__ import annotations

import math
from dataclasses import asdict

import pytest
import torch
import torch.nn.functional as F

from exp19_norm_residual.model import (
    CLEAN_GROUP_R1_JOINT,
    CLEAN_GROUP_R1_TOKEN,
    CORRECTED_NO_ROUTER_TOKEN,
    CORRECTED_TRANSFORMER_DEEP,
    CORRECTED_TRANSFORMER_WIDE,
    LEGACY_POSTNORM_R1,
    MODEL_NAMES,
    PRENORM_AFFINE_FREE_SCALED,
    PRENORM_LEARNED_SCALED,
    CleanRankOneGroupSwiGLU,
    CorrectedKronLayer,
    CorrectedTransformerLayer,
    LearnedRMSNorm,
    ModelSpec,
    build_model,
    correctness_checks,
    model_inventory,
)
from exp19_norm_residual.telemetry import (
    norm_gamma_telemetry,
    small_factor_spectra,
    telemetry,
)


def tiny_overrides(name: str) -> dict:
    family = {
        CORRECTED_NO_ROUTER_TOKEN: "token-ffn",
        CORRECTED_TRANSFORMER_DEEP: "transformer",
        CORRECTED_TRANSFORMER_WIDE: "transformer",
    }.get(name, "group-kron")
    values = dict(
        context_length=8,
        vocab_size=32,
        width=8,
        depth=2,
        group_size=4,
        workspace1=2,
        workspace2=2,
        channel1=2,
        channel2=4,
        mixer_rank=2,
        hidden_workspace=5,
        hidden_channel=6,
        heads=2,
        transformer_ffn_plus_one_layers=0,
        activation_checkpointing=False,
    )
    if family == "token-ffn":
        values["token_ffn_width"] = 7
    elif family == "transformer":
        values["transformer_ffn_width"] = 7
    return values


def tiny_spec(name: str = CLEAN_GROUP_R1_TOKEN, **changes: object) -> ModelSpec:
    base = asdict(ModelSpec(name=name, **tiny_overrides(name)))
    base.update(changes)
    return ModelSpec(**base)


def test_learned_rms_norm_uses_fp32_statistic_and_learned_gamma() -> None:
    norm = LearnedRMSNorm(4, kind="learned-token").to(dtype=torch.bfloat16)
    with torch.no_grad():
        norm.weight.copy_(torch.tensor([0.5, 1.0, 1.5, 2.0]))
    value = torch.tensor(
        [[[1e-2, 2e-2, 3e-2, 4e-2], [10.0, 20.0, 30.0, 40.0]]],
        dtype=torch.bfloat16,
    )
    expected = value * torch.rsqrt(
        value.float().square().mean(-1, keepdim=True) + 1e-6
    ).to(value.dtype)
    expected = expected * norm.weight
    assert norm(value).dtype == torch.bfloat16
    torch.testing.assert_close(norm(value), expected)
    assert norm.weight.requires_grad


def test_joint_group_norm_shares_statistic_but_not_between_groups() -> None:
    norm = LearnedRMSNorm(2, kind="learned-joint", group_size=2)
    value = torch.tensor([[[1.0, 1.0], [3.0, 3.0], [10.0, 10.0], [10.0, 10.0]]])
    output = norm(value)
    assert float(output[0, 0, 0].detach()) != pytest.approx(1.0)
    torch.testing.assert_close(output[0, 2:], torch.ones_like(output[0, 2:]))


@pytest.mark.parametrize("name", [CLEAN_GROUP_R1_TOKEN, CLEAN_GROUP_R1_JOINT])
def test_zero_branch_clean_kron_layer_is_exact_identity(name: str) -> None:
    torch.manual_seed(1)
    layer = CorrectedKronLayer(tiny_spec(name, norm_kind=(
        "learned-joint" if name == CLEAN_GROUP_R1_JOINT else "learned-token"
    )), 0)
    with torch.no_grad():
        for parameter_name, parameter in layer.named_parameters():
            if "_norm.weight" not in parameter_name:
                parameter.zero_()
    value = torch.randn(2, 8, 8)
    torch.testing.assert_close(layer(value), value, rtol=0, atol=0)
    assert layer.mixer_gain is None and layer.ffn_gain is None
    assert layer.spec.branch_scale == pytest.approx(1 / math.sqrt(4))


def test_zero_branch_corrected_transformer_is_exact_identity() -> None:
    spec = tiny_spec(
        CORRECTED_TRANSFORMER_DEEP,
        family="transformer",
        transformer_ffn_width=7,
    )
    layer = CorrectedTransformerLayer(spec)
    with torch.no_grad():
        for name, parameter in layer.named_parameters():
            if "_norm.weight" not in name:
                parameter.zero_()
    value = torch.randn(2, 8, 8)
    torch.testing.assert_close(layer(value), value, rtol=0, atol=0)


def test_structured_contraction_matches_explicit_kronecker_matrix() -> None:
    torch.manual_seed(3)
    layer = CorrectedKronLayer(tiny_spec(), 1).double()
    value = torch.randn(2, 8, 8, dtype=torch.float64)
    normalized = layer.mixer_norm(value)
    actual = layer.mixer.rank_outputs(normalized)
    expected = layer.mixer.materialized_rank_outputs(normalized)
    torch.testing.assert_close(actual, expected, rtol=1e-10, atol=1e-10)


def test_clean_group_ffn_matches_explicit_dense_kronecker_matrices() -> None:
    torch.manual_seed(5)
    module = CleanRankOneGroupSwiGLU(4, 6, 3, 5).double()
    value = torch.randn(2, 1, 4, 6, dtype=torch.float64)

    def normalized(item: torch.Tensor) -> torch.Tensor:
        norm = item.float().square().sum(-1, keepdim=True).sqrt().clamp_min(1e-8)
        return item / norm.to(item.dtype)

    flat = value.flatten(2)
    gate_matrix = torch.kron(
        normalized(module.gate_workspace), normalized(module.gate_channel)
    )
    up_matrix = torch.kron(
        normalized(module.up_workspace), normalized(module.up_channel)
    )
    down_matrix = torch.kron(
        normalized(module.down_workspace), normalized(module.down_channel)
    )
    hidden = F.silu(F.linear(flat, gate_matrix)) * F.linear(flat, up_matrix)
    expected = F.linear(hidden, down_matrix).reshape_as(value)
    torch.testing.assert_close(module(value), expected, rtol=1e-10, atol=1e-10)


@pytest.mark.parametrize(
    "name",
    [
        CLEAN_GROUP_R1_TOKEN,
        CLEAN_GROUP_R1_JOINT,
        CORRECTED_NO_ROUTER_TOKEN,
        CORRECTED_TRANSFORMER_DEEP,
        CORRECTED_TRANSFORMER_WIDE,
    ],
)
def test_future_group_cannot_change_past_group(name: str) -> None:
    torch.manual_seed(7)
    model = build_model(name, **tiny_overrides(name)).eval()
    first = torch.randint(0, 32, (1, 8))
    second = first.clone()
    second[:, 4:] = torch.randint(0, 32, (1, 4))
    with torch.no_grad():
        first_hidden = model.hidden(first)
        second_hidden = model.hidden(second)
    torch.testing.assert_close(first_hidden[:, :4], second_hidden[:, :4])


@pytest.mark.parametrize("name", MODEL_NAMES[1:])
def test_tiny_models_have_finite_complete_gradients(name: str) -> None:
    torch.manual_seed(11)
    model = build_model(name, **tiny_overrides(name))
    tokens = torch.randint(0, 32, (2, 8))
    loss = F.cross_entropy(model(tokens).flatten(0, 1), tokens.flatten())
    loss.backward()
    assert torch.isfinite(loss)
    assert all(
        parameter.grad is not None and torch.isfinite(parameter.grad).all()
        for parameter in model.parameters()
    )


def test_clean_candidate_parameter_inventory_and_no_redundant_scales() -> None:
    inventory = model_inventory(build_model(CLEAN_GROUP_R1_TOKEN))
    assert inventory["total_parameters"] == 5_400_960
    assert inventory["body_parameters"] == 3_303_808
    assert inventory["norm_parameters"] == 8_320
    assert inventory["redundant_scale_parameters"] == 0
    assert inventory["fixed_branch_scale"] == pytest.approx(0.125)
    model = build_model(CLEAN_GROUP_R1_TOKEN)
    names = {name for name, _ in model.named_parameters()}
    forbidden = {
        "path_amplitudes",
        "input_channel_scale",
        "output_channel_scale",
        "mixer_gain",
        "ffn_gain",
    }
    assert not any(any(marker in name for marker in forbidden) for name in names)


def test_audit_inventory_records_semantic_not_forced_parameter_matches() -> None:
    expected = {
        LEGACY_POSTNORM_R1: 5_400_928,
        PRENORM_AFFINE_FREE_SCALED: 5_400_928,
        PRENORM_LEARNED_SCALED: 5_409_248,
        CLEAN_GROUP_R1_TOKEN: 5_400_960,
        CLEAN_GROUP_R1_JOINT: 5_400_960,
        CORRECTED_NO_ROUTER_TOKEN: 5_400_960,
        CORRECTED_TRANSFORMER_DEEP: 5_401_088,
        CORRECTED_TRANSFORMER_WIDE: 5_401_088,
    }
    assert {
        name: model_inventory(build_model(name))["total_parameters"] for name in MODEL_NAMES
    } == expected


def test_telemetry_reports_updates_hidden_gammas_and_factor_spectra() -> None:
    torch.manual_seed(13)
    model = build_model(CLEAN_GROUP_R1_TOKEN, **tiny_overrides(CLEAN_GROUP_R1_TOKEN)).eval()
    tokens = torch.randint(0, 32, (2, 8))
    report = telemetry(model, tokens, layers=[0], include_spectra=True)
    assert report["architecture_identity"] == CLEAN_GROUP_R1_TOKEN
    assert report["embedding_residual_scaled"] is True
    assert report["initial_state"]["rms"] > 0.5
    assert len(report["layers"]) == 1
    row = report["layers"][0]
    assert row["mixer"]["update_to_state_rms"] >= 0
    assert -1 <= row["mixer"]["residual_input_output_cosine"] <= 1
    assert row["mixer_raw_branch_rms"] >= 0
    assert row["ffn"]["update_to_state_rms"] >= 0
    assert row["group_hidden"]["participation_ratio"] > 0
    assert len(report["norm_gammas"]) == 5
    assert report["small_factor_spectra"]
    assert all(item["condition"] >= 1 for item in report["small_factor_spectra"])
    assert all(
        item["row_energy_maximum"] >= item["row_energy_minimum"]
        for item in report["small_factor_spectra"]
    )
    assert report["scale_parameters"]
    assert report["conditioning"]["scaled_update_to_state_ratios"]


def test_norm_and_spectrum_helpers_are_independently_callable() -> None:
    model = build_model(CLEAN_GROUP_R1_JOINT, **tiny_overrides(CLEAN_GROUP_R1_JOINT))
    gammas = norm_gamma_telemetry(model)
    spectra = small_factor_spectra(model, layers=[0])
    assert {row["kind"] for row in gammas} == {"learned-token", "learned-joint"}
    assert any(row["factor"].startswith("mixer.outer") for row in spectra)
    assert any(row["factor"].startswith("group_ffn.gate") for row in spectra)


def test_checkpoint_toggle_preserves_state_and_forward() -> None:
    torch.manual_seed(17)
    eager = build_model(
        CLEAN_GROUP_R1_TOKEN,
        activation_checkpointing=False,
        **{k: v for k, v in tiny_overrides(CLEAN_GROUP_R1_TOKEN).items() if k != "activation_checkpointing"},
    )
    checked = build_model(
        CLEAN_GROUP_R1_TOKEN,
        activation_checkpointing=True,
        **{k: v for k, v in tiny_overrides(CLEAN_GROUP_R1_TOKEN).items() if k != "activation_checkpointing"},
    )
    checked.load_state_dict(eager.state_dict())
    eager.train()
    checked.train()
    tokens = torch.randint(0, 32, (2, 8))
    torch.testing.assert_close(eager.hidden(tokens), checked.hidden(tokens))


def test_clean_tracks_scale_only_embedding_use_in_residual_stream() -> None:
    torch.manual_seed(19)
    clean = build_model(CLEAN_GROUP_R1_TOKEN, **tiny_overrides(CLEAN_GROUP_R1_TOKEN))
    audit = build_model(
        PRENORM_LEARNED_SCALED, **tiny_overrides(PRENORM_LEARNED_SCALED)
    )
    audit.vocabulary.data.copy_(clean.vocabulary.data)
    tokens = torch.arange(8).repeat(4, 1)
    clean_rms = telemetry(clean, tokens, layers=[0], include_spectra=False)[
        "initial_state"
    ]["rms"]
    audit_rms = telemetry(audit, tokens, layers=[0], include_spectra=False)[
        "initial_state"
    ]["rms"]
    assert clean_rms == pytest.approx(audit_rms * math.sqrt(8), rel=1e-6)


@pytest.mark.parametrize("name", MODEL_NAMES)
def test_reduced_architecture_correctness_gate(name: str) -> None:
    report = correctness_checks(name, torch.device("cpu"))
    assert report["pass"], report
