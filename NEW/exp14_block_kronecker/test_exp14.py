from __future__ import annotations

import base64
import gzip
import hashlib
import json
import subprocess
from pathlib import Path

import numpy as np
import pytest
import torch

from exp14_block_kronecker.data import GROUP_SIZE, block_batch
from exp13_wikitext_confirmation.study import Recipe, recipe_slug
from exp14_block_kronecker.campaign import _cell_path, fused_loss
from exp14_block_kronecker.compile_probe import (
    EXECUTION_MODES,
    MODE_REPEATS,
    mode_summary,
    selected_mode,
)
from exp14_block_kronecker.compiled_batch_probe import BATCHES, summarize_batches
from exp14_block_kronecker.optimizer_audit import (
    ADAMW_LRS,
    MUON_AUXILIARY_LRS,
    MUON_BODY_LRS,
    coarse_recipes as optimizer_audit_recipes,
    schedule_variants,
    summarize_recipes,
    top_by_family,
)
from exp14_block_kronecker import remote_runner
from exp14_block_kronecker.model import (
    CANDIDATE,
    MODEL_NAMES,
    NO_WORKSPACE_PERMUTATION,
    STANDARD_CONTROLS,
    BlockKroneckerLayer,
    build_model,
    model_inventory,
)


def tiny_overrides() -> dict[str, int]:
    return {
        "context_length": 12,
        "vocab_size": 19,
        "width": 12,
        "depth": 2,
        "heads": 3,
        "ffn_width": 24,
        "group_count": 3,
        "workspace1": 2,
        "workspace2": 2,
        "channel1": 3,
        "channel2": 4,
        "rank": 2,
    }


def test_remote_shell_readiness_retries_only_missing_sentinel(monkeypatch: pytest.MonkeyPatch) -> None:
    attempts = 0

    def probe(command: str, *, capture: bool = False) -> object:
        nonlocal attempts
        assert command == "true" and capture
        attempts += 1
        if attempts < 3:
            raise RuntimeError("Northflank exec lacked remote sentinel")
        return object()

    monkeypatch.setattr(remote_runner.infrastructure, "_exec", probe)
    monkeypatch.setattr(remote_runner.time, "sleep", lambda _: None)
    assert remote_runner.wait_for_remote_shell(timeout_seconds=1) == {
        "ready": True,
        "attempts": 3,
    }


def test_every_remote_exec_retries_missing_sentinel_transport(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    attempts = 0

    def raw(command: str, *, capture: bool = False) -> object:
        nonlocal attempts
        attempts += 1
        if attempts < 3:
            raise RuntimeError("Northflank exec lacked remote sentinel")
        return object()

    monkeypatch.setattr(remote_runner, "_RAW_REMOTE_EXEC", raw)
    monkeypatch.setattr(remote_runner.time, "sleep", lambda _: None)
    assert remote_runner.resilient_remote_exec("nvidia-smi", capture=True) is not None
    assert attempts == 3


def test_remote_exec_does_not_retry_real_remote_failure(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    def raw(command: str, *, capture: bool = False) -> object:
        raise RuntimeError("invalid GPU inventory")

    monkeypatch.setattr(remote_runner, "_RAW_REMOTE_EXEC", raw)
    with pytest.raises(RuntimeError, match="invalid GPU inventory"):
        remote_runner.resilient_remote_exec("nvidia-smi", capture=True)


def test_campaign_job_control_paths_are_persistent_and_attempt_scoped() -> None:
    paths = remote_runner._job_paths(
        "/cache/exp14/state-0123456789ab", "exp19", 3
    )
    assert paths["done"] == "/cache/exp14/state-0123456789ab/runner/exp19/done"
    assert paths["heartbeat"] == "/cache/exp14/state-0123456789ab/heartbeat"
    assert paths["log"].endswith("/runner/exp19/attempt-3.log")


@pytest.mark.parametrize(
    ("output", "expected"),
    [
        ("EXP14_JOB_RUNNING 12\n", {"status": "running", "heartbeat_age_seconds": 12}),
        ("EXP14_JOB_STALE 181\n", {"status": "stale", "heartbeat_age_seconds": 181}),
        ("EXP14_JOB_DONE 0\n", {"status": "done", "return_code": 0}),
        ("EXP14_JOB_MISSING\n", {"status": "missing"}),
    ],
)
def test_persistent_job_status_parsing(
    monkeypatch: pytest.MonkeyPatch, output: str, expected: dict[str, object]
) -> None:
    def probe(command: str, *, capture: bool = False) -> subprocess.CompletedProcess[str]:
        assert capture and "EXP14_JOB_STALE" in command
        return subprocess.CompletedProcess([], 0, stdout=output)

    monkeypatch.setattr(remote_runner.infrastructure, "_exec", probe)
    actual = remote_runner.remote_job_status(
        remote_runner._job_paths("/cache/exp14/state-0123456789ab", "exp19", 1)
    )
    for key, value in expected.items():
        assert actual[key] == value


def test_resume_bundle_is_hash_and_state_verified(tmp_path: Path) -> None:
    payload = tmp_path / "norm-cells" / "preflight.json"
    payload.parent.mkdir()
    payload.write_text('{"status":"pass"}')
    digest = hashlib.sha256(payload.read_bytes()).hexdigest()
    manifest = {
        "schema": remote_runner.RESUME_BUNDLE_SCHEMA,
        "state_digest": "0123456789ab",
        "files": {"norm-cells/preflight.json": digest},
    }
    (tmp_path / "manifest.json").write_text(json.dumps(manifest))
    assert remote_runner.validate_resume_bundle(tmp_path, "0123456789ab") == manifest
    payload.write_text("corrupted")
    with pytest.raises(ValueError, match="hash mismatch"):
        remote_runner.validate_resume_bundle(tmp_path, "0123456789ab")


def test_gpu_inventory_waits_for_one_complete_eight_gpu_snapshot(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    snapshots = [
        "0, NVIDIA H100 80GB HBM3, 81559 MiB, 0 %\n",
        "\n".join(
            f"{index}, NVIDIA H100 80GB HBM3, 81559 MiB, 0 %"
            for index in range(2)
        ),
        "\n".join(
            f"{index}, NVIDIA H100 80GB HBM3, 81559 MiB, 0 %"
            for index in range(8)
        ),
    ]

    def probe(command: str, *, capture: bool = False) -> object:
        assert capture
        if "nohup bash" in command:
            assert "nvidia-smi" in command
            return subprocess.CompletedProcess([], 0, stdout="")
        assert "cat /root/exp14-gpu-inventory.ready" in command
        return subprocess.CompletedProcess([], 0, stdout=snapshots.pop(0))

    monkeypatch.setattr(remote_runner.infrastructure, "_exec", probe)
    monkeypatch.setattr(remote_runner.time, "sleep", lambda _: None)
    inventory, rows, attempts = remote_runner.wait_for_gpu_inventory(timeout_seconds=1)
    assert attempts == 3
    assert len(rows) == 8
    assert inventory.count("NVIDIA H100") == 8


def test_long_remote_setup_runs_detached_and_polls_done_file(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    polls = iter(("", "0\n"))

    def probe(command: str, *, capture: bool = False) -> object:
        assert capture
        if "nohup bash" in command:
            assert "cache-restore" in command
            return subprocess.CompletedProcess([], 0, stdout="")
        assert "cat /root/exp14-cache-restore.done" in command
        return subprocess.CompletedProcess([], 0, stdout=next(polls))

    monkeypatch.setattr(remote_runner.infrastructure, "_exec", probe)
    monkeypatch.setattr(remote_runner.time, "sleep", lambda _: None)
    assert remote_runner.run_remote_command_detached(
        "tar -xf snapshot.tar", operation="cache-restore", timeout_seconds=1
    ) == {"operation": "cache-restore", "polls": 2, "status": "complete"}


def test_artifact_retrieval_falls_back_to_gzip_base64(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path
) -> None:
    payload = b'{"status":"failed-preflight"}\n'
    encoded = base64.b64encode(gzip.compress(payload)).decode()
    monkeypatch.setattr(
        remote_runner.infrastructure,
        "_download",
        lambda *args, **kwargs: False,
    )
    monkeypatch.setattr(
        remote_runner.infrastructure,
        "_exec",
        lambda *args, **kwargs: subprocess.CompletedProcess(
            [],
            0,
            stdout=(
                "Exec service gpu in project p with command artifact\n"
                + encoded
                + "\nCommand completed successfully, exit code: 0\n"
            ),
        ),
    )
    target = tmp_path / "preflight.json"
    assert remote_runner.retrieve_artifact("/remote/preflight.json", target)
    assert target.read_bytes() == payload


def test_factored_order_five_path_matches_materialized_forward_and_backward() -> None:
    torch.manual_seed(1401)
    model = build_model(CANDIDATE, **tiny_overrides()).double()
    block = model.blocks[1]
    assert isinstance(block, BlockKroneckerLayer)
    value = torch.randn(2, 12, 12, dtype=torch.float64, requires_grad=True)
    factored = block.rank_outputs(value)
    materialized = block.materialized_rank_outputs(value)
    assert torch.allclose(factored, materialized, atol=1e-10, rtol=1e-10)
    parameters = (
        value,
        block.outer.raw,
        block.workspace1.raw,
        block.workspace2.raw,
        block.channel1.raw,
        block.channel2.raw,
    )
    factored_gradients = torch.autograd.grad(
        factored.square().sum(), parameters, retain_graph=True
    )
    materialized_gradients = torch.autograd.grad(
        materialized.square().sum(), parameters
    )
    for left, right in zip(factored_gradients, materialized_gradients, strict=True):
        assert torch.allclose(left, right, atol=1e-9, rtol=1e-9)


@pytest.mark.parametrize("name", MODEL_NAMES)
def test_models_are_group_causal_but_not_inner_token_causal(name: str) -> None:
    torch.manual_seed(1402)
    model = build_model(name, **tiny_overrides()).eval()
    first = torch.randint(0, 19, (2, 12))
    future_group = first.clone()
    future_group[:, 8:] = torch.randint(0, 19, (2, 4))
    with torch.no_grad():
        base = model(first)
        changed = model(future_group)
    assert torch.allclose(base[:, :8], changed[:, :8], atol=1e-6, rtol=0.0)

    same_group = first.clone()
    same_group[:, 3] = (same_group[:, 3] + 1) % 19
    with torch.no_grad():
        inner_changed = model(same_group)
    assert float((base[:, 0] - inner_changed[:, 0]).abs().max()) > 1e-8


def test_outer_factor_is_block_lower_triangular_and_inner_block_is_dense() -> None:
    torch.manual_seed(1403)
    model = build_model(CANDIDATE, **tiny_overrides()).double()
    block = model.blocks[0]
    assert isinstance(block, BlockKroneckerLayer)
    outer = block.outer.value()[0]
    inner = torch.kron(block.workspace1.value()[0], block.workspace2.value()[0])
    token = torch.kron(outer, inner)
    group = tiny_overrides()["workspace1"] * tiny_overrides()["workspace2"]
    for destination_group in range(3):
        for source_group in range(destination_group + 1, 3):
            rows = slice(destination_group * group, (destination_group + 1) * group)
            columns = slice(source_group * group, (source_group + 1) * group)
            assert torch.count_nonzero(token[rows, columns]) == 0
    diagonal_block = token[:group, :group]
    assert torch.count_nonzero(torch.triu(diagonal_block, diagonal=1)) > 0


def test_block_batch_reconstructs_contiguous_stream_and_shifts_one_group() -> None:
    windows = np.arange(4 * 257, dtype=np.int64).reshape(4, 257)
    inputs, targets = block_batch(windows, np.array([0, 2]), torch.device("cpu"))
    assert torch.equal(inputs[0], torch.arange(256))
    expected = torch.cat((torch.arange(16, 257), torch.arange(257, 272)))
    assert torch.equal(targets[0], expected)
    assert targets.shape == inputs.shape == (2, 256)
    assert GROUP_SIZE == 16


def test_fused_loss_makes_shifted_targets_and_hidden_states_contiguous() -> None:
    class FakeModel:
        vocabulary = torch.randn(7, 3)

        def hidden(self, inputs: torch.Tensor) -> torch.Tensor:
            base = torch.randn(*inputs.shape, 6)
            return base[..., ::2]

    class ViewOnlyLoss:
        def __call__(
            self,
            hidden: torch.Tensor,
            targets: torch.Tensor,
            vocabulary: torch.Tensor,
        ) -> torch.Tensor:
            assert hidden.is_contiguous()
            assert targets.is_contiguous()
            hidden.view(-1, hidden.shape[-1])
            targets.view(-1)
            return hidden.sum() * 0.0 + vocabulary.sum() * 0.0

    inputs = torch.ones(2, 4, dtype=torch.long)
    targets = torch.ones(2, 8, dtype=torch.long)[:, 1:5]
    assert not targets.is_contiguous()
    loss = fused_loss(FakeModel(), inputs, targets, ViewOnlyLoss())  # type: ignore[arg-type]
    assert torch.isfinite(loss)


def test_compile_probe_selects_only_a_measurably_faster_mode() -> None:
    rows = []
    speeds = {
        "eager": 100.0,
        "default": 103.0,
        "reduce-overhead": 106.0,
    }
    for mode in EXECUTION_MODES:
        for repeat in range(MODE_REPEATS[mode]):
            rows.append(
                {
                    "status": "complete",
                    "execution_mode": mode,
                    "tokens_per_second": speeds[mode] + repeat,
                    "median_gpu_utilization_percent": 99.0,
                    "peak_allocated_gib": 70.0,
                    "peak_reserved_gib": 72.0,
                    "warmup_seconds": 1.0,
                }
            )
    summary = mode_summary(rows)
    assert selected_mode(summary) == "reduce-overhead"
    for row in rows:
        if row["execution_mode"] in {"reduce-overhead", "default"}:
            row["tokens_per_second"] = 102.0
    assert selected_mode(mode_summary(rows)) == "eager"


def test_compiled_batch_probe_selects_throughput_not_largest_batch() -> None:
    rows = []
    speeds = {1024: 80.0, 896: 100.0, 768: 120.0, 640: 110.0}
    for batch in BATCHES:
        for repeat in range(2):
            rows.append(
                {
                    "status": "complete",
                    "batch": batch,
                    "finite_forward_backward_optimizer": True,
                    "tokens_per_second": speeds[batch] + repeat,
                    "median_gpu_utilization_percent": 99.0,
                    "peak_allocated_gib": 70.0,
                    "peak_reserved_gib": 72.0,
                    "warmup_seconds": 1.0,
                }
            )
    summary = summarize_batches(rows)
    assert summary["selected"]["batch"] == 768
    assert summary["selected"]["global_tokens_per_step"] == 196_608


def test_compiled_cells_do_not_collide_with_eager_cells(tmp_path: Path) -> None:
    recipe = Recipe("adamw", 0.003, 0.003)
    eager = _cell_path(tmp_path, CANDIDATE, recipe, 3, 10_000_000)
    compiled = _cell_path(
        tmp_path, CANDIDATE, recipe, 3, 10_000_000, "default"
    )
    assert eager != compiled
    assert "execution-default" in compiled.parts


def test_optimizer_audit_covers_joint_muon_and_refined_adamw_grid() -> None:
    recipes = optimizer_audit_recipes()
    adamw = [recipe for recipe in recipes if recipe.family == "adamw"]
    muon = [recipe for recipe in recipes if recipe.family == "muon"]
    assert len(adamw) == len(ADAMW_LRS)
    assert len(muon) == len(MUON_BODY_LRS) * len(MUON_AUXILIARY_LRS)
    assert {(recipe.body_lr, recipe.auxiliary_lr) for recipe in muon} == {
        (body, auxiliary)
        for body in MUON_BODY_LRS
        for auxiliary in MUON_AUXILIARY_LRS
    }


def test_optimizer_audit_advances_each_family_independently() -> None:
    rows = []
    recipes = [
        Recipe("adamw", 0.003, 0.003),
        Recipe("adamw", 0.006, 0.006),
        Recipe("muon", 0.03, 0.003),
        Recipe("muon", 0.1, 0.01),
    ]
    losses = (1.0, 2.0, 8.0, 9.0)
    for recipe, loss in zip(recipes, losses, strict=True):
        rows.append(
            {
                "model": CANDIDATE,
                "recipe_slug": recipe_slug(recipe),
                "recipe": recipe.__dict__,
                "seed": 7,
                "validation": {"nll": loss},
            }
        )
    summaries = summarize_recipes(rows, model=CANDIDATE, seeds=(7,))
    selected = top_by_family(summaries, 2)
    assert len(selected["adamw"]) == 2
    assert len(selected["muon"]) == 2
    assert {recipe.family for recipe in selected["muon"]} == {"muon"}


def test_optimizer_audit_schedule_expansion_is_deduplicated() -> None:
    base = Recipe("muon", 0.1, 0.003)
    variants = schedule_variants([base, base])
    assert {recipe.schedule for recipe in variants} == {
        "constant",
        "warmup-cosine",
    }
    assert len(variants) == 2


def test_workspace_permutations_change_layout_and_stay_inside_groups() -> None:
    permuted = build_model(CANDIDATE)
    fixed = build_model(NO_WORKSPACE_PERMUTATION)
    layouts = {
        tuple(block.workspace_permutation.tolist()) for block in permuted.blocks
    }
    assert len(layouts) > 1
    identity = tuple(range(16))
    assert {
        tuple(block.workspace_permutation.tolist()) for block in fixed.blocks
    } == {identity}
    for layout in layouts:
        assert sorted(layout) == list(range(16))


def test_controls_are_tightly_total_parameter_matched() -> None:
    candidate = model_inventory(build_model(CANDIDATE))
    assert candidate["token_factor_parameters"] < 50_000
    assert candidate["total_parameters"] == 5_400_896
    for name in STANDARD_CONTROLS:
        control = model_inventory(build_model(name))
        assert abs(control["total_parameters"] / candidate["total_parameters"] - 1.0) <= 0.001


def test_finite_forward_backward_and_every_candidate_parameter_receives_gradient() -> None:
    torch.manual_seed(1404)
    model = build_model(CANDIDATE, **tiny_overrides())
    inputs = torch.randint(0, 19, (2, 12))
    loss = model(inputs).square().mean()
    loss.backward()
    assert torch.isfinite(loss)
    assert all(
        parameter.grad is not None and torch.isfinite(parameter.grad).all()
        for parameter in model.parameters()
    )
