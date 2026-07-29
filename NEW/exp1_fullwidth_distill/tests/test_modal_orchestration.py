import copy
import hashlib
import json
from dataclasses import replace
from types import SimpleNamespace

import pytest

import modal_app
from qwen_fullwidth_distill.study import kronecker_edu_lr_trials


def _hydra_plan():
    return modal_app._resolve_hydra_experiment(
        "edu_wsd_large_batch_lr"
    )


def _materialize(plan, cell, source):
    return modal_app._trial_from_hydra_cell(
        cell,
        source,
        resolved_root_config=plan["resolved_config"],
        resolved_config_hash=plan["resolved_config_hash"],
    )


def test_microbatch_candidates_include_nondivisor_tails_and_full_search():
    runtime = _hydra_plan()["cells"][0]["runtime"]
    candidates = modal_app._microbatch_candidates(3_168, runtime)
    assert candidates[:3] == [1_024, 1_023, 1_022]
    assert candidates[-1] == 1
    assert 1_000 in candidates
    assert 3_168 % 1_000 != 0


def test_preflight_request_identity_covers_runtime_graph():
    cell = _hydra_plan()["cells"][0]
    payload = cell["trial"] | {
        "_runtime": cell["runtime"],
        "_kronecker_backend": "cutensor",
        "_kronecker_rank_chunk": 512,
        "_precision": "bfloat16",
    }
    baseline = modal_app._preflight_request_key(payload)
    assert baseline != modal_app._preflight_request_key(
        payload | {"_kronecker_backend": "torch"}
    )
    assert baseline != modal_app._preflight_request_key(
        payload | {"_kronecker_rank_chunk": 256}
    )
    assert baseline != modal_app._preflight_request_key(
        payload | {"_precision": "float32"}
    )
    assert baseline == modal_app._preflight_request_key(
        payload | {"effective_batch": 6_336}
    )
    changed_runtime = {
        **cell["runtime"],
        "microbatch": {
            **cell["runtime"]["microbatch"],
            "maximum": 900,
        },
    }
    assert baseline != modal_app._preflight_request_key(
        payload | {"_runtime": changed_runtime}
    )


def test_cuda_oom_detection_is_narrow():
    assert modal_app._is_cuda_oom(
        RuntimeError("CUDA out of memory. Tried to allocate 1 GiB")
    )
    assert not modal_app._is_cuda_oom(
        RuntimeError("cuTENSOR contraction failed")
    )


def test_hydra_dry_run_materializes_exact_fresh_adam_wave():
    plan = _hydra_plan()
    assert plan["cell_count"] == 8
    assert len(plan["resolved_config_hash"]) == 64
    source = kronecker_edu_lr_trials()[0]
    trials = [
        _materialize(plan, cell, source)
        for cell in plan["cells"]
    ]
    assert len({trial.label for trial in trials}) == 8
    assert {trial.steps * trial.effective_batch for trial in trials} == {
        811_008
    }
    assert {trial.lr_schedule for trial in trials} == {"wsd"}
    assert {trial.cooldown_steps for trial in trials} == {12, 24}
    assert all(trial.warm_start_weights_only for trial in trials)
    assert all(trial.warm_start_resume_step == 0 for trial in trials)
    assert all(trial.warm_start_stage == source.stage for trial in trials)
    assert all(trial.warm_start_label == source.label for trial in trials)
    assert all(trial.warm_start_step == source.steps for trial in trials)
    assert all(trial.use_teacher_cache for trial in trials)
    assert all(not trial.allow_data_reuse for trial in trials)
    assert all(trial.target_validation_kl is None for trial in trials)
    resolved = json.loads(trials[0].hydra_resolved_config_json)
    encoded = json.dumps(
        resolved,
        sort_keys=True,
        separators=(",", ":"),
        ensure_ascii=False,
        allow_nan=False,
    ).encode("utf-8")
    assert hashlib.sha256(encoded).hexdigest() == (
        trials[0].hydra_config_hash
    )
    misleading = copy.deepcopy(plan["cells"][0])
    misleading["runtime"]["precision"] = "float32"
    with pytest.raises(ValueError, match="runtime contract"):
        _materialize(plan, misleading, source)


def test_source_gate_requires_full_validation_and_checkpoint(
    tmp_path,
    monkeypatch,
):
    source = kronecker_edu_lr_trials()[0]
    checkpoint = tmp_path / source.stage / source.label / "student.pt"
    checkpoint.parent.mkdir(parents=True)
    checkpoint.write_bytes(b"checkpoint")
    monkeypatch.setattr(modal_app, "OUTPUT_ROOT", str(tmp_path))
    selector = _hydra_plan()["cells"][0]["warm_start"]["source"]
    result = {
        "label": source.label,
        "status": "complete",
        "steps_completed": source.steps,
        "validation": {"kl": 1.25},
        "dataset_manifest": {
            "split_sizes": {"validation": 8_192}
        },
    }
    modal_app._validate_full_validation_source(
        source, result, selector
    )
    with pytest.raises(RuntimeError, match="full 8,192"):
        modal_app._validate_full_validation_source(
            source,
            {
                **result,
                "dataset_manifest": {
                    "split_sizes": {"validation": 2_048}
                },
            },
            selector,
        )


def test_ranker_accepts_only_exact_cooled_full_validation_endpoints():
    plan = _hydra_plan()
    source = kronecker_edu_lr_trials()[0]
    trials = [
        _materialize(plan, cell, source)
        for cell in plan["cells"]
    ]
    results = [
        {
            "label": trial.label,
            "status": "complete",
            "steps_completed": trial.steps,
            "lr_schedule": "wsd",
            "cooldown_steps": trial.cooldown_steps,
            "hydra_config_hash": trial.hydra_config_hash,
            "validation": {"kl": 1.8 - index / 10},
            "dataset_manifest": {
                "split_sizes": {"validation": 8_192}
            },
            "warm_started": True,
            "warm_start_weights_only": True,
            "optimizer_state_resumed": False,
            "resumed_from_step": 0,
            "optimizer_steps": trial.steps,
            "examples_seen":
                393_216 + trial.steps * trial.effective_batch,
            "input_tokens_seen": (
                393_216 + trial.steps * trial.effective_batch
            ) * trial.architecture.context_length,
        }
        for index, trial in enumerate(trials)
    ]
    ranking = modal_app._rank_cooled_full_validation(trials, results)
    modal_app._audit_fresh_adam_continuations(
        trials,
        results,
        source_examples_seen=393_216,
    )
    assert [row["rank"] for row in ranking] == list(range(1, 9))
    assert ranking[0]["validation_kl"] == pytest.approx(1.1)
    with pytest.raises(RuntimeError, match="not an exact cooled"):
        modal_app._rank_cooled_full_validation(
            trials,
            [
                {**results[0], "steps_completed": trials[0].steps - 1},
                *results[1:],
            ],
        )
    uncooled = replace(trials[0], cooldown_steps=0)
    with pytest.raises(RuntimeError, match="not an exact cooled"):
        modal_app._rank_cooled_full_validation(
            [uncooled, *trials[1:]],
            [{**results[0], "label": uncooled.label}, *results[1:]],
        )
    with pytest.raises(RuntimeError, match="data-cursor audit failed"):
        modal_app._audit_fresh_adam_continuations(
            trials,
            [{**results[0], "optimizer_state_resumed": True}, *results[1:]],
            source_examples_seen=393_216,
        )


def test_detached_dispatch_is_idempotent_across_coordinator_restarts(
    tmp_path,
    monkeypatch,
):
    plan = modal_app._resolve_hydra_experiment(
        "edu_wsd_optimizer_tokens_262m"
    )
    source = kronecker_edu_lr_trials()[0]
    trials = [
        _materialize(plan, cell, source)
        for cell in plan["cells"]
    ]

    class FakeVolume:
        def reload(self):
            return None

        def commit(self):
            return None

    class FakeTrainer:
        def __init__(self):
            self.payloads = []

        def spawn(self, payload):
            self.payloads.append(payload)
            return SimpleNamespace(
                object_id=f"fc-{len(self.payloads):02d}"
            )

    trainer = FakeTrainer()
    monkeypatch.setattr(modal_app, "OUTPUT_ROOT", str(tmp_path))
    monkeypatch.setattr(modal_app, "volume", FakeVolume())
    monkeypatch.setattr(modal_app, "train_trial_remote", trainer)
    kwargs = {
        "root_hash": plan["resolved_config_hash"],
        "config_name": "edu_wsd_optimizer_tokens_262m",
        "data_root": "/data",
        "teacher_cache_root": "/teacher-cache",
        "runtime_by_label": {
            trial.label: {"_runtime": {"accelerator": "H100"}}
            for trial in trials
        },
    }
    first = modal_app._dispatch_detached(trials, **kwargs)
    second = modal_app._dispatch_detached(trials, **kwargs)
    assert first["status"] == second["status"] == "launched"
    assert len(first["calls"]) == len(second["calls"]) == 8
    assert len(trainer.payloads) == 8
    assert all(
        payload["_teacher_cache_root"] == "/teacher-cache"
        for payload in trainer.payloads
    )
