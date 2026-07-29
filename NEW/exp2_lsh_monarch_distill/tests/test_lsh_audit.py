import json

import torch
from qwen_lsh_monarch.audit import _wandb_config_matches, audit_study
from qwen_lsh_monarch.config import LSH_BITS, MODEL_ID, MODEL_REVISION
from qwen_lsh_monarch.study import (
    final_trials,
    screen_trials,
    select_lr,
    select_topology,
    tuning_trials,
)


def result(trial, kl):
    code_hash = "b" * 64
    return {
        "label": trial.label,
        "architecture": trial.architecture.to_dict(),
        "lr": trial.lr,
        "seed": trial.seed,
        "steps": trial.steps,
        "trainable_parameters": 1,
        "frozen_parameters": 18,
        "initial_validation": {"kl": kl + 1},
        "validation": {"kl": kl},
        "test": {"kl": kl + 0.1} if trial.stage == "final" else {},
        "embedding_sha256": "a" * 64,
        "codebook_sha256": code_hash,
        "codebook_report": {
            "bits": LSH_BITS,
            "codebook_sha256": code_hash,
        },
    }


def write_result(root, trial, value):
    output = root / trial.stage / trial.label
    output.mkdir(parents=True)
    (output / "result.json").write_text(json.dumps(value))
    return output


def test_auditor_checks_complete_grid_plan_summary_and_checkpoints(tmp_path):
    screens = []
    for index, trial in enumerate(screen_trials()):
        value = result(trial, 1.0 if index == 3 else 10.0)
        write_result(tmp_path, trial, value)
        screens.append(value)
    selected = select_topology(screens)
    tuning = []
    for trial in tuning_trials(selected):
        value = result(trial, 0.5 if trial.lr == 3e-4 else 2.0)
        write_result(tmp_path, trial, value)
        tuning.append(value)
    finals = []
    final_grid = final_trials(screens, tuning)
    for trial in final_grid:
        value = result(trial, 0.3 + 0.01 * trial.seed)
        output = write_result(tmp_path, trial, value)
        torch.save(
            {
                "schema": "qwen-lsh18-monarch-student-v1",
                "state_dict": {"weight": torch.zeros(1)},
                "trial": trial.to_dict(),
                "model_id": MODEL_ID,
                "model_revision": MODEL_REVISION,
                "embedding_sha256": value["embedding_sha256"],
                "codebook_sha256": value["codebook_sha256"],
                "metrics": value,
            },
            output / "student.pt",
        )
        finals.append(value)
    plan = {
        "selection_basis": "validation_only",
        "selected_topology": selected.to_dict(),
        "selected_lr": select_lr(selected, screens + tuning),
        "final_trials": [trial.to_dict() for trial in final_grid],
    }
    (tmp_path / "study-plan.json").write_text(json.dumps(plan))
    (tmp_path / "study-summary.json").write_text(json.dumps({
        "screen": screens,
        "tune": tuning,
        "final": finals,
    }))
    audited = audit_study(tmp_path)
    assert audited["status"] == "complete"
    assert audited["screen_trials"] == 16
    assert audited["tune_trials"] == 2
    assert audited["final_trials"] == 3
    assert audited["test"][selected.label]["seeds"] == 3


def test_wandb_match_requires_representation_and_hashes():
    trial = screen_trials()[0]
    value = result(trial, 1.0)
    config = trial.to_dict() | {
        "objective": "full_vocab_forward_kl",
        "representation": "signed_repaired_lsh18",
        "effective_batch": 1_024,
        "embedding_sha256": value["embedding_sha256"],
        "codebook_sha256": value["codebook_sha256"],
    }
    assert _wandb_config_matches(config, value, "screen")
    assert not _wandb_config_matches(
        config | {"representation": "raw_lsh18"}, value, "screen"
    )
