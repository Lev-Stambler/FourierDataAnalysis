from __future__ import annotations

from exp13_wikitext_confirmation import campaign as v1
from exp13_wikitext_confirmation import campaign_v2 as v2
from exp13_wikitext_confirmation.__main__ import campaign_audit
from exp13_wikitext_confirmation.model import MODEL_NAMES
from exp13_wikitext_confirmation.study import Recipe
from exp13_wikitext_confirmation.study import CONFIRMATION_SEEDS
import json
import torch


def test_v2_forces_truthful_eager_training_tasks(monkeypatch, tmp_path) -> None:
    captured = {}

    def fake(*args, **kwargs):
        captured.update(kwargs)
        return {"compiled": kwargs["compiled"]}

    monkeypatch.setattr(v2, "_original_train_task", fake)
    result = v2._eager_train_task(
        "standard-d3-w256",
        Recipe("adamw", 0.001, 0.001),
        3,
        10,
        batch=384,
        output_root=tmp_path,
        data_root=tmp_path,
        compiled=True,
    )
    assert captured["compiled"] is False
    assert result["compiled"] is False


def test_v2_monkeypatch_targets_are_restorable() -> None:
    assert v2._original_preflight is v1.paid_preflight
    assert v2._original_train_task is v1._train_task
    assert v2._original_schema == "exp13-wikitext-confirmation-v1"


def test_gpu_sampler_reports_measured_medians() -> None:
    sampler = v1.GpuSampler(torch.device("cuda:3"))
    sampler.rows = [(90.0, 400.0, 10_000.0), (100.0, 500.0, 20_000.0)]
    assert sampler.summary() == {
        "gpu_samples": 2,
        "median_gpu_utilization_percent": 95.0,
        "median_power_watts": 450.0,
        "peak_nvidia_memory_mib": 20_000.0,
    }


def test_loss_agreement_task_routes_through_worker_dispatch(monkeypatch) -> None:
    monkeypatch.setattr(
        v1,
        "loss_agreement",
        lambda model, data_root, device, batch: {
            "status": "complete",
            "model": model,
            "batch": batch,
        },
    )
    result = v1.execute_task(
        {
            "kind": "loss-agreement",
            "model": "deep-kron-r8",
            "data_root": "/sealed/train",
            "batch": 17,
        },
        torch.device("cpu"),
    )
    assert result == {"status": "complete", "model": "deep-kron-r8", "batch": 17}


def test_campaign_audit_accepts_complete_v2_eager_result(tmp_path) -> None:
    result_path = tmp_path / "result.json"
    result_path.write_text(json.dumps({
        "schema": "exp13-wikitext-confirmation-v2",
        "status": "complete",
        "wandb_url": "https://wandb.ai/test/run",
        "preflight": {
            "status": "pass",
            "gpu_count": 8,
            "gradient_accumulation": 1,
            "common_batch": 384,
            "execution_mode": "eager",
            "minimum_gpu_utilization_percent": 85.0,
            "eight_way_scaling_efficiency": 0.95,
            "minimum_eight_way_scaling_efficiency": 0.80,
            "loss_agreement": {
                name: {
                    "status": "complete",
                    "relative_error": 0.001,
                    "maximum_relative_error": 0.02,
                }
                for name in MODEL_NAMES
            },
        },
        "confirmation_decision": {"status": "fail"},
        "confirmation_evaluations": [
            {"model": model, "seed": seed}
            for model in ("deep-kron-r8", "standard-d6-w192")
            for seed in CONFIRMATION_SEEDS
        ],
        "final_holdout_opened": False,
        "verdict": "not_confirmed",
    }))
    audit = campaign_audit(result_path)
    assert audit["status"] == "pass", audit["failures"]
