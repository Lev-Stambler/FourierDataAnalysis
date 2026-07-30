from types import SimpleNamespace

import pytest

import gpu_finder
from gpu_finder import Candidate, managed_candidates, race, service_payload


def regions():
    return {
        "regions": [
            {
                "id": "z-region",
                "gpuDevices": [
                    {
                        "id": "h100-80",
                        "countOptions": [1, 8],
                        "pricing": {"onDemand": 274},
                    },
                    {"id": "a100-80", "countOptions": [8]},
                ],
            },
            {
                "id": "a-region",
                "gpuDevices": [
                    {
                        "id": "h200-141",
                        "countOptions": [8],
                        "pricing": {"onDemand": 314},
                    }
                ],
            },
        ]
    }


def test_inventory_only_returns_requested_hopper_gpus():
    values = managed_candidates(regions(), count=8, project_prefix="race")
    assert [(value.region, value.gpu_type) for value in values] == [
        ("a-region", "h200-141"),
        ("z-region", "h100-80"),
    ]


def test_service_has_exactly_eight_requested_gpus():
    candidate = managed_candidates(regions(), count=8, project_prefix="race")[0]
    payload = service_payload(candidate)
    assert payload["deployment"]["gpu"]["configuration"] == {
        "gpuType": "h200-141",
        "gpuCount": 8,
    }
    assert payload["billing"]["deploymentPlan"] == "nf-gpu-h200-141-8g"


def test_region_filter_is_exact():
    values = managed_candidates(
        regions(),
        count=8,
        project_prefix="race",
        allowed_regions={"z-region"},
    )
    assert [value.region for value in values] == ["z-region"]


def test_timeout_pauses_every_candidate(monkeypatch, tmp_path):
    candidates = [
        Candidate("a", "h200-141", 8, 314, "pa", "sa"),
        Candidate("b", "h100-80", 8, 274, "pb", "sb"),
    ]
    paused = []
    monkeypatch.setattr(gpu_finder, "ensure_service", lambda candidate, team: None)
    monkeypatch.setattr(
        gpu_finder, "pause", lambda candidate, team: paused.append(candidate)
    )
    args = SimpleNamespace(
        team="team",
        timeout_minutes=0,
        poll_seconds=0,
        winner_file=str(tmp_path / "winner.json"),
    )
    with pytest.raises(RuntimeError, match="no H100/H200"):
        race(args, candidates)
    assert paused == candidates


def test_race_polls_one_candidate_per_tick_and_keeps_one_winner(
    monkeypatch, tmp_path
):
    candidates = [
        Candidate("a", "h200-141", 8, 314, "pa", "sa"),
        Candidate("b", "h100-80", 8, 274, "pb", "sb"),
    ]
    polled = []
    paused = []
    monkeypatch.setattr(gpu_finder, "ensure_service", lambda candidate, team: None)
    monkeypatch.setattr(
        gpu_finder,
        "status",
        lambda candidate, team: (
            polled.append(candidate) or
            ("TASK_RUNNING" if candidate == candidates[1] else "TASK_STAGING")
        ),
    )
    monkeypatch.setattr(
        gpu_finder, "pause", lambda candidate, team: paused.append(candidate)
    )
    winner_file = tmp_path / "winner.json"
    args = SimpleNamespace(
        team="team",
        timeout_minutes=1,
        poll_seconds=0,
        winner_file=str(winner_file),
    )
    race(args, candidates)
    assert polled == candidates
    assert paused == [candidates[0]]
    assert winner_file.is_file()
