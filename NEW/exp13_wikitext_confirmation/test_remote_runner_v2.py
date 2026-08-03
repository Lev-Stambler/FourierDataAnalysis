from __future__ import annotations

from pathlib import Path

import pytest

from exp13_wikitext_confirmation import remote_runner_v2 as remote


def test_target_is_exact_and_rejects_drift(monkeypatch) -> None:
    monkeypatch.delenv("RC_NF_PROJECT", raising=False)
    monkeypatch.delenv("RC_NF_SERVICE", raising=False)
    assert remote._target_args() == [
        "--projectId",
        "fda-race-us-central",
        "--serviceId",
        "gpu-h100-8",
    ]
    monkeypatch.setenv("RC_NF_SERVICE", "some-other-service")
    with pytest.raises(RuntimeError, match="pinned"):
        remote._target_args()


def test_remote_command_runs_all_locked_cloud_stages() -> None:
    command = remote._remote_command("/cache/source", "/cache/state", "/root/env.sh")
    assert "/root/fda/NEW/.venv/bin/python" in command
    assert "test -s /cache/exp10/data/wikitext/train.npy" in command
    assert "--stage=local-audit" in command
    assert "--stage=prepare-holdout" in command
    assert "--stage=wikitext-confirmation-v2" in command
    assert "exp13_wikitext_confirmation audit" in command
    assert "research_control audit" in command


def test_source_digest_verifies_manifest_hashes() -> None:
    root = Path(__file__).resolve().parents[1]
    assert len(remote._source_digest(root)) == 64
