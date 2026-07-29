import json
from pathlib import Path
from types import SimpleNamespace

from qwen_kron_distill.coordinator import run_preflight


def test_preflight_sweeps_large_microbatches_and_selects_viable_one(
    monkeypatch,
    tmp_path,
):
    calls = []

    def fake_run(command, *, stdout, stderr, env, check):
        del stdout, stderr, check
        microbatch = int(env["QWEN_KRON_MICROBATCH"])
        world_size = 8 if "torch.distributed.run" in command else 1
        calls.append((microbatch, world_size))
        if microbatch == 8_192 and world_size == 8:
            return SimpleNamespace(returncode=1)
        result_file = command[command.index("--result-file") + 1]
        throughput = 2_000.0 if world_size == 1 else 16_000.0
        value = {
            "schema": "qwen-kron-preflight-worker-v1",
            "status": "complete",
            "world_size": world_size,
            "microbatch": microbatch,
            "global_batch": 8_192 * world_size,
            "global_token_batch": 8_192 * world_size * 16,
            "contexts_per_second": throughput,
            "memory_ratio": 0.5,
            "factor_state_initialized": True,
            "embedding_state_initialized": True,
            "loss_min": 0.1,
            "loss_max": 0.2,
            "dense_optimized_loss_error": 1e-5,
        }
        Path(result_file).write_text(json.dumps(value))
        return SimpleNamespace(returncode=0)

    monkeypatch.setenv(
        "QWEN_KRON_PREFLIGHT_MICROBATCHES",
        "8192,4096",
    )
    monkeypatch.setattr(
        "qwen_kron_distill.coordinator.subprocess.run",
        fake_run,
    )

    result = run_preflight(
        data_root="/unused",
        output_root=str(tmp_path),
    )

    assert calls == [
        (8_192, 1),
        (8_192, 8),
        (4_096, 1),
        (4_096, 8),
    ]
    assert result["accepted"] is True
    assert result["selected_microbatch"] == 4_096
    assert result["throughput_scaling"] == 8.0
    assert result["attempts"][0]["status"] == "eight_h200_failed"
    assert result["attempts"][1]["status"] == "viable"
