"""Modal H100 runner for the 18-bit LSH Monarch distillation study.

Run from this directory:
    uv run modal run modal_app.py --stage tests
    uv run modal run modal_app.py --stage prepare
    uv run modal run modal_app.py --stage smoke
    uv run modal run modal_app.py --stage screen
    uv run modal run --detach modal_app.py::app.study_remote
    uv run modal run modal_app.py --stage audit
"""

from __future__ import annotations

import json
import os
from pathlib import Path

import modal

app = modal.App("qwen-lsh18-monarch-distill")
volume = modal.Volume.from_name("fda-cache", create_if_missing=True)
DATA_ROOT = "/cache/qwen_fullwidth_distill/context16-data-v2"
SMOKE_DATA_ROOT = "/cache/qwen_fullwidth_distill/context16-smoke-data-v2"
EXPERIMENT_ROOT = "/cache/qwen_lsh18_monarch"
CODEBOOK_PATH = f"{EXPERIMENT_ROOT}/codebook/lsh18.pt"
OUTPUT_ROOT = f"{EXPERIMENT_ROOT}/runs"

image = (
    modal.Image.debian_slim(python_version="3.11")
    .pip_install(
        "numpy>=2.0",
        "torch==2.10.0",
        "transformers==5.13.1",
        "accelerate>=1.14",
        "datasets==5.0.0",
        "safetensors",
        "sentencepiece",
        "wandb>=0.18",
        "pytest>=8.0",
    )
    .pip_install(
        "https://github.com/Dao-AILab/causal-conv1d/releases/download/"
        "v1.6.2.post1/causal_conv1d-1.6.2.post1%2Bcu12torch2.10"
        "cxx11abiTRUE-cp311-cp311-linux_x86_64.whl",
        "flash-linear-attention[cuda]==0.5.1",
        extra_options="--no-build-isolation",
    )
    .env({
        "HF_HOME": "/cache/hf",
        "HF_XET_HIGH_PERFORMANCE": "1",
        "PYTORCH_ALLOC_CONF": "expandable_segments:True",
        "TORCHINDUCTOR_CACHE_DIR": "/cache/torchinductor/qwen_lsh18_monarch",
    })
    .add_local_python_source("qwen_lsh_monarch")
    .add_local_dir("tests", remote_path="/root/tests")
)

try:
    WANDB_SECRET = [modal.Secret.from_name("wandb")]
except Exception:  # noqa: BLE001 - local environments need not define Modal secrets
    WANDB_SECRET = []
try:
    HF_SECRET = [modal.Secret.from_name("hf-token")]
except Exception:  # noqa: BLE001 - local environments need not define Modal secrets
    HF_SECRET = []


def _trial_from_payload(payload: dict):
    from qwen_lsh_monarch.config import ArchitectureConfig, TrialConfig

    value = dict(payload)
    value.pop("_data_root", None)
    architecture = dict(value["architecture"])
    architecture.pop("label", None)
    value["architecture"] = ArchitectureConfig(**architecture)
    value.pop("label", None)
    return TrialConfig(**value)


@app.function(
    image=image,
    volumes={"/cache": volume},
    timeout=86_400,
    memory=32_768,
    secrets=HF_SECRET,
)
def prepare_data_remote(smoke: bool = False):
    from qwen_lsh_monarch.config import MODEL_ID, MODEL_REVISION
    from qwen_lsh_monarch.data import prepare_dataset
    from transformers import AutoTokenizer

    volume.reload()
    tokenizer = AutoTokenizer.from_pretrained(
        MODEL_ID, revision=MODEL_REVISION
    )
    root = SMOKE_DATA_ROOT if smoke else DATA_ROOT
    sizes = {"train": 2_048, "validation": 128, "test": 128} if smoke else None
    result = prepare_dataset(root, tokenizer, split_sizes=sizes)
    volume.commit()
    print(json.dumps(result, sort_keys=True), flush=True)
    return result


@app.function(
    image=image,
    volumes={"/cache": volume},
    timeout=7_200,
    cpu=8.0,
    memory=32_768,
    secrets=HF_SECRET,
)
def prepare_codebook_remote():
    from qwen_lsh_monarch.train import prepare_codebook

    volume.reload()
    result = prepare_codebook(CODEBOOK_PATH)
    volume.commit()
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)
    return result


@app.function(image=image, timeout=3_600, memory=32_768)
def tests_remote():
    import subprocess

    environment = dict(os.environ)
    environment["PYTHONPATH"] = "/root"
    result = subprocess.run(
        ["python", "-m", "pytest", "-q", "/root/tests"],
        env=environment,
        text=True,
        capture_output=True,
        check=False,
    )
    print(result.stdout, flush=True)
    if result.stderr:
        print(result.stderr, flush=True)
    if result.returncode:
        raise RuntimeError(f"remote tests failed with code {result.returncode}")
    return result.stdout


@app.function(
    image=image,
    volumes={"/cache": volume},
    timeout=3_600,
    memory=32_768,
    secrets=WANDB_SECRET,
)
def audit_remote():
    from qwen_lsh_monarch.audit import audit_study, audit_wandb

    volume.reload()
    result = audit_study(OUTPUT_ROOT, CODEBOOK_PATH)
    result["wandb"] = audit_wandb(OUTPUT_ROOT)
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)
    return result


@app.function(
    image=image,
    gpu="H100",
    volumes={"/cache": volume},
    timeout=86_400,
    memory=65_536,
    secrets=HF_SECRET + WANDB_SECRET,
    max_containers=8,
    retries=3,
)
def train_trial_remote(payload: dict):
    from qwen_lsh_monarch.train import run_trial

    volume.reload()
    data_root = payload.get("_data_root", DATA_ROOT)
    trial = _trial_from_payload(payload)
    result = run_trial(
        trial,
        data_root=data_root,
        codebook_path=CODEBOOK_PATH,
        output_root=OUTPUT_ROOT,
        device="cuda",
        checkpoint_callback=volume.commit,
    )
    volume.commit()
    return result


def _dispatch(
    trials,
    data_root: str = DATA_ROOT,
    *,
    reuse_committed: bool = False,
):
    all_trials = list(trials)
    pending = all_trials
    cached = []
    if reuse_committed:
        from qwen_lsh_monarch.codebook import load_codebook_artifact
        from qwen_lsh_monarch.train import _load_completed_result

        volume.reload()
        _, metadata = load_codebook_artifact(CODEBOOK_PATH)
        pending = []
        for trial in all_trials:
            output = Path(OUTPUT_ROOT) / trial.stage / trial.label
            result = _load_completed_result(
                output, trial, metadata["codebook_sha256"]
            )
            if result is None:
                pending.append(trial)
            else:
                cached.append(result)
        print(
            f"[cache-barrier] reused={len(cached)} pending={len(pending)}",
            flush=True,
        )
    if not pending:
        return cached
    payloads = [
        trial.to_dict() | {"_data_root": data_root} for trial in pending
    ]
    return cached + list(
        train_trial_remote.map(payloads, order_outputs=False)
    )


def _write_json_artifact(name: str, value: dict) -> None:
    path = Path(OUTPUT_ROOT) / name
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_text(json.dumps(value, indent=2, sort_keys=True))
    os.replace(temporary, path)


def _run_study():
    from qwen_lsh_monarch.study import (
        final_trials,
        screen_trials,
        select_lr,
        select_topology,
        tuning_trials,
    )

    prepare_data_remote.remote()
    prepare_codebook_remote.remote()
    screens = _dispatch(screen_trials(), reuse_committed=True)
    selected = select_topology(screens)
    print(f"[selection] topology={selected.label}", flush=True)
    tuning = _dispatch(tuning_trials(selected), reuse_committed=True)
    selected_lr = select_lr(selected, screens + tuning)
    finals_grid = final_trials(screens, tuning)
    volume.reload()
    _write_json_artifact(
        "study-plan.json",
        {
            "selection_basis": "validation_only",
            "selected_topology": selected.to_dict(),
            "selected_lr": selected_lr,
            "final_trials": [trial.to_dict() for trial in finals_grid],
        },
    )
    volume.commit()
    finals = _dispatch(finals_grid, reuse_committed=True)
    summary = {"screen": screens, "tune": tuning, "final": finals}
    volume.reload()
    _write_json_artifact("study-summary.json", summary)
    volume.commit()
    print(json.dumps(summary, indent=2), flush=True)
    return summary


@app.function(
    image=image,
    volumes={"/cache": volume},
    timeout=86_400,
    memory=8_192,
)
def study_remote():
    volume.reload()
    return _run_study()


@app.local_entrypoint()
def main(stage: str = "tests"):
    from qwen_lsh_monarch.config import ArchitectureConfig, TrialConfig
    from qwen_lsh_monarch.study import screen_trials

    if stage == "tests":
        print(tests_remote.remote())
        return
    if stage == "audit":
        print(json.dumps(audit_remote.remote(), indent=2, sort_keys=True))
        return
    if stage == "prepare":
        print(prepare_data_remote.remote())
        print(prepare_codebook_remote.remote())
        return
    if stage == "smoke":
        prepare_data_remote.remote(True)
        prepare_codebook_remote.remote()
        trials = [
            TrialConfig(
                ArchitectureConfig("sequential", 1),
                steps=2,
                stage="smoke",
                audit_every=1,
                smoke=True,
            ),
            TrialConfig(
                ArchitectureConfig("residual_ffn", 1, 4),
                steps=2,
                stage="smoke",
                audit_every=1,
                smoke=True,
            ),
        ]
        print(json.dumps(_dispatch(trials, SMOKE_DATA_ROOT), indent=2))
        return
    if stage == "screen":
        prepare_data_remote.remote()
        prepare_codebook_remote.remote()
        print(json.dumps(_dispatch(screen_trials()), indent=2))
        return
    if stage != "study":
        raise ValueError(
            "stage must be tests, prepare, smoke, screen, study, or audit"
        )
    print(json.dumps(study_remote.remote(), indent=2))
