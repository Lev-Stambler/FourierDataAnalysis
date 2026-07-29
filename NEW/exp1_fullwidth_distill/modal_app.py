"""Modal H100 runner for the context-16 full-width Qwen distillation study.

Examples:
    uv run modal run modal_app.py --stage tests
    uv run modal run modal_app.py --stage prepare
    uv run modal run modal_app.py --stage smoke
    uv run modal run modal_app.py --stage tensor-smoke
    uv run modal run modal_app.py --stage kron-rank-smoke
    uv run modal run --detach modal_app.py --stage kron-rank
    uv run modal run modal_app.py --stage screen
    uv run modal run modal_app.py --stage audit
    uv run modal run --detach modal_app.py --stage study
    uv run modal run --detach modal_app.py --stage depth
    uv run modal run --detach modal_app.py --stage tensor
"""

from __future__ import annotations

import hashlib
import json
import math
import os
from pathlib import Path

import modal
import torch  # Load fully during container startup, never inside cancellable input.


app = modal.App("qwen-fullwidth-monarch-distill")
volume = modal.Volume.from_name("fda-cache", create_if_missing=True)
DATA_ROOT = "/cache/qwen_fullwidth_distill/context16-data-v2"
SMOKE_DATA_ROOT = "/cache/qwen_fullwidth_distill/context16-smoke-data-v2"
EDU_DATA_ROOT = (
    "/cache/qwen_fullwidth_distill/context16-fineweb-edu-350bt-v1p4"
)
EDU_TEACHER_CACHE_ROOT = (
    "/cache/qwen_fullwidth_distill/"
    "context16-fineweb-edu-350bt-v1p4-teacher-hidden-v1"
)
OUTPUT_ROOT = "/cache/qwen_fullwidth_distill/runs"
PREFLIGHT_SCHEMA = "qwen-fullwidth-h100-microbatch-preflight-v1"
PREFLIGHT_TARGET_VRAM_RATIO = 0.88
PREFLIGHT_HARD_VRAM_RATIO = 0.92
_TEACHER_CACHE_WORKER_MODEL = None
_TEACHER_CACHE_WORKER_EMBEDDING_SHA256 = None

image = (
    modal.Image.debian_slim(python_version="3.11")
    .pip_install(
        "numpy>=2.0",
        "torch==2.10.0",
        "transformers==5.13.1",
        "accelerate>=1.14",
        "datasets==5.0.0",
        "hydra-core~=1.3",
        "modal>=1.5.1",
        "safetensors",
        "sentencepiece",
        "wandb>=0.18",
        "pytest>=8.0",
        "nvmath-python[cu12]>=0.9",
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
        "TORCHINDUCTOR_CACHE_DIR": "/cache/torchinductor/qwen_fullwidth16",
    })
    .add_local_python_source("qwen_fullwidth_distill")
    .add_local_dir(
        "qwen_fullwidth_distill/conf",
        remote_path="/root/qwen_fullwidth_distill/conf",
    )
    .add_local_dir("tests", remote_path="/root/tests")
)

try:
    WANDB_SECRET = [modal.Secret.from_name("wandb")]
except Exception:
    WANDB_SECRET = []
try:
    HF_SECRET = [modal.Secret.from_name("hf-token")]
except Exception:
    HF_SECRET = []


def _trial_from_payload(payload: dict):
    from qwen_fullwidth_distill.config import ArchitectureConfig, TrialConfig

    value = dict(payload)
    value.pop("_data_root", None)
    value.pop("_kronecker_backend", None)
    value.pop("_kronecker_rank_chunk", None)
    value.pop("_kronecker_microbatch", None)
    value.pop("_teacher_cache_root", None)
    value.pop("_teacher_cache_manifest_sha256", None)
    value.pop("_precision", None)
    value.pop("_runtime", None)
    value.pop("_hydra", None)
    architecture = dict(value["architecture"])
    architecture.pop("label", None)
    value["architecture"] = ArchitectureConfig(**architecture)
    value.pop("label", None)
    return TrialConfig(**value)


def _canonical_payload_hash(value: dict) -> str:
    encoded = json.dumps(
        value,
        sort_keys=True,
        separators=(",", ":"),
    ).encode("utf-8")
    return hashlib.sha256(encoded).hexdigest()


def _microbatch_candidates(
    effective_batch: int,
    runtime: dict,
) -> list[int]:
    """Return descending physical-batch candidates, including tail batches."""
    if effective_batch <= 0:
        raise ValueError("effective batch must be positive")
    microbatch = runtime.get("microbatch", {})
    if not isinstance(microbatch, dict):
        raise ValueError("runtime.microbatch must be a mapping")
    minimum = int(microbatch.get("minimum", 1))
    maximum = int(microbatch.get("maximum", effective_batch))
    if minimum <= 0 or maximum < minimum:
        raise ValueError("invalid microbatch minimum/maximum")
    declared = microbatch.get("candidates")
    if declared is None:
        candidates = list(
            range(min(effective_batch, maximum), minimum - 1, -1)
        )
    else:
        if not isinstance(declared, (list, tuple)) or not declared:
            raise ValueError("microbatch candidates must be a non-empty list")
        candidates = sorted(
            {
                int(value)
                for value in declared
                if (
                    minimum <= int(value) <= min(effective_batch, maximum)
                )
            },
            reverse=True,
        )
    if not candidates:
        raise ValueError("runtime has no valid microbatch candidate")
    return candidates


def _is_cuda_oom(error: BaseException) -> bool:
    name = type(error).__name__.lower()
    message = str(error).lower()
    return (
        "outofmemory" in name
        or "out of memory" in message
        or "cuda error: memory allocation" in message
    )


def _preflight_key(payload: dict, runtime_fingerprint: dict) -> str:
    architecture = dict(payload["architecture"])
    architecture.pop("label", None)
    identity = {
        "schema": PREFLIGHT_SCHEMA,
        "architecture": architecture,
        "backend": payload["_kronecker_backend"],
        "rank_chunk": int(payload["_kronecker_rank_chunk"]),
        "precision": payload["_precision"],
        "effective_batch": int(payload["effective_batch"]),
        "teacher_cache_root": payload.get("_teacher_cache_root"),
        "teacher_cache_manifest_sha256":
            payload.get("_teacher_cache_manifest_sha256"),
        "runtime_policy": payload["_runtime"],
        "runtime_fingerprint": runtime_fingerprint,
    }
    return _canonical_payload_hash(identity)


def _preflight_request_key(payload: dict) -> str:
    """Deduplicate cells whose peak H100 allocation graph is identical.

    Effective batch size only changes how many physical microbatches repeat
    before the optimizer step.  It does not change the peak allocation for a
    fixed physical microbatch, model, optimizer, precision, and runtime.
    """
    architecture = dict(payload["architecture"])
    architecture.pop("label", None)
    return _canonical_payload_hash({
        "architecture": architecture,
        "backend": payload["_kronecker_backend"],
        "rank_chunk": int(payload["_kronecker_rank_chunk"]),
        "precision": payload["_precision"],
        "teacher_cache_root": payload.get("_teacher_cache_root"),
        "teacher_cache_manifest_sha256":
            payload.get("_teacher_cache_manifest_sha256"),
        "runtime": payload["_runtime"],
    })


def _resolve_hydra_experiment(
    config_name: str,
    overrides: list[str] | tuple[str, ...] | None = None,
) -> dict:
    from qwen_fullwidth_distill.hydra_config import dry_run_experiment

    plan = dry_run_experiment(config_name, overrides)
    cells = plan.get("cells")
    if (
        plan.get("cell_count") != 8
        or not isinstance(cells, list)
        or len(cells) != 8
        or [cell.get("cell_index") for cell in cells] != list(range(8))
        or len({cell.get("config_hash") for cell in cells}) != 8
    ):
        raise RuntimeError("Hydra replacement stage must resolve exactly 8 cells")
    if any(
        cell.get("schema") != "qwen-fullwidth-hydra-cell-v1"
        for cell in cells
    ):
        raise RuntimeError("Hydra cell schema mismatch")
    return plan


def _trial_from_hydra_cell(
    cell: dict,
    source,
    *,
    resolved_root_config: dict,
    resolved_config_hash: str,
):
    """Materialize a gated Hydra cell as a fresh-Adam continuation."""
    from qwen_fullwidth_distill.config import (
        FINEWEB_EDU_CONFIG,
        FINEWEB_EDU_ID,
        FINEWEB_EDU_REVISION,
        ArchitectureConfig,
        TrialConfig,
    )

    if cell.get("schema") != "qwen-fullwidth-hydra-cell-v1":
        raise ValueError("invalid Hydra cell schema")
    warm_start = cell.get("warm_start", {})
    source_selector = warm_start.get("source", {})
    if (
        warm_start.get("mode") != "weights_only"
        or warm_start.get("optimizer") != "fresh_adam"
    ):
        raise ValueError("Hydra cell must request weights-only fresh Adam")
    if (
        source.stage != source_selector.get("stage")
        or source.dataset_tag != source_selector.get("dataset_tag")
        or source.architecture.label
        != source_selector.get("architecture_label")
    ):
        raise ValueError("selected source does not satisfy Hydra source selector")
    data = cell.get("data", {})
    optimizer = cell.get("optimizer", {})
    schedule = cell.get("schedule", {})
    objective = cell.get("objective", {})
    if (
        data.get("data_root") != EDU_DATA_ROOT
        or data.get("teacher_cache_root") != EDU_TEACHER_CACHE_ROOT
        or data.get("dataset_id") != FINEWEB_EDU_ID
        or data.get("dataset_config") != FINEWEB_EDU_CONFIG
        or data.get("dataset_revision") != FINEWEB_EDU_REVISION
        or data.get("dataset_tag") != "fwedu350bt-v1p4"
        or int(data.get("context_length", -1)) != 16
        or data.get("document_disjoint") is not True
        or data.get("allow_data_reuse") is not False
    ):
        raise ValueError("Hydra cell data/cache identity mismatch")
    if (
        optimizer.get("name") != "adamw"
        or list(optimizer.get("betas", ())) != [0.9, 0.999]
        or float(optimizer.get("eps", -1.0)) != 1e-8
        or float(optimizer.get("weight_decay", -1.0)) != 0.01
        or optimizer.get("fused") is not True
        or optimizer.get("parameterization") != "mup"
    ):
        raise ValueError("Hydra cell must use standard AdamW hyperparameters")
    if (
        schedule.get("name") != "wsd"
        or schedule.get("unit") != "optimizer_steps"
        or float(schedule.get("warmup_start_lr_ratio", -1.0)) != 0.0
        or schedule.get("decay_shape") != "linear"
        or float(schedule.get("decay_end_lr_ratio", -1.0)) != 0.0
    ):
        raise ValueError("Hydra cell must use linear cooldown to zero")
    runtime = cell.get("runtime", {})
    microbatch = runtime.get("microbatch", {})
    if (
        runtime.get("platform") != "modal"
        or runtime.get("accelerator") != "H100"
        or int(runtime.get("gpu_count", -1)) != 1
        or int(runtime.get("max_parallel", -1)) != 8
        or runtime.get("precision") != "bfloat16"
        or runtime.get("matmul_tf32") is not True
        or runtime.get("kronecker_backend") != "cutensor"
        or int(runtime.get("rank_chunk", 0)) <= 0
        or microbatch.get("mode") != "auto"
        or microbatch.get("require_effective_batch_divisor") is not False
        or int(microbatch.get("maximum", 0)) < 1_024
        or float(microbatch.get("target_memory_fraction", -1.0)) != 0.88
        or float(microbatch.get("hard_memory_fraction", -1.0)) != 0.92
        or microbatch.get("oom_backoff") is not True
    ):
        raise ValueError("Hydra cell runtime contract mismatch")
    if (
        objective.get("name") != "forward_kl"
        or float(objective.get("temperature", -1.0)) != 1.0
        or float(objective.get("temperature2_weight", -1.0)) != 0.0
        or float(objective.get("hidden_mse_weight", -1.0)) != 0.0
        or objective.get("use_teacher_cache") is not True
    ):
        raise ValueError("Hydra cell must preserve the teacher cache")

    trial_value = dict(cell["trial"])
    architecture = dict(trial_value.pop("architecture"))
    architecture.pop("label", None)
    # A <=1 KL value is the study goal, not an early-stop policy. Every cell
    # must traverse the complete WSD cooldown before endpoint ranking.
    trial_value["target_validation_kl"] = None
    trial_value.update({
        "architecture": ArchitectureConfig(**architecture),
        "warm_start_stage": source.stage,
        "warm_start_label": source.label,
        "warm_start_step": source.steps,
        "warm_start_resume_step": 0,
        "warm_start_lr_override": source.lr != float(trial_value["lr"]),
        "warm_start_weights_only": True,
        "warm_start_from_stable": False,
        "optimizer_role_lr_multipliers": tuple(
            sorted(optimizer.get("role_lr_multipliers", {}).items())
        ),
        "optimizer_role_weight_decays": tuple(
            sorted(optimizer.get("role_weight_decays", {}).items())
        ),
        "hydra_config_hash": (
            resolved_config_hash
        ),
        "hydra_resolved_config_json": json.dumps(
            resolved_root_config,
            sort_keys=True,
            separators=(",", ":"),
            ensure_ascii=False,
            allow_nan=False,
        ),
    })
    trial = TrialConfig(**trial_value)
    if (
        trial.architecture.label != source.architecture.label
        or trial.lr_schedule != "wsd"
        or trial.warmup_steps <= 0
        or trial.cooldown_steps <= 0
        or trial.min_lr_ratio != 0.0
        or not trial.warm_start_weights_only
        or trial.warm_start_resume_step
        or not trial.use_teacher_cache
        or trial.allow_data_reuse
        or trial.steps * trial.effective_batch != int(cell["fresh_contexts"])
    ):
        raise RuntimeError("materialized Hydra trial violated stage invariants")
    return trial


def _teacher_cache_worker_model():
    """Load the pinned teacher once per reused Modal shard container."""
    global _TEACHER_CACHE_WORKER_MODEL
    global _TEACHER_CACHE_WORKER_EMBEDDING_SHA256

    if _TEACHER_CACHE_WORKER_MODEL is None:
        from qwen_fullwidth_distill.train import (
            load_teacher,
            tensor_sha256,
        )

        teacher, _ = load_teacher("cuda")
        _TEACHER_CACHE_WORKER_MODEL = teacher
        _TEACHER_CACHE_WORKER_EMBEDDING_SHA256 = tensor_sha256(
            teacher.get_input_embeddings().weight
        )
    return (
        _TEACHER_CACHE_WORKER_MODEL,
        _TEACHER_CACHE_WORKER_EMBEDDING_SHA256,
    )


@app.function(
    image=image,
    gpu="H100",
    volumes={"/cache": volume},
    timeout=3_600,
    memory=32_768,
    secrets=HF_SECRET,
)
def teacher_cache_identity_remote():
    """Resolve the exact pinned teacher identity before a resumable build."""
    from qwen_fullwidth_distill.config import (
        CONTEXT_LENGTH,
        MODEL_ID,
        MODEL_REVISION,
    )
    from qwen_fullwidth_distill.data import load_manifest
    from qwen_fullwidth_distill.teacher_cache import make_cache_identity

    volume.reload()
    dataset_manifest = load_manifest(EDU_DATA_ROOT)
    teacher, embedding_hash = _teacher_cache_worker_model()
    return make_cache_identity(
        model_id=MODEL_ID,
        model_revision=MODEL_REVISION,
        embedding_sha256=embedding_hash,
        dataset_manifest=dataset_manifest,
        context_length=CONTEXT_LENGTH,
    )


@app.function(
    image=image,
    gpu="H100",
    volumes={"/cache": volume},
    timeout=86_400,
    memory=65_536,
    secrets=HF_SECRET,
)
def prepare_data_remote(smoke: bool = False, edu: bool = False):
    from transformers import AutoTokenizer

    from qwen_fullwidth_distill.config import (
        FINEWEB_EDU_CONFIG,
        FINEWEB_EDU_ID,
        FINEWEB_EDU_REVISION,
        MODEL_ID,
        MODEL_REVISION,
    )
    from qwen_fullwidth_distill.data import prepare_dataset

    volume.reload()
    tokenizer = AutoTokenizer.from_pretrained(
        MODEL_ID, revision=MODEL_REVISION
    )
    if smoke and edu:
        raise ValueError("smoke and FineWeb-Edu caches are separate modes")
    root = EDU_DATA_ROOT if edu else (SMOKE_DATA_ROOT if smoke else DATA_ROOT)
    sizes = (
        {"train": 2_048, "validation": 128, "test": 128}
        if smoke
        else (
            {"train": 16_777_216, "validation": 8_192, "test": 8_192}
            if edu
            else None
        )
    )
    dataset = (
        {
            "dataset_id": FINEWEB_EDU_ID,
            "dataset_config": FINEWEB_EDU_CONFIG,
            "dataset_revision": FINEWEB_EDU_REVISION,
        }
        if edu
        else {}
    )
    result = prepare_dataset(
        root,
        tokenizer,
        split_sizes=sizes,
        **dataset,
    )
    volume.commit()
    print(json.dumps(result, sort_keys=True), flush=True)
    return result


@app.function(
    image=image,
    gpu="H100",
    volumes={"/cache": volume},
    timeout=86_400,
    memory=65_536,
    secrets=HF_SECRET,
    max_containers=8,
    retries=2,
)
def prepare_teacher_cache_shard_remote(payload: dict):
    import numpy as np
    import torch

    from qwen_fullwidth_distill.data import load_split
    from qwen_fullwidth_distill.teacher_cache import (
        load_hidden_state_shard_record,
        make_cache_identity,
        write_hidden_state_shard,
    )
    from qwen_fullwidth_distill.data import load_manifest

    volume.reload()
    split = str(payload["split"])
    row_start = int(payload["row_start"])
    rows = int(payload["rows"])
    shard_index = int(payload["shard_index"])
    identity = dict(payload["identity"])
    try:
        existing = load_hidden_state_shard_record(
            EDU_TEACHER_CACHE_ROOT,
            split,
            shard_index,
            expected_identity=identity,
            expected_row_start=row_start,
            expected_rows=rows,
            validate_checksum=True,
        )
    except RuntimeError as error:
        # A stale/corrupt same-name shard is never a cache hit.  Recompute its
        # exact target and atomically replace it below.
        print(
            f"[teacher-cache-stale] {split}/{shard_index}: {error}",
            flush=True,
        )
        existing = None
    if existing is not None:
        return {
            "split": split,
            "embedding_sha256": identity["embedding_sha256"],
            "record": existing,
            "cache_hit": True,
        }
    contexts, _, _ = load_split(EDU_DATA_ROOT, split)
    if row_start < 0 or rows <= 0 or row_start + rows > len(contexts):
        raise ValueError("invalid teacher-cache shard range")
    teacher, embedding_hash = _teacher_cache_worker_model()
    current_identity = make_cache_identity(
        model_id=identity["model_id"],
        model_revision=identity["model_revision"],
        embedding_sha256=embedding_hash,
        dataset_manifest=load_manifest(EDU_DATA_ROOT),
        context_length=identity["context_length"],
    )
    if current_identity != identity:
        raise RuntimeError(
            "teacher-cache worker identity changed after dispatch"
        )
    hidden = torch.empty(
        rows,
        1_024,
        dtype=torch.bfloat16,
        device="cpu",
    )
    batch_size = 512
    with torch.inference_mode():
        for offset in range(0, rows, batch_size):
            stop = min(offset + batch_size, rows)
            indices = slice(row_start + offset, row_start + stop)
            token_ids = torch.from_numpy(
                np.array(contexts[indices], copy=True)
            ).to(device="cuda", dtype=torch.long)
            output = teacher.model(
                input_ids=token_ids,
                use_cache=False,
                return_dict=True,
            )
            hidden[offset:stop].copy_(
                output.last_hidden_state[:, -1, :].cpu()
            )
    record = write_hidden_state_shard(
        EDU_TEACHER_CACHE_ROOT,
        split,
        shard_index,
        row_start,
        hidden,
        cache_identity=identity,
        replace_existing=True,
    )
    volume.commit()
    return {
        "split": split,
        "embedding_sha256": embedding_hash,
        "record": record,
        "cache_hit": False,
    }


@app.function(
    image=image,
    volumes={"/cache": volume},
    timeout=86_400,
    memory=8_192,
)
def prepare_teacher_cache_remote():
    from qwen_fullwidth_distill.data import load_manifest
    from qwen_fullwidth_distill.teacher_cache import (
        finalize_teacher_cache,
        load_teacher_cache_manifest,
    )

    prepare_data_remote.remote(edu=True)
    volume.reload()
    dataset_manifest = load_manifest(EDU_DATA_ROOT)
    split_sizes = {
        split: int(rows)
        for split, rows in dataset_manifest["split_sizes"].items()
    }
    # Resolve the embedding hash from the same pinned teacher loader used by
    # training.  Do this even for a completed cache so preparation never
    # certifies a manifest by echoing its own claimed embedding hash.
    identity = teacher_cache_identity_remote.remote()
    manifest_path = Path(EDU_TEACHER_CACHE_ROOT) / "manifest.json"
    if manifest_path.exists():
        existing = load_teacher_cache_manifest(
            EDU_TEACHER_CACHE_ROOT,
            expected_identity=identity,
            validate_shards=False,
        )
        print(json.dumps(existing, sort_keys=True), flush=True)
        return existing
    shard_rows = 65_536
    payloads = []
    records = {split: [] for split in split_sizes}
    for split in ("train", "validation", "test"):
        rows = split_sizes[split]
        for shard_index, row_start in enumerate(
            range(0, rows, shard_rows)
        ):
            payload = {
                "split": split,
                "shard_index": shard_index,
                "row_start": row_start,
                "rows": min(shard_rows, rows - row_start),
                "identity": identity,
            }
            # Send every shard through the bounded worker pool.  Existing
            # sidecars are checksum-verified there, in parallel, while missing
            # or corrupt shards are rebuilt by the same worker path.
            payloads.append(payload)
    print(
        f"[teacher-cache-verify] shards={len(payloads)} "
        "max_parallel=8",
        flush=True,
    )
    outputs = list(prepare_teacher_cache_shard_remote.map(
        payloads,
        order_outputs=True,
        return_exceptions=False,
    ))
    # Child functions commit their shards independently.  Refresh this
    # coordinator's mount before final validation reads them.
    volume.reload()
    hashes = {output["embedding_sha256"] for output in outputs}
    if hashes and hashes != {identity["embedding_sha256"]}:
        raise RuntimeError("teacher cache shards disagree on embedding hash")
    for output in outputs:
        records[output["split"]].append(output["record"])
    cache_hits = sum(bool(output["cache_hit"]) for output in outputs)
    print(
        f"[teacher-cache-verified] cache_hits={cache_hits} "
        f"rebuilt={len(outputs) - cache_hits}",
        flush=True,
    )
    result = finalize_teacher_cache(
        EDU_TEACHER_CACHE_ROOT,
        identity,
        records,
        expected_split_rows=split_sizes,
        records_checksum_validated=True,
    )
    volume.commit()
    print(json.dumps(result, sort_keys=True), flush=True)
    return result


@app.function(
    image=image,
    gpu="H100",
    timeout=3_600,
    memory=32_768,
)
def tests_remote():
    import os
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
    from qwen_fullwidth_distill.audit import audit_study, audit_wandb

    volume.reload()
    result = audit_study(OUTPUT_ROOT)
    result["wandb"] = audit_wandb(OUTPUT_ROOT)
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)
    return result


@app.function(
    image=image,
    volumes={"/cache": volume},
    timeout=600,
    memory=4_096,
    secrets=WANDB_SECRET,
)
def depth_audit_remote():
    from qwen_fullwidth_distill.audit import audit_depth_study, audit_wandb

    volume.reload()
    result = audit_depth_study(OUTPUT_ROOT)
    result["wandb"] = audit_wandb(
        OUTPUT_ROOT,
        stages=("depth_screen", "depth_opt", "depth_final"),
    )
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)
    return result


@app.function(
    image=image,
    volumes={"/cache": volume},
    timeout=900,
    memory=4_096,
    secrets=WANDB_SECRET,
)
def tensor_audit_remote():
    import subprocess

    from qwen_fullwidth_distill.audit import audit_tensor_study, audit_wandb

    volume.reload()
    sync = subprocess.run(
        ["wandb", "sync", "--sync-all", OUTPUT_ROOT],
        text=True,
        capture_output=True,
        check=False,
    )
    if sync.stdout:
        print(sync.stdout, flush=True)
    if sync.stderr:
        print(sync.stderr, flush=True)
    result = audit_tensor_study(OUTPUT_ROOT)
    result["wandb"] = audit_wandb(
        OUTPUT_ROOT,
        stages=(
            "tensor_lr_probe",
            "tensor_gate_control",
            "tensor_kron_probe",
            "tensor_kron_depth",
            "tensor_param",
            "tensor_time",
            "tensor_final",
            "tensor_long",
        ),
    )
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)
    return result


@app.function(
    image=image,
    gpu="H100",
    volumes={"/cache": volume},
    timeout=3_600,
    memory=65_536,
    secrets=HF_SECRET,
)
def tensor_invariants_remote():
    from qwen_fullwidth_distill.config import ArchitectureConfig
    from qwen_fullwidth_distill.train import embedding_head_invariants

    volume.reload()
    result = embedding_head_invariants(
        ArchitectureConfig(
            "kronecker",
            "residual_ffn",
            1,
            4,
        ),
        device="cuda",
    )
    _write_json_artifact("tensor-invariants.json", result)
    volume.commit()
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)
    return result


@app.function(
    image=image,
    volumes={"/cache": volume},
    timeout=600,
    memory=4_096,
    secrets=WANDB_SECRET,
)
def wandb_probe_remote():
    import wandb
    from qwen_fullwidth_distill.audit import audit_wandb

    project = "umd-leans-well/qwen-fullwidth-monarch-distill"
    volume.reload()
    committed = audit_wandb(OUTPUT_ROOT, project)
    runs = list(wandb.Api(timeout=60).runs(project))
    finished = [
        run
        for run in runs
        if run.state == "finished"
        and (run.name.startswith("screen-") or run.name.startswith("tune-"))
    ]
    if not finished:
        raise RuntimeError("W&B project has no finished screen/tune runs")
    sample = finished[0]
    summary = dict(sample.summary)
    required = (
        "train/kl",
        "validation/kl",
        "final_validation/kl",
        "test/kl",
        "validation",
        "test",
        "performance/step_seconds",
        "performance/peak_allocated_gib",
        "optimizer/lr",
        "diagnostic/gradient_norm",
        "diagnostic/nonfinite_gradients",
        "final_validation/kl",
    )
    result = {
        "project_runs": len(runs),
        "matched_committed_trials": committed["matched_trials"],
        "finished_screen_tune": len(finished),
        "sample_name": sample.name,
        "sample_state": sample.state,
        "required_keys_present": {
            key: key in summary for key in required
        },
    }
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)
    return result


@app.function(
    image=image,
    timeout=300,
    memory=4_096,
    secrets=WANDB_SECRET,
)
def wandb_run_status_remote(run_id: str):
    import wandb

    if not run_id:
        raise ValueError("run_id is required")
    project = "umd-leans-well/qwen-fullwidth-monarch-distill"
    run = wandb.Api(timeout=60).run(f"{project}/{run_id}")
    keys = (
        "_step",
        "train/kl",
        "train/teacher_cross_entropy",
        "train/student_cross_entropy",
        "train/teacher_perplexity",
        "train/student_perplexity",
        "validation/kl",
        "validation/teacher_cross_entropy",
        "validation/student_cross_entropy",
        "validation/teacher_perplexity",
        "validation/student_perplexity",
        "progress/optimizer_steps",
        "progress/input_tokens_seen",
        "optimizer/lr",
        "performance/step_seconds",
        "performance/examples_per_second",
        "performance/peak_allocated_gib",
        "performance/peak_reserved_gib",
        "diagnostic/activation_rms_growth",
        "diagnostic/nonfinite_gradients",
    )
    history = run.history(samples=100, keys=list(keys), pandas=False)
    rows = [
        {key: row[key] for key in keys if key in row}
        for row in history[-10:]
    ]
    result = {
        "id": run.id,
        "name": run.name,
        "state": run.state,
        "summary": {
            key: run.summary[key]
            for key in keys
            if key in run.summary
        },
        "history": rows,
    }
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)
    return result


@app.function(
    image=image,
    timeout=300,
    memory=4_096,
    secrets=WANDB_SECRET,
)
def kronecker_rank_status_remote():
    import wandb

    project = "umd-leans-well/qwen-fullwidth-monarch-distill"
    prefixes = (
        "tensor_kron_rank_early-",
        "tensor_kron_rank_control-",
        "tensor_kron_rank_boundary-",
        "tensor_kron_rank_batch_boundary-",
        "tensor_kron_rank_batch_lr_boundary-",
        "tensor_kron_rank_batch_lr_continue-",
        "tensor_kron_rank_depth_optimized-",
        "tensor_kron_rank_depth_winner_continue-",
        "tensor_kron_rank_continue-",
        "tensor_kron_rank_monarch_long-",
        "tensor_kron_rank_monarch_continue-",
        "tensor_kron_rank_monarch_capacity-",
        "tensor_kron_rank_monarch_capacity_checkpoint-",
        "tensor_kron_rank_monarch_capacity_continue-",
        "tensor_kron_rank_monarch_batch-",
        "tensor_kron_rank_monarch_batch_frontier-",
        "tensor_kron_rank_monarch_batch_continue-",
        "tensor_kron_rank_monarch_batch_long-",
        "tensor_kron_rank_monarch_batch_million-",
        "tensor_kron_rank_monarch_batch_two_million-",
        "tensor_kron_rank_monarch_batch_three_million-",
        "tensor_kron_rank_monarch_batch_rank2_retry-",
        "tensor_kron_rank_monarch_batch_transition-",
        "tensor_kron_rank_monarch_batch_transition_lr-",
        "tensor_kron_rank_monarch_mature_continue-",
        "tensor_kron_rank_monarch_decay-",
        "tensor_kron_rank_monarch_decay_continue-",
        "tensor_kron_rank_monarch_depth_parameter_matched-",
        "tensor_kron_rank_monarch_lr_boundary-",
        "tensor_kron_rank_monarch_loop-",
        "tensor_kron_edu_lr-",
        "tensor_kron_edu_long-",
    )
    keys = (
        "_step",
        "train/kl",
        "validation/kl",
        "final_validation/kl",
        "test/kl",
        "performance/step_seconds",
        "performance/examples_per_second",
        "performance/peak_allocated_gib",
        "diagnostic/activation_rms_growth",
        "diagnostic/gradient_norm",
        "diagnostic/kronecker_mixing_effective_rank_min",
    )
    candidates = [
        run for run in wandb.Api(timeout=60).runs(project)
        if run.name.startswith(prefixes)
    ]
    # Retries and deliberate cancellation tests can leave failed W&B records
    # with the same deterministic name. Prefer a live/finished record, then
    # the newest record, so the status view has one scientific cell per row.
    by_name = {}
    for run in candidates:
        previous = by_name.get(run.name)
        priority = (
            run.state in ("running", "finished"),
            str(getattr(run, "created_at", "")),
        )
        previous_priority = (
            previous.state in ("running", "finished"),
            str(getattr(previous, "created_at", "")),
        ) if previous is not None else (False, "")
        if previous is None or priority > previous_priority:
            by_name[run.name] = run
    runs = list(by_name.values())
    result = []
    for run in sorted(runs, key=lambda value: value.name):
        history = run.history(
            samples=200,
            keys=list(keys),
            pandas=False,
        )
        result.append({
            "id": run.id,
            "name": run.name,
            "url": run.url,
            "state": run.state,
            "summary": {
                key: run.summary[key]
                for key in keys
                if key in run.summary
            },
            "recent": [
                {key: row[key] for key in keys if key in row}
                for row in history[-5:]
            ],
        })
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)
    return result


@app.function(
    image=image,
    timeout=300,
    memory=4_096,
    secrets=WANDB_SECRET,
)
def kronecker_edu_status_remote():
    """Return one concise row per FineWeb-Edu LR cell."""
    import wandb

    project = "umd-leans-well/qwen-fullwidth-monarch-distill"
    keys = (
        "_step",
        "train/kl",
        "validation/kl",
        "final_validation/kl",
        "test/kl",
        "optimizer/base_lr",
        "optimizer/lr",
        "optimizer/lr_min_group",
        "optimizer/lr_schedule_multiplier",
        "diagnostic/gradient_norm_preclip",
        "diagnostic/activation_rms_growth",
        "diagnostic/nonfinite_gradients",
        "performance/examples_per_second",
        "performance/peak_allocated_gib",
    )
    prefixes = (
        "tensor_kron_edu_lr-",
        "tensor_kron_edu_long-",
        "tensor_kron_edu_optimizer-",
        "tensor_kron_edu_objective-",
        "tensor_kron_edu_architecture-",
        "tensor_kron_edu_scale-",
    )
    candidates = [
        run for run in wandb.Api(timeout=60).runs(project)
        if run.name.startswith(prefixes)
    ]
    by_name = {}
    for run in candidates:
        previous = by_name.get(run.name)
        priority = (
            run.state in ("running", "finished"),
            str(getattr(run, "created_at", "")),
        )
        previous_priority = (
            previous.state in ("running", "finished"),
            str(getattr(previous, "created_at", "")),
        ) if previous is not None else (False, "")
        if previous is None or priority > previous_priority:
            by_name[run.name] = run
    result = []
    for run in sorted(
        by_name.values(),
        key=lambda value: float(value.config.get("lr", 0.0)),
    ):
        history = run.history(samples=200, keys=list(keys), pandas=False)
        result.append({
            "id": run.id,
            "url": run.url,
            "state": run.state,
            "base_lr": float(run.config.get("lr", 0.0)),
            "summary": {
                key: run.summary[key]
                for key in keys
                if key in run.summary
            },
            "recent_validation": [
                {key: row[key] for key in keys if key in row}
                for row in history
                if "validation/kl" in row
            ][-5:],
        })
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)
    return result


@app.function(
    image=image,
    timeout=300,
    memory=4_096,
    secrets=WANDB_SECRET,
)
def monarch_depth_status_remote():
    """Read the retained partial depth ablation without allocating an H100."""
    import wandb

    project = "umd-leans-well/qwen-fullwidth-monarch-distill"
    keys = (
        "_step",
        "train/kl",
        "validation/kl",
        "performance/step_seconds",
        "performance/examples_per_second",
        "performance/peak_allocated_gib",
        "diagnostic/activation_rms_growth",
    )
    runs = [
        run for run in wandb.Api(timeout=60).runs(project)
        if run.name.startswith("depth_screen-monarch-")
    ]
    result = []
    for run in sorted(runs, key=lambda value: value.name):
        history = run.history(samples=200, keys=list(keys), pandas=False)
        result.append({
            "id": run.id,
            "name": run.name,
            "url": run.url,
            "state": run.state,
            "summary": {
                key: run.summary[key]
                for key in keys
                if key in run.summary
            },
            "validation_history": [
                {key: row[key] for key in keys if key in row}
                for row in history
                if "validation/kl" in row
            ],
        })
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)
    return result


@app.function(
    image=image,
    volumes={"/cache": volume},
    timeout=600,
    memory=4_096,
)
def cache_probe_remote():
    from qwen_fullwidth_distill.study import screen_trials

    results = _dispatch(screen_trials(), reuse_committed=True)
    labels = {result["label"] for result in results}
    if len(results) != 23 or len(labels) != 23:
        raise RuntimeError("screen cache barrier did not return 23 unique results")
    result = {"screen_cache_hits": len(results), "unique_labels": len(labels)}
    print(json.dumps(result, sort_keys=True), flush=True)
    return result


@app.function(
    image=image,
    gpu="H100",
    volumes={"/cache": volume},
    timeout=86_400,
    memory=65_536,
    secrets=HF_SECRET + WANDB_SECRET,
    max_containers=8,
    # The full-FineWeb wave intentionally exceeds Modal's 24-hour per-call
    # horizon. Progress checkpoints plus stable W&B IDs make timeout retries
    # exact continuations rather than fresh runs.
    retries=8,
)
def train_trial_remote(payload: dict):
    from qwen_fullwidth_distill.train import run_trial

    try:
        volume.reload()
    except RuntimeError as error:
        # A retried input can reuse a container whose failed W&B process still
        # has its debug log open on this already-mounted Volume. Reloading is
        # then forbidden, but the existing mount remains usable. Keep every
        # other Volume failure fatal.
        if "open files preventing the operation" not in str(error):
            raise
        print(
            f"[volume-reload-skipped] retry has open W&B files: {error}",
            flush=True,
        )
    data_root = payload.get("_data_root", DATA_ROOT)
    trial = _trial_from_payload(payload)
    if trial.architecture.operator in ("kronecker", "hybrid"):
        backend = str(
            payload.get("_kronecker_backend", "cutensor")
        )
        rank_chunk = int(
            payload.get(
                "_kronecker_rank_chunk",
                1024 if trial.effective_batch <= 128 else 512,
            )
        )
        microbatch_limit = int(
            payload.get("_kronecker_microbatch", 352)
        )
        if backend not in ("torch", "cutensor"):
            raise ValueError("training backend must be torch or cutensor")
        if rank_chunk <= 0:
            raise ValueError("training rank chunk must be positive")
        if microbatch_limit <= 0:
            raise ValueError("training microbatch limit must be positive")
        os.environ["QWEN_KRONECKER_BACKEND"] = backend
        os.environ["QWEN_KRONECKER_RANK_CHUNK"] = str(rank_chunk)
        os.environ["QWEN_KRONECKER_MICROBATCH"] = str(microbatch_limit)
    else:
        os.environ.pop("QWEN_KRONECKER_BACKEND", None)
        os.environ.pop("QWEN_KRONECKER_RANK_CHUNK", None)
        os.environ.pop("QWEN_KRONECKER_MICROBATCH", None)
    result = run_trial(
        trial,
        data_root=data_root,
        output_root=OUTPUT_ROOT,
        device="cuda",
        checkpoint_callback=volume.commit,
        teacher_cache_root=payload.get("_teacher_cache_root"),
    )
    if "_hydra" in payload:
        result["hydra"] = payload["_hydra"]
        result["resolved_runtime"] = payload.get("_runtime", {})
        result_path = (
            Path(OUTPUT_ROOT) / trial.stage / trial.label / "result.json"
        )
        temporary = result_path.with_suffix(".json.tmp")
        temporary.write_text(json.dumps(result, indent=2, sort_keys=True))
        os.replace(temporary, result_path)
    volume.commit()
    return result


@app.function(
    image=image,
    gpu="H100",
    volumes={"/cache": volume},
    timeout=86_400,
    memory=65_536,
    secrets=HF_SECRET,
    max_containers=8,
    retries=1,
)
def benchmark_trial_remote(payload: dict):
    from dataclasses import replace

    from qwen_fullwidth_distill.train import benchmark_trial

    volume.reload()
    backend = str(payload.get("_kronecker_backend", "torch"))
    rank_chunk = payload.get("_kronecker_rank_chunk")
    microbatch = payload.get("_kronecker_microbatch")
    if backend not in ("torch", "cutensor"):
        raise ValueError("benchmark backend must be torch or cutensor")
    os.environ["QWEN_KRONECKER_BACKEND"] = backend
    if microbatch is None:
        os.environ.pop("QWEN_KRONECKER_MICROBATCH", None)
    else:
        os.environ["QWEN_KRONECKER_MICROBATCH"] = str(int(microbatch))
    trial = _trial_from_payload(payload)
    if rank_chunk is not None:
        trial = replace(
            trial,
            architecture=replace(
                trial.architecture,
                kronecker_rank_chunk=int(rank_chunk),
            ),
        )
    result = benchmark_trial(
        trial,
        data_root=payload.get("_data_root", DATA_ROOT),
        device="cuda",
        teacher_cache_root=payload.get("_teacher_cache_root"),
    )
    result["kronecker_backend"] = backend
    result["runtime_kronecker_rank_chunk"] = (
        trial.architecture.kronecker_rank_chunk
    )
    result["runtime_kronecker_microbatch"] = (
        result.get("microbatch")
        if microbatch is not None
        else None
    )
    result["runtime_kronecker_microbatch_limit"] = (
        int(microbatch) if microbatch is not None else None
    )
    return result


@app.function(
    image=image,
    gpu="H100",
    volumes={"/cache": volume},
    timeout=7_200,
    memory=65_536,
    secrets=HF_SECRET,
    max_containers=8,
    retries=1,
)
def microbatch_preflight_remote(payload: dict):
    """Tune one exact runtime identity through complete AdamW updates."""
    import gc
    from dataclasses import replace

    import torch

    from qwen_fullwidth_distill.train import benchmark_trial

    volume.reload()
    runtime = payload.get("_runtime")
    if not isinstance(runtime, dict):
        raise ValueError("preflight requires a resolved runtime mapping")
    microbatch_policy = runtime.get("microbatch")
    if not isinstance(microbatch_policy, dict):
        raise ValueError("preflight requires runtime.microbatch")
    target_ratio = float(
        microbatch_policy.get("target_memory_fraction", -1.0)
    )
    hard_ratio = float(
        microbatch_policy.get("hard_memory_fraction", -1.0)
    )
    if (
        target_ratio != PREFLIGHT_TARGET_VRAM_RATIO
        or hard_ratio != PREFLIGHT_HARD_VRAM_RATIO
    ):
        raise ValueError(
            "preflight must use the approved 0.88 target and 0.92 hard cap"
        )
    if microbatch_policy.get("mode") != "auto":
        raise ValueError("preflight requires automatic microbatch mode")
    if microbatch_policy.get("oom_backoff") is not True:
        raise ValueError("preflight requires OOM fallback")
    if microbatch_policy.get("require_effective_batch_divisor") is not False:
        raise ValueError("preflight must permit a final tail microbatch")

    backend = str(payload.get("_kronecker_backend", "cutensor"))
    rank_chunk = int(payload.get("_kronecker_rank_chunk", 0))
    precision = str(payload.get("_precision", "bfloat16"))
    if backend not in ("torch", "cutensor"):
        raise ValueError("preflight backend must be torch or cutensor")
    if rank_chunk <= 0:
        raise ValueError("preflight rank chunk must be positive")
    if precision != "bfloat16":
        raise ValueError("preflight currently supports bfloat16 only")

    device = torch.cuda.current_device()
    properties = torch.cuda.get_device_properties(device)
    if "H100" not in properties.name.upper():
        raise RuntimeError(
            f"preflight requires an H100, found {properties.name}"
        )
    try:
        driver_version = int(torch._C._cuda_getDriverVersion())
    except (AttributeError, RuntimeError):
        driver_version = None
    runtime_fingerprint = {
        "torch_version": torch.__version__,
        "cuda_runtime": torch.version.cuda,
        "cuda_driver": driver_version,
        "device_name": properties.name,
        "device_total_memory_bytes": int(properties.total_memory),
        "compute_capability": [
            int(properties.major),
            int(properties.minor),
        ],
    }
    key = _preflight_key(payload, runtime_fingerprint)
    artifact_path = (
        Path(OUTPUT_ROOT) / "microbatch-preflight" / f"{key}.json"
    )
    try:
        cached = json.loads(artifact_path.read_text())
    except (FileNotFoundError, json.JSONDecodeError, OSError):
        cached = None
    if (
        isinstance(cached, dict)
        and cached.get("schema") == PREFLIGHT_SCHEMA
        and cached.get("status") == "complete"
        and cached.get("preflight_key") == key
        and cached.get("runtime_fingerprint") == runtime_fingerprint
        and cached.get("target_memory_fraction") == target_ratio
        and cached.get("hard_memory_fraction") == hard_ratio
        and cached.get("artifact_sha256")
        == _canonical_payload_hash({
            name: value
            for name, value in cached.items()
            if name != "artifact_sha256"
        })
    ):
        return cached

    candidates = _microbatch_candidates(
        int(payload["effective_batch"]),
        runtime,
    )
    trial = _trial_from_payload(payload)
    trial = replace(
        trial,
        architecture=replace(
            trial.architecture,
            kronecker_rank_chunk=rank_chunk,
        ),
    )
    os.environ["QWEN_KRONECKER_BACKEND"] = backend
    os.environ["QWEN_KRONECKER_RANK_CHUNK"] = str(rank_chunk)
    attempts = []
    selected = None
    # CUDA memory is monotone in the physical batch for this fixed graph.
    # Binary search finds the largest declared candidate under the hard cap.
    lower = 0
    upper = len(candidates) - 1
    while lower <= upper:
        index = (lower + upper) // 2
        candidate = candidates[index]
        os.environ["QWEN_KRONECKER_MICROBATCH"] = str(candidate)
        try:
            result = benchmark_trial(
                trial,
                data_root=payload.get("_data_root", EDU_DATA_ROOT),
                device="cuda",
                warmup_steps=1,
                measured_steps=1,
                teacher_cache_root=payload.get("_teacher_cache_root"),
            )
            if result.get("teacher_cache_enabled") is not True:
                raise RuntimeError(
                    "production preflight did not use cached teacher hidden states"
                )
            peak_allocated = float(result["peak_allocated_gib"])
            peak_reserved = torch.cuda.max_memory_reserved() / 2**30
            total_gib = int(properties.total_memory) / 2**30
            peak_gib = max(peak_allocated, peak_reserved)
            memory_ratio = peak_gib / total_gib
            attempt = {
                "microbatch": candidate,
                "status": (
                    "over_hard_cap"
                    if memory_ratio > hard_ratio
                    else "viable"
                ),
                "memory_ratio": memory_ratio,
                "peak_allocated_gib": peak_allocated,
                "peak_reserved_gib": peak_reserved,
                "median_step_seconds": result["median_step_seconds"],
                "median_examples_per_second":
                    result["median_examples_per_second"],
                "gradient_accumulation":
                    result["gradient_accumulation"],
            }
            attempts.append(attempt)
            if memory_ratio <= hard_ratio:
                selected = attempt
                upper = index - 1
            else:
                lower = index + 1
        except BaseException as error:
            if not _is_cuda_oom(error):
                raise
            attempts.append({
                "microbatch": candidate,
                "status": "oom",
                "error": str(error),
            })
            lower = index + 1
        finally:
            gc.collect()
            torch.cuda.empty_cache()

    if selected is None:
        raise RuntimeError(
            "no H100 microbatch candidate survived the 0.92 hard cap: "
            f"{attempts}"
        )
    artifact = {
        "schema": PREFLIGHT_SCHEMA,
        "status": "complete",
        "preflight_key": key,
        "runtime_fingerprint": runtime_fingerprint,
        "architecture": trial.architecture.to_dict(),
        "effective_batch": trial.effective_batch,
        "backend": backend,
        "rank_chunk": rank_chunk,
        "precision": precision,
        "teacher_cache_manifest_sha256":
            payload.get("_teacher_cache_manifest_sha256"),
        "target_memory_fraction": target_ratio,
        "hard_memory_fraction": hard_ratio,
        "selected_microbatch": selected["microbatch"],
        "selected_memory_ratio": selected["memory_ratio"],
        "target_reached": selected["memory_ratio"] >= target_ratio,
        "target_distance": abs(selected["memory_ratio"] - target_ratio),
        "attempts": attempts,
    }
    artifact["artifact_sha256"] = _canonical_payload_hash(artifact)
    artifact_path.parent.mkdir(parents=True, exist_ok=True)
    temporary = artifact_path.with_suffix(".json.tmp")
    temporary.write_text(json.dumps(artifact, indent=2, sort_keys=True))
    os.replace(temporary, artifact_path)
    volume.commit()
    return artifact


@app.function(
    image=image,
    gpu="H100",
    volumes={"/cache": volume},
    timeout=7_200,
    memory=65_536,
    secrets=HF_SECRET,
    max_containers=8,
)
def kronecker_edu_architecture_benchmark_remote(payload: dict):
    """Run one complete full-width optimizer step as an OOM/time gate."""
    from dataclasses import replace

    from qwen_fullwidth_distill.train import benchmark_trial

    volume.reload()
    os.environ["QWEN_KRONECKER_BACKEND"] = "torch"
    os.environ["QWEN_KRONECKER_RANK_CHUNK"] = str(
        int(payload.get("_kronecker_rank_chunk", 1024))
    )
    os.environ["QWEN_KRONECKER_MICROBATCH"] = str(
        int(payload.get("_kronecker_microbatch", 32))
    )
    trial = _trial_from_payload(payload)
    trial = replace(
        trial,
        architecture=replace(
            trial.architecture,
            kronecker_rank_chunk=int(
                payload.get("_kronecker_rank_chunk", 1024)
            ),
        ),
    )
    return benchmark_trial(
        trial,
        data_root=EDU_DATA_ROOT,
        device="cuda",
        warmup_steps=1,
        measured_steps=1,
    )


@app.function(
    image=image,
    gpu="H100",
    timeout=3_600,
    memory=65_536,
)
def kronecker_operator_profile_remote(backend: str = "torch"):
    """Profile one exact d4/r2407 stack step without teacher/head noise."""
    import time

    import torch

    from qwen_fullwidth_distill.config import ArchitectureConfig
    from qwen_fullwidth_distill.model import FullWidthStack

    if backend not in ("torch", "cutensor"):
        raise ValueError("backend must be torch or cutensor")
    os.environ["QWEN_KRONECKER_BACKEND"] = backend
    architecture = ArchitectureConfig(
        "kronecker",
        "residual_ffn",
        4,
        4,
        kronecker_rank=2407,
        kronecker_rank_chunk=32,
    )
    stack = FullWidthStack(architecture).cuda()
    value = torch.randn(
        32,
        architecture.full_width,
        device="cuda",
        dtype=torch.bfloat16,
        requires_grad=True,
    )

    def step():
        stack.zero_grad(set_to_none=True)
        if value.grad is not None:
            value.grad = None
        with torch.autocast(device_type="cuda", dtype=torch.bfloat16):
            output = stack(value)
            loss = output.float().square().mean()
        loss.backward()

    step()
    torch.cuda.synchronize()
    started = time.perf_counter()
    with torch.profiler.profile(
        activities=[
            torch.profiler.ProfilerActivity.CPU,
            torch.profiler.ProfilerActivity.CUDA,
        ],
        record_shapes=True,
        profile_memory=True,
    ) as profile:
        step()
        torch.cuda.synchronize()
    wall_seconds = time.perf_counter() - started
    events = sorted(
        profile.key_averages(group_by_input_shape=True),
        key=lambda event: float(
            getattr(event, "device_time_total", 0.0)
            or getattr(event, "cuda_time_total", 0.0)
        ),
        reverse=True,
    )
    rows = []
    for event in events[:40]:
        rows.append({
            "key": event.key,
            "count": event.count,
            "cpu_time_total_us": float(event.cpu_time_total),
            "device_time_total_us": float(
                getattr(event, "device_time_total", 0.0)
                or getattr(event, "cuda_time_total", 0.0)
            ),
            "cpu_memory_bytes": int(event.cpu_memory_usage),
            "device_memory_bytes": int(
                getattr(event, "device_memory_usage", 0)
                or getattr(event, "cuda_memory_usage", 0)
            ),
            "input_shapes": event.input_shapes,
        })
    return {
        "backend": backend,
        "architecture": architecture.to_dict(),
        "wall_seconds": wall_seconds,
        "peak_allocated_gib":
            torch.cuda.max_memory_allocated() / 2**30,
        "events": rows,
    }


@app.function(
    image=image,
    gpu="H100",
    timeout=3_600,
    memory=65_536,
    max_containers=8,
)
def kronecker_operator_benchmark_remote(payload: dict):
    """Time exact d4/r2407 forward+backward across backend/chunk choices."""
    import statistics
    import time

    import torch

    from qwen_fullwidth_distill.config import ArchitectureConfig
    from qwen_fullwidth_distill.model import FullWidthStack

    backend = str(payload["backend"])
    rank_chunk = int(payload["rank_chunk"])
    if backend not in ("torch", "cutensor"):
        raise ValueError("backend must be torch or cutensor")
    os.environ["QWEN_KRONECKER_BACKEND"] = backend
    architecture = ArchitectureConfig(
        "kronecker",
        "residual_ffn",
        4,
        4,
        kronecker_rank=2407,
        kronecker_rank_chunk=rank_chunk,
    )
    stack = FullWidthStack(architecture).cuda()
    value = torch.randn(
        32,
        architecture.full_width,
        device="cuda",
        dtype=torch.bfloat16,
        requires_grad=True,
    )

    def step():
        stack.zero_grad(set_to_none=True)
        if value.grad is not None:
            value.grad = None
        with torch.autocast(device_type="cuda", dtype=torch.bfloat16):
            output = stack(value)
            loss = output.float().square().mean()
        loss.backward()
        torch.cuda.synchronize()

    step()
    step()
    torch.cuda.reset_peak_memory_stats()
    durations = []
    for _ in range(5):
        started = time.perf_counter()
        step()
        durations.append(time.perf_counter() - started)
    median = statistics.median(durations)
    return {
        "backend": backend,
        "rank_chunk": rank_chunk,
        "median_step_seconds": median,
        "examples_per_second": 32 / median,
        "peak_allocated_gib":
            torch.cuda.max_memory_allocated() / 2**30,
        "durations": durations,
    }


@app.function(
    image=image,
    gpu="H100",
    timeout=3_600,
    memory=65_536,
)
def kronecker_cutensor_correctness_remote():
    """Compare cuTENSOR forward and backward with the Torch reference."""
    import torch

    from qwen_fullwidth_distill.kronecker import KroneckerLinear

    torch.manual_seed(1741)
    reference = KroneckerLinear(
        512,
        64,
        input_modes=(8, 8, 8),
        output_modes=(4, 4, 4),
        rank=5,
        rank_chunk=3,
    ).cuda()
    candidate = KroneckerLinear(
        512,
        64,
        input_modes=(8, 8, 8),
        output_modes=(4, 4, 4),
        rank=5,
        rank_chunk=3,
    ).cuda()
    candidate.load_state_dict(reference.state_dict())
    reference_input = torch.randn(
        7, 512, device="cuda", dtype=torch.bfloat16, requires_grad=True
    )
    candidate_input = reference_input.detach().clone().requires_grad_(True)

    os.environ["QWEN_KRONECKER_BACKEND"] = "torch"
    with torch.autocast(device_type="cuda", dtype=torch.bfloat16):
        reference_output = reference(reference_input)
        reference_loss = reference_output.float().square().mean()
    reference_loss.backward()

    os.environ["QWEN_KRONECKER_BACKEND"] = "cutensor"
    with torch.autocast(device_type="cuda", dtype=torch.bfloat16):
        candidate_output = candidate(candidate_input)
        candidate_loss = candidate_output.float().square().mean()
    candidate_loss.backward()
    torch.cuda.synchronize()

    parameter_rows = []
    for (name, reference_parameter), (_, candidate_parameter) in zip(
        reference.named_parameters(),
        candidate.named_parameters(),
        strict=True,
    ):
        if reference_parameter.grad is None:
            continue
        difference = (
            reference_parameter.grad.float()
            - candidate_parameter.grad.float()
        ).abs()
        parameter_rows.append({
            "name": name,
            "max_abs": float(difference.max()),
            "mean_abs": float(difference.mean()),
        })
    output_difference = (
        reference_output.float() - candidate_output.float()
    ).abs()
    input_difference = (
        reference_input.grad.float() - candidate_input.grad.float()
    ).abs()
    return {
        "output_max_abs": float(output_difference.max()),
        "output_mean_abs": float(output_difference.mean()),
        "input_grad_max_abs": float(input_difference.max()),
        "input_grad_mean_abs": float(input_difference.mean()),
        "parameter_gradients": parameter_rows,
    }


def _training_payload(
    trial,
    data_root: str,
    teacher_cache_root: str | None,
    runtime_by_label: dict[str, dict] | None,
) -> dict:
    return (
        trial.to_dict()
        | {"_data_root": data_root}
        | (
            {"_teacher_cache_root": teacher_cache_root}
            if trial.use_teacher_cache
            else {}
        )
        | (
            runtime_by_label.get(trial.label, {})
            if runtime_by_label is not None
            else {}
        )
    )


def _dispatch(
    trials,
    data_root: str = DATA_ROOT,
    *,
    reuse_committed: bool = False,
    teacher_cache_root: str | None = None,
    runtime_by_label: dict[str, dict] | None = None,
):
    all_trials = list(trials)
    pending = all_trials
    cached = []
    if reuse_committed:
        from qwen_fullwidth_distill.train import _load_completed_result

        volume.reload()
        pending = []
        for trial in all_trials:
            output = Path(OUTPUT_ROOT) / trial.stage / trial.label
            result = _load_completed_result(output, trial)
            expected_runtime = (
                runtime_by_label.get(trial.label, {})
                if runtime_by_label is not None
                else {}
            )
            if (
                result is not None
                and expected_runtime
                and (
                    result.get("hydra") != expected_runtime.get("_hydra")
                    or result.get("resolved_runtime")
                    != expected_runtime.get("_runtime")
                    or result.get("runtime_kronecker_microbatch_limit")
                    != expected_runtime.get("_kronecker_microbatch")
                )
            ):
                raise RuntimeError(
                    f"{trial.label}: committed result collides with a "
                    "different Hydra/runtime identity"
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
    completed = list(cached)
    remaining = list(pending)
    # A mapped input already has three function-level retries. These outer
    # rounds make the barrier resilient if a warm container itself is poisoned
    # by cancellation or an infrastructure failure. return_exceptions keeps one
    # exhausted input from canceling every healthy trainer in the same wave.
    for dispatch_round in range(1, 4):
        payloads = [
            _training_payload(
                trial,
                data_root,
                teacher_cache_root,
                runtime_by_label,
            )
            for trial in remaining
        ]
        outputs = list(
            train_trial_remote.map(
                payloads,
                order_outputs=True,
                return_exceptions=True,
            )
        )
        failed = []
        for trial, output in zip(remaining, outputs, strict=True):
            if isinstance(output, BaseException):
                failed.append(trial)
                print(
                    f"[dispatch-retry] round={dispatch_round}/3 "
                    f"trial={trial.label} error={output!r}",
                    flush=True,
                )
                continue
            if not isinstance(output, dict) or output.get("label") != trial.label:
                raise RuntimeError(
                    f"{trial.label}: mapped output identity mismatch"
                )
            completed.append(output)
        if not failed:
            return completed
        remaining = failed
    raise RuntimeError(
        "training inputs exhausted dispatch retries: "
        f"{[trial.label for trial in remaining]}"
    )


def _dispatch_detached(
    trials,
    *,
    root_hash: str,
    config_name: str,
    data_root: str,
    teacher_cache_root: str | None,
    runtime_by_label: dict[str, dict],
) -> dict:
    """Idempotently spawn a long-running wave without a 24-hour coordinator."""
    from qwen_fullwidth_distill.train import _load_completed_result

    trials = list(trials)
    if len(trials) != 8 or len({trial.label for trial in trials}) != 8:
        raise RuntimeError("detached Hydra dispatch requires eight unique cells")
    launch_name = f"hydra-detached-launch-{root_hash}.json"
    launch_path = Path(OUTPUT_ROOT) / launch_name
    volume.reload()
    if launch_path.is_file():
        manifest = json.loads(launch_path.read_text())
        if (
            manifest.get("schema")
            != "qwen-fullwidth-hydra-detached-launch-v1"
            or manifest.get("resolved_config_hash") != root_hash
            or manifest.get("config_name") != config_name
        ):
            raise RuntimeError("detached launch manifest identity mismatch")
    else:
        manifest = {
            "schema": "qwen-fullwidth-hydra-detached-launch-v1",
            "status": "dispatching",
            "resolved_config_hash": root_hash,
            "config_name": config_name,
            "calls": [],
            "completed_labels": [],
        }

    calls_by_label = {
        row["label"]: row
        for row in manifest.get("calls", [])
        if isinstance(row, dict) and isinstance(row.get("label"), str)
    }
    completed_labels = []
    for trial in trials:
        output = Path(OUTPUT_ROOT) / trial.stage / trial.label
        if _load_completed_result(output, trial) is not None:
            completed_labels.append(trial.label)
            continue
        if trial.label in calls_by_label:
            continue
        payload = _training_payload(
            trial,
            data_root,
            teacher_cache_root,
            runtime_by_label,
        )
        function_call = train_trial_remote.spawn(payload)
        row = {
            "label": trial.label,
            "function_call_id": function_call.object_id,
        }
        manifest.setdefault("calls", []).append(row)
        calls_by_label[trial.label] = row
        manifest["completed_labels"] = sorted(completed_labels)
        manifest["status"] = "dispatching"
        manifest["artifact_sha256"] = _canonical_payload_hash({
            key: value
            for key, value in manifest.items()
            if key != "artifact_sha256"
        })
        _write_json_artifact(launch_name, manifest)
        volume.commit()

    manifest["completed_labels"] = sorted(completed_labels)
    manifest["status"] = (
        "complete" if len(completed_labels) == len(trials) else "launched"
    )
    manifest["active_labels"] = sorted(
        set(calls_by_label) - set(completed_labels)
    )
    manifest["artifact_sha256"] = _canonical_payload_hash({
        key: value
        for key, value in manifest.items()
        if key != "artifact_sha256"
    })
    _write_json_artifact(launch_name, manifest)
    volume.commit()
    return manifest


def _benchmark_dispatch(trials):
    all_trials = list(trials)
    if not all_trials:
        return []
    payloads = [
        trial.to_dict() | {"_data_root": DATA_ROOT}
        for trial in all_trials
    ]
    outputs = list(
        benchmark_trial_remote.map(
            payloads,
            order_outputs=True,
            return_exceptions=True,
        )
    )
    results = []
    for trial, output in zip(all_trials, outputs, strict=True):
        if isinstance(output, BaseException):
            results.append({
                "architecture": trial.architecture.to_dict(),
                "architecture_label": trial.architecture.label,
                "depth": trial.architecture.depth,
                "status": "failed",
                "error": repr(output),
            })
        else:
            output["status"] = "complete"
            results.append(output)
    return results


def _read_committed_trials(trials):
    from qwen_fullwidth_distill.train import _load_completed_result

    volume.reload()
    results = []
    for trial in trials:
        output = Path(OUTPUT_ROOT) / trial.stage / trial.label
        result = _load_completed_result(output, trial)
        if result is None:
            raise RuntimeError(
                f"required committed reference is missing: {trial.label}"
            )
        results.append(result)
    return results


def _write_json_artifact(name: str, value: dict) -> None:
    path = Path(OUTPUT_ROOT) / name
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_text(json.dumps(value, indent=2, sort_keys=True))
    os.replace(temporary, path)


def _run_study():
    from qwen_fullwidth_distill.study import (
        final_trials,
        screen_trials,
        select_common_topology,
        select_unrestricted_monarch,
        tuning_trials,
    )

    prepare_data_remote.remote()
    screens = _dispatch(screen_trials(), reuse_committed=True)
    common = select_common_topology(screens)
    deep = select_unrestricted_monarch(screens)
    print(f"[selection] common={common.label} deep={deep.label}", flush=True)
    tuning = _dispatch(tuning_trials(common, deep), reuse_committed=True)
    final_grid = final_trials(screens, tuning)
    volume.reload()
    _write_json_artifact(
        "study-plan.json",
        {
            "selection_basis": "validation_only",
            "common_topology": common.to_dict(),
            "unrestricted_monarch": deep.to_dict(),
            "final_trials": [trial.to_dict() for trial in final_grid],
        },
    )
    volume.commit()
    finals = _dispatch(final_grid, reuse_committed=True)
    summary = {"screen": screens, "tune": tuning, "final": finals}
    volume.reload()
    _write_json_artifact("study-summary.json", summary)
    volume.commit()
    print(json.dumps(summary, indent=2), flush=True)
    return summary


def _run_depth_study():
    from qwen_fullwidth_distill.study import (
        depth_reference_trials,
        depth_screen_trials,
        optimization_trials,
        optimized_depth_final_trials,
        select_depth_winners,
        select_optimized_winners,
    )

    prepare_data_remote.remote()
    reference_grid = depth_reference_trials()
    references = _read_committed_trials(reference_grid)
    screens = _dispatch(depth_screen_trials(), reuse_committed=True)
    selection_rows = references + screens
    untied, untied_lr, looped, looped_lr = select_depth_winners(selection_rows)
    print(
        f"[depth-selection] untied={untied.label}@{untied_lr:g} "
        f"looped={looped.label}@{looped_lr:g}",
        flush=True,
    )
    optimization_grid = optimization_trials(selection_rows)
    optimization = _dispatch(optimization_grid, reuse_committed=True)
    (
        optimized_untied,
        optimized_untied_lr,
        optimized_untied_batch,
        optimized_looped,
        optimized_looped_lr,
        optimized_looped_batch,
    ) = select_optimized_winners(selection_rows, optimization)
    print(
        "[optimization-selection] "
        f"untied={optimized_untied.label}@{optimized_untied_lr:g}"
        f"/b{optimized_untied_batch} "
        f"looped={optimized_looped.label}@{optimized_looped_lr:g}"
        f"/b{optimized_looped_batch}",
        flush=True,
    )
    final_grid = optimized_depth_final_trials(selection_rows, optimization)
    volume.reload()
    _write_json_artifact(
        "depth-study-plan.json",
        {
            "selection_basis": "endpoint_validation_only",
            "reference_trials": [
                trial.to_dict() for trial in reference_grid
            ],
            "screen_trials": [
                trial.to_dict() for trial in depth_screen_trials()
            ],
            "structural_untied_winner": {
                "architecture": untied.to_dict(),
                "lr": untied_lr,
            },
            "structural_looped_winner": {
                "architecture": looped.to_dict(),
                "lr": looped_lr,
            },
            "optimization_trials": [
                trial.to_dict() for trial in optimization_grid
            ],
            "optimized_untied_winner": {
                "architecture": optimized_untied.to_dict(),
                "lr": optimized_untied_lr,
                "effective_batch": optimized_untied_batch,
            },
            "optimized_looped_winner": {
                "architecture": optimized_looped.to_dict(),
                "lr": optimized_looped_lr,
                "effective_batch": optimized_looped_batch,
            },
            "final_trials": [trial.to_dict() for trial in final_grid],
        },
    )
    volume.commit()
    finals = _dispatch(final_grid, reuse_committed=True)
    summary = {
        "reference": references,
        "depth_screen": screens,
        "depth_opt": optimization,
        "depth_final": finals,
    }
    volume.reload()
    _write_json_artifact("depth-study-summary.json", summary)
    volume.commit()
    print(json.dumps(summary, indent=2), flush=True)
    return summary


def _match_tensor_time_depths(
    reference: dict,
    btt_settings: list[tuple[float, str]],
    kronecker_settings: list[tuple[float, str]],
):
    from dataclasses import replace

    from qwen_fullwidth_distill.config import ArchitectureConfig, TrialConfig
    from qwen_fullwidth_distill.study import (
        btt_parameter_architectures,
        kronecker_parameter_architecture,
        tensor_family_key,
    )

    batch = int(reference.get("effective_batch", 1_024))
    reference_trial = TrialConfig(
        ArchitectureConfig(**reference["architecture"]),
        lr=float(reference["lr"]),
        seed=0,
        steps=1,
        stage="tensor_benchmark",
        audit_every=1,
        compile_model=False,
        effective_batch=batch,
        lr_parameterization=str(
            reference.get("lr_parameterization", "uniform")
        ),
    )
    reference_benchmark = _benchmark_dispatch([reference_trial])[0]
    if reference_benchmark.get("status") != "complete":
        raise RuntimeError(
            f"Monarch time reference benchmark failed: {reference_benchmark}"
        )
    target = float(reference_benchmark["median_step_seconds"])
    families = [
        *btt_parameter_architectures(),
        kronecker_parameter_architecture(),
    ]
    maximum_depths = {
        tensor_family_key(config): (
            128 if config.operator == "btt" else config.depth
        )
        for config in families
    }

    def trial_for(config, depth):
        lr, parameterization = (
            btt_settings[0]
            if config.operator == "btt"
            else kronecker_settings[0]
        )
        return TrialConfig(
            replace(config, depth=depth),
            lr=lr,
            seed=0,
            steps=1,
            stage="tensor_benchmark",
            audit_every=1,
            compile_model=config.operator == "btt",
            effective_batch=batch,
            lr_parameterization=parameterization,
        )

    measurements: dict[str, list[dict]] = {
        tensor_family_key(config): [] for config in families
    }
    edge_trials = [
        trial_for(config, depth)
        for config in families
        for depth in (1, maximum_depths[tensor_family_key(config)])
    ]
    for result in _benchmark_dispatch(edge_trials):
        config = ArchitectureConfig(**result["architecture"])
        measurements[tensor_family_key(config)].append(result)

    def feasible(result):
        return (
            result.get("status") == "complete"
            and float(result.get("peak_allocated_gib", float("inf"))) < 75
            and float(result["median_step_seconds"]) <= 1.05 * target
        )

    bounds = {}
    matched = {}
    for config in families:
        key = tensor_family_key(config)
        by_depth = {
            int(result["depth"]): result for result in measurements[key]
        }
        if not feasible(by_depth[1]):
            continue
        maximum = maximum_depths[key]
        if feasible(by_depth[maximum]):
            matched[key] = maximum
        else:
            bounds[key] = [1, maximum]

    while bounds:
        candidates = []
        candidate_keys = []
        for config in families:
            key = tensor_family_key(config)
            if key not in bounds:
                continue
            low, high = bounds[key]
            if high - low <= 1:
                matched[key] = low
                continue
            middle = (low + high) // 2
            candidates.append(trial_for(config, middle))
            candidate_keys.append(key)
        bounds = {
            key: value for key, value in bounds.items()
            if value[1] - value[0] > 1
        }
        if not candidates:
            break
        for key, result in zip(
            candidate_keys,
            _benchmark_dispatch(candidates),
            strict=True,
        ):
            measurements[key].append(result)
            depth = int(result["depth"])
            if feasible(result):
                bounds[key][0] = depth
            else:
                bounds[key][1] = depth

    details = {}
    for config in families:
        key = tensor_family_key(config)
        if key not in matched:
            details[key] = {
                "status": "infeasible_at_depth_1",
                "measurements": measurements[key],
            }
            continue
        depth = matched[key]
        selected = next(
            result
            for result in measurements[key]
            if int(result["depth"]) == depth
        )
        details[key] = {
            "status": "matched",
            "matched_depth": depth,
            "target_step_seconds": target,
            "matched_step_seconds": selected["median_step_seconds"],
            "ratio": float(selected["median_step_seconds"]) / target,
            "measurements": sorted(
                measurements[key], key=lambda result: int(result["depth"])
            ),
        }
    return {
        "reference": reference_benchmark,
        "target_step_seconds": target,
        "memory_limit_gib": 75,
        "time_tolerance": 1.05,
        "max_depths": maximum_depths,
        "matched_depths": matched,
        "families": details,
    }


def _run_tensor_study():
    from qwen_fullwidth_distill.audit import audit_depth_study
    from qwen_fullwidth_distill.study import (
        depth_reference_trials,
        depth_screen_trials,
        evaluate_tensor_gate,
        kronecker_depth_trials,
        kronecker_lr_probe_trials,
        kronecker_parameter_trials,
        next_tensor_boundary_probe,
        next_kronecker_boundary_probe,
        optimization_trials,
        select_kronecker_lr_settings,
        select_tensor_finalists,
        select_tensor_lr_settings,
        select_tensor_reference,
        tensor_final_trials,
        tensor_gate_btt_trials,
        tensor_gate_control_trial,
        tensor_long_trial,
        tensor_lr_probe_trials,
        tensor_parameter_trials,
        tensor_time_trials,
    )

    prerequisite = audit_depth_study(OUTPUT_ROOT)
    invariants = tensor_invariants_remote.remote()
    if invariants.get("status") != "complete":
        raise RuntimeError("frozen embedding/head invariant gate failed")
    references = _read_committed_trials(depth_reference_trials())
    screens = _read_committed_trials(depth_screen_trials())
    selection_rows = references + screens
    optimization = _read_committed_trials(
        optimization_trials(selection_rows)
    )
    reference = select_tensor_reference(optimization)
    print(
        f"[tensor-reference] {reference['label']} "
        f"val={reference['validation']['kl']:.6f}",
        flush=True,
    )

    control_trial = tensor_gate_control_trial(reference)
    control = _dispatch([control_trial], reuse_committed=True)[0]
    gate_trials = tensor_gate_btt_trials(reference)
    probes = _dispatch(gate_trials, reuse_committed=True)
    gate = evaluate_tensor_gate(control, probes)
    volume.reload()
    _write_json_artifact("tensor-gate.json", gate)
    volume.commit()
    if gate["status"] != "passed":
        raise RuntimeError(f"BTT controlled gate failed: {gate}")

    probe_grid = tensor_lr_probe_trials(reference)
    completed_labels = {result["label"] for result in probes}
    probes.extend(_dispatch(
        [
            trial
            for trial in probe_grid
            if trial.label not in completed_labels
        ],
        reuse_committed=True,
    ))
    boundary_trials = []
    for _ in range(2):
        boundary = next_tensor_boundary_probe(reference, probes)
        if boundary is None:
            break
        boundary_trials.append(boundary)
        probes.extend(_dispatch([boundary], reuse_committed=True))
    settings = select_tensor_lr_settings(probes)
    print(f"[tensor-lr-selection] {settings}", flush=True)

    kronecker_probe_grid = kronecker_lr_probe_trials(reference)
    kronecker_probes = _dispatch(
        kronecker_probe_grid,
        reuse_committed=True,
    )
    kronecker_boundary_trials = []
    for _ in range(2):
        boundary = next_kronecker_boundary_probe(
            reference,
            kronecker_probes,
        )
        if boundary is None:
            break
        kronecker_boundary_trials.append(boundary)
        kronecker_probes.extend(
            _dispatch([boundary], reuse_committed=True)
        )
    kronecker_settings = select_kronecker_lr_settings(
        kronecker_probes
    )
    print(
        f"[kronecker-lr-selection] {kronecker_settings}",
        flush=True,
    )
    kronecker_depth_grid = kronecker_depth_trials(
        reference,
        kronecker_probes,
    )
    kronecker_depth_results = _dispatch(
        kronecker_depth_grid,
        reuse_committed=True,
    )

    parameter_grid = [
        *tensor_parameter_trials(reference, probes),
        *kronecker_parameter_trials(reference, kronecker_probes),
    ]
    parameter_results = _dispatch(parameter_grid, reuse_committed=True)
    benchmarks = _match_tensor_time_depths(
        reference,
        settings,
        kronecker_settings,
    )
    time_grid = tensor_time_trials(
        reference,
        probes,
        benchmarks["matched_depths"],
        kronecker_probes,
    )
    time_results = _dispatch(time_grid, reuse_committed=True)
    btt_winner, kronecker_winner, time_winner = select_tensor_finalists(
        parameter_results, time_results
    )
    final_grid = tensor_final_trials(parameter_results, time_results)

    plan = {
        "selection_basis": "endpoint_validation_only",
        "prerequisite_audit": prerequisite,
        "embedding_head_invariants": invariants,
        "monarch_reference": reference,
        "gate_control_trial": control_trial.to_dict(),
        "controlled_gate": gate,
        "lr_probe_trials": [
            trial.to_dict() for trial in probe_grid + boundary_trials
        ],
        "selected_lr_settings": [
            {"lr": lr, "lr_parameterization": parameterization}
            for lr, parameterization in settings
        ],
        "kronecker_lr_probe_trials": [
            trial.to_dict()
            for trial in (
                kronecker_probe_grid + kronecker_boundary_trials
            )
        ],
        "selected_kronecker_lr_settings": [
            {"lr": lr, "lr_parameterization": parameterization}
            for lr, parameterization in kronecker_settings
        ],
        "kronecker_depth_trials": [
            trial.to_dict() for trial in kronecker_depth_grid
        ],
        "parameter_frontier_trials": [
            trial.to_dict() for trial in parameter_grid
        ],
        "time_benchmarks": benchmarks,
        "time_frontier_trials": [trial.to_dict() for trial in time_grid],
        "btt_parameter_winner": btt_winner,
        "kronecker_parameter_winner": kronecker_winner,
        "time_winner": time_winner,
        "final_trials": [trial.to_dict() for trial in final_grid],
    }
    volume.reload()
    _write_json_artifact("tensor-study-plan.json", plan)
    _write_json_artifact("tensor-benchmark.json", benchmarks)
    volume.commit()

    finals = _dispatch(final_grid, reuse_committed=True)
    long_grid = [
        tensor_long_trial(
            parameter_results,
            time_results,
            finals,
        )
    ]
    plan["long_trials"] = [trial.to_dict() for trial in long_grid]
    volume.reload()
    _write_json_artifact("tensor-study-plan.json", plan)
    volume.commit()
    long_results = _dispatch(long_grid, reuse_committed=True)
    summary = {
        "tensor_gate_control": [control],
        "tensor_lr_probe": probes,
        "tensor_kron_probe": kronecker_probes,
        "tensor_kron_depth": kronecker_depth_results,
        "tensor_param": parameter_results,
        "tensor_time": time_results,
        "tensor_final": finals,
        "tensor_long": long_results,
    }
    volume.reload()
    _write_json_artifact("tensor-study-summary.json", summary)
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
    """Persistent cloud-side coordinator for the barriered three-stage study."""
    volume.reload()
    return _run_study()


@app.function(
    image=image,
    volumes={"/cache": volume},
    timeout=86_400,
    memory=8_192,
)
def depth_study_remote():
    """Persistent coordinator for the depth and tied-cycle ablation."""
    volume.reload()
    return _run_depth_study()


@app.function(
    image=image,
    volumes={"/cache": volume},
    timeout=86_400,
    memory=8_192,
    secrets=WANDB_SECRET,
)
def tensor_study_remote():
    """Persistent coordinator for BTT LR, parameter, time, and final stages."""
    volume.reload()
    return _run_tensor_study()


@app.function(
    image=image,
    volumes={"/cache": volume},
    timeout=86_400,
    memory=8_192,
)
def kronecker_edu_lr_study_remote():
    """Persistent coordinator for data preparation and the eight-cell LR wave."""
    from qwen_fullwidth_distill.study import kronecker_edu_lr_trials

    volume.reload()
    prepare_data_remote.remote(edu=True)
    return _dispatch(kronecker_edu_lr_trials(), EDU_DATA_ROOT)


def _run_kronecker_edu_optimizer_study():
    from qwen_fullwidth_distill.study import (
        kronecker_edu_lr_trials,
        kronecker_edu_optimizer_trials,
        select_kronecker_edu_lr_winner,
        select_kronecker_edu_stage_winner,
    )

    lr_results = _read_committed_trials(kronecker_edu_lr_trials())
    source = select_kronecker_edu_lr_winner(lr_results)
    cache_manifest = prepare_teacher_cache_remote.remote()
    trials = kronecker_edu_optimizer_trials(source)
    results = _dispatch(
        trials,
        EDU_DATA_ROOT,
        reuse_committed=True,
        teacher_cache_root=EDU_TEACHER_CACHE_ROOT,
    )
    winner = select_kronecker_edu_stage_winner(trials, results)
    summary = {
        "source": source.to_dict(),
        "teacher_cache_manifest_sha256":
            cache_manifest["manifest_sha256"],
        "trials": results,
        "winner": winner.to_dict(),
    }
    volume.reload()
    _write_json_artifact("kronecker-edu-optimizer-summary.json", summary)
    volume.commit()
    return summary


def _wait_for_committed_trials(
    trials,
    *,
    timeout_seconds: int = 21_600,
    poll_seconds: int = 60,
):
    """Wait without allocating a GPU for an already-running external wave."""
    import time

    from qwen_fullwidth_distill.train import _load_completed_result

    trials = list(trials)
    deadline = time.monotonic() + timeout_seconds
    while True:
        volume.reload()
        results = []
        pending = []
        for trial in trials:
            output = Path(OUTPUT_ROOT) / trial.stage / trial.label
            result = _load_completed_result(output, trial)
            if result is None:
                pending.append(trial.label)
            else:
                results.append(result)
        if not pending:
            return results
        if time.monotonic() >= deadline:
            raise TimeoutError(
                "timed out waiting for committed trials: "
                f"{pending}"
            )
        print(
            f"[artifact-wait] complete={len(results)}/{len(trials)} "
            f"pending={pending}",
            flush=True,
        )
        time.sleep(poll_seconds)


def _validate_full_validation_source(
    source,
    result: dict,
    source_selector: dict,
) -> None:
    validation = result.get("validation")
    manifest = result.get("dataset_manifest")
    split_sizes = (
        manifest.get("split_sizes", {})
        if isinstance(manifest, dict)
        else {}
    )
    checkpoint = (
        Path(OUTPUT_ROOT) / source.stage / source.label / "student.pt"
    )
    if (
        source_selector.get("selector") != "completed_validation_winner"
        or source_selector.get("metric") != "validation.kl"
        or source_selector.get("direction") != "min"
        or source_selector.get("require_status") != "complete"
        or source_selector.get("require_exact_steps") is not True
        or source_selector.get("require_full_validation") is not True
        or source_selector.get("require_checkpoint") is not True
    ):
        raise ValueError("unsupported Hydra source-selection contract")
    if (
        result.get("label") != source.label
        or result.get("status") != "complete"
        or int(result.get("steps_completed", -1)) != source.steps
        or not isinstance(validation, dict)
        or not math.isfinite(float(validation.get("kl", math.nan)))
        or int(split_sizes.get("validation", -1)) != 8_192
        or not checkpoint.is_file()
        or checkpoint.stat().st_size <= 0
    ):
        raise RuntimeError(
            "Hydra source gate requires an exact completed checkpoint and "
            "full 8,192-example validation endpoint"
        )


def _rank_cooled_full_validation(trials, results: list[dict]) -> list[dict]:
    """Audit and rank only exact, post-cooldown full-validation endpoints."""
    trials = list(trials)
    if len(trials) != 8 or len(results) != 8:
        raise RuntimeError("cooled endpoint audit requires exactly eight cells")
    by_label = {result.get("label"): result for result in results}
    if len(by_label) != 8 or set(by_label) != {
        trial.label for trial in trials
    }:
        raise RuntimeError("cooled endpoint result identities are incomplete")
    rows = []
    for trial in trials:
        result = by_label[trial.label]
        validation = result.get("validation")
        manifest = result.get("dataset_manifest")
        split_sizes = (
            manifest.get("split_sizes", {})
            if isinstance(manifest, dict)
            else {}
        )
        if (
            trial.lr_schedule != "wsd"
            or trial.cooldown_steps <= 0
            or result.get("lr_schedule") != "wsd"
            or int(result.get("cooldown_steps", -1))
            != trial.cooldown_steps
            or result.get("hydra_config_hash")
            != trial.hydra_config_hash
            or int(result.get("steps_completed", -1)) != trial.steps
            or result.get("status") != "complete"
            or not isinstance(validation, dict)
            or not math.isfinite(float(validation.get("kl", math.nan)))
            or int(split_sizes.get("validation", -1)) != 8_192
        ):
            raise RuntimeError(
                f"{trial.label}: result is not an exact cooled "
                "full-validation endpoint"
            )
        rows.append({
            "label": trial.label,
            "validation_kl": float(validation["kl"]),
            "effective_batch": trial.effective_batch,
            "lr": trial.lr,
            "steps": trial.steps,
            "fresh_contexts": trial.steps * trial.effective_batch,
            "cooldown_steps": trial.cooldown_steps,
        })
    rows.sort(key=lambda row: (row["validation_kl"], row["label"]))
    for rank, row in enumerate(rows, start=1):
        row["rank"] = rank
    return rows


def _audit_fresh_adam_continuations(
    trials,
    results: list[dict],
    *,
    source_examples_seen: int,
) -> None:
    """Prove that data age continued while Adam and schedule age restarted."""
    by_label = {result.get("label"): result for result in results}
    for trial in trials:
        result = by_label.get(trial.label, {})
        expected_examples = (
            source_examples_seen
            + trial.steps * trial.effective_batch
        )
        if (
            result.get("warm_started") is not True
            or result.get("warm_start_weights_only") is not True
            or result.get("optimizer_state_resumed") is not False
            or int(result.get("resumed_from_step", -1)) != 0
            or int(result.get("optimizer_steps", -1)) != trial.steps
            or int(result.get("examples_seen", -1)) != expected_examples
            or int(result.get("input_tokens_seen", -1))
            != expected_examples * trial.architecture.context_length
        ):
            raise RuntimeError(
                f"{trial.label}: fresh-Adam/data-cursor audit failed"
            )


def _run_hydra_replacement_pipeline(
    config_name: str = "edu_wsd_large_batch_lr",
    overrides: list[str] | tuple[str, ...] | None = None,
) -> dict:
    """Run the artifact-gated eight-cell WSD replacement stage."""
    from qwen_fullwidth_distill.study import (
        kronecker_edu_lr_trials,
        select_kronecker_edu_lr_winner,
    )

    hydra_plan = _resolve_hydra_experiment(config_name, overrides)
    cells = hydra_plan["cells"]
    root_hash = hydra_plan["resolved_config_hash"]
    runtime = cells[0]["runtime"]
    if (
        any(cell["runtime"] != runtime for cell in cells)
        or runtime.get("accelerator") != "H100"
        or int(runtime.get("max_parallel", -1)) != 8
    ):
        raise RuntimeError("replacement wave requires one exact max-8 H100 runtime")

    source_trials = kronecker_edu_lr_trials()
    source_results = _wait_for_committed_trials(source_trials)
    source = select_kronecker_edu_lr_winner(source_results)
    source_result = next(
        result
        for result in source_results
        if result.get("label") == source.label
    )
    source_selector = cells[0]["warm_start"]["source"]
    _validate_full_validation_source(source, source_result, source_selector)

    trials = [
        _trial_from_hydra_cell(
            cell,
            source,
            resolved_root_config=hydra_plan["resolved_config"],
            resolved_config_hash=root_hash,
        )
        for cell in cells
    ]
    if len({trial.label for trial in trials}) != 8:
        raise RuntimeError("materialized Hydra trials are not uniquely labeled")

    cache_manifest = prepare_teacher_cache_remote.remote()
    if not isinstance(cache_manifest, dict) or not isinstance(
        cache_manifest.get("manifest_sha256"), str
    ):
        raise RuntimeError("teacher-cache artifact gate failed")

    preflight_payloads_by_key = {}
    runtime_by_label = {}
    for cell, trial in zip(cells, trials, strict=True):
        hydra_metadata = {
            **cell["hydra"],
            "resolved_config_hash": root_hash,
            "config_name": config_name,
            "overrides": list(overrides or ()),
            "resolved_root_config": hydra_plan["resolved_config"],
            "resolved_cell": cell,
            "teacher_cache_manifest_sha256":
                cache_manifest["manifest_sha256"],
        }
        runtime_payload = {
            "_kronecker_backend": runtime["kronecker_backend"],
            "_kronecker_rank_chunk": int(runtime["rank_chunk"]),
            "_precision": runtime["precision"],
            "_runtime": runtime,
            "_hydra": hydra_metadata,
            "_teacher_cache_manifest_sha256":
                cache_manifest["manifest_sha256"],
        }
        preflight_payload = (
            trial.to_dict()
            | runtime_payload
            | {
                "_data_root": cells[0]["data"]["data_root"],
                "_teacher_cache_root":
                    cells[0]["data"]["teacher_cache_root"],
            }
        )
        request_key = _preflight_request_key(preflight_payload)
        existing_payload = preflight_payloads_by_key.get(request_key)
        if (
            existing_payload is None
            or int(preflight_payload["effective_batch"])
            > int(existing_payload["effective_batch"])
        ):
            # Benchmark the largest accumulation workload.  This is the most
            # conservative representative and lets an exact cached preflight
            # for that workload satisfy all cells sharing the memory graph.
            preflight_payloads_by_key[request_key] = preflight_payload
        runtime_by_label[trial.label] = (
            runtime_payload | {"_preflight_request_key": request_key}
        )
    preflight_keys = list(preflight_payloads_by_key)
    preflight_outputs = list(microbatch_preflight_remote.map(
        [preflight_payloads_by_key[key] for key in preflight_keys],
        order_outputs=True,
        return_exceptions=True,
    ))
    failures = [
        f"{key}: {output!r}"
        for key, output in zip(
            preflight_keys, preflight_outputs, strict=True
        )
        if isinstance(output, BaseException) or not isinstance(output, dict)
    ]
    if failures:
        raise RuntimeError("H100 preflight gate failed: " + "; ".join(failures))
    preflights_by_request = dict(zip(
        preflight_keys,
        preflight_outputs,
        strict=True,
    ))
    preflights_by_artifact = {}
    for label, value in runtime_by_label.items():
        preflight = preflights_by_request[
            value.pop("_preflight_request_key")
        ]
        if (
            preflight.get("status") != "complete"
            or float(preflight.get("selected_memory_ratio", math.inf))
            > PREFLIGHT_HARD_VRAM_RATIO
        ):
            raise RuntimeError(f"{label}: invalid H100 preflight artifact")
        value["_kronecker_microbatch"] = int(
            preflight["selected_microbatch"]
        )
        value["_hydra"] = value["_hydra"] | {
            "preflight_key": preflight["preflight_key"],
            "preflight_artifact_sha256": preflight["artifact_sha256"],
            "selected_microbatch": preflight["selected_microbatch"],
        }
        preflights_by_artifact[
            value["_hydra"]["preflight_key"]
        ] = preflight

    gate = {
        "schema": "qwen-fullwidth-hydra-dispatch-gate-v1",
        "status": "ready",
        "config_name": config_name,
        "overrides": list(overrides or ()),
        "resolved_config_hash": root_hash,
        "source": source.to_dict(),
        "source_validation": source_result["validation"],
        "source_checkpoint": str(
            Path(OUTPUT_ROOT) / source.stage / source.label / "student.pt"
        ),
        "teacher_cache_root": EDU_TEACHER_CACHE_ROOT,
        "teacher_cache_manifest_sha256":
            cache_manifest["manifest_sha256"],
        "preflights": list(preflights_by_artifact.values()),
        "trials": [trial.to_dict() for trial in trials],
        "runtime_by_label": runtime_by_label,
    }
    gate["artifact_sha256"] = _canonical_payload_hash(gate)
    gate_name = f"hydra-dispatch-gate-{root_hash}.json"
    volume.reload()
    _write_json_artifact(gate_name, gate)
    volume.commit()
    volume.reload()
    persisted_gate = json.loads((Path(OUTPUT_ROOT) / gate_name).read_text())
    if (
        persisted_gate != gate
        or persisted_gate.get("artifact_sha256")
        != _canonical_payload_hash({
            key: value
            for key, value in persisted_gate.items()
            if key != "artifact_sha256"
        })
    ):
        raise RuntimeError("persisted Hydra dispatch gate failed verification")

    dispatch_mode = hydra_plan["resolved_config"]["experiment"][
        "dispatch_mode"
    ]
    if dispatch_mode == "detached":
        launch = _dispatch_detached(
            trials,
            root_hash=root_hash,
            config_name=config_name,
            data_root=EDU_DATA_ROOT,
            teacher_cache_root=EDU_TEACHER_CACHE_ROOT,
            runtime_by_label=runtime_by_label,
        )
        if launch["status"] != "complete":
            fresh_tokens_per_cell = (
                int(cells[0]["fresh_contexts"])
                * trials[0].architecture.context_length
            )
            return {
                "schema": "qwen-fullwidth-hydra-detached-wave-v1",
                "status": "launched",
                "resolved_config_hash": root_hash,
                "dispatch_gate_sha256": gate["artifact_sha256"],
                "source": source.to_dict(),
                "teacher_cache_manifest_sha256":
                    cache_manifest["manifest_sha256"],
                "cell_count": len(trials),
                "fresh_tokens_per_cell": fresh_tokens_per_cell,
                "fresh_tokens_across_grid":
                    fresh_tokens_per_cell * len(trials),
                "launch": launch,
            }
        results = _read_committed_trials(trials)
    elif dispatch_mode == "wait":
        results = _dispatch(
            trials,
            EDU_DATA_ROOT,
            reuse_committed=True,
            teacher_cache_root=EDU_TEACHER_CACHE_ROOT,
            runtime_by_label=runtime_by_label,
        )
    else:
        raise RuntimeError(f"unsupported Hydra dispatch mode {dispatch_mode}")
    source_examples_seen = int(source_result.get(
        "examples_seen",
        source.steps * source.effective_batch,
    ))
    _audit_fresh_adam_continuations(
        trials,
        results,
        source_examples_seen=source_examples_seen,
    )
    ranking = _rank_cooled_full_validation(trials, results)
    summary = {
        "schema": "qwen-fullwidth-hydra-replacement-summary-v1",
        "status": "complete",
        "resolved_config_hash": root_hash,
        "dispatch_gate_sha256": gate["artifact_sha256"],
        "source": source.to_dict(),
        "source_examples_seen": source_examples_seen,
        "teacher_cache_manifest_sha256":
            cache_manifest["manifest_sha256"],
        "ranking": ranking,
        "winner": ranking[0],
        "results": results,
    }
    summary["artifact_sha256"] = _canonical_payload_hash(summary)
    volume.reload()
    _write_json_artifact(
        f"hydra-replacement-summary-{root_hash}.json",
        summary,
    )
    volume.commit()
    return summary


def _run_kronecker_edu_objective_study():
    from qwen_fullwidth_distill.study import (
        kronecker_edu_lr_trials,
        kronecker_edu_objective_trials,
        kronecker_edu_optimizer_trials,
        select_kronecker_edu_lr_winner,
        select_kronecker_edu_stage_winner,
    )

    lr_results = _read_committed_trials(kronecker_edu_lr_trials())
    lr_source = select_kronecker_edu_lr_winner(lr_results)
    optimizer_trials = kronecker_edu_optimizer_trials(lr_source)
    optimizer_results = _read_committed_trials(optimizer_trials)
    source = select_kronecker_edu_stage_winner(
        optimizer_trials,
        optimizer_results,
    )
    trials = kronecker_edu_objective_trials(source)
    results = _dispatch(
        trials,
        EDU_DATA_ROOT,
        reuse_committed=True,
        teacher_cache_root=EDU_TEACHER_CACHE_ROOT,
    )
    winner = select_kronecker_edu_stage_winner(trials, results)
    summary = {
        "source": source.to_dict(),
        "trials": results,
        "winner": winner.to_dict(),
    }
    volume.reload()
    _write_json_artifact("kronecker-edu-objective-summary.json", summary)
    volume.commit()
    return summary


def _kronecker_edu_architecture_benchmarks(trials):
    """Cache one-step H100 gates for the exact architecture slate."""
    path = Path(OUTPUT_ROOT) / "kronecker-edu-architecture-benchmarks.json"
    expected_labels = [trial.architecture.label for trial in trials]
    volume.reload()
    try:
        cached = json.loads(path.read_text())
    except (FileNotFoundError, json.JSONDecodeError, OSError):
        cached = None
    if (
        isinstance(cached, list)
        and [row.get("architecture_label") for row in cached]
        == expected_labels
    ):
        return cached
    payloads = [
        trial.to_dict()
        | {
            "_data_root": EDU_DATA_ROOT,
            "_kronecker_rank_chunk": 1024,
            "_kronecker_microbatch": trial.effective_batch,
        }
        for trial in trials
    ]
    outputs = list(kronecker_edu_architecture_benchmark_remote.map(
        payloads,
        order_outputs=True,
        return_exceptions=True,
    ))
    failures = [
        f"{trial.architecture.label}: {output!r}"
        for trial, output in zip(trials, outputs, strict=True)
        if isinstance(output, BaseException)
    ]
    if failures:
        raise RuntimeError(
            "architecture H100 benchmark gate failed: "
            + "; ".join(failures)
        )
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_text(json.dumps(outputs, indent=2, sort_keys=True))
    os.replace(temporary, path)
    volume.commit()
    return outputs


def _run_kronecker_edu_architecture_study():
    from qwen_fullwidth_distill.study import (
        kronecker_edu_architecture_trials,
        kronecker_edu_lr_trials,
        kronecker_edu_objective_trials,
        kronecker_edu_optimizer_trials,
        select_kronecker_edu_lr_winner,
        select_kronecker_edu_stage_winner,
    )

    lr_results = _read_committed_trials(kronecker_edu_lr_trials())
    lr_source = select_kronecker_edu_lr_winner(lr_results)
    optimizer_trials = kronecker_edu_optimizer_trials(lr_source)
    optimizer_source = select_kronecker_edu_stage_winner(
        optimizer_trials,
        _read_committed_trials(optimizer_trials),
    )
    objective_trials = kronecker_edu_objective_trials(optimizer_source)
    source = select_kronecker_edu_stage_winner(
        objective_trials,
        _read_committed_trials(objective_trials),
    )
    trials = kronecker_edu_architecture_trials(source)
    benchmarks = _kronecker_edu_architecture_benchmarks(trials)
    results = _dispatch(
        trials,
        EDU_DATA_ROOT,
        reuse_committed=True,
        teacher_cache_root=EDU_TEACHER_CACHE_ROOT,
    )
    winner = select_kronecker_edu_stage_winner(trials, results)
    summary = {
        "source": source.to_dict(),
        "benchmarks": benchmarks,
        "trials": results,
        "winner": winner.to_dict(),
    }
    volume.reload()
    _write_json_artifact("kronecker-edu-architecture-summary.json", summary)
    volume.commit()
    return summary


def _run_kronecker_edu_scale_study(target_examples: int):
    from qwen_fullwidth_distill.study import (
        KRONECKER_EDU_SCALE_BOUNDARIES,
        kronecker_edu_architecture_trials,
        kronecker_edu_lr_trials,
        kronecker_edu_objective_trials,
        kronecker_edu_optimizer_trials,
        kronecker_edu_scale_trials,
        select_kronecker_edu_lr_winner,
        select_kronecker_edu_stage_winner,
    )

    boundaries = dict(KRONECKER_EDU_SCALE_BOUNDARIES)
    if target_examples not in boundaries:
        raise ValueError("unsupported FineWeb-Edu scale target")
    lr_trials = kronecker_edu_lr_trials()
    lr_source = select_kronecker_edu_lr_winner(
        _read_committed_trials(lr_trials)
    )
    optimizer_trials = kronecker_edu_optimizer_trials(lr_source)
    optimizer_source = select_kronecker_edu_stage_winner(
        optimizer_trials,
        _read_committed_trials(optimizer_trials),
    )
    objective_trials = kronecker_edu_objective_trials(optimizer_source)
    objective_source = select_kronecker_edu_stage_winner(
        objective_trials,
        _read_committed_trials(objective_trials),
    )
    source_trials = kronecker_edu_architecture_trials(objective_source)
    source_results = _read_committed_trials(source_trials)
    for boundary_examples, keep in KRONECKER_EDU_SCALE_BOUNDARIES:
        trials = kronecker_edu_scale_trials(
            source_trials,
            source_results,
            target_examples=boundary_examples,
            keep=keep,
        )
        if boundary_examples == target_examples:
            results = _dispatch(
                trials,
                EDU_DATA_ROOT,
                reuse_committed=True,
                teacher_cache_root=EDU_TEACHER_CACHE_ROOT,
            )
            winner = select_kronecker_edu_stage_winner(trials, results)
            summary = {
                "target_examples": target_examples,
                "source_trials": [
                    trial.to_dict() for trial in source_trials
                ],
                "trials": results,
                "winner": winner.to_dict(),
            }
            volume.reload()
            _write_json_artifact(
                f"kronecker-edu-scale-{target_examples}-summary.json",
                summary,
            )
            volume.commit()
            return summary
        source_trials = trials
        source_results = _read_committed_trials(source_trials)
    raise AssertionError("unreachable scale boundary")


@app.function(
    image=image,
    volumes={"/cache": volume},
    timeout=86_400,
    memory=8_192,
)
def kronecker_edu_optimizer_study_remote():
    """Select the completed LR winner, cache the teacher, and tune AdamW."""
    volume.reload()
    return _run_kronecker_edu_optimizer_study()


@app.function(
    image=image,
    volumes={"/cache": volume},
    timeout=86_400,
    memory=8_192,
)
def kronecker_edu_objective_study_remote():
    """Tune hidden-state and softened-KL losses from the AdamW winner."""
    volume.reload()
    return _run_kronecker_edu_objective_study()


@app.function(
    image=image,
    volumes={"/cache": volume},
    timeout=86_400,
    memory=8_192,
)
def kronecker_edu_architecture_study_remote():
    """Run the exact eight-cell, approximately 84M architecture screen."""
    volume.reload()
    return _run_kronecker_edu_architecture_study()


@app.function(
    image=image,
    volumes={"/cache": volume},
    timeout=86_400,
    memory=8_192,
)
def kronecker_edu_scale_study_remote(target_examples: int):
    """Continue the validation-ranked 4/2/1 architecture frontier."""
    volume.reload()
    return _run_kronecker_edu_scale_study(target_examples)


@app.function(
    image=image,
    volumes={"/cache": volume},
    timeout=86_400,
    memory=8_192,
)
def kronecker_edu_push_study_remote():
    """Run every barrier in order, reusing all exact committed artifacts."""
    from qwen_fullwidth_distill.study import kronecker_edu_lr_trials

    volume.reload()
    _wait_for_committed_trials(kronecker_edu_lr_trials())
    optimizer = _run_kronecker_edu_optimizer_study()
    objective = _run_kronecker_edu_objective_study()
    architecture = _run_kronecker_edu_architecture_study()
    return {
        "optimizer": optimizer,
        "objective": objective,
        "architecture": architecture,
    }


@app.function(
    image=image,
    volumes={"/cache": volume},
    timeout=86_400,
    memory=8_192,
)
def hydra_replacement_study_remote(
    config_name: str = "edu_wsd_large_batch_lr",
    overrides: tuple[str, ...] = (),
):
    """Wait for the LR wave, gate artifacts, and dispatch the replacement."""
    volume.reload()
    return _run_hydra_replacement_pipeline(config_name, overrides)


@app.local_entrypoint()
def main(
    stage: str = "tests",
    run_id: str = "",
    lr: float = 0.0,
    config_name: str = "edu_wsd_large_batch_lr",
    overrides_json: str = "[]",
):
    from qwen_fullwidth_distill.config import ArchitectureConfig, TrialConfig
    from qwen_fullwidth_distill.study import (
        kronecker_lr_probe_trials,
        kronecker_edu_long_trial,
        kronecker_edu_lr_trials,
        kronecker_rank_boundary_trials,
        kronecker_rank_batch_boundary_trials,
        kronecker_rank_batch_lr_boundary_trials,
        kronecker_rank_batch_lr_continuation_trials,
        kronecker_rank_chunk_benchmark_trials,
        kronecker_rank_compile_benchmark_trial,
        kronecker_rank_continuation_trial,
        kronecker_rank_control_trials,
        kronecker_rank_depth_optimized_trials,
        kronecker_rank_depth_winner_continuation_trials,
        kronecker_rank_monarch_batch_trials,
        kronecker_rank_monarch_batch_continuation_trials,
        kronecker_rank_monarch_batch_frontier_trials,
        kronecker_rank_monarch_batch_long_trials,
        kronecker_rank_monarch_batch_million_trials,
        kronecker_rank_monarch_batch_three_million_trials,
        kronecker_rank_monarch_batch_two_million_trials,
        kronecker_rank_monarch_mature_continuation_trials,
        kronecker_rank_monarch_rank2_retry_trial,
        kronecker_rank_next_eight_trials,
        kronecker_rank_resume_after_spend_trials,
        kronecker_rank_monarch_batch_transition_trials,
        kronecker_rank_monarch_batch_transition_lr_trials,
        kronecker_rank_monarch_capacity_checkpoint_trial,
        kronecker_rank_monarch_capacity_continuation_trial,
        kronecker_rank_monarch_capacity_trials,
        kronecker_rank_monarch_continuation_trial,
        kronecker_rank_monarch_decay_continuation_trial,
        kronecker_rank_monarch_decay_trials,
        kronecker_rank_monarch_depth_parameter_matched_trials,
        kronecker_rank_monarch_long_trial,
        kronecker_rank_monarch_lr_boundary_trials,
        kronecker_rank_monarch_loop_trials,
        kronecker_rank_monarch_scaled_depth_trial,
        kronecker_rank_probe_trials,
        screen_trials,
        select_common_topology,
        select_unrestricted_monarch,
        tuning_trials,
    )

    try:
        overrides = json.loads(overrides_json)
    except json.JSONDecodeError as error:
        raise ValueError("--overrides-json must be a JSON list of strings") from error
    if (
        not isinstance(overrides, list)
        or not all(isinstance(value, str) for value in overrides)
    ):
        raise ValueError("--overrides-json must be a JSON list of strings")
    if stage == "hydra-dry-run":
        print(json.dumps(
            _resolve_hydra_experiment(config_name, overrides),
            indent=2,
            sort_keys=True,
        ))
        return
    if stage == "hydra-replacement":
        print(json.dumps(
            hydra_replacement_study_remote.remote(
                config_name,
                tuple(overrides),
            ),
            indent=2,
            sort_keys=True,
        ))
        return
    if stage == "tests":
        print(tests_remote.remote())
        return
    if stage == "audit":
        print(json.dumps(audit_remote.remote(), indent=2, sort_keys=True))
        return
    if stage == "depth-audit":
        print(json.dumps(depth_audit_remote.remote(), indent=2, sort_keys=True))
        return
    if stage == "tensor-audit":
        print(json.dumps(tensor_audit_remote.remote(), indent=2, sort_keys=True))
        return
    if stage == "tensor-invariants":
        print(json.dumps(
            tensor_invariants_remote.remote(),
            indent=2,
            sort_keys=True,
        ))
        return
    if stage == "wandb-probe":
        print(json.dumps(wandb_probe_remote.remote(), indent=2, sort_keys=True))
        return
    if stage == "wandb-status":
        print(json.dumps(
            wandb_run_status_remote.remote(run_id),
            indent=2,
            sort_keys=True,
        ))
        return
    if stage == "kron-rank-status":
        print(json.dumps(
            kronecker_rank_status_remote.remote(),
            indent=2,
            sort_keys=True,
        ))
        return
    if stage == "kron-edu-status":
        print(json.dumps(
            kronecker_edu_status_remote.remote(),
            indent=2,
            sort_keys=True,
        ))
        return
    if stage in (
        "kron-rank-operator-profile",
        "kron-rank-operator-profile-cutensor",
    ):
        print(json.dumps(
            kronecker_operator_profile_remote.remote(
                "cutensor" if stage.endswith("-cutensor") else "torch"
            ),
            indent=2,
            sort_keys=True,
        ))
        return
    if stage == "kron-rank-cutensor-correctness":
        print(json.dumps(
            kronecker_cutensor_correctness_remote.remote(),
            indent=2,
            sort_keys=True,
        ))
        return
    if stage == "kron-rank-operator-benchmark":
        payloads = [
            {"backend": "cutensor", "rank_chunk": chunk}
            for chunk in (512, 640, 768, 896, 1024, 1204, 1600, 2407)
        ]
        print(json.dumps(
            list(kronecker_operator_benchmark_remote.map(
                payloads,
                order_outputs=True,
                return_exceptions=False,
            )),
            indent=2,
            sort_keys=True,
        ))
        return
    if stage == "kron-rank-fullstep-benchmark":
        base = next(
            trial
            for trial in kronecker_rank_depth_optimized_trials()
            if trial.architecture.depth == 4
        )
        # Representative fast-path boundary for the exact d4/r2407 stack.
        # Chunk 512 remains fast through batch 352, then crosses onto a much
        # slower cuTENSOR algorithm despite batch 384 still fitting in VRAM.
        choices = [
            ("cutensor", chunk, batch)
            for chunk, batch in (
                (512, 256),
                (512, 320),
                (512, 352),
                (512, 384),
            )
        ]
        payloads = [
            base.to_dict()
            | {
                "effective_batch": batch,
                "_data_root": DATA_ROOT,
                "_kronecker_backend": backend,
                "_kronecker_rank_chunk": chunk,
                "_kronecker_microbatch": batch,
            }
            for backend, chunk, batch in choices
        ]
        outputs = list(benchmark_trial_remote.map(
            payloads,
            order_outputs=True,
            return_exceptions=True,
        ))
        rows = [
            (
                output
                if isinstance(output, dict)
                else {
                    "status": "failed",
                    "effective_batch": batch,
                    "error": repr(output),
                }
            )
            for (_, _, batch), output in zip(
                choices, outputs, strict=True
            )
        ]
        summary_keys = (
            "effective_batch",
            "runtime_kronecker_rank_chunk",
            "runtime_kronecker_microbatch",
            "median_examples_per_second",
            "median_step_seconds",
            "peak_allocated_gib",
            "status",
            "error",
        )
        print(json.dumps(
            [
                {
                    key: row[key]
                    for key in summary_keys
                    if key in row
                }
                for row in rows
            ],
            indent=2,
            sort_keys=True,
        ))
        return
    if stage == "monarch-depth-status":
        print(json.dumps(
            monarch_depth_status_remote.remote(),
            indent=2,
            sort_keys=True,
        ))
        return
    if stage == "cache-probe":
        print(json.dumps(cache_probe_remote.remote(), indent=2, sort_keys=True))
        return
    if stage == "prepare":
        print(prepare_data_remote.remote())
        return
    if stage == "prepare-edu":
        print(prepare_data_remote.remote(edu=True))
        return
    if stage == "prepare-teacher-cache":
        print(json.dumps(
            prepare_teacher_cache_remote.remote(),
            indent=2,
            sort_keys=True,
        ))
        return
    if stage == "smoke":
        prepare_data_remote.remote(True)
        trials = [
            TrialConfig(
                ArchitectureConfig("dense", "sequential", 1),
                steps=2,
                stage="smoke",
                audit_every=1,
                smoke=True,
            ),
            TrialConfig(
                ArchitectureConfig("monarch", "sequential", 1),
                steps=2,
                stage="smoke",
                audit_every=1,
                smoke=True,
            ),
        ]
        print(json.dumps(_dispatch(trials, SMOKE_DATA_ROOT), indent=2))
        return
    if stage == "tensor-smoke":
        prepare_data_remote.remote(True)
        trials = [
            TrialConfig(
                ArchitectureConfig(
                    "btt",
                    "residual_ffn",
                    1,
                    4,
                    btt_cores=4,
                    btt_rank=1,
                ),
                lr=1e-5,
                seed=0,
                steps=4,
                stage="tensor_smoke",
                audit_every=3,
                compile_model=True,
                smoke=True,
                effective_batch=512,
                lr_parameterization="mup",
            ),
            TrialConfig(
                ArchitectureConfig(
                    "kronecker",
                    "residual_ffn",
                    1,
                    4,
                ),
                lr=1e-5,
                seed=0,
                steps=4,
                stage="tensor_smoke",
                audit_every=3,
                compile_model=True,
                smoke=True,
                effective_batch=512,
                lr_parameterization="mup",
            ),
        ]
        print(json.dumps(_dispatch(trials, SMOKE_DATA_ROOT), indent=2))
        return
    if stage == "kron-rank-smoke":
        prepare_data_remote.remote(True)
        trial = TrialConfig(
            ArchitectureConfig(
                "kronecker",
                "residual_ffn",
                64,
                4,
                kronecker_rank=140,
                kronecker_rank_chunk=32,
            ),
            lr=1e-5,
            seed=0,
            steps=2,
            stage="tensor_kron_rank_smoke",
            audit_every=1,
            compile_model=False,
            smoke=True,
            effective_batch=64,
            lr_parameterization="mup",
            allow_divergence=True,
        )
        print(json.dumps(_dispatch([trial], SMOKE_DATA_ROOT), indent=2))
        return
    if stage == "kron-rank":
        # Six parameter-matched cells: three depth/rank allocations and two
        # conservative base LRs below the unstable full-rank 1e-5 smoke.
        prepare_data_remote.remote()
        print(json.dumps(
            _dispatch(kronecker_rank_probe_trials({"effective_batch": 512})),
            indent=2,
        ))
        return
    if stage in (
        "kron-rank-control",
        "kron-rank-control-5e4",
        "kron-rank-control-1e3",
    ):
        # Two d4 Monarch controls at the same ~84M trainable parameters,
        # 65,536-example budget, data order, and validation cadence. The
        # production data was already validated by the active rank grid, so
        # do not consume a ninth H100 with a redundant preparation call.
        trials = kronecker_rank_control_trials({"effective_batch": 512})
        if stage == "kron-rank-control-5e4":
            trials = [trial for trial in trials if trial.lr == 5e-4]
        elif stage == "kron-rank-control-1e3":
            trials = [trial for trial in trials if trial.lr == 1e-3]
        print(json.dumps(
            _dispatch(trials),
            indent=2,
        ))
        return
    if stage in (
        "kron-rank-boundary",
        "kron-rank-boundary-6e6",
        "kron-rank-boundary-b256",
    ):
        # Production data is already present; avoid a redundant H100 data
        # preparation worker while the rank grid is active.
        trials = kronecker_rank_boundary_trials()
        if stage == "kron-rank-boundary-6e6":
            trials = [
                trial
                for trial in trials
                if trial.lr == 6e-6 and trial.effective_batch == 512
            ]
        elif stage == "kron-rank-boundary-b256":
            trials = [
                trial
                for trial in trials
                if trial.effective_batch == 256
            ]
        print(json.dumps(
            _dispatch(trials),
            indent=2,
        ))
        return
    if stage in (
        "kron-rank-batch-boundary",
        "kron-rank-batch-boundary-b32",
        "kron-rank-batch-boundary-b16",
    ):
        trials = kronecker_rank_batch_boundary_trials()
        if stage in (
            "kron-rank-batch-boundary-b32",
            "kron-rank-batch-boundary-b16",
        ):
            batch = 32 if stage.endswith("-b32") else 16
            trials = [
                trial for trial in trials
                if trial.effective_batch == batch
            ]
        print(json.dumps(
            _dispatch(trials),
            indent=2,
        ))
        return
    if stage in (
        "kron-rank-batch-lr-boundary",
        "kron-rank-batch-lr-boundary-b64-lr6e6",
        "kron-rank-batch-lr-boundary-b32-lr6e6",
        "kron-rank-batch-lr-boundary-b16-lr6e6",
        "kron-rank-batch-lr-boundary-b8-lr6e6",
        "kron-rank-batch-lr-boundary-b8",
    ):
        trials = kronecker_rank_batch_lr_boundary_trials()
        if stage == "kron-rank-batch-lr-boundary-b64-lr6e6":
            trials = [
                trial
                for trial in trials
                if trial.effective_batch == 64 and trial.lr == 6e-6
            ]
        elif stage == "kron-rank-batch-lr-boundary-b32-lr6e6":
            trials = [
                trial
                for trial in trials
                if trial.effective_batch == 32 and trial.lr == 6e-6
            ]
        elif stage == "kron-rank-batch-lr-boundary-b16-lr6e6":
            trials = [
                trial
                for trial in trials
                if trial.effective_batch == 16 and trial.lr == 6e-6
            ]
        elif stage == "kron-rank-batch-lr-boundary-b8-lr6e6":
            trials = [
                trial
                for trial in trials
                if trial.effective_batch == 8 and trial.lr == 6e-6
            ]
        elif stage == "kron-rank-batch-lr-boundary-b8":
            trials = [
                trial for trial in trials
                if trial.effective_batch == 8 and trial.lr == 3e-6
            ]
        print(json.dumps(
            _dispatch(trials),
            indent=2,
        ))
        return
    if stage in (
        "kron-rank-batch-lr-continue",
        "kron-rank-batch-lr-continue-6e6",
        "kron-rank-batch-lr-continue-12e6",
    ):
        trials = kronecker_rank_batch_lr_continuation_trials()
        if stage != "kron-rank-batch-lr-continue":
            lr = 6e-6 if stage.endswith("-6e6") else 1.2e-5
            trials = [trial for trial in trials if trial.lr == lr]
        print(json.dumps(_dispatch(trials), indent=2))
        return
    if stage in (
        "kron-rank-depth-optimized",
        "kron-rank-depth-optimized-d4",
        "kron-rank-depth-optimized-d8",
        "kron-rank-depth-optimized-d16",
        "kron-rank-depth-optimized-d64",
        "kron-rank-depth-optimized-d128",
    ):
        trials = kronecker_rank_depth_optimized_trials()
        if stage != "kron-rank-depth-optimized":
            depth = int(stage.rsplit("-d", 1)[1])
            trials = [
                trial
                for trial in trials
                if trial.architecture.depth == depth
            ]
        print(json.dumps(_dispatch(trials), indent=2))
        return
    if stage == "kron-rank-compile-benchmark":
        print(json.dumps(
            _benchmark_dispatch([kronecker_rank_compile_benchmark_trial()]),
            indent=2,
        ))
        return
    if stage in (
        "kron-rank-chunk-benchmark",
        "kron-rank-chunk-benchmark-small",
    ):
        trials = kronecker_rank_chunk_benchmark_trials()
        if stage == "kron-rank-chunk-benchmark-small":
            trials = [
                trial
                for trial in trials
                if trial.architecture.kronecker_rank_chunk <= 48
            ]
        print(json.dumps(
            _benchmark_dispatch(trials),
            indent=2,
        ))
        return
    if stage == "kron-rank-continue":
        # The step-96 progress checkpoint is archived separately before the
        # 128-step probe removes its rolling progress file.
        print(json.dumps(
            _dispatch([kronecker_rank_continuation_trial(96)]),
            indent=2,
        ))
        return
    if stage == "kron-rank-monarch-long":
        print(json.dumps(
            _dispatch([kronecker_rank_monarch_long_trial()]),
            indent=2,
        ))
        return
    if stage == "kron-rank-monarch-continue":
        print(json.dumps(
            _dispatch([kronecker_rank_monarch_continuation_trial(512)]),
            indent=2,
        ))
        return
    if stage in (
        "kron-rank-monarch-capacity",
        "kron-rank-monarch-capacity-depth",
        "kron-rank-monarch-capacity-rank",
    ):
        trials = kronecker_rank_monarch_capacity_trials()
        if stage == "kron-rank-monarch-capacity-depth":
            trials = [
                trial for trial in trials
                if trial.architecture.depth == 8
            ]
        elif stage == "kron-rank-monarch-capacity-rank":
            trials = [
                trial for trial in trials
                if trial.architecture.monarch_rank == 2
            ]
        print(json.dumps(_dispatch(trials), indent=2))
        return
    if stage == "kron-rank-monarch-capacity-depth-scaled":
        print(json.dumps(
            _dispatch([kronecker_rank_monarch_scaled_depth_trial()]),
            indent=2,
        ))
        return
    if stage == "kron-rank-monarch-capacity-checkpoint":
        print(json.dumps(
            _dispatch([
                kronecker_rank_monarch_capacity_checkpoint_trial()
            ]),
            indent=2,
        ))
        return
    if stage == "kron-rank-monarch-capacity-continue":
        print(json.dumps(
            _dispatch([
                kronecker_rank_monarch_capacity_continuation_trial()
            ]),
            indent=2,
        ))
        return
    if stage in (
        "kron-rank-monarch-batch",
        "kron-rank-monarch-batch-b128",
        "kron-rank-monarch-batch-b64",
        "kron-rank-monarch-batch-b32",
        "kron-rank-monarch-batch-r2-b32",
    ):
        trials = kronecker_rank_monarch_batch_trials()
        if stage != "kron-rank-monarch-batch":
            batch = int(stage.rsplit("-b", 1)[1])
            trials = [
                trial for trial in trials
                if trial.effective_batch == batch
            ]
            if "-r2-" in stage:
                trials = [
                    trial for trial in trials
                    if trial.architecture.monarch_rank == 2
                ]
        print(json.dumps(
            _dispatch(trials),
            indent=2,
        ))
        return
    if stage in (
        "kron-rank-monarch-batch-frontier",
        "kron-rank-monarch-batch-frontier-b32",
        "kron-rank-monarch-batch-frontier-b16",
    ):
        trials = kronecker_rank_monarch_batch_frontier_trials()
        if stage != "kron-rank-monarch-batch-frontier":
            batch = int(stage.rsplit("-b", 1)[1])
            trials = [
                trial for trial in trials
                if trial.effective_batch == batch
            ]
        print(json.dumps(_dispatch(trials), indent=2))
        return
    if stage in (
        "kron-rank-monarch-batch-continue",
        "kron-rank-monarch-batch-continue-1e3",
        "kron-rank-monarch-batch-continue-5e4",
    ):
        trials = kronecker_rank_monarch_batch_continuation_trials()
        if stage != "kron-rank-monarch-batch-continue":
            lr = 1e-3 if stage.endswith("-1e3") else 5e-4
            trials = [trial for trial in trials if trial.lr == lr]
        print(json.dumps(_dispatch(trials), indent=2))
        return
    if stage in (
        "kron-rank-monarch-batch-long",
        "kron-rank-monarch-batch-long-5e4",
        "kron-rank-monarch-batch-long-2p5e4",
    ):
        trials = kronecker_rank_monarch_batch_long_trials()
        if stage != "kron-rank-monarch-batch-long":
            lr = 5e-4 if stage.endswith("-5e4") else 2.5e-4
            trials = [trial for trial in trials if trial.lr == lr]
        print(json.dumps(_dispatch(trials), indent=2))
        return
    if stage in (
        "kron-rank-monarch-batch-million",
        "kron-rank-monarch-batch-million-2p5e4",
        "kron-rank-monarch-batch-million-1p25e4",
    ):
        trials = kronecker_rank_monarch_batch_million_trials()
        if stage != "kron-rank-monarch-batch-million":
            lr = 2.5e-4 if stage.endswith("-2p5e4") else 1.25e-4
            trials = [trial for trial in trials if trial.lr == lr]
        print(json.dumps(_dispatch(trials), indent=2))
        return
    if stage in (
        "kron-rank-monarch-batch-two-million",
        "kron-rank-monarch-batch-two-million-1p25e4",
        "kron-rank-monarch-batch-two-million-6p25e5",
    ):
        trials = kronecker_rank_monarch_batch_two_million_trials()
        if stage != "kron-rank-monarch-batch-two-million":
            lr = 1.25e-4 if stage.endswith("-1p25e4") else 6.25e-5
            trials = [trial for trial in trials if trial.lr == lr]
        print(json.dumps(_dispatch(trials), indent=2))
        return
    if stage == "kron-rank-resume-after-spend":
        print(json.dumps(
            _dispatch(kronecker_rank_resume_after_spend_trials()),
            indent=2,
        ))
        return
    if stage == "kron-rank-next-eight":
        print(json.dumps(
            _dispatch(kronecker_rank_next_eight_trials()),
            indent=2,
        ))
        return
    if stage == "kron-rank-depth-winner-continue":
        print(json.dumps(
            _dispatch(kronecker_rank_depth_winner_continuation_trials()),
            indent=2,
        ))
        return
    if stage == "kron-edu-lr":
        print(json.dumps(kronecker_edu_lr_study_remote.remote(), indent=2))
        return
    if stage == "kron-edu-optimizer":
        print(json.dumps(
            kronecker_edu_optimizer_study_remote.remote(),
            indent=2,
        ))
        return
    if stage == "kron-edu-objective":
        print(json.dumps(
            kronecker_edu_objective_study_remote.remote(),
            indent=2,
        ))
        return
    if stage == "kron-edu-architecture":
        print(json.dumps(
            kronecker_edu_architecture_study_remote.remote(),
            indent=2,
        ))
        return
    edu_scale_stages = {
        "kron-edu-scale-262k": 262_144,
        "kron-edu-scale-1m": 1_048_576,
        "kron-edu-scale-4m": 4_194_304,
    }
    if stage in edu_scale_stages:
        print(json.dumps(
            kronecker_edu_scale_study_remote.remote(
                edu_scale_stages[stage]
            ),
            indent=2,
        ))
        return
    if stage == "kron-edu-push":
        call = kronecker_edu_push_study_remote.spawn()
        print(json.dumps({
            "function_call_id": call.object_id,
            "message": (
                "spawned artifact-gated FineWeb-Edu pipeline; "
                "the call continues after this entrypoint exits"
            ),
        }, indent=2))
        return
    if stage == "kron-edu-long":
        if lr <= 0:
            raise ValueError("kron-edu-long requires --lr from the pilot grid")
        prepare_data_remote.remote(edu=True)
        print(json.dumps(
            _dispatch([kronecker_edu_long_trial(lr)], EDU_DATA_ROOT),
            indent=2,
        ))
        return
    if stage == "kron-rank-monarch-mature-continue":
        print(json.dumps(
            _dispatch(kronecker_rank_monarch_mature_continuation_trials()),
            indent=2,
        ))
        return
    if stage == "kron-rank-monarch-batch-three-million":
        print(json.dumps(
            _dispatch(kronecker_rank_monarch_batch_three_million_trials()),
            indent=2,
        ))
        return
    if stage == "kron-rank-monarch-rank2-retry":
        print(json.dumps(
            _dispatch([kronecker_rank_monarch_rank2_retry_trial()]),
            indent=2,
        ))
        return
    if stage in (
        "kron-rank-monarch-depth-parameter-matched",
        "kron-rank-monarch-depth-parameter-matched-1e3",
        "kron-rank-monarch-depth-parameter-matched-5e4",
    ):
        trials = kronecker_rank_monarch_depth_parameter_matched_trials()
        if stage != "kron-rank-monarch-depth-parameter-matched":
            lr = 1e-3 if stage.endswith("-1e3") else 5e-4
            trials = [trial for trial in trials if trial.lr == lr]
        print(json.dumps(_dispatch(trials), indent=2))
        return
    if stage in (
        "kron-rank-monarch-batch-transition",
        "kron-rank-monarch-batch-transition-b512",
        "kron-rank-monarch-batch-transition-b128",
    ):
        trials = kronecker_rank_monarch_batch_transition_trials()
        if stage != "kron-rank-monarch-batch-transition":
            batch = int(stage.rsplit("-b", 1)[1])
            trials = [
                trial for trial in trials
                if trial.effective_batch == batch
            ]
        print(json.dumps(_dispatch(trials), indent=2))
        return
    if stage in (
        "kron-rank-monarch-batch-transition-lr",
        "kron-rank-monarch-batch-transition-lr-7p5e5",
        "kron-rank-monarch-batch-transition-lr-3p75e5",
        "kron-rank-monarch-batch-transition-lr-b64",
        "kron-rank-monarch-batch-transition-lr-b32",
    ):
        trials = kronecker_rank_monarch_batch_transition_lr_trials()
        if stage != "kron-rank-monarch-batch-transition-lr":
            if stage.endswith("-b64") or stage.endswith("-b32"):
                batch = 64 if stage.endswith("-b64") else 32
                trials = [
                    trial for trial in trials
                    if trial.effective_batch == batch
                ]
            else:
                lr = 7.5e-5 if stage.endswith("-7p5e5") else 3.75e-5
                trials = [trial for trial in trials if trial.lr == lr]
        print(json.dumps(_dispatch(trials), indent=2))
        return
    if stage in (
        "kron-rank-monarch-decay",
        "kron-rank-monarch-decay-5e4",
        "kron-rank-monarch-decay-3e4",
    ):
        trials = kronecker_rank_monarch_decay_trials()
        if stage != "kron-rank-monarch-decay":
            lr = 5e-4 if stage.endswith("-5e4") else 3e-4
            trials = [trial for trial in trials if trial.lr == lr]
        print(json.dumps(
            _dispatch(trials),
            indent=2,
        ))
        return
    monarch_decay_continuations = {
        "kron-rank-monarch-decay-continue-5e4": (5e-4, 5e-4),
        "kron-rank-monarch-decay-continue-5e4-to-2p5e4": (5e-4, 2.5e-4),
        "kron-rank-monarch-decay-continue-3e4": (3e-4, 3e-4),
        "kron-rank-monarch-decay-continue-3e4-to-1p5e4": (3e-4, 1.5e-4),
    }
    if stage in monarch_decay_continuations:
        source_lr, target_lr = monarch_decay_continuations[stage]
        trial = kronecker_rank_monarch_decay_continuation_trial(
            source_lr,
            target_lr,
        )
        print(json.dumps(_dispatch([trial]), indent=2))
        return
    if stage == "kron-rank-monarch-lr-boundary":
        print(json.dumps(
            _dispatch(kronecker_rank_monarch_lr_boundary_trials()),
            indent=2,
        ))
        return
    if stage == "kron-rank-monarch-loop":
        print(json.dumps(
            _dispatch(kronecker_rank_monarch_loop_trials()),
            indent=2,
        ))
        return
    if stage == "kron-probe":
        # This diagnostic can run before the depth study has selected its
        # final batch size. Keep its artifact namespace separate so a batch-512
        # early look can never contaminate the audited tensor frontier.
        from dataclasses import replace

        prepare_data_remote.remote()
        reference = {"effective_batch": 512}
        trials = [
            replace(trial, stage="tensor_kron_early")
            for trial in kronecker_lr_probe_trials(reference)
        ]
        print(json.dumps(
            _dispatch(trials),
            indent=2,
        ))
        return
    if stage == "screen":
        prepare_data_remote.remote()
        print(json.dumps(_dispatch(screen_trials()), indent=2))
        return
    if stage == "study":
        print(json.dumps(study_remote.remote(), indent=2))
        return
    if stage == "depth":
        print(json.dumps(depth_study_remote.remote(), indent=2))
        return
    if stage == "tensor":
        print(json.dumps(tensor_study_remote.remote(), indent=2))
        return
    if stage not in ("study", "depth", "tensor"):
        raise ValueError(
            "stage must be tests, audit, depth-audit, tensor-audit, "
            "tensor-invariants, wandb-probe, wandb-status, "
            "kron-rank-status, monarch-depth-status, prepare, prepare-edu, "
            "prepare-teacher-cache, smoke, "
            "tensor-smoke, "
            "kron-rank-smoke, kron-rank, kron-rank-control[-5e4|-1e3], "
            "kron-rank-boundary[-6e6|-b256], "
            "kron-rank-batch-boundary[-b32|-b16], "
            "kron-rank-batch-lr-boundary, "
            "kron-rank-chunk-benchmark[-small], "
            "kron-rank-compile-benchmark, "
            "kron-rank-continue, "
            "kron-rank-monarch-long, kron-rank-monarch-lr-boundary, "
            "kron-rank-monarch-loop, kron-rank-monarch-continue, "
            "kron-rank-monarch-decay, kron-probe, "
            "kron-edu-lr, kron-edu-optimizer, kron-edu-objective, "
            "kron-edu-architecture, kron-edu-scale-{262k|1m|4m}, "
            "kron-edu-push, "
            "hydra-dry-run, hydra-replacement, "
            "screen, cache-probe, "
            "study, depth, or tensor"
        )
