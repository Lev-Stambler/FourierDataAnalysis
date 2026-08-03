"""KISS H100 runner for 16-token full-vocabulary Walsh KL distillation."""

from __future__ import annotations

import modal

from fourier_full_next_token_kl.streaming import (
    CONTEXT_TOKENS, TOKEN_BITS, fresh_context_batch,
)


OUTPUT_BITS = 18
VOCAB_SIZE = 248_077
RAW_LOGIT_WIDTH = 248_320
TERMS = 524_288
BATCH_SIZE = 4_096
TEACHER_BATCH_SIZE = 256
CHAR_CHUNK = 8_192
STEPS = 2_000
AUDIT_EVERY = 50
STE_LR = 0.01
COEFFICIENT_LR = 0.003
MODEL_REVISION = "5c8a1b97ddef11f79b47ab9d07bf82b9117413f6"
TARGET_LAW = "qwen_x_only_context_16_qwen_5c8a1b9"
ROOT = "/cache/fourier_full_next_token_kl_16"
INPUT_CODE_PATH = (
    "/cache/fourier_full_next_token_kl/"
    "token_codes_b32_outtree18_s0_qwen5c8a1b9.npz"
)
REFERENCE_BYTES = 1_600_000_000
ARTIFACT_BYTE_BUDGET = 32_000_000
CAPACITY_TERMS = 3_950_000
CAPACITY_BATCH = 512
CAPACITY_TEACHER_BATCH = 256
CAPACITY_CHAR_CHUNK = 8_192
CAPACITY_MAX_TOTAL_DEGREE = 4
CAPACITY_STE_LR = 0.03
CAPACITY_COEFFICIENT_LR = 0.01
CAPACITY_INITIAL_SCORE_GAP = 1.0
CAPACITY_EVAL_EXAMPLES = 2_048
CAPACITY_GATE_STEPS = 250
CAPACITY_STEPS = 8_000
CAPACITY_WALL_SECONDS = 25_200
HIGH_BATCH_ACCUMULATION_STEPS = 8
HIGH_BATCH_GATE_STEPS = 32
HIGH_BATCH_STEPS = 1_000
HIGH_BATCH_STE_LR = 0.03
HIGH_BATCH_COEFFICIENT_LR = 0.003
ADAM_EPS = 1e-8
GRADIENT_SAMPLE_ROWS = 4_096
EARLY_DIAGNOSTIC_STEPS = 50


app = modal.App("fourier-full-next-token-kl-16")
volume = modal.Volume.from_name("fda-cache", create_if_missing=True)
image = (
    modal.Image.debian_slim(python_version="3.11")
    .pip_install(
        "numpy>=1.26", "torch==2.10.0", "transformers>=5.13.1",
        "accelerate>=1.14", "datasets>=4.0", "safetensors", "sentencepiece",
        "wandb>=0.18", "pytest>=8.0",
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
        "TORCHINDUCTOR_CACHE_DIR": "/cache/torchinductor/full_next_kl_16",
    })
    .add_local_python_source("fda_exp")
    .add_local_python_source("fourier_full_next_token_kl")
)
try:
    WANDB_SECRET = [modal.Secret.from_name("wandb")]
except Exception:
    WANDB_SECRET = []
try:
    HF_SECRET = [modal.Secret.from_name("hf-token")]
except Exception:
    HF_SECRET = []


@app.function(
    image=image, gpu="H100", volumes={"/cache": volume}, timeout=7200,
    memory=32768, secrets=HF_SECRET,
)
def prepare_token_codes(output_path: str = INPUT_CODE_PATH, seed: int = 0):
    """Create or validate the pinned input codes and balanced output tree."""
    import hashlib
    import json
    import os
    from pathlib import Path

    import numpy as np
    import torch
    from fda_exp.qwen_argl import load_teacher
    from fourier_full_next_token_kl.model import (
        balanced_projection_vertices, validate_balanced_token_artifact,
    )

    volume.reload()
    if os.path.exists(output_path):
        with np.load(output_path, allow_pickle=False) as cached:
            packed = np.asarray(cached["lsh_codes_packed"], dtype=np.uint8)
            vertices = np.asarray(cached["output_vertices"], dtype=np.uint32)
            metadata = json.loads(str(cached["metadata_json"]))
        validate_balanced_token_artifact(
            packed, vertices, metadata, model_revision=MODEL_REVISION,
            vocab_size=VOCAB_SIZE, raw_logit_width=RAW_LOGIT_WIDTH,
            input_bits=TOKEN_BITS, output_bits=OUTPUT_BITS, seed=seed,
        )
        print(f"[codes] cached {output_path}", flush=True)
        return output_path

    teacher, _, q, raw_width = load_teacher(device="cuda", dtype="bfloat16")
    if q != VOCAB_SIZE or raw_width != RAW_LOGIT_WIDTH:
        raise RuntimeError(
            f"pinned teacher shape changed: q={q}, raw_width={raw_width}"
        )
    embedding = teacher.get_input_embeddings().weight[:q]
    mean = embedding.float().mean(0)
    generator = torch.Generator(device="cuda").manual_seed(seed)
    projection = torch.randn(
        TOKEN_BITS, embedding.shape[1], generator=generator,
        device="cuda", dtype=torch.float32,
    )
    codes = np.empty((q, TOKEN_BITS), dtype=np.uint8)
    projection_scores = np.empty((q, OUTPUT_BITS), dtype=np.float32)
    with torch.no_grad():
        for lo in range(0, q, 8192):
            hi = min(lo + 8192, q)
            value = (embedding[lo:hi].float() - mean) @ projection.t()
            codes[lo:hi] = (value > 0).cpu().numpy().astype(np.uint8)
            projection_scores[lo:hi] = value[:, :OUTPUT_BITS].cpu().numpy()
    packed = np.packbits(codes, axis=1, bitorder="little")
    seen = set()
    repairs = 0
    for token_id in range(q):
        key = packed[token_id].tobytes()
        if key in seen:
            repairs += 1
            nonce = 0
            while True:
                candidate = hashlib.shake_256(
                    f"full-next-lsh:{seed}:{token_id}:{nonce}".encode()
                ).digest(TOKEN_BITS // 8)
                if candidate not in seen:
                    packed[token_id] = np.frombuffer(candidate, dtype=np.uint8)
                    key = candidate
                    break
                nonce += 1
        seen.add(key)
    output_vertices = balanced_projection_vertices(
        projection_scores, output_bits=OUTPUT_BITS
    )
    metadata = {
        "model_revision": MODEL_REVISION,
        "vocab_size": q,
        "raw_logit_width": raw_width,
        "lsh_bits": TOKEN_BITS,
        "output_bits": OUTPUT_BITS,
        "seed": seed,
        "collision_repairs": repairs,
        "output_vertex_scheme":
            "balanced_recursive_embedding_projections",
        "packed_sha256": hashlib.sha256(packed.tobytes()).hexdigest(),
        "output_vertices_sha256":
            hashlib.sha256(output_vertices.tobytes()).hexdigest(),
    }
    validate_balanced_token_artifact(
        packed, output_vertices, metadata, model_revision=MODEL_REVISION,
        vocab_size=VOCAB_SIZE, raw_logit_width=RAW_LOGIT_WIDTH,
        input_bits=TOKEN_BITS, output_bits=OUTPUT_BITS, seed=seed,
    )
    Path(output_path).parent.mkdir(parents=True, exist_ok=True)
    temporary = output_path + ".tmp"
    with open(temporary, "wb") as handle:
        np.savez(
            handle, lsh_codes_packed=packed,
            output_vertices=output_vertices,
            metadata_json=np.asarray(json.dumps(metadata, sort_keys=True)),
        )
    os.replace(temporary, output_path)
    volume.commit()
    print(f"[codes] wrote {output_path}", flush=True)
    return output_path


@app.function(image=image, gpu="H100", timeout=7200, memory=65536)
def run_tests():
    import os
    import subprocess

    environment = dict(os.environ)
    environment["PYTHONPATH"] = "/root"
    result = subprocess.run(
        ["python", "-m", "pytest", "-q",
         "/root/fourier_full_next_token_kl/test_model.py"],
        env=environment, check=False, text=True, capture_output=True,
    )
    print(result.stdout, flush=True)
    if result.stderr:
        print(result.stderr, flush=True)
    if result.returncode:
        raise RuntimeError(f"tests failed with code {result.returncode}")
    return result.stdout


def _save_compact(path, compact, packed_codes, metadata):
    import json
    import os
    from pathlib import Path

    import numpy as np

    Path(path).parent.mkdir(parents=True, exist_ok=True)
    temporary = path + ".tmp"
    with open(temporary, "wb") as handle:
        np.savez(
            handle,
            schema=np.asarray(compact["schema"]),
            n_input_bits=np.asarray(compact["n_input_bits"], dtype=np.int32),
            output_bits=np.asarray(compact["output_bits"], dtype=np.uint8),
            vocab_size=np.asarray(compact["vocab_size"], dtype=np.int32),
            unigram_terms=np.asarray(compact["unigram_terms"], dtype=np.int32),
            input_degrees=compact["input_degrees"],
            input_index_bits=np.asarray(
                compact["input_index_bits"], dtype=np.uint8
            ),
            input_index_count=np.asarray(
                compact["input_index_count"], dtype=np.int32
            ),
            packed_input_indices=compact["packed_input_indices"],
            packed_output_frequency=compact["packed_output_frequency"],
            packed_token_vertices=compact["packed_token_vertices"],
            coefficient_fp16=compact["coefficient_fp16"],
            coefficient_scale=compact["coefficient_scale"],
            coefficient_block_size=np.asarray(
                compact["coefficient_block_size"], dtype=np.int32
            ),
            token_bias_fp16=compact["token_bias_fp16"],
            input_lsh_codes_packed=packed_codes,
            metadata_json=np.asarray(json.dumps(metadata, sort_keys=True)),
        )
    os.replace(temporary, path)


@app.function(
    image=image, gpu="H100", volumes={"/cache": volume}, timeout=28800,
    memory=65536, secrets=HF_SECRET + WANDB_SECRET,
)
def train_exact_kl(steps: int = STEPS, max_wall_seconds: int = 10_800,
                   run_label: str = "", smoke: bool = False,
                   ste_lr: float = STE_LR,
                   coefficient_lr: float = COEFFICIENT_LR,
                   wandb_group: str = "",
                   terms: int = TERMS,
                   batch_size: int = BATCH_SIZE,
                   teacher_batch_size: int = TEACHER_BATCH_SIZE,
                   char_chunk: int = CHAR_CHUNK,
                   support_layout: str = "joint_cartesian",
                   max_total_degree: int = 4,
                   initial_score_gap: float = 1.0,
                   eval_examples: int = CAPACITY_EVAL_EXAMPLES,
                   accumulation_steps: int = 1,
                   gate_mode: bool = False):
    import concurrent.futures
    import json
    import math
    import os
    import time
    from collections import deque

    import numpy as np
    import torch
    import wandb
    from datasets import load_dataset
    from transformers import AutoModelForImageTextToText, AutoTokenizer

    from fda_exp.qwen_argl import (
        FINEWEB_CONFIG, FINEWEB_ID, FINEWEB_REVISION, MODEL_ID,
    )
    from fourier_full_next_token_kl.model import (
        InputOutputWalshStudent, encode_compact_student,
        estimate_compact_artifact_bytes, exact_joint_collision_audit,
        full_teacher_student_kl, hard_output_mask, hard_support_margin,
        hard_topk_mask,
        joint_cartesian_avg_input_degree, joint_cartesian_degree_counts,
        merge_duplicate_coefficients, support_flip_metrics,
        validate_balanced_token_artifact,
    )

    if (steps <= 0 or max_wall_seconds <= 0
            or min(terms, batch_size, teacher_batch_size, char_chunk) <= 0
            or max_total_degree < 2
            or not math.isfinite(ste_lr) or ste_lr <= 0
            or not math.isfinite(coefficient_lr) or coefficient_lr <= 0
            or not math.isfinite(initial_score_gap)
            or initial_score_gap <= 0
            or eval_examples <= 0
            or accumulation_steps <= 0):
        raise ValueError("training dimensions, wall time, and learning rates must be positive")
    terms = 4096 if smoke else terms
    batch_size = 64 if smoke else batch_size
    teacher_batch_size = 64 if smoke else teacher_batch_size
    char_chunk = 256 if smoke else char_chunk
    accumulation_steps = 1 if smoke else accumulation_steps
    audit_every = (
        min(steps, 3) if smoke
        else max(1, AUDIT_EVERY // accumulation_steps)
    )
    eval_examples = 128 if smoke else eval_examples
    final_examples = eval_examples
    run_name = run_label or (
        "kiss16-exact-kl-smoke" if smoke else (
            f"kiss16-exact-kl-x{terms}-b{batch_size}-"
            f"{support_layout}-d{max_total_degree}-"
            f"ste{ste_lr:g}-coeff{coefficient_lr:g}"
        )
    )

    torch.manual_seed(0)
    torch.set_float32_matmul_precision("high")
    torch.backends.cuda.matmul.allow_tf32 = True
    torch.backends.cudnn.allow_tf32 = True
    torch.backends.cuda.enable_flash_sdp(True)
    torch.backends.cuda.enable_mem_efficient_sdp(False)
    torch.backends.cuda.enable_math_sdp(False)
    torch.cuda.reset_peak_memory_stats()

    with np.load(INPUT_CODE_PATH, allow_pickle=False) as artifact:
        packed_codes = np.asarray(
            artifact["lsh_codes_packed"], dtype=np.uint8
        ).copy()
        token_vertices = np.asarray(
            artifact["output_vertices"], dtype=np.uint32
        ).copy()
        codebook_metadata = json.loads(str(artifact["metadata_json"]))
    validate_balanced_token_artifact(
        packed_codes, token_vertices, codebook_metadata,
        model_revision=MODEL_REVISION, vocab_size=VOCAB_SIZE,
        raw_logit_width=RAW_LOGIT_WIDTH, input_bits=TOKEN_BITS,
        output_bits=OUTPUT_BITS, seed=0,
    )
    token_code_array = np.unpackbits(
        packed_codes, axis=-1, count=TOKEN_BITS, bitorder="little"
    ).astype(np.uint8, copy=False)
    q = len(token_code_array)
    token_codes = torch.from_numpy(token_code_array).cuda(non_blocking=True)
    if support_layout != "joint_cartesian":
        raise ValueError("corrected exact-KL runner requires joint_cartesian")
    expected_degree_counts = joint_cartesian_degree_counts(
        terms, n_input_bits=CONTEXT_TOKENS * TOKEN_BITS,
        output_bits=OUTPUT_BITS, max_total_degree=max_total_degree,
    )

    config = {
        "objective": "exact_KL_teacher_to_student_full_vocabulary",
        "context_tokens": CONTEXT_TOKENS,
        "input_bits": CONTEXT_TOKENS * TOKEN_BITS,
        "output_bits": OUTPUT_BITS,
        "vocab_size": q,
        "terms": terms,
        "batch_size": batch_size,
        "micro_batch_size": batch_size,
        "effective_batch_size": batch_size * accumulation_steps,
        "accumulation_steps": accumulation_steps,
        "teacher_batch_size": teacher_batch_size,
        "steps": steps,
        "audit_every": audit_every,
        "support_layout": support_layout,
        "max_total_degree": max_total_degree,
        "initial_score_gap": initial_score_gap,
        "ste_lr": ste_lr,
        "coefficient_lr": coefficient_lr,
        "duplicate_handling": "audit_during_training_final_exact_merge_only",
        "merge_during_training": False,
        "backward_loss_scale": 1.0,
        "reported_loss_scale": 1.0,
        "diagnostic_sample_rows": min(terms, GRADIENT_SAMPLE_ROWS),
        "dense_diagnostic_steps": EARLY_DIAGNOSTIC_STEPS,
        "support_flip_metric":
            "post_adamw_exact_per_step_on_fixed_deterministic_sample",
        "eval_examples": eval_examples,
        "gate_mode": gate_mode,
        "wandb_group": wandb_group or "fourier-full-next-token-kl-16",
        "adam_betas": [0.9, 0.999],
        "adam_eps": ADAM_EPS,
        "adam_weight_decay": 0.01,
        "cached_examples": False,
        "cross_entropy_objective": False,
        "model_revision": MODEL_REVISION,
        "fineweb_revision": FINEWEB_REVISION,
        "target_law": TARGET_LAW,
        "output_vertex_scheme":
            codebook_metadata["output_vertex_scheme"],
        "output_vertices_sha256":
            codebook_metadata["output_vertices_sha256"],
    }
    projected_avg_input_degree = joint_cartesian_avg_input_degree(
        terms, n_input_bits=CONTEXT_TOKENS * TOKEN_BITS,
        output_bits=OUTPUT_BITS, max_total_degree=max_total_degree,
    )
    projected_artifact_bytes = estimate_compact_artifact_bytes(
        terms, vocab_size=q, max_total_degree=max_total_degree,
        output_bits=OUTPUT_BITS, n_input_bits=CONTEXT_TOKENS * TOKEN_BITS,
        input_code_bits=TOKEN_BITS,
        avg_input_degree=projected_avg_input_degree,
    )
    projected_compression = REFERENCE_BYTES / projected_artifact_bytes
    if projected_artifact_bytes > ARTIFACT_BYTE_BUDGET:
        raise ValueError(
            f"projected artifact {projected_artifact_bytes} bytes exceeds "
            f"budget {ARTIFACT_BYTE_BUDGET} bytes for terms={terms} "
            f"max_total_degree={max_total_degree}"
        )
    config["projected_artifact_bytes"] = projected_artifact_bytes
    config["projected_compression"] = projected_compression
    config["artifact_byte_budget"] = ARTIFACT_BYTE_BUDGET
    config["degree1_terms"] = expected_degree_counts["degree1"]
    config["degree2_terms"] = expected_degree_counts["degree2"]
    run = wandb.init(
        project="fda-fourier-noun",
        group=wandb_group or "fourier-full-next-token-kl-16",
        name=run_name, job_type="kiss16-exact-kl", config=config,
    )
    print(f"[wandb] {run.url}", flush=True)

    hf_token = (
        os.environ.get("HF_TOKEN") or os.environ.get("HF_HUB_TOKEN")
        or os.environ.get("HUGGING_FACE_HUB_TOKEN")
    )
    tokenizer = AutoTokenizer.from_pretrained(
        MODEL_ID, revision=MODEL_REVISION, token=hf_token
    )
    teacher = AutoModelForImageTextToText.from_pretrained(
        MODEL_ID, revision=MODEL_REVISION, dtype=torch.bfloat16,
        device_map="cuda", low_cpu_mem_usage=True, token=hf_token,
    ).eval()
    teacher.requires_grad_(False)
    teacher.set_attn_implementation({"text_config": "sdpa"})
    if len(tokenizer) != q:
        raise RuntimeError("teacher tokenizer and token codebook disagree")

    def eager_teacher(ids):
        output = teacher(
            input_ids=ids, use_cache=False, return_dict=True, logits_to_keep=1
        )
        return output.logits[:, -1, :q]

    teacher_forward = torch.compile(
        eager_teacher, fullgraph=False, dynamic=False
    )
    print(
        f"[compile] teacher batch={teacher_batch_size} context={CONTEXT_TOKENS}",
        flush=True,
    )
    with torch.inference_mode():
        teacher_forward(torch.zeros(
            teacher_batch_size, CONTEXT_TOKENS,
            device="cuda", dtype=torch.long,
        ))
    print("[compile] teacher ready", flush=True)

    model = InputOutputWalshStudent(
        CONTEXT_TOKENS * TOKEN_BITS, token_vertices, terms,
        output_bits=OUTPUT_BITS, unigram_terms=0, seed=0,
        max_total_degree=max_total_degree, char_chunk=char_chunk,
        support_layout=support_layout, token_bits=TOKEN_BITS,
        initial_score_gap=initial_score_gap, coefficient_std=0.02,
        checkpoint_chunks=False, compile_chunks=True,
        compile_transforms=True, parity_dtype=torch.bfloat16,
    ).cuda()
    if model.compile_error:
        raise RuntimeError(f"student compilation failed: {model.compile_error}")
    actual_degree_counts = {
        "degree1": int((model.input_degree == 1).sum()),
        "degree2": int((model.input_degree == 2).sum()),
    }
    total_degree = model.input_degree + model.output_degree
    support_invariants_ok = (
        actual_degree_counts == expected_degree_counts
        and int(total_degree.max()) <= max_total_degree
        and bool(torch.all(model.output_degree > 0))
    )
    if not support_invariants_ok:
        raise RuntimeError(
            "joint Cartesian degree invariant failed: "
            f"actual={actual_degree_counts}, expected={expected_degree_counts}, "
            f"max_total={int(total_degree.max())}"
        )
    print(f"[student] terms={terms} batch={batch_size}", flush=True)

    adam = {
        "betas": (0.9, 0.999), "eps": ADAM_EPS,
        "weight_decay": 0.01, "fused": True,
    }
    ste_optimizer = torch.optim.AdamW(
        [model.theta, model.output_theta], lr=ste_lr, **adam
    )
    coefficient_optimizer = torch.optim.AdamW(
        [model.coefficient, model.token_bias], lr=coefficient_lr, **adam
    )
    optimizers = (ste_optimizer, coefficient_optimizer)
    gradient_sample_rows = torch.linspace(
        0, terms - 1, min(terms, GRADIENT_SAMPLE_ROWS),
        device="cuda", dtype=torch.float64,
    ).long().unique()
    bias_sample_rows = torch.linspace(
        0, q - 1, min(q, GRADIENT_SAMPLE_ROWS),
        device="cuda", dtype=torch.float64,
    ).long().unique()
    sampled_parameters = {
        "theta": (model.theta, gradient_sample_rows, ste_optimizer),
        "output_theta": (
            model.output_theta, gradient_sample_rows, ste_optimizer
        ),
        "coefficient": (
            model.coefficient, gradient_sample_rows, coefficient_optimizer
        ),
        "token_bias": (
            model.token_bias, bias_sample_rows, coefficient_optimizer
        ),
    }
    input_two_steps_ago = None
    output_two_steps_ago = None
    input_ever_flipped = torch.zeros(
        len(gradient_sample_rows), dtype=torch.bool, device="cuda"
    )
    output_ever_flipped = torch.zeros_like(input_ever_flipped)
    input_transition_count = torch.zeros(
        len(gradient_sample_rows), dtype=torch.long, device="cuda"
    )
    output_transition_count = torch.zeros_like(input_transition_count)

    @torch.no_grad()
    def sampled_hard_masks():
        return (
            hard_topk_mask(
                model.theta.index_select(0, gradient_sample_rows),
                model.input_degree.index_select(0, gradient_sample_rows),
                model.max_input_degree,
            ),
            hard_output_mask(
                model.output_theta.index_select(0, gradient_sample_rows),
                model.output_degree.index_select(0, gradient_sample_rows),
                model.max_output_degree,
            ),
        )

    @torch.no_grad()
    def pre_optimizer_diagnostics():
        metrics = {
            "optim/backward_loss_scale": 1.0,
            "optim/effective_coefficient_rms": float(
                (model.coefficient.float() * model.output_scale).square().mean().sqrt()
            ),
        }
        snapshots = {}
        for name, (parameter, sampled_rows, _) in sampled_parameters.items():
            sampled_parameter = parameter.index_select(
                0, sampled_rows
            ).detach().float()
            snapshots[name] = sampled_parameter.clone()
            metrics[f"parameter/{name}_rms"] = float(
                sampled_parameter.square().mean().sqrt()
            )
            if parameter.grad is None:
                continue
            gradient = parameter.grad.index_select(
                0, sampled_rows
            ).float().abs()
            metrics[f"gradient/{name}_abs_mean"] = float(gradient.mean())
            metrics[f"gradient/{name}_rms"] = float(
                gradient.square().mean().sqrt()
            )
            metrics[f"gradient/{name}_fraction_le_adam_eps"] = float(
                (gradient <= ADAM_EPS).float().mean()
            )
            metrics[f"gradient/{name}_mean_over_adam_eps"] = float(
                gradient.mean() / ADAM_EPS
            )
        return metrics, snapshots

    @torch.no_grad()
    def post_optimizer_diagnostics(
        before_input_mask, before_output_mask, snapshots,
    ):
        nonlocal input_two_steps_ago, output_two_steps_ago
        after_input_mask, after_output_mask = sampled_hard_masks()
        metrics = {}
        for prefix, before, after, two_steps_ago in (
            ("input", before_input_mask, after_input_mask,
             input_two_steps_ago),
            ("output", before_output_mask, after_output_mask,
             output_two_steps_ago),
        ):
            transition = support_flip_metrics(
                before, after, two_steps_ago
            )
            metrics.update({
                f"support/{prefix}_{key}_step": value
                for key, value in transition.items()
            })

        input_changed = torch.any(
            before_input_mask != after_input_mask, dim=1
        )
        output_changed = torch.any(
            before_output_mask != after_output_mask, dim=1
        )
        input_ever_flipped.logical_or_(input_changed)
        output_ever_flipped.logical_or_(output_changed)
        input_transition_count.add_(input_changed)
        output_transition_count.add_(output_changed)
        metrics.update({
            "support/input_ever_flipped_fraction": float(
                input_ever_flipped.float().mean()
            ),
            "support/output_ever_flipped_fraction": float(
                output_ever_flipped.float().mean()
            ),
            "support/input_transitions_per_sampled_row": float(
                input_transition_count.float().mean()
            ),
            "support/output_transitions_per_sampled_row": float(
                output_transition_count.float().mean()
            ),
        })

        if snapshots:
            for name, (parameter, sampled_rows, optimizer) in (
                    sampled_parameters.items()):
                sampled_parameter = parameter.index_select(
                    0, sampled_rows
                ).detach().float()
                update = sampled_parameter - snapshots[name]
                metrics[f"update/{name}_abs_mean"] = float(
                    update.abs().mean()
                )
                metrics[f"update/{name}_rms"] = float(
                    update.square().mean().sqrt()
                )
                metrics[f"update/{name}_nonzero_fraction"] = float(
                    (update != 0).float().mean()
                )
                state = optimizer.state.get(parameter, {})
                for state_name in ("exp_avg", "exp_avg_sq"):
                    state_value = state.get(state_name)
                    if (torch.is_tensor(state_value)
                            and state_value.shape == parameter.shape):
                        sampled_state = state_value.index_select(
                            0, sampled_rows
                        ).float()
                        if state_name == "exp_avg_sq":
                            sampled_state = sampled_state.sqrt()
                        metrics[
                            f"adam/{name}_{state_name}_rms"
                        ] = float(sampled_state.square().mean().sqrt())

            for prefix, parameter, degree, maximum, lr in (
                ("input", model.theta, model.input_degree,
                 model.max_input_degree, ste_lr),
                ("output", model.output_theta, model.output_degree,
                 model.max_output_degree, ste_lr),
            ):
                margin = hard_support_margin(
                    parameter.index_select(0, gradient_sample_rows),
                    degree.index_select(0, gradient_sample_rows),
                    maximum,
                ).float()
                metrics.update({
                    f"support/{prefix}_margin_min": float(margin.min()),
                    f"support/{prefix}_margin_p10": float(
                        torch.quantile(margin, 0.10)
                    ),
                    f"support/{prefix}_margin_median": float(margin.median()),
                    f"support/{prefix}_margin_mean": float(margin.mean()),
                    f"support/{prefix}_margin_le_lr_fraction": float(
                        (margin <= lr).float().mean()
                    ),
                    f"support/{prefix}_margin_le_2lr_fraction": float(
                        (margin <= 2 * lr).float().mean()
                    ),
                })

        input_two_steps_ago = before_input_mask
        output_two_steps_ago = before_output_mask
        return metrics

    dataset = iter(load_dataset(
        FINEWEB_ID, name=FINEWEB_CONFIG, split="train", streaming=True,
        revision=FINEWEB_REVISION, token=hf_token,
    ))
    pending = deque()
    documents_seen = 0

    def next_contexts():
        nonlocal documents_seen
        value, consumed = fresh_context_batch(
            dataset, tokenizer, pending, batch_size, CONTEXT_TOKENS
        )
        documents_seen += consumed
        return torch.from_numpy(value)

    def encode(contexts):
        ids = contexts.to(device="cuda", dtype=torch.long, non_blocking=True)
        return token_codes[ids].reshape(len(ids), -1)

    def teacher_logits(contexts):
        pieces = []
        with torch.inference_mode():
            for lo in range(0, len(contexts), teacher_batch_size):
                ids = contexts[lo:lo + teacher_batch_size].to(
                    device="cuda", dtype=torch.long, non_blocking=True
                )
                pieces.append(teacher_forward(ids).detach())
        return torch.cat(pieces)

    @torch.no_grad()
    def evaluate_fresh(examples):
        was_training = model.training
        model.eval()
        total_kl = 0.0
        total = 0
        while total < examples:
            contexts = take_prefetched()
            count = min(len(contexts), examples - total)
            contexts = contexts[:count]
            target = teacher_logits(contexts)
            value = full_teacher_student_kl(model(encode(contexts)), target)
            total_kl += float(value) * count
            total += count
        if was_training:
            model.train()
        return total_kl / total

    first_contexts = next_contexts()
    first_target = teacher_logits(first_contexts)
    mean_probability = torch.softmax(first_target.float(), -1).mean(0)
    model.initialize_output_prior(mean_probability.clamp_min(1e-12).log())
    executor = concurrent.futures.ThreadPoolExecutor(
        max_workers=1, thread_name_prefix="fineweb"
    )
    future = executor.submit(next_contexts)

    def take_prefetched():
        nonlocal future
        value = future.result()
        future = executor.submit(next_contexts)
        return value

    mean_prior_kl = evaluate_fresh(eval_examples)
    run.log({
        "global_step": 0,
        "baseline/mean_prior_kl": mean_prior_kl,
        "baseline/eval_examples": eval_examples,
    })
    print(
        f"[baseline] fresh_examples={eval_examples} "
        f"mean_prior_kl={mean_prior_kl:.6f}",
        flush=True,
    )

    model.train()
    contexts = first_contexts
    target = first_target
    examples_trained = 0
    last_eval_kl = math.nan
    last_audit = exact_joint_collision_audit(model)
    loop_started = time.perf_counter()
    reserve_seconds = min(600, max_wall_seconds // 4)
    final_step = 0
    for step in range(1, steps + 1):
        step_started = time.perf_counter()
        for optimizer in optimizers:
            optimizer.zero_grad(set_to_none=True)
        step_kl = 0.0
        step_examples = 0
        microbatch_kls = []
        for micro_step in range(accumulation_steps):
            scores = model(encode(contexts))
            kl = full_teacher_student_kl(scores, target)
            if not bool(torch.isfinite(kl)):
                raise FloatingPointError(
                    f"non-finite exact KL at step {step}, "
                    f"micro_step {micro_step}: {float(kl.detach())}"
                )
            (kl / accumulation_steps).backward()
            microbatch_kl = float(kl.detach())
            microbatch_kls.append(microbatch_kl)
            step_kl += microbatch_kl / accumulation_steps
            step_examples += len(contexts)
            if micro_step + 1 < accumulation_steps:
                contexts = take_prefetched()
                target = teacher_logits(contexts)
        dense_diagnostics = (
            step <= EARLY_DIAGNOSTIC_STEPS or step % 5 == 0
        )
        before_input_mask, before_output_mask = sampled_hard_masks()
        if dense_diagnostics:
            pre_optimizer_metrics, parameter_snapshots = (
                pre_optimizer_diagnostics()
            )
        else:
            pre_optimizer_metrics, parameter_snapshots = {}, {}
        for optimizer in optimizers:
            optimizer.step()
        post_optimizer_metrics = post_optimizer_diagnostics(
            before_input_mask, before_output_mask, parameter_snapshots
        )
        torch.cuda.synchronize()
        elapsed = time.perf_counter() - step_started
        examples_trained += step_examples
        final_step = step
        microbatch_kl_variance = sum(
            (value - step_kl) ** 2 for value in microbatch_kls
        ) / len(microbatch_kls)
        run.log({
            "global_step": step,
            "train/kl": step_kl,
            "train/microbatch_kl_min": min(microbatch_kls),
            "train/microbatch_kl_max": max(microbatch_kls),
            "train/microbatch_kl_std": math.sqrt(microbatch_kl_variance),
            "stream/examples_trained": examples_trained,
            "stream/documents_seen": documents_seen,
            "performance/step_seconds": elapsed,
            "performance/examples_per_second": step_examples / elapsed,
            "performance/peak_memory_bytes": torch.cuda.max_memory_allocated(),
            "optim/ste_lr": ste_lr,
            "optim/coefficient_lr": coefficient_lr,
            **pre_optimizer_metrics,
            **post_optimizer_metrics,
        })
        if step <= EARLY_DIAGNOSTIC_STEPS or step % 5 == 0:
            print(
                f"[train] step={step} kl={step_kl:.6f} "
                f"effective_batch={step_examples} "
                f"in_flips={int(post_optimizer_metrics['support/input_rows_flipped_step'])} "
                f"out_flips={int(post_optimizer_metrics['support/output_rows_flipped_step'])} "
                f"micro_std={math.sqrt(microbatch_kl_variance):.4f} "
                f"examples/s={step_examples / elapsed:.1f}",
                flush=True,
            )

        if step % audit_every == 0:
            last_audit = exact_joint_collision_audit(model)
            if int(last_audit["active_rows"]) != terms:
                raise RuntimeError("training audit found deactivated characters")
            last_eval_kl = evaluate_fresh(eval_examples)
            run.log({
                "global_step": step,
                "eval/kl": last_eval_kl,
                **{
                    f"collision/full_{key}": value
                    for key, value in last_audit.items()
                },
            })
            print(
                f"[eval] step={step} kl={last_eval_kl:.6f} "
                f"duplicates={int(last_audit['duplicate_rows'])} "
                f"active={int(last_audit['active_rows'])}",
                flush=True,
            )

        if step == steps:
            break
        if (time.perf_counter() - loop_started
                >= max_wall_seconds - reserve_seconds):
            print(f"[wall] stopping after step={step}", flush=True)
            break
        contexts = take_prefetched()
        target = teacher_logits(contexts)

    before_final_merge = exact_joint_collision_audit(model)
    if int(before_final_merge["active_rows"]) != terms:
        raise RuntimeError("characters were deactivated before final export")
    verification_contexts = take_prefetched()[:min(batch_size, 64)]
    verification_target = teacher_logits(verification_contexts)
    model.eval()
    with torch.no_grad():
        verification_bits = encode(verification_contexts)
        scores_before_merge = model(verification_bits)
        kl_before_merge = full_teacher_student_kl(
            scores_before_merge, verification_target
        )
    final_merge = merge_duplicate_coefficients(model)
    with torch.no_grad():
        scores_after_merge = model(verification_bits)
        kl_after_merge = full_teacher_student_kl(
            scores_after_merge, verification_target
        )
        final_merge_max_logit_delta = float(
            (scores_after_merge - scores_before_merge).abs().max()
        )
        final_merge_kl_delta = float(
            (kl_after_merge - kl_before_merge).abs()
        )
    if final_merge_max_logit_delta > 2e-5:
        raise RuntimeError(
            "final exact merge changed logits by "
            f"{final_merge_max_logit_delta:.8g}"
        )
    final_audit = exact_joint_collision_audit(model)
    if int(final_audit["duplicate_rows"]) != 0:
        raise RuntimeError("final exact merge left duplicate characters")
    final_kl = evaluate_fresh(final_examples)
    executor.shutdown(wait=False, cancel_futures=True)
    gate_threshold = mean_prior_kl - 0.5
    gate_passed = (
        bool(math.isfinite(final_kl))
        and support_invariants_ok
        and final_audit["duplicate_fraction"] <= 0.005
        and final_kl <= gate_threshold
    )
    compact = encode_compact_student(model.sparse_state())
    model_path = f"{ROOT}/models/kiss16-x{terms}-{run.id}.npz"
    metadata = {
        **config,
        "run_id": run.id,
        "ste_lr": ste_lr,
        "coefficient_lr": coefficient_lr,
        "backward_loss_scale": 1.0,
        "final_step": final_step,
        "final_fresh_examples": final_examples,
        "final_fresh_kl": final_kl,
        "mean_prior_kl": mean_prior_kl,
        "gate_threshold_kl": gate_threshold,
        "gate_passed": gate_passed,
        "examples_trained": examples_trained,
        "documents_seen": documents_seen,
        "pre_merge_duplicate_rows": before_final_merge["duplicate_rows"],
        "final_merge_max_logit_delta": final_merge_max_logit_delta,
        "final_merge_kl_delta": final_merge_kl_delta,
        "active_terms": final_audit["active_rows"],
        "inactive_terms": final_audit["inactive_rows"],
        "unique_joint_supports": final_audit["unique_rows"],
        "duplicate_joint_support_fraction":
            final_audit["duplicate_fraction"],
    }
    _save_compact(model_path, compact, packed_codes, metadata)
    artifact_bytes = os.path.getsize(model_path)
    if artifact_bytes > ARTIFACT_BYTE_BUDGET:
        raise RuntimeError(
            f"final artifact {artifact_bytes} bytes exceeds "
            f"budget {ARTIFACT_BYTE_BUDGET} bytes"
        )
    compression = REFERENCE_BYTES / artifact_bytes
    volume.commit()
    run.log({
        "global_step": final_step,
        "final/kl": final_kl,
        "final/examples": final_examples,
        "final/mean_prior_kl": mean_prior_kl,
        "final/gate_threshold_kl": gate_threshold,
        "final/gate_passed": int(gate_passed),
        **{
            f"final/collision_{key}": value
            for key, value in final_audit.items()
        },
        **{
            f"final/merge_{key}": value
            for key, value in final_merge.items()
        },
        "final/merge_max_logit_delta": final_merge_max_logit_delta,
        "final/merge_kl_delta": final_merge_kl_delta,
        "export/artifact_bytes": artifact_bytes,
        "export/compression": compression,
        "export/projected_artifact_bytes": projected_artifact_bytes,
        "export/projected_compression": projected_compression,
    })
    run.summary.update({
        "final_kl": final_kl,
        "final_step": final_step,
        "validation_kl_at_most_1": final_kl <= 1.0,
        "mean_prior_kl": mean_prior_kl,
        "gate_threshold_kl": gate_threshold,
        "gate_passed": gate_passed,
        "duplicate_joint_support_fraction":
            final_audit["duplicate_fraction"],
        "active_terms": final_audit["active_rows"],
        "inactive_terms": final_audit["inactive_rows"],
        "examples_trained": examples_trained,
        "documents_seen": documents_seen,
        "pre_merge_duplicate_rows": before_final_merge["duplicate_rows"],
        "final_merge_max_logit_delta": final_merge_max_logit_delta,
        "final_merge_kl_delta": final_merge_kl_delta,
        "artifact_bytes": artifact_bytes,
        "compression": compression,
        "projected_artifact_bytes": projected_artifact_bytes,
        "projected_compression": projected_compression,
        "artifact_byte_budget": ARTIFACT_BYTE_BUDGET,
    })
    artifact = wandb.Artifact(
        f"fourier-full-next-token-kl-16-x{terms}-{run.id}",
        type="model", metadata=metadata,
    )
    artifact.add_file(model_path)
    run.log_artifact(artifact)
    result = {
        "wandb_url": run.url,
        "run_id": run.id,
        "ste_lr": ste_lr,
        "coefficient_lr": coefficient_lr,
        "backward_loss_scale": 1.0,
        "model_path": model_path,
        "final_step": final_step,
        "final_kl": final_kl,
        "last_eval_kl": last_eval_kl,
        "mean_prior_kl": mean_prior_kl,
        "gate_threshold_kl": gate_threshold,
        "gate_passed": gate_passed,
        "duplicate_joint_support_fraction":
            final_audit["duplicate_fraction"],
        "active_terms": final_audit["active_rows"],
        "inactive_terms": final_audit["inactive_rows"],
        "examples_trained": examples_trained,
        "documents_seen": documents_seen,
        "pre_merge_duplicate_rows": before_final_merge["duplicate_rows"],
        "final_merge_max_logit_delta": final_merge_max_logit_delta,
        "final_merge_kl_delta": final_merge_kl_delta,
        "artifact_bytes": artifact_bytes,
        "compression": compression,
        "projected_artifact_bytes": projected_artifact_bytes,
        "projected_compression": projected_compression,
    }
    run.finish()
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)
    return result


def _corrected_capacity_kwargs(run_label: str, *, gate: bool) -> dict:
    return {
        "steps": CAPACITY_GATE_STEPS if gate else CAPACITY_STEPS,
        "max_wall_seconds": CAPACITY_WALL_SECONDS,
        "run_label": run_label or (
            "kiss16-capacity-gate-x3950000-joint-cartesian"
            if gate else "kiss16-capacity-x3950000-joint-cartesian"
        ),
        "smoke": False,
        "ste_lr": CAPACITY_STE_LR,
        "coefficient_lr": CAPACITY_COEFFICIENT_LR,
        "wandb_group": "fourier-full-next-token-kl-16-corrected",
        "terms": CAPACITY_TERMS,
        "batch_size": CAPACITY_BATCH,
        "teacher_batch_size": CAPACITY_TEACHER_BATCH,
        "char_chunk": CAPACITY_CHAR_CHUNK,
        "support_layout": "joint_cartesian",
        "max_total_degree": CAPACITY_MAX_TOTAL_DEGREE,
        "initial_score_gap": CAPACITY_INITIAL_SCORE_GAP,
        "eval_examples": CAPACITY_EVAL_EXAMPLES,
        "gate_mode": gate,
    }


def _high_batch_kwargs(run_label: str, *, gate: bool) -> dict:
    value = _corrected_capacity_kwargs(run_label, gate=gate)
    value.update({
        "steps": HIGH_BATCH_GATE_STEPS if gate else HIGH_BATCH_STEPS,
        "run_label": run_label or (
            "kiss16-high-batch4096-gate"
            if gate else "kiss16-high-batch4096-full"
        ),
        "accumulation_steps": HIGH_BATCH_ACCUMULATION_STEPS,
        "ste_lr": HIGH_BATCH_STE_LR,
        "coefficient_lr": HIGH_BATCH_COEFFICIENT_LR,
    })
    return value


@app.function(image=image, timeout=32_400, memory=4096)
def train_corrected_capacity_gate(run_label: str = ""):
    """Run the fixed 250-step corrected-capacity acceptance gate."""
    return train_exact_kl.remote(
        **_corrected_capacity_kwargs(run_label, gate=True)
    )


@app.function(image=image, timeout=32_400, memory=4096)
def train_corrected_capacity(run_label: str = ""):
    """Run the fixed seven-hour corrected-capacity experiment."""
    return train_exact_kl.remote(
        **_corrected_capacity_kwargs(run_label, gate=False)
    )


@app.function(image=image, timeout=32_400, memory=4096)
def train_high_batch_gate(run_label: str = ""):
    """Matched 128k-example gate with effective batch 4,096."""
    return train_exact_kl.remote(
        **_high_batch_kwargs(run_label, gate=True)
    )


@app.function(image=image, timeout=32_400, memory=4096)
def train_high_batch(run_label: str = ""):
    """Seven-hour run with exact effective batch 4,096."""
    return train_exact_kl.remote(
        **_high_batch_kwargs(run_label, gate=False)
    )


@app.function(image=image, timeout=32_400, memory=4096)
def train_capacity_exact_kl(run_label: str = ""):
    """Compatibility alias for the corrected full-capacity run."""
    return train_corrected_capacity.remote(run_label=run_label)


@app.local_entrypoint()
def main(stage: str = "tests", steps: int = STEPS,
         max_wall_hours: float = 3.0, run_label: str = ""):
    if stage == "tests":
        print(run_tests.remote())
        return
    if stage not in {"smoke", "train", "gate", "capacity", "high-batch-gate",
                     "high-batch"}:
        raise ValueError(
            "stage must be tests, smoke, train, gate, capacity, "
            "high-batch-gate, or high-batch"
        )
    prepare_token_codes.remote(INPUT_CODE_PATH, 0)
    if stage == "high-batch-gate":
        print(train_high_batch_gate.remote(run_label=run_label))
        return
    if stage == "high-batch":
        print(train_high_batch.remote(run_label=run_label))
        return
    if stage == "gate":
        print(train_corrected_capacity_gate.remote(run_label=run_label))
        return
    if stage == "capacity":
        print(train_corrected_capacity.remote(run_label=run_label))
        return
    print(train_exact_kl.remote(
        steps=(3 if stage == "smoke" else steps),
        max_wall_seconds=(900 if stage == "smoke" else int(max_wall_hours * 3600)),
        run_label=run_label,
        smoke=stage == "smoke",
    ))
