"""Modal/H100 runner for the fresh output-conditioned Fourier student."""

from __future__ import annotations

import modal


app = modal.App("fourier-output-kl")
volume = modal.Volume.from_name("fda-cache", create_if_missing=True)
image = (
    modal.Image.debian_slim(python_version="3.11")
    .pip_install(
        "numpy>=1.26", "torch==2.10.0", "wandb>=0.18",
        "nvidia-ml-py>=12.0", "pytest>=8.0",
    )
    .env({"TORCHINDUCTOR_CACHE_DIR": "/cache/torchinductor/fourier_output_kl"})
    .add_local_python_source("fourier_output_kl")
)
try:
    WANDB_SECRET = [modal.Secret.from_name("wandb")]
except Exception:
    WANDB_SECRET = []

WEB_LABELS = (
    "/cache/fourier_noun/"
    "labeled_tr1000000_v8192_te8192_l128_b32_s0_schema8.pt"
)
EWT_LABELS = (
    "/cache/fourier_noun/"
    "labeled_tr90000_v8192_te8192_l128_b32_s0_schema8.pt"
)
ROOT = "/cache/fourier_output_kl"
REFERENCE_BYTES = 1_600_000_000
MAX_ARTIFACT_BYTES = 16_000_000


def _retry():
    return modal.Retries(
        max_retries=2, backoff_coefficient=2.0, initial_delay=1.0
    )


class _GpuSampler:
    def __init__(self):
        import threading
        self.values: list[float] = []
        self.memory: list[float] = []
        self.stop_event = threading.Event()
        self.thread = None

    def start(self):
        import threading
        try:
            import pynvml
            pynvml.nvmlInit()
            handle = pynvml.nvmlDeviceGetHandleByIndex(0)
        except Exception as error:
            print(f"[gpu-monitor] unavailable: {error!r}", flush=True)
            return

        def sample():
            while not self.stop_event.wait(0.1):
                try:
                    utilization = pynvml.nvmlDeviceGetUtilizationRates(handle)
                    memory = pynvml.nvmlDeviceGetMemoryInfo(handle)
                    self.values.append(float(utilization.gpu))
                    self.memory.append(float(memory.used))
                except Exception:
                    return

        self.thread = threading.Thread(target=sample, daemon=True)
        self.thread.start()

    def finish(self):
        import numpy as np
        self.stop_event.set()
        if self.thread is not None:
            self.thread.join(timeout=2)
        value = np.asarray(self.values, dtype=np.float64)
        memory = np.asarray(self.memory, dtype=np.float64)
        return {
            "gpu_util_mean": float(value.mean()) if len(value) else 0.0,
            "gpu_util_p50": float(np.quantile(value, 0.5)) if len(value) else 0.0,
            "gpu_util_p90": float(np.quantile(value, 0.9)) if len(value) else 0.0,
            "nvml_memory_peak_bytes": float(memory.max()) if len(memory) else 0.0,
        }


def _load_data(train_examples: int):
    import gc
    import numpy as np
    import torch

    if not torch.cuda.is_available():
        raise RuntimeError("output-KL training is intentionally H100-only")
    web = torch.load(WEB_LABELS, map_location="cpu", weights_only=False)
    ewt = torch.load(EWT_LABELS, map_location="cpu", weights_only=False)
    n_bits = int(web["metadata"]["n_bits"])
    if n_bits != 4096 or int(ewt["metadata"]["n_bits"]) != n_bits:
        raise RuntimeError(f"expected 4096 input bits, found {n_bits}")
    if web["metadata"]["lsh_sha256"] != ewt["metadata"]["lsh_sha256"]:
        raise RuntimeError("web and EWT caches use different token codes")

    def unpack(source, split: str, count: int | None = None):
        item = source["splits"][split]
        packed = item["packed_bits"][:count].numpy()
        bits = np.unpackbits(
            packed, axis=-1, count=n_bits, bitorder="little"
        ).astype(np.uint8, copy=False)
        probability = item["teacher_probability"][:count].float().numpy()
        return (
            torch.from_numpy(bits).cuda(non_blocking=True),
            torch.from_numpy(probability).cuda(non_blocking=True),
        )

    available = len(web["splits"]["train"]["teacher_probability"])
    if train_examples > available:
        raise ValueError(f"requested {train_examples} examples but cache has {available}")
    web_bits, web_probability = unpack(web, "train", train_examples)
    ewt_bits, ewt_probability = unpack(ewt, "train")
    val_bits, val_probability = unpack(web, "val")
    test_bits, test_probability = unpack(web, "test")
    codes = np.asarray(web["lsh_codes_packed"], dtype=np.uint8)
    unique_codes = len(np.unique(codes, axis=0))
    if unique_codes != len(codes):
        raise RuntimeError(f"token code collision: {unique_codes}/{len(codes)}")
    metadata = {
        "n_bits": n_bits,
        "web_train_examples": len(web_bits),
        "ewt_train_examples": len(ewt_bits),
        "total_unique_train_examples": len(web_bits) + len(ewt_bits),
        "val_examples": len(val_bits),
        "test_examples": len(test_bits),
        "lsh_bits": int(web["metadata"]["lsh_bits"]),
        "lsh_sha256": web["metadata"]["lsh_sha256"],
        "token_codes": len(codes),
        "unique_token_codes": unique_codes,
        "teacher_probability_dtype": str(
            web["splits"]["train"]["teacher_probability"].dtype
        ),
    }
    del web, ewt
    gc.collect()
    return (
        web_bits, web_probability, ewt_bits, ewt_probability,
        val_bits, val_probability,
        test_bits, test_probability, codes, metadata,
    )


def _gradient_norm(parameters):
    import torch
    pieces = [parameter.grad.detach().float().norm().square()
              for parameter in parameters if parameter.grad is not None]
    return float(torch.stack(pieces).sum().sqrt()) if pieces else 0.0


def _wsd_factor(step: int, total: int, warmup: int, decay: int,
                minimum: float) -> float:
    import math
    if step < warmup:
        return (step + 1) / max(1, warmup)
    stable_end = max(warmup, total - decay)
    if step < stable_end:
        return 1.0
    progress = min(1.0, (step - stable_end) / max(1, decay))
    return minimum + (1.0 - minimum) * 0.5 * (
        1.0 + math.cos(math.pi * progress)
    )


@app.function(
    image=image, gpu="H100", volumes={"/cache": volume}, timeout=21600,
    memory=65536, secrets=WANDB_SECRET, retries=_retry(),
)
def run_tests():
    import os
    import subprocess

    environment = dict(os.environ)
    environment["PYTHONPATH"] = "/root"
    result = subprocess.run(
        ["python", "-m", "pytest", "-q", "/root/fourier_output_kl/test_model.py"],
        env=environment, check=False, text=True, capture_output=True,
    )
    print(result.stdout, flush=True)
    if result.stderr:
        print(result.stderr, flush=True)
    if result.returncode:
        raise RuntimeError(f"remote tests failed with code {result.returncode}")
    return result.stdout


@app.function(
    image=image, gpu="H100", volumes={"/cache": volume}, timeout=28800,
    memory=65536, secrets=WANDB_SECRET, retries=_retry(),
)
def train_output_selector(
    terms: int = 131072,
    steps: int = 200,
    batch_size: int = 16384,
    char_chunk: int = 1024,
    mask_lr: float = 0.1,
    coefficient_lr: float = 0.01,
    diversity_weight: float = 1e-3,
    diversity_characters: int = 128,
    diversity_examples: int = 512,
    train_examples: int = 1_000_000,
    ewt_sampling_weight: int = 10,
    support_layout: str = "uniform",
    initial_score_gap: float = 0.02,
    seed: int = 0,
    run_label: str = "",
    resume_run_id: str = "",
    parent_run_id: str = "",
    parent_terms: int = 0,
    compile_chunks: bool = True,
):
    import dataclasses
    import gc
    import json
    import os
    import time
    from pathlib import Path

    import numpy as np
    import torch
    import wandb

    from fourier_output_kl.model import (
        OutputSelectorWalshStudent, distribution_metrics,
        encode_compact_student, functional_diversity_loss,
        load_compact_prefix, load_compact_student, repair_duplicate_supports,
        sampled_signature_audit, support_locality_audit,
        support_retention_audit, teacher_student_kl,
    )

    @dataclasses.dataclass(frozen=True)
    class Config:
        terms: int
        steps: int
        batch_size: int
        char_chunk: int
        mask_lr: float
        coefficient_lr: float
        diversity_weight: float
        diversity_characters: int
        diversity_examples: int
        train_examples: int
        ewt_sampling_weight: int
        support_layout: str
        initial_score_gap: float
        seed: int
        run_label: str
        parent_run_id: str
        parent_terms: int
        compile_chunks: bool
        max_degree: int = 8
        coefficient_std: float = 0.02
        mask_beta2: float = 0.95
        coefficient_beta2: float = 0.999
        coefficient_weight_decay: float = 1e-4
        clip_norm: float = 1.0
        wsd_minimum_lr_ratio: float = 0.05
        eval_every: int = 50
        checkpoint_every: int = 500
        large_bank_mask_schedule_fraction: float = 0.5
        large_bank_mask_decay_max: int = 100
        signature_examples: int = 2048
        signature_characters: int = 4096
        reference_bytes: int = REFERENCE_BYTES
        artifact_budget_bytes: int = MAX_ARTIFACT_BYTES

    config = Config(
        terms=terms, steps=steps, batch_size=batch_size,
        char_chunk=char_chunk, mask_lr=mask_lr,
        coefficient_lr=coefficient_lr,
        diversity_weight=diversity_weight,
        diversity_characters=diversity_characters,
        diversity_examples=diversity_examples,
        train_examples=train_examples, ewt_sampling_weight=ewt_sampling_weight,
        support_layout=support_layout, initial_score_gap=initial_score_gap,
        seed=seed, run_label=run_label, parent_run_id=parent_run_id,
        parent_terms=parent_terms,
        compile_chunks=compile_chunks,
    )
    if min(terms, steps, batch_size, char_chunk, train_examples) <= 0:
        raise ValueError("positive model/training sizes are required")
    if (mask_lr <= 0 or coefficient_lr <= 0 or diversity_weight < 0
            or ewt_sampling_weight < 0 or initial_score_gap <= 0):
        raise ValueError("invalid optimizer configuration")
    if terms > 786432:
        raise ValueError("dense single-H100 implementation is capped at 786432 terms")
    if resume_run_id and parent_run_id:
        raise ValueError("resume_run_id and parent_run_id are mutually exclusive")
    if parent_run_id and not 0 < (parent_terms or terms) <= terms:
        raise ValueError("parent_terms must be positive and no larger than terms")
    if parent_terms and not parent_run_id:
        raise ValueError("parent_terms requires parent_run_id")

    started = time.perf_counter()
    torch.manual_seed(seed)
    torch.set_float32_matmul_precision("high")
    torch.backends.cuda.matmul.allow_tf32 = True
    torch.backends.cudnn.allow_tf32 = True
    torch.cuda.reset_peak_memory_stats()
    data = _load_data(train_examples)
    (web_bits, web_probability, ewt_bits, ewt_probability,
     val_bits, val_probability,
     test_bits, test_probability, lsh_codes, data_metadata) = data
    weighted_ewt = ewt_sampling_weight * len(ewt_bits)
    ewt_fraction = weighted_ewt / (len(web_bits) + weighted_ewt)
    initial_probability = (
        float(web_probability.mean()) * len(web_bits)
        + float(ewt_probability.mean()) * weighted_ewt
    ) / (len(web_bits) + weighted_ewt)
    config_dict = dataclasses.asdict(config)
    config_dict["ewt_sampling_fraction"] = ewt_fraction
    config_dict.update(data_metadata)
    run_name = (
        run_label or
        f"output-kl-x{terms}-mlr{mask_lr:g}-clr{coefficient_lr:g}-s{seed}"
    )
    run = wandb.init(
        project="fda-fourier-noun", group="fourier-output-kl",
        name=run_name, job_type="output-kl-distillation",
        config=config_dict, id=resume_run_id or None,
        resume="allow" if resume_run_id else None,
    )
    print(f"[wandb] {run.url}", flush=True)
    monitor = _GpuSampler()
    monitor.start()
    model = OutputSelectorWalshStudent(
        data_metadata["n_bits"], terms, seed=seed, max_degree=config.max_degree,
        char_chunk=char_chunk,
        initial_probability=initial_probability,
        initial_score_gap=config.initial_score_gap,
        coefficient_std=config.coefficient_std,
        support_layout=config.support_layout,
        checkpoint_chunks=True, compile_chunks=compile_chunks,
    ).cuda()
    gc.collect()
    optimizer = torch.optim.AdamW([
        {
            "params": [model.theta], "lr": mask_lr, "weight_decay": 0.0,
            "betas": (0.9, config.mask_beta2),
        },
        {
            "params": [model.coefficient], "lr": coefficient_lr,
            "weight_decay": config.coefficient_weight_decay,
            "betas": (0.9, config.coefficient_beta2),
        },
        {
            "params": [model.bias], "lr": coefficient_lr,
            "weight_decay": 0.0, "betas": (0.9, config.coefficient_beta2),
        },
    ], fused=True, eps=1e-8)
    warmup_steps = 10 if steps <= 300 else 50
    decay_steps = min(30, steps // 4) if steps <= 300 else min(250, steps // 4)
    mask_schedule_steps = (
        steps if terms <= 131072 else
        max(warmup_steps + 1, round(
            steps * config.large_bank_mask_schedule_fraction
        ))
    )
    mask_decay_steps = (
        decay_steps if terms <= 131072 else
        min(config.large_bank_mask_decay_max, mask_schedule_steps // 4)
    )
    scheduler = torch.optim.lr_scheduler.LambdaLR(
        optimizer,
        [
            lambda step: _wsd_factor(
                step, mask_schedule_steps, warmup_steps, mask_decay_steps,
                config.wsd_minimum_lr_ratio,
            ),
            lambda step: _wsd_factor(
                step, steps, warmup_steps, decay_steps,
                config.wsd_minimum_lr_ratio,
            ),
            lambda step: _wsd_factor(
                step, steps, warmup_steps, decay_steps,
                config.wsd_minimum_lr_ratio,
            ),
        ],
    )
    run.config.update({
        "mask_schedule_steps": mask_schedule_steps,
        "mask_decay_steps": mask_decay_steps,
        "coefficient_decay_steps": decay_steps,
    })
    checkpoint_path = f"{ROOT}/checkpoints/{run.id}-x{terms}.pt"
    Path(checkpoint_path).parent.mkdir(parents=True, exist_ok=True)
    start_step = 0
    best_kl = float("inf")
    best_step = 0
    best_compact = None
    best_val_metrics = None
    generator = torch.Generator(device="cuda").manual_seed(seed + 1)
    if resume_run_id and os.path.exists(checkpoint_path):
        saved = torch.load(checkpoint_path, map_location="cpu", weights_only=False)
        with torch.no_grad():
            model.theta.copy_(saved["theta"].float().cuda())
            model.coefficient.copy_(saved["coefficient"].cuda())
            model.bias.copy_(saved["bias"].cuda())
        start_step = int(saved["step"])
        best_kl = float(saved["best_kl"])
        best_step = int(saved["best_step"])
        best_compact = saved.get("best_compact")
        best_val_metrics = saved.get("best_val_metrics")
        if "generator_state" in saved:
            generator.set_state(saved["generator_state"].cuda())
        for _ in range(start_step):
            scheduler.step()
        print(f"[resume] step={start_step} best_kl={best_kl:.6g}", flush=True)
    elif parent_run_id:
        resolved_parent_terms = parent_terms or terms
        parent_checkpoint_path = (
            f"{ROOT}/checkpoints/{parent_run_id}-x{resolved_parent_terms}.pt"
        )
        if not os.path.exists(parent_checkpoint_path):
            raise FileNotFoundError(
                f"parent checkpoint does not exist: {parent_checkpoint_path}"
            )
        parent = torch.load(
            parent_checkpoint_path, map_location="cpu", weights_only=False
        )
        best_compact = parent.get("best_compact")
        best_val_metrics = parent.get("best_val_metrics")
        if best_compact is None or best_val_metrics is None:
            raise RuntimeError("parent checkpoint has no selected compact model")
        loaded_terms = load_compact_prefix(
            model, best_compact, score_gap=config.initial_score_gap
        )
        parent_kl = float(best_val_metrics["kl"])
        best_step = 0
        growth_repair = repair_duplicate_supports(
            model, optimizer, seed=seed ^ terms ^ resolved_parent_terms
        )
        if loaded_terms < terms:
            # A smaller parent artifact cannot itself be exported as the child.
            # The initial evaluation selects the functionally equivalent grown
            # bank, including its zero-weight suffix.
            best_kl = float("inf")
            best_compact = None
            best_val_metrics = None
        else:
            best_kl = parent_kl
        print(
            f"[parent] run={parent_run_id} source_step={parent['best_step']} "
            f"source_terms={loaded_terms} target_terms={terms} "
            f"best_kl={parent_kl:.6g} reset_gap={config.initial_score_gap:g} "
            f"growth_repair={growth_repair['duplicates_repaired']}",
            flush=True,
        )

    @torch.no_grad()
    def evaluate(bits, probability, eval_batch: int = 1024):
        was_training = model.training
        model.eval()
        chunks = []
        for lo in range(0, len(bits), eval_batch):
            chunks.append(model(bits[lo:lo + eval_batch]).float().cpu())
        if was_training:
            model.train()
        scores = torch.cat(chunks)
        return scores, distribution_metrics(scores, probability.float().cpu())

    # The first compiled call may fail after torch.compile itself succeeded.
    # Probe it once and fall back to the same eager math without changing data.
    model.train()
    probe_index = torch.arange(min(64, len(web_bits)), device="cuda")
    try:
        with torch.no_grad():
            model(web_bits[probe_index])
    except Exception as error:
        print(f"[compile] eager fallback: {error!r}", flush=True)
        model.use_eager_chunks()
        model.compile_error = repr(error)
        with torch.no_grad():
            model(web_bits[probe_index])

    # Calibrate a fixed multiplier once.  AdamW is mostly scale invariant, but
    # this keeps clipping/epsilon behavior and reported norms in a useful range.
    calibration_batch = min(batch_size, 1024)
    calibration_ewt = round(calibration_batch * ewt_fraction)
    calibration_web = calibration_batch - calibration_ewt
    calibration_web_index = torch.randint(
        len(web_bits), (calibration_web,), generator=generator, device="cuda"
    )
    calibration_ewt_index = torch.randint(
        len(ewt_bits), (calibration_ewt,), generator=generator, device="cuda"
    )
    calibration_bits = torch.cat((
        web_bits[calibration_web_index], ewt_bits[calibration_ewt_index]
    ))
    calibration_probability = torch.cat((
        web_probability[calibration_web_index],
        ewt_probability[calibration_ewt_index],
    ))
    optimizer.zero_grad(set_to_none=True)
    calibration_kl = teacher_student_kl(
        model(calibration_bits), calibration_probability,
    )
    calibration_kl.backward()
    initial_mask_norm = _gradient_norm([model.theta])
    initial_coefficient_norm = _gradient_norm([model.coefficient, model.bias])
    loss_scale = float(np.clip(
        0.2 / max(initial_mask_norm, initial_coefficient_norm, 1e-8),
        0.01, 128.0,
    ))
    optimizer.zero_grad(set_to_none=True)
    run.log({
        "global_step": start_step,
        "grad/calibration_mask_norm": initial_mask_norm,
        "grad/calibration_coefficient_norm": initial_coefficient_norm,
        "optim/fixed_loss_scale": loss_scale,
        "compile/error": model.compile_error or "",
    })

    initial_support_rows = model.hard_index_rows().numpy()
    initial_scores, initial_metrics = evaluate(val_bits, val_probability)
    initial_locality = support_locality_audit(model)
    run.log({
        "global_step": start_step,
        **{f"val/{key}": value for key, value in initial_metrics.items()},
        **{f"locality/initial_{key}": value
           for key, value in initial_locality.items()},
    })
    if best_compact is None:
        best_kl = initial_metrics["kl"]
        best_step = start_step
        best_val_metrics = initial_metrics
        best_compact = encode_compact_student(model.sparse_state())

    recent_step_seconds: list[float] = []
    recent_mask_norms: list[float] = []
    recent_coefficient_norms: list[float] = []
    last_diversity: dict[str, float] = {}
    loop_started = time.perf_counter()
    ewt_batch = round(batch_size * ewt_fraction)
    web_batch = batch_size - ewt_batch
    for step in range(start_step, steps):
        step_started = time.perf_counter()
        web_index = torch.randint(
            len(web_bits), (web_batch,), generator=generator, device="cuda"
        )
        ewt_index = torch.randint(
            len(ewt_bits), (ewt_batch,), generator=generator, device="cuda"
        )
        bits = torch.cat((web_bits[web_index], ewt_bits[ewt_index]))
        target = torch.cat((
            web_probability[web_index], ewt_probability[ewt_index]
        ))
        optimizer.zero_grad(set_to_none=True)
        scores = model(bits)
        kl = teacher_student_kl(scores, target)
        diversity, diversity_metrics = functional_diversity_loss(
            model, bits[:min(diversity_examples, len(bits))],
            character_sample=diversity_characters,
            generator=generator, report=(step + 1) % 10 == 0,
        )
        loss = (kl + diversity_weight * diversity) * loss_scale
        loss.backward()
        mask_norm = torch.nn.utils.clip_grad_norm_([model.theta], config.clip_norm)
        coefficient_norm = torch.nn.utils.clip_grad_norm_(
            [model.coefficient, model.bias], config.clip_norm
        )
        optimizer.step()
        scheduler.step()
        current = step + 1
        torch.cuda.synchronize()
        step_seconds = time.perf_counter() - step_started
        recent_step_seconds.append(step_seconds)
        recent_mask_norms.append(float(mask_norm))
        recent_coefficient_norms.append(float(coefficient_norm))
        recent_step_seconds = recent_step_seconds[-50:]
        recent_mask_norms = recent_mask_norms[-50:]
        recent_coefficient_norms = recent_coefficient_norms[-50:]
        if diversity_metrics:
            last_diversity = diversity_metrics
        if current % 10 == 0:
            probability = scores.detach().softmax(-1)[:, 1]
            absolute = (probability - target).abs()
            run.log({
                "global_step": current,
                "train/kl": float(kl.detach()),
                "train/loss": float(loss.detach()),
                "train/probability_mae": float(absolute.mean()),
                "train/probability_abs_max": float(absolute.max()),
                "train/probability_abs_variance": float(absolute.var(unbiased=False)),
                "grad/mask_preclip": float(mask_norm),
                "grad/coefficient_bias_preclip": float(coefficient_norm),
                "grad/mask_clipped": float(mask_norm > config.clip_norm),
                "grad/coefficient_clipped": float(
                    coefficient_norm > config.clip_norm
                ),
                "optim/mask_lr": optimizer.param_groups[0]["lr"],
                "optim/coefficient_lr": optimizer.param_groups[1]["lr"],
                "performance/step_seconds": step_seconds,
                "performance/examples_per_second": batch_size / step_seconds,
                "performance/peak_memory_bytes": torch.cuda.max_memory_allocated(),
                **{f"diversity/{key}": value
                   for key, value in diversity_metrics.items()},
            })
        if current % config.eval_every == 0 or current == steps:
            repair = repair_duplicate_supports(
                model, optimizer, seed=seed * 1_000_003 + current
            )
            signature = sampled_signature_audit(
                model, val_bits[:config.signature_examples],
                character_sample=config.signature_characters,
                seed=seed + current,
            )
            locality = support_locality_audit(model)
            retention = support_retention_audit(model, initial_support_rows)
            val_scores, val_metrics = evaluate(val_bits, val_probability)
            improved = (
                val_metrics["kl"] < best_kl - 1e-8
                or (
                    abs(val_metrics["kl"] - best_kl) <= 1e-8
                    and best_val_metrics is not None
                    and val_metrics["balanced_agreement"]
                    > best_val_metrics["balanced_agreement"]
                )
            )
            if improved:
                best_kl = val_metrics["kl"]
                best_step = current
                best_val_metrics = val_metrics
                best_compact = encode_compact_student(model.sparse_state())
            run.log({
                "global_step": current,
                **{f"val/{key}": value for key, value in val_metrics.items()},
                **{f"repair/{key}": value for key, value in repair.items()},
                **{f"signature/{key}": value for key, value in signature.items()},
                **{f"locality/{key}": value for key, value in locality.items()},
                **{f"retention/{key}": value for key, value in retention.items()},
                "selection/best_val_kl": best_kl,
                "selection/best_step": best_step,
                "selection/improved": float(improved),
            })
            print(
                f"[eval] step={current} kl={val_metrics['kl']:.6f} "
                f"agree={val_metrics['agreement']:.4f} "
                f"best={best_kl:.6f}@{best_step}", flush=True,
            )
        if current % config.checkpoint_every == 0 or current == steps:
            checkpoint = {
                "step": current,
                "theta": model.theta.detach().to("cpu", dtype=torch.float16),
                "coefficient": model.coefficient.detach().cpu(),
                "bias": model.bias.detach().cpu(),
                "best_kl": best_kl, "best_step": best_step,
                "best_compact": best_compact,
                "best_val_metrics": best_val_metrics,
                "generator_state": generator.get_state().cpu(),
                "config": config_dict,
            }
            temporary = checkpoint_path + ".tmp"
            torch.save(checkpoint, temporary)
            os.replace(temporary, checkpoint_path)
            volume.commit()
            del checkpoint
            gc.collect()

    if best_compact is None:
        raise RuntimeError("training produced no selectable model")
    load_compact_student(model, best_compact, score_gap=config.initial_score_gap)
    model.eval()
    serialized_val_scores, serialized_val_metrics = evaluate(
        val_bits, val_probability
    )
    serialized_test_scores, serialized_test_metrics = evaluate(
        test_bits, test_probability
    )
    unique_supports = len(np.unique(model.hard_index_rows().numpy(), axis=0))
    if unique_supports != terms:
        raise RuntimeError("final model contains duplicate supports")

    model_path = f"{ROOT}/models/output-kl-x{terms}-{run.id}.npz"
    summary_path = f"{ROOT}/results/output-kl-x{terms}-{run.id}.json"
    Path(model_path).parent.mkdir(parents=True, exist_ok=True)
    Path(summary_path).parent.mkdir(parents=True, exist_ok=True)
    metadata = {
        "format": "centered-binary-output-selector-v1",
        "objective": "teacher_to_student_kl_after_two_class_log_softmax",
        "score_gauge": "[-gap/2,+gap/2]",
        "every_character_contains_output_selector": True,
        "terms": terms, "best_step": best_step,
        "config": config_dict, "data": data_metadata,
    }
    temporary = model_path + ".tmp"
    with open(temporary, "wb") as handle:
        np.savez(
            handle,
            schema=np.asarray(best_compact["schema"]),
            n_bits=np.asarray(best_compact["n_bits"], dtype=np.int32),
            degrees=best_compact["degrees"],
            index_bits=np.asarray(best_compact["index_bits"], dtype=np.uint8),
            index_count=np.asarray(best_compact["index_count"], dtype=np.int32),
            packed_indices=best_compact["packed_indices"],
            coefficient_fp16=best_compact["coefficient_fp16"],
            coefficient_scale=best_compact["coefficient_scale"],
            coefficient_block_size=np.asarray(
                best_compact["coefficient_block_size"], dtype=np.int32
            ),
            bias=np.asarray(best_compact["bias"], dtype=np.float32),
            lsh_codes_packed=lsh_codes,
            metadata_json=np.asarray(json.dumps(metadata)),
        )
    os.replace(temporary, model_path)
    artifact_bytes = os.path.getsize(model_path)
    compression = REFERENCE_BYTES / artifact_bytes
    if artifact_bytes > MAX_ARTIFACT_BYTES:
        raise RuntimeError(
            f"artifact is {artifact_bytes} bytes, above the 100x budget"
        )
    performance = monitor.finish()
    performance.update({
        "wall_seconds": time.perf_counter() - started,
        "loop_seconds": time.perf_counter() - loop_started,
        "torch_peak_memory_bytes": int(torch.cuda.max_memory_allocated()),
        "recent_step_seconds_p50": float(np.median(recent_step_seconds)),
        "recent_examples_per_second": (
            batch_size / float(np.median(recent_step_seconds))
        ),
        "recent_mask_grad_norm_p50": float(np.median(recent_mask_norms)),
        "recent_coefficient_grad_norm_p50": float(
            np.median(recent_coefficient_norms)
        ),
    })
    acceptance = {
        "kl_at_most_0p01": serialized_test_metrics["kl"] <= 0.01,
        "agreement_at_least_0p95": serialized_test_metrics["agreement"] >= 0.95,
        "positive_recall_at_least_0p90": (
            serialized_test_metrics["teacher_positive_recall"] >= 0.90
        ),
        "negative_recall_at_least_0p90": (
            serialized_test_metrics["teacher_negative_recall"] >= 0.90
        ),
        "compression_at_least_100x": compression >= 100.0,
        "all_supports_unique": unique_supports == terms,
    }
    summary = {
        "wandb_url": run.url, "run_id": run.id,
        "model_path": model_path, "checkpoint_path": checkpoint_path,
        "best_step": best_step, "best_float_val": best_val_metrics,
        "serialized_val": serialized_val_metrics,
        "serialized_test": serialized_test_metrics,
        "export": {
            "terms": terms, "unique_supports": unique_supports,
            "artifact_bytes": artifact_bytes,
            "compression_vs_1_6gb": compression,
            "unique_token_codes": data_metadata["unique_token_codes"],
            "val_score_quantization_max_delta": float(
                (serialized_val_scores - initial_scores).abs().max()
            ) if best_step == start_step else None,
        },
        "acceptance": acceptance, "performance": performance,
        "last_diversity": last_diversity,
        "initial_locality": initial_locality,
        "final_locality": support_locality_audit(model),
    }
    with open(summary_path + ".tmp", "w") as handle:
        json.dump(summary, handle, indent=2, sort_keys=True)
    os.replace(summary_path + ".tmp", summary_path)
    volume.commit()
    run.log({
        "global_step": steps,
        **{f"deployed_val/{key}": value
           for key, value in serialized_val_metrics.items()},
        **{f"test/{key}": value
           for key, value in serialized_test_metrics.items()},
        **{f"acceptance/{key}": float(value)
           for key, value in acceptance.items()},
        **{f"performance/{key}": value for key, value in performance.items()},
        "export/artifact_bytes": artifact_bytes,
        "export/compression": compression,
    })
    run.summary.update({
        "best_step": best_step, "best_val_kl": best_kl,
        "test_kl": serialized_test_metrics["kl"],
        "test_agreement": serialized_test_metrics["agreement"],
        "test_positive_recall": serialized_test_metrics["teacher_positive_recall"],
        "test_negative_recall": serialized_test_metrics["teacher_negative_recall"],
        "artifact_bytes": artifact_bytes, "compression": compression,
        **acceptance,
    })
    artifact = wandb.Artifact(
        f"fourier-output-kl-x{terms}-{run.id}", type="model",
        metadata=summary,
    )
    artifact.add_file(model_path)
    run.log_artifact(artifact)
    run.finish()
    print(json.dumps(summary, indent=2, sort_keys=True), flush=True)
    return summary


@app.function(image=image, secrets=WANDB_SECRET)
def compare_runs(results, label: str):
    import wandb
    ordered = sorted(results, key=lambda row: (
        row["serialized_val"]["kl"],
        -row["serialized_val"]["balanced_agreement"],
    ))
    run = wandb.init(
        project="fda-fourier-noun", group="fourier-output-kl",
        name=f"output-kl-{label}-comparison", job_type="comparison",
        config={"source_runs": [row["wandb_url"] for row in ordered]},
    )
    columns = [
        "terms", "val_kl", "val_agreement", "test_kl", "test_agreement",
        "positive_recall", "negative_recall", "compression", "wandb_url",
    ]
    rows = [[
        item["export"]["terms"], item["serialized_val"]["kl"],
        item["serialized_val"]["agreement"], item["serialized_test"]["kl"],
        item["serialized_test"]["agreement"],
        item["serialized_test"]["teacher_positive_recall"],
        item["serialized_test"]["teacher_negative_recall"],
        item["export"]["compression_vs_1_6gb"], item["wandb_url"],
    ] for item in ordered]
    run.log({"runs": wandb.Table(columns=columns, data=rows)})
    run.summary.update({
        "winner": ordered[0]["wandb_url"],
        "winner_val_kl": ordered[0]["serialized_val"]["kl"],
    })
    url = run.url
    run.finish()
    return {"wandb_url": url, "winner": ordered[0], "runs": ordered}


@app.local_entrypoint()
def main(stage: str = "tests", x: int = 131072, steps: int = 200,
         batch_size: int = 16384, char_chunk: int = 1024,
         mask_lr: float = 0.1, coefficient_lr: float = 0.01,
         diversity_weight: float = 1e-3, seed: int = 0,
         ewt_sampling_weight: int = 10, run_label: str = "",
         resume_run_id: str = "", support_layout: str = "uniform",
         initial_score_gap: float = 0.02, parent_run_id: str = "",
         parent_terms: int = 0):
    common = {
        "batch_size": batch_size, "char_chunk": char_chunk,
        "diversity_weight": diversity_weight, "seed": seed,
        "ewt_sampling_weight": ewt_sampling_weight,
        "support_layout": support_layout,
        "initial_score_gap": initial_score_gap,
    }
    if stage == "tests":
        print(run_tests.remote())
    elif stage == "smoke":
        print(train_output_selector.remote(
            terms=min(x, 4096), steps=min(steps, 10),
            batch_size=min(batch_size, 512), char_chunk=min(char_chunk, 256),
            mask_lr=mask_lr, coefficient_lr=coefficient_lr,
            diversity_weight=diversity_weight, diversity_characters=32,
            diversity_examples=128, train_examples=8192, seed=seed,
            run_label=run_label or "output-kl-smoke",
        ))
    elif stage == "train":
        print(train_output_selector.remote(
            terms=x, steps=steps, mask_lr=mask_lr,
            coefficient_lr=coefficient_lr, run_label=run_label,
            resume_run_id=resume_run_id, parent_run_id=parent_run_id,
            parent_terms=parent_terms, **common,
        ))
    elif stage == "sweep":
        calls = []
        for candidate_mask_lr in (0.03, 0.1, 0.3):
            for candidate_coefficient_lr in (0.003, 0.01):
                calls.append(train_output_selector.spawn(
                    terms=131072, steps=200,
                    mask_lr=candidate_mask_lr,
                    coefficient_lr=candidate_coefficient_lr,
                    run_label=(f"output-kl-lr-mlr{candidate_mask_lr:g}-"
                               f"clr{candidate_coefficient_lr:g}"),
                    **common,
                ))
        result = [call.get() for call in calls]
        print(compare_runs.remote(result, "lr"))
    elif stage == "scale":
        calls = [train_output_selector.spawn(
            terms=terms, steps=steps, mask_lr=mask_lr,
            coefficient_lr=coefficient_lr,
            run_label=f"output-kl-scale-x{terms}", **common,
        ) for terms in (262144, 524288, 786432)]
        result = [call.get() for call in calls]
        print(compare_runs.remote(result, "scale"))
    elif stage == "structured_sweep":
        calls = []
        for candidate_mask_lr in (0.03, 0.1):
            for candidate_gap in (0.1, 0.5):
                calls.append(train_output_selector.spawn(
                    terms=x, steps=steps, batch_size=batch_size,
                    char_chunk=char_chunk, mask_lr=candidate_mask_lr,
                    coefficient_lr=coefficient_lr,
                    diversity_weight=diversity_weight,
                    ewt_sampling_weight=ewt_sampling_weight,
                    support_layout="semantic_structured_v2",
                    initial_score_gap=candidate_gap, seed=seed,
                    run_label=(f"output-kl-structured-mlr{candidate_mask_lr:g}-"
                               f"gap{candidate_gap:g}"),
                ))
        result = [call.get() for call in calls]
        print(compare_runs.remote(result, "structured"))
    elif stage == "mobility_sweep":
        calls = []
        for candidate_mask_lr, candidate_gap in (
            (0.003, 0.5), (0.01, 0.5), (0.03, 2.0), (0.01, 2.0),
        ):
            calls.append(train_output_selector.spawn(
                terms=x, steps=steps, batch_size=batch_size,
                char_chunk=char_chunk, mask_lr=candidate_mask_lr,
                coefficient_lr=coefficient_lr,
                diversity_weight=diversity_weight,
                ewt_sampling_weight=ewt_sampling_weight,
                support_layout="semantic_structured_v2",
                initial_score_gap=candidate_gap, seed=seed,
                run_label=(f"output-kl-semantic-v2-mlr{candidate_mask_lr:g}-"
                           f"gap{candidate_gap:g}"),
            ))
        result = [call.get() for call in calls]
        print(compare_runs.remote(result, "semantic-v2-mobility"))
    elif stage == "coefficient_sweep":
        calls = []
        for candidate_coefficient_lr in (0.02, 0.03):
            calls.append(train_output_selector.spawn(
                terms=x, steps=steps, batch_size=batch_size,
                char_chunk=char_chunk, mask_lr=0.01,
                coefficient_lr=candidate_coefficient_lr,
                diversity_weight=diversity_weight,
                ewt_sampling_weight=ewt_sampling_weight,
                support_layout="semantic_structured_v2",
                initial_score_gap=2.0, seed=seed,
                run_label=(f"output-kl-semantic-v2-clr"
                           f"{candidate_coefficient_lr:g}"),
            ))
        result = [call.get() for call in calls]
        print(compare_runs.remote(result, "semantic-v2-coefficient"))
    else:
        raise SystemExit(f"unknown stage {stage!r}")
