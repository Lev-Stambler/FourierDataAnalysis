"""Minimal Modal runner for one directly trained Walsh character bank."""

from __future__ import annotations

import modal


app = modal.App("fourier-kiss")
volume = modal.Volume.from_name("fda-cache", create_if_missing=True)
image = (
    modal.Image.debian_slim(python_version="3.11")
    .pip_install(
        "numpy>=1.26", "torch==2.10.0", "wandb>=0.18",
        "nvidia-ml-py>=12.0",
    )
    .env({"TORCHINDUCTOR_CACHE_DIR": "/cache/torchinductor/fourier_kiss"})
    .add_local_python_source("fourier_kiss")
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
ROOT = "/cache/fourier_kiss"


class _GpuSampler:
    def __init__(self):
        import threading
        self.values: list[float] = []
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
                    self.values.append(float(
                        pynvml.nvmlDeviceGetUtilizationRates(handle).gpu
                    ))
                except Exception:
                    return

        self.thread = threading.Thread(target=sample, daemon=True)
        self.thread.start()

    def finish(self):
        self.stop_event.set()
        if self.thread is not None:
            self.thread.join(timeout=2)
        return self.values


@app.function(
    image=image, gpu="H100", volumes={"/cache": volume}, timeout=10800,
    memory=49152, secrets=WANDB_SECRET,
)
def train_direct(terms: int = 524288, steps: int = 1000,
                 batch_size: int = 16384, char_chunk: int = 4096,
                 seed: int = 0, mask_lr: float = 1.0,
                 coefficient_lr: float = 0.03,
                 diversity_weight: float = 0.05,
                 diversity_sample: int = 1024):
    import dataclasses
    import gc
    import json
    import math
    import os
    import time
    from pathlib import Path

    import numpy as np
    import torch
    import wandb

    from fourier_kiss.model import (
        DirectWalshStudent, binary_metrics, decode_compact_student,
        encode_compact_student, repair_duplicate_masks,
        sampled_diversity_loss,
    )

    @dataclasses.dataclass(frozen=True)
    class Config:
        terms: int
        steps: int
        batch_size: int
        char_chunk: int
        seed: int
        mask_lr: float
        coefficient_lr: float
        diversity_weight: float
        diversity_sample: int
        max_degree: int = 8
        mask_magnitude: float = 8.0
        mask_beta2: float = 0.95
        coefficient_beta2: float = 0.999
        coefficient_weight_decay: float = 1e-4
        loss_scale: float = 10.0
        clip_norm: float = 1.0
        warmup_steps: int = 20
        mask_delay_steps: int = 5
        mask_hold_steps: int = 50
        mask_decay_steps: int = 25
        mask_minimum_lr_ratio: float = 0.01
        coefficient_hold_steps: int = 50
        coefficient_decay_steps: int = 100
        minimum_lr_ratio: float = 1.0 / 3.0
        eval_every: int = 25
        ewt_sampling_weight: int = 10

    config = Config(
        terms=terms, steps=steps, batch_size=batch_size,
        char_chunk=char_chunk, seed=seed, mask_lr=mask_lr,
        coefficient_lr=coefficient_lr,
        diversity_weight=diversity_weight,
        diversity_sample=diversity_sample,
    )
    if terms <= 0 or steps <= 0 or batch_size <= 0:
        raise ValueError("terms, steps, and batch size must be positive")
    volume.reload()
    torch.manual_seed(seed)
    torch.set_float32_matmul_precision("high")
    torch.backends.cuda.matmul.allow_tf32 = True
    device = torch.device("cuda")

    web = torch.load(WEB_LABELS, map_location="cpu", weights_only=False)
    ewt = torch.load(EWT_LABELS, map_location="cpu", weights_only=False)
    n_bits = int(web["metadata"]["n_bits"])
    if n_bits != 4096 or int(ewt["metadata"]["n_bits"]) != n_bits:
        raise ValueError("KISS run requires the cached 4,096-bit layout")
    if web["metadata"]["lsh_sha256"] != ewt["metadata"]["lsh_sha256"]:
        raise ValueError("training sources have different token codes")

    def unpack_split(source, split):
        item = source["splits"][split]
        packed = item["packed_bits"].numpy()
        bits = np.unpackbits(
            packed, axis=-1, count=n_bits, bitorder="little"
        ).astype(np.uint8, copy=False)
        probability = item["teacher_probability"].float().numpy()
        return bits, probability

    def gpu_split(source, split):
        bits, probability = unpack_split(source, split)
        result = (
            torch.from_numpy(bits).to(device),
            torch.from_numpy(probability).to(device),
        )
        del bits, probability
        return result

    web_bits, web_probability = gpu_split(web, "train")
    ewt_bits, ewt_probability = gpu_split(ewt, "train")
    val_bits, val_probability = gpu_split(web, "val")
    test_bits, test_probability = gpu_split(web, "test")
    train_probe_bits = web_bits[:8192]
    train_probe_probability = web_probability[:8192]
    lsh_codes = web["lsh_codes_packed"]
    if torch.is_tensor(lsh_codes):
        lsh_codes = lsh_codes.numpy()
    lsh_codes = np.asarray(lsh_codes, dtype=np.uint8)
    unique_token_codes = len(np.unique(lsh_codes, axis=0))
    if unique_token_codes != len(lsh_codes):
        raise RuntimeError("cached vocabulary token codes are not unique")
    web_count, ewt_count = len(web_bits), len(ewt_bits)
    weighted_ewt = config.ewt_sampling_weight * ewt_count
    ewt_fraction = weighted_ewt / (web_count + weighted_ewt)
    initial_probability = (
        float(web_probability.mean()) * web_count
        + float(ewt_probability.mean()) * weighted_ewt
    ) / (web_count + weighted_ewt)
    del web, ewt
    gc.collect()

    run = wandb.init(
        project="fda-fourier-noun", group="fourier-kiss",
        name=f"fourier-kiss-x{terms}-s{seed}", job_type="direct-student",
        config={
            **dataclasses.asdict(config), "n_bits": n_bits,
            "web_labels": WEB_LABELS, "ewt_labels": EWT_LABELS,
            "objective": "original_teacher_soft_bce",
        },
    )
    monitor = _GpuSampler()
    monitor.start()
    started = time.perf_counter()
    model = DirectWalshStudent(
        n_bits, terms, seed=seed, max_degree=config.max_degree,
        char_chunk=char_chunk, initial_probability=initial_probability,
        mask_magnitude=config.mask_magnitude, checkpoint_chunks=True,
        compile_chunks=True,
    ).to(device)
    optimizer = torch.optim.AdamW(
        [
            {
                "params": [model.theta], "lr": mask_lr,
                "weight_decay": 0.0, "betas": (0.9, config.mask_beta2),
            },
            {
                "params": [model.coefficient], "lr": coefficient_lr,
                "weight_decay": config.coefficient_weight_decay,
                "betas": (0.9, config.coefficient_beta2),
            },
            {
                "params": [model.bias], "lr": coefficient_lr,
                "weight_decay": 0.0,
                "betas": (0.9, config.coefficient_beta2),
            },
        ],
        fused=True, eps=1e-8,
    )

    def mask_factor(step):
        if step < config.mask_delay_steps:
            return 0.0
        local = step - config.mask_delay_steps
        if local < config.warmup_steps:
            return (local + 1) / config.warmup_steps
        if step <= config.mask_hold_steps:
            return 1.0
        progress = min(
            1.0,
            (step - config.mask_hold_steps) / config.mask_decay_steps,
        )
        return config.mask_minimum_lr_ratio + (
            1.0 - config.mask_minimum_lr_ratio
        ) * 0.5 * (1.0 + math.cos(math.pi * progress))

    def coefficient_factor(step):
        if step < config.warmup_steps:
            return (step + 1) / config.warmup_steps
        if step <= config.coefficient_hold_steps:
            return 1.0
        progress = min(
            1.0,
            (step - config.coefficient_hold_steps)
            / config.coefficient_decay_steps,
        )
        return config.minimum_lr_ratio + (
            1.0 - config.minimum_lr_ratio
        ) * 0.5 * (1.0 + math.cos(math.pi * progress))

    scheduler = torch.optim.lr_scheduler.LambdaLR(
        optimizer,
        [
            mask_factor,
            coefficient_factor,
            coefficient_factor,
        ],
    )
    generator = torch.Generator(device=device).manual_seed(seed + 1)

    def sample_batch():
        ewt_batch = round(batch_size * ewt_fraction)
        web_batch = batch_size - ewt_batch
        web_index = torch.randint(
            web_count, (web_batch,), generator=generator, device=device
        )
        ewt_index = torch.randint(
            ewt_count, (ewt_batch,), generator=generator, device=device
        )
        return (
            torch.cat((web_bits[web_index], ewt_bits[ewt_index])),
            torch.cat((
                web_probability[web_index], ewt_probability[ewt_index]
            )),
        )

    @torch.no_grad()
    def evaluate(bits, probability, evaluation_batch=8192):
        model.eval()
        output = []
        for lo in range(0, len(bits), evaluation_batch):
            output.append(model(bits[lo:lo + evaluation_batch]).float().cpu())
        model.train()
        logits = torch.cat(output).numpy()
        return logits, binary_metrics(logits, probability.float().cpu().numpy())

    def exact_sparse_gpu(bits, state, evaluation_batch=8192):
        degree = np.asarray(state["degrees"], dtype=np.int64)
        indices = np.asarray(state["indices"], dtype=np.int64)
        offsets = np.concatenate(([0], np.cumsum(degree)))
        padded = np.zeros((len(degree), config.max_degree), dtype=np.int64)
        active = np.arange(config.max_degree)[None, :] < degree[:, None]
        row = np.repeat(np.arange(len(degree)), degree)
        column = np.arange(len(indices)) - np.repeat(offsets[:-1], degree)
        padded[row, column] = indices
        padded = torch.from_numpy(padded).to(device)
        active = torch.from_numpy(active).to(device)
        coefficient = torch.from_numpy(
            np.asarray(state["coefficient"], dtype=np.float32)
        ).to(device)
        outputs = []
        for batch_lo in range(0, len(bits), evaluation_batch):
            x = bits[batch_lo:batch_lo + evaluation_batch]
            output = torch.full(
                (len(x),), float(state["bias"]), device=device
            )
            for lo in range(0, len(degree), char_chunk):
                hi = min(lo + char_chunk, len(degree))
                selected = x[:, padded[lo:hi]]
                selected.mul_(active[None, lo:hi])
                parity = selected.sum(dim=2, dtype=torch.int32).bitwise_and_(1)
                output.add_((1.0 - 2.0 * parity.float()) @ coefficient[lo:hi])
            outputs.append(output.cpu())
        return torch.cat(outputs).numpy()

    initial_repair = repair_duplicate_masks(model, optimizer, seed + 10_000)
    initial_val_logits, initial_val = evaluate(val_bits, val_probability)
    initial_train_logits, initial_train = evaluate(
        train_probe_bits, train_probe_probability
    )
    run.log({
        "global_step": 0,
        **{f"val/{key}": value for key, value in initial_val.items()},
        **{f"train_probe/{key}": value for key, value in initial_train.items()},
        **{f"diversity/{key}": value for key, value in initial_repair.items()},
    })
    best_kl = initial_val["kl"]
    best_step = 0
    best_state = model.sparse_state()
    best_val_logits = initial_val_logits
    best_train_probe_ce = initial_train["ce"]
    last_log = time.perf_counter()
    last_diversity = {}
    recent_norms = []
    for step in range(steps):
        bits, target = sample_batch()
        optimizer.zero_grad(set_to_none=True)
        logits = model(bits)
        ce = torch.nn.functional.binary_cross_entropy_with_logits(logits, target)
        diversity, diversity_diagnostics = sampled_diversity_loss(
            model, sample_size=diversity_sample, generator=generator,
            report=((step + 1) % 10 == 0),
        )
        loss = config.loss_scale * (ce + diversity_weight * diversity)
        loss.backward()
        global_norm = torch.nn.utils.clip_grad_norm_(
            model.parameters(), config.clip_norm, foreach=True
        )
        optimizer.step()
        scheduler.step()
        current = step + 1
        recent_norms.append(float(global_norm))
        recent_norms = recent_norms[-100:]
        last_diversity = diversity_diagnostics
        if current % 10 == 0:
            torch.cuda.synchronize()
            now = time.perf_counter()
            elapsed = now - last_log
            last_log = now
            run.log({
                "global_step": current,
                "train/ce": float(ce.detach()),
                "train/diversity_loss": float(diversity.detach()),
                "grad/global_preclip": float(global_norm),
                "grad/clipped": float(global_norm > config.clip_norm),
                "grad/mask": float(model.theta.grad.float().norm()),
                "grad/coefficient": float(model.coefficient.grad.float().norm()),
                "grad/bias": float(model.bias.grad.float().norm()),
                "optim/lr_mask": optimizer.param_groups[0]["lr"],
                "optim/lr_coefficient": optimizer.param_groups[1]["lr"],
                "perf/examples_per_second": 10 * batch_size / max(elapsed, 1e-9),
                **{
                    f"diversity/{key}": value
                    for key, value in diversity_diagnostics.items()
                },
            })
        if current % config.eval_every == 0 or current == steps:
            repair = repair_duplicate_masks(
                model, optimizer, seed + 10_000 + current
            )
            val_logits, val_metrics = evaluate(val_bits, val_probability)
            _, train_metrics = evaluate(
                train_probe_bits, train_probe_probability
            )
            row = {
                "global_step": current,
                **{f"val/{key}": value for key, value in val_metrics.items()},
                **{
                    f"train_probe/{key}": value
                    for key, value in train_metrics.items()
                },
                **{f"diversity/{key}": value for key, value in repair.items()},
            }
            run.log(row)
            print(json.dumps({
                "terms": terms,
                "step": current,
                "val_kl": val_metrics["kl"],
                "val_agreement": val_metrics["agreement"],
                "train_probe_ce": train_metrics["ce"],
                "duplicates_repaired": repair["duplicates_repaired"],
                "recent_grad_norm": float(np.median(recent_norms[-25:])),
            }), flush=True)
            if val_metrics["kl"] < best_kl:
                best_kl = val_metrics["kl"]
                best_step = current
                best_state = model.sparse_state()
                best_val_logits = val_logits
                best_train_probe_ce = train_metrics["ce"]

    compact = encode_compact_student(best_state)
    deployed = decode_compact_student(compact)
    val_logits = exact_sparse_gpu(val_bits, deployed)
    test_logits = exact_sparse_gpu(test_bits, deployed)
    val_metrics = binary_metrics(val_logits, val_probability.cpu().numpy())
    test_metrics = binary_metrics(test_logits, test_probability.cpu().numpy())
    unique_masks = len(np.unique(model.hard_index_rows().numpy(), axis=0))
    if unique_masks != terms:
        raise RuntimeError("final model does not contain exactly X unique characters")

    run_id = run.id
    model_path = f"{ROOT}/models/fourier-kiss-x{terms}-{run_id}.npz"
    summary_path = f"{ROOT}/results/fourier-kiss-x{terms}-{run_id}.json"
    Path(model_path).parent.mkdir(parents=True, exist_ok=True)
    Path(summary_path).parent.mkdir(parents=True, exist_ok=True)
    metadata = {
        "format": "fourier-kiss-v1", "terms": terms, "n_bits": n_bits,
        "max_degree": config.max_degree, "lsh_bits": 32,
        "objective": "original_teacher_soft_bce", "best_step": best_step,
        "config": dataclasses.asdict(config),
    }
    temporary = model_path + ".tmp"
    with open(temporary, "wb") as handle:
        np.savez(
            handle,
            n_bits=np.asarray(n_bits, dtype=np.int32),
            degrees=compact["degrees"],
            index_bits=np.asarray(compact["index_bits"], dtype=np.uint8),
            index_count=np.asarray(compact["index_count"], dtype=np.int32),
            packed_indices=compact["packed_indices"],
            coefficient_fp16=compact["coefficient_fp16"],
            coefficient_scale=compact["coefficient_scale"],
            coefficient_block_size=np.asarray(
                compact["coefficient_block_size"], dtype=np.int32
            ),
            bias=np.asarray(compact["bias"], dtype=np.float32),
            lsh_codes_packed=lsh_codes,
            metadata_json=np.asarray(json.dumps(metadata)),
        )
    os.replace(temporary, model_path)
    artifact_bytes = os.path.getsize(model_path)
    compression = 1_600_000_000 / artifact_bytes
    if artifact_bytes > 16_000_000:
        raise RuntimeError(f"artifact exceeds 100x budget: {artifact_bytes} bytes")
    gpu_values = monitor.finish()
    active_gpu = [value for value in gpu_values if value > 0]
    summary = {
        "wandb_url": run.url,
        "model_path": model_path,
        "best_step": best_step,
        "best_train_probe_ce": best_train_probe_ce,
        "best_float_val_kl": best_kl,
        "val": val_metrics,
        "test": test_metrics,
        "export": {
            "terms": terms,
            "unique_masks": unique_masks,
            "indices": int(compact["index_count"]),
            "artifact_bytes": artifact_bytes,
            "compression_vs_1_6gb": compression,
            "unique_token_codes": unique_token_codes,
            "quantization_max_val_logit_delta": float(np.max(np.abs(
                val_logits - best_val_logits
            ))),
        },
        "performance": {
            "seconds": time.perf_counter() - started,
            "peak_memory_bytes": int(torch.cuda.max_memory_allocated()),
            "median_gpu_utilization": (
                float(np.median(gpu_values)) if gpu_values else None
            ),
            "median_active_gpu_utilization": (
                float(np.median(active_gpu)) if active_gpu else None
            ),
            "recent_median_grad_norm": float(np.median(recent_norms)),
            "grad_norm_healthy": bool(
                recent_norms and 0.05 <= np.median(recent_norms) <= 1.0
            ),
        },
        "last_sampled_diversity": last_diversity,
    }
    with open(summary_path + ".tmp", "w") as handle:
        json.dump(summary, handle, indent=2, sort_keys=True)
    os.replace(summary_path + ".tmp", summary_path)
    volume.commit()
    run.log({
        "global_step": steps,
        **{f"deployed_val/{key}": value for key, value in val_metrics.items()},
        **{f"test/{key}": value for key, value in test_metrics.items()},
    })
    run.summary.update({
        "best_step": best_step, "best_float_val_kl": best_kl,
        "test_kl": test_metrics["kl"], "test_mae": test_metrics["mae"],
        "test_rmse": test_metrics["rmse"],
        "test_r_squared": test_metrics["r_squared"],
        "compression": compression, "artifact_bytes": artifact_bytes,
        "unique_masks": unique_masks,
    })
    artifact = wandb.Artifact(
        f"fourier-kiss-x{terms}-{run_id}", type="model", metadata=summary
    )
    artifact.add_file(model_path)
    run.log_artifact(artifact)
    run.finish()
    print(json.dumps(summary, indent=2, sort_keys=True), flush=True)
    return summary


@app.function(image=image, secrets=WANDB_SECRET)
def compare_scale(results):
    import wandb

    ordered = sorted(results, key=lambda row: row["export"]["terms"])
    baseline, target = ordered[0], ordered[-1]
    run = wandb.init(
        project="fda-fourier-noun", group="fourier-kiss",
        name="fourier-kiss-scale", job_type="scale-comparison",
        config={"source_runs": [row["wandb_url"] for row in ordered]},
    )
    columns = [
        "terms", "train_probe_ce", "val_kl", "test_kl", "test_mae",
        "test_rmse", "test_r_squared", "artifact_bytes", "compression",
        "unique_masks", "wandb_url",
    ]
    table_rows = []
    for row in ordered:
        table_rows.append([
            row["export"]["terms"], row["best_train_probe_ce"],
            row["val"]["kl"], row["test"]["kl"], row["test"]["mae"],
            row["test"]["rmse"], row["test"]["r_squared"],
            row["export"]["artifact_bytes"],
            row["export"]["compression_vs_1_6gb"],
            row["export"]["unique_masks"], row["wandb_url"],
        ])
    run.log({"scale": wandb.Table(columns=columns, data=table_rows)})
    acceptance = {
        "training_ce_improved": (
            target["best_train_probe_ce"] < baseline["best_train_probe_ce"]
        ),
        "validation_kl_improved": target["val"]["kl"] < baseline["val"]["kl"],
        "target_unique": (
            target["export"]["unique_masks"] == target["export"]["terms"]
        ),
        "target_within_100x": (
            target["export"]["artifact_bytes"] <= 16_000_000
        ),
    }
    run.summary.update(acceptance)
    url = run.url
    run.finish()
    return {"wandb_url": url, "acceptance": acceptance, "runs": ordered}


@app.local_entrypoint()
def main(stage: str = "scale", x: int = 524288, steps: int = 1000,
         batch_size: int = 16384, char_chunk: int = 4096, seed: int = 0,
         mask_lr: float = 1.0, coefficient_lr: float = 0.03,
         diversity_weight: float = 0.05, diversity_sample: int = 1024):
    arguments = {
        "steps": steps, "batch_size": batch_size, "char_chunk": char_chunk,
        "seed": seed, "mask_lr": mask_lr,
        "coefficient_lr": coefficient_lr,
        "diversity_weight": diversity_weight,
        "diversity_sample": diversity_sample,
    }
    if stage == "train":
        print(train_direct.remote(terms=x, **arguments))
    elif stage == "scale":
        calls = [
            train_direct.spawn(terms=131072, **arguments),
            train_direct.spawn(terms=x, **arguments),
        ]
        results = [call.get() for call in calls]
        print(compare_scale.remote(results))
    elif stage == "smoke":
        print(train_direct.remote(
            terms=min(x, 4096), steps=min(steps, 10),
            batch_size=min(batch_size, 512), char_chunk=min(char_chunk, 512),
            seed=seed, mask_lr=mask_lr, coefficient_lr=coefficient_lr,
            diversity_weight=diversity_weight,
            diversity_sample=min(diversity_sample, 256),
        ))
    else:
        raise SystemExit(f"unknown stage {stage!r}")
