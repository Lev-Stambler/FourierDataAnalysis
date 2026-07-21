"""Modal-only diagnostic and training ladder for the screened Fourier student."""

from __future__ import annotations

import modal


app = modal.App("fourier-kiss-rescue")
volume = modal.Volume.from_name("fda-cache", create_if_missing=True)
image = (
    modal.Image.debian_slim(python_version="3.11")
    .pip_install(
        "numpy>=1.26", "torch==2.10.0", "wandb>=0.18",
        "nvidia-ml-py>=12.0",
    )
    .env({"TORCHINDUCTOR_CACHE_DIR": "/cache/torchinductor/fourier_kiss_rescue"})
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
ROOT = "/cache/fourier_kiss_rescue"


def _retry():
    return modal.Retries(
        max_retries=3, backoff_coefficient=2.0, initial_delay=1.0
    )


def _remote_setup(seed: int, train_examples: int = 1_000_000):
    """Load exactly ``train_examples`` unique examples into H100 memory."""
    import gc

    import numpy as np
    import torch

    if not torch.cuda.is_available():
        raise RuntimeError("the rescue pipeline is GPU-only")
    volume.reload()
    torch.manual_seed(seed)
    torch.cuda.manual_seed_all(seed)
    torch.set_float32_matmul_precision("high")
    torch.backends.cuda.matmul.allow_tf32 = True
    web = torch.load(WEB_LABELS, map_location="cpu", weights_only=False)
    ewt = torch.load(EWT_LABELS, map_location="cpu", weights_only=False)
    n_bits = int(web["metadata"]["n_bits"])
    if n_bits != int(ewt["metadata"]["n_bits"]):
        raise RuntimeError("source encodings disagree")
    if web["metadata"]["lsh_sha256"] != ewt["metadata"]["lsh_sha256"]:
        raise RuntimeError("source token codebooks disagree")

    def unpack(source, split, count=None):
        item = source["splits"][split]
        packed = item["packed_bits"][:count].numpy()
        bits = np.unpackbits(
            packed, axis=-1, count=n_bits, bitorder="little"
        ).astype(np.uint8, copy=False)
        probability = item["teacher_probability"][:count].float().numpy()
        return torch.from_numpy(bits).cuda(), torch.from_numpy(probability).cuda()

    ewt_count = min(len(ewt["splits"]["train"]["teacher_probability"]),
                    train_examples)
    web_count = train_examples - ewt_count
    if web_count > len(web["splits"]["train"]["teacher_probability"]):
        raise ValueError("requested more unique examples than are cached")
    web_bits, web_probability = unpack(web, "train", web_count)
    ewt_bits, ewt_probability = unpack(ewt, "train", ewt_count)
    train_bits = torch.cat((web_bits, ewt_bits), dim=0)
    train_probability = torch.cat((web_probability, ewt_probability), dim=0)
    del web_bits, ewt_bits, web_probability, ewt_probability
    val_bits, val_probability = unpack(web, "val")
    test_bits, test_probability = unpack(web, "test")
    codes = web["lsh_codes_packed"]
    if torch.is_tensor(codes):
        codes = codes.numpy()
    codes = np.asarray(codes, dtype=np.uint8)
    unique_codes = len(np.unique(codes, axis=0))
    if unique_codes != len(codes):
        raise RuntimeError(
            f"token codebook collision: {unique_codes}/{len(codes)} unique"
        )
    metadata = {
        "n_bits": n_bits,
        "lsh_bits": int(web["metadata"].get("lsh_bits", 32)),
        "lsh_sha256": web["metadata"]["lsh_sha256"],
        "train_examples": len(train_bits),
        "web_examples": web_count,
        "ewt_examples": ewt_count,
        "unique_token_codes": unique_codes,
        "token_codes": len(codes),
    }
    del web, ewt
    gc.collect()
    return (
        train_bits, train_probability, val_bits, val_probability,
        test_bits, test_probability, codes, metadata,
    )


def _importance_weights(train_probability, val_probability, bins: int = 32):
    """Bounded density-ratio weights over teacher-probability bins."""
    import torch

    boundaries = torch.linspace(
        0.0, 1.0, bins + 1, device=train_probability.device
    )
    train_bin = torch.bucketize(train_probability, boundaries[1:-1])
    val_bin = torch.bucketize(val_probability, boundaries[1:-1])
    train_hist = torch.bincount(train_bin, minlength=bins).float()
    val_hist = torch.bincount(val_bin, minlength=bins).float()
    ratio = (val_hist / val_hist.sum()) / (
        train_hist / train_hist.sum()
    ).clamp_min(1e-8)
    ratio = ratio.clamp(0.25, 4.0)
    weights = ratio[train_bin]
    weights /= weights.mean()
    return weights, {
        "importance_weight_min": float(weights.min()),
        "importance_weight_max": float(weights.max()),
        "importance_weight_std": float(weights.std()),
    }


def _evaluate(model, bits, probability, *, batch_size=8192):
    import torch

    output = []
    model.eval()
    with torch.no_grad():
        for lo in range(0, len(bits), batch_size):
            output.append(model(bits[lo:lo + batch_size]).float())
    model.train()
    return torch.cat(output), probability


def _calibrate(logits, probability, minimum_recall=0.60):
    from fourier_kiss.rescue import (
        calibrate_decision_threshold, calibrated_metrics,
        fit_affine_calibration,
    )

    scale, bias = fit_affine_calibration(logits, probability)
    calibrated = logits * scale + bias
    threshold, agreement, recall = calibrate_decision_threshold(
        calibrated, probability, minimum_recall
    )
    metrics = calibrated_metrics(
        logits, probability, logit_scale=scale, logit_bias=bias,
        decision_threshold=threshold,
    )
    metrics["calibration_selected_agreement"] = agreement
    metrics["calibration_selected_positive_recall"] = recall
    return scale, bias, threshold, metrics


@app.function(
    image=image, gpu="H100", volumes={"/cache": volume}, timeout=7200,
    memory=65536, secrets=WANDB_SECRET, retries=_retry(),
)
def diagnose_upper_bound(steps: int = 500, batch_size: int = 8192,
                         width: int = 1024, seed: int = 0):
    """Train a nonlinear diagnostic to test whether the encoding has headroom."""
    import json
    import time

    import torch
    import wandb

    (
        train_bits, train_probability, val_bits, val_probability,
        _test_bits, _test_probability, _codes, metadata,
    ) = _remote_setup(seed)
    weights, weight_diagnostics = _importance_weights(
        train_probability, val_probability
    )
    model = torch.nn.Sequential(
        torch.nn.Linear(metadata["n_bits"], width),
        torch.nn.GELU(),
        torch.nn.Linear(width, width // 2),
        torch.nn.GELU(),
        torch.nn.Linear(width // 2, 1),
    ).cuda()
    model = torch.compile(model, mode="max-autotune-no-cudagraphs")
    optimizer = torch.optim.AdamW(
        model.parameters(), lr=3e-3, weight_decay=1e-4, fused=True
    )
    warmup = max(1, round(steps * 0.05))
    scheduler = torch.optim.lr_scheduler.LambdaLR(
        optimizer,
        lambda step: ((step + 1) / warmup if step < warmup else
                      0.05 + 0.95 * 0.5 * (1.0 + __import__("math").cos(
                          __import__("math").pi * (step - warmup)
                          / max(1, steps - warmup))))
    )
    run = wandb.init(
        project="fda-fourier-noun", group="fourier-rescue",
        name=f"encoding-upper-bound-w{width}-s{seed}",
        job_type="representation-upper-bound",
        config={"steps": steps, "batch_size": batch_size, "width": width,
                "seed": seed, **metadata, **weight_diagnostics},
    )
    generator = torch.Generator(device="cuda").manual_seed(seed + 17)
    started = time.perf_counter()
    best = None
    for step in range(steps):
        index = torch.randint(
            len(train_bits), (batch_size,), generator=generator, device="cuda"
        )
        x = train_bits[index].float()
        target = train_probability[index]
        weight = weights[index]
        optimizer.zero_grad(set_to_none=True)
        logits = model(x).squeeze(1)
        loss_items = torch.nn.functional.binary_cross_entropy_with_logits(
            logits, target, reduction="none"
        )
        loss = (loss_items * weight).sum() / weight.sum()
        loss.backward()
        norm = torch.nn.utils.clip_grad_norm_(model.parameters(), 1.0,
                                              foreach=True)
        optimizer.step()
        scheduler.step()
        current = step + 1
        if current % 10 == 0:
            run.log({"global_step": current, "train/ce": float(loss),
                     "grad/global_preclip": float(norm),
                     "optim/lr": optimizer.param_groups[0]["lr"]})
        if current % 50 == 0 or current == steps:
            val_logits = []
            with torch.no_grad():
                for lo in range(0, len(val_bits), batch_size):
                    val_logits.append(model(
                        val_bits[lo:lo + batch_size].float()
                    ).squeeze(1))
            val_logits = torch.cat(val_logits)
            scale, bias, threshold, metrics = _calibrate(
                val_logits, val_probability
            )
            run.log({"global_step": current,
                     **{f"val/{k}": v for k, v in metrics.items()}})
            candidate = {
                "step": current, "scale": scale, "bias": bias,
                "threshold": threshold, "val": metrics,
            }
            if best is None or metrics["calibrated_agreement"] > \
                    best["val"]["calibrated_agreement"]:
                best = candidate
            print(json.dumps(candidate, sort_keys=True), flush=True)
    result = {
        **best, "wandb_url": run.url,
        "encoding_has_95_percent_headroom": bool(
            best["val"]["calibrated_agreement"] >= 0.96
        ),
        "seconds": time.perf_counter() - started,
    }
    run.summary.update(result["val"])
    run.summary["encoding_has_95_percent_headroom"] = result[
        "encoding_has_95_percent_headroom"
    ]
    run.finish()
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)
    return result


@app.function(
    image=image, gpu="H100", volumes={"/cache": volume}, timeout=21600,
    memory=65536, secrets=WANDB_SECRET, retries=_retry(),
)
def train_screened(terms: int = 131072, steps: int = 500,
                   batch_size: int = 16384, char_chunk: int = 4096,
                   coefficient_lr: float = 0.01, hard_weight: float = 0.0,
                   teacher_logit_scale: float = 1.0, seed: int = 0,
                   resume: bool = True):
    """Screen a unique low-degree bank, then fit it with resumable AdamW."""
    import dataclasses
    import hashlib
    import json
    import math
    import os
    import time
    from pathlib import Path

    import numpy as np
    import torch
    import wandb

    from fourier_kiss.model import encode_compact_student
    from fourier_kiss.rescue import (
        SparseWalshStudent, calibrated_metrics, canonical_input_columns,
        screen_degree_one_two,
    )

    config = {
        "terms": terms, "steps": steps, "batch_size": batch_size,
        "char_chunk": char_chunk, "coefficient_lr": coefficient_lr,
        "hard_weight": hard_weight,
        "teacher_logit_scale": teacher_logit_scale, "seed": seed,
        "train_examples": 1_000_000, "minimum_positive_recall": 0.60,
        "clip_norm": 1.0, "warmup_fraction": 0.05,
        "weight_decay": 1e-4, "eval_every": 25,
    }
    if not 0.0 <= hard_weight <= 1.0 or teacher_logit_scale < 1.0:
        raise ValueError("invalid distillation mixture")
    key = hashlib.sha256(json.dumps(
        {k: v for k, v in config.items() if k != "steps"}, sort_keys=True
    ).encode()).hexdigest()[:16]
    checkpoint_path = f"{ROOT}/checkpoints/screened-{key}.pt"
    candidate_path = f"{ROOT}/candidates/screened-v2-{terms}-s{seed}.pt"
    Path(checkpoint_path).parent.mkdir(parents=True, exist_ok=True)
    Path(candidate_path).parent.mkdir(parents=True, exist_ok=True)
    (
        train_bits, train_probability, val_bits, val_probability,
        test_bits, test_probability, codes, metadata,
    ) = _remote_setup(seed, config["train_examples"])
    weights, weight_diagnostics = _importance_weights(
        train_probability, val_probability
    )

    audit_count = min(65536, len(train_bits))
    audit = torch.cat((train_bits[:audit_count], val_bits), dim=0).cpu().numpy()
    columns_np, _column_sign, column_diagnostics = canonical_input_columns(audit)
    columns = torch.from_numpy(columns_np).cuda()
    del audit
    if os.path.exists(candidate_path):
        candidate = torch.load(candidate_path, map_location="cuda",
                               weights_only=False)
        indices = candidate["indices"]
        degrees = candidate["degrees"]
        scores = candidate["scores"]
        screen_diagnostics = candidate["diagnostics"]
    else:
        indices, degrees, scores, screen_diagnostics = screen_degree_one_two(
            train_bits, train_probability, columns, terms
        )
        torch.save({
            "indices": indices.cpu(), "degrees": degrees.cpu(),
            "scores": scores.cpu(), "diagnostics": screen_diagnostics,
        }, candidate_path)
        volume.commit()
    indices, degrees, scores = indices.cuda(), degrees.cuda(), scores.cuda()
    canonical_rows = torch.cat((
        degrees[:, None].long(), indices.long()
    ), dim=1)
    if len(torch.unique(canonical_rows, dim=0)) != terms:
        raise RuntimeError("screened bank contains duplicate characters")

    initial_probability = float((train_probability * weights).mean())
    model = SparseWalshStudent(
        indices, degrees, n_bits=metadata["n_bits"],
        initial_probability=initial_probability, char_chunk=char_chunk,
        initial_scores=scores,
    ).cuda()
    optimizer = torch.optim.AdamW([
        {"params": [model.coefficient], "lr": coefficient_lr,
         "weight_decay": config["weight_decay"]},
        {"params": [model.bias], "lr": coefficient_lr,
         "weight_decay": 0.0},
    ], betas=(0.9, 0.999), eps=1e-8, fused=True)
    warmup = max(1, round(steps * config["warmup_fraction"]))
    scheduler = torch.optim.lr_scheduler.LambdaLR(
        optimizer,
        lambda step: ((step + 1) / warmup if step < warmup else
                      0.05 + 0.95 * 0.5 * (1.0 + math.cos(
                          math.pi * (step - warmup) /
                          max(1, steps - warmup))))
    )
    checkpoint = None
    if resume and os.path.exists(checkpoint_path):
        checkpoint = torch.load(checkpoint_path, map_location="cuda",
                                weights_only=False)
        model.load_state_dict(checkpoint["model"])
        optimizer.load_state_dict(checkpoint["optimizer"])
        scheduler.load_state_dict(checkpoint["scheduler"])
    run = wandb.init(
        project="fda-fourier-noun", group="fourier-rescue",
        name=f"screened-x{terms}-lr{coefficient_lr:g}-s{seed}",
        job_type="screened-sparse-student",
        id=(checkpoint or {}).get("wandb_id"), resume="allow",
        config={**config, **metadata, **column_diagnostics,
                **screen_diagnostics, **weight_diagnostics},
    )
    start_step = int((checkpoint or {}).get("step", 0))
    best = (checkpoint or {}).get("best")
    generator = torch.Generator(device="cuda").manual_seed(seed + 31)
    if checkpoint and "generator_state" in checkpoint:
        generator.set_state(checkpoint["generator_state"])
    started = time.perf_counter()
    stale = 0
    for step in range(start_step, steps):
        index = torch.randint(
            len(train_bits), (batch_size,), generator=generator, device="cuda"
        )
        bits = train_bits[index]
        soft_target = train_probability[index]
        weight = weights[index]
        optimizer.zero_grad(set_to_none=True)
        logits = model(bits)
        soft_items = torch.nn.functional.binary_cross_entropy_with_logits(
            logits, soft_target, reduction="none"
        )
        if hard_weight:
            sharpened = torch.sigmoid(
                torch.logit(soft_target.clamp(1e-5, 1 - 1e-5))
                * teacher_logit_scale
            )
            hard_items = torch.nn.functional.binary_cross_entropy_with_logits(
                logits, sharpened, reduction="none"
            )
            items = (1.0 - hard_weight) * soft_items + hard_weight * hard_items
        else:
            items = soft_items
        loss = (items * weight).sum() / weight.sum()
        loss.backward()
        coefficient_norm = model.coefficient.grad.float().norm()
        bias_norm = model.bias.grad.float().norm()
        global_norm = torch.nn.utils.clip_grad_norm_(
            model.parameters(), config["clip_norm"], foreach=True
        )
        optimizer.step()
        scheduler.step()
        current = step + 1
        if current % 10 == 0:
            run.log({
                "global_step": current, "train/ce": float(loss),
                "grad/global_preclip": float(global_norm),
                "grad/coefficient": float(coefficient_norm),
                "grad/bias": float(bias_norm),
                "grad/clipped": float(global_norm > config["clip_norm"]),
                "optim/lr": optimizer.param_groups[0]["lr"],
            })
        if current % config["eval_every"] == 0 or current == steps:
            val_logits, _ = _evaluate(model, val_bits, val_probability)
            scale, calibration_bias, threshold, val_metrics = _calibrate(
                val_logits, val_probability,
                config["minimum_positive_recall"],
            )
            run.log({"global_step": current,
                     **{f"val/{k}": v for k, v in val_metrics.items()}})
            feasible = val_metrics["kl"] <= 0.015
            rank = (
                int(feasible),
                val_metrics["calibrated_agreement"] if feasible
                else -val_metrics["kl"],
            )
            prior_rank = tuple(best["rank"]) if best else (-1, -float("inf"))
            if rank > prior_rank:
                best = {
                    "rank": rank, "step": current,
                    "model": {k: v.detach().cpu() for k, v in
                              model.state_dict().items()},
                    "scale": scale, "calibration_bias": calibration_bias,
                    "threshold": threshold, "val": val_metrics,
                }
                stale = 0
            else:
                stale += 1
            payload = {
                "step": current, "model": model.state_dict(),
                "optimizer": optimizer.state_dict(),
                "scheduler": scheduler.state_dict(), "best": best,
                "generator_state": generator.get_state(),
                "wandb_id": run.id,
            }
            temporary = checkpoint_path + ".tmp"
            torch.save(payload, temporary)
            os.replace(temporary, checkpoint_path)
            volume.commit()
            print(json.dumps({
                "step": current, "terms": terms,
                "val_kl": val_metrics["kl"],
                "val_agreement": val_metrics["calibrated_agreement"],
                "positive_recall": val_metrics[
                    "calibrated_teacher_positive_recall"
                ], "best_step": best["step"],
            }), flush=True)
            if stale >= 12:
                break

    model.load_state_dict(best["model"])
    val_logits, _ = _evaluate(model, val_bits, val_probability)
    test_logits, _ = _evaluate(model, test_bits, test_probability)
    scale = best["scale"]
    calibration_bias = best["calibration_bias"]
    threshold = best["threshold"]
    val_metrics = calibrated_metrics(
        val_logits, val_probability, logit_scale=scale,
        logit_bias=calibration_bias, decision_threshold=threshold,
    )
    test_metrics = calibrated_metrics(
        test_logits, test_probability, logit_scale=scale,
        logit_bias=calibration_bias, decision_threshold=threshold,
    )
    sparse = model.sparse_state()
    compact = encode_compact_student(sparse)
    run_id = run.id
    model_path = f"{ROOT}/models/screened-x{terms}-{run_id}.npz"
    summary_path = f"{ROOT}/results/screened-x{terms}-{run_id}.json"
    Path(model_path).parent.mkdir(parents=True, exist_ok=True)
    Path(summary_path).parent.mkdir(parents=True, exist_ok=True)
    metadata_json = {
        "format": "fourier-kiss-screened-v2", "terms": terms,
        "n_bits": metadata["n_bits"], "lsh_bits": metadata["lsh_bits"],
        "logit_scale": scale, "logit_bias": calibration_bias,
        "decision_logit_threshold": threshold, "config": config,
    }
    temporary = model_path + ".tmp"
    with open(temporary, "wb") as handle:
        np.savez(
            handle, n_bits=np.asarray(metadata["n_bits"], np.int32),
            degrees=compact["degrees"],
            index_bits=np.asarray(compact["index_bits"], np.uint8),
            index_count=np.asarray(compact["index_count"], np.int32),
            packed_indices=compact["packed_indices"],
            coefficient_fp16=compact["coefficient_fp16"],
            coefficient_scale=compact["coefficient_scale"],
            coefficient_block_size=np.asarray(
                compact["coefficient_block_size"], np.int32
            ), bias=np.asarray(compact["bias"], np.float32),
            logit_scale=np.asarray(scale, np.float32),
            logit_bias=np.asarray(calibration_bias, np.float32),
            decision_logit_threshold=np.asarray(threshold, np.float32),
            lsh_codes_packed=codes,
            metadata_json=np.asarray(json.dumps(metadata_json)),
        )
    os.replace(temporary, model_path)
    artifact_bytes = os.path.getsize(model_path)
    compression = 1_600_000_000 / artifact_bytes
    if artifact_bytes > 16_000_000:
        raise RuntimeError("artifact exceeds the 100x compression budget")
    result = {
        "wandb_url": run.url, "model_path": model_path,
        "checkpoint_path": checkpoint_path, "best_step": best["step"],
        "val": val_metrics, "test": test_metrics,
        "export": {
            "terms": terms, "unique_characters": terms,
            "artifact_bytes": artifact_bytes,
            "compression_vs_1_6gb": compression,
            "unique_token_codes": metadata["unique_token_codes"],
        },
        "seconds": time.perf_counter() - started,
    }
    with open(summary_path + ".tmp", "w") as handle:
        json.dump(result, handle, indent=2, sort_keys=True)
    os.replace(summary_path + ".tmp", summary_path)
    volume.commit()
    run.log({**{f"deployed_val/{k}": v for k, v in val_metrics.items()},
             **{f"test/{k}": v for k, v in test_metrics.items()}})
    run.summary.update({
        "best_step": best["step"], "val_kl": val_metrics["kl"],
        "val_agreement": val_metrics["calibrated_agreement"],
        "val_positive_recall": val_metrics[
            "calibrated_teacher_positive_recall"
        ], "test_kl": test_metrics["kl"],
        "test_agreement": test_metrics["calibrated_agreement"],
        "test_positive_recall": test_metrics[
            "calibrated_teacher_positive_recall"
        ], "artifact_bytes": artifact_bytes, "compression": compression,
    })
    artifact = wandb.Artifact(
        f"fourier-rescue-x{terms}-{run_id}", type="model", metadata=result
    )
    artifact.add_file(model_path)
    run.log_artifact(artifact)
    run.finish()
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)
    return result


@app.local_entrypoint()
def main(stage: str = "upper", x: int = 131072, steps: int = 500,
         batch_size: int = 16384, char_chunk: int = 4096,
         coefficient_lr: float = 0.01, hard_weight: float = 0.0,
         teacher_logit_scale: float = 1.0, seed: int = 0,
         resume: bool = True):
    if stage == "upper":
        print(diagnose_upper_bound.remote(
            steps=steps, batch_size=min(batch_size, 8192), seed=seed
        ))
    elif stage == "smoke":
        print(train_screened.remote(
            terms=min(x, 4096), steps=min(steps, 10),
            batch_size=min(batch_size, 512),
            char_chunk=min(char_chunk, 512), coefficient_lr=coefficient_lr,
            hard_weight=hard_weight, teacher_logit_scale=teacher_logit_scale,
            seed=seed, resume=False,
        ))
    elif stage == "train":
        print(train_screened.remote(
            terms=x, steps=steps, batch_size=batch_size,
            char_chunk=char_chunk, coefficient_lr=coefficient_lr,
            hard_weight=hard_weight, teacher_logit_scale=teacher_logit_scale,
            seed=seed, resume=resume,
        ))
    else:
        raise SystemExit(f"unknown stage {stage!r}")
