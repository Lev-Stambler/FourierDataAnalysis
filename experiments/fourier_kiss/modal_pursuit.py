"""Modal/H100 runner for data-aware staircase Walsh pursuit."""

from __future__ import annotations

import modal


app = modal.App("fourier-pursuit")
volume = modal.Volume.from_name("fda-cache", create_if_missing=True)
image = (
    modal.Image.debian_slim(python_version="3.11")
    .pip_install(
        "numpy>=1.26", "torch==2.10.0", "wandb>=0.18",
        "nvidia-ml-py>=12.0", "pytest>=8.0",
    )
    .env({"TORCHINDUCTOR_CACHE_DIR": "/cache/torchinductor/fourier_pursuit"})
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
ROOT = "/cache/fourier_kiss/pursuit"


def _retry():
    return modal.Retries(
        max_retries=3, backoff_coefficient=2.0, initial_delay=1.0
    )


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
        import numpy as np
        self.stop_event.set()
        if self.thread is not None:
            self.thread.join(timeout=2)
        if not self.values:
            return {"gpu_util_mean": 0.0, "gpu_util_p50": 0.0,
                    "gpu_util_p90": 0.0}
        value = np.asarray(self.values)
        return {
            "gpu_util_mean": float(value.mean()),
            "gpu_util_p50": float(np.quantile(value, 0.50)),
            "gpu_util_p90": float(np.quantile(value, 0.90)),
        }


def _remote_setup(train_examples: int = 1_000_000):
    """Load the pinned mixed train split and web validation/test onto H100."""
    import gc
    import numpy as np
    import torch

    if not torch.cuda.is_available():
        raise RuntimeError("pursuit is intentionally CUDA-only")
    volume.reload()
    web = torch.load(WEB_LABELS, map_location="cpu", weights_only=False)
    ewt = torch.load(EWT_LABELS, map_location="cpu", weights_only=False)
    n_bits = int(web["metadata"]["n_bits"])
    if n_bits != 4096 or int(ewt["metadata"]["n_bits"]) != n_bits:
        raise RuntimeError("unexpected or inconsistent student bit width")
    if web["metadata"]["lsh_sha256"] != ewt["metadata"]["lsh_sha256"]:
        raise RuntimeError("training sources use different LSH codebooks")

    def unpack(source, split, count=None):
        item = source["splits"][split]
        packed = item["packed_bits"][:count].numpy()
        bits = np.unpackbits(
            packed, axis=-1, count=n_bits, bitorder="little"
        ).astype(np.uint8, copy=False)
        probability = item["teacher_probability"][:count].float().numpy()
        return (torch.from_numpy(bits).cuda(),
                torch.from_numpy(probability).cuda())

    ewt_count = min(train_examples, len(
        ewt["splits"]["train"]["teacher_probability"]
    ))
    web_count = train_examples - ewt_count
    if web_count > len(web["splits"]["train"]["teacher_probability"]):
        raise ValueError("not enough cached unique web examples")
    web_bits, web_probability = unpack(web, "train", web_count)
    ewt_bits, ewt_probability = unpack(ewt, "train", ewt_count)
    train_bits = torch.cat((web_bits, ewt_bits))
    train_probability = torch.cat((web_probability, ewt_probability))
    del web_bits, web_probability, ewt_bits, ewt_probability
    val_bits, val_probability = unpack(web, "val")
    test_bits, test_probability = unpack(web, "test")
    codes = np.asarray(web["lsh_codes_packed"], dtype=np.uint8)
    unique_codes = len(np.unique(codes, axis=0))
    if unique_codes != len(codes):
        raise RuntimeError(
            f"token LSH collision: {unique_codes}/{len(codes)} unique"
        )
    metadata = {
        "n_bits": n_bits,
        "lsh_bits": int(web["metadata"]["lsh_bits"]),
        "lsh_sha256": web["metadata"]["lsh_sha256"],
        "train_examples": len(train_bits),
        "web_examples": web_count,
        "ewt_examples": ewt_count,
        "token_codes": len(codes),
        "unique_token_codes": unique_codes,
    }
    del web, ewt
    gc.collect()
    return (train_bits, train_probability, val_bits, val_probability,
            test_bits, test_probability, codes, metadata)


def _importance_weights(train_probability, val_probability, bins: int = 32):
    import torch
    boundaries = torch.linspace(0.0, 1.0, bins + 1,
                                device=train_probability.device)
    train_bin = torch.bucketize(train_probability, boundaries[1:-1])
    val_bin = torch.bucketize(val_probability, boundaries[1:-1])
    train_hist = torch.bincount(train_bin, minlength=bins).float()
    val_hist = torch.bincount(val_bin, minlength=bins).float()
    ratio = (val_hist / val_hist.sum()) / (
        train_hist / train_hist.sum()
    ).clamp_min(1e-8)
    ratio = ratio.clamp(0.25, 4.0)
    weight = ratio[train_bin]
    weight /= weight.mean()
    return weight, {
        "importance_weight_min": float(weight.min()),
        "importance_weight_max": float(weight.max()),
        "importance_weight_std": float(weight.std()),
    }


def _snapshot(model):
    return {
        "active_terms": model.active_terms,
        "indices": model.indices[:model.active_terms].detach().cpu(),
        "degrees": model.degrees[:model.active_terms].detach().cpu(),
        "parent": model.parent[:model.active_terms].detach().cpu(),
        "appended_bit": model.appended_bit[:model.active_terms].detach().cpu(),
        "coefficient": model.coefficient[:model.active_terms].detach().cpu(),
        "bias": model.bias.detach().cpu(),
    }


def _restore(model, snapshot):
    import torch
    count = int(snapshot["active_terms"])
    with torch.no_grad():
        model.indices[:count].copy_(snapshot["indices"].to(model.indices.device))
        model.degrees[:count].copy_(snapshot["degrees"].to(model.degrees.device))
        model.parent[:count].copy_(snapshot["parent"].to(model.parent.device))
        model.appended_bit[:count].copy_(
            snapshot["appended_bit"].to(model.appended_bit.device)
        )
        model.coefficient.zero_()
        model.coefficient[:count].copy_(
            snapshot["coefficient"].to(model.coefficient.device)
        )
        model.bias.copy_(snapshot["bias"].to(model.bias.device))
    model.active_terms = count


def _model_logits(model, bits, batch_size=8192):
    import torch
    values = []
    with torch.no_grad():
        for lo in range(0, len(bits), batch_size):
            values.append(model(bits[lo:lo + batch_size]))
    return torch.cat(values)


def _threshold_metrics(logits, target, threshold):
    import math
    import torch
    from fourier_kiss.model import binary_metrics
    result = binary_metrics(logits, target)
    teacher = target >= 0.5
    student = logits >= threshold
    positive = teacher.sum().clamp_min(1)
    negative = (~teacher).sum().clamp_min(1)
    tp = (teacher & student).sum()
    tn = ((~teacher) & (~student)).sum()
    joint = torch.stack((tn, ((~teacher) & student).sum(),
                         (teacher & (~student)).sum(), tp)).reshape(2, 2).double()
    joint /= joint.sum().clamp_min(1)
    independent = joint.sum(1, keepdim=True) * joint.sum(0, keepdim=True)
    active = joint > 0
    mi = float((joint[active] * torch.log2(
        joint[active] / independent[active].clamp_min(1e-12)
    )).sum())
    rate = float(teacher.double().mean())
    entropy = (-rate * math.log2(rate) - (1 - rate) * math.log2(1 - rate)
               if 0 < rate < 1 else 0.0)
    positive_recall = float(tp / positive)
    negative_recall = float(tn / negative)
    result.update({
        "agreement": float((teacher == student).float().mean()),
        "balanced_agreement": 0.5 * (positive_recall + negative_recall),
        "teacher_positive_recall": positive_recall,
        "teacher_negative_recall": negative_recall,
        "student_positive_rate": float(student.float().mean()),
        "hard_mutual_information_bits": mi,
        "hard_mutual_information_fraction": mi / entropy if entropy else 0.0,
        "decision_logit_threshold": float(threshold),
    })
    return result


def _calibrated_evaluation(model, bits, probability, val_logits=None,
                           val_probability=None):
    import torch
    from fourier_kiss.pursuit import constrained_threshold
    from fourier_kiss.rescue import fit_affine_calibration
    logits = _model_logits(model, bits)
    if val_logits is None:
        val_logits, val_probability = logits, probability
    scale, bias = fit_affine_calibration(val_logits, val_probability)
    calibrated_val = val_logits * scale + bias
    threshold, threshold_info = constrained_threshold(
        calibrated_val, val_probability, 0.85, 0.95
    )
    calibrated = logits * scale + bias
    metrics = _threshold_metrics(calibrated, probability, threshold)
    metrics.update({
        "logit_scale": scale, "logit_bias": bias,
        "threshold_feasible": threshold_info["threshold_feasible"],
    })
    return logits, metrics, (scale, bias, threshold)


def _passes_acceptance_gates(metrics):
    return (
        metrics["agreement"] >= 0.95
        and metrics["teacher_positive_recall"] >= 0.85
        and metrics["teacher_negative_recall"] >= 0.95
        and metrics["balanced_agreement"] >= 0.90
        and metrics["hard_mutual_information_fraction"] >= 0.50
        and metrics["kl"] <= 0.010
        and metrics["mae"] <= 0.040
        and metrics["r_squared"] >= 0.85
    )


def _acceptance_progress(metrics):
    """Worst normalized gate progress, used only before all gates pass."""
    return min(
        metrics["agreement"] / 0.95,
        metrics["teacher_positive_recall"] / 0.85,
        metrics["teacher_negative_recall"] / 0.95,
        metrics["balanced_agreement"] / 0.90,
        metrics["hard_mutual_information_fraction"] / 0.50,
        0.010 / max(metrics["kl"], 1e-12),
        0.040 / max(metrics["mae"], 1e-12),
        metrics["r_squared"] / 0.85,
    )


def _bank_audit(model, signature_bits, sample=1024):
    import torch
    count = model.active_terms
    if not count:
        return {}
    rows = torch.linspace(
        0, count - 1, min(sample, count), device=signature_bits.device
    ).long().unique()
    feature = model.feature_block(signature_bits, rows).float()
    feature = feature - feature.mean(0, keepdim=True)
    norm = feature.norm(dim=0, keepdim=True).clamp_min(1e-8)
    feature = feature / norm
    gram = feature.t() @ feature
    gram.fill_diagonal_(0)
    squared = feature.t() @ feature
    trace = float(torch.trace(squared))
    effective_rank = trace * trace / float(squared.square().sum().clamp_min(1e-8))
    degree = model.degrees[:count].long()
    return {
        "bank/terms": count,
        "bank/maximum_degree": int(degree.max()),
        "bank/mean_degree": float(degree.float().mean()),
        "bank/sampled_effective_rank": effective_rank,
        "bank/sampled_effective_rank_fraction": effective_rank / len(rows),
        "bank/sampled_max_abs_correlation": float(
            gram.abs().max().clamp_max(1.0)
        ),
        "bank/sampled_p99_abs_correlation": float(torch.quantile(
            gram.abs().flatten(), 0.99
        )),
    }


@app.function(
    image=image, gpu="H100", volumes={"/cache": volume}, timeout=1800,
    memory=32768,
)
def remote_tests():
    import pathlib
    import pytest
    import fourier_kiss
    root = pathlib.Path(fourier_kiss.__file__).parent
    return pytest.main(["-q", str(root / "test_pursuit.py")])


@app.function(
    image=image, gpu="H100", timeout=1800, memory=32768,
    secrets=WANDB_SECRET,
)
def inspect_wandb_run(run_id: str):
    """Read a prior experiment on Modal so comparisons never use local CPU."""
    import json
    import wandb

    run = wandb.Api().run(f"umd-leans-well/fda-fourier-noun/{run_id}")
    interesting = (
        "agreement", "recall", "kl", "mae", "r_squared", "cosine",
        "grad", "lr", "terms", "characters", "step", "compression",
    )
    summary = {
        key: value for key, value in dict(run.summary).items()
        if any(token in key.lower() for token in interesting)
    }
    result = {
        "id": run.id, "name": run.name, "url": run.url,
        "state": run.state, "config": dict(run.config), "summary": summary,
    }
    print(json.dumps(result, indent=2, sort_keys=True, default=str), flush=True)
    return result


@app.function(
    image=image, gpu="H100", volumes={"/cache": volume}, timeout=7200,
    memory=65536, secrets=WANDB_SECRET, retries=_retry(),
)
def diagnose(seed: int = 0, initial_terms: int = 32768,
             audit_examples: int = 65536):
    """Audit input/character aliasing and the complete degree-1/2 screen."""
    import json
    import numpy as np
    import torch
    import wandb
    from fourier_kiss.pursuit import (
        StaircaseWalshStudent, filter_seed_candidates,
        initial_low_degree_bank,
        initialize_uniqueness_sets, staircase_metadata,
    )
    from fourier_kiss.rescue import canonical_input_columns

    torch.manual_seed(seed)
    torch.set_float32_matmul_precision("high")
    torch.backends.cuda.matmul.allow_tf32 = True
    (train_bits, train_probability, val_bits, _val_probability,
     _test_bits, _test_probability, _codes, metadata) = _remote_setup()
    audit = torch.cat((train_bits[:audit_examples], val_bits)).cpu().numpy()
    columns_np, _sign, column_info = canonical_input_columns(audit)
    columns = torch.from_numpy(columns_np).cuda()
    seed_candidates = min(
        initial_terms + max(4096, initial_terms // 8),
        len(columns_np) * (len(columns_np) - 1) // 2 + len(columns_np),
    )
    supports, scores, screen_info = initial_low_degree_bank(
        train_bits, train_probability, columns, seed_candidates
    )
    supports, scores, filter_info = filter_seed_candidates(
        train_bits[:8192], supports, scores, initial_terms
    )
    if len(supports) != initial_terms:
        raise RuntimeError(
            f"empirically unique seed bank underfilled: "
            f"{len(supports)}/{initial_terms}"
        )
    parent, appended = staircase_metadata(supports)
    model = StaircaseWalshStudent(
        metadata["n_bits"], initial_terms, 32,
        initial_probability=float(train_probability.mean()), char_chunk=1024,
    ).cuda()
    model.append(supports, parent, appended, np.clip(scores, -0.01, 0.01))
    _, signatures, uniqueness = initialize_uniqueness_sets(
        model, train_bits[:8192]
    )
    bank = _bank_audit(model, train_bits[:8192])
    result = {**metadata, **column_info, **screen_info, **filter_info,
              **uniqueness,
              **bank, "signature_set_size": len(signatures)}
    run = wandb.init(
        project="fda-fourier-noun", group="fourier-pursuit",
        name=f"pursuit-diagnostic-s{seed}", job_type="fourier-diagnostic",
        config={"seed": seed, "initial_terms": initial_terms,
                "audit_examples": audit_examples, **metadata},
    )
    run.log(result)
    run.summary.update(result)
    result["wandb_url"] = run.url
    run.finish()
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)
    return result


@app.function(
    image=image, gpu="H100", volumes={"/cache": volume}, timeout=86400,
    memory=65536, secrets=WANDB_SECRET, retries=_retry(),
)
def train_pursuit(
    max_terms: int = 131072,
    initial_terms: int = 32768,
    add_per_round: int = 16384,
    parent_beam: int = 2048,
    discovery_probe: int = 32768,
    signature_probe: int = 8192,
    batch_size: int = 8192,
    fit_steps_per_round: int = 25,
    final_refit_steps: int = 300,
    coefficient_lr: float = 0.01,
    final_lr_ratio: float = 0.25,
    char_chunk: int = 1024,
    seed: int = 0,
    resume: bool = True,
    parent_model_path: str = "",
):
    """Grow a hard Walsh bank through residual pursuit, then WSD-refit it."""
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

    from fourier_kiss.pursuit import (
        StaircaseWalshStudent, decode_staircase_student,
        encode_staircase_student, filter_extension_candidates,
        filter_seed_candidates, initial_low_degree_bank,
        initialize_uniqueness_sets,
        score_staircase_extensions, select_parent_frontier,
        staircase_metadata,
    )
    from fourier_kiss.rescue import canonical_input_columns

    @dataclasses.dataclass(frozen=True)
    class Config:
        max_terms: int
        initial_terms: int
        add_per_round: int
        parent_beam: int
        discovery_probe: int
        signature_probe: int
        batch_size: int
        fit_steps_per_round: int
        final_refit_steps: int
        coefficient_lr: float
        final_lr_ratio: float
        char_chunk: int
        seed: int
        parent_model_path: str
        max_degree: int = 32
        clip_norm: float = 1.0
        loss_scale: float = 10.0
        weight_decay: float = 1e-4
        eval_batch_size: int = 8192

    config = Config(
        max_terms, initial_terms, add_per_round, parent_beam,
        discovery_probe, signature_probe, batch_size,
        fit_steps_per_round, final_refit_steps, coefficient_lr,
        final_lr_ratio, char_chunk, seed, parent_model_path,
    )
    if not 0 < initial_terms <= max_terms <= 1_000_000:
        raise ValueError("invalid character budget")
    if min(add_per_round, parent_beam, discovery_probe, signature_probe,
           batch_size) <= 0:
        raise ValueError("all pursuit sizes must be positive")
    if not 0 < final_lr_ratio <= 1:
        raise ValueError("final_lr_ratio must lie in (0, 1]")
    torch.manual_seed(seed)
    torch.cuda.manual_seed_all(seed)
    torch.set_float32_matmul_precision("high")
    torch.backends.cuda.matmul.allow_tf32 = True
    volume.reload()
    (train_bits, train_probability, val_bits, val_probability,
     test_bits, test_probability, codes, metadata) = _remote_setup()
    weight, weight_info = _importance_weights(
        train_probability, val_probability
    )
    audit_count = min(65536, len(train_bits))
    audit = torch.cat((train_bits[:audit_count], val_bits)).cpu().numpy()
    columns_np, _column_sign, column_info = canonical_input_columns(audit)
    del audit
    columns = torch.from_numpy(columns_np).cuda()

    cache_key = hashlib.sha256(json.dumps({
        "initial_terms": initial_terms, "seed": seed, "version": 2,
        "lsh": metadata["lsh_sha256"],
    }, sort_keys=True).encode()).hexdigest()[:16]
    initial_path = f"{ROOT}/candidates/initial-{cache_key}.pt"
    parent_sha256 = ""
    if parent_model_path:
        if not os.path.exists(parent_model_path):
            raise FileNotFoundError(parent_model_path)
        parent_hash = hashlib.sha256()
        with open(parent_model_path, "rb") as handle:
            while block := handle.read(1 << 20):
                parent_hash.update(block)
        parent_sha256 = parent_hash.hexdigest()
    checkpoint_identity = {
        "format_version": 3,
        "config": dataclasses.asdict(config),
        "lsh_sha256": metadata["lsh_sha256"],
        "parent_model_sha256": parent_sha256,
        "objective": "original_teacher_soft_bce",
        "support_optimizer": "stagewise_residual_staircase_pursuit",
    }
    checkpoint_key = hashlib.sha256(json.dumps(
        checkpoint_identity, sort_keys=True
    ).encode()).hexdigest()[:20]
    checkpoint_path = f"{ROOT}/checkpoints/pursuit-v3-{checkpoint_key}.pt"
    Path(initial_path).parent.mkdir(parents=True, exist_ok=True)
    Path(checkpoint_path).parent.mkdir(parents=True, exist_ok=True)
    parent_compact = None
    if parent_model_path:
        if not os.path.exists(parent_model_path):
            raise FileNotFoundError(parent_model_path)
        with np.load(parent_model_path, allow_pickle=False) as archive:
            parent_compact = {
                key: archive[key] for key in (
                    "n_bits", "terms", "parent", "appended_bit",
                    "coefficient_fp16", "coefficient_scale",
                    "coefficient_block_size", "bias",
                )
            }
            parent_codes = np.asarray(archive["lsh_codes_packed"])
        if not np.array_equal(parent_codes, codes):
            raise RuntimeError("parent artifact uses a different token codebook")
        decoded_parent = decode_staircase_student(parent_compact)
        supports = decoded_parent["supports"]
        if not 0 < len(supports) < max_terms:
            raise ValueError("parent character bank must be smaller than target")
        initial_scores = None
        screen_info = {
            "continued_from_terms": float(len(supports)),
            "continued_from_artifact": parent_model_path,
        }
    elif os.path.exists(initial_path):
        initial = torch.load(initial_path, map_location="cpu",
                             weights_only=False)
        supports = initial["supports"]
        initial_scores = initial["scores"]
        screen_info = initial["diagnostics"]
    else:
        seed_candidates = min(
            initial_terms + max(4096, initial_terms // 8),
            len(columns_np) * (len(columns_np) - 1) // 2 + len(columns_np),
        )
        supports, initial_scores, screen_info = initial_low_degree_bank(
            train_bits, train_probability, columns, seed_candidates
        )
        supports, initial_scores, filter_info = filter_seed_candidates(
            train_bits[:signature_probe], supports, initial_scores,
            initial_terms,
        )
        if len(supports) != initial_terms:
            raise RuntimeError(
                f"empirically unique seed bank underfilled: "
                f"{len(supports)}/{initial_terms}"
            )
        screen_info = {**screen_info, **filter_info}
        torch.save({"supports": supports, "scores": initial_scores,
                    "diagnostics": screen_info}, initial_path)
        volume.commit()

    initial_probability = float((train_probability * weight).mean())
    model = StaircaseWalshStudent(
        metadata["n_bits"], max_terms, config.max_degree,
        initial_probability=initial_probability, char_chunk=char_chunk,
    ).cuda()
    if parent_compact is not None:
        parent = np.asarray(parent_compact["parent"], dtype=np.int32)
        appended = np.asarray(parent_compact["appended_bit"], dtype=np.uint16)
        # Compact coefficients are already in represented Fourier units.
        # Convert them back to the raw parameterization for the larger fixed
        # budget; this preserves logits exactly across continuation.
        initial_coefficient = (
            decoded_parent["coefficient"] / model.output_scale
        )
        model.append(supports, parent, appended, initial_coefficient)
        with torch.no_grad():
            model.bias.fill_(decoded_parent["bias"])
    else:
        parent, appended = staircase_metadata(supports)
        # Preserve the stable 1/sqrt(M) coefficient parameterization from the
        # direct Fourier baseline. Correlation scores choose signs only;
        # jointly seeding many correlated OMP projections explodes logits.
        initial_coefficient = (
            initial_scores / max(float(np.abs(initial_scores).max()), 1e-8)
        ) * 1e-3
        model.append(supports, parent, appended, initial_coefficient)
    def make_optimizer(lr=coefficient_lr, coefficient_weight_decay=None):
        decay = (config.weight_decay if coefficient_weight_decay is None
                 else coefficient_weight_decay)
        return torch.optim.AdamW([
            {"params": [model.coefficient], "lr": lr,
             "weight_decay": decay},
            {"params": [model.bias], "lr": lr, "weight_decay": 0.0},
        ], betas=(0.9, 0.999), eps=1e-8, fused=True)

    optimizer = make_optimizer()

    checkpoint = None
    if resume and os.path.exists(checkpoint_path):
        checkpoint = torch.load(checkpoint_path, map_location="cpu",
                                weights_only=False)
        if checkpoint.get("identity") != checkpoint_identity:
            raise RuntimeError("checkpoint identity/configuration mismatch")
        _restore(model, checkpoint["model"])
        optimizer.load_state_dict(checkpoint["optimizer"])
    run = wandb.init(
        project="fda-fourier-noun", group="fourier-pursuit",
        name=(f"staircase-x{max_terms}-p{parent_beam}-"
              f"d{discovery_probe}-s{seed}"),
        job_type="staircase-walsh-student",
        id=(checkpoint or {}).get("wandb_id"), resume="allow",
        config={**dataclasses.asdict(config), **metadata, **column_info,
                **screen_info, **weight_info,
                "objective": "original_teacher_soft_bce",
                "support_optimizer": "stagewise_residual_staircase_pursuit",
                "new_block_fit": "freeze_existing_coefficients_and_bias"},
    )
    generator = torch.Generator(device="cuda").manual_seed(seed + 991)
    if checkpoint and "generator_state" in checkpoint:
        generator.set_state(checkpoint["generator_state"])
    signature_bits = train_bits[:signature_probe]
    support_keys, signature_keys, unique_info = initialize_uniqueness_sets(
        model, signature_bits
    )
    run.log({"global_step": 0, **unique_info,
             **_bank_audit(model, signature_bits)})
    best = (checkpoint or {}).get("best")
    round_index = int((checkpoint or {}).get("round", 0))
    global_step = int((checkpoint or {}).get("global_step", 0))
    monitor = _GpuSampler()
    monitor.start()
    started = time.perf_counter()

    def fit_steps(count, scheduler=None, coefficient_start=None,
                  freeze_bias=False):
        nonlocal global_step
        total_examples = 0
        total_started = time.perf_counter()
        for _ in range(count):
            index = torch.randint(
                len(train_bits), (batch_size,), generator=generator,
                device="cuda",
            )
            target = train_probability[index]
            item_weight = weight[index]
            optimizer.zero_grad(set_to_none=True)
            logits = model(train_bits[index])
            items = torch.nn.functional.binary_cross_entropy_with_logits(
                logits, target, reduction="none"
            )
            unscaled_loss = (items * item_weight).sum() / item_weight.sum()
            loss = unscaled_loss * config.loss_scale
            loss.backward()
            if coefficient_start is not None:
                model.coefficient.grad[:coefficient_start].zero_()
            if freeze_bias:
                model.bias.grad.zero_()
            coefficient_norm = model.coefficient.grad.float().norm()
            bias_norm = model.bias.grad.float().norm()
            global_norm = torch.nn.utils.clip_grad_norm_(
                model.parameters(), config.clip_norm, foreach=True
            )
            optimizer.step()
            if scheduler is not None:
                scheduler.step()
            global_step += 1
            total_examples += batch_size
            if global_step % 10 == 0:
                elapsed = max(1e-6, time.perf_counter() - total_started)
                run.log({
                    "global_step": global_step,
                    "train/soft_bce": float(unscaled_loss.detach()),
                    "grad/global_preclip": float(global_norm),
                    "grad/coefficient": float(coefficient_norm),
                    "grad/bias": float(bias_norm),
                    "grad/clipped": float(global_norm > config.clip_norm),
                    "optim/lr": optimizer.param_groups[0]["lr"],
                    "optim/stagewise_new_only": float(
                        coefficient_start is not None
                    ),
                    "throughput/examples_per_second": total_examples / elapsed,
                    "bank/terms": model.active_terms,
                })

    def evaluate_and_maybe_save(label):
        nonlocal best
        val_raw = _model_logits(model, val_bits, config.eval_batch_size)
        _raw, val_metrics, calibration = _calibrated_evaluation(
            model, val_bits, val_probability,
            val_logits=val_raw, val_probability=val_probability,
        )
        scale, bias, threshold = calibration
        passes = _passes_acceptance_gates(val_metrics)
        rank = (
            int(passes),
            val_metrics["agreement"] if passes
            else _acceptance_progress(val_metrics),
            val_metrics["hard_mutual_information_fraction"],
            -val_metrics["kl"],
        )
        payload = {
            "rank": rank, "label": label, "step": global_step,
            "model": _snapshot(model), "calibration": calibration,
            "val": val_metrics,
        }
        if best is None or tuple(rank) > tuple(best["rank"]):
            best = payload
        telemetry = {
            "global_step": global_step,
            **{f"val/{key}": value for key, value in val_metrics.items()},
            **_bank_audit(model, signature_bits),
            "selection/is_best": float(best is payload),
            "selection/passes_all_validation_gates": float(passes),
            "calibration/scale": scale,
            "calibration/bias": bias,
            "calibration/threshold": threshold,
        }
        run.log(telemetry)
        print(json.dumps({
            "label": label, "terms": model.active_terms,
            "step": global_step, "val_kl": val_metrics["kl"],
            "val_agreement": val_metrics["agreement"],
            "positive_recall": val_metrics["teacher_positive_recall"],
            "negative_recall": val_metrics["teacher_negative_recall"],
            "best_label": best["label"],
        }, sort_keys=True), flush=True)
        return val_metrics

    if checkpoint is None:
        if parent_compact is not None:
            # Retain the untouched parent as an explicit fallback before any
            # continuation update can perturb its fitted coefficients.
            evaluate_and_maybe_save(f"parent-x{model.active_terms}")
        fit_steps(fit_steps_per_round)
        evaluate_and_maybe_save(f"x{model.active_terms}")

    while model.active_terms < max_terms:
        round_index += 1
        remaining = max_terms - model.active_terms
        add_count = min(add_per_round, remaining)
        if model.active_terms < 65536:
            degree_limit = 4
        elif model.active_terms < 131072:
            degree_limit = 8
        elif model.active_terms < 262144:
            degree_limit = 16
        else:
            degree_limit = 32
        parents = select_parent_frontier(model, parent_beam, degree_limit)
        probe_start = ((round_index - 1) * discovery_probe) % len(train_bits)
        probe_index = (
            torch.arange(discovery_probe, device="cuda") + probe_start
        ).remainder(len(train_bits))
        proposals = score_staircase_extensions(
            model, train_bits[probe_index], train_probability[probe_index],
            columns, parents, weights=weight[probe_index],
            parents_per_chunk=128, top_per_parent=32,
        )
        (new_supports, new_parents, new_bits, new_coefficients,
         filter_info) = filter_extension_candidates(
            model, proposals, signature_bits, support_keys, signature_keys,
            add_count,
        )
        if not new_supports:
            run.log({"global_step": global_step,
                     "pursuit/exhausted": 1.0, **filter_info})
            break
        # A block of residual candidates is not an orthogonal OMP block. Add
        # it at zero so the represented function is exactly unchanged. Fit
        # only that new block with a fresh optimizer: established coefficients
        # and bias remain bit-exact while the residual learner is constructed.
        start, stop = model.append(
            new_supports, new_parents, new_bits,
            [0.0] * len(new_coefficients),
        )
        optimizer = make_optimizer(coefficient_weight_decay=0.0)
        fit_steps(
            fit_steps_per_round, coefficient_start=start, freeze_bias=True
        )
        run.log({
            "global_step": global_step, "pursuit/round": round_index,
            "pursuit/added": stop - start,
            "pursuit/proposals": len(proposals),
            "pursuit/top_normalized_score": proposals[0][0],
            **{f"pursuit/{key}": value for key, value in filter_info.items()},
        })
        evaluate_and_maybe_save(f"x{model.active_terms}")
        checkpoint_payload = {
            "identity": checkpoint_identity,
            "round": round_index, "global_step": global_step,
            "model": _snapshot(model), "optimizer": optimizer.state_dict(),
            "best": best, "wandb_id": run.id,
            "generator_state": generator.get_state().cpu(),
        }
        temporary = checkpoint_path + ".tmp"
        torch.save(checkpoint_payload, temporary)
        os.replace(temporary, checkpoint_path)
        volume.commit()

    # Jointly refit the complete discovered dictionary. The smaller validation
    # winner remains in ``best`` as a fallback, but restoring it here would
    # discard later optional basis functions before the convex solver can set
    # their coefficients (including setting unhelpful terms back to zero).
    winner = best["model"]
    winner_terms = int(winner["active_terms"])
    if winner_terms > model.active_terms:
        raise RuntimeError("best dictionary is not nested in full dictionary")
    if not (
        torch.equal(
            model.indices[:winner_terms].cpu(), winner["indices"]
        ) and torch.equal(
            model.degrees[:winner_terms].cpu(), winner["degrees"]
        )
    ):
        raise RuntimeError("best dictionary is not a prefix of full dictionary")
    with torch.no_grad():
        # Preserve every discovered support, initialize their shared prefix
        # from the validation winner, and make every later optional feature a
        # true no-op. Thus the full dictionary begins at exactly the best
        # smaller model's logits rather than at a harmful stagewise endpoint.
        model.coefficient.zero_()
        model.coefficient[:winner_terms].copy_(
            winner["coefficient"].to(model.coefficient.device)
        )
        model.bias.copy_(winner["bias"].to(model.bias.device))
    run.log({"global_step": global_step,
             "refit/full_dictionary_terms": model.active_terms,
             "refit/warm_start_terms": winner_terms,
             "refit/zero_initialized_optional_terms": (
                 model.active_terms - winner_terms
             )})
    evaluate_and_maybe_save("refit-warm-start")
    # Reset AdamW so stagewise/new-only moments cannot distort joint refitting.
    optimizer = make_optimizer(coefficient_lr * final_lr_ratio)
    run.log({"global_step": global_step,
             "refit/base_lr": coefficient_lr * final_lr_ratio})
    warmup, decay = min(20, final_refit_steps), min(200, final_refit_steps // 3)
    stable = max(0, final_refit_steps - warmup - decay)

    def wsd(step):
        if warmup and step < warmup:
            return (step + 1) / warmup
        if step < warmup + stable:
            return 1.0
        progress = min(1.0, (step - warmup - stable) / max(1, decay))
        return 0.1 + 0.9 * 0.5 * (1.0 + math.cos(math.pi * progress))

    scheduler = torch.optim.lr_scheduler.LambdaLR(optimizer, wsd)
    for start in range(0, final_refit_steps, 100):
        fit_steps(min(100, final_refit_steps - start), scheduler)
        evaluate_and_maybe_save(f"refit-{start + min(100, final_refit_steps-start)}")
    _restore(model, best["model"])

    val_raw = _model_logits(model, val_bits, config.eval_batch_size)
    test_raw = _model_logits(model, test_bits, config.eval_batch_size)
    scale, calibration_bias, threshold = best["calibration"]
    val_metrics = _threshold_metrics(
        val_raw * scale + calibration_bias, val_probability, threshold
    )
    test_metrics = _threshold_metrics(
        test_raw * scale + calibration_bias, test_probability, threshold
    )
    compact = encode_staircase_student(model)
    # Validate the complete parent topology before writing the artifact.
    decoded = decode_staircase_student(compact)
    if len(decoded["supports"]) != model.active_terms:
        raise RuntimeError("serialized staircase lost characters")
    original_coefficient = model.coefficient[:model.active_terms].detach().clone()
    quantized_coefficient = (
        torch.from_numpy(decoded["coefficient"]).cuda() / model.output_scale
    )
    with torch.no_grad():
        model.coefficient[:model.active_terms].copy_(quantized_coefficient)
        model.bias.fill_(decoded["bias"])
    quantized_val = _model_logits(model, val_bits, config.eval_batch_size)
    quantized_test = _model_logits(model, test_bits, config.eval_batch_size)
    serialized_val_metrics = _threshold_metrics(
        quantized_val * scale + calibration_bias, val_probability, threshold
    )
    serialized_test_metrics = _threshold_metrics(
        quantized_test * scale + calibration_bias, test_probability, threshold
    )
    with torch.no_grad():
        model.coefficient[:model.active_terms].copy_(original_coefficient)
    if serialized_val_metrics["kl"] - val_metrics["kl"] > 1e-4:
        raise RuntimeError("coefficient serialization degraded KL by >1e-4")
    if serialized_val_metrics["mae"] - val_metrics["mae"] > 1e-4:
        raise RuntimeError("coefficient serialization degraded MAE by >1e-4")
    classification_tolerance = 2.0 / len(val_probability)
    for key in (
        "agreement", "balanced_agreement", "teacher_positive_recall",
        "teacher_negative_recall", "hard_mutual_information_fraction",
        "r_squared",
    ):
        if (serialized_val_metrics[key] + classification_tolerance
                < val_metrics[key]):
            raise RuntimeError(
                f"coefficient serialization degraded {key} beyond tolerance"
            )
    if (_passes_acceptance_gates(val_metrics)
            and not _passes_acceptance_gates(serialized_val_metrics)):
        raise RuntimeError("serialized student crossed an acceptance gate")

    run_id = run.id
    model_path = f"{ROOT}/models/staircase-x{model.active_terms}-{run_id}.npz"
    summary_path = f"{ROOT}/results/staircase-x{model.active_terms}-{run_id}.json"
    Path(model_path).parent.mkdir(parents=True, exist_ok=True)
    Path(summary_path).parent.mkdir(parents=True, exist_ok=True)
    metadata_json = {
        "format": "fourier-staircase-v1", "config": dataclasses.asdict(config),
        "lsh_sha256": metadata["lsh_sha256"],
        "logit_scale": scale, "logit_bias": calibration_bias,
        "decision_logit_threshold": threshold,
    }
    temporary = model_path + ".tmp"
    with open(temporary, "wb") as handle:
        np.savez(
            handle, **compact,
            lsh_codes_packed=codes,
            logit_scale=np.asarray(scale, np.float32),
            logit_bias=np.asarray(calibration_bias, np.float32),
            decision_logit_threshold=np.asarray(threshold, np.float32),
            metadata_json=np.asarray(json.dumps(metadata_json)),
        )
    os.replace(temporary, model_path)
    artifact_bytes = os.path.getsize(model_path)
    compression = 1_600_000_000 / artifact_bytes
    if artifact_bytes > 16_000_000:
        raise RuntimeError("serialized student violates 100x compression")
    utilization = monitor.finish()
    result = {
        "wandb_url": run.url, "model_path": model_path,
        "checkpoint_path": checkpoint_path,
        "best_label": best["label"], "best_step": best["step"],
        "val": val_metrics, "test": test_metrics,
        "serialized_val": serialized_val_metrics,
        "serialized_test": serialized_test_metrics,
        "export": {
            "terms": model.active_terms, "artifact_bytes": artifact_bytes,
            "compression_vs_1_6gb": compression,
            "unique_token_codes": metadata["unique_token_codes"],
            "bytes_per_character_with_codebook": artifact_bytes/model.active_terms,
        },
        "utilization": utilization,
        "seconds": time.perf_counter() - started,
    }
    with open(summary_path + ".tmp", "w") as handle:
        json.dump(result, handle, indent=2, sort_keys=True)
    os.replace(summary_path + ".tmp", summary_path)
    volume.commit()
    run.log({
        **{f"deployed_val/{key}": value
           for key, value in serialized_val_metrics.items()},
        **{f"test/{key}": value
           for key, value in serialized_test_metrics.items()},
        **{f"gpu/{key}": value for key, value in utilization.items()},
        "export/artifact_bytes": artifact_bytes,
        "export/compression": compression,
    })
    run.summary.update({
        "test_kl": serialized_test_metrics["kl"],
        "test_agreement": serialized_test_metrics["agreement"],
        "test_positive_recall": serialized_test_metrics[
            "teacher_positive_recall"
        ],
        "test_negative_recall": serialized_test_metrics[
            "teacher_negative_recall"
        ],
        "test_mae": serialized_test_metrics["mae"],
        "test_r_squared": serialized_test_metrics["r_squared"],
        "test_hard_mi_fraction": serialized_test_metrics[
            "hard_mutual_information_fraction"
        ],
        "artifact_bytes": artifact_bytes, "compression": compression,
        **utilization,
    })
    artifact = wandb.Artifact(
        f"fourier-staircase-{run_id}", type="model", metadata=result
    )
    artifact.add_file(model_path)
    run.log_artifact(artifact)
    run.finish()
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)
    return result


@app.local_entrypoint()
def main(stage: str = "tests", x: int = 131072, seed: int = 0,
         initial_terms: int = 32768, add_per_round: int = 16384,
         parent_beam: int = 2048, discovery_probe: int = 32768,
         signature_probe: int = 8192, batch_size: int = 8192,
         fit_steps: int = 25, final_steps: int = 300,
         coefficient_lr: float = 0.01, final_lr_ratio: float = 0.25,
         char_chunk: int = 1024,
         resume: bool = True, run_id: str = "g5pafut2",
         parent_model: str = ""):
    if stage == "tests":
        print(remote_tests.remote())
    elif stage == "diagnose":
        print(diagnose.remote(seed=seed, initial_terms=initial_terms))
    elif stage == "inspect":
        print(inspect_wandb_run.remote(run_id))
    elif stage in {"pilot", "train"}:
        print(train_pursuit.remote(
            max_terms=x, initial_terms=initial_terms,
            add_per_round=add_per_round, parent_beam=parent_beam,
            discovery_probe=discovery_probe,
            signature_probe=signature_probe, batch_size=batch_size,
            fit_steps_per_round=fit_steps,
            final_refit_steps=final_steps,
            coefficient_lr=coefficient_lr, final_lr_ratio=final_lr_ratio,
            char_chunk=char_chunk,
            seed=seed, resume=resume, parent_model_path=parent_model,
        ))
    else:
        raise SystemExit(f"unknown stage {stage!r}")
