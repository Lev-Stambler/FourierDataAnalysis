"""Exp V7: dense-free tied vocabulary, deliberately kept in one file."""

from __future__ import annotations

import ast
import hashlib
import json
import math
import os
import subprocess
import sys
import time
from pathlib import Path

import numpy as np
import torch
import torch.distributed as dist
import torch.nn.functional as F
from qwen_kron_distill.objective import exact_kl_rows, load_teacher, teacher_distribution
from qwen_normuon_pretrain.data import load_split
from qwen_normuon_pretrain.normuon import SingleDeviceNorMuon
from torch import nn
from torch.nn.parallel import DistributedDataParallel as DDP


CONFIG = {
    "mode": "train",  # self_test | study | benchmark | train
    "seed": 0,
    "context_length": 16,
    "vocab_size": 248_320,
    "vocab_modes": (485, 512),  # source checkpoint only
    "width": 64,
    "depth": 32,
    "rank": 8,
    "physical_local_batch": 245_760,
    "optimizer_local_batch": 16_384,
    "teacher_microbatch": 2_048,
    "train_contexts": 16_777_216,
    "max_hours": 2.0,
    "lr": 0.006,
    "min_lr": 0.00075,
    "weight_decay": 0.01,
    "warmup_contexts": 2_097_152,
    "log_contexts": 1_048_576,
    "eval_contexts": 16_777_216,
    "checkpoint_contexts": 16_777_216,
    "plateau_patience": 4,
    "plateau_delta": 0.005,
    "min_lr_stagnant_evals": 6,
    "eval_examples": 8_192,
    "eval_batch": 512,
    "data_root": "/cache/qwen_fullwidth_distill/context16-fineweb-edu-next-token-4m-v1",
    "source": "/cache/expv6-kiss-long/best.pt",
    "resume": "",
    "output_dir": "/cache/expv7-dense-free/lr-0.006",
    "study_root": "/cache/expv7-dense-free",
    "wandb_project": "qwen-causal-kron-distill",
    "run_name": "exp7-dense-free-lr-0.006",
    "target_kl": 1.0,
    "compile": True,
}


def overrides() -> None:
    for arg in sys.argv[1:]:
        if not arg.startswith("--") or "=" not in arg:
            raise SystemExit(f"expected --key=value, got {arg}")
        key, raw = arg[2:].split("=", 1)
        if key not in CONFIG:
            raise SystemExit(f"unknown option {key}")
        old = CONFIG[key]
        if isinstance(old, bool):
            CONFIG[key] = raw.lower() == "true"
        elif isinstance(old, str):
            CONFIG[key] = raw
        else:
            value = ast.literal_eval(raw)
            CONFIG[key] = tuple(value) if isinstance(old, tuple) else value


def per_token_rms(value: torch.Tensor) -> torch.Tensor:
    scale = torch.rsqrt(value.float().square().mean(-1, keepdim=True) + 1e-6)
    return value * scale.to(value.dtype)


def kron(value: torch.Tensor, a: torch.Tensor, b: torch.Tensor) -> torch.Tensor:
    channel = torch.einsum("btc,roc->brto", value, b)
    return torch.einsum("brto,rst->bso", channel, a) / math.sqrt(a.shape[0])


class Block(nn.Module):
    def __init__(self) -> None:
        super().__init__()
        rank, length, width = CONFIG["rank"], CONFIG["context_length"], CONFIG["width"]
        self.a = nn.Parameter(
            torch.randn(rank, length, length, dtype=torch.bfloat16) / math.sqrt(length)
        )
        self.b = nn.Parameter(
            torch.randn(rank, width, width, dtype=torch.bfloat16) / math.sqrt(width)
        )

    def forward(self, value: torch.Tensor) -> torch.Tensor:
        normalized = F.silu(per_token_rms(value))
        return value + kron(normalized, self.a, self.b) / math.sqrt(CONFIG["depth"])


class Student(nn.Module):
    def __init__(self) -> None:
        super().__init__()
        self.vocabulary = nn.Parameter(
            torch.empty(CONFIG["vocab_size"], CONFIG["width"], dtype=torch.bfloat16)
        )
        nn.init.normal_(self.vocabulary, std=0.02)
        self.blocks = nn.ModuleList(Block() for _ in range(CONFIG["depth"]))

    def hidden(self, token_ids: torch.Tensor) -> torch.Tensor:
        value = F.embedding(token_ids, self.vocabulary)
        for block in self.blocks:
            value = block(value)
        return per_token_rms(value)[:, -1]

    def logits(self, token_ids: torch.Tensor) -> torch.Tensor:
        return F.linear(self.hidden(token_ids), self.vocabulary)

    def forward(
        self, token_ids: torch.Tensor, teacher_probability: torch.Tensor
    ) -> torch.Tensor:
        logits = self.logits(token_ids)
        return -(teacher_probability * F.log_softmax(logits.float(), -1)).sum(-1).mean()


def parameter_count() -> int:
    return sum(parameter.numel() for parameter in Student().parameters())


def clean_state_dict(state: dict[str, torch.Tensor]) -> dict[str, torch.Tensor]:
    prefixes = ("module.", "_orig_mod.")
    clean = {}
    for name, value in state.items():
        while any(name.startswith(prefix) for prefix in prefixes):
            name = name.split(".", 1)[1]
        clean[name] = value
    return clean


def file_sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as source:
        for chunk in iter(lambda: source.read(8 << 20), b""):
            digest.update(chunk)
    return digest.hexdigest()


@torch.no_grad()
def load_initial(student: Student, device: torch.device) -> tuple[dict, dict]:
    """Load a dense resume or exactly materialize the Exp V6 tied factors."""
    resume = Path(CONFIG["resume"])
    source = resume if resume.is_file() else Path(CONFIG["source"])
    if not source.is_file():
        raise RuntimeError(f"checkpoint is missing: {source}")
    saved = torch.load(source, map_location="cpu", weights_only=False)
    state = clean_state_dict(saved["model"])
    if "vocabulary" in state:
        student.load_state_dict(state)
        return saved, {
            "initialization": "dense_resume",
            "source_checkpoint": str(source),
            "source_sha256": file_sha256(source),
        }
    if "vocabulary.0" not in state or "vocabulary.1" not in state:
        raise RuntimeError("source is neither an Exp V6 factorized nor Exp V7 dense checkpoint")
    first, second = state.pop("vocabulary.0"), state.pop("vocabulary.1")
    dense = (first[:, None, :] * second[None, :, :]).reshape(
        CONFIG["vocab_size"], CONFIG["width"]
    )
    missing, unexpected = student.load_state_dict(
        {"vocabulary": dense, **state}, strict=False
    )
    if missing or unexpected:
        raise RuntimeError(f"source mismatch: missing={missing}, unexpected={unexpected}")
    return {}, {
        "initialization": "materialized_expv6_best",
        "source_checkpoint": str(source),
        "source_sha256": file_sha256(source),
        "source_optimizer_updates": int(saved.get("optimizer_updates", saved.get("step", 0))),
        "source_validation_kl": float(saved.get("best_validation_kl", math.nan)),
    }


def self_test() -> None:
    torch.manual_seed(0)
    assert parameter_count() == 17_006_592
    student = Student()
    assert student.vocabulary.shape == (248_320, 64)
    assert all(parameter.ndim >= 2 for parameter in student.parameters())
    value = torch.randn(3, 16, 64, dtype=torch.bfloat16)
    normalized = per_token_rms(value)
    torch.testing.assert_close(
        normalized.float().square().mean(-1), torch.ones(3, 16), atol=1e-2, rtol=1e-2
    )
    token_ids = torch.randint(0, CONFIG["vocab_size"], (3, 16))
    torch.testing.assert_close(
        student.logits(token_ids),
        F.linear(student.hidden(token_ids), student.vocabulary),
    )
    logits = torch.randn(3, 11)
    probability = torch.softmax(torch.randn(3, 11), -1)
    entropy = -(probability * probability.log()).sum(-1)
    loss = -(probability * F.log_softmax(logits, -1)).sum(-1).mean()
    torch.testing.assert_close(loss - entropy.mean(), exact_kl_rows(logits, probability, entropy).mean())
    print(json.dumps({"self_test": "passed", "parameters": parameter_count()}))


@torch.no_grad()
def teacher_targets(teacher, token_ids: torch.Tensor) -> tuple[torch.Tensor, torch.Tensor]:
    probability = torch.empty(
        token_ids.shape[0], CONFIG["vocab_size"], device=token_ids.device, dtype=torch.bfloat16
    )
    entropy = torch.empty(token_ids.shape[0], device=token_ids.device, dtype=torch.float32)
    weight = teacher.get_output_embeddings().weight[: CONFIG["vocab_size"]]
    for start in range(0, len(token_ids), CONFIG["teacher_microbatch"]):
        stop = min(start + CONFIG["teacher_microbatch"], len(token_ids))
        hidden = teacher.model(
            input_ids=token_ids[start:stop], use_cache=False, return_dict=True
        ).last_hidden_state[:, -1]
        log_probability = F.log_softmax(F.linear(hidden, weight).float(), -1)
        chunk = log_probability.exp()
        probability[start:stop].copy_(chunk)
        entropy[start:stop].copy_(-(chunk * log_probability).sum(-1))
    return probability, entropy


@torch.inference_mode()
def evaluate(
    student: nn.Module,
    teacher,
    contexts: np.ndarray,
    targets: np.ndarray,
    rank: int,
    world: int,
    device: torch.device,
) -> dict[str, float]:
    student.eval()
    totals = torch.zeros(5, device=device, dtype=torch.float64)
    indices = np.arange(rank, min(CONFIG["eval_examples"], len(contexts)), world)
    for start in range(0, len(indices), CONFIG["eval_batch"]):
        index = indices[start : start + CONFIG["eval_batch"]]
        token_ids = torch.as_tensor(np.asarray(contexts[index], dtype=np.int64), device=device)
        target = torch.as_tensor(np.asarray(targets[index], dtype=np.int64), device=device)
        probability, log_probability, entropy = teacher_distribution(teacher, token_ids)
        with torch.autocast("cuda", dtype=torch.bfloat16):
            logits = student.logits(token_ids)
        totals[0] += exact_kl_rows(logits, probability, entropy).double().sum()
        totals[1] += F.cross_entropy(logits.float(), target, reduction="sum")
        totals[2] += (-log_probability.gather(1, target[:, None])).double().sum()
        totals[3] += (logits.argmax(-1) == target).double().sum()
        totals[4] += len(index)
    dist.all_reduce(totals)
    student.train()
    count = float(totals[4])
    return {
        "validation_kl": float(totals[0] / count),
        "validation_student_nll": float(totals[1] / count),
        "validation_teacher_nll": float(totals[2] / count),
        "validation_accuracy": float(totals[3] / count),
        "validation_examples": int(count),
    }


def due(previous: int, current: int, interval: int) -> bool:
    return interval > 0 and previous // interval != current // interval


def batch_indices(size: int, outer: int, rank: int, world: int) -> np.ndarray:
    local = CONFIG["physical_local_batch"]
    generator = np.random.default_rng(CONFIG["seed"] + outer)
    indices = generator.choice(size, local * world, replace=False)
    return indices[rank * local : (rank + 1) * local]


def save_checkpoint(
    path: Path,
    student: Student,
    optimizer: torch.optim.Optimizer,
    state: dict,
    metadata: dict,
    wandb_url: str,
) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(".tmp")
    torch.save(
        {
            "schema": "exp7-dense-free-v1",
            "config": dict(CONFIG),
            "model": student.state_dict(),
            "optimizer": optimizer.state_dict(),
            "metadata": metadata,
            "wandb_url": wandb_url,
            **state,
        },
        temporary,
    )
    os.replace(temporary, path)


def train() -> None:
    world = int(os.environ.get("WORLD_SIZE", "1"))
    rank = int(os.environ.get("RANK", "0"))
    local_rank = int(os.environ.get("LOCAL_RANK", "0"))
    if not torch.cuda.is_available() or world != 8:
        raise RuntimeError("Exp V7 paid training requires exactly eight CUDA ranks")
    torch.cuda.set_device(local_rank)
    device = torch.device(f"cuda:{local_rank}")
    dist.init_process_group("nccl", device_id=device)
    primary = rank == 0
    torch.manual_seed(CONFIG["seed"])

    physical_local = int(CONFIG["physical_local_batch"])
    optimizer_local = int(CONFIG["optimizer_local_batch"])
    if physical_local % optimizer_local:
        raise RuntimeError("physical_local_batch must be divisible by optimizer_local_batch")
    global_batch = optimizer_local * world
    global_tokens = global_batch * CONFIG["context_length"]
    if global_tokens < 100_000:
        raise RuntimeError("global token batch violates accelerator policy")

    contexts, _, _ = load_split(CONFIG["data_root"], "train")
    validation_contexts, validation_targets, _ = load_split(CONFIG["data_root"], "validation")
    if physical_local * world > len(contexts):
        raise RuntimeError("physical teacher batch exceeds the training split")
    teacher = load_teacher(str(device))
    student = Student().to(device)
    saved, metadata = load_initial(student, device)
    optimizer = SingleDeviceNorMuon(
        student.parameters(),
        lr=CONFIG["lr"],
        weight_decay=CONFIG["weight_decay"],
        beta1=0.95,
        beta2=0.95,
        ns_steps=5,
        nesterov=True,
        eps=1e-8,
    )

    run_contexts = 0
    total_contexts = 0
    optimizer_updates = 0
    current_lr = float(CONFIG["lr"])
    best_kl = math.inf
    plateau_reference = math.inf
    bad_validations = 0
    minimum_lr_bad_validations = 0
    physical_batches = 0
    resume = Path(CONFIG["resume"])
    if resume.is_file() and saved:
        optimizer.load_state_dict(saved["optimizer"])
        total_contexts = int(saved.get("contexts_seen", 0))
        optimizer_updates = int(saved.get("optimizer_updates", saved.get("step", 0)))
        current_lr = float(saved.get("current_lr", CONFIG["lr"]))
        best_kl = float(saved.get("best_validation_kl", math.inf))
        plateau_reference = float(saved.get("plateau_reference_kl", math.inf))
        bad_validations = int(saved.get("bad_validations", 0))
        minimum_lr_bad_validations = int(saved.get("minimum_lr_bad_validations", 0))
        physical_batches = int(saved.get("physical_batches", 0))
        for group in optimizer.param_groups:
            group["lr"] = current_lr

    compiled = (
        torch.compile(student, fullgraph=True, dynamic=False)
        if CONFIG["compile"]
        else student
    )
    model = DDP(
        compiled, device_ids=[local_rank], broadcast_buffers=False, gradient_as_bucket_view=True
    )

    run = None
    if primary:
        key = os.environ.get("WANDB_API_KEY")
        if not key:
            raise RuntimeError("WANDB_API_KEY is required")
        import wandb

        wandb.login(key=key, relogin=True)
        run = wandb.init(
            project=CONFIG["wandb_project"],
            name=CONFIG["run_name"],
            config={
                **CONFIG,
                **metadata,
                "schema": "exp7-dense-free-v1",
                "parameters": parameter_count(),
                "world_size": world,
                "physical_global_batch": physical_local * world,
                "physical_global_token_batch": physical_local * world * CONFIG["context_length"],
                "optimizer_global_batch": global_batch,
                "optimizer_global_token_batch": global_tokens,
            },
        )
        if not run.url:
            raise RuntimeError("W&B did not return a direct run URL")
        print(f"WANDB_URL={run.url}", flush=True)

    dist.barrier()
    initial = evaluate(
        compiled, teacher, validation_contexts, validation_targets, rank, world, device
    )
    best_kl = min(best_kl, initial["validation_kl"])
    if not math.isfinite(plateau_reference):
        plateau_reference = initial["validation_kl"]
    if primary:
        run.log({**initial, "contexts_seen": total_contexts, "lr": current_lr}, step=total_contexts)
        print(json.dumps({"initial_validation": initial, **metadata}), flush=True)

    output = Path(CONFIG["output_dir"])
    started = time.perf_counter()
    status = "running"
    last_log_contexts = 0
    last_log_time = started
    teacher_seconds = 0.0
    last_loss = math.nan
    last_kl = math.nan
    train_budget = int(CONFIG["train_contexts"])
    if CONFIG["mode"] == "benchmark":
        train_budget = physical_local * world

    while status == "running" and run_contexts < train_budget:
        if time.perf_counter() - started >= CONFIG["max_hours"] * 3600:
            status = "timeout"
            break
        physical_batches += 1
        index = batch_indices(len(contexts), physical_batches, rank, world)
        token_ids = torch.as_tensor(np.asarray(contexts[index], dtype=np.int64), device=device)
        teacher_started = time.perf_counter()
        # no_grad (not inference_mode): the frozen target is later consumed by
        # compiled autograd, which may need to save it for the student backward.
        with torch.no_grad(), torch.autocast("cuda", dtype=torch.bfloat16):
            probability, entropy = teacher_targets(teacher, token_ids)
        teacher_seconds += time.perf_counter() - teacher_started
        for start in range(0, physical_local, optimizer_local):
            if run_contexts >= train_budget:
                break
            stop = start + optimizer_local
            previous_contexts = run_contexts
            optimizer.zero_grad(set_to_none=True)
            with torch.autocast("cuda", dtype=torch.bfloat16):
                cross_entropy = model(token_ids[start:stop], probability[start:stop])
            cross_entropy.backward()
            grad_norm = torch.nn.utils.clip_grad_norm_(student.parameters(), 1.0)
            if not torch.isfinite(cross_entropy) or not torch.isfinite(grad_norm):
                raise RuntimeError("non-finite loss or gradient")
            warmup = min(
                1.0,
                (run_contexts + global_batch) / max(1, CONFIG["warmup_contexts"]),
            )
            step_lr = current_lr * warmup
            for group in optimizer.param_groups:
                group["lr"] = step_lr
            optimizer.step()
            optimizer_updates += 1
            run_contexts += global_batch
            total_contexts += global_batch
            last_loss = float(cross_entropy.detach())
            last_kl = last_loss - float(entropy[start:stop].mean())

            if due(last_log_contexts, run_contexts, CONFIG["log_contexts"]) or optimizer_updates == 1:
                values = torch.tensor([last_loss, last_kl, float(grad_norm)], device=device)
                dist.all_reduce(values)
                values /= world
                now = time.perf_counter()
                interval_contexts = run_contexts - last_log_contexts
                interval_seconds = now - last_log_time
                memory = torch.tensor(
                    [
                        torch.cuda.max_memory_allocated() / 2**30,
                        torch.cuda.max_memory_reserved() / 2**30,
                    ],
                    device=device,
                )
                memories = [torch.empty_like(memory) for _ in range(world)]
                dist.all_gather(memories, memory)
                metrics = {
                    "train_cross_entropy": float(values[0]),
                    "train_kl": float(values[1]),
                    "grad_norm_pre_clip": float(values[2]),
                    "gradient_clipped": float(values[2] > 1.0),
                    "lr": step_lr,
                    "optimizer_updates": optimizer_updates,
                    "run_contexts": run_contexts,
                    "contexts_seen": total_contexts,
                    "input_tokens_seen": total_contexts * CONFIG["context_length"],
                    "optimizer_global_batch": global_batch,
                    "optimizer_global_token_batch": global_tokens,
                    "tokens_per_second": interval_contexts * CONFIG["context_length"] / interval_seconds,
                    "end_to_end_tokens_per_second": (
                        run_contexts
                        * CONFIG["context_length"]
                        / max(now - started, 1e-9)
                    ),
                    "teacher_target_seconds": teacher_seconds,
                    "per_gpu_peak_allocated_gib": [float(value[0]) for value in memories],
                    "per_gpu_peak_reserved_gib": [float(value[1]) for value in memories],
                }
                if primary:
                    print(json.dumps(metrics), flush=True)
                    run.log(metrics, step=total_contexts)
                last_log_contexts, last_log_time = run_contexts, now

            validation = None
            if due(previous_contexts, run_contexts, CONFIG["eval_contexts"]):
                validation = evaluate(
                    compiled, teacher, validation_contexts, validation_targets, rank, world, device
                )
                improved = validation["validation_kl"] < best_kl
                best_kl = min(best_kl, validation["validation_kl"])
                if validation["validation_kl"] <= plateau_reference - CONFIG["plateau_delta"]:
                    plateau_reference = validation["validation_kl"]
                    bad_validations = 0
                    minimum_lr_bad_validations = 0
                else:
                    bad_validations += 1
                    if current_lr <= CONFIG["min_lr"]:
                        minimum_lr_bad_validations += 1
                if bad_validations >= CONFIG["plateau_patience"] and current_lr > CONFIG["min_lr"]:
                    current_lr = max(CONFIG["min_lr"], current_lr / 2)
                    plateau_reference = validation["validation_kl"]
                    bad_validations = 0
                if primary:
                    run.log({**validation, "best_validation_kl": best_kl, "lr": current_lr}, step=total_contexts)
                    print(json.dumps(validation), flush=True)
                if validation["validation_kl"] <= CONFIG["target_kl"]:
                    status = "target"
                elif minimum_lr_bad_validations >= CONFIG["min_lr_stagnant_evals"]:
                    status = "plateau"

            state = {
                "optimizer_updates": optimizer_updates,
                "contexts_seen": total_contexts,
                "run_contexts": run_contexts,
                "physical_batches": physical_batches,
                "current_lr": current_lr,
                "best_validation_kl": best_kl,
                "plateau_reference_kl": plateau_reference,
                "bad_validations": bad_validations,
                "minimum_lr_bad_validations": minimum_lr_bad_validations,
                "last_validation": validation,
            }
            if validation is not None or due(previous_contexts, run_contexts, CONFIG["checkpoint_contexts"]):
                if primary:
                    save_checkpoint(output / "checkpoint.pt", student, optimizer, state, metadata, run.url)
                    if validation is not None and improved:
                        save_checkpoint(output / "best.pt", student, optimizer, state, metadata, run.url)
                dist.barrier()
            if status != "running":
                break
        del token_ids, probability, entropy

    if status == "running":
        status = "complete"
    elapsed = time.perf_counter() - started
    final = evaluate(
        compiled, teacher, validation_contexts, validation_targets, rank, world, device
    )
    best_kl = min(best_kl, final["validation_kl"])
    state = {
        "optimizer_updates": optimizer_updates,
        "contexts_seen": total_contexts,
        "run_contexts": run_contexts,
        "physical_batches": physical_batches,
        "current_lr": current_lr,
        "best_validation_kl": best_kl,
        "plateau_reference_kl": plateau_reference,
        "bad_validations": bad_validations,
        "minimum_lr_bad_validations": minimum_lr_bad_validations,
        "last_validation": final,
    }
    if primary:
        save_checkpoint(output / "checkpoint.pt", student, optimizer, state, metadata, run.url)
        if final["validation_kl"] <= best_kl:
            save_checkpoint(output / "best.pt", student, optimizer, state, metadata, run.url)
        result = {
            "schema": "exp7-dense-free-result-v1",
            "status": status,
            **metadata,
            **final,
            "best_validation_kl": best_kl,
            "parameters": parameter_count(),
            "optimizer_updates": optimizer_updates,
            "run_contexts": run_contexts,
            "contexts_seen": total_contexts,
            "input_tokens_seen": total_contexts * CONFIG["context_length"],
            "run_elapsed_seconds": elapsed,
            "tokens_per_second": run_contexts * CONFIG["context_length"] / max(elapsed, 1e-9),
            "teacher_target_seconds": teacher_seconds,
            "optimizer_global_batch": global_batch,
            "optimizer_global_token_batch": global_tokens,
            "wandb_url": run.url,
        }
        output.mkdir(parents=True, exist_ok=True)
        (output / "result.json").write_text(json.dumps(result, indent=2))
        run.log(result, step=total_contexts)
        run.finish()
        print(json.dumps(result, indent=2), flush=True)
    dist.barrier()
    dist.destroy_process_group()


def study() -> None:
    """Sequential batch/LR screen followed by a resumable two-hour winner run."""
    if not os.environ.get("WANDB_API_KEY"):
        raise RuntimeError("WANDB_API_KEY is required before the paid study")
    root = Path(CONFIG["study_root"])
    root.mkdir(parents=True, exist_ok=True)
    base = [
        sys.executable,
        "-m",
        "torch.distributed.run",
        "--standalone",
        "--nproc_per_node=8",
        str(Path(__file__).resolve()),
    ]

    def launch(arguments: list[str]) -> None:
        print(json.dumps({"launch": arguments}), flush=True)
        subprocess.run(base + arguments, check=True)

    # 8,192 was completed during the corrected paid preflight. Sweep upward.
    benchmark_results = [root / "bench-8192-retry" / "result.json"]
    for local in (12_288, 16_384):
        output = root / f"bench-{local}"
        launch(
            [
                "--mode=benchmark",
                f"--optimizer_local_batch={local}",
                "--physical_local_batch=245760",
                "--max_hours=0.5",
                "--log_contexts=524288",
                "--eval_contexts=0",
                "--checkpoint_contexts=0",
                f"--output_dir={output}",
                f"--run_name=exp7-dense-free-bench-{local}",
            ]
        )
        benchmark_results.append(output / "result.json")
    benchmarks = [json.loads(path.read_text()) for path in benchmark_results]
    winner_batch = max(benchmarks, key=lambda value: value["tokens_per_second"])[
        "optimizer_global_batch"
    ] // 8

    screen_results = []
    for lr in (0.003, 0.006, 0.0125, 0.025):
        label = str(lr).replace(".", "p")
        output = root / f"lr-{label}"
        launch(
            [
                "--mode=train",
                f"--optimizer_local_batch={winner_batch}",
                "--physical_local_batch=245760",
                "--train_contexts=16777216",
                "--max_hours=1.0",
                f"--lr={lr}",
                f"--min_lr={lr / 8}",
                "--eval_contexts=16777216",
                "--checkpoint_contexts=16777216",
                f"--output_dir={output}",
                f"--run_name=exp7-dense-free-lr-{label}",
            ]
        )
        screen_results.append((lr, output, json.loads((output / "result.json").read_text())))
    winner_lr, winner_output, winner = min(
        screen_results, key=lambda item: (item[2]["validation_kl"], item[0])
    )

    launch(
        [
            "--mode=train",
            f"--optimizer_local_batch={winner_batch}",
            "--physical_local_batch=245760",
            "--train_contexts=268435456",
            "--max_hours=2.0",
            f"--lr={winner_lr}",
            f"--min_lr={winner_lr / 8}",
            "--eval_contexts=16777216",
            "--checkpoint_contexts=16777216",
            f"--resume={winner_output / 'best.pt'}",
            f"--output_dir={winner_output}",
            f"--run_name=exp7-dense-free-long-lr-{str(winner_lr).replace('.', 'p')}",
        ]
    )
    final = json.loads((winner_output / "result.json").read_text())
    summary = {
        "schema": "exp7-dense-free-study-v1",
        "status": "complete",
        "winner_optimizer_local_batch": winner_batch,
        "winner_lr": winner_lr,
        "benchmarks": benchmarks,
        "screens": [value for _, _, value in screen_results],
        "screen_winner": winner,
        "final": final,
    }
    (root / "study.json").write_text(json.dumps(summary, indent=2))
    print(json.dumps(summary, indent=2), flush=True)


if __name__ == "__main__":
    overrides()
    if CONFIG["mode"] == "self_test":
        self_test()
    elif CONFIG["mode"] == "study":
        study()
    else:
        train()
