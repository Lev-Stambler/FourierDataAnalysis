"""Exp V7: dense-free tied vocabulary, deliberately kept in one file."""

from __future__ import annotations

import ast
import hashlib
import json
import math
import os
import queue
import subprocess
import sys
import threading
import time
import uuid
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
    "mode": "train",  # self_test | study | benchmark | train | long | long_supervisor
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
    "train_tokens": 1_000_000_000_000,
    "optimizer": "adamw8bit",
    "reset_optimizer": False,
    "long_lr": 0.003,
    "long_output_dir": "",
    "max_hours": 2.0,
    "lr": 0.006,
    "min_lr": 0.00075,
    "weight_decay": 0.1,
    "warmup_contexts": 2_097_152,
    "log_contexts": 1_048_576,
    "eval_contexts": 16_777_216,
    "checkpoint_contexts": 16_777_216,
    "log_tokens": 16_777_216,
    "eval_tokens": 268_435_456,
    "checkpoint_tokens": 268_435_456,
    "plateau_patience": 4,
    "plateau_delta": 0.005,
    "min_lr_stagnant_evals": 6,
    "eval_examples": 8_192,
    "eval_batch": 512,
    "data_root": "/cache/qwen_fullwidth_distill/context16-fineweb-edu-next-token-4m-v1",
    "stream_root": "/cache/expv7-dense-free/fineweb-edu-stream",
    "fineweb_id": "HuggingFaceFW/fineweb-edu",
    "fineweb_config": "default",
    "fineweb_revision": "87f09149ef4734204d70ed1d046ddc9ca3f2b8f9",
    "model_id": "Qwen/Qwen3.5-0.8B-Base",
    "model_revision": "5c8a1b97ddef11f79b47ab9d07bf82b9117413f6",
    "stream_prefetch": 4,
    "stream_tokenize_documents": 256,
    "stream_shuffle_buffer": 10_000,
    "stream_benchmark_batches": 3,
    "source": "/cache/expv6-kiss-long/best.pt",
    "resume": "",
    "output_dir": "/cache/expv7-dense-free/lr-0.006",
    "study_root": "/cache/expv7-dense-free",
    "wandb_project": "qwen-causal-kron-distill",
    "wandb_id": "",
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


def cosine_lr(start: float, progress_tokens: int, budget_tokens: int) -> float:
    """Token-based cosine from ``start`` to one percent of ``start``."""
    fraction = min(1.0, max(0.0, progress_tokens / max(1, budget_tokens)))
    minimum = start * 0.01
    return minimum + 0.5 * (start - minimum) * (1.0 + math.cos(math.pi * fraction))


def exact_local_contexts(remaining_tokens: int, world: int) -> int:
    """Return the equal per-rank context count for an exact input-token tail."""
    denominator = CONFIG["context_length"] * world
    if remaining_tokens <= 0 or remaining_tokens % denominator:
        raise ValueError(
            f"remaining token budget {remaining_tokens} is not divisible by {denominator}"
        )
    return remaining_tokens // denominator


def document_contexts(token_ids) -> np.ndarray:
    """All non-overlapping 16-input/one-target windows from one document."""
    length = CONFIG["context_length"] + 1
    values = np.asarray(token_ids, dtype=np.int32)
    count = len(values) // length
    if count == 0:
        return np.empty((0, CONFIG["context_length"]), dtype=np.int32)
    return values[: count * length].reshape(count, length)[:, :-1].copy()


def _hash_set(data_root: str, splits: tuple[str, ...]) -> set[bytes]:
    values = []
    root = Path(data_root)
    for split in splits:
        path = root / f"{split}_hashes.npy"
        if path.is_file():
            values.append(np.asarray(np.load(path, mmap_mode="r")))
    if not values:
        return set()
    return {bytes(value) for value in np.unique(np.concatenate(values))}


class FineWebEduStream:
    """Rank-sharded FineWeb-Edu producer with an in-memory asynchronous queue."""

    def __init__(
        self,
        rank: int,
        world: int,
        local_batch: int,
        resume_state: dict | None = None,
    ) -> None:
        os.environ.setdefault("RAYON_NUM_THREADS", "4")
        os.environ.setdefault("TOKENIZERS_PARALLELISM", "true")
        self.rank = rank
        self.world = world
        self.local_batch = local_batch
        self.resume_state = resume_state or {}
        self.ready: queue.Queue = queue.Queue(maxsize=int(CONFIG["stream_prefetch"]))
        self.stop = threading.Event()
        self.thread = threading.Thread(target=self._produce, daemon=True)
        self.thread.start()

    def _put(self, item) -> bool:
        while not self.stop.is_set():
            try:
                self.ready.put(item, timeout=0.25)
                return True
            except queue.Full:
                pass
        return False

    def _open(self, epoch: int, state: dict | None):
        from datasets import load_dataset

        dataset = load_dataset(
            CONFIG["fineweb_id"],
            name=CONFIG["fineweb_config"],
            split="train",
            streaming=True,
            revision=CONFIG["fineweb_revision"],
        )
        dataset = dataset.shuffle(
            seed=CONFIG["seed"] + epoch,
            buffer_size=int(CONFIG["stream_shuffle_buffer"]),
        )
        dataset = dataset.shard(num_shards=self.world, index=self.rank)
        if state:
            dataset.load_state_dict(state)
        return dataset

    def _produce(self) -> None:
        try:
            from transformers import AutoTokenizer

            tokenizer = AutoTokenizer.from_pretrained(
                CONFIG["model_id"],
                revision=CONFIG["model_revision"],
                use_fast=True,
            )
            epoch = int(self.resume_state.get("epoch", 0))
            ordinal = int(self.resume_state.get("batch_ordinal", 0))
            pending = np.asarray(
                self.resume_state.get(
                    "pending",
                    np.empty((0, CONFIG["context_length"]), dtype=np.int32),
                ),
                dtype=np.int32,
            ).reshape(-1, CONFIG["context_length"])
            dataset = self._open(epoch, self.resume_state.get("dataset_state"))
            iterator = iter(dataset)
            held_out = _hash_set(CONFIG["data_root"], ("validation", "test"))
            seen_by_screen = _hash_set(CONFIG["data_root"], ("train",))
            document_batch = int(CONFIG["stream_tokenize_documents"])

            while not self.stop.is_set():
                started = time.perf_counter()
                while len(pending) < self.local_batch:
                    texts = []
                    while len(texts) < document_batch:
                        try:
                            record = next(iterator)
                        except StopIteration:
                            epoch += 1
                            dataset = self._open(epoch, None)
                            iterator = iter(dataset)
                            continue
                        text = record.get("text") or ""
                        if not text:
                            continue
                        digest = hashlib.sha256(text.encode("utf-8", "ignore")).digest()
                        if digest in held_out or (epoch == 0 and digest in seen_by_screen):
                            continue
                        texts.append(text)
                    tokenized = tokenizer(
                        texts,
                        add_special_tokens=False,
                        padding=False,
                        truncation=False,
                    )["input_ids"]
                    chunks = [
                        contexts
                        for ids in tokenized
                        if len(contexts := document_contexts(ids))
                    ]
                    if chunks:
                        addition = np.concatenate(chunks)
                        pending = (
                            addition
                            if not len(pending)
                            else np.concatenate((pending, addition))
                        )

                contexts = np.ascontiguousarray(pending[: self.local_batch])
                pending = np.ascontiguousarray(pending[self.local_batch :])
                ordinal += 1
                state = {
                    "epoch": epoch,
                    "batch_ordinal": ordinal,
                    "dataset_state": dataset.state_dict(),
                    "pending": pending.copy(),
                }
                if not self._put(
                    {
                        "contexts": contexts,
                        "state": state,
                        "producer_seconds": time.perf_counter() - started,
                    }
                ):
                    return
        except BaseException as error:
            self._put(error)

    def next(self) -> tuple[np.ndarray, dict, float, float, int]:
        started = time.perf_counter()
        item = self.ready.get()
        waited = time.perf_counter() - started
        if isinstance(item, BaseException):
            raise RuntimeError("FineWeb-Edu producer failed") from item
        return (
            item["contexts"],
            item["state"],
            waited,
            float(item["producer_seconds"]),
            self.ready.qsize(),
        )

    def close(self) -> None:
        self.stop.set()
        while True:
            try:
                self.ready.get_nowait()
            except queue.Empty:
                break
        self.thread.join()


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
    long_run = CONFIG["mode"] == "long"
    if physical_local % optimizer_local:
        raise RuntimeError("physical_local_batch must be divisible by optimizer_local_batch")
    global_batch = optimizer_local * world
    global_tokens = global_batch * CONFIG["context_length"]
    if global_tokens < 100_000:
        raise RuntimeError("global token batch violates accelerator policy")

    contexts = None
    if not long_run:
        contexts, _, _ = load_split(CONFIG["data_root"], "train")
    validation_contexts, validation_targets, _ = load_split(CONFIG["data_root"], "validation")
    if not long_run and physical_local * world > len(contexts):
        raise RuntimeError("physical teacher batch exceeds the training split")
    teacher = load_teacher(str(device))
    student = Student().to(device)
    saved, metadata = load_initial(student, device)
    if CONFIG["optimizer"] == "adamw8bit":
        from bitsandbytes.optim import AdamW8bit

        optimizer = AdamW8bit(
            student.parameters(),
            lr=CONFIG["lr"],
            betas=(0.9, 0.95),
            eps=1e-8,
            weight_decay=CONFIG["weight_decay"],
        )
    elif CONFIG["optimizer"] == "normuon":
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
    else:
        raise ValueError(f"unknown optimizer: {CONFIG['optimizer']}")

    run_contexts = 0
    total_contexts = 0
    optimizer_updates = 0
    current_lr = float(CONFIG["lr"])
    best_kl = math.inf
    plateau_reference = math.inf
    bad_validations = 0
    minimum_lr_bad_validations = 0
    physical_batches = 0
    long_input_tokens_seen = 0
    stream_states = None
    wandb_id = CONFIG["wandb_id"]
    resume = Path(CONFIG["resume"])
    if resume.is_file() and saved:
        saved_optimizer = str(saved.get("optimizer_name", "normuon"))
        if saved_optimizer == CONFIG["optimizer"] and not CONFIG["reset_optimizer"]:
            optimizer.load_state_dict(saved["optimizer"])
        total_contexts = int(saved.get("contexts_seen", 0))
        optimizer_updates = int(saved.get("optimizer_updates", saved.get("step", 0)))
        current_lr = float(saved.get("current_lr", CONFIG["lr"]))
        best_kl = float(saved.get("best_validation_kl", math.inf))
        plateau_reference = float(saved.get("plateau_reference_kl", math.inf))
        bad_validations = int(saved.get("bad_validations", 0))
        minimum_lr_bad_validations = int(saved.get("minimum_lr_bad_validations", 0))
        physical_batches = int(saved.get("physical_batches", 0))
        long_input_tokens_seen = int(saved.get("long_input_tokens_seen", 0))
        stream_states = saved.get("stream_states")
        if not wandb_id:
            wandb_id = str(saved.get("wandb_id", ""))
        for group in optimizer.param_groups:
            group["lr"] = current_lr

    stream = None
    local_stream_state = None
    if long_run:
        if int(CONFIG["train_tokens"]) % (CONFIG["context_length"] * world):
            raise RuntimeError("train_tokens must divide evenly across ranks and contexts")
        local_stream_state = stream_states[rank] if stream_states else None
        stream = FineWebEduStream(rank, world, physical_local, local_stream_state)

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
        if not wandb_id:
            wandb_id = wandb.util.generate_id()
        run = wandb.init(
            project=CONFIG["wandb_project"],
            name=CONFIG["run_name"],
            id=wandb_id,
            resume="allow",
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
                "long_input_token_budget": int(CONFIG["train_tokens"]) if long_run else 0,
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
        run.log(
            {
                **initial,
                "contexts_seen": total_contexts,
                "input_tokens_seen": total_contexts * CONFIG["context_length"],
                "long_input_tokens_seen": long_input_tokens_seen,
                "lr": current_lr,
            },
            step=total_contexts * CONFIG["context_length"],
        )
        print(json.dumps({"initial_validation": initial, **metadata}), flush=True)

    output = Path(CONFIG["output_dir"])
    session_start_long_tokens = long_input_tokens_seen
    started = time.perf_counter()
    status = "running"
    last_log_contexts = 0
    last_log_tokens = long_input_tokens_seen
    last_log_time = started
    teacher_seconds = 0.0
    data_wait_seconds = 0.0
    producer_seconds = 0.0
    stream_queue_depth = 0
    last_loss = math.nan
    last_kl = math.nan
    train_budget = int(CONFIG["train_contexts"])
    if CONFIG["mode"] == "benchmark":
        train_budget = physical_local * world

    def unfinished() -> bool:
        return (
            long_input_tokens_seen < int(CONFIG["train_tokens"])
            if long_run
            else run_contexts < train_budget
        )

    while status == "running" and unfinished():
        if CONFIG["max_hours"] > 0 and time.perf_counter() - started >= CONFIG["max_hours"] * 3600:
            status = "timeout"
            break
        physical_batches += 1
        previous_outer_tokens = long_input_tokens_seen
        if long_run:
            remaining_tokens = int(CONFIG["train_tokens"]) - long_input_tokens_seen
            local_count = min(
                physical_local,
                exact_local_contexts(remaining_tokens, world),
            )
            streamed, local_stream_state, waited, produced, stream_queue_depth = stream.next()
            data_wait_seconds += waited
            producer_seconds += produced
            token_ids = torch.as_tensor(
                np.asarray(streamed[:local_count], dtype=np.int64),
                device=device,
            )
        else:
            local_count = physical_local
            index = batch_indices(len(contexts), physical_batches, rank, world)
            token_ids = torch.as_tensor(np.asarray(contexts[index], dtype=np.int64), device=device)
        teacher_started = time.perf_counter()
        # no_grad (not inference_mode): the frozen target is later consumed by
        # compiled autograd, which may need to save it for the student backward.
        with torch.no_grad(), torch.autocast("cuda", dtype=torch.bfloat16):
            probability, entropy = teacher_targets(teacher, token_ids)
        teacher_seconds += time.perf_counter() - teacher_started
        for start in range(0, local_count, optimizer_local):
            if not long_run and run_contexts >= train_budget:
                break
            stop = min(start + optimizer_local, local_count)
            step_local_contexts = stop - start
            step_global_contexts = step_local_contexts * world
            step_tokens = step_global_contexts * CONFIG["context_length"]
            previous_contexts = run_contexts
            optimizer.zero_grad(set_to_none=True)
            with torch.autocast("cuda", dtype=torch.bfloat16):
                cross_entropy = model(token_ids[start:stop], probability[start:stop])
            cross_entropy.backward()
            grad_norm = torch.nn.utils.clip_grad_norm_(student.parameters(), 1.0)
            if not torch.isfinite(cross_entropy) or not torch.isfinite(grad_norm):
                raise RuntimeError("non-finite loss or gradient")
            if long_run:
                step_lr = cosine_lr(
                    float(CONFIG["lr"]),
                    long_input_tokens_seen,
                    int(CONFIG["train_tokens"]),
                )
                current_lr = step_lr
            else:
                warmup = min(
                    1.0,
                    (run_contexts + global_batch) / max(1, CONFIG["warmup_contexts"]),
                )
                step_lr = current_lr * warmup
            for group in optimizer.param_groups:
                group["lr"] = step_lr
            optimizer.step()
            optimizer_updates += 1
            run_contexts += step_global_contexts
            total_contexts += step_global_contexts
            if long_run:
                long_input_tokens_seen += step_tokens
            last_loss = float(cross_entropy.detach())
            last_kl = last_loss - float(entropy[start:stop].mean())

            log_due = (
                due(last_log_tokens, long_input_tokens_seen, CONFIG["log_tokens"])
                if long_run
                else due(last_log_contexts, run_contexts, CONFIG["log_contexts"])
            )
            if log_due or (long_input_tokens_seen == step_tokens if long_run else optimizer_updates == 1):
                values = torch.tensor([last_loss, last_kl, float(grad_norm)], device=device)
                dist.all_reduce(values)
                values /= world
                now = time.perf_counter()
                interval_tokens = (
                    long_input_tokens_seen - last_log_tokens
                    if long_run
                    else (run_contexts - last_log_contexts) * CONFIG["context_length"]
                )
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
                    "long_input_tokens_seen": long_input_tokens_seen,
                    "long_input_token_budget": int(CONFIG["train_tokens"]) if long_run else 0,
                    "optimizer_global_batch": step_global_contexts,
                    "optimizer_global_token_batch": step_tokens,
                    "tokens_per_second": interval_tokens / interval_seconds,
                    "end_to_end_tokens_per_second": (
                        (
                            long_input_tokens_seen - session_start_long_tokens
                            if long_run
                            else run_contexts * CONFIG["context_length"]
                        )
                        / max(now - started, 1e-9)
                    ),
                    "teacher_target_seconds": teacher_seconds,
                    "data_wait_seconds": data_wait_seconds,
                    "producer_seconds": producer_seconds,
                    "stream_queue_depth": stream_queue_depth if long_run else 0,
                    "stream_epoch": (
                        int(local_stream_state["epoch"])
                        if long_run and local_stream_state is not None
                        else 0
                    ),
                    "per_gpu_peak_allocated_gib": [float(value[0]) for value in memories],
                    "per_gpu_peak_reserved_gib": [float(value[1]) for value in memories],
                }
                if primary:
                    print(json.dumps(metrics), flush=True)
                    run.log(metrics, step=total_contexts * CONFIG["context_length"])
                last_log_contexts = run_contexts
                last_log_tokens = long_input_tokens_seen
                last_log_time = now

            validation = None
            if not long_run and due(previous_contexts, run_contexts, CONFIG["eval_contexts"]):
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
                    run.log(
                        {**validation, "best_validation_kl": best_kl, "lr": current_lr},
                        step=total_contexts * CONFIG["context_length"],
                    )
                    print(json.dumps(validation), flush=True)
                if validation["validation_kl"] <= CONFIG["target_kl"]:
                    status = "target"
                elif minimum_lr_bad_validations >= CONFIG["min_lr_stagnant_evals"]:
                    status = "plateau"

            state = {
                "optimizer_updates": optimizer_updates,
                "optimizer_name": CONFIG["optimizer"],
                "contexts_seen": total_contexts,
                "run_contexts": run_contexts,
                "physical_batches": physical_batches,
                "current_lr": current_lr,
                "best_validation_kl": best_kl,
                "plateau_reference_kl": plateau_reference,
                "bad_validations": bad_validations,
                "minimum_lr_bad_validations": minimum_lr_bad_validations,
                "long_input_tokens_seen": long_input_tokens_seen,
                "stream_states": stream_states,
                "wandb_id": wandb_id,
                "last_validation": validation,
            }
            if not long_run and (
                validation is not None
                or due(previous_contexts, run_contexts, CONFIG["checkpoint_contexts"])
            ):
                if primary:
                    save_checkpoint(output / "checkpoint.pt", student, optimizer, state, metadata, run.url)
                    if validation is not None and improved:
                        save_checkpoint(output / "best.pt", student, optimizer, state, metadata, run.url)
                dist.barrier()
            if status != "running":
                break

        if long_run:
            validation = None
            eval_due = due(
                previous_outer_tokens,
                long_input_tokens_seen,
                int(CONFIG["eval_tokens"]),
            ) or long_input_tokens_seen == int(CONFIG["train_tokens"])
            checkpoint_due = due(
                previous_outer_tokens,
                long_input_tokens_seen,
                int(CONFIG["checkpoint_tokens"]),
            ) or long_input_tokens_seen == int(CONFIG["train_tokens"])
            improved = False
            if eval_due:
                validation = evaluate(
                    compiled,
                    teacher,
                    validation_contexts,
                    validation_targets,
                    rank,
                    world,
                    device,
                )
                improved = validation["validation_kl"] < best_kl
                best_kl = min(best_kl, validation["validation_kl"])
                if primary:
                    run.log(
                        {
                            **validation,
                            "best_validation_kl": best_kl,
                            "lr": current_lr,
                            "long_input_tokens_seen": long_input_tokens_seen,
                        },
                        step=total_contexts * CONFIG["context_length"],
                    )
                    print(json.dumps(validation), flush=True)
            if checkpoint_due or eval_due:
                gathered_states = [None for _ in range(world)]
                dist.all_gather_object(gathered_states, local_stream_state)
                stream_states = gathered_states
                state = {
                    "optimizer_updates": optimizer_updates,
                    "optimizer_name": CONFIG["optimizer"],
                    "contexts_seen": total_contexts,
                    "run_contexts": run_contexts,
                    "physical_batches": physical_batches,
                    "current_lr": current_lr,
                    "best_validation_kl": best_kl,
                    "plateau_reference_kl": plateau_reference,
                    "bad_validations": bad_validations,
                    "minimum_lr_bad_validations": minimum_lr_bad_validations,
                    "long_input_tokens_seen": long_input_tokens_seen,
                    "stream_states": stream_states,
                    "wandb_id": wandb_id,
                    "last_validation": validation,
                }
                if primary:
                    save_checkpoint(
                        output / "checkpoint.pt",
                        student,
                        optimizer,
                        state,
                        metadata,
                        run.url,
                    )
                    if validation is not None and improved:
                        save_checkpoint(
                            output / "best.pt",
                            student,
                            optimizer,
                            state,
                            metadata,
                            run.url,
                        )
                dist.barrier()
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
        "optimizer_name": CONFIG["optimizer"],
        "contexts_seen": total_contexts,
        "run_contexts": run_contexts,
        "physical_batches": physical_batches,
        "current_lr": current_lr,
        "best_validation_kl": best_kl,
        "plateau_reference_kl": plateau_reference,
        "bad_validations": bad_validations,
        "minimum_lr_bad_validations": minimum_lr_bad_validations,
        "long_input_tokens_seen": long_input_tokens_seen,
        "stream_states": stream_states,
        "wandb_id": wandb_id,
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
            "long_input_tokens_seen": long_input_tokens_seen,
            "long_input_token_budget": int(CONFIG["train_tokens"]) if long_run else 0,
            "run_elapsed_seconds": elapsed,
            "tokens_per_second": (
                (
                    long_input_tokens_seen - session_start_long_tokens
                    if long_run
                    else run_contexts * CONFIG["context_length"]
                )
                / max(elapsed, 1e-9)
            ),
            "teacher_target_seconds": teacher_seconds,
            "data_wait_seconds": data_wait_seconds,
            "producer_seconds": producer_seconds,
            "optimizer_global_batch": global_batch,
            "optimizer_global_token_batch": global_tokens,
            "wandb_url": run.url,
        }
        output.mkdir(parents=True, exist_ok=True)
        (output / "result.json").write_text(json.dumps(result, indent=2))
        run.log(result, step=total_contexts * CONFIG["context_length"])
        run.finish()
        print(json.dumps(result, indent=2), flush=True)
    dist.barrier()
    dist.destroy_process_group()
    if long_run:
        # The HF/Arrow streaming stack owns background HTTP workers. A direct
        # successful process exit after W&B/checkpoint finalization avoids
        # racing those workers during CPython interpreter teardown.
        os._exit(0)


def study() -> None:
    """Sequential batch/LR screen followed by the exact 1T-token run."""
    if not os.environ.get("WANDB_API_KEY"):
        raise RuntimeError("WANDB_API_KEY is required before the paid study")
    root = Path(CONFIG["study_root"])
    root.mkdir(parents=True, exist_ok=True)
    gpu_memory_gib = torch.cuda.get_device_properties(0).total_memory / 2**30
    h200 = gpu_memory_gib >= 100
    physical_local = 245_760 if h200 else 131_072
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

    if h200:
        # 8,192 was completed during the corrected paid preflight. Sweep upward.
        benchmark_results = [root / "bench-8192-retry" / "result.json"]
        for local in (12_288, 16_384):
            output = root / f"bench-{local}"
            launch(
                [
                    "--mode=benchmark",
                    f"--optimizer_local_batch={local}",
                    f"--physical_local_batch={physical_local}",
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
    else:
        # 131,072 target contexts occupy about 61 GiB; with the teacher and
        # dense student this keeps an 80-GiB H100 full without accumulation.
        winner_batch = 8_192
        benchmarks = []

    screen_results = []
    for lr in (0.003, 0.006, 0.0125, 0.025):
        label = str(lr).replace(".", "p")
        output = root / f"lr-{label}"
        launch(
            [
                "--mode=train",
                f"--optimizer_local_batch={winner_batch}",
                f"--physical_local_batch={physical_local}",
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

    summary = {
        "schema": "exp7-dense-free-study-v1",
        "status": "screen_complete",
        "winner_optimizer_local_batch": winner_batch,
        "gpu_memory_gib": gpu_memory_gib,
        "physical_local_batch": physical_local,
        "winner_lr": winner_lr,
        "benchmarks": benchmarks,
        "screens": [value for _, _, value in screen_results],
        "screen_winner": winner,
    }
    (root / "study.json").write_text(json.dumps(summary, indent=2))
    print(json.dumps(summary, indent=2), flush=True)
    long_supervisor()


def screen_winner() -> tuple[float, Path, dict]:
    root = Path(CONFIG["study_root"])
    candidates = []
    for lr in (0.003, 0.006, 0.0125, 0.025):
        output = root / f"lr-{str(lr).replace('.', 'p')}"
        result_path = output / "result.json"
        if not result_path.is_file():
            raise RuntimeError(f"screen is incomplete: {result_path}")
        candidates.append((lr, output, json.loads(result_path.read_text())))
    return min(candidates, key=lambda item: (item[2]["validation_kl"], item[0]))


def long_supervisor() -> None:
    """Run and resume the exact 1T-token job after selecting the screen winner."""
    if not os.environ.get("WANDB_API_KEY"):
        raise RuntimeError("WANDB_API_KEY is required before the paid long run")
    winner_lr, winner_output, winner = screen_winner()
    long_lr = float(CONFIG["long_lr"])
    wandb_id = uuid.uuid4().hex[:8]
    output = (
        Path(CONFIG["long_output_dir"])
        if CONFIG["long_output_dir"]
        else Path(CONFIG["study_root"]) / "long-1t"
    )
    output.mkdir(parents=True, exist_ok=True)
    gpu_memory_gib = torch.cuda.get_device_properties(0).total_memory / 2**30
    physical_local = 245_760 if gpu_memory_gib >= 100 else 131_072
    optimizer_local = 16_384 if gpu_memory_gib >= 100 else 8_192
    base = [
        sys.executable,
        "-m",
        "torch.distributed.run",
        "--standalone",
        "--nproc_per_node=8",
        str(Path(__file__).resolve()),
    ]
    failures = 0
    while True:
        checkpoint = output / "checkpoint.pt"
        resume = (
            checkpoint
            if checkpoint.is_file()
            else Path(CONFIG["resume"])
            if CONFIG["resume"]
            else winner_output / "best.pt"
        )
        reset_optimizer = not checkpoint.is_file()
        arguments = [
            "--mode=long",
            f"--optimizer_local_batch={optimizer_local}",
            f"--physical_local_batch={physical_local}",
            "--train_tokens=1000000000000",
            "--max_hours=0",
            f"--lr={long_lr}",
            f"--min_lr={long_lr * 0.01}",
            f"--resume={resume}",
            f"--output_dir={output}",
            f"--run_name=exp7-dense-free-1t-lr-{str(long_lr).replace('.', 'p')}",
            f"--wandb_id={wandb_id}",
            f"--reset_optimizer={str(reset_optimizer).lower()}",
        ]
        print(json.dumps({"long_launch": arguments, "screen_winner": winner}), flush=True)
        completed = subprocess.run(base + arguments, check=False)
        result_path = output / "result.json"
        if completed.returncode == 0 and result_path.is_file():
            result = json.loads(result_path.read_text())
            if (
                result.get("status") == "complete"
                and result.get("long_input_tokens_seen") == 1_000_000_000_000
            ):
                summary = {
                    "schema": "exp7-dense-free-study-v2",
                    "status": "complete",
                    "winner_lr": winner_lr,
                    "long_lr": long_lr,
                    "screen_winner": winner,
                    "final": result,
                }
                (Path(CONFIG["study_root"]) / "study.json").write_text(
                    json.dumps(summary, indent=2)
                )
                print(json.dumps(summary, indent=2), flush=True)
                return
        failures += 1
        if failures >= 5:
            raise RuntimeError("1T run failed five consecutive times")
        print(
            json.dumps(
                {
                    "long_restart": failures,
                    "returncode": completed.returncode,
                    "resume": str(checkpoint),
                }
            ),
            flush=True,
        )
        time.sleep(min(30, 2**failures))


def stream_benchmark() -> None:
    rank = int(os.environ.get("LOCAL_RANK", "0"))
    world = int(os.environ.get("WORLD_SIZE", "1"))
    stream = FineWebEduStream(
        rank,
        world,
        int(CONFIG["physical_local_batch"]),
    )
    for batch in range(int(CONFIG["stream_benchmark_batches"])):
        contexts, _, waited, produced, depth = stream.next()
        result = {
            "rank": rank,
            "batch": batch,
            "contexts": len(contexts),
            "input_tokens": len(contexts) * CONFIG["context_length"],
            "wait_seconds": waited,
            "producer_seconds": produced,
            "input_tokens_per_second": (
                len(contexts) * CONFIG["context_length"] / max(produced, 1e-9)
            ),
            "queue_depth": depth,
        }
        print(json.dumps(result), flush=True)
    os._exit(0)


if __name__ == "__main__":
    overrides()
    if CONFIG["mode"] == "self_test":
        self_test()
    elif CONFIG["mode"] == "study":
        study()
    elif CONFIG["mode"] == "long_supervisor":
        long_supervisor()
    elif CONFIG["mode"] == "stream_benchmark":
        stream_benchmark()
    else:
        train()
