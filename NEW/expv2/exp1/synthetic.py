"""Deterministic causal operator-capability tasks."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Literal

import torch

from .config import CONTEXT_LENGTH, SYNTHETIC_VOCAB_SIZE


TASKS = ("delay-copy", "associative-recall", "two-hop-recall")
Task = Literal["delay-copy", "associative-recall", "two-hop-recall"]


@dataclass(frozen=True)
class SyntheticBatch:
    inputs: torch.Tensor
    targets: torch.Tensor
    mask: torch.Tensor

    def validate(self) -> None:
        if self.inputs.shape != self.targets.shape or self.inputs.shape != self.mask.shape:
            raise ValueError("synthetic input, target, and mask shapes must match")
        if self.inputs.ndim != 2 or self.inputs.shape[1] != CONTEXT_LENGTH:
            raise ValueError("synthetic batches require context length 128")
        if not bool(self.mask.any()):
            raise ValueError("synthetic target mask is empty")


def _generator(seed: int, device: torch.device | str) -> torch.Generator:
    generator = torch.Generator(device=device)
    generator.manual_seed(seed)
    return generator


def delayed_copy_batch(
    batch_size: int,
    *,
    seed: int,
    split: str,
    device: torch.device | str = "cpu",
) -> SyntheticBatch:
    if split not in ("train", "id", "ood"):
        raise ValueError("split must be train, id, or ood")
    generator = _generator(seed, device)
    low, high = ((1, 17) if split in ("train", "id") else (17, 33))
    delays = torch.randint(low, high, (batch_size,), generator=generator, device=device)
    inputs = torch.randint(
        40,
        SYNTHETIC_VOCAB_SIZE,
        (batch_size, CONTEXT_LENGTH),
        generator=generator,
        device=device,
    )
    inputs[:, 0] = delays
    targets = torch.zeros_like(inputs)
    mask = torch.zeros_like(inputs, dtype=torch.bool)
    destinations = torch.arange(33, CONTEXT_LENGTH, device=device)
    sources = destinations[None] - delays[:, None]
    targets[:, destinations] = inputs.gather(1, sources)
    mask[:, destinations] = True
    result = SyntheticBatch(inputs, targets, mask)
    result.validate()
    return result


def associative_recall_batch(
    batch_size: int,
    *,
    seed: int,
    split: str,
    device: torch.device | str = "cpu",
) -> SyntheticBatch:
    if split not in ("train", "id", "ood"):
        raise ValueError("split must be train, id, or ood")
    pairs = 4 if split in ("train", "id") else 8
    generator = _generator(seed, device)
    inputs = torch.randint(
        96,
        SYNTHETIC_VOCAB_SIZE,
        (batch_size, CONTEXT_LENGTH),
        generator=generator,
        device=device,
    )
    targets = torch.zeros_like(inputs)
    mask = torch.zeros_like(inputs, dtype=torch.bool)
    key_scores = torch.rand(batch_size, 30, generator=generator, device=device)
    keys = key_scores.argsort(1)[:, :pairs] + 1
    value_scores = torch.rand(batch_size, 30, generator=generator, device=device)
    values = value_scores.argsort(1)[:, :pairs] + 64
    inputs[:, 1 : 1 + 2 * pairs : 2] = keys
    inputs[:, 2 : 2 + 2 * pairs : 2] = values
    query_order = torch.rand(batch_size, pairs, generator=generator, device=device).argsort(1)
    queries = keys.gather(1, query_order)
    answers = values.gather(1, query_order)
    query_positions = torch.arange(64, 64 + pairs, device=device)
    inputs[:, query_positions] = queries
    targets[:, query_positions] = answers
    mask[:, query_positions] = True
    result = SyntheticBatch(inputs, targets, mask)
    result.validate()
    return result


def two_hop_recall_batch(
    batch_size: int,
    *,
    seed: int,
    split: str,
    device: torch.device | str = "cpu",
) -> SyntheticBatch:
    if split not in ("train", "id", "ood"):
        raise ValueError("split must be train, id, or ood")
    chains = 4 if split in ("train", "id") else 8
    generator = _generator(seed, device)
    inputs = torch.randint(
        100,
        SYNTHETIC_VOCAB_SIZE,
        (batch_size, CONTEXT_LENGTH),
        generator=generator,
        device=device,
    )
    targets = torch.zeros_like(inputs)
    mask = torch.zeros_like(inputs, dtype=torch.bool)
    a = torch.rand(batch_size, 20, generator=generator, device=device).argsort(1)[:, :chains] + 1
    b = torch.rand(batch_size, 20, generator=generator, device=device).argsort(1)[:, :chains] + 33
    c = torch.rand(batch_size, 20, generator=generator, device=device).argsort(1)[:, :chains] + 65
    first = torch.arange(1, 1 + 2 * chains, 2, device=device)
    second = torch.arange(33, 33 + 2 * chains, 2, device=device)
    inputs[:, first] = a
    inputs[:, first + 1] = b
    inputs[:, second] = b
    inputs[:, second + 1] = c
    query_order = torch.rand(batch_size, chains, generator=generator, device=device).argsort(1)
    queries = a.gather(1, query_order)
    answers = c.gather(1, query_order)
    query_positions = torch.arange(80, 80 + chains, device=device)
    inputs[:, query_positions] = queries
    targets[:, query_positions] = answers
    mask[:, query_positions] = True
    result = SyntheticBatch(inputs, targets, mask)
    result.validate()
    return result


def make_batch(
    task: Task,
    batch_size: int,
    *,
    seed: int,
    split: str,
    device: torch.device | str = "cpu",
) -> SyntheticBatch:
    functions = {
        "delay-copy": delayed_copy_batch,
        "associative-recall": associative_recall_batch,
        "two-hop-recall": two_hop_recall_batch,
    }
    if task not in functions:
        raise ValueError(f"unknown synthetic task: {task}")
    return functions[task](batch_size, seed=seed, split=split, device=device)


def masked_loss_and_accuracy(
    logits: torch.Tensor, targets: torch.Tensor, mask: torch.Tensor
) -> tuple[torch.Tensor, torch.Tensor]:
    selected_logits = logits[mask]
    selected_targets = targets[mask]
    loss = torch.nn.functional.cross_entropy(selected_logits.float(), selected_targets)
    accuracy = (selected_logits.argmax(-1) == selected_targets).float().mean()
    return loss, accuracy
