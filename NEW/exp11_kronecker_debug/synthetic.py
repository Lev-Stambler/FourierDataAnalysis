"""Deterministic synthetic tasks that isolate causal mixing capabilities."""

from __future__ import annotations

from dataclasses import dataclass

import torch
import torch.nn.functional as F
from torch import nn


@dataclass(frozen=True)
class ProbeBatch:
    inputs: torch.Tensor
    targets: torch.Tensor
    mask: torch.Tensor


def delayed_copy_batch(
    batch_size: int,
    context_length: int,
    vocab_size: int,
    delay: int,
    *,
    seed: int,
    device: torch.device | str = "cpu",
) -> ProbeBatch:
    if not 0 < delay < context_length:
        raise ValueError("delay must be inside the context")
    generator = torch.Generator(device="cpu").manual_seed(seed)
    inputs = torch.randint(
        0, vocab_size, (batch_size, context_length), generator=generator
    ).to(device)
    targets = torch.zeros_like(inputs)
    targets[:, delay:] = inputs[:, :-delay]
    mask = torch.zeros_like(inputs, dtype=torch.bool)
    mask[:, delay:] = True
    return ProbeBatch(inputs, targets, mask)


def associative_recall_batch(
    batch_size: int,
    pairs: int,
    key_count: int,
    value_count: int,
    *,
    seed: int,
    device: torch.device | str = "cpu",
) -> ProbeBatch:
    """Emit key/value pairs followed by a query key; predict its paired value."""
    if pairs > key_count:
        raise ValueError("pairs cannot exceed key count")
    generator = torch.Generator(device="cpu").manual_seed(seed)
    inputs, targets = [], []
    for _ in range(batch_size):
        keys = torch.randperm(key_count, generator=generator)[:pairs]
        values = torch.randint(0, value_count, (pairs,), generator=generator)
        query_index = int(torch.randint(0, pairs, (), generator=generator))
        sequence = torch.stack((keys, values + key_count), dim=1).flatten()
        sequence = torch.cat((sequence, keys[query_index : query_index + 1]))
        inputs.append(sequence)
        target = torch.zeros_like(sequence)
        target[-1] = values[query_index] + key_count
        targets.append(target)
    input_tensor = torch.stack(inputs).to(device)
    target_tensor = torch.stack(targets).to(device)
    mask = torch.zeros_like(input_tensor, dtype=torch.bool)
    mask[:, -1] = True
    return ProbeBatch(input_tensor, target_tensor, mask)


def probe_loss(model: nn.Module, batch: ProbeBatch) -> torch.Tensor:
    logits = model(batch.inputs)
    return F.cross_entropy(logits[batch.mask].float(), batch.targets[batch.mask])


@torch.no_grad()
def probe_accuracy(model: nn.Module, batch: ProbeBatch) -> float:
    predictions = model(batch.inputs).argmax(-1)
    return float((predictions[batch.mask] == batch.targets[batch.mask]).float().mean())


def fit_probe(
    model: nn.Module,
    train_batch: ProbeBatch,
    validation_batch: ProbeBatch,
    *,
    steps: int,
    lr: float,
) -> dict[str, float]:
    optimizer = torch.optim.AdamW(model.parameters(), lr=lr, weight_decay=0.0)
    model.train()
    loss = torch.tensor(float("nan"))
    for _ in range(steps):
        optimizer.zero_grad(set_to_none=True)
        loss = probe_loss(model, train_batch)
        loss.backward()
        optimizer.step()
    model.eval()
    return {
        "train_loss": float(loss.detach()),
        "train_accuracy": probe_accuracy(model, train_batch),
        "validation_accuracy": probe_accuracy(model, validation_batch),
    }
