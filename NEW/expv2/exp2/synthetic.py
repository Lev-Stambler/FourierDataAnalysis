"""Corrected synthetic tasks with one isolated distribution shift per split."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Literal

import torch

from .config import (
    ASSOCIATIVE_HELDOUT_QUERY_SLOTS,
    ASSOCIATIVE_QUERY_SLOTS,
    ASSOCIATIVE_SUPPORT_SLOTS,
    BIT_ONE,
    BIT_ZERO,
    CONTEXT_LENGTH,
    DELAY_BIT_POSITIONS,
    DELAY_HELDOUT_QUERY_SLOTS,
    DELAY_QUERY_SLOTS,
    DELAY_TARGETS_PER_CONTEXT,
    HELDOUT_COMPOSITION_DELAYS,
    OOD_CARDINALITY,
    SANITY_CARDINALITY,
    TRAIN_CARDINALITIES,
    TRAIN_DELAYS,
    TWO_HOP_FIRST_SLOTS,
    TWO_HOP_HELDOUT_QUERY_SLOTS,
    TWO_HOP_QUERY_SLOTS,
    TWO_HOP_SECOND_SLOTS,
    VALID_SPLITS,
    VOCAB_SIZE,
)


Task = Literal["delay-copy", "associative-recall", "two-hop-recall"]
Split = Literal[
    "sanity",
    "train",
    "id",
    "ood-cardinality",
    "ood-composition",
    "ood-position",
]


@dataclass(frozen=True)
class SyntheticBatch:
    inputs: torch.Tensor
    targets: torch.Tensor
    mask: torch.Tensor
    query_positions: torch.Tensor
    support_positions: torch.Tensor
    candidate_values: torch.Tensor | None = None
    delays: torch.Tensor | None = None
    cardinality: int | None = None

    def validate(self) -> None:
        if self.inputs.shape != self.targets.shape or self.inputs.shape != self.mask.shape:
            raise ValueError("input, target, and mask shapes must match")
        if self.inputs.ndim != 2 or self.inputs.shape[1] != CONTEXT_LENGTH:
            raise ValueError(f"batches must be [batch,{CONTEXT_LENGTH}]")
        if self.query_positions.ndim != 2:
            raise ValueError("query positions must be rank two")
        rows = torch.arange(self.inputs.shape[0], device=self.inputs.device)[:, None]
        expected = torch.zeros_like(self.mask)
        expected[rows, self.query_positions] = True
        if not torch.equal(expected, self.mask):
            raise ValueError("mask must select exactly the recorded query positions")
        if int(self.inputs.min()) < 0 or int(self.inputs.max()) >= VOCAB_SIZE:
            raise ValueError("token outside synthetic vocabulary")


def _generator(seed: int, device: torch.device | str) -> torch.Generator:
    value = torch.Generator(device=device)
    value.manual_seed(seed)
    return value


def _sample_unique(
    pool: tuple[int, ...] | range,
    count: int,
    batch_size: int,
    *,
    generator: torch.Generator,
    device: torch.device | str,
) -> torch.Tensor:
    if count > len(pool):
        raise ValueError("cannot sample more unique values than the pool contains")
    scores = torch.rand(batch_size, len(pool), generator=generator, device=device)
    indices = scores.argsort(1)[:, :count]
    values = torch.tensor(tuple(pool), device=device)
    return values[indices]


def _cardinality(split: str, generator: torch.Generator, device: torch.device | str) -> int:
    if split == "sanity":
        return SANITY_CARDINALITY
    if split == "ood-cardinality":
        return OOD_CARDINALITY
    index = int(
        torch.randint(
            0, len(TRAIN_CARDINALITIES), (), generator=generator, device=device
        ).item()
    )
    return TRAIN_CARDINALITIES[index]


def _query_positions(
    slots: tuple[int, ...],
    batch_size: int,
    count: int,
    *,
    generator: torch.Generator,
    device: torch.device | str,
) -> torch.Tensor:
    return _sample_unique(
        slots, count, batch_size, generator=generator, device=device
    )


def associative_recall_batch(
    batch_size: int,
    *,
    seed: int,
    split: str,
    device: torch.device | str = "cpu",
) -> SyntheticBatch:
    if split not in VALID_SPLITS["associative-recall"]:
        raise ValueError(f"invalid associative-recall split: {split}")
    generator = _generator(seed, device)
    cardinality = _cardinality(split, generator, device)
    inputs = torch.randint(
        96, VOCAB_SIZE, (batch_size, CONTEXT_LENGTH),
        generator=generator, device=device,
    )
    targets = torch.zeros_like(inputs)
    mask = torch.zeros_like(inputs, dtype=torch.bool)
    keys = _sample_unique(range(4, 32), cardinality, batch_size, generator=generator, device=device)
    values = _sample_unique(range(40, 72), cardinality, batch_size, generator=generator, device=device)
    support = _sample_unique(
        ASSOCIATIVE_SUPPORT_SLOTS,
        cardinality,
        batch_size,
        generator=generator,
        device=device,
    )
    rows = torch.arange(batch_size, device=device)[:, None]
    inputs[rows, support] = keys
    inputs[rows, support + 1] = values
    choice = torch.randint(
        0, cardinality, (batch_size, 1), generator=generator, device=device
    )
    query_slots = (
        ASSOCIATIVE_HELDOUT_QUERY_SLOTS
        if split == "ood-position"
        else ASSOCIATIVE_QUERY_SLOTS
    )
    query = _query_positions(
        query_slots, batch_size, 1, generator=generator, device=device
    )
    query_keys = keys.gather(1, choice)
    answers = values.gather(1, choice)
    inputs[rows, query] = query_keys
    targets[rows, query] = answers
    mask[rows, query] = True
    result = SyntheticBatch(
        inputs, targets, mask, query, support, values, cardinality=cardinality
    )
    result.validate()
    return result


def two_hop_recall_batch(
    batch_size: int,
    *,
    seed: int,
    split: str,
    device: torch.device | str = "cpu",
) -> SyntheticBatch:
    if split not in VALID_SPLITS["two-hop-recall"]:
        raise ValueError(f"invalid two-hop-recall split: {split}")
    generator = _generator(seed, device)
    cardinality = _cardinality(split, generator, device)
    inputs = torch.randint(
        96, VOCAB_SIZE, (batch_size, CONTEXT_LENGTH),
        generator=generator, device=device,
    )
    targets = torch.zeros_like(inputs)
    mask = torch.zeros_like(inputs, dtype=torch.bool)
    first_tokens = _sample_unique(range(4, 28), cardinality, batch_size, generator=generator, device=device)
    middle_tokens = _sample_unique(range(32, 56), cardinality, batch_size, generator=generator, device=device)
    final_tokens = _sample_unique(range(64, 88), cardinality, batch_size, generator=generator, device=device)
    first_slots = _sample_unique(
        TWO_HOP_FIRST_SLOTS, cardinality, batch_size,
        generator=generator, device=device,
    )
    second_slots = _sample_unique(
        TWO_HOP_SECOND_SLOTS, cardinality, batch_size,
        generator=generator, device=device,
    )
    rows = torch.arange(batch_size, device=device)[:, None]
    inputs[rows, first_slots] = first_tokens
    inputs[rows, first_slots + 1] = middle_tokens
    inputs[rows, second_slots] = middle_tokens
    inputs[rows, second_slots + 1] = final_tokens
    choice = torch.randint(
        0, cardinality, (batch_size, 1), generator=generator, device=device
    )
    query_slots = (
        TWO_HOP_HELDOUT_QUERY_SLOTS
        if split == "ood-position"
        else TWO_HOP_QUERY_SLOTS
    )
    query = _query_positions(
        query_slots, batch_size, 1, generator=generator, device=device
    )
    inputs[rows, query] = first_tokens.gather(1, choice)
    targets[rows, query] = final_tokens.gather(1, choice)
    mask[rows, query] = True
    support = torch.stack((first_slots, second_slots), dim=1)
    result = SyntheticBatch(
        inputs, targets, mask, query, support, final_tokens,
        cardinality=cardinality,
    )
    result.validate()
    return result


def _delay_values(
    split: str,
    batch_size: int,
    *,
    generator: torch.Generator,
    device: torch.device | str,
) -> torch.Tensor:
    if split == "sanity":
        return torch.full((batch_size,), 8, dtype=torch.long, device=device)
    pool = (
        HELDOUT_COMPOSITION_DELAYS
        if split == "ood-composition"
        else TRAIN_DELAYS
    )
    indices = torch.randint(
        0, len(pool), (batch_size,), generator=generator, device=device
    )
    return torch.tensor(pool, device=device)[indices]


def delayed_copy_batch(
    batch_size: int,
    *,
    seed: int,
    split: str,
    device: torch.device | str = "cpu",
) -> SyntheticBatch:
    if split not in VALID_SPLITS["delay-copy"]:
        raise ValueError(f"invalid delay-copy split: {split}")
    generator = _generator(seed, device)
    inputs = torch.randint(
        40, VOCAB_SIZE, (batch_size, CONTEXT_LENGTH),
        generator=generator, device=device,
    )
    targets = torch.zeros_like(inputs)
    mask = torch.zeros_like(inputs, dtype=torch.bool)
    delays = _delay_values(
        split, batch_size, generator=generator, device=device
    )
    bit_positions = torch.tensor(DELAY_BIT_POSITIONS, device=device)
    bits = (delays[:, None] >> bit_positions[None]) & 1
    inputs[:, bit_positions] = torch.where(bits.bool(), BIT_ONE, BIT_ZERO)
    query_slots = (
        DELAY_HELDOUT_QUERY_SLOTS
        if split == "ood-position"
        else DELAY_QUERY_SLOTS
    )
    query = _query_positions(
        query_slots,
        batch_size,
        DELAY_TARGETS_PER_CONTEXT,
        generator=generator,
        device=device,
    )
    sources = query - delays[:, None]
    if int(sources.min()) <= max(DELAY_BIT_POSITIONS):
        raise RuntimeError("delay source overlaps the instruction")
    rows = torch.arange(batch_size, device=device)[:, None]
    targets[rows, query] = inputs[rows, sources]
    mask[rows, query] = True
    result = SyntheticBatch(
        inputs, targets, mask, query, sources, delays=delays
    )
    result.validate()
    return result


def make_batch(
    task: Task,
    batch_size: int,
    *,
    seed: int,
    split: Split | str,
    device: torch.device | str = "cpu",
) -> SyntheticBatch:
    functions = {
        "delay-copy": delayed_copy_batch,
        "associative-recall": associative_recall_batch,
        "two-hop-recall": two_hop_recall_batch,
    }
    if task not in functions:
        raise ValueError(f"unknown task: {task}")
    return functions[task](
        batch_size, seed=seed, split=str(split), device=device
    )


def masked_loss_and_accuracy(
    logits: torch.Tensor, targets: torch.Tensor, mask: torch.Tensor
) -> tuple[torch.Tensor, torch.Tensor]:
    selected_logits = logits[mask]
    selected_targets = targets[mask]
    loss = torch.nn.functional.cross_entropy(
        selected_logits.float(), selected_targets
    )
    accuracy = (selected_logits.argmax(-1) == selected_targets).float().mean()
    return loss, accuracy
