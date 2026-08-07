from __future__ import annotations

import random
import queue
import threading
import gzip
import io
import json
from pathlib import Path
from collections.abc import Iterator
from dataclasses import dataclass

import torch

from .data import DataSource


@dataclass
class PackedTrainingExample:
    input_ids: torch.Tensor
    eligible_mask: torch.Tensor
    source_id: str


def _rank_urls(urls: list[str], rank: int, world_size: int) -> list[str]:
    if not urls:
        raise ValueError("resolved source has no URLs")
    return urls[rank::world_size] if len(urls) >= world_size else [urls[rank % len(urls)]]


def _shared_file_partition(urls: list[str], rank: int, world_size: int) -> tuple[int, int]:
    """Return row offset/stride when more ranks than immutable source files share a file."""
    if len(urls) >= world_size:
        return 0, 1
    file_index = rank % len(urls)
    ranks_for_file = (world_size - 1 - file_index) // len(urls) + 1
    return rank // len(urls), ranks_for_file


def _raw_json_rows(urls: list[str]) -> Iterator[dict]:
    """Stream JSONL directly, bypassing datasets' costly Hub tree discovery."""
    import requests
    import zstandard

    while True:
        yielded = False
        for url in urls:
            local = Path(url)
            response = None
            if local.is_file():
                raw = local.open("rb")
            else:
                response = requests.get(url, stream=True, timeout=(30, 300))
                response.raise_for_status()
                response.raw.decode_content = False
                raw = response.raw
            try:
                if url.endswith(".zst"):
                    binary = zstandard.ZstdDecompressor().stream_reader(raw)
                elif url.endswith(".gz"):
                    binary = gzip.GzipFile(fileobj=raw)
                else:
                    binary = raw
                with io.TextIOWrapper(binary, encoding="utf-8", errors="replace") as text:
                    for line in text:
                        if line.strip():
                            yielded = True
                            yield json.loads(line)
            finally:
                if response is not None:
                    response.close()
        if not yielded:
            raise RuntimeError("resolved JSON shards produced no rows")


def _raw_parquet_rows(urls: list[str]) -> Iterator[dict]:
    """Range-stream Parquet directly without any Hugging Face API calls."""
    import fsspec
    import pyarrow.parquet as parquet

    while True:
        yielded = False
        for url in urls:
            with fsspec.open(url, "rb", block_size=8 << 20).open() as file:
                reader = parquet.ParquetFile(file)
                for batch in reader.iter_batches(batch_size=128):
                    for row in batch.to_pylist():
                        yielded = True
                        yield row
        if not yielded:
            raise RuntimeError("resolved Parquet shards produced no rows")


def _row_allowed(row: dict, source: DataSource) -> bool:
    if not source.allowed_row_sources:
        return True
    value = str(row.get("source", row.get("source_dataset", "")))
    lowered = value.lower()
    return any(candidate.lower() in lowered for candidate in source.allowed_row_sources)


def _conversation_tokens(row: dict, tokenizer) -> tuple[list[int], list[bool]] | None:
    messages = row.get("messages")
    if not isinstance(messages, list):
        return None
    assistant = next(
        (index for index in range(len(messages) - 1, -1, -1) if messages[index].get("role") == "assistant"),
        None,
    )
    if assistant is None:
        return None
    selected = messages[: assistant + 1]
    prefix = messages[:assistant]
    prefix_ids = tokenizer.apply_chat_template(
        prefix, tokenize=True, add_generation_prompt=True
    )
    all_ids = tokenizer.apply_chat_template(
        selected, tokenize=True, add_generation_prompt=False
    )
    if hasattr(prefix_ids, "keys"):
        prefix_ids = prefix_ids["input_ids"]
    if hasattr(all_ids, "keys"):
        all_ids = all_ids["input_ids"]
    if torch.is_tensor(prefix_ids):
        prefix_ids = prefix_ids.tolist()
    if torch.is_tensor(all_ids):
        all_ids = all_ids.tolist()
    start = min(len(prefix_ids), len(all_ids))
    eligibility = [False] * start + [True] * (len(all_ids) - start)
    return list(all_ids), eligibility


def source_examples(
    source: DataSource,
    tokenizer,
    *,
    context_length: int,
    rank: int,
    world_size: int,
    resolved: dict | None = None,
) -> Iterator[PackedTrainingExample]:
    from datasets import load_dataset

    if resolved is None:
        dataset = load_dataset(
            source.dataset,
            source.subset,
            split="train",
            revision=source.revision,
            streaming=True,
        )
        dataset = dataset.shard(num_shards=world_size, index=rank)
    else:
        urls = resolved["urls"]
        # Partition before builder construction so each worker opens only its
        # own files. Tiny instruction sets may have fewer files than ranks; in
        # that case deterministic reuse is preferable to an empty rank.
        # Conversation shards are often grouped by originating dataset. Pool
        # them before the license/subset filter and shard the *allowed rows*;
        # assigning one physical file per rank can otherwise give a rank a
        # shard containing zero permitted conversations.
        partition_allowed_rows = source.kind == "conversation"
        rank_urls = urls if partition_allowed_rows else _rank_urls(urls, rank, world_size)
        dataset = (
            _raw_json_rows(rank_urls)
            if resolved["format"] == "json"
            else _raw_parquet_rows(rank_urls)
        )
        row_offset, row_stride = _shared_file_partition(urls, rank, world_size)
        if not partition_allowed_rows and row_stride > 1:
            dataset = (
                row
                for index, row in enumerate(dataset)
                if index % row_stride == row_offset
            )
    if resolved is None:
        partition_allowed_rows = False
    eos = tokenizer.eos_token_id
    if eos is None:
        raise ValueError("teacher tokenizer must define eos_token_id")
    token_buffer: list[int] = []
    eligibility_buffer: list[bool] = []
    while True:
        yielded = False
        allowed_row_index = 0
        for row in dataset:
            if not _row_allowed(row, source):
                continue
            if partition_allowed_rows:
                selected_rank = allowed_row_index % world_size
                allowed_row_index += 1
                if selected_rank != rank:
                    continue
            if source.kind == "conversation":
                converted = _conversation_tokens(row, tokenizer)
                if converted is None:
                    continue
                tokens, eligibility = converted
            else:
                text = row.get(source.text_field)
                if not isinstance(text, str) or not text.strip():
                    continue
                tokens = tokenizer.encode(text, add_special_tokens=False)
                eligibility = [True] * len(tokens)
            tokens.append(eos)
            eligibility.append(source.kind != "conversation")
            token_buffer.extend(tokens)
            eligibility_buffer.extend(eligibility)
            while len(token_buffer) >= context_length:
                yielded = True
                yield PackedTrainingExample(
                    torch.tensor(token_buffer[:context_length], dtype=torch.long),
                    torch.tensor(eligibility_buffer[:context_length], dtype=torch.bool),
                    source.source_id,
                )
                del token_buffer[:context_length]
                del eligibility_buffer[:context_length]
        if not yielded and len(token_buffer) < context_length:
            raise RuntimeError(f"source {source.source_id} produced no complete contexts")


class WeightedMixture:
    def __init__(
        self,
        sources: list[DataSource],
        tokenizer,
        *,
        stage: int,
        context_length: int,
        rank: int,
        world_size: int,
        seed: int,
        resolved_shards: dict[str, dict] | None = None,
    ) -> None:
        if stage not in (0, 1):
            raise ValueError("stage must be zero or one")
        self.sources = sources
        self.weights = [
            source.stage0_weight if stage == 0 else source.stage1_weight for source in sources
        ]
        self.random = random.Random(seed + rank * 1_000_003)
        self.iterators = {
            source.source_id: iter(
                source_examples(
                    source,
                    tokenizer,
                    context_length=context_length,
                    rank=rank,
                    world_size=world_size,
                    resolved=None if resolved_shards is None else resolved_shards[source.source_id],
                )
            )
            for source in sources
        }

    def __iter__(self) -> "WeightedMixture":
        return self

    def __next__(self) -> PackedTrainingExample:
        source = self.random.choices(self.sources, weights=self.weights, k=1)[0]
        return next(self.iterators[source.source_id])


class PrefetchMixture:
    """Bounded background prefetch so tokenization cannot starve paid GPUs."""

    def __init__(self, source: Iterator[PackedTrainingExample], *, capacity: int = 128) -> None:
        self.source = source
        self.queue: queue.Queue[PackedTrainingExample | BaseException] = queue.Queue(capacity)
        self.thread = threading.Thread(target=self._produce, name="v2-sbd-data-prefetch", daemon=True)
        self.thread.start()

    def _produce(self) -> None:
        try:
            while True:
                self.queue.put(next(self.source))
        except BaseException as error:
            self.queue.put(error)

    def __iter__(self) -> "PrefetchMixture":
        return self

    def __next__(self) -> PackedTrainingExample:
        value = self.queue.get()
        if isinstance(value, BaseException):
            raise value
        return value


def collate_examples(examples: list[PackedTrainingExample], device: torch.device) -> tuple[torch.Tensor, torch.Tensor]:
    return (
        torch.stack([example.input_ids for example in examples]).to(device, non_blocking=True),
        torch.stack([example.eligible_mask for example in examples]).to(device, non_blocking=True),
    )
