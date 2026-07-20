"""Learned Walsh--Fourier distillation for contextual noun classification.

This module deliberately has no Dataset-GL dependency.  A text example is
tokenized into a fixed layout, every token is mapped to a sign-LSH code, and
the flattened code is a point in F_2^n.  The student is

    logit(x) = bias + sum_j coefficient[j] * (-1)^<mask[j], x>.

Masks are exact, hard Walsh characters in the forward pass.  The saturated
mask logits are trained with the same log-magnitude straight-through
surrogate that is used by the educational Fourier experiments in this repo.
"""

from __future__ import annotations

import dataclasses
import hashlib
import json
import math
import os
import re
import time
from collections.abc import Callable, Iterable, Mapping, Sequence
from pathlib import Path
from typing import Any

import numpy as np
import torch
from torch import nn


MODEL_ID = "Qwen/Qwen3.5-0.8B"
MODEL_REVISION = "2fc06364715b967f1860aea9cf38778875588b17"
DATASET_ID = "commul/universal_dependencies"
DATASET_CONFIG = "en_ewt"
DATASET_REVISION = "aa5843797153240f714aa375a593310f45c06ea7"
WEB_DATASET_ID = "HuggingFaceFW/fineweb-edu"
WEB_DATASET_CONFIG = "sample-10BT"
WEB_DATASET_REVISION = "87f09149ef4734204d70ed1d046ddc9ca3f2b8f9"
SCHEMA_VERSION = 7
POSITIVE_UPOS = frozenset({"NOUN", "PROPN"})
SKIP_UPOS = frozenset({"PUNCT", "SYM", "X"})
_TOKEN_PATTERN = re.compile(r"\w+(?:['’\-]\w+)*|[^\w\s]", flags=re.UNICODE)


def format_noun_payload(tokens: Sequence[str], target_index: int,
                        context_words: int = 24) -> str:
    """Make a short target-first payload whose marked occurrence is retained.

    UD token lists are used instead of reconstructing the source text so a
    repeated word has an unambiguous target occurrence.  The centered context
    also keeps the teacher prompt comfortably below its fixed 192-token shape.
    """
    if not 0 <= target_index < len(tokens):
        raise IndexError("target_index lies outside the token sequence")
    if context_words < 3:
        raise ValueError("context_words must be at least 3")
    target = str(tokens[target_index])
    side = (context_words - 1) // 2
    lo = max(0, target_index - side)
    hi = min(len(tokens), lo + context_words)
    lo = max(0, hi - context_words)
    marked = [str(x) for x in tokens[lo:target_index]]
    marked.extend(("[TARGET]", target, "[/TARGET]"))
    marked.extend(str(x) for x in tokens[target_index + 1:hi])
    return f"Target: {target}\nContext: {' '.join(marked)}"


def format_student_fields(tokens: Sequence[str], target_index: int,
                          field_count: int = 16) -> list[str]:
    """Put the target and its neighbors in stable, independently-tokenized fields.

    The target is always field zero, followed by left/right neighbors in
    increasing distance.  The final field is a coarse sentence-position cue.
    Prefixing lexical fields with a space gives Qwen's tokenizer the same word
    boundary it normally sees in running text.
    """
    if not 0 <= target_index < len(tokens):
        raise IndexError("target_index lies outside the token sequence")
    if field_count < 4:
        raise ValueError("field_count must be at least 4")
    target = str(tokens[target_index])
    lower = target.lower()
    if target.isupper() and any(character.isalpha() for character in target):
        shape = " shape-all-caps"
    elif target[:1].isupper():
        shape = " shape-capitalized"
    elif any(character.isdigit() for character in target):
        shape = " shape-has-digit"
    else:
        shape = " shape-lower"
    fields = [
        " " + target,
        " " + lower,
        " prefix-" + lower[:4],
        " suffix-" + lower[-4:],
    ]
    distance = 1
    while len(fields) < field_count - 2:
        left = target_index - distance
        right = target_index + distance
        fields.append(" " + str(tokens[left]) if left >= 0 else "")
        if len(fields) < field_count - 2:
            fields.append(" " + str(tokens[right]) if right < len(tokens) else "")
        distance += 1
    if target_index == 0:
        position = " sentence-start"
    elif target_index == len(tokens) - 1:
        position = " sentence-end"
    else:
        position = " sentence-middle"
    fields.extend((position, shape))
    return fields


def teacher_instruction(payload: str) -> str:
    """Prompt used to obtain the two restricted teacher logits."""
    return (
        "You are a part-of-speech tagger. Classify only the occurrence between "
        "[TARGET] markers. Classify it as noun if its Universal Dependencies "
        "tag is NOUN or PROPN, and as other for every other tag, including "
        "verbs, adjectives, pronouns, determiners, adverbs, and numbers.\n"
        "Examples:\n"
        "Context: The [TARGET] cat [/TARGET] sleeps. Classification: noun\n"
        "Context: They [TARGET] sleep [/TARGET] here. Classification: other\n"
        "Context: [TARGET] Alice [/TARGET] smiled. Classification: noun\n"
        "Context: a [TARGET] blue [/TARGET] car. Classification: other"
        "\n\n" + payload + "\nClassification:"
    )


def sample_ud_examples(rows: Iterable[Mapping[str, Any]], upos_names: Sequence[str] | None,
                       count: int, seed: int, balanced: bool = True,
                       context_words: int = 24) -> list[dict[str, Any]]:
    """Select deterministic lexical token targets from a UD split.

    ``upos_names`` is the ClassLabel name table when rows store integer UPOS
    ids; it may be omitted when the rows already contain strings.
    """
    positive: list[dict[str, Any]] = []
    negative: list[dict[str, Any]] = []
    for row_number, row in enumerate(rows):
        tokens = row["tokens"]
        tags = row["upos"]
        sent_id = str(row.get("sent_id") or f"row-{row_number}")
        for token_index, raw_tag in enumerate(tags):
            tag = upos_names[int(raw_tag)] if not isinstance(raw_tag, str) else raw_tag
            if tag in SKIP_UPOS:
                continue
            item = {
                "sent_id": sent_id,
                "token_index": int(token_index),
                "payload": format_noun_payload(tokens, token_index, context_words),
                "student_fields": format_student_fields(tokens, token_index),
                "gold": tag in POSITIVE_UPOS,
                "upos": tag,
            }
            (positive if item["gold"] else negative).append(item)
    rng = np.random.default_rng(seed)
    if balanced:
        per_class = count // 2
        if len(positive) < per_class or len(negative) < count - per_class:
            raise ValueError(
                f"not enough balanced examples: positive={len(positive)} "
                f"negative={len(negative)} requested={count}"
            )
        pi = rng.choice(len(positive), per_class, replace=False)
        ni = rng.choice(len(negative), count - per_class, replace=False)
        chosen = [positive[int(i)] for i in pi] + [negative[int(i)] for i in ni]
    else:
        pool = positive + negative
        if len(pool) < count:
            raise ValueError(f"only {len(pool)} lexical examples for requested {count}")
        chosen = [pool[int(i)] for i in rng.choice(len(pool), count, replace=False)]
    rng.shuffle(chosen)
    return chosen


def iter_text_examples(rows: Iterable[Mapping[str, Any]], count: int, seed: int,
                       max_targets_per_document: int = 256
                       ) -> Iterable[dict[str, Any]]:
    """Yield unique pseudo-label targets from a stream of English documents."""
    if count <= 0 or max_targets_per_document <= 0:
        raise ValueError("count and max_targets_per_document must be positive")
    rng = np.random.default_rng(seed)
    seen_documents: set[str] = set()
    yielded = 0
    for row_number, row in enumerate(rows):
        document_id = str(row.get("id") or f"row-{row_number}")
        if document_id in seen_documents:
            continue
        seen_documents.add(document_id)
        tokens = _TOKEN_PATTERN.findall(str(row.get("text") or ""))
        eligible = [index for index, token in enumerate(tokens)
                    if any(character.isalnum() for character in token)]
        if not eligible:
            continue
        take = min(max_targets_per_document, len(eligible), count - yielded)
        selected = rng.choice(eligible, size=take, replace=False)
        for token_index in selected:
            index = int(token_index)
            yield {
                "sent_id": f"fineweb:{document_id}",
                "token_index": index,
                "payload": format_noun_payload(tokens, index),
                "student_fields": format_student_fields(tokens, index),
                "gold": False,
                "gold_known": False,
                "upos": "TEACHER_ONLY",
            }
            yielded += 1
        if yielded >= count:
            return
    raise ValueError(f"document stream ended after {yielded}/{count} unique targets")


def tokenize_student_payloads(tokenizer: Any, payloads: Sequence[str],
                              length: int = 64) -> tuple[np.ndarray, np.ndarray]:
    """Tokenize the teacher-free student layout with right padding."""
    old_padding = getattr(tokenizer, "padding_side", "right")
    old_truncation = getattr(tokenizer, "truncation_side", "right")
    tokenizer.padding_side = "right"
    tokenizer.truncation_side = "right"
    try:
        enc = tokenizer(
            list(payloads), add_special_tokens=False, padding="max_length",
            truncation=True, max_length=length, return_attention_mask=True,
        )
    finally:
        tokenizer.padding_side = old_padding
        tokenizer.truncation_side = old_truncation
    return (np.asarray(enc["input_ids"], dtype=np.int32),
            np.asarray(enc["attention_mask"], dtype=np.uint8))


def tokenize_student_fields(tokenizer: Any, rows: Sequence[Sequence[str]],
                            length: int = 64, field_width: int = 4
                            ) -> tuple[np.ndarray, np.ndarray]:
    """Tokenize each contextual field independently so positions cannot drift."""
    if length <= 0 or field_width <= 0 or length % field_width:
        raise ValueError("length must be a positive multiple of field_width")
    field_count = length // field_width
    if any(len(row) != field_count for row in rows):
        raise ValueError(f"every row must contain exactly {field_count} fields")
    flat = [field for row in rows for field in row]
    old_padding = getattr(tokenizer, "padding_side", "right")
    old_truncation = getattr(tokenizer, "truncation_side", "right")
    tokenizer.padding_side = "right"
    tokenizer.truncation_side = "right"
    try:
        enc = tokenizer(
            flat, add_special_tokens=False, padding="max_length",
            truncation=True, max_length=field_width, return_attention_mask=True,
        )
    finally:
        tokenizer.padding_side = old_padding
        tokenizer.truncation_side = old_truncation
    shape = (len(rows), length)
    return (np.asarray(enc["input_ids"], dtype=np.int32).reshape(shape),
            np.asarray(enc["attention_mask"], dtype=np.uint8).reshape(shape))


def apply_teacher_chat_template(tokenizer: Any, payloads: Sequence[str],
                                length: int = 192) -> tuple[np.ndarray, np.ndarray]:
    """Render non-thinking Qwen prompts and left-pad for final-token logits."""
    rendered: list[str] = []
    for payload in payloads:
        messages = [{"role": "user", "content": teacher_instruction(payload)}]
        try:
            text = tokenizer.apply_chat_template(
                messages, tokenize=False, add_generation_prompt=True,
                enable_thinking=False,
            )
        except TypeError:  # older compatible tokenizer templates
            text = tokenizer.apply_chat_template(
                messages, tokenize=False, add_generation_prompt=True,
            )
        # An assistant-side prefix makes the two next-token candidates natural
        # continuations and removes large response-format priors from the soft
        # labels. Both candidates below include their leading word-boundary.
        rendered.append(text + "The answer is")
    encoded = tokenizer(rendered, add_special_tokens=False)["input_ids"]
    pad_id = tokenizer.pad_token_id
    if pad_id is None:
        raise ValueError("teacher tokenizer needs a pad token")
    ids = np.full((len(encoded), length), pad_id, dtype=np.int32)
    attention = np.zeros((len(encoded), length), dtype=np.uint8)
    # Preserve both the target-first instruction and the assistant generation
    # suffix.  A simple right truncation can silently remove the latter on an
    # unusually token-fragmented URL in EWT.
    suffix_tokens = min(24, length // 4)
    for row, sequence in enumerate(encoded):
        if len(sequence) > length:
            sequence = sequence[:length - suffix_tokens] + sequence[-suffix_tokens:]
        ids[row, -len(sequence):] = sequence
        attention[row, -len(sequence):] = 1
    if np.any(attention[:, -1] != 1):
        raise RuntimeError("left-padded teacher prompts must end in a live token")
    return ids, attention


def verbalizer_token_ids(tokenizer: Any) -> tuple[int, int]:
    """Return assistant-prefixed single-token ``other``/``noun`` ids.

    Bare digits and YES/NO both had large response priors on the 0.8B
    checkpoint. These word-boundary tokens complete ``The answer is ...`` and
    therefore expose the post-trained model's semantic POS preference.
    """
    other = tokenizer.encode(" other", add_special_tokens=False)
    noun = tokenizer.encode(" noun", add_special_tokens=False)
    if len(other) != 1 or len(noun) != 1 or other == noun:
        raise ValueError(
            "' other'/' noun' must be distinct single-token verbalizers: "
            f"{other}, {noun}"
        )
    return int(other[0]), int(noun[0])


def restricted_binary_probability(logits: torch.Tensor, negative_id: int,
                                  positive_id: int, temperature: float = 1.0
                                  ) -> torch.Tensor:
    """Teacher probability after renormalizing over the two verbalizers."""
    if temperature <= 0:
        raise ValueError("temperature must be positive")
    pair = logits[..., [negative_id, positive_id]].float() / temperature
    return pair.softmax(dim=-1)[..., 1]


def make_lsh_codes_unique(codes: np.ndarray, seed: int = 0
                          ) -> tuple[np.ndarray, int]:
    """Deterministically repair sign-LSH collisions with unused bit strings."""
    value = np.asarray(codes, dtype=np.uint8).copy()
    if value.ndim != 2 or value.shape[1] <= 0:
        raise ValueError("codes must be a non-empty-width [vocab,bits] array")
    n_bits = value.shape[1]
    if len(value) > (1 << n_bits):
        raise ValueError(f"{n_bits} bits cannot uniquely encode {len(value)} tokens")
    packed = pack_bits(value)
    seen: set[bytes] = set()
    repairs = 0
    final_mask = (1 << (n_bits % 8)) - 1 if n_bits % 8 else 0xFF
    for token_id in range(len(packed)):
        key = packed[token_id].tobytes()
        if key not in seen:
            seen.add(key)
            continue
        repairs += 1
        nonce = 0
        while True:
            payload = f"lsh-repair:{seed}:{token_id}:{nonce}".encode()
            candidate = bytearray(hashlib.shake_256(payload).digest(packed.shape[1]))
            candidate[-1] &= final_mask
            key = bytes(candidate)
            if key not in seen:
                packed[token_id] = np.frombuffer(key, dtype=np.uint8)
                seen.add(key)
                break
            nonce += 1
    repaired = unpack_bits(packed, n_bits)
    if len({row.tobytes() for row in pack_bits(repaired)}) != len(repaired):
        raise RuntimeError("LSH collision repair failed to make token codes unique")
    return repaired, repairs


def build_lsh_codes_torch(embedding: torch.Tensor, bits: int = 64, seed: int = 0,
                          block: int = 8192, return_repairs: bool = False
                          ) -> torch.Tensor | tuple[torch.Tensor, int]:
    """Mean-centered Gaussian sign-LSH table with unique token codes."""
    if embedding.ndim != 2 or bits <= 0:
        raise ValueError("embedding must be [vocab, width] and bits must be positive")
    dev = embedding.device
    weight = embedding.float()
    mean = weight.mean(0)
    generator = torch.Generator(device=dev).manual_seed(seed)
    projection = torch.randn(
        bits, weight.shape[1], generator=generator, device=dev, dtype=torch.float32
    )
    out = torch.empty((len(weight), bits), dtype=torch.uint8, device="cpu")
    for lo in range(0, len(weight), block):
        code = ((weight[lo:lo + block] - mean) @ projection.t() > 0)
        out[lo:lo + block] = code.to("cpu", dtype=torch.uint8)
    repaired, repairs = make_lsh_codes_unique(out.numpy(), seed=seed)
    result = torch.from_numpy(repaired)
    return (result, repairs) if return_repairs else result


def build_lsh_codes_numpy(embedding: np.ndarray, bits: int = 64,
                          seed: int = 0, return_repairs: bool = False
                          ) -> np.ndarray | tuple[np.ndarray, int]:
    """Small CPU reference used by tests."""
    embedding = np.asarray(embedding, dtype=np.float32)
    centered = embedding - embedding.mean(0, keepdims=True)
    projection = np.random.default_rng(seed).standard_normal(
        (bits, embedding.shape[1]), dtype=np.float32
    )
    raw = (centered @ projection.T > 0).astype(np.uint8)
    repaired, repairs = make_lsh_codes_unique(raw, seed=seed)
    return (repaired, repairs) if return_repairs else repaired


def tokens_to_lsh_bits(input_ids: np.ndarray, attention_mask: np.ndarray,
                       codes: np.ndarray, chunk: int = 8192) -> np.ndarray:
    """Map fixed token layouts to flattened bits and neutralize padding."""
    ids = np.asarray(input_ids)
    attention = np.asarray(attention_mask, dtype=np.uint8)
    table = np.asarray(codes, dtype=np.uint8)
    if ids.shape != attention.shape or ids.ndim != 2:
        raise ValueError("input_ids and attention_mask must have the same 2-D shape")
    if ids.size and (ids.min() < 0 or ids.max() >= len(table)):
        raise ValueError("input token id lies outside the LSH table")
    out = np.empty((len(ids), ids.shape[1] * table.shape[1]), dtype=np.uint8)
    for lo in range(0, len(ids), chunk):
        block_ids = ids[lo:lo + chunk]
        block_mask = attention[lo:lo + chunk, :, None]
        out[lo:lo + chunk] = (table[block_ids] * block_mask).reshape(len(block_ids), -1)
    return out


def pack_bits(bits: np.ndarray) -> np.ndarray:
    return np.packbits(np.asarray(bits, dtype=np.uint8), axis=-1, bitorder="little")


def unpack_bits(packed: np.ndarray, n_bits: int) -> np.ndarray:
    return np.unpackbits(
        np.asarray(packed, dtype=np.uint8), axis=-1, count=n_bits,
        bitorder="little",
    ).astype(np.uint8, copy=False)


def sha256_array(array: np.ndarray) -> str:
    value = np.ascontiguousarray(array)
    return hashlib.sha256(value.view(np.uint8)).hexdigest()


def logspace_ste_characters(bits: torch.Tensor, theta: torch.Tensor,
                            eps: float = 1e-3) -> torch.Tensor:
    """Exact hard Walsh values with a non-vanishing log-space STE gradient."""
    bits_f = bits.float()
    probability = torch.sigmoid(theta.float())
    factor = torch.clamp((1.0 - 2.0 * probability).abs(), min=eps)
    log_magnitude = bits_f @ torch.log(factor).t()
    hard = (theta > 0).to(dtype=bits_f.dtype)
    parity = torch.remainder(bits_f @ hard.t(), 2.0)
    sign = 1.0 - 2.0 * parity
    surrogate = sign.detach() * torch.exp(log_magnitude.clamp(min=-30.0, max=0.0))
    return surrogate + (sign - surrogate).detach()


class _ProductCharacterSTE(torch.autograd.Function):
    """Exact hard characters with the multilinear extension's vertex gradient."""

    @staticmethod
    def forward(ctx, bits: torch.Tensor, theta: torch.Tensor,
                hard: torch.Tensor, scale: float) -> torch.Tensor:
        bits_f = bits.float()
        hard = hard.to(dtype=bits_f.dtype)
        parity = torch.remainder(bits_f @ hard.t(), 2.0)
        character = 1.0 - 2.0 * parity
        ctx.save_for_backward(bits_f, hard, character)
        ctx.scale = float(scale)
        return character

    @staticmethod
    def backward(ctx, grad_output: torch.Tensor):
        bits_f, hard, character = ctx.saved_tensors
        # At a Boolean mask vertex, d prod_i(1-2*x_i*m_i)/d m_n is
        # -2*x_n*character*(1-2*m_n). This matrix form avoids materializing
        # [batch, characters, input_bits].
        weighted = grad_output.float() * character
        theta_gradient = -2.0 * ctx.scale * (weighted.t() @ bits_f)
        theta_gradient = theta_gradient * (1.0 - 2.0 * hard)
        return None, theta_gradient, None, None


def product_ste_characters(bits: torch.Tensor, theta: torch.Tensor,
                           scale: float = 0.01,
                           hard: torch.Tensor | None = None) -> torch.Tensor:
    """Apply the hard multilinear-product STE with a tunable gradient scale."""
    if scale <= 0:
        raise ValueError("product STE scale must be positive")
    if hard is None:
        hard = theta > 0
    return _ProductCharacterSTE.apply(bits, theta, hard, scale)


class HardWalshStudent(nn.Module):
    """A configurable sum of M learned exact F_2 characters."""

    def __init__(self, n_bits: int, terms: int, seed: int = 0,
                 min_degree: int = 1, max_degree: int = 8,
                 off_init: float = -8.0, on_init: float = 8.0,
                 coefficient_std: float = 0.05, initial_probability: float = 0.5,
                 char_chunk: int = 8192,
                 bits_per_token: int | None = None,
                 initialization: str = "target_token",
                 ste_variant: str = "logspace", ste_scale: float = 0.01,
                 mask_parameterization: str = "threshold"):
        super().__init__()
        if not (0 < min_degree <= max_degree <= n_bits):
            raise ValueError("invalid initial degree range")
        if terms <= 0 or char_chunk <= 0:
            raise ValueError("terms and char_chunk must be positive")
        if bits_per_token is not None and bits_per_token <= 0:
            raise ValueError("bits_per_token must be positive")
        if initialization not in {
            "fixed_context", "target_token", "singleton_cover", "random"
        }:
            raise ValueError(f"unknown initialization {initialization!r}")
        if ste_variant not in {"logspace", "product"}:
            raise ValueError(f"unknown STE variant {ste_variant!r}")
        if mask_parameterization not in {"threshold", "topk"}:
            raise ValueError(
                f"unknown mask parameterization {mask_parameterization!r}"
            )
        if mask_parameterization == "topk" and ste_variant != "product":
            raise ValueError("topk masks currently require the product STE")
        rng = np.random.default_rng(seed)
        theta = np.full((terms, n_bits), off_init, dtype=np.float32)
        covered = (min(terms, n_bits) if initialization in {
            "fixed_context", "target_token", "singleton_cover"
        } else 0)
        # Cover every input coordinate before spending terms on interactions.
        # The target-first layout makes these degree-one Walsh coefficients a
        # strong, fully learnable baseline rather than hoping random masks land
        # on the few relevant token slots.
        if covered:
            permutation = rng.permutation(n_bits)
            theta[np.arange(covered), permutation[:covered]] = on_init
        focused = (round(0.75 * (terms - covered))
                   if initialization == "target_token" else 0)
        # Hybrid layouts append prompt-context bits after the original 2048
        # fixed-field bits. Reserve substantially more random/global characters
        # there; otherwise 85% of interactions never see the appended context.
        target_fraction = 0.35 if n_bits > 2048 else 0.40
        context_fraction = 0.25 if n_bits > 2048 else 0.45
        target_focused = (round(target_fraction * (terms - covered))
                          if initialization == "fixed_context" else 0)
        context_focused = (round(context_fraction * (terms - covered))
                           if initialization == "fixed_context" else 0)
        bits_per_token = (max(1, n_bits // 64) if bits_per_token is None
                          else bits_per_token)
        focus_start = min(n_bits, 2 * bits_per_token)
        focus_stop = min(n_bits, max(4 * bits_per_token, focus_start + max_degree))
        if focus_stop - focus_start < max_degree:
            focus_start, focus_stop = 0, n_bits
        for row in range(covered, terms):
            lower = max(2, min_degree) if covered else min_degree
            degree = int(rng.integers(lower, max_degree + 1))
            if initialization == "fixed_context" and row < covered + target_focused:
                pool = np.arange(0, min(n_bits, 16 * bits_per_token))
            elif (initialization == "fixed_context"
                  and row < covered + target_focused + context_focused):
                pool = np.arange(0, min(n_bits, 24 * bits_per_token))
            else:
                pool = (np.arange(focus_start, focus_stop)
                        if row < covered + focused else np.arange(n_bits))
            degree = min(degree, len(pool))
            theta[row, rng.choice(pool, degree, replace=False)] = on_init
        coefficient = rng.normal(0.0, coefficient_std, terms).astype(np.float32)
        p = float(np.clip(initial_probability, 1e-4, 1 - 1e-4))
        self.theta = nn.Parameter(torch.from_numpy(theta))
        self.register_buffer(
            "mask_degree", torch.from_numpy((theta > 0).sum(1).astype(np.int64))
        )
        self.coefficient = nn.Parameter(torch.from_numpy(coefficient))
        self.bias = nn.Parameter(torch.tensor(math.log(p / (1 - p)), dtype=torch.float32))
        self.n_bits = int(n_bits)
        self.terms = int(terms)
        self.char_chunk = int(char_chunk)
        self.bits_per_token = int(bits_per_token)
        self.initialization = initialization
        self.ste_variant = ste_variant
        self.ste_scale = float(ste_scale)
        self.mask_parameterization = mask_parameterization
        self.max_mask_degree = int(max_degree)
        self.output_scale = terms ** -0.5

    def forward(self, bits: torch.Tensor) -> torch.Tensor:
        if bits.ndim != 2 or bits.shape[1] != self.n_bits:
            raise ValueError(f"expected bits [batch,{self.n_bits}], got {tuple(bits.shape)}")
        output = self.bias.expand(len(bits)).float()
        for lo in range(0, self.terms, self.char_chunk):
            hi = min(lo + self.char_chunk, self.terms)
            theta = self.theta[lo:hi]
            hard = self._harden_chunk(theta, self.mask_degree[lo:hi])
            phi = (product_ste_characters(
                       bits, theta, self.ste_scale, hard=hard,
                   )
                   if self.ste_variant == "product"
                   else logspace_ste_characters(bits, theta))
            output = output + self.output_scale * (phi @ self.coefficient[lo:hi])
        return output

    @torch.no_grad()
    def hardened_masks(self) -> torch.Tensor:
        return self._harden_chunk(self.theta, self.mask_degree)

    def _harden_chunk(self, theta: torch.Tensor,
                      degree: torch.Tensor) -> torch.Tensor:
        if self.mask_parameterization == "threshold":
            return theta > 0
        # Rows have different fixed degrees. ``sorted=True`` is required:
        # taking the first ``degree`` entries from an unsorted top-max-degree
        # result does not necessarily select that row's actual top scores.
        indices = theta.topk(self.max_mask_degree, dim=1, sorted=True).indices
        active = (torch.arange(self.max_mask_degree, device=theta.device)[None, :]
                  < degree[:, None])
        hard = torch.zeros_like(theta, dtype=torch.bool)
        return hard.scatter(1, indices, active)

    @torch.no_grad()
    def hardened_index_rows(self, extra_candidates: int = 0) -> torch.Tensor:
        """Return sorted selected coordinates and optional score-ranked runners-up."""
        if self.mask_parameterization != "topk":
            raise ValueError("index rows are only defined for top-k masks")
        width = min(self.n_bits, self.max_mask_degree + extra_candidates)
        rows = []
        for lo in range(0, self.terms, self.char_chunk):
            hi = min(lo + self.char_chunk, self.terms)
            ranked = self.theta[lo:hi].topk(width, dim=1, sorted=True).indices
            selected = ranked[:, :self.max_mask_degree]
            active = (
                torch.arange(self.max_mask_degree, device=self.theta.device)[None, :]
                < self.mask_degree[lo:hi, None]
            )
            selected = selected.masked_fill(~active, self.n_bits).sort(dim=1).values
            rows.append(torch.cat((selected, ranked), dim=1))
        return torch.cat(rows)


@dataclasses.dataclass(slots=True)
class TrainConfig:
    terms: int = 131072
    steps: int = 1000
    batch_size: int = 16384
    char_chunk: int = 8192
    bits_per_token: int | None = None
    mask_lr: float = 0.2
    coefficient_lr: float = 3e-2
    mask_beta2: float = 0.999
    coefficient_beta2: float = 0.999
    weight_decay: float = 1e-4
    warmup_fraction: float = 0.02
    mask_warmup_fraction: float = 0.20
    minimum_lr_ratio: float = 0.10
    clip_norm: float = 1.0
    eval_every: int = 25
    patience: int = 12
    seed: int = 0
    compile_model: bool = True
    compile_mode: str = "max-autotune-no-cudagraphs"
    initialization: str = "fixed_context"
    mask_init_magnitude: float = 1.0
    ste_variant: str = "product"
    ste_scale: float = 0.01
    mask_parameterization: str = "topk"
    hard_target_mix: float = 0.25
    mask_discovery_fraction: float = 0.15
    mask_delay_fraction: float = 0.0
    mask_schedule: str = "discovery_cosine"
    balance_teacher_classes: bool = False
    repair_duplicate_masks: bool = False
    loss_scale: float = 40.0


def warmup_cosine_factor(step: int, total_steps: int, warmup_steps: int,
                         minimum_ratio: float = 0.1) -> float:
    if total_steps <= 0 or warmup_steps < 0 or not 0 <= minimum_ratio <= 1:
        raise ValueError("invalid scheduler arguments")
    if warmup_steps and step < warmup_steps:
        return (step + 1) / warmup_steps
    span = max(1, total_steps - warmup_steps)
    progress = min(1.0, max(0.0, (step - warmup_steps) / span))
    cosine = 0.5 * (1.0 + math.cos(math.pi * progress))
    return minimum_ratio + (1.0 - minimum_ratio) * cosine


def binary_metrics(logits: torch.Tensor | np.ndarray,
                   teacher_probability: torch.Tensor | np.ndarray,
                   gold: torch.Tensor | np.ndarray | None = None) -> dict[str, float]:
    z = torch.as_tensor(logits, dtype=torch.float64).reshape(-1)
    p = torch.as_tensor(teacher_probability, dtype=torch.float64).reshape(-1)
    if len(z) != len(p):
        raise ValueError("logits and teacher probabilities have different lengths")
    ce = torch.nn.functional.binary_cross_entropy_with_logits(z, p)
    entropy = -(p * torch.log(p.clamp_min(1e-12))
                + (1 - p) * torch.log((1 - p).clamp_min(1e-12))).mean()
    probability = torch.sigmoid(z)
    answer = probability >= 0.5
    teacher_answer = p >= 0.5
    teacher_positive = int(teacher_answer.sum())
    teacher_negative = len(teacher_answer) - teacher_positive
    positive_recall = float((answer & teacher_answer).sum() / max(1, teacher_positive))
    negative_recall = float((~answer & ~teacher_answer).sum() / max(1, teacher_negative))
    result = {
        "ce": float(ce),
        "kl": float(ce - entropy),
        "agreement": float((answer == teacher_answer).double().mean()),
        "balanced_agreement": 0.5 * (positive_recall + negative_recall),
        "teacher_positive_recall": positive_recall,
        "teacher_negative_recall": negative_recall,
        "teacher_positive_rate": teacher_positive / max(1, len(teacher_answer)),
        "brier": float(((probability - p) ** 2).mean()),
    }
    if gold is not None:
        y = torch.as_tensor(gold, dtype=torch.bool).reshape(-1)
        if len(y) != len(z):
            raise ValueError("gold has a different length")
        tp = int((answer & y).sum())
        fp = int((answer & ~y).sum())
        fn = int((~answer & y).sum())
        result["gold_accuracy"] = float((answer == y).double().mean())
        result["gold_f1"] = 2 * tp / max(1, 2 * tp + fp + fn)
    return result


def calibrate_agreement_threshold(logits: torch.Tensor | np.ndarray,
                                  teacher_probability: torch.Tensor | np.ndarray,
                                  minimum_positive_recall: float = 0.0,
                                  ) -> tuple[float, float]:
    """Select a validation logit threshold maximizing hard-teacher agreement."""
    if not 0.0 <= minimum_positive_recall <= 1.0:
        raise ValueError("minimum_positive_recall must lie in [0,1]")
    z = np.asarray(torch.as_tensor(logits).detach().cpu(), dtype=np.float64).reshape(-1)
    y = np.asarray(torch.as_tensor(teacher_probability).detach().cpu() >= 0.5,
                   dtype=np.bool_).reshape(-1)
    if len(z) != len(y) or not len(z):
        raise ValueError("calibration arrays must be non-empty and equally sized")
    order = np.argsort(z, kind="stable")
    sorted_z, sorted_y = z[order], y[order]
    positives = int(sorted_y.sum())
    correct = positives
    positive_correct = positives
    best_correct, best_threshold = correct, float("-inf")
    for index in range(len(sorted_z)):
        if sorted_y[index]:
            correct -= 1
            positive_correct -= 1
        else:
            correct += 1
        if index + 1 < len(sorted_z) and sorted_z[index] == sorted_z[index + 1]:
            continue
        threshold = (float(sorted_z[index]) + 1e-7 if index + 1 == len(sorted_z)
                     else float((sorted_z[index] + sorted_z[index + 1]) * 0.5))
        positive_recall = positive_correct / max(1, positives)
        if positive_recall >= minimum_positive_recall and correct > best_correct:
            best_correct, best_threshold = correct, threshold
    return best_threshold, best_correct / len(y)


def _gradient_norm(parameters: Sequence[nn.Parameter]) -> float:
    norms = [p.grad.detach().float().norm() for p in parameters if p.grad is not None]
    if not norms:
        return 0.0
    return float(torch.stack(norms).norm())


@torch.no_grad()
def _repair_duplicate_topk_masks(model: HardWalshStudent,
                                 optimizer: torch.optim.Optimizer,
                                 ) -> tuple[float, int]:
    """Move exact duplicate masks to their learned next-best coordinates."""
    candidates = model.hardened_index_rows(extra_candidates=8).cpu().numpy()
    selected = candidates[:, :model.max_mask_degree]
    _, first = np.unique(selected, axis=0, return_index=True)
    keep = np.zeros(len(selected), dtype=np.bool_)
    keep[first] = True
    duplicate_rows = np.flatnonzero(~keep)
    unique_fraction = len(first) / max(1, len(selected))
    if not len(duplicate_rows):
        return unique_fraction, 0
    degrees = model.mask_degree.cpu().numpy()
    remove = np.empty(len(duplicate_rows), dtype=np.int64)
    add = np.empty(len(duplicate_rows), dtype=np.int64)
    for out_index, row in enumerate(duplicate_rows):
        degree = int(degrees[row])
        active = selected[row, :degree]
        remove[out_index] = active[row % degree]
        runners_up = candidates[row, model.max_mask_degree:]
        replacement = next((int(x) for x in runners_up if x not in active), None)
        if replacement is None:
            replacement = int((active[-1] + row + 1) % model.n_bits)
            while replacement in active:
                replacement = (replacement + 1) % model.n_bits
        add[out_index] = replacement
    rows = torch.as_tensor(duplicate_rows, device=model.theta.device)
    old_columns = torch.as_tensor(remove, device=model.theta.device)
    new_columns = torch.as_tensor(add, device=model.theta.device)

    def swap_coordinates(value: torch.Tensor) -> None:
        old = value[rows, old_columns].clone()
        value[rows, old_columns] = value[rows, new_columns]
        value[rows, new_columns] = old

    swap_coordinates(model.theta)
    for value in optimizer.state.get(model.theta, {}).values():
        if torch.is_tensor(value) and value.shape == model.theta.shape:
            swap_coordinates(value)
    return unique_fraction, len(duplicate_rows)


@torch.no_grad()
def _sparse_mask_changed_fraction(model: HardWalshStudent,
                                  initial_indices: torch.Tensor) -> float:
    current = model.hardened_index_rows()[:, :model.max_mask_degree]
    initial = initial_indices.to(current.device)
    valid = current < model.n_bits
    common = ((current[:, :, None] == initial[:, None, :])
              & valid[:, :, None]).any(dim=2).sum(dtype=torch.int64)
    changed = 2 * (model.mask_degree.sum(dtype=torch.int64) - common)
    return float(changed / (model.terms * model.n_bits))


@torch.no_grad()
def evaluate_student(model: nn.Module, bits: torch.Tensor,
                     teacher_probability: torch.Tensor, gold: torch.Tensor,
                     batch_size: int = 8192) -> dict[str, float]:
    was_training = model.training
    model.eval()
    outputs = []
    for lo in range(0, len(bits), batch_size):
        outputs.append(model(bits[lo:lo + batch_size]).float().cpu())
    if was_training:
        model.train()
    logits = torch.cat(outputs)
    result = binary_metrics(logits, teacher_probability.cpu(), gold.cpu())
    threshold, agreement = calibrate_agreement_threshold(
        logits, teacher_probability.cpu()
    )
    result["calibrated_agreement"] = agreement
    result["calibrated_threshold"] = threshold
    return result


def _atomic_torch_save(value: Any, path: str | os.PathLike[str]) -> None:
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    torch.save(value, temporary)
    os.replace(temporary, path)


def train_student(train_bits: np.ndarray, train_probability: np.ndarray,
                  val_bits: np.ndarray, val_probability: np.ndarray,
                  val_gold: np.ndarray, config: TrainConfig,
                  device: str = "cuda", log: Callable[[dict[str, Any]], None] | None = None,
                  checkpoint_path: str | None = None,
                  resume: bool = False) -> tuple[HardWalshStudent, dict[str, Any]]:
    """Train with soft BCE, warmup+cosine AdamW, clipping, and early stopping."""
    overall_started = time.perf_counter()
    logger = log or (lambda _: None)
    if not 0.0 <= config.hard_target_mix <= 1.0:
        raise ValueError("hard_target_mix must lie in [0,1]")
    if config.loss_scale <= 0:
        raise ValueError("loss_scale must be positive")
    if config.mask_init_magnitude <= 0:
        raise ValueError("mask_init_magnitude must be positive")
    if not 0.0 <= config.mask_discovery_fraction <= 1.0:
        raise ValueError("mask_discovery_fraction must lie in [0,1]")
    if not 0.0 <= config.mask_delay_fraction <= config.mask_discovery_fraction:
        raise ValueError("mask_delay_fraction must lie in [0,mask_discovery_fraction]")
    if not 0.0 < config.mask_beta2 < 1.0:
        raise ValueError("mask_beta2 must lie in (0,1)")
    if not 0.0 < config.coefficient_beta2 < 1.0:
        raise ValueError("coefficient_beta2 must lie in (0,1)")
    if not 0.0 < config.mask_warmup_fraction <= 1.0:
        raise ValueError("mask_warmup_fraction must lie in (0,1]")
    if config.mask_schedule not in {"discovery_cosine", "global_cosine"}:
        raise ValueError("unknown mask schedule")
    torch.manual_seed(config.seed)
    dev = torch.device(device)
    if dev.type == "cuda":
        torch.set_float32_matmul_precision("high")
        torch.backends.cuda.matmul.allow_tf32 = True
    xtr = torch.as_tensor(train_bits, dtype=torch.uint8, device=dev)
    ptr = torch.as_tensor(train_probability, dtype=torch.float32, device=dev)
    xva = torch.as_tensor(val_bits, dtype=torch.uint8, device=dev)
    pva = torch.as_tensor(val_probability, dtype=torch.float32, device=dev)
    yva = torch.as_tensor(val_gold, dtype=torch.bool, device=dev)
    model = HardWalshStudent(
        xtr.shape[1], config.terms, seed=config.seed,
        initial_probability=float(ptr.mean()), char_chunk=config.char_chunk,
        bits_per_token=config.bits_per_token,
        initialization=config.initialization,
        off_init=-config.mask_init_magnitude,
        on_init=config.mask_init_magnitude,
        ste_variant=config.ste_variant, ste_scale=config.ste_scale,
        mask_parameterization=config.mask_parameterization,
    ).to(dev)
    initial_hard_masks = (model.hardened_index_rows()[:, :model.max_mask_degree].clone()
                          if model.mask_parameterization == "topk"
                          else model.hardened_masks().clone())
    optimizer = torch.optim.AdamW(
        [
            {"params": [model.theta], "lr": config.mask_lr, "weight_decay": 0.0,
             "betas": (0.9, config.mask_beta2)},
            {"params": [model.coefficient], "lr": config.coefficient_lr,
             "weight_decay": config.weight_decay,
             "betas": (0.9, config.coefficient_beta2)},
            {"params": [model.bias], "lr": config.coefficient_lr,
             "weight_decay": 0.0, "betas": (0.9, config.coefficient_beta2)},
        ],
        betas=(0.9, 0.999), eps=1e-8, fused=(dev.type == "cuda"),
    )
    warmup_steps = max(1, round(config.steps * config.warmup_fraction))
    mask_freeze_step = round(config.steps * config.mask_discovery_fraction)
    mask_start_step = round(config.steps * config.mask_delay_fraction)
    mask_active_steps = mask_freeze_step - mask_start_step
    mask_warmup_steps = max(1, round(mask_active_steps * config.mask_warmup_fraction))
    def coefficient_factor(step: int) -> float:
        return warmup_cosine_factor(
            step, config.steps, warmup_steps, config.minimum_lr_ratio
        )

    if mask_freeze_step == 0:
        def mask_factor(_step: int) -> float:
            return 0.0
    elif config.mask_schedule == "discovery_cosine":
        def mask_factor(step: int) -> float:
            if step < mask_start_step:
                return 0.0
            return warmup_cosine_factor(
                step - mask_start_step, mask_active_steps, mask_warmup_steps, 0.0
            )
    else:
        def mask_factor(step: int) -> float:
            if step < mask_start_step:
                return 0.0
            return warmup_cosine_factor(
                step - mask_start_step, config.steps - mask_start_step,
                mask_warmup_steps, config.minimum_lr_ratio,
            )
    scheduler = torch.optim.lr_scheduler.LambdaLR(
        optimizer,
        [mask_factor, coefficient_factor, coefficient_factor],
    )
    start_step = 0
    best_ce = math.inf
    best_calibrated_agreement = -math.inf
    best_step = 0
    stale = 0
    best_state: dict[str, torch.Tensor] | None = None
    if resume and checkpoint_path and Path(checkpoint_path).exists():
        saved = torch.load(checkpoint_path, map_location=dev, weights_only=False)
        model.load_state_dict(saved["model"])
        optimizer.load_state_dict(saved["optimizer"])
        scheduler.load_state_dict(saved["scheduler"])
        start_step = int(saved["step"])
        best_ce = float(saved["best_ce"])
        best_calibrated_agreement = float(
            saved.get("best_calibrated_agreement", -math.inf)
        )
        best_step = int(saved["best_step"])
        stale = int(saved["stale"])
        best_state = saved.get("best_state")
    if start_step >= mask_freeze_step:
        model.theta.requires_grad_(False)
        optimizer.param_groups[0]["lr"] = 0.0
        scheduler.base_lrs[0] = 0.0
    train_model: nn.Module = model
    compile_error = None
    if config.compile_model and dev.type == "cuda":
        try:
            train_model = torch.compile(
                model, mode=config.compile_mode, fullgraph=True, dynamic=False
            )
        except Exception as error:  # compilation is an optimization, not correctness
            compile_error = repr(error)
            train_model = model
    generator = torch.Generator(device=dev).manual_seed(config.seed + 1)
    base_logit = torch.logit(ptr.mean().clamp(1e-4, 1 - 1e-4))
    baseline = binary_metrics(
        torch.full_like(pva, base_logit).cpu(), pva.cpu(), yva.cpu()
    )
    try:
        initial = evaluate_student(train_model, xva, pva, yva, config.batch_size)
    except Exception as error:
        if train_model is model:
            raise
        compile_error = repr(error)
        train_model = model
        initial = evaluate_student(train_model, xva, pva, yva, config.batch_size)
    if best_state is None:
        best_ce = initial["ce"]
        best_calibrated_agreement = initial["calibrated_agreement"]
        best_state = {k: v.detach().cpu().clone() for k, v in model.state_dict().items()}
    logger({"global_step": start_step, **{f"val/{k}": v for k, v in initial.items()},
            **{f"baseline/{k}": v for k, v in baseline.items()}})
    history: list[dict[str, Any]] = []
    loop_started = time.perf_counter()
    last_log = loop_started
    current = start_step
    hard_train_target = (ptr >= 0.5).to(ptr.dtype)
    positive_weight = None
    if config.balance_teacher_classes:
        positives = hard_train_target.sum().clamp_min(1.0)
        negatives = (len(hard_train_target) - positives).clamp_min(1.0)
        positive_weight = negatives / positives
    for step in range(start_step, config.steps):
        if step == mask_freeze_step:
            model.theta.requires_grad_(False)
            model.theta.grad = None
            optimizer.param_groups[0]["lr"] = 0.0
            scheduler.base_lrs[0] = 0.0
            logger({"global_step": step, "optim/masks_frozen": 1.0})
        selection = torch.randint(
            0, len(xtr), (config.batch_size,), generator=generator, device=dev
        )
        optimizer.zero_grad(set_to_none=True)
        try:
            logits = train_model(xtr[selection])
            selected_probability = ptr[selection]
            training_target = torch.lerp(
                selected_probability,
                (selected_probability >= 0.5).to(selected_probability.dtype),
                config.hard_target_mix,
            )
            ce_loss = torch.nn.functional.binary_cross_entropy_with_logits(
                logits, training_target, pos_weight=positive_weight,
            )
            loss = ce_loss * config.loss_scale
            loss.backward()
        except Exception as error:
            if train_model is model or step != start_step:
                raise
            compile_error = repr(error)
            train_model = model
            optimizer.zero_grad(set_to_none=True)
            logits = train_model(xtr[selection])
            selected_probability = ptr[selection]
            training_target = torch.lerp(
                selected_probability,
                (selected_probability >= 0.5).to(selected_probability.dtype),
                config.hard_target_mix,
            )
            ce_loss = torch.nn.functional.binary_cross_entropy_with_logits(
                logits, training_target, pos_weight=positive_weight,
            )
            loss = ce_loss * config.loss_scale
            loss.backward()
        should_log = (step + 1) % 10 == 0
        group_norms = ({
            "grad/mask": _gradient_norm([model.theta]),
            "grad/coefficient": _gradient_norm([model.coefficient]),
            "grad/bias": _gradient_norm([model.bias]),
        } if should_log else {})
        global_norm_tensor = torch.nn.utils.clip_grad_norm_(
            model.parameters(), config.clip_norm
        )
        optimizer.step()
        scheduler.step()
        current = step + 1
        if should_log:
            if dev.type == "cuda":
                torch.cuda.synchronize(dev)
            now = time.perf_counter()
            elapsed = now - last_log
            last_log = now
            row = {
                "global_step": current,
                "train/ce": float(ce_loss.detach()),
                "grad/global_preclip": float(global_norm_tensor),
                "grad/clipped": float(global_norm_tensor > config.clip_norm),
                "optim/lr_mask": optimizer.param_groups[0]["lr"],
                "optim/lr_coefficient_raw": optimizer.param_groups[1]["lr"],
                "optim/lr_coefficient_exported": (
                    optimizer.param_groups[1]["lr"] * model.output_scale
                ),
                "perf/examples_per_second": 10 * config.batch_size / max(elapsed, 1e-9),
                **group_norms,
            }
            logger(row)
            history.append(row)
        if current % config.eval_every == 0 or current == config.steps:
            metrics = evaluate_student(train_model, xva, pva, yva, config.batch_size)
            with torch.no_grad():
                degrees = model.mask_degree.float()
                changed = (_sparse_mask_changed_fraction(model, initial_hard_masks)
                           if model.mask_parameterization == "topk" else
                           float((model.hardened_masks()
                                  != initial_hard_masks).float().mean()))
            logger({
                "global_step": current,
                **{f"val/{k}": v for k, v in metrics.items()},
                "mask/mean_degree": float(degrees.mean()),
                "mask/max_degree": float(degrees.max()),
                "mask/changed_fraction": float(changed),
            })
            improved = (
                metrics["calibrated_agreement"] > best_calibrated_agreement + 1e-7
                or (abs(metrics["calibrated_agreement"]
                        - best_calibrated_agreement) <= 1e-7
                    and metrics["ce"] < best_ce - 1e-7)
            )
            if improved:
                best_calibrated_agreement = metrics["calibrated_agreement"]
                best_ce = metrics["ce"]
                best_step = current
                stale = 0
                best_state = {k: v.detach().cpu().clone()
                              for k, v in model.state_dict().items()}
            else:
                stale += 1
            if (config.repair_duplicate_masks
                    and current <= mask_freeze_step
                    and model.mask_parameterization == "topk"):
                unique_fraction, repaired = _repair_duplicate_topk_masks(
                    model, optimizer
                )
                logger({
                    "global_step": current,
                    "mask/unique_fraction_before_repair": unique_fraction,
                    "mask/duplicates_repaired": repaired,
                })
            if checkpoint_path and (current % max(250, config.eval_every) == 0
                                    or stale >= config.patience):
                _atomic_torch_save({
                    "schema_version": SCHEMA_VERSION,
                    "config": dataclasses.asdict(config),
                    "model": model.state_dict(),
                    "optimizer": optimizer.state_dict(),
                    "scheduler": scheduler.state_dict(),
                    "step": current,
                    "best_ce": best_ce,
                    "best_calibrated_agreement": best_calibrated_agreement,
                    "best_step": best_step,
                    "stale": stale,
                    "best_state": best_state,
                }, checkpoint_path)
            if stale >= config.patience:
                break
    if best_state is not None:
        model.load_state_dict(best_state)
    final = evaluate_student(model, xva, pva, yva, config.batch_size)
    recent_norms = [row["grad/global_preclip"] for row in history[-10:]]
    median_norm = float(np.median(recent_norms)) if recent_norms else math.nan
    summary = {
        "steps_run": current,
        "best_step": best_step,
        "seconds": time.perf_counter() - overall_started,
        "training_loop_seconds": time.perf_counter() - loop_started,
        "mask_freeze_step": mask_freeze_step,
        "mask_start_step": mask_start_step,
        "mask_changed_fraction": float(
            _sparse_mask_changed_fraction(model, initial_hard_masks)
            if model.mask_parameterization == "topk" else
            (model.hardened_masks() != initial_hard_masks).float().mean()
        ),
        "baseline": baseline,
        "initial": initial,
        "val": final,
        "compile_error": compile_error,
        "grad_health": {
            "recent_median_preclip": median_norm,
            "preferred_range": [0.05, 1.0],
            "healthy": bool(math.isfinite(median_norm) and 0.05 <= median_norm <= 1.0),
        },
    }
    return model, summary


def export_sparse_student(model: HardWalshStudent,
                          coefficient_epsilon: float = 0.0) -> dict[str, Any]:
    """Deduplicate hard masks and export a compact CSR character list."""
    coefficient = (model.coefficient.detach().cpu().numpy().astype(np.float64)
                   * model.output_scale)
    if model.mask_parameterization == "topk":
        index_rows = (model.hardened_index_rows()[:, :model.max_mask_degree]
                      .cpu().numpy())
        unique_rows, inverse = np.unique(index_rows, axis=0, return_inverse=True)
    else:
        masks = model.hardened_masks().cpu().numpy().astype(np.uint8)
        packed = pack_bits(masks)
        unique, inverse = np.unique(packed, axis=0, return_inverse=True)
        unique_rows = None
    summed = np.zeros(len(unique_rows) if unique_rows is not None else len(unique),
                      dtype=np.float64)
    np.add.at(summed, inverse, coefficient)
    bias = float(model.bias.detach().cpu())
    if unique_rows is not None:
        counts = (unique_rows < model.n_bits).sum(1)
        empty = counts == 0
    else:
        unique_masks = unpack_bits(unique, model.n_bits)
        empty = unique_masks.sum(1) == 0
    if empty.any():
        bias += float(summed[empty].sum())
    keep = (~empty) & (np.abs(summed) > coefficient_epsilon)
    summed = summed[keep].astype(np.float32)
    if unique_rows is not None:
        unique_rows = unique_rows[keep]
        valid = unique_rows < model.n_bits
        counts = valid.sum(1)
        columns = unique_rows[valid]
    else:
        unique_masks = unique_masks[keep]
        rows, columns = np.nonzero(unique_masks)
        counts = np.bincount(rows, minlength=len(unique_masks))
    offsets = np.concatenate(([0], np.cumsum(counts))).astype(np.int32)
    index_dtype = np.uint16 if model.n_bits <= np.iinfo(np.uint16).max else np.uint32
    return {
        "schema_version": SCHEMA_VERSION,
        "n_bits": model.n_bits,
        "offsets": offsets,
        "indices": columns.astype(index_dtype),
        "coefficient": summed,
        "bias": bias,
    }


def sparse_student_logits(bits: np.ndarray, artifact: Mapping[str, Any],
                          chunk: int = 4096) -> np.ndarray:
    """Exact XOR reference inference for an exported sparse student."""
    x = np.asarray(bits, dtype=np.uint8)
    offsets = np.asarray(artifact["offsets"], dtype=np.int64)
    indices = np.asarray(artifact["indices"], dtype=np.int64)
    coefficient = np.asarray(artifact["coefficient"], dtype=np.float32)
    output = np.full(len(x), float(artifact["bias"]), dtype=np.float32)
    counts = np.diff(offsets)
    max_degree = int(counts.max(initial=0))
    if max_degree == 0:
        return output + coefficient.sum()
    padded = np.zeros((len(coefficient), max_degree), dtype=indices.dtype)
    active = np.arange(max_degree)[None, :] < counts[:, None]
    rows = np.repeat(np.arange(len(coefficient)), counts)
    columns = np.arange(len(indices)) - np.repeat(offsets[:-1], counts)
    padded[rows, columns] = indices
    # Bound the temporary selected-bit tensor near 64 MB. The learned masks
    # have degree <=8, making this substantially faster than a Python loop over
    # every Fourier term while retaining exact XOR arithmetic.
    memory_chunk = max(1, 64_000_000 // max(1, len(x) * max_degree))
    chunk = min(chunk, memory_chunk)
    for lo in range(0, len(coefficient), chunk):
        hi = min(lo + chunk, len(coefficient))
        parity = np.bitwise_xor.reduce(
            x[:, padded[lo:hi]], axis=2,
            where=active[None, lo:hi, :], initial=0,
        )
        phi = 1.0 - 2.0 * parity.astype(np.float32)
        output += phi @ coefficient[lo:hi]
    return output


def predict_token_layout(input_ids: np.ndarray, attention_mask: np.ndarray,
                         model_artifact: Mapping[str, Any]) -> np.ndarray:
    """Standalone inference from Qwen token ids without loading Qwen weights."""
    metadata = model_artifact["metadata"]
    lsh_bits = int(metadata["lsh_bits"])
    packed_codes = model_artifact["lsh_codes_packed"]
    if torch.is_tensor(packed_codes):
        packed_codes = packed_codes.cpu().numpy()
    codes = unpack_bits(packed_codes, lsh_bits)
    bits = tokens_to_lsh_bits(input_ids, attention_mask, codes)
    return sparse_student_logits(bits, model_artifact["student"])


def load_compact_student(path: str | os.PathLike[str]) -> dict[str, Any]:
    """Load the standalone ``.npz`` ensemble format without PyTorch pickle."""
    with np.load(path, allow_pickle=False) as saved:
        metadata = json.loads(str(saved["metadata_json"]))
        return {
            "metadata": metadata,
            "lsh_codes_packed": saved["lsh_codes_packed"].copy(),
            "student": {
                "schema_version": int(metadata["schema_version"]),
                "n_bits": int(saved["n_bits"]),
                "offsets": saved["offsets"].copy(),
                "indices": saved["indices"].copy(),
                "coefficient": saved["coefficient"].copy(),
                "bias": float(saved["bias"]),
            },
        }


def artifact_metadata(**extra: Any) -> dict[str, Any]:
    base = {
        "schema_version": SCHEMA_VERSION,
        "model_id": MODEL_ID,
        "model_revision": MODEL_REVISION,
        "dataset_id": DATASET_ID,
        "dataset_config": DATASET_CONFIG,
        "dataset_revision": DATASET_REVISION,
        "positive_upos": sorted(POSITIVE_UPOS),
    }
    base.update(extra)
    base["metadata_sha256"] = hashlib.sha256(
        json.dumps(base, sort_keys=True, separators=(",", ":")).encode()
    ).hexdigest()
    return base
