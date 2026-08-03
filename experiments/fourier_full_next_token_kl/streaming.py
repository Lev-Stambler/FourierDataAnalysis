"""Pure forward-only context streaming for the KISS experiment."""

from __future__ import annotations


CONTEXT_TOKENS = 16
TOKEN_BITS = 32


def fresh_context_batch(records, tokenizer, pending, batch_size: int,
                        context_tokens: int = CONTEXT_TOKENS):
    """Take one deterministic context from each new document without wrapping."""
    import numpy as np
    from fda_exp.qwen_argl import deterministic_span

    rows = []
    documents = 0
    while len(rows) < batch_size:
        while pending and len(rows) < batch_size:
            rows.append(pending.popleft())
        if len(rows) == batch_size:
            break
        texts = []
        while len(texts) < 256:
            record = next(records)
            documents += 1
            text = record.get("text") or ""
            if text:
                texts.append(text)
        tokenized = tokenizer(texts, add_special_tokens=False)["input_ids"]
        for token_ids, text in zip(tokenized, texts, strict=True):
            span = deterministic_span(token_ids, text, length=context_tokens)
            if span is not None:
                pending.append(span)
    return np.stack(rows).astype(np.int32, copy=False), documents
